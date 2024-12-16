(defpackage #:qr-symbol
  (:use #:common-lisp)
  (:export #:generate-qr-symbol
           ;; constants for error correction levels
           #:+ecl-L+
           #:+ecl-M+
           #:+ecl-Q+
           #:+ecl-H+
           ;; constants for implemented modes
           #:+mode-numeric+
           #:+mode-alphanumeric+
           #:+mode-byte+))

(in-package #:qr-symbol)

;;;; ! note that this code refers to the resulting "QR code" as the QR-symbol, following the lead of the specification document
;;;; this program uses what I call d-r coordinates, completely avoiding the use of x-y naming conventions and (what I consider) their very awkward adaptation from ...
;;;; ... mathematics (particularly in relation to x-first or y-first issues); in using the d-r (down-right) coordinate conceptualization, every grid structure is then ...
;;;; ... considered according to its innate numerical progression (such as with AREF) such that for any element (or coordinate) in the grid, to find it, the user ...
;;;; ... or program will always count down first, and then right; thus the d-r coordinate (3,5) means, start at very top left of grid, and then count down 3 rows ...
;;;; ... and then right 5 columns; also, note that coordinates are usually represented within this code as CONSes -- as in (CONS d r); finally, whenever coordinates ...
;;;; ... are split up into distinct values they are usually bound to the named symbols D and R

;;; these are module-specific constants; a module is the square-shaped section of the resulting QR-symbol, which is always either white or black
;;; ! note that every resulting QR-symbol is internally represented as a grid of CONSes where the CAR of each CONS is a code that indicates the type of the module ...
;;; ... and where the CDR value is the actual value of the module, being either a 0 or 1 -- white or black
(defparameter +white+ 0) ;; white is represented as 0 in grid
(defparameter +black+ 1) ;; black is represented as 1 in grid

;; this hash-table is used exclusively by the APPLY-MASK function (to change +WHITE+ to +BLACK+ or +BLACK+ to +WHITE+)
(defparameter *color-change* (make-hash-table))
(when (= 0 (hash-table-count *color-change*))
  (setf (gethash +white+ *color-change*) +black+)
  (setf (gethash +black+ *color-change*) +white+))

;;; here are some QR specification constants
(defparameter +timing-pattern-index+ 6) ;; the timing-pattern occurs in both the row and column at index 6
(defparameter +format-information-index+ 8) ;; the format-information can be found in row or column 8 (by 0-based index) following its own specific pattern
(defparameter +format-information-length+ 15) ;; the format-information binary is 15-bits
(defparameter +first-version-with-version-information+ 7) ;; only those QR-symbols of version 7 or higher will have version information
(defparameter +version-information-length+ 18) ;; the total length of the version-information binary is 18-bits
(defparameter +version-information-consed-values+ (cons 6 3)) ;; the version-information binary is 18-bits spread across 3 rows (6-bits per row)
(defparameter +version-information-distance+ 9) ;; this is the count of indexes to reach the bottom row of the version-information from bottom edge of QR-symbol
(defparameter +version-count+ 40) ;; this program supports generating QR-symbols up to version 40

(defun check-that-version-is-valid (version)
  "Triggers an error if VERSION is not of a correct value."
  (unless (integerp version)
    (error "VERSION must be an integer!"))
  (when (or (< version 1) (> version +version-count+))
    (error (format nil "The provided VERSION is invalid: ~d~%VERSION must be an integer 1-~d inclusive." version +version-count+))))

;; this hash-table is for speed and convenience and is used for converting zero or one characters to their appropriate integer values
(defparameter *zero/one-char-to-integer* (make-hash-table))
(when (= 0 (hash-table-count *zero/one-char-to-integer*))
  (setf (gethash #\0 *zero/one-char-to-integer*) 0)
  (setf (gethash #\1 *zero/one-char-to-integer*) 1))

;;; these are module codes (mc) that categorize a QR-symbol module
;;; ! note that every element of the QR-symbol follows this format: (CONS <module-code> <module-value>), where module-code is one of the following, and module-value ...
;;; ... is either +black+ or +white+
(defparameter +mc-position-detection-pattern+ 0)
(defparameter +mc-alignment-pattern+ 1)
(defparameter +mc-timing-pattern+ 2)
(defparameter +mc-format-information+ 3)
(defparameter +mc-format-information-placeholder+ 4)
(defparameter +mc-version-information+ 5)
(defparameter +mc-data+ 6)
(defparameter +mc-error-correction+ 7)

;; ! alignment patterns are the smaller squares (having a black-white-black pattern) that fill out regular positions within the QR-symbol
;; the following hash table yields, for each key -- where each key is a valid QR version -- a CONS where the CAR value is the number of ...
;; ... alignment patterns that need to be drawn for the given version; and the CDR value is a LIST where each element specifies a potential central ...
;; ... location for a single alignment pattern such that, for example, a 6 indicates that the coordinate 6-6 is the center of an alignment pattern ...
;; ... while a following 18 indicates that 6-18, 18-6, and 18-18 are further (potential) centers of other alignment patterns; notice the combinatorial pattern; ...
;; ... finally, any coordinate pair that does not directly fall on a "position detection pattern" should be considered the center of a valid alignment pattern
(defparameter *alignment-pattern-centers-by-version* (make-hash-table))
(when (= 0 (hash-table-count *alignment-pattern-centers-by-version*))
  (setf (gethash 1 *alignment-pattern-centers-by-version*) (cons 0 (list)))
  (setf (gethash 2 *alignment-pattern-centers-by-version*) (cons 1 (list 6 18)))
  (setf (gethash 3 *alignment-pattern-centers-by-version*) (cons 1 (list 6 22)))
  (setf (gethash 4 *alignment-pattern-centers-by-version*) (cons 1 (list 6 26)))
  (setf (gethash 5 *alignment-pattern-centers-by-version*) (cons 1 (list 6 30)))
  (setf (gethash 6 *alignment-pattern-centers-by-version*) (cons 1 (list 6 34)))
  (setf (gethash 7 *alignment-pattern-centers-by-version*) (cons 6 (list 6 22 38)))
  (setf (gethash 8 *alignment-pattern-centers-by-version*) (cons 6 (list 6 24 42)))
  (setf (gethash 9 *alignment-pattern-centers-by-version*) (cons 6 (list 6 26 46)))
  (setf (gethash 10 *alignment-pattern-centers-by-version*) (cons 6 (list 6 28 50)))
  (setf (gethash 11 *alignment-pattern-centers-by-version*) (cons 6 (list 6 30 54)))
  (setf (gethash 12 *alignment-pattern-centers-by-version*) (cons 6 (list 6 32 58)))
  (setf (gethash 13 *alignment-pattern-centers-by-version*) (cons 6 (list 6 34 62)))
  (setf (gethash 14 *alignment-pattern-centers-by-version*) (cons 13 (list 6 26 46 66)))
  (setf (gethash 15 *alignment-pattern-centers-by-version*) (cons 13 (list 6 26 48 70)))
  (setf (gethash 16 *alignment-pattern-centers-by-version*) (cons 13 (list 6 26 50 74)))
  (setf (gethash 17 *alignment-pattern-centers-by-version*) (cons 13 (list 6 30 54 78)))
  (setf (gethash 18 *alignment-pattern-centers-by-version*) (cons 13 (list 6 30 56 82)))
  (setf (gethash 19 *alignment-pattern-centers-by-version*) (cons 13 (list 6 30 58 86)))
  (setf (gethash 20 *alignment-pattern-centers-by-version*) (cons 13 (list 6 34 62 90)))
  (setf (gethash 21 *alignment-pattern-centers-by-version*) (cons 22 (list 6 28 50 72 94)))
  (setf (gethash 22 *alignment-pattern-centers-by-version*) (cons 22 (list 6 26 50 74 98)))
  (setf (gethash 23 *alignment-pattern-centers-by-version*) (cons 22 (list 6 30 54 78 102)))
  (setf (gethash 24 *alignment-pattern-centers-by-version*) (cons 22 (list 6 28 54 80 106)))
  (setf (gethash 25 *alignment-pattern-centers-by-version*) (cons 22 (list 6 32 58 84 110)))
  (setf (gethash 26 *alignment-pattern-centers-by-version*) (cons 22 (list 6 30 58 86 114)))
  (setf (gethash 27 *alignment-pattern-centers-by-version*) (cons 22 (list 6 34 62 90 118)))
  (setf (gethash 28 *alignment-pattern-centers-by-version*) (cons 33 (list 6 26 50 74 98 122)))
  (setf (gethash 29 *alignment-pattern-centers-by-version*) (cons 33 (list 6 30 54 78 102 126)))
  (setf (gethash 30 *alignment-pattern-centers-by-version*) (cons 33 (list 6 26 52 78 104 130)))
  (setf (gethash 31 *alignment-pattern-centers-by-version*) (cons 33 (list 6 30 56 82 108 134)))
  (setf (gethash 32 *alignment-pattern-centers-by-version*) (cons 33 (list 6 34 60 86 112 138)))
  (setf (gethash 33 *alignment-pattern-centers-by-version*) (cons 33 (list 6 30 58 86 114 142)))
  (setf (gethash 34 *alignment-pattern-centers-by-version*) (cons 33 (list 6 34 62 90 118 146)))
  (setf (gethash 35 *alignment-pattern-centers-by-version*) (cons 46 (list 6 30 54 78 102 126 150)))
  (setf (gethash 36 *alignment-pattern-centers-by-version*) (cons 46 (list 6 24 50 76 102 128 154)))
  (setf (gethash 37 *alignment-pattern-centers-by-version*) (cons 46 (list 6 28 54 80 106 132 158)))
  (setf (gethash 38 *alignment-pattern-centers-by-version*) (cons 46 (list 6 32 58 84 110 136 162)))
  (setf (gethash 39 *alignment-pattern-centers-by-version*) (cons 46 (list 6 26 54 82 110 138 166)))
  (setf (gethash 40 *alignment-pattern-centers-by-version*) (cons 46 (list 6 30 58 86 114 142 170))))

;;; these are descriptive constants that are to be used to access the related element of each vector in the hash-table *DATA-CAPACITIES-BY-VERSION* (further below)
(defparameter +width/height+ 0) ;; count of modules along any edge of QR-symbol ;; ! note that every QR-symbol is perfectly square in terms of module count
(defparameter +function-pattern-modules+ 1) ;; count of modules that are part of "position detection", "alignment", and "timing" patterns
(defparameter +format-and-version-modules+ 2) ;; count of modules that are part of "format" or "version" areas
(defparameter +data-modules+ 3) ;; count of all data modules including remainder bits
(defparameter +data-capacity+ 4) ;; total number of full 8-bit groupings
(defparameter +remainder-bits+ 5) ;; number of unusable bits at end

;; this hash-table keeps track of all related data limitations for every QR version, so that the program can more easily choose the optimal version ...
;; ... and keep track of relevant constraints
;; ! notice that each vector in this hash-table has a length of 6, so that each of the above constants acts as a descriptor of the numerically related index
(defparameter *data-capacities-by-version* (make-hash-table))
(when (= 0 (hash-table-count *data-capacities-by-version*))
  (setf (gethash 1 *data-capacities-by-version*) (vector 21 202 31 208 26 0))
  (setf (gethash 2 *data-capacities-by-version*) (vector 25 235 31 359 44 7))
  (setf (gethash 3 *data-capacities-by-version*) (vector 29 243 31 567 70 7))
  (setf (gethash 4 *data-capacities-by-version*) (vector 33 251 31 807 100 7))
  (setf (gethash 5 *data-capacities-by-version*) (vector 37 259 31 1079 134 7))
  (setf (gethash 6 *data-capacities-by-version*) (vector 41 267 31 1383 172 7))
  (setf (gethash 7 *data-capacities-by-version*) (vector 45 390 67 1568 196 0))
  (setf (gethash 8 *data-capacities-by-version*) (vector 49 398 67 1936 242 0))
  (setf (gethash 9 *data-capacities-by-version*) (vector 53 406 67 2336 292 0))
  (setf (gethash 10 *data-capacities-by-version*) (vector 57 414 67 2768 346 0))
  (setf (gethash 11 *data-capacities-by-version*) (vector 61 422 67 3232 404 0))
  (setf (gethash 12 *data-capacities-by-version*) (vector 65 430 67 3728 466 0))
  (setf (gethash 13 *data-capacities-by-version*) (vector 69 438 67 4256 532 0))
  (setf (gethash 14 *data-capacities-by-version*) (vector 73 611 67 4651 581 3))
  (setf (gethash 15 *data-capacities-by-version*) (vector 77 619 67 5243 655 3))
  (setf (gethash 16 *data-capacities-by-version*) (vector 81 627 67 5867 733 3))
  (setf (gethash 17 *data-capacities-by-version*) (vector 85 635 67 6523 815 3))
  (setf (gethash 18 *data-capacities-by-version*) (vector 89 643 67 7211 901 3))
  (setf (gethash 19 *data-capacities-by-version*) (vector 93 651 67 7931 991 3))
  (setf (gethash 20 *data-capacities-by-version*) (vector 97 659 67 8683 1085 3))
  (setf (gethash 21 *data-capacities-by-version*) (vector 101 882 67 9252 1156 4))
  (setf (gethash 22 *data-capacities-by-version*) (vector 105 890 67 10068 1258 4))
  (setf (gethash 23 *data-capacities-by-version*) (vector 109 898 67 10916 1364 4))
  (setf (gethash 24 *data-capacities-by-version*) (vector 113 906 67 11796 1474 4))
  (setf (gethash 25 *data-capacities-by-version*) (vector 117 914 67 12708 1588 4))
  (setf (gethash 26 *data-capacities-by-version*) (vector 121 922 67 13652 1706 4))
  (setf (gethash 27 *data-capacities-by-version*) (vector 125 930 67 14628 1828 4))
  (setf (gethash 28 *data-capacities-by-version*) (vector 129 1203 67 15371 1921 3))
  (setf (gethash 29 *data-capacities-by-version*) (vector 133 1211 67 16411 2051 3))
  (setf (gethash 30 *data-capacities-by-version*) (vector 137 1219 67 17483 2185 3))
  (setf (gethash 31 *data-capacities-by-version*) (vector 141 1227 67 18587 2323 3))
  (setf (gethash 32 *data-capacities-by-version*) (vector 145 1235 67 19723 2465 3))
  (setf (gethash 33 *data-capacities-by-version*) (vector 149 1243 67 20891 2611 3))
  (setf (gethash 34 *data-capacities-by-version*) (vector 153 1251 67 22091 2761 3))
  (setf (gethash 35 *data-capacities-by-version*) (vector 157 1574 67 23008 2876 0))
  (setf (gethash 36 *data-capacities-by-version*) (vector 161 1582 67 24272 3034 0))
  (setf (gethash 37 *data-capacities-by-version*) (vector 165 1590 67 25568 3196 0))
  (setf (gethash 38 *data-capacities-by-version*) (vector 169 1598 67 26896 3362 0))
  (setf (gethash 39 *data-capacities-by-version*) (vector 173 1606 67 28256 3532 0))
  (setf (gethash 40 *data-capacities-by-version*) (vector 177 1614 67 29648 3706 0)))

;;; these are descriptive constants that are to be used to access the related hash-table value in the hash-tables *ECL-VALUE-TO-ECL-STRING* and *ECL-TO-BINARY* ...
;;; ... and are to be used to access the appropriate array-element (as a direct index) for other tables that organize data by error-correction-level (ecl)
;;; they are also to be used when setting the error-correction-level (ecl) when generating a QR-symbol
(defparameter +ecl-L+ 0)
(defparameter +ecl-M+ 1)
(defparameter +ecl-Q+ 2)
(defparameter +ecl-H+ 3)

;; this hash-table is used to get the string descriptor of each of the error-correction-level (ecl) types
(defparameter *ecl-value-to-ecl-string* (make-hash-table))
(when (= 0 (hash-table-count *ecl-value-to-ecl-string*))
  (setf (gethash +ecl-L+ *ecl-value-to-ecl-string*) "L")
  (setf (gethash +ecl-M+ *ecl-value-to-ecl-string*) "M")
  (setf (gethash +ecl-Q+ *ecl-value-to-ecl-string*) "Q")
  (setf (gethash +ecl-H+ *ecl-value-to-ecl-string*) "H"))

(defun check-that-ecl-is-valid (ecl)
  "Triggers an error if ECL is not one of the implemented error-correction-levels."
  (unless (multiple-value-bind (value present-p) (gethash ecl *ecl-value-to-ecl-string*) (declare (ignore value)) present-p) ;; when integer not in table...
    (error (format nil "The value ~a is not a valid error-correction-level (ECL)!~%Please use one of the +ECL-*+ constants." ecl))))

;; this hash-table is used to get binary values for each error-correction-level (ecl) as used in format-information
;; ! note that the binary values yielded by this hash-table are NOT equivalent to the ecl integer values as set for the constants
(defparameter *ecl-to-binary* (make-hash-table))
(when (= 0 (hash-table-count *ecl-to-binary*))
  (setf (gethash +ecl-L+ *ecl-to-binary*) "01")
  (setf (gethash +ecl-M+ *ecl-to-binary*) "00")
  (setf (gethash +ecl-Q+ *ecl-to-binary*) "11")
  (setf (gethash +ecl-H+ *ecl-to-binary*) "10"))

;;; these are descriptive constants that are to be used to access the related index of each integer-only vector as found in the vector ...
;;; ... yielded up by the hash-table *DATA-CAPACITIES-FOR-ERROR-CORRECTION-LEVEL-BY-VERSION* (further below)
;;; ! note that a subset of +DCFEC-*+ constants (2-5 inclusively) are ordered according to +MODE-*+ constants below
(defparameter +dcfec-codeword-count+ 0)
(defparameter +dcfec-bit-count+ 1)
(defparameter +dcfec-numeric+ 2)
(defparameter +dcfec-alphanumeric+ 3)
(defparameter +dcfec-byte+ 4)
(defparameter +dcfec-kanji+ 5)

;; this hash-table is used to look up "data capacities" by version
;; ! note that each value provided through this hash-table is a vector of 4 vectors where each of the 4 vectors correlates to ...
;; ... an error-correction-level by its index, and whose integer literals correspond directly (by index) to the +DCFEC-*+ constants above
(defparameter *data-capacities-for-error-correction-level-by-version* (make-hash-table))
(when (= 0 (hash-table-count *data-capacities-for-error-correction-level-by-version*))
  (setf (gethash 1 *data-capacities-for-error-correction-level-by-version*) (vector (vector 19 152 41 25 17 10)
                                                                                    (vector 16 128 34 20 14 8)
                                                                                    (vector 13 104 27 16 11 7)
                                                                                    (vector 9 72 17 10 7 4)))
  (setf (gethash 2 *data-capacities-for-error-correction-level-by-version*) (vector (vector 34 272 77 47 32 20)
                                                                                    (vector 28 224 63 38 26 16)
                                                                                    (vector 22 176 48 29 20 12)
                                                                                    (vector 16 128 34 20 14 8)))
  (setf (gethash 3 *data-capacities-for-error-correction-level-by-version*) (vector (vector 55 440 127 77 53 32)
                                                                                    (vector 44 352 101 61 42 26)
                                                                                    (vector 34 272 77 47 32 20)
                                                                                    (vector 26 208 58 35 24 15)))
  (setf (gethash 4 *data-capacities-for-error-correction-level-by-version*) (vector (vector 80 640 187 114 78 48)
                                                                                    (vector 64 512 149 90 62 38)
                                                                                    (vector 48 384 111 67 46 28)
                                                                                    (vector 36 288 82 50 34 21)))
  (setf (gethash 5 *data-capacities-for-error-correction-level-by-version*) (vector (vector 108 864 255 154 106 65)
                                                                                    (vector 86 688 202 122 84 52)
                                                                                    (vector 62 496 144 87 60 37)
                                                                                    (vector 46 368 106 64 44 27)))
  (setf (gethash 6 *data-capacities-for-error-correction-level-by-version*) (vector (vector 136 1088 322 195 134 82)
                                                                                    (vector 108 864 255 154 106 65)
                                                                                    (vector 76 608 178 108 74 45)
                                                                                    (vector 60 480 139 84 58 36)))
  (setf (gethash 7 *data-capacities-for-error-correction-level-by-version*) (vector (vector 156 1248 370 224 154 95)
                                                                                    (vector 124 992 293 178 122 75)
                                                                                    (vector 88 704 207 125 86 53)
                                                                                    (vector 66 528 154 93 64 39)))
  (setf (gethash 8 *data-capacities-for-error-correction-level-by-version*) (vector (vector 194 1552 461 279 192 118)
                                                                                    (vector 154 1232 365 221 152 93)
                                                                                    (vector 110 880 259 157 108 66)
                                                                                    (vector 86 688 202 122 84 52)))
  (setf (gethash 9 *data-capacities-for-error-correction-level-by-version*) (vector (vector 232 1856 552 335 230 141)
                                                                                    (vector 182 1456 432 262 180 111)
                                                                                    (vector 132 1056 312 189 130 80)
                                                                                    (vector 100 800 235 143 98 60)))
  (setf (gethash 10 *data-capacities-for-error-correction-level-by-version*) (vector (vector 274 2192 652 395 271 167)
                                                                                     (vector 216 1728 513 311 213 131)
                                                                                     (vector 154 1232 364 221 151 93)
                                                                                     (vector 122 976 288 174 119 74)))
  (setf (gethash 11 *data-capacities-for-error-correction-level-by-version*) (vector (vector 324 2592 772 468 321 198)
                                                                                     (vector 254 2032 604 366 251 155)
                                                                                     (vector 180 1440 427 259 177 109)
                                                                                     (vector 140 1120 331 200 137 85)))
  (setf (gethash 12 *data-capacities-for-error-correction-level-by-version*) (vector (vector 370 2960 883 535 367 226)
                                                                                     (vector 290 2320 691 419 287 177)
                                                                                     (vector 206 1648 489 296 203 125)
                                                                                     (vector 158 1264 374 227 155 96)))
  (setf (gethash 13 *data-capacities-for-error-correction-level-by-version*) (vector (vector 428 3424 1022 619 425 262)
                                                                                     (vector 334 2672 796 483 331 204)
                                                                                     (vector 244 1952 580 352 241 149)
                                                                                     (vector 180 1440 427 259 177 109)))
  (setf (gethash 14 *data-capacities-for-error-correction-level-by-version*) (vector (vector 461 3688 1101 667 458 282)
                                                                                     (vector 365 2920 871 528 362 223)
                                                                                     (vector 261 2088 621 376 258 159)
                                                                                     (vector 197 1576 468 283 194 120)))
  (setf (gethash 15 *data-capacities-for-error-correction-level-by-version*) (vector (vector 523 4184 1250 758 520 320)
                                                                                     (vector 415 3320 991 600 412 254)
                                                                                     (vector 295 2360 703 426 292 180)
                                                                                     (vector 223 1784 530 321 220 136)))
  (setf (gethash 16 *data-capacities-for-error-correction-level-by-version*) (vector (vector 589 4712 1408 854 586 361)
                                                                                     (vector 453 3624 1082 656 450 277)
                                                                                     (vector 325 2600 775 470 322 198)
                                                                                     (vector 253 2024 602 365 250 154)))
  (setf (gethash 17 *data-capacities-for-error-correction-level-by-version*) (vector (vector 647 5176 1548 938 644 397)
                                                                                     (vector 507 4056 1212 734 504 310)
                                                                                     (vector 367 2936 876 531 364 224)
                                                                                     (vector 283 2264 674 408 280 173)))
  (setf (gethash 18 *data-capacities-for-error-correction-level-by-version*) (vector (vector 721 5768 1725 1046 718 442)
                                                                                     (vector 563 4504 1346 816 560 345)
                                                                                     (vector 397 3176 948 574 394 243)
                                                                                     (vector 313 2504 746 452 310 191)))
  (setf (gethash 19 *data-capacities-for-error-correction-level-by-version*) (vector (vector 795 6360 1903 1153 792 488)
                                                                                     (vector 627 5016 1500 909 624 384)
                                                                                     (vector 445 3560 1063 644 442 272)
                                                                                     (vector 341 2728 813 493 338 208)))
  (setf (gethash 20 *data-capacities-for-error-correction-level-by-version*) (vector (vector 861 6888 2061 1249 858 528)
                                                                                     (vector 669 5352 1600 970 666 410)
                                                                                     (vector 485 3880 1159 702 482 297)
                                                                                     (vector 385 3080 919 557 382 235)))
  (setf (gethash 21 *data-capacities-for-error-correction-level-by-version*) (vector (vector 932 7456 2232 1352 929 572)
                                                                                     (vector 714 5712 1708 1035 711 438)
                                                                                     (vector 512 4096 1224 742 509 314)
                                                                                     (vector 406 3248 969 587 403 248)))
  (setf (gethash 22 *data-capacities-for-error-correction-level-by-version*) (vector (vector 1006 8048 2409 1460 1003 618)
                                                                                     (vector 782 6256 1872 1134 779 480)
                                                                                     (vector 568 4544 1358 823 565 348)
                                                                                     (vector 442 3536 1056 640 439 270)))
  (setf (gethash 23 *data-capacities-for-error-correction-level-by-version*) (vector (vector 1094 8752 2620 1588 1091 672)
                                                                                     (vector 860 6880 2059 1248 857 528)
                                                                                     (vector 614 4912 1468 890 611 376)
                                                                                     (vector 464 3712 1108 672 461 284)))
  (setf (gethash 24 *data-capacities-for-error-correction-level-by-version*) (vector (vector 1174 9392 2812 1704 1171 721)
                                                                                     (vector 914 7312 2188 1326 911 561)
                                                                                     (vector 664 5312 1588 963 661 407)
                                                                                     (vector 514 4112 1228 744 511 315)))
  (setf (gethash 25 *data-capacities-for-error-correction-level-by-version*) (vector (vector 1276 10208 3057 1853 1273 784)
                                                                                     (vector 1000 8000 2395 1451 997 614)
                                                                                     (vector 718 5744 1718 1041 715 440)
                                                                                     (vector 538 4304 1286 779 535 330)))
  (setf (gethash 26 *data-capacities-for-error-correction-level-by-version*) (vector (vector 1370 10960 3283 1990 1367 842)
                                                                                     (vector 1062 8496 2544 1542 1059 652)
                                                                                     (vector 754 6032 1804 1094 751 462)
                                                                                     (vector 596 4768 1425 864 593 365)))
  (setf (gethash 27 *data-capacities-for-error-correction-level-by-version*) (vector (vector 1468 11744 3517 2132 1465 902)
                                                                                     (vector 1128 9024 2701 1637 1125 692)
                                                                                     (vector 808 6464 1933 1172 805 496)
                                                                                     (vector 628 5024 1501 910 625 385)))
  (setf (gethash 28 *data-capacities-for-error-correction-level-by-version*) (vector (vector 1531 12248 3669 2223 1528 940)
                                                                                     (vector 1193 9544 2857 1732 1190 732)
                                                                                     (vector 871 6968 2085 1263 868 534)
                                                                                     (vector 661 5288 1581 958 658 405)))
  (setf (gethash 29 *data-capacities-for-error-correction-level-by-version*) (vector (vector 1631 13048 3909 2369 1628 1002)
                                                                                     (vector 1267 10136 3035 1839 1264 778)
                                                                                     (vector 911 7288 2181 1322 908 559)
                                                                                     (vector 701 5608 1677 1016 698 430)))
  (setf (gethash 30 *data-capacities-for-error-correction-level-by-version*) (vector (vector 1735 13880 4158 2520 1732 1066)
                                                                                     (vector 1373 10984 3289 1994 1370 843)
                                                                                     (vector 985 7880 2358 1429 982 604)
                                                                                     (vector 745 5960 1782 1080 742 457)))
  (setf (gethash 31 *data-capacities-for-error-correction-level-by-version*) (vector (vector 1843 14744 4417 2677 1840 1132)
                                                                                     (vector 1455 11640 3486 2113 1452 894)
                                                                                     (vector 1033 8264 2473 1499 1030 634)
                                                                                     (vector 793 6344 1897 1150 790 486)))
  (setf (gethash 32 *data-capacities-for-error-correction-level-by-version*) (vector (vector 1955 15640 4686 2840 1952 1201)
                                                                                     (vector 1541 12328 3693 2238 1538 947)
                                                                                     (vector 1115 8920 2670 1618 1112 684)
                                                                                     (vector 845 6760 2022 1226 842 518)))
  (setf (gethash 33 *data-capacities-for-error-correction-level-by-version*) (vector (vector 2071 16568 4965 3009 2068 1273)
                                                                                     (vector 1631 13048 3909 2369 1628 1002)
                                                                                     (vector 1171 9368 2805 1700 1168 719)
                                                                                     (vector 901 7208 2157 1307 898 553)))
  (setf (gethash 34 *data-capacities-for-error-correction-level-by-version*) (vector (vector 2191 17528 5253 3183 2188 1347)
                                                                                     (vector 1725 13800 4134 2506 1722 1060)
                                                                                     (vector 1231 9848 2949 1787 1228 756)
                                                                                     (vector 961 7688 2301 1394 958 590)))
  (setf (gethash 35 *data-capacities-for-error-correction-level-by-version*) (vector (vector 2306 18448 5529 3351 2303 1417)
                                                                                     (vector 1812 14496 4343 2632 1809 1113)
                                                                                     (vector 1286 10288 3081 1867 1283 790)
                                                                                     (vector 986 7888 2361 1431 983 605)))
  (setf (gethash 36 *data-capacities-for-error-correction-level-by-version*) (vector (vector 2434 19472 5836 3537 2431 1496)
                                                                                     (vector 1914 15312 4588 2780 1911 1176)
                                                                                     (vector 1354 10832 3244 1966 1351 832)
                                                                                     (vector 1054 8432 2524 1530 1051 647)))
  (setf (gethash 37 *data-capacities-for-error-correction-level-by-version*) (vector (vector 2566 20528 6153 3729 2563 1577)
                                                                                     (vector 1992 15936 4775 2894 1989 1224)
                                                                                     (vector 1426 11408 3417 2071 1423 876)
                                                                                     (vector 1096 8768 2625 1591 1093 673)))
  (setf (gethash 38 *data-capacities-for-error-correction-level-by-version*) (vector (vector 2702 21616 6479 3927 2699 1661)
                                                                                     (vector 2102 16816 5039 3054 2099 1292)
                                                                                     (vector 1502 12016 3599 2181 1499 923)
                                                                                     (vector 1142 9136 2735 1658 1139 701)))
  (setf (gethash 39 *data-capacities-for-error-correction-level-by-version*) (vector (vector 2812 22496 6743 4087 2809 1729)
                                                                                     (vector 2216 17728 5313 3220 2213 1362)
                                                                                     (vector 1582 12656 3791 2298 1579 972)
                                                                                     (vector 1222 9776 2927 1774 1219 750)))
  (setf (gethash 40 *data-capacities-for-error-correction-level-by-version*) (vector (vector 2956 23648 7089 4296 2953 1817)
                                                                                     (vector 2334 18672 5596 3391 2331 1435)
                                                                                     (vector 1666 13328 3993 2420 1663 1024)
                                                                                     (vector 1276 10208 3057 1852 1273 784))))

;;; these are descriptive constants that are to be used to access each element of each distinct ecl-separated vector, as found in ...
;;; ... the *ERROR-CORRECTION-CHARACTERISTICS* hash-table below; note that ECC is simply an acronym for that hash-table's name
(defparameter +ecc-has-two+ 0) ;; indicates by boolean whether the list at index 2 (+ECC-BLOCK-COUNT+) AND at index 3 (+ECC-DETAILS+) has two elements
(defparameter +ecc-codeword-count+ 1) ;; the number of error-correction codewords (counting by 8-bit sections) in the final QR-symbol
(defparameter +ecc-block-count+ 2) ;; a list that directly relates to +ECC-DETAILS+, indicating the number of blocks that must be made to comply with ...
                                   ;; the specifications given by +ECC-DETAILS+; (for example, with +ECC-BLOCK-COUNT+ as (list 2 4), there would need ...
                                   ;; to be 2 groups for FIRST element of +ECC-DETAILS+ and 4 groups for SECOND element of +ECC-DETAILS+)
(defparameter +ecc-details+ 3) ;; a list of 1 or 2 vectors specifying other requirements (or details) that are explained immediately below...

;;; the following are descriptive constants that are to be used as indexes to access the related elements of any vector contained by the list +ECC-DETAILS+
(defparameter +ecc-details-total+ 0) ;; ! NOT ACTUALLY USED ;; refers to the total number of 8-bit sequences (data and error-correction) in each distinct block
(defparameter +ecc-details-count-per-block+ 1) ;; refers to the number of 8-bit sequences in each data-block as indicated by +ECC-BLOCK-COUNT+
(defparameter +ecc-details-capacity+ 2) ;; ! NOT ACTUALLY USED ;; is less than or equal to: ecc-codeword-count / <the sum of ecc-block-count list> / 2

;; this hash-table is used to lookup error-correction "characteristics" (ecc) by version number
;; ! note that each value provided through this hash-table is a vector of 4 vectors where each of the 4 vectors correlates to ...
;; ... an error-correction-level by its index, with each internal vector having values directly related to the "ecc" constants above
(defparameter *error-correction-characteristics* (make-hash-table))
(when (= 0 (hash-table-count *error-correction-characteristics*))
  (setf (gethash 1 *error-correction-characteristics*) (vector (vector nil 7 (list 1) (list (vector 26 19 2)))
                                                               (vector nil 10 (list 1) (list (vector 26 16 4)))
                                                               (vector nil 13 (list 1) (list (vector 26 13 6)))
                                                               (vector nil 17 (list 1) (list (vector 26 9 8)))))
  (setf (gethash 2 *error-correction-characteristics*) (vector (vector nil 10 (list 1) (list (vector 44 34 4)))
                                                               (vector nil 16 (list 1) (list (vector 44 28 8)))
                                                               (vector nil 22 (list 1) (list (vector 44 22 11)))
                                                               (vector nil 28 (list 1) (list (vector 44 16 14)))))
  (setf (gethash 3 *error-correction-characteristics*) (vector (vector nil 15 (list 1) (list (vector 70 55 7)))
                                                               (vector nil 26 (list 1) (list (vector 70 44 13)))
                                                               (vector nil 36 (list 2) (list (vector 35 17 9)))
                                                               (vector nil 44 (list 2) (list (vector 35 13 11)))))
  (setf (gethash 4 *error-correction-characteristics*) (vector (vector nil 20 (list 1) (list (vector 100 80 10)))
                                                               (vector nil 36 (list 2) (list (vector 50 32 9)))
                                                               (vector nil 52 (list 2) (list (vector 50 24 13)))
                                                               (vector nil 64 (list 4) (list (vector 25 9 8)))))
  (setf (gethash 5 *error-correction-characteristics*) (vector (vector nil 26 (list 1) (list (vector 134 108 13)))
                                                               (vector nil 48 (list 2) (list (vector 67 43 12)))
                                                               (vector t 72 (list 2 2) (list (vector 33 15 9) (vector 34 16 9)))
                                                               (vector t 88 (list 2 2) (list (vector 33 11 11) (vector 34 12 11)))))
  (setf (gethash 6 *error-correction-characteristics*) (vector (vector nil 36 (list 2) (list (vector 86 68 9)))
                                                               (vector nil 64 (list 4) (list (vector 43 27 8)))
                                                               (vector nil 96 (list 4) (list (vector 43 19 12)))
                                                               (vector nil 112 (list 4) (list (vector 43 15 14)))))
  (setf (gethash 7 *error-correction-characteristics*) (vector (vector nil 40 (list 2) (list (vector 98 78 10)))
                                                               (vector nil 72 (list 4) (list (vector 49 31 9)))
                                                               (vector t 108 (list 2 4) (list (vector 32 14 9) (vector 33 15 9)))
                                                               (vector t 130 (list 4 1) (list (vector 39 13 13) (vector 40 14 13)))))
  (setf (gethash 8 *error-correction-characteristics*) (vector (vector nil 48 (list 2) (list (vector 121 97 12)))
                                                               (vector t 88 (list 2 2) (list (vector 60 38 11) (vector 61 39 11)))
                                                               (vector t 132 (list 4 2) (list (vector 40 18 11) (vector 41 19 11)))
                                                               (vector t 156 (list 4 2) (list (vector 40 14 13) (vector 41 15 13)))))
  (setf (gethash 9 *error-correction-characteristics*) (vector (vector nil 60 (list 2) (list (vector 146 116 15)))
                                                               (vector t 110 (list 3 2) (list (vector 58 36 11) (vector 59 37 11)))
                                                               (vector t 160 (list 4 4) (list (vector 36 16 10) (vector 37 17 10)))
                                                               (vector t 192 (list 4 4) (list (vector 36 12 12) (vector 37 13 12)))))
  (setf (gethash 10 *error-correction-characteristics*) (vector (vector t 72 (list 2 2) (list (vector 86 68 9) (vector 87 69 9)))
                                                                (vector t 130 (list 4 1) (list (vector 69 43 13) (vector 70 44 13)))
                                                                (vector t 192 (list 6 2) (list (vector 43 19 12) (vector 44 20 12)))
                                                                (vector t 224 (list 6 2) (list (vector 43 15 14) (vector 44 16 14)))))
  (setf (gethash 11 *error-correction-characteristics*) (vector (vector nil 80 (list 4) (list (vector 101 81 10)))
                                                                (vector t 150 (list 1 4) (list (vector 80 50 15) (vector 81 51 15)))
                                                                (vector t 224 (list 4 4) (list (vector 50 22 14) (vector 51 23 14)))
                                                                (vector t 264 (list 3 8) (list (vector 36 12 12) (vector 37 13 12)))))
  (setf (gethash 12 *error-correction-characteristics*) (vector (vector t 96 (list 2 2) (list (vector 116 92 12) (vector 117 93 12)))
                                                                (vector t 176 (list 6 2) (list (vector 58 36 11) (vector 59 37 11)))
                                                                (vector t 260 (list 4 6) (list (vector 46 20 13) (vector 47 21 13)))
                                                                (vector t 308 (list 7 4) (list (vector 42 14 14) (vector 43 15 14)))))
  (setf (gethash 13 *error-correction-characteristics*) (vector (vector nil 104 (list 4) (list (vector 133 107 13)))
                                                                (vector t 198 (list 8 1) (list (vector 59 37 11) (vector 60 38 11)))
                                                                (vector t 288 (list 8 4) (list (vector 44 20 12) (vector 45 21 12)))
                                                                (vector t 352 (list 12 4) (list (vector 33 11 11) (vector 34 12 11)))))
  (setf (gethash 14 *error-correction-characteristics*) (vector (vector t 120 (list 3 1) (list (vector 145 115 15) (vector 146 116 15)))
                                                                (vector t 216 (list 4 5) (list (vector 64 40 12) (vector 65 41 12)))
                                                                (vector t 320 (list 11 5) (list (vector 36 16 10) (vector 37 17 10)))
                                                                (vector t 384 (list 11 5) (list (vector 36 12 12) (vector 37 13 12)))))
  (setf (gethash 15 *error-correction-characteristics*) (vector (vector t 132 (list 5 1) (list (vector 109 87 11) (vector 110 88 11)))
                                                                (vector t 240 (list 5 5) (list (vector 65 41 12) (vector 66 42 12)))
                                                                (vector t 360 (list 5 7) (list (vector 54 24 15) (vector 55 25 15)))
                                                                (vector t 432 (list 11 7) (list (vector 36 12 12) (vector 37 13 12)))))
  (setf (gethash 16 *error-correction-characteristics*) (vector (vector t 144 (list 5 1) (list (vector 122 98 12) (vector 123 99 12)))
                                                                (vector t 280 (list 7 3) (list (vector 73 45 14) (vector 74 46 14)))
                                                                (vector t 408 (list 15 2) (list (vector 43 19 12) (vector 44 20 12)))
                                                                (vector t 480 (list 3 13) (list (vector 45 15 15) (vector 46 16 15)))))
  (setf (gethash 17 *error-correction-characteristics*) (vector (vector t 168 (list 1 5) (list (vector 135 107 14) (vector 136 108 14)))
                                                                (vector t 308 (list 10 1) (list (vector 74 46 14) (vector 75 47 14)))
                                                                (vector t 448 (list 1 15) (list (vector 50 22 14) (vector 51 23 14)))
                                                                (vector t 532 (list 2 17) (list (vector 42 14 14) (vector 43 15 14)))))
  (setf (gethash 18 *error-correction-characteristics*) (vector (vector t 180 (list 5 1) (list (vector 150 120 15) (vector 151 121 15)))
                                                                (vector t 338 (list 9 4) (list (vector 69 43 13) (vector 70 44 13)))
                                                                (vector t 504 (list 17 1) (list (vector 50 22 14) (vector 51 23 14)))
                                                                (vector t 588 (list 2 19) (list (vector 42 14 14) (vector 43 15 14)))))
  (setf (gethash 19 *error-correction-characteristics*) (vector (vector t 196 (list 3 4) (list (vector 141 113 14) (vector 142 114 14)))
                                                                (vector t 364 (list 3 11) (list (vector 70 44 13) (vector 71 45 13)))
                                                                (vector t 546 (list 17 4) (list (vector 47 21 13) (vector 48 22 13)))
                                                                (vector t 650 (list 9 16) (list (vector 39 13 13) (vector 40 14 13)))))
  (setf (gethash 20 *error-correction-characteristics*) (vector (vector t 224 (list 3 5) (list (vector 135 107 14) (vector 136 108 14)))
                                                                (vector t 416 (list 3 13) (list (vector 67 41 13) (vector 68 42 13)))
                                                                (vector t 600 (list 15 5) (list (vector 54 24 15) (vector 55 25 15)))
                                                                (vector t 700 (list 15 10) (list (vector 43 15 14) (vector 44 16 14)))))
  (setf (gethash 21 *error-correction-characteristics*) (vector (vector t 224 (list 4 4) (list (vector 144 116 14) (vector 145 117 14)))
                                                                (vector nil 442 (list 17) (list (vector 68 42 13)))
                                                                (vector t 644 (list 17 6) (list (vector 50 22 14) (vector 51 23 14)))
                                                                (vector t 750 (list 19 6) (list (vector 46 16 15) (vector 47 17 15)))))
  (setf (gethash 22 *error-correction-characteristics*) (vector (vector t 252 (list 2 7) (list (vector 139 111 14) (vector 140 112 14)))
                                                                (vector nil 476 (list 17) (list (vector 74 46 14)))
                                                                (vector t 690 (list 7 16) (list (vector 54 24 15) (vector 55 25 15)))
                                                                (vector nil 816 (list 34) (list (vector 37 13 12)))))
  (setf (gethash 23 *error-correction-characteristics*) (vector (vector t 270 (list 4 5) (list (vector 151 121 15) (vector 152 122 15)))
                                                                (vector t 504 (list 4 14) (list (vector 75 47 14) (vector 76 48 14)))
                                                                (vector t 750 (list 11 14) (list (vector 54 24 15) (vector 55 25 15)))
                                                                (vector t 900 (list 16 14) (list (vector 45 15 15) (vector 46 16 15)))))
  (setf (gethash 24 *error-correction-characteristics*) (vector (vector t 300 (list 6 4) (list (vector 147 117 15) (vector 148 118 15)))
                                                                (vector t 560 (list 6 14) (list (vector 73 45 14) (vector 74 46 14)))
                                                                (vector t 810 (list 11 16) (list (vector 54 24 15) (vector 55 25 15)))
                                                                (vector t 960 (list 30 2) (list (vector 46 16 15) (vector 47 17 15)))))
  (setf (gethash 25 *error-correction-characteristics*) (vector (vector t 312 (list 8 4) (list (vector 132 106 13) (vector 133 107 13)))
                                                                (vector t 588 (list 8 13) (list (vector 75 47 14) (vector 76 48 14)))
                                                                (vector t 870 (list 7 22) (list (vector 54 24 15) (vector 55 25 15)))
                                                                (vector t 1050 (list 22 13) (list (vector 45 15 15) (vector 46 16 15)))))
  (setf (gethash 26 *error-correction-characteristics*) (vector (vector t 336 (list 10 2) (list (vector 142 114 14) (vector 143 115 14)))
                                                                (vector t 644 (list 19 4) (list (vector 74 46 14) (vector 75 47 14)))
                                                                (vector t 952 (list 28 6) (list (vector 50 22 14) (vector 51 23 14)))
                                                                (vector t 1110 (list 33 4) (list (vector 46 16 15) (vector 47 17 15)))))
  (setf (gethash 27 *error-correction-characteristics*) (vector (vector t 360 (list 8 4) (list (vector 152 122 15) (vector 153 123 15)))
                                                                (vector t 700 (list 22 3) (list (vector 73 45 14) (vector 74 46 14)))
                                                                (vector t 1020 (list 8 26) (list (vector 53 23 15) (vector 54 24 15)))
                                                                (vector t 1200 (list 12 28) (list (vector 45 15 15) (vector 46 16 15)))))
  (setf (gethash 28 *error-correction-characteristics*) (vector (vector t 390 (list 3 10) (list (vector 147 117 15) (vector 148 118 15)))
                                                                (vector t 728 (list 3 23) (list (vector 73 45 14) (vector 74 46 14)))
                                                                (vector t 1050 (list 4 31) (list (vector 54 24 15) (vector 55 25 15)))
                                                                (vector t 1260 (list 11 31) (list (vector 45 15 15) (vector 46 16 15)))))
  (setf (gethash 29 *error-correction-characteristics*) (vector (vector t 420 (list 7 7) (list (vector 146 116 15) (vector 147 117 15)))
                                                                (vector t 784 (list 21 7) (list (vector 73 45 14) (vector 74 46 14)))
                                                                (vector t 1140 (list 1 37) (list (vector 53 23 15) (vector 54 24 15)))
                                                                (vector t 1350 (list 19 26) (list (vector 45 15 15) (vector 46 16 15)))))
  (setf (gethash 30 *error-correction-characteristics*) (vector (vector t 450 (list 5 10) (list (vector 145 115 15) (vector 146 116 15)))
                                                                (vector t 812 (list 19 10) (list (vector 75 47 14) (vector 76 48 14)))
                                                                (vector t 1200 (list 15 25) (list (vector 54 24 15) (vector 55 25 15)))
                                                                (vector t 1440 (list 23 25) (list (vector 45 15 15) (vector 46 16 15)))))
  (setf (gethash 31 *error-correction-characteristics*) (vector (vector t 480 (list 13 3) (list (vector 145 115 15) (vector 146 116 15)))
                                                                (vector t 868 (list 2 29) (list (vector 74 46 14) (vector 75 47 14)))
                                                                (vector t 1290 (list 42 1) (list (vector 54 24 15) (vector 55 25 15)))
                                                                (vector t 1530 (list 23 28) (list (vector 45 15 15) (vector 46 16 15)))))
  (setf (gethash 32 *error-correction-characteristics*) (vector (vector nil 510 (list 17) (list (vector 145 115 15)))
                                                                (vector t 924 (list 10 23) (list (vector 74 46 14) (vector 75 47 14)))
                                                                (vector t 1350 (list 10 35) (list (vector 54 24 15) (vector 55 25 15)))
                                                                (vector t 1620 (list 19 35) (list (vector 45 15 15) (vector 46 16 15)))))
  (setf (gethash 33 *error-correction-characteristics*) (vector (vector t 540 (list 17 1) (list (vector 145 115 15) (vector 146 116 15)))
                                                                (vector t 980 (list 14 21) (list (vector 74 46 14) (vector 75 47 14)))
                                                                (vector t 1440 (list 29 19) (list (vector 54 24 15) (vector 55 25 15)))
                                                                (vector t 1710 (list 11 46) (list (vector 45 15 15) (vector 46 16 15)))))
  (setf (gethash 34 *error-correction-characteristics*) (vector (vector t 570 (list 13 6) (list (vector 145 115 15) (vector 146 116 15)))
                                                                (vector t 1036 (list 14 23) (list (vector 74 46 14) (vector 75 47 14)))
                                                                (vector t 1530 (list 44 7) (list (vector 54 24 15) (vector 55 25 15)))
                                                                (vector t 1800 (list 59 1) (list (vector 46 16 15) (vector 47 17 15)))))
  (setf (gethash 35 *error-correction-characteristics*) (vector (vector t 570 (list 12 7) (list (vector 151 121 15) (vector 152 122 15)))
                                                                (vector t 1064 (list 12 26) (list (vector 75 47 14) (vector 76 48 14)))
                                                                (vector t 1590 (list 39 14) (list (vector 54 24 15) (vector 55 25 15)))
                                                                (vector t 1890 (list 22 41) (list (vector 45 15 15) (vector 46 16 15)))))
  (setf (gethash 36 *error-correction-characteristics*) (vector (vector t 600 (list 6 14) (list (vector 151 121 15) (vector 152 122 15)))
                                                                (vector t 1120 (list 6 34) (list (vector 75 47 14) (vector 76 48 14)))
                                                                (vector t 1680 (list 46 10) (list (vector 54 24 15) (vector 55 25 15)))
                                                                (vector t 1980 (list 2 64) (list (vector 45 15 15) (vector 46 16 15)))))
  (setf (gethash 37 *error-correction-characteristics*) (vector (vector t 630 (list 17 4) (list (vector 152 122 15) (vector 153 123 15)))
                                                                (vector t 1204 (list 29 14) (list (vector 74 46 14) (vector 75 47 14)))
                                                                (vector t 1770 (list 49 10) (list (vector 54 24 15) (vector 55 25 15)))
                                                                (vector t 2100 (list 24 46) (list (vector 45 15 15) (vector 46 16 15)))))
  (setf (gethash 38 *error-correction-characteristics*) (vector (vector t 660 (list 4 18) (list (vector 152 122 15) (vector 153 123 15)))
                                                                (vector t 1260 (list 13 32) (list (vector 74 46 14) (vector 75 47 14)))
                                                                (vector t 1860 (list 48 14) (list (vector 54 24 15) (vector 55 25 15)))
                                                                (vector t 2220 (list 42 32) (list (vector 45 15 15) (vector 46 16 15)))))
  (setf (gethash 39 *error-correction-characteristics*) (vector (vector t 720 (list 20 4) (list (vector 147 117 15) (vector 148 118 15)))
                                                                (vector t 1316 (list 40 7) (list (vector 75 47 14) (vector 76 48 14)))
                                                                (vector t 1950 (list 43 22) (list (vector 54 24 15) (vector 55 25 15)))
                                                                (vector t 2310 (list 10 67) (list (vector 45 15 15) (vector 46 16 15)))))
  (setf (gethash 40 *error-correction-characteristics*) (vector (vector t 750 (list 19 6) (list (vector 148 118 15) (vector 149 119 15)))
                                                                (vector t 1372 (list 18 31) (list (vector 75 47 14) (vector 76 48 14)))
                                                                (vector t 2040 (list 34 34) (list (vector 54 24 15) (vector 55 25 15)))
                                                                (vector t 2430 (list 20 61) (list (vector 45 15 15) (vector 46 16 15))))))

;; the following hash-table contains all necessary "generator polynomials" as used in the generation of error-correction codewords
;; ! see the function CREATE-FINAL-BIT-STREAM-WITH-ERROR-CORRECTION for details on how this hash-table is used
;; ! this code was generated by other code in the file, create-generator-polynomial-hardcode.lisp; refer to that file for more details on how this was prepared
;; ! note that each value is an array of size equivalent to key-value plus one
(defparameter *generator-polynomials* (make-hash-table))
(when (= 0 (hash-table-count *generator-polynomials*))
  (setf (gethash 7 *generator-polynomials*) (vector (vector 0 7) (vector 87 6) (vector 229 5) (vector 146 4) (vector 149 3) (vector 238 2) (vector 102 1) (vector 21 0)))
  (setf (gethash 10 *generator-polynomials*) (vector (vector 0 10) (vector 251 9) (vector 67 8) (vector 46 7) (vector 61 6) (vector 118 5) (vector 70 4) (vector 64 3) (vector 94 2) (vector 32 1) (vector 45 0)))
  (setf (gethash 13 *generator-polynomials*) (vector (vector 0 13) (vector 74 12) (vector 152 11) (vector 176 10) (vector 100 9) (vector 86 8) (vector 100 7) (vector 106 6) (vector 104 5) (vector 130 4) (vector 218 3) (vector 206 2) (vector 140 1) (vector 78 0)))
  (setf (gethash 15 *generator-polynomials*) (vector (vector 0 15) (vector 8 14) (vector 183 13) (vector 61 12) (vector 91 11) (vector 202 10) (vector 37 9) (vector 51 8) (vector 58 7) (vector 58 6) (vector 237 5) (vector 140 4) (vector 124 3) (vector 5 2) (vector 99 1) (vector 105 0)))
  (setf (gethash 16 *generator-polynomials*) (vector (vector 0 16) (vector 120 15) (vector 104 14) (vector 107 13) (vector 109 12) (vector 102 11) (vector 161 10) (vector 76 9) (vector 3 8) (vector 91 7) (vector 191 6) (vector 147 5) (vector 169 4) (vector 182 3) (vector 194 2) (vector 225 1) (vector 120 0)))
  (setf (gethash 17 *generator-polynomials*) (vector (vector 0 17) (vector 43 16) (vector 139 15) (vector 206 14) (vector 78 13) (vector 43 12) (vector 239 11) (vector 123 10) (vector 206 9) (vector 214 8) (vector 147 7) (vector 24 6) (vector 99 5) (vector 150 4) (vector 39 3) (vector 243 2) (vector 163 1) (vector 136 0)))
  (setf (gethash 18 *generator-polynomials*) (vector (vector 0 18) (vector 215 17) (vector 234 16) (vector 158 15) (vector 94 14) (vector 184 13) (vector 97 12) (vector 118 11) (vector 170 10) (vector 79 9) (vector 187 8) (vector 152 7) (vector 148 6) (vector 252 5) (vector 179 4) (vector 5 3) (vector 98 2) (vector 96 1) (vector 153 0)))
  (setf (gethash 20 *generator-polynomials*) (vector (vector 0 20) (vector 17 19) (vector 60 18) (vector 79 17) (vector 50 16) (vector 61 15) (vector 163 14) (vector 26 13) (vector 187 12) (vector 202 11) (vector 180 10) (vector 221 9) (vector 225 8) (vector 83 7) (vector 239 6) (vector 156 5) (vector 164 4) (vector 212 3) (vector 212 2) (vector 188 1) (vector 190 0)))
  (setf (gethash 22 *generator-polynomials*) (vector (vector 0 22) (vector 210 21) (vector 171 20) (vector 247 19) (vector 242 18) (vector 93 17) (vector 230 16) (vector 14 15) (vector 109 14) (vector 221 13) (vector 53 12) (vector 200 11) (vector 74 10) (vector 8 9) (vector 172 8) (vector 98 7) (vector 80 6) (vector 219 5) (vector 134 4) (vector 160 3) (vector 105 2) (vector 165 1) (vector 231 0)))
  (setf (gethash 24 *generator-polynomials*) (vector (vector 0 24) (vector 229 23) (vector 121 22) (vector 135 21) (vector 48 20) (vector 211 19) (vector 117 18) (vector 251 17) (vector 126 16) (vector 159 15) (vector 180 14) (vector 169 13) (vector 152 12) (vector 192 11) (vector 226 10) (vector 228 9) (vector 218 8) (vector 111 7) (vector 0 6) (vector 117 5) (vector 232 4) (vector 87 3) (vector 96 2) (vector 227 1) (vector 21 0)))
  (setf (gethash 26 *generator-polynomials*) (vector (vector 0 26) (vector 173 25) (vector 125 24) (vector 158 23) (vector 2 22) (vector 103 21) (vector 182 20) (vector 118 19) (vector 17 18) (vector 145 17) (vector 201 16) (vector 111 15) (vector 28 14) (vector 165 13) (vector 53 12) (vector 161 11) (vector 21 10) (vector 245 9) (vector 142 8) (vector 13 7) (vector 102 6) (vector 48 5) (vector 227 4) (vector 153 3) (vector 145 2) (vector 218 1) (vector 70 0)))
  (setf (gethash 28 *generator-polynomials*) (vector (vector 0 28) (vector 168 27) (vector 223 26) (vector 200 25) (vector 104 24) (vector 224 23) (vector 234 22) (vector 108 21) (vector 180 20) (vector 110 19) (vector 190 18) (vector 195 17) (vector 147 16) (vector 205 15) (vector 27 14) (vector 232 13) (vector 201 12) (vector 21 11) (vector 43 10) (vector 245 9) (vector 87 8) (vector 42 7) (vector 195 6) (vector 212 5) (vector 119 4) (vector 242 3) (vector 37 2) (vector 9 1) (vector 123 0)))
  (setf (gethash 30 *generator-polynomials*) (vector (vector 0 30) (vector 41 29) (vector 173 28) (vector 145 27) (vector 152 26) (vector 216 25) (vector 31 24) (vector 179 23) (vector 182 22) (vector 50 21) (vector 48 20) (vector 110 19) (vector 86 18) (vector 239 17) (vector 96 16) (vector 222 15) (vector 125 14) (vector 42 13) (vector 173 12) (vector 226 11) (vector 193 10) (vector 224 9) (vector 130 8) (vector 156 7) (vector 37 6) (vector 251 5) (vector 216 4) (vector 238 3) (vector 40 2) (vector 192 1) (vector 180 0)))
  (setf (gethash 32 *generator-polynomials*) (vector (vector 0 32) (vector 10 31) (vector 6 30) (vector 106 29) (vector 190 28) (vector 249 27) (vector 167 26) (vector 4 25) (vector 67 24) (vector 209 23) (vector 138 22) (vector 138 21) (vector 32 20) (vector 242 19) (vector 123 18) (vector 89 17) (vector 27 16) (vector 120 15) (vector 185 14) (vector 80 13) (vector 156 12) (vector 38 11) (vector 69 10) (vector 171 9) (vector 60 8) (vector 28 7) (vector 222 6) (vector 80 5) (vector 52 4) (vector 254 3) (vector 185 2) (vector 220 1) (vector 241 0)))
  (setf (gethash 34 *generator-polynomials*) (vector (vector 0 34) (vector 111 33) (vector 77 32) (vector 146 31) (vector 94 30) (vector 26 29) (vector 21 28) (vector 108 27) (vector 19 26) (vector 105 25) (vector 94 24) (vector 113 23) (vector 193 22) (vector 86 21) (vector 140 20) (vector 163 19) (vector 125 18) (vector 58 17) (vector 158 16) (vector 229 15) (vector 239 14) (vector 218 13) (vector 103 12) (vector 56 11) (vector 70 10) (vector 114 9) (vector 61 8) (vector 183 7) (vector 129 6) (vector 167 5) (vector 13 4) (vector 98 3) (vector 62 2) (vector 129 1) (vector 51 0)))
  (setf (gethash 36 *generator-polynomials*) (vector (vector 0 36) (vector 200 35) (vector 183 34) (vector 98 33) (vector 16 32) (vector 172 31) (vector 31 30) (vector 246 29) (vector 234 28) (vector 60 27) (vector 152 26) (vector 115 25) (vector 0 24) (vector 167 23) (vector 152 22) (vector 113 21) (vector 248 20) (vector 238 19) (vector 107 18) (vector 18 17) (vector 63 16) (vector 218 15) (vector 37 14) (vector 87 13) (vector 210 12) (vector 105 11) (vector 177 10) (vector 120 9) (vector 74 8) (vector 121 7) (vector 196 6) (vector 117 5) (vector 251 4) (vector 113 3) (vector 233 2) (vector 30 1) (vector 120 0)))
  (setf (gethash 40 *generator-polynomials*) (vector (vector 0 40) (vector 59 39) (vector 116 38) (vector 79 37) (vector 161 36) (vector 252 35) (vector 98 34) (vector 128 33) (vector 205 32) (vector 128 31) (vector 161 30) (vector 247 29) (vector 57 28) (vector 163 27) (vector 56 26) (vector 235 25) (vector 106 24) (vector 53 23) (vector 26 22) (vector 187 21) (vector 174 20) (vector 226 19) (vector 104 18) (vector 170 17) (vector 7 16) (vector 175 15) (vector 35 14) (vector 181 13) (vector 114 12) (vector 88 11) (vector 41 10) (vector 47 9) (vector 163 8) (vector 125 7) (vector 134 6) (vector 72 5) (vector 20 4) (vector 232 3) (vector 53 2) (vector 35 1) (vector 15 0)))
  (setf (gethash 42 *generator-polynomials*) (vector (vector 0 42) (vector 250 41) (vector 103 40) (vector 221 39) (vector 230 38) (vector 25 37) (vector 18 36) (vector 137 35) (vector 231 34) (vector 0 33) (vector 3 32) (vector 58 31) (vector 242 30) (vector 221 29) (vector 191 28) (vector 110 27) (vector 84 26) (vector 230 25) (vector 8 24) (vector 188 23) (vector 106 22) (vector 96 21) (vector 147 20) (vector 15 19) (vector 131 18) (vector 139 17) (vector 34 16) (vector 101 15) (vector 223 14) (vector 39 13) (vector 101 12) (vector 213 11) (vector 199 10) (vector 237 9) (vector 254 8) (vector 201 7) (vector 123 6) (vector 171 5) (vector 162 4) (vector 194 3) (vector 117 2) (vector 50 1) (vector 96 0)))
  (setf (gethash 44 *generator-polynomials*) (vector (vector 0 44) (vector 190 43) (vector 7 42) (vector 61 41) (vector 121 40) (vector 71 39) (vector 246 38) (vector 69 37) (vector 55 36) (vector 168 35) (vector 188 34) (vector 89 33) (vector 243 32) (vector 191 31) (vector 25 30) (vector 72 29) (vector 123 28) (vector 9 27) (vector 145 26) (vector 14 25) (vector 247 24) (vector 1 23) (vector 238 22) (vector 44 21) (vector 78 20) (vector 143 19) (vector 62 18) (vector 224 17) (vector 126 16) (vector 118 15) (vector 114 14) (vector 68 13) (vector 163 12) (vector 52 11) (vector 194 10) (vector 217 9) (vector 147 8) (vector 204 7) (vector 169 6) (vector 37 5) (vector 130 4) (vector 113 3) (vector 102 2) (vector 73 1) (vector 181 0)))
  (setf (gethash 46 *generator-polynomials*) (vector (vector 0 46) (vector 112 45) (vector 94 44) (vector 88 43) (vector 112 42) (vector 253 41) (vector 224 40) (vector 202 39) (vector 115 38) (vector 187 37) (vector 99 36) (vector 89 35) (vector 5 34) (vector 54 33) (vector 113 32) (vector 129 31) (vector 44 30) (vector 58 29) (vector 16 28) (vector 135 27) (vector 216 26) (vector 169 25) (vector 211 24) (vector 36 23) (vector 1 22) (vector 4 21) (vector 96 20) (vector 60 19) (vector 241 18) (vector 73 17) (vector 104 16) (vector 234 15) (vector 8 14) (vector 249 13) (vector 245 12) (vector 119 11) (vector 174 10) (vector 52 9) (vector 25 8) (vector 157 7) (vector 224 6) (vector 43 5) (vector 202 4) (vector 223 3) (vector 19 2) (vector 82 1) (vector 15 0)))
  (setf (gethash 48 *generator-polynomials*) (vector (vector 0 48) (vector 228 47) (vector 25 46) (vector 196 45) (vector 130 44) (vector 211 43) (vector 146 42) (vector 60 41) (vector 24 40) (vector 251 39) (vector 90 38) (vector 39 37) (vector 102 36) (vector 240 35) (vector 61 34) (vector 178 33) (vector 63 32) (vector 46 31) (vector 123 30) (vector 115 29) (vector 18 28) (vector 221 27) (vector 111 26) (vector 135 25) (vector 160 24) (vector 182 23) (vector 205 22) (vector 107 21) (vector 206 20) (vector 95 19) (vector 150 18) (vector 120 17) (vector 184 16) (vector 91 15) (vector 21 14) (vector 247 13) (vector 156 12) (vector 140 11) (vector 238 10) (vector 191 9) (vector 11 8) (vector 94 7) (vector 227 6) (vector 84 5) (vector 50 4) (vector 163 3) (vector 39 2) (vector 34 1) (vector 108 0)))
  (setf (gethash 50 *generator-polynomials*) (vector (vector 0 50) (vector 232 49) (vector 125 48) (vector 157 47) (vector 161 46) (vector 164 45) (vector 9 44) (vector 118 43) (vector 46 42) (vector 209 41) (vector 99 40) (vector 203 39) (vector 193 38) (vector 35 37) (vector 3 36) (vector 209 35) (vector 111 34) (vector 195 33) (vector 242 32) (vector 203 31) (vector 225 30) (vector 46 29) (vector 13 28) (vector 32 27) (vector 160 26) (vector 126 25) (vector 209 24) (vector 130 23) (vector 160 22) (vector 242 21) (vector 215 20) (vector 242 19) (vector 75 18) (vector 77 17) (vector 42 16) (vector 189 15) (vector 32 14) (vector 113 13) (vector 65 12) (vector 124 11) (vector 69 10) (vector 228 9) (vector 114 8) (vector 235 7) (vector 175 6) (vector 124 5) (vector 170 4) (vector 215 3) (vector 232 2) (vector 133 1) (vector 205 0)))
  (setf (gethash 52 *generator-polynomials*) (vector (vector 0 52) (vector 116 51) (vector 50 50) (vector 86 49) (vector 186 48) (vector 50 47) (vector 220 46) (vector 251 45) (vector 89 44) (vector 192 43) (vector 46 42) (vector 86 41) (vector 127 40) (vector 124 39) (vector 19 38) (vector 184 37) (vector 233 36) (vector 151 35) (vector 215 34) (vector 22 33) (vector 14 32) (vector 59 31) (vector 145 30) (vector 37 29) (vector 242 28) (vector 203 27) (vector 134 26) (vector 254 25) (vector 89 24) (vector 190 23) (vector 94 22) (vector 59 21) (vector 65 20) (vector 124 19) (vector 113 18) (vector 100 17) (vector 233 16) (vector 235 15) (vector 121 14) (vector 22 13) (vector 76 12) (vector 86 11) (vector 97 10) (vector 39 9) (vector 242 8) (vector 200 7) (vector 220 6) (vector 101 5) (vector 33 4) (vector 239 3) (vector 254 2) (vector 116 1) (vector 51 0)))
  (setf (gethash 54 *generator-polynomials*) (vector (vector 0 54) (vector 183 53) (vector 26 52) (vector 201 51) (vector 87 50) (vector 210 49) (vector 221 48) (vector 113 47) (vector 21 46) (vector 46 45) (vector 65 44) (vector 45 43) (vector 50 42) (vector 238 41) (vector 184 40) (vector 249 39) (vector 225 38) (vector 102 37) (vector 58 36) (vector 209 35) (vector 218 34) (vector 109 33) (vector 165 32) (vector 26 31) (vector 95 30) (vector 184 29) (vector 192 28) (vector 52 27) (vector 245 26) (vector 35 25) (vector 254 24) (vector 238 23) (vector 175 22) (vector 172 21) (vector 79 20) (vector 123 19) (vector 25 18) (vector 122 17) (vector 43 16) (vector 120 15) (vector 108 14) (vector 215 13) (vector 80 12) (vector 128 11) (vector 201 10) (vector 235 9) (vector 8 8) (vector 153 7) (vector 59 6) (vector 101 5) (vector 31 4) (vector 198 3) (vector 76 2) (vector 31 1) (vector 156 0)))
  (setf (gethash 56 *generator-polynomials*) (vector (vector 0 56) (vector 106 55) (vector 120 54) (vector 107 53) (vector 157 52) (vector 164 51) (vector 216 50) (vector 112 49) (vector 116 48) (vector 2 47) (vector 91 46) (vector 248 45) (vector 163 44) (vector 36 43) (vector 201 42) (vector 202 41) (vector 229 40) (vector 6 39) (vector 144 38) (vector 254 37) (vector 155 36) (vector 135 35) (vector 208 34) (vector 170 33) (vector 209 32) (vector 12 31) (vector 139 30) (vector 127 29) (vector 142 28) (vector 182 27) (vector 249 26) (vector 177 25) (vector 174 24) (vector 190 23) (vector 28 22) (vector 10 21) (vector 85 20) (vector 239 19) (vector 184 18) (vector 101 17) (vector 124 16) (vector 152 15) (vector 206 14) (vector 96 13) (vector 23 12) (vector 163 11) (vector 61 10) (vector 27 9) (vector 196 8) (vector 247 7) (vector 151 6) (vector 154 5) (vector 202 4) (vector 207 3) (vector 20 2) (vector 61 1) (vector 10 0)))
  (setf (gethash 58 *generator-polynomials*) (vector (vector 0 58) (vector 82 57) (vector 116 56) (vector 26 55) (vector 247 54) (vector 66 53) (vector 27 52) (vector 62 51) (vector 107 50) (vector 252 49) (vector 182 48) (vector 200 47) (vector 185 46) (vector 235 45) (vector 55 44) (vector 251 43) (vector 242 42) (vector 210 41) (vector 144 40) (vector 154 39) (vector 237 38) (vector 176 37) (vector 141 36) (vector 192 35) (vector 248 34) (vector 152 33) (vector 249 32) (vector 206 31) (vector 85 30) (vector 253 29) (vector 142 28) (vector 65 27) (vector 165 26) (vector 125 25) (vector 23 24) (vector 24 23) (vector 30 22) (vector 122 21) (vector 240 20) (vector 214 19) (vector 6 18) (vector 129 17) (vector 218 16) (vector 29 15) (vector 145 14) (vector 127 13) (vector 134 12) (vector 206 11) (vector 245 10) (vector 117 9) (vector 29 8) (vector 41 7) (vector 63 6) (vector 159 5) (vector 142 4) (vector 233 3) (vector 125 2) (vector 148 1) (vector 123 0)))
  (setf (gethash 60 *generator-polynomials*) (vector (vector 0 60) (vector 107 59) (vector 140 58) (vector 26 57) (vector 12 56) (vector 9 55) (vector 141 54) (vector 243 53) (vector 197 52) (vector 226 51) (vector 197 50) (vector 219 49) (vector 45 48) (vector 211 47) (vector 101 46) (vector 219 45) (vector 120 44) (vector 28 43) (vector 181 42) (vector 127 41) (vector 6 40) (vector 100 39) (vector 247 38) (vector 2 37) (vector 205 36) (vector 198 35) (vector 57 34) (vector 115 33) (vector 219 32) (vector 101 31) (vector 109 30) (vector 160 29) (vector 82 28) (vector 37 27) (vector 38 26) (vector 238 25) (vector 49 24) (vector 160 23) (vector 209 22) (vector 121 21) (vector 86 20) (vector 11 19) (vector 124 18) (vector 30 17) (vector 181 16) (vector 84 15) (vector 25 14) (vector 194 13) (vector 87 12) (vector 65 11) (vector 102 10) (vector 190 9) (vector 220 8) (vector 70 7) (vector 27 6) (vector 209 5) (vector 16 4) (vector 89 3) (vector 7 2) (vector 33 1) (vector 240 0)))
  (setf (gethash 62 *generator-polynomials*) (vector (vector 0 62) (vector 65 61) (vector 202 60) (vector 113 59) (vector 98 58) (vector 71 57) (vector 223 56) (vector 248 55) (vector 118 54) (vector 214 53) (vector 94 52) (vector 0 51) (vector 122 50) (vector 37 49) (vector 23 48) (vector 2 47) (vector 228 46) (vector 58 45) (vector 121 44) (vector 7 43) (vector 105 42) (vector 135 41) (vector 78 40) (vector 243 39) (vector 118 38) (vector 70 37) (vector 76 36) (vector 223 35) (vector 89 34) (vector 72 33) (vector 50 32) (vector 70 31) (vector 111 30) (vector 194 29) (vector 17 28) (vector 212 27) (vector 126 26) (vector 181 25) (vector 35 24) (vector 221 23) (vector 117 22) (vector 235 21) (vector 11 20) (vector 229 19) (vector 149 18) (vector 147 17) (vector 123 16) (vector 213 15) (vector 40 14) (vector 115 13) (vector 6 12) (vector 200 11) (vector 100 10) (vector 26 9) (vector 246 8) (vector 182 7) (vector 218 6) (vector 127 5) (vector 215 4) (vector 36 3) (vector 186 2) (vector 110 1) (vector 106 0)))
  (setf (gethash 64 *generator-polynomials*) (vector (vector 0 64) (vector 45 63) (vector 51 62) (vector 175 61) (vector 9 60) (vector 7 59) (vector 158 58) (vector 159 57) (vector 49 56) (vector 68 55) (vector 119 54) (vector 92 53) (vector 123 52) (vector 177 51) (vector 204 50) (vector 187 49) (vector 254 48) (vector 200 47) (vector 78 46) (vector 141 45) (vector 149 44) (vector 119 43) (vector 26 42) (vector 127 41) (vector 53 40) (vector 160 39) (vector 93 38) (vector 199 37) (vector 212 36) (vector 29 35) (vector 24 34) (vector 145 33) (vector 156 32) (vector 208 31) (vector 150 30) (vector 218 29) (vector 209 28) (vector 4 27) (vector 216 26) (vector 91 25) (vector 47 24) (vector 184 23) (vector 146 22) (vector 47 21) (vector 140 20) (vector 195 19) (vector 195 18) (vector 125 17) (vector 242 16) (vector 238 15) (vector 63 14) (vector 99 13) (vector 108 12) (vector 140 11) (vector 230 10) (vector 242 9) (vector 31 8) (vector 204 7) (vector 11 6) (vector 178 5) (vector 243 4) (vector 217 3) (vector 156 2) (vector 213 1) (vector 231 0)))
  (setf (gethash 66 *generator-polynomials*) (vector (vector 0 66) (vector 5 65) (vector 118 64) (vector 222 63) (vector 180 62) (vector 136 61) (vector 136 60) (vector 162 59) (vector 51 58) (vector 46 57) (vector 117 56) (vector 13 55) (vector 215 54) (vector 81 53) (vector 17 52) (vector 139 51) (vector 247 50) (vector 197 49) (vector 171 48) (vector 95 47) (vector 173 46) (vector 65 45) (vector 137 44) (vector 178 43) (vector 68 42) (vector 111 41) (vector 95 40) (vector 101 39) (vector 41 38) (vector 72 37) (vector 214 36) (vector 169 35) (vector 197 34) (vector 95 33) (vector 7 32) (vector 44 31) (vector 154 30) (vector 77 29) (vector 111 28) (vector 236 27) (vector 40 26) (vector 121 25) (vector 143 24) (vector 63 23) (vector 87 22) (vector 80 21) (vector 253 20) (vector 240 19) (vector 126 18) (vector 217 17) (vector 77 16) (vector 34 15) (vector 232 14) (vector 106 13) (vector 50 12) (vector 168 11) (vector 82 10) (vector 76 9) (vector 146 8) (vector 67 7) (vector 106 6) (vector 171 5) (vector 25 4) (vector 132 3) (vector 93 2) (vector 45 1) (vector 105 0)))
  (setf (gethash 68 *generator-polynomials*) (vector (vector 0 68) (vector 247 67) (vector 159 66) (vector 223 65) (vector 33 64) (vector 224 63) (vector 93 62) (vector 77 61) (vector 70 60) (vector 90 59) (vector 160 58) (vector 32 57) (vector 254 56) (vector 43 55) (vector 150 54) (vector 84 53) (vector 101 52) (vector 190 51) (vector 205 50) (vector 133 49) (vector 52 48) (vector 60 47) (vector 202 46) (vector 165 45) (vector 220 44) (vector 203 43) (vector 151 42) (vector 93 41) (vector 84 40) (vector 15 39) (vector 84 38) (vector 253 37) (vector 173 36) (vector 160 35) (vector 89 34) (vector 227 33) (vector 52 32) (vector 199 31) (vector 97 30) (vector 95 29) (vector 231 28) (vector 52 27) (vector 177 26) (vector 41 25) (vector 125 24) (vector 137 23) (vector 241 22) (vector 166 21) (vector 225 20) (vector 118 19) (vector 2 18) (vector 54 17) (vector 32 16) (vector 82 15) (vector 215 14) (vector 175 13) (vector 198 12) (vector 43 11) (vector 238 10) (vector 235 9) (vector 27 8) (vector 101 7) (vector 184 6) (vector 127 5) (vector 3 4) (vector 5 3) (vector 8 2) (vector 163 1) (vector 238 0))))

;; ! the "main" bitstream is created (and later recreated [due to the need to block the data]) with this function
(defun create-adjustable-bit-stream ()
  "Creates and returns an empty, adjustable bit-vector."
  (make-array 0 :element-type 'bit :adjustable t :fill-pointer 0))

;; ! this function is used by the mode functions and also (very extensively) by the function CREATE-FINAL-BIT-STREAM-WITH-ERROR-CORRECTION
(defun extend-adjustable-bit-stream (bit-stream bv)
  "Extends BIT-STREAM by the contents of the passed bit-vector, BV, changing BIT-STREAM by reference. Note that each BIT of BV is \"push-extended\" onto BIT-STREAM to ensure efficiency (avoiding array-with-array concatenation). BIT-STREAM must be an adjustable bit-vector as created by the function CREATE-ADJUSTABLE-BIT-STREAM to ensure the ability to use VECTOR-PUSH-EXTEND. BV must be a bit-vector, either simple or adjustable."
  (dotimes (i (length bv))
    (let ((v (elt bv i)))
      (vector-push-extend v bit-stream))))

(defun make-bit-vector-from-binary-string (binary-string &optional bit-vector-length)
  "Returns a simple bit-vector that is made from the passed binary string. BINARY-STRING must be a string consisting exclusively of 1s and 0s; note that providing other values with signal an error. BIT-VECTOR-LENGTH indicates the final length of the resulting bit-vector, allowing the leftward end of the binary string to be additionally padded with 0s; if provided, it must be an integer greater than or equal to the length of BINARY-STRING (or else there will be an error); if not provided, the resulting bit-vector will simply match the size of the passed string."
  (let ((str-len (length binary-string)))
    (unless bit-vector-length
      (setq bit-vector-length str-len))
    (do ((bv (make-array bit-vector-length :element-type 'bit)) ;; ! notice that BV is NOT adjustable
         (si (1- str-len) (1- si))
         (bvi (1- bit-vector-length) (1- bvi)))
        ((< si 0) bv)
      (setf (aref bv bvi) (gethash (char binary-string si) *zero/one-char-to-integer*)))))

(defun trim-left-zeroes-from-bit-vector (bv)
  "Returns a copy of the passed bit-vector, BV, that has all consecutive leftmost 0s removed. BV must be a bit-vector."
  (let ((i (position 1 bv)))
    (if i
        (subseq bv i)
        (make-array 0 :element-type 'bit))))

(defun add-zeroes-to-left-end-of-bit-vector (bv count)
  "Returns a copy of the passed bit-vector, BV, that has COUNT 0s appended onto its left side. BV must be a bit-vector. COUNT must be an integer; note that passing a count of 0 will still return a copy of BV; note further, that negative values will simply be ignored and treated as 0."
  (if (> count 0)
      (concatenate 'bit-vector (make-array count :element-type 'bit) bv)
      (subseq bv 0)))

(defun add-zeroes-to-right-end-of-bit-vector (bv count)
  "Returns a copy of the passed bit-vector, BV, that has COUNT 0s appended onto its right side. BV must be a bit-vector. COUNT must be an integer; note that passing a count of 0 will still return a copy of BV; note further, that negative values will simply be ignored and treated as 0."
  (if (> count 0)
      (concatenate 'bit-vector bv (make-array count :element-type 'bit))
      (subseq bv 0)))

(defun get-copy-of-generator-polynomial (key)
  "Creates and returns a \"deep copy\" of the generator polynomial as indicated by KEY. Note that providing an invalid KEY will simply trigger an error. Note that getting a deep copy is very important in order to avoid changing the original generator polynomial by reference."
  (let ((original (gethash key *generator-polynomials*))
        (copy (make-array (1+ key)))) ;; ! each polynomial array has a length of one more than its key-value
    (dotimes (i (1+ key))
      (let ((cur-vec (aref original i)))
        (setf (aref copy i) (make-array 2 :element-type 'number :initial-contents (list (aref cur-vec 0) (aref cur-vec 1))))))
    copy))

(defun generate-values-for-galois-field ()
  "Creates and returns an array of 256 numbers where all numbers lie in the range of 1-255 (inclusive) without only one repeat: the 1 at start and end. These numbers are generated by starting with 1, saving the value, and then doubling the value, and \"Xor\"ing -- by 285 -- any value greater than 255 before saving again, repeating this process until 1 is reached again."
  (let ((arr (make-array 256 :element-type 'number))
        (tef (make-array 9 :element-type 'bit :initial-contents (make-bit-vector-from-binary-string (write-to-string 285 :base 2) 9))))
    (do ((i 0 (1+ i))
         (v 1 (* v 2)))
        ((>= i 256))
      (when (> v 255)
        (let ((b (make-array 9 :element-type 'bit :initial-contents (make-bit-vector-from-binary-string (write-to-string v :base 2) 9))))
          (setq v (parse-integer (subseq (format nil "~a" (bit-xor b tef)) 2) :radix 2))))
      (setf (aref arr i) v))
    arr))

;; this is an array of integers following the Galois Field pattern
;; ! note that because the Galois Field pattern is followed for 256 iterations, there is a 1 in the first and last elements of the *GALOIS-FIELD* array
(defparameter *galois-field* (generate-values-for-galois-field))

;; this is a hash-table that will yield up the index (in *GALOIS-FIELD*) of the value looked up
;; ! note that 1 will yield up 255 (and not 0), and 0 cannot be looked up as there is no zero in the *GALOIS-FIELD* array
(defparameter *galois-field-reverse-lookup* (make-hash-table))
;; ! note that the index of the value 1 is set to 255 due to overwriting an earlier entry (where it was set to 0); this is intentional!
;; the reverse lookup table takes the value looked for (as key), and gives back the index of that number from within the *GALOIS-FIELD* array (as value)
;; a key on the range of 1-255 will return a value on the range of 1-255 (both inclusive)
(when (= 0 (hash-table-count *galois-field-reverse-lookup*))
  (dotimes (i 256)
    (setf (gethash (aref *galois-field* i) *galois-field-reverse-lookup*) i)))

;;; these are the index constants for mode, as used for the vectors in the *BITS-BY-GROUPING* hash-table
;;; ! note that mode values are ordered according to the +DCFEC-*+ constants
;;; !!!!! note that this program currently excludes Extended Channel Interpretation (ECI) mode (and its related data), Structure Append mode, and FNC1 mode
(defparameter +mode-numeric+ 0)
(defparameter +mode-alphanumeric+ 1)
(defparameter +mode-byte+ 2) ;; !!! not implemented exactly according to specification...
(defparameter +mode-kanji+ 3) ;; !!! not implemented (due to the need to have the JIS X 0208 standardized character-set included in code)

;; ! when accessing the DCFEC table by mode, it must be offset by 2 due to the fact that the DCFEC table's inner vectors have two additional elements at front
(defun get-dcfec-index-from-mode-index (mode)
  "This function receives a mode index (as in +MODE-*+) and returns the related DCFEC constant (as in +DCFEC-*+), which is then to be used to access the appropriate element within the inner vector of an entry from the *DATA-CAPACITIES-FOR-ERROR-CORRECTION-LEVEL-BY-VERSION* hash-table. In actuality, all this function does is increment an integer by two, and this function only does as specified above so long as the associated constants are appropriately related numerically; note that no tests are done to verify whether a valid mode constant is actually passed."
  (+ 2 mode))

;; this hash-table accepts a version number and yields up an integer of value 0-2 (inclusive), indicating which "grouping" the version falls into
;; ! note that versions 1-9 (group 0), 10-26 (group 1), and 27-40 (group 2) have consistent specifications for bit counts according to mode
(defparameter *grouping-for-character-count-indicator* (make-hash-table))
(when (= 0 (hash-table-count *grouping-for-character-count-indicator*))
  (do ((version 1 (1+ version))
       (group 0))
      ((>= version 41))
    (when (or (= version 10) (= version 27))
      (incf group))
    (setf (gethash version *grouping-for-character-count-indicator*) group)))

;; this hash-table has keys in the range 0-2 (inclusive) and yields a vector appropriate to the grouping
;; this hash-table is meant to be used in combination with the above hash-table *GROUPING-FOR-CHARACTER-COUNT-INDICATOR*, accepting its yielded integer
;; ! note that the vector yielded up by this hash-table is meant to be accessed by index via the above +MODE-*+ constants
(defparameter *bits-by-grouping* (make-hash-table))
(when (= 0 (hash-table-count *bits-by-grouping*))
  (setf (gethash 0 *bits-by-grouping*) (vector 10 9 8 8))
  (setf (gethash 1 *bits-by-grouping*) (vector 12 11 16 10))
  (setf (gethash 2 *bits-by-grouping*) (vector 14 13 16 12)))

;; ! this is a convenience function for retrieving character-count details, since there are two hash-tables that need to be accessed in sequence to get this information
(defun get-bits-for-character-count-indicator (version mode)
  "Returns the number of bits (for character count) according VERSION and MODE. VERSION must be an integer valued 1-40 (inclusive). MODE must be one of the +MODE-*+ constants."
  (let ((group (gethash version *grouping-for-character-count-indicator*)))
    (aref (gethash group *bits-by-grouping*) mode)))

;; this hash-table maps characters to their appropriate decimal values (as used within +MODE-NUMERIC+)
;; ! note that only those characters found within this table are allowed to be used within numeric mode
(defparameter *numeric-mode-hash-table* (make-hash-table))
(when (= 0 (hash-table-count *numeric-mode-hash-table*))
  (setf (gethash #\0 *numeric-mode-hash-table*) 0)
  (setf (gethash #\1 *numeric-mode-hash-table*) 1)
  (setf (gethash #\2 *numeric-mode-hash-table*) 2)
  (setf (gethash #\3 *numeric-mode-hash-table*) 3)
  (setf (gethash #\4 *numeric-mode-hash-table*) 4)
  (setf (gethash #\5 *numeric-mode-hash-table*) 5)
  (setf (gethash #\6 *numeric-mode-hash-table*) 6)
  (setf (gethash #\7 *numeric-mode-hash-table*) 7)
  (setf (gethash #\8 *numeric-mode-hash-table*) 8)
  (setf (gethash #\9 *numeric-mode-hash-table*) 9))

;; this hash-table maps characters to their appropriate decimal values (as used within +MODE-ALPHANUMERIC+)
;; ! note that only those characters found within this table are allowed to be used within alphanumeric mode
(defparameter *alphanumeric-mode-hash-table* (make-hash-table))
(when (= 0 (hash-table-count *alphanumeric-mode-hash-table*))
  (setf (gethash #\0 *alphanumeric-mode-hash-table*) 0)
  (setf (gethash #\1 *alphanumeric-mode-hash-table*) 1)
  (setf (gethash #\2 *alphanumeric-mode-hash-table*) 2)
  (setf (gethash #\3 *alphanumeric-mode-hash-table*) 3)
  (setf (gethash #\4 *alphanumeric-mode-hash-table*) 4)
  (setf (gethash #\5 *alphanumeric-mode-hash-table*) 5)
  (setf (gethash #\6 *alphanumeric-mode-hash-table*) 6)
  (setf (gethash #\7 *alphanumeric-mode-hash-table*) 7)
  (setf (gethash #\8 *alphanumeric-mode-hash-table*) 8)
  (setf (gethash #\9 *alphanumeric-mode-hash-table*) 9)
  (setf (gethash #\a *alphanumeric-mode-hash-table*) 10)
  (setf (gethash #\b *alphanumeric-mode-hash-table*) 11)
  (setf (gethash #\c *alphanumeric-mode-hash-table*) 12)
  (setf (gethash #\d *alphanumeric-mode-hash-table*) 13)
  (setf (gethash #\e *alphanumeric-mode-hash-table*) 14)
  (setf (gethash #\f *alphanumeric-mode-hash-table*) 15)
  (setf (gethash #\g *alphanumeric-mode-hash-table*) 16)
  (setf (gethash #\h *alphanumeric-mode-hash-table*) 17)
  (setf (gethash #\i *alphanumeric-mode-hash-table*) 18)
  (setf (gethash #\j *alphanumeric-mode-hash-table*) 19)
  (setf (gethash #\k *alphanumeric-mode-hash-table*) 20)
  (setf (gethash #\l *alphanumeric-mode-hash-table*) 21)
  (setf (gethash #\m *alphanumeric-mode-hash-table*) 22)
  (setf (gethash #\n *alphanumeric-mode-hash-table*) 23)
  (setf (gethash #\o *alphanumeric-mode-hash-table*) 24)
  (setf (gethash #\p *alphanumeric-mode-hash-table*) 25)
  (setf (gethash #\q *alphanumeric-mode-hash-table*) 26)
  (setf (gethash #\r *alphanumeric-mode-hash-table*) 27)
  (setf (gethash #\s *alphanumeric-mode-hash-table*) 28)
  (setf (gethash #\t *alphanumeric-mode-hash-table*) 29)
  (setf (gethash #\u *alphanumeric-mode-hash-table*) 30)
  (setf (gethash #\v *alphanumeric-mode-hash-table*) 31)
  (setf (gethash #\w *alphanumeric-mode-hash-table*) 32)
  (setf (gethash #\x *alphanumeric-mode-hash-table*) 33)
  (setf (gethash #\y *alphanumeric-mode-hash-table*) 34)
  (setf (gethash #\z *alphanumeric-mode-hash-table*) 35)
  (setf (gethash #\  *alphanumeric-mode-hash-table*) 36)
  (setf (gethash #\$ *alphanumeric-mode-hash-table*) 37)
  (setf (gethash #\% *alphanumeric-mode-hash-table*) 38)
  (setf (gethash #\* *alphanumeric-mode-hash-table*) 39)
  (setf (gethash #\+ *alphanumeric-mode-hash-table*) 40)
  (setf (gethash #\- *alphanumeric-mode-hash-table*) 41)
  (setf (gethash #\. *alphanumeric-mode-hash-table*) 42)
  (setf (gethash #\/ *alphanumeric-mode-hash-table*) 43)
  (setf (gethash #\: *alphanumeric-mode-hash-table*) 44))

;; ! note that the different conditionals within this function follow the patterns used by the very much related function DETERMINE-BEST-MODE
(defun check-that-mode-is-valid (text mode)
  "Triggers an error if MODE is invalid for the provided TEXT."
  (if (not (integerp mode))
      (error "MODE must be an integer!~%Please provide one of the valid +MODE-*+ constants!")
      (cond
        ((= mode +mode-numeric+)
         (dotimes (i (length text))
           (let ((c (char text i)))
             (unless (multiple-value-bind (value present-p) (gethash c *numeric-mode-hash-table*) (declare (ignore value)) present-p) ;; when character not in table...
               (error (format nil "The character ~s is not a valid character for numeric-mode!~%It is generally recommended that MODE be determined automatically." c))))))
        ((= mode +mode-alphanumeric+)
         (let ((text (string-downcase text))) ;; ! notice that text must be downcased for alphanumeric-mode
           (dotimes (i (length text))
             (let ((c (char text i)))
               (unless (multiple-value-bind (value present-p) (gethash c *alphanumeric-mode-hash-table*) (declare (ignore value)) present-p) ;; when character not in table...
                 (error (format nil "The character ~s is not a valid character for alphanumeric-mode!~%It is generally recommended that MODE be determined automatically." c)))))))
        ((= mode +mode-byte+)
         (dotimes (i (length text))
           (let ((c (char text i)))
             (when (> (char-code c) 255) ;; ! note that the binary "11111111" is 255 in decimal
               (error (format nil "The character ~s cannot be encoded in any of the currently available modes... Sorry!~%Currently this program can only encode text that consists exclusively of characters that are available on a standard US-based keyboard." c))))))
        (t
         (error (format nil "Invalid MODE was provided: ~a~%MODE must be an integer that matches one of the currently implemented +MODE-*+ constants!" mode))))))

;; !!!!! note that this function does not include Kanji-mode (or other more complex modes), and considers byte-mode in accordance with my custom approach
(defun determine-best-mode (text)
  "Determines the best mode to use given the passed TEXT, returning the corresponding +MODE-*+ constant. The \"best\" mode is one that can accommodate the provided TEXT while yielding the least complex QR-symbol."
  (let ((hash-table-restricted-modes (list (cons t *numeric-mode-hash-table*) (cons t *alphanumeric-mode-hash-table*)))
        (text-len (length text))
        (text (string-downcase text))) ;; ! downcasing will not affect numeric mode, but will be required for alphanumeric mode
    ;; for every character in TEXT...
    (dotimes (i text-len)
      (let ((c (char text i)))
        ;; for every hash-table restricted mode...
        (dolist (cns hash-table-restricted-modes) ;; ! note that CNS is a CONS
          (when (car cns) ;; when the mode is still viable for TEXT so far
            ;; check that it is viable for current character (C)
            (unless (multiple-value-bind (value present-p) (gethash c (cdr cns)) (declare (ignore value)) present-p) ;; when character not in table...
              ;; indicate that this mode is NOT viable for TEXT
              (setf (car cns) nil))))
        ;; verifying that C is valid for byte-mode
        (when (> (char-code c) 255) ;; ! note that the binary "11111111" is 255 in decimal
          (error (format nil "The character ~s cannot be encoded in any of the currently available modes... Sorry!~%Currently this program can only encode text that consists exclusively of characters that are available on a standard US-based keyboard." c)))))
    ;; returning the appropriate mode
    (cond
      ((car (first hash-table-restricted-modes))
       +mode-numeric+)
      ((car (second hash-table-restricted-modes))
       +mode-alphanumeric+)
      (t
       +mode-byte+))))

;; this hash-table accepts bit masks as strings, yielding the appropriate related function
;; for these masking functions, whenever true is returned, the related module must change to the opposite color; whenever false, the module is unchanged
(defparameter *mask-reference-to-function* (make-hash-table :test 'equal))
(when (= 0 (hash-table-count *mask-reference-to-function*))
  (setf (gethash "000" *mask-reference-to-function*) #'(lambda (d r) (= 0 (mod (+ d r) 2))))
  (setf (gethash "001" *mask-reference-to-function*) #'(lambda (d r) (declare (ignore r)) (= 0 (mod d 2))))
  (setf (gethash "010" *mask-reference-to-function*) #'(lambda (d r) (declare (ignore d)) (= 0 (mod r 3))))
  (setf (gethash "011" *mask-reference-to-function*) #'(lambda (d r) (= 0 (mod (+ d r) 3))))
  (setf (gethash "100" *mask-reference-to-function*) #'(lambda (d r) (= 0 (mod (+ (floor (/ d 2)) (floor (/ r 3))) 2))))
  (setf (gethash "101" *mask-reference-to-function*) #'(lambda (d r) (= 0 (+ (mod (* d r) 2) (mod (* d r) 3)))))
  (setf (gethash "110" *mask-reference-to-function*) #'(lambda (d r) (= 0 (mod (+ (mod (* d r) 2) (mod (* d r) 3)) 2))))
  (setf (gethash "111" *mask-reference-to-function*) #'(lambda (d r) (= 0 (mod (+ (mod (* d r) 3) (mod (+ d r) 2)) 2)))))

;; this list holds all unique mask strings, and is used primarily (by iteration) to test all masks to find the best mask for the data
;; ! note that ordering of masks in this list follows the top-to-bottom setting of the above hash-table *MASK-REFERENCE-TO-FUNCTION*
(defparameter *all-mask-references* nil)
(unless *all-mask-references*
  (maphash #'(lambda (mask f) (declare (ignore f)) (push mask *all-mask-references*)) *mask-reference-to-function*)
  (setf *all-mask-references* (reverse *all-mask-references*)))

(defun check-that-mask-is-valid (mask)
  "Triggers an error if MASK is not one of the eight 3-digit binary sequences."
  (unless (multiple-value-bind (function present-p) (gethash mask *mask-reference-to-function*) (declare (ignore function)) present-p) ;; when string not in table...
    (error (format nil "The value ~a is not a valid MASK!~%It is recommended that mask be determined automatically, but, if you insist on providing one manually, please provide one from the following list:~%~a" mask *all-mask-references*))))

(defun determine-best-version (mode error-correction-level text)
  "Returns a integer that indicates the smallest usable QR-symbol version according to the MODE, ERROR-CORRECTION-LEVEL, and TEXT. MODE must be one of the +MODE-*+ constants. ERROR-CORRECTION-LEVEL must be one of the +ECL-*+ constants. TEXT must be a string; note that TEXT must not have characters that are disallowed by type of MODE."
  (let ((text-len (length text)))
    ;; manually iterating through version data, starting at version 1, to check whether that version (with appropriate mode, and ECL) can accommodate size of text
    (do ((version 1 (1+ version)))
        ((> version 40) (error "The string TEXT is too large to be encoded!"))
      (let ((max-characters (aref (aref (gethash version *data-capacities-for-error-correction-level-by-version*) error-correction-level) (get-dcfec-index-from-mode-index mode))))
        (when (<= text-len max-characters)
          (return version))))))

(defun create-2d-array-for-qr-symbol (version)
  "Creates and returns a custom 2d array, sized according to VERSION, that will be used to represent the QR-symbol. VERSION must be an integer valued 1-40 (inclusive). Note that the generated array initially has and empty CONS at every element -- as in (CONS NIL NIL) or '(NIL . NIL) -- and follows the format (CONS <code> <value>), where code is one of the +MC-*+ constants, and where value is +BLACK+ or +WHITE+."
  (let ((size (aref (gethash version *data-capacities-by-version*) +width/height+))) ;; ! note that all QR-symbols in this specification are perfect squares
    (make-array `(,size ,size) :element-type 'cons :initial-element (cons nil nil))))

;; ! note that the ability to handle illegal indexes is only important for adding the position detection patterns, whose outermost white squares extend into the "quiet zone"
(defun add-to-qr-symbol-flexibly (qr consed-coords code value &key (d-adjustment 0) (r-adjustment 0))
  "Modifies QR at the element indicated by CONSED-COORDS so that the related element is set according to CODE and VALUE, but only so long as the provided coordinates are valid according to the height and width of the QR-symbol; note that if an invalid coordinate is provided, this function will do nothing and will signal neither error nor warning. QR must be a two-dimensional array as created with the function CREATE-2D-ARRAY-FOR-QR-SYMBOL. CONSED-COORDS must be a CONS of two integers, where the CAR position is the down-coordinate and the CDR position is the right-coordinate. CODE must be one of the +MC-*+ constants. VALUE must be either +BLACK+ or +WHITE+. D-ADJUSTMENT and R-ADJUSTMENT must both be an integers (and may be negative); each will be used to adjust CONSED-COORDS accordingly such that the resulting D-R coordinate may no longer be valid for the 2d array. Finally, note that this function is used to add module information according to certain patterns, and is used to set the position detection patterns and alignment patterns within the QR-symbol."
  (let ((d (+ (car consed-coords) d-adjustment))
        (r (+ (cdr consed-coords) r-adjustment))
        (height (first (array-dimensions qr)))
        (width (second (array-dimensions qr))))
    (when (and (and (>= d 0) (< d height)) (and (>= r 0) (< r width))) ;; when D and R are both valid
      (setf (aref qr d r) (cons code value)))))

(defun add-to-qr-symbol (qr d r code value)
  "Modifies QR at the element indicated by the coordinates D and R so that the related element is set according to CODE and VALUE. QR must be a two-dimensional array as created with the function CREATE-2D-ARRAY-FOR-QR-SYMBOL. D and R must be valid integer coordinates according to the actual height and width of QR. CODE must be one of the +MC-*+ constants. VALUE must be either +BLACK+ or +WHITE+."
  (setf (aref qr d r) (cons code value)))

;; ! for clarity, the following function follows the below pattern when STEPS is 2, according to alphabetical order (note that the starting 'a' is at center):
;; ... joqsu
;; ... kbegv
;; ... lcahw
;; ... mdfix
;; ... nprty
(defun apply-alternating-outward-pattern-to-qr-symbol (qr consed-coords steps &key (code -1) (alternate-immediately t))
  "Modifies QR such that every element within a rippling pattern, originating at CONSED-COORDS, is alternatingly set, STEPS times outside of origin, to +WHITE+ or +BLACK+ along with a single provided CODE, such that the origin is always colored +BLACK+. QR must be a two-dimensional array as created with the function CREATE-2D-ARRAY-FOR-QR-SYMBOL. CONSED-COORDS must be a CONS of two integers, where the CAR position is the down-coordinate and the CDR position is the right-coordinate. STEPS should be a positive integer, and it indicates by its value how many \"circles\" around the origin the ripple-effect will alter; note that the origin will be changed regardless of the value of STEPS. CODE should be one of the +MC-*+ constants. ALTERNATE-IMMEDIATELY is a boolean; set it to NIL to prevent the first color-change such that the first ripple will also (like the origin) be +BLACK+ (as in \"position detection patterns\")."
  (let ((pattern (make-array 2 :element-type 'number))
        (pattern-i 0))
    (setf (aref pattern 0) +black+)
    (setf (aref pattern 1) +white+)
    (add-to-qr-symbol-flexibly qr consed-coords code (elt pattern pattern-i)) ;; setting origin
    (when alternate-immediately
      (incf pattern-i))
    (when (>= steps 1)
      ;; advancing a single step each iteration, and alternating color each step...
      (do ((step 1 (1+ step))
           (pattern-i pattern-i (mod (1+ pattern-i) 2)))
          ((> step steps))
        ;; determining the size of each new "ripple" and iterating that many times (so as to step left-to-right making marks)...
        (do ((i 0 (1+ i))
             (square-size (1+ (* 2 step))) ;; at each step is the "length" of the square-like ripple's side
             (left-i (- (cdr consed-coords) step))
             (right-i (+ (cdr consed-coords) step)))
            ((>= i square-size))
          ;; creating a new variable (NEGPOS) to modify the down-coordinate so that top and (then) bottom of ripple-edges may be marked (again left-to-right)...
          (do ((counter 0 (1+ counter))
               (negpos -1 1)) ;; ! NEGPOS, meaning NEGative 1, and then, next, POSitive one
              ((>= counter 2))
            (let ((coord (cons (+ (* negpos step) (car consed-coords)) (+ left-i i)))) ;; ! when NEGPOS is negative COORD is at top edge of ripple; but at bottom otherwise
              (add-to-qr-symbol-flexibly qr coord code (elt pattern pattern-i))
              (when (and (or (= (cdr coord) left-i) (= (cdr coord) right-i)) (= -1 negpos)) ;; when on left or right edge of ripple and first iteration of COUNTER loop
                ;; mark the left or right edge of ripple, top-to-bottom, skipping top and bottom corners since they are handled by above call to ADD-TO-QR-FLEXIBLY
                (dotimes (x (- square-size 2))
                  (add-to-qr-symbol-flexibly qr coord code (elt pattern pattern-i) :d-adjustment (1+ x)))))))))))

(defun apply-alternating-line-pattern-to-qr-symbol (qr index &key (code -1) (horizontal t))
  "Modifies QR such that the entire row/column (according to HORIZONTAL) at INDEX will be set with an alternating +BLACK+ and +WHITE+ pattern, starting with +BLACK+, with CODE being applied uniformly across line; note that every position along this line that already has a code set will be ignored (to avoid overwriting the data of any already applied patterns, such as \"position detection patterns\" and \"alignment patterns\"). This function is used exclusively to add in the \"timing patterns\". QR must be a two-dimensional array as created with the function CREATE-2D-ARRAY-FOR-QR-SYMBOL. INDEX must be a non-negative integer, and indicates which row/column to apply the pattern to. CODE should be one of the +MC-*+ constants. When HORIZONTAL is T, then INDEX is a down-coordinate that indicates the related row; when HORIZONTAL is NIL, then INDEX is a right-coordinate that indicates the related column."
  (let ((size (first (array-dimensions qr)))
        (pattern (make-array 2 :element-type 'number)))
    (setf (aref pattern 0) +black+)
    (setf (aref pattern 1) +white+)
    (if horizontal
        ;; THEN, adding in horizontal pattern...
        (dotimes (x size)
          (unless (car (aref qr index x))
            (add-to-qr-symbol qr index x code (elt pattern (mod x 2)))))
        ;; ELSE, adding in vertical pattern...
        (dotimes (x size)
          (unless (car (aref qr x index))
            (add-to-qr-symbol qr x index code (elt pattern (mod x 2))))))))

;; ! note that about half of the white outermost ripple of every "position detection pattern" overlaps with the "quiet zone" which lies outside of any valid ...
;; ... QR-symbol index; this is why 4 is passed as STEPS to APPLY-ALTERNATING-OUTWARD-PATTERN-TO-QR-SYMBOL (yielding 4 ripples and an origin) when moving only ...
;; ... 4 positions inward from edge; this is also why the function ADD-TO-QR-SYMBOL-FLEXIBLY checks for valid indexes, ignoring any that are out of bounds
(defun add-position-detection-patterns (qr)
  "Modifies QR so as to add the three standard \"position detection patterns\" to the QR-symbol. Note that the location of these patterns is the same -- in relation to the edges of the QR-symbol -- throughout all versions. This function is meant to be called on an essentially empty qr-symbol, and the \"position detection patterns\" must be added before any other patterns. QR must be a two-dimensional array as created with the function CREATE-2D-ARRAY-FOR-QR-SYMBOL."
  (apply-alternating-outward-pattern-to-qr-symbol qr (cons 3 3) 4 :code +mc-position-detection-pattern+ :alternate-immediately nil)
  (let ((size (first (array-dimensions qr))))
    (let ((i (- size 4))) ;; getting appropriate distance from right and bottom edges
      (apply-alternating-outward-pattern-to-qr-symbol qr (cons 3 i) 4 :code +mc-position-detection-pattern+ :alternate-immediately nil)
      (apply-alternating-outward-pattern-to-qr-symbol qr (cons i 3) 4 :code +mc-position-detection-pattern+ :alternate-immediately nil))))

(defun add-alignment-patterns (qr version)
  "Modifies QR so as to add all needed \"alignment patterns\" according to the VERSION number. Note that this function adds \"alignment patterns\" according to their possible placement as indicated by the data in the table *ALIGNMENT-PATTERN-CENTERS-BY-VERSION*, simply adding the \"alignment pattern\" where there is not already a \"position detection pattern\" (or anything else) blocking its placement; thus, this function is only to be called on QR immediately after the calling of the function ADD-POSITION-DETECTION-PATTERNS. QR must be a two-dimensional array as created with the function CREATE-2D-ARRAY-FOR-QR-SYMBOL. VERSION must be an integer valued 1-40 (inclusive)."
  (let ((indexes (cdr (gethash version *alignment-pattern-centers-by-version*))))
    (dolist (i indexes)
      (dolist (j indexes)
        (unless (car (aref qr i j)) ;; when the module-code is NOT already set at this position
          (apply-alternating-outward-pattern-to-qr-symbol qr (cons i j) 2 :code +mc-alignment-pattern+ :alternate-immediately t))))))

(defun add-timing-patterns (qr)
  "Modifies QR so as to add both the horizontal and vertical \"timing patterns\"; note that the locations of the timing patterns are consistent -- in relation to the edges of the qr-symbol -- throughout all versions. This function is only to be called on QR immediately after the calling of both ADD-POSITION-DETECTION-PATTERNS and ADD-ALIGNMENT-PATTERNS, to ensure the proper setup of the qr-symbol; this function will not cause the overwriting of those pattern's modules. QR must be a two-dimensional array as created with the function CREATE-2D-ARRAY-FOR-QR-SYMBOL."
  (apply-alternating-line-pattern-to-qr-symbol qr +timing-pattern-index+ :code +mc-timing-pattern+ :horizontal t)
  (apply-alternating-line-pattern-to-qr-symbol qr +timing-pattern-index+ :code +mc-timing-pattern+ :horizontal nil))

(defun add-format-information (qr &key (bit-stream nil) (added-to-copy nil))
  "Modifies QR (or ADDED-TO-COPY) so as to add all \"format information\" patterns; note that the \"format information\" is a 15-bit masked sequence that occurs twice in the qr symbol, wrapping around the 3 \"position detection patterns\", giving the error-correction-level (ecl) used and the mask that was applied to the QR-symbol's data. This function must be called after the setting of \"timing patterns\" (because this function intends to step over \"timing patterns\" without knowing exactly where they are). This function must also be called once before the setting of any data modules in such a way as to mark all \"format information\" modules with their code but not any value so that other processes know that the affected modules are reserved for formatting information; after all other modules have been set, it must then be called again -- repeatedly, and each time on a new copy -- to help in determining the best mask; once the best mask is determined this function is to be called one last time to finally set the relevant formatting information. QR must be a two-dimensional array as created with the function CREATE-2D-ARRAY-FOR-QR-SYMBOL. BIT-STREAM, when NIL, causes module codes only to be set; when BIT-STREAM is non-NIL, however, it must be a BIT-ARRAY as prepared by the function CREATE-FORMAT-BIT-STREAM. ADDED-TO-COPY, when NIL, indicates that the original QR-symbol (QR) is to be modified; when non-NIL ADDED-TO-COPY must be a simplified QR-symbol as prepared by the function APPLY-MASK (with MAKE-SIMPLE-COPY set to T), and when thus provided all changes will instead be made to this simplified copy according to its different structure."
  (let ((size (first (array-dimensions qr))))
    ;; setting BIT-STREAM to an empty default when it was not provided...
    (unless bit-stream
      (when added-to-copy
        (error "Cannot have BIT-STREAM set to NIL when adding data to a copy."))
      ;; setting the constant black module
      ;; ! note that the top-left format information section includes 15 available positions, whereas the combined total of positions between ...
      ;; ... the top-right and bottom-left format information sections (which are to be taken together) equals 16; to account for this difference ...
      ;; ... there is a single module which has no significance and is always +BLACK+; the below call sets this module manually
      (add-to-qr-symbol qr (- size +format-information-index+) +format-information-index+ +mc-format-information-placeholder+ +black+)
      (setf bit-stream (make-array +format-information-length+ :initial-element nil)))
    ;; setting upper-left format information
    ;; ! note that alignment patterns interrupt the continuity of the format information in the upper-left section
    (let ((index 0) ;; index of bit-stream; note that index 0 refers to the most significant bit
          (d +format-information-index+)
          (r 0))
      ;; setting the horizontal part (left-to-right)
      ;; ! note that this "do" loop is similar to the 3 "do" loops that follow it; the larger comments in this section apply to similar parts of below "do" loops
      (do ()
          ((>= r +format-information-index+))
        (let ((code (car (aref qr d r))))
          ;; when no code has been set (implying that this is the first time this function is called) or when the code indicates format information (implying ...
          ;; ... that this function has been called before)...
          (when (or (not code) (= code +mc-format-information+))
            (if added-to-copy
                (setf (aref added-to-copy d r) (aref bit-stream index)) ;; THEN, mark the provided simplified qr-symbol with the appropriate value in the passed BIT-STREAM
                (add-to-qr-symbol qr d r +mc-format-information+ (aref bit-stream index))) ;; ELSE, set the module-code accordingly and set value via BIT-STREAM
            (incf index)))
        (incf r))
      ;; setting the vertical part (bottom-to-top)
      (do ()
          ((< d 0))
        (let ((code (car (aref qr d r))))
          (when (or (not code) (= code +mc-format-information+))
            (if added-to-copy
                (setf (aref added-to-copy d r) (aref bit-stream index))
                (add-to-qr-symbol qr d r +mc-format-information+ (aref bit-stream index)))
            (incf index)))
        (decf d)))
    ;; setting the lower-left and upper-right format information
    ;; ! again, note that these two sections are to be taken together to get the full 15 bit sequence
    (let ((index 0) ;; index of bit-stream; note that index 0 is the most significant bit
          (d (1- size))
          (r +format-information-index+))
      ;; setting the vertical lower-left part (bottom-to-top)
      ;; ! note that the previously set "constant black module" is in this section, but it is not reached by "do" loop due to value of END-INDEX
      (do ((end-index (- size +format-information-index+)))
          ((<= d end-index))
        (let ((code (car (aref qr d r))))
          (when (or (not code) (= code +mc-format-information+))
            (if added-to-copy
                (setf (aref added-to-copy d r) (aref bit-stream index))
                (add-to-qr-symbol qr d r +mc-format-information+ (aref bit-stream index)))
            (incf index)))
        (decf d))
      ;; setting the horizontal upper-right part (left-to-right)
      (do ((d +format-information-index+)
           (r (- size +format-information-index+)))
          ((>= r size))
        (let ((code (car (aref qr d r))))
          (when (or (not code) (= code +mc-format-information+))
            (if added-to-copy
                (setf (aref added-to-copy d r) (aref bit-stream index))
                (add-to-qr-symbol qr d r +mc-format-information+ (aref bit-stream index)))
            (incf index)))
        (incf r)))))

(defun add-version-information (qr version &optional bit-stream)
  "Modifies QR so as to add all \"version information\" patterns; note that the \"version information\" is a masked 18-bit sequence that occurs twice in the QR-symbol, as 3 columns or rows of 6-bit sequences near both the top-right and bottom-left \"position detection patterns\", respectively, giving the version number of the QR-symbol. This function must be called before the setting of any data modules. QR must be a two-dimensional array as created with the function CREATE-2D-ARRAY-FOR-QR-SYMBOL. VERSION must be an integer valued 1-40 (inclusive). BIT-STREAM, when NIL or not provided, causes module codes only to be set; when BIT-STREAM is provided, however, it must be a BIT-ARRAY as prepared by the function CREATE-VERSION-BIT-STREAM."
  (when (or (<= version 0) (> version +version-count+))
    (error (format nil "The passed VERSION is invalid! Only integers 1-~d (inclusive) are allowed." +VERSION-COUNT+)))
  (when (>= version +first-version-with-version-information+)
    (let ((size (first (array-dimensions qr))))
      (unless bit-stream
        (setf bit-stream (make-array +version-information-length+ :initial-element nil)))
      (let ((index 0)
            (d (1- (car +version-information-consed-values+)))
            (r (- size +version-information-distance+)))
        ;; adding version information...
        ;; ! note that version information is read, at bottom-left, top-to-bottom down the stack of three sequentially left-to-right across all stacks ...
        ;; and at top-right, symmetrically, so as to be read, left-to-right across the row of three sequentially top-to-bottom across all rows
        (dotimes (d-adj (car +version-information-consed-values+))
          (dotimes (r-adj (cdr +version-information-consed-values+))
            ;; ! note that the positions of the distinct "version information" groups are symmetrical when viewed diagonally, and this section ...
            ;; ... fills them both out in tandem by swapping the D-R coordinates within two, otherwise the same, calls to ADD-TO-QR-SYMBOL
            (add-to-qr-symbol qr (- d d-adj) (- r r-adj) +mc-version-information+ (aref bit-stream index))
            (add-to-qr-symbol qr (- r r-adj) (- d d-adj) +mc-version-information+ (aref bit-stream index))
            (incf index)))))))

(defun create-bit-stream-in-numeric-mode (version text &key (verbose nil))
  "Creates and returns an adjustable bit-stream according to VERSION and TEXT in numeric mode; note that numeric mode is only to be used when the TEXT consists of nothing but numbers. The returned numeric-mode bit-stream consists of a 4-bit sequence to represent the mode, then an n-bit (10, 12, or 14) sequence -- according to VERSION -- that represents the count of characters, and finally a collection of 10-bit sequences where each sequence encodes a grouping of 3 numbers (although the last grouping may be smaller than 3 and then has a length of 4 or 7 bits). VERSION must be an integer valued 1-40 (inclusive). TEXT must be a string consisting of numeric characters exclusively; this function assumes that TEXT is valid."
  (let ((mode "0001")
        (step 3) ;; in numeric-mode, characters are considered 3 at a time
        (char-count (length text))
        (character-count-bits (get-bits-for-character-count-indicator version +mode-numeric+)) ;; the number of bits used to represent the character count
        (bit-count 10) ;; each grouping of 3 numbers will be represented with a 10-bit sequence
        (remainder-to-value-ht (make-hash-table))) ;; used to get the size of the bit-sequence when the numeric-character-group is of a size less than 3
    ;; preparing hash-table
    (setf (gethash 0 remainder-to-value-ht) 0) ;; ! note that the 0 entry is only necessary for setting FULL-BIT-COUNT
    (setf (gethash 1 remainder-to-value-ht) 4)
    (setf (gethash 2 remainder-to-value-ht) 7)
    (let ((full-bit-count (+ 4 character-count-bits (* bit-count (floor (/ char-count 3))) (gethash (mod char-count 3) remainder-to-value-ht)))
          (bit-stream (create-adjustable-bit-stream)))
      ;; adding the mode (as binary) to the bit-stream
      (extend-adjustable-bit-stream bit-stream (make-bit-vector-from-binary-string mode))
      ;; adding the character count (as binary) to the bit-stream
      (extend-adjustable-bit-stream bit-stream (make-bit-vector-from-binary-string (write-to-string char-count :base 2) character-count-bits))
      (when verbose
        (format t "Creating BIT-STREAM in Numeric Mode for version ~d QR-symbol...~%~%~s <-- initial BIT-STREAM without any data codewords...~%~s <-- first 4 bits (mode codeword)~%    ~s <-- next ~d bits indicating count of characters in text (~d)~%~%Generating data codewords...~%~%" version bit-stream (subseq bit-stream 0 4) (subseq bit-stream 4) character-count-bits char-count))
      ;; adding each of the number groupings (as binary) to the bit-stream
      (do ((index 0 (+ index step)))
          ((>= index char-count))
        ;; get the next 3 (or fewer) numeric-characters and represent them in binary of the appropriate length
        (let* ((numeric-characters (subseq text index (if (> (+ index step) char-count) char-count (+ index step))))
               (numeric-character-count (length numeric-characters))
               (bits-added-this-iteration (if (< numeric-character-count step) (gethash numeric-character-count remainder-to-value-ht) bit-count))
               (binary (write-to-string (parse-integer numeric-characters) :base 2))
               (bv (make-bit-vector-from-binary-string binary bits-added-this-iteration)))
          (when verbose
            (format t "~s yields ~a~%" numeric-characters bv))
          (extend-adjustable-bit-stream bit-stream bv)))
      (when verbose
        (format t "~%BIT-STREAM so far, including data codewords, by end of function CREATE-BIT-STREAM-IN-NUMERIC-MODE:~%~a~%~%" bit-stream))
      (when (/= (length bit-stream) full-bit-count)
        (error (format nil "The length of BIT-STREAM is not equal to the expected length as indicated by FULL-BIT-COUNT!~%Actual length: ~d~%Expected: ~d~%" (length bit-stream) full-bit-count)))
      bit-stream)))

(defun create-bit-stream-in-alphanumeric-mode (version text &key (verbose nil))
  "Creates and returns and adjustable bit-stream according to VERSION and TEXT in alphanumeric mode; this mode is to be used when TEXT consists of English letters, numbers, and basic punctuation. The returned alphanumeric-mode bit-stream consists of a 4-bit sequence to represent the mode, then an n-bit (9, 11, or 13) sequence -- according to VERSION -- that represents the count of characters, and finally a collection of 11-bit sequences where each sequence encodes a grouping of 2 characters (although the last grouping may consist of only one character, in which case it will be represented with a 6-bit sequence); note that the value of each character is determined by referencing the hash-table *ALPHANUMERIC-MODE-HASH-TABLE*. To get the 11-bit sequence that encodes two characters, the first hash-table-derived value is multiplied by 45 and then added to the second, with the result being converted to binary (of 11 bits); for a final unmatched character, though, its value is simply converted to a 6-bit binary string. VERSION must be an integer valued 1-40 (inclusive). TEXT must be a string consisting exclusively of characters existing in the hash-table *ALPHANUMERIC-MODE-HASH-TABLE*."
  (let ((mode "0010")
        (char-count (length text))
        (multiple (hash-table-count *alphanumeric-mode-hash-table*)) ;; this collects the value 45, which is an important value in calculating each 11-bit sequence
        (character-count-bits (get-bits-for-character-count-indicator version +mode-alphanumeric+)) ;; the number of bits used to represent character-count (9, 11, or 13)
        (bit-count 11) ;; each grouping of 2 characters will be represented with an 11-bit sequence
        (short-bit-count 6)) ;; when the grouping has only 1 character the bit-sequence has a length of 6
    (let ((full-bit-count (+ 4 character-count-bits (* bit-count (floor (/ char-count 2))) (* 6 (mod char-count 2))))
          (bit-stream (create-adjustable-bit-stream)))
      ;; adding the mode (as binary) to the bit-stream
      (extend-adjustable-bit-stream bit-stream (make-bit-vector-from-binary-string mode))
      ;; adding the character count (as binary) to the bit-stream
      (extend-adjustable-bit-stream bit-stream (make-bit-vector-from-binary-string (write-to-string char-count :base 2) character-count-bits))
      (when verbose
        (format t "Creating BIT-STREAM in Alphanumeric Mode for version ~d QR-symbol...~%~%~s <-- initial BIT-STREAM without any data codewords...~%~s <-- first 4 bits (mode codeword)~%    ~s <-- next ~d bits indicating count of characters in text (~d)~%~%Generating data codewords...~%~%" version bit-stream (subseq bit-stream 0 4) (subseq bit-stream 4) character-count-bits char-count))
      ;; adding each of the character groupings (as binary) to the bit-stream
      ;; ! note that the "finally" part of the loop handles the ungrouped character (when it exists)
      (do ((i 0 (1+ i))
           (grouped-values nil) ;; a list that holds up to 2 grouped character-values
           (c 0)) ;; keeps track of the length of the list grouped-values
          ((>= i char-count) (when (= c 1)
                               (let ((bv (make-bit-vector-from-binary-string (write-to-string (first grouped-values) :base 2) short-bit-count)))
                                 (when verbose
                                   (format t "~s  yields ~a~%" (make-array 1 :element-type 'character :initial-contents (list (char text (1- i)))) bv))
                                 (extend-adjustable-bit-stream bit-stream bv))))
        (push (gethash (char text i) *alphanumeric-mode-hash-table*) grouped-values)
        (incf c)
        (when (= c 2)
          (setq c 0)
          ;; preparing BV, which is the bit-vector that encodes the two groups characters
          ;; ! note that because GROUPED-VALUES is a list that was filled with pushes, its ordering of values is reversed
          (let ((bv (make-bit-vector-from-binary-string (write-to-string (+ (first grouped-values) (* multiple (second grouped-values))) :base 2) bit-count)))
            (when verbose
              (format t "~s yields ~a~%" (make-array 2 :element-type 'character :initial-contents (list (char text (1- i)) (char text i))) bv))
            (setq grouped-values nil)
            (extend-adjustable-bit-stream bit-stream bv))))
      (when verbose
        (format t "~%BIT-STREAM so far, including data codewords, by end of function CREATE-BIT-STREAM-IN-ALPHANUMERIC-MODE:~%~a~%~%" bit-stream))
      (when (/= (length bit-stream) full-bit-count)
        (error (format nil "The length of BIT-STREAM is not equal to the expected length as indicated by FULL-BIT-COUNT!~%Actual length: ~d~%Expected: ~d~%" (length bit-stream) full-bit-count)))
      bit-stream)))

;;; !!! NOTE that this is a simplified byte mode that does not exactly follow the specification, but it does work with low unicode values
(defun create-bit-stream-in-byte-mode (version text &key (verbose nil))
  "Creates and returns and adjustable bit-stream according to VERSION and TEXT in a custom byte mode; this mode is to be used when TEXT consists of any standard characters from a US-based keyboard. The returned byte-mode bit-stream consists of a 4-bit sequence to represent the mode, then an n-bit (8 or 16) sequence -- according to VERSION -- that represents the count of characters, and finally a collection of 8-bit sequences where each sequence encodes a single character according to the binary equivalent of its Unicode value. To be clear, this custom byte mode only allows up to the Unicode value of 255 (in decimal, NOT hex) and includes, at least, all standard US keyboard characters. VERSION must be an integer valued 1-40 (inclusive). TEXT must be a string consisting exclusively of characters that can be typed on a standard US-English keyboard."
  (let ((mode "0100")
        (char-count (length text))
        (bit-count 8) ;; each character will be represented with an 8-bit sequence
        (character-count-bits (get-bits-for-character-count-indicator version +mode-byte+))) ;; the number of bits used to represent the character count
    (let ((full-bit-count (+ 4 character-count-bits (* bit-count char-count)))
          (bit-stream (create-adjustable-bit-stream)))
      ;; adding the mode (as binary) to the bit-stream
      (extend-adjustable-bit-stream bit-stream (make-bit-vector-from-binary-string mode))
      ;; adding the character count (as binary) to the bit-stream
      (extend-adjustable-bit-stream bit-stream (make-bit-vector-from-binary-string (write-to-string char-count :base 2) character-count-bits))
      (when verbose
        (format t "Creating BIT-STREAM in (custom) Byte Mode for version ~d QR-symbol...~%~%~s <-- initial BIT-STREAM without any data codewords...~%~s <-- first 4 bits (mode codeword)~%    ~s <-- next ~d bits indicating count of characters in text (~d)~%~%Generating data codewords...~%~%" version bit-stream (subseq bit-stream 0 4) (subseq bit-stream 4) character-count-bits char-count))
      ;; adding each of the characters (as 8-bit binary) to the bit-stream
      (dotimes (i char-count)
        (let* ((ch (char text i))
               (binary (write-to-string (char-code ch) :base 2))
               (bv (make-bit-vector-from-binary-string binary bit-count)))
          (when verbose
            (format t "~s yields ~a~%" ch bv))
          (extend-adjustable-bit-stream bit-stream bv)))
      (when verbose
        (format t "~%BIT-STREAM so far, including data codewords, by end of function CREATE-BIT-STREAM-IN-BYTE-MODE:~%~a~%~%" bit-stream))
      (when (/= (length bit-stream) full-bit-count)
        (error (format nil "The length of BIT-STREAM is not equal to the expected length as indicated by FULL-BIT-COUNT!~%Actual length: ~d~%Expected: ~d~%" (length bit-stream) full-bit-count)))
      bit-stream)))

(defun add-padding-to-data-bit-stream (bit-stream version error-correction-level &key (verbose nil))
  "Modifies BIT-STREAM by extending it with padding until it is of the length specified by VERSION and ERROR-CORRECTION-LEVEL, according to the hash-table *DATA-CAPACITIES-FOR-ERROR-CORRECTION-LEVEL-BY-VERSION*. This function adds a terminator sequence (0000) to the end of BIT-STREAM, adding additional 0s until the remaining space in the bit-stream is divisible by 8; after that, two alternating binary sequences (11101100 and 00010001) are appended until the required length is met; note that the terminator sequence (0000) may be shortened or even excluded if the maximum total length of the data bit-stream will not accommodate it. BIT-STREAM must be an adjustable bit-vector as returned by one of the CREATE-BIT-STREAM-IN-*-MODE functions. VERSION must be an integer valued 1-40 (inclusive). ERROR-CORRECTION-LEVEL must be one of the +ECL-*+ constants."
  (let ((standard-terminator-size 4)
        (padding-size 8))
    (let ((pad-codewords (vector (make-array padding-size :element-type 'bit :initial-contents (list 1 1 1 0 1 1 0 0)) (make-array padding-size :element-type 'bit :initial-contents (list 0 0 0 1 0 0 0 1))))
          (pad-index 0) ;; used to access the vector PAD-CODEWORDS at index 0 or 1, alternatingly (through use of MOD)
          (total-bit-count (aref (aref (gethash version *data-capacities-for-error-correction-level-by-version*) error-correction-level) +dcfec-bit-count+)))
      ;; add terminator 0000 (unless appending the terminator would cause an overflow of the symbol's capacity [in which case, shorten or exclude terminator])
      (let ((terminator-size (- total-bit-count (length bit-stream))))
        (when (< terminator-size 0) ;; when TERMINATOR-SIZE has a negative value
          (error "BIT-STREAM has a length already exceeding TOTAL-BIT-COUNT!")) ;; ! note that this should not happen unless a table is wrong or this function is misused
        (when (> terminator-size standard-terminator-size)
          (setq terminator-size standard-terminator-size))
        (extend-adjustable-bit-stream bit-stream (make-array terminator-size :element-type 'bit))
        (when verbose
          (format t "Adding padding to BIT-STREAM...~%~%Adding terminator: ~a~%" (make-array terminator-size :element-type 'bit))))
      ;; make sure that the remaining space in the bit-stream is of a size divisible by 8, adding zeroes to end until it is
      (let ((added-zeroes (mod (- total-bit-count (length bit-stream)) padding-size)))
        (when (not (zerop added-zeroes))
          (extend-adjustable-bit-stream bit-stream (make-array added-zeroes :element-type 'bit))
          (when verbose
            (format t "Adding additional 0s to make length of BIT-STREAM divisible by 8: ~a~%" (make-array added-zeroes :element-type 'bit)))))
      ;; fill out BIT-STREAM with pad-codewords until it is completely full (being of length TOTAL-BIT-COUNT)
      (let ((pad-count 0))
        (do ()
            ((>= (length bit-stream) total-bit-count)) ;; ! note that calling LENGTH this way is efficient because BIT-STREAM is an array with a set fill-pointer
          (incf pad-count)
          (extend-adjustable-bit-stream bit-stream (aref pad-codewords pad-index))
          (setq pad-index (mod (1+ pad-index) 2)))
        (when verbose
          (format t "Added ~d padding codewords, in alternation, extending BIT-STREAM's length by ~d to make it of length ~d.~%~%Full BIT-STREAM so far with padding added:~%~a~%~%" pad-count (* pad-count padding-size) total-bit-count bit-stream))))))

(defun create-data-block-vector (version error-correction-level)
  "Creates and returns a vector that indicates the number of 8-bit sequences that must exist in each sequential block when the data is finally \"blocked\" according to its VERSION and ERROR-CORRECTION-LEVEL; note that the returned vector has a FILL-POINTER set. To clarify what this function actually does, if the error-correction-characteristic (ecc) indicated by VERSION and ERROR-CORRECTION-LEVEL was this VECTOR, (vector t 108 (list 2 4) (list (vector 32 14 9) (vector 33 15 9))), then the T indicates that there are both primary and secondary (instead of only primary) block-sizes, and the LIST, (list 2 4), indicates that there should be 2 blocks of primary size and 4 blocks of secondary size, and the other LIST, (list (vector 32 14 9) (vector 33 15 9)), indicates that the primary size is 14 and the secondary size is 15, yielding a vector like the following: #(14 14 15 15 15 15). See the function CREATE-FINAL-BIT-STREAM-WITH-ERROR-CORRECTION to better understand how the returned vector is used in the \"blocking\" process. VERSION must be an integer valued 1-40 (inclusive). ERROR-CORRECTION-LEVEL must be one of the +ECL-*+ constants."
  (let ((codewords-per-block (make-array 0 :element-type 'number :adjustable t :fill-pointer 0))
        (ecc-by-ecl (aref (gethash version *error-correction-characteristics*) error-correction-level)))
    (let ((multi-p (aref ecc-by-ecl +ecc-has-two+))
          (block-count (aref ecc-by-ecl +ecc-block-count+))
          (details (aref ecc-by-ecl +ecc-details+)))
      ;; collecting primary block-sizes
      (dotimes (x (first block-count))
        (vector-push-extend (aref (first details) +ecc-details-count-per-block+) codewords-per-block))
      (when multi-p ;; when there are different block sizes...
        ;; collecting the secondary block-sizes
        (dotimes (x (second block-count))
          (vector-push-extend (aref (second details) +ecc-details-count-per-block+) codewords-per-block)))
      codewords-per-block)))

(defun make-adjustable-copy-of-polynomial (polynomial)
  "Creates and returns an adjustable copy of POLYNOMIAL. Note that this function is used exclusively within the function CREATE-FINAL-BIT-STREAM-WITH-ERROR-CORRECTION to ensure that its polynomial variable reattains adjustability; thus, POLYNOMIAL is expected to be a polynomial array subsequence as derived from the \"initial\" polynomial of that same function."
  (let ((adjustable-polynomial (make-array 0 :adjustable t :fill-pointer 0)))
    (dotimes (i (length polynomial))
      (let ((v (elt polynomial i)))
        (vector-push-extend v adjustable-polynomial)))
    adjustable-polynomial))

(defun add-remainder-bits-to-bit-stream (bit-stream version &key (verbose nil))
  "Modifies BIT-STREAM by adding 0, 3, 4, or 7 zeroes to its end, according to version number. Note that this function is used exclusively with the function CREATE-FINAL-BIT-STREAM-WITH-ERROR-CORRECTION."
  (let ((bit-count (aref (gethash version *data-capacities-by-version*) +remainder-bits+)))
    (when (> bit-count 0)
      (extend-adjustable-bit-stream bit-stream (make-array bit-count :element-type 'bit))
      (when verbose
        (format t "Adding unusable remainder-bits to end of BIT-STREAM: ~a~%~%" (make-array bit-count :element-type 'bit))))))

(defun create-final-bit-stream-with-error-correction (bit-stream version error-correction-level &key (verbose nil) (verbose-polynomials nil))
  "Creates and returns the finalized bit-stream, which contains the potentially reorganized (via blocking) data bit-stream followed by the newly created error-correction bit-stream, according to the passed data BIT-STREAM and its VERSION and ERROR-CORRECTION-LEVEL. Note that a \"blocking\" process occurs for QR-symbols with higher VERSION or ECL, causing the codewords of the data BIT-STREAM to be interleaved with one another -- according to specification -- within the finalized bit-stream. To create the error correction bit-stream, a mathematical process involving polynomials is used; for each data block, an associated error-correction block is created by first creating a custom polynomial from the data block, which is then put through repeating and complex mathematical interactions with the appropriate generator polynomial, yielding up the \"final\" polynomial, from which the actual the error-correction block is derived. BIT-STREAM must be an adjustable bit-stream as returned from CREATE-BIT-STREAM-IN-*-MODE with padding already added via the function ADD-PADDING-TO-DATA-BIT-STREAM. VERSION must be an integer valued 1-40 (inclusive). ERROR-CORRECTION-LEVEL must be one of the +ECL-*+ constants."
  (let ((bit-stream-length (aref (aref (gethash version *data-capacities-for-error-correction-level-by-version*) error-correction-level) +dcfec-bit-count+))
        (bit-stream-as-string (subseq (format nil "~a" bit-stream) 2)) ;; bit-stream is transformed into string so that subsequencing a string out of it (later) is easier
        (data-block-vector (create-data-block-vector version error-correction-level)))
    (when (/= (length bit-stream) bit-stream-length)
      (error "BIT-STREAM, according to tables, must have length of ~d, but it has length of ~d!" bit-stream-length (length bit-stream)))
    (when verbose
      (format t "Generating error correction codewords for the following BIT-STREAM of length ~d:~%~a~%~%DATA-BLOCK-VECTOR: ~a~%" (length bit-stream) bit-stream data-block-vector))
    (let ((ec-codeword-count (/ (aref (aref (gethash version *error-correction-characteristics*) error-correction-level) +ecc-codeword-count+) (length data-block-vector)))
          (current-index-in-string 0) ;; used to track index within BIT-STREAM-AS-STRING
          (data-blocks (make-array (length data-block-vector))) ;; will hold all the data-codewords in block groupings
          (ec-blocks (make-array (length data-block-vector)))) ;; will hold all the error-correction codewords in block groupings
      ;; filling out the two vectors, DATA-BLOCKS and EC-BLOCKS, one block at a time...
      (dotimes (block-index (length data-block-vector)) ;; block-index is used with both vectors
        (setf (aref data-blocks block-index) (make-array 0 :element-type 'bit-vector :adjustable t :fill-pointer 0))
        (setf (aref ec-blocks block-index) (make-array 0 :element-type 'bit-vector :adjustable t :fill-pointer 0))
        (let ((data-codeword-count (aref data-block-vector block-index)))
          (let ((polynomial (make-array data-codeword-count :adjustable t :fill-pointer data-codeword-count)) ;; each element will be a vector of size 2, like #(integer-value x-power)
                (leading-zeros 0))
            ;; preparing the DATA-BLOCKS vector and also the initial POLYNOMIAL, which is made from the consecutive bytes of the passed data BIT-STREAM, with ...
            ;; ... each of them being changed to an integer and followed by a descending power of x (with the very last x equal to ec-codeword-count)
            (let ((adj 8) ;; collecting 8-bits at a time
                  (end-index (+ current-index-in-string (* data-codeword-count 8))))
              (do ((i current-index-in-string (+ i adj))
                   (ipi 0 (1+ ipi))) ;; ipi -> initial polynomial index
                  ((>= i end-index))
                (let ((codeword-string (subseq bit-stream-as-string i (+ i adj))))
                  (setf (aref polynomial ipi) (vector (parse-integer codeword-string :radix 2) (+ (- data-codeword-count (1+ ipi)) ec-codeword-count))) ;; note that by last iteration, (1+ ipi) is equal to data-codeword-count because the polynomial length is equal to data-codeword-count
                  (vector-push-extend (make-bit-vector-from-binary-string codeword-string 8) (aref data-blocks block-index))))
              (setq current-index-in-string end-index))
            ;; removing any lead zeros from POLYNOMIAL and tracking their removal
            ;; ! note that when an 8-bit sequence of all 0s occurs as CODEWORD-STRING, it is represented within POLYNOMIAL as 0x^y, or in code as #(0 y); whenever ...
            ;; ... any such polynomial part occurs at the front of a distinct block, either once or repeatedly, it must be removed because the very first leading ...
            ;; ... value in the polynomial sequence is (later) used in a galois-field reverse-lookup (which requires the values 1-255 inclusive)
            ;; !!! also note that this DO loop is very similar to one below
            (do ((first-iteration t nil))
                ((/= 0 (aref (aref polynomial 0) 0)) (unless first-iteration (setq polynomial (make-adjustable-copy-of-polynomial polynomial)))) ;; if the polynomial was subsequenced once or more before end of loop, make a new adjustable copy of it
              (setq polynomial (subseq polynomial 1)) ;; ! note that after SUBSEQ, POLYNOMIAL is now a simple array without a fill-pointer
              (incf leading-zeros))
            (when (and verbose verbose-polynomials)
              (format t "~%~%data: ~a~%ec: ~a" data-codeword-count ec-codeword-count)
              (format t "~%Starting polynomial:~%~a~%" polynomial))
            ;; recreating POLYNOMIAL by having it repeatedly mathematically interact with a generator polynomial
            (dotimes (total-steps (- data-codeword-count leading-zeros)) ;; the process must occur as many times as there are data-codewords (minus any removed leading 0s)
              (let ((generator-polynomial (get-copy-of-generator-polynomial ec-codeword-count))) ;; ! the generator polynomial is renewed every iteration
                ;; here, all of the x-powers of the generator-polynomial are increased (simulating multiplication) by the difference between ...
                ;; ... the first x-power of the initial POLYNOMIAL and the first x-power of the generator-polynomial so that the first x-powers ...
                ;; ... of the different polynomials are the same
                ;; ! note that modulus will occur later, so that if any x goes over 255 now, it will brought into the appropriate range later
                (let ((dif (- (aref (aref polynomial 0) 1) (aref (aref generator-polynomial 0) 1)))) ;; the difference between the first x-powers in each of the polynomials
                  (dotimes (i (1+ ec-codeword-count))
                    (incf (aref (aref generator-polynomial i) 1) dif)))
                ;; here, the alpha-powers of the generator polynomial are increased (again simulating multiplication) by the leading-term and then modded by 255...
                ;; ... they are then converted into integer notation by finding them individually (by index) in the *galois-field*
                ;; ! note that the "lead term" must not be 0 -- as in 0x^y -- (and cannot be 0 due to above steps that remove leading 0s) or else the reverse lookup fails
                (let ((leading-term (gethash (aref (aref polynomial 0) 0) *galois-field-reverse-lookup*))) ;; leading term of polynomial, such as 5 in 5x^3
                  (dotimes (i (1+ ec-codeword-count))
                    (setf (aref (aref generator-polynomial i) 0) (aref *galois-field* (mod (+ leading-term (aref (aref generator-polynomial i) 0)) 255)))))
                ;; here, the initial polynomial is XORed with the generator-polynomial, by taking consecutive integer values, changing them to binary, and then XORing them
                ;; the result of XORing overwrites the current POLYNOMIAL value index, or else a new vector of the appropriate format is appended to the end of POLYNOMIAL
                ;; ! since the size of POLYNOMIAL changes throughout the various iterations, care must be taken to account for issues with the index
                (let ((l1 (length polynomial))
                      (l2 (length generator-polynomial)))
                  (let ((len (if (> l1 l2) l1 l2))
                        (last-power nil))
                    (dotimes (i len)
                      (let (;; ! note that for both of the below symbols a 0 value is taken when the index is out of bounds
                            (pb (make-bit-vector-from-binary-string (write-to-string (if (>= i l1) 0 (aref (aref polynomial i) 0)) :base 2) 8))
                            (gpb (make-bit-vector-from-binary-string (write-to-string (if (>= i l2) 0 (aref (aref generator-polynomial i) 0)) :base 2) 8)))
                        (let ((v (parse-integer (subseq (format nil "~a" (bit-xor pb gpb)) 2) :radix 2))) ;; the result of XORing as a decimal
                          (if (>= i l1) ;; if the index is out of bounds for POLYNOMIAL
                              (progn ;; THEN, append a new vector onto POLYNOMIAL
                                (vector-push-extend (vector v (1- last-power)) polynomial)
                                (when last-power
                                  (decf last-power)))
                              (progn ;; ELSE, set the value at the correct index of POLYNOMIAL to V
                                (setf (aref (aref polynomial i) 0) v)
                                (setq last-power (aref (aref polynomial i) 1)))))))))
                ;; the previous steps ensure that the leading value of POLYNOMIAL is always 0 (such as in 0x^25); this section removes any such "0"s
                ;; ! there may be multiple such "0"s at front of POLYNOMIAL (although there is usually just one), but if there are more than one the removal of ...
                ;; ... a zero beyond the first must be counted toward TOTAL-STEPS
                (do ((first-iteration t nil))
                    ((/= 0 (aref (aref polynomial 0) 0)) (unless first-iteration (setq polynomial (make-adjustable-copy-of-polynomial polynomial)))) ;; if the polynomial was subsequenced once or more before end of loop, make a new adjustable copy of it (so that the next iteration in the DOTIMES-loop still has an adjustable POLYNOMIAL)
                  (setq polynomial (subseq polynomial 1)) ;; ! note that after SUBSEQ, POLYNOMIAL is now a simple array without a fill-pointer
                  (unless first-iteration
                    (incf total-steps)))))
            (when verbose
              (format t "~%Polynomial:~%~a~%" polynomial))
            ;; now that POLYNOMIAL has been recreated through its own complex process...
            ;; ... finally, the vector EC-BLOCKS (at index BLOCK-INDEX) is filled out based on the contents of POLYNOMIAL
            (let ((poly-len (length polynomial)))
              (dotimes (i ec-codeword-count)
                (if (>= i poly-len)
                    (vector-push-extend (make-bit-vector-from-binary-string "0" 8) (aref ec-blocks block-index)) ;; add zeroes at end if resulting polynomial is short
                    (vector-push-extend (make-bit-vector-from-binary-string (write-to-string (aref (aref polynomial i) 0) :base 2) 8) (aref ec-blocks block-index))))))))
      (when verbose
        (format t "~%Data blocks:~%~a~%~%Error-correction blocks:~%~a~%~%" data-blocks ec-blocks))
      (let ((final-bit-stream (create-adjustable-bit-stream)))
        ;; interleaving the __data-codewords__ from the different blocks into a single bit-stream ...
        ;; ... such that the codeword at index 1 of block 1 is followed by the codeword from index 1 of block 2, etc.
        (dotimes (i (aref data-block-vector (1- (length data-block-vector)))) ;; ! providing the last value in DATA-BLOCK-VECTOR
          (dotimes (block-index (length data-block-vector))
            (when (< i (length (aref data-blocks block-index)))
              (extend-adjustable-bit-stream final-bit-stream (aref (aref data-blocks block-index) i)))))
        ;; interleaving the __error-correction-codewords__ from the different blocks into a single bit-stream ...
        ;; ... such that the codeword at index 1 of block 1 is followed by the codeword from index 1 of block 2, etc.
        (dotimes (i ec-codeword-count)
          (dotimes (block-index (length data-block-vector))
            (extend-adjustable-bit-stream final-bit-stream (aref (aref ec-blocks block-index) i))))
        ;; adding any remainder bits if necessary
        (add-remainder-bits-to-bit-stream final-bit-stream version :verbose verbose)
        (when verbose
          (format t "The finalized BIT-STREAM with error-correction~a:~%~a~%~%" (if (> (length data-block-vector) 1) ", fully reorganized via blocking" " added at end") final-bit-stream))
        ;; checking that the FINAL-BIT-STREAM's length accords to the specification's tables
        (let ((expected-length (elt (gethash version *data-capacities-by-version*) +data-modules+)))
          (when (/= expected-length (length final-bit-stream))
            (error "The FINAL-BIT-STREAM has a length that does not accord to the specification's tables!~%Actual length: ~d~%Expected length: ~d~%" (length final-bit-stream) expected-length)))
        ;; returning final-bit-stream, which contains the potentially reorganized data bit-stream with the error correction bit-stream following it
        final-bit-stream))))

;; ! note that this function does NOT touch any part of the column that relates to the vertical timing pattern
(defun write-final-bit-stream-to-qr-symbol (qr bit-stream &key (allow-incomplete-bit-stream nil))
  "Modifies QR so as to mark remaining data modules according to the passed BIT-STREAM; note that this function is only to be called once the QR-symbol has been marked with all patterns and all version and format information (or, at least, all related modules must have the associated codes set). QR-symbols have their data written to them starting at the bottom-right corner; data is then written in a zig-zag pattern with a width of 2; the zig-zag always moves right to left horizontally, after which it always moves diagonally to the next rightmost module along its current vertical trajectory except where that leads to an invalid D-index, at which point it steps two spaces towards left and then switches its vertical trajectory, moving diagonally afterwards, and then resumes the former pattern; note that the first vertical trajectory is always up. In this implementation, this zig-zag pattern is followed up/down through the entire QR-symbol, without skipping any modules -- writing immediately to any module whose code has not been set -- until finally \"running off\" the left side of the QR-symbol; this is why all patterns and information modules must be set before calling this function. QR must be a two-dimensional array as created with the function CREATE-2D-ARRAY-FOR-QR-SYMBOL. BIT-STREAM must be an adjustable bit-stream that was returned by the function CREATE-FINAL-BIT-STREAM-WITH-ERROR-CORRECTION. By default, BIT-STREAM must be complete and exactly sized -- or else an error will be triggered -- being able to completely fill out the QR-symbol; set ALLOW-INCOMPLETE-BIT-STREAM to T to allow for an incomplete BIT-STRING to be passed and used, with default values being provided after the exhaustion of the BIT-STREAM; this feature is only intended for testing and other experimental reasons."
  (let ((size (first (array-dimensions qr)))
        (step 2) ;; this represents the width of the zig-zag pattern
        (bsl (length bit-stream)))
    ;; writing all of BIT-STREAM to QR-symbol until the pattern is followed to the point where the R-index is negative (and thus, invalid)
    (do ((index 0) ;; this is used to access the values in BIT-STREAM
         (d (1- size))
         (r (1- size))
         (at-start t)
         (change-in-d -1) ;; alternates between -1 and 1, starting at -1; -1 indicates that the zig-zag moves upward; 1 indicates that the zig-zag moves downward
         (reverse-d-next-iteration nil))
        ((< r 0) (when (< index bsl) (error "BIT-STREAM is too large for QR-symbol!")))
      ;; attempting to write to QR-symbol at current D-R coordinate and one space to the left
      (dotimes (x step)
        (unless (car (aref qr d (- r x))) ;; only write to an empty module (or rather a module with no "code")
          (let ((data nil)) ;; ! data is set immediately below in IF sequence
            (if (>= index bsl) ;; if index is out of bounds for BIT-STREAM
                ;; THEN, check whether this is allowed...
                (if allow-incomplete-bit-stream
                    (setq data +white+) ;; when allowed, set a default white color
                    (error "BIT-STREAM is incomplete and too small to fill out QR-symbol!~%Set ALLOW-INCOMPLETE-BIT-STREAM to T if you want to fill out remainder with default values."))
                ;; ELSE, set data according to BIT-STREAM
                (setq data (aref bit-stream index)))
            (add-to-qr-symbol qr d (- r x) +mc-data+ data) ;; !!! currently, all data written to qr is labelled as +mc-data+ even when it is error-correction-data...
            (incf index))))
      ;; deciding on directional adjustments for use in the next iteration
      (cond
        (at-start ;; at first iteration, move up (the first iteration is on an edge and therefore gets extra attention)
         (setq at-start nil)
         (setq d (+ d change-in-d)))
        (reverse-d-next-iteration ;; when an edge was reached on last iteration, start moving in opposite vertical direction
         (setq reverse-d-next-iteration nil)
         (setq change-in-d (* -1 change-in-d))
         (setq d (+ d change-in-d)))
        ((or (= 0 d) (= (1- size) d)) ;; when at a top or bottom edge of qr symbol, step left (not up or down)
         (setq reverse-d-next-iteration t)
         (decf r step)
         (when (= r +timing-pattern-index+) ;; ! notice that the column for the vertical timing-pattern is entirely skipped!
           (decf r))) ;; offset a single space to the left due to the presence of the vertical timing pattern
        (t ;; when in a central position of the qr symbol, continue moving in same direction
         (setq d (+ d change-in-d)))))))

(defun create-format-bit-stream (error-correction-level mask-ref)
  "Creates and returns a \"format information\" bit-stream according to ERROR-CORRECTION-LEVEL and MASK-REF. First, a 5-bit sequence is created according to the ECL binary (the first 2 bits) and the MASK-REF binary (the remaining 3 bits); then, a custom 10-bit error correction binary is generated -- according to a process called (15, 5) BCH -- from the 5-bit sequence; afterwards the 5-bit and 10-bit sequences are combined into one, and the resulting binary is masked (via an XOR) with a custom binary sequence, yielding the fully complete \"format information\" bit-stream. ERROR-CORRECTION-LEVEL must be one of the +ECL-*+ constants. MASK-REF must be one of 3-bit binary strings as held in the list *ALL-MASK-REFERENCES*."
  (let ((ecl-and-mask (make-bit-vector-from-binary-string (concatenate 'string (gethash error-correction-level *ecl-to-binary*) mask-ref) 5))
        (end-length 10))
    (let ((bit-stream (trim-left-zeroes-from-bit-vector (add-zeroes-to-right-end-of-bit-vector ecl-and-mask end-length)))
          (original-generator (make-array (1+ end-length) :element-type 'bit :initial-contents (list 1 0 1 0 0 1 1 0 1 1 1))))
      ;; repeatedly XORing the BIT-STREAM that was derived from the ecl-and-mask (which was right-extended with 0s) with the generator (right-extending ...
      ;; ... with 0s to make lengths match) until the bit-stream is 10 (END-LENGTH) or less in length
      (do ((cur-len (length bit-stream) (length bit-stream)))
          ((<= cur-len end-length))
        ;; ! note that GENERATOR (in below LET) is set up as a deep copy of ORIGINAL-GENERATOR
        (let ((generator (add-zeroes-to-right-end-of-bit-vector original-generator (- cur-len (1+ end-length))))) ;; ! adding zeroes to GENERATOR so that it is same length as BIT-STREAM
          (setq bit-stream (trim-left-zeroes-from-bit-vector (bit-xor bit-stream generator)))))
      ;; preparing to mask the final 15-bit sequence
      (let ((format-mask (make-array 15 :element-type 'bit :initial-contents (list 1 0 1 0 1 0 0 0 0 0 1 0 0 1 0))) ;; this binary masks the end result
            (cur-len (length bit-stream)))
        ;; extending BIT-STREAM by 0s (on left side) when necessary...
        (when (< cur-len end-length)
          (setq bit-stream (concatenate 'bit-vector (make-array (- end-length cur-len) :element-type 'bit) bit-stream)))
        ;; masking and returning the completed 15-bit sequence
        (bit-xor format-mask (concatenate 'bit-vector ecl-and-mask bit-stream))))))

(defun create-version-bit-stream (version)
  "Creates and returns a \"version information\" bit-stream according to VERSION. First, a 6-bit sequence is created according to the VERSION number as binary; then, a custom 12-bit error correction binary is generated -- according to a process called (18, 6) BCH -- from the 6-bit sequence; afterwards the 6-bit and 12-bit sequences are combined into one, yielding the fully complete \"version information\" bit-stream. VERSION must be an integer valued 1-40 (inclusive)."
  (when (>= version +first-version-with-version-information+)
    (let ((version-bv (make-bit-vector-from-binary-string (write-to-string version :base 2) 6))
          (end-length 12))
      (let ((bit-stream (trim-left-zeroes-from-bit-vector (add-zeroes-to-right-end-of-bit-vector version-bv end-length)))
            (original-generator (make-array (1+ end-length) :element-type 'bit :initial-contents (list 1 1 1 1 1 0 0 1 0 0 1 0 1))))
        ;; repeatedly XORing the BIT-STREAM that was derived from the version binary (which was right-extended with 0s) with the generator (right-extending ...
        ;; ... with 0s to make lengths match) until the bit-stream is 12 (END-LENGTH) or less in length
        (do ((cur-len (length bit-stream) (length bit-stream)))
            ((<= cur-len end-length))
          ;; ! note that GENERATOR (in below LET) is set up as a deep copy of ORIGINAL-GENERATOR
          (let ((generator (add-zeroes-to-right-end-of-bit-vector original-generator (- cur-len (1+ end-length)))))
            (setq bit-stream (trim-left-zeroes-from-bit-vector (bit-xor bit-stream generator)))))
        ;; preparing to create final bit-sequence
        (let ((cur-len (length bit-stream)))
          ;; extending BIT-STREAM by 0s (on left side) when necessary...
          (when (< cur-len end-length)
            (setq bit-stream (concatenate 'bit-vector (make-array (- end-length cur-len) :element-type 'bit) bit-stream)))
          ;; returning the completed 18-bit sequence
          (concatenate 'bit-vector version-bv bit-stream))))))

(defun apply-mask (qr mask-ref &key (make-simple-copy nil))
  "Modifies QR (or else a simplified copy of QR) so as to reverse the color of the appropriate data and error-correction modules according to the passed mask reference (as MASK-REF), via the mathematical patterning of that mask. Note that in the creation of the final QR-symbol, the result of masking the original unmasked QR-symbol (with every available mask) must be analyzed and scored in order to choose the best mask; this function allows for creating a simplified copy in order to smooth this process. QR must be a two-dimensional array as created with the function CREATE-2D-ARRAY-FOR-QR-SYMBOL; it is expected that all modules of the QR-symbol have been filled out by the time this function is used, with one exception: format-information modules may be without values due to the fact that format-information encodes mask. MASK-REF must be one of 3-bit binary strings as held in the list *ALL-MASK-REFERENCES*. When MAKE-SIMPLE-COPY is NIL, then QR will be directly modified; when NIL, then a simplified copy will be created with relevant mask, and then returned. Note that a simplified copy of QR is a two-dimensional array where every element is either +BLACK+ or +WHITE+; thus, the simplified copy does not track module codes."
  (let ((size (first (array-dimensions qr)))
        (mask-fun (gethash mask-ref *mask-reference-to-function*)))
    (let ((simple-copy (if make-simple-copy (make-array `(,size ,size) :element-type 'bit) nil))) ;; ! note that the simple copy only holds values (no "codes")
      ;; swapping the value of any data and error-correction modules that meet the criteria of the masking function...
      (dotimes (d size)
        (dotimes (r size)
          (let* ((module-code-value-cons (aref qr d r))
                 (code (car module-code-value-cons))
                 (value (cdr module-code-value-cons)))
            (if (and code ;; if a code is set
                     (or (= code +mc-data+) (= code +mc-error-correction+)) ;; if the code is +mc-data+ or +mc-error-correction+
                     (funcall mask-fun d r)) ;; ! when the masking function returns true, the module's color should change to the opposite
                ;; THEN, the criteria is met, so swap the value
                (if make-simple-copy
                    (setf (aref simple-copy d r) (gethash value *color-change*))
                    (setf (cdr (aref qr d r)) (gethash value *color-change*)))
                ;; ELSE, the criteria is NOT met...
                (if make-simple-copy
                    ;; THEN, when copy is being made, fill out the newly created SIMPLE-COPY according to QR
                    (progn
                      (unless value
                        (if (= code +mc-format-information+)
                            (setq value +white+) ;; provide a default white value to the copy when this module represents a, as of yet, valueless format-information module
                            (error "The passed QR-symbol is incomplete!")))
                      (setf (aref simple-copy d r) value))
                    ;; ELSE, when working with actual QR-symbol
                    (when (and (not value) (/= code +mc-format-information+))
                      (error "The passed QR-symbol is incomplete!~%All elements of QR must have a value set before a mask can be applied.")))))))
      (when make-simple-copy
        simple-copy))))

(defun get-penalty-score-for-masked-copy (masked-copy)
  "Calculates and returns the penalty-score for the passed MASKED-COPY. A penalty score suggests through its integer value the readability of the related QR-symbol in terms of its black and white modules, with a lower value implying greater readability. Note that this function is only used within the function DETERMINE-BEST-MASK. MASKED-COPY must be 2-dimensional BIT array as created and returned by the function APPLY-MASK.  Note that the approach used in this function to calculate penalty-score may be somewhat inconsistent with the specification-document for conditions 3 and 4; see the comments in the code for more details."
  (let ((size (first (array-dimensions masked-copy)))
        (score 0))
    ;; condition 1a (horizontal repetition)
    ;; ! the penalty score will increase whenever 5 or more modules of the same color are found in sequence in any row
    (dotimes (d size)
      ;; comparing each element with the one preceding it in the row, left-to-right
      (do* ((r 1 (1+ r)) ;; ! notice that R is 1 initially, and that LAST-VALUE covers the first index
            (last-value (aref masked-copy d 0) (aref masked-copy d (1- r)))
            (c 1)) ;; ! C is count of same modules; it starts at 1 because there has already been one in sequence before first comparison
           ((>= r size) (when (>= c 5)
                          (incf score (+ 3 (- c 5)))))
        (if (= last-value (aref masked-copy d r))
            ;; THEN, they are same, so increment count...
            (incf c)
            ;; ELSE, they are different, so check the count up to this point, adjusting SCORE if appropriate, and then reset count...
            (progn
              (when (>= c 5)
                (incf score (+ 3 (- c 5)))) ;; ! this is N1+i from specification
              (setq c 1)))))
    ;; condition 1b (vertical repetition)
    ;; ! the penalty score will increase whenever 5 or more modules of the same color are found in sequence in any column
    (dotimes (r size)
      ;; comparing each element with the one preceding it in the column, top-to-bottom
      (do* ((d 1 (1+ d)) ;; ! notice that D is 1 initially, and that LAST-VALUE covers the first index
            (last-value (aref masked-copy 0 r) (aref masked-copy (1- d) r))
            (c 1)) ;; ! C is count of same modules; it starts at 1 because there has already been one in sequence before first comparison
           ((>= d size) (when (>= c 5)
                          (incf score (+ 3 (- c 5)))))
        (if (= last-value (aref masked-copy d r))
            ;; THEN, they are same, so increment count...
            (incf c)
            ;; ELSE, they are different, so check the count up to this point, adjusting SCORE if appropriate, and then reset count...
            (progn
              (when (>= c 5)
                (incf score (+ 3 (- c 5)))) ;; ! again, this is N1+i from specification
              (setq c 1)))))
    ;; condition 2 (2x2 squares)
    ;; ! the penalty score will increase for every 2x2 block consisting entirely of same color as found throughout the entire QR-symbol
    ;; ! although the specification states that blocks are to be assessed according to size (as in MxN), this approach yields the same result by ...
    ;; ... more simplistically looking for the smallest 2x2 block sizes while yet following the specification's equation for scoring
    (do ((d 0 (1+ d)))
        ((>= d (1- size))) ;; ! notice that last column is not considered, in order to avoid the attempt at using invalid indexes (because looking down)
      (do ((r 0 (1+ r)))
          ((>= r (1- size))) ;; ! notice that last row is not considered, in order to avoid the attempt at using invalid indexes (because looking right)
        ;; when the module at index d-r has the same value as the one to its right, the one immediately below it, and the one to its bottom right...
        (when (= (aref masked-copy d r)
                 (aref masked-copy (1+ d) r)
                 (aref masked-copy d (1+ r))
                 (aref masked-copy (1+ d) (1+ r)))
          ;; increase the SCORE
          (incf score 3)))) ;; ! note that this is the specification's formula of N2 * (m - 1) * (n - 1), yielding 3*1*1, because only 2x2 blocks are considered
    ;; condition 3 (finding specific pattern)
    ;; ! the penalty score will increase any time either of two patterns is found in any row or column
    ;; !!! note that this condition does not appear to exactly match what is in the specification; the specification mentions a 1:1:3:1:1 pattern, but says ...
    ;; ... nothing about the condition of four 0s being either left or right of it; the approach used here accords more to an approach I found online ...
    ;; ... which may or may not be in accord with the actual specification (... it is hard to say due to the specification's relative vagueness here)
    (let* ((pattern-size 11)
           (last-index (- size pattern-size)) ;; ! this is the last index to search (rightward or downward) from, due to the length of the patterns
           (pattern1 (vector 1 0 1 1 1 0 1 0 0 0 0))
           (pattern2 (reverse pattern1)))
      ;; checking for the patterns in each of the rows...
      (do ((d 0 (1+ d)))
          ((>= d size))
        (do ((r 0 (1+ r)))
            ((> r last-index)) ;; ! notice the restriction here (used in order to avoid bad indexes)
          ;; looking rightward for the pattern (PATTERN1) from each index in row, stopping as soon as there is a mismatch ...
          ;; ... but if no mismatch is found, increasing the SCORE
          (do ((i 0 (1+ i)))
              ((>= i pattern-size) (incf score 40))
            (when (/= (aref masked-copy d (+ r i)) (aref pattern1 i))
              (return)))
          ;; doing the same as above DO loop, but searching for PATTERN2 this time...
          (do ((i 0 (1+ i)))
              ((>= i pattern-size) (incf score 40))
            (when (/= (aref masked-copy d (+ r i)) (aref pattern2 i))
              (return)))))
      ;; checking for the patterns in each of the columns...
      (do ((r 0 (1+ r)))
          ((>= r size))
        (do ((d 0 (1+ d)))
            ((> d last-index)) ;; ! notice the restriction here (used in order to avoid bad indexes)
          ;; looking downward for the pattern (PATTERN1) from each index in column, stopping as soon as there is a mismatch ...
          ;; ... but if no mismatch is found, increasing the SCORE
          (do ((i 0 (1+ i)))
              ((>= i pattern-size) (incf score 40))
            (when (/= (aref masked-copy (+ d i) r) (aref pattern1 i))
              (return)))
          ;; doing the same as above DO loop, but searching for PATTERN2 this time...
          (do ((i 0 (1+ i)))
              ((>= i pattern-size) (incf score 40))
            (when (/= (aref masked-copy (+ d i) r) (aref pattern2 i))
              (return))))))
    ;; condition 4 (proportions of black and white modules)
    ;; ! the penalty score will increase based on the preponderance of the most numerous color-module ...
    ;; ... the closer the color proportion is to 50%, the smaller the penalty (with no penalty added when sufficiently close)
    ;; !!! this condition may not exactly follow the specification, which gave scarce information on the details
    (let ((total-count 0)
          (white-count 0)
          (black-count 0))
      ;; counting black modules, white modules, and all modules
      (dotimes (d size)
        (dotimes (r size)
          (incf total-count)
          (cond
            ((= (aref masked-copy d r) +black+)
             (incf black-count))
            ((= (aref masked-copy d r) +white+)
             (incf white-count))
            (t
             (error "MASKED-COPY has in improperly set element; each element must be set to either 0 or 1!")))))
      ;; getting the percentage of the most numerous module from within the QR-symbol
      ;; ! note that the most numerous module is used in order to ensure consistency in the determination of the multiple of N4
      ;; ! notice also that because the most numerous module is considered, MODULE-PERCENT must always be 50 or greater
      (let* ((most-numerous-module-count (if (>= black-count white-count) black-count white-count))
             (module-percent (* 100 (/ most-numerous-module-count total-count))))
        ;; collecting nearest multiples of 5 for the value of MODULE-PERCENT
        ;; ! for example, if MODULE-PERCENT were 53, NEAREST-MULTIPLES-OF-5 would be set to (CONS 50 55)
        ;; ! notice that if MODULE-PERCENT were 100, that would yield (CONS 100 105)
        (let ((nearest-multiples-of-5 (cons (- module-percent (mod module-percent 5)) (+ module-percent (- 5 (mod module-percent 5))))))
          ;; creating another CONS of values derived from NEAREST-MULTIPLES-OF-5, which will serve as the potential multiple of N4 (10); ...
          ;; ... each value suggests the multiple's closeness to 50%, by an order of 5, as an integer 0-10 (inclusive) with 0 being closest
          ;; ! to continue the example, (CONS 50 55) would yield (CONS 0 1)
          (let ((cv (cons (/ (abs (- (car nearest-multiples-of-5) 50)) 5) (/ (abs (- (cdr nearest-multiples-of-5) 50)) 5))))
            ;; adjusting SCORE according to the smallest value in the CONS of values, CV
            (if (< (car cv) (cdr cv))
                (incf score (* 10 (car cv)))
                (incf score (* 10 (cdr cv))))))))
    score))

(defun determine-best-mask (qr error-correction-level)
  "Returns the best mask-reference for the QR-symbol. To determine the best mask, this function creates a simplified copy of QR, masks it, and then determines the penalty score for that newly created QR-symbol copy; the mask-reference that yields the QR-symbol copy with the lowest penalty score is considered the best mask. QR must be a two-dimensional array as created with the function CREATE-2D-ARRAY-FOR-QR-SYMBOL; it is expected that all modules of the QR-symbol have been filled out by the time this function is used. ERROR-CORRECTION-LEVEL must be one of the +ECL-*+ constants, and it is expected to be consistent with the ECL used to generate the error-correction-modules."
  (let ((best-score nil)
        (final-ref nil))
    ;; determining the best mask-reference by determining which mask, when applied, has the lowest relative penalty score
    (dolist (mask-ref *all-mask-references*)
      (let ((masked-copy (apply-mask qr mask-ref :make-simple-copy t)))
        ;; adding format information; note that "format information" encodes both ECL and mask reference (and MASK_REF is changing each iteration of DOLIST)
        (add-format-information qr :bit-stream (create-format-bit-stream error-correction-level mask-ref) :added-to-copy masked-copy)
        (let ((score (get-penalty-score-for-masked-copy masked-copy)))
          (when (or (not best-score)
                    (< score best-score))
            ;; saving current lowest score and reference when lower than any yet tested
            (setq best-score score)
            (setq final-ref mask-ref)))))
    final-ref))

(defun show-simple-qr (qr)
  "Prints a simplified string representation of QR to standard output; in this simplified output, only values (and no codes) are represented. A period character is used to represent any module that has no related value. QR must be a two-dimensional array as created with the function CREATE-2D-ARRAY-FOR-QR-SYMBOL; note that the QR-symbol need not be complete."
  (let ((size (first (array-dimensions qr))))
    (dotimes (d size)
      (let ((str (make-string size)))
        (dotimes (r size)
          (let ((v (cdr (aref qr d r))))
            (let ((v (if v (char (write-to-string v) 0) #\.))) ;; if v is set, convert it to a character; if unset, represent with period character
              (setf (char str r) v))))
        (format t "~s~%" str))))
  (format t "~%"))

(defun output-qr-to-text-file (qr &key (file-path #p"./qr-code.txt"))
  "Writes a textual representation of the QR-symbol to a text file; this textual representation can be scanned with a valid QR-code-reader. In the textual representation a +WHITE+ square is represented with 2 spaces, and a +BLACK+ square is represented with two special Unicode characters in sequence (because together they make a square shape). Note that for the resulting QR-symbol to be readable and uniform, the font used in the text-file-reading software must be set to a monospace font and the font-size may need to be adjusted (and will need to be particularly small for larger QR-symbol versions). QR must be a two-dimensional array as created with the function CREATE-2D-ARRAY-FOR-QR-SYMBOL. Note that this function allows for QR to be incomplete; the user is expected to make appropriate use of the function WRITE-FINAL-BIT-STREAM-TO-QR-SYMBOL to guarantee completeness of the QR-symbol when completeness is expected or required. FILE-PATH must be a valid full or relative path, including the filename as a text file; note that the passed value is only minimally checked for correctness; please use this parameter carefully."
  (unless (pathnamep file-path)
    (error "FILE-PATH must be a valid full or relative path, including the filename as a text file (please precede string with pathname designator, as in: #P\"\")."))
  (with-open-file (fout file-path :direction :output :if-exists :supersede :external-format :utf8)
    (let ((size (first (array-dimensions qr)))
          (w #\Space)
          (b #\U+2588) ;; this character is a tall black rectangle
          (v-to-c (make-hash-table))) ;; v-to-c -> value-to-character; this hash-table is used to map +white+ and +black+ values to the above 2 characters
      (setf (gethash +white+ v-to-c) w)
      (setf (gethash +black+ v-to-c) b)
      ;; adding some spacing above QR-symbol
      (dotimes (x 3)
        (write-char #\Newline fout))
      ;; writing to each row of text file
      (dotimes (d size)
        ;; adding some spacing on left side of QR-symbol
        (dotimes (x 9)
          (write-char w fout))
        ;; finally writing out each row of the QR-symbol to the file
        (dotimes (r size)
          (let* ((value (cdr (aref qr d r)))
                 (c (if value (gethash value v-to-c) w))) ;; ! setting C to appropriate character based on VALUE, or, when a value was not set for the related QR element, to the default white character
            ;; writing the character twice (because the black character is tall rectangle, that, when written twice in a row, resembles a square)
            (write-char c fout)
            (write-char c fout)))
        ;; moving on to next row
        (write-char #\Newline fout)))))

(defun generate-qr-symbol (text &key (ecl +ecl-l+) (mode nil) (mask nil) (simple-output nil) (write-to-file t) (file-path #p"./qr-code.txt") (return-values nil) (verbose nil) (verbose-polynomials nil))
  "Generates a QR-symbol from TEXT using the passed ECL (error-correction-level), providing various forms of output according to the passed arguments.
TEXT must be a string consisting of only those characters that are available on a standard US-based keyboard; note that Extended Channel Interpretation (ECI), which enables the use of various language libraries, has not been implemented within this program; hence the limitation on TEXT.
ECL refers to error-correction-level type and must be one of the +ECL-*+ constants.
MODE must be NIL or else one of the +MODE-*+ constants; when NIL, MODE will be determined automatically; otherwise, when passed, the function will attempt to encode the text using the passed MODE; it is strongly recommended that MODE not be passed at all.
MASK must be NIL or else one of 3-bit binary strings as held in the list *ALL-MASK-REFERENCES*; when NIL, MASK will be set automatically according to penalty-scoring; otherwise, when passed, the provided mask will be used without the penalty-scoring process ever taking place.
SIMPLE-OUTPUT, when T, will cause the a simple textual representation of the QR-symbol to be output to standard out.
WRITE-TO-FILE, when T, will cause the actual \"qr code\" to be output as a text file to the location specified by FILE-PATH.
FILE-PATH must be a valid full or relative path, including the filename as a text file; note that the passed value is only minimally checked for correctness; please use this parameter carefully.
RETURN-VALUES, when T, will cause this function to return 3 separate values: QR (the QR-symbol's internal representation), VERSION (an integer indicating the generated QR-symbol's version), and MODE (an integer indicating the mode used in encoding the TEXT).
VERBOSE, when T, will cause textual output to be printed to standard out, helping to explain the generation of the complete BIT-STREAM according to MODE and ECL, while giving guidance on little else (at this point).
VERBOSE-POLYNOMIALS, when T, will cause textual output to be printed to standard out, helping to illustrate all polynomial transformations during the generation of error-correction-data."
  (check-that-ecl-is-valid ecl)
  (if mode
      (check-that-mode-is-valid text mode)
      (setq mode (determine-best-mode text)))
  (when mask
    (check-that-mask-is-valid mask))
  (when verbose
    (format t "Character count in TEXT: ~d~%" (length text))
    (format t "Mode: ~d~%" mode))
  (let ((version (determine-best-version mode ecl text)))
    (when verbose
      (format t "Version: ~d~%~%" version))
    (let ((qr (create-2d-array-for-qr-symbol version))
          (bit-stream nil)) ;; ! note that this is set according to mode, immediately below
      (cond
        ((= mode +mode-numeric+)
         (setq bit-stream (create-bit-stream-in-numeric-mode version text :verbose verbose)))
        ((= mode +mode-alphanumeric+)
         (setq bit-stream (create-bit-stream-in-alphanumeric-mode version (string-downcase text) :verbose verbose))) ;; ! notice that TEXT must be downcased when using alphanumeric mode
        ((= mode +mode-byte+)
         (setq bit-stream (create-bit-stream-in-byte-mode version text :verbose verbose)))
        ;; !!! etc...
        (t
         (error "No valid mode was determined...")))
      ;; adding all patterns to QR-symbol...
      (add-position-detection-patterns qr)
      (add-alignment-patterns qr version)
      (add-timing-patterns qr)
      ;; setting (or preparing) all informational modules
      (add-format-information qr) ;; ! because mask is not yet determined, only setting "format information" module codes (and not values)
      (add-version-information qr version (create-version-bit-stream version))
      ;; padding out BIT-STREAM (so that the data-section is fully complete)
      (add-padding-to-data-bit-stream bit-stream version ecl :verbose verbose)
      ;; creating the finalized BIT-STREAM with error correction modules
      ;; ! notice that BIT-STREAM is reset with a SETQ below because it is fully recreated and returned by the below function
      (setq bit-stream (create-final-bit-stream-with-error-correction bit-stream version ecl :verbose verbose :verbose-polynomials verbose-polynomials))
      ;; finally setting the data and error-correction modules of QR-symbol (by writing the completed BIT-STREAM to QR)
      (write-final-bit-stream-to-qr-symbol qr bit-stream)
      ;; determining the best mask and then masking the QR-symbol with it
      (let ((mask-ref (if mask mask (determine-best-mask qr ecl))))
        (apply-mask qr mask-ref)
        (let ((format-bit-stream (create-format-bit-stream ecl mask-ref)))
          ;; setting "format information" modules with the appropriate values, now that best mask is certainly determined
          (add-format-information qr :bit-stream format-bit-stream)))
      ;; outputting simplified textual representation of QR-symbol
      (when simple-output
        (show-simple-qr qr))
      ;; writing final QR-symbol to a file
      (when write-to-file
        (output-qr-to-text-file qr :file-path file-path))
      (if return-values
          (values qr version mode)
          nil))))

(defun test-qr-masks (version &key (path-to-directory "./mask-tests/"))
  "This is a test function that causes every mask to be applied to a QR-symbol of the provided VERSION, both WITH, and then WITHOUT, patterns being applied to it; each such generated QR-symbol is output as a separate text file within the directory indicated by PATH-TO-DIRECTORY. A total of 16 text files will be generated. VERSION must be an integer valued 1-40 (inclusive). PATH-TO-DIRECTORY should be a simple string that indicates a full or relative path to a directory; thus it must end with a slash character; also, note that the actual directory must exist before calling this function."
  (check-that-version-is-valid version)
  ;; setting PATH-TO-DIRECTORY to a simple string, if it is of type pathname
  (when (pathnamep path-to-directory)
    (setq path-to-directory (format nil "~a" path-to-directory)))
  ;; checking that PATH-TO-DIRECTORY ends with a '/' character to indicate that the path ends with a directory
  (when (char/= #\/ (char path-to-directory (1- (length path-to-directory))))
    (error "PATH-TO-DIRECTORY must end with a '/' character."))
  ;; testing with QR-symbol patterns, and then without...
  (dolist (include-pattern-p (list t nil))
    ;; for every mask...
    (dolist (mask *all-mask-references*)
      (let ((qr (create-2d-array-for-qr-symbol version))
            (path (pathname (concatenate 'string path-to-directory mask (if include-pattern-p "p" "") ".txt"))))
        (if include-pattern-p
            (progn ;; THEN, include all patterns
              (add-position-detection-patterns qr)
              (add-alignment-patterns qr version)
              (add-timing-patterns qr)
              (add-format-information qr :bit-stream (make-array +format-information-length+ :element-type 'bit))
              ;; ! notice that the constant dark-module near format information is NOT set (since ADD-FORMAT-INFORMATION was not called with BIT-STREAM set to NIL)
              (add-version-information qr version (make-array +version-information-length+ :element-type 'bit))
              (write-final-bit-stream-to-qr-symbol qr (make-array 1 :element-type 'bit) :allow-incomplete-bit-stream t))
            (progn ;; ELSE, exclude patterns
              (write-final-bit-stream-to-qr-symbol qr (make-array 1 :element-type 'bit) :allow-incomplete-bit-stream t)
              ;; filling out the column of the timing-pattern to make it all white, since the preceding function skips that column
              (dotimes (d (first (array-dimensions qr)))
                (add-to-qr-symbol qr d +timing-pattern-index+ +mc-data+ +white+))))
        (apply-mask qr mask)
        (output-qr-to-text-file qr :file-path path)))))

(defun check-returned-version-at-generation (text ecl mode version)
  "This function is used exclusively within the test function TEST-QR-SYMBOL-GENERATION to verify that the returned version is as expected."
  (multiple-value-bind (qr returned-version returned-mode) (generate-qr-symbol text :ecl ecl :mode mode :write-to-file nil :return-values t)
    (declare (ignore qr returned-mode))
    (when (/= version returned-version)
      (error "Actual version of QR-symbol does not match the expected version."))))

(defun test-qr-symbol-generation (&key (demonstrative-test nil))
  "Tests the function GENERATE-QR-SYMBOL by generating QR-symbols, via random strings, of every mode and error-correction-level for every version (for a total of 1440 tests). If any error occurs there must be a problem somewhere in the code; but if this test completes without error it merely suggests that the code is okay, and all text-related QR-symbols were successfully generated (although they have not been checked for readability). This test does, however, specifically prove that the table *DATA-CAPACITIES-FOR-ERROR-CORRECTION-LEVEL-BY-VERSION* has correct capacity values set. Set DEMONSTRATIVE-TEST to T, to see a short sample of what this test does."
  ;; ! note that in the vector STRING-BY-TYPE there are 4 vectors with each one being for each distinct error-correction level ...
  ;; ... where each ecl has 3 strings for each distinct mode
  (let ((string-by-type (vector (vector (make-array 0 :element-type 'character :fill-pointer 0) 
                                        (make-array 0 :element-type 'character :fill-pointer 0)
                                        (make-array 0 :element-type 'character :fill-pointer 0))
                                (vector (make-array 0 :element-type 'character :fill-pointer 0)
                                        (make-array 0 :element-type 'character :fill-pointer 0)
                                        (make-array 0 :element-type 'character :fill-pointer 0))
                                (vector (make-array 0 :element-type 'character :fill-pointer 0)
                                        (make-array 0 :element-type 'character :fill-pointer 0)
                                        (make-array 0 :element-type 'character :fill-pointer 0))
                                (vector (make-array 0 :element-type 'character :fill-pointer 0)
                                        (make-array 0 :element-type 'character :fill-pointer 0)
                                        (make-array 0 :element-type 'character :fill-pointer 0))))
        (numeric-characters (make-array 0 :element-type 'character :fill-pointer 0))
        (alphanumeric-characters (make-array 0 :element-type 'character :fill-pointer 0)))
    (maphash #'(lambda (k v) (declare (ignore v)) (vector-push-extend k numeric-characters)) *numeric-mode-hash-table*)
    (maphash #'(lambda (k v) (declare (ignore v)) (vector-push-extend k alphanumeric-characters)) *alphanumeric-mode-hash-table*)
    (let ((character-function-by-mode (vector #'(lambda () (elt numeric-characters (random (length numeric-characters))))
                                              #'(lambda () (elt alphanumeric-characters (random (length alphanumeric-characters))))
                                              #'(lambda () (elt alphanumeric-characters (random (length alphanumeric-characters)))))))
      ;; performing 3 tests per mode per ecl per version (3*3*4*40, or 1440, tests)
      (dotimes (x (if demonstrative-test 2 40))
        (let ((version (1+ x)))
          (dotimes (ecl 4) ;; ecl l, m, q, then h
            (dotimes (mode 3) ;; numeric, alphanumeric, and then byte mode
              (format t "Testing Version-~d, ECL-~d, Mode-~d...~%" version ecl mode)
              (let ((character-capacity (elt (elt (gethash version *data-capacities-for-error-correction-level-by-version*) ecl) (+ 2 mode)))
                    (cur-str (elt (elt string-by-type ecl) mode)))
                ;; making CUR-STR one character longer than last VERSION's limitation
                (vector-push-extend (funcall (elt character-function-by-mode mode)) cur-str)
                (when demonstrative-test
                  (format t "    ~s~%" cur-str))
                (check-returned-version-at-generation cur-str ecl mode version)
                ;; making CUR-STR of random length between smallest size (by current size) and largest size (by CHARACTER-CAPACITY)
                (let ((characters-to-add (1+ (random (- (1- character-capacity) (length cur-str))))))
                  (dotimes (y characters-to-add)
                    (vector-push-extend (funcall (elt character-function-by-mode mode)) cur-str)))
                (when demonstrative-test
                  (format t "    ~s~%" cur-str))
                (check-returned-version-at-generation cur-str ecl mode version)
                ;; making CUR-STR to have the size CHARACTER-CAPACITY
                (let ((characters-to-add (- character-capacity (length cur-str))))
                  (dotimes (y characters-to-add)
                    (vector-push-extend (funcall (elt character-function-by-mode mode)) cur-str)))
                (when demonstrative-test
                  (format t "    ~s~%" cur-str))
                (check-returned-version-at-generation cur-str ecl mode version)))))))))
