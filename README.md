# QR-symbol -- A QR code generator written in Common Lisp

![sample-qr-yielding-github-link](./images/qr-link.png)

## About

This program allows the user to generate QR-codes from within a Common Lisp REPL though the use of a singular function and its various arguments. Otherwise, this repository could be used as something like a QR-code library.

Please note up-front that this program follows the old/original **[ISO/IEC 18004:2000 specification](https://www.iso.org/standard/30789.html)** from the year 2000; although this specification is old, the resulting QR-codes are very much readable. I have never seen the newer specifications -- they are quite expensive -- so I cannot speak to the differences between the old and new ones.

## Purpose

Originally I created this program for fun simply to see what it was like to implement, from scratch, industry-standard software according to a specification.

As for why anyone might use this software, it allows for an explorative process of generating QR-symbols from within a REPL without the need for an internet connection, while also allowing for flexibility in how the QR-codes are generated (such as by directly specifying the error-correction-level, or forcing the use of a desired mask). The main function can also be informative: if the VERBOSE argument is set to T, the user can see the full process of bit-stream generation, getting to see data codewords and error-correction codewords before they are combined, potentially blocked, and then ascribed to the actual QR-grid and later masked.

## Implemented Modes

1. Numeric mode
2. Alphanumeric mode
3. (A custom english-only) Byte mode

Note that these three distinct modes allow for the encodation of any text consisting of characters from a standard US-based keyboard, each providing a different level of data efficiency.

## Missing Modes

I did not implement Kanji mode, because it depends on a programmatic implementation of the JIS X 0208 language standard. I did not implement Extended Channel Interpretation (ECI) mode for similar reasons -- this feature allows for the encodation of many different languages, thus making it depend on the existence of extensive language libraries (which may or may not exist in Common Lisp). I also did not implement Structured Append mode and FNC1.

## The output QR-code

In this implementation, the generated QR-code is actually output as a text file. In order for the resulting QR-code to be uniform and machine-readable, the font used in the text-file-reading software must be set to a monospace font and the font-size should ideally be small (and will need to be particularly small for larger QR-code versions). In Windows notepad, for example, the font Courier New at size 3 works well.

Once the QR-code is generated as text and prepared as above, you can screenshot (and crop if needed), or use software like "Snipping Tool" to create an image from it.

## Running the software

Run the following code in a common-lisp REPL, making sure to provide the appropriate path for the asd file:
```common-lisp
(asdf:load-asd #p"provide/path/to/qr-symbol.asd")
(asdf:load-system "qr-symbol")
```

Then, if you want access to the full library of functions (including the tests), enter:
```common-lisp
(in-package :qr-symbol)
```

Or, if you only need the main function, enter:
```common-lisp
(use-package :qr-symbol)
```

### Sample executions

```common-lisp
(generate-qr-symbol "2024" :file-path #p"c:/users/pc/desktop/qr.txt" :verbose T)
```
![sample-qr-yielding-2024](./images/2024.png)

```common-lisp
(generate-qr-symbol "Hello! How are you today?" :ecl +ecl-m+ :file-path #p"c:/users/pc/desktop/qr.txt")
```
![sample-qr-yielding-a-greeting](./images/greeting.png)

## Testing of software

I have tested this software fairly thoroughly (in all aspects). There are no dedicated test suites, but there are two functions dedicated to testing: one which tests QR-code generation at all versions for all modes at all error-correction-levels against random text, and another that tests the application of masks; these functions are TEST-QR-SYMBOL-GENERATION and TEST-QR-MASKS respectively. The former function (combined with ERROR checks from within used functions) verifies the correctness of many tables as used in the code.