# Edn.el [![Build Status](https://travis-ci.org/expez/edn.el.png?branch=master)](https://travis-ci.org/expez/edn.el)

Edn.el is an emacs lisp library for reading and writing the data
format [edn](https://github.com/edn-format/edn).

## Installation

I highly recommend installing edn.el through elpa.

It's available on [melpa](http://melpa.milkbox.net/):

    M-x package-install edn

## Examples
```elisp
;;; Reading
(edn-read "[(:foo bar :bar 12 ) \"foo\"]")
;; => [(:foo bar :bar 12 ) "foo"]

;;; Writing
(edn-print-string [(:foo bar :bar 12 ) \"foo\"])
;; => "[(:foo bar :bar 12 ) \"foo\"]"

;;; Using the internal representations
(edn-set-p (edn-list-to-set '(1 2 3 3)))
;; => t
(edn-set-to-list (edn-list-to-set '(1 2 3 3)))
;; => (1 2 3)

(defvar time (edn-inst-to-time (edn-read "#inst \"1985-04-12T23:20:50.52Z\"")))
;; => (7357 47698) ; a time object as found in `time-date.el'
(edn-inst-p (edn-time-to-inst time))
;; => t

(defvar uuid (edn-read "#uuid  \"f81d4fae-7dec-11d0-a765-00a0c91e6bf6\""))
(edn-uuid-p uuid)
;; => t
(edn-uuid-to-string uuid)
;; => "f81d4fae-7dec-11d0-a765-00a0c91e6bf6"

;;; Custom readers
(edn-read "#my/handler value")
;; => error unknown handler
(edn-add-reader :my/reader (lambda (val) :val))
(edn-read "#my/handler value")
;; => :val

;;; Custom writers
(cl-defstruct my-type (val))
(edn-print-string (make-my-type :val))
;; => "[cl-struct-my-type \"val\"]"
(edn-add-writer #'my-type-p (lambda (v) (concat "#my/value " (my-type-val v))))
(edn-print-string (make-my-type :val))
;; => :val
```

## Known limitations

### Set representation

Emacs lisp doesn't have have a data structure dedicated to sets.  In emacs lisp, and in other lisps like common lisp, sets are just lists without duplicate elements.  This means that when outputting edn and we encounter a list without duplicates we can't know if we should write a set or not.  There are three solutions to this problem:

1. Let the user tag the lists that should be turned into sets prior to serialization.
2. Always create lists, unless the user passes an option to eagerly create sets.
3. Use our own datastructure to represent sets.

I've chosen to take the third approach.

### Time instances and UUIDs

The problem of representation also arises with regard to instants in time and UUIDs.  I've opted to create an internal representation here as well.

Functions are provided to convert between `edn-time` and the repesentation used in `time-date.el`.

### Bignums

Elisp doesn't have support for bignums without pulling in `calc` or an external lib.  For now I've opted to just throw an error when edn containing bignums is put on the input stream.

## Is it any good?

Yes!

## Contribute

Please do!  There is a suite of tests that I'd like you to add to whenever a bug is fixed or a new feature is added.  If you don't do this I'm likely to break your code when I stumble around the codebase.

To fetch the test dependencies, install [cask](https://github.com/rejeep/cask.el) if you haven't already, then:

    $ cd /path/to/edn.el
    $ cask

Run the tests with:

    $ make test

## License

Copyright (c)  2015, Lars Andersen

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
