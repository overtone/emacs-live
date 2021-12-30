;;; haskell-customize.el --- Customization settings -*- lexical-binding: t -*-

;; Copyright (c) 2014 Vasantha Ganesh Kanniappan <vasanthaganesh.k@tuta.io>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'haskell-customize)
(require 'haskell-test-utils)

(defvar dir-structure nil)

(ert-deftest haskell-process-type-test-1 ()
  (with-temp-dir-structure
   (("README.md" . "Hello world")
    ("Main.hs" . "-- Empty file")
    ("abc.cabal" . "-- Empty File")
    ("stack.yaml" . "# Empty file")
    ("src" . (("moduleA.hs" . "-- Empty file")
              ("moduleB.hs" . "-- Empty file")))
    ("tests" . (("test1.hs" . "-- Empty file")
                ("test2.hs" . "-- Empty file"))))
   (progn
     (cd "tests")
     (should (eq 'stack-ghci (haskell-process-type))))))

(ert-deftest haskell-process-type-test-2 ()
  (with-temp-dir-structure
   (("README.md" . "Hello world")
    ("Main.hs" . "-- Empty file")
    ("stack.yaml" . "# Empty file")
    ("src" . (("moduleA.hs" . "-- Empty file")
                 ("moduleB.hs" . "-- Empty file")))
    ("tests" . (("test1.hs" . "-- Empty file")
                ("test2.hs" . "-- Empty file"))))
   (progn
     (cd "src")
     (should (eq 'stack-ghci (haskell-process-type))))))

(ert-deftest haskell-process-type-test-3 ()
  (with-temp-dir-structure
   (("README.md" . "Hello world")
    ("Main.hs" . "-- Empty file")
    ("abc.cabal" . "-- Empty file")
    ("src" . (("moduleA.hs" . "-- Empty file")
              ("moduleB.hs" . "-- Empty file")))
    ("tests" . (("test1.hs" . "-- Empty file")
                ("test2.hs" . "-- Empty file"))))
   (progn
     (should (eq 'cabal-repl (haskell-process-type))))))

(ert-deftest haskell-process-type-test-4 ()
  (with-temp-dir-structure
   (("README.md" . "Hello world")
    ("Main.hs" . "-- Empty file")
    ("src" . (("moduleA.hs" . "-- Empty file")
              ("moduleB.hs" . "-- Empty file")))
    ("tests" . (("test1.hs" . "-- Empty file")
                ("test2.hs" . "-- Empty file"))))
   (progn
     (should (eq 'ghci (haskell-process-type))))))
