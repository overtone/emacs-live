;;; clojure-mode-syntax-test.el --- Clojure Mode: syntax related tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2021 Bozhidar Batsov <bozhidar@batsov.dev>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The unit test suite of Clojure Mode

;;; Code:

(require 'clojure-mode)
(require 'buttercup)
(require 'test-helper "test/utils/test-helper")

(defun non-func (form-a form-b)
  (with-clojure-buffer form-a
    (save-excursion (insert form-b))
    (clojure--not-function-form-p)))

(describe "clojure--not-function-form-p"
  (it "should handle forms that are not funcions"
    (dolist (form '(("#?@ " "(c d)")
                    ("#?@" "(c d)")
                    ("#? " "(c d)")
                    ("#?" "(c d)")
                    ("" "[asda]")
                    ("" "{a b}")
                    ("#" "{a b}")
                    ("" "(~)")))
      (expect (apply #'non-func form))))

  (it "should handle forms that are funcions"
    (dolist (form '("(c d)"
                    "(.c d)"
                    "(:c d)"
                    "(c/a d)"
                    "(.c/a d)"
                    "(:c/a d)"
                    "(c/a)"
                    "(:c/a)"
                    "(.c/a)"))
      (expect (non-func "" form) :to-be nil)
      (expect (non-func "^hint" form) :to-be nil)
      (expect (non-func "#macro" form) :to-be nil)
      (expect (non-func "^hint " form) :to-be nil)
      (expect (non-func "#macro " form) :to-be nil))))

(describe "clojure syntax"
  (it "handles prefixed symbols"
    (dolist (form '(("#?@aaa" . "aaa")
                    ("#?aaa"  . "?aaa")
                    ("#aaa"   . "aaa")
                    ("'aaa"   . "aaa")))
      (with-clojure-buffer (car form)
        ;; FIXME: Shouldn't there be an `expect' here?
        (equal (symbol-name (symbol-at-point)) (cdr form)))))

  (it "skips prefixes"
    (dolist (form '("#?@aaa" "#?aaa" "#aaa" "'aaa"))
      (with-clojure-buffer form
        (backward-word)
        (backward-prefix-chars)
        (expect (bobp))))))

(describe "fill-paragraph"

  (it "should work within comments"
    (with-clojure-buffer "
;; Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt
;; ut labore et dolore magna aliqua."
      (goto-char (point-min))
      (let ((fill-column 80))
        (fill-paragraph))
      (expect (buffer-string) :to-equal "
;; Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
;; tempor incididunt ut labore et dolore magna aliqua.")))

  (it "should work within inner comments"
    (with-clojure-buffer "
(let [a 1]
  ;; Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt
  ;; ut labore et dolore
  ;; magna aliqua.
  )"
      (goto-char (point-min))
      (forward-line 2)
      (let ((fill-column 80))
        (fill-paragraph))
      (expect (buffer-string) :to-equal "
(let [a 1]
  ;; Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
  ;; tempor incididunt ut labore et dolore magna aliqua.
  )")))

(when (fboundp 'font-lock-ensure)
  (it "should not alter surrounding code"
    (with-clojure-buffer "(def my-example-variable
  \"It has a very long docstring. So long, in fact, that it wraps onto multiple lines! This is to demonstrate what happens when the docstring wraps over three lines.\"
  nil)"
      (font-lock-ensure)
      (goto-char 40)
      (let ((clojure-docstring-fill-column 80)
            (fill-column 80))
        (fill-paragraph))
      (expect (buffer-string) :to-equal "(def my-example-variable
  \"It has a very long docstring. So long, in fact, that it wraps onto multiple
  lines! This is to demonstrate what happens when the docstring wraps over three
  lines.\"
  nil)")))))

(when (fboundp 'font-lock-ensure)
  (describe "clojure-in-docstring-p"
    (it "should handle def with docstring"
      (with-clojure-buffer "(def my-example-variable
  \"Doc here and `doc-here`\"
  nil)"
        (font-lock-ensure)
        (goto-char 32)
        (expect (clojure-in-docstring-p))
        (goto-char 46)
        (expect (clojure-in-docstring-p))))))

(provide 'clojure-mode-syntax-test)

;;; clojure-mode-syntax-test.el ends here
