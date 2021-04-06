;;; clojure-mode-syntax-test.el --- Clojure Mode: syntax related tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2016 Bozhidar Batsov <bozhidar@batsov.com>

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
(require 'ert)

(defun non-func (form-a form-b)
  (with-temp-buffer
    (clojure-mode)
    (insert form-a)
    (save-excursion (insert form-b))
    (clojure--not-function-form-p)))

(ert-deftest non-function-form ()
  (dolist (form '(("#?@ " "(c d)")
                  ("#?@" "(c d)")
                  ("#? " "(c d)")
                  ("#?" "(c d)")
                  ("" "[asda]")
                  ("" "{a b}")
                  ("#" "{a b}")
                  ("" "(~)")))
    (should (apply #'non-func form)))
  (dolist (form '("(c d)"
                  "(.c d)"
                  "(:c d)"
                  "(c/a d)"
                  "(.c/a d)"
                  "(:c/a d)"
                  "(c/a)"
                  "(:c/a)"
                  "(.c/a)"))
    (should-not (non-func "" form))
    (should-not (non-func "^hint" form))
    (should-not (non-func "#macro" form))
    (should-not (non-func "^hint " form))
    (should-not (non-func "#macro " form))))

(ert-deftest clojure-syntax-prefixed-symbols ()
  (dolist (form '(("#?@aaa" . "aaa")
                  ("#?aaa"  . "?aaa")
                  ("#aaa"   . "aaa")
                  ("'aaa"   . "aaa")))
    (with-temp-buffer
      (clojure-mode)
      (insert (car form))
      (equal (symbol-name (symbol-at-point)) (cdr form)))))

(ert-deftest clojure-syntax-skip-prefixes ()
  (dolist (form '("#?@aaa" "#?aaa" "#aaa" "'aaa"))
    (with-temp-buffer
      (clojure-mode)
      (insert form)
      (backward-word)
      (backward-prefix-chars)
      (should (bobp)))))

(provide 'clojure-mode-syntax-test)
