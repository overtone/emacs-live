;;; clojure-mode-indentation-test.el --- Clojure Mode: indentation tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Bozhidar Batsov <bozhidar@batsov.com>

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
(require 'cl-lib)
(require 'ert)
(require 's)

(defmacro check-indentation (description before after &optional var-bindings)
  "Declare an ert test for indentation behaviour.
The test will check that the swift indentation command changes the buffer
from one state to another.  It will also test that point is moved to an
expected position.

DESCRIPTION is a symbol describing the test.

BEFORE is the buffer string before indenting, where a pipe (|) represents
point.

AFTER is the expected buffer string after indenting, where a pipe (|)
represents the expected position of point.

VAR-BINDINGS is an optional let-bindings list.  It can be used to set the
values of customisable variables."
  (declare (indent 1))
  (let ((fname (intern (format "indentation/%s" description))))
    `(ert-deftest ,fname ()
       (let* ((after ,after)
              (expected-cursor-pos (1+ (s-index-of "|" after)))
              (expected-state (delete ?| after))
              ,@var-bindings)
         (with-temp-buffer
           (insert ,before)
           (goto-char (point-min))
           (search-forward "|")
           (delete-char -1)
           (clojure-mode)
           (indent-according-to-mode)

           (should (equal expected-state (buffer-string)))
           (should (equal expected-cursor-pos (point))))))))

;; Provide font locking for easier test editing.

(font-lock-add-keywords
 'emacs-lisp-mode
 `((,(rx "(" (group "check-indentation") eow)
    (1 font-lock-keyword-face))
   (,(rx "("
         (group "check-indentation") (+ space)
         (group bow (+ (not space)) eow)
         )
    (1 font-lock-keyword-face)
    (2 font-lock-function-name-face))))


;;; Tests


(check-indentation no-indentation-at-top-level
  "|x"
  "|x")

(check-indentation cond-indentation
  "
(cond
|x)"
  "
(cond
  |x)")

(check-indentation threading-with-expression-on-first-line
  "
(->> expr
 |ala)"
  "
(->> expr
     |ala)")

(check-indentation threading-with-expression-on-second-line
  "
(->>
|expr)"
  "
(->>
 |expr)")

(check-indentation doc-strings-without-indent-specified
  "
(defn some-fn
|\"some doc string\""
  "
(defn some-fn
  |\"some doc string\"")

(check-indentation doc-strings-with-correct-indent-specified
  "
(defn some-fn
  |\"some doc string\""
  "
(defn some-fn
  |\"some doc string\"")

(check-indentation doc-strings-with-additional-indent-specified
  "
(defn some-fn
  |\"some doc string
    - some note\""
  "
(defn some-fn
  |\"some doc string
    - some note\"")

;; we can specify different indentation for symbol with some ns prefix
(put-clojure-indent 'bala 0)
(put-clojure-indent 'ala/bala 1)

(check-indentation symbol-without-ns
  "
(bala
|one)"
  "
(bala
  |one)")

(check-indentation symbol-with-ns
  "
(ala/bala top
|one)"
  "
(ala/bala top
  |one)")


(provide 'clojure-mode-indentation-test)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; clojure-mode-indentation-test.el ends here
