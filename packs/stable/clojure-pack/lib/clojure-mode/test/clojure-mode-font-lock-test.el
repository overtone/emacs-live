;;; clojure-mode-font-lock-test.el --- Clojure Mode: Font lock test suite  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2016 Bozhidar Batsov <bozhidar@batsov.com>

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


;;;; Utilities

(defmacro clojure-test-with-temp-buffer (content &rest body)
  "Evaluate BODY in a temporary buffer with CONTENTS."
  (declare (debug t)
           (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (clojure-mode)
     (font-lock-fontify-buffer)
     (goto-char (point-min))
     ,@body))

(defmacro clojurex-test-with-temp-buffer (content &rest body)
  "Evaluate BODY in a temporary buffer with CONTENTS."
  (declare (debug t)
           (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (clojurex-mode)
     (font-lock-fontify-buffer)
     (goto-char (point-min))
     ,@body))

(defun clojure-get-face-at-range (start end)
  (let ((start-face (get-text-property start 'face))
        (all-faces (cl-loop for i from start to end collect (get-text-property i 'face))))
    (if (cl-every (lambda (face) (eq face start-face)) all-faces)
        start-face
      'various-faces)))

(defun clojure-test-face-at (start end &optional content)
  "Get the face between START and END in CONTENT.

If CONTENT is not given, return the face at the specified range in the current
buffer."
  (if content
      (clojure-test-with-temp-buffer content
        (clojure-get-face-at-range start end))
    (clojure-get-face-at-range start end)))

(defun clojurex-test-face-at (start end &optional content)
  "Get the face between START and END in CONTENT.

If CONTENT is not given, return the face at the specified range in the current
buffer."
  (if content
      (clojurex-test-with-temp-buffer content
                                      (clojure-get-face-at-range start end))
    (clojure-get-face-at-range start end)))

(defconst clojure-test-syntax-classes
  [whitespace punctuation word symbol open-paren close-paren expression-prefix
              string-quote paired-delim escape character-quote comment-start
              comment-end inherit generic-comment generic-string]
  "Readable symbols for syntax classes.

Each symbol in this vector corresponding to the syntax code of
its index.")

(defun clojure-test-syntax-at (pos)
  "Get the syntax at POS.

Get the syntax class symbol at POS, or nil if there is no syntax a
POS."
  (let ((code (syntax-class (syntax-after pos))))
    (aref clojure-test-syntax-classes code)))


;;;; Font locking

(ert-deftest clojure-mode-syntax-table/fontify-clojure-keyword ()
  :tags '(fontification syntax-table)
  (should (equal (clojure-test-face-at 2 11 "{:something 20}") '(clojure-keyword-face))))

(ert-deftest clojure-mode-syntax-table/stuff-in-backticks ()
  :tags '(fontification syntax-table)
  (should (equal (clojure-test-face-at 1 2 "\"`#'s/trim`\"") font-lock-string-face))
  (should (equal (clojure-test-face-at 3 10 "\"`#'s/trim`\"") '(font-lock-constant-face font-lock-string-face)))
  (should (equal (clojure-test-face-at 11 12 "\"`#'s/trim`\"") font-lock-string-face))
  (should (equal (clojure-test-face-at 1 1 ";`#'s/trim`") font-lock-comment-delimiter-face))
  (should (equal (clojure-test-face-at 2 2 ";`#'s/trim`") font-lock-comment-face))
  (should (equal (clojure-test-face-at 3 10 ";`#'s/trim`") '(font-lock-constant-face font-lock-comment-face)))
  (should (equal (clojure-test-face-at 11 11 ";`#'s/trim`") font-lock-comment-face)))

(ert-deftest clojure-mode-syntax-table/stuff-in-backticks ()
  :tags '(fontification syntax-table)
  (should (equal (clojure-test-face-at 1 2 "\"a\\bc\\n\"") font-lock-string-face))
  (should (equal (clojure-test-face-at 3 4 "\"a\\bc\\n\"") '(bold font-lock-string-face)))
  (should (equal (clojure-test-face-at 5 5 "\"a\\bc\\n\"") font-lock-string-face))
  (should (equal (clojure-test-face-at 6 7 "\"a\\bc\\n\"") '(bold font-lock-string-face)))
  (should (equal (clojure-test-face-at 4 5 "#\"a\\bc\\n\"") '(bold font-lock-string-face))))

(ert-deftest clojure-mode-syntax-table/fontify-namespaced-keyword ()
  :tags '(fontification syntax-table)
  (should (equal (clojure-test-face-at 9 11 "(:alias/def x 10)") '(clojure-keyword-face)))
  (should (equal (clojure-test-face-at 2 2 "{:alias/some 20}")  '(clojure-keyword-face)))
  (should (equal (clojure-test-face-at 3 7 "{:alias/some 20}")  '(font-lock-type-face clojure-keyword-face)))
  (should (equal (clojure-test-face-at 8 8 "{:alias/some 20}")  '(default clojure-keyword-face)))
  (should (equal (clojure-test-face-at 9 12 "{:alias/some 20}") '(clojure-keyword-face)))
  (should (equal (clojure-test-face-at 2 2 "{:a.ias/some 20}")  '(clojure-keyword-face)))
  (should (equal (clojure-test-face-at 3 7 "{:a.ias/some 20}")  '(font-lock-type-face clojure-keyword-face)))
  (should (equal (clojure-test-face-at 8 8 "{:a.ias/some 20}")  '(default clojure-keyword-face)))
  (should (equal (clojure-test-face-at 9 12 "{:a.ias/some 20}") '(clojure-keyword-face))))

(ert-deftest clojure-mode-syntax-table/fontify-let-when-while-type-forms ()
  :tags '(fontification syntax-table)
  (should (equal (clojure-test-face-at 2 11 "(when-alist [x 1]\n  ())") 'font-lock-keyword-face))
  (should (equal (clojure-test-face-at 2 12 "(while-alist [x 1]\n  ())") 'font-lock-keyword-face))
  (should (equal (clojure-test-face-at 2 10 "(let-alist [x 1]\n  ())") 'font-lock-keyword-face)))

(ert-deftest clojure-mode-syntax-table/comment-macros ()
  :tags '(fontification syntax-table)
  (should (not (clojure-test-face-at 1 2 "#_ \n;; some crap\n (lala 0101\n lao\n\n 0 0i)")))
  (should (equal (clojure-test-face-at 5 41 "#_ \n;; some crap\n (lala 0101\n lao\n\n 0 0i)") 'font-lock-comment-face)))

(ert-deftest clojure-mode-syntax-table/type ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 1 9 "SomeClass") 'font-lock-type-face)))

(ert-deftest clojure-mode-syntax-table/type-hint ()
  :tags '(fontification syntax-table)
  (clojure-test-with-temp-buffer "#^SomeClass"
    (should (eq (clojure-test-face-at 3 11) 'font-lock-type-face))
    (should (eq (clojure-test-face-at 1 2) nil))))

(ert-deftest clojure-mode-syntax-table/constructor ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 2 11 "(SomeClass.)") 'font-lock-type-face))
  (clojure-test-with-temp-buffer "(ns/SomeClass.)"
    (should (eq (clojure-test-face-at 2 3) 'font-lock-type-face))
    (should (eq (clojure-test-face-at 5 14) 'font-lock-type-face))))

(ert-deftest clojure-mode-syntax-table/namespace ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 1 5 "one.p") 'font-lock-type-face))
  (should (eq (clojure-test-face-at 1 11 "one.p.top13") 'font-lock-type-face))
  (should (eq (clojure-test-face-at 2 12 "^one.p.top13") 'font-lock-type-face)))

(ert-deftest clojure-mode-syntax-table/namespaced-symbol ()
  :tags '(fontification syntax-table)
  (clojure-test-with-temp-buffer "clo.core/something"
    (should (eq (clojure-test-face-at 9 9) 'default))
    (should (eq (clojure-test-face-at 1 8) 'font-lock-type-face))
    (should (eq (clojure-test-face-at 10 18) nil)))
  (clojure-test-with-temp-buffer "a/something"
    (should (eq (clojure-test-face-at 1 1) 'font-lock-type-face))
    (should (eq (clojure-test-face-at 3 12) 'nil))
    (should (eq (clojure-test-face-at 2 2) 'default)))
  (clojure-test-with-temp-buffer "abc/something"
    (should (eq (clojure-test-face-at 1 3) 'font-lock-type-face))
    (should (eq (clojure-test-face-at 5 14) 'nil))
    (should (eq (clojure-test-face-at 4 4) 'default))))

(ert-deftest clojure-mode-syntax-table/static-method ()
  :tags '(fontification syntax-table)
  (clojure-test-with-temp-buffer "Class/methodName"
    (should (eq (clojure-test-face-at 1 5) 'font-lock-type-face))
    (should (eq (clojure-test-face-at 6 6) 'default))
    (should (eq (clojure-test-face-at 7 16) 'clojure-interop-method-face)))
  (clojure-test-with-temp-buffer "SomeClass/methodName"
    (should (eq (clojure-test-face-at 1 9) 'font-lock-type-face))
    (should (eq (clojure-test-face-at 10 10) 'default))
    (should (eq (clojure-test-face-at 11 20) 'clojure-interop-method-face)))
  (clojure-test-with-temp-buffer "clojure.lang.Var/someMethod"
    (should (eq (clojure-test-face-at 1 16) 'font-lock-type-face))
    (should (eq (clojure-test-face-at 17 17) 'default))
    (should (eq (clojure-test-face-at 18 27) 'clojure-interop-method-face))))

(ert-deftest clojure-mode-syntax-table/interop-method ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 1 11 ".someMethod") 'clojure-interop-method-face))
  (should (eq (clojure-test-face-at 1 10 "someMethod") 'clojure-interop-method-face))
  (should (eq (clojure-test-face-at 1 11 "topHttpTest") 'clojure-interop-method-face))
  (should (eq (clojure-test-face-at 1 4 "getX") 'clojure-interop-method-face)))

(ert-deftest clojure-mode-syntax-table/constant ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 1 5 "CONST") 'font-lock-constant-face))
  (should (eq (clojure-test-face-at 1 10 "CONST_NAME") 'font-lock-constant-face)))

(ert-deftest clojure-mode-syntax-table/class-constant ()
  :tags '(fontification syntax-table)
  (clojure-test-with-temp-buffer "Class/CONST_NAME"
    (should (eq (clojure-test-face-at 6 6) 'default))
    (should (eq (clojure-test-face-at 1 5) 'font-lock-type-face))
    (should (eq (clojure-test-face-at 7 16) 'font-lock-constant-face))))

(ert-deftest clojure-mode-syntax-table/namespaced-def ()
  :tags '(fontification syntax-table)
  (clojure-test-with-temp-buffer "(_c4/defconstrainedfn bar [] nil)"
    (should (eq (clojure-test-face-at 2 4) 'font-lock-type-face))
    (should (eq (clojure-test-face-at 5 5) 'default))
    (should (eq (clojure-test-face-at 6 18) 'font-lock-keyword-face))
    (should (eq (clojure-test-face-at 23 25) 'font-lock-function-name-face)))
  (clojure-test-with-temp-buffer "(clo/defbar foo nil)"
    (should (eq (clojure-test-face-at 2 4) 'font-lock-type-face))
    (should (eq (clojure-test-face-at 5 5) 'default))
    (should (eq (clojure-test-face-at 6 11) 'font-lock-keyword-face))
    (should (eq (clojure-test-face-at 13 15) 'font-lock-function-name-face))))

(ert-deftest clojure-mode-syntax-table/variable-def ()
  :tags '(fontification syntax-table)
  (clojure-test-with-temp-buffer "(def foo 10)"
    (should (eq (clojure-test-face-at 2 4) 'font-lock-keyword-face))
    (should (eq (clojure-test-face-at 6 8) 'font-lock-variable-name-face))))

(ert-deftest clojure-mode-syntax-table/type-def ()
  :tags '(fontification syntax-table)
  (clojure-test-with-temp-buffer "(deftype Foo)"
    (should (eq (clojure-test-face-at 2 8) 'font-lock-keyword-face))
    (should (eq (clojure-test-face-at 10 12) 'font-lock-type-face))))

(ert-deftest clojure-mode-syntax-table/function-def ()
  :tags '(fontification syntax-table)
  (clojure-test-with-temp-buffer "(defn foo [x] x)"
    (should (eq (clojure-test-face-at 2 5) 'font-lock-keyword-face))
    (should (eq (clojure-test-face-at 7 9) 'font-lock-function-name-face))))

(ert-deftest clojure-mode-syntax-table/custom-def-with-special-chars1 ()
  :tags '(fontification syntax-table)
  (clojure-test-with-temp-buffer "(defn* foo [x] x)"
    (should (eq (clojure-test-face-at 2 6) 'font-lock-keyword-face))
    (should (eq (clojure-test-face-at 8 10) 'font-lock-function-name-face))))

(ert-deftest clojure-mode-syntax-table/custom-def-with-special-chars2 ()
  :tags '(fontification syntax-table)
  (clojure-test-with-temp-buffer "(defsomething! foo [x] x)"
    (should (eq (clojure-test-face-at 2 14) 'font-lock-keyword-face))
    (should (eq (clojure-test-face-at 16 18) 'font-lock-function-name-face))))

(ert-deftest clojure-mode-syntax-table/custom-def-with-special-chars3 ()
  :tags '(fontification syntax-table)
  (clojure-test-with-temp-buffer "(def-something foo [x] x)"
    (should (eq (clojure-test-face-at 2 14) 'font-lock-keyword-face))
    (should (eq (clojure-test-face-at 16 18) 'font-lock-function-name-face))))

(ert-deftest clojure-mode-syntax-table/fn ()
  :tags '(fontification syntax-table)
  (clojure-test-with-temp-buffer "(fn foo [x] x)"
    (should (eq (clojure-test-face-at 2 3) 'font-lock-keyword-face))
    (should (eq (clojure-test-face-at 5 7) 'font-lock-function-name-face))))

(ert-deftest clojure-mode-syntax-table/lambda-params ()
  :tags '(fontification syntax-table)
  (clojure-test-with-temp-buffer "#(+ % %2 %3 %&)"
    (should (eq (clojure-test-face-at 5 5) 'font-lock-variable-name-face))
    (should (eq (clojure-test-face-at 7 8) 'font-lock-variable-name-face))
    (should (eq (clojure-test-face-at 10 11) 'font-lock-variable-name-face))
    (should (eq (clojure-test-face-at 13 14) 'font-lock-variable-name-face))))

(ert-deftest clojure-mode-syntax-table/nil ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 4 6 "(= nil x)") 'font-lock-constant-face))
  (should-not (eq (clojure-test-face-at 3 5 "(fnil x)") 'font-lock-constant-face)))

(ert-deftest clojure-mode-syntax-table/true ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 4 7 "(= true x)") 'font-lock-constant-face)))

(ert-deftest clojure-mode-syntax-table/false ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 4 8 "(= false x)") 'font-lock-constant-face)))

(ert-deftest clojure-mode-syntax-table/keyword-meta ()
  :tags '(fontification syntax-table)
  (clojure-test-with-temp-buffer "^:meta-data"
    (should (eq (clojure-test-face-at 1 1) nil))
    (should (equal (clojure-test-face-at 2 11) '(clojure-keyword-face)))))

(ert-deftest clojure-mode-syntax-table/keyword-allowed-chars ()
  :tags '(fontification syntax-table)
  (should (equal (clojure-test-face-at 1 8 ":aaa#bbb") '(clojure-keyword-face))))

(ert-deftest clojure-mode-syntax-table/keyword-disallowed-chars ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 1 5 ":aaa@bbb") 'various-faces))
  (should (equal (clojure-test-face-at 1 4 ":aaa@bbb") '(clojure-keyword-face)))
  (should (eq (clojure-test-face-at 1 5 ":aaa~bbb") 'various-faces))
  (should (equal (clojure-test-face-at 1 4 ":aaa~bbb") '(clojure-keyword-face)))
  (should (eq (clojure-test-face-at 1 5 ":aaa@bbb") 'various-faces))
  (should (equal (clojure-test-face-at 1 4 ":aaa@bbb") '(clojure-keyword-face))))

(ert-deftest clojure-mode-syntax-table/characters ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 1 2 "\\a") 'clojure-character-face))
  (should (eq (clojure-test-face-at 1 8 "\\newline") 'clojure-character-face))
  (should (eq (clojure-test-face-at 1 2 "\\1") 'clojure-character-face))
  (should (eq (clojure-test-face-at 1 6 "\\u0032") 'clojure-character-face))
  (should (eq (clojure-test-face-at 1 2 "\\+") 'clojure-character-face))
  (should (eq (clojure-test-face-at 1 2 "\\.") 'clojure-character-face))
  (should (eq (clojure-test-face-at 1 2 "\\,") 'clojure-character-face))
  (should (eq (clojure-test-face-at 1 2 "\\;") 'clojure-character-face)))

(ert-deftest clojurex-mode-syntax-table/cljx ()
  :tags '(fontification syntax-table)
  (should (eq (clojurex-test-face-at 1 5 "#+clj x") 'font-lock-preprocessor-face))
  (should (eq (clojurex-test-face-at 1 6 "#+cljs x") 'font-lock-preprocessor-face)))

(ert-deftest clojure-mode-syntax-table/refer-ns ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 1 3 "foo/var") 'font-lock-type-face))
  (should (eq (clojure-test-face-at 2 4 "@foo/var") 'font-lock-type-face)))

(ert-deftest clojure-mode-syntax-table/dynamic-var ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 1 10 "*some-var*") 'font-lock-variable-name-face))
  (should (eq (clojure-test-face-at 2 11 "@*some-var*") 'font-lock-variable-name-face))
  (should (eq (clojure-test-face-at 9 13 "some.ns/*var*") 'font-lock-variable-name-face)))

(ert-deftest clojure-mode-syntax-table/ns-macro ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 5 8 "(ns name)") 'font-lock-type-face))
  (should (eq (clojure-test-face-at 5 13 "(ns name.name)") 'font-lock-type-face))
  (should (eq (clojure-test-face-at 1 10 "[ns name]") nil)))

(provide 'clojure-mode-font-lock-test)

;;; clojure-mode-font-lock-test.el ends here
