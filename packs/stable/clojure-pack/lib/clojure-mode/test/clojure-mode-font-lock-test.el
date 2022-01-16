;;; clojure-mode-font-lock-test.el --- Clojure Mode: Font lock test suite
;; -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2021 Bozhidar Batsov <bozhidar@batsov.dev>

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
(require 'buttercup)
(require 'test-helper "test/utils/test-helper")


;;;; Utilities

(defmacro with-fontified-clojure-buffer (content &rest body)
  "Evaluate BODY in a temporary buffer with CONTENT."
  (declare (debug t)
           (indent 1))
  `(with-clojure-buffer ,content
     (font-lock-ensure)
     (goto-char (point-min))
     ,@body))

(defun clojure-get-face-at (start end content)
  "Get the face between START and END in CONTENT."
  (with-fontified-clojure-buffer content
    (let ((start-face (get-text-property start 'face))
          (all-faces (cl-loop for i from start to end collect (get-text-property
                                                               i 'face))))
      (if (cl-every (lambda (face) (eq face start-face)) all-faces)
          start-face
        'various-faces))))

(defun expect-face-at (content start end face)
  "Expect face in CONTENT between START and END to be equal to FACE."
  (expect (clojure-get-face-at start end content) :to-equal face))

(defun expect-faces-at (content &rest faces)
  "Expect FACES in CONTENT.

FACES is a list of the form (content (start end expected-face)*)"
  (dolist (face faces)
    (apply (apply-partially #'expect-face-at content) face)))

(defconst clojure-test-syntax-classes
  [whitespace punctuation word symbol open-paren close-paren expression-prefix
              string-quote paired-delim escape character-quote comment-start
              comment-end inherit generic-comment generic-string]
  "Readable symbols for syntax classes.

Each symbol in this vector corresponding to the syntax code of
its index.")

(defmacro when-fontifying-it (description &rest tests)
  "Return a buttercup spec.

TESTS are lists of the form (content (start end expected-face)*).  For each test
check that each `expected-face` is found in `content` between `start` and `end`.

DESCRIPTION is the description of the spec."
  (declare (indent 1))
  `(it ,description
     (dolist (test (quote ,tests))
       (apply #'expect-faces-at test))))

;;;; Font locking

(describe "clojure-mode-syntax-table"

  (when-fontifying-it "should handle stuff in backticks"
    ("\"`#'s/trim`\""
     (1 2 font-lock-string-face)
     (3 10 (font-lock-constant-face font-lock-string-face))
     (11 12 font-lock-string-face))

    (";`#'s/trim`"
     (1 1 font-lock-comment-delimiter-face)
     (2 2 font-lock-comment-face)
     (3 10 (font-lock-constant-face font-lock-comment-face))
     (11 11 font-lock-comment-face)))

  (when-fontifying-it "should handle stuff in strings"
    ("\"a\\bc\\n\""
     (1 2 font-lock-string-face)
     (3 4 (bold font-lock-string-face))
     (5 5 font-lock-string-face)
     (6 7 (bold font-lock-string-face)))

    ("#\"a\\bc\\n\""
     (4 5 (bold font-lock-string-face))))

  (when-fontifying-it "should handle stuff in double brackets"
    ("\"[[#'s/trim]]\""
     (1 3 font-lock-string-face)
     (4 11 (font-lock-constant-face font-lock-string-face))
     (12 14 font-lock-string-face))

    (";[[#'s/trim]]"
     (1 1 font-lock-comment-delimiter-face)
     (2 3 font-lock-comment-face)
     (4 11 (font-lock-constant-face font-lock-comment-face))
     (12 13 font-lock-comment-face)))

  (when-fontifying-it "should fontify let, when, and while type forms"
    ("(when-alist [x 1]\n  ())"
     (2 11 font-lock-keyword-face))

    ("(while-alist [x 1]\n  ())"
     (2 12 font-lock-keyword-face))

    ("(let-alist [x 1]\n  ())"
     (2 10 font-lock-keyword-face)))

  (when-fontifying-it "should handle comment macros"
    ("#_"
     (1 2 nil))

    ("#_#_"
     (1 2 nil))

    ("#_#_"
     (3 2 font-lock-comment-face))

    ("#_ #_"
     (1 3 nil))

    ("#_ #_"
     (4 2 font-lock-comment-face))

    ("#_ \n;; some crap\n (lala 0101\n lao\n\n 0 0i)"
     (1 2 nil))

    ("#_ \n;; some crap\n (lala 0101\n lao\n\n 0 0i)"
     (5 41 font-lock-comment-face))

    ("#_#_ \n;; some crap\n (lala 0101\n lao\n\n 0 0i)\n;; more crap\n (foobar tnseriao)"
     (1 4 nil))

    ("#_ #_ \n;; some crap\n (lala 0101\n lao\n\n 0 0i)\n;; more crap\n (foobar tnseriao)"
     (1 5 nil))

    ("#_#_ \n;; some crap\n (lala 0101\n lao\n\n 0 0i)\n;; more crap\n (foobar tnseriao)"
     (7 75 font-lock-comment-face))

    ("#_ #_ \n;; some crap\n (lala 0101\n lao\n\n 0 0i)\n;; more crap\n (foobar tnseriao)"
     (8 75 font-lock-comment-face)))

  (when-fontifying-it "should handle namespace declarations"
    ("(ns .validns)"
     (5 12 font-lock-type-face))

    ("(ns =validns)"
     (5 12 font-lock-type-face))

    ("(ns .ValidNs=<>?+|?*.)"
     (5 21 font-lock-type-face))

    ("(ns ValidNs<>?+|?*.b*ar.ba*z)"
     (5 28 font-lock-type-face))

    ("(ns other.valid.ns)"
     (5 18 font-lock-type-face))

    ("(ns oneword)"
     (5 11 font-lock-type-face))

    ("(ns foo.bar)"
     (5 11 font-lock-type-face))

    ("(ns Foo.bar)"
     (5 11 font-lock-type-face)
     (5 11 font-lock-type-face)
     (5 11 font-lock-type-face))

    ("(ns Foo-bar)"
     (5 11 font-lock-type-face)
     (5 11 font-lock-type-face))

    ("(ns foo-Bar)"
     (5 11 font-lock-type-face))

    ("(ns one.X)"
     (5 9 font-lock-type-face))

    ("(ns ^:md ns-name)"
     (10 16 font-lock-type-face))

    ("(ns ^:md \n  ns-name)"
     (13 19 font-lock-type-face))

    ("(ns ^:md1 ^:md2 ns-name)"
     (17 23 font-lock-type-face))

    ("(ns ^:md1 ^{:md2 true} ns-name)"
     (24 30 font-lock-type-face))

    ("(ns ^{:md2 true} ^:md1 ns-name)"
     (24 30 font-lock-type-face))

    ("(ns ^:md1 ^{:md2 true} \n  ns-name)"
     (27 33 font-lock-type-face))

    ("(ns ^{:md2 true} ^:md1 \n  ns-name)"
     (27 33 font-lock-type-face)))

  (when-fontifying-it "should handle one word"
    (" oneword"
     (2 8 nil))

    ("@oneword"
     (2 8 nil))

    ("#oneword"
     (2 8 nil))

    (".oneword"
     (2 8 nil))

    ("#^oneword"
     (3 9 font-lock-type-face)) ;; type-hint

    ("(oneword)"
     (2 8 nil))

    ("(oneword/oneword)"
     (2 8 font-lock-type-face)
     (9 10 nil)
     (11 16 nil))

    ("(oneword/seg.mnt)"
     (2 8 font-lock-type-face)
     (9 10 nil)
     (11 16 nil))

    ("(oneword/mxdCase)"
     (2 8 font-lock-type-face)
     (9 10 nil)
     (11 16 nil))

    ("(oneword/CmlCase)"
     (2 8 font-lock-type-face)
     (9 10 nil)
     (11 16 nil))

    ("(oneword/ve/yCom|pLex.stu-ff)"
     (2 8 font-lock-type-face)
     (9 10 nil)
     (11 28 nil))

    ("(oneword/.ve/yCom|pLex.stu-ff)"
     (2 8 font-lock-type-face)
     (9 10 nil)
     (12 29 nil)))

  (when-fontifying-it "should handle a segment"
    (" seg.mnt"
     (2 8 nil))

    ("@seg.mnt"
     (2 8 nil))

    ("#seg.mnt"
     (2 8 nil))

    (".seg.mnt"
     (2 8 nil))

    ("#^seg.mnt"
     (3 9 font-lock-type-face)) ;; type-hint

    ("(seg.mnt)"
     (2 8 nil))

    ("(seg.mnt/oneword)"
     (2 8 font-lock-type-face)
     (9 10 nil)
     (11 16 nil))

    ("(seg.mnt/seg.mnt)"
     (2 8 font-lock-type-face)
     (9 10 nil)
     (11 16 nil))

    ("(seg.mnt/mxdCase)"
     (2 8 font-lock-type-face)
     (9 10 nil)
     (11 16 nil))

    ("(seg.mnt/CmlCase)"
     (2 8 font-lock-type-face)
     (9 10 nil)
     (11 16 nil))

    ("(seg.mnt/ve/yCom|pLex.stu-ff)"
     (2 8 font-lock-type-face)
     (9 10 nil)
     (11 28 nil))

    ("(seg.mnt/.ve/yCom|pLex.stu-ff)"
     (2 8 font-lock-type-face)
     (9 10 nil)
     (12 29 nil)))

  (when-fontifying-it "should handle camelcase"
    (" CmlCase"
     (2 8 nil))

    ("@CmlCase"
     (2 8 nil))

    ("#CmlCase"
     (2 8 nil))

    (".CmlCase"
     (2 8 nil))

    ("#^CmlCase"
     (3 9 font-lock-type-face)) ;; type-hint

    ("(CmlCase)"
     (2 8 nil))

    ("(CmlCase/oneword)"
     (2 8 font-lock-type-face)
     (9 10 nil)
     (11 16 nil))

    ("(CmlCase/seg.mnt)"
     (2 8 font-lock-type-face)
     (9 10 nil)
     (11 16 nil))

    ("(CmlCase/mxdCase)"
     (2 8 font-lock-type-face)
     (9 10 nil)
     (11 16 nil))

    ("(CmlCase/CmlCase)"
     (2 8 font-lock-type-face)
     (9 10 nil)
     (11 16 nil))

    ("(CmlCase/ve/yCom|pLex.stu-ff)"
     (2 8 font-lock-type-face)
     (9 10 nil)
     (11 28 nil))

    ("(CmlCase/.ve/yCom|pLex.stu-ff)"
     (2 8 font-lock-type-face)
     (9 10 nil)
     (12 29 nil)))

  (when-fontifying-it "should handle mixed case"
    (" mxdCase"
     (2 8 nil))

    ("@mxdCase"
     (2 8 nil))

    ("#mxdCase"
     (2 8 nil))

    (".mxdCase"
     (2 8 nil))

    ("#^mxdCase"
     (3 9 font-lock-type-face)) ;; type-hint

    ("(mxdCase)"
     (2 8 nil))

    ("(mxdCase/oneword)"
     (2 8 font-lock-type-face)
     (9 10 nil)
     (11 16 nil))

    ("(mxdCase/seg.mnt)"
     (2 8 font-lock-type-face)
     (9 10 nil)
     (11 16 nil))

    ("(mxdCase/mxdCase)"
     (2 8 font-lock-type-face)
     (9 10 nil)
     (11 16 nil))

    ("(mxdCase/CmlCase)"
     (2 8 font-lock-type-face)
     (9 10 nil)
     (11 16 nil))

    ("(mxdCase/ve/yCom|pLex.stu-ff)"
     (2 8 font-lock-type-face)
     (9 10 nil)
     (11 28 nil))

    ("(mxdCase/.ve/yCom|pLex.stu-ff)"
     (2 8 font-lock-type-face)
     (9 10 nil)
     (12 29 nil)))

  (when-fontifying-it "should handle quotes in tail of symbols and keywords"
    ("'quot'ed'/sy'm'bol''"
     (2 9 font-lock-type-face)
     (10 20 nil))

    (":qu'ote'd''/key'word'"
     (2 11 font-lock-type-face)
     (12 12 default)
     (13 21 clojure-keyword-face)))

  (when-fontifying-it "should handle very complex stuff"
    ("  ve/yCom|pLex.stu-ff"
     (3 4 font-lock-type-face)
     (5 21 nil))

    (" @ve/yCom|pLex.stu-ff"
     (2 2 nil)
     (3 4 font-lock-type-face)
     (5 21 nil))

    (" #ve/yCom|pLex.stu-ff"
     (2 4 font-lock-type-face)
     (5 21 nil))

    (" .ve/yCom|pLex.stu-ff"
     (2 4 font-lock-type-face)
     (5 21 nil))

    ;; type-hint
    ("#^ve/yCom|pLex.stu-ff"
     (1 2 default)
     (3 4 font-lock-type-face)
     (5 21 default))

    ("^ve/yCom|pLex.stu-ff"
     (2 3 font-lock-type-face)
     (5 20 default))

    (" (ve/yCom|pLex.stu-ff)"
     (3 4 font-lock-type-face)
     (5 21 nil))

    (" (ve/yCom|pLex.stu-ff/oneword)"
     (3 4 font-lock-type-face)
     (5 29 nil))

    (" (ve/yCom|pLex.stu-ff/seg.mnt)"
     (3 4 font-lock-type-face)
     (5 29 nil))

    (" (ve/yCom|pLex.stu-ff/mxdCase)"
     (3 4 font-lock-type-face)
     (5 29 nil))

    (" (ve/yCom|pLex.stu-ff/CmlCase)"
     (3 4 font-lock-type-face)
     (5 29 nil))

    (" (ve/yCom|pLex.stu-ff/ve/yCom|pLex.stu-ff)"
     (3 4 font-lock-type-face)
     (5 41 nil))

    (" (ve/yCom|pLex.stu-ff/.ve/yCom|pLex.stu-ff)"
     (3 4 font-lock-type-face)
     (5 42 nil)))

  (when-fontifying-it "should handle oneword keywords"
    (" :oneword"
     (3 9 clojure-keyword-face ))

    ("{:oneword 0}"
     (3 9 clojure-keyword-face))

    ("{:#oneword 0}"
     (3 10 clojure-keyword-face))

    ("{:.oneword 0}"
     (3 10 clojure-keyword-face))

    ("{:oneword/oneword 0}"
     (3 9 font-lock-type-face)
     (10 10 default)
     (11 17 clojure-keyword-face))

    ("{:oneword/seg.mnt 0}"
     (3 9 font-lock-type-face)
     (10 10 default)
     (11 17 clojure-keyword-face))

    ("{:oneword/CmlCase 0}"
     (3 9 font-lock-type-face)
     (10 10 default)
     (11 17 clojure-keyword-face))

    ("{:oneword/mxdCase 0}"
     (3 9 font-lock-type-face)
     (10 10 default)
     (11 17 clojure-keyword-face))

    ("{:oneword/ve/yCom|pLex.stu-ff 0}"
     (3 9 font-lock-type-face)
     (10 10 default)
     (11 29 clojure-keyword-face))

    ("{:oneword/.ve/yCom|pLex.stu-ff 0}"
     (3 9 font-lock-type-face)
     (10 10 default)
     (11 30 clojure-keyword-face)))

  (when-fontifying-it "should handle namespaced keywords"
    ("::foo"
     (1 5 clojure-keyword-face))

    (":_::_:foo"
     (1 9 clojure-keyword-face))

    (":_:_:foo"
     (1 8 clojure-keyword-face))

    (":foo/:bar"
     (1 9 clojure-keyword-face))

    ("::_:foo"
     (1 7 clojure-keyword-face))

    ("::_:_:foo"
     (1 9 clojure-keyword-face))

    (":_:_:foo/_"
     (1 1 clojure-keyword-face)
     (2 8 font-lock-type-face)
     (9 9 default)
     (10 10 clojure-keyword-face))

    (":_:_:foo/bar"
     (10 12 clojure-keyword-face))

    (":_:_:foo/bar/eee"
     (10 16 clojure-keyword-face))

    (":_:_:foo/bar_:foo"
     (10 17 clojure-keyword-face))

    (":_:_:foo/bar_:_:foo"
     (10 19 clojure-keyword-face)))

  (when-fontifying-it "should handle segment keywords"
    (" :seg.mnt"
     (3 9 clojure-keyword-face))

    ("{:seg.mnt 0}"
     (3 9 clojure-keyword-face))

    ("{:#seg.mnt 0}"
     (3 10 clojure-keyword-face))

    ("{:.seg.mnt 0}"
     (3 10 clojure-keyword-face))

    ("{:seg.mnt/oneword 0}"
     (3 9 font-lock-type-face)
     (10 10 default)
     (11 17 clojure-keyword-face))

    ("{:seg.mnt/seg.mnt 0}"
     (3 9 font-lock-type-face )
     (10 10 default)
     (11 17 clojure-keyword-face))

    ("{:seg.mnt/CmlCase 0}"
     (3 9 font-lock-type-face)
     (10 10 default)
     (11 17 clojure-keyword-face))

    ("{:seg.mnt/mxdCase 0}"
     (3 9 font-lock-type-face)
     (10 10 default)
     (11 17 clojure-keyword-face))

    ("{:seg.mnt/ve/yCom|pLex.stu-ff 0}"
     (3 9 font-lock-type-face)
     (10 10 default)
     (11 29 clojure-keyword-face))

    ("{:seg.mnt/.ve/yCom|pLex.stu-ff 0}"
     (3 9 font-lock-type-face)
     (10 10 default)
     (11 30 clojure-keyword-face)))

  (when-fontifying-it "should handle camel case keywords"
    (" :CmlCase"
     (3 9 clojure-keyword-face))

    ("{:CmlCase 0}"
     (3 9 clojure-keyword-face))

    ("{:#CmlCase 0}"
     (3 10 clojure-keyword-face))

    ("{:.CmlCase 0}"
     (3 10 clojure-keyword-face))

    ("{:CmlCase/oneword 0}"
     (3 9 font-lock-type-face)
     (10 10 default)
     (11 17 clojure-keyword-face))

    ("{:CmlCase/seg.mnt 0}"
     (3 9 font-lock-type-face)
     (10 10 default)
     (11 17 clojure-keyword-face))

    ("{:CmlCase/CmlCase 0}"
     (3 9 font-lock-type-face)
     (10 10 default)
     (11 17 clojure-keyword-face))

    ("{:CmlCase/mxdCase 0}"
     (3 9 font-lock-type-face)
     (10 10 default)
     (11 17 clojure-keyword-face))

    ("{:CmlCase/ve/yCom|pLex.stu-ff 0}"
     (3 9 font-lock-type-face)
     (10 10 default)
     (11 29 clojure-keyword-face))

    ("{:CmlCase/.ve/yCom|pLex.stu-ff 0}"
     (3 9 font-lock-type-face)
     (10 10 default)
     (11 30 clojure-keyword-face)))

  (when-fontifying-it "should handle mixed case keywords"
    (" :mxdCase"
     (3 9 clojure-keyword-face))

    ("{:mxdCase 0}"
     (3 9 clojure-keyword-face))

    ("{:#mxdCase 0}"
     (3 10 clojure-keyword-face))

    ("{:.mxdCase 0}"
     (3 10 clojure-keyword-face))

    ("{:mxdCase/oneword 0}"
     (3 9 font-lock-type-face)
     (10 10 default)
     (11 17 clojure-keyword-face))

    ("{:mxdCase/seg.mnt 0}"
     (3 9 font-lock-type-face)
     (10 10 default)
     (11 17 clojure-keyword-face))

    ("{:mxdCase/CmlCase 0}"
     (3 9 font-lock-type-face)
     (10 10 default)
     (11 17 clojure-keyword-face))

    ("{:mxdCase/mxdCase 0}"
     (3 9 font-lock-type-face)
     (10 10 default)
     (11 17 clojure-keyword-face))

    ("{:mxdCase/ve/yCom|pLex.stu-ff 0}"
     (3 9 font-lock-type-face)
     (10 10 default)
     (11 29 clojure-keyword-face))

    ("{:mxdCase/.ve/yCom|pLex.stu-ff 0}"
     (3 9 font-lock-type-face)
     (10 10 default)
     (11 30 clojure-keyword-face)))

  (when-fontifying-it "should handle very complex keywords"
    (" :ve/yCom|pLex.stu-ff"
     (3 4 font-lock-type-face)
     (5 5 default)
     (6 21 clojure-keyword-face))

    ("{:ve/yCom|pLex.stu-ff 0}"
     (2 2 clojure-keyword-face)
     (3 4 font-lock-type-face)
     (5 5 default)
     (6 21 clojure-keyword-face))

    ("{:#ve/yCom|pLex.stu-ff 0}"
     (2 2 clojure-keyword-face)
     (3 5 font-lock-type-face)
     (6 6 default)
     (7 22 clojure-keyword-face))

    ("{:.ve/yCom|pLex.stu-ff 0}"
     (2 2 clojure-keyword-face)
     (3 5 font-lock-type-face)
     (6 6 default)
     (7 22 clojure-keyword-face))

    ("{:ve/yCom|pLex.stu-ff/oneword 0}"
     (2 2 clojure-keyword-face)
     (3 4 font-lock-type-face)
     (5 5 default)
     (6 29 clojure-keyword-face))

    ("{:ve/yCom|pLex.stu-ff/seg.mnt 0}"
     (2 2 clojure-keyword-face)
     (3 4 font-lock-type-face)
     (5 5 default)
     (6 29 clojure-keyword-face))

    ("{:ve/yCom|pLex.stu-ff/ClmCase 0}"
     (2 2 clojure-keyword-face)
     (3 4 font-lock-type-face)
     (5 5 default)
     (6 29 clojure-keyword-face))

    ("{:ve/yCom|pLex.stu-ff/mxdCase 0}"
     (2 2 clojure-keyword-face)
     (3 4 font-lock-type-face)
     (5 5 default)
     (6 29 clojure-keyword-face))

    ("{:ve/yCom|pLex.stu-ff/ve/yCom|pLex.stu-ff 0}"
     (2 2 clojure-keyword-face)
     (3 4 font-lock-type-face)
     (5 5 default)
     (6 41 clojure-keyword-face))

    ("{:ve/yCom|pLex.stu-ff/.ve/yCom|pLex.stu-ff 0}"
     (2 2 clojure-keyword-face)
     (3 4 font-lock-type-face)
     (5 5 default)
     (6 42 clojure-keyword-face)))

  (when-fontifying-it "should handle namespaced defs"
    ("(_c4/defconstrainedfn bar [] nil)"
     (2 4 font-lock-type-face)
     (5 5 nil)
     (6 18 font-lock-keyword-face)
     (23 25 font-lock-function-name-face))

    ("(clo/defbar foo nil)"
     (2 4 font-lock-type-face)
     (5 5 nil)
     (6 11 font-lock-keyword-face)
     (13 15 font-lock-function-name-face))

    ("(s/def ::keyword)"
     (2 2 font-lock-type-face)
     (3 3 nil)
     (4 6 font-lock-keyword-face)
     (8 16 clojure-keyword-face)))

  (when-fontifying-it "should handle variables defined with def"
    ("(def foo 10)"
     (2 4 font-lock-keyword-face)
     (6 8 font-lock-variable-name-face)))

  (when-fontifying-it "should handle variables definitions of type string"
    ("(def foo \"hello\")"
     (10 16 font-lock-string-face))

    ("(def foo \"hello\"   )"
     (10 16 font-lock-string-face))

    ("(def foo \n  \"hello\")"
     (13 19 font-lock-string-face))

    ("(def foo \n  \"hello\"\n)"
     (13 19 font-lock-string-face)))

  (when-fontifying-it "variable-def-string-with-docstring"
    ("(def foo \"usage\" \"hello\")"
     (10 16 font-lock-doc-face)
     (18 24 font-lock-string-face))

    ("(def foo \"usage\" \"hello\"   )"
     (18 24 font-lock-string-face))

    ("(def foo \"usage\" \n  \"hello\")"
     (21 27 font-lock-string-face))

    ("(def foo \n  \"usage\" \"hello\")"
     (13 19 font-lock-doc-face))

    ("(def foo \n  \"usage\" \n  \"hello\")"
     (13 19 font-lock-doc-face)
     (24 30 font-lock-string-face))

    ("(def test-string\n  \"this\\n\n  is\n  my\n  string\")"
     (20 24 font-lock-string-face)
     (25 26 (bold font-lock-string-face))
     (27 46 font-lock-string-face)))

  (when-fontifying-it "should handle deftype"
    ("(deftype Foo)"
     (2 8 font-lock-keyword-face)
     (10 12 font-lock-type-face)))

  (when-fontifying-it "should handle defn"
    ("(defn foo [x] x)"
     (2 5 font-lock-keyword-face)
     (7 9 font-lock-function-name-face)))

  (when-fontifying-it "should handle a custom def with special chars 1"
    ("(defn* foo [x] x)"
     (2 6 font-lock-keyword-face)
     (8 10 font-lock-function-name-face)))

  (when-fontifying-it "should handle a custom def with special chars 2"
    ("(defsomething! foo [x] x)"
     (2 14 font-lock-keyword-face)
     (16 18 font-lock-function-name-face)))

  (when-fontifying-it "should handle a custom def with special chars 3"
    ("(def-something foo [x] x)"
     (2 14 font-lock-keyword-face))

    ("(def-something foo [x] x)"
     (16 18 font-lock-function-name-face)))

  (when-fontifying-it "should handle fn"
    ;; try to byte-recompile the clojure-mode.el when the face of 'fn' is 't'
    ("(fn foo [x] x)"
     (2 3 font-lock-keyword-face)
     ( 5 7 font-lock-function-name-face)))

  (when-fontifying-it "should handle lambda-params"
    ("#(+ % %2 %3 %&)"
     (5 5 font-lock-variable-name-face)
     (7 8 font-lock-variable-name-face)
     (10 11 font-lock-variable-name-face)
     (13 14 font-lock-variable-name-face)))

  (when-fontifying-it "should handle nils"
    ("(= nil x)"
     (4 6 font-lock-constant-face))

    ("(fnil x)"
     (3 5 nil)))

  (when-fontifying-it "should handle true"
    ("(= true x)"
     (4 7 font-lock-constant-face)))

  (when-fontifying-it "should handle false"
    ("(= false x)"
     (4 8 font-lock-constant-face)))

  (when-fontifying-it "should handle keyword-meta"
    ("^:meta-data"
     (1 1 nil)
     (2 11 clojure-keyword-face)))

  (when-fontifying-it "should handle a keyword with allowed characters"
    (":aaa#bbb"
     (1 8 clojure-keyword-face)))

  (when-fontifying-it "should handle a keyword with disallowed characters"
    (":aaa@bbb"
     (1 5 various-faces))

    (":aaa@bbb"
     (1 4 clojure-keyword-face))

    (":aaa~bbb"
     (1 5 various-faces))

    (":aaa~bbb"
     (1 4 clojure-keyword-face))

    (":aaa@bbb"
     (1 5 various-faces))

    (":aaa@bbb"
     (1 4 clojure-keyword-face)))

  (when-fontifying-it "should handle characters"
    ("\\a"
     (1 2 clojure-character-face))

    ("\\A"
     (1 2 clojure-character-face))

    ("\\newline"
     (1 8 clojure-character-face))

    ("\\abc"
     (1 4 nil))

    ("\\newlin"
     (1 7 nil))

    ("\\newlinex"
     (1 9 nil))

    ("\\1"
     (1 2 clojure-character-face))

    ("\\u0032"
     (1 6 clojure-character-face))

    ("\\o127"
     (1 4 clojure-character-face))

    ("\\+"
     (1 2 clojure-character-face))

    ("\\."
     (1 2 clojure-character-face))

    ("\\,"
     (1 2 clojure-character-face))

    ("\\;"
     (1 2 clojure-character-face))

    ("\\Ω"
     (1 2 clojure-character-face))

    ("\\ク"
     (1 2 clojure-character-face)))

  (when-fontifying-it "should handle characters not by themselves"
    ("[\\,,]"
     (1 1 nil)
     (2 3 clojure-character-face)
     (4 5 nil))

    ("[\\[]"
     (1 1 nil)
     (2 3 clojure-character-face)
     (4 4 nil)))

  (when-fontifying-it "should handle % character literal"
    ("#(str \\% %)"
     (7 8 clojure-character-face)
     (10 10 font-lock-variable-name-face)))

  (when-fontifying-it "should handle referred vars"
    ("foo/var"
     (1 3 font-lock-type-face))

    ("@foo/var"
     (2 4 font-lock-type-face)))

  (when-fontifying-it "should handle dynamic vars"
    ("*some-var*"
     (1 10 font-lock-variable-name-face))

    ("@*some-var*"
     (2 11 font-lock-variable-name-face))

    ("some.ns/*var*"
     (9 13 font-lock-variable-name-face))

    ("*some-var?*"
     (1 11 font-lock-variable-name-face))))

(provide 'clojure-mode-font-lock-test)

;;; clojure-mode-font-lock-test.el ends here
