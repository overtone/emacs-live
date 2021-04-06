;;; clojure-mode-indentation-test.el --- Clojure Mode: indentation tests  -*- lexical-binding: t; -*-

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
(require 'cl-lib)
(require 'ert)
(require 's)

(ert-deftest dont-hang-on-eob ()
  (with-temp-buffer
    (insert "(let [a b]")
    (clojure-mode)
    (goto-char (point-max))
    (should
     (with-timeout (2)
       (newline-and-indent)
       t))))

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
              (clojure-indent-style :always-align)
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
|\"some doc string\")"
  "
(defn some-fn
  |\"some doc string\")")

(check-indentation doc-strings-with-correct-indent-specified
  "
(defn some-fn
  |\"some doc string\")"
  "
(defn some-fn
  |\"some doc string\")")

(check-indentation doc-strings-with-additional-indent-specified
  "
(defn some-fn
  |\"some doc string
    - some note\")"
  "
(defn some-fn
  |\"some doc string
    - some note\")")

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

;; we can pass a lambda to explicitely set the column
(put-clojure-indent 'arsymbol (lambda (indent-point state) 0))

(check-indentation symbol-with-lambda
  "
(arsymbol
 |one)"
  "
(arsymbol
|one)")

(check-indentation form-with-metadata
  "
(ns ^:doc app.core
|(:gen-class))"
"
(ns ^:doc app.core
  |(:gen-class))")

(check-indentation multiline-sexps
  "
[[
  2] a
|x]"
"
[[
  2] a
 |x]")

(check-indentation reader-conditionals
  "
#?(:clj :foo
|:cljs :bar)"
  "
#?(:clj :foo
   |:cljs :bar)")

(check-indentation backtracking-with-aliases
  "
(clojure.core/letfn [(twice [x]
|(* x 2))]
  :a)"
  "
(clojure.core/letfn [(twice [x]
                       |(* x 2))]
  :a)")

(check-indentation fixed-normal-indent
  "(cond
  (or 1
      2) 3
|:else 4)"
  "(cond
  (or 1
      2) 3
  |:else 4)")

(check-indentation fixed-normal-indent-2
  "(fact {:spec-type
       :charnock-column-id} #{\"charnock\"}
|{:spec-type
       :charnock-column-id} #{\"current_charnock\"})"
  "(fact {:spec-type
       :charnock-column-id} #{\"charnock\"}
      |{:spec-type
       :charnock-column-id} #{\"current_charnock\"})")


;;; Backtracking indent
(defmacro def-full-indent-test (name &optional style &rest forms)
  "Verify that all FORMs correspond to a properly indented sexps."
  (declare (indent 1))
  (when (stringp style)
    (setq forms (cons style forms))
    (setq style :always-align))
  `(ert-deftest ,(intern (format "test-backtracking-%s" name)) ()
     (progn
       ,@(mapcar (lambda (form)
                   `(with-temp-buffer
                      (clojure-mode)
                      (insert "\n" ,(replace-regexp-in-string "\n +" "\n " form))
                      (let ((clojure-indent-style ,style))
                        (indent-region (point-min) (point-max)))
                      (should (equal (buffer-string)
                                     ,(concat "\n" form)))))
                 forms))))

(def-full-indent-test closing-paren
  "(ns ca
  (:gen-class)
  )")

(def-full-indent-test default-is-not-a-define
  "(default a
         b
         b)"
  "(some.namespace/default a
                        b
                        b)")

(def-full-indent-test extend-type-allow-multiarity
  "(extend-type Banana
  Fruit
  (subtotal
    ([item]
     (* 158 (:qty item)))
    ([item a]
     (* a (:qty item)))))"
  "(extend-protocol Banana
  Fruit
  (subtotal
    ([item]
     (* 158 (:qty item)))
    ([item a]
     (* a (:qty item)))))")

(def-full-indent-test deftype-allow-multiarity
  "(deftype Banana []
  Fruit
  (subtotal
    ([item]
     (* 158 (:qty item)))
    ([item a]
     (* a (:qty item)))))")

(def-full-indent-test defprotocol
  "(defprotocol IFoo
  (foo [this]
    \"Why is this over here?\")
  (foo-2
    [this]
    \"Why is this over here?\"))")


(def-full-indent-test definterface
  "(definterface IFoo
  (foo [this]
    \"Why is this over here?\")
  (foo-2
    [this]
    \"Why is this over here?\"))")

(def-full-indent-test specify
  "(specify obj
  ISwap
  (-swap!
    ([this f] (reset! this (f @this)))
    ([this f a] (reset! this (f @this a)))
    ([this f a b] (reset! this (f @this a b)))
    ([this f a b xs] (reset! this (apply f @this a b xs)))))")

(def-full-indent-test specify!
  "(specify! obj
  ISwap
  (-swap!
    ([this f] (reset! this (f @this)))
    ([this f a] (reset! this (f @this a)))
    ([this f a b] (reset! this (f @this a b)))
    ([this f a b xs] (reset! this (apply f @this a b xs)))))")

(def-full-indent-test non-symbol-at-start
  "{\"1\" 2
 *3 4}")

(def-full-indent-test non-symbol-at-start-2
  "(\"1\" 2
 *3 4)")

(def-full-indent-test defrecord
  "(defrecord TheNameOfTheRecord
    [a pretty long argument list]
  SomeType
  (assoc [_ x]
    (.assoc pretty x 10)))")

(def-full-indent-test defrecord-2
  "(defrecord TheNameOfTheRecord [a pretty long argument list]
  SomeType (assoc [_ x]
             (.assoc pretty x 10)))")

(def-full-indent-test defrecord-allow-multiarity
  "(defrecord Banana []
  Fruit
  (subtotal
    ([item]
     (* 158 (:qty item)))
    ([item a]
     (* a (:qty item)))))")

(def-full-indent-test letfn
  "(letfn [(f [x]
          (* x 2))
        (f [x]
          (* x 2))]
  (a b
     c) (d)
  e)")

(def-full-indent-test reify
  "(reify Object
  (x [_]
    1))"
  "(reify
  om/IRender
  (render [this]
    (let [indent-test :fail]
      ...))
  om/IRender
  (render [this]
    (let [indent-test :fail]
      ...)))")

(def-full-indent-test proxy
  "(proxy [Writer] []
  (close [] (.flush ^Writer this))
  (write
    ([x]
     (with-out-binding [out messages]
       (.write out x)))
    ([x ^Integer off ^Integer len]
     (with-out-binding [out messages]
       (.write out x off len))))
  (flush []
    (with-out-binding [out messages]
      (.flush out))))")

(def-full-indent-test reader-conditionals
  "#?@ (:clj []
     :cljs [])")

(def-full-indent-test empty-close-paren
  "(let [x]
  )"

  "(ns ok
  )"

  "(ns ^{:zen :dikar}
    ok
  )")

(def-full-indent-test unfinished-sexps
  "(letfn [(tw [x]
          dd")

(def-full-indent-test symbols-ending-in-crap
  "(msg? ExceptionInfo
      10)"
  "(thrown-with-msg? ExceptionInfo
                  #\"Storage must be initialized before use\"
                  (f))"
  "(msg' 1
      10)")

(def-full-indent-test let-when-while-forms
  "(let-alist [x 1]\n  ())"
  "(while-alist [x 1]\n  ())"
  "(when-alist [x 1]\n  ())")

(defun indent-cond (indent-point state)
  (goto-char (elt state 1))
  (let ((pos -1)
        (base-col (current-column)))
    (forward-char 1)
    ;; `forward-sexp' will error if indent-point is after
    ;; the last sexp in the current sexp.
    (condition-case nil
        (while (and (<= (point) indent-point)
                    (not (eobp)))
          (clojure-forward-logical-sexp 1)
          (cl-incf pos))
      ;; If indent-point is _after_ the last sexp in the
      ;; current sexp, we detect that by catching the
      ;; `scan-error'. In that case, we should return the
      ;; indentation as if there were an extra sexp at point.
      (scan-error (cl-incf pos)))
    (+ base-col (if (cl-evenp pos) 0 2))))
(put-clojure-indent 'test-cond #'indent-cond)

(defun indent-cond-0 (_indent-point _state) 0)
(put-clojure-indent 'test-cond-0 #'indent-cond-0)

(def-full-indent-test function-spec
  "(when me
  (test-cond
    x
  1
    2
  3))"
  "(when me
  (test-cond-0
x
1
2
3))")

(def-full-indent-test align-arguments
  :align-arguments
  "(some-function
  10
  1
  2)"
  "(some-function 10
               1
               2)")

(def-full-indent-test always-indent
  :always-indent
  "(some-function
  10
  1
  2)"
  "(some-function 10
  1
  2)")

;;; Alignment
(defmacro def-full-align-test (name &rest forms)
  "Verify that all FORMs correspond to a properly indented sexps."
  (declare (indent defun))
  `(ert-deftest ,(intern (format "test-align-%s" name)) ()
     (let ((clojure-align-forms-automatically t))
       ,@(mapcar (lambda (form)
                   `(with-temp-buffer
                      (clojure-mode)
                      (insert "\n" ,(replace-regexp-in-string " +" " " form))
                      (indent-region (point-min) (point-max))
                      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                                     ,(concat "\n" form)))))
                 forms))
     (let ((clojure-align-forms-automatically nil))
       ,@(mapcar (lambda (form)
                   `(with-temp-buffer
                      (clojure-mode)
                      (insert "\n" ,(replace-regexp-in-string " +" " " form))
                      ;; This is to check that we did NOT align anything. Run
                      ;; `indent-region' and then check that no extra spaces
                      ;; where inserted besides the start of the line.
                      (indent-region (point-min) (point-max))
                      (goto-char (point-min))
                      (should-not (search-forward-regexp "\\([^\s\n]\\)  +" nil 'noerror))))
                 forms))))

(def-full-align-test basic
  "{:this-is-a-form b
 c               d}"
  "{:this-is b
 c        d}"
  "{:this b
 c     d}"
  "{:a b
 c  d}"

  "(let [this-is-a-form b
      c              d])"
  "(let [this-is b
      c       d])"
  "(let [this b
      c    d])"
  "(let [a b
      c d])")

(def-full-align-test blank-line
  "(let [this-is-a-form b
      c              d

      another form
      k       g])"
  "{:this-is-a-form b
 c               d

 :another form
 k        g}")

(def-full-align-test basic-reversed
  "{c               d
 :this-is-a-form b}"
  "{c        d
 :this-is b}"
  "{c     d
 :this b}"
  "{c  d
 :a b}"

  "(let [c              d
      this-is-a-form b])"
  "(let [c       d
      this-is b])"
  "(let [c    d
      this b])"
  "(let [c d
      a b])")

(def-full-align-test incomplete-sexp
  "(cond aa b
      casodkas )"
  "(cond aa b
      casodkas)"
  "(cond aa b
      casodkas "
  "(cond aa b
      casodkas"
  "(cond aa       b
      casodkas a)"
  "(cond casodkas a
      aa       b)"
  "(cond casodkas
      aa b)")

(def-full-align-test multiple-words
  "(cond this     is    just
      a        test  of
      how      well
      multiple words will work)")

(def-full-align-test nested-maps
  "{:a    {:a    :a
        :bbbb :b}
 :bbbb :b}")

(def-full-align-test end-is-a-marker
  "{:a {:a                                :a
     :aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa :a}
 :b {:a  :a
     :aa :a}}")

(def-full-align-test trailing-commas
  "{:a {:a  :a,
     :aa :a},
 :b {:a  :a,
     :aa :a}}")

(ert-deftest clojure-align-remove-extra-commas ()
  (with-temp-buffer
    (clojure-mode)
    (insert "{:a 2, ,:c 4}")
    (call-interactively #'clojure-align)
    (should (string= (buffer-string) "{:a 2, :c 4}"))))

(provide 'clojure-mode-indentation-test)

;;; clojure-mode-indentation-test.el ends here
