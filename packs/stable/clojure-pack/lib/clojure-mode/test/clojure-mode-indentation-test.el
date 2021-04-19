;;; clojure-mode-indentation-test.el --- Clojure Mode: indentation tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2020 Bozhidar Batsov <bozhidar@batsov.com>

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
(require 's)

(defmacro when-indenting-with-point-it (description before after)
  "Return a buttercup spec.

Check whether the swift indentation command will correctly change the buffer.
Will also check whether point is moved to the expected position.

BEFORE is the buffer string before indenting, where a pipe (|) represents
point.

AFTER is the expected buffer string after indenting, where a pipe (|)
represents the expected position of point.

DESCRIPTION is a string with the description of the spec."
  (declare (indent 1))
  `(it ,description
    (let* ((after ,after)
           (clojure-indent-style 'always-align)
           (expected-cursor-pos (1+ (s-index-of "|" after)))
           (expected-state (delete ?| after)))
      (with-clojure-buffer ,before
        (goto-char (point-min))
        (search-forward "|")
        (delete-char -1)
        (font-lock-ensure)
        (indent-according-to-mode)
        (expect (buffer-string) :to-equal expected-state)
        (expect (point) :to-equal expected-cursor-pos)))))

;; Backtracking indent
(defmacro when-indenting-it (description &optional style &rest forms)
  "Return a buttercup spec.

Check that all FORMS correspond to properly indented sexps.

STYLE allows overriding the default clojure-indent-style 'always-align.

DESCRIPTION is a string with the description of the spec."
  (declare (indent 1))
  (when (stringp style)
    (setq forms (cons style forms))
    (setq style '(quote always-align)))
  `(it ,description
     (progn
       ,@(mapcar (lambda (form)
                   `(with-temp-buffer
                      (clojure-mode)
                      (insert "\n" ,form);,(replace-regexp-in-string "\n +" "\n " form))
                      (let ((clojure-indent-style ,style))
                        (indent-region (point-min) (point-max)))
                      (expect (buffer-string) :to-equal ,(concat "\n" form))))
                 forms))))

(defmacro when-aligning-it (description &rest forms)
  "Return a buttercup spec.

Check that all FORMS correspond to properly indented sexps.

DESCRIPTION is a string with the description of the spec."
  (declare (indent defun))
  `(it ,description
     (let ((clojure-align-forms-automatically t)
           (clojure-align-reader-conditionals t))
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

;; Provide font locking for easier test editing.

(font-lock-add-keywords
 'emacs-lisp-mode
 `((,(rx "(" (group "when-indenting-with-point-it") eow)
    (1 font-lock-keyword-face))
   (,(rx "("
         (group "when-indenting-with-point-it") (+ space)
         (group bow (+ (not space)) eow)
         )
    (1 font-lock-keyword-face)
    (2 font-lock-function-name-face))))

(describe "indentation"
  (it "should not hang on end of buffer"
    (with-clojure-buffer "(let [a b]"
      (goto-char (point-max))
      (expect
       (with-timeout (2)
         (newline-and-indent)
         t))))

  (when-indenting-with-point-it "should have no indentation at top level"
    "|x"

    "|x")

  (when-indenting-with-point-it "should indent cond"
    "
    (cond
    |x)"

    "
    (cond
      |x)")

  (when-indenting-with-point-it "should indent cond-> with a namespaced map"
    "
(cond-> #:a{:b 1}
|x 1)"

    "
(cond-> #:a{:b 1}
  |x 1)")

  (when-indenting-with-point-it "should indent cond-> with a namespaced map 2"
    "
(cond-> #::a{:b 1}
|x 1)"

    "
(cond-> #::a{:b 1}
  |x 1)")

  (when-indenting-with-point-it "should indent threading macro with expression on first line"
    "
    (->> expr
     |ala)"

    "
    (->> expr
         |ala)")

  (when-indenting-with-point-it "should indent threading macro with expression on second line"
    "
    (->>
    |expr)"

    "
    (->>
     |expr)")

  (when-indenting-with-point-it "should not indent for def string"
    "(def foo \"hello|\")"
    "(def foo \"hello|\")")

  (when-indenting-with-point-it "should indent doc strings"
    "
    (defn some-fn
    |\"some doc string\")"
    "
    (defn some-fn
      |\"some doc string\")")

  (when-indenting-with-point-it "should not indent doc strings when correct indent already specified"
    "
    (defn some-fn
      |\"some doc string\")"
    "
    (defn some-fn
      |\"some doc string\")")

  (when-indenting-with-point-it "should handle doc strings with additional indent specified"
    "
    (defn some-fn
      |\"some doc string
        - some note\")"
    "
    (defn some-fn
      |\"some doc string
        - some note\")")

  (describe "specify different indentation for symbol with some ns prefix"
    (put-clojure-indent 'bala 0)
    (put-clojure-indent 'ala/bala 1)

    (when-indenting-with-point-it "should handle a symbol without ns"
      "
      (bala
      |one)"
      "
      (bala
        |one)")

    (when-indenting-with-point-it "should handle a symbol with ns"
      "
      (ala/bala top
      |one)"
      "
      (ala/bala top
        |one)"))

  (describe "we can pass a lambda to explicitly set the column"
    (put-clojure-indent 'arsymbol (lambda (indent-point state) 0))

    (when-indenting-with-point-it "should handle a symbol with lambda"
      "
(arsymbol
|one)"
      "
(arsymbol
|one)"))

  (when-indenting-with-point-it "should indent a form with metadata"
    "
    (ns ^:doc app.core
    |(:gen-class))"
    "
    (ns ^:doc app.core
      |(:gen-class))")

  (when-indenting-with-point-it "should handle multiline sexps"
    "
    [[
      2] a
    |x]"
    "
    [[
      2] a
     |x]")

  (when-indenting-with-point-it "should indent reader conditionals"
    "
    #?(:clj :foo
    |:cljs :bar)"
    "
    #?(:clj :foo
       |:cljs :bar)")

  (when-indenting-with-point-it "should handle backtracking with aliases"
    "
    (clojure.core/letfn [(twice [x]
    |(* x 2))]
      :a)"
    "
    (clojure.core/letfn [(twice [x]
                           |(* x 2))]
      :a)")

  (when-indenting-with-point-it "should handle fixed-normal-indent"
    "(cond
      (or 1
          2) 3
    |:else 4)"

    "(cond
      (or 1
          2) 3
      |:else 4)")

  (when-indenting-with-point-it "should handle fixed-normal-indent-2"
    "(fact {:spec-type
       :charnock-column-id} #{\"charnock\"}
|{:spec-type
       :charnock-column-id} #{\"current_charnock\"})"

    "(fact {:spec-type
       :charnock-column-id} #{\"charnock\"}
      |{:spec-type
       :charnock-column-id} #{\"current_charnock\"})")

  (when-indenting-it "closing-paren"
    "(ns ca
  (:gen-class)
  )")

  (when-indenting-it "default-is-not-a-define"
    "(default a
         b
         b)"
    "(some.namespace/default a
                        b
                        b)")


  (when-indenting-it "should handle extend-type with multiarity"
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


  (when-indenting-it "should handle deftype with multiarity"
    "(deftype Banana []
  Fruit
  (subtotal
    ([item]
     (* 158 (:qty item)))
    ([item a]
     (* a (:qty item)))))")

  (when-indenting-it "should handle defprotocol"
    "(defprotocol IFoo
  (foo [this]
    \"Why is this over here?\")
  (foo-2
    [this]
    \"Why is this over here?\"))")


  (when-indenting-it "should handle definterface"
    "(definterface IFoo
  (foo [this]
    \"Why is this over here?\")
  (foo-2
    [this]
    \"Why is this over here?\"))")

  (when-indenting-it "should handle specify"
    "(specify obj
  ISwap
  (-swap!
    ([this f] (reset! this (f @this)))
    ([this f a] (reset! this (f @this a)))
    ([this f a b] (reset! this (f @this a b)))
    ([this f a b xs] (reset! this (apply f @this a b xs)))))")

  (when-indenting-it "should handle specify!"
    "(specify! obj
  ISwap
  (-swap!
    ([this f] (reset! this (f @this)))
    ([this f a] (reset! this (f @this a)))
    ([this f a b] (reset! this (f @this a b)))
    ([this f a b xs] (reset! this (apply f @this a b xs)))))")

  (when-indenting-it "should handle non-symbol at start"
    "{\"1\" 2
 *3 4}")

  (when-indenting-it "should handle non-symbol at start 2"
    "(\"1\" 2
 *3 4)")

  (when-indenting-it "should handle defrecord"
    "(defrecord TheNameOfTheRecord
    [a pretty long argument list]
  SomeType
  (assoc [_ x]
    (.assoc pretty x 10)))")

  (when-indenting-it "should handle defrecord 2"
    "(defrecord TheNameOfTheRecord [a pretty long argument list]
  SomeType (assoc [_ x]
             (.assoc pretty x 10)))")

  (when-indenting-it "should handle defrecord with multiarity"
    "(defrecord Banana []
  Fruit
  (subtotal
    ([item]
     (* 158 (:qty item)))
    ([item a]
     (* a (:qty item)))))")

  (when-indenting-it "should handle letfn"
    "(letfn [(f [x]
          (* x 2))
        (f [x]
          (* x 2))]
  (a b
     c) (d)
  e)")

  (when-indenting-it "should handle reify"
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

  (when-indenting-it "proxy"
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

  (when-indenting-it "should handle reader conditionals"
    "#?@ (:clj []
     :cljs [])")

  (when-indenting-it "should handle an empty close paren"
    "(let [x]
  )"

    "(ns ok
  )"

    "(ns ^{:zen :dikar}
    ok
  )")

  (when-indenting-it "should handle unfinished sexps"
    "(letfn [(tw [x]
          dd")

  (when-indenting-it "should handle symbols ending in crap"
    "(msg? ExceptionInfo
      10)"

    "(thrown-with-msg? ExceptionInfo
                  #\"Storage must be initialized before use\"
                  (f))"

    "(msg' 1
      10)")

  (when-indenting-it "should handle let, when and while forms"
    "(let-alist [x 1]\n  ())"
    "(while-alist [x 1]\n  ())"
    "(when-alist [x 1]\n  ())"
    "(if-alist [x 1]\n  ())"
    "(indents-like-fn-when-let-while-if-are-not-the-start [x 1]\n                                                     ())")

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


  (when-indenting-it "should handle function spec"
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

  (when-indenting-it "should respect indent style 'align-arguments"
    'align-arguments

    "(some-function
  10
  1
  2)"

    "(some-function 10
               1
               2)")

  (when-indenting-it "should respect indent style 'always-indent"
    'always-indent

    "(some-function
  10
  1
  2)"

    "(some-function 10
  1
  2)")

  (when-aligning-it "should basic forms"
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

  (when-aligning-it "should handle a blank line"
    "(let [this-is-a-form b
      c              d

      another form
      k       g])"

    "{:this-is-a-form b
 c               d

 :another form
 k        g}")

  (when-aligning-it "should handle basic forms (reversed)"
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

  (when-aligning-it "should handle incomplete sexps"
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


  (when-aligning-it "should handle multiple words"
    "(cond this     is    just
      a        test  of
      how      well
      multiple words will work)")

  (when-aligning-it "should handle nested maps"
    "{:a    {:a    :a
        :bbbb :b}
 :bbbb :b}")

  (when-aligning-it "should regard end as a marker"
    "{:a {:a                                :a
     :aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa :a}
 :b {:a  :a
     :aa :a}}")

  (when-aligning-it "should handle trailing commas"
    "{:a {:a  :a,
     :aa :a},
 :b {:a  :a,
     :aa :a}}")

  (when-aligning-it "should handle standard reader conditionals"
    "#?(:clj  2
   :cljs 2)")

  (when-aligning-it "should handle splicing reader conditional"
    "#?@(:clj  [2]
    :cljs [2])")

  (it "should handle improperly indented content"
    (let ((content "(let [a-long-name 10\nb 20])")
          (aligned-content "(let [a-long-name 10\n      b           20])"))
      (with-clojure-buffer content
        (call-interactively #'clojure-align)
        (expect (buffer-string) :to-equal aligned-content))))

  (it "should not align reader conditionals by default"
    (let ((content "#?(:clj 2\n   :cljs 2)"))
      (with-clojure-buffer content
        (call-interactively #'clojure-align)
        (expect (buffer-string) :to-equal content))))

  (it "should align reader conditionals when clojure-align-reader-conditionals is true"
    (let ((content "#?(:clj 2\n   :cljs 2)"))
      (with-clojure-buffer content
        (setq-local clojure-align-reader-conditionals t)
        (call-interactively #'clojure-align)
        (expect (buffer-string) :not :to-equal content))))

  (it "should remove extra commas"
    (with-clojure-buffer "{:a 2, ,:c 4}"
      (call-interactively #'clojure-align)
      (expect (string= (buffer-string) "{:a 2, :c 4}")))))

(provide 'clojure-mode-indentation-test)

;;; clojure-mode-indentation-test.el ends here
