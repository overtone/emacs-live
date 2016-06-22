;;; tests/indent.el --- Some tests for js2-mode.

;; Copyright (C) 2009, 2011-2013  Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'js2-mode)
(require 'cl-lib)
(require 'js2-old-indent)

(defun js2-test-indent (content keep-indent)
  (let ((s (replace-regexp-in-string "^ *|" "" content)))
    (with-temp-buffer
      (insert
       (if keep-indent
           s
         (replace-regexp-in-string "^ *" "" s)))
      (js2-jsx-mode)
      (js2-reparse) ; solely for js2-jsx-self-closing, for some reason
      (indent-region (point-min) (point-max))
      (should (string= s (buffer-substring-no-properties
                          (point-min) (point)))))))

(cl-defmacro js2-deftest-indent (name content &key bind keep-indent)
  `(ert-deftest ,(intern (format "js2-%s" name)) ()
     (let ,(append '(indent-tabs-mode
                     (js2-basic-offset 2)
                     (js2-pretty-multiline-declarations t)
                     (inhibit-point-motion-hooks t))
                   bind)
       (js2-test-indent ,content ,keep-indent))))

(put 'js2-deftest-indent 'lisp-indent-function 'defun)

(js2-deftest-indent no-multiline-decl-indent-after-semicolon
  "var foo = 1;
  |bar = 2")

(js2-deftest-indent multiline-decl-indent-after-comma
  "let foo = 1,
  |    bar = 2")

(js2-deftest-indent no-multiline-decl-when-disabled
  "let foo = 1,
  |bar = 2"
  :bind ((js2-pretty-multiline-declarations nil)))

(js2-deftest-indent multiline-decl-with-continued-expr
  "var foo = 100500
  |      + 1")

(js2-deftest-indent multiline-decl-with-continued-expr-same-line
  "var foo = 100500 /
  |      16;")

(js2-deftest-indent no-multiline-decl-with-operator-inside-string
  "var foo = bar('/protocols/')
  |baz()")

(js2-deftest-indent no-multiline-decl-implicit-semicolon
  "var foo = 100500
  |1")

(js2-deftest-indent multiline-decl-sees-keyword-width
  "const foo = 1,
  |      bar = 2;")

(js2-deftest-indent multiline-decl-second-arg-value-parenthesised
  "var foo = 1,
  |    bar = [
  |      1, 2,
  |      3, 4
  |    ],
  |    baz = 5;")

(js2-deftest-indent multiline-decl-first-arg-function-normal
  "var foo = function() {
  |  return 7;
  |},
  |    bar = 8;")

(js2-deftest-indent multiline-decl-first-arg-function-indent-all
  "var foo = function() {
  |      return 7;
  |    },
  |    bar = 8;"
  :bind ((js2-pretty-multiline-declarations 'all)))

(js2-deftest-indent default-keyword-as-property
  "var foo = {
  |  case: 'zzzz',
  |  default: 'donkey',
  |  tee: 'ornery'
  |};")

(js2-deftest-indent multiline-string-noop
  "`multiline string
  |       contents
  |  are kept
  |        unchanged!`"
  :keep-indent t)

(js2-deftest-indent no-multiline-decl-first-arg-function-dynamic
  "var foo = function() {
  |  return 7;
  |};"
  :bind ((js2-pretty-multiline-declarations 'dynamic)))

(js2-deftest-indent multiline-decl-first-arg-function-indent-dynamic
  "var foo = function() {
  |      return 7;
  |    },
  |    bar = 8;"
  :bind ((js2-pretty-multiline-declarations 'dynamic)))

(js2-deftest-indent multiline-decl-first-arg-function-indent-dynamic-comment
  "var foo = function() {
  |      return 7;
  |    }/* MUAHAHAHA, ah ha! */,
  |    bar = 8;"
  :bind ((js2-pretty-multiline-declarations 'dynamic)))

(js2-deftest-indent multiline-decl-first-arg-function-indent-dynamic-scan-error
  "var foo = function() {
  |  return 7;
  |  ,
  |  bar = 8;"
  :bind ((js2-pretty-multiline-declarations 'dynamic)))

(js2-deftest-indent indent-generator-method
  "class A {
  |  * x() {
  |    return 1
  |      * a(2);
  |  }
  |}")

(js2-deftest-indent indent-generator-computed-method
  "class A {
  |  *[Symbol.iterator]() {
  |    yield 'Foo';
  |    yield 'Bar';
  |  }
  |}")

(js2-deftest-indent case-inside-switch
  "switch(true) {
  |case 'true':
  |  return 1;
  |}")

(js2-deftest-indent case-inside-switch-with-extra-indent
  "switch(true) {
  |  case 'true':
  |    return 1;
  |}"
  :bind ((js2-indent-switch-body t)))

(js2-deftest-indent case-inside-switch-with-extra-indent-curly-after-newline
  "switch(true)
  |{
  |  case 'true':
  |    return 1;
  |}"
  :bind ((js2-indent-switch-body t)))

(js2-deftest-indent continued-expression-vs-unary-minus
  "var arr = [
  |  -1, 2,
  |  -3, 4 +
  |    -5
  |];")

(js2-deftest-indent spread-inside-array
  "var z = [
  |  ...iterableObj,
  |  4,
  |  5
  |]")

(js2-deftest-indent jsx-one-line
  "var foo = <div></div>;")

(js2-deftest-indent jsx-children-parentheses
  "return (
  |  <div>
  |  </div>
  |  <div>
  |    <div></div>
  |    <div>
  |      <div></div>
  |    </div>
  |  </div>
  |);")

(js2-deftest-indent jsx-children-unclosed
  "return (
  |  <div>
  |    <div>")

(js2-deftest-indent jsx-argument
  "React.render(
  |  <div>
  |    <div></div>
  |  </div>,
  |  {
  |    a: 1
  |  },
  |  <div>
  |    <div></div>
  |  </div>
  |);")

(js2-deftest-indent jsx-leading-comment
  "return (
  |  // Sneaky!
  |  <div></div>
  |);")

(js2-deftest-indent jsx-trailing-comment
  "return (
  |  <div></div>
  |  // Sneaky!
  |);")

(js2-deftest-indent jsx-self-closing
  ;; This ensures we know the bounds of a self-closing element
  "React.render(
  |  <input
  |     />,
  |  {
  |    a: 1
  |  }
  |);"
  :bind ((sgml-attribute-offset 1))) ; Emacs 24.5 -> 25 compat

(js2-deftest-indent jsx-embedded-js-content
  "return (
  |  <div>
  |    {array.map(function () {
  |      return {
  |        a: 1
  |      };
  |    })}
  |  </div>
  |);")

(js2-deftest-indent jsx-embedded-js-unclosed
  "return (
  |  <div>
  |    {array.map(function () {
  |      return {
  |        a: 1")

(js2-deftest-indent jsx-embedded-js-attribute
  "return (
  |  <div attribute={array.map(function () {
  |         return {
  |           a: 1
  |         };
  |
  |         return {
  |           a: 1
  |         };
  |
  |         return {
  |           a: 1
  |         };
  |       })}>
  |  </div>
  |);")
