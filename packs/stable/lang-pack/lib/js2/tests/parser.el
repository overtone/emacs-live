;;; tests/parser.el --- Some tests for js2-mode.

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
(require 'ert-x)
(require 'js2-mode)
(require 'cl-lib)

(defmacro js2-deftest (name buffer-contents &rest body)
  (declare (indent defun))
  `(ert-deftest ,(intern (format "js2-%s" name)) ()
     (with-temp-buffer
       (save-excursion
         (insert ,buffer-contents))
       (unwind-protect
           (progn
             ,@body)
         (fundamental-mode)))))

(defun js2-mode--and-parse ()
  (js2-mode)
  (js2-reparse))

(defun js2-test-string-to-ast (s)
  (insert s)
  (js2-mode--and-parse)
  (should (null js2-mode-buffer-dirty-p))
  js2-mode-ast)

(cl-defun js2-test-parse-string (code-string &key syntax-error errors-count
                                             reference warnings-count)
  (ert-with-test-buffer (:name 'origin)
    (let ((ast (js2-test-string-to-ast code-string)))
      (if syntax-error
          (let ((errors (js2-ast-root-errors ast)))
            (should (= (or errors-count 1) (length errors)))
            (cl-destructuring-bind (_ pos len) (car (last errors))
              (should (string= syntax-error (substring code-string
                                                       (1- pos) (+ pos len -1))))))
        (should (= 0 (length (js2-ast-root-errors ast))))
        (ert-with-test-buffer (:name 'copy)
          (js2-print-tree ast)
          (skip-chars-backward " \t\n")
          (should (string= (or reference code-string)
                           (buffer-substring-no-properties
                            (point-min) (point)))))
        (when warnings-count
          (should (= warnings-count
                     (length (js2-ast-root-warnings ast)))))))))

(cl-defmacro js2-deftest-parse (name code-string &key bind syntax-error errors-count
                                     reference warnings-count)
  "Parse CODE-STRING.  If SYNTAX-ERROR is nil, print syntax tree
with `js2-print-tree' and assert the result to be equal to
REFERENCE, if present, or the original string.  If SYNTAX-ERROR
is passed, expect syntax error highlighting substring equal to
SYNTAX-ERROR value.  BIND defines bindings to apply them around
the test."
  (declare (indent defun))
  `(ert-deftest ,(intern (format "js2-%s" name)) ()
     (let ,(append bind '((js2-basic-offset 2)))
       (js2-test-parse-string ,code-string
                              :syntax-error ,syntax-error
                              :errors-count ,errors-count
                              :warnings-count ,warnings-count
                              :reference ,reference))))

;;; Basics

(js2-deftest-parse variable-assignment
  "a = 1;")

(js2-deftest-parse empty-object-literal
  "b = {};")

(js2-deftest-parse empty-array-literal
  "c = [];")

(js2-deftest-parse array-with-missing-elements
  "var a = [1, 2, ,];")

(js2-deftest-parse comma-after-regexp
  "d = /eee/, 42;")

(js2-deftest-parse return-statement
  "function foo() {\n  return 2;\n}")

(js2-deftest-parse function-statement
  "function foo() {\n}")

(js2-deftest-parse function-statement-inside-block
  "if (true) {\n  function foo() {\n  }\n}")

(js2-deftest-parse function-expression-statements-are-verboten
  "function() {}" :syntax-error "(")

(js2-deftest-parse member-expr-as-function-name
  "function a.b.c[2](x, y) {\n}"
  :bind ((js2-allow-member-expr-as-function-name t)))

(js2-deftest-parse named-function-expression
  "a = function b() {};")

(js2-deftest-parse parenthesized-expression
  "(1 + 2);")

(js2-deftest-parse for-with-in-operator-in-parens
  "for (var y = (0 in []) in {}) {\n}")

(js2-deftest-parse for-with-in-operator-in-cond
  "for (var y = 1 ? 0 in [] : false in {}) {\n}")

(js2-deftest-parse let-expression
  "(let (x = 42) x);")

(js2-deftest-parse let-expression-statement
  "let (x = 42) x;")

(js2-deftest-parse void
  "void 0;")

;;; Callers of `js2-valid-prop-name-token'

(js2-deftest-parse parse-property-access-when-not-keyword
  "A.foo = 3;")

(js2-deftest-parse parse-property-access-when-keyword
  "A.in = 3;"
  :bind ((js2-allow-keywords-as-property-names t)))

(js2-deftest-parse parse-property-access-when-keyword-no-xml
  "A.in = 3;"
  :bind ((js2-allow-keywords-as-property-names t)
         (js2-compiler-xml-available nil)))

(js2-deftest-parse parse-object-literal-when-not-keyword
  "a = {b: 1};")

(js2-deftest-parse parse-object-literal-when-keyword
  "a = {in: 1};"
  :bind ((js2-allow-keywords-as-property-names t)))

;;; 'of' contextual keyword

(js2-deftest-parse parse-legacy-array-comp-loop-with-of
  "[a for (a of [])];")

(js2-deftest-parse parse-array-comp-loop
  "[for (a of []) a];")

(js2-deftest-parse parse-for-of
  "for (var a of []) {\n}")

(js2-deftest-parse of-can-be-name
  "void of;")

(js2-deftest-parse of-can-be-object-name
  "of.z;")

(js2-deftest-parse of-can-be-var-name
  "var of = 3;")

(js2-deftest-parse of-can-be-function-name
  "function of() {\n}")

;;; Destructuring binding

(js2-deftest-parse destruct-in-declaration
  "var {a, b} = {a: 1, b: 2};"
  :warnings-count 0)

(js2-deftest-parse destruct-in-arguments
  "function f({a: aa, b: bb}) {\n}"
  :warnings-count 0)

(js2-deftest-parse destruct-in-array-comp-loop
  "[a + b for ([a, b] in [[0, 1], [1, 2]])];")

(js2-deftest-parse destruct-in-catch-clause
  "try {\n} catch ({a, b}) {\n  a + b;\n}"
  :warnings-count 0)

(js2-deftest-parse destruct-with-initializer-in-object
  "var {a, b = 2, c} = {};\nb;"
  :warnings-count 0)

(js2-deftest-parse destruct-with-initializer-in-array
  "var [a, b = 2, c] = [];\nb;"
  :warnings-count 0)

(js2-deftest-parse destruct-non-name-target-is-error
  "var {1=1} = {};" :syntax-error "1" :errors-count 1)

(js2-deftest-parse destruct-with-initializer-in-function-params
  "function f({a, b = 1, c}, [d, e = 1, f]) {\n}")

(js2-deftest-parse destruct-with-default-in-function-params
  "function f({x = 1, y = 2} = {}, [x, y] = [1, 2]) {\n}")

(js2-deftest-parse destruct-name-conflict-is-error-in-object
  "\"use strict\";\nvar {a=1,a=2} = {};" :syntax-error "a" :errors-count 1)

(js2-deftest destruct-name-conflict-is-warning-in-array "\"use strict\";\nvar [a=1,a=2] = [];"
  (js2-mode--and-parse)
  (should (equal '("msg.var.redecl" "a")
                 (caar js2-parsed-warnings))))

(js2-deftest initializer-outside-destruct-is-error "({a=1});"
  (js2-mode--and-parse)
  (should (equal "msg.init.no.destruct"
                 (car (caar js2-parsed-errors)))))

;;; Object literals

(js2-deftest-parse object-literal-shorthand
  "var x = {a: 1, b, c: 1, d};")

(js2-deftest-parse object-literal-shorthard-with-number
  "var a = {1};" :syntax-error "}" :errors-count 2)

(js2-deftest-parse object-literal-method
  "var x = {f(y) {  return y;\n}};")

(js2-deftest object-literal-method-own-name-in-scope "({f(){f();}});"
  (js2-mode--and-parse)
  (should (equal '("msg.undeclared.variable" "f")
                 (caar js2-parsed-warnings))))

(js2-deftest-parse object-literal-getter-method
  "var x = {get f() {  return 42;\n}};")

(js2-deftest-parse object-literal-setter-method
  "var x = {set f(y) {  x = y;\n}};")

(js2-deftest-parse object-literal-computed-keys
  "var x = {[Symbol.iterator]: function() {}};")

(js2-deftest-parse object-literal-computed-function-keys
  "var x = {[foo + bar](y) {  return y;\n}};")

(js2-deftest-parse object-literal-computed-getter-key
  "var x = {get [foo + bar]() {  return 42;\n}};")

(js2-deftest-parse object-literal-generator
  "var x = {*foo() {  yield* 42;\n}};")

(js2-deftest-parse object-literal-computed-generator-key
  "var x = {*[foo + bar]() {  yield 42;\n}};")

;;; Function definition

(js2-deftest function-redeclaring-var "var gen = 3; function gen() {};"
  (js2-mode--and-parse)
  (should (= (length (js2-ast-root-warnings js2-mode-ast)) 1)))

(js2-deftest function-expression-var-same-name "var gen = function gen() {};"
  (js2-mode--and-parse)
  (should (null (js2-ast-root-warnings js2-mode-ast))))

;;; Function parameters

(js2-deftest-parse function-with-default-parameters
  "function foo(a = 1, b = a + 1) {\n}")

(js2-deftest-parse function-with-no-default-after-default
  "function foo(a = 1, b) {\n}")

(js2-deftest-parse function-with-destruct-after-default
  "function foo(a = 1, {b, c}) {\n}")

(js2-deftest-parse function-with-rest-parameter
  "function foo(a, b, ...rest) {\n}")

(js2-deftest-parse function-with-param-after-rest-parameter
  "function foo(a, ...b, rest) {\n}"
  :syntax-error "rest")

(js2-deftest-parse function-with-destruct-after-rest-parameter
  "function foo(a, ...b, {}) {\n}"
  :syntax-error "{}")

(js2-deftest-parse function-with-rest-after-default-parameter
  "function foo(a = 1, ...rest) {\n}")

;;; Strict mode errors

(js2-deftest-parse function-bad-strict-parameters
  "'use strict';\nfunction foo(eval, {arguments}, bar) {\n}"
  :syntax-error "eval" :errors-count 2)

(js2-deftest-parse function-retroactive-bad-strict-parameters
  "function foo(arguments) {'use strict';}"
  :syntax-error "arguments" :errors-count 1)

(js2-deftest-parse function-duplicate-strict-parameters
  "'use strict';\nfunction foo(a, a) {\n}"
  :syntax-error "a" :errors-count 1)

(js2-deftest-parse function-bad-strict-function-name
  "'use strict';\nfunction eval() {\n}"
  :syntax-error "eval" :errors-count 1)

(js2-deftest-parse function-bad-retroactive-strict-function-name
  "function arguments() {'use strict';}"
  :syntax-error "arguments" :errors-count 1)

(js2-deftest-parse function-bad-strict-catch-name
  "'use strict';\ntry {} catch (eval) {}"
  :syntax-error "eval" :errors-count 1)

(js2-deftest-parse function-bad-strict-variable-name
  "'use strict';\nvar eval = 'kekeke';"
  :syntax-error "eval" :errors-count 1)

(js2-deftest-parse function-bad-strict-assignment
  "'use strict';\narguments = 'fufufu';"
  :syntax-error "arguments" :errors-count 1)

(js2-deftest-parse function-property-strict-assignment
  "'use strict';\narguments.okay = 'alright';")

(js2-deftest-parse function-strict-with
  "'use strict';\nwith ({}) {}"
  :syntax-error "with" :errors-count 1)

(js2-deftest-parse function-strict-octal
  "'use strict';\nvar number = 0644;"
  :syntax-error "0644" :errors-count 1)

(js2-deftest-parse function-strict-octal-allow-0o
  "'use strict';\n0o644;" :reference "'use strict';\n420;")

(js2-deftest-parse function-strict-duplicate-keys
  "'use strict';\nvar object = {a: 1, a: 2, 'a': 3, ['a']: 4, 1: 5, '1': 6, [1 + 1]: 7};"
  :syntax-error "a" :errors-count 4) ; "a" has 3 dupes, "1" has 1 dupe.

(js2-deftest-parse function-strict-duplicate-getter
  "'use strict';\nvar a = {get x() {}, get x() {}};"
  :syntax-error "x" :errors-count 1)

(js2-deftest-parse function-strict-duplicate-setter
  "'use strict';\nvar a = {set x() {}, set x() {}};"
  :syntax-error "x" :errors-count 1)

;;; Lack of errors in strict mode

(js2-deftest-parse function-strict-const-scope
  "'use strict';\nconst a;\nif (1) {\n  const a;\n}")

(js2-deftest-parse function-strict-no-getter-setter-duplicate
  "'use strict';\nvar a = {get x() {}, set x() {}};")

;;; Spread operator

(js2-deftest-parse spread-in-array-literal
  "[1, ...[2, 3], 4, ...[5, 6]];")

(js2-deftest-parse spread-in-function-call
  "f(3, ...[t(2), t(3)], 42, ...[t(4)]);")

(js2-deftest-parse rest-in-array-destructure
  "let [x, y, z, ...w] = [1, ...a, ...b, c];")

(js2-deftest-parse comma-after-rest-in-array
  "let [...x,] = [1, 2, 3];"
  :syntax-error "," :errors-count 1)

(js2-deftest-parse elem-after-rest-in-array
  "let [...x, y] = [1, 2, 3];"
  :syntax-error "," :errors-count 2)

(js2-deftest-parse array-destructure-expr-default
  "let [[x] = [3]] = y;")

(js2-deftest-parse spread-in-object-literal
  "f({x, y, ...z});")

(js2-deftest-parse rest-in-object-literal
  "const {x, y, ...z} = f();")

;;; Arrow functions

(js2-deftest-parse arrow-function-with-empty-args-and-no-curlies
  "() => false;" :reference "() => {false};")

(js2-deftest-parse arrow-function-with-args-and-curlies
  "(a, b = 1, ...c) => {  c;\n};")

(js2-deftest-parse arrow-function-with-destructuring
  "([{a}, b]) => {  a + b;\n};")

(js2-deftest-parse parenless-arrow-function-prohibits-rest
  "...b => {b + 1;};" :syntax-error "=>")

(js2-deftest-parse parenless-arrow-function-prohibits-destructuring
  "[a, b] => {a + b;};" :syntax-error "]" :errors-count 4)

(js2-deftest-parse arrow-function-recovers-from-error
  "[(,foo) => 1];" :syntax-error "," :errors-count 6)

;;; Automatic semicolon insertion

(js2-deftest-parse no-auto-semi-insertion-after-if
  "if (true) {\n}")

(js2-deftest-parse auto-semi-insertion-after-function
  "a = function() {}" :reference "a = function() {};")

(js2-deftest-parse auto-semi-one-variable-per-line
  "x\ny" :reference "x;\ny;")

;;; Labels

(js2-deftest-parse labeled-stmt-node
  "foo:\nbar:\nx = y + 1;")

(js2-deftest no-label-node-inside-expr "x = y:"
  (let (js2-parse-interruptable-p)
    (js2-mode--and-parse))
  (let ((assignment (js2-expr-stmt-node-expr (car (js2-scope-kids js2-mode-ast)))))
    (should (js2-name-node-p (js2-assign-node-right assignment)))))

(js2-deftest-parse label-and-loops "for (; ; ) {
  loop:
  for (; ; ) {
    continue loop;
  }
}")

;;; Generators

(js2-deftest-parse legacy-generator "function foo() {\n  yield 1;\n}")

(js2-deftest-parse legacy-generator-cannot-return
  "function foo() {\n  yield 1;\n return 2;\n}" :syntax-error "return 2")

(js2-deftest-parse harmony-generator "function* bar() {\n  yield 2;\n  return 3;\n}")

(js2-deftest-parse harmony-generator-yield-star "(function*(a) {  yield* a;\n});")

;;; Comprehensions

(js2-deftest-parse parse-legacy-array-comp-loop-with-filter
  "[a for (a in b) if (a == 2)];")

(js2-deftest-parse parse-array-comp-loop-with-filters
  "[for (a in b) if (a == 2) if (b != 10) a];")

(js2-deftest-parse parse-generator-comp-loop-with-filters
  "(for (x of y) if (x != 4) x);")

(js2-deftest-parse parse-array-comp-with-yield-is-ok
  "(function() {  return [for (x of []) yield x];\n});")

(js2-deftest-parse parse-generator-comp-with-yield-is-not-ok
  "(function() {  return (for (x of []) yield x);\n});"
  :syntax-error "yield")

(js2-deftest-parse parse-generator-comp-with-yield-inside-function-is-ok
  "(for (x of []) function*() {  yield x;\n});")

;;; Async

(js2-deftest-parse async-function-statement
  "async function foo() {\n}")

(js2-deftest-parse async-function-statement-inside-block
  "if (true) {\n  async function foo() {\n  }\n}")

(js2-deftest-parse async-function-expression-statements-are-verboten
  "async function() {}" :syntax-error "(")

(js2-deftest-parse async-named-function-expression
  "a = async function b() {};")

(js2-deftest-parse async-arrow-function-expression
  "a = async (b) => {  b;\n};")

(js2-deftest-parse async-method-in-object-literal
  "({async f() {}});")

(js2-deftest-parse async-method-in-class-body
  "class C {\n  async foo() {}\n}")

(js2-deftest-parse static-async-method-in-class-body
  "class C {\n  static async foo() {}\n}")

(js2-deftest-parse async-method-allow-await
  "({async f() {  await x;\n}});")

;;; Await

(js2-deftest-parse await-is-ok "async function foo() {\n  await bar();\n}")

(js2-deftest-parse await-inside-assignment-is-ok
                   "async function foo() {\n  var result = await bar();\n}")

(js2-deftest-parse await-inside-array-is-ok
                   "async function foo() {\n  var results = [await bar(), await baz()];\n}")

(js2-deftest-parse await-inside-non-async-function-is-not-ok
                   "function foo() {\n  await bar();\n}"
                   :syntax-error "await")

(js2-deftest-parse await-inside-non-async-arrow-function-is-not-ok
                   "a = () => {  await bar();\n}"
                   :syntax-error "await")

;;; 'async' and 'await' are contextual keywords

(js2-deftest-parse async-can-be-name
  "void async;")

(js2-deftest-parse async-can-be-object-name
  "async.z;")

(js2-deftest-parse async-can-be-var-name
  "var async = 3;")

(js2-deftest-parse async-can-be-function-name
  "function async() {\n}")

(js2-deftest-parse await-can-be-name
  "void await;")

(js2-deftest-parse await-can-be-object-name
  "await.z;")

(js2-deftest-parse await-can-be-var-name
  "var await = 3;")

(js2-deftest-parse await-can-be-function-name
  "function await() {\n}")

;;; Numbers

(js2-deftest-parse decimal-starting-with-zero "081;" :reference "81;")

(js2-deftest-parse huge-hex "0x0123456789abcdefABCDEF;" :reference "-1;")

(js2-deftest-parse octal-without-o "071;" :reference "57;")

(js2-deftest-parse hex-number-okay "0x123;" :reference "291;")

(js2-deftest-parse hex-number-broken "0xz23;"
  :syntax-error "0xz" :errors-count 2)

(js2-deftest-parse binary-number-okay "0b101;" :reference "5;")

(js2-deftest-parse binary-number-broken "0b210;"
  :syntax-error "0b2" :errors-count 2)

(js2-deftest-parse octal-number-okay "0o765;" :reference "501;")

(js2-deftest-parse octal-number-broken "0o812;"
  :syntax-error "0o8" :errors-count 2)

;;; Modules

(js2-deftest parse-export-bindings "{one, two as dos}"
  (js2-init-scanner)
  (should (js2-match-token js2-LC))
  (let ((imports (js2-parse-export-bindings)))
    (should (not (equal nil imports)))
    (should (= 2 (length imports)))
    (let ((first (nth 0 imports))
          (second (nth 1 imports)))
      (should (equal "one" (js2-name-node-name (js2-export-binding-node-extern-name first))))
      (should (equal "two" (js2-name-node-name (js2-export-binding-node-extern-name second))))
      (let ((first-name (js2-export-binding-node-local-name first))
            (second-name (js2-export-binding-node-local-name second)))
        (should (equal first (js2-node-parent first-name)))
        (should (equal 3 (js2-node-len first-name)))
        (should (equal "one" (js2-name-node-name first-name)))
        (should (equal second (js2-node-parent second-name)))
        (should (equal 3 (js2-node-len second-name)))
        (should (equal "dos" (js2-name-node-name second-name)))))))

(js2-deftest parse-export-binding-as-default "one as default"
  (js2-init-scanner)
  (let ((binding (js2-maybe-parse-export-binding)))
    (should binding)
    (should (js2-export-binding-node-p binding))
    (let ((name (js2-export-binding-node-local-name binding)))
      (should name)
      (should (equal "default" (js2-name-node-name name))))))

(js2-deftest parse-namepsace-import "* as lib;"
  (js2-init-scanner)
  (should (js2-match-token js2-MUL))
  (let ((namespace-import (js2-parse-namespace-import)))
    (should (not (equal nil namespace-import)))
    (should (js2-namespace-import-node-p namespace-import))
    (should (= 1 (js2-node-pos namespace-import)))
    (should (equal 8 (js2-node-len namespace-import)))
    (let ((name-node (js2-namespace-import-node-name namespace-import)))
      (should (equal "lib" (js2-name-node-name name-node)))
      (should (= 5 (js2-node-pos name-node))))))

(js2-deftest parse-from-clause "from 'foo/bar';"
  (js2-init-scanner)
  (let ((from (js2-parse-from-clause)))
    (should (not (equal nil from)))
    (should (= 1 (js2-node-pos from)))
    (should (= 14 (js2-node-len from)))
    (should (equal "foo/bar" (js2-from-clause-node-module-id from)))))

(js2-deftest parse-import-module-id-only "import 'src/lib'"
  (js2-init-scanner)
  (should (js2-match-token js2-IMPORT))
  (let ((import (js2-parse-import)))
    (should (not (equal nil import)))
    (should (= 1 (js2-node-pos import)))
    (should (= 16 (js2-node-len import)))
    (should (equal nil (js2-import-node-import import)))
    (should (equal nil (js2-import-node-from import)))))

(js2-deftest parse-imported-default-binding "import theDefault from 'src/lib'"
  (js2-push-scope (make-js2-scope :pos 0))
  (js2-init-scanner)
  (should (js2-match-token js2-IMPORT))
  (let ((import-node (js2-parse-import)))
    (should (not (equal nil import-node)))
    (should (equal "src/lib" (js2-import-node-module-id import-node)))
    (let ((import (js2-import-node-import import-node)))
      (should (not (equal nil import)))
      (should (equal nil (js2-import-clause-node-namespace-import import)))
      (should (equal nil (js2-import-clause-node-named-imports import)))
      (let ((default (js2-import-clause-node-default-binding import)))
        (should (not (equal nil default)))
        (should (js2-export-binding-node-p default))
        (should (equal "theDefault" (js2-name-node-name (js2-export-binding-node-extern-name default)))))))
  (should (js2-scope-get-symbol js2-current-scope "theDefault")))

(js2-deftest parse-import-namespace-binding "import * as lib from 'src/lib'"
  (js2-push-scope (make-js2-scope :pos 0))
  (js2-init-scanner)
  (should (js2-match-token js2-IMPORT))
  (let ((import-node (js2-parse-import)))
    (should (not (equal nil import-node)))
    (should (equal "src/lib" (js2-import-node-module-id import-node)))
    (let ((import (js2-import-node-import import-node)))
      (should (not (equal nil import)))
      (should (equal nil (js2-import-clause-node-default-binding import)))
      (should (equal nil (js2-import-clause-node-named-imports import)))
      (let ((ns-import (js2-import-clause-node-namespace-import import)))
        (should (not (equal nil ns-import)))
        (should (js2-namespace-import-node-p ns-import))
        (should (equal "lib" (js2-name-node-name (js2-namespace-import-node-name ns-import)))))))
  (should (js2-scope-get-symbol js2-current-scope "lib")))

(js2-deftest parse-import-named-imports "import {foo as bar, baz} from 'src/lib'"
  (js2-push-scope (make-js2-scope :pos 0))
  (js2-init-scanner)
  (should (js2-match-token js2-IMPORT))
  (let ((import-node (js2-parse-import)))
    (should (not (equal nil import-node)))
    (should (equal "src/lib" (js2-import-node-module-id import-node)))
    (let ((import (js2-import-node-import import-node)))
      (should (not (equal nil import)))
      (should (equal nil (js2-import-clause-node-default-binding import)))
      (should (equal nil (js2-import-clause-node-namespace-import import)))
      (let ((named-imports (js2-import-clause-node-named-imports import)))
        (should (not (equal nil named-imports)))
        (should (listp named-imports))
        (should (= 2 (length named-imports)))
        (let ((first (nth 0 named-imports))
              (second (nth 1 named-imports)))
          (should (equal "bar" (js2-name-node-name (js2-export-binding-node-local-name first))))
          (should (equal "baz" (js2-name-node-name (js2-export-binding-node-local-name second))))))))
  (should (js2-scope-get-symbol js2-current-scope "bar"))
  (should (js2-scope-get-symbol js2-current-scope "baz")))

(js2-deftest parse-import-default-and-namespace "import stuff, * as lib from 'src/lib'"
  (js2-push-scope (make-js2-scope :pos 0))
  (js2-init-scanner)
  (should (js2-match-token js2-IMPORT))
  (let ((import-node (js2-parse-import)))
    (should (not (equal nil import-node)))
    (should (equal "src/lib" (js2-import-node-module-id import-node)))
    (let ((import (js2-import-node-import import-node)))
      (should (not (equal nil import)))
      (should (equal nil (js2-import-clause-node-named-imports import)))
      (let ((default (js2-import-clause-node-default-binding import))
            (ns-import (js2-import-clause-node-namespace-import import)))
        (should (not (equal nil default)))
        (should (equal "stuff" (js2-name-node-name (js2-export-binding-node-local-name default))))
        (should (not (equal nil ns-import)))
        (should (js2-namespace-import-node-p ns-import))
        (should (equal "lib" (js2-name-node-name (js2-namespace-import-node-name ns-import)))))))
  (should (js2-scope-get-symbol js2-current-scope "stuff"))
  (should (js2-scope-get-symbol js2-current-scope "lib")))

(js2-deftest parse-import-default-and-named-imports
  "import robert as bob, {cookies, pi as PIE} from 'src/lib'"
  (js2-push-scope (make-js2-scope :pos 0))
  (js2-init-scanner)
  (should (js2-match-token js2-IMPORT))
  (let ((import-node (js2-parse-import)))
    (should (not (equal nil import-node)))
    (should (equal "src/lib" (js2-import-node-module-id import-node)))
    (let ((import (js2-import-node-import import-node)))
      (should (not (equal nil import)))
      (should (not (equal nil (js2-import-clause-node-named-imports import))))
      (let ((default (js2-import-clause-node-default-binding import))
            (named-imports (js2-import-clause-node-named-imports import)))
        (should (not (equal nil default)))
        (should (equal "bob" (js2-name-node-name (js2-export-binding-node-local-name default))))
        (should (not (equal nil named-imports)))
        (should (= 2 (length named-imports))))))
  (should (js2-scope-get-symbol js2-current-scope "bob"))
  (should (js2-scope-get-symbol js2-current-scope "cookies"))
  (should (js2-scope-get-symbol js2-current-scope "PIE")))

(js2-deftest parse-this-module-in-from-clause "import {url} from this module;"
  (js2-push-scope (make-js2-scope :pos 0))
  (js2-init-scanner)
  (should (js2-match-token js2-IMPORT))
  (let ((import-node (js2-parse-import)))
    (should import-node)
    (let ((from-clause (js2-import-node-from import-node)))
      (should from-clause)
      (should (equal "this" (js2-from-clause-node-module-id from-clause)))
      (should (js2-from-clause-node-metadata-p from-clause)))))

(js2-deftest-parse import-only-for-side-effects "import 'src/lib';")
(js2-deftest-parse import-default-only "import theDefault from 'src/lib';")
(js2-deftest-parse import-named-only "import {one, two} from 'src/lib';")
(js2-deftest-parse import-default-and-named "import theDefault, {one, two} from 'src/lib';")
(js2-deftest-parse import-renaming-default "import * as lib from 'src/mylib';")
(js2-deftest-parse import-renaming-named "import {one as uno, two as dos} from 'src/lib';")
(js2-deftest-parse import-default-and-namespace "import robert as bob, * as lib from 'src/lib';")
(js2-deftest-parse import-from-this-module "import {url} from this module;")

;; Module Exports

(js2-deftest export-rexport "export * from 'other/lib'"
  (js2-init-scanner)
  (should (js2-match-token js2-EXPORT))
  (let ((export-node (js2-parse-export)))
    (should export-node)
    (should (js2-export-node-from-clause export-node))))

(js2-deftest export-export-named-list "export {foo, bar as bang};"
  (js2-init-scanner)
  (should (js2-match-token js2-EXPORT))
  (let ((export-node (js2-parse-export)))
    (should export-node)
    (let ((exports (js2-export-node-exports-list export-node)))
      (should exports)
      (should (= 2 (length exports))))))

(js2-deftest re-export-named-list "export {foo, bar as bang} from 'other/lib'"
  (js2-init-scanner)
  (should (js2-match-token js2-EXPORT))
  (let ((export-node (js2-parse-export)))
    (should export-node)
    (should (js2-export-node-from-clause export-node))
    (let ((exports (js2-export-node-exports-list export-node)))
      (should exports)
      (should (= 2 (length exports))))))

(js2-deftest export-variable-statement "export var foo = 'bar', baz = 'bang';"
  (js2-init-scanner)
  (js2-push-scope (make-js2-scope :pos 0))
  (should (js2-match-token js2-EXPORT))
  (let ((export-node (js2-parse-export)))
    (should export-node)
    (should (js2-export-node-declaration export-node))))

(js2-deftest export-const-declaration "export const PI = Math.PI;"
  (js2-init-scanner)
  (js2-push-scope (make-js2-scope :pos 0))
  (should (js2-match-token js2-EXPORT))
  (let ((export-node (js2-parse-export)))
    (should export-node)
    (should (js2-export-node-declaration export-node))))

(js2-deftest export-let-declaration "export let foo = [1];"
  (js2-init-scanner)
  (js2-push-scope (make-js2-scope :pos 0))
  (should (js2-match-token js2-EXPORT))
  (let ((export-node (js2-parse-export)))
    (should export-node)
    (should (js2-var-decl-node-p (js2-export-node-declaration export-node)))))

(js2-deftest export-class-declaration "export class Foo {}"
  (js2-init-scanner)
  (js2-push-scope (make-js2-scope :pos 0))
  (should (js2-match-token js2-EXPORT))
  (let ((export-node (js2-parse-export)))
    (should export-node)
    (should (js2-class-node-p (js2-export-node-declaration export-node)))))

(js2-deftest export-function-declaration "export default function doStuff() {}"
  (js2-init-scanner)
  (js2-push-scope (make-js2-scope :pos 0))
  (should (js2-match-token js2-EXPORT))
  (let ((export-node (js2-parse-export)))
    (should export-node)
    (should (js2-export-node-default export-node))))

(js2-deftest export-generator-declaration "export default function* one() {}"
  (js2-init-scanner)
  (js2-push-scope (make-js2-scope :pos 0))
  (should (js2-match-token js2-EXPORT))
  (let ((export-node (js2-parse-export)))
    (should export-node)
    (should (js2-export-node-default export-node))))

(js2-deftest export-assignment-expression "export default a = b;"
  (js2-init-scanner)
  (js2-push-scope (make-js2-scope :pos 0))
  (should (js2-match-token js2-EXPORT))
  (let ((export-node (js2-parse-export)))
    (should export-node)
    (should (js2-export-node-default export-node))))

(js2-deftest export-function-no-semicolon "export default function foo() {}"
  (js2-mode--and-parse)
  (should (equal nil js2-parsed-warnings)))
(js2-deftest export-default-function-no-semicolon "export function foo() {}"
  (js2-mode--and-parse)
  (should (equal nil js2-parsed-warnings)))
(js2-deftest export-anything-else-does-require-a-semicolon "export var obj = {}"
  (js2-mode--and-parse)
  (should (not (equal nil js2-parsed-warnings))))

(js2-deftest export-default-async-function-no-semicolon "export default async function foo() {}"
  (js2-mode--and-parse)
  (should (equal nil js2-parsed-warnings)))
(js2-deftest export-async-function-no-semicolon "export async function foo() {}"
  (js2-mode--and-parse)
  (should (equal nil js2-parsed-warnings)))

(js2-deftest-parse parse-export-rexport "export * from 'other/lib';")
(js2-deftest-parse parse-export-export-named-list "export {foo, bar as bang};")
(js2-deftest-parse parse-re-export-named-list "export {foo, bar as bang} from 'other/lib';")
(js2-deftest-parse parse-export-const-declaration "export const PI = Math.PI;")
(js2-deftest-parse parse-export-let-declaration "export let foo = [1];")
(js2-deftest-parse parse-export-function-declaration "export default function doStuff() {\n}")
(js2-deftest-parse parse-export-generator-declaration "export default function* one() {\n}")
(js2-deftest-parse parse-export-assignment-expression "export default a = b;")

(js2-deftest-parse parse-export-function-declaration-no-semi
  "export function f() {\n}")

(js2-deftest-parse parse-export-class-declaration-no-semi
  "export class C {\n}")

(js2-deftest-parse parse-export-async-function-allow-await
  "export async function f() {\n  await f();\n}")

(js2-deftest-parse parse-export-default-async-function-allow-await
  "export default async function f() {\n  await f();\n}")

;;; Strings

(js2-deftest-parse string-literal
  "var x = 'y';")

(js2-deftest-parse object-get-string-literal
  "var x = {y: 5};\nvar z = x[\"y\"];")

(js2-deftest-parse template-no-substritutions
  "var x = `abc
            def`, y = `\\u0000`;")

(js2-deftest-parse template-with-substitutions
  "var y = `${a + b} ${d + e + f}`;")

(js2-deftest-parse tagged-template
  "foo.args`${++x, \"o\"}k`;")

;;; Classes

(js2-deftest-parse parse-harmony-class-statement
  "class Foo {\n  get bar() {  return 42;\n}\n  set bar(x) {  y = x;\n}\n}")

(js2-deftest-parse parse-harmony-class-statement-without-name-is-not-ok
  "class {\n  get bar() {  return 42;\n}\n}"
  :syntax-error "{")

(js2-deftest-parse parse-harmony-class-expression
  "var Foo1 = class Foo {\n  bar() {  return 42;\n}\n};")

(js2-deftest-parse parse-harmony-anonymous-class-expression
  "var Foo = class {\n  set bar(x) {  bar = x;\n}\n};")

(js2-deftest-parse parse-harmony-class-with-extends
  "class Foo extends Bar {\n}")

(js2-deftest-parse parse-harmony-anonymous-class-with-extends
  "foo.Foo = class extends Bar {\n  set bar(x) {  bar = x;\n}\n};")

(js2-deftest-parse parse-harmony-class-with-complex-extends
  "class Foo extends foo[BAR][2].Baz {\n}")

(js2-deftest-parse parse-harmony-class-missing-extended-class-is-not-ok
  "class Foo extends {\n}"
  :syntax-error "extends")

(js2-deftest-parse parse-harmony-class-static-method
  "class Foo extends Bar {\n  static bar() {  return 42;\n}\n}")

(js2-deftest-parse parse-unterminated-class-is-not-okay
  "class Foo {\n  get bar() {  return 42;\n}"
  :syntax-error "}")

(js2-deftest-parse parse-super-keyword
  "class Foo {\n  constructor() {  super(42);\n}\n  foo() {  super.foo();\n}\n}")

(js2-deftest-parse parse-class-keywordlike-method
  "class C {\n  delete() {}\n  if() {}\n}")

(js2-deftest-parse parse-harmony-class-allow-semicolon-element
  "class Foo {;}" :reference "class Foo {\n}")

;;; Scopes

(js2-deftest ast-symbol-table-includes-fn-node "function foo() {}"
  (js2-mode--and-parse)
  (let ((entry (js2-scope-get-symbol js2-mode-ast 'foo)))
    (should (= (js2-symbol-decl-type entry) js2-FUNCTION))
    (should (equal (js2-symbol-name entry) "foo"))
    (should (js2-function-node-p (js2-symbol-ast-node entry)))))

(js2-deftest fn-symbol-table-includes-nested-fn "function foo() {
  function bar() {}
  var x;
}"
  (js2-mode--and-parse)
  (let* ((scope (js2-node-at-point (point-min)))
         (fn-entry (js2-scope-get-symbol scope 'bar))
         (var-entry (js2-scope-get-symbol scope 'x)))
    (should (string= (js2-name-node-name (js2-function-node-name scope)) "foo"))
    (should (= (js2-symbol-decl-type fn-entry) js2-FUNCTION))
    (should (js2-function-node-p (js2-symbol-ast-node fn-entry)))
    (should (= (js2-symbol-decl-type var-entry) js2-VAR))
    (should (js2-name-node-p (js2-symbol-ast-node var-entry)))))

(defun js2-test-scope-of-nth-variable-satisifies-predicate (variable nth predicate)
  (goto-char (point-min))
  (dotimes (n (1+ nth)) (search-forward variable))
  (forward-char -1)
  (let ((scope (js2-node-get-enclosing-scope (js2-node-at-point))))
    (should (funcall predicate (js2-get-defining-scope scope variable)))))

(js2-deftest for-node-is-declaration-scope "for (let i = 0; i; ++i) {};"
  (js2-mode--and-parse)
  (js2-test-scope-of-nth-variable-satisifies-predicate "i" 0 #'js2-for-node-p))

(js2-deftest const-scope-inside-script "{ const a; } a;"
  (js2-mode--and-parse)
  (js2-test-scope-of-nth-variable-satisifies-predicate "a" 0 #'js2-block-node-p)
  (js2-test-scope-of-nth-variable-satisifies-predicate "a" 1 #'null))

(js2-deftest const-scope-inside-function "function f() { { const a; } a; }"
  (js2-mode--and-parse)
  (js2-test-scope-of-nth-variable-satisifies-predicate "a" 0 #'js2-block-node-p)
  (js2-test-scope-of-nth-variable-satisifies-predicate "a" 1 #'null))

(js2-deftest array-comp-is-result-scope "[x * 2 for (x in y)];"
  (js2-mode--and-parse)
  (js2-test-scope-of-nth-variable-satisifies-predicate "x" 0 #'js2-comp-loop-node-p))

(js2-deftest array-comp-has-parent-scope
             "var a,b=[for (i of [[1,2]]) for (j of i) j * a];"
  (js2-mode--and-parse)
  (search-forward "for")
  (forward-char -3)
  (let ((node (js2-node-at-point)))
    (should (js2-scope-parent-scope node))
    (should (js2-get-defining-scope node "j"))))

;;; Tokenizer

(js2-deftest get-token "(1+1)"
  (js2-init-scanner)
  (should (eq js2-LP (js2-next-token)))
  (should (eq js2-NUMBER (js2-next-token)))
  (should (eq js2-ADD (js2-next-token)))
  (should (eq js2-NUMBER (js2-next-token)))
  (should (eq js2-RP (js2-next-token))))

(js2-deftest unget-token "()"
  (js2-init-scanner)
  (should (eq js2-LP (js2-next-token)))
  (js2-unget-token)
  (should (eq js2-LP (js2-next-token)))
  (should (eq js2-RP (js2-next-token))))

(js2-deftest get-token-or-eol "x\n++;"
  (js2-init-scanner)
  (should (eq js2-NAME (js2-next-token)))
  (should (eq js2-EOL (js2-peek-token-or-eol)))
  (should (eq js2-INC (js2-next-token)))
  (should (eq js2-SEMI (js2-peek-token-or-eol))))

(js2-deftest unget-token-over-eol-and-comment "x\n//abc\ny"
  (js2-init-scanner)
  (should (eq js2-NAME (js2-next-token)))
  (should (eq js2-NAME (js2-next-token)))
  (should (equal "y" (js2-current-token-string)))
  (js2-unget-token)
  (should (eq js2-NAME (js2-current-token-type)))
  (should (equal "x" (js2-current-token-string))))

(js2-deftest ts-seek "(1+2)"
  (js2-init-scanner)
  (should (eq js2-LP (js2-next-token)))
  (should (eq js2-NUMBER (js2-next-token)))
  (js2-unget-token)
  (let ((state (make-js2-ts-state)))
    (should (eq js2-NUMBER (js2-next-token)))
    (should (eq js2-ADD (js2-next-token)))
    (js2-ts-seek state)
    (should (eq 1 js2-ti-lookahead))
    (should (eq js2-NUMBER (js2-next-token)))
    (should (eq 1 (js2-token-number
                   (js2-current-token))))))

(js2-deftest get-token-template-literal "`abc ${i} z ${j} def`"
  (js2-init-scanner)
  (should (eq js2-TEMPLATE_HEAD (js2-next-token)))
  (should (equal "abc " (js2-current-token-string)))
  (should (eq js2-NAME (js2-next-token)))
  (should (eq js2-RC (js2-next-token)))
  (should (eq js2-TEMPLATE_HEAD (js2-next-token 'TEMPLATE_TAIL)))
  (should (equal " z " (js2-current-token-string)))
  (should (eq js2-NAME (js2-next-token)))
  (should (eq js2-RC (js2-next-token)))
  (should (eq js2-NO_SUBS_TEMPLATE (js2-next-token 'TEMPLATE_TAIL)))
  (should (equal " def" (js2-current-token-string))))

;;; Error handling

(js2-deftest for-node-with-error-len "for "
  (js2-mode--and-parse)
  (let ((node (js2-node-at-point (point-min))))
    (should (= (js2-node-len (js2-node-parent node)) 4))))

(js2-deftest function-without-parens-error "function b {}"
  ;; Should finish the parse.
  (js2-mode--and-parse))

;;; Comments

(js2-deftest comment-node-length "//"
  (js2-mode--and-parse)
  (let ((node (js2-node-at-point (point-min))))
    (should (= (js2-node-len node) 2))))

(js2-deftest comment-node-length-newline "//\n"
  (js2-mode--and-parse)
  (let ((node (js2-node-at-point (point-min))))
    (should (= (js2-node-len node) 3))))

;;; Variables classification

(defun js2--variables-summary (vars)
  (let (r)
    (setq vars (let (aslist)
                 (maphash (lambda (k v) (push (cons k v) aslist)) vars)
                 aslist))
    (dolist (v (sort vars (lambda (a b) (< (js2-node-abs-pos (js2-symbol-ast-node (car a)))
                                      (js2-node-abs-pos (js2-symbol-ast-node (car b)))))))
      (let* ((symbol (car v))
             (inition (cadr v))
             (uses (cddr v))
             (symn (js2-symbol-ast-node symbol))
             (namen (js2--get-name-node symn)))
        (push (format "%s@%s:%s"
                      (js2-symbol-name symbol)
                      (js2-node-abs-pos namen)
                      (if (eq inition ?P)
                          "P"
                        (if uses
                            (if inition "I" "N")
                          "U"))) r)
        (dolist (u (sort (cddr v) (lambda (a b) (< (js2-node-abs-pos a)
                                              (js2-node-abs-pos b)))))
          (push (js2-node-abs-pos u) r))))
    (reverse r)))

(defmacro js2-deftest-classify-variables (name buffer-contents summary)
 (declare (indent defun))
  `(ert-deftest ,(intern (format "js2-classify-variables-%s" name)) ()
     (with-temp-buffer
       (save-excursion
         (insert ,buffer-contents))
       (unwind-protect
           (progn
             (js2-mode--and-parse)
             (should (equal ,summary (js2--variables-summary
                                      (js2--classify-variables)))))
         (fundamental-mode)))))

(js2-deftest-classify-variables incomplete-var-statement
  "var"
  '())

(js2-deftest-classify-variables unused-variable
  "function foo () { var x; return 42; }"
  '("foo@10:U" "x@23:U"))

(js2-deftest-classify-variables unused-variable-declared-twice
  "function foo (a) { var x; function bar () { var x; x=42; }; return a;}"
  '("foo@10:U" "a@15:P" 68 "x@24:U" "bar@36:U" "x@49:U"))

(js2-deftest-classify-variables assigned-variable
  "function foo () { var x; x=42; return x; }"
  '("foo@10:U" "x@23:I" 39))

(js2-deftest-classify-variables assignment-in-nested-function
  "function foo () { var x; function bar () { x=42; }; }"
  '("foo@10:U" "x@23:U" "bar@35:U"))

(js2-deftest-classify-variables unused-nested-function
  "function foo() { var i, j=1; function bar() { var x, y=42, z=i; return y; } return i; }"
  '("foo@10:U" "i@22:N" 62 84 "j@25:U" "bar@39:U" "x@51:U" "y@54:I" 72 "z@60:U"))

(js2-deftest-classify-variables prop-get-initialized
  "function foo () { var x, y={}; y.a=x; }"
  '("foo@10:U" "x@23:N" 36 "y@26:I" 32))

(js2-deftest-classify-variables prop-get-uninitialized
  "function foo () { var x; if(x.foo) alert('boom'); }"
  '("foo@10:U" "x@23:N" 29))

(js2-deftest-classify-variables prop-get-function-assignment
  "(function(w) { w.f = function() { var a=42, m; return a; }; })(window);"
  '("w@11:P" 11 16 "a@39:I" 55 "m@45:U"))

(js2-deftest-classify-variables let-declaration
  "function foo () { let x,y=1; return x; }"
  '("foo@10:U" "x@23:N" 37 "y@25:U"))

(js2-deftest-classify-variables external-function-call
  "function foo (m) { console.log(m, arguments); }"
  '("foo@10:U" "m@15:P" 32))

(js2-deftest-classify-variables global-function-call
  "function bar () { return 42; } function foo (a) { return bar(); }"
  '("bar@10:I" 58 "foo@41:U" "a@46:P"))

(js2-deftest-classify-variables let-declaration-for-scope
  "function foo () { for(let x=1,y; x<y; y++) {} }"
  '("foo@10:U" "x@27:I" 34 "y@31:N" 36 39))

(js2-deftest-classify-variables arguments-implicit-var
  "function foo () { var p; for(p in arguments) { return p; } }"
  '("foo@10:U" "p@23:I" 55))

(js2-deftest-classify-variables catch-error-variable
  "function foo () { try { throw 'Foo'; } catch (e) { console.log(e); }"
  '("foo@10:U" "e@47:I" 64))

(js2-deftest-classify-variables prop-get-assignment
  "function foo () { var x={y:{z:{}}}; x.y.z=42; }"
  '("foo@10:U" "x@23:I" 37))

(js2-deftest-classify-variables unused-function-argument
  "function foo (a) { return 42; }"
  '("foo@10:U" "a@15:P"))

(js2-deftest-classify-variables used-function-argument
  "function foo (a) { a=42; return a; }"
  '("foo@10:U" "a@15:P" 33))

(js2-deftest-classify-variables prop-get
  "function foo (a) { a=navigator.x||navigator.y; return a; }"
  '("foo@10:U" "a@15:P" 55))

(js2-deftest-classify-variables for-in-loop
  "function foo () { var d={}; for(var k in d) {var v=d[k]; } }"
  '("foo@10:U" "d@23:I" 42 52 "k@37:I" 54 "v@50:U"))

(js2-deftest-classify-variables array-comprehension-legacy
  "function foo() { var j,a=[for (i of [1,2,3]) i*j]; }"
  '("foo@10:U" "j@22:N" 48 "a@24:U" "i@32:I" 46))

(js2-deftest-classify-variables array-comprehension
  "function foo() { var j,a=[[i,j] for (i of [1,2,3])]; }"
  '("foo@10:U" "j@22:N" 30 "a@24:U" "i@38:I" 28))

(js2-deftest-classify-variables return-named-function
  "function foo() { var a=42; return function bar() { return a; } }"
  '("foo@10:U" "a@22:I" 59 "bar@44:I" 44))

(js2-deftest-classify-variables named-wrapper-function
  "function foo() { var a; (function bar() { a=42; })(); return a; }"
  '("foo@10:U" "a@22:I" 62 "bar@35:I" 35))
