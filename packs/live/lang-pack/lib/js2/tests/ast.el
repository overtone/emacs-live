(require 'ert)
(require 'ert-x)
(require 'js2-mode)

(defun js2-test-string-to-ast (s)
  (ert-with-test-buffer (:name 'origin)
    (insert s)
    (js2-mode)
    (should (null js2-mode-buffer-dirty-p))
    js2-mode-ast))

(defun js2-test-ast-string (code-string &key syntax-error)
  (let ((ast (js2-test-string-to-ast code-string)))
    (if syntax-error
        (let ((errors (js2-ast-root-errors ast)))
          (should (= 1 (length errors)))
          (destructuring-bind (_ pos len) (first errors)
            (should (string= syntax-error (substring code-string
                                                     (1- pos) (+ pos len -1))))))
      (should (= 0 (length (js2-ast-root-errors ast))))
      (ert-with-test-buffer (:name 'copy)
        (js2-print-tree ast)
        (skip-chars-backward " \t\n")
        (should (string= code-string (buffer-substring-no-properties
                                      (point-min) (point))))))))

(defmacro* js2-deftest-ast (name code-string &key bind syntax-error)
  "Parse CODE-STRING.  If SYNTAX-ERROR is nil, print syntax tree
with `js2-print-tree' and assert the result to be equal to the
original string.  If SYNTAX-ERROR is passed, expect syntax error
highlighting substring equal to SYNTAX-ERROR value.
BIND defines bindings to apply them around the test."
  `(ert-deftest ,name ()
     (let ,(append bind '((js2-basic-offset 2)))
       (js2-test-ast-string ,code-string :syntax-error ,syntax-error))))

(put 'js2-deftest-ast 'lisp-indent-function 'defun)

;;; Callers of `js2-valid-prop-name-token'.

(js2-deftest-ast parse-property-access-when-not-keyword
  "A.foo = 3;")

(js2-deftest-ast parse-property-access-when-keyword
  "A.in = 3;"
  :bind ((js2-allow-keywords-as-property-names t)))

(js2-deftest-ast parse-property-access-when-keyword-no-xml
  "A.in = 3;"
  :bind ((js2-allow-keywords-as-property-names t)
         (js2-compiler-xml-available nil)))

(js2-deftest-ast parse-array-literal-when-not-keyword
  "a = {b: 1};")

(js2-deftest-ast parse-array-literal-when-keyword
  "a = {in: 1};"
  :bind ((js2-allow-keywords-as-property-names t)))

;;; 'of' contextual keyword.

(js2-deftest-ast parse-array-comp-loop-with-of
  "[a for (a of [])];")

(js2-deftest-ast parse-for-of
  "for (var a of []) {\n}")

(js2-deftest-ast of-can-be-var-name
  "var of = 3;")

(js2-deftest-ast of-can-be-function-name
  "function of() {\n}")

;;; Destructuring binding.

(js2-deftest-ast destruct-in-declaration
  "var {a, b} = {a: 1, b: 2};")

(js2-deftest-ast destruct-in-arguments
  "function f({a: aa, b: bb}) {\n}")

(js2-deftest-ast destruct-in-array-comp-loop
  "[a + b for ([a, b] in [[0, 1], [1, 2]])];")

(js2-deftest-ast destruct-in-catch-clause
  "try {\n} catch ({a, b}) {\n  a + b;\n}")

;;; Function parameters.

(js2-deftest-ast function-with-default-parameters
  "function foo(a = 1, b = a + 1) {\n}")

(js2-deftest-ast function-with-no-default-after-default
  "function foo(a = 1, b) {\n}"
  :syntax-error "b")

(js2-deftest-ast function-with-destruct-after-default
  "function foo(a = 1, {b, c}) {\n}"
  :syntax-error "{")

(js2-deftest-ast function-with-rest-parameter
  "function foo(a, b, ...rest) {\n}")

(js2-deftest-ast function-with-param-after-rest-parameter
  "function foo(a, ...b, rest) {\n}"
  :syntax-error "rest")

(js2-deftest-ast function-with-destruct-after-rest-parameter
  "function foo(a, ...b, {}) {\n}"
  :syntax-error "{}")

(js2-deftest-ast function-with-rest-after-default-parameter
  "function foo(a = 1, ...rest) {\n}")
