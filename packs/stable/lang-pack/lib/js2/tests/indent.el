(require 'ert)
(require 'js2-mode)

(defun js2-test-indent (content)
  (let ((s (replace-regexp-in-string "^ *|" "" content)))
    (with-temp-buffer
      (insert (replace-regexp-in-string "^ *" "" s))
      (js2-mode)
      (indent-region (point-min) (point-max))
      (should (string= s (buffer-substring-no-properties
                          (point-min) (point)))))))

(defmacro* js2-deftest-indent (name content &key bind)
  `(ert-deftest ,(intern (format "js2-%s" name)) ()
     (let ,(append '((js2-basic-offset 2)
                     (js2-pretty-multiline-declarations t)
                     (inhibit-point-motion-hooks t))
                   bind)
       (js2-test-indent ,content))))

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
