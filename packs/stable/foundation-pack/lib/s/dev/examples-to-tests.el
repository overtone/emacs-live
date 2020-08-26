(require 'ert)

(defun examples-to-should-1 (examples)
  (let ((actual (car examples))
        (expected (cadr (cdr examples))))
    `(let ((previous-match-data (match-data)))
       (should (equal-including-properties ,actual ,expected))
       (should (equal (match-data) previous-match-data)))))

(defun examples-to-should (examples)
  (let (result)
    (while examples
      (setq result (cons (examples-to-should-1 examples) result))
      (setq examples (cddr (cdr examples))))
    (nreverse result)))

(defmacro defexamples (cmd &rest examples)
  (declare (indent 1))
  `(ert-deftest ,cmd ()
     ,@(examples-to-should examples)))

(defun def-example-group (&rest _)) ; ignore

(provide 'examples-to-tests)
