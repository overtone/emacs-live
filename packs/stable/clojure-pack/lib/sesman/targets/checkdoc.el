
(let ((sentence-end-double-space)
      (checkdoc-arguments-in-order-flag)
      (checkdoc-verb-check-experimental-flag)
      (checkdoc-force-docstrings-flag))
  (checkdoc-file "sesman-test.el")
  (checkdoc-file "sesman.el"))
