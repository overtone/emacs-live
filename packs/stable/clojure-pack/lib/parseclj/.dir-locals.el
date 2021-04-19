((emacs-lisp-mode
  (buffer-save-without-query . t)
  (indent-tabs-mode . nil)
  (eval . (flycheck-mode))
  (eval . (checkdoc-minor-mode))
  (sentence-end-double-space . t)
  (emacs-lisp-docstring-fill-column . 75)
  (checkdoc-package-keywords-flag)
  (checkdoc-arguments-in-order-flag)))
