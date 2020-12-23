;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil
  (sentence-end-double-space)
  (checkdoc-arguments-in-order-flag)
  (checkdoc-verb-check-experimental-flag)
  (checkdoc-force-docstrings-flag)
  ;; To use the bug-reference stuff, do:
  ;;     (add-hook 'text-mode-hook #'bug-reference-mode)
  ;;     (add-hook 'prog-mode-hook #'bug-reference-prog-mode)
  (bug-reference-bug-regexp . "#\\(?2:[[:digit:]]+\\)")
  (bug-reference-url-format . "https://github.com/vspinu/sesman/issues/%s"))
 (emacs-lisp-mode
  (eval . (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))
  (indent-tabs-mode)
  (fill-column . 80)
  (emacs-lisp-docstring-fill-column . 80)))
