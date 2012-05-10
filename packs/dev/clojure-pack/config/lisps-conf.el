(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

(dolist (x '(scheme emacs-lisp lisp))
  (add-hook (intern (concat (symbol-name x) "-mode-hook")) 'enable-paredit-mode)
  (add-hook (intern (concat (symbol-name x) "-mode-hook")) 'rainbow-delimiters-mode))
