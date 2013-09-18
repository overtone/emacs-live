(live-add-pack-lib "extempore")

(autoload 'extempore-mode "extempore.el" "" t)
(add-to-list 'auto-mode-alist '("\\.xtm$" . extempore-mode))

(add-hook 'extempore-mode-hook 'enable-paredit-mode)
(add-hook 'extempore-mode-hook 'rainbow-delimiters-mode)
