(live-add-pack-lib "elisp-slime-nav")
(require 'elisp-slime-nav)
(add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t)))
