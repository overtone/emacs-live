(live-add-pack-lib "emacs-git-gutter")
(require 'git-gutter)

(setq git-gutter:window-width 2)

(global-git-gutter-mode t)

(setq git-gutter:lighter " ++")
(setq git-gutter:always-show-gutter t)

(setq git-gutter:modified-sign "~ ") ;; two space
(setq git-gutter:added-sign "+ ")    ;; multiple character is OK
(setq git-gutter:deleted-sign "- ")
(setq git-gutter:unchanged-sign "  ")
