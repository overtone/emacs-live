(live-add-pack-lib "emacs-git-gutter")
(require 'git-gutter)

(global-git-gutter-mode nil)
(setq git-gutter:window-width 2)

(setq git-gutter:lighter "GG")

(setq git-gutter:modified-sign "~") ;; two space
(setq git-gutter:added-sign "+")    ;; multiple character is OK
(setq git-gutter:deleted-sign "-")
