(live-add-pack-lib "emacs-git-gutter")
(require 'git-gutter)

(global-git-gutter-mode nil)
(setq git-gutter:window-width 2)

(setq git-gutter:diff-option "-w")
(setq git-gutter:lighter " GG")

(setq git-gutter:modified-sign "~") ;; two space
(setq git-gutter:added-sign "+")    ;; multiple character is OK
(setq git-gutter:deleted-sign "-")

(set-face-background 'git-gutter:modified "purple") ;; background color
(set-face-foreground 'git-gutter:added "green")
(set-face-foreground 'git-gutter:deleted "red")
