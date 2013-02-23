(live-add-pack-lib "emacs-git-gutter")
(require 'git-gutter)


(global-git-gutter-mode t)

(setq git-gutter:diff-option "-w")
(setq git-gutter:lighter " GG")
