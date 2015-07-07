;;; git-pack/init.el
(live-add-pack-lib "git-modes")
(require 'gitattributes-mode)
(require 'gitconfig-mode)
(require 'gitignore-mode)

(live-load-config-file "magit-conf.el")
