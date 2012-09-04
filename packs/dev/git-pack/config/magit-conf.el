;;; git-pack/magit-conf.el

(live-add-pack-lib "magit")
(require 'magit)

(add-hook 'magit-log-edit-mode-hook
          (lambda ()
             (set-fill-column 72)
             (auto-fill-mode 1)))
