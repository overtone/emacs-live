;; zenburn-emacs theme

(live-add-pack-lib "zenburn-emacs")

(defun color-theme-zenburn ()
  "Zenburn theme from zenburn-emacs"
  (interactive)
  (require 'zenburn-theme)
  (load-theme 'zenburn t)
  ;; Fixes the selection background color
  (set-face-attribute 'region nil :background "#666"))
