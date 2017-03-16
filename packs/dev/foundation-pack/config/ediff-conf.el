;; From https://www.emacswiki.org/emacs/EdiffMode

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

(defcustom live-ediff-restore-windows t
  "Restore the window configuration that was in place before
calling ediff"
  :type 'boolean
  :package-version '(emacs-live . live-version)
  :group 'emacs-live)

(defun live-ediff-quit ()
  "Function to be called when ediff quits."
  (when live-ediff-restore-windows
    (winner-undo)))

(add-hook 'ediff-quit-hook 'live-ediff-quit)
