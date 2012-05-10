(require 'backup-dir)
(make-variable-buffer-local 'backup-inhibited)
(setq bkup-backup-directory-info
      `((t ,live-backups-dir ok-create full-path prepend-name)))

(setq auto-save-file-name-transforms `((".*" ,(concat live-autosaves-dir "\\1") t)))
(setq backup-by-copying t)
(setq backup-directory-alist `((".*" . ,live-backups-dir)))
(setq auto-save-list-file-name (concat live-autosaves-dir "autosave-list"))

(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)
