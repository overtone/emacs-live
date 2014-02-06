(let* ((current-directory (file-name-directory load-file-name))
       (features-directory (expand-file-name ".." current-directory))
       (project-directory (expand-file-name ".." features-directory)))
  (setq expand-region-root-path project-directory))

(add-to-list 'load-path expand-region-root-path)

(require 'expand-region)
(require 'espuds)
(require 'ert)

(Before
 (global-set-key (kbd "C-@") 'er/expand-region)
 (global-set-key (kbd "C-S-@") 'er/contract-region)
 (switch-to-buffer
  (get-buffer-create "*expand-region*"))
 (erase-buffer)
 (fundamental-mode)
 (transient-mark-mode 1)
 (cua-mode 0)
 (setq er--show-expansion-message t)
 (setq set-mark-default-inactive nil)
 (deactivate-mark))

(After)
