(live-add-pack-lib "nrepl")
(require 'nrepl )

(defun live-windows-hide-eol ()
 "Do not show ^M in files containing mixed UNIX and DOS line endings."
 (interactive)
 (setq buffer-display-table (make-display-table))
 (aset buffer-display-table ?\^M []))


(when (eq system-type 'windows-nt)
  (add-hook 'nrepl-mode-hook 'live-windows-hide-eol ))

(add-hook 'nrepl-interaction-mode-hook
          (lambda ()
            (nrepl-turn-on-eldoc-mode)
            (enable-paredit-mode)))

(add-hook 'nrepl-mode-hook
          (lambda ()
            (nrepl-turn-on-eldoc-mode)
            (enable-paredit-mode)
            (define-key nrepl-mode-map
              (kbd "{") 'paredit-open-curly)
            (define-key nrepl-mode-map
              (kbd "}") 'paredit-close-curly)))

(setq nrepl-popup-stacktraces nil)
(add-to-list 'same-window-buffer-names "*nrepl*")

;;Auto Complete
(live-add-pack-lib "ac-nrepl")
(require 'ac-nrepl )
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))
