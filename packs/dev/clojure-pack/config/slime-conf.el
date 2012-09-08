(live-add-pack-lib "slime")
(require 'slime)

(slime-setup '(slime-repl slime-scratch slime-editing-commands))
(setq slime-protocol-version 'ignore)
(setq slime-net-coding-system 'utf-8-unix)

(add-hook 'slime-repl-mode-hook 'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook (lambda ()
                                  (define-key slime-repl-mode-map
                                    (kbd "DEL") 'paredit-backward-delete)
                                  (define-key slime-repl-mode-map
                                    (kbd "{") 'paredit-open-curly)
                                  (define-key slime-repl-mode-map
                                    (kbd "}") 'paredit-close-curly)
                                  (modify-syntax-entry ?\{ "(}")
                                  (modify-syntax-entry ?\} "){")
                                  (modify-syntax-entry ?\[ "(]")
                                  (modify-syntax-entry ?\] ")[")))

(defun live-clojure-reset-namespace ()
  "Reloads the current Clojure namespace by first removing it and
then re-evaluating the slime buffer. Use this to remove old
symbol definitions and reset the namespace to contain only what
is defined in your current Emacs buffer."
  (interactive)
  (slime-interactive-eval (concat "(remove-ns '" (slime-current-package) ")"))
  (slime-eval-buffer))



(defun live-windows-hide-eol ()
 "Do not show ^M in files containing mixed UNIX and DOS line endings."
 (interactive)
 (setq buffer-display-table (make-display-table))
 (aset buffer-display-table ?\^M []))


(when (eq system-type 'windows-nt)
  (add-hook 'slime-repl-mode-hook 'live-windows-hide-eol ))

;; Use clojure-mode SLIME Description popup buffer
(add-hook 'slime-popup-buffer-mode-hook
          (lambda ()
            (when (string-match "<clojure>\\*$"
                                (buffer-name))
              (clojure-mode))))
