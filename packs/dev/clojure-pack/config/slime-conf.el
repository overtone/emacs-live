(live-add-pack-lib "slime")
(require 'slime)
(slime-setup '(slime-repl slime-scratch slime-editing-commands))
(setq slime-protocol-version 'ignore)
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

;;why isn't this working?
;; (eval-after-load 'slime-repl-mode
;;   '(add-to-list (make-local-variable 'paredit-space-for-delimiter-predicates)
;;                 (lambda (_ _) nil)))
