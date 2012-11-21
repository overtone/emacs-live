(live-add-pack-lib "live-fontify-hex")
(require 'live-fontify-hex)

(font-lock-add-keywords 'lisp-mode
                        '((live-fontify-hex-colors)))

(font-lock-add-keywords 'emacs-lisp-mode
                        '((live-fontify-hex-colors)))

(font-lock-add-keywords 'lisp-interaction-mode
                        '((live-fontify-hex-colors)))

(font-lock-add-keywords 'css-mode
                        '((live-fontify-hex-colors)))
