(live-add-pack-lib "clojure-mode")

;; tell clj-refactor not to eagerly eval namespaces
;; on connection (this totally conflicts with Overtone
;; namespaces that have ready-to-sound side-effecting
;; functions!
(setq cljr-eagerly-build-asts-on-startup nil)

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("(\\(fn\\)[\[[:space:]]"
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "λ")
                               nil))))))

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("\\(#\\)("
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "ƒ")
                               nil))))))

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("\\(#\\){"
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "∈")
                               nil))))))

(eval-after-load 'find-file-in-project
  '(add-to-list 'ffip-patterns "*.clj"))

(require 'clojure-mode)

(add-hook 'clojure-mode-hook
          (lambda ()
            (setq buffer-save-without-query t)))

;;command to align let statements
;;To use: M-x align-cljlet
(live-add-pack-lib "align-cljlet")
(require 'align-cljlet)

;;Treat hyphens as a word character when transposing words
(defvar clojure-mode-with-hyphens-as-word-sep-syntax-table
  (let ((st (make-syntax-table clojure-mode-syntax-table)))
    (modify-syntax-entry ?- "w" st)
    st))

(defun live-transpose-words-with-hyphens (arg)
  "Treat hyphens as a word character when transposing words"
  (interactive "*p")
  (with-syntax-table clojure-mode-with-hyphens-as-word-sep-syntax-table
    (transpose-words arg)))

(define-key clojure-mode-map (kbd "M-t") 'live-transpose-words-with-hyphens)

(dolist (x '(scheme emacs-lisp lisp clojure))
  (add-hook (intern (concat (symbol-name x) "-mode-hook")) 'enable-paredit-mode)
  (add-hook (intern (concat (symbol-name x) "-mode-hook")) 'rainbow-delimiters-mode))

(defun live-warn-when-cider-not-connected ()
      (interactive)
      (message "nREPL server not connected. Run M-x cider or M-x cider-jack-in to connect."))

(define-key clojure-mode-map (kbd "C-M-x")   'live-warn-when-cider-not-connected)
(define-key clojure-mode-map (kbd "C-x C-e") 'live-warn-when-cider-not-connected)
(define-key clojure-mode-map (kbd "C-c C-e") 'live-warn-when-cider-not-connected)
(define-key clojure-mode-map (kbd "C-c C-l") 'live-warn-when-cider-not-connected)
(define-key clojure-mode-map (kbd "C-c C-r") 'live-warn-when-cider-not-connected)
