(live-add-pack-lib "clojure-mode")

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

(setq auto-mode-alist (append '(("\\.cljs$" . clojure-mode))
                              auto-mode-alist))

(dolist (x '(scheme emacs-lisp lisp clojure))
  (add-hook (intern (concat (symbol-name x) "-mode-hook")) 'enable-paredit-mode)
  (add-hook (intern (concat (symbol-name x) "-mode-hook")) 'rainbow-delimiters-mode))

(defun live-toggle-clj-keyword-string ()
  "convert the string or keyword at (point) from string->keyword or keyword->string."
  (interactive)
  (let* ((original-point (point)))
    (while (and (> (point) 1)
                (not (equal "\"" (buffer-substring-no-properties (point) (+ 1 (point)))))
                (not (equal ":" (buffer-substring-no-properties (point) (+ 1 (point))))))
      (backward-char))
    (cond
     ((equal 1 (point))
      (message "beginning of file reached, this was probably a mistake."))
     ((equal "\"" (buffer-substring-no-properties (point) (+ 1 (point))))
      (insert ":" (substring (live-delete-and-extract-sexp) 1 -1)))
     ((equal ":" (buffer-substring-no-properties (point) (+ 1 (point))))
      (insert "\"" (substring (live-delete-and-extract-sexp) 1) "\"")))
    (goto-char original-point)))

(define-key clojure-mode-map (kbd "C-:") 'live-toggle-clj-keyword-string)
