(live-add-pack-lib "smartparens")

(require 'smartparens)

;; do not autoinsert ' pair if the point is preceeded by word.  This
;; will handle the situation when ' is used as a contraction symbol in
;; natural language.  Nil for second argument means to keep the
;; original definition of closing pair.
(sp-pair "'" nil :unless '(sp-point-after-word-p))

(defun live-sp-add-space-after-sexp-insertion (id action _context)
  (when (eq action 'insert)
    (save-excursion
      (forward-char (length (plist-get (sp-get-pair id) :close)))
      (when (or (eq (char-syntax (following-char)) ?w)
                (looking-at (sp--get-opening-regexp)))
        (insert " ")))))

;; this is called before the closing paren is inserted, and since we add
;; in front of the pair, it will also adjust the "active sexp overlay" accordingly.
(defun live-sp-add-space-before-sexp-insertion (id action _context)
  (when (eq action 'insert)
    (save-excursion
      (backward-char (length id))
      (when (or (eq (char-syntax (preceding-char)) ?w)
                (looking-at (sp--get-closing-regexp)))
        (insert " ")))))

;; emacs is lisp hacking enviroment, so we set up some most common
;; lisp modes too
(sp-with-modes sp--lisp-modes
  ;; disable ', it's the quote character!
  (sp-local-pair "'" nil :actions nil)
  ;; also only use the pseudo-quote inside strings where it serve as
  ;; hyperlink.
  (sp-local-pair "`" "'" :when '(sp-in-string-p))
  (sp-local-pair "(" nil
                 :pre-handlers '(live-sp-add-space-before-sexp-insertion)
                 :post-handlers '(live-sp-add-space-after-sexp-insertion)))

;; NOTE: Normally, `sp-local-pair' accepts list of modes (or a single
;; mode) as a first argument.  The macro `sp-with-modes' adds this
;; automatically.  If you want to call sp-local-pair outside this
;; macro, you MUST supply the major mode argument.

;; LaTeX modes
(sp-with-modes '(
                 tex-mode
                 plain-tex-mode
                 latex-mode
                 )
  ;; math modes, yay.  The :actions are provided automatically if
  ;; these pairs do not have global definition.
  (sp-local-pair "$" "$")
  (sp-local-pair "\\[" "\\]")
  (sp-local-pair "`" "'")
  (sp-local-tag "\\b" "\\begin{_}" "\\end{_}"))

;; html modes
(sp-with-modes '(
                 sgml-mode
                 html-mode
                 )
  (sp-local-pair "<" ">")
  (sp-local-tag  "<" "<_>" "</_>" :transform 'sp-match-sgml-tags))


;; handy lisp extensions

(defun live-sp-forward ()
  "Feels more natural to move to the beginning of the next item
   in the sexp, not the end of the current one."
  (interactive)
  (sp-forward-sexp)
  (sp-skip-forward-to-symbol))

(defun live-sp-next-top-level-form ()
  (interactive)
  (while (ignore-errors (sp-backward-up-sexp)))
  (live-sp-forward))

(defun live-sp-previous-top-level-form ()
  (interactive)
  (if (ignore-errors (sp-backward-up-sexp))
      (while (ignore-errors (sp-backward-up-sexp)))
    (sp-backward-sexp)))

(defun live-sp-forward-slurp-sexp-neatly ()
  (interactive)
  (save-excursion
    (cond ((sp-point-in-comment)
           (error "Invalid context for slurping S-expressions."))

          ((sp-point-in-string)
           (sp-forward-slurp-sexp))

          (t
           (save-excursion
             (sp-up-sexp)
             (sp-backward-down-sexp)
             (sp-forward-slurp-sexp)
             (just-one-space)))))
  (when (not (save-excursion
               (ignore-errors
                 (sp-backward-sexp)
                 t)))
    (delete-horizontal-space)))

(defun live-sp-forward-kill-sexp (&optional arg)
  (interactive "p")
  (cond ((sp-point-in-string-or-comment)
         (kill-word (or arg 1)))

        (t (sp-kill-sexp (or arg 1)))))

(defun live-sp-backward-kill ()
  (interactive)
  (let ((m (point-marker)))
    (sp-backward-up-sexp)
    (forward-char)
    (delete-region (point) m)))

(defun live-sp-delete-horizontal-space ()
  (interactive)
  (just-one-space -1)
  (sp-backward-delete-char))

;; Don't kill entire symbol with C-k
(setq sp-hybrid-kill-entire-symbol nil)
