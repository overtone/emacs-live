(eval-after-load 'paredit
  ;; need a binding that works in the terminal
  '(define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp))

(require 'paredit)

(defun live-paredit-next-top-level-form ()
  (interactive)
  (while (ignore-errors (paredit-backward-up) t))
  (paredit-forward)
  (ignore-errors (search-forward "(")
                 (backward-char))
  (while (and (paredit-in-comment-p)
              (not (eobp)))
      (ignore-errors (search-forward "(")
                     (backward-char))))

(defun live-paredit-previous-top-level-form ()
  (interactive)
  (while (ignore-errors (paredit-backward-up) t))
  (paredit-backward))

(defun live-paredit-forward ()
  "Feels more natural to move to the beginning of the next item
in the sexp, not the end of the current one."
  (interactive)
  (if (and (not (paredit-in-string-p))
           (save-excursion
             (ignore-errors
               (forward-sexp)
               (forward-sexp)
               t)))
      (progn
        (forward-sexp)
        (forward-sexp)
        (backward-sexp))
    (paredit-forward)))

(defun live-paredit-forward-slurp-sexp-neatly ()
  (interactive)
  (save-excursion
    (cond ((or (paredit-in-comment-p)
               (paredit-in-char-p))
           (error "Invalid context for slurping S-expressions."))
          ((paredit-in-string-p)
           (paredit-forward-slurp-into-string))
          (t

           (save-excursion
             (paredit-forward-up)
             (paredit-backward-down)
             (paredit-forward-slurp-sexp)
             (live-delete-horizontal-space-except-one)))))
  (when (not (save-excursion
               (ignore-errors
                 (backward-sexp)
                 t)))
    (delete-horizontal-space)))
