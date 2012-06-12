(eval-after-load 'paredit
  ;; need a binding that works in the terminal
  '(define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp))

(require 'paredit)

(defun live-paredit-next-top-level-form ()
  (interactive)
  (while (ignore-errors (paredit-backward-up) t))
  (paredit-forward)
  (ignore-errors (search-forward "(")
                 (backward-char)))

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
