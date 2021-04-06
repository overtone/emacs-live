;; highlight expression on eval
(require 'highlight)
(live-add-pack-lib "eval-sexp-fu")
(live-add-pack-lib "cider-eval-sexp-fu")
(require 'eval-sexp-fu)
(require 'cider-eval-sexp-fu)

(setq eval-sexp-fu-flash-duration 0.5)

(defun live-bounds-of-preceding-sexp ()
  "Return the bounds of sexp before the point. Copies semantics
   directly from the fn preceding-sexp to ensure highlighted area
   is identical to that which is evaluated."
  (let ((opoint (point))
	ignore-quotes
	expr)
    (save-excursion
      (with-syntax-table emacs-lisp-mode-syntax-table
	;; If this sexp appears to be enclosed in `...'
	;; then ignore the surrounding quotes.
	(setq ignore-quotes
	      (or (eq (following-char) ?\')
		  (eq (preceding-char) ?\')))
	(forward-sexp -1)
	;; If we were after `?\e' (or similar case),
	;; use the whole thing, not just the `e'.
	(when (eq (preceding-char) ?\\)
	  (forward-char -1)
	  (when (eq (preceding-char) ??)
	    (forward-char -1)))

	;; Skip over hash table read syntax.
	(and (> (point) (1+ (point-min)))
	     (looking-back "#s" (- (point) 2))
	     (forward-char -2))

	;; Skip over `#N='s.
	(when (eq (preceding-char) ?=)
	  (let (labeled-p)
	    (save-excursion
	      (skip-chars-backward "0-9#=")
	      (setq labeled-p (looking-at "\\(#[0-9]+=\\)+")))
	    (when labeled-p
	      (forward-sexp -1))))

	(save-restriction
	  ;; vladimir@cs.ualberta.ca 30-Jul-1997: skip ` in
	  ;; `variable' so that the value is returned, not the
	  ;; name
	  (if (and ignore-quotes
		   (eq (following-char) ?`))
	      (forward-char))
	  (cons (point) opoint))))))

(defun live-bounds-of-defun ()
  "Return the bounds of the defun around point. Copies semantics
   directly from the fn eval-defun-2 to ensure highlighted area
   is identical to that which is evaluated."
  (save-excursion
    (end-of-defun)
    (beginning-of-defun)
    (setq beg (point))
    (read (current-buffer))
    (setq end (point))
    (cons beg end)))

;; fix up esf to highlight exactly what emacs evaluates
(defun live-esf-initialize-elisp ()
  (define-eval-sexp-fu-flash-command eval-last-sexp
    (eval-sexp-fu-flash (when (ignore-errors (preceding-sexp))
                          (with-esf-end-of-sexp
                            (live-bounds-of-preceding-sexp)))))
  (define-eval-sexp-fu-flash-command eval-defun
    (eval-sexp-fu-flash (live-bounds-of-defun))))

(live-esf-initialize-elisp)

;; cider extensions


(defun live-bounds-of-cider-last-sexp ()
  "Return the bounds of the defun around point. Copies semantics
   directly from the fn cider-last-sexp to ensure highlighted
   area is identical to that which is evaluated."
  (cons (save-excursion (backward-sexp) (point)) (point)))

(defun live-esf-initialize-cider ()
  (define-eval-sexp-fu-flash-command cider-eval-last-sexp
    (eval-sexp-fu-flash (live-bounds-of-cider-last-sexp)))

  (define-eval-sexp-fu-flash-command cider-pprint-eval-last-sexp
    (eval-sexp-fu-flash (live-bounds-of-cider-last-sexp)))

  (progn
    ;; Defines:
    ;; `eval-sexp-fu-cider-sexp-inner-list',
    ;; `eval-sexp-fu-cider-sexp-inner-sexp'
    ;; and the pprint variants respectively.
    (define-eval-sexp-fu-eval-sexp eval-sexp-fu-cider-eval-sexp
      cider-eval-last-sexp)
    (define-eval-sexp-fu-eval-sexp eval-sexp-fu-cider-pprint-eval-sexp
      cider-pprint-eval-last-sexp)))

(eval-after-load 'cider
  '(live-esf-initialize-cider))
