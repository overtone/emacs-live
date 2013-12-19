;; highlight expression on eval
(require 'highlight)
(live-add-pack-lib "eval-sexp-fu")
(require 'eval-sexp-fu)
(setq eval-sexp-fu-flash-duration 0.5)

;; cider extensions

(defun live-esf-initialize-cider ()
  (define-eval-sexp-fu-flash-command cider-eval-last-sexp
    (eval-sexp-fu-flash (with-esf-end-of-sexp
                         (bounds-of-thing-at-point 'sexp))))

  (define-eval-sexp-fu-flash-command cider-pprint-eval-last-sexp
    (eval-sexp-fu-flash (with-esf-end-of-sexp
                         (bounds-of-thing-at-point 'sexp))))

  (define-eval-sexp-fu-flash-command cider-eval-defun-at-point
    (eval-sexp-fu-flash  (with-esf-end-of-sexp
                           (when (not (and (live-paredit-top-level-p)
                                          (save-excursion
                                            (ignore-errors (forward-char))
                                            (live-paredit-top-level-p))))
                            (save-excursion
                              (save-match-data
                                (ignore-errors (live-paredit-forward-down))
                                (paredit-forward-up)
                                (while (ignore-errors (paredit-forward-up) t))
                                (let ((end (point)))
                                  (backward-sexp)
                                  (cons (point) end))))))))


  (progn
    ;; Defines:
    ;; `eval-sexp-fu-cider-sexp-inner-list',
    ;; `eval-sexp-fu-cider-sexp-inner-sexp'
    ;; and the pprint variants respectively.
    (define-eval-sexp-fu-eval-sexp eval-sexp-fu-cider-eval-sexp
      cider-eval-last-sexp)
    (define-eval-sexp-fu-eval-sexp eval-sexp-fu-cider-pprint-eval-sexp
      cider-pprint-eval-last-sexp)))

(defun live-esf-initialize-cider ()
  (define-eval-sexp-fu-flash-command cider-eval-last-sexp
    (eval-sexp-fu-flash (with-esf-end-of-sexp
                         (bounds-of-thing-at-point 'sexp))))

  (define-eval-sexp-fu-flash-command cider-pprint-eval-last-sexp
    (eval-sexp-fu-flash (with-esf-end-of-sexp
                         (bounds-of-thing-at-point 'sexp))))

  (define-eval-sexp-fu-flash-command cider-eval-defun-at-point
    (eval-sexp-fu-flash (let ((bounds (cider--region-for-defun-at-point)))
                          (cons (first bounds) (second bounds)))))


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
