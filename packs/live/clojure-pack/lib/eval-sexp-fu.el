;;; eval-sexp-fu.el --- Tiny functionality enhancements for evaluating sexps.

;; Copyright (C) 2009 Takeshi Banse <takebi@laafc.net>
;; Author: Takeshi Banse <takebi@laafc.net>
;; Keywords: lisp, highlight, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Tiny functionality enhancements for evaluating sexps.
;; This package provides:
;; - Flashing the sexps during the evaluation.
;; - `eval-last-sexp' variants (inner-list/inner-sexp).

;;; Installation:
;;
;; Put the highlight.el to your load-path.
;; Then require this package.

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `eval-sexp-fu-flash-mode'
;;    Toggle EvalSexpFuFlash mode on or off. If this mode is on, some `eval-last-sexp'-ish commands will highlight the sexps during evaluation.
;;  `turn-on-eval-sexp-fu-flash-mode'
;;    Unequivocally turn on EvalSexpFuFlash mode
;;  `eval-sexp-fu-eval-sexp-inner-list'
;;    Evaluate the list _currently_ pointed at as sexp; print value in minibuffer.
;;  `eval-sexp-fu-eval-sexp-inner-sexp'
;;    Evaluate the sexp _currently_ pointed; print value in minibuffer.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `eval-sexp-fu-flash-face'
;;    *Face to use for showing the sexps' overlay during the evaluation.
;;    default = (quote eval-sexp-fu-flash)
;;  `eval-sexp-fu-flash-error-face'
;;    *Face to use for showing the sexps' overlay if the evaluation signaled any error. The error highlighting is take into account only if non-nil value.
;;    default = (quote eval-sexp-fu-flash-error)
;;  `eval-sexp-fu-flash-duration'
;;    *For time duration, highlight the evaluating sexps.
;;    default = 0.15
;;  `eval-sexp-fu-flash-error-duration'
;;    *For time duration, highlight the evaluating sexps signaled errors.
;;    default = 0.3
;;  `eval-sexp-fu-flash-function'
;;    *Function to be used to create all of the actual flashing implementations.
;;    default = (quote eval-sexp-fu-flash-default)
;;  `eval-sexp-fu-flash-doit-function'
;;    *Function to use for flashing the sexps.
;;    default = (quote eval-sexp-fu-flash-doit-simple)

;;; Note:
;;
;; For SLIME user, this package registers the setup clauses which set up the
;; flashers and the several interactive commands at `eval-after-load' the
;; 'slime phase. The interactive commands bellow will be defined,
;; `eval-sexp-fu-slime-eval-expression-inner-list',
;; `eval-sexp-fu-slime-eval-expression-inner-sexp'
;; and the pprint variants respectively.

;;; Code:

(eval-when-compile (require 'cl))
(require 'highlight)

(defgroup eval-sexp-fu nil
  "Tiny functionality enhancements for evaluating sexps."
  :prefix "eval-sexp-fu-"
  :group 'eval-sexp-fu)

;;; Flashing the sexps during the evaluation for just an eye candy.
(defface eval-sexp-fu-flash
  '((((class color)) (:background "blue" :foreground "white" :bold t))
    (t (:inverse-video t)))
  "Face for highlighting sexps during evaluation."
  :group 'eval-sexp-fu)
(defface eval-sexp-fu-flash-error
  '((((class color)) (:foreground "red" :bold t))
    (t (:inverse-video t)))
  "Face for highlighting sexps signaled errors during evaluation."
  :group 'eval-sexp-fu)

(defcustom eval-sexp-fu-flash-face 'eval-sexp-fu-flash
  "*Face to use for showing the sexps' overlay during the evaluation."
  :type 'face
  :group 'eval-sexp-fu)
(defcustom eval-sexp-fu-flash-error-face 'eval-sexp-fu-flash-error
  "*Face to use for showing the sexps' overlay if the evaluation signaled any error. The error highlighting is take into account only if non-nil value."
  :type 'face
  :group 'eval-sexp-fu)
(defcustom eval-sexp-fu-flash-duration 0.15
  "*For time duration, highlight the evaluating sexps."
  :type 'number
  :group 'eval-sexp-fu)
(defcustom eval-sexp-fu-flash-error-duration 0.3
  "*For time duration, highlight the evaluating sexps signaled errors."
  :type 'number
  :group 'eval-sexp-fu)
(defcustom eval-sexp-fu-flash-function 'eval-sexp-fu-flash-default
  "*Function to be used to create all of the actual flashing implementations."
  :type 'function
  :group 'eval-sexp-fu)

(defun esf-hl-highlight-bounds (bounds face buf)
  (with-current-buffer buf
    (hlt-highlight-region (car bounds) (cdr bounds) face)))
(defun esf-hl-unhighlight-bounds (bounds buf)
  (with-current-buffer buf
    (hlt-unhighlight-region (car bounds) (cdr bounds))))
(defun esf-flash-error-bounds (bounds buf face)
  (when face
    (let ((flash-error
           (lambda (bounds buf face)
             (esf-hl-highlight-bounds bounds face buf)
             (run-at-time eval-sexp-fu-flash-error-duration nil
                          'esf-hl-unhighlight-bounds
                          bounds buf))))
      (run-with-idle-timer (max eval-sexp-fu-flash-error-duration
                                eval-sexp-fu-flash-duration)
                           nil flash-error
                           bounds buf face))))
(defun* eval-sexp-fu-flash (bounds &optional (face eval-sexp-fu-flash-face) (eface eval-sexp-fu-flash-error-face))
  "BOUNS is either the cell or the function returns, such that (BEGIN . END).
Reurn the 4 values; bounds, highlighting, un-highlighting and error flashing procedure. This function is convenient to use with `define-eval-sexp-fu-flash-command'."
  (when (ignore-errors (preceding-sexp))
    (flet ((bounds () (if (functionp bounds) (funcall bounds) bounds)))
      (let ((b (bounds)) (buf (current-buffer)))
        (when b
          (funcall eval-sexp-fu-flash-function b face eface buf))))))
(defun eval-sexp-fu-flash-default (bounds face eface buf)
  "Create all of the actual flashing implementations. See also `eval-sexp-fu-flash'."
  (lexical-let ((bounds bounds) (face face) (eface eface) (buf buf))
    (values bounds
            (apply-partially 'esf-hl-highlight-bounds bounds face buf)
            (apply-partially 'esf-hl-unhighlight-bounds bounds buf)
            (apply-partially 'esf-flash-error-bounds bounds buf eface))))

(defcustom eval-sexp-fu-flash-doit-function 'eval-sexp-fu-flash-doit-simple
  "*Function to use for flashing the sexps.

Please see the actual implementations:
- `eval-sexp-fu-flash-doit-simple'
- `eval-sexp-fu-flash-doit-hold-on-error'"
  :type 'function
  :group 'eval-sexp-fu)
(defun eval-sexp-fu-flash-doit (do-it-thunk hi unhi)
  (funcall eval-sexp-fu-flash-doit-function do-it-thunk hi unhi))
(defun eval-sexp-fu-flash-doit-simple (do-it-thunk hi unhi)
  (funcall hi)
  (run-at-time eval-sexp-fu-flash-duration nil unhi)
  (funcall do-it-thunk))
(defun eval-sexp-fu-flash-doit-hold-on-error (do-it-thunk hi unhi)
  (funcall hi)
  (unwind-protect
       (funcall do-it-thunk)
    (run-at-time eval-sexp-fu-flash-duration nil unhi)))

(defmacro esf-konstantly (v)
  `(lambda (&rest _it) ,v))
(defmacro esf-unwind-protect-with-tracking (normallyp body unwind)
  (declare (indent 2))
  `(let (,normallyp)
     (unwind-protect
          (prog1 ,body
            (setq ,normallyp t))
       ,unwind)))
(defun esf-flash-doit (do-it-thunk hi unhi eflash)
  (esf-unwind-protect-with-tracking ret
      (eval-sexp-fu-flash-doit do-it-thunk hi unhi)
    (unless ret
      (funcall eflash))))

;; Entry point.
(defmacro define-eval-sexp-fu-flash-command (command form)
  "Install the flasher implemented as the COMMAND's around advice.

FORM is expected to return 4 values;
- A bounds (BEGIN . END) to be highlighted or nil.
- An actual highlighting procedure takes 0 arguments.
- An actual un-highliting procedure takes 0 arguments.
- An actual flashing error procedure takes 0 arguments.
See also `eval-sexp-fu-flash'."
  (declare (indent 1))
  `(defadvice ,command (around eval-sexp-fu-flash-region activate)
     (if eval-sexp-fu-flash-mode
         (multiple-value-bind (bounds hi unhi eflash) ,form
           (if bounds
               (esf-flash-doit (esf-konstantly ad-do-it) hi unhi eflash)
             ad-do-it))
       ad-do-it)))
(define-minor-mode eval-sexp-fu-flash-mode
    "Toggle EvalSexpFuFlash mode on or off. If this mode is on, some `eval-last-sexp'-ish commands will highlight the sexps during evaluation."
  :init-value t :global t)
(defun turn-on-eval-sexp-fu-flash-mode ()
  "Unequivocally turn on EvalSexpFuFlash mode
 (see also `eval-sexp-fu-flash-mode')."
  (interactive)
  (eval-sexp-fu-flash-mode 1))

;;; eval-inner- stuff.
(defun esf-funcall-and-eval-last-sexp (before eval-last-sexp)
  "Call 0 arg procedure BEFORE then call interactive command EVAL-LAST-SEXP."
  (save-excursion
    (funcall before)
    (call-interactively eval-last-sexp)))

(require 'rx)
(defun esf-forward-inner-sexp0 ()
  (flet ((poss ()
           (let
               ((prev (save-excursion (backward-sexp) (forward-sexp) (point)))
                (next (save-excursion (forward-sexp) (backward-sexp) (point))))
             (list prev (line-number-at-pos prev)
                   next (line-number-at-pos next)
                   (point) (line-number-at-pos)))))
    (cond ((looking-at (rx (or (syntax symbol) (syntax word)
                               (syntax open-parenthesis))))
           (forward-sexp))
          (t (destructuring-bind (pp pl np nl cp cl) (poss)
               (cond ((and (<=  pp cp) (<= cp np))
                      (cond ((= pl cl) (backward-sexp))
                            ((= nl cl))
                            ((< (- cl pl) (- nl cl)) (backward-sexp))
                            ((< (- nl cl) (- cl pl)))
                            (t (backward-sexp)))
                      (forward-sexp))
                     (t (backward-sexp) (forward-sexp))))))))
(defun esf-forward-inner-sexp ()
  (condition-case nil
      (esf-forward-inner-sexp0)
    (scan-error nil)))
(defun esf-backward-up-inner-list0 (steps)
  (unless steps (setq steps 1))
  (when (looking-at (rx (syntax open-parenthesis))) (decf steps))
  (dotimes (_ steps) (backward-up-list)))
(defun esf-backward-up-inner-list (steps)
  (condition-case nil
      (esf-backward-up-inner-list0 steps)
    (scan-error nil)))
(defun esf-end-of-backward-up-inner-list (steps)
  (esf-backward-up-inner-list steps)
  (esf-forward-inner-sexp))

(defun eval-sexp-fu-eval-sexp-inner-list (&optional arg)
  "Evaluate the list _currently_ pointed at as sexp; print value in minibuffer.

Interactivelly with numeric prefix argument, call to `backward-up-list' happens several times. This function is an \"Evaluate this N lists, please.\" thing."
  (interactive "P")
  (esf-funcall-and-eval-last-sexp (apply-partially
                                   'esf-end-of-backward-up-inner-list arg)
                                  'esf-eval-last-sexp))
(defun eval-sexp-fu-eval-sexp-inner-sexp ()
  "Evaluate the sexp _currently_ pointed; print value in minibuffer."
  (interactive)
  (esf-funcall-and-eval-last-sexp 'esf-forward-inner-sexp 'esf-eval-last-sexp))

(defmacro define-esf-eval-last-sexp-1 (command-name eval-last-sexp)
  "Define an interactive command COMMAND-NAME kind of EVAL-LAST-SEXP
such that ignores any prefix arguments."
  `(defun ,command-name ()
     (interactive)
     (let (current-prefix-arg)
       (call-interactively ',eval-last-sexp))))
(define-esf-eval-last-sexp-1 esf-eval-last-sexp eval-last-sexp)

;; Piece of code which defines the above inner-{sexp,list} functions.
;; This makes it possible to batch install the
;; eval-sexp-fu-eval-sexp-inner-{sexp,list} with below form.
;; * (define-eval-sexp-fu-eval-sexp eval-sexp-fu-eval-sexp eval-last-sexp)
;; Used by making the `slime-eval-last-expression' variant functions.
(defmacro define-esf-eval-sexp* (eval-last-sexp inner-sexp inner-list)
  "Based on EVAL-LAST-SEXP, define INNER-SEXP and INNER-LIST interactive commands."
  (declare (indent 1))
  `(progn
     (defun ,inner-sexp ()
       (interactive)
       (esf-funcall-and-eval-last-sexp 'esf-forward-inner-sexp
                                       ',eval-last-sexp))
     (defun ,inner-list (&optional arg)
       (interactive "P")
       (esf-funcall-and-eval-last-sexp (apply-partially
                                        'esf-end-of-backward-up-inner-list arg)
                                       ',eval-last-sexp))))
(defmacro define-eval-sexp-fu-eval-sexp (command-name-prefix eval-last-sexp)
  "Define -inner-sexp and -inner-list interactive commands prefixed by COMMAND-NAME-PREFIX based on EVAL-LAST-SEXP. Actual work is done by `define-esf-eval-sexp*'."
  (let ((esf-eval-last-sexp-1
         (intern (format "esf-%s-1" (symbol-name eval-last-sexp)))))
    `(progn
       (define-esf-eval-last-sexp-1 ,esf-eval-last-sexp-1 ,eval-last-sexp)
       (define-esf-eval-sexp* ,esf-eval-last-sexp-1
         ,@(mapcar (lambda (post)
                     (intern (concat (symbol-name command-name-prefix) post)))
                   '("-inner-sexp" "-inner-list"))))))

;;; initialize.
(defun esf-initialize ()
  (define-eval-sexp-fu-flash-command eval-last-sexp
    (eval-sexp-fu-flash (save-excursion
                          (backward-char)
                          (bounds-of-thing-at-point 'sexp))))
  (define-eval-sexp-fu-flash-command eval-defun
    (eval-sexp-fu-flash (save-excursion
                          (end-of-defun)
                          (beginning-of-defun)
                          (bounds-of-thing-at-point 'sexp))))
  (eval-after-load 'eev
    '(progn
      ;; `eek-eval-last-sexp' is defined in eev.el.
      (define-eval-sexp-fu-flash-command eek-eval-last-sexp
        (eval-sexp-fu-flash (cons (save-excursion (eek-backward-sexp))
                                  (point)))))))
(defun esf-initialize-slime ()
  (define-eval-sexp-fu-flash-command slime-eval-last-expression
    (eval-sexp-fu-flash (save-excursion
                          (backward-char)
                          (bounds-of-thing-at-point 'sexp))))
  (define-eval-sexp-fu-flash-command slime-pprint-eval-last-expression
    (eval-sexp-fu-flash (save-excursion
                          (backward-char)
                          (bounds-of-thing-at-point 'sexp))))
  (define-eval-sexp-fu-flash-command slime-eval-defun
    (eval-sexp-fu-flash (save-excursion
                          (slime-end-of-defun)
                          (slime-beginning-of-defun)
                          (bounds-of-thing-at-point 'sexp))))
  (progn
    ;; Defines:
    ;; `eval-sexp-fu-slime-eval-expression-inner-list',
    ;; `eval-sexp-fu-slime-eval-expression-inner-sexp'
    ;; and the pprint variants respectively.
    (define-eval-sexp-fu-eval-sexp eval-sexp-fu-slime-eval-expression
        slime-eval-last-expression)
    (define-eval-sexp-fu-eval-sexp eval-sexp-fu-slime-pprint-eval-expression
        slime-pprint-eval-last-expression)))

(eval-when (load eval)
  (esf-initialize)
  (eval-after-load 'slime
    '(esf-initialize-slime)))

(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "esf-forward-inner-sexp0")
      (expect ?p
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "s+exp")
          (goto-char (point-min))
          (esf-forward-inner-sexp0)
          (char-before)))
      (expect ?p
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "s+exp")
          (goto-char (1+ (point-min)))
          (esf-forward-inner-sexp0)
          (char-before)))
      (expect ?\)
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "s(exp)")
          (goto-char (1+ (point-min)))
          (esf-forward-inner-sexp0)
          (char-before)))
      (desc "esf-forward-inner-sexp0 same line, but far near the next")
      ;; Always previous, is this OK?
      (expect ?0
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "sexp0   sexp1")
          (goto-char (+ (point-min) 7))
          (esf-forward-inner-sexp0)
          (char-before)))
      (desc "esf-forward-inner-sexp0 across lines")
      (expect ?0
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "sexp0\n\n\n\nsexp1")
          (goto-char (point-min))
          (esf-forward-inner-sexp0)
          (char-before)))
      (expect ?0
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "sexp0\n\n\n\nsexp1")
          (goto-char (point-min))
          (forward-line)
          (esf-forward-inner-sexp0)
          (char-before)))
      (expect ?1
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "sexp0\n\n\n\nsexp1")
          (goto-char (point-min))
          (forward-line 3)
          (esf-forward-inner-sexp0)
          (char-before)))
      (expect ?1
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "sexp0\n\n\n\nsexp1")
          (goto-char (point-min))
          (forward-line 3)
          (esf-forward-inner-sexp0)
          (char-before)))
      (expect ?1
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "sexp0\n\n\n\nsexp1")
          (goto-char (point-min))
          (forward-line 4)
          (esf-forward-inner-sexp0)
          (char-before)))
      (desc "esf-forward-inner-sexp0 across lines (equal delta)")
      ;; Always previous lines', is this OK?
      (expect ?0
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "sexp0\n\n\n\nsexp1")
          (goto-char (point-min))
          (forward-line 2)
          (esf-forward-inner-sexp0)
          (char-before)))
      (desc "esf-forward-inner-sexp0 no more")
      (expect ?0
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "sexp0\n\n")
          (goto-char (point-max))
          (esf-forward-inner-sexp0)
          (char-before)))
      (desc "esf-forward-inner-sexp0 no less")
      (expect ?0
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "\n\nsexp0")
          (goto-char (point-min))
          (esf-forward-inner-sexp0)
          (char-before)))
      (desc "esf-forward-inner-sexp0 no any")
      (expect 5
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "\n\n\n\n")
          (goto-char (point-min))
          (esf-forward-inner-sexp0)
          (point)))
      )))

(provide 'eval-sexp-fu)
;;; eval-sexp-fu.el ends here
