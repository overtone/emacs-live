;;; window-number.el --- Select windows by numbers

;; Copyright (C) 2004 Johann "Myrkraverk" Oskarsson
;; <myrkraverk@users.sourceforge.net>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; Introduction
;; ============

;; Window number mode allows you to select windows by numbers.  This
;; edition now works with XEmacs as well as GNU Emacs.  The window
;; numbers do not show up in the mode-line in XEmacs yet, instead a
;; -?- is displayed.  Hopefully this can be fixed soon, but really
;; depends on XEmacs developers.

;; Installation
;; ============

;; Drop this file into your load path.  C-h v load-path RET or F1 v
;; load-path RET will help.  Then place the following lines into your
;; .emacs or ~/.xemacs/init.el and uncomment them.

;; ----------------------------------------------------------------------------

;; (autoload 'window-number-mode "window-number"
;;   "A global minor mode that enables selection of windows according to
;; numbers with the C-x C-j prefix.  Another mode,
;; `window-number-meta-mode' enables the use of the M- prefix."
;;   t)

;; (autoload 'window-number-meta-mode "window-number"
;;   "A global minor mode that enables use of the M- prefix to select
;; windows, use `window-number-mode' to display the window numbers in
;; the mode-line."
;;   t)

;; ----------------------------------------------------------------------------

;; Then you can use M-x window-number-mode RET to turn the mode on, or
;; place (window-number-mode 1) and (window-number-meta-mode 1) into
;; your .emacs or ~/.xemacs/init.el.

;; ----------------------------------------------------------------------------

;; Code starts here.

;; ----------------------------------------------------------------------------

(require 'cl); for set-difference and loop

(defun window-number-list ()
  "Returns a list of the windows, in fixed order and the
minibuffer (even if not active) last."
  (let* ((walk-windows-start
          (car (set-difference
                (window-list (selected-frame) t)
                (window-list (selected-frame) 1))))
         (walk-windows-current walk-windows-start)
         list)
    (while (progn
             (setq walk-windows-current
                   (next-window walk-windows-current t))
             (setq list (cons walk-windows-current list))
             (not (eq walk-windows-current walk-windows-start))))
    (reverse (cons (car list) (cdr list)))))

(defun window-number-select (number)
  "Selects the nth window."
  (interactive "P")
  (if (integerp number)
      (let ((window (nth (1- number) (window-number-list))))
        (if (and window (or (not (window-minibuffer-p window))
                            (minibuffer-window-active-p window)))
            (select-window window)
          (error "No such window.")))))

(defun window-number ()
  "Returns the the number of the current window."
  (length
   (memq (selected-window)
         (nreverse (window-number-list)))))

(defun window-number-string ()
  "Returns the string containing the number of the current window"
  (propertize
   (concat " -" (number-to-string (window-number)) "-")
   'face
   'window-number-face))

(defvar window-number-mode-map nil
  "Keymap for the window number mode.")

(defvar window-number-meta-mode-map nil
  "Keymap for the window number meta mode.")

(defmacro window-number-define-keys (mode-map prefix)
  `(progn
     ,@(loop for number from 1 to 10 collect
             `(define-key ,mode-map
                (kbd ,(concat prefix (number-to-string
                                      (if (>= number 10) 0 number))))
                (lambda nil (interactive)
                  (window-number-select ,number))))))

; define C-x C-j 1 to switch to win 1, etc (C-x C-j 0 = win 10)
(unless window-number-mode-map
  (setq window-number-mode-map (make-sparse-keymap))
  ; space after C-j is important
  (window-number-define-keys window-number-mode-map "C-x C-j "))

; define M-1 to switch to win 1, etc (M-0 = win 10)
(unless window-number-meta-mode-map
  (setq window-number-meta-mode-map (make-sparse-keymap))
  (window-number-define-keys window-number-meta-mode-map "M-"))

(if (featurep 'xemacs)
    (define-minor-mode window-number-mode
      "A global minor mode that enables selection of windows
according to numbers with the C-x C-j prefix.  Another mode,
`window-number-meta-mode' enables the use of the M- prefix."
      :global t
      :init-value nil
      :lighter " -?-")

  (define-minor-mode window-number-mode
    "A global minor mode that enables selection of windows
according to numbers with the C-x C-j prefix.  Another mode,
`window-number-meta-mode' enables the use of the M- prefix."
    :global t
    :init-value nil
    :lighter (:eval (window-number-string))))

(define-minor-mode window-number-meta-mode
  "A global minor mode that enables use of the M- prefix to
select windows, use `window-number-mode' to display the window
numbers in the mode-line."
  :global t
  :init-value nil)

;;(push (cons 'my-window-number-meta-mode my-window-number-mode-map)
;;       minor-mode-map-alist)


(defface window-number-face
  '((((type tty) (class color))
     (:background "red"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((type x w32 mac))
     (:foreground "red")))
  "The face used for the window number in the mode-line.")

(provide 'window-number)

;;; window-number.el ends here.
