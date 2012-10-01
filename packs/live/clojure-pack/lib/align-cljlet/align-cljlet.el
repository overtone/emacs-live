;;; align-cljlet.el --- Space align various Clojure forms 

;; Copyrigth (C) 2011  Glen Stampoultzis

;; Author: Glen Stampoultzis <gstamp(at)gmail.com>
;; Version: 0.2
;; Package-Requires: ((clojure-mode "1.11.5"))
;; Keywords; clojure, align, let
;; URL: https://github.com/gstamp/align-cljlet
;;

;; This file is *NOT* part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; Description:
;;
;; This program exists because I was tired of manually aligning let
;; statements in clojure.  This program is designed to quickly and
;; easily allow let forms to be aligned.  This is my first emacs
;; lisp program and as a result if probably less than optimal.  Feel
;; free to suggest improvements or send in patches.
;;
;; This program was inspired by align-let.el although does not share
;; any of it's code.  I had considered altering align-let.el to
;; work correctly with Clojure however it was easiler to simply
;; start from scratch.
;;
;;; Changes:
;;
;; 14-Jan-2011 - Initial release
;; 23-Jan-2011 - Bug fixes and code cleanup.
;; 02-Apr-2012 - Package up for Marmalade
;; 30-Aug-2012 - Support for aligning defroute.
;;
;;; Known limitations:
;;
;; * This program requires clojure mode to be running in order to
;;   function correctly.
;;
;;; Installation:
;;
;;   To use align-cljlet.el, put it in your load-path and add
;;   the following to your .emacs
;;
;;   (require 'align-cljlet)
;;
;;; Usage:
;;
;; To invoke simply position anywhere inside the let statement and
;; invoke:
;;
;; M-x align-cljlet
;;
;; You may wish to bind this to a specific key.
;;
;; Contains one custom variable called defroute-columns which is
;; used to determine how many columns to align in a defroute call.
;; Defaults to 1.
;;

(defcustom defroute-columns 1
  "The number of columns to align in a defroute call"
  :type 'integer
  :group 'align-cljlet)

(defun acl-found-alignable-form ()
  "Check if we are currently looking at a let form"
  (save-excursion
    (if (looking-at "(")
        (progn 
          (down-list)
          (let ((start (point))
                name)
            (forward-sexp)
            (setq name (buffer-substring-no-properties start (point)))
            (or
             (string-match " *let" name)
             (string-match " *when-let" name)
             (string-match " *if-let" name)
             (string-match " *binding" name)
             (string-match " *loop" name)
             (string-match " *with-open" name)
             (string-match " *defroutes" name)
             )))
      (if (looking-at "{")
          t))))

(defun acl-try-go-up ()
  "Go upwards if possible.  If we can't then we're obviously not in an
   alignable form."
  (condition-case nil
      (up-list -1)
    (error
     (error "Not in a \"let\" form")))
  t)

(defun acl-find-alignable-form ()
  "Find the let form by moving looking upwards until nowhere to go"
  (while
      (if (acl-found-alignable-form)
          nil
        (acl-try-go-up)
        ))
  t)

(defun acl-goto-next-pair ()
  "Skip ahead to the next definition"
  (condition-case nil
      (progn
        (forward-sexp)
        (forward-sexp)
        (forward-sexp)
        (backward-sexp)
        t)
    (error nil)))

(defun acl-get-width ()
  "Get the width of the current definition"
  (save-excursion
    (let ((col (current-column)))
      (forward-sexp)
      (- (current-column) col))))

(defun acl-has-next-sexp ()
  "Checks if there is another sexp after the point"
  (save-excursion
    (condition-case nil
        (progn
          (forward-sexp)
          't)
      ('error nil))))

(defun acl-next-sexp ()
  "Goes to the next sexp, returning true or false if there is no next"

  (condition-case nil
      (progn
        (forward-sexp 2)
        (backward-sexp)
        't)
    ('error nil)))

(defun acl-calc-route-widths ()
  "Calculate the widths required to align a defroutes macro"
  (save-excursion
    (let ((width1 0)
          (width2 0)
          (width3 0))
      (while (progn
               (down-list)
               (if (> (acl-get-width) width1)
                   (setq width1 (acl-get-width)))
               ;; next column
               (if (and (acl-next-sexp) (> (acl-get-width) width2))
                   (setq width2 (acl-get-width)))
               ;; next column
               (if (and (acl-next-sexp) (> (acl-get-width) width3))
                   (setq width3 (acl-get-width)))

               (up-list)
               (acl-has-next-sexp)))
      (list width1 width2 width3))))

(defun acl-calc-width ()
  "Calculate the width needed for all the definitions in the form"
  (save-excursion
    (let ((width 0))
      (while (progn
               (if (> (acl-get-width) width)
                   (setq width (acl-get-width)))
               (acl-goto-next-pair)))
      width)))

(defun acl-lines-correctly-paired ()
  "Determine if all the pairs are on different lines"
  (save-excursion
    (let ((current-line (line-number-at-pos)))
      (while (progn
               (acl-goto-next-pair))
        (if (= current-line (line-number-at-pos))
            (error "multiple pairs on one line")
          (setq current-line (line-number-at-pos))))))
  t)

(defun acl-respace-single-let (max-width)
  "Respace the current definition"
  (acl-respace-subform (list max-width)))

(defun acl-respace-subform (widths)
  "Respace a defroute subform using the widths given. Point must
be positioned on the first s-exp in the subform."
  (save-excursion
    (while widths
      (let (col
            current-width
            difference
            (max-width (car widths)))
        (setq col (current-column))
        (if (acl-next-sexp)
            (progn
              (setq current-width (- (- (current-column) col) 1)
                    difference    (- max-width current-width))
              (cond ((> difference 0)
                     (insert (make-string difference ? )))
                    ((< difference 0)
                     (delete-backward-char (abs difference)))))))
      
      (setq widths (cdr widths)))
    )
  )

(defun acl-respace-defroute-form (widths)
  "Respace the entire defroute definition. Point must be
positioned on the defroute form."
  (let ((begin (point)))
    (while (progn
             (down-list)
             (acl-respace-subform widths)
             (up-list)
             (acl-has-next-sexp)
             ))
    (indent-region begin (point))))

(defun acl-respace-form (width)
  "Respace the entire definition"
  (let ((begin (point)))
    (while (progn
             (acl-respace-single-let width)
             (acl-goto-next-pair)))
    (indent-region begin (point))))

(defun acl-take-n (n xs)
  "Take n elements from a list returning a new list"
  (butlast xs (- (length xs) n)))

(defun acl-align-form ()
  "Determine what type of form we are currently positioned at and align it"
  (if (looking-at "( *defroutes")
      (progn
        (down-list 1)
        (forward-sexp 3)
        (backward-sexp)                 ; this position's us back at the start of the
                                        ; first form.
        (acl-respace-defroute-form (acl-take-n defroute-columns (acl-calc-route-widths))))
    (progn
      (if (not (looking-at "{"))
          ;; move to start of [
          (down-list 2)
        (down-list 1))
      
      (if (acl-lines-correctly-paired)
          (let ((w (acl-calc-width)))
            (acl-respace-form w)
            )))))

;; Borrowed from align-let.el:
(defun acl-backward-to-code ()
  "Move point back to the start of a preceding sexp form.
This gets out of strings, comments, backslash quotes, etc, to a
place where it makes sense to start examining sexp code forms.

The preceding form is found by a `parse-partial-sexp' starting
from `beginning-of-defun'.  If it finds nothing then just go to
`beginning-of-defun'."

  (let ((old (point)))
    (beginning-of-defun)
    (let ((parse (parse-partial-sexp (point) old)))
      (cond ((nth 2 parse) ;; innermost sexp
             (goto-char (nth 2 parse))
             (forward-sexp))
            ((nth 8 parse) ;; outside of comment or string
             (goto-char (nth 8 parse)))
            ((nth 5 parse) ;; after a quote char
             (goto-char (1- (point))))))))


(defun align-cljlet ()
  "Align a let form so that the bindings neatly align into columns"
  (interactive)
  (save-excursion
    (acl-backward-to-code)
    (if (acl-find-alignable-form)
        (acl-align-form))))

(provide 'align-cljlet)

;;; align-cljlet.el ends here
