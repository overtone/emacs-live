;;; iy-go-to-char.el -- Go to next CHAR which is similar to "f" in vim

;; Copyright (C) 2009 Ian Yang

;; Author: Ian Yang <doit dot ian (at) gmail dot com>
;; Keywords: navigation, search
;; Filename: iy-go-to-char.el
;; Description: Go to char
;; Created: 2009-08-23 01:27:34
;; Version: 1.1
;; Last-Updated: 2012-04-16 08:42:00
;; URL: http://www.emacswiki.org/emacs/download/iy-go-to-char.el
;; Compatibility: GNU Emacs 23.1.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package defines the function `iy-go-to-char' which behaves
;; like "f" in vim. It reads a char and go the next Nth occurence of
;; the char. User can continue such search using that char key.

;; To use, make sure this file is on your `load-path' and put the
;; following in your .emacs file:
;;
;; (require 'iy-go-to-char)
;;
;; Then you can bind functions like:
;;
;; (global-set-key (kbd "C-c f") 'iy-go-to-char)
;; (global-set-key (kbd "C-c F") 'iy-go-to-char-backward)
;; (global-set-key (kbd "C-c ;") 'iy-go-to-char-continue)
;; (global-set-key (kbd "C-c ,") 'iy-go-to-char-continue-backward)

;; Except repeating the char key, followings keys are defined before
;; quitting the search:
;;
;;    ;   -- search forward the char, customizable:
;;           `iy-go-to-char-key-forward'
;;
;;    ,   -- search backward the char, customizable:
;;           `iy-go-to-char-key-backward'
;;
;;    C-g -- quit
;;
;;    C-s -- start `isearch-forward' using char as initial search
;;           string
;;
;;    C-r -- start `isearch-backward' using char as initial search
;;           string
;;
;;    C-w -- quit and kill region between start and current point. If region is
;;           activated before search, then use the original mark instead of the
;;           start position.
;;
;;    M-w -- quit and save region between start and current point. If region is
;;           activated before search, use the mark instead of start position.
;;
;; All other keys will quit the search. Then the key event is
;; intepreted in the original environment before search.
;;
;; if the search quits because of error or using "C-g", point is set
;; back to the start position. Otherwise, point is not change and the
;; start position is set as marker. So you can use "C-x C-x" back to
;; that position.

;; `iy-go-to-char-backward' search backward by default. Not like in
;; "vim", ";" is always searching forward and "," is searching
;; backward, whether the search is started forward or backward. It
;; does be the same even when the search char is ";" or ",". Also the
;; search can cross lines. To continue search last char, use
;; `iy-go-to-char-continue' and `iy-go-to-char-continue-backward'.

;;; Change Log:
;; 2012-04-16 (1.1)
;;    fix C-s/C-r to enter isearch

;;; Code:

(defgroup iy-go-to-char nil
  "go to char like f in vim."
  :link '(emacs-commentary-link "iy-go-to-char")
  :prefix "iy-go-to-char-"
  :group 'matching)

(defcustom iy-go-to-char-key-forward ?\;
  "Default key used to go to next occurence of the char"
  :type 'character
  :group 'iy-go-to-char)
(defcustom iy-go-to-char-key-backward ?\,
  "Default key used to go to previous occurence of the char"
  :type 'character
  :group 'iy-go-to-char)

(defvar iy-go-to-char-last-char nil
  "last char used in iy-go-to-char"
  )

(defun iy-go-to-char-isearch-setup ()
  (remove-hook 'isearch-mode-hook 'iy-go-to-char-isearch-setup)
  (setq isearch-string (if iy-go-to-char-last-char (string iy-go-to-char-last-char) ""))
  (isearch-search-and-update))

;;;###autoload
(defun iy-go-to-char (n char)
  "Move forward to Nth occurence of CHAR.

Typing key of CHAR will move to the next occurence of CHAR.
Typing ; will move to the next occurence of CHAR.
Typing , will move to the previous occurence of CHAR.
Typing C-g will quit and return to the original point.
Typing C-s or C-r will start `isearch` using CHAR.
Typing C-w or M-w will kill/copy between current point and the start point.
Unless quit using C-g or the region is activated before searching, the start
 point is set as mark.
"
  (interactive "p\ncGo to char: ")
  (let ((count (if (zerop n) 1 n))
        (cont t)
        (orig (if (region-active-p) (mark) (point)))
        (dir (if (< n 0) -1 1))
        ev pt)
    (when (and (= char ?\C-z) iy-go-to-char-last-char)
      (setq char iy-go-to-char-last-char))
    (save-excursion
      (search-forward (string char) nil nil count)
      (setq pt (match-end 0))
      (setq iy-go-to-char-last-char char) ;; save char only if success
      (while cont
        (setq ev (read-event))
        (cond ((eq ev iy-go-to-char-key-forward)
               (search-forward (string char) nil nil 1)
               (setq pt (match-end 0)))
              ((eq ev iy-go-to-char-key-backward)
               (search-forward (string char) nil nil -1)
               (setq pt (match-end 0)))
              ((eq ev char)
               (search-forward (string char) nil nil dir)
               (setq pt (match-end 0)))
              ((eq ev ?\C-z)
               (search-forward (string char) nil nil dir)
               (setq pt (match-end 0)))
              (t
               (setq cont nil)))))
    (if (not pt)
        (push ev unread-command-events)
      (goto-char pt)
      (push-mark orig t)
      (cond
       ((or (eq ev ?\C-s) (eq ev ?\C-r))
        (add-hook 'isearch-mode-hook 'iy-go-to-char-isearch-setup)
        (if (eq ev ?\C-s) (isearch-forward) (isearch-backward)))
       ((eq ev ?\C-w)
        (goto-char pt)
        (push-mark orig t)
        (kill-region orig pt))
       ((eq ev ?\M-w)
        (goto-char pt)
           (push-mark orig t)
           (kill-ring-save orig pt))
       (t
        (push ev unread-command-events)
        (goto-char pt)
        (push-mark orig t))))))

;;;###autoload
(defun iy-go-to-char-backward (n char)
  "Move backward to Nth occurence of CHAR.
Typing key of CHAR will move to the previous occurence of CHAR.
Typing ; will move to the next occurence of CHAR.
Typing , will move to the previous occurence of CHAR.
Typing C-g will quit and return to the original point.
Typing C-s or C-r will start `isearch` using CHAR"
  (interactive "p\ncGo to char: ")
  (iy-go-to-char (- n) char))

;;;###autoload
(defun iy-go-to-char-continue (n)
  "Continue last `iy-go-to-char` or `iy-go-to-char-backward`"
  (interactive "p")
  (when iy-go-to-char-last-char
    (iy-go-to-char n iy-go-to-char-last-char)))

;;;###autoload
(defun iy-go-to-char-continue-backward (n)
  "Continue last `iy-go-to-char` or `iy-go-to-char-backward`"
  (interactive "p")
  (when iy-go-to-char-last-char
    (iy-go-to-char (- n) iy-go-to-char-last-char)))



(provide 'iy-go-to-char)
