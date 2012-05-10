;;;; quick-jump.el ---Remember current position,and jump back

;; Filename: quick-jump.el
;; Description:Remember current position,and jump back cyclely.
;; Author: Joseph <jixiuf@gmail.com>
;; Maintainer: Joseph <jixiuf@gmail.com>
;; Copyright (C) 2011~, Joseph, all rights reserved.
;; Created: 2011-02-22
;; Version: 0.1.0
;; URL: http://www.emacswiki.org/quick-jump.el
;; Keywords: quick jump marker
;; Compatibility: (Test on GNU Emacs 23.2.1).
;;
;;
;;; This file is NOT part of GNU Emacs
;;
;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;   quick-jump.el works like `register' in Emacs. but it is
;;   different to it ,quick-jump.el push current position to a
;;   ring named `qj-marker-ring' then you can go back(or forward)
;;   to the position ,and you can clear all the position in `qj-marker-ring'
;;   to manager the ring by `quick-jump-clear-all-marker'.
;;
;;   Suppose you have run `quick-jump-push-marker' on line 1,5,10
;;   then three positions is pushed into `qj-marker-ring'. now the ring is
;;   [10,5,1],you  are on line 10 now ,then you run  `quick-jump-go-back'
;;   you will be back to line 5, the ring still is [10,5,1]. but
;;   `qj-current-marker' is not the last position you have pushed on line
;;   10, it's line 5 now.
;;   go back and go forward are related to `qj-current-marker'.
;;
;;   when you `quick-jump-go-back' or `quick-jump-go-forward'
;;   it will decide whether push current position to `qj-marker-ring'
;;   automaticly or not . when the count of lines between current line
;;   and the line of `qj-current-marker' are larger than `qj-line-count',
;;   then current position will be automaticly put into `qj-marker-ring'
;;
;;   So suppose the ring is [15,10,5,1] and `qj-current-marker' is 10 ,if you
;;   are on line 100 now ,and you run `quick-jump-go-back' now you position
;;   is go back to line 5,the ring become [100,15,10.5.1],because
;;   100-10>`qj-line-count' it is larger than `qj-line-count' default value
;;   10.but if you are on line 16 and run `quick-jump-go-back', the ring still
;;   is [15,10,5,1] ,it isn't [16,15,10,5,1],because 16-10<`qj-line-count'.
;;
;;
;;; Install:
;;
;; Just put quick-jump.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'quick-jump)
;; (quick-jump-default-keybinding)
;;
;; or
;;
;; (autoload 'quick-jump-push-marker "quick-jump"
;;   " push current marker in ring. you can jump back" t)
;; (autoload 'quick-jump-go-back "quick-jump"
;;   "Go back in `qj-marker-ring'")
;; (autoload 'quick-jump-go-forward "quick-jump"
;;   "Go forward in `qj-marker-ring'")
;; (autoload 'quick-jump-clear-all-marker "quick-jump"
;;   "clear all marker in `qj-marker-ring'.")
;; (autoload 'quick-jump-default-keybinding "quick-jump"
;;   "default keybindings for quick-jump" nil)
;; (quick-jump-default-keybinding)


;;; Commands:
;;
;; Below are complete command list:
;;
;;  `quick-jump-push-marker'
;;    push current marker in ring. you can jump back
;;  `quick-jump-go-back'
;;    Go back in `qj-marker-ring'.
;;  `quick-jump-go-forward'
;;    Go forward in `qj-marker-ring'.
;;  `quick-jump-clear-all-marker'
;;    clear all marker in `qj-marker-ring'.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `qj-line-count'
;;    about the meaning of me see Commentary.
;;    default = 10

(require 'ring)

(defgroup quick-jump nil
  "Remember current position,and jump back."
  :group 'convenience)

(defcustom  qj-line-count 10
  "about the meaning of me see Commentary."
  :type 'number
  :group 'quick-jump)

(defvar qj-current-marker nil)
(defvar qj-marker-ring (make-ring 5))
(defvar qj-previous-action-flag nil)

;;; util func
(defun qj-is-marker-available(marker)
  "return nil if marker is nil or  in dead buffer ,
   return marker if it is live"
  (if (and marker
           (markerp marker)
           (marker-buffer marker))
      marker))

(defmacro qj-if+ (test-form then-form &rest else-forms)
  "Anaphoric if. Temporary variable `it'
  is the result of test-form.(borrowed from anything.el)"
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))
;;; funcs
(defun qj-init()
  "remove #<marker in no buffer> from `qj-marker-ring'."
      (let ((tmp-marker-ring))
        (while (not (ring-empty-p qj-marker-ring))
          (qj-if+ (qj-is-marker-available (ring-remove qj-marker-ring 0))
              (setq tmp-marker-ring (append tmp-marker-ring (list it)));;new item first
              (while (not (ring-empty-p qj-marker-ring));;remove all old marker
              (ring-remove qj-marker-ring))))
        ;;reinsert all available marker to `qj-marker-ring'
        (mapc (lambda(marker) (ring-insert-at-beginning qj-marker-ring marker)) tmp-marker-ring))
      ;;add (point-marker) to marker-ring, when ...
        (when (and (not (ring-empty-p qj-marker-ring))
                   (not (ring-member qj-marker-ring (point-marker)))
                    (or (not (equal (marker-buffer qj-current-marker) (current-buffer)))
                       (>  (count-lines  (point) (marker-position qj-current-marker)) qj-line-count)))
          (ring-insert qj-marker-ring (point-marker)))

        (when (ring-empty-p qj-marker-ring)
          (message "please push marker before jumping. using `quick-jump-push-marker'")))



(defun qj-action-go(marker)
  "Go to location."
  (let ((buf (marker-buffer marker))
        (pos (marker-position marker)))
    (when buf
      (switch-to-buffer buf)
      (set-buffer buf)
      (goto-char pos)))
  (setq qj-current-marker marker))

(defun quick-jump-push-marker()
  "push current marker in ring. you can jump back
by `quick-jump-go-back'"
  (interactive)
  (when (not (ring-member qj-marker-ring (point-marker)))
    (ring-insert qj-marker-ring (point-marker)))
  (setq qj-current-marker (point-marker))
  (message "a marker is pushed."))

(defun quick-jump-go-back()
  "Go back in `qj-marker-ring'."
  (interactive)
  (qj-init)
  (when (and
         (qj-is-marker-available qj-current-marker)
         (ring-member qj-marker-ring qj-current-marker))
    (when (and (not (equal qj-previous-action-flag "back"))
               (equal (current-buffer) (marker-buffer qj-current-marker))
               (<  (count-lines  (point) (marker-position qj-current-marker)) qj-line-count))
      (setq qj-current-marker (ring-next qj-marker-ring qj-current-marker)))

    (message (concat  "jump to " (prin1-to-string qj-current-marker)))
    (qj-action-go qj-current-marker)

    (setq qj-current-marker (ring-next qj-marker-ring qj-current-marker))
    (setq qj-previous-action-flag "back")))

(defun quick-jump-go-forward()
  "Go forward in `qj-marker-ring'."
  (interactive)
  (qj-init)
  (when (and
         (qj-is-marker-available qj-current-marker)
         (ring-member qj-marker-ring qj-current-marker))
    (when (and (not (equal qj-previous-action-flag "forward"))
               (equal (current-buffer) (marker-buffer qj-current-marker))
               (<  (count-lines  (point) (marker-position qj-current-marker)) qj-line-count))
      (setq qj-current-marker (ring-previous qj-marker-ring qj-current-marker)))

    (message (concat  "jump to " (prin1-to-string qj-current-marker)))
    (qj-action-go qj-current-marker)

    (setq qj-current-marker (ring-previous qj-marker-ring qj-current-marker))
    (setq qj-previous-action-flag "forward")))


(defun quick-jump-clear-all-marker()
  "clear all marker in `qj-marker-ring'."
  (interactive)
  (message "clear all marker for joseph-quick-jump.")
  (setq qj-previous-action-flag nil)
  (setq qj-current-marker nil)
  (while (not (ring-empty-p qj-marker-ring))
    (ring-remove qj-marker-ring)))


(defun quick-jump-default-keybinding()
  (global-set-key (kbd "C-,") 'quick-jump-go-back)
  (global-set-key (kbd "C-.") 'quick-jump-push-marker)
  (global-set-key (kbd "C-<") 'quick-jump-go-forward)
  (global-set-key (kbd "C->") 'quick-jump-clear-all-marker))

(provide 'quick-jump)

;;;quick-jump.el ends here.
