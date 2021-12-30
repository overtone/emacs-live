;;; wc-mode.el --- Running word count with goals (minor mode) -*- lexical-binding: t -*-

;; Copyright: 2010-2017  Benjamin Beckwith
;;            2019  Nicholas D Steeves (willing to assign to FSF for GNU Emacs)

;; Author: Benjamin Beckwith
;; Created: 2010-6-19
;; Version: 1.4
;; Last-Updated: 2019-06-28
;; URL: https://github.com/bnbeckwith/wc-mode
;; Package-Requires: ((emacs "24.1"))
;; Keywords:
;; Compatibility:
;; License: GPL-3.0-or-later
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Read the following for how to use the 'how-many' function
;; http://www.neverfriday.com/sweetfriday/2008/06/emacs-tip-word-counting-with-a.html
;; The following site had a good idea on how to produce number of chars
;; http://xahlee.org/emacs/elisp_count-region.html
;; Inspired by http://750words.com
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 1.4 Switch to lexical binding.
;;     Add configurable timer, which, if set to a positive value
;;     defers CPU intensive stat counting to idle periods.
;;     Wc-mode now requires Emacs >= 24.1
;; 1.3 Goal functions now perform reset by default
;; 1.2 Reset functions added
;; 1.1 Counting functions tied to buffer-local variables
;;     This allows customization of the counting methods
;; 1.0 Keystrokes for all goals added.
;;     Hooks variable added.
;;     In-code documentation updated.
;; 0.9 Added keymap for basic mode features/functions
;; 0.8 Added modeline format customization
;;     Added other customizations
;; 0.7 Added stats for lines and characters
;; 0.6 Mode line goal code added
;; 0.5 Initial version focused on word-count
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defgroup wc nil
  "Customization group for `wc-mode'."
  :group 'wp)

(defcustom wc-modeline-format "WC[%W%w/%tw]"
  "The format string for the modeline.
The detailed information for this minor mode can be shown in many
ways in the modeline. The formatting strings recognized in this
format are as follows.

  %W  Original word count (before changes)
  %L  Original line count
  %C  Original character count
  %w  Change in words
  %l  Change in lines
  %c  Change in characters
  %gc Character change goal
  %gl Line change goal
  %gw Word change goal
  %tw Total words in buffer
  %tl Total lines in buffer
  %tc Total characters in buffer

The default modeline, WC[%W%w/%tw], will display the original number
of words followed by the change in words (delta), followed by the total
number of words in the buffer.
It will looks something like WC[742+360/1100] in the modeline.
"
  :group 'wc)

(defcustom wc-mode-hook nil
  "Hook to run when entering wc-mode."
  :type 'hook
  :group 'wc)

(defcustom wc-idle-wait 0
  "This variable configures how many idle seconds wc-mode will
wait before initiating wc-count.  0 and negative numbers provide
a continuously updating word-count in the modeline.  Set this to
a positive integer or float to defend against the lag and
potential distraction of a continuously updating modeline.  A
high value may enhance battery life, because large buffers will
not be processed until one takes a break from work.

Defaults to 0 for backwards compatibility."
  :type 'number
  :group 'wc)

(defface wc-goal-face
  '((t (:inherit highlight)))
  "Face for modeline when goal is reached"
  :group 'wc)

(defvar wc-mode-map
  (let ((map (make-sparse-keymap "Wordcount")))
    (define-key map (kbd "C-c C-w w") 'wc-set-word-goal)
    (define-key map (kbd "C-c C-w l") 'wc-set-line-goal)
    (define-key map (kbd "C-c C-w a") 'wc-set-char-goal)
    (define-key map (kbd "C-c C-w c") 'wc-count)
    map)
  "Keymap for wc-mode")

(defvar wc-orig-words nil "Original count of words in the buffer")
(defvar wc-orig-lines nil "Original count of words in the buffer")
(defvar wc-orig-chars nil "Original count of words in the buffer")
(make-variable-buffer-local 'wc-orig-words)
(make-variable-buffer-local 'wc-orig-lines)
(make-variable-buffer-local 'wc-orig-chars)

(defvar wc-words-delta 0 "Change in word count")
(defvar wc-lines-delta 0 "Change in line count")
(defvar wc-chars-delta 0 "Change in char count")
(make-variable-buffer-local 'wc-words-delta)
(make-variable-buffer-local 'wc-lines-delta)
(make-variable-buffer-local 'wc-chars-delta)

(defvar wc-word-goal nil "Goal for number of words added")
(defvar wc-line-goal nil "Goal for number of lines added")
(defvar wc-char-goal nil "Goal for number of chars added")
(make-variable-buffer-local 'wc-word-goal)
(make-variable-buffer-local 'wc-line-goal)
(make-variable-buffer-local 'wc-char-goal)

(defvar wc-count-chars-function
  (function (lambda (rstart rend)
    "Count the characters specified by the region bounded by
RSTART and REND."
    (- rend rstart))))

(defvar wc-count-words-function
  (function (lambda (rstart rend)
    "Count the words specified by the region bounded by
RSTART and REND."
    (if (boundp 'count-words)
        (count-words rstart rend)
      (how-many "\\w+" rstart rend)))))

(defvar wc-count-lines-function
  (function (lambda (rstart rend)
    "Count the lines specified by the region bounded by
RSTART and REND."
    (how-many "\n" rstart rend))))

(defvar wc-modeline-format-alist
  '(("%W" . (number-to-string wc-orig-words))
    ("%L" . (number-to-string wc-orig-lines))
    ("%C" . (number-to-string wc-orig-chars))
    ("%w" . (wc-prepend-sign wc-words-delta))
    ("%l" . (wc-prepend-sign wc-lines-delta))
    ("%c" . (wc-prepend-sign wc-chars-delta))
    ("%gc" . (wc-prepend-sign wc-char-goal))
    ("%gl" . (wc-prepend-sign wc-line-goal))
    ("%gw" . (wc-prepend-sign wc-word-goal))
    ("%tc" . (number-to-string (+ wc-orig-chars wc-chars-delta)))
    ("%tl" . (number-to-string (+ wc-orig-lines wc-lines-delta)))
    ("%tw" . (number-to-string (+ wc-orig-words wc-words-delta))))
  "Format and value pair
Format will be evaluated in `wc-generate-modeline'")

(defvar wc-mode-hooks nil "Hooks to run upon entry to wc-mode")

(defvar wc-timer-tracker nil
  "Global timer for wc-count.

Ensure functions on this timer are not run when wc-mode is false.")

(defvar-local wc-buffer-stats nil
  "This variable holds the per-buffer word-count statistics used to
update the modeline.")

(defun wc-format-modeline-string (fmt)
  "Format the modeline string according to specification and return result"
  (let ((case-fold-search nil))
    (dolist (pair wc-modeline-format-alist fmt)
      (when (string-match (car pair) fmt)
	(setq fmt (replace-match (eval (cdr pair)) t t fmt))))))

(defun wc-prepend-sign (val)
  "Add a sign to the beginning of a value.
Also cheat here a bit and add nil-value processing."
  (if val
      (format "%s%d"
	      (if (< val 0)
		  "-" "+")
	      (abs val))
    "none"))

(defun wc-reset ()
  "Reset the original word, line, and char count to their current
value."
  (interactive)
  (setq wc-orig-words nil)
  (setq wc-orig-lines nil)
  (setq wc-orig-chars nil)
  (wc-mode-update))

(defun wc-set-word-goal (goal)
  "Set a goal for adding or removing words in the buffer"
  (interactive "nHow many words: ")
  (setq wc-word-goal goal)
  (wc-reset)
  (message "Goal set at %d words" goal))

(defun wc-set-line-goal (goal)
  "Set a goal for adding or removing lines in the buffer"
  (interactive "nHow many lines: ")
  (setq wc-line-goal goal)
  (wc-reset)
  (message "Goal set at %d lines" goal))

(defun wc-set-char-goal (goal)
  "Set a goal for adding or removing chars in the buffer"
  (interactive "nHow many characters: ")
  (setq wc-char-goal goal)
  (wc-reset)
  (message "Goal set at %d characters" goal))

(defun wc-goal-reached ()
  "Returns t when the goal change is reached."
  (or
   (if wc-line-goal
       (if (< wc-line-goal 0)
	   (<= wc-lines-delta wc-line-goal)
	 (>= wc-lines-delta wc-line-goal)))
   (if wc-word-goal
       (if (< wc-word-goal 0)
	   (<= wc-words-delta wc-word-goal)
	 (>= wc-words-delta wc-word-goal)))
   (if wc-char-goal
       (if (< wc-char-goal 0)
	   (<= wc-chars-delta wc-char-goal)
	 (>= wc-chars-delta wc-char-goal)))))


(defun wc-count (&optional rstart rend field)
  "Count the words, lines and characters present in the region
following point. This function follows most of the rules present
in the `how-many' function. If INTERACTIVE is omitted or nil,
just return the word count, do not print it. Otherwise, if
INTERACTIVE is t, the function behaves according to interactive
behavior.

START and END specify the region to operate on.

When called interactively, this function first checks to see if
it is in Transient Mark mode.  If that is the case, then the
function operates over the marked region.  Otherwise, it will
operate over the entire buffer.
"
  (interactive)
  (if rstart
      (setq rend (max rstart rend))
    (if (and (interactive-p) transient-mark-mode mark-active)
	(setq rstart (region-beginning)
	      rend (region-end))
      (setq rstart (point-min)
	    rend (point-max))))
  (let ((wcount (funcall wc-count-words-function rstart rend))
	(lcount (funcall wc-count-lines-function rstart rend))
	(ccount (funcall wc-count-chars-function rstart rend)))
    (when (interactive-p) (message "%d line%s, %d word%s, %d char%s"
				   lcount
				   (if (= lcount 1) "" "s")
				   wcount
				   (if (= wcount 1) "" "s")
				   ccount
				   (if (= ccount 1) "" "s")
				   ))
    (if field
	(nth field (list lcount wcount ccount))
      (list lcount wcount ccount))))

(defalias 'wc 'wc-count
  "Alias function `wc-count' to the more legible `wc'.")

(defun wc-generate-modeline ()
  (let ((modeline (wc-format-modeline-string wc-modeline-format)))
    (when (wc-goal-reached)
      (put-text-property 0 (length modeline) 'face 'wc-goal-face modeline))
    (list " " modeline)))

(defun wc-mode-update ()
  "Return a string to update the modeline appropriately"
  (let* ((stats (wc-count (point-min) (point-max))))
    (unless wc-orig-lines (setq wc-orig-lines (nth 0 stats)))
    (unless wc-orig-words (setq wc-orig-words (nth 1 stats)))
    (unless wc-orig-chars (setq wc-orig-chars (nth 2 stats)))
    (setq wc-lines-delta (- (nth 0 stats) wc-orig-lines))
    (setq wc-words-delta (- (nth 1 stats) wc-orig-words))
    (setq wc-chars-delta (- (nth 2 stats) wc-orig-chars))
    (wc-generate-modeline)))

(setq wc-timer-tracker
      (run-with-idle-timer
       wc-idle-wait t
       '(lambda ()
          (when wc-mode
            (setq wc-buffer-stats (wc-mode-update))))))

;;;###autoload
(define-minor-mode wc-mode
  "Toggle wc mode With no argument, this command toggles the
mode.  Non-null prefix argument turns on the mode.  Null prefix
argument turns off the mode.

When Wc mode is enabled on a buffer, it counts the current words
in the buffer and keeps track of a differential of added or
subtracted words.

A goal of number of words added/subtracted can be set while using
this mode. Upon completion of the goal, the modeline text will
highlight indicating that the goal has been reached.

Commands:
\\{wc-mode-map}

Entry to this mode calls the value of `wc-mode-hook' if that
value is non-nil."
  ;; initial value (off)
  :init-value nil
  ;; The indicator for the mode line
  :lighter (:eval wc-buffer-stats)
  ;; The customization group
  :group 'wc
  ;; The local keymap to use
  :keymap wc-mode-map
  ;; The mode body code
  (if wc-mode
	(run-mode-hooks 'wc-mode-hooks)))

(provide 'wc-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; wc-mode.el ends here
