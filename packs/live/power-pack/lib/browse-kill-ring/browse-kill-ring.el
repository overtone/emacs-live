;;; browse-kill-ring.el --- interactively insert items from kill-ring -*- coding: utf-8 -*-

;; Copyright (C) 2001, 2002 Colin Walters <walters@verbum.org>

;; Author: Colin Walters <walters@verbum.org>
;; Maintainer: browse-kill-ring <browse-kill-ring@tonotdo.com>
;; Created: 7 Apr 2001
;; Version: 1.4
;; URL: https://github.com/browse-kill-ring/browse-kill-ring
;; Keywords: convenience

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Ever feel that 'C-y M-y M-y M-y ...' is not a great way of trying
;; to find that piece of text you know you killed a while back?  Then
;; browse-kill-ring.el is for you.

;; This package is simple to install; add (require 'browse-kill-ring)
;; to your ~/.emacs file, after placing this file somewhere in your
;; `load-path'.  If you want to use 'M-y' to invoke
;; `browse-kill-ring', also add (browse-kill-ring-default-keybindings)
;; to your ~/.emacs file.  Alternatively, you can bind it to another
;; key such as "C-c k", with:
;; (global-set-key (kbd "C-c k") 'browse-kill-ring)

;; Note that the command keeps track of the last window displayed to
;; handle insertion of chosen text; this might have unexpected
;; consequences if you do 'M-x browse-kill-ring', then switch your
;; window configuration, and try to use the same *Kill Ring* buffer
;; again.

;;; Change Log:

;; Changes from 1.3c to 1.4:

;; * 16-Aug-2012: Toon Claes
;;   No actual changes to the code, but released again by the
;;   browse-kill-ring user at Github.com.

;; * 28-Feb-2011: Andrew Burgess <aburgess@broadcom.com>
;;   Fix a bug where having other overlays active in the kill ring buffer,
;;   for example with show-paren-mode would block insertion.

;; Changes from 1.3a to 1.3b:

;; * 24-Feb-2011: Andrew Burgess <aburgess@broadcom.com>
;;   Correctly handle inserting when multiple windows exist for the
;;   same buffer.

;; Changes from 1.3 to 1.3a:

;; * Sneak update by Benjamin Andresen <bandresen@gmail.com>
;; * Added the read-only bugfix (http://bugs.debian.org/225082) from
;;   the emacs-goodies-el package

;; Changes from 1.2 to 1.3:

;; * New maintainer, Nick Hurley <hurley@cis.ohio-state.edu>
;; * New functions `browse-kill-ring-prepend-insert', and
;;   `browse-kill-ring-append-insert', bound to 'b' and 'a' by
;;   default. There are also the unbound functions
;;   `browse-kill-ring-prepend-insert-and-quit',
;;   `browse-kill-ring-prepend-insert-and-move',
;;   `browse-kill-ring-prepend-insert-move-and-quit',
;;   `browse-kill-ring-append-insert-and-quit',
;;   `browse-kill-ring-append-insert-and-move',
;;   `browse-kill-ring-append-insert-move-and-quit'.

;; Changes from 1.1 to 1.2:

;; * New variable `browse-kill-ring-resize-window', which controls
;;   whether or not the browse-kill-ring window will try to resize
;;   itself to fit the buffer.  Implementation from Juanma Barranquero
;;   <lektu@terra.es>.
;; * New variable `browse-kill-ring-highlight-inserted-item'.
;;   Implementation from Yasutaka SHINDOH <ring-pub@fan.gr.jp>.
;; * `browse-kill-ring-mouse-insert' (normally bound to mouse-2) now
;;   calls `browse-kill-ring-quit'.
;; * Some non-user-visible code cleanup.
;; * New variable `browse-kill-ring-recenter', implementation from
;;   René Kyllingstad <kyllingstad@users.sourceforge.net>.
;; * Patch from Michal Maršuka <mmc@maruska.dyndns.org> which handles
;;   read-only text better.
;; * New ability to move unkilled entries back to the beginning of the
;;   ring; patch from Yasutaka SHINDOH <ring-pub@fan.gr.jp>.
;; * Do nothing if the user invokes `browse-kill-ring' when we're
;;   already in a *Kill Ring* buffer (initial patch from Juanma
;;   Barranquero <lektu@terra.es>).

;; Changes from 1.0 to 1.1:

;; * Important keybinding change!  The default bindings of RET and 'i'
;;   have switched; this means typing RET now by default inserts the
;;   text and calls `browse-kill-ring-quit'; 'i' just inserts.
;; * The variable `browse-kill-ring-use-fontification' is gone;
;;   browse-kill-ring.el has been rewritten to use font-lock.  XEmacs
;;   users who want fontification will have to do:
;;   (add-hook 'browse-kill-ring-hook 'font-lock-mode)
;; * Integrated code from Michael Slass <mikesl@wrq.com> into
;;   `browse-kill-ring-default-keybindings'.
;; * New Japanese homepage for browse-kill-ring.el, thanks to
;;   Yasutaka SHINDOH <ring-pub@fan.gr.jp>.
;; * Correctly restore window configuration after editing an entry.
;; * New command `browse-kill-ring-insert-and-delete'.
;; * Bug reports and patches from Michael Slass <mikesl@wrq.com> and
;;   Juanma Barranquero <lektu@terra.es>.

;; Changes from 0.9b to 1.0:

;; * Add autoload cookie to `browse-kill-ring'; suggestion from
;;   D. Goel <deego@glue.umd.edu> and Dave Pearson <davep@davep.org>.
;; * Add keybinding tip from Michael Slass <mikesl@wrq.com>.

;; Changes from 0.9a to 0.9b:

;; * Remove extra parenthesis.  Duh.

;; Changes from 0.9 to 0.9a:

;; * Fix bug making `browse-kill-ring-quit-action' uncustomizable.
;;   Patch from Henrik Enberg <henrik@enberg.org>.
;; * Add `url-link' and `group' attributes to main Customization
;;   group.

;; Changes from 0.8 to 0.9:

;; * Add new function `browse-kill-ring-insert-and-quit', bound to 'i'
;;   by default (idea from Yasutaka Shindoh).
;; * Make default `browse-kill-ring-quit-action' be
;;   `bury-and-delete-window', which handles the case of a single window
;;   more nicely.
;; * Note change of home page and author address.

;; Changes from 0.7 to 0.8:

;; * Fix silly bug in `browse-kill-ring-edit' which made it impossible
;;   to edit entries.
;; * New variable `browse-kill-ring-quit-action'.
;; * `browse-kill-ring-restore' renamed to `browse-kill-ring-quit'.
;; * Describe the keymaps in mode documentation.  Patch from
;;   Marko Slyz <mslyz@eecs.umich.edu>.
;; * Fix advice documentation for `browse-kill-ring-no-duplicates'.

;; Changes from 0.6 to 0.7:

;; * New functions `browse-kill-ring-search-forward' and
;;   `browse-kill-ring-search-backward', bound to "s" and "r" by
;;   default, respectively.
;; * New function `browse-kill-ring-edit' bound to "e" by default, and
;;   a associated new major mode.
;; * New function `browse-kill-ring-occur', bound to "l" by default.

;; Changes from 0.5 to 0.6:

;; * Fix bug in `browse-kill-ring-forward' which sometimes would cause
;;   a message "Wrong type argument: overlayp, nil" to appear.
;; * New function `browse-kill-ring-update'.
;; * New variable `browse-kill-ring-highlight-current-entry'.
;; * New variable `browse-kill-ring-display-duplicates'.
;; * New optional advice `browse-kill-ring-no-kill-new-duplicates',
;;   and associated variable `browse-kill-ring-no-duplicates'.  Code
;;   from Klaus Berndl <Klaus.Berndl@sdm.de>.
;; * Bind "?" to `describe-mode'.  Patch from Dave Pearson
;;   <dave@davep.org>.
;; * Fix typo in `browse-kill-ring-display-style' defcustom form.
;;   Thanks "Kahlil (Kal) HODGSON" <kahlil@discus.anu.edu.au>.

;; Changes from 0.4 to 0.5:

;; * New function `browse-kill-ring-delete', bound to "d" by default.
;; * New function `browse-kill-ring-undo', bound to "U" by default.
;; * New variable `browse-kill-ring-maximum-display-length'.
;; * New variable `browse-kill-ring-use-fontification'.
;; * New variable `browse-kill-ring-hook', called after the
;;   "*Kill Ring*" buffer is created.

;; Changes from 0.3 to 0.4:

;; * New functions `browse-kill-ring-forward' and
;;   `browse-kill-ring-previous', bound to "n" and "p" by default,
;;   respectively.
;; * Change the default `browse-kill-ring-display-style' to
;;   `separated'.
;; * Removed `browse-kill-ring-original-window-config'; Now
;;   `browse-kill-ring-restore' just buries the "*Kill Ring*" buffer
;;   and deletes its window, which is simpler and more intuitive.
;; * New variable `browse-kill-ring-separator-face'.

;;; Bugs:

;; * Sometimes, in Emacs 21, the cursor will jump to the end of an
;;   entry when moving backwards using `browse-kill-ring-previous'.
;;   This doesn't seem to occur in Emacs 20 or XEmacs.

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'derived))

(when (featurep 'xemacs)
  (require 'overlay))

(defun browse-kill-ring-depropertize-string (str)
  "Return a copy of STR with text properties removed."
  (let ((str (copy-sequence str)))
    (set-text-properties 0 (length str) nil str)
    str))

(cond ((fboundp 'propertize)
       (defalias 'browse-kill-ring-propertize 'propertize))
      ;; Maybe save some memory :)
      ((fboundp 'ibuffer-propertize)
       (defalias 'browse-kill-ring-propertize 'ibuffer-propertize))
      (t
       (defun browse-kill-ring-propertize (string &rest properties)
         "Return a copy of STRING with text properties added.

 [Note: this docstring has been copied from the Emacs 21 version]

First argument is the string to copy.
Remaining arguments form a sequence of PROPERTY VALUE pairs for text
properties to add to the result."
         (let ((str (copy-sequence string)))
           (add-text-properties 0 (length str)
                                properties
                                str)
           str))))

(defgroup browse-kill-ring nil
  "A package for browsing and inserting the items in `kill-ring'."
  :link '(url-link "http://freedom.cis.ohio-state.edu/~hurley/")
  :group 'convenience)

(defvar browse-kill-ring-display-styles
  '((separated . browse-kill-ring-insert-as-separated)
    (one-line . browse-kill-ring-insert-as-one-line)))

(defcustom browse-kill-ring-display-style 'separated
  "How to display the kill ring items.

If `one-line', then replace newlines with \"\\n\" for display.

If `separated', then display `browse-kill-ring-separator' between
entries."
  :type '(choice (const :tag "One line" one-line)
                 (const :tag "Separated" separated))
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-quit-action 'bury-and-delete-window
  "What action to take when `browse-kill-ring-quit' is called.

If `bury-buffer', then simply bury the *Kill Ring* buffer, but keep
the window.

If `bury-and-delete-window', then bury the buffer, and (if there is
more than one window) delete the window.  This is the default.

If `save-and-restore', then save the window configuration when
`browse-kill-ring' is called, and restore it at quit.

If `kill-and-delete-window', then kill the *Kill Ring* buffer, and
delete the window on close.

Otherwise, it should be a function to call."
  :type '(choice (const :tag "Bury buffer" :value bury-buffer)
                 (const :tag "Delete window" :value delete-window)
                 (const :tag "Save and restore" :value save-and-restore)
                 (const :tag "Bury buffer and delete window" :value bury-and-delete-window)
                 (const :tag "Kill buffer and delete window" :value kill-and-delete-window)
                 function)
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-resize-window nil
  "Whether to resize the `browse-kill-ring' window to fit its contents.
Value is either t, meaning yes, or a cons pair of integers,
 (MAXIMUM . MINIMUM) for the size of the window.  MAXIMUM defaults to
the window size chosen by `pop-to-buffer'; MINIMUM defaults to
`window-min-height'."
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t)
                 (cons (integer :tag "Maximum") (integer :tag "Minimum")))
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-separator "-------"
  "The string separating entries in the `separated' style.
See `browse-kill-ring-display-style'."
  :type 'string
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-recenter nil
  "If non-nil, then always keep the current entry at the top of the window."
  :type 'boolean
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-highlight-current-entry nil
  "If non-nil, highlight the currently selected `kill-ring' entry."
  :type 'boolean
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-highlight-inserted-item browse-kill-ring-highlight-current-entry
  "If non-nil, temporarily highlight the inserted `kill-ring' entry."
  :type 'boolean
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-separator-face 'bold
  "The face in which to highlight the `browse-kill-ring-separator'."
  :type 'face
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-current-entry-face 'highlight
  "The face in which to highlight the browse kill current entry."
  :type 'face
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-inserted-item-face 'highlight
  "The face in which to highlight the inserted item."
  :type 'face
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-maximum-display-length nil
  "Whether or not to limit the length of displayed items.

If this variable is an integer, the display of `kill-ring' will be
limited to that many characters.
Setting this variable to nil means no limit."
  :type '(choice (const :tag "None" nil)
                 integer)
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-display-duplicates t
  "If non-nil, then display duplicate items in `kill-ring'."
  :type 'boolean
  :group 'browse-kill-ring)

(defadvice kill-new (around browse-kill-ring-no-kill-new-duplicates)
  "An advice for not adding duplicate elements to `kill-ring'.
Even after being \"activated\", this advice will only modify the
behavior of `kill-new' when `browse-kill-ring-no-duplicates'
is non-nil."
  (if browse-kill-ring-no-duplicates
      (setq kill-ring (delete (ad-get-arg 0) kill-ring)))
  ad-do-it)

(defcustom browse-kill-ring-no-duplicates nil
  "If non-nil, then the `b-k-r-no-kill-new-duplicates' advice will operate.
This means that duplicate entries won't be added to the `kill-ring'
when you call `kill-new'.

If you set this variable via customize, the advice will be activated
or deactivated automatically.  Otherwise, to enable the advice, add

B (ad-enable-advice 'kill-new 'around 'browse-kill-ring-no-kill-new-duplicates)
 (ad-activate 'kill-new)

to your init file."
  :type 'boolean
  :set (lambda (symbol value)
         (set symbol value)
         (if value
             (ad-enable-advice 'kill-new 'around
                               'browse-kill-ring-no-kill-new-duplicates)
           (ad-disable-advice 'kill-new 'around
                              'browse-kill-ring-no-kill-new-duplicates))
         (ad-activate 'kill-new))
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-depropertize nil
  "If non-nil, remove text properties from `kill-ring' items.
This only changes the items for display and insertion from
`browse-kill-ring'; if you call `yank' directly, the items will be
inserted with properties."
  :type 'boolean
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-hook nil
  "A list of functions to call after `browse-kill-ring'."
  :type 'hook
  :group 'browse-kill-ring)

(defvar browse-kill-ring-original-window-config nil
  "The window configuration to restore for `browse-kill-ring-quit'.")
(make-variable-buffer-local 'browse-kill-ring-original-window-config)

(defvar browse-kill-ring-original-window nil
  "The window in which chosen kill ring data will be inserted.
It is probably not a good idea to set this variable directly; simply
call `browse-kill-ring' again.")

(defvar browse-kill-ring-original-buffer nil
  "The buffer in which chosen kill ring data will be inserted.
It is probably not a good idea to set this variable directly; simply
call `browse-kill-ring' again.")

(defun browse-kill-ring-mouse-insert (e)
  "Insert the chosen text, and close the *Kill Ring* buffer afterwards."
  (interactive "e")
  (let* ((data (save-excursion
                 (mouse-set-point e)
                 (cons (current-buffer) (point))))
         (buf (car data))
         (pt (cdr data)))
    (browse-kill-ring-do-insert buf pt))
  (browse-kill-ring-quit))

(if (fboundp 'fit-window-to-buffer)
    (defalias 'browse-kill-ring-fit-window 'fit-window-to-buffer)
  (defun browse-kill-ring-fit-window (window max-height min-height)
    (setq min-height (or min-height window-min-height))
    (setq max-height (or max-height (- (frame-height) (window-height) 1)))
    (let* ((window-min-height min-height)
           (windows (count-windows))
           (config (current-window-configuration)))
      (enlarge-window (- max-height (window-height)))
      (when (> windows (count-windows))
        (set-window-configuration config))
      (if (/= (point-min) (point-max))
          (shrink-window-if-larger-than-buffer window)
        (shrink-window (- (window-height) window-min-height))))))

(defun browse-kill-ring-resize-window ()
  (when browse-kill-ring-resize-window
    (apply #'browse-kill-ring-fit-window (selected-window)
           (if (consp browse-kill-ring-resize-window)
               (list (car browse-kill-ring-resize-window)
                     (or (cdr browse-kill-ring-resize-window)
                         window-min-height))
             (list nil window-min-height)))))

(defun browse-kill-ring-undo-other-window ()
  "Undo the most recent change in the other window's buffer.
You most likely want to use this command for undoing an insertion of
yanked text from the *Kill Ring* buffer."
  (interactive)
  (with-current-buffer (window-buffer browse-kill-ring-original-window)
    (undo)))

(defun browse-kill-ring-insert (&optional quit)
  "Insert the kill ring item at point into the last selected buffer.
If optional argument QUIT is non-nil, close the *Kill Ring* buffer as
well."
  (interactive "P")
  (browse-kill-ring-do-insert (current-buffer)
                              (point))
  (when quit
    (browse-kill-ring-quit)))

(defun browse-kill-ring-insert-and-delete (&optional quit)
  "Insert the kill ring item at point, and remove it from the kill ring.
If optional argument QUIT is non-nil, close the *Kill Ring* buffer as
well."
  (interactive "P")
  (browse-kill-ring-do-insert (current-buffer)
                              (point))
  (browse-kill-ring-delete)
  (when quit
    (browse-kill-ring-quit)))

(defun browse-kill-ring-insert-and-quit ()
  "Like `browse-kill-ring-insert', but close the *Kill Ring* buffer afterwards."
  (interactive)
  (browse-kill-ring-insert t))

(defun browse-kill-ring-insert-and-move (&optional quit)
  "Like `browse-kill-ring-insert', but move the entry to the front."
  (interactive "P")
  (let ((buf (current-buffer))
        (pt (point)))
    (browse-kill-ring-do-insert buf pt)
    (let ((str (browse-kill-ring-current-string buf pt)))
      (browse-kill-ring-delete)
      (kill-new str)))
  (if quit
      (browse-kill-ring-quit)
    (browse-kill-ring-update)))

(defun browse-kill-ring-insert-move-and-quit ()
  "Like `browse-kill-ring-insert-and-move', but close the *Kill Ring* buffer."
  (interactive)
  (browse-kill-ring-insert-and-move t))

(defun browse-kill-ring-prepend-insert (&optional quit)
  "Like `browse-kill-ring-insert', but it places the entry at the beginning
of the buffer as opposed to point."
  (interactive "P")
  (browse-kill-ring-do-prepend-insert (current-buffer)
                                      (point))
  (when quit
    (browse-kill-ring-quit)))

(defun browse-kill-ring-prepend-insert-and-quit ()
  "Like `browse-kill-ring-prepend-insert', but close the *Kill Ring* buffer."
  (interactive)
  (browse-kill-ring-prepend-insert t))

(defun browse-kill-ring-prepend-insert-and-move (&optional quit)
  "Like `browse-kill-ring-prepend-insert', but move the entry to the front
of the *Kill Ring*."
  (interactive "P")
  (let ((buf (current-buffer))
        (pt (point)))
    (browse-kill-ring-do-prepend-insert buf pt)
    (let ((str (browse-kill-ring-current-string buf pt)))
      (browse-kill-ring-delete)
      (kill-new str)))
  (if quit
      (browse-kill-ring-quit)
    (browse-kill-ring-update)))

(defun browse-kill-ring-prepend-insert-move-and-quit ()
  "Like `browse-kill-ring-prepend-insert-and-move', but close the
*Kill Ring* buffer."
  (interactive)
  (browse-kill-ring-prepend-insert-and-move t))

(defun browse-kill-ring-do-prepend-insert (buf pt)
  (let ((str (browse-kill-ring-current-string buf pt)))
    (let ((orig (current-buffer)))
      (unwind-protect
          (progn
            (unless (window-live-p browse-kill-ring-original-window)
              (error "Window %s has been deleted; Try calling `browse-kill-ring' again"
                     browse-kill-ring-original-window))
            (set-buffer (window-buffer browse-kill-ring-original-window))
            (save-excursion
              (let ((pt (point)))
                (goto-char (point-min))
                (insert (if browse-kill-ring-depropertize
                            (browse-kill-ring-depropertize-string str)
                          str))
                (when browse-kill-ring-highlight-inserted-item
                  (let ((o (make-overlay (point-min) (point))))
                    (overlay-put o 'face 'browse-kill-ring-inserted-item-face)
                    (sit-for 0.5)
                    (delete-overlay o)))
                (goto-char pt))))
        (set-buffer orig)))))

(defun browse-kill-ring-append-insert (&optional quit)
  "Like `browse-kill-ring-insert', but places the entry at the end of the
buffer as opposed to point."
  (interactive "P")
  (browse-kill-ring-do-append-insert (current-buffer)
                                     (point))
  (when quit
    (browse-kill-ring-quit)))

(defun browse-kill-ring-append-insert-and-quit ()
  "Like `browse-kill-ring-append-insert', but close the *Kill Ring* buffer."
  (interactive)
  (browse-kill-ring-append-insert t))

(defun browse-kill-ring-append-insert-and-move (&optional quit)
  "Like `browse-kill-ring-append-insert', but move the entry to the front
of the *Kill Ring*."
  (interactive "P")
  (let ((buf (current-buffer))
        (pt (point)))
    (browse-kill-ring-do-append-insert buf pt)
    (let ((str (browse-kill-ring-current-string buf pt)))
      (browse-kill-ring-delete)
      (kill-new str)))
  (if quit
      (browse-kill-ring-quit)
    (browse-kill-ring-update)))

(defun browse-kill-ring-append-insert-move-and-quit ()
  "Like `browse-kill-ring-append-insert-and-move', but close the
*Kill Ring* buffer."
  (interactive)
  (browse-kill-ring-append-insert-and-move t))

(defun browse-kill-ring-do-append-insert (buf pt)
  (let ((str (browse-kill-ring-current-string buf pt)))
    (let ((orig (current-buffer)))
      (unwind-protect
          (progn
            (unless (window-live-p browse-kill-ring-original-window)
              (error "Window %s has been deleted; Try calling `browse-kill-ring' again"
                     browse-kill-ring-original-window))
            (set-buffer (window-buffer browse-kill-ring-original-window))
            (save-excursion
              (let ((pt (point))
                    (begin-pt (point-max)))
                (goto-char begin-pt)
                (insert (if browse-kill-ring-depropertize
                            (browse-kill-ring-depropertize-string str)
                          str))
                (when browse-kill-ring-highlight-inserted-item
                  (let ((o (make-overlay begin-pt (point-max))))
                    (overlay-put o 'face 'browse-kill-ring-inserted-item-face)
                    (sit-for 0.5)
                    (delete-overlay o)))
                (goto-char pt))))
        (set-buffer orig)))))

(defun browse-kill-ring-delete ()
  "Remove the item at point from the `kill-ring'."
  (interactive)
  (let ((over (car (overlays-at (point)))))
    (unless (overlayp over)
      (error "No kill ring item here"))
    (unwind-protect
        (progn
          (setq buffer-read-only nil)
          (let ((target (overlay-get over 'browse-kill-ring-target)))
            (delete-region (overlay-start over)
                           (1+ (overlay-end over)))
            (setq kill-ring (delete target kill-ring)))
          (when (get-text-property (point) 'browse-kill-ring-extra)
            (let ((prev (previous-single-property-change (point)
                                                         'browse-kill-ring-extra))
                  (next (next-single-property-change (point)
                                                     'browse-kill-ring-extra)))
              ;; This is some voodoo.
              (when prev
                (incf prev))
              (when next
                (incf next))
              (delete-region (or prev (point-min))
                             (or next (point-max))))))
      (setq buffer-read-only t)))
  (browse-kill-ring-resize-window)
  (browse-kill-ring-forward 0))

;; Helper function for browse-kill-ring-current-string, takes a list of
;; overlays and returns the string from the first overlay that has the
;; property. There might be more than just our overlay at this point.
(defun browse-kill-ring-current-string-1 (overs)
  (if overs
      (let ((str (overlay-get (car overs) 'browse-kill-ring-target)))
        (if str str (browse-kill-ring-current-string-1 (cdr overs))))
    nil))

;; Find the string to insert at the point by looking for the overlay.
(defun browse-kill-ring-current-string (buf pt)
  (or (browse-kill-ring-current-string-1 (overlays-at pt))
      (error "No kill ring item here")))

(defun browse-kill-ring-do-insert (buf pt)
  (let ((str (browse-kill-ring-current-string buf pt)))
    (with-current-buffer browse-kill-ring-original-buffer

      (let (deactivate-mark)
        (insert-for-yank str))

      (when browse-kill-ring-highlight-inserted-item
        (let ((o (make-overlay pt (point))))
          (overlay-put o 'face 'browse-kill-ring-inserted-item-face)
          (sit-for 0.5)
          (delete-overlay o))))))

(defun browse-kill-ring-forward (&optional arg)
  "Move forward by ARG `kill-ring' entries."
  (interactive "p")
  (beginning-of-line)
  (while (not (zerop arg))
    (if (< arg 0)
        (progn
          (incf arg)
          (if (overlays-at (point))
              (progn
                (goto-char (overlay-start (car (overlays-at (point)))))
                (goto-char (previous-overlay-change (point)))
                (goto-char (previous-overlay-change (point))))
            (progn
              (goto-char (1- (previous-overlay-change (point))))
              (unless (bobp)
                (goto-char (overlay-start (car (overlays-at (point)))))))))
      (progn
        (decf arg)
        (if (overlays-at (point))
            (progn
              (goto-char (overlay-end (car (overlays-at (point)))))
              (goto-char (next-overlay-change (point))))
          (goto-char (next-overlay-change (point)))
          (unless (eobp)
            (goto-char (overlay-start (car (overlays-at (point))))))))))
  ;; This could probably be implemented in a more intelligent manner.
  ;; Perhaps keep track over the overlay we started from?  That would
  ;; break when the user moved manually, though.
  (when (and browse-kill-ring-highlight-current-entry
             (overlays-at (point)))
    (let ((overs (overlay-lists))
          (current-overlay (car (overlays-at (point)))))
      (mapcar #'(lambda (o)
                  (overlay-put o 'face nil))
              (nconc (car overs) (cdr overs)))
      (overlay-put current-overlay 'face 'browse-kill-ring-current-entry-face)))
  (when browse-kill-ring-recenter
    (recenter 1)))

(defun browse-kill-ring-previous (&optional arg)
  "Move backward by ARG `kill-ring' entries."
  (interactive "p")
  (browse-kill-ring-forward (- arg)))

(defun browse-kill-ring-read-regexp (msg)
  (let* ((default (car regexp-history))
         (input
          (read-from-minibuffer
           (if default
               (format "%s for regexp (default `%s'): "
                       msg
                       default)
             (format "%s (regexp): " msg))
           nil
           nil
           nil
           'regexp-history)))
    (if (equal input "")
        default
      input)))

(defun browse-kill-ring-search-forward (regexp &optional backwards)
  "Move to the next `kill-ring' entry matching REGEXP from point.
If optional arg BACKWARDS is non-nil, move to the previous matching
entry."
  (interactive
   (list (browse-kill-ring-read-regexp "Search forward")
         current-prefix-arg))
  (let ((orig (point)))
    (browse-kill-ring-forward (if backwards -1 1))
    (let ((overs (overlays-at (point))))
      (while (and overs
                  (not (if backwards (bobp) (eobp)))
                  (not (string-match regexp
                                     (overlay-get (car overs)
                                                  'browse-kill-ring-target))))
        (browse-kill-ring-forward (if backwards -1 1))
        (setq overs (overlays-at (point))))
      (unless (and overs
                   (string-match regexp
                                 (overlay-get (car overs)
                                              'browse-kill-ring-target)))
        (progn
          (goto-char orig)
          (message "No more `kill-ring' entries matching %s" regexp))))))

(defun browse-kill-ring-search-backward (regexp)
  "Move to the previous `kill-ring' entry matching REGEXP from point."
  (interactive
   (list (browse-kill-ring-read-regexp "Search backward")))
  (browse-kill-ring-search-forward regexp t))

(defun browse-kill-ring-quit ()
  "Take the action specified by `browse-kill-ring-quit-action'."
  (interactive)
  (case browse-kill-ring-quit-action
    (save-and-restore
     (let (buf (current-buffer))
       (set-window-configuration browse-kill-ring-original-window-config)
       (kill-buffer buf)))
    (kill-and-delete-window
     (kill-buffer (current-buffer))
     (unless (= (count-windows) 1)
       (delete-window)))
    (bury-and-delete-window
     (bury-buffer)
     (unless (= (count-windows) 1)
       (delete-window)))
    (t
     (funcall browse-kill-ring-quit-action))))

(put 'browse-kill-ring-mode 'mode-class 'special)
(define-derived-mode browse-kill-ring-mode fundamental-mode
  "Kill Ring"
  "A major mode for browsing the `kill-ring'.
You most likely do not want to call `browse-kill-ring-mode' directly; use
`browse-kill-ring' instead.

\\{browse-kill-ring-mode-map}"
  (set (make-local-variable 'font-lock-defaults)
       '(nil t nil nil nil
             (font-lock-fontify-region-function . browse-kill-ring-fontify-region)))
  (define-key browse-kill-ring-mode-map (kbd "q") 'browse-kill-ring-quit)
  (define-key browse-kill-ring-mode-map (kbd "U") 'browse-kill-ring-undo-other-window)
  (define-key browse-kill-ring-mode-map (kbd "d") 'browse-kill-ring-delete)
  (define-key browse-kill-ring-mode-map (kbd "s") 'browse-kill-ring-search-forward)
  (define-key browse-kill-ring-mode-map (kbd "r") 'browse-kill-ring-search-backward)
  (define-key browse-kill-ring-mode-map (kbd "g") 'browse-kill-ring-update)
  (define-key browse-kill-ring-mode-map (kbd "l") 'browse-kill-ring-occur)
  (define-key browse-kill-ring-mode-map (kbd "e") 'browse-kill-ring-edit)
  (define-key browse-kill-ring-mode-map (kbd "n") 'browse-kill-ring-forward)
  (define-key browse-kill-ring-mode-map (kbd "p") 'browse-kill-ring-previous)
  (define-key browse-kill-ring-mode-map [(mouse-2)] 'browse-kill-ring-mouse-insert)
  (define-key browse-kill-ring-mode-map (kbd "?") 'describe-mode)
  (define-key browse-kill-ring-mode-map (kbd "h") 'describe-mode)
  (define-key browse-kill-ring-mode-map (kbd "y") 'browse-kill-ring-insert)
  (define-key browse-kill-ring-mode-map (kbd "u") 'browse-kill-ring-insert-move-and-quit)
  (define-key browse-kill-ring-mode-map (kbd "i") 'browse-kill-ring-insert)
  (define-key browse-kill-ring-mode-map (kbd "o") 'browse-kill-ring-insert-and-move)
  (define-key browse-kill-ring-mode-map (kbd "x") 'browse-kill-ring-insert-and-delete)
  (define-key browse-kill-ring-mode-map (kbd "RET") 'browse-kill-ring-insert-and-quit)
  (define-key browse-kill-ring-mode-map (kbd "b") 'browse-kill-ring-prepend-insert)
  (define-key browse-kill-ring-mode-map (kbd "a") 'browse-kill-ring-append-insert))

;;;###autoload
(defun browse-kill-ring-default-keybindings ()
  "Set up M-y (`yank-pop') so that it can invoke `browse-kill-ring'.
Normally, if M-y was not preceeded by C-y, then it has no useful
behavior.  This function sets things up so that M-y will invoke
`browse-kill-ring'."
  (interactive)
  (defadvice yank-pop (around kill-ring-browse-maybe (arg))
    "If last action was not a yank, run `browse-kill-ring' instead."
    ;; yank-pop has an (interactive "*p") form which does not allow
    ;; it to run in a read-only buffer.  We want browse-kill-ring to
    ;; be allowed to run in a read only buffer, so we change the
    ;; interactive form here.  In that case, we need to
    ;; barf-if-buffer-read-only if we're going to call yank-pop with
    ;; ad-do-it
    (interactive "p")
    (if (not (eq last-command 'yank))
        (browse-kill-ring)
      (barf-if-buffer-read-only)
      ad-do-it))
  (ad-activate 'yank-pop))

(define-derived-mode browse-kill-ring-edit-mode fundamental-mode
  "Kill Ring Edit"
  "A major mode for editing a `kill-ring' entry.
You most likely do not want to call `browse-kill-ring-edit-mode'
directly; use `browse-kill-ring' instead.

\\{browse-kill-ring-edit-mode-map}"
  (define-key browse-kill-ring-edit-mode-map (kbd "C-c C-c")
    'browse-kill-ring-edit-finish))

(defvar browse-kill-ring-edit-target nil)
(make-variable-buffer-local 'browse-kill-ring-edit-target)

(defun browse-kill-ring-edit ()
  "Edit the `kill-ring' entry at point."
  (interactive)
  (let ((overs (overlays-at (point))))
    (unless overs
      (error "No kill ring entry here"))
    (let* ((target (overlay-get (car overs)
                                'browse-kill-ring-target))
           (target-cell (member target kill-ring)))
      (unless target-cell
        (error "Item deleted from the kill-ring"))
      (switch-to-buffer (get-buffer-create "*Kill Ring Edit*"))
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert target)
      (goto-char (point-min))
      (browse-kill-ring-resize-window)
      (browse-kill-ring-edit-mode)
      (message "%s"
               (substitute-command-keys
                "Use \\[browse-kill-ring-edit-finish] to finish editing."))
      (setq browse-kill-ring-edit-target target-cell))))

(defun browse-kill-ring-edit-finish ()
  "Commit the changes to the `kill-ring'."
  (interactive)
  (if browse-kill-ring-edit-target
      (setcar browse-kill-ring-edit-target (buffer-string))
    (when (y-or-n-p "The item has been deleted; add to front? ")
      (push (buffer-string) kill-ring)))
  (bury-buffer)
  ;; The user might have rearranged the windows
  (when (eq major-mode 'browse-kill-ring-mode)
    (browse-kill-ring-setup (current-buffer)
                            browse-kill-ring-original-window
                            nil
                            browse-kill-ring-original-window-config)
    (browse-kill-ring-resize-window)))

(defmacro browse-kill-ring-add-overlays-for (item &rest body)
  (let ((beg (gensym "browse-kill-ring-add-overlays-"))
        (end (gensym "browse-kill-ring-add-overlays-")))
    `(let ((,beg (point))
           (,end
            (progn
              ,@body
              (point))))
       (let ((o (make-overlay ,beg ,end)))
         (overlay-put o 'browse-kill-ring-target ,item)
         (overlay-put o 'mouse-face 'highlight)))))
;; (put 'browse-kill-ring-add-overlays-for 'lisp-indent-function 1)

(defun browse-kill-ring-elide (str)
  (if (and browse-kill-ring-maximum-display-length
           (> (length str)
              browse-kill-ring-maximum-display-length))
      (concat (substring str 0 (- browse-kill-ring-maximum-display-length 3))
              (browse-kill-ring-propertize "..." 'browse-kill-ring-extra t))
    str))

(defun browse-kill-ring-insert-as-one-line (items)
  (dolist (item items)
    (browse-kill-ring-add-overlays-for item
      (let* ((item (browse-kill-ring-elide item))
             (len (length item))
             (start 0)
             (newl (browse-kill-ring-propertize "\\n" 'browse-kill-ring-extra t)))
        (while (and (< start len)
                    (string-match "\n" item start))
          (insert (substring item start (match-beginning 0))
                  newl)
          (setq start (match-end 0)))
        (insert (substring item start len))))
    (insert "\n")))

(defun browse-kill-ring-insert-as-separated (items)
  (while (cdr items)
    (browse-kill-ring-insert-as-separated-1 (car items) t)
    (setq items (cdr items)))
  (when items
    (browse-kill-ring-insert-as-separated-1 (car items) nil)))

(defun browse-kill-ring-insert-as-separated-1 (origitem separatep)
  (let* ((item (browse-kill-ring-elide origitem))
         (len (length item)))
    (browse-kill-ring-add-overlays-for origitem
                                       (insert item))
    ;; When the kill-ring has items with read-only text property at
    ;; **the end of** string, browse-kill-ring-setup fails with error
    ;; `Text is read-only'.  So inhibit-read-only here.
    ;; See http://bugs.debian.org/225082
    ;; - INOUE Hiroyuki <dombly@kc4.so-net.ne.jp>
    (let ((inhibit-read-only t))
      (insert "\n")
      (when separatep
        (insert (browse-kill-ring-propertize browse-kill-ring-separator
                                             'browse-kill-ring-extra t
                                             'browse-kill-ring-separator t))
        (insert "\n")))))

(defun browse-kill-ring-occur (regexp)
  "Display all `kill-ring' entries matching REGEXP."
  (interactive
   (list
    (browse-kill-ring-read-regexp "Display kill ring entries matching")))
  (assert (eq major-mode 'browse-kill-ring-mode))
  (browse-kill-ring-setup (current-buffer)
                          browse-kill-ring-original-window
                          regexp)
  (browse-kill-ring-resize-window))

(defun browse-kill-ring-fontify-on-property (prop face beg end)
  (save-excursion
    (goto-char beg)
    (let ((prop-end nil))
      (while
          (setq prop-end
                (let ((prop-beg (or (and (get-text-property (point) prop) (point))
                                    (next-single-property-change (point) prop nil end))))
                  (when (and prop-beg (not (= prop-beg end)))
                    (let ((prop-end (next-single-property-change prop-beg prop nil end)))
                      (when (and prop-end (not (= prop-end end)))
                        (put-text-property prop-beg prop-end 'face face)
                        prop-end)))))
        (goto-char prop-end)))))

(defun browse-kill-ring-fontify-region (beg end &optional verbose)
  (when verbose (message "Fontifying..."))
  (let ((buffer-read-only nil))
    (browse-kill-ring-fontify-on-property 'browse-kill-ring-extra 'bold beg end)
    (browse-kill-ring-fontify-on-property 'browse-kill-ring-separator
                                          browse-kill-ring-separator-face beg end))
  (when verbose (message "Fontifying...done")))

(defun browse-kill-ring-update ()
  "Update the buffer to reflect outside changes to `kill-ring'."
  (interactive)
  (assert (eq major-mode 'browse-kill-ring-mode))
  (browse-kill-ring-setup (current-buffer)
                          browse-kill-ring-original-window)
  (browse-kill-ring-resize-window))

(defun browse-kill-ring-setup (kill-buf orig-buf window &optional regexp window-config)
  (with-current-buffer kill-buf
    (unwind-protect
        (progn
          (browse-kill-ring-mode)
          (setq buffer-read-only nil)
          (when (eq browse-kill-ring-display-style
                    'one-line)
            (setq truncate-lines t))
          (let ((inhibit-read-only t))
            (erase-buffer))
          (setq browse-kill-ring-original-buffer orig-buf
                browse-kill-ring-original-window window
                browse-kill-ring-original-window-config
                (or window-config
                    (current-window-configuration)))
          (let ((browse-kill-ring-maximum-display-length
                 (if (and browse-kill-ring-maximum-display-length
                          (<= browse-kill-ring-maximum-display-length 3))
                     4
                   browse-kill-ring-maximum-display-length))
                (items (mapcar
                        (if browse-kill-ring-depropertize
                            #'browse-kill-ring-depropertize-string
                          #'copy-sequence)
                        kill-ring)))
            (when (not browse-kill-ring-display-duplicates)
              ;; I'm not going to rewrite `delete-duplicates'.  If
              ;; someone really wants to rewrite it here, send me a
              ;; patch.
              (require 'cl)
              (setq items (delete-duplicates items :test #'equal)))
            (when (stringp regexp)
              (setq items (delq nil
                                (mapcar
                                 #'(lambda (item)
                                     (when (string-match regexp item)
                                       item))
                                 items))))
            (funcall (or (cdr (assq browse-kill-ring-display-style
                                    browse-kill-ring-display-styles))
                         (error "Invalid `browse-kill-ring-display-style': %s"
                                browse-kill-ring-display-style))
                     items)
;; Code from Michael Slass <mikesl@wrq.com>
            (message
             (let ((entry (if (= 1 (length kill-ring)) "entry" "entries")))
               (concat
                (if (and (not regexp)
                         browse-kill-ring-display-duplicates)
                    (format "%s %s in the kill ring."
                            (length kill-ring) entry)
                  (format "%s (of %s) %s in the kill ring shown."
                          (length items) (length kill-ring) entry))
                (substitute-command-keys
                 (concat "    Type \\[browse-kill-ring-quit] to quit.  "
                         "\\[describe-mode] for help.")))))
;; End code from Michael Slass <mikesl@wrq.com>
            (set-buffer-modified-p nil)
            (goto-char (point-min))
            (browse-kill-ring-forward 0)
            (when regexp
              (setq mode-name (concat "Kill Ring [" regexp "]")))
            (run-hooks 'browse-kill-ring-hook)
            ;; I will be very glad when I can get rid of this gross
            ;; hack, which solely exists for XEmacs users.
            (when (and (featurep 'xemacs)
                       font-lock-mode)
              (browse-kill-ring-fontify-region (point-min) (point-max)))))
      (progn
        (setq buffer-read-only t)))))

;;;###autoload
(defun browse-kill-ring ()
  "Display items in the `kill-ring' in another buffer."
  (interactive)
  (if (eq major-mode 'browse-kill-ring-mode)
      (message "Already viewing the kill ring")
    (let* ((orig-win (selected-window))
           (orig-buf (window-buffer orig-win))
           (buf (get-buffer-create "*Kill Ring*")))
      (browse-kill-ring-setup buf orig-buf orig-win)
      (pop-to-buffer buf)
      (browse-kill-ring-resize-window))))

(provide 'browse-kill-ring)

;;; browse-kill-ring.el ends here
