;;; win-switch.el --- fast, dynamic bindings for window-switching/resizing

;; Copyright (C) 2011, 2012 Christopher R. Genovese, all rights reserved.

;; Author: Christopher Genovese <genovese@cmu.edu>
;; Maintainer: Christopher R. Genovese <genovese@cmu.edu>
;; URL: http://www.stat.cmu.edu/~genovese/emacs/win-switch/

;; Version: 1.0.6
;; Update#: 17
;; Created:      Wed 28 Jul 2011 at 00:27 EDT
;; Last-Updated: Sat 04 Aug 2012 at 14:58 EDT
;; By: Christopher R. Genovese

;; Keywords: window, switch, key bindings, ergonomic, efficient
;; Compatibility: GNU Emacs 22, GNU Emacs 23, Gnu Emacs 24.0.50
;;                Tested on these versions on Mac OS X 10.5.
;;                Testing or feedback for other platforms/versions
;;                would be very much appreciated.


;;; Commentary:
;;
;;  If you use multiple windows in an Emacs frame, you may find yourself
;;  moving through the window configuration using `other-window' (C-x o)
;;  again and again. Because the order of windows in the window list
;;  need not relate intuitively to windows' positions, moving
;;  efficiently can require context-specific prefix arguments along the
;;  way. The tiring outcome is that navigation through a complex window
;;  configuration demands many keystrokes and nontrivial attention.
;;  This package is designed to solve that problem.
;;
;;  While the `windmove' package provides functions for moving
;;  intuitively among windows, the natural key bindings for these
;;  functions (e.g., the arrow keys with some modifier) require a
;;  distant and thus inefficient hand movement. Moreover, one often
;;  wants to mix a variety of window-based operations (other-window,
;;  previous-window, directional movement, resizing) in rapid
;;  succession.
;;
;;  This package builds on the windmove functionality by defining a
;;  command `win-switch-dispatch' that engages a dynamic, transient
;;  keyboard override, allowing one to efficiently move among defined
;;  windows (and frames) -- and even resize, split, delete them -- with
;;  minimal fuss and effort. When the override is engaged, the movement
;;  and resizing commands are bound to simple keys that can be pressed
;;  quickly with one hand. The override ends either when the user exits
;;  explicitly or after a configurable idle time threshold. The happy
;;  outcome is fast and seamless navigation.
;;
;;  To use the package, execute the following code either directly
;;  or in your .emacs file:
;;
;;      (require 'win-switch)
;;      (global-set-key "\C-xo" 'win-switch-dispatch)
;;
;;  or use whatever keybinding you ordinarily have set to `other-window'.
;;  Alternatively, you can use one of a variety of predefined configuration
;;  commands, as in
;;
;;      (require 'win-switch)
;;      (win-switch-setup-keys-ijkl "\C-xo")
;;
;;  which has the same effect as the above.
;;
;;  Now, when executing a window switch (i.e., hitting C-xo), Emacs enters
;;  window switching mode, which lasts until either the user exits the
;;  mode or the idle time exceeds the threshold `win-switch-idle-time'.
;;  During this override, selected keys move among windows (or frames)
;;  or resize the windows. The following keys are bound by default:
;;
;;    + i select the window above the current window.
;;    + k select the window below the current window.
;;    + j select the window left of the current window.
;;    + l select the window right of the current window.
;;    + o cycle forward through the window list in the current frame.
;;    + p cycle backward through the window list in the current frame.
;;    + SPACE cycles among existing frames.
;;    + u (and RETURN) exit window switching mode.
;;    + I and K vertically enlarge and shrink the current window, respectively.
;;    + J and L horizontally enlarge and shrink the current window, respectively.
;;    + h and ; split the current window, horizontally and vertically, respectively.
;;    + ESCAPE acts as an "emergency" exit
;;
;;  All other keys exit window switching mode and execute their original function.
;;
;;  By default, window selection wraps around when moving across a frame
;;  edge and window switching mode is forgone when there are only two
;;  windows. But these features, the key bindings, and other parameters
;;  can all be customized, either with the customization facility or
;;  with defvar and setter functions.
;;
;;  The default keybindings are designed for fast and intuitve,
;;  one-handed operation, but if desired the key bindings can be easily
;;  adjusted or reset. Several alternative key configurations are pre-defined
;;  (see `win-switch-setup-keys-ijkl', `win-switch-setup-keys-arrow-ctrl',
;;  `win-switch-setup-keys-arrow-meta', and `win-switch-setup-keys-esdf'
;;  below). The keys also can be rebound in groups via the variables
;;  `win-switch-<name>-keys' where <name> can be one of up, down, left,
;;  right, next-window, previous-window, enlarge-vertically,
;;  shrink-vertically, enlarge-horizontally, shrink-horizontally,
;;  other-frame, exit, split-vertically, split-horizontally, delete-window,
;;  or emergency-exit. These variables should not be set directly,
;;  but rather should be set either by customize or by
;;  using the functions `win-switch-add-key', `win-switch-delete-key',
;;  and `win-switch-set-keys'. For example:
;;
;;    (win-switch-add-key    "O" 'previous-window)
;;    (win-switch-delete-key "p" 'previous-window)
;;    (win-switch-set-keys   '(" " "," "m") 'other-frame)
;;
;;  Note that the last arguments here are win-switch commands not elisp
;;  functions. (Note also that the emergency-exit keys do a hard exit in
;;  case of an unexpected error in user-defined code such as in
;;  customized feedback functions. This command may be removed in future
;;  versions.) At least one exit key must always be defined. Revised
;;  bindings can be set in in the hook `win-switch-load-hook' before
;;  loading the package. (Also see `win-switch-define-key' for setting
;;  general commands in the win-switch keymap, and
;;  `win-switch-set-once-key' for setting commands in the once only
;;  keymap used by `win-switch-dispatch-once'.)
;;
;;  Besides key bindings, the most important customization options are
;;  the following:
;;
;;    + `win-switch-idle-time'
;;    + `win-switch-window-threshold'
;;    + `win-switch-other-window-first'
;;    + `win-switch-wrap-around'  (set via `win-switch-set-wrap-around')
;;
;;  The idle time should be set so that one does not have to either rush
;;  or wait. (While explicit exit always works, it is nice to have
;;  window-switching mode end on its own at just the right time.) This
;;  may require some personalized fiddling to find a comfortable value,
;;  though the default should be pretty good. The window-threshold and
;;  other-window-first control when and if window switching mode is
;;  entered. And wrap-around determines if moving across the edge of the
;;  frame wraps around to the window on the other side.
;;
;;  The other customizable parameters are as follows:
;;
;;    + `win-switch-provide-visual-feedback'
;;    + `win-switch-feedback-background-color'
;;    + `win-switch-feedback-foreground-color'
;;    + `win-switch-on-feedback-function'
;;    + `win-switch-off-feedback-function'
;;    + `win-switch-other-window-function'
;;
;;  The feedback mechanisms are intended to make it salient when
;;  window switching mode is on or off and can be customized at
;;  several scales. The other-window-function replaces `other-window'
;;  for moving between window; the primary motivation is to allow
;;  `icicle-other-window-or-frame' under icicles.
;;
;;  And three hooks can be set as well:
;;
;;    + `win-switch-load-hook'
;;    + `win-switch-on-hook'
;;    + `win-switch-off-hook'
;;
;;  The following functions are used to set options:
;;
;;    + `win-switch-set-wrap-around'
;;    + `win-switch-add-key'
;;    + `win-switch-delete-key'
;;    + `win-switch-set-keys'
;;
;;  There are three main entry points for using this functionality
;;
;;    + `win-switch-dispatch' (alias `win-switch-mode')
;;    + `win-switch-dispatch-once'
;;    + `win-switch-dispatch-with'
;;
;;  The first is the main function, the second is a prefix command that
;;  gives one switch only but allows easy maneuvering in up to five
;;  windows with a single keystroke. (The `once' keys can be set using
;;  the `win-switch-set-once-keys' command.) The last constructs
;;  commands for keybindings that dispatch after some other command.
;;  (See `win-switch-setup-keys-arrow' for a nice example of its use.)
;;
;;  NOTE: win-switch is not a formal major or minor mode, more of an
;;        overriding mode. This started as a way to explore dynamic
;;        keybindings, an idea that is generalized considerably in
;;        my packages `quick-nav' and `power-keys'. The latter
;;        introduces some programming abstractions that can be used
;;        to easily install dynamic keymaps of several flavors.
;;        I plan to use the `power-keys' mechanisms for this package
;;        in a later version.
;;
;; Code Contents
;;   1. (@> "User-Configurable Parameters")
;;   2. (@> "User-Configurable Key Bindings")
;;   3. (@> "Preventing Default Shadowing")
;;   4. (@> "Internal Configuration Data")
;;   5. (@> "Internal Functions and Macros")
;;   6. (@> "Customization Initializers and Option Setters")
;;   7. (@> "User Entry Points")
;;   8. (@> "Pre-defined Configurations")
;;


;;; Change Log:
;;
;;  * 17 Mar 2012 -- Fixed *two* silly typos in fset and in a string constant.
;;                   in win-switch-setup-keys-arrows. The former was causing
;;                   load failure from package.el.
;;
;;  * 17 Jan 2012 -- Removed linkd minor mode nad Package-Requires header
;;                   because they were causing problems with loading
;;                   the package through package.el.
;;
;;  * 18 Dec 2011 -- Fixed error in keylist management functions that
;;                   masked changes to keys on other lists. This
;;                   affected `win-switch-custom-set-keys',
;;                   `win-switch-add-key', and `win-switch-delete-key'.
;;                   Also fixed a few documentation typos.
;;                   Thanks to Mark Hepburn and Brett Presnell for
;;                   finding the error.
;;
;;  * 11 Sep 2011 -- Fixed typo in `win-switch-setup-keys-arrow-ctrl'
;;                   and `win-switch-setup-keys-arrow-meta'.
;;                   Thanks to mpdflaccuesupport.
;;
;;  * 27 Aug 2011 -- Fixed missing hyphen in `win-switch-authors-configuration'
;;
;;  * 20 Aug 2011 -- Updated documentation, moved split and delete
;;                   keys after the others in the file and added function
;;                   to suppress them if desired. Added timer to end
;;                   `win-switch-off-alert' message.
;;
;;  * 19 Aug 2011 -- Adjusted for Emacs shadowing default bindings
;;                   in sub-keymaps. Whenever `win-switch-map' is
;;                   set or adjusted, the default binding is added
;;                   if necessary to the map and sub-keymaps.
;;                   Also, allowed `win-switch-other-window-first'
;;                   to be a function to allow context sensitive
;;                   behavior. Added `win-switch-delete-window-keys'
;;                   and put delete-window in command list.
;;
;;  * 05 Aug 2011 -- Fixed setting functions for the once keys,
;;                   added author config FYI, and minor bug fixes.
;;
;;  * 02 Aug 2011 -- Added setup-key functions, win-switch-dispatch-with,
;;                   win-switch-dispatch-once, minor bug fixes in
;;                   in setters, some clode clean up.
;;
;;  * 01 Aug 2011 -- Added additional hooks, adjusted window-threshold
;;                   handling, added add-key and delete-key setters,
;;                   finalized the documentation, and ran tests.
;;
;;  * 30 Jul 2011 -- Added customization framework, frame switching, and
;;                   custom feedback and other-window functions.
;;
;;  * 29 Jul 2011 -- Adjusted keymap initialization, changed behavior
;;                   of `win-switch-dispatch' with prefix arguments
;;
;;  * 28 Jul 2011 -- Completed and tested main functionality; rediscovered
;;                   several times that messing with overriding-loca-map
;;                   can be dangerous.
;;


;;; License:
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

;;; Code:


(require 'windmove)
(eval-when-compile (require 'cl)) ; push, pop, dolist


;; (@* "User-Configurable Parameters")

(defgroup win-switch nil
  "All customization options for win-switch mode."
  :prefix "win-switch-"
  :group 'convenience
  :link `(url-link :tag "Send Bug Report"
                   ,(concat "mailto:" "genovese@cmu.edu"
                            "?subject=win-switch.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Be sure to include your platform and your Emacs and win-switch versions."))
  :link `(url-link :tag "Download"
                   "http://www.github.com/genovese/emacs-utils/win-switch")
  :link `(url-link :tag "Description"
                   "http://www.emacsiki.org/cgi-bin/wiki/WinSwitch"))

(defgroup win-switch-keys nil
  "Command key bindings for win-switch mode."
  :prefix "win-switch-"
  :group 'win-switch)

;;;###autoload
(defcustom win-switch-idle-time 0.75
  "Cancel window switching mode when idle time exceeds this threshold.
The time is measured in seconds and can be an integer or
floating-point number."
  :type 'number
  :group 'win-switch)

;;;###autoload
(defcustom win-switch-window-threshold 2
  "Number of windows above which dispatch always enters switching mode.
When the current frame has more than this many windows,
`win-switch-dispatch' enters window-switching mode
unconditionally; otherwise, it acts like like
`win-switch-other-window-function' (which is `other-window' by
default).

Besides its effect on window switching behavior, this option also
affects how `win-switch-dispatch' interprets its prefix argument.
See the documentation for `win-switch-dispatch' for details."
  :type 'boolean
  :group 'win-switch)

;;;###autoload
(defcustom win-switch-other-window-first t
  "Whether to move to next window before entering window switching mode.
Should be either a boolean or a boolean function that takes no arguments.
If equal to t or if a function and the function returns a non-nil value,
`win-switch-dispatch' calls `win-switch-next-window' before changing
window-switching modes."
  :type '(choice boolean function)
  :group 'win-switch)

;;;###autoload
(defcustom win-switch-wrap-around  t
  "Whether movement off the edge of the frame wraps around.

To set this variable in Lisp code, do not set the variable
directly but rather call the function
`win-switch-set-wrap-around' with argument 1 to turn wrapping
on and -1 to turn wrapping off."
  :type 'boolean
  :set (lambda (symbol value) (win-switch-set-wrap-around (if value 1 -1)))
  :initialize (lambda (symbol value)
                (setq windmove-wrap-around value)
                (custom-initialize-default symbol value))
  :require 'windmove
  :group 'win-switch)

;;;###autoload
(defcustom win-switch-provide-visual-feedback t
  "Whether to provide visual feedback during window switching mode."
  :type 'boolean
  :group 'win-switch)

;;;###autoload
(defcustom win-switch-feedback-background-color "red"
  "Mode line background color of active window during switching mode."
  :type 'string
  :group 'win-switch)

;;;###autoload
(defcustom win-switch-feedback-foreground-color "white"
  "Mode line foreground color of active window during switching mode."
  :type 'string
  :group 'win-switch)

;;;###autoload
(defcustom win-switch-on-feedback-function  nil
  "Function to turn on visual feedback, or nil for default behavior.
This function of zero arguments is called when entering window
switching mode, and it should set up conditions that make salient
that window switching mode is turned on. Setting this function
should usually be paired with setting
`win-switch-off-feedback-function' to ensure that what is set on
entry is unset on exit. See `win-switch-on-feedback' for the
default behavior."
  :type '(choice (const :tag "Default" nil) ; nil here because other-window has distinct calling sequence
                 function)
  :group 'win-switch)

;;;###autoload
(defcustom win-switch-off-feedback-function  nil
  "Function to turn off visual feedback, or nil for default behavior.
This function of zero arguments is called when exiting window
switching mode, and it should make salient that window switching
mode is turned off and clear any conditions that were set on
entry. Setting this function should usually be paired with
setting `win-switch-on-feedback-function' to ensure that what is
unset on exit had been set on entry. See the function
`win-switch-off-feedback' for the default behavior."
  :type '(choice (const :tag "Default" nil) ; nil here because other-window has distinct calling sequence
                 function)
  :group 'win-switch)

;;;###autoload
(defcustom win-switch-other-window-function  nil
  "Function to switch windows or nil for default, `other-window'."
  :type '(choice (const :tag "Default" nil) ; nil here because other-window has distinct calling sequence
                 function)
  :group 'win-switch)

;;;###autoload
(defvar win-switch-load-hook nil
  "List of functions to be called when win-switch module is loaded.")

;;;###autoload
(defvar win-switch-on-hook nil
  "List of functions to be called as window switching mode is entered.
When these functions are called, the overriding key map will have been
set up, but before the timer has been started.")

;;;###autoload
(defvar win-switch-off-hook nil
  "List of functions to be called after window switching mode is exited.")


;; (@* "User-Configurable Key Bindings")

;;;###autoload
(defcustom win-switch-up-keys  '("i")
  "List of key sequences that select the window above the current one.

To set this variable from Lisp code, do not just set it directly, but
rather use the function `win-switch-set-keys' as follows:

   (win-switch-set-keys <key-list> 'up)

where <key-list> is a list of key bindings."
  :type '(repeat (sexp :format "%v"))
  :set 'win-switch-custom-set-keys
  :initialize 'custom-initialize-default
  :group 'win-switch-keys)

;;;###autoload
(defcustom win-switch-down-keys  '("k")
  "List of key sequences that select the window below the current one.

To set this variable from Lisp code, do not just set it directly, but
rather use the function `win-switch-set-keys' as follows:

   (win-switch-set-keys <key-list> 'down)

where <key-list> is a list of key bindings."
  :type '(repeat (sexp :format "%v"))
  :set 'win-switch-custom-set-keys
  :initialize 'custom-initialize-default
  :group 'win-switch-keys)

;;;###autoload
(defcustom win-switch-left-keys  '("j")
  "List of key sequences that select the window left of the current one.

To set this variable from Lisp code, do not just set it directly, but
rather use the function `win-switch-set-keys' as follows:

   (win-switch-set-keys <key-list> 'left)

where <key-list> is a list of key bindings."
  :type '(repeat (sexp :format "%v"))
  :set 'win-switch-custom-set-keys
  :initialize 'custom-initialize-default
  :group 'win-switch-keys)

;;;###autoload
(defcustom win-switch-right-keys  '("l")
  "List of key sequences that select the window left of the current one.

To set this variable from Lisp code, do not just set it directly, but
rather use the function `win-switch-set-keys' as follows:

   (win-switch-set-keys <key-list> 'right)

where <key-list> is a list of key bindings."
  :type '(repeat (sexp :format "%v"))
  :set 'win-switch-custom-set-keys
  :initialize 'custom-initialize-default
  :group 'win-switch-keys)

;;;###autoload
(defcustom win-switch-next-window-keys  '("o")
  "List of key sequences that select the next window.

To set this variable from Lisp code, do not just set it directly, but
rather use the function `win-switch-set-keys' as follows:

   (win-switch-set-keys <key-list> 'next-window)

where <key-list> is a list of key bindings."
  :type '(repeat (sexp :format "%v"))
  :set 'win-switch-custom-set-keys
  :initialize 'custom-initialize-default
  :group 'win-switch-keys)

;;;###autoload
(defcustom win-switch-previous-window-keys  '("p")
  "List of key sequences that select the next window.

To set this variable from Lisp code, do not just set it directly, but
rather use the function `win-switch-set-keys' as follows:

   (win-switch-set-keys <key-list> 'previous-window)

where <key-list> is a list of key bindings."
  :type '(repeat (sexp :format "%v"))
  :set 'win-switch-custom-set-keys
  :initialize 'custom-initialize-default
  :group 'win-switch-keys)

;;;###autoload
(defcustom win-switch-enlarge-vertically-keys  '("I")
  "List of key sequences that vertically enlarges current window.

To set this variable from Lisp code, do not just set it directly, but
rather use the function `win-switch-set-keys' as follows:

   (win-switch-set-keys <key-list> 'enlarge-vertically)

where <key-list> is a list of key bindings."
  :type '(repeat (sexp :format "%v"))
  :set 'win-switch-custom-set-keys
  :initialize 'custom-initialize-default
  :group 'win-switch-keys)

;;;###autoload
(defcustom win-switch-shrink-vertically-keys  '("K")
  "List of key sequences that vertically shrinks current window.

To set this variable from Lisp code, do not just set it directly, but
rather use the function `win-switch-set-keys' as follows:

   (win-switch-set-keys <key-list> 'shrink-vertically)

where <key-list> is a list of key bindings."
  :type '(repeat (sexp :format "%v"))
  :set 'win-switch-custom-set-keys
  :initialize 'custom-initialize-default
  :group 'win-switch-keys)

;;;###autoload
(defcustom win-switch-shrink-horizontally-keys  '("J")
  "List of key sequences that horizontally shrinks current window.

To set this variable from Lisp code, do not just set it directly, but
rather use the function `win-switch-set-keys' as follows:

   (win-switch-set-keys <key-list> 'shrink-horizontally)

where <key-list> is a list of key bindings."
  :type '(repeat (sexp :format "%v"))
  :set 'win-switch-custom-set-keys
  :initialize 'custom-initialize-default
  :group 'win-switch-keys)

;;;###autoload
(defcustom win-switch-enlarge-horizontally-keys  '("L")
  "List of key sequences that horizontally enlarges current window.

To set this variable from Lisp code, do not just set it directly, but
rather use the function `win-switch-set-keys' as follows:

   (win-switch-set-keys <key-list> 'enlarge-horizontally)

where <key-list> is a list of key bindings."
  :type '(repeat (sexp :format "%v"))
  :set 'win-switch-custom-set-keys
  :initialize 'custom-initialize-default
  :group 'win-switch-keys)

;;;###autoload
(defcustom win-switch-other-frame-keys  '(" ")
  "List of key sequences that select the next frame.

To set this variable from Lisp code, do not just set it directly, but
rather use the function `win-switch-set-keys' as follows:

   (win-switch-set-keys <key-list> 'other-frame)

where <key-list> is a list of key bindings."
  :type '(repeat (sexp :format "%v"))
  :set 'win-switch-custom-set-keys
  :initialize 'custom-initialize-default
  :group 'win-switch-keys)

;;;###autoload
(defcustom win-switch-exit-keys  '("u" [return])
  "List of key sequences that will exit window switching mode.

To set this variable from Lisp code, do not just set it directly, but
rather use the function `win-switch-set-keys' as follows:

   (win-switch-set-keys <key-list> 'exit)

where <key-list> is a list of key bindings."
  :type '(repeat (sexp :format "%v"))
  :set 'win-switch-custom-set-keys
  :initialize 'custom-initialize-default
  :group 'win-switch-keys)

;;;###autoload
(defcustom win-switch-split-horizontally-keys  '(";")  ; visual mnemonic
  "List of key sequences that horizontally splits current window.

To set this variable from Lisp code, do not just set it directly, but
rather use the function `win-switch-set-keys' as follows:

   (win-switch-set-keys <key-list> 'split-horizontally)

where <key-list> is a list of key bindings."
  :type '(repeat (sexp :format "%v"))
  :set 'win-switch-custom-set-keys
  :initialize 'custom-initialize-default
  :group 'win-switch-keys)

;;;###autoload
(defcustom win-switch-split-vertically-keys  '("h")  ; visual not letter mnemonic
  "List of key sequences that vertically splits current window.

To set this variable from Lisp code, do not just set it directly, but
rather use the function `win-switch-set-keys' as follows:

   (win-switch-set-keys <key-list> 'split-vertically)

where <key-list> is a list of key bindings."
  :type '(repeat (sexp :format "%v"))
  :set 'win-switch-custom-set-keys
  :initialize 'custom-initialize-default
  :group 'win-switch-keys)

;;;###autoload
(defcustom win-switch-delete-window-keys  '("0")
  "List of key sequences that deletes current window.

To set this variable from Lisp code, do not just set it directly, but
rather use the function `win-switch-set-keys' as follows:

   (win-switch-set-keys <key-list> 'delete-window)

where <key-list> is a list of key bindings."
  :type '(repeat (sexp :format "%v"))
  :set 'win-switch-custom-set-keys
  :initialize 'custom-initialize-default
  :group 'win-switch-keys)

;;;###autoload
(defcustom win-switch-emergency-exit-keys  '("\M-\C-g")
  "List of additional key sequences that will exit window switching mode.
This exits window switching without any niceties, feedback, or
hooks and so should be used only as a last resort. It is intended
only as a precaution for cases in which an unexpected
problem (e.g., in user defined hooks or function-valued options)
makes it impossible to exit window switching mode by another way.
This should not need really be necessary and may be removed in
future versions.

To set this variable from Lisp code, do not just set it directly, but
rather use the function `win-switch-set-keys' as follows:

   (win-switch-set-keys <key-list> 'emergency-exit)

where <key-list> is a list of key bindings."
  :type '(repeat (sexp :format "%v"))
  :set 'win-switch-custom-set-keys
  :initialize 'custom-initialize-default
  :group 'win-switch-keys)

;;;###autoload
(defcustom win-switch-once-double-next-keys  '("u")
  "List of keys that will advance two windows in `win-switch-dispatch-once'.

To set this variable from Lisp code, do not just set it directly, but
rather use the function `win-switch-set-once-keys' as follows:

   (win-switch-set-once-keys <key-list> 'once-double-next)

where <key-list> is a list of key bindings."
  :type '(repeat (sexp :format "%v"))
  :set (lambda (sym value) (win-switch-set-once-keys value 'once-double-next))
  :initialize 'custom-initialize-default
  :group 'win-switch-keys)

;;;###autoload
(defcustom win-switch-once-double-prev-keys  '("y")
  "List of keys that will move back two windows in `win-switch-dispatch-once'.

To set this variable from Lisp code, do not just set it directly, but
rather use the function `win-switch-set-once-keys' as follows:

   (win-switch-set-once-keys <key-list> 'once-double-prev)

where <key-list> is a list of key bindings."
  :type '(repeat (sexp :format "%v"))
  :set (lambda (sym value) (win-switch-set-once-keys value 'once-double-prev))
  :initialize 'custom-initialize-default
  :group 'win-switch-keys)


;; (@* "Preventing Default Shadowing")

;; Because of a bug (or what I think should be considered a bug) in
;; Emacs, adding sparse prefix key map can shadow the default binding
;; for unbound keys with the same prefix. For instance, if the user adds
;; a binding to a key involving the Meta modifier when no such binding
;; was included before, a sub-keymap is created under meta-prefix-char
;; (default escape or 27). But the default for `win-switch-map' will
;; *not* be found by the lookup process for any unbound meta key
;; sequence. This can cause problems because window-switching mode will
;; not be exited automatically in this case as it should.
;;
;; The function `win-switch-fix-keymap-defaults' adds the necessary
;; defaults in the keymap and sub-keymaps. It is used in the key setting
;; functions and in the `win-switch-map' keymap definition itself.
;;

(defun win-switch-fix-keymap-defaults (map)
  "Adjust keymap MAP to include proper exit defaults.
Returns a modified version of MAP that may share some structure
with the original. In the modified map itself and in any
sub-keymaps, a default binding is added, if a default is not
already present. The default is bound to
`win-switch-exit-and-redo'. This ensures that unbound key
sequences exit from window switching mode. Without this, adding a
sparse prefix key map can shadow the default binding for unbound
keys with the same prefix."
  (let ((fixed-map
         (mapcar (lambda (entry)
                   (if (and (consp entry)
                            (keymapp (cdr entry))
                            (null (lookup-key (cdr entry) [t])))
                       (cons (car entry) (win-switch-fix-keymap-defaults (cdr entry)))
                     entry)) map)))
    (unless (lookup-key fixed-map [t])
      (define-key fixed-map [t] 'win-switch-exit-and-redo))
    fixed-map))


;; (@* "Internal Configuration Data")

(defvar win-switch-commands
  '((win-switch-up-keys                   . win-switch-up)
    (win-switch-down-keys                 . win-switch-down)
    (win-switch-left-keys                 . win-switch-left)
    (win-switch-right-keys                . win-switch-right)
    (win-switch-next-window-keys          . win-switch-next-window)
    (win-switch-previous-window-keys      . win-switch-previous-window)
    (win-switch-enlarge-vertically-keys   . enlarge-window)
    (win-switch-shrink-vertically-keys    . shrink-window)
    (win-switch-shrink-horizontally-keys  . shrink-window-horizontally)
    (win-switch-enlarge-horizontally-keys . enlarge-window-horizontally)
    (win-switch-other-frame-keys          . other-frame)
    (win-switch-exit-keys                 . win-switch-exit)
    (win-switch-split-vertically-keys     . split-window-vertically)
    (win-switch-split-horizontally-keys   . split-window-horizontally)
    (win-switch-delete-window-keys        . delete-window)
    (win-switch-emergency-exit-keys       . win-switch-emergency-exit))
  "Associates pre-defined key lists to window-switching mode commands.")

(defvar win-switch-once-commands
  '((win-switch-once-double-next-keys     . win-switch-double-next-window)
    (win-switch-once-double-prev-keys     . win-switch-double-previous-window))
  "Associates once-only key lists to dispatch-once commands.
See `win-switch-dispatch-once'.")

(defvar win-switch-map
  (let ((map (make-sparse-keymap)))
    ;; must have an exit or we will regret it
    (when (null win-switch-exit-keys)
      (error "The exit keys list for win-switch must remain non-empty"))
    ;; assign specified commands
    (dolist (cmdpair win-switch-commands)
      (dolist (key (symbol-value (car cmdpair)))
        (define-key map key (cdr cmdpair))))
    ;; all other keys exit and then perform their original function
    ;; see comment above regarding the need for the `esc' submap
    (define-key map [t] 'win-switch-exit-and-redo)
    ;; fix up the defaults
    (win-switch-fix-keymap-defaults map))
  "Keymap that is active during window switching mode.
The functions `win-switch-set-keys', `win-switch-add-key', and
`win-switch-delete-key', can be used to set parts of this keymap
corresponding to the available commands. To add an arbitrary
command to this keymap, use `win-switch-define-key' rather than
changing this variable directly. If you do change this keymap
directly, using `define-key' for instance, be very careful to
leave an exit key available")

(defvar win-switch-once-map
  (let ((map (make-sparse-keymap "Window Switching")))
    (dolist (cmdpair (concatenate 'list
                                  win-switch-commands win-switch-once-commands))
      (let ((keysym (car cmdpair))
            (cmd    (cdr cmdpair)))
        (dolist (key (symbol-value keysym))
          (unless (or (eq keysym 'win-switch-exit-keys)
                      (eq keysym 'win-switch-emergency-exit-keys))
            (define-key map key cmd)))))
    map)
  "Keymap referenced by `win-switch-dispatch-once' to make simple moves.
Commands using this keymap do *not* enter window-switching mode, so
no exit keys or commands are required (or helpful). It is safe
to assign directly to this keymap. See `win-switch-dispatch-once.'")

(defvar win-switch-timer nil
  "When non-nil, measures the idle time until window switching mode expires.")

(defvar win-switch-engaged nil
  "Non-nil when window switching mode is on, nil otherwise.")

(defvar win-switch-overriding-map-stack nil
  "Stack to hold saved values of `overridiing-local-map'.")

(defvar win-switch-saved-mode-line-faces nil
  "Holds cons with previous mode-line background and foreground.")


;; (@* "Internal Functions and Macros")

(defmacro win-switch-start-timer (timer secs func)
  "Cancel TIMER, if valid, then run for SECS seconds before executing FUNC.
TIMER is a symbol; SECS is a number or time; and FUNC is a function,
usually a quoted symbol. SECS and FUNC are interpreted as in the corresponding
arguments to `run-with-idle-timer'."
  `(progn
     (when (and ,timer (timerp ,timer))
       (cancel-timer ,timer))
     (setq ,timer (run-with-idle-timer ,secs nil ,func))))

(defmacro win-switch-clear-timer (timer)
  "Cancel TIMER, if valid, then null the symbol's value."
  `(progn
     (when ,timer
       (when (timerp ,timer)
         (cancel-timer ,timer))
       (setq ,timer nil))))

(defmacro win-switch-override-map (map)
  "Save keymap bound to symbol MAP and set MAP to `win-switch-map'."
  `(progn
     (when (and ,map
                (not (eq ,map (car win-switch-overriding-map-stack))))
       (push ,map win-switch-overriding-map-stack))
     (setq ,map win-switch-map)))

(defmacro win-switch-restore-map (map)
  "Reset symbol MAP's value to most recently saved keymap."
  `(setq ,map (pop win-switch-overriding-map-stack)))

(defun win-switch-number-of-windows (&optional maybe-frame)
  "The number of windows in frame MAYBE-FRAME.
If nil, MAYBE-FRAME defaults to the current frame."
  (let* ((frame
          (or maybe-frame (selected-frame)))
         (window
          (frame-selected-window frame)))
    (length (window-list frame nil window))))

(defalias 'win-switch-up     'windmove-up)
(defalias 'win-switch-down   'windmove-down)
(defalias 'win-switch-left   'windmove-left)
(defalias 'win-switch-right  'windmove-right)

(defun win-switch-next-window (arg &optional interactive?)
  "Move to next window in window list.

ARG is in raw prefix argument format, and INTERACTIVE?
is non-nil if the function was called interactively.
When `win-switch-other-window-function' is non-nil,
call that function, either interactively when INTERACTIVE?
is non-nil or passing arg otherwise. When it is nil,
calls `other-window' with the numeric value of ARG.

This is a wrapper that allows the user to override
the standard window switching behavior, for instance
when using icicles."
  (interactive "P\np")
  (if win-switch-other-window-function
      (if interactive?
          (call-interactively 'win-switch-other-window-function)
        (funcall win-switch-other-window-function arg))
    (other-window (prefix-numeric-value arg))))

(defun win-switch-previous-window ()
  "Move to previous window in window list."
  (interactive)
  (win-switch-next-window -1))

(defun win-switch-double-next-window ()
  "Advance two windows in window list."
  (interactive)
  (win-switch-next-window 2))

(defun win-switch-double-previous-window ()
  "Move to second previous window in window list."
  (interactive)
  (win-switch-next-window -2))

(defun win-switch-on-alert ()
  "Alert users, usually in echo area, that window switching is on."
  (message "Window Switching Mode On..."))

(defun win-switch-off-alert ()
  "Alert users, usually in echo area, that window switching is off."
  (message "Window Switching Mode Off.")
  (run-with-timer 0.5 nil (lambda () (message nil))))

(defun win-switch-on-feedback ()
  "Provide visual feedback for the start of window switching mode."
  (if win-switch-on-feedback-function
      (funcall win-switch-on-feedback-function)
    (win-switch-on-alert)
    (setq win-switch-saved-mode-line-faces
          (cons (face-attribute 'mode-line :background)
                (face-attribute 'mode-line :foreground)))
    (set-face-background 'mode-line win-switch-feedback-background-color)
    (set-face-foreground 'mode-line win-switch-feedback-foreground-color)))

(defun win-switch-off-feedback ()
  "Provide visual feedback for the end of window switching mode."
  (if win-switch-off-feedback-function
      (funcall win-switch-off-feedback-function)
    (win-switch-off-alert)
    (when win-switch-saved-mode-line-faces
      (set-face-background 'mode-line (car win-switch-saved-mode-line-faces))
      (set-face-foreground 'mode-line (cdr win-switch-saved-mode-line-faces)))))

(defun win-switch-begin-override ()
  "Engage window switching interface."
  (when (not win-switch-engaged)
    (win-switch-override-map overriding-local-map)
    (condition-case err-val
        (run-hooks 'win-switch-on-hook)
      (error
       (message "win-switch encountered error (%s) in on hook" err-val)))
    (win-switch-start-timer win-switch-timer win-switch-idle-time
                            'win-switch-exit-by-timeout)
    (setq win-switch-engaged t)))

(defun win-switch-end-override ()
  "Disengage window switching interface."
  (when win-switch-engaged
    (win-switch-restore-map overriding-local-map)
    (win-switch-clear-timer win-switch-timer)
    (condition-case err-val
        (run-hooks 'win-switch-off-hook)
      (error
       (message "win-switch encountered error (%s) in off hook" err-val)))
    (setq win-switch-engaged nil)))

(defun win-switch-enter ()
  "Enter window switching mode with feedback."
  (interactive)
  (win-switch-begin-override)
  (when win-switch-provide-visual-feedback
    (win-switch-on-feedback)))

(defun win-switch-exit ()
  "Exit window switching mode with feedback."
  (interactive)
  (win-switch-end-override)
  (when win-switch-provide-visual-feedback
    (win-switch-off-feedback)))

(defun win-switch-exit-by-timeout ()
  "Exit window switching mode when timer expires.
Because an idle timeout necessarily occurs when Emacs is
waiting for an input event, with a stored copy of the
active keymaps, ending the override does not take effect
until *after* the next command. This functions issues
an exit command event."
  (interactive)
  (when win-switch-engaged
    (mapc (lambda (u) (push u unread-command-events))
          (reverse (listify-key-sequence
                    (car win-switch-exit-keys))))))

(defun win-switch-emergency-exit ()
  "Exit from window switching mode without niceties."
  (interactive)
  (let ((win-switch-off-hook nil))
    (win-switch-end-override)
    (win-switch-off-alert)))

(defun win-switch-exit-and-redo ()
  "End window switching mode and re-execute the last key event."
  (interactive)
  (let ((my-keys (this-command-keys-vector)))
    (win-switch-exit)
    (mapc (lambda (u) (push u unread-command-events))
          (reverse (listify-key-sequence my-keys)))))


;; (@* "Customization Initializers and Option Setters")

(defun win-switch-clear-from-keylists (key-list)
  "Remove each key in KEY-LIST from all predefined keylists.

This is used to keep the predefined keylists synchronized with
updates, additions, and deletions. KEY-LIST should be a list of
keybindings. The predefined keylists are the customizable
variables listed as the keys in `win-switch-commands'. No keymap
changes are made by this function."
  (let ((keyvars (mapcar 'first win-switch-commands)))
    (dolist (keyvar keyvars)
      (let ((keys (symbol-value keyvar)))
        (dolist (key key-list)
          (when (member key keys)
            (set keyvar (delete key keys))))))))


;;;###autoload
(defun win-switch-custom-set-keys (key-sym key-list)
  "Set specified key list and adjust `win-switch-map' and other key lists.

This is the most general function for changing the bindings of
commands represented in the predefined key lists. See also
`win-switch-set-keys', which is the API entry point to this
function, along with `win-switch-add-key' and
`win-switch-delete-key'. Note that changing `win-switch-map'
directly need not be reliable, and also see
`win-switch-define-key' for a safe way to make general bindings.

KEY-SYM is a symbol that must be represented in the alist
`win-switch-commands'. The corresponding list of keys will be set
to KEY-LIST, which should be a list of keybindings, and each key
in the list will be bound to the corresponding commands.
An error is raised when trying to empty the exit list."
  (let ((cmd (cdr (assoc key-sym win-switch-commands))))
    (unless cmd
      (error "Symbol %s is not a valid win-switch key list" key-sym))
    (when (and (null key-list) (eq key-sym 'win-switch-exit-keys))
      (error "The exit keys list for win-switch must remain non-empty"))
    ;; Remove old bindings for the command before rebinding
    ;; | ATTN: Could use
    ;; |  (substitute-key-definition cmd 'win-switch-exit-and-redo
    ;; |                                 win-switch-map)
    ;; | here but that would also clear user bindings made directly by
    ;; | user and not in the corresponding key list. It might still be
    ;; | a good idea, though, but now keeping key lists in better sync.
    (dolist (oldkey (symbol-value key-sym))
      (when (eq cmd (lookup-key win-switch-map oldkey))
        (define-key win-switch-map oldkey 'win-switch-exit-and-redo)))
    ;; Remove keys in key-list from all key lists to keep lists in sync
    (win-switch-clear-from-keylists key-list)
    ;; Set new keylist and rebind
    (set key-sym key-list)
    (dolist (newkey key-list)
      (define-key win-switch-map newkey cmd)))
  (setq win-switch-map (win-switch-fix-keymap-defaults win-switch-map))
  key-list)

(defun win-switch-name-to-command-sym (name)
  "Convert abbreviated command name to a key list symbol.

NAME can be either a symbol or a string, and can be either
a full name of the form win-switch-<cmd>-keys or just
the <cmd> component, where for example <cmd> is up, down,
and so forth. Returns the associated win-switch-<cmd>-keys
symbol."
  (let* ((name-str (if (stringp name) name (symbol-name name)))
         (name-sym (if (symbolp name) name (intern name))))
    (if (assoc name-sym win-switch-commands)
        name-sym
      (intern (format "win-switch-%s-keys" name-str)))))

;;;###autoload
(defun win-switch-set-keys (key-list name)
  "Bind specified keys to a command and adjust `win-switch-map'.

KEY-LIST is a list of keys, each acceptable to `define-key'. NAME
is either a string or a symbol, either of the form
win-switch-<cmd>-keys or just <cmd>, where <cmd> is one of `up',
`down', `left', `right', `next-window', `previous-window',
`enlarge-vertically', `shrink-vertically',
`enlarge-horizontally', `shrink-horizontally', `other-frame',
`exit', `split-vertically', `split-horizontally',
`delete-window', or `emergency-exit'. (Specifically, it must be
on of the keys in the alist `win-switch-commands'.)"
  (interactive "XList of Key Sequences: \nSSet to win-switch command name: ")
  (win-switch-custom-set-keys (win-switch-name-to-command-sym name) key-list))

;;;###autoload
(defun win-switch-add-key (key name)
  "Add KEY to command list associated with NAME.
KEY is a key binding in any form acceptable to `define-key'.
NAME should be a symbol or string for which the variable
`win-switch-NAME-keys' is defined."
  (interactive "KKey sequence: \nSAdd key %s to win-switch command name: ")
  (let* ((sym (win-switch-name-to-command-sym name))
         (cmd (cdr (assoc sym win-switch-commands)))
         (lst (and cmd (symbol-value sym))))
    (unless cmd
      (error "%s is not a valid win-switch key type" name))
    (if (member key lst)
        lst
      ;; ATTN: Should probably use win-switch-custom-set-keys here instead
      ;;       of what follows, even though it would do extra work. E.g.,
      ;;       (win-switch-custom-set-keys sym (cons key lst))
      (define-key win-switch-map key cmd)
      (win-switch-clear-from-keylists (list key))
      (set sym (cons key lst))))
  (setq win-switch-map (win-switch-fix-keymap-defaults win-switch-map)))

;;;###autoload
(defun win-switch-delete-key (key name)
  "Remove KEY from command list associated with NAME.
NAME should be a symbol or string for which the variable
`win-switch-NAME-keys' is defined. KEY is a key binding in any
form acceptable to `define-key'. Removing the last exit key
raises an error, and the last of any other key prompts an alert
message."
  (interactive "KKey sequence: \nSDelete key %s from win-switch command name: ")
  (let* ((sym (win-switch-name-to-command-sym name))
         (cmd (cdr (assoc sym win-switch-commands)))
         (lst (and cmd (symbol-value sym))))
    (unless cmd
      (error "%s is not a valid win-switch key type" name))
    (unless (member key lst)
      (error "Key %s is not associated with command %s" key name))
    (when (null (cdr lst))       ; only one key remaining to be deleted
      (if (eq name 'exit)
          (error "The exit keys list for win-switch must remain non-empty")
        (message "Removing last key from command list win-switch-%s-keys" name)))
    (define-key win-switch-map key 'win-switch-exit-and-redo)
    (set sym (delete key lst))
    (setq win-switch-map (win-switch-fix-keymap-defaults win-switch-map))))

;;;###autoload
(defun win-switch-define-key (key def &optional force-no-default)
  "Safely bind KEY to DEF in win-switch keymap.
Attempting to bind the last exit key raises an error. KEY and DEF
are a keybinding and definition, respectively, as would be
acceptable to `define-key'. If DEF is a keymap, ensure that
keymap (and all sub-keymaps) have an exit-inducing
default (`win-switch-exit-and-redo'), unless FORCE-NO-DEFAULT is
non-nil."
  (interactive "KKey sequence: \nCSet key %s to command: ")
  (when (and (null (cdr win-switch-exit-keys)) ; length <= 1
             (member key win-switch-exit-keys)
             (not (eq def 'win-switch-exit)))
    (error "The exit keys list for win-switch must remain non-empty"))
  (win-switch-clear-from-keylists (list key))
  (define-key win-switch-map key def)
  (unless force-no-default
    (setq win-switch-map (win-switch-fix-keymap-defaults win-switch-map))))

;; Ideally, setting the once keys would be integrated into key setting
;; interface for the regular commands. But this would add nontrivial
;; complexity for limited gain, so we will just handle these keys
;; separately, with only one entry point.

;;;###autoload
(defun win-switch-set-once-keys (key-list name)
  "Bind specified keys to a once-only command and adjust `win-switch-once-map'.
KEY-LIST is a list of keys, each acceptable to `define-key'. NAME
is either a string or a symbol, either of the form
win-switch-once-<cmd>-keys or just <cmd>, where <cmd> is one of
`double-next' or `double-prev'. (Specifically, it must be
on of the keys in the alist `win-switch-once-commands'.)"
  (interactive "XList of Key Sequences: \nSSet to win-switch-once command name: ")
  (let* ((name-str (if (stringp name) name (symbol-name name)))
         (name-sym (if (symbolp name) name (intern name)))
         (cmd-list (concatenate 'list win-switch-commands win-switch-once-commands))
         (cmdpair  (or (assoc name-sym cmd-list)
                       (assoc (intern (format "win-switch-%s-keys" name-str))
                              cmd-list)))
         (keysym   (car cmdpair))
         (cmd      (cdr cmdpair)))
    (unless cmd
      (error "%s is not a valid win-switch-once-command" name))
    (unless (or (eq keysym 'win-switch-exit-keys) (eq keysym 'win-switch-emergency-exit-keys))
      (dolist (key (symbol-value keysym))
        (define-key win-switch-once-map key nil))
      (set keysym key-list)
      (dolist (key key-list)
        (define-key win-switch-once-map key cmd)))))

;;;###autoload
(defun win-switch-set-wrap-around (&optional wrap)
  "Toggle or set window wrapping behavior.
When WRAP is nil, toggle setting of `win-switch-wrap-around'.
Otherwise, WRAP should be an integer, negative to turn off
wrapping and non-negative to turn it on. (Value t also
turns it on for convenience.) This function
synchronizes `windmove-wrap-around' accordingly."
  (interactive "p")
  (setq win-switch-wrap-around
        (cond
         ((integerp wrap)
          (if (< wrap 0) nil t))
         ((eq wrap t)
          t)
         (t
          (not win-switch-wrap-around))))
  (setq windmove-wrap-around win-switch-wrap-around))


;; (@* "User Entry Points")

;;;###autoload
(defun win-switch-dispatch (&optional must-enter-or-prefix)
  "Enter window switching mode, or select next window in the frame.

If the variable `win-switch-other-window-first' is non-nil, then
`win-switch-next-window' is called *before* entering window
switching mode.

The variable `win-switch-window-threshold' determines both the
switching behavior and how this function interprets its argument.

  * When `win-switch-window-threshold' is less than or equal to 0,
    window switching mode is always entered and the argument
    MUST-ENTER-OR-PREFIX is interpreted as a raw format prefix
    argument for any calls to `win-switch-next-window'. This only
    matters if `win-switch-other-window-first' is (or returns)
    a non-nil value.

  * When `win-switch-window-threshold' is greater than 0, entry
    to window switching mode occurs if either the number of
    windows in the current frame is above
    `win-switch-window-threshold' or if the prefix
    argument (MUST-ENTER-OR-PREFIX) is non-nil. Any calls to
    `win-switch-next-window' determined by the configuration are
    still made, and they are given nil as an argument. If window
    switching mode is not entered, `win-switch-next-window' is
    called.

While more complicated than ideal, this dichotomy gives maximum
flexibility for several common use cases."
  (interactive "P")
  (let ((enter (or (<= win-switch-window-threshold 0)
                   must-enter-or-prefix
                   (> (win-switch-number-of-windows) win-switch-window-threshold)))
        (arg (and (<= win-switch-window-threshold 0)
                  must-enter-or-prefix)))
    (when (or (and win-switch-other-window-first
                   (if (functionp win-switch-other-window-first)
                       (funcall win-switch-other-window-first)
                     t))
              (not enter))
      (win-switch-next-window arg))
    (when enter
      (win-switch-enter))))

;;;###autoload
(defalias 'win-switch-mode 'win-switch-dispatch)

;;;###autoload
(defmacro win-switch-dispatch-with (command &optional force-enter)  ; would prefer a closure here
  "Produce a command to execute COMMAND and then `win-switch-dispatch'.
COMMAND can be a (quoted) symbol, a lambda form, or a variable
bound to a function. FORCE-ENTER, if non-nil, forces
`win-switch-dispatch' to enter window switching mode.

Note that the call to COMMAND replaces any automatic calls to
`win-switch-next-window' or `win-switch-other-window-function'
during dispatch."
  `(lambda (&optional arg)
     (interactive "P")
     (let ((win-switch-other-window-first t)
           (win-switch-other-window-function ,command)
           ,@(if force-enter (list '(win-switch-window-threshold 0)) nil))
       (win-switch-dispatch arg))))

;;;###autoload
(fset 'win-switch-dispatch-once 'win-switch-once-map)
(put  'win-switch-dispatch-once 'function-documentation
      "Prefix command to execute one window-switching operation.
This command does not enter window-switching mode, nor does it
require an exit. Except for the exit commands, which are
excluded, the commands and keys are shared with
`win-switch-dispatch'. In addition, this includes commands to
move forward and backward by two windows. See
`win-shift-once-double-next-keys' and
`win-shift-once-double-prev-keys' for the associated keys. Taken
together, these bindings make it convenient to use a single key
sequence to navigate conveniently with up to five windows.")


;; (@* "Pre-defined Configurations")

;;;###autoload
(defun win-switch-remove-split-and-delete-keys ()
  "Eliminate window-splitting and deleting keys from win-switch mode."
  (win-switch-set-keys '() 'split-horizontally)
  (win-switch-set-keys '() 'split-vertically)
  (win-switch-set-keys '() 'delete-window))

;;;###autoload
(defun win-switch-setup-keys-ijkl (&rest dispatch-keys)
  "Restore default key commands and bind global dispatch keys.
Under this setup, keys i, j, k, and l will switch windows,
respectively, up, left, down, and right, with other functionality
bound to nearby keys. The arguments DISPATCH-KEYS, if non-nil,
should be a list of keys that will be bound globally to
`win-switch-dispatch'."
  (interactive)
  (win-switch-set-keys '("i") 'up)
  (win-switch-set-keys '("k") 'down)
  (win-switch-set-keys '("j") 'left)
  (win-switch-set-keys '("l") 'right)
  (win-switch-set-keys '("o") 'next-window)
  (win-switch-set-keys '("p") 'previous-window)
  (win-switch-set-keys '("I") 'enlarge-vertically)
  (win-switch-set-keys '("K") 'shrink-vertically)
  (win-switch-set-keys '("J") 'shrink-horizontally)
  (win-switch-set-keys '("L") 'enlarge-horizontally)
  (win-switch-set-keys '(" ") 'other-frame)
  (win-switch-set-keys '("u" [return]) 'exit)
  (win-switch-set-keys '(";") 'split-horizontally)
  (win-switch-set-keys '("h") 'split-vertically) ; visual not letter mnemonic
  (win-switch-set-keys '("0") 'delete-window)
  (win-switch-set-keys '("\M-\C-g") 'emergency-exit)
  (dolist (key dispatch-keys)
    (global-set-key key 'win-switch-dispatch)))

;;;###autoload
(defun win-switch-setup-keys-ijkl-minimal (&rest dispatch-keys)
  "Restore default key commands and bind global dispatch keys.
Split and delete keys are excluded from the map for simplicity.
Under this setup, keys i, j, k, and l will switch windows,
respectively, up, left, down, and right, with other functionality
bound to nearby keys. The arguments DISPATCH-KEYS, if non-nil,
should be a list of keys that will be bound globally to
`win-switch-dispatch'."
  (interactive)
  (apply 'win-switch-setup-keys-ijkl dispatch-keys)
  (win-switch-remove-split-and-delete-keys))

;;;###autoload
(defalias 'win-switch-setup-keys-default 'win-switch-setup-keys-ijkl)

;;;###autoload
(defun win-switch-setup-keys-arrows (modifier &rest dispatch-keys)
  "Set arrow keys as both dispatch and direction control.
Under this setup, pressing an arrow key with MODIFIER does a
window switch in the corresponding direction and then calls
`win-switch-dispatch'. When window-switching mode is engaged, the
arrow keys continue to switch windows in the corresponding
direction, with all the other functionality bound to nearby keys.
MODIFIER is a symbol, one of control, meta, alt, hyper, super but
*not* shift, which is used for enlarging. The arguments
DISPATCH-KEYS, if non-nil, should be a list of keys that will be
bound globally to `win-switch-dispatch'."
  (interactive "SModifier symbol: ")
  (when (eq modifier 'shift)
    (error "The shift modifier cannot be used for dispatch"))
  (win-switch-set-keys '([up]) 'up)
  (win-switch-set-keys '([down]) 'down)
  (win-switch-set-keys '([left]) 'left)
  (win-switch-set-keys '([right]) 'right)
  (win-switch-set-keys '("/") 'next-window)
  (win-switch-set-keys '(".") 'previous-window)
  (win-switch-set-keys '([(shift up)]) 'enlarge-vertically)
  (win-switch-set-keys '([(shift down)]) 'shrink-vertically)
  (win-switch-set-keys '([(shift left)]) 'shrink-horizontally)
  (win-switch-set-keys '([(shift right)]) 'enlarge-horizontally)
  (win-switch-set-keys '(" ") 'other-frame)
  (win-switch-set-keys '([return]) 'exit)
  (win-switch-set-keys '(";") 'split-horizontally)
  (win-switch-set-keys '("'") 'split-vertically)
  (win-switch-set-keys '("l") 'delete-window)
  (win-switch-set-keys '("\M-\C-g") 'emergency-exit)
  (dolist (key dispatch-keys)
    (global-set-key key 'win-switch-dispatch))
  (global-set-key (vector (list modifier 'up))    (win-switch-dispatch-with 'win-switch-up))
  (global-set-key (vector (list modifier 'down))  (win-switch-dispatch-with 'win-switch-down))
  (global-set-key (vector (list modifier 'left))  (win-switch-dispatch-with 'win-switch-left))
  (global-set-key (vector (list modifier 'right)) (win-switch-dispatch-with 'win-switch-right)))

;;;###autoload
(defun win-switch-setup-keys-arrow-ctrl (&rest dispatch-keys)
  "Set arrow keys as both dispatch (w/control modifer) and direction.
With a control modifier, Each arrow key causes a window switch in
the corresponding direction and engages window-switching mode if
the configuration parameters indicate so. When window-switching
mode is engaged, the arrow keys theh continue to switch windows
in the corresponding direction. The arguments DISPATCH-KEYS, if
non-nil, should be a list of keys that will be bound globally to
`win-switch-dispatch'."
  (interactive)
  (apply 'win-switch-setup-keys-arrows (cons 'control dispatch-keys)))

;;;###autoload
(defun win-switch-setup-keys-arrow-meta (&rest dispatch-keys)
  "Set arrow keys as both dispatch (w/meta modifer) and direction.
With a meta modifier, Each arrow key causes a window switch in
the corresponding direction and engages window-switching mode if
the configuration parameters indicate so. When window-switching
mode is engaged, the arrow keys theh continue to switch windows
in the corresponding direction. The arguments DISPATCH-KEYS, if
non-nil, should be a list of keys that will be bound globally to
`win-switch-dispatch'."
  (interactive)
  (apply 'win-switch-setup-keys-arrows (cons 'meta dispatch-keys)))

;;;###autoload
(defun win-switch-setup-keys-esdf (&rest dispatch-keys)
  "Set left-handed keys mirroring defaults and bind global dispatch keys.
Under this setup, keys e, s, d, and f will switch windows,
respectively, up, left, down, and right, with other functionality
bound to nearby keys. The arguments DISPATCH-KEYS, if non-nil,
should be a list of keys that will be bound globally to
`win-switch-dispatch'."
  (interactive)
  (win-switch-set-keys '("e") 'up)
  (win-switch-set-keys '("d") 'down)
  (win-switch-set-keys '("s") 'left)
  (win-switch-set-keys '("f") 'right)
  (win-switch-set-keys '("w") 'next-window)
  (win-switch-set-keys '("q") 'previous-window)
  (win-switch-set-keys '("E") 'enlarge-vertically)
  (win-switch-set-keys '("D") 'shrink-vertically)
  (win-switch-set-keys '("S") 'shrink-horizontally)
  (win-switch-set-keys '("F") 'enlarge-horizontally)
  (win-switch-set-keys '(" ") 'other-frame)
  (win-switch-set-keys '("r" [return]) 'exit)
  (win-switch-set-keys '("3") 'split-horizontally)
  (win-switch-set-keys '("2") 'split-vertically)
  (win-switch-set-keys '("4") 'delete-window)
  (win-switch-set-keys '("\M-\C-g") 'emergency-exit)
  (dolist (key dispatch-keys)
    (global-set-key key 'win-switch-dispatch)))

;;;###autoload
(defun win-switch-authors-configuration ()
  "Win-switch configuration prefered by the package author."
  ;; Perhaps even a little shorter...
  (setq win-switch-idle-time 0.7)
  ;; For two windows, we can always use C-u to force entry
  (setq win-switch-window-threshold 2)
  ;; With more than 3 windows, doing an other-window first can be
  ;; confusing -- easier to just move where you want to go.
  ;; But with 3 or fewer windows, it's much more efficient.
  (setq win-switch-other-window-first (lambda () (null (nthcdr 3 (window-list)))))
  ;; Mode-line visual feedback is a potent cue
  (setq win-switch-provide-visual-feedback t)
  (setq win-switch-feedback-background-color "red")
  (setq win-switch-feedback-foreground-color "white")
  ;; No special functions, though icicles remaps other-window
  ;; which gets used here and whose argument is respected
  (setq win-switch-on-feedback-function nil)
  (setq win-switch-off-feedback-function nil)
  (setq win-switch-other-window-function nil)
  ;; Wrap around makes things easier
  (win-switch-set-wrap-around 1)
  ;; These two keys get easily confused when typing quickly
  ;; so it's easiest to not have to be too precise.
  ;; I just use both for window switching.
  (win-switch-setup-keys-ijkl "\C-xo" "\C-x\C-o"))

(run-hooks 'win-switch-load-hook)

(provide 'win-switch)

;; Local Variables:
;; mode: emacs-lisp
;; End:

;;; win-switch.el ends here
