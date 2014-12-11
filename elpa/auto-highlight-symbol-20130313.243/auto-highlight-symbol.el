;;; auto-highlight-symbol.el --- Automatic highlighting current symbol minor mode

;; Copyright (C) 2009 2010 Mitsuo Saito
;; Created date 2009-03-03 21:44 +0900

;; Author: Mitsuo Saito <arch320@NOSPAM.gmail.com>
;; Adapted-By: Gennadiy Zlobin <gennad.zlobin@NOSPAM.gmail.com>
;; Version: 20130313.243
;; X-Original-Version: 1.55
;; Keywords: highlight face match convenience
;; URL: http://github.com/gennad/auto-highlight-symbol/raw/master/auto-highlight-symbol.el
;; Compatibility: GNU Emacs 22.3 23.x 24.x later
;;
;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;
;; If you have `linkd.el' turn on `linkd-mode'
;; and (setq linkd-use-icons t ) more easily navigation.
;; You can get `linkd.el' here:
;;  http://www.emacswiki.org/cgi-bin/wiki/download/linkd.el
;;  http://www.emacswiki.org/emacs/linkd.tar.gz -- with cool icon
;;

;;; (@* "Index" )
;;
;; (@> "What's this")        I am ...
;; (@> "Setup")              Basic setup
;; (@> "Screencast")         Screencast
;; (@> "Mode map")           Key binding
;;
;; (@> "Note")               Performance note
;;
;; (@> "Custom variable")    Customizable varible
;; (@> "Face")               Face used in auto-highlight-symbol-mode
;; (@> "Highlight Rules")    Whether to highlight the symbol.
;;
;; (@> "Internal variable")  Internal variables
;; (@> "Logging")            Log data
;; (@> "Range plugin")       Range plugin functions
;; (@> "Built-in plugin")    Built-in plugin section
;; (@> "Timer")              Timer functions
;; (@> "Idle")               Idle functions
;; (@> "Highlight")          Highlight functions
;; (@> "Edit mode")          Edit mode functions
;; (@> "Select")             Selective functions
;; (@> "Misc")               Miscellaneous
;; (@> "Interactive")        Interactive functions
;; (@> "Define mode")        Mode definition
;; (@> "Revert")             Protect from revert-buffer
;;

;;; (@* "What's this" )
;;
;;  A minor mode for emacs.
;;
;;   * automatic highlighting current symbol like eclipse IDE.
;;   * cycle through highlighted locations.
;;   * can specify the range to highlight.
;;   * can edit the highlighted symbols at a time.
;;
;;  Tested on GNU Emacs 22.3/23.2/24.0.50/24.2
;;

;;; (@* "Setup" )
;;
;; Basic steps to setup:
;;   1. Place `auto-highlight-symbol.el' in your `load-path'.
;;
;;   2. In your `.emacs.el' file
;;      (require 'auto-highlight-symbol)
;;      (global-auto-highlight-symbol-mode t)
;;

;;; (@* "Screencast" )
;;
;;  Screencast on YouTube and ScreenToaster
;;    YouTube -- http://www.youtube.com/watch?v=xzJ2r4-s7fo
;;    ScreenToaster -- http://www.screentoaster.com/watch/stUE9VQ0dMRFtXRlVeU19cX1Bd/auto_highlight_symbol_mode_screencast
;;
;;
;;; More Information (currently underconstruction)
;;   See also http://github.com/mitsuo-saito/auto-highlight-symbol-mode/wiki/
;;

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `ahs-forward'
;;    Select highlighted symbols forwardly.
;;  `ahs-backward'
;;    Select highlighted symbols backwardly.
;;  `ahs-forward-definition'
;;    Select highlighted symbols forwardly. only symbol definition.
;;  `ahs-backward-definition'
;;    Select highlighted symbols backwardly. only symbol definition.
;;  `ahs-back-to-start'
;;    Go back to the starting point.
;;  `ahs-change-range'
;;    Current plugin change to `RANGE' plugin. `RANGE' defaults to next runnable plugin.
;;  `ahs-set-idle-interval'
;;    Set wait until highlighting symbol when emacs is idle.
;;  `ahs-display-stat'
;;    Display current status.
;;  `ahs-highlight-now'
;;    Highlight NOW!!
;;  `ahs-goto-web'
;;    Go to official? web site.
;;  `ahs-edit-mode'
;;    Turn on edit mode. With a prefix argument, current plugin change to `whole buffer' temporary.
;;  `auto-highlight-symbol-mode'
;;    Toggle Auto Highlight Symbol Mode
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `ahs-modes'
;;    Major modes `auto-highlight-symbol-mode' can run on.
;;  `ahs-suppress-log'
;;    *Non-nil means suppress log message.
;;  `ahs-log-echo-area-only'
;;    *Non-nil means log doesn't display the `*Messages*' buffer.
;;  `ahs-decorate-log'
;;    *Non-nil means decorate logs.
;;  `ahs-default-range'
;;    Default Plugin.
;;  `ahs-edit-mode-lighter-pair'
;;    Decorate mode line lighter in edit mode.
;;  `ahs-select-invisible'
;;    Behavior when selected symbol in hidden text.
;;  `auto-highlight-symbol-mode-hook'
;;    Hook for `auto-highlight-symbol-mode'.
;;  `ahs-edit-mode-on-hook'
;;    Normal hook for run when entering edit mode.
;;  `ahs-edit-mode-off-hook'
;;    Normal hook for run when go out edit mode.
;;  `ahs-idle-interval'
;;    Number of seconds to wait before highlighting symbol.
;;  `ahs-case-fold-search'
;;    *Non-nil means symbol search ignores case.
;;  `ahs-include'
;;    Variable for start highlighting.
;;  `ahs-exclude'
;;    Variable for inhibit highlighting.
;;  `ahs-face-check-include-overlay'
;;    *Non-nil means face checks include overlay face.
;;  `ahs-inhibit-face-list'
;;    Face list for inhibit highlighting.
;;  `ahs-definition-face-list'
;;    Face list for higilight definition.
;;  `ahs-plugin-bod-modes'
;;    Major modes `beginning of defun' plugin can run on.
;;  `ahs-plugin-bod-function'
;;    Function used in `beginning of defun' plugin.

;;
;; Happy Coding !!
;;

;;; SCM Log
;;
;;   $Revision: 243:6aa59061b1df tip $
;;   $Commiter: Mitso Saito <arch320@NOSPAM.gmail.com> $
;;   $LastModified: Sun, 21 Nov 2010 14:42:11 +0900 $
;;
;;   $Lastlog: font lock again $
;;

;;; (@* "Changelog" )
;;
;; v1.54 beta
;;   Bug fix release
;;   ** fix overlay violation problem in edit mode(backward) - !incomplete!
;;   fix font-lock problem
;;   fix built-in plugin
;;   add onekey edit
;;   remove ahs-invisible-face-list
;;   remove obsoleted alias
;;   minor bug fix
;;
;; v1.53 2010-11-03 22:17 +0900
;;   improve invisible overlay's handling
;;   new plugin property `face' available
;;   add ahs-back-to-start
;;   minor bug fix
;;
;; v1.52 2010-10-31 14:46 +0900
;;   skip folding(select function only)
;;
;; v1.51 2010-10-30 09:17 +0900
;;   plugin minor change
;;
;; v1.5  2010-10-30 02:31 +0900
;;   add range plugin
;;    ahs-whole-of-buffer is not working.
;;    use ahs-default-range instead.
;;    ahs-mode-lighter , ahs-wmode-lighter is not be used
;;
;; v1.03 2010-10-28 07:00 +0900
;;   bug fix
;;
;; v1.02 2010-10-26 23:39 +0900
;;   minor fix
;;
;; v1.01 2010-10-26 20:50 +0900
;;   add edit mode hook for protect overlay
;;
;; v1.0  2010-10-26 16:33 +0900
;;   first release
;;

;;; (@* "TODO" )
;;
;;  fix overlay violation problem
;;  fix poor doc
;;  face check tweak
;;  add onekey-***
;;  refactor
;;  cleanup
;;  add comment
;;

;;; Code:

(eval-when-compile
  ;; Suppress bytecompiler error warning
  (require 'easy-mmode)
  (require 'cl)
  (defvar dropdown-list-overlays nil))

(eval-and-compile
  (defconst ahs-web "http://github.com/mitsuo-saito/auto-highlight-symbol-mode/")
  ;; Compatibility
  (if (or (>= emacs-major-version 24)
          (and (= emacs-major-version 23)
               (>= emacs-minor-version 2)))
      (defalias 'ahs-called-interactively-p 'called-interactively-p)
    (defmacro ahs-called-interactively-p (&optional arg)
      '(called-interactively-p))))

(defconst ahs-mode-vers
  "$Id: auto-highlight-symbol.el,v 243:6aa59061b1df 2010-11-21 14:42 +0900 arch320 $"
  "auto-highlight-symbol-mode version.")

;;
;; (@* "Custom variable" )
;;
(defgroup auto-highlight-symbol nil
  "Automatic highlighting current symbol minor mode"
  :group 'convenience
  :link `(url-link :tag "Download latest version"
                   ,(eval-when-compile (concat ahs-web "raw/master/auto-highlight-symbol.el")))
  :link `(url-link :tag "Wiki" ,(eval-when-compile (concat ahs-web "wiki/")))
  :link `(url-link :tag "Information" ,(eval-when-compile ahs-web)))

(defcustom ahs-modes
  '( actionscript-mode
     apache-mode
     bat-generic-mode
     c++-mode
     c-mode
     csharp-mode
     css-mode
     dos-mode
     emacs-lisp-mode
     html-mode
     ini-generic-mode
     java-mode
     javascript-mode
     js-mode
     lisp-interaction-mode
     lua-mode
     latex-mode
     makefile-mode
     makefile-gmake-mode
     markdown-mode
     moccur-edit-mode
     nxml-mode
     nxhtml-mode
     outline-mode
     perl-mode cperl-mode
     php-mode
     python-mode
     rc-generic-mode
     reg-generic-mode
     ruby-mode
     sgml-mode
     sh-mode
     squirrel-mode
     text-mode
     tcl-mode
     visual-basic-mode )
  "Major modes `auto-highlight-symbol-mode' can run on."
  :group 'auto-highlight-symbol
  :type '(repeat symbol))

(defcustom ahs-suppress-log nil
  "*Non-nil means suppress log message."
  :group 'auto-highlight-symbol
  :type 'boolean)

(defcustom ahs-log-echo-area-only t
  "*Non-nil means log doesn't display the `*Messages*' buffer."
  :group 'auto-highlight-symbol
  :type 'boolean)

(defcustom ahs-decorate-log t
  "*Non-nil means decorate logs."
  :group 'auto-highlight-symbol
  :type 'boolean)

(defcustom ahs-default-range 'ahs-range-display
  "Default Plugin."
  :group 'auto-highlight-symbol
  :type '(choice (symbol :tag "Display area" ahs-range-display)
                 (symbol :tag "Whole buffer" ahs-range-whole-buffer)))

(defcustom ahs-edit-mode-lighter-pair '( "*" . "*" )
  "Decorate mode line lighter in edit mode."
  :group 'auto-highlight-symbol
  :type '(choice (cons :tag "Asterisk"    (string "*") (string "*"))
                 (cons :tag "Exclamation" (string "!") (string "!"))
                 (cons :tag "DANGEROUS"   (string "DANGER->") (string "<-DANGER"))
                 (cons :tag "Silence!!"   (string "") (string ""))))

(defcustom ahs-select-invisible 'immediate
  "Behavior when selected symbol in hidden text.

When the value is
  `immediate' Open hidden text. When leaving opened text, close it immediately.
  `temporary' Open hidden text. When unhighlight or change plugin, close the opened texts except selected.
  `open'      Open hidden text permanently.
  `skip'      Select next visible symbol.

Affects only overlay(hidden text) has a property `isearch-open-invisible'."

  :group 'auto-highlight-symbol
  :type '(choice (const :tag "Open hidden text only when necessary" immediate)
                 (const :tag "Open hidden text temporary"           temporary)
                 (const :tag "Open hidden text permanently"         open)
                 (const :tag "Skip over all symbols in hidden text" skip)))

(defcustom auto-highlight-symbol-mode-hook nil
  "Hook for `auto-highlight-symbol-mode'."
  :group 'auto-highlight-symbol
  :type 'hook)

(defcustom ahs-edit-mode-on-hook nil
  "Normal hook for run when entering edit mode."
  :group 'auto-highlight-symbol
  :type 'hook)

(defcustom ahs-edit-mode-off-hook nil
  "Normal hook for run when go out edit mode."
  :group 'auto-highlight-symbol
  :type 'hook)

(defvar ahs-idle-timer nil
  "Timer used to highlighting symbol whenever emacs is idle.")

(defcustom ahs-idle-interval 1.0
  "Number of seconds to wait before highlighting symbol."
  :group 'auto-highlight-symbol
  :type 'float
  :set (lambda (symbol value)
         (set-default symbol value)
         (when (timerp ahs-idle-timer)
           (cancel-timer ahs-idle-timer)
           (setq ahs-idle-timer nil)
           (ahs-start-timer))))

;;
;; (@* "Face" )
;;
(defface ahs-face
  '((t (:foreground "GhostWhite" :background "LightYellow4")))
  "Highlight the symbol using this face."
  :group 'auto-highlight-symbol)
(defvar ahs-face 'ahs-face)

(defface ahs-definition-face
  '((t (:foreground "moccasin" :background "CadetBlue" :underline t)))
  "Highlight the symbol definition using this face."
  :group 'auto-highlight-symbol)
(defvar ahs-definition-face 'ahs-definition-face)

(defface ahs-warning-face
  '((t (:foreground "Red" :bold t)))
  "Face for warning message."
  :group 'auto-highlight-symbol)
(defvar ahs-warning-face 'ahs-warning-face)

(defface ahs-plugin-defalt-face
  '((t (:foreground "Black" :background "Orange1")))
  "Face used in `display' plugin."
  :group 'auto-highlight-symbol)
(defvar ahs-plugin-defalt-face 'ahs-plugin-defalt-face)

(defface ahs-plugin-whole-buffer-face
  '((t (:foreground "Black" :background "GreenYellow")))
  "Face used in `whole buffer' plugin."
  :group 'auto-highlight-symbol)
(defvar ahs-plugin-whole-buffer-face 'ahs-plugin-whole-buffer-face)

(defface ahs-plugin-bod-face
  '((t (:foreground "Black" :background "DodgerBlue")))
  "Face used in `beginning of defun' plugin."
  :group 'auto-highlight-symbol)
(defvar ahs-plugin-bod-face 'ahs-plugin-bod-face)

(defface ahs-edit-mode-face
  '((t (:foreground "White" :background "Coral3")))
  "Face used in edit mode."
  :group 'auto-highlight-symbol)
(defvar ahs-edit-mode-face 'ahs-edit-mode-face)

;;
;; (@* "Highlight Rules" )
;;
(defcustom ahs-case-fold-search t
  "*Non-nil means symbol search ignores case."
  :group 'auto-highlight-symbol
  :type 'boolean)

(defconst ahs-default-symbol-regexp "^[0-9A-Za-z/_.,:;*+=&%|$#@!^?-]+$"
  "Default symbol regular expression.")

(defcustom ahs-include ahs-default-symbol-regexp
  "Variable for start highlighting.

This variable can be set in three different types.

  1. `REGEXP' Regular expression.
    If symbol matches regular expression `REGEXP' then start highlighting.

  2. `my-include-function' Function predicate.
    If return value is Non-nil then start highlighting.
    Function is called with one argument, the symbol.

  3. `alist'
      '(
        ( emacs-lisp-mode . \"REGEXP\")          ;; Regular expression in emacs-lisp-mode
        ( php-mode        . my-include-function) ;; Function predicate in php-mode
        )
    If major mode not in list `ahs-default-symbol-regexp' will be used instead."

  :group 'auto-highlight-symbol
  :type '(choice (regexp :tag "Regexp" ahs-default-symbol-regexp)
                 (symbol :tag "Function" function)
                 (alist  :tag "alist")))

(defcustom ahs-exclude nil
  "Variable for inhibit highlighting.

This variable can be set in three different types.

  1. `REGEXP' Regular expression.
    If symbol matches regular expression `REGEXP' then inhibit highlighting.

  2. `my-exclude-function' Function predicate.
    If return value is Non-nil then inhibit highlighting.
    Function is called with one argument, the symbol.

  3. `alist'
      '(
        ( ruby-mode . \"\\_<\\(end\\|def\\|class\\)\\_>\") ;; Regular expression in ruby-mode
        ( dos-mode  . i-hate-wxxxxxs)                      ;; Function predicate in dos-mode
        )
      If major mode not in list all symbols can be highlighted."

  :group 'auto-highlight-symbol
  :type '(choice (const  :tag "All symbols can be highlighted" nil)
                 (regexp :tag "Regexp" "")
                 (symbol :tag "Function" function)
                 (alist  :tag "alist")))

(defcustom ahs-face-check-include-overlay nil
  "*Non-nil means face checks include overlay face."
  :group 'auto-highlight-symbol
  :type 'boolean)

(defcustom ahs-inhibit-face-list
  '( font-lock-comment-delimiter-face
     font-lock-comment-face
     font-lock-doc-face
     font-lock-doc-string-face
     font-lock-string-face )
  "Face list for inhibit highlighting."
  :group 'auto-highlight-symbol
  :type '(repeat symbol))

(defcustom ahs-definition-face-list
  '( font-lock-function-name-face
     font-lock-variable-name-face )
  "Face list for higilight definition."
  :group 'auto-highlight-symbol
  :type  '(repeat symbol))

;;
;; (@* "Mode map" )
;;
(defvar auto-highlight-symbol-mode-map nil
  "Keymap used in `auto-highlight-symbol-mode'.")

(if auto-highlight-symbol-mode-map
    nil
  (setq auto-highlight-symbol-mode-map
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "M-<left>"    ) 'ahs-backward            )
          (define-key map (kbd "M-<right>"   ) 'ahs-forward             )
          (define-key map (kbd "M-S-<left>"  ) 'ahs-backward-definition )
          (define-key map (kbd "M-S-<right>" ) 'ahs-forward-definition  )
          (define-key map (kbd "M--"         ) 'ahs-back-to-start       )
          (define-key map (kbd "C-x C-'"     ) 'ahs-change-range        )
          (define-key map (kbd "C-x C-a"     ) 'ahs-edit-mode           )
          map)))

(defmacro ahs-onekey-edit (keys plugin-name &optional keep keymap)
  "Macro of One Key Edit.

ahs-change-range   -> ahs-edit-mode -> editing... ->
ahs-edit-mode(off) -> ahs-change-range... sigh...

You can do these operations at One Key!

   `KEYS'        Keyboard macro
   `PLUGIN-NAME' Plugin name
   `KEEP'        Keep plugin after exiting edit mode.
   `KEYMAP'      Keymap If value is `nil' or not keymap
                        `auto-highlight-symbol-mode-map' will be used instead.

  ex.(ahs-onekey-edit \"C-x C-y\" beginning-of-defun)"

  `(define-key (if (keymapp ,keymap)
                   ,keymap
                 auto-highlight-symbol-mode-map)
     (read-kbd-macro ,keys)
     (lambda()
       (interactive)
       (ahs-onekey-edit-function ',plugin-name ,keep))))

(defmacro ahs-onekey-change (keys plugin-name &optional keymap)
  "Macro of change plugin.

    `KEYS'        Keyboard macro
    `PLUGIN-NAME' Plugin name
    `KEYMAP'      Keymap If value is `nil' or not keymap
                         `auto-highlight-symbol-mode-map' will be used instead.

  ex.(ahs-onekey-change \"C-x C-y\" display)"

  `(define-key (if (keymapp ,keymap)
                   ,keymap
                 auto-highlight-symbol-mode-map)
     (read-kbd-macro ,keys)
     ',(intern (format "ahs-chrange-%s" plugin-name))))

;;
;; (@* "Internal variable" )
;;
(defvar auto-highlight-symbol-mode nil
  "Dummy for suppress bytecompiler warning.")

(defvar ahs-inhibit-modification-commands
  '( undo
     redo ))

(defvar ahs-unhighlight-allowed-commands
  '( universal-argument
     universal-argument-other-key
     ahs-back-to-start
     ahs-backward
     ahs-backward-definition
     ahs-display-stat
     ahs-edit-mode
     ahs-forward
     ahs-forward-definition ))

(defvar ahs-range-plugin-list nil
  "List of installed plugin.")

(defvar ahs-search-work  nil)
(defvar ahs-need-fontify nil)

;; Buffer local variable
(defvar ahs-current-overlay      nil)
(defvar ahs-current-range        nil)
(defvar ahs-edit-mode-enable     nil)
(defvar ahs-highlighted          nil)
(defvar ahs-inhibit-modification nil)
(defvar ahs-mode-line            nil)
(defvar ahs-onekey-range-store   nil)
(defvar ahs-opened-overlay-list  nil)
(defvar ahs-overlay-list         nil)
(defvar ahs-start-modification   nil)
(defvar ahs-start-point          nil)

(make-variable-buffer-local 'ahs-current-overlay      )
(make-variable-buffer-local 'ahs-current-range        )
(make-variable-buffer-local 'ahs-edit-mode-enable     )
(make-variable-buffer-local 'ahs-highlighted          )
(make-variable-buffer-local 'ahs-inhibit-modification )
(make-variable-buffer-local 'ahs-mode-line            )
(make-variable-buffer-local 'ahs-onekey-range-store   )
(make-variable-buffer-local 'ahs-opened-overlay-list  )
(make-variable-buffer-local 'ahs-overlay-list         )
(make-variable-buffer-local 'ahs-start-modification   )
(make-variable-buffer-local 'ahs-start-point          )

;;
;; (@* "Logging" )
;;
(defconst ahs-log-data
  '(;; plugin
    ( plugin-badcondition . "Plugin `%s' incorrect major-mode or condition property is `nil'.")
    ( plugin-changed      . "Current plugin has been changed to `%s'.")
    ( plugin-notfound     . "Plugin `%s' doesn't exist.")
    ( plugin-notplugin    . "Plugin `%s' wrong type plugin.")

    ( plugin-error-log1 . "---- auto-highlight-symbol-mode plugin error log ----")
    ( plugin-error-log2 . "%s in `%s' plugin `%s' property")
    ( plugin-error-log3 . "---- end")
    ( plugin-error-log4 . "Plugin error occurred. see *Messages*. Current plugin has been changed to `%s'.")

    ;; error
    ( error-ahs-disable . "`auto-highlight-symbol-mode' is not working at current buffer.")
    ( error-read-only   . "Buffer is read-only: `%s'")
    ( error-scan-sexp   . "%s: \"%s\" %s %s")

    ;; edit-mode
    ( turn-on-edit-mode         . "Entering edit mode. %s")
    ( turn-off-edit-mode        . "Exited edit mode.")
    ( onekey-turn-on-edit-mode  . "Entering edit mode. Current plugin has been changed to `%s'. %s")
    ( onekey-turn-off-edit-mode . "Exited edit mode. Current plugin has been changed to `%s'.")
    ( onekey-no-symbol-at-point . "No symbol to highlight at point. Current plugin is `%s' now.")

    ;; misc
    ( no-symbol-at-point . "No symbol to highlight at point.")
    ( exist-elsewhere    . "%s symbols exist elsewhere.")
    ( stat               . "Current plugin `%s' matched %s  displayed %s  hidden %s  before %s  after %s.")
    ( self               . "%s")
    )
  "Log data")

(defmacro ahs-decorate-if (body face)
  `(if ahs-decorate-log
       (propertize ,body 'face ,face)
     ,body))

(defmacro ahs-log-format (key)
  `(cdr (assoc ,key ahs-log-data)))

(defun ahs-log (key &rest args)
  "Display log."
  (unless ahs-suppress-log
    (let* ((data (ahs-log-format key))
           (msg (apply 'format data args))
           (message-log-max
            (not ahs-log-echo-area-only)))
      (message "%s" msg))) nil)

;;
;; (@* "Range plugin" )
;;
(defmacro ahs-regist-range-plugin (plugin-name body &optional docstring)
  "Macro of regist range plugin.

\(fn PLUGIN-NAME BODY [DOCSTRING])"

  (declare (indent 1))
  `(progn
     (defvar ,(intern (format "ahs-range-%s" plugin-name))
       nil ,docstring)
     (setq ,(intern (format "ahs-range-%s" plugin-name)) ,body)
     (add-to-list 'ahs-range-plugin-list ',(intern (format "ahs-range-%s" plugin-name)))
     (defun ,(intern (format "ahs-chrange-%s" plugin-name)) ()
       (interactive)
       (ahs-change-range ',(intern (format "ahs-range-%s" plugin-name)))
       (when (ahs-called-interactively-p 'interactive)
         (ahs-idle-function)))))

(defun ahs-decorated-current-plugin-name ()
  "Return decorated current plugin's name."
  (let ((name (ahs-current-plugin-prop 'name)))
    (if ahs-decorate-log
        (propertize name 'face (ahs-current-plugin-prop 'face))
      name)))

(defun ahs-plugin-error-message (err prop range)
  "Display plugin error message."
  (let ((ahs-suppress-log)
        (ahs-log-echo-area-only))
    (ahs-log 'plugin-error-log1)
    (ahs-log 'plugin-error-log2
             err (ahs-get-plugin-prop 'name range) prop) ;; infinite loop? if 'name is badly function
    (ahs-log 'plugin-error-log3)

    (ahs-change-range-internal ahs-default-range)
    (ahs-log 'plugin-error-log4
              (ahs-decorated-current-plugin-name))))

(defun ahs-get-plugin-prop (prop range &optional arg)
  "Return value of the `PROP' property of the `RANGE' plugin."
  (let ((value (cdr (assoc prop (symbol-value range)))))
    (cond
     ((equal value 'abort) 'abort)          ;; abort
     ((equal prop 'face)                    ;; face
      (if (facep value)
          value
        ahs-plugin-defalt-face))

     ((and (functionp value)
           (equal prop 'major-mode)) value) ;; major-mode
     ((functionp value)                     ;; function
      (condition-case err
          (if arg
              (funcall value arg)
            (funcall value))
        (error err
               (ahs-plugin-error-message err prop range)
               'abort)))

     ((null value) 'none)                   ;; property not found

     ((symbolp value)                       ;; symbol
      (ignore-errors
        (symbol-value value)))
     (t value))))                           ;; others

(defun ahs-current-plugin-prop (prop &optional arg)
  "Return value of the `PROP' property of the current plugin."
  (ahs-get-plugin-prop prop 'ahs-current-range arg))

(defun ahs-valid-plugin-p (range &optional plugin-name)
  "Return Non-nil if `RANGE' plugin can run."
  (setq plugin-name
        (or plugin-name
            (let ((name (format "%s" range)))
              (if (string-match "ahs-range-" name)
                  (substring name (match-end 0) (length name))
                name))))
  (cond
   ((not (boundp range))
    (ahs-log 'plugin-notfound plugin-name))
   ((not (memq range ahs-range-plugin-list))
    (ahs-log 'plugin-notplugin plugin-name))
   ((not (memq range (ahs-runnable-plugins)))
    (ahs-log 'plugin-badcondition
             (ahs-get-plugin-prop 'name range)))
   (t t)))

(defun ahs-runnable-plugins (&optional getnext)
  "Return list of runnable plugins."
  (loop with current   = nil
        with available = nil

        for range  in ahs-range-plugin-list
        for plugin = (symbol-value range)
        for mode   = (ahs-get-plugin-prop 'major-mode range)

        when (equal plugin ahs-current-range) do (setq current range)

        when (or (equal 'none mode)
                 (and (listp mode)
                      (memq major-mode mode))
                 (eq major-mode mode))
        when (ahs-get-plugin-prop 'condition range)
        collect range into available

        finally
        return (if getnext
                   (or (cadr (memq current available))
                       (car available))
                 available)))

(defun ahs-change-range-internal (range)
  "Current plugin change to `RANGE' plugin."
  (setq ahs-current-range (symbol-value range))
  (ahs-current-plugin-prop 'init))

;;
;; (@* "Built-in plugin" )
;;
(ahs-regist-range-plugin
 display
 '((name    . "display area")
   (lighter . "HS")
   (face    . ahs-plugin-defalt-face)
   (start   . window-start)
   (end     . window-end))
  "Display area")

(ahs-regist-range-plugin
 whole-buffer
 '((name    . "whole buffer")
   (lighter . "HSA")
   (face    . ahs-plugin-whole-buffer-face)
   (start   . point-min)
   (end     . point-max))
 "Whole buffer")

;; beginning-of-defun
(defvar ahs-plugin-bod-start nil)
(defvar ahs-plugin-bod-end   nil)

(defcustom ahs-plugin-bod-modes
  '( emacs-lisp-mode lisp-interaction-mode c++-mode c-mode )
  "Major modes `beginning of defun' plugin can run on."
  :group 'auto-highlight-symbol
  :type '(repeat symbol))

(defcustom ahs-plugin-bod-function 'ahs-plugin-ahs-bod
  "Function used in `beginning of defun' plugin."
  :group 'auto-highlight-symbol
  :type '(choice
          (symbol :tag "Use built-in function"        ahs-plugin-ahs-bod)
          (symbol :tag "Use original narrow-to-defun" ahs-plugin-orignal-n2d)))

(defmacro ahs-plugin-bod-error (err)
  `(if (= 4 (length ,err))
      (apply 'ahs-log 'error-scan-sexp ,err)
    (ahs-log 'self ,err)))

(defun ahs-plugin-orignal-n2d ()
  "Original narrow-to-defun."
  (save-restriction
    (condition-case err
        (progn
          (narrow-to-defun)
          (cons (point-min) (point-max)))
      (error err (ahs-plugin-bod-error err)))))

(defun ahs-plugin-ahs-bod ()
  "Another narrow-to-defun."
  (condition-case err
      (let ((opoint (point))
            beg end)
        ;; Point in function
        (beginning-of-defun)
        (setq beg (point))
        (end-of-defun)
        (setq end (point))
        (cond
         ;; Between point-min and function
         ((equal beg (point-min))
          (goto-char opoint)
          (beginning-of-defun -1)
          (if (and (>= opoint beg)
                   (<  opoint end))
              ;; From point-min to first function
              (when (> end (point))
                (setq end (point)))
            ;; Outside function
            (setq beg end
                  end (point))))
         ;; Between function and function
         ((>= opoint end)
          (setq beg end)
          (beginning-of-defun -1)
          (setq end (point))))
        (cons beg end))
    (error err (ahs-plugin-bod-error err))))

(ahs-regist-range-plugin
 beginning-of-defun
 '((name          . "beginning of defun")
   (lighter       . "HSD")
   (face          . ahs-plugin-bod-face)
   (major-mode    . ahs-plugin-bod-modes)
   (before-search . (lambda(symbol)
                      (save-excursion
                        (let ((pos (funcall ahs-plugin-bod-function)))
                          (if (not (consp pos))
                              'abort
                            (setq ahs-plugin-bod-start (car pos))
                            (setq ahs-plugin-bod-end   (cdr pos)))))))
   (start         . ahs-plugin-bod-start)
   (end           . ahs-plugin-bod-end))
 "beginning-of-defun to end-of-defun.")

;;
;; (@* "Timer" )
;;
(defun ahs-start-timer ()
  "Start idle timer."
  (unless ahs-idle-timer
    (setq ahs-idle-timer
          (run-with-idle-timer ahs-idle-interval t 'ahs-idle-function))))

(defun ahs-restart-timer ()
  "Restart idle timer."
  (when (timerp ahs-idle-timer)
    (cancel-timer ahs-idle-timer)
    (setq ahs-idle-timer nil)
    (ahs-start-timer)))

;;
;; (@* "Idle" )
;;
(defun ahs-idle-function ()
  "Idle function. Called by `ahs-idle-timer'."
  (when (and auto-highlight-symbol-mode
             (not ahs-highlighted))
    (let ((hl (ahs-highlight-p)))
      (when hl
        (ahs-highlight (nth 0 hl)
                       (nth 1 hl)
                       (nth 2 hl))))))

(defmacro ahs-add-overlay-face (pos face)
  `(if ahs-face-check-include-overlay
       (append (ahs-get-overlay-face ,pos)
               (if (listp ,face)
                   ,face
                 (list ,face))) ,face))

(defun ahs-highlight-p ()
  "Ruturn Non-nil if symbols can be highlighted."
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (beg (car bounds))
         (end (cdr bounds))
         (face (when bounds
                 (get-text-property beg 'face)))
         (symbol (when bounds
                   (buffer-substring beg end))))
    (when (and symbol
               (not (ahs-dropdown-list-p))
               (not (ahs-face-p (ahs-add-overlay-face beg face) 'ahs-inhibit-face-list))
               (not (ahs-symbol-p ahs-exclude symbol t))
               (ahs-symbol-p ahs-include symbol))
      (list symbol beg end))))

(defun ahs-symbol-p (pred symbol &optional nodefs)
  "Return Non-nil if `SYMBOL' matches `PRED'."
  (cond
   ;; Default include/no exclude
   ((null pred)
    (unless nodefs
      (let ((case-fold-search ahs-case-fold-search))
        (string-match ahs-default-symbol-regexp symbol))))

   ;; REGEXP
   ((stringp pred)
    (let ((case-fold-search ahs-case-fold-search))
      (string-match pred symbol)))

   ;; Major mode
   ((listp pred)
    (let ((new-pred (cdr (assoc major-mode pred))))
      (ahs-symbol-p new-pred symbol nodefs)))

   ;; Function predicate
   ((functionp pred)
    (funcall pred symbol))))

(defun ahs-dropdown-list-p ()
  "Return Non-nil if dropdown-list is expanded."
  (and (featurep 'dropdown-list)
       dropdown-list-overlays))

(defun ahs-face-p (face faces)
  "Return Non-nil if `FACE' in `FACES'."
  (let ((facelist (symbol-value faces)))
    (if (listp face)
        (loop for x in face
              when (memq x facelist)
              return x)
      (memq face facelist))))

(defun ahs-get-overlay-face (pos)
  "Return list of all overlays face at `POS'."
  (loop for overlay in (overlays-at pos)
        for face = (overlay-get overlay 'face)
        when face
        when (symbolp face)
        collect face))

;;
;; (@* "Highlight" )
;;
(defun ahs-prepare-highlight (symbol)
  "Prepare for highlight."
  (let ((before (ahs-current-plugin-prop 'before-search symbol))
        (beg (ahs-current-plugin-prop 'start))
        (end (ahs-current-plugin-prop 'end)))
    (cond ((equal before 'abort) nil)
          ((not (numberp beg)) nil)
          ((not (numberp end)) nil)
          ((> beg end) nil)
          (t (cons beg end)))))

(defun ahs-search-symbol (symbol search-range)
  "Search `SYMBOL' in `SEARCH-RANGE'."
  (save-excursion
    (let ((case-fold-search ahs-case-fold-search)
          (regexp (concat "\\_<\\(" (regexp-quote symbol) "\\)\\_>" ))
          (beg (car search-range))
          (end (cdr search-range)))
      (goto-char end)
      (while (re-search-backward regexp beg t)
        (let* ((symbol-beg (match-beginning 1))
               (symbol-end (match-end 1))
               (tprop (text-properties-at symbol-beg))
               (face (cadr (memq 'face tprop)))
               (fontified (cadr (memq 'fontified tprop))))
          (unless (or face fontified)
            (setq ahs-need-fontify t))
          (push (list symbol-beg
                      symbol-end
                      face fontified) ahs-search-work))))))

(defun ahs-fontify ()
  "Fontify symbols for strict check."
  ;;;;
  ;;
  ;; (@* "Note" )
  ;;
  ;;  If symbol has no text properties, will be called `jit-lock-fontify-now'
  ;; to strict check.
  ;;
  ;; Some old PCs performance may be degraded when:
  ;;  * Editing large file.
  ;;  * So many matched symbols exists outside the display area.
  ;;
  ;; Tested on my old pentium4 pc (bought in 2002 xD)
  ;;  So dirty `font-lock-keywords' and use `whole buffer' plugin.
  ;; Result:
  ;;  +---------------+-----------+----------------+----------+
  ;;  | filename      | filesize  | matched symbol | result   |
  ;;  +---------------+-----------+----------------+----------+
  ;;  | `loaddefs.el' | 1,207,715 | `autoload'     | so slow  |
  ;;  | `org.el'      |   753,991 | `if'           | slow     |
  ;;  +---------------+-----------+----------------+----------+
  ;;
  ;; If you feel slow, please use `display area' plugin instead of `whole buffer' plugin.
  ;; And use `ahs-onekey-edit' to use `whole buffer' plugin.
  ;;
  (loop with beg = nil
        with end = nil

        for symbol in ahs-search-work
        for fontified = (or (nth 2 symbol)
                            (nth 3 symbol))

        unless (or beg fontified) do (setq beg (nth 0 symbol))
        unless fontified          do (setq end (nth 1 symbol))

        when (and beg end fontified)
        do (progn
             (jit-lock-fontify-now beg end)
             (setq beg nil
                   end nil))

        finally
        do (when (and beg end)
             (jit-lock-fontify-now beg end))))

(defun ahs-light-up ()
  "Light up symbols."
  (loop for symbol in ahs-search-work

        for beg  = (nth 0 symbol)
        for end  = (nth 1 symbol)
        for face = (or (nth 2 symbol)
                       (get-text-property beg 'face))
        for face = (ahs-add-overlay-face beg face)

        unless (ahs-face-p face 'ahs-inhibit-face-list)
        do (let ((overlay (make-overlay beg end nil nil t)))
             (overlay-put overlay 'ahs-symbol t)
             (overlay-put overlay 'face
                          (if (ahs-face-p face 'ahs-definition-face-list)
                              ahs-definition-face
                            ahs-face))
             (push overlay ahs-overlay-list))))

(defun ahs-highlight (symbol beg end)
  "Highlight"
  (setq ahs-search-work  nil
        ahs-need-fontify nil)
  (let ((search-range (ahs-prepare-highlight symbol)))
    (when (consp search-range)
      ;;(msell-bench
       (ahs-search-symbol symbol search-range)
       (when ahs-need-fontify
         (ahs-fontify))
       (ahs-light-up)
      ;;)
      (when ahs-overlay-list
        (ahs-highlight-current-symbol beg end)
        (setq ahs-highlighted  t
              ahs-start-point  beg
              ahs-search-work  nil
              ahs-need-fontify nil)
        (add-hook 'pre-command-hook 'ahs-unhighlight nil t) t))))

(defun ahs-unhighlight (&optional force)
  "Unhighlight"
  (when (or force
            (not (memq this-command
                       ahs-unhighlight-allowed-commands)))
    (ahs-remove-all-overlay)
    (remove-hook 'pre-command-hook 'ahs-unhighlight t)))

(defun ahs-highlight-current-symbol (beg end)
  "Highlight current symbol."
  (let* ((overlay  (make-overlay beg end nil nil t)))

    (overlay-put overlay 'ahs-symbol 'current)
    (overlay-put overlay 'priority 1000)
    (overlay-put overlay 'face (ahs-current-plugin-prop 'face))
    (overlay-put overlay 'help-echo '(ahs-stat-string))

    (overlay-put overlay 'modification-hooks    '(ahs-modification-hook))
    (overlay-put overlay 'insert-in-front-hooks '(ahs-modification-hook))
    (overlay-put overlay 'insert-behind-hooks   '(ahs-modification-hook))

    (setq ahs-current-overlay overlay)))

(defun ahs-remove-all-overlay ()
  "Remove all overlays."
  (delete-overlay ahs-current-overlay)
  (mapc 'delete-overlay ahs-overlay-list)
  (mapc 'ahs-open-necessary-overlay ahs-opened-overlay-list)
  (setq ahs-current-overlay     nil
        ahs-highlighted         nil
        ahs-opened-overlay-list nil
        ahs-overlay-list        nil
        ahs-start-point         nil))

;;
;; (@* "Edit mode" )
;;
(defun ahs-modification-hook (overlay after debut fin &optional length)
  "Overlay's `modification-hook' used in edit mode."
  (when ahs-edit-mode-enable
    (if (not after)
        (setq ahs-inhibit-modification
              (memq this-command
                    ahs-inhibit-modification-commands))
      (setq ahs-start-modification t))))

(defun ahs-edit-post-command-hook-function ()
  "`post-command-hook' used in edit mode."
  (cond
   ;; Exit edit mode
   ((not (ahs-inside-overlay-p ahs-current-overlay))
    (ahs-edit-mode-off nil nil))

   ;; Modify!!
   ((and ahs-start-modification
         (not ahs-inhibit-modification))
    (ahs-symbol-modification)))

  (setq ahs-start-modification   nil
        ahs-inhibit-modification nil))

(defun ahs-symbol-modification ()
  "Modify all highlighted symbols."
  (let ((source (buffer-substring-no-properties
                 (overlay-start ahs-current-overlay)
                 (overlay-end ahs-current-overlay))))
    (dolist (change ahs-overlay-list)
      (when (overlayp change)
        (let* ((beg (overlay-start change))
               (end (overlay-end change))
               (len (- end beg))
               (target (buffer-substring-no-properties beg end)))
          (unless (string= source target)
            (save-excursion
              (goto-char beg)
              (insert source)
              (delete-region (point) (+ len (point))))))))))

(defun ahs-edit-mode-on ()
  "Turn `ON' edit mode."
  (setq ahs-edit-mode-enable     t
        ahs-start-modification   nil
        ahs-inhibit-modification nil)
  (overlay-put ahs-current-overlay 'face ahs-edit-mode-face)
  (remove-hook 'pre-command-hook 'ahs-unhighlight t)
  (add-hook 'post-command-hook 'ahs-edit-post-command-hook-function nil t)
  (run-hooks 'ahs-edit-mode-on-hook)

  ;; Exit edit mode when undo over edit mode.
  (push '(apply ahs-clear t) buffer-undo-list)

  ;; Display log
  (unless ahs-suppress-log
    (let* ((st (ahs-stat))
           (alert
            (if (ahs-stat-alert-p st)
                (format (ahs-log-format 'exist-elsewhere)
                        (ahs-decorate-if
                         (number-to-string
                          (+ (nth 2 st)
                             (nth 3 st))) ahs-warning-face)) "")))
      (if ahs-onekey-range-store
          (ahs-log 'onekey-turn-on-edit-mode
                   (ahs-decorated-current-plugin-name) alert)
        (ahs-log 'turn-on-edit-mode alert))))

  (ahs-set-lighter))

(defun ahs-edit-mode-off (nomsg interactive)
  "Turn `OFF' edit mode."
  (setq ahs-edit-mode-enable nil)
  (if (and interactive
           (not ahs-onekey-range-store)
           (ahs-inside-overlay-p ahs-current-overlay))
      (progn
        (overlay-put ahs-current-overlay 'face (ahs-current-plugin-prop 'face))
        (add-hook 'pre-command-hook 'ahs-unhighlight nil t))
    (ahs-remove-all-overlay))
  (remove-hook 'post-command-hook 'ahs-edit-post-command-hook-function t)
  (run-hooks 'ahs-edit-mode-off-hook)

  ;; Display log
  (let ((ahs-suppress-log
         (or nomsg
             ahs-suppress-log)))
    (if (not ahs-onekey-range-store)
        (ahs-log 'turn-off-edit-mode)
      ;; Restore plugin
      (ahs-change-range-internal 'ahs-onekey-range-store)
      (ahs-log 'onekey-turn-off-edit-mode
               (ahs-decorated-current-plugin-name))
      (setq ahs-onekey-range-store nil)
      (when interactive
        (ahs-idle-function))))

  (ahs-set-lighter))

(defun ahs-edit-mode-condition-p ()
  "Return Non-nil if edit mode can turn on."
  (cond
   ((not auto-highlight-symbol-mode)
    (ahs-log 'error-ahs-disable))
   (buffer-read-only
    (ahs-log 'error-read-only (buffer-name)))
   (t t)))

(defun ahs-onekey-edit-function (plugin-name keep)
  "One Key Edit internal function."
  (let ((range (intern-soft (format "ahs-range-%s" plugin-name))))
    (cond
     ((not (ahs-edit-mode-condition-p)) nil)
     ((not (ahs-valid-plugin-p range plugin-name)) nil)

     ;; Entering edit mode.
     ((and (not ahs-onekey-range-store)
           (equal ahs-current-range (symbol-value range)))
      ;; No change.
      (ahs-clear t)
      (if (ahs-idle-function)
          (ahs-edit-mode-on)
        (ahs-log 'no-symbol-at-point)))

     (t
      ;; Change plugin temporary.
      (ahs-clear t)
      (setq ahs-onekey-range-store ahs-current-range)
      (ahs-change-range-internal range)
      (if (ahs-idle-function)
          (progn
            (ahs-edit-mode-on)
            (when keep
              (setq ahs-onekey-range-store nil)))
        ;; No symbol at point. Restore plugin.
        (ahs-change-range-internal 'ahs-onekey-range-store)
        (setq ahs-onekey-range-store nil)
        (ahs-log 'onekey-no-symbol-at-point
                 (ahs-decorated-current-plugin-name))
        (ahs-set-lighter))))))

;;
;; (@* "Select" )
;;
(defun ahs-select (pred &optional reverse onlydef)
  "Select highlighted symbol."
  (when ahs-highlighted
    (let* ((next (loop with start = nil
                       for overlay in (if reverse
                                          (reverse ahs-overlay-list)
                                        ahs-overlay-list)

                       for skip = (loop for hidden in (overlays-at (overlay-start overlay))
                                        when (overlay-get hidden 'invisible)
                                        when (or (equal ahs-select-invisible 'skip)
                                                 (not (overlay-get hidden 'isearch-open-invisible)))
                                        return hidden)

                       for selectable = (and (not skip)
                                             (or (not onlydef)
                                                 (ahs-definition-p overlay)))

                       when selectable
                       unless start do (setq start overlay)

                       when selectable
                       when (funcall pred overlay) return overlay

                       finally
                       return (or start
                                  ahs-current-overlay)))

           (beg (overlay-start next))
           (end (overlay-end next)))

      (dolist (overlay
               (unless (equal ahs-select-invisible 'skip)
                 (ahs-get-openable-overlays next)))
        (ahs-open-invisible-overlay-temporary overlay))

      (goto-char (+ beg (- (point) (overlay-start ahs-current-overlay))))
      (move-overlay ahs-current-overlay beg end))

    (when (equal ahs-select-invisible 'immediate)
      (ahs-close-unnecessary-overlays))))

(defun ahs-get-openable-overlays (overlay)
  "Return list of openable overlays."
  (loop for openable in (overlays-at (overlay-start overlay))
        when (overlay-get openable 'invisible)
        when (overlay-get openable 'isearch-open-invisible)
        collect openable))

;; Modified from isearch.el
(defun ahs-close-unnecessary-overlays ()
  "Close unnecessary overlays."
  (let ((overlays ahs-opened-overlay-list)
        (newlist))
    (setq ahs-opened-overlay-list nil)
    (dolist (overlay overlays)
      (if (ahs-inside-overlay-p overlay)
          (push overlay newlist)
        (let ((func-temp (overlay-get overlay 'isearch-open-invisible-temporary)))
          (if func-temp
              (funcall func-temp overlay t)
            (ahs-store-property  overlay 'isearch-invisible  'invisible)
            (ahs-store-property  overlay 'isearch-intangible 'intangible)))))
    (setq ahs-opened-overlay-list newlist)))

;; Modified from isearch.el
(defun ahs-open-necessary-overlay (overlay)
  "Open the `OVERLAY' if it is necessary. Otherwise close."
  (when (overlayp overlay)
    (let ((inside-overlay (ahs-inside-overlay-p overlay))
          (func-temp (overlay-get overlay 'isearch-open-invisible-temporary))
          (func      (overlay-get overlay 'isearch-open-invisible)))
      (when (or inside-overlay (not func-temp))
        (ahs-store-property overlay 'isearch-invisible  'invisible)
        (ahs-store-property overlay 'isearch-intangible 'intangible))
      (if (or inside-overlay
              (equal ahs-select-invisible 'open))
          (when func (funcall func overlay))
        (when func-temp (funcall func-temp overlay t))))))

;; Modified from isearch.el
(defun ahs-open-invisible-overlay-temporary (overlay)
  "Open the `OVERLAY' temporary."
  (let ((func (overlay-get overlay 'isearch-open-invisible-temporary)))
    (if func
        (funcall func overlay nil)
      (ahs-store-property overlay 'invisible  'isearch-invisible)
      (ahs-store-property overlay 'intangible 'isearch-intangible)) ;; intangible need?
    (push overlay ahs-opened-overlay-list)))

(defun ahs-store-property (overlay from to)
  "Store `OVERLAY' property."
  (overlay-put overlay to (overlay-get overlay from))
  (overlay-put overlay from nil))

;; No doc xD
(defun ahs-forward-p        (x) (< (overlay-start ahs-current-overlay) (overlay-start x)))
(defun ahs-backward-p       (x) (> (overlay-start ahs-current-overlay) (overlay-start x)))
(defun ahs-definition-p     (x) (eq (overlay-get x 'face) 'ahs-definition-face))
(defun ahs-start-point-p    (x) (equal (overlay-start x) ahs-start-point))
(defun ahs-inside-overlay-p (x) (and (>= (point) (overlay-start x)) (<= (point) (overlay-end x))))
(defun ahs-inside-display-p (x) (and (>= (window-end) (overlay-start x)) (<= (window-start) (overlay-start x))))
(defun ahs-hidden-p         (x) (loop for overlay in (overlays-at (overlay-start x))
                                      when (overlay-get overlay 'invisible)
                                      return t))

;;
;; (@* "Misc" )
;;
(defun ahs-stat ()
  "Return list of the current status."
  (append (list (ahs-decorated-current-plugin-name)
                (length ahs-overlay-list))

          (loop with hidden?   = 0
                with before    = 0
                with after     = 0
                with displayed = 0

                for x in ahs-overlay-list

                count (funcall 'ahs-backward-p x) into before
                count (funcall 'ahs-forward-p x)  into after

                count (and (funcall 'ahs-inside-display-p x)
                           (incf hidden?)
                           (not (funcall 'ahs-hidden-p x)))
                into displayed

                finally
                return (list before after displayed (- hidden? displayed)))))

(defun ahs-stat-alert-p (status)
  "Return Non-nil if many symbols are highlighted but displayed one or zero."
  (and (< (nth 4 status) 2)
       (or (> (nth 2 status) 0)
           (> (nth 3 status) 0)
           (> (nth 5 status) 0))))

(defmacro ahs-decorate-number (number)
  `(unless (equal ,number 0)
     (setq ,number
          (ahs-decorate-if
           (number-to-string ,number) ahs-warning-face))))

(defun ahs-stat-string (&optional status)
  "Return the formatted `STATUS'. `STATUS' defaults to the current status."
  (let* ((st (or status
                 (ahs-stat)))
         (before (nth 2 st))
         (after  (nth 3 st))
         (hidden (nth 5 st)))

    (when (ahs-stat-alert-p st)
      (ahs-decorate-number before)
      (ahs-decorate-number after)
      (ahs-decorate-number hidden))

    (format (ahs-log-format 'stat)
            (nth 0 st)
            (nth 1 st)
            (nth 4 st)
            hidden before after)))

(defun ahs-set-lighter ()
  "Set mode line lighter."
  (setq ahs-mode-line
        (concat " "
                (when ahs-edit-mode-enable
                  (car ahs-edit-mode-lighter-pair))
                (ahs-current-plugin-prop 'lighter)
                (when ahs-edit-mode-enable
                  (cdr ahs-edit-mode-lighter-pair))))
  (when auto-highlight-symbol-mode
    (force-mode-line-update)))

(defun ahs-init ()
  "Initialize"
  (unless ahs-current-range
    (ahs-change-range-internal ahs-default-range))
  (ahs-set-lighter)
  (ahs-start-timer))

(defun ahs-clear (&optional verbose)
  "Remove all overlays and exit edit mode."
  (if ahs-edit-mode-enable
      (ahs-edit-mode-off (not verbose) nil)
    (when ahs-highlighted
      (ahs-unhighlight t))))

(defun ahs-mode-maybe ()
  "Fire up `auto-highlight-symbol-mode' if major-mode in ahs-modes."
  (if (and (not (minibufferp (current-buffer)))
           (memq major-mode ahs-modes))
      (auto-highlight-symbol-mode t)))

;;
;; (@* "Interactive" )
;;
(defun ahs-forward ()
  "Select highlighted symbols forwardly."
  (interactive)
  (ahs-select 'ahs-forward-p t))

(defun ahs-backward ()
  "Select highlighted symbols backwardly."
  (interactive)
  (ahs-select 'ahs-backward-p))

(defun ahs-forward-definition ()
  "Select highlighted symbols forwardly. only symbol definition."
  (interactive)
  (ahs-select 'ahs-forward-p t t))

(defun ahs-backward-definition ()
  "Select highlighted symbols backwardly. only symbol definition."
  (interactive)
  (ahs-select 'ahs-backward-p nil t))

(defun ahs-back-to-start ()
  "Go back to the starting point.

Limitation:
  If you change plugin during highlights, starting point will be reset."
  (interactive)
  (ahs-select 'ahs-start-point-p))

(defun ahs-change-range (&optional range nomsg)
  "Current plugin change to `RANGE' plugin. `RANGE' defaults to next runnable plugin."
  (interactive)
  (ahs-clear (not nomsg))

  (when (if range
            (ahs-valid-plugin-p range)
          (setq range (ahs-runnable-plugins t)))
    (ahs-change-range-internal range)
    (let ((ahs-suppress-log nomsg))
      (ahs-log 'plugin-changed
               (ahs-decorated-current-plugin-name))))

  (when (ahs-called-interactively-p 'interactive)
    (ahs-idle-function))
  (ahs-set-lighter))

(defun ahs-set-idle-interval (secs)
  "Set wait until highlighting symbol when emacs is idle."
  (interactive "nSeconds to idle, before highlighting symbol: ")
  (when (and (numberp secs)
             (not (equal secs 0)))
    (setq ahs-idle-interval secs)
    (ahs-restart-timer)))

(defun ahs-display-stat ()
  "Display current status.

Display current plugin name, number of matched symbols and the details.

The details are as follows:
  1. Displayed symbols
  2. Hidden symbols inside the display area
  3. Symbols before the cursor
  4. Symbols after the cursor

That's all."

  (interactive)
  (let ((ahs-suppress-log
         (and (not (ahs-called-interactively-p 'interactive))
              ahs-suppress-log)))
    (ahs-log 'self (ahs-stat-string))))

(defun ahs-highlight-now ()
  "Highlight NOW!!"
  (interactive)
  (ahs-idle-function))

(defun ahs-goto-web ()
  "Go to official? web site."
  (interactive)
  (browse-url ahs-web))

;;
;; (@* "Define mode" )
;;
(defun ahs-edit-mode (arg &optional temporary)
  "Turn on edit mode. With a prefix argument, current plugin change to `whole buffer' temporary."
  (interactive
   (if ahs-edit-mode-enable
       (list nil)
     (list t current-prefix-arg)))

  (when (and arg
             (not temporary))
    (ahs-idle-function))

  (cond
   ((and arg temporary)
    (ahs-onekey-edit-function 'whole-buffer nil))

   ((not (ahs-edit-mode-condition-p)) nil)
   ((not ahs-highlighted)
    (ahs-log 'no-symbol-at-point))
   (arg
    (ahs-edit-mode-on))
   ((not arg)
    (ahs-edit-mode-off nil (ahs-called-interactively-p 'interactive)))))

;;;###autoload
(define-globalized-minor-mode global-auto-highlight-symbol-mode
  auto-highlight-symbol-mode ahs-mode-maybe
  :group 'auto-highlight-symbol)

;;;###autoload
(define-minor-mode auto-highlight-symbol-mode
  "Toggle Auto Highlight Symbol Mode"
  :group 'auto-highlight-symbol
  :lighter ahs-mode-line
  (if auto-highlight-symbol-mode
      (ahs-init)
    (ahs-clear)))

;;
;; (@* "Revert" )
;;
;; Remove all overlays and exit edit mode before revert-buffer
(add-hook 'before-revert-hook 'ahs-clear)

;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'auto-highlight-symbol)

;;; Local Variables:
;;; indent-tabs-mode: nil
;;; End:

;;
;; $Id: auto-highlight-symbol.el,v 243:6aa59061b1df 2010-11-21 14:42 +0900 arch320 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto-highlight-symbol.el ends here
