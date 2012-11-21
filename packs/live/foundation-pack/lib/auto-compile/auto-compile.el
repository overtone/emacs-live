;;; auto-compile.el --- automatically compile Emacs Lisp libraries

;; Copyright (C) 2008-2012  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20080830
;; Version: 1.0.7
;; Status: beta
;; Package-Requires: ((packed "0.3.2"))
;; Homepage: http://tarsius.github.com/auto-compile
;; Keywords: compile, convenience, lisp

;; This is a beta release.  Version numbers are inspired by how
;; Emacs is versioned - 1.1.0 will be the first stable version.

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides the minor mode `auto-compile-on-save-mode' which
;; automatically compiles Emacs Lisp code when the visiting buffers are
;; saved to their source files, provided that the respective byte code
;; files already exists.  If the byte code file does not already exist
;; nothing is done.  Also provided is `auto-compile-on-load-mode' which
;; is described toward the end of this commentary.

;; To start or stop compiling a source file or multiple files at once use
;; the command `toggle-auto-compile' which toggles automatic compilation
;; by either compiling the selected source file(s) or by removing the
;; respective byte code file(s).  The appropriate action is determined by
;; the existence respectively absence of the byte code file.

;; Automatically compiling Emacs Lisp source files after each save is
;; useful for at least the following reasons:

;; * Emacs prefers the byte code file over the source file even if the
;;   former is outdated.  Without a mode which automatically recompiles
;;   the source files you will at least occasionally forget to do so
;;   manually and end up with an old version of your code being loaded.

;; * There are many otherwise fine libraries to be found on the Internet
;;   which when compiled will confront the user with a wall of compile
;;   warnings and an occasional error.  If authors are informed about
;;   these (often trivial) problems after each save they will likely fix
;;   them quite quickly.  That or they have a high noise tolerance.

;; * It's often easier and less annoying to fix errors and warnings as
;;   they are introduced than to do a "let's compile today's work and see
;;   how it goes".

;; So do yourself and others a favor and enable this mode by adding the
;; following to your init file:
;;
;;     (auto-compile-on-save-mode 1)

;; Auto-Compile mode is designed to stay out of your way as much as it
;; can while still motivating you to get things fixed.  But Auto-Compile
;; mode can also be configured to be more insistent, which might be
;; annoying initially but less so once existing problems have been fixed.

;; Occasionally you might be tempted to turn of Auto-Compile mode locally
;; because you are doing some work which causes lots of expected warnings
;; until you are actually done.  Don't do so: because Emacs prefers the
;; byte code file you would also have to remove that, in which case you
;; don't have to turn of this mode anymore.  In other words use the
;; command `toggle-auto-compile' instead.

;; Even when using `auto-compile-mode' it can sometimes happen that the
;; source file is newer than the byte compile destination.  This can for
;; example happen when performing version control operations. To ensure
;; that byte code files are always up-to-date when being loaded using
;; `require' and `load' enable `auto-compile-on-load-mode' which advises
;; this functions to recompile the source files when needed.  Enable this
;; mode before any potentially byte compiled files are loaded by beginning
;; your init file with:
;;
;;     ;; -*- no-byte-compile: t -*-
;;     (add-to-list 'load-path "/path/to/auto-compile")
;;     (require 'auto-compile)
;;     (auto-compile-on-load-mode 1)
;;     (auto-compile-on-save-mode 1)

;; Also note that just because no warnings and/or errors are reported when
;; Auto-Compile mode compiles a source file this does not necessarily mean
;; that users of your libraries won't see any.  A likely cause for this
;; would be that you forgot to require a feature which is loaded on your
;; system but not necessarily on the users' systems.  So you should still
;; manually compile your packages before release:
;;
;;     emacs -batch -Q -L . -L ../dependency/ -f batch-byte-compile *.el

;;; Code:

(eval-when-compile
  (require 'cl)) ; push

(require 'bytecomp)
(require 'packed)

(declare-function autoload-rubric "autoload")
(declare-function autoload-find-destination "autoload")
(declare-function autoload-file-load-name "autoload")

(defgroup auto-compile nil
  "Compile Emacs Lisp source files after the visiting buffers are saved."
  :group 'convenience
  :prefix 'auto-compile
  :link '(function-link toggle-auto-compile)
  :link '(function-link auto-compile-byte-compile)
  :link '(function-link auto-compile-mode))


;;; Auto-Compile Mode.

;;;###autoload
(define-minor-mode auto-compile-mode
  "Compile Emacs Lisp source files after the visiting buffers are saved.

After a buffer containing Emacs Lisp code is saved to its source file
update the respective byte code file.  If the latter does not exist do
nothing.  Therefore to disable automatic compilation remove the byte code
file.  See command `toggle-auto-compile' for a convenient way to do so.

This mode should be enabled globally, using it's globalized variant
`auto-compile-on-save-mode'."
  :lighter auto-compile-mode-lighter
  :group 'auto-compile
  (or (derived-mode-p 'emacs-lisp-mode)
      (error "This mode only makes sense with emacs-lisp-mode"))
  (if auto-compile-mode
      (add-hook  'after-save-hook 'auto-compile-byte-compile nil t)
    (remove-hook 'after-save-hook 'auto-compile-byte-compile t))
  (auto-compile-set-use-mode-line
   'auto-compile-use-mode-line
   (bound-and-true-p auto-compile-use-mode-line)))

;;;###autoload
(define-globalized-minor-mode auto-compile-on-save-mode
  auto-compile-mode turn-on-auto-compile-mode)

(define-obsolete-function-alias 'auto-compile-global-mode
  'auto-compile-on-save-mode)

(defun turn-on-auto-compile-mode ()
  (when (eq major-mode 'emacs-lisp-mode)
    (auto-compile-mode 1)))

(defvar auto-compile-mode-lighter ""
  "Mode lighter for Auto-Compile Mode.")

(defcustom auto-compile-verbose nil
  "Whether to print messages describing progress of byte-compiler."
  :group 'auto-compile
  :type 'boolean)

(defcustom auto-compile-always-recompile t
  "Whether to recompile all source files when turning on auto compilation.

When turning on auto compilation for files in a directory recompile source
files even if their byte code file already exist and are up-to-date.

If you disable this you may alternatively turn off, then turn on again
auto compilation to recompile all files in the directory."
  :group 'auto-compile
  :type 'boolean)

(defcustom auto-compile-recursive "^[^.]"
  "Whether to recurse into subdirectories when toggling auto compilation.

Must be a boolean or a regular expression in which case only directories
whose file-name match are recursed into.  The files in a directory
explicitly selected are always processed."
  :group 'auto-compile
  :type '(choice (const  :tag "All subdirectories" t)
                 (const  :tag "Non-hidden subdirectories" "^[^.]")
                 (string :tag "Matching subdirectories")
                 (const  :tag "Don't" nil)))

(defcustom auto-compile-visit-failed t
  "Whether to visit source files which failed to compile.

If this is non-nil visit but don't select a source file if it isn't being
visited in a buffer already.  Also set the buffer local value of variable
`auto-compile-pretend-byte-compiled' (which see) to t and mark the buffer
as modified if the value of variable `auto-compile-mark-failed-modified'
is non-nil."
  :group 'auto-compile
  :type 'boolean)

(defcustom auto-compile-mark-failed-modified t
  "Whether to mark buffers which failed to compile as modified.

This serves as a reminder to fix fatal errors.  While useful this can
get annoying so this variable can be quickly toggled with the command
`auto-compile-toggle-mark-failed-modified'."
  :group 'auto-compile
  :type 'boolean)

(defcustom auto-compile-ding t
  "Whether to beep (or flash the screen) when an error occurs.

Auto-Compile mode continues after an errors occurs (compile error,
unmatched parens, or failure to remove file) because aborting and
therefor not processing the remaining files would be confusing.  Instead
it continues and beeps or flashes the screen to get the users attention;
set this variable to nil to disable even that."
  :group 'auto-compile
  :type 'boolean)

(defcustom auto-compile-check-parens t
  "Whether to check for unbalanced parentheses before compiling.

This only has as an effect on files which are currently being visited in
a buffer other files are compiled without this prior check.  If unbalanced
parentheses are found no attempt is made to compile the file as that would
obviously fail also."
  :group 'auto-compile
  :type 'boolean)

(defcustom auto-compile-delete-stray-dest nil
  "Whether to remove stray byte-compile destination files.

If this is non-nil byte-compile destinations files are removed if
the respective source file does not exist anymore.  While this
happens automatically in certain situations it does not guarantee
that such files never exist.  When `auto-compile-on-load-mode' is
turned on it *does* guarantee that a stray elc file cannot shadow
a source file that is located in a directory that comes later in
the `load-path'."
  :group 'auto-compile
  :type 'boolean)

(defun auto-compile-set-use-mode-line (symbol value)
  (set-default symbol value)
  (set-default 'mode-line-format
               (delete 'mode-line-auto-compile mode-line-format))
  (when (and value auto-compile-mode)
    (push 'mode-line-auto-compile
          (cdr (member value mode-line-format)))))

(defcustom auto-compile-use-mode-line 'mode-line-modified
  "Whether to show information about the byte compiled file in the mode line.

This works by inserting `mode-line-auto-compile' into the default value of
`mode-line-format' after the construct specified here.  If nil do not insert
`mode-line-auto-compile' at all."
  :group 'auto-compile
  :set 'auto-compile-set-use-mode-line
  :type '(choice (const :tag "don't insert" nil)
                 (const :tag "after mode-line-modified" mode-line-modified)
                 (const :tag "after mode-line-remote" mode-line-remote)
                 (sexp  :tag "after construct")))

;;;###autoload
(defun toggle-auto-compile (file action)
  "Toggle automatic compilation of an Emacs Lisp source file or files.

Read a file or directory name from the minibuffer defaulting to the
visited Emacs Lisp source file or `default-directory' if no such file is
being visited in the current buffer.  If the user exits with a directory
selected then all source files in that directory will have their status
set, otherwise just the selected file.

Toggling happens by either compiling the source files(s) or by removing
the respective byte code file(s).  See `auto-compile-mode'.

The appropriate action is determined by the existence respectively absence
of the byte code file for the selected source file.  If a directory was
selected but a source file was current when this command was invoked
use that file to determine the action.  Otherwise prompt the user.

To explicitly select an action use a positive prefix argument to compile
the source file(s) or a negative prefix argument to remove the respective
byte code file(s).

Note that even when a directory was selected, the action is determined
only once and then applied to all source files regardless of the presence
or absence of the respective byte code files."
  (interactive
   (let* ((buf  (current-buffer))
          (file (when (eq major-mode 'emacs-lisp-mode)
                  (buffer-file-name)))
          (action
           (cond
            (current-prefix-arg
             (if (> (prefix-numeric-value current-prefix-arg) 0)
                 'start
               'quit))
            (file
             (if (or (file-exists-p (byte-compile-dest-file file))
                     (and (eq major-mode 'emacs-lisp-mode)
                          (file-exists-p (byte-compile-dest-file
                                          (buffer-file-name buf)))))
                 'quit
               'start))
            (t
             (case (read-char-choice
                    "Toggle automatic compilation (s=tart, q=uit, C-g)? "
                    '(?s ?q))
               (?s 'start)
               (?q 'quit))))))
     (list (read-file-name (concat (capitalize (symbol-name action))
                                   " auto-compiling: ")
                           (when file (file-name-directory file))
                           nil t
                           (when file (file-name-nondirectory file)))
           action)))
  (if (file-regular-p file)
      (case action
        (start (auto-compile-byte-compile file t))
        (quit  (auto-compile-delete-dest (byte-compile-dest-file file))))
    (when (called-interactively-p 'any)
      (let ((log (get-buffer byte-compile-log-buffer)))
        (when log
          (kill-buffer log))))
    (dolist (f (directory-files file t))
      (cond
       ((file-directory-p f)
        (when (and auto-compile-recursive
                   (or (not (stringp auto-compile-recursive))
                       (string-match
                        auto-compile-recursive
                        (file-name-nondirectory (directory-file-name f)))))
          (toggle-auto-compile f action)))
       ((packed-library-p f)
        (let ((dest (byte-compile-dest-file f)))
          (if (eq action 'start)
              (and (file-exists-p f)
                   (or auto-compile-always-recompile
                       (file-newer-than-file-p f dest))
                   (or (not (string-match "^\\.?#" (file-name-nondirectory f)))
                       (file-exists-p dest))
                   (auto-compile-byte-compile f t))
            (auto-compile-delete-dest dest))))
       ((and auto-compile-delete-stray-dest
             (string-match "\\.elc$" f)
             (not (file-exists-p (packed-el-file f))))
        (auto-compile-delete-dest f))))))

(defalias 'auto-compile-toggle 'toggle-auto-compile)

(defun auto-compile-toggle-mark-failed-modified ()
  "Toggle whether buffers which failed to compile are marked as modified."
  (interactive)
  (message (concat (if (setq auto-compile-mark-failed-modified
                             (not auto-compile-mark-failed-modified))
                       "Mark "
                     "Don't mark ")
                   "files that failed to compile as modified")))

(defvar auto-compile-pretend-byte-compiled nil
  "Whether to try again to compile this file after a failed attempt.

Command `auto-compile-byte-compile' sets this buffer local variable to
t after failing to compile a source file being visited in a buffer (or
when variable `auto-compile-visit-failed' is non-nil for all files being
compiled) causing it to try again when being called again. Command
`toggle-auto-compile' will also pretend the byte code file exists.")
(make-variable-buffer-local 'auto-compile-pretend-byte-compiled)

(defun auto-compile-byte-compile (&optional file start)
  "Perform byte compilation for Auto-Compile mode."
  (let ((default-directory default-directory)
        dest buf success)
    (when (and file
               (setq buf (get-file-buffer file))
               (buffer-modified-p buf)
               (y-or-n-p (format "Save buffer %s first? " (buffer-name buf))))
      (with-current-buffer buf (save-buffer)))
    (unless file
      (setq file (buffer-file-name)
            buf  (get-file-buffer file)))
    (setq default-directory (file-name-directory file))
    (catch 'auto-compile
      (when (and auto-compile-check-parens buf)
        (condition-case check-parens
            (save-restriction
              (widen)
              (check-parens))
          (error
           (auto-compile-handle-compile-error file buf)
           (throw 'auto-compile nil))))
      (when (or start
                (file-exists-p (byte-compile-dest-file file))
                (when buf
                  (with-current-buffer buf
                    auto-compile-pretend-byte-compiled)))
        (condition-case byte-compile
            (let ((byte-compile-verbose auto-compile-verbose))
              (byte-compile-file file)
              (when buf
                (with-current-buffer buf
                  (kill-local-variable auto-compile-pretend-byte-compiled)))
              (setq success t))
          (file-error
           (message "Byte-compiling %s failed" file)
           (auto-compile-handle-compile-error file buf))))
      success)))

(defun auto-compile-delete-dest (dest &optional failurep)
  (unless failurep
    (let ((buf (get-file-buffer (packed-el-file dest))))
      (when buf
        (with-current-buffer buf
          (kill-local-variable 'auto-compile-pretend-byte-compiled)))))
  (condition-case nil
      (when (file-exists-p dest)
        (message "Deleting %s..." dest)
        (delete-file dest)
        (message "Deleting %s...done" dest))
    (file-error
     (auto-compile-ding)
     (message "Deleting %s...failed" dest))))

(defun auto-compile-handle-compile-error (file buf)
  (auto-compile-ding)
  (let ((dest (byte-compile-dest-file file)))
    (when (file-exists-p dest)
      (auto-compile-delete-dest dest t)))
  (when (or buf
            (and auto-compile-visit-failed
                 (setq buf (find-file-noselect file))))
    (with-current-buffer buf
      (setq auto-compile-pretend-byte-compiled t)
      (when auto-compile-mark-failed-modified
        (set-buffer-modified-p t)))))

(defun auto-compile-handle-autoloads-error (dest)
  (auto-compile-ding)
  (packed-remove-autoloads dest nil))

(defun auto-compile-ding ()
  (when auto-compile-ding
    (ding)))


;;; Mode-Line.

(defvar mode-line-auto-compile
  '(auto-compile-mode (:eval (mode-line-auto-compile-control))))

(defun mode-line-auto-compile-control ()
  (let ((src (buffer-file-name))
        dst)
    (when (and src (setq dst (byte-compile-dest-file src)))
      (list
       (cond
        ((file-writable-p dst)
         (propertize
          "-"
          'help-echo "Byte-compile destination is writable"
          'mouse-face 'mode-line))
        (t
         (propertize
          "%%"
          'help-echo "Byte-compile destination is read-only"
          'mouse-face 'mode-line)))
       (cond
        ((and auto-compile-pretend-byte-compiled
              (not (file-exists-p dst)))
         (propertize
          "!"
          'help-echo "Failed to byte-compile updating\nmouse-1 retry"
          'mouse-face 'mode-line-highlight
          'local-map (purecopy (make-mode-line-mouse-map
                                'mouse-1
                                #'auto-compile-mode-line-byte-compile))))
        ((not (file-exists-p dst))
         (propertize
          "%%"
          'help-echo "Byte-compiled file doesn't exist\nmouse-1 create"
          'mouse-face 'mode-line-highlight
          'local-map (purecopy (make-mode-line-mouse-map
                                'mouse-1
                                #'mode-line-toggle-auto-compile))))
        ((file-newer-than-file-p src dst)
         (propertize
          "*"
          'help-echo "Byte-compiled file needs updating\nmouse-1 update"
          'mouse-face 'mode-line-highlight
          'local-map (purecopy (make-mode-line-mouse-map
                                'mouse-1
                                #'auto-compile-mode-line-byte-compile))))
        (t
         (propertize
          "-"
          'help-echo "Byte-compiled file is up-to-date\nmouse-1 remove"
          'mouse-face 'mode-line-highlight
          'local-map (purecopy (make-mode-line-mouse-map
                                'mouse-1
                                #'mode-line-toggle-auto-compile)))))))))

(put 'mode-line-auto-compile 'risky-local-variable t)
(make-variable-buffer-local 'mode-line-auto-compile)

(defun mode-line-toggle-auto-compile (event)
  "Toggle automatic compilation from the mode-line."
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (toggle-auto-compile
     (buffer-file-name)
     (if (file-exists-p (byte-compile-dest-file (buffer-file-name)))
         'quit
       'start))
    (force-mode-line-update)))

(defun auto-compile-mode-line-byte-compile (event)
  "Recompile from the mode-line."
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (auto-compile-byte-compile (buffer-file-name) t)
    (force-mode-line-update)))


;;; Auto-Compile-On-Load Mode.

(define-minor-mode auto-compile-on-load-mode
  "Before loading a library recompile it if it needs recompilation.

It needs recompilation if it is newer than the byte-compile destination.
Without this advice the outdated byte compiled file would get loaded."
  :lighter auto-compile-on-load-mode-lighter
  :group 'auto-compile
  :global t
  (if auto-compile-on-load-mode
      (progn
        (ad-enable-advice 'load    'before 'auto-compile-on-load)
        (ad-enable-advice 'require 'before 'auto-compile-on-load)
        (ad-activate 'load)
        (ad-activate 'require))
    (ad-disable-advice 'load    'before 'auto-compile-on-load)
    (ad-disable-advice 'require 'before 'auto-compile-on-load)))

(defvar auto-compile-on-load-mode-lighter ""
  "Mode lighter for Auto-Compile-On-Load Mode.")

(defadvice load (before auto-compile-on-load disable)
  ;; (file &optional noerror nomessage nosuffix must-suffix)
  "Before loading the library recompile it if it needs recompilation.
It needs recompilation if it is newer than the byte-compile destination.
Without this advice the outdated byte-compiled file would get loaded."
  (auto-compile-on-load file nosuffix))

(defadvice require (before auto-compile-on-load disable)
  ;; (feature &optional FILENAME NOERROR)
  "Before loading the library recompile it if it needs recompilation.
It needs recompilation if it is newer than the byte-compile destination.
Without this advice the outdated byte-compiled file would get loaded."
  (unless (featurep feature)
    (auto-compile-on-load (or filename (symbol-name feature)))))

(defun auto-compile-on-load (file &optional nosuffix)
  (let (byte-compile-verbose el elc el*)
    (condition-case nil
        (when (setq el (packed-locate-library file nosuffix))
          (setq elc (byte-compile-dest-file el))
          (when (and (file-exists-p elc)
                     (file-newer-than-file-p el elc))
            (message "Recompiling %s..." el)
            (byte-compile-file el)
            (message "Recompiling %s...done" el))
          (when auto-compile-delete-stray-dest
            (setq el* (locate-library file))
            (unless (equal (file-name-directory el)
                           (file-name-directory el*))
              (auto-compile-delete-dest el* t))))
      (error
       (message "Recompiling %s...failed" el)
       (when elc
         (auto-compile-delete-dest elc t))))))

(provide 'auto-compile)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; auto-compile.el ends here
