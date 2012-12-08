;;; auto-compile.el --- automatically compile Emacs Lisp libraries

;; Copyright (C) 2008-2012  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20080830
;; Version: 1.0.9
;; Status: beta
;; Package-Requires: ((cl-lib "0.2") (packed "0.3.3"))
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

;; This package provides two minor modes which automatically recompile
;; Emacs Lisp source files.  Together these modes guarantee that Emacs
;; never loads outdated byte code files.

;; `auto-compile-on-save-mode' recompiles source files when they are
;; being saved and `auto-compile-on-save-load' does so before they are
;; being loaded.  Both modes only _recompile_ a source file when the
;; respective byte code file exists and is outdated.  Otherwise (when
;; the byte code file doesn't exist or is up-to-date) these modes do
;; _not_ compile the source file.

;; To permanently or temporarily toggle automatic compilation of some
;; source file use the command `toggle-auto-compile'.  Since the modes
;; only ever _update_ byte code files, toggling automatic compilation
;; is done simply by either creating the byte code file or removing
;; it.  `toggle-auto-compile' can also toggle automatic compilation of
;; multiple files at once; see it's doc-string for more information.

;; Even when using `auto-compile-on-save-mode' it can happen that some
;; source file is newer than the respective byte code file.  This can
;; for example happen when performing version control operations and
;; is a problem because Emacs always loads the byte code file even
;; when the respective source file has been modified since the former
;; was created.

;; To prevent outdated byte code files from being loaded
;; `auto-compile-on-load-mode' advises `require' and `load' to first
;; recompile a source file if it is newer than the respective byte
;; code file.

;; Enabling `auto-compile-on-load-mode' as early as possible reduces
;; the risk of loading an outdated byte code file.  It is best if you
;; enable both modes together at the beginning of your init file, even
;; before loading your package manager and having it initialize the
;; `load-path'.

;;     ;; -*- no-byte-compile: t -*-
;;     (add-to-list 'load-path "/path/to/auto-compile")
;;     (require 'auto-compile)
;;     (auto-compile-on-load-mode 1)
;;     (auto-compile-on-save-mode 1)

;; Automatically compilation of Emacs Lisp source files is useful for
;; at least the following reasons:

;; * Emacs prefers the byte code file over the source file even if the
;;   former is outdated.  If you have to do the compilation manually
;;   you will at least occasionally forget to do so and end up with an
;;   old version of your code being loaded.

;; * There are many otherwise fine libraries to be found on the
;;   Internet which when compiled will confront the user with a wall
;;   of compile warnings and an occasional error.  If authors are
;;   informed about these (often trivial) problems after each save
;;   they will likely fix them quite quickly.  That or they have a
;;   high noise tolerance.

;; * It's often easier and less annoying to fix errors and warnings as
;;   they are introduced than to do a "let's compile today's work and
;;   see how it goes".

;; This package is designed to stay out of your way as much as it can
;; while still motivating you to get things fixed.  That might be
;; annoying initially but less so once existing problems have been
;; fixed.  By default Auto-Compile won't let you quit Emacs on the
;; first attempt without fixing fatal errors in visited source files
;; first.  To disable this set `auto-compile-mark-failed-modified' to
;; nil.

;; Also note that just because no warnings and/or errors are reported
;; when a source file is compiled by the modes defined here this does
;; not necessarily mean that users of your libraries won't see any.  A
;; likely cause for this would be that you forgot to require a feature
;; which is loaded on your system but not necessarily on the users'
;; systems.  So you should still manually compile your packages before
;; release:
;;
;;     emacs -batch -Q -L . -L ../dependency/ -f batch-byte-compile *.el

;;; Code:

(require 'bytecomp)
(require 'cl-lib)
(require 'packed)

(declare-function autoload-rubric "autoload")
(declare-function autoload-find-destination "autoload")
(declare-function autoload-file-load-name "autoload")

(defgroup auto-compile nil
  "Automatically compile Emacs Lisp source libraries."
  :group 'convenience
  :prefix 'auto-compile
  :link '(function-link toggle-auto-compile)
  :link '(function-link auto-compile-byte-compile)
  :link '(function-link auto-compile-mode))


;;; Auto-Compile Mode.

;;;###autoload
(define-minor-mode auto-compile-mode
  "Compile Emacs Lisp source files after the visiting buffers are saved.

After a buffer containing Emacs Lisp code is saved to its source
file update the respective byte code file.  If the latter does
not exist do nothing.  Therefore to disable automatic compilation
remove the byte code file.  See command `toggle-auto-compile' for
a convenient way to do so.

This mode should be enabled globally, using it's globalized
variant `auto-compile-on-save-mode'.  Also see the related
`auto-compile-on-load-mode'."
  :lighter auto-compile-mode-lighter
  :group 'auto-compile
  (or (derived-mode-p 'emacs-lisp-mode)
      (error "This mode only makes sense with emacs-lisp-mode"))
  (if auto-compile-mode
      (add-hook  'after-save-hook 'auto-compile-byte-compile nil t)
    (remove-hook 'after-save-hook 'auto-compile-byte-compile t))
  (auto-compile-modify-mode-line auto-compile-use-mode-line))

;;;###autoload
(define-globalized-minor-mode auto-compile-on-save-mode
  auto-compile-mode turn-on-auto-compile-mode)

(defun turn-on-auto-compile-mode ()
  (when (eq major-mode 'emacs-lisp-mode)
    (auto-compile-mode 1)))

(defvar auto-compile-mode-lighter ""
  "Mode lighter for Auto-Compile Mode.")

(defcustom auto-compile-verbose nil
  "Whether to print messages describing progress of byte-compiler."
  :group 'auto-compile
  :type 'boolean)

(defcustom auto-compile-visit-failed t
  "Whether to visit source files which failed to compile.

If this is non-nil visit but don't select a source file if it
isn't being visited in a buffer already.  Also set the buffer
local value of variable `auto-compile-pretend-byte-compiled'
\(which see) to t and mark the buffer as modified if the value
of variable `auto-compile-mark-failed-modified' is non-nil."
  :group 'auto-compile
  :type 'boolean)

(defcustom auto-compile-mark-failed-modified t
  "Whether to mark buffers which failed to compile as modified.

This serves as a reminder to fix fatal errors.  While useful this
can get annoying so this variable can be quickly toggled with the
command `auto-compile-toggle-mark-failed-modified'."
  :group 'auto-compile
  :type 'boolean)

(defcustom auto-compile-ding t
  "Whether to beep (or flash the screen) when an error occurs.

Expected errors (such as compile error, unmatched parens, or
failure to remove a file) are caught and execution continues so
that failure to process one file does not prevent other files
from being processed.

To inform users of such errors Auto-Compile instead beeps or
flashes the screen; set this variable to nil to disable even
that."
  :group 'auto-compile
  :type 'boolean)

(defcustom auto-compile-check-parens t
  "Whether to check for unbalanced parentheses before compiling.

This only has as an effect on files which are currently being
visited in a buffer.  Other files are compiled without performing
this check first.  If unbalanced parentheses are found no attempt
is made to compile the file as that would obviously fail also."
  :group 'auto-compile
  :type 'boolean)

(defcustom auto-compile-delete-stray-dest t
  "Whether to remove stray byte code files.

If this is non-nil byte code files without a corresponding source
file are removed as they are encountered.  This happens in the
functions `auto-compile-on-save' and `toggle-auto-compile'.  The
main purpose of this functionality is to prevent leftover byte
code files from shadowing a source or byte code file in a
directory that comes later in the `load-path'."
  :group 'auto-compile
  :type 'boolean)

(defun auto-compile-modify-mode-line (after)
  (let ((format (delete 'mode-line-auto-compile
                        (default-value 'mode-line-format)))
        cell)
    (when (and after auto-compile-mode
               (setq cell (member after format)))
      (push 'mode-line-auto-compile (cdr cell)))
    (set-default 'mode-line-format format)))

(defcustom auto-compile-use-mode-line
  (car (memq 'mode-line-modified (default-value 'mode-line-format)))
  "Whether to show information about the byte code file in the mode line.

This works by inserting `mode-line-auto-compile' into the default
value of `mode-line-format' after the construct (usually a symbol)
specified here.  This happens every time local Auto-Compile mode
is turned on so the specified construct does not have to a member
of `mode-line-format' when this is set (this allows loading that
package after `auto-compile-on-load-mode' has been activated, so
that it can ensures the respective byte code file is up-to-date).

If you want to add `mode-line-auto-compile' as a member of a
variable that is itself a member of `mode-line-format' then you
have to set this option to nil and manually modify that variable
to include `mode-line-auto-compile'."
  :group 'auto-compile
  :set (lambda (symbol value)
         (set-default symbol value)
         (auto-compile-modify-mode-line value))
  :type '(choice (const :tag "don't insert" nil)
                 (const :tag "after mode-line-modified" mode-line-modified)
                 (const :tag "after mode-line-remote" mode-line-remote)
                 (sexp  :tag "after construct")))

(defcustom auto-compile-toggle-recursively t
  "Whether to recurse into subdirectories when toggling compilation.

If this non-nil only recurse into subdirectories for which
`packed-ignore-directory-p' returns nil.  Most importanly don't
enter hidden directories or those containing a file named
\".nosearch\".  Files in the top directory explicitly selected by
the user are always processed."
  :group 'auto-compile
  :type 'boolean)

(defcustom auto-compile-toggle-recompiles t
  "Whether to recompile all source files when turning on compilation.

When turning on auto compilation for multiple files at once
recompile source files even if their byte code file already
exist and are up-to-date."
  :group 'auto-compile
  :type 'boolean)

(defcustom auto-compile-toggle-deletes-nonlib-dest nil
  "Whether to remove non-library byte code files when toggling compilation."
  :group 'auto-compile
  :type 'boolean)

;;;###autoload
(defun toggle-auto-compile (file action)
  "Toggle automatic compilation of an Emacs Lisp source file or files.

Read a file or directory name from the minibuffer defaulting to
the visited Emacs Lisp source file or `default-directory' if no
such file is being visited in the current buffer.

If the user selects a file then automatic compilation of only
that file is toggled.  Since both `auto-compile-on-save' and
`auto-compile-on-save' only ever _recompile_ byte code files,
toggling automatic compilation is done simply by creating or
removing the respective byte code file.

If the user selects a directory then automatic compilation for
multiple files is toggled as follows:

* Whether byte code files should be created or removed is
  determined by the existence or absence of the byte code file of
  the source file that was current when this command was invoked.

* If no Emacs Lisp source file is being visited in the buffer
  that was current when the command was invoked ask the user what
  to do.

* With a positive prefix argument always compile source files;
  with a negative prefix argument always remove byte code files.

* When _removing_ byte code files then all byte code files are
  removed.  If `auto-compile-deletes-stray-dest' is non-nil this
  even includes byte code files for which no source file exists.

* When _creating_ byte code files only do so for source files
  that are actual libraries.  Source files that provide the
  correct feature are considered to be libraries; see
  `packed-library-p'.

* Note that non-libraries can still be automatically compiled,
  you just cannot _recursively_ turn on automatic compilation
  using this command.

* When `auto-compile-toggle-recompiles' is non-nil recompile all
  affected source files even when the respective source files are
  up-to-date.  Do so even for non-library source files.

* When `auto-compile-toggle-recursively' is non-nil recurse into
  subdirectories otherwise only files in the selected directory
  are affected.  Only enter subdirectories for which function
  `packed-ignore-directory-p' returns nil; most importantly don't
  enter hidden directories or those containing a file named
  \".nosearch\"."
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
             (if (file-exists-p (byte-compile-dest-file file))
                 'quit
               'start))
            (t
             (cl-case (read-char-choice
                       "Toggle automatic compilation (s=tart, q=uit, C-g)? "
                       '(?s ?q))
               (?s 'start)
               (?q 'quit))))))
     (list (read-file-name (concat (capitalize (symbol-name action))
                                   " auto-compiling: ")
                           (and file (file-name-directory file))
                           nil t
                           (and file (file-name-nondirectory file)))
           action)))
  (if (file-regular-p file)
      (cl-case action
        (start (auto-compile-byte-compile file t))
        (quit  (auto-compile-delete-dest (byte-compile-dest-file file))))
    (when (called-interactively-p 'any)
      (let ((log (get-buffer byte-compile-log-buffer)))
        (when log
          (kill-buffer log))))
    (dolist (f (directory-files file t))
      (cond
       ((file-directory-p f)
        (when (and auto-compile-toggle-recursively
                   ;; TODO pass the package name if we are certain
                   (not (packed-ignore-directory-p f nil)))
          (toggle-auto-compile f action)))
       ((packed-library-p f)
        (let ((dest (byte-compile-dest-file f)))
          (if (eq action 'start)
              (and (file-exists-p f)
                   (or auto-compile-toggle-recompiles
                       (file-newer-than-file-p f dest))
                   (or (not (string-match "^\\.?#" (file-name-nondirectory f)))
                       (file-exists-p dest))
                   (auto-compile-byte-compile f t))
            (auto-compile-delete-dest dest))))
       ((and auto-compile-toggle-deletes-nonlib-dest
             (eq action 'quit)
             (string-match (packed-el-regexp) f))
        (auto-compile-delete-dest (byte-compile-dest-file f)))
       ((and auto-compile-delete-stray-dest
             (string-match "\\.elc$" f)
             (not (file-exists-p (packed-el-file f))))
        (auto-compile-delete-dest f))))))

(defalias 'auto-compile-toggle 'toggle-auto-compile)

(defun auto-compile-toggle-mark-failed-modified (&optional arg)
  "Toggle whether buffers which failed to compile are marked as modified."
  (interactive)
  (message (concat (if (setq auto-compile-mark-failed-modified
                             (not auto-compile-mark-failed-modified))
                       "Mark "
                     "Don't mark ")
                   "files that failed to compile as modified")))

(defvar auto-compile-pretend-byte-compiled nil
  "Whether to try again to compile this file after a failed attempt.

Command `auto-compile-byte-compile' sets this buffer local
variable to t after failing to compile a source file being
visited in a buffer (or when variable `auto-compile-visit-failed'
is non-nil for all files being compiled) causing it to try again
when being called again. Command `toggle-auto-compile' will also
pretend the byte code file exists.")
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
  "Recompile visited file from the mode-line."
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (auto-compile-byte-compile (buffer-file-name) t)
    (force-mode-line-update)))


;;; Auto-Compile-On-Load Mode.

(define-minor-mode auto-compile-on-load-mode
  "Before loading a library recompile it if it needs recompilation.

A library needs to be recompiled if the source file is newer than
it's byte-compile destination.  Without this advice the outdated
byte code file would be loaded instead.

Also see the related `auto-compile-on-load-mode'."
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
It needs recompilation if it is newer than the byte-compile
destination.  Without this advice the outdated byte-compiled
file would get loaded."
  (auto-compile-on-load file nosuffix))

(defadvice require (before auto-compile-on-load disable)
  ;; (feature &optional FILENAME NOERROR)
  "Before loading the library recompile it if it needs recompilation.
It needs recompilation if it is newer than the byte-compile
destination.  Without this advice the outdated byte-compiled
file would get loaded."
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
