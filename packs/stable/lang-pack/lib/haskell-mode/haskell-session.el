;;; haskell-session.el --- Haskell sessions

;; Copyright (C) 2011-2012  Chris Done

;; Author: Chris Done <chrisdone@gmail.com>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Todo:

;;; Code:

(require 'haskell-cabal)
(require 'haskell-string)
(with-no-warnings (require 'cl))

(declare-function haskell-interactive-mode "haskell-interactive-mode" ())
(declare-function haskell-kill-session-process "haskell-process" (&optional session))
(declare-function haskell-process-start "haskell-process" (session))
(declare-function haskell-process-cd "haskell-process" (&optional not-interactive))

;; Dynamically scoped variables.
(defvar haskell-process-type)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Globals

(defvar haskell-sessions (list)
  "All Haskell sessions in the Emacs session.")

(defun haskell-session-tags-filename (session)
  "Get the filename for the TAGS file."
  (concat (haskell-session-cabal-dir session) "/TAGS"))

;;;###autoload
(defun haskell-session-all-modules (&optional dontcreate)
  "Get all modules -- installed or in the current project.
If DONTCREATE is non-nil don't create a new session."
  (append (haskell-session-installed-modules dontcreate)
          (haskell-session-project-modules dontcreate)))

;;;###autoload
(defun haskell-session-installed-modules (&optional dontcreate)
  "Get the modules installed in the current package set.
If DONTCREATE is non-nil don't create a new session."
  ;; TODO: Again, this makes HEAVY use of unix utilities. It'll work
  ;; fine in Linux, probably okay on OS X, and probably not at all on
  ;; Windows. Again, if someone wants to test on Windows and come up
  ;; with alternatives that's OK.
  ;;
  ;; Ideally all these package queries can be provided by a Haskell
  ;; program based on the Cabal API. Possibly as a nice service. Such
  ;; a service could cache and do nice things like that. For now, this
  ;; simple shell script takes us far.
  ;;
  ;; Probably also we can take the code from inferior-haskell-mode.
  ;;
  ;; Ugliness aside, if it saves us time to type it's a winner.
  ;;
  ;; FIXME/TODO: add support for (eq 'cabal-repl haskell-process-type)
  (require 'haskell-process) ; hack for accessing haskell-process-type
  (let ((modules (shell-command-to-string
                  (format "%s | %s | %s"
                          (if (eq 'cabal-dev haskell-process-type)
                              (if (or (not dontcreate) (haskell-session-maybe))
                                  (format "cabal-dev -s %s/cabal-dev ghc-pkg dump"
                                          (haskell-session-cabal-dir (haskell-session)))
                                "echo ''")
                            "ghc-pkg dump")
                          "egrep '^(exposed-modules: |                 )[A-Z]'"
                          "cut -c18-"))))
    (split-string modules)))

(defun haskell-session-project-modules (&optional dontcreate)
  "Get the modules of the current project.
If DONTCREATE is non-nil don't create a new session."
  (if (or (not dontcreate) (haskell-session-maybe))
      (let* ((session (haskell-session))
             (modules
              (shell-command-to-string
               (format "%s && %s"
                       (format "cd %s" (haskell-session-cabal-dir session))
                       ;; TODO: Use a different, better source. Possibly hasktags or some such.
                       ;; TODO: At least make it cross-platform. Linux
                       ;; (and possibly OS X) have egrep, Windows
                       ;; doesn't -- or does it via Cygwin or MinGW?
                       ;; This also doesn't handle module\nName. But those gits can just cut it out!
                       "egrep '^module[\t\r ]+[^(\t\r ]+' . -r -I --include='*.*hs' --include='*.hsc' -s -o -h | sed 's/^module[\t\r ]*//' | sort | uniq"))))
        (split-string modules))))

(defun haskell-session-kill (&optional leave-interactive-buffer)
  "Kill the session process and buffer, delete the session.
0. Prompt to kill all associated buffers.
1. Kill the process.
2. Kill the interactive buffer.
3. Walk through all the related buffers and set their haskell-session to nil.
4. Remove the session from the sessions list."
  (interactive)
  (let* ((session (haskell-session))
         (name (haskell-session-name session))
         (also-kill-buffers (y-or-n-p (format "Killing `%s'. Also kill all associated buffers?" name))))
    (haskell-kill-session-process session)
    (unless leave-interactive-buffer
      (kill-buffer (haskell-session-interactive-buffer session)))
    (loop for buffer in (buffer-list)
          do (with-current-buffer buffer
               (when (and (boundp 'haskell-session)
                          (string= (haskell-session-name haskell-session) name))
                 (setq haskell-session nil)
                 (when also-kill-buffers
                   (kill-buffer)))))
    (setq haskell-sessions
          (remove-if (lambda (session)
                       (string= (haskell-session-name session)
                                name))
                     haskell-sessions))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finding/clearing the session

;; Used internally
(defvar haskell-session)

;;;###autoload
(defun haskell-session-maybe ()
  "Maybe get the Haskell session, return nil if there isn't one."
  (if (default-boundp 'haskell-session)
      haskell-session
    (setq haskell-session nil)))

;;;###autoload
(defun haskell-session ()
  "Get the Haskell session, prompt if there isn't one or fail."
  (or (haskell-session-maybe)
      (haskell-session-assign
       (or (haskell-session-from-buffer)
           (haskell-session-new-assume-from-cabal)
           (haskell-session-choose)
           (haskell-session-new)))))

(defun haskell-session-new-assume-from-cabal ()
  "Prompt to create a new project based on a guess from the nearest Cabal file."
  (when (y-or-n-p (format "Start a new project named “%s”? "
                          (haskell-session-default-name)))
    (haskell-session-make (haskell-session-default-name))))

(defun haskell-session-from-buffer ()
  "Get the session based on the buffer."
  (when (and (buffer-file-name)
             (consp haskell-sessions))
    (reduce (lambda (acc a)
              (if (haskell-is-prefix-of (haskell-session-cabal-dir a)
                                        (file-name-directory (buffer-file-name)))
                  (if acc
                      (if (and
                           (> (length (haskell-session-cabal-dir a))
                              (length (haskell-session-cabal-dir acc))))
                          a
                        acc)
                    a)
                acc))
            haskell-sessions
            :initial-value nil)))

(defun haskell-session-new ()
  "Make a new session."
  (let ((name (read-from-minibuffer "Project name: " (haskell-session-default-name))))
    (when (not (string= name ""))
      (haskell-session-make name))))

(defun haskell-session-default-name ()
  "Generate a default project name for the new project prompt."
  (let ((file (haskell-cabal-find-file)))
    (or (when file
          (downcase (file-name-sans-extension
                     (file-name-nondirectory file))))
        "haskell")))

(defun haskell-session-assign (session)
  "Set the current session."
  (set (make-local-variable 'haskell-session) session))

(defun haskell-session-choose ()
  "Find a session by choosing from a list of the current sessions."
  (when haskell-sessions
    (let* ((session-name (ido-completing-read
                          "Choose Haskell session: "
                          (mapcar 'haskell-session-name haskell-sessions)))
           (session (find-if (lambda (session)
                               (string= (haskell-session-name session)
                                        session-name))
                             haskell-sessions)))
      session)))

(defun haskell-session-clear ()
  "Clear the buffer of any Haskell session choice."
  (set (make-local-variable 'haskell-session) nil))

(defun haskell-session-change ()
  "Change the session for the current buffer."
  (interactive)
  (haskell-session-clear)
  (haskell-session-assign (or (haskell-session-new-assume-from-cabal)
                              (haskell-session-choose)
                              (haskell-session-new))))

(defun haskell-session-change-target (target)
  "Set the build target for cabal repl"
  (interactive "sNew build target:")
  (let* ((session haskell-session)
         (old-target (haskell-session-get session 'target)))
    (when session
      (haskell-session-set-target session target)
      (when (and (not (string= old-target target))
                 (y-or-n-p "Target changed, restart haskell process?"))
        (haskell-process-start session)))))

(defun haskell-session-strip-dir (session file)
  "Strip the load dir from the file path."
  (let ((cur-dir (haskell-session-current-dir session)))
    (if (> (length file) (length cur-dir))
        (if (string= (substring file 0 (length cur-dir))
                     cur-dir)
            (replace-regexp-in-string
             "^[/\\]" ""
             (substring file
                        (length cur-dir)))
          file)
      file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Building the session

(defun haskell-session-make (name)
  "Make a Haskell session."
  (let ((session (set (make-local-variable 'haskell-session)
                      (list (cons 'name name)))))
    (add-to-list 'haskell-sessions session)
    (haskell-process-start session)
    session))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Accessing the session

(defun haskell-session-name (s)
  "Get the session name."
  (haskell-session-get s 'name))

(defun haskell-session-target (s)
  "Get the session build target."
  (let* ((maybe-target (haskell-session-get s 'target))
         (target (if maybe-target maybe-target
                   (let ((new-target
                          (read-string "build target (empty for default):")))
                     (haskell-session-set-target s new-target)))))
    (if (not (string= target "")) target nil)))

(defun haskell-session-set-target (s target)
  "Set the session build target."
  (haskell-session-set s 'target target))

(defun haskell-session-interactive-buffer (s)
  "Get the session interactive buffer."
  (let ((buffer (haskell-session-get s 'interactive-buffer)))
    (if (and buffer (buffer-live-p buffer))
        buffer
      (let ((buffer (get-buffer-create (format "*%s*" (haskell-session-name s)))))
        (haskell-session-set-interactive-buffer s buffer)
        (with-current-buffer buffer
          (haskell-interactive-mode)
          (haskell-session-assign s))
        (switch-to-buffer-other-window buffer)
        buffer))))

(defun haskell-session-set-interactive-buffer (s v)
  "Set the session interactive buffer."
  (haskell-session-set s 'interactive-buffer v))

(defun haskell-session-set-process (s v)
  "Set the session process."
  (haskell-session-set s 'process v))

;;;###autoload
(defun haskell-session-process (s)
  "Get the session process."
  (haskell-session-get s 'process))

(defun haskell-session-set-cabal-dir (s v)
  "Set the session cabal-dir."
  (let ((true-path (file-truename v)))
    (haskell-session-set s 'cabal-dir true-path)
    (haskell-session-set-cabal-checksum s true-path)))

(defun haskell-session-set-current-dir (s v)
  "Set the session current directory."
  (let ((true-path (file-truename v)))
    (haskell-session-set s 'current-dir true-path)))

(defun haskell-session-set-cabal-checksum (s cabal-dir)
  "Set the session checksum of .cabal files"
  (haskell-session-set s 'cabal-checksum
                       (haskell-cabal-compute-checksum cabal-dir)))

(defun haskell-session-current-dir (s)
  "Get the session current directory."
  (let ((dir (haskell-session-get s 'current-dir)))
    (or dir
        (haskell-process-cd t))))

(defun haskell-session-cabal-dir (s)
  "Get the session cabal-dir."
  (let ((dir (haskell-session-get s 'cabal-dir)))
    (if dir
        dir
      (let ((set-dir (haskell-cabal-get-dir)))
        (if set-dir
            (progn (haskell-session-set-cabal-dir s set-dir)
                   set-dir)
          (haskell-session-cabal-dir s))))))

(defun haskell-session-modify (session key update)
  "Update the value at KEY in SESSION with UPDATE."
  (haskell-session-set
   session
   key
   (funcall update
            (haskell-session-get session key))))

(defun haskell-session-get (session key)
  "Get the SESSION's KEY value.
Returns nil if KEY not set."
  (cdr (assq key session)))

(defun haskell-session-set (session key value)
  "Set the SESSION's KEY to VALUE.
Returns newly set VALUE."
  (let ((cell (assq key session)))
    (if cell
        (setcdr cell value) ; modify cell in-place
      (setcdr session (cons (cons key value) (cdr session))) ; new cell
      value)))

(provide 'haskell-session)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; haskell-session.el ends here
