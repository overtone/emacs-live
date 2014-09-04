;;; haskell-process.el --- Communicating with the inferior Haskell process

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

(require 'haskell-complete-module)
(require 'haskell-mode)
(require 'haskell-session)
(require 'haskell-compat)
(require 'haskell-str)
(require 'haskell-compile)
(require 'haskell-utils)
(require 'haskell-presentation-mode)
(require 'haskell-navigate-imports)
(with-no-warnings (require 'cl))

;; FIXME: haskell-process shouldn't depend on haskell-interactive-mode to avoid module-dep cycles
(declare-function haskell-interactive-mode-echo "haskell-interactive-mode" (session message &optional mode))
(declare-function haskell-interactive-mode-compile-error "haskell-interactive-mode" (session message))
(declare-function haskell-interactive-mode-compile-warning "haskell-interactive-mode" (session message))
(declare-function haskell-interactive-mode-insert "haskell-interactive-mode" (session message))
(declare-function haskell-interactive-mode-reset-error "haskell-interactive-mode" (session))
(declare-function haskell-interactive-show-load-message "haskell-interactive-mode" (session type module-name file-name echo th))
(declare-function haskell-interactive-mode-insert-garbage "haskell-interactive-mode" (session message))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration
(defgroup haskell-interactive nil
  "Settings for REPL interaction via `haskell-interactive-mode'"
  :link '(custom-manual "(haskell-mode)haskell-interactive-mode")
  :group 'haskell)

(defcustom haskell-process-path-ghci
  "ghci"
  "The path for starting ghci."
  :group 'haskell-interactive
  :type '(choice string (repeat string)))

(defcustom haskell-process-path-cabal
  "cabal"
  "Path to the `cabal' executable."
  :group 'haskell-interactive
  :type '(choice string (repeat string)))

(defcustom haskell-process-path-cabal-ghci
  "cabal-ghci"
  "The path for starting cabal-ghci."
  :group 'haskell-interactive
  :type '(choice string (repeat string)))

(defcustom haskell-process-path-cabal-dev
  "cabal-dev"
  "The path for starting cabal-dev."
  :group 'haskell-interactive
  :type '(choice string (repeat string)))

(defcustom haskell-process-args-ghci
  '("-ferror-spans")
  "Any arguments for starting ghci."
  :group 'haskell-interactive
  :type '(repeat (string :tag "Argument")))

(defcustom haskell-process-args-cabal-repl
  '("--ghc-option=-ferror-spans")
  "Additional arguments to for `cabal repl' invocation.
Note: The settings in `haskell-process-path-ghci' and
`haskell-process-args-ghci' are not automatically reused as `cabal repl'
currently invokes `ghc --interactive'. Use
`--with-ghc=<path-to-executable>' if you want to use a different
interactive GHC frontend; use `--ghc-option=<ghc-argument>' to
pass additional flags to `ghc'."
  :group 'haskell-interactive
  :type '(repeat (string :tag "Argument")))

(defcustom haskell-process-do-cabal-format-string
  ":!cd %s && %s"
  "The way to run cabal comands. It takes two arguments -- the directory and the command.
See `haskell-process-do-cabal' for more details."
  :group 'haskell-interactive
  :type 'string)

(defcustom haskell-process-type
  'ghci
  "The inferior Haskell process type to use."
  :type '(choice (const ghci) (const cabal-repl) (const cabal-dev) (const cabal-ghci))
  :group 'haskell-interactive)

(defcustom haskell-process-log
  nil
  "Enable debug logging to \"*haskell-process-log*\" buffer."
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-process-show-debug-tips
  t
  "Show debugging tips when starting the process."
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-notify-p
  nil
  "Notify using notifications.el (if loaded)?"
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-process-suggest-no-warn-orphans
  t
  "Suggest adding -fno-warn-orphans pragma to file when getting orphan warnings."
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-process-suggest-hoogle-imports
  nil
  "Suggest to add import statements using Hoogle as a backend."
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-process-suggest-haskell-docs-imports
  nil
  "Suggest to add import statements using haskell-docs as a backend."
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-process-suggest-add-package
  t
  "Suggest to add packages to your .cabal file when Cabal says it
is a member of the hidden package, blah blah."
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-process-suggest-language-pragmas
  t
  "Suggest adding LANGUAGE pragmas recommended by GHC."
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-process-suggest-remove-import-lines
  nil
  "Suggest removing import lines as warned by GHC."
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-process-suggest-overloaded-strings
  t
  "Suggest adding OverloadedStrings pragma to file when getting type mismatches with [Char]."
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-process-check-cabal-config-on-load
  t
  "Check changes cabal config on loading Haskell files and
restart the GHCi process if changed.."
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-process-prompt-restart-on-cabal-change
  t
  "Ask whether to restart the GHCi process when the Cabal file
has changed?"
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-process-auto-import-loaded-modules
  nil
  "Auto import the modules reported by GHC to have been loaded?"
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-process-reload-with-fbytecode
  nil
  "When using -fobject-code, auto reload with -fbyte-code (and
then restore the -fobject-code) so that all module info and
imports become available?"
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-process-use-presentation-mode
  nil
  "Use presentation mode to show things like type info instead of
  printing to the message area."
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-process-suggest-restart
  t
  "Suggest restarting the process when it has died"
  :type 'boolean
  :group 'haskell-interactive)

(defvar haskell-process-prompt-regex "\4")
(defvar haskell-reload-p nil)

(defvar haskell-process-greetings
  (list "Hello, Haskell!"
        "The lambdas must flow."
        "Hours of hacking await!"
        "The next big Haskell project is about to start!"
        "Your wish is my IO ().")
  "Greetings for when the Haskell process starts up.")

(defconst haskell-process-logo
  (expand-file-name "logo.svg" haskell-mode-pkg-base-dir)
  "Haskell logo for notifications.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Accessing commands -- using cl 'defstruct'
(defstruct haskell-command
  "Data structure representing a command to be executed when with
  a custom state and three callback."
  ;; hold the custom command state
  ;; state :: a
  state
  ;; called when to execute a command
  ;; go :: a -> ()
  go
  ;; called whenever output was collected from the haskell process
  ;; live :: a -> Response -> Bool
  live
  ;; called when the output from the haskell process indicates that the command
  ;; is complete
  ;; complete :: a -> Response -> ()
  complete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Accessing commands


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specialised commands

;;;###autoload
(defun haskell-process-generate-tags (&optional and-then-find-this-tag)
  "Regenerate the TAGS table."
  (interactive)
  (let ((process (haskell-process)))
    (haskell-process-queue-command
     process
     (make-haskell-command
      :state (cons process and-then-find-this-tag)
      :go (lambda (state)
            (if (eq system-type 'windows-nt)
                (haskell-process-send-string
                 (car state)
                 (format ":!powershell -Command \"& { cd %s ; hasktags -e -x (ls -fi *.hs -exclude \\\"#*#\\\" -name -r) ; exit }\""
                         (haskell-session-cabal-dir
                          (haskell-process-session (car state)))))
                (haskell-process-send-string
                 (car state)
                 (format ":!cd %s && %s | %s | %s"
                         (haskell-session-cabal-dir
                          (haskell-process-session (car state)))
                         "find . -name '*.hs*'"
                         "grep -v '#'" ; To avoid Emacs back-up files. Yeah.
                         "xargs hasktags -e -x"))))
      :complete (lambda (state response)
                  (when (cdr state)
                    (let ((tags-file-name
                           (haskell-session-tags-filename
                            (haskell-process-session (car state)))))
                      (find-tag (cdr state))))
                  (haskell-mode-message-line "Tags generated."))))))

;;;###autoload
(defun haskell-process-do-type (&optional insert-value)
  "Print the type of the given expression."
  (interactive "P")
  (if insert-value
      (haskell-process-insert-type)
    (haskell-process-do-simple-echo
     (let ((ident (haskell-ident-at-point)))
       ;; TODO: Generalize all these `string-match' of ident calls into
       ;; one function.
       (format (if (string-match "^[_[:lower:][:upper:]]" ident)
                   ":type %s"
                 ":type (%s)")
               ident))
     'haskell-mode)))

(defun haskell-process-insert-type ()
  "Get the identifer at the point and insert its type, if
possible, using GHCi's :type."
  (let ((process (haskell-process))
        (query (let ((ident (haskell-ident-at-point)))
                 (format (if (string-match "^[_[:lower:][:upper:]]" ident)
                             ":type %s"
                           ":type (%s)")
                         ident))))
    (haskell-process-queue-command
     process
     (make-haskell-command
      :state (list process query (current-buffer))
      :go (lambda (state)
            (haskell-process-send-string (nth 0 state)
                                         (nth 1 state)))
      :complete (lambda (state response)
                  (cond
                   ;; TODO: Generalize this into a function.
                   ((or (string-match "^Top level" response)
                        (string-match "^<interactive>" response))
                    (message response))
                   (t
                    (with-current-buffer (nth 2 state)
                      (goto-char (line-beginning-position))
                      (insert (format "%s\n" (replace-regexp-in-string "\n$" "" response)))))))))))

;;;###autoload
(defun haskell-process-do-info (&optional prompt-value)
  "Print info on the identifier at point.
If PROMPT-VALUE is non-nil, request identifier via mini-buffer."
  (interactive "P")
  (haskell-process-do-simple-echo
   (let ((ident (if prompt-value
                    (read-from-minibuffer "Info: " (haskell-ident-at-point))
                  (haskell-ident-at-point)))
         (modname (unless prompt-value
                    (haskell-utils-parse-import-statement-at-point))))
     (if modname
         (format ":browse! %s" modname)
       (format (if (string-match "^[a-zA-Z_]" ident)
                   ":info %s"
                 ":info (%s)")
               (or ident
                   (haskell-ident-at-point)))))
   'haskell-mode))

(defun haskell-process-do-try-info (sym)
  "Get info of `sym' and echo in the minibuffer."
  (let ((process (haskell-process)))
    (haskell-process-queue-command
     process
     (make-haskell-command
      :state (cons process sym)
      :go (lambda (state)
            (haskell-process-send-string
             (car state)
             (if (string-match "^[A-Za-z_]" (cdr state))
                 (format ":info %s" (cdr state))
               (format ":info (%s)" (cdr state)))))
      :complete (lambda (state response)
                  (unless (or (string-match "^Top level" response)
                              (string-match "^<interactive>" response))
                    (haskell-mode-message-line response)))))))

(defun haskell-process-do-try-type (sym)
  "Get type of `sym' and echo in the minibuffer."
  (let ((process (haskell-process)))
    (haskell-process-queue-command
     process
     (make-haskell-command
      :state (cons process sym)
      :go (lambda (state)
            (haskell-process-send-string
             (car state)
             (if (string-match "^[A-Za-z_]" (cdr state))
                 (format ":type %s" (cdr state))
               (format ":type (%s)" (cdr state)))))
      :complete (lambda (state response)
                  (unless (or (string-match "^Top level" response)
                              (string-match "^<interactive>" response))
                    (haskell-mode-message-line response)))))))

(defun haskell-process-do-simple-echo (line &optional mode)
  "Send LINE to the GHCi process and echo the result in some
fashion, such as printing in the minibuffer, or using
haskell-present, depending on configuration."
  (let ((process (haskell-process)))
    (haskell-process-queue-command
     process
     (make-haskell-command
      :state (list process line mode)
      :go (lambda (state)
            (haskell-process-send-string (car state) (cadr state)))
      :complete (lambda (state response)
                  ;; TODO: TBD: don't do this if
                  ;; `haskell-process-use-presentation-mode' is t.
                  (haskell-interactive-mode-echo
                   (haskell-process-session (car state))
                   response
                   (caddr state))
                  (if haskell-process-use-presentation-mode
                      (progn (haskell-present (cadr state)
                                              (haskell-process-session (car state))
                                              response)
                             (haskell-session-assign
                              (haskell-process-session (car state))))
                    (haskell-mode-message-line response)))))))

(defun haskell-process-look-config-changes (session)
  "Checks whether a cabal configuration file has
changed. Restarts the process if that is the case."
  (let ((current-checksum (haskell-session-get session 'cabal-checksum))
        (new-checksum (haskell-cabal-compute-checksum
                       (haskell-session-get session 'cabal-dir))))
    (when (not (string= current-checksum new-checksum))
      (haskell-interactive-mode-echo session (format "Cabal file changed: %s" new-checksum))
      (haskell-session-set-cabal-checksum session
                                          (haskell-session-get session 'cabal-dir))
      (unless (and haskell-process-prompt-restart-on-cabal-change
                   (not (y-or-n-p "Cabal file changed; restart GHCi process? ")))
        (haskell-process-start (haskell-session))))))

;;;###autoload
(defun haskell-process-load-file ()
  "Load the current buffer file."
  (interactive)
  (save-buffer)
  (haskell-interactive-mode-reset-error (haskell-session))
  (haskell-process-file-loadish (concat "load " (buffer-file-name))
                                nil
                                (current-buffer)))

;;;###autoload
(defun haskell-process-reload-file ()
  "Re-load the current buffer file."
  (interactive)
  (save-buffer)
  (haskell-interactive-mode-reset-error (haskell-session))
  (haskell-process-file-loadish "reload" t nil))

;;;###autoload
(defun haskell-process-load-or-reload (&optional toggle)
  "Load or reload. Universal argument toggles which."
  (interactive "P")
  (if toggle
      (progn (setq haskell-reload-p (not haskell-reload-p))
             (message "%s (No action taken this time)"
                      (if haskell-reload-p
                          "Now running :reload."
                        "Now running :load <buffer-filename>.")))
    (if haskell-reload-p (haskell-process-reload-file) (haskell-process-load-file))))

(defun haskell-process-file-loadish (command reload-p module-buffer)
  "Run a loading-ish COMMAND that wants to pick up type errors
and things like that. RELOAD-P indicates whether the notification
should say 'reloaded' or 'loaded'. MODULE-BUFFER may be used
for various things, but is optional."
  (let ((session (haskell-session)))
    (haskell-session-current-dir session)
    (when haskell-process-check-cabal-config-on-load
      (haskell-process-look-config-changes session))
    (let ((process (haskell-process)))
      (haskell-process-queue-command
       process
       (make-haskell-command
        :state (list session process command reload-p module-buffer)
        :go (lambda (state)
              (haskell-process-send-string
               (cadr state) (format ":%s" (caddr state))))
        :live (lambda (state buffer)
                (haskell-process-live-build
                 (cadr state) buffer nil))
        :complete (lambda (state response)
                    (haskell-process-load-complete
                     (car state)
                     (cadr state)
                     response
                     (cadddr state)
                     (cadddr (cdr state)))))))))

;;;###autoload
(defun haskell-process-cabal-build ()
  "Build the Cabal project."
  (interactive)
  (haskell-process-do-cabal "build")
  (haskell-process-add-cabal-autogen))

;;;###autoload
(defun haskell-process-cabal (p)
  "Prompts for a Cabal command to run."
  (interactive "P")
  (if p
      (haskell-process-do-cabal
       (read-from-minibuffer "Cabal command (e.g. install): "))
    (haskell-process-do-cabal
     (funcall haskell-completing-read-function "Cabal command: "
              haskell-cabal-commands))))

(defun haskell-process-add-cabal-autogen ()
  "Add <cabal-project-dir>/dist/build/autogen/ to the ghci search
path. This allows modules such as 'Path_...', generated by cabal,
to be loaded by ghci."
  (unless (eq 'cabal-repl haskell-process-type) ;; redundant with "cabal repl"
    (let*
        ((session       (haskell-session))
         (cabal-dir     (haskell-session-cabal-dir session))
         (ghci-gen-dir  (format "%sdist/build/autogen/" cabal-dir)))
      (haskell-process-queue-without-filters
       (haskell-process)
       (format ":set -i%s" ghci-gen-dir)))))

(defun haskell-process-do-cabal (command)
  "Run a Cabal command."
  (let ((process (haskell-process)))
    (haskell-process-queue-command
     process
     (make-haskell-command
      :state (list (haskell-session) process command 0)

      :go
      (lambda (state)
        (haskell-process-send-string
         (cadr state)
         (format haskell-process-do-cabal-format-string
                 (haskell-session-cabal-dir (car state))
                 (format "%s %s"
                         (ecase haskell-process-type
                           ('ghci haskell-process-path-cabal)
                           ('cabal-repl haskell-process-path-cabal)
                           ('cabal-ghci haskell-process-path-cabal)
                           ('cabal-dev haskell-process-path-cabal-dev))
                         (caddr state)))))

      :live
      (lambda (state buffer)
        (let ((cmd (replace-regexp-in-string "^\\([a-z]+\\).*"
                                             "\\1"
                                             (caddr state))))
          (cond ((or (string= cmd "build")
                     (string= cmd "install"))
                 (haskell-process-live-build (cadr state) buffer t))
                (t
                 (haskell-process-cabal-live state buffer)))))

      :complete
      (lambda (state response)
        (let* ((process (cadr state))
               (session (haskell-process-session process))
               (message-count 0)
               (cursor (haskell-process-response-cursor process)))
          (haskell-process-set-response-cursor process 0)
          (while (haskell-process-errors-warnings session process response)
            (setq message-count (1+ message-count)))
          (haskell-process-set-response-cursor process cursor)
          (let ((msg (format "Complete: cabal %s (%s compiler messages)"
                             (caddr state)
                             message-count)))
            (haskell-interactive-mode-echo session msg)
            (when (= message-count 0)
              (haskell-interactive-mode-echo
               session
               "No compiler messages, dumping complete output:")
              (haskell-interactive-mode-echo session response))
            (haskell-mode-message-line msg)
            (when (and haskell-notify-p
                       (fboundp 'notifications-notify))
              (notifications-notify
               :title (format "*%s*" (haskell-session-name (car state)))
               :body msg
               :app-name (ecase haskell-process-type
                           ('ghci haskell-process-path-cabal)
                           ('cabal-repl haskell-process-path-cabal)
                           ('cabal-ghci haskell-process-path-cabal)
                           ('cabal-dev haskell-process-path-cabal-dev))
               :app-icon haskell-process-logo
               )))))))))

(defun haskell-process-cabal-live (state buffer)
  "Do live updates for Cabal processes."
  (haskell-interactive-mode-insert
   (haskell-process-session (cadr state))
   (replace-regexp-in-string
    haskell-process-prompt-regex
    ""
    (substring buffer (cadddr state))))
  (setf (cdddr state) (list (length buffer)))
  nil)

(defun haskell-process-load-complete (session process buffer reload module-buffer &optional cont)
  "Handle the complete loading response. BUFFER is the string of
text being sent over the process pipe. MODULE-BUFFER is the
actual Emacs buffer of the module being loaded."
  (cond ((haskell-process-consume process "Ok, modules loaded: \\(.+\\)\\.$")
         (let* ((modules (haskell-process-extract-modules buffer))
                (cursor (haskell-process-response-cursor process)))
           (haskell-process-set-response-cursor process 0)
           (let ((warning-count 0))
             (while (haskell-process-errors-warnings session process buffer)
               (setq warning-count (1+ warning-count)))
             (haskell-process-set-response-cursor process cursor)
             (if (and (not reload)
                      haskell-process-reload-with-fbytecode)
                 (haskell-process-reload-with-fbytecode process module-buffer)
               (haskell-process-import-modules process (car modules)))
             (haskell-mode-message-line
              (if reload "Reloaded OK." "OK."))
             (when cont
               (condition-case e
                   (funcall cont t)
                 (error (message "%S" e))
                 (quit nil))))))
        ((haskell-process-consume process "Failed, modules loaded: \\(.+\\)\\.$")
         (let* ((modules (haskell-process-extract-modules buffer))
                (cursor (haskell-process-response-cursor process)))
           (haskell-process-set-response-cursor process 0)
           (while (haskell-process-errors-warnings session process buffer))
           (haskell-process-set-response-cursor process cursor)
           (if (and (not reload) haskell-process-reload-with-fbytecode)
               (haskell-process-reload-with-fbytecode process module-buffer)
             (haskell-process-import-modules process (car modules)))
           (haskell-interactive-mode-compile-error session "Compilation failed.")
           (when cont
             (condition-case e
                 (funcall cont nil)
               (error (message "%S" e))
               (quit nil)))))))

(defun haskell-process-reload-with-fbytecode (process module-buffer)
  "Reload FILE-NAME with -fbyte-code set, and then restore -fobject-code."
  (haskell-process-queue-without-filters process ":set -fbyte-code")
  (haskell-process-touch-buffer process module-buffer)
  (haskell-process-queue-without-filters process ":reload")
  (haskell-process-queue-without-filters process ":set -fobject-code"))

(defun haskell-process-touch-buffer (process buffer)
  "Updates mtime on the file for BUFFER by queing a touch on
PROCESS."
  (interactive)
  (haskell-process-queue-command
   process
   (make-haskell-command
    :state (cons process buffer)
    :go (lambda (state)
          (haskell-process-send-string
           (car state)
           (format ":!%s %s"
                   "touch"
                   (shell-quote-argument (buffer-file-name
                                          (cdr state))))))
    :complete (lambda (state _)
                (with-current-buffer (cdr state)
                  (clear-visited-file-modtime))))))

(defun haskell-process-extract-modules (buffer)
  "Extract the modules from the process buffer."
  (let* ((modules-string (match-string 1 buffer))
         (modules (split-string modules-string ", ")))
    (cons modules modules-string)))

(defun haskell-process-import-modules (process modules)
  "Import `modules' with :m +, and send any import statements
from `module-buffer'."
  (when haskell-process-auto-import-loaded-modules
    (haskell-process-queue-command
     process
     (make-haskell-command
      :state (cons process modules)
      :go (lambda (state)
            (haskell-process-send-string
             (car state)
             (format ":m + %s" (mapconcat 'identity (cdr state) " "))))))))

(defun haskell-process-live-build (process buffer echo-in-repl)
  "Show live updates for loading files."
  (cond ((haskell-process-consume
          process
          (concat "\\[[ ]*\\([0-9]+\\) of \\([0-9]+\\)\\]"
                  " Compiling \\([^ ]+\\)[ ]+"
                  "( \\([^ ]+\\), \\([^ ]+\\) )[^\r\n]*[\r\n]+"))
         (haskell-process-echo-load-message process buffer echo-in-repl nil)
         t)
        ((haskell-process-consume
          process
          (concat "\\[[ ]*\\([0-9]+\\) of \\([0-9]+\\)\\]"
                  " Compiling \\[TH\\] \\([^ ]+\\)[ ]+"
                  "( \\([^ ]+\\), \\([^ ]+\\) )[^\r\n]*[\r\n]+"))
         (haskell-process-echo-load-message process buffer echo-in-repl t)
         t)
        ((haskell-process-consume process "Loading package \\([^ ]+\\) ... linking ... done.\n")
         (haskell-mode-message-line
          (format "Loading: %s"
                  (match-string 1 buffer)))
         t)
        ((haskell-process-consume
          process
          "^Preprocessing executables for \\(.+?\\)\\.\\.\\.")
         (let ((msg (format "Preprocessing: %s" (match-string 1 buffer))))
           (haskell-interactive-mode-echo
            (haskell-process-session process)
            msg)
           (haskell-mode-message-line msg)))
        ((haskell-process-consume process "Linking \\(.+?\\) \\.\\.\\.")
         (let ((msg (format "Linking: %s" (match-string 1 buffer))))
           (haskell-interactive-mode-echo (haskell-process-session process) msg)
           (haskell-mode-message-line msg)))
        ((haskell-process-consume process "\nBuilding \\(.+?\\)\\.\\.\\.")
         (let ((msg (format "Building: %s" (match-string 1 buffer))))
           (haskell-interactive-mode-echo
            (haskell-process-session process)
            msg)
           (haskell-mode-message-line msg)))))

(defun haskell-process-echo-load-message (process buffer echo-in-repl th)
  "Echo a load message."
  (let ((session (haskell-process-session process))
        (module-name (match-string 3 buffer))
        (file-name (match-string 4 buffer)))
    (haskell-interactive-show-load-message
     session
     'compiling
     module-name
     (haskell-session-strip-dir session file-name)
     echo-in-repl
     th)))

(defun haskell-process-errors-warnings (session process buffer)
  "Trigger handling type errors or warnings."
  (cond
   ((haskell-process-consume
     process
     "\\(Module imports form a cycle:[ \n]+module [^ ]+ ([^)]+)[[:unibyte:][:nonascii:]]+?\\)\nFailed")
    (let ((err (match-string 1 buffer)))
      (when (string-match "module [`'‘‛]\\([^ ]+\\)['’`] (\\([^)]+\\))" err)
        (let* ((default-directory (haskell-session-current-dir session))
               (module (match-string 1 err))
               (file (match-string 2 err))
               (relative-file-name (file-relative-name file)))
          (haskell-interactive-show-load-message
           session
           'import-cycle
           module
           relative-file-name
           nil
           nil)
          (haskell-interactive-mode-compile-error
           session
           (format "%s:1:0: %s"
                   relative-file-name
                   err)))))
    t)
   ((haskell-process-consume
     process
     (concat "[\r\n]\\([A-Z]?:?[^ \r\n:][^:\n\r]+\\):\\([0-9()-:]+\\):"
             "[ \n\r]+\\([[:unibyte:][:nonascii:]]+?\\)\n[^ ]"))
    (haskell-process-set-response-cursor process
                                         (- (haskell-process-response-cursor process) 1))
    (let* ((buffer (haskell-process-response process))
           (file (match-string 1 buffer))
           (location (match-string 2 buffer))
           (error-msg (match-string 3 buffer))
           (warning (string-match "^Warning:" error-msg))
           (final-msg (format "%s:%s: %s"
                              (haskell-session-strip-dir session file)
                              location
                              error-msg)))
      (funcall (if warning
                   'haskell-interactive-mode-compile-warning
                 'haskell-interactive-mode-compile-error)
               session final-msg)
      (unless warning
        (haskell-mode-message-line final-msg))
      (haskell-process-trigger-suggestions
       session
       error-msg
       file
       (plist-get (haskell-process-parse-error final-msg) :line)))
    t)))

(defun haskell-process-parse-error (string)
  "Parse the line number from the error."
  (let ((span nil))
    (loop for regex
          in haskell-compilation-error-regexp-alist
          do (when (string-match (car regex) string)
               (setq span
                     (list :file (match-string 1 string)
                           :line (string-to-number (match-string 2 string))
                           :col (string-to-number (match-string 4 string))
                           :line2 (when (match-string 3 string)
                                    (string-to-number (match-string 3 string)))
                           :col2 (when (match-string 5 string)
                                   (string-to-number (match-string 5 string)))))))
    span))

(defun haskell-process-trigger-suggestions (session msg file line)
  "Trigger prompting to add any extension suggestions."
  (cond ((let ((case-fold-search nil))
           (or (string-match " -X\\([A-Z][A-Za-z]+\\)" msg)
               (string-match "Use \\([A-Z][A-Za-z]+\\) to permit this" msg)
               (string-match "Use \\([A-Z][A-Za-z]+\\) to allow" msg)
               (string-match "use \\([A-Z][A-Za-z]+\\)" msg)
               (string-match "You need \\([A-Z][A-Za-z]+\\)" msg)))
         (when haskell-process-suggest-language-pragmas
           (haskell-process-suggest-pragma session "LANGUAGE" (match-string 1 msg) file)))
        ((string-match " The \\(qualified \\)?import of[ ][‘`‛]\\([^ ]+\\)['’] is redundant" msg)
         (when haskell-process-suggest-remove-import-lines
           (haskell-process-suggest-remove-import session
                                                  file
                                                  (match-string 2 msg)
                                                  line)))
        ((string-match "Warning: orphan instance: " msg)
         (when haskell-process-suggest-no-warn-orphans
           (haskell-process-suggest-pragma session "OPTIONS" "-fno-warn-orphans" file)))
        ((or (string-match "against inferred type [‘`‛]\\[Char\\]['’]" msg)
             (string-match "with actual type [‘`‛]\\[Char\\]['’]" msg))
         (when haskell-process-suggest-overloaded-strings
           (haskell-process-suggest-pragma session "LANGUAGE" "OverloadedStrings" file)))
        ((string-match "^Not in scope: .*[‘`‛]\\(.+\\)['’]$" msg)
         (when haskell-process-suggest-hoogle-imports
           (haskell-process-suggest-hoogle-imports session msg file))
         (when haskell-process-suggest-haskell-docs-imports
           (haskell-process-suggest-haskell-docs-imports session msg file)))
        ((string-match "^[ ]+It is a member of the hidden package [‘`‛]\\(.+\\)['’].$" msg)
         (when haskell-process-suggest-add-package
           (haskell-process-suggest-add-package session msg)))))

(defun haskell-process-suggest-add-package (session msg)
  "Add the (matched) module to your cabal file."
  (let* ((suggested-package (match-string 1 msg))
         (package-name (replace-regexp-in-string "-[^-]+$" "" suggested-package))
         (version (progn (string-match "\\([^-]+\\)$" suggested-package)
                         (match-string 1 suggested-package)))
         (cabal-file (concat (haskell-session-name session)
                             ".cabal")))
    (when (y-or-n-p
           (format "Add `%s' to %s?"
                   package-name
                   cabal-file))
      (haskell-cabal-add-dependency package-name version nil t))))

(defun haskell-process-suggest-hoogle-imports (session msg file)
  "Given an out of scope identifier, Hoogle for that identifier,
and if a result comes back, suggest to import that identifier
now."
  (let* ((process (haskell-session-process session))
         (suggested-already (haskell-process-suggested-imports process))
         (ident (let ((i (match-string 1 msg)))
                  ;; Skip qualification.
                  (if (string-match "^[A-Za-z0-9_'.]+\\.\\(.+\\)$" i)
                      (match-string 1 i)
                    i)))
         (modules (haskell-process-hoogle-ident ident))
         (module
          (cond
           ((> (length modules) 1)
            (when (y-or-n-p (format "Identifier `%s' not in scope, choose module to import?"
                                    ident))
              (haskell-complete-module-read "Module: " modules)))
           ((= (length modules) 1)
            (let ((module (car modules)))
              (unless (member module suggested-already)
                (haskell-process-set-suggested-imports process (cons module suggested-already))
                (when (y-or-n-p (format "Identifier `%s' not in scope, import `%s'?"
                                        ident
                                        module))
                  module)))))))
    (when module
      (haskell-process-find-file session file)
      (save-excursion
        (goto-char (point-max))
        (haskell-navigate-imports)
        (insert (read-from-minibuffer "Import line: " (concat "import " module))
                "\n")
        (haskell-sort-imports)
        (haskell-align-imports)))))

(defun haskell-process-suggest-haskell-docs-imports (session msg file)
  "Given an out of scope identifier, haskell-docs search for that identifier,
and if a result comes back, suggest to import that identifier
now."
  (let* ((process (haskell-session-process session))
         (suggested-already (haskell-process-suggested-imports process))
         (ident (let ((i (match-string 1 msg)))
                  ;; Skip qualification.
                  (if (string-match "^[A-Za-z0-9_'.]+\\.\\(.+\\)$" i)
                      (match-string 1 i)
                    i)))
         (modules (haskell-process-haskell-docs-ident ident))
         (module
          (cond
           ((> (length modules) 1)
            (when (y-or-n-p (format "Identifier `%s' not in scope, choose module to import?"
                                    ident))
              (haskell-complete-module-read "Module: " modules)))
           ((= (length modules) 1)
            (let ((module (car modules)))
              (unless (member module suggested-already)
                (haskell-process-set-suggested-imports process (cons module suggested-already))
                (when (y-or-n-p (format "Identifier `%s' not in scope, import `%s'?"
                                        ident
                                        module))
                  module)))))))
    (when module
      (haskell-process-find-file session file)
      (save-excursion
        (goto-char (point-max))
        (haskell-navigate-imports)
        (insert (read-from-minibuffer "Import line: " (concat "import " module))
                "\n")
        (haskell-sort-imports)
        (haskell-align-imports)))))

(defun haskell-process-haskell-docs-ident (ident)
  "Search with haskell-docs for IDENT, returns a list of modules."
  (remove-if-not (lambda (a) (string-match "^[A-Z][A-Za-b0-9_'.]+$" a))
                 (split-string (shell-command-to-string (concat "haskell-docs --modules " ident))
                               "\n")))

(defun haskell-process-hoogle-ident (ident)
  "Hoogle for IDENT, returns a list of modules."
  (with-temp-buffer
    (let ((hoogle-error (call-process "hoogle" nil t nil "search" "--exact" ident)))
      (goto-char (point-min))
      (unless (or (/= 0 hoogle-error)
                  (looking-at "^No results found")
                  (looking-at "^package "))
        (while (re-search-forward "^\\([^ ]+\\).*$" nil t)
          (replace-match "\\1" nil nil))
        (remove-if (lambda (a) (string= "" a))
                   (split-string (buffer-string)
                                 "\n"))))))

(defun haskell-process-suggest-remove-import (session file import line)
  "Suggest removing or commenting out IMPORT on LINE."
  (let ((continue t)
        (first t))
    (case (read-event
           (propertize (format "%sThe import line `%s' is redundant. Remove? (y, n, c: comment out)  "
                               (if (not first)
                                   "Please answer n, y or c: "
                                 "")
                               import)
                       'face 'minibuffer-prompt))
      (?y
       (haskell-process-find-file session file)
       (save-excursion
         (goto-char (point-min))
         (forward-line (1- line))
         (goto-char (line-beginning-position))
         (delete-region (line-beginning-position)
                        (line-end-position))))
      (?n
       (message "Ignoring redundant import %s" import))
      (?c
       (haskell-process-find-file session file)
       (save-excursion
         (goto-char (point-min))
         (forward-line (1- line))
         (goto-char (line-beginning-position))
         (insert "-- "))))))

(defun haskell-process-suggest-pragma (session pragma extension file)
  "Suggest to add something to the top of the file."
  (let ((string  (format "{-# %s %s #-}" pragma extension)))
    (when (y-or-n-p (format "Add %s to the top of the file? " string))
      (haskell-process-find-file session file)
      (save-excursion
        (goto-char (point-min))
        (insert (concat string "\n"))))))

(defun haskell-process-find-file (session file)
  "Find the given file in the project."
  (find-file (cond ((file-exists-p (concat (haskell-session-current-dir session) "/" file))
                    (concat (haskell-session-current-dir session) "/" file))
                   ((file-exists-p (concat (haskell-session-cabal-dir session) "/" file))
                    (concat (haskell-session-cabal-dir session) "/" file))
                   (t file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Building the process

;;;###autoload
(defun haskell-process-start (session)
  "Start the inferior Haskell process."
  (let ((existing-process (get-process (haskell-session-name (haskell-session)))))
    (when (processp existing-process)
      (haskell-interactive-mode-echo session "Restarting process ...")
      (haskell-process-set (haskell-session-process session) 'is-restarting t)
      (delete-process existing-process)))
  (let ((process (or (haskell-session-process session)
                     (haskell-process-make (haskell-session-name session))))
        (old-queue (haskell-process-get (haskell-session-process session)
                                        'command-queue)))
    (haskell-session-set-process session process)
    (haskell-process-set-session process session)
    (haskell-process-set-cmd process nil)
    (haskell-process-set (haskell-session-process session) 'is-restarting nil)
    (let ((default-directory (haskell-session-cabal-dir session)))
      (haskell-session-pwd session)
      (haskell-process-set-process
       process
       (ecase haskell-process-type
         ('ghci
          (haskell-process-log
           (propertize (format "Starting inferior GHCi process %s ..."
                               haskell-process-path-ghci)
                       'face font-lock-comment-face))
          (apply #'start-process
                 (append (list (haskell-session-name session)
                               nil
                               haskell-process-path-ghci)
                         haskell-process-args-ghci)))
         ('cabal-repl
          (haskell-process-log
           (propertize
            (format "Starting inferior `cabal repl' process using %s ..."
                    haskell-process-path-cabal)
            'face font-lock-comment-face))

          (apply #'start-process
                 (append (list (haskell-session-name session)
                               nil
                               haskell-process-path-cabal)
                         '("repl") haskell-process-args-cabal-repl
                         (let ((target (haskell-session-target session)))
                           (if target (list target) nil)))))
         ('cabal-ghci
          (haskell-process-log
           (propertize
            (format "Starting inferior cabal-ghci process using %s ..."
                    haskell-process-path-cabal-ghci)
            'face font-lock-comment-face))
          (start-process (haskell-session-name session)
                         nil
                         haskell-process-path-cabal-ghci))
         ('cabal-dev
          (let ((dir (concat (haskell-session-cabal-dir session)
                             "/cabal-dev")))
            (haskell-process-log
             (propertize (format "Starting inferior cabal-dev process %s -s %s ..."
                                 haskell-process-path-cabal-dev
                                 dir)
                         'face font-lock-comment-face))
            (start-process (haskell-session-name session)
                           nil
                           haskell-process-path-cabal-dev
                           "ghci"
                           "-s"
                           dir))))))
    (progn (set-process-sentinel (haskell-process-process process) 'haskell-process-sentinel)
           (set-process-filter (haskell-process-process process) 'haskell-process-filter))
    (haskell-process-send-startup process)
    (unless (eq 'cabal-repl haskell-process-type) ;; "cabal repl" sets the proper CWD
      (haskell-process-change-dir session
                                  process
                                  (haskell-session-current-dir session)))
    (haskell-process-set process 'command-queue
                         (append (haskell-process-get (haskell-session-process session)
                                                      'command-queue)
                                 old-queue))
    process))

(defun haskell-process-clear ()
  "Clear the current process."
  (interactive)
  (haskell-process-reset (haskell-process))
  (haskell-process-set (haskell-process) 'command-queue nil))

(defun haskell-process-restart ()
  "Restart the inferior Haskell process."
  (interactive)
  (haskell-process-reset (haskell-process))
  (haskell-process-set (haskell-process) 'command-queue nil)
  (haskell-process-start (haskell-session)))

(defun haskell-kill-session-process (&optional session)
  "Kill the process."
  (interactive)
  (let* ((session (or session (haskell-session)))
         (existing-process (get-process (haskell-session-name session))))
    (when (processp existing-process)
      (haskell-interactive-mode-echo session "Killing process ...")
      (haskell-process-set (haskell-session-process session) 'is-restarting t)
      (delete-process existing-process))))

(defun haskell-process-make (name)
  "Make an inferior Haskell process."
  (list (cons 'name name)))

;;;###autoload
(defun haskell-process ()
  "Get the current process from the current session."
  (haskell-session-process (haskell-session)))

(defun haskell-process-interrupt ()
  "Interrupt the process (SIGINT)."
  (interactive)
  (interrupt-process (haskell-process-process (haskell-process))))

(defun haskell-process-cd (&optional not-interactive)
  "Change directory."
  (interactive)
  (let* ((session (haskell-session))
         (dir (haskell-session-pwd session t)))
    (haskell-process-log
     (propertize (format "Changing directory to %s ...\n" dir)
                 'face font-lock-comment-face))
    (haskell-process-change-dir session
                                (haskell-process)
                                dir)))

(defun haskell-session-pwd (session &optional change)
  "Prompt for the current directory."
  (or (unless change
        (haskell-session-get session 'current-dir))
      (progn (haskell-session-set-current-dir
              session
              (haskell-utils-read-directory-name
               (if change "Change directory: " "Set current directory: ")
               (or (haskell-session-get session 'current-dir)
                   (haskell-session-get session 'cabal-dir)
                   (if (buffer-file-name)
                       (file-name-directory (buffer-file-name))
                     "~/"))))
             (haskell-session-get session 'current-dir))))

(defun haskell-process-change-dir (session process dir)
  "Change the directory of the current process."
  (haskell-process-queue-command
   process
   (make-haskell-command
    :state (list session process dir)

    :go
    (lambda (state)
      (haskell-process-send-string
       (cadr state) (format ":cd %s" (caddr state))))

    :complete
    (lambda (state _)
      (haskell-session-set-current-dir (car state) (caddr state))
      (haskell-interactive-mode-echo (car state)
                                     (format "Changed directory: %s"
                                             (caddr state)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process communication

(defun haskell-process-send-startup (process)
  "Send the necessary start messages."
  (haskell-process-queue-command
   process
   (make-haskell-command
    :state process

    :go (lambda (process)
          (haskell-process-send-string process ":set prompt \"\\4\"")
          (haskell-process-send-string process "Prelude.putStrLn \"\"")
          (haskell-process-send-string process ":set -v1"))

    :live (lambda (process buffer)
            (when (haskell-process-consume
                   process
                   "^\*\*\* WARNING: \\(.+\\) is writable by someone else, IGNORING!$")
              (let ((path (match-string 1 buffer)))
                (haskell-session-modify
                 (haskell-process-session process)
                 'ignored-files
                 (lambda (files)
                   (remove-duplicates (cons path files) :test 'string=)))
                (haskell-interactive-mode-compile-warning
                 (haskell-process-session process)
                 (format "GHCi is ignoring: %s (run M-x haskell-process-unignore)"
                         path)))))

    :complete (lambda (process _)
                (haskell-interactive-mode-echo
                 (haskell-process-session process)
                 (concat (nth (random (length haskell-process-greetings))
                              haskell-process-greetings)
                         (when haskell-process-show-debug-tips
                           "
If I break, you can:
  1. Restart:           M-x haskell-process-restart
  2. Configure logging: C-h v haskell-process-log (useful for debugging)
  3. General config:    M-x customize-mode
  4. Hide these tips:   C-h v haskell-process-show-debug-tips")))))))

(defun haskell-process-sentinel (proc event)
  "The sentinel for the process pipe."
  (let ((session (haskell-process-project-by-proc proc)))
    (when session
      (let* ((process (haskell-session-process session)))
        (unless (haskell-process-restarting process)
          (haskell-process-log
           (propertize (format "Event: %S\n" event)
                       'face '((:weight bold))))
          (haskell-process-log
           (propertize "Process reset.\n"
                       'face font-lock-comment-face))
          (haskell-process-prompt-restart process))))))

(defun haskell-process-filter (proc response)
  "The filter for the process pipe."
  (let ((i 0))
    (loop for line in (split-string response "\n")
          do (haskell-process-log
              (concat (if (= i 0)
                          (propertize "<- " 'face font-lock-comment-face)
                        "   ")
                      (propertize line 'face 'haskell-interactive-face-compile-warning)))
          do (setq i (1+ i))))
  (let ((session (haskell-process-project-by-proc proc)))
    (when session
      (if (haskell-process-cmd (haskell-session-process session))
          (haskell-process-collect session
                                   response
                                   (haskell-session-process session))
        (haskell-interactive-mode-insert-garbage
         session
         (replace-regexp-in-string "\4" "" response))))))

(defun haskell-process-log (msg)
  "Write MSG to the process log (if enabled)."
  (when haskell-process-log
    (with-current-buffer (get-buffer-create "*haskell-process-log*")
      (goto-char (point-max))
      (insert msg "\n"))))

(defun haskell-process-project-by-proc (proc)
  "Find project by process."
  (find-if (lambda (project)
             (string= (haskell-session-name project)
                      (process-name proc)))
           haskell-sessions))

(defun haskell-process-collect (session response process)
  "Collect input for the response until receives a prompt."
  (haskell-process-set-response process
                                (concat (haskell-process-response process) response))
  (while (haskell-process-live-updates process))
  (when (string-match haskell-process-prompt-regex
                      (haskell-process-response process))
    (haskell-command-exec-complete
     (haskell-process-cmd process)
     (replace-regexp-in-string
      haskell-process-prompt-regex
      ""
      (haskell-process-response process)))
    (haskell-process-reset process)
    (haskell-process-trigger-queue process)))

(defun haskell-process-reset (process)
  "Reset the process's state, ready for the next send/reply."
  (progn (haskell-process-set-response-cursor process 0)
         (haskell-process-set-response process "")
         (haskell-process-set-cmd process nil)))

(defun haskell-process-consume (process regex)
  "Consume a regex from the response and move the cursor along if succeed."
  (when (string-match regex
                      (haskell-process-response process)
                      (haskell-process-response-cursor process))
    (haskell-process-set-response-cursor process (match-end 0))
    t))

(defun haskell-process-send-string (process string)
  "Try to send a string to the process's process. Ask to restart if it's not running."
  (let ((child (haskell-process-process process)))
    (if (equal 'run (process-status child))
        (let ((out (concat string "\n")))
          (haskell-process-log
           (propertize (concat (propertize "-> " 'face font-lock-comment-face)
                               (propertize string 'face font-lock-string-face))
                       'face '((:weight bold))))
          (process-send-string child out))
      (unless (haskell-process-restarting process)
        (haskell-process-prompt-restart process)))))

(defun haskell-process-prompt-restart (process)
  "Prompt to restart the died process."
  (let ((process-name (haskell-process-name process)))
    (if haskell-process-suggest-restart
        (case (read-event
               (propertize (format "The Haskell process `%s' has died. Restart? (y, n, l: show process log)"
                                   process-name)
                           'face 'minibuffer-prompt))
          (?y (haskell-process-start (haskell-process-session process)))
          (?l (let* ((response (haskell-process-response process))
                     (buffer (get-buffer "*haskell-process-log*")))
                (if buffer
                    (switch-to-buffer buffer)
                  (progn (switch-to-buffer (get-buffer-create "*haskell-process-log*"))
                         (insert response)))))
          (?n))
      (message (format "The Haskell process `%s' is dearly departed."
                       process-name)))))

(defun haskell-process-live-updates (process)
  "Process live updates."
  (haskell-command-exec-live (haskell-process-cmd process)
                             (haskell-process-response process)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Making commands

(defun haskell-process-queue-without-filters (process line)
  "Queue LINE to be sent to PROCESS without bothering to look at
the response."
  (haskell-process-queue-command
   process
   (make-haskell-command
    :state (cons process line)
    :go (lambda (state)
          (haskell-process-send-string (car state)
                                       (cdr state))))))

(defun haskell-process-queue-command (process command)
  "Add a command to the process command queue."
  (haskell-process-cmd-queue-add process command)
  (haskell-process-trigger-queue process))

(defun haskell-process-trigger-queue (process)
  "Trigger the next command in the queue to be ran if there is no current command."
  (if (and (haskell-process-process process)
           (process-live-p (haskell-process-process process)))
      (unless (haskell-process-cmd process)
        (let ((cmd (haskell-process-cmd-queue-pop process)))
          (when cmd
            (haskell-process-set-cmd process cmd)
            (haskell-command-exec-go cmd))))
    (progn (haskell-process-reset process)
           (haskell-process-set (haskell-process) 'command-queue nil)
           (haskell-process-prompt-restart process))))

(defun haskell-process-queue-flushed-p (process)
  "Return t if command queue has been completely processed."
  (not (or (haskell-process-cmd-queue process)
           (haskell-process-cmd process))))

(defun haskell-process-queue-flush (process)
  "Block till PROCESS' command queue has been completely processed.
This uses `accept-process-output' internally."
  (while (not (haskell-process-queue-flushed-p process))
    (haskell-process-trigger-queue process)
    (accept-process-output (haskell-process-process process) 1)))

(defun haskell-process-queue-sync-request (process reqstr)
  "Queue submitting REQSTR to PROCESS and return response blockingly."
  (let ((cmd (make-haskell-command
              :state (cons nil process)
              :go `(lambda (s) (haskell-process-send-string (cdr s) ,reqstr))
              :complete 'setcar)))
    (haskell-process-queue-command process cmd)
    (haskell-process-queue-flush process)
    (car-safe (haskell-command-state cmd))))

(defun haskell-process-get-repl-completions (process inputstr)
  "Perform `:complete repl ...' query for INPUTSTR using PROCESS."
  (let* ((reqstr (concat ":complete repl "
                         (haskell-str-literal-encode inputstr)))
         (rawstr (haskell-process-queue-sync-request process reqstr)))
    (if (string-prefix-p "unknown command " rawstr)
        (error "GHCi lacks `:complete' support")
      (let* ((s1 (split-string rawstr "\r?\n" t))
             (cs (mapcar #'haskell-str-literal-decode (cdr s1)))
             (h0 (car s1))) ;; "<cnt1> <cnt2> <quoted-str>"
        (unless (string-match "\\`\\([0-9]+\\) \\([0-9]+\\) \\(\".*\"\\)\\'" h0)
          (error "Invalid `:complete' response"))
        (let ((cnt1 (match-string 1 h0))
              (h1 (haskell-str-literal-decode (match-string 3 h0))))
          (unless (= (string-to-number cnt1) (length cs))
            (error "Lengths inconsistent in `:complete' reponse"))
          (cons h1 cs))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Accessing the process

(defun haskell-process-get (process key)
  "Get the PROCESS's KEY value.
Returns nil if KEY not set."
  (cdr (assq key process)))

(defun haskell-process-set (process key value)
  "Set the PROCESS's KEY to VALUE.
Returns newly set VALUE."
  (if process
      (let ((cell (assq key process)))
        (if cell
            (setcdr cell value)         ; modify cell in-place
          (setcdr process (cons (cons key value) (cdr process))) ; new cell
          value))
    (display-warning 'haskell-interactive
                     "`haskell-process-set' called with nil process")))

;; Wrappers using haskell-process-{get,set}

(defun haskell-process-set-sent-stdin (p v)
  "We've sent stdin, so let's not clear the output at the end."
  (haskell-process-set p 'sent-stdin v))

(defun haskell-process-sent-stdin-p (p)
  "Did we send any stdin to the process during evaluation?"
  (haskell-process-get p 'sent-stdin))

(defun haskell-process-set-suggested-imports (p v)
  "Remember what imports have been suggested, to avoid
re-asking about the same imports."
  (haskell-process-set p 'suggested-imported v))

(defun haskell-process-suggested-imports (p)
  "Get what modules have already been suggested and accepted."
  (haskell-process-get p 'suggested-imported))

(defun haskell-process-set-evaluating (p v)
  "Set status of evaluating to be on/off."
  (haskell-process-set p 'evaluating v))

(defun haskell-process-evaluating-p (p)
  "Set status of evaluating to be on/off."
  (haskell-process-get p 'evaluating))

(defun haskell-process-set-process (p v)
  "Set the process's inferior process."
  (haskell-process-set p 'inferior-process v))

(defun haskell-process-process (p)
  "Get the process child."
  (haskell-process-get p 'inferior-process))

(defun haskell-process-name (p)
  "Get the process name."
  (haskell-process-get p 'name))

(defun haskell-process-cmd (p)
  "Get the process's current command.
Return nil if no current command."
  (haskell-process-get p 'current-command))

(defun haskell-process-set-cmd (p v)
  "Set the process's current command."
  (haskell-process-set-evaluating p nil)
  (haskell-process-set-sent-stdin p nil)
  (haskell-process-set-suggested-imports p nil)
  (haskell-process-set p 'current-command v))

(defun haskell-process-response (p)
  "Get the process's current response."
  (haskell-process-get p 'current-response))

(defun haskell-process-session (p)
  "Get the process's current session."
  (haskell-process-get p 'session))

(defun haskell-process-set-response (p v)
  "Set the process's current response."
  (haskell-process-set p 'current-response v))

(defun haskell-process-set-session (p v)
  "Set the process's current session."
  (haskell-process-set p 'session v))

(defun haskell-process-response-cursor (p)
  "Get the process's current response cursor."
  (haskell-process-get p 'current-response-cursor))

(defun haskell-process-set-response-cursor (p v)
  "Set the process's response cursor."
  (haskell-process-set p 'current-response-cursor v))

;; low-level command queue operations

(defun haskell-process-restarting (process)
  "Is the PROCESS restarting?"
  (haskell-process-get process 'is-restarting))

(defun haskell-process-cmd-queue (process)
  "Get the PROCESS' command queue.
New entries get added to the end of the list. Use
`haskell-process-cmd-queue-add' and
`haskell-process-cmd-queue-pop' to modify the command queue."
  (haskell-process-get process 'command-queue))

(defun haskell-process-cmd-queue-add (process cmd)
  "Add CMD to end of PROCESS's command queue."
  (check-type cmd haskell-command)
  (haskell-process-set process
                       'command-queue
                       (append (haskell-process-cmd-queue process)
                               (list cmd))))

(defun haskell-process-cmd-queue-pop (process)
  "Pop the PROCESS' next entry from command queue.
Returns nil if queue is empty."
  (let ((queue (haskell-process-cmd-queue process)))
    (when queue
      (haskell-process-set process 'command-queue (cdr queue))
      (car queue))))

(defun haskell-process-unignore ()
  "Unignore any files that were specified as being ignored by the
  inferior GHCi process."
  (interactive)
  (let ((session (haskell-session))
        (changed nil))
    (if (null (haskell-session-get session
                                   'ignored-files))
        (message "Nothing to unignore!")
      (loop for file in (haskell-session-get session
                                             'ignored-files)
            do (case (read-event
                      (propertize (format "Set permissions? %s (y, n, v: stop and view file)"
                                          file)
                                  'face 'minibuffer-prompt))
                 (?y
                  (haskell-process-unignore-file session file)
                  (setq changed t))
                 (?v
                  (find-file file)
                  (return))))
      (when (and changed
                 (y-or-n-p "Restart GHCi process now? "))
        (haskell-process-restart)))))

(defun haskell-process-reload-devel-main ()
  "Reload the module `DevelMain' and then run
`DevelMain.update'. This is for doing live update of the code of
servers or GUI applications. Put your development version of the
program in `DevelMain', and define `update' to auto-start the
program on a new thread, and use the `foreign-store' package to
access the running context across :load/:reloads in GHCi."
  (interactive)
  (with-current-buffer (or (get-buffer "DevelMain.hs")
                           (if (y-or-n-p "You need to open a buffer named DevelMain.hs. Find now?")
                               (ido-find-file)
                             (error "No DevelMain.hs buffer.")))
    (let ((session (haskell-session)))
      (let ((process (haskell-process)))
        (haskell-process-queue-command
         process
         (make-haskell-command
          :state (list :session session
                       :process process
                       :buffer (current-buffer))
          :go (lambda (state)
                (haskell-process-send-string (plist-get state ':process)
                                             ":l DevelMain"))
          :live (lambda (state buffer)
                  (haskell-process-live-build (plist-get state ':process)
                                              buffer
                                              nil))
          :complete (lambda (state response)
                      (haskell-process-load-complete
                       (plist-get state ':session)
                       (plist-get state ':process)
                       response
                       nil
                       (plist-get state ':buffer)
                       (lambda (ok)
                         (when ok
                           (haskell-process-queue-without-filters
                            (haskell-process)
                            "DevelMain.update")
                           (message "DevelMain updated.")))))))))))

(defun haskell-process-unignore-file (session file)
  "

Note to Windows Emacs hackers:

chmod is how to change the mode of files in POSIX
systems. This will not work on your operating
system.

There is a command a bit like chmod called \"Calcs\"
that you can try using here:

http://technet.microsoft.com/en-us/library/bb490872.aspx

If it works, you can submit a patch to this
function and remove this comment.
"
  (shell-command (read-from-minibuffer "Permissions command: "
                                       (concat "chmod 700 "
                                               file)))
  (haskell-session-modify
   (haskell-session)
   'ignored-files
   (lambda (files)
     (remove-if (lambda (path)
                  (string= path file))
                files))))

(defun haskell-command-exec-go (command)
  "Call the command's go function."
  (let ((go-func (haskell-command-go command)))
    (when go-func
      (funcall go-func (haskell-command-state command)))))

(defun haskell-command-exec-complete (command response)
  "Call the command's complete function."
  (let ((comp-func (haskell-command-complete command)))
    (when comp-func
      (condition-case e
          (funcall comp-func
                   (haskell-command-state command)
                   response)
        (quit (message "Quit"))
        (error (message "Haskell process command errored with: %S" e))))))

(defun haskell-command-exec-live (command response)
  "Trigger the command's live updates callback."
  (let ((live-func (haskell-command-live command)))
    (when live-func
      (funcall live-func
               (haskell-command-state command)
               response))))

(defun haskell-process-cabal-macros ()
  "Send the cabal macros string."
  (interactive)
  (haskell-process-queue-without-filters (haskell-process)
                                         ":set -optP-include -optPdist/build/autogen/cabal_macros.h"))

(defun haskell-process-minimal-imports ()
  "Dump minimal imports."
  (interactive)
  (unless (> (save-excursion
               (goto-char (point-min))
               (haskell-navigate-imports-go)
               (point))
             (point))
    (goto-char (point-min))
    (haskell-navigate-imports-go))
  (haskell-process-queue-sync-request (haskell-process)
                                      ":set -ddump-minimal-imports")
  (haskell-process-load-file)
  (insert-file-contents-literally
   (concat (haskell-session-current-dir (haskell-session))
           "/"
           (haskell-guess-module-name)
           ".imports")))

(defvar interactive-haskell-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-l") 'haskell-process-load-or-reload)
    (define-key map (kbd "C-c C-t") 'haskell-process-do-type)
    (define-key map (kbd "C-c C-i") 'haskell-process-do-info)
    (define-key map (kbd "M-.") 'haskell-mode-jump-to-def-or-tag)
    (define-key map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
    (define-key map (kbd "C-c C-c") 'haskell-process-cabal-build)
    (define-key map (kbd "C-c c") 'haskell-process-cabal)
    (define-key map [?\C-c ?\C-b] 'haskell-interactive-switch)
    (define-key map [?\C-c ?\C-z] 'haskell-interactive-switch)
    map)
  "Keymap for using haskell-interactive-mode.")

;;;###autoload
(define-minor-mode interactive-haskell-mode
  "Minor mode for enabling haskell-process interaction."
  :lighter " Interactive"
  :keymap interactive-haskell-mode-map)

(provide 'haskell-process)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; haskell-process.el ends here
