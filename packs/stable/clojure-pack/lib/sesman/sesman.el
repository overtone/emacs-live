;;; sesman.el --- Generic Session Manager -*- lexical-binding: t -*-
;;
;; Copyright (C) 2018, Vitalie Spinu
;; Author: Vitalie Spinu
;; URL: https://github.com/vspinu/sesman
;; Keywords: process
;; Version: 0.3.3-DEV
;; Package-Requires: ((emacs "25"))
;; Keywords: processes, tools, vc
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is *NOT* part of GNU Emacs.
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
;;; Commentary:
;;
;; Sesman provides facilities for session management and interactive session
;; association with the current contexts (project, directory, buffers etc).  See
;; project's readme for more details.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'cl-generic)
(require 'seq)
(require 'subr-x)
(require 'vc)

(defgroup sesman nil
  "Generic Session Manager."
  :prefix "sesman-"
  :group 'tools
  :link '(url-link :tag "GitHub" "https://github.com/vspinu/sesman"))

(defface sesman-project-face
  '((default (:inherit font-lock-doc-face)))
  "Face used to mark projects."
  :group 'sesman)

(defface sesman-directory-face
  '((default (:inherit font-lock-type-face)))
  "Face used to mark directories."
  :group 'sesman)

(defface sesman-buffer-face
  '((default (:inherit font-lock-preprocessor-face)))
  "Face used to mark buffers."
  :group 'sesman)

(defcustom sesman-use-friendly-sessions t
  "If non-nil consider friendly sessions when looking for current sessions.
The definition of friendly sessions is system dependent but usually means
sessions running in dependent projects."
  :group 'sesman
  :type 'boolean
  :package-version '(sesman . "0.3.2"))

(defcustom sesman-follow-symlinks 'vc
  "When non-nil, follow symlinks during the file expansion.
When nil, don't follow symlinks. When 'vc, follow symlinks only when
`vc-follow-symlinks' is non-nil. When t, always follow symlinks."
  :group 'sesman
  :type '(choice (const :tag "Comply with `vc-follow-symlinks'" vc)
                 (const :tag "Don't follow symlinks"   nil)
                 (const :tag "Follow symlinks"         t))
  :package-version '(sesman . "0.3.3"))
(put 'sesman-follow-symlinks 'safe-local-variable (lambda (x) (memq x '(vc nil t))))

;; (defcustom sesman-disambiguate-by-relevance t
;;   "If t choose most relevant session in ambiguous situations, otherwise ask.
;; Ambiguity arises when multiple sessions are associated with current context.  By
;; default only projects could be associated with multiple sessions.  See
;; `sesman-single-link-contexts' in order to change that.  Relevance is decided by
;; system's implementation, see `sesman-more-relevant-p'."
;;   :group 'sesman
;;   :type 'boolean)

(defcustom sesman-single-link-context-types '(buffer)
  "List of context types to which at most one session can be linked."
  :group 'sesman
  :type '(repeat symbol)
  :package-version '(sesman . "0.1.0"))

;; FIXME:
;; (defcustom sesman-abbreviate-paths 2
;;   "Abbreviate paths to that many parents.
;; When set to nil, don't abbreviate directories."
;;   :group 'sesman
;;   :type '(choice number
;;                  (const :tag "Don't abbreviate" nil)))

(defvar sesman-sessions-hashmap (make-hash-table :test #'equal)
  "Hash-table of all sesman sessions.
Key is a cons (system-name . session-name).")

(defvar sesman-links-alist nil
  "An alist of all sesman links.
Each element is of the form (key cxt-type cxt-value) where
\"key\" is of the form (system-name . session-name). system-name
and cxt-type must be symbols.")

(defvar-local sesman-system nil
  "Name of the system managed by `sesman'.
Can be either a symbol, or a function returning a symbol.")
(put 'sesman-system 'permanent-local 't)



;; Internal Utilities

(defun sesman--on-C-u-u-sessions (system which)
  (cond
   ((null which)
    (let ((ses (sesman-current-session system)))
      (when ses
        (list ses))))
   ((or (equal which '(4)) (eq which 'linked))
    (sesman--linked-sessions system 'sort))
   ((or (equal which '(16)) (eq which 'all) (eq which t))
    (sesman--all-system-sessions system 'sort))
   ;; session itself
   ((and (listp which)
         (or (stringp (car which))
             (symbolp (car which))))
    (list which))
   ;; session name
   ((or (stringp which)
        (symbolp which)
        (gethash (cons system which) sesman-sessions-hashmap)))
   (t (error "Invalid which argument (%s)" which))))

(defun sesman--cap-system-name (system)
  (let ((name (symbol-name system)))
    (if (string-match-p "^[[:upper:]]" name)
        name
      (capitalize name))))

(defun sesman--least-specific-context (system)
  (seq-some (lambda (ctype)
              (when-let (val (sesman-context ctype system))
                (cons ctype val)))
            (reverse (sesman-context-types system))))

(defun sesman--link-session-interactively (session cxt-type cxt-val)
  (let ((system (sesman--system)))
    (unless cxt-type
      (let ((cxt (sesman--least-specific-context system)))
        (setq cxt-type (car cxt)
              cxt-val (cdr cxt))))
    (let ((cxt-name (symbol-name cxt-type)))
      (if (member cxt-type (sesman-context-types system))
          (let ((session (or session
                             (sesman-ask-for-session
                              system
                              (format "Link with %s %s: "
                                      cxt-name (sesman--abbrev-path-maybe
                                                (sesman-context cxt-type system)))
                              (sesman--all-system-sessions system 'sort)
                              'ask-new))))
            (sesman-link-session system session cxt-type cxt-val))
        (error (format "%s association not allowed for this system (%s)"
                       (capitalize cxt-name)
                       system))))))

;; FIXME: incorporate `sesman-abbreviate-paths'
(defun sesman--abbrev-path-maybe (obj)
  (if (stringp obj)
      (abbreviate-file-name obj)
    obj))

(defun sesman--system-in-buffer (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (if (functionp sesman-system)
        (funcall sesman-system)
      sesman-system)))

(defun sesman-get-system ()
  (if sesman-system
      (if (functionp sesman-system)
          (funcall sesman-system)
        sesman-system)
    (error "No `sesman-system' in buffer `%s'" (current-buffer))))

(defalias 'sesman--system #'sesman-get-system)

(defun sesman--linked-sessions (system &optional sort cxt-types)
  (let* ((system (or system (sesman--system)))
         (cxt-types (or cxt-types (sesman-context-types system))))
    ;; just in case some links are lingering due to user errors
    (sesman--clear-links)
    (delete-dups
     (mapcar (lambda (assoc)
               (gethash (car assoc) sesman-sessions-hashmap))
             (sesman-current-links system nil sort cxt-types)))))

(defun sesman--friendly-sessions (system &optional sort)
  (let ((sessions (seq-filter (lambda (ses) (sesman-friendly-session-p system ses))
                              (sesman--all-system-sessions system))))
    (if sort
        (sesman--sort-sessions system sessions)
      sessions)))

(defun sesman--all-system-sessions (&optional system sort)
  "Return a list of sessions registered with SYSTEM.
If SORT is non-nil, sort in relevance order."
  (let ((system (or system (sesman--system)))
        sessions)
    (maphash
     (lambda (k s)
       (when (eql (car k) system)
         (push s sessions)))
     sesman-sessions-hashmap)
    (if sort
        (sesman--sort-sessions system sessions)
      sessions)))

;; FIXME: make this a macro
(defun sesman--link-lookup-fn (&optional system ses-name cxt-type cxt-val x)
  (let ((system (or system (caar x)))
        (ses-name (or ses-name (cdar x)))
        (cxt-type (or cxt-type (nth 1 x)))
        (cxt-val (or cxt-val (nth 2 x))))
    (lambda (el)
      (and (or (null system) (eq (caar el) system))
           (or (null ses-name) (equal (cdar el) ses-name))
           (or (null cxt-type)
               (if (listp cxt-type)
                   (member (nth 1 el) cxt-type)
                 (eq (nth 1 el) cxt-type)))
           (or (null cxt-val) (equal (nth 2 el) cxt-val))))))

(defun sesman--unlink (x)
  (setq sesman-links-alist
        (seq-remove (sesman--link-lookup-fn nil nil nil nil x)
                    sesman-links-alist)))

(defun sesman--clear-links ()
  (setq sesman-links-alist
        (seq-filter (lambda (x)
                      (gethash (car x) sesman-sessions-hashmap))
                    sesman-links-alist)))

(defun sesman--format-session-objects (system session &optional sep)
  (let ((info (sesman-session-info system session)))
    (if (and (listp info)
             (keywordp (car info)))
        (let ((ses-name (car session))
              (sep (or sep " "))
              (strings (or (plist-get info :strings)
                           (mapcar (lambda (x) (format "%s" x))
                                   (plist-get info :objects)))))
          (mapconcat (lambda (str)
                       (replace-regexp-in-string ses-name "..." str nil t))
                     strings sep))
      (format "%s" info))))

(defun sesman--format-session (system ses &optional prefix)
  (format (propertize "%s%s [%s] linked-to %s" 'face 'bold)
          (or prefix "")
          (propertize (car ses) 'face 'bold)
          (propertize (sesman--format-session-objects system ses ", ") 'face 'italic)
          (sesman-grouped-links system ses t t)))

(defun sesman--format-link (link)
  (let* ((system (sesman--lnk-system-name link))
         (session (gethash (car link) sesman-sessions-hashmap)))
    (format "%s(%s) -> %s [%s]"
            (sesman--lnk-context-type link)
            (propertize (format "%s" (sesman--abbrev-path-maybe (sesman--lnk-value link)))
                        'face 'bold)
            (propertize (sesman--lnk-session-name link) 'face 'bold)
            (if session
                (sesman--format-session-objects system session)
              "invalid"))))

(defun sesman--ask-for-link (prompt links &optional ask-all)
  (let* ((name.keys (mapcar (lambda (link)
                              (cons (sesman--format-link link) link))
                            links))
         (name.keys (append name.keys
                            (when (and ask-all (> (length name.keys) 1))
                              '(("*all*")))))
         (nms (mapcar #'car name.keys))
         (sel (completing-read prompt nms nil t nil nil (car nms))))
    (cond ((string= sel "*all*")
           links)
          (ask-all
           (list (cdr (assoc sel name.keys))))
          (t
           (cdr (assoc sel name.keys))))))

(defun sesman--sort-sessions (system sessions)
  (seq-sort (lambda (x1 x2)
              (sesman-more-relevant-p system x1 x2))
            sessions))

(defun sesman--sort-links (system links)
  (seq-sort (lambda (x1 x2)
              (sesman-more-relevant-p system
                                      (gethash (car x1) sesman-sessions-hashmap)
                                      (gethash (car x2) sesman-sessions-hashmap)))
            links))

;; link data structure accessors
(defun sesman--lnk-system-name (lnk)
  (caar lnk))
(defun sesman--lnk-session-name (lnk)
  (cdar lnk))
(defun sesman--lnk-context-type (lnk)
  (cadr lnk))
(defun sesman--lnk-value (lnk)
  (nth 2 lnk))


;;; User Interface

(defun sesman-post-command-hook nil
  "Normal hook ran after every state-changing Sesman command.")

;;;###autoload
(defun sesman-start ()
  "Start a Sesman session."
  (interactive)
  (let ((system (sesman--system)))
    (message "Starting new %s session ..." system)
    (prog1 (sesman-start-session system)
      (run-hooks 'sesman-post-command-hook))))

;;;###autoload
(defun sesman-restart (&optional which)
  "Restart sesman session.
When WHICH is nil, restart the current session; when a single universal
argument or 'linked, restart all linked sessions; when a double universal
argument, t or 'all, restart all sessions. For programmatic use, WHICH can also
be a session or a name of the session, in which case that session is restarted."
  (interactive "P")
  (let* ((system (sesman--system))
         (sessions (sesman--on-C-u-u-sessions system which)))
    (if (null sessions)
        (message "No %s sessions found" system)
      (with-temp-message (format "Restarting %s %s %s"  system
                                 (if (= 1 (length sessions)) "session" "sessions")
                                 (mapcar #'car sessions))
        (mapc (lambda (s)
                (sesman-restart-session system s))
              sessions))
      ;; restarting is not guaranteed to finish here, but what can we do?
      (run-hooks 'sesman-post-command-hook))))

;;;###autoload
(defun sesman-quit (&optional which)
  "Terminate a Sesman session.
When WHICH is nil, kill only the current session; when a single universal
argument or 'linked, kill all linked sessions; when a double universal argument,
t or 'all, kill all sessions. For programmatic use, WHICH can also be a session
or a name of the session, in which case that session is killed."
  (interactive "P")
  (let* ((system (sesman--system))
         (sessions (sesman--on-C-u-u-sessions system which)))
    (if (null sessions)
        (message "No %s sessions found" system)
      (with-temp-message (format "Killing %s %s %s"  system
                                 (if (= 1 (length sessions)) "session" "sessions")
                                 (mapcar #'car sessions))
        (mapc (lambda (s)
                (sesman-unregister system s)
                (sesman-quit-session system s))
              sessions))
      (run-hooks 'sesman-post-command-hook))))

;;;###autoload
(defun sesman-info (&optional all)
  "Display info for all current sessions (`sesman-current-sessions').
In the resulting minibuffer display linked sessions are numbered and the
other (friendly) sessions are not. When ALL is non-nil, show info for all
sessions."
  (interactive "P")
  (let* ((system (sesman--system))
         (i 1)
         (sessions (if all
                       (sesman-sessions system t)
                     (sesman-current-sessions system)))
         (empty-prefix (if (> (length sessions) 1) "  " "")))
    (if sessions
        (message (mapconcat (lambda (ses)
                              (let ((prefix (if (sesman-relevant-session-p system ses)
                                                (prog1 (format "%d " i)
                                                  (setq i (1+ i)))
                                              empty-prefix)))
                                (sesman--format-session system ses prefix)))
                            sessions
                            "\n"))
      (message "No %s%s sessions"
               (if all "" "current ")
               system))))

;;;###autoload
(defun sesman-link-with-buffer (&optional buffer session)
  "Ask for SESSION and link with BUFFER.
BUFFER defaults to current buffer. On universal argument, or if BUFFER is 'ask,
ask for buffer."
  (interactive "P")
  (let ((buf (if (or (eq buffer 'ask)
                     (equal buffer '(4)))
                 (let ((this-system (sesman--system)))
                   (read-buffer "Link buffer: " (current-buffer) t
                                (lambda (buf-cons)
                                  (equal this-system
                                         (sesman--system-in-buffer (cdr buf-cons))))))
               (or buffer (current-buffer)))))
    (sesman--link-session-interactively session 'buffer buf)))

;;;###autoload
(defun sesman-link-with-directory (&optional dir session)
  "Ask for SESSION and link with DIR.
DIR defaults to `default-directory'. On universal argument, or if DIR is 'ask,
ask for directory."
  (interactive "P")
  (let ((dir (if (or (eq dir 'ask)
                     (equal dir '(4)))
                 (read-directory-name "Link directory: ")
               (or dir default-directory))))
    (sesman--link-session-interactively session 'directory dir)))

;;;###autoload
(defun sesman-link-with-project (&optional project session)
  "Ask for SESSION and link with PROJECT.
PROJECT defaults to current project. On universal argument, or if PROJECT is
'ask, ask for the project. SESSION defaults to the current session."
  (interactive "P")
  (let* ((system (sesman--system))
         (project (expand-file-name
                   (if (or (eq project 'ask)
                           (equal project '(4)))
                       ;; FIXME: should be a completion over all known projects for this system
                       (read-directory-name "Project: " (sesman-project system))
                     (or project (sesman-project system))))))
    (sesman--link-session-interactively session 'project project)))

;;;###autoload
(defun sesman-link-with-least-specific (&optional session)
  "Ask for SESSION and link with the least specific context available.
Normally the least specific context is the project. If not in a project, link
with the `default-directory'. If `default-directory' is nil, link with current
buffer."
  (interactive "P")
  (sesman--link-session-interactively session nil nil))

;;;###autoload
(defun sesman-unlink (&optional links)
  "Break sesman LINKS.
If LINKS is nil, ask interactively for a link. With a prefix argument break all
links."
  (interactive)
  (mapc #'sesman--unlink (or (when current-prefix-arg
                               (sesman-current-links (sesman--system)))
                             links
                             (sesman--ask-for-link "Unlink: "
                                                   (or (sesman-current-links (sesman--system))
                                                       (user-error "No %s links found" (sesman--system)))
                                                   'ask-all)))
  (run-hooks 'sesman-post-command-hook))

(declare-function sesman-browser "sesman-browser")
;;;###autoload (autoload 'sesman-map "sesman" "Session management prefix keymap." t 'keymap)
(defvar sesman-map
  (let (sesman-map)
    (define-prefix-command 'sesman-map)
    (define-key sesman-map (kbd "C-i") #'sesman-info)
    (define-key sesman-map (kbd   "i") #'sesman-info)
    (define-key sesman-map (kbd "C-w") #'sesman-browser)
    (define-key sesman-map (kbd   "w") #'sesman-browser)
    (define-key sesman-map (kbd "C-s") #'sesman-start)
    (define-key sesman-map (kbd   "s") #'sesman-start)
    (define-key sesman-map (kbd "C-r") #'sesman-restart)
    (define-key sesman-map (kbd   "r") #'sesman-restart)
    (define-key sesman-map (kbd "C-q") #'sesman-quit)
    (define-key sesman-map (kbd   "q") #'sesman-quit)
    (define-key sesman-map (kbd "C-l") #'sesman-link-with-least-specific)
    (define-key sesman-map (kbd   "l") #'sesman-link-with-least-specific)
    (define-key sesman-map (kbd "C-b") #'sesman-link-with-buffer)
    (define-key sesman-map (kbd   "b") #'sesman-link-with-buffer)
    (define-key sesman-map (kbd "C-d") #'sesman-link-with-directory)
    (define-key sesman-map (kbd   "d") #'sesman-link-with-directory)
    (define-key sesman-map (kbd "C-p") #'sesman-link-with-project)
    (define-key sesman-map (kbd   "p") #'sesman-link-with-project)
    (define-key sesman-map (kbd "C-u") #'sesman-unlink)
    (define-key sesman-map (kbd "  u") #'sesman-unlink)
    sesman-map)
  "Session management prefix keymap.")

(defvar sesman-menu
  '("Sesman"
    ["Show Session Info" sesman-info]
    "--"
    ["Start" sesman-start]
    ["Restart" sesman-restart :active (sesman-current-session (sesman--system))]
    ["Quit" sesman-quit :active (sesman-current-session (sesman--system))]
    "--"
    ["Link with Buffer" sesman-link-with-buffer :active (sesman-current-session (sesman--system))]
    ["Link with Directory" sesman-link-with-directory :active (sesman-current-session (sesman--system))]
    ["Link with Project" sesman-link-with-project :active (sesman-current-session (sesman--system))]
    ["Unlink" sesman-unlink :active (sesman-current-session (sesman--system))]
    "--"
    ["Browser" sesman-browser :active (sesman-current-session (sesman--system))])
  "Sesman Menu.")

(defun sesman-install-menu (map)
  "Install `sesman-menu' into MAP."
  (easy-menu-do-define 'sesman-menu-open
                       map
                       (get 'sesman-menu 'variable-documentation)
                       sesman-menu))


;;; System Generic

(cl-defgeneric sesman-start-session (system)
  "Start and return SYSTEM SESSION.")

(cl-defgeneric sesman-quit-session (system session)
  "Terminate SYSTEM SESSION.")

(cl-defgeneric sesman-restart-session (system session)
  "Restart SYSTEM SESSION.
By default, calls `sesman-quit-session' and then
`sesman-start-session'."
  (let ((old-name (car session)))
    (sesman-quit-session system session)
    (let ((new-session (sesman-start-session system)))
      (setcar new-session old-name))))

(cl-defgeneric sesman-session-info (_system session)
  "Return a plist with :objects key containing user \"visible\" objects.
Optional :strings value is a list of string representations of objects. Optional
:map key is a local keymap to place on every object in the session browser.
Optional :buffers is a list of buffers which will be used for navigation from
the session browser. If :buffers is missing, buffers from :objects are used
instead."
  (list :objects (cdr session)))

(cl-defgeneric sesman-project (_system)
  "Retrieve project root in current directory (`default-directory') for SYSTEM.
Return a string or nil if no project has been found."
  nil)

(cl-defgeneric sesman-more-relevant-p (_system session1 session2)
  "Return non-nil if SESSION1 should be sorted before SESSION2.
By default, sort by session name. Systems should overwrite this method to
provide a more meaningful ordering. If your system objects are buffers you can
use `sesman-more-recent-p' utility in this method."
  (not (string-greaterp (car session1) (car session2))))

(cl-defgeneric sesman-friendly-session-p (_system _session)
  "Return non-nil if SESSION is a friendly session in current context.
The \"friendship\" is system dependent but usually means sessions running in
dependent projects. Unless SYSTEM has defined a method for this generic, there
are no friendly sessions."
  nil)

(cl-defgeneric sesman-context-types (_system)
  "Return a list of context types understood by SYSTEM.
Contexts must be sorted from most specific to least specific."
  '(buffer directory project))


;;; System API

(defun sesman-session (system session-name)
  "Retrieve SYSTEM's session with SESSION-NAME from global hash."
  (let ((system (or system (sesman--system))))
    (gethash (cons system session-name) sesman-sessions-hashmap)))

(defun sesman-sessions (system &optional sort type cxt-types)
  "Return a list of sessions registered with SYSTEM.
When TYPE is either 'all or nil return all sessions registered with the SYSTEM,
when 'linked, only linked to the current context sessions, when 'friendly - only
friendly sessions. If SORT is non-nil, sessions are sorted in the relevance
order with linked sessions leading the list. CXT-TYPES is a list of context
types to consider for linked sessions."
  (let ((system (or system (sesman--system))))
    (cond
     ((eq type 'linked)
      (sesman--linked-sessions system sort cxt-types))
     ((eq type 'friendly)
      (sesman--friendly-sessions system sort))
     ((memq type '(all nil))
      (if sort
          (delete-dups
           (append (sesman--linked-sessions system 'sort cxt-types)
                   (sesman--all-system-sessions system 'sort)))
        (sesman--all-system-sessions system)))
     (t (error "Invalid session TYPE argument %s" type)))))

(defun sesman-current-sessions (system &optional cxt-types)
  "Return a list of SYSTEM sessions active in the current context.
Sessions are ordered by the relevance order and linked sessions come first. If
`sesman-use-friendly-sessions' current sessions consist of linked and friendly
sessions, otherwise only of linked sessions. CXT-TYPES is a list of context
types to consider. Defaults to the list returned from `sesman-context-types'."
  (if sesman-use-friendly-sessions
      (delete-dups
       (append (sesman--linked-sessions system 'sort cxt-types)
               (sesman--friendly-sessions system 'sort)))
    (sesman--linked-sessions system 'sort cxt-types)))

(defun sesman-current-session (system &optional cxt-types)
  "Get the most relevant current session for the SYSTEM.
CXT-TYPES is a list of context types to consider."
  (or (car (sesman--linked-sessions system 'sort cxt-types))
      (car (sesman--friendly-sessions system 'sort))))

(defun sesman-ensure-session (system &optional cxt-types)
  "Get the most relevant linked session for SYSTEM or throw if none exists.
CXT-TYPES is a list of context types to consider."
  (or (sesman-current-session system cxt-types)
      (user-error "No linked %s sessions" system)))

(defun sesman-has-sessions-p (system)
  "Return t if there is at least one session registered with SYSTEM."
  (let ((system (or system (sesman--system)))
        (found))
    (condition-case nil
        (maphash (lambda (k _)
                   (when (eq (car k) system)
                     (setq found t)
                     (throw 'found nil)))
                 sesman-sessions-hashmap)
      (error))
    found))

(defvar sesman--select-session-history nil)
(defun sesman-ask-for-session (system prompt &optional sessions ask-new ask-all)
  "Ask for a SYSTEM session with PROMPT.
SESSIONS defaults to value returned from `sesman-sessions'.  If
ASK-NEW is non-nil, offer *new* option to start a new session.  If
ASK-ALL is non-nil offer *all* option.  If ASK-ALL is non-nil,
return a list of sessions, otherwise a single session."
  (let* ((sessions (or sessions (sesman-sessions system)))
         (name.syms (mapcar (lambda (s)
                              (let ((name (car s)))
                                (cons (if (symbolp name) (symbol-name name) name)
                                      name)))
                            sessions))
         (nr (length name.syms))
         (syms (if (and (not ask-new) (= nr 0))
                   (error "No %s sessions found" system)
                 (append name.syms
                         (when ask-new '(("*new*")))
                         (when (and ask-all (> nr 1))
                           '(("*all*"))))))
         (def (caar syms))
         ;; (def (if (assoc (car sesman--select-session-history) syms)
         ;;          (car sesman--select-session-history)
         ;;        (caar syms)))
         (sel (completing-read
               prompt (mapcar #'car syms) nil t nil 'sesman--select-session-history def)))
    (cond
     ((string= sel "*new*")
      (let ((ses (sesman-start-session system)))
        (message "Started %s" (car ses))
        (if ask-all (list ses) ses)))
     ((string= sel "*all*")
      sessions)
     (t
      (let* ((sym (cdr (assoc sel syms)))
             (ses (assoc sym sessions)))
        (if ask-all (list ses) ses))))))

(defvar sesman--cxt-abbrevs '(buffer "buf" project "proj" directory "dir"))
(defun sesman--format-context (cxt-type cxt-val extra-face)
  (let* ((face (intern (format "sesman-%s-face" cxt-type)))
         (short-type (propertize (or (plist-get sesman--cxt-abbrevs cxt-type)
                                     (symbol-value cxt-type))
                                 'face (list (if (facep face)
                                                 face
                                               'font-lock-function-name-face)
                                             extra-face))))
    (concat short-type
            (propertize (format "(%s)" cxt-val)
                        'face extra-face))))

(defun sesman-grouped-links (system session &optional current-first as-string)
  "Retrieve all links for SYSTEM's SESSION from the global `sesman-links-alist'.
Return an alist of the form

   ((buffer buffers..)
    (directory directories...)
    (project projects...)).

When `CURRENT-FIRST' is non-nil, a cons of two lists as above is returned with
car containing links relevant in current context and cdr all other links. If
AS-STRING is non-nil, return an equivalent string representation."
  (let* ((system (or system (sesman--system)))
         (session (or session (sesman-current-session system)))
         (ses-name (car session))
         (links (thread-last sesman-links-alist
                  (seq-filter (sesman--link-lookup-fn system ses-name))
                  (sesman--sort-links system)
                  (reverse)))
         (out (mapcar (lambda (x) (list x))
                      (sesman-context-types system)))
         (out-rel (when current-first
                    (copy-alist out))))
    (mapc (lambda (link)
            (let* ((type (sesman--lnk-context-type link))
                   (entry (if (and current-first
                                   (sesman-relevant-link-p link))
                              (assoc type out-rel)
                            (assoc type out))))
              (when entry
                (setcdr entry (cons link (cdr entry))))))
          links)
    (let ((out (delq nil (mapcar (lambda (el) (and (cdr el) el)) out)))
          (out-rel (delq nil (mapcar (lambda (el) (and (cdr el) el)) out-rel))))
      (if as-string
          (let ((fmt-fn (lambda (typed-links)
                          (let* ((type (car typed-links)))
                            (mapconcat (lambda (lnk)
                                         (let ((val (sesman--abbrev-path-maybe
                                                     (sesman--lnk-value lnk))))
                                           (sesman--format-context type val 'italic)))
                                       (cdr typed-links)
                                       ", ")))))
            (if out-rel
                (concat (mapconcat fmt-fn out-rel ", ")
                        (when out " | ")
                        (mapconcat fmt-fn out ", "))
              (mapconcat fmt-fn out ", ")))
        (if current-first
            (cons out-rel out)
          out)))))

(defun sesman-link-session (system session &optional cxt-type cxt-val)
  "Link SYSTEM's SESSION to context give by CXT-TYPE and CXT-VAL.
If CXT-TYPE is nil, use the least specific type available in the current
context. If CXT-TYPE is non-nil, and CXT-VAL is not given, retrieve it with
`sesman-context'. See also `sesman-link-with-project',
`sesman-link-with-directory' and `sesman-link-with-buffer'."
  (let* ((ses-name (or (car-safe session)
                       (error "SESSION must be a headed list")))
         (cxt-val (or cxt-val
                      (or (if cxt-type
                              (sesman-context cxt-type system)
                            (let ((cxt (sesman--least-specific-context system)))
                              (setq cxt-type (car cxt))
                              (cdr cxt)))
                          (error "No local context of type %s" cxt-type))))
         (cxt-val (if (stringp cxt-val)
                      (expand-file-name cxt-val)
                    cxt-val))
         (key (cons system ses-name))
         (link (list key cxt-type cxt-val)))
    (if (member cxt-type sesman-single-link-context-types)
        (thread-last sesman-links-alist
          (seq-remove (sesman--link-lookup-fn system nil cxt-type cxt-val))
          (cons link)
          (setq sesman-links-alist))
      (unless (seq-filter (sesman--link-lookup-fn system ses-name cxt-type cxt-val)
                          sesman-links-alist)
        (setq sesman-links-alist (cons link sesman-links-alist))))
    (run-hooks 'sesman-post-command-hook)
    link))

(defun sesman-links (system &optional session-or-name cxt-types sort)
  "Retrieve all links for SYSTEM, SESSION-OR-NAME and CXT-TYPES.
SESSION-OR-NAME can be either a session or a name of the session. If SORT is
non-nil links are sorted in relevance order and `sesman-current-links' lead the
list, otherwise links are returned in the creation order."
  (let* ((ses-name (if (listp session-or-name)
                       (car session-or-name)
                     session-or-name))
         (lfn (sesman--link-lookup-fn system ses-name cxt-types)))
    (if sort
        (delete-dups (append
                      (sesman-current-links system ses-name)
                      (sesman--sort-links system (seq-filter lfn sesman-links-alist))))
      (seq-filter lfn sesman-links-alist))))

(defun sesman-current-links (system &optional session-or-name sort cxt-types)
  "Retrieve all active links in current context for SYSTEM and SESSION-OR-NAME.
SESSION-OR-NAME can be either a session or a name of the session. CXT-TYPES is a
list of context types to consider. Returned links are a subset of
`sesman-links-alist' sorted in order of relevance if SORT is non-nil."
  ;; mapcan is a built-in in 26.1; don't want to require cl-lib for one function
  (let ((ses-name (if (listp session-or-name)
                      (car session-or-name)
                    session-or-name)))
    (seq-mapcat
     (lambda (cxt-type)
       (let* ((lfn (sesman--link-lookup-fn system ses-name cxt-type))
              (links (seq-filter (lambda (l)
                                   (and (funcall lfn l)
                                        (sesman-relevant-context-p cxt-type (sesman--lnk-value l))))
                                 sesman-links-alist)))
         (if sort
             (sesman--sort-links system links)
           links)))
     (or cxt-types (sesman-context-types system)))))

(defun sesman-has-links-p (system &optional cxt-types)
  "Return t if there is at least one linked session.
CXT-TYPES defaults to `sesman-context-types' for current SYSTEM."
  (let ((cxt-types (or cxt-types (sesman-context-types system)))
        (found))
    (condition-case nil
        (mapc (lambda (l)
                (when (eq system (sesman--lnk-system-name l))
                  (let ((cxt (sesman--lnk-context-type l)))
                    (when (and (member cxt cxt-types)
                               (sesman-relevant-context-p cxt (sesman--lnk-value l)))
                      (setq found t)
                      (throw 'found nil)))))
              sesman-links-alist)
      (error))
    found))

(defun sesman-register (system session)
  "Register SESSION into `sesman-sessions-hashmap' and `sesman-links-alist'.
SYSTEM defaults to current system.  If a session with same name is already
registered in `sesman-sessions-hashmap', change the name by appending \"#1\",
\"#2\" ... to the name.  This function should be called by system-specific
connection initializers (\"run-xyz\", \"xyz-jack-in\" etc.)."
  (let* ((system (or system (sesman--system)))
         (ses-name (car session))
         (ses-name0 (car session))
         (i 1))
    (while (sesman-session system ses-name)
      (setq ses-name (format "%s#%d" ses-name0 i)
            i (1+ i)))
    (setq session (cons ses-name (cdr session)))
    (puthash (cons system ses-name) session sesman-sessions-hashmap)
    (sesman-link-session system session)
    session))

(defun sesman-unregister (system session)
  "Unregister SESSION.
SYSTEM defaults to current system.  Remove session from
`sesman-sessions-hashmap' and `sesman-links-alist'."
  (let ((ses-key (cons system (car session))))
    (remhash ses-key sesman-sessions-hashmap)
    (sesman--clear-links)
    session))

(defun sesman-add-object (system session-name object &optional allow-new)
  "Add (destructively) OBJECT to session SESSION-NAME of SYSTEM.
If ALLOW-NEW is nil and session with SESSION-NAME does not exist
throw an error, otherwise register a new session with
session (list SESSION-NAME OBJECT)."
  (let* ((system (or system (sesman--system)))
         (session (sesman-session system session-name)))
    (if session
        (setcdr session (cons object (cdr session)))
      (if allow-new
          (sesman-register system (list session-name object))
        (error "%s session '%s' does not exist"
               (sesman--cap-system-name system) session-name)))))

(defun sesman-remove-object (system session-name object &optional auto-unregister no-error)
  "Remove (destructively) OBJECT from session SESSION-NAME of SYSTEM.
If SESSION-NAME is nil, retrieve the session with
`sesman-session-for-object'.  If OBJECT is the last object in sesman
session, `sesman-unregister' the session.  If AUTO-UNREGISTER is non-nil
unregister sessions of length 0 and remove all the links with the session.
If NO-ERROR is non-nil, don't throw an error if OBJECT is not found in any
session.  This is useful if there are several \"concurrent\" parties which
can remove the object."
  (let* ((system (or system (sesman--system)))
         (session (if session-name
                      (sesman-session system session-name)
                    (sesman-session-for-object system object no-error)))
         (new-session (delete object session)))
    (cond ((null new-session))
          ((= (length new-session) 1)
           (when auto-unregister
             (sesman-unregister system session)))
          (t
           (puthash (cons system (car session)) new-session sesman-sessions-hashmap)))))

(defun sesman-session-for-object (system object &optional no-error)
  "Retrieve SYSTEM session which contains OBJECT.
When NO-ERROR is non-nil, don't throw an error if OBJECT is not part of any
session.  In such case, return nil."
  (let* ((system (or system (sesman--system)))
         (sessions (sesman--all-system-sessions system)))
    (or (seq-find (lambda (ses)
                    (seq-find (lambda (x) (equal object x)) (cdr ses)))
                  sessions)
        (unless no-error
          (error "%s is not part of any %s sessions"
                 object system)))))

(defun sesman-session-name-for-object (system object &optional no-error)
  "Retrieve the name of the SYSTEM's session containing OBJECT.
When NO-ERROR is non-nil, don't throw an error if OBJCECT is not part of
any session.  In such case, return nil."
  (car (sesman-session-for-object system object no-error)))

(defun sesman-more-recent-p (bufs1 bufs2)
  "Return t if BUFS1 is more recent than BUFS2.
BUFS1 and BUFS2 are either buffers or lists of buffers.  When lists of
buffers, most recent buffers from each list are considered.  To be used
primarily in `sesman-more-relevant-p' methods when session objects are
buffers."
  (let ((bufs1 (if (bufferp bufs1) (list bufs1) bufs1))
        (bufs2 (if (bufferp bufs2) (list bufs2) bufs2)))
    (eq 1 (seq-some (lambda (b)
                      (if (member b bufs1)
                          1
                        (when (member b bufs2)
                          -1)))
                    (buffer-list)))))

;; path caching because file-truename is very slow
(defvar sesman--path-cache (make-hash-table :test #'equal))
(defun sesman-expand-path (path)
  "Expand PATH with optionally follow symlinks.
Whether symlinks are followed is controlled by `sesman-follow-symlinks' custom
variable. Always return the expansion without the trailing directory slash."
  (directory-file-name
   (if sesman-follow-symlinks
       (let ((true-name (or (gethash path sesman--path-cache)
                            (puthash path (file-truename path) sesman--path-cache))))
         (if (or (eq sesman-follow-symlinks t)
                 vc-follow-symlinks)
             true-name
           ;; sesman-follow-symlinks is 'vc but vc-follow-symlinks is nil
           (expand-file-name path)))
     (expand-file-name path))))


;;; Contexts

(cl-defgeneric sesman-context (_cxt-type _system)
  "Given SYSTEM and context type CXT-TYPE return the context.")
(cl-defmethod sesman-context ((_cxt-type (eql buffer)) _system)
  "Return current buffer."
  (current-buffer))
(cl-defmethod sesman-context ((_cxt-type (eql directory)) _system)
  "Return current directory."
  (sesman-expand-path default-directory))
(cl-defmethod sesman-context ((_cxt-type (eql project)) system)
  "Return current project."
  (let* ((default-directory (sesman-expand-path default-directory))
         (proj (or
                (sesman-project (or system (sesman--system)))
                ;; Normally we would use (project-roots (project-current)) but currently
                ;; project-roots fails on nil and doesn't work on custom `('foo .
                ;; "path/to/project"). So, use vc as a fallback and don't use project.el at
                ;; all for now.
                ;; NB: `vc-root-dir' doesn't work from symlinked files. Emacs Bug?
                (vc-root-dir))))
    (when proj
      (expand-file-name proj))))

(cl-defgeneric sesman-relevant-context-p (_cxt-type cxt)
  "Non-nil if context CXT is relevant to current context of type CXT-TYPE.")
(cl-defmethod sesman-relevant-context-p ((_cxt-type (eql buffer)) buf)
  "Non-nil if BUF is `current-buffer'."
  (eq (current-buffer) buf))
(cl-defmethod sesman-relevant-context-p ((_cxt-type (eql directory)) dir)
  "Non-nil if DIR is the parent or equals the `default-directory'."
  (when (and dir default-directory)
    (string-match-p (concat "^" (sesman-expand-path dir))
                    (sesman-expand-path default-directory))))
(cl-defmethod sesman-relevant-context-p ((_cxt-type (eql project)) proj)
  "Non-nil if PROJ is the parent or equal to the `default-directory'."
  (when (and proj default-directory)
    (string-match-p (concat "^" (sesman-expand-path proj))
                    (sesman-expand-path default-directory))))

(defun sesman-relevant-link-p (link &optional cxt-types)
  "Return non-nil if LINK is relevant to the current context.
If CXT-TYPES is non-nil, only check relevance for those contexts."
  (when (or (null cxt-types)
            (member (sesman--lnk-context-type link) cxt-types))
    (sesman-relevant-context-p
     (sesman--lnk-context-type link)
     (sesman--lnk-value link))))

(defun sesman-relevant-session-p (system session &optional cxt-types)
  "Return non-nil if SYSTEM's SESSION is relevant to the current context.
If CXT-TYPES is non-nil, only check relevance for those contexts."
  (seq-some #'sesman-relevant-link-p
            (sesman-links system session cxt-types)))

(define-obsolete-function-alias 'sesman-linked-sessions 'sesman--linked-sessions "v0.3.2")

(provide 'sesman)

;;; sesman.el ends here
