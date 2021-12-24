;;; magit-clone.el --- clone a repository  -*- lexical-binding: t -*-

;; Copyright (C) 2008-2020  The Magit Project Contributors
;;
;; You should have received a copy of the AUTHORS.md file which
;; lists all contributors.  If not, see http://magit.vc/authors.

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; Magit is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; This library implements clone commands.

;;; Code:

(require 'magit)

;;; Options

(defcustom magit-clone-set-remote-head nil
  "Whether cloning creates the symbolic-ref `<remote>/HEAD'."
  :package-version '(magit . "2.4.2")
  :group 'magit-commands
  :type 'boolean)

(defcustom magit-clone-set-remote.pushDefault 'ask
  "Whether to set the value of `remote.pushDefault' after cloning.

If t, then set without asking.  If nil, then don't set.  If
`ask', then ask."
  :package-version '(magit . "2.4.0")
  :group 'magit-commands
  :type '(choice (const :tag "set" t)
                 (const :tag "ask" ask)
                 (const :tag "don't set" nil)))

(defcustom magit-clone-default-directory nil
  "Default directory to use when `magit-clone' reads destination.
If nil (the default), then use the value of `default-directory'.
If a directory, then use that.  If a function, then call that
with the remote url as only argument and use the returned value."
  :package-version '(magit . "2.90.0")
  :group 'magit-commands
  :type '(choice (const     :tag "value of default-directory")
                 (directory :tag "constant directory")
                 (function  :tag "function's value")))

(defcustom magit-clone-always-transient nil
  "Whether `magit-clone' always acts as a transient prefix command.
If nil, then a prefix argument has to be used to show the transient
popup instead of invoking the default suffix `magit-clone-regular'
directly."
  :package-version '(magit . "3.0.0")
  :group 'magit-commands
  :type 'boolean)

(defcustom magit-clone-name-alist
  '(("\\`\\(?:github:\\|gh:\\)?\\([^:]+\\)\\'" "github.com" "github.user")
    ("\\`\\(?:gitlab:\\|gl:\\)\\([^:]+\\)\\'"  "gitlab.com" "gitlab.user"))
  "Alist mapping repository names to repository urls.

Each element has the form (REGEXP HOSTNAME USER).  When the user
enters a name when a cloning command asks for a name or url, then
that is looked up in this list.  The first element whose REGEXP
matches is used.

The format specified by option `magit-clone-url-format' is used
to turn the name into an url, using HOSTNAME and the repository
name.  If the provided name contains a slash, then that is used.
Otherwise if the name omits the owner of the repository, then the
default user specified in the matched entry is used.

If USER contains a dot, then it is treated as a Git variable and
the value of that is used as the username.  Otherwise it is used
as the username itself."
  :package-version '(magit . "3.0.0")
  :group 'magit-commands
  :type '(repeat (list regexp
                       (string :tag "hostname")
                       (string :tag "user name or git variable"))))

(defcustom magit-clone-url-format "git@%h:%n.git"
  "Format used when turning repository names into urls.
%h is the hostname and %n is the repository name, including
the name of the owner.  Also see `magit-clone-name-alist'."
  :package-version '(magit . "3.0.0")
  :group 'magit-commands
  :type 'regexp)

;;; Commands

;;;###autoload (autoload 'magit-clone "magit-clone" nil t)
(transient-define-prefix magit-clone (&optional transient)
  "Clone a repository."
  :man-page "git-clone"
  ["Fetch arguments"
   ("-B" "Clone a single branch"  "--single-branch")
   ("-n" "Do not clone tags"      "--no-tags")
   ("-S" "Clones submodules"      "--recurse-submodules" :level 6)
   ("-l" "Do not optimize"        "--no-local" :level 7)]
  ["Setup arguments"
   ("-o" "Set name of remote"     ("-o" "--origin="))
   ("-b" "Set HEAD branch"        ("-b" "--branch="))
   ("-g" "Separate git directory" "--separate-git-dir="
    transient-read-directory :level 7)
   ("-t" "Use template directory" "--template="
    transient-read-existing-directory :level 6)]
  ["Local sharing arguments"
   ("-s" "Share objects"          ("-s" "--shared" :level 7))
   ("-h" "Do not use hardlinks"   "--no-hardlinks")]
  ["Clone"
   ("C" "regular"            magit-clone-regular)
   ("s" "shallow"            magit-clone-shallow)
   ("d" "shallow since date" magit-clone-shallow-since :level 7)
   ("e" "shallow excluding"  magit-clone-shallow-exclude :level 7)
   ("b" "bare"               magit-clone-bare)
   ("m" "mirror"             magit-clone-mirror)]
  (interactive (list (or magit-clone-always-transient current-prefix-arg)))
  (if transient
      (transient-setup #'magit-clone)
    (call-interactively #'magit-clone-regular)))

;;;###autoload
(defun magit-clone-regular (repository directory args)
  "Create a clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository."
  (interactive (magit-clone-read-args))
  (magit-clone-internal repository directory args))

;;;###autoload
(defun magit-clone-shallow (repository directory args depth)
  "Create a shallow clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.
With a prefix argument read the DEPTH of the clone;
otherwise use 1."
  (interactive (append (magit-clone-read-args)
                       (list (if current-prefix-arg
                                 (read-number "Depth: " 1)
                               1))))
  (magit-clone-internal repository directory
                        (cons (format "--depth=%s" depth) args)))

;;;###autoload
(defun magit-clone-shallow-since (repository directory args date)
  "Create a shallow clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.
Exclude commits before DATE, which is read from the
user."
  (interactive (append (magit-clone-read-args)
                       (list (transient-read-date "Exclude commits before: "
                                                  nil nil))))
  (magit-clone-internal repository directory
                        (cons (format "--shallow-since=%s" date) args)))

;;;###autoload
(defun magit-clone-shallow-exclude (repository directory args exclude)
  "Create a shallow clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.
Exclude commits reachable from EXCLUDE, which is a
branch or tag read from the user."
  (interactive (append (magit-clone-read-args)
                       (list (read-string "Exclude commits reachable from: "))))
  (magit-clone-internal repository directory
                        (cons (format "--shallow-exclude=%s" exclude) args)))

;;;###autoload
(defun magit-clone-bare (repository directory args)
  "Create a bare clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository."
  (interactive (magit-clone-read-args))
  (magit-clone-internal repository directory (cons "--bare" args)))

;;;###autoload
(defun magit-clone-mirror (repository directory args)
  "Create a mirror of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository."
  (interactive (magit-clone-read-args))
  (magit-clone-internal repository directory (cons "--mirror" args)))

(defun magit-clone-internal (repository directory args)
  (let* ((checkout (not (memq (car args) '("--bare" "--mirror"))))
         (set-push-default
          (and checkout
               (or (eq  magit-clone-set-remote.pushDefault t)
                   (and magit-clone-set-remote.pushDefault
                        (y-or-n-p "Set `remote.pushDefault' to \"origin\"? "))))))
    (run-hooks 'magit-credential-hook)
    (setq directory (file-name-as-directory (expand-file-name directory)))
    (when (file-exists-p directory)
      (if (file-directory-p directory)
          (when (> (length (directory-files directory)) 2)
            (let ((name (magit-clone--url-to-name repository)))
              (unless (and name
                           (setq directory (file-name-as-directory
                                            (expand-file-name name directory)))
                           (not (file-exists-p directory)))
                (user-error "%s already exists" directory))))
        (user-error "%s already exists and is not a directory" directory)))
    (magit-run-git-async "clone" args "--" repository
                         (magit-convert-filename-for-git directory))
    ;; Don't refresh the buffer we're calling from.
    (process-put magit-this-process 'inhibit-refresh t)
    (set-process-sentinel
     magit-this-process
     (lambda (process event)
       (when (memq (process-status process) '(exit signal))
         (let ((magit-process-raise-error t))
           (magit-process-sentinel process event)))
       (when (and (eq (process-status process) 'exit)
                  (= (process-exit-status process) 0))
         (when checkout
           (let ((default-directory directory))
             (when set-push-default
               (setf (magit-get "remote.pushDefault") "origin"))
             (unless magit-clone-set-remote-head
               (magit-remote-unset-head "origin"))))
         (with-current-buffer (process-get process 'command-buf)
           (magit-status-setup-buffer directory)))))))

(defun magit-clone-read-args ()
  (let ((repo (magit-clone-read-repository)))
    (list repo
          (read-directory-name
           "Clone to: "
           (if (functionp magit-clone-default-directory)
               (funcall magit-clone-default-directory repo)
             magit-clone-default-directory)
           nil nil
           (magit-clone--url-to-name repo))
          (transient-args 'magit-clone))))

(defun magit-clone-read-repository ()
  (magit-read-char-case "Clone from " nil
    (?u "[u]rl or name"
        (let ((str (magit-read-string-ns "Clone from url or name")))
          (if (string-match-p "\\(://\\|@\\)" str)
              str
            (magit-clone--name-to-url str))))
    (?p "[p]ath"
        (read-directory-name "Clone repository: "))
    (?l "or [l]ocal url"
        (concat "file://" (read-directory-name "Clone repository: file://")))))

(defun magit-clone--url-to-name (url)
  (and (string-match "\\([^/:]+?\\)\\(/?\\.git\\)?$" url)
       (match-string 1 url)))

(defun magit-clone--name-to-url (name)
  (or (seq-some
       (pcase-lambda (`(,re ,host ,user))
         (and (string-match re name)
              (let ((repo (match-string 1 name)))
                (magit-clone--format-url host user repo))))
       magit-clone-name-alist)
      (user-error "Not an url and no matching entry in `%s'"
                  'magit-clone-name-alist)))

(defun magit-clone--format-url (host user repo)
  (format-spec
   magit-clone-url-format
   `((?h . ,host)
     (?n . ,(if (string-match-p "/" repo)
                repo
              (if (string-match-p "\\." user)
                  (if-let ((user (magit-get user)))
                      (concat user "/" repo)
                    (user-error "Set %S or specify owner explicitly" user))
                (concat user "/" repo)))))))

;;; _
(provide 'magit-clone)
;;; magit-clone.el ends here
