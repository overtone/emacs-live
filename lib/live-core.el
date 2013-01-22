(require 'cl)

(defvar live-current-pack-dir nil "The directory of the pack bein currently loaded")
(defvar live-current-pack-version nil "The version string of the pack being currently loaded")
(defvar live-current-pack-name nil "The name of the pack being currently loaded")
(defvar live-current-pack-description nil "The description of the pack being currently loaded")

(defun live-filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (when (funcall condp x) x)) lst)))

(defun live-list-buffer-paths ()
  (mapcar #'file-truename
          (live-filter 'identity
                       (mapcar (lambda (b) (buffer-file-name b))
                               (buffer-list)))))

(defun live-list-buffer-names ()
  (live-filter 'identity (mapcar (lambda (el) (buffer-name el)) (buffer-list))))

(defun live-file-open-as-buffer-p (path)
  (if (member (file-truename path) (live-list-buffer-paths))
      t
    nil))

(defun live-find-buffer-by-path (path)
  (car (live-filter (lambda (b)
                      (equal (file-truename path)
                             (file-truename (or (buffer-file-name b)
                                                "/dev/null"))))
                    (buffer-list))))

(defun live-empty-p (seq)
  (eq 0 (length seq)))

(defun live-alist-keys (alist)
  (mapcar (lambda (el) (car el)) alist))

(defun live-alist-vals (alist)
  (mapcar (lambda (el) (cadr el)) alist))

(defun live-pack-config-dir ()
  "Returns the path of the config dir for the current pack"
  (file-name-as-directory (concat live-current-pack-dir "config")))

(defun live-pack-lib-dir ()
  "Returns the path of the lib dir for the current pack"
  (file-name-as-directory (concat live-current-pack-dir "lib")))

(defun live-pack-version (version)
  "Specify the version of the current pack. This should typically
   only be used in the pack's info.el file"
  (setq live-current-pack-version version))

(defun live-pack-description (desc)
  "Specify the description of the current pack. This should typically
   only be used in the pack's info.el file"
  (setq live-current-pack-description desc))

(defun live-pack-name (name)
  "Specify the name of the current pack. This should typically
   only be used in the pack's info.el file"
  (setq live-current-pack-name name))

(defun live-clear-pack-info ()
  "Clears the pack-info vars for the current pack."
  (setq live-current-pack-version nil)
  (setq live-current-pack-name nil)
  (setq live-current-pack-description nil))

(defun live-check-pack-info ()
  "Checks that that all the correct pack-info has been set by the
  current pack's pack-info.el"
  (when (not live-current-pack-version)
    (message (concat "Error - no pack version found for pack in path: " live-current-pack-dir)))
  ;;(version-to-list live-current-pack-version)
  (when (not live-current-pack-name)
    (message (concat "Error - no pack name found for pack in path: " live-current-pack-dir)))
  (when (not live-current-pack-description)
    (message (concat "Error - no pack description found for pack in path: " live-current-pack-dir))))

(defun live-load-pack (pack-dir)
  "Load a live pack. This is a dir that contains at least the
  files pack-info.el and init.el. Adds the packs's lib dir
  to the load-path"
  (let* ((pack-info (concat pack-dir "info.el"))
         (pack-init (concat pack-dir "init.el")))
    (setq live-current-pack-dir pack-dir)
    (message (concat "Live pack lib dir: " (live-pack-lib-dir)))
    (live-clear-pack-info)
    (if (file-exists-p pack-info)
        (load-file pack-info)
      (message (concat "Could not find info.el file for pack with location: " pack-dir)))
    (live-check-pack-info)
    (add-to-list 'load-path (live-pack-lib-dir))
    (if (file-exists-p pack-init)
        (load-file pack-init))
    (setq live-current-pack-dir nil)))

(defun live-add-pack-lib (p)
  "Adds the path (specified relative to the the pack's lib dir)
  to the load-path"
  (add-to-list 'load-path (concat (live-pack-lib-dir) p)))

(defun live-load-config-file (f-name)
  "Load the config file with name f-name in the current pack"
  (let* ((config-dir (live-pack-config-dir)))
    (load-file (concat config-dir f-name))))

(defun live-use-packs (pack-list)
  "Use the packs in pack-list - overrides the defaults and any
  previous packs added with live-add-packs."
  (setq live-packs pack-list))

(defun live-use-dev-packs ()
  "Reset all packs to the list of dev packs"
  (setq live-packs live-dev-pack-list))

(defun live-prepend-packs (pack-list)
  "Add the list pack-list to the beginning of the current list of
  packs to load"
  (setq live-packs (append pack-list live-packs)))

(defun live-add-packs (pack-list)
  "Add the list pack-list to end of the current list of packs to
  load"
  (setq live-packs (append live-packs pack-list)))

(defun live-append-packs (pack-list)
  "Add the list pack-list to end of the current list of packs to
  load"
  (setq live-packs (append live-packs pack-list)))

(defun live-pack-name-as-str (pack)
  (if (symbolp pack)
      (symbol-name pack)
    pack))

(defun live-ignore-packs (pack-list)
  "Do not load any of the packs in pack-list"
  (setq live-packs (live-filter (lambda (l) (not (member l (mapcar #'live-pack-name-as-str pack-list)))) (mapcar #'live-pack-name-as-str live-packs))))

(defun live-pack-dir (pack)
  "Determine a pack name's absolute path"
  (let* ((pack-name (live-pack-name-as-str pack))
         (pack-name-dir (if (file-name-absolute-p pack-name)
                            (file-name-as-directory pack-name)
                          (file-name-as-directory (concat live-packs-dir pack-name)))))
    pack-name-dir))

(defun live-pack-dirs ()
  "Returns a list of absolute directories of all the registered packs"
  (mapcar (lambda (pack-name) (live-pack-dir pack-name)) live-packs))

(defun live-byte-recompile-directory-sl (directory &optional arg force follow-symlinks?)
  "Recompile every `.el' file in DIRECTORY that needs recompilation.
This happens when a `.elc' file exists but is older than the `.el' file.
Files in subdirectories of DIRECTORY are processed also.

If the `.elc' file does not exist, normally this function *does not*
compile the corresponding `.el' file.  However, if the prefix argument
ARG is 0, that means do compile all those files.  A nonzero
ARG means ask the user, for each such `.el' file, whether to
compile it.  A nonzero ARG also means ask about each subdirectory
before scanning it.

If the third argument FORCE is non-nil, recompile every `.el' file
that already has a `.elc' file.

If the fourth argument FOLLOW-SYMLINKS? is non-nil, follow symlinks in
children of DIRECTORY."
  (interactive "DByte recompile directory: \nP")
  (if arg (setq arg (prefix-numeric-value arg)))
  (if noninteractive
      nil
    (save-some-buffers)
    (force-mode-line-update))
  (with-current-buffer (get-buffer-create byte-compile-log-buffer)
    (setq default-directory (expand-file-name directory))
    ;; compilation-mode copies value of default-directory.
    (unless (eq major-mode 'compilation-mode)
      (compilation-mode))
    (let ((directories (list default-directory))
          (default-directory default-directory)
          (skip-count 0)
          (fail-count 0)
          (file-count 0)
          (dir-count 0)
          last-dir)
      (displaying-byte-compile-warnings
       (while directories
         (setq directory (car directories))
         (message "Checking %s..." directory)
         (dolist (file (directory-files directory))
           (let ((source (expand-file-name file directory)))
             (if (and (not (member file '("RCS" "CVS")))
                      (not (eq ?\. (aref file 0)))
                      (file-directory-p source)
                      (if follow-symlinks?
                          t
                        (not (file-symlink-p source))))
                 ;; This file is a subdirectory.  Handle them differently.
                 (when (or (null arg) (eq 0 arg)
                           (y-or-n-p (concat "Check " source "? ")))
                   (setq directories (nconc directories (list source))))
               ;; It is an ordinary file.  Decide whether to compile it.
               (if (and (string-match emacs-lisp-file-regexp source)
                        (file-readable-p source)
                        (not (auto-save-file-name-p source))
                        (not (string-equal dir-locals-file
                                           (file-name-nondirectory source))))
                   (progn (case (byte-recompile-file source force arg)
                            (no-byte-compile (setq skip-count (1+ skip-count)))
                            ((t) (setq file-count (1+ file-count)))
                            ((nil) (setq fail-count (1+ fail-count))))
                          (or noninteractive
                              (message "Checking %s..." directory))
                          (if (not (eq last-dir directory))
                              (setq last-dir directory
                                    dir-count (1+ dir-count)))
                          )))))
         (setq directories (cdr directories))))
      (message "Done (Total of %d file%s compiled%s%s%s)"
               file-count (if (= file-count 1) "" "s")
               (if (> fail-count 0) (format ", %d failed" fail-count) "")
               (if (> skip-count 0) (format ", %d skipped" skip-count) "")
               (if (> dir-count 1)
                   (format " in %d directories" dir-count) "")))))

(defun live-recompile-packs ()
  "Byte-recompile all registered packs"
  (interactive)
  (mapcar (lambda (pack-dir)
            (live-byte-recompile-directory-sl pack-dir 0 1 1))
          (live-pack-dirs)))

(defun live-user-first-name ()
  (let* ((first-name (car (split-string user-full-name))))
    (if first-name
        (capitalize first-name)
      "")))

(defun live-user-first-name-p ()
  (not (string-equal "" (live-user-first-name))))

(defun live-server-kill-terminal ()
  (interactive)
  "Kill the current client without offering to save the current
   buffers. Useful if you want to quickly exit but have a server
   running in the background"
  (let ((proc (frame-parameter (selected-frame) 'client)))
    (cond ((processp proc)
           (server-delete-client proc))
          (t (error "Could not kill terminal: invalid client frame")))))
