;; Emacs LIVE
;;
;; This is where everything starts. Do you remember this place?
;; It remembers you...

(add-to-list 'command-switch-alist
             (cons "--live-safe-mode"
                   (lambda (switch)
                     nil)))

(setq live-safe-modep
      (if (member "--live-safe-mode" command-line-args)
          "debug-mode-on"
        nil))

(setq initial-scratch-message "
;; I'm sorry, Emacs Live failed to start correctly.
;; Hopefully the issue will be simple to resolve.
;;
;; First up, could you try running Emacs Live in safe mode:
;;
;;    emacs --live-safe-mode
;;
;; This will only load the default packs. If the error no longer occurs
;; then the problem is probably in a pack that you are loading yourself.
;; If the problem still exists, it may be a bug in Emacs Live itself.
;;
;; In either case, you should try starting Emacs in debug mode to get
;; more information regarding the error:
;;
;;    emacs --debug-init
;;
;; Please feel free to raise an issue on the Gihub tracker:
;;
;;    https://github.com/overtone/emacs-live/issues
;;
;; Alternatively, let us know in the mailing list:
;;
;;    http://groups.google.com/group/overtone
;;
;; Good luck, and thanks for using Emacs Live!
;;
;;                _.-^^---....,,--
;;            _--                  --_
;;           <          SONIC         >)
;;           |       BOOOOOOOOM!       |
;;            \._                   _./
;;               ```--. . , ; .--'''
;;                     | |   |
;;                  .-=||  | |=-.
;;                  `-=#$%&%$#=-'
;;                     | ;  :|
;;            _____.,-#%&$@%#&#~,._____
")

;; Store live base dirs
(setq live-root-dir (file-name-directory
                     (or (buffer-file-name) load-file-name)))

(setq
 live-tmp-dir      (file-name-as-directory (concat live-root-dir "tmp"))
 live-etc-dir      (file-name-as-directory (concat live-root-dir "etc"))
 live-packs-dir    (file-name-as-directory (concat live-root-dir "packs"))
 live-autosaves-dir(file-name-as-directory (concat live-tmp-dir  "autosaves"))
 live-backups-dir  (file-name-as-directory (concat live-tmp-dir  "backups"))
 live-load-pack-dir nil)

;; create tmp dirs if necessary
(make-directory live-etc-dir t)
(make-directory live-tmp-dir t)
(make-directory live-autosaves-dir t)
(make-directory live-backups-dir t)

;; Load manifest
(load-file (concat live-root-dir "manifest.el"))

;;default live packs
(let* ((live-dir (file-name-as-directory "live")))
  (setq live-packs (list (concat live-dir "foundation-pack")
                         (concat live-dir "colour-pack")
                         (concat live-dir "clojure-pack")
                         (concat live-dir "lang-pack")
                         (concat live-dir "power-pack"))))

;; Helper fn for loading live packs

(defun live-alist-keys (alist)
  (mapcar (lambda (el) (car el)) alist))

(defun live-alist-vals (alist)
  (mapcar (lambda (el) (cadr el)) alist))

(defun live-version ()
  (interactive)
  (if (called-interactively-p 'interactive)
      (message "%s" (concat "This is Emacs Live " live-version))
    live-version))

(defun live-pack-config-dir ()
  "Returns the path of the config dir for the current pack"
  (file-name-as-directory (concat live-load-pack-dir "config")))

(defun live-pack-lib-dir ()
  "Returns the path of the lib dir for the current pack"
  (file-name-as-directory (concat live-load-pack-dir "lib")))

(defun live-load-pack (pack-dir)
  "Load a live pack. This is a dir that contains at least a file
  called init.el. Adds the packs's lib dir to the load-path"
  (let* ((pack-init (concat pack-dir "init.el")))
    (setq live-load-pack-dir pack-dir)
    (add-to-list 'load-path (live-pack-lib-dir))
    (if (file-exists-p pack-init)
        (load-file pack-init))
    (setq live-load-pack-dir nil)))

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

(defun live-add-packs (pack-list)
  "Add the list pack-list to end of the current list of packs to
  load"
  (setq live-packs (append live-packs pack-list)))

(defun live-pack-dir (pack)
  "Determine a pack name's absolute path"
  (let* ((pack-name (if (symbolp pack-name)
                        (symbol-name pack-name)
                      pack-name))
         (pack-name-dir (if (file-name-absolute-p pack-name)
                            (file-name-as-directory pack-name)
                          (file-name-as-directory (concat live-packs-dir pack-name)))))
    pack-name-dir))

(defun live-pack-dirs ()
  "Returns a list of absolute directories of all the registered packs"
  (mapcar (lambda (pack-name) (live-pack-dir pack-name)) live-packs))

;; Load `~/.emacs-live.el`. This allows you to override variables such
;; as live-packs (allowing you to specify pack loading order)
;; Does not load if running in safe mode
(let* ((pack-file (concat (file-name-as-directory "~") ".emacs-live.el")))
  (if (and (file-exists-p pack-file) (not live-safe-modep))
      (load-file pack-file)))

(defun byte-recompile-directory-sl (directory &optional arg force follow-symlinks?)
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
            (byte-recompile-directory-sl pack-dir 0 1 1))
          (live-pack-dirs)))

;; Load all packs - Power Extreme!
(mapcar (lambda (pack-dir)
          (live-load-pack pack-dir))
        (live-pack-dirs))

(defun live-user-first-name ()
  (let* ((first-name (car (split-string user-full-name))))
    (if first-name
        (capitalize first-name)
      "")))

(defun live-user-first-name-p ()
  (not (string-equal "" (live-user-first-name))))

(setq live-welcome-messages
      (if (live-user-first-name-p)
          (list (concat "Hello " (live-user-first-name) ", somewhere in the world the sun is shining for you right now.")
                (concat "Hello " (live-user-first-name) ", it's lovely to see you again. I do hope that you're well.")
                (concat (live-user-first-name) ", turn your head towards the sun and the shadows will fall behind you.")
                )
        (list  "Hello, somewhere in the world the sun is shining for you right now."
               "Hello, it's lovely to see you again. I do hope that you're well."
               "Turn your head towards the sun and the shadows will fall behind you.")))

(defun live-welcome-message ()
  (nth (random* (length live-welcome-messages)) live-welcome-messages))

(setq initial-scratch-message (concat ";;     MM\"\"\"\"\"\"\"\"`M
;;     MM  mmmmmmmM
;;     M`      MMMM 88d8b.d8b. .d8888b. .d8888b. .d8888b.
;;     MM  MMMMMMMM 88''88'`88 88'  `88 88'  `\"\" Y8ooooo.
;;     MM  MMMMMMMM 88  88  88 88.  .88 88.  ...       88
;;     MM        .M dP  dP  dP `88888P8 '88888P' '88888P'
;;     MMMMMMMMMMMM
;;
;;         M\"\"MMMMMMMM M\"\"M M\"\"MMMMM\"\"M MM\"\"\"\"\"\"\"\"`M
;;         M  MMMMMMMM M  M M  MMMMM  M MM  mmmmmmmM
;;         M  MMMMMMMM M  M M  MMMMP  M M`      MMMM
;;         M  MMMMMMMM M  M M  MMMM' .M MM  MMMMMMMM
;;         M  MMMMMMMM M  M M  MMP' .MM MM  MMMMMMMM
;;         M         M M  M M     .dMMM MM        .M
;;         MMMMMMMMMMM MMMM MMMMMMMMMMM MMMMMMMMMMMM  Version " live-version
                                                                (if live-safe-modep
                                                                    "
;;                                                     --*SAFE MODE*--"
                                                                  "
;;"
                                                                  ) "
;;           http://github.com/overtone/emacs-live
;;
;; "                                                      (live-welcome-message) "

"))
