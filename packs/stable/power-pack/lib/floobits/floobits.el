;;; Floobits.el --- Floobits plugin for real-time collaborative editing
;;
;; Filename: floobits.el
;; Description: Real-time collaborative editing.
;;
;; Copyright 2013-2014 Floobits, Inc.
;;
;; Author: Matt Kaniaris
;;      Geoff Greer
;; Keywords: comm, tools
;; Package-Requires: ((json "1.2") (highlight "0"))
;; Package-Version: 0.3
;; URL: http://github.com/Floobits/floobits-emacs
;; Version: 23.0
;;
;;; Commentary:
;;
;;    Real-time collaborative editing.
;;
;;  This plugin requires Python 2.7 or later and a Floobits account.
;;
;;  Usage
;;  -----
;;  All commands are documented in `apropos-command <RET> floobits'
;;
;;  `floobits-join-workspace <RET> https://floobits.com/owner/workspace/ <RET>'
;;  Join an existing floobits workspace.
;;
;;  `floobits-share-dir-private <RET> DIR <RET>'
;;  Create a workspace and populate it with the contents of the directory, DIR (or make it).
;;
;;  `floobits-share-dir-public <RET> DIR <RET>'
;;  Create a workspace and populate it with the contents of the directory, DIR (or make it).
;;
;;  `floobits-leave-workspace <RET>'
;;  Leave the current workspace.
;;
;;  `floobits-summon <RET>'
;;  Summon everyone in the workspace to your cursor position.
;;
;;  `floobits-follow-mode-toggle <RET>'
;;  Toggle following of recent changes.
;;
;;  `floobits-clear-highlights <RET>'
;;  Clears all mirrored highlights.
;;

;;; Code:
(require 'cl)
(require 'json)
(require 'url)
(require 'bookmark)

(defgroup floobits nil
  "Floobits"
  :prefix "floobits-"
  :group 'editing
  :link '(url-link :tag "Description" "https://github.com/Floobits/floobits-emacs"))

(defcustom floobits-python-executable "python"
  "Python executable to use when running Floobits"
  :type 'string
  :group 'floobits)

(defvar floobits-plugin-dir (file-name-directory load-file-name))
(add-to-list 'load-path floobits-plugin-dir)
(require 'highlight)

(setq max-specpdl-size 1500)

(defvar floobits-debug nil)
(defvar floobits-agent-host "127.0.0.1")
(defvar floobits-message-buffer-name "*Floobits*")
(defvar floobits-python-path (concat floobits-plugin-dir "floobits.py"))
(defvar floobits-python-agent)

(defvar floobits-agent-buffer)
(defvar floobits-conn)
(defvar floobits-current-position)
(defvar floobits-open-buffers)
(defvar floobits-complete-signup)
(defvar floobits-follow-mode)
(defvar floobits-follow-users)
(defvar floobits-perms)
(defvar floobits-share-dir)
(defvar floobits-user-highlights)
(defvar floobits-on-connect)
(defvar floobits-last-highlight)
(defvar floobits-auth)
(defvar floobits-default-host)
(defvar floobits-user-input-events)
(defvar floobits-delete_workspace)

(defun floobits-initialize ()
  (setq floobits-agent-buffer "")
  (setq floobits-user-input-events nil)
  (setq floobits-conn nil)
  (setq floobits-current-position '((mark . 1) (point . 1) (name . "")))
  (setq floobits-open-buffers nil)
  (setq floobits-follow-mode nil)
  (setq floobits-follow-users ())
  (setq floobits-perms nil)
  (setq floobits-share-dir "")
  (setq floobits-on-connect nil)
  (setq floobits-last-highlight nil)
  (setq floobits-user-highlights (make-hash-table :test 'equal)))

(add-hook 'kill-emacs-hook (lambda ()
  (ignore-errors
    (delete-process floobits-conn))
  (ignore-errors
    (delete-process floobits-python-agent))))

(floobits-initialize)

(defun floobits-debug-message (text &rest rest)
  (when floobits-debug
    (apply 'message text rest)))

(defun floobits-add-hooks ()
  (add-hook 'after-change-functions 'floobits-after-change nil nil)
  (add-hook 'after-revert-hook 'floobits-after-revert nil nil)
  (if (> emacs-major-version 23)
    (progn
      (add-hook 'post-command-hook 'floobits-send-highlight nil nil)
      (add-hook 'buffer-list-update-hook 'floobits-buffer-list-change nil nil))
    (add-hook 'post-command-hook 'floobits-post-command-func nil nil))
  (add-hook 'after-save-hook 'floobits-after-save-hook nil nil)
  (add-hook 'minibuffer-exit-hook 'floobits-minibuffer-exit-hook nil nil)
  ; (add-hook 'buffer-list-update-hook 'floobits-buffer-list-change nil nil)
  (ad-enable-advice 'delete-file 'before 'floobits-delete-file)
  (ad-enable-advice 'rename-file 'before 'floobits-rename-file)
  (ad-activate 'delete-file)
  (ad-activate 'rename-file))

(defun floobits-remove-hooks ()
  (remove-hook 'after-change-functions 'floobits-after-change)
  (remove-hook 'after-revert-hook 'floobits-after-revert)
  (if (> emacs-major-version 23)
    (progn
      (remove-hook 'post-command-hook 'floobits-send-highlight)
      (remove-hook 'buffer-list-update-hook 'floobits-buffer-list-change))
    (remove-hook 'post-command-hook 'floobits-post-command-func))

  (remove-hook 'after-save-hook 'floobits-after-save-hook)
  (remove-hook 'minibuffer-exit-hook 'floobits-minibuffer-exit-hook)
  (ad-disable-advice 'delete-file 'before 'floobits-delete-file)
  (ad-disable-advice 'rename-file 'before 'floobits-rename-file))

(defadvice delete-file (before floobits-delete-file (name &optional trash))
  (when (floobits-path-is-shared name)
    (if (member "delete_buf" floobits-perms)
      (floobits-send-to-agent (list (cons 'path name)) 'delete_buf)
      (message "You don't have permission to delete buffers in this workspace."))))

(defadvice rename-file (before floobits-rename-file
    (old-name new-name &optional OK-IF-ALREADY-EXISTS))
  (when (floobits-path-is-shared old-name)
    (if (member "rename_buf" floobits-perms)
      (let ((req (list
            (cons 'path new-name)
            (cons 'old_path old-name))))
        (floobits-send-to-agent req 'rename_buf))
      (message "You don't have permission to rename buffers in this workspace."))))

(defmacro floo-get-item (alist key)
  "just grab an element from an alist"
  (list 'cdr (list 'assoc-string key alist)))

(defmacro floo-set-item (alist key value)
  "set an element in an alist"
  (list 'add-to-list alist (list 'cons key value)))

(defmacro floo-when-buf (buf &rest body)
  "save excursion and widen"
  (list 'when buf
    (list 'with-current-buffer buf
      (list 'save-excursion
        (list 'save-restriction
          (list 'widen)
          (cons 'progn body))))))

(defun floobits-send-debug ()
  (when floobits-conn
    (floobits-send-to-agent
      (list
        (cons 'name 'debug)
        (cons 'value floobits-debug)) 'setting)))

;;;###autoload
(defun floobits-debug ()
  "Toggles debug logging."
  (interactive)
  (setq floobits-debug (not floobits-debug))
  (message "Debug logging %s." (if floobits-debug "enabled" "disabled"))
  (floobits-send-debug))

;;;###autoload
(defun floobits-summon ()
  "Summons all users to your cursor position."
  (interactive)
  (floobits-send-highlight t))

;;;###autoload
(defun floobits-follow-mode-toggle ()
  "Toggles following of recent changes in a workspace"
  (interactive)
  (when floobits-conn
    (setq floobits-follow-mode (not floobits-follow-mode))
    (setq floobits-follow-users ())
    (floobits-send-to-agent (list (cons 'follow_mode floobits-follow-mode)) 'set_follow_mode)
    (when (and floobits-follow-mode floobits-last-highlight)
      (floobits-event-highlight floobits-last-highlight))
    (message "Follow mode %s." (if floobits-follow-mode "enabled" "disabled"))))

;;;###autoload
(defun floobits-follow-user ()
  "Follow a users changes. This also toggles follow mode."
  (interactive)
  (when floobits-conn
    (floobits-send-to-agent () 'follow_user)))

;;;###autoload
(defun floobits-leave-workspace ()
  "leaves the current workspace"
  (interactive)
  (floobits-destroy-connection))

;;;###autoload
(defun floobits-complete-signup ()
  "If you created an Floobits account via emacs, you must call this command before you can login to
  the website."
  (interactive)
  (floobits-destroy-connection)
  (floobits-create-connection (lambda () (floobits-send-to-agent () 'pinocchio))))

;;;###autoload
(defun floobits-share-dir-public (dir-to-share)
  "Create a workspace and populate it with the contents of the directory, dir-to-share, or make it.
If the directory corresponds to an existing floobits workspace, you will instead join the workspace.
"
  (interactive "DDirectory to share: ")
  (floobits-destroy-connection)
  (lexical-let* ((req (list
                (cons 'perms '((AnonymousUser . ["view_room"])))
                (cons 'line_endings (floobits-get-line-endings))
                (cons 'dir_to_share dir-to-share)))
                (func (lambda () (floobits-send-to-agent req 'share_dir))))
    (floobits-create-connection func)))

;;;###autoload
(defun floobits-share-dir-private (dir-to-share)
  "Create a workspace and populate it with the contents of the directory, dir-to-share, or make it.
If the directory corresponds to an existing floobits workspace, you will instead join the workspace.
"
  (interactive "DDirectory to share: ")
  (floobits-destroy-connection)
  (lexical-let* (
      (req (list
        (cons 'perms '((AnonymousUser . [])))
        (cons 'line_endings (floobits-get-line-endings))
        (cons 'dir_to_share dir-to-share)))
      (func (lambda () (floobits-send-to-agent req 'share_dir))))
    (floobits-create-connection func)))

(defun floobits-event-error (req)
  (display-message-or-buffer (floo-get-item req 'msg)))

(defun _floobits-read-persistent ()
  (condition-case nil
    (with-temp-buffer
      (insert-file-contents "~/floobits/persistent.json")
      (let* ((json-key-type 'string)
            (data (json-read-from-string (buffer-string)))
            (data (floo-get-item data 'recent_workspaces)))
        (mapcar (lambda (x) (floo-get-item x 'url)) data)))
    (error '(""))))

(defun _floobits-get-url-from-dot-floo ()
  (condition-case nil
    (with-temp-buffer
      (insert-file-contents ".floo")
      (let* ((json-key-type 'string)
          (entry (json-read-from-string  (buffer-string))))
        (cdr (assoc-string "url" entry))))
    (error "https://floobits.com/")))

;;;###autoload
(defun floobits-join-workspace (floourl)
  "Join an existing floobits workspace.
See floobits-share-dir to create one or visit floobits.com."
  (interactive (list 
    ; read-from-minibuffer prompt &optional initial keymap read history default inherit-input-method
    (let ((histories (_floobits-read-persistent)))
      (read-from-minibuffer "Floobits workspace URL (owner/workspace): " 
        (_floobits-get-url-from-dot-floo) nil nil 'histories))))
  (let* ((url-struct (url-generic-parse-url floourl))
        (domain (url-host url-struct))
        (port (url-port url-struct))
        (path (url-filename url-struct))
        (path
          (if (string= "/" (substring path -1))
            (concat path "")
            (concat path "/")))
        (path-components (split-string path "\\/"))
        (owner (nth 1 path-components))
        (workspace (nth 2 path-components)))
    (if (and path workspace owner)
      (progn
        (floobits-destroy-connection)
        (lexical-let* ((req
          (list
            (cons 'host domain)
            (cons 'workspace workspace)
            (cons 'line_endings (floobits-get-line-endings))
            (cons 'workspace_owner owner)
            (cons 'current_directory default-directory))))
        (floobits-create-connection (lambda () (floobits-send-to-agent req 'join_workspace)))))
      (message "Invalid url! I should look like: https://floobits.com/owner/workspace/"))))

(defun floobits-delete-workspace ()
  (interactive)
  (floobits-create-connection (lambda () (floobits-send-to-agent () 'delete_workspace))))

;;;###autoload
(defun floobits-workspace-settings ()
  (interactive)
  (floobits-send-to-agent () 'open_workspace_settings))

;;;###autoload
(defun floobits-remove-from-workspace (path)
  "Removes a file from the remote workspace without deleting it locally"
  (interactive "fpath: ")
  (if (member "delete_buf" floobits-perms)
      (progn
        (message "removing %s from workspace" path)
        (floobits-send-to-agent (list (cons 'path path)) 'delete_buf))
      (message "You don't have permission to delete buffers in this workspace.")))

;;;###autoload
(defun floobits-open-workspace-in-browser ()
  (interactive)
  (floobits-send-to-agent () 'open_workspace))

;;;###autoload
(defun floobits-clear-highlights ()
  "Clears all highlights"
  (interactive)
  (maphash
    (lambda (key highlight)
      (floo-when-buf (get-file-buffer (cadr key))
        (hlt-unhighlight-region 0 (buffer-size))))
    floobits-user-highlights))

;;;###autoload
(defun floobits-add-to-workspace (path)
  "Adds a file or directory to the workspace"
  (interactive "fpath: ")
  (floobits-send-to-agent (list (cons 'full_path path)) 'create_buf))

(defun floobits-process-live-p (process)
  "Returns non-nil if PROCESS is alive.
  A process is considered alive if its status is `run', `open',
  `listen', `connect' or `stop'."
  (memq (process-status process)
    '(run open listen connect stop)))

(defun floobits-listener (process response)
  (setq floobits-agent-buffer (concat floobits-agent-buffer response))
  (let ((position (search "\n" floobits-agent-buffer)))
    (when position
      (floobits-switch (substring floobits-agent-buffer 0 position))
      (setq floobits-agent-buffer
      (substring floobits-agent-buffer
        (if (> (length floobits-agent-buffer) position) (+ 1 position) position)))
      (floobits-listener process ""))))

(defun floobits-create-connection (on_connect)
  (setq floobits-on-connect on_connect)
  (floobits-launch-agent))

(defun floobits-destroy-connection ()
  (when floobits-conn
    (message "Destroying Floobits conn")
    (ignore-errors
      (floobits-remove-hooks))
    (ignore-errors
      (delete-process floobits-conn))
    (ignore-errors
      (delete-process floobits-python-agent))
    (floobits-initialize)
    (setq floobits-python-agent nil)
    (message "You have left the workspace.")))

(defun floobits-filter-func (condp lst)
  (delq nil
  (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun floobits-post-command-func ()
  "used for grabbing changes in point for highlighting"
  (floobits-buffer-list-change)
  (floobits-send-highlight))

(defun floobits-agent-listener (proc string)
  (with-current-buffer floobits-message-buffer-name
    (let ((moving (= (point) (process-mark proc)))
          (callback floobits-on-connect))
      ;; Insert the text, advancing the process marker.
      (goto-char (process-mark proc))
      (insert string)
      (set-marker (process-mark proc) (point))
      (goto-char (point-max))
      (when (and floobits-on-connect (search-backward "Now listening on " nil t))
        (let ((port (car (split-string (buffer-substring (+ (length "Now listening on ") (point)) (point-max)) "\n" t))))
          (setq floobits-on-connect nil)
          (setq floobits-conn (open-network-stream "floobits" nil floobits-agent-host port))
          (set-process-coding-system floobits-conn 'utf-8 'utf-8)
          (set-process-query-on-exit-flag floobits-conn nil)
          (set-process-filter floobits-conn 'floobits-listener)
          (funcall callback))
      (if moving (goto-char (process-mark proc)))))))

(defun floobits-launch-agent ()
  (condition-case nil
    (progn
      (delete-process floobits-python-agent))
    (error nil))
  (message "Launching Floobits python agent...")
  (setq floobits-python-agent (start-process "" floobits-message-buffer-name floobits-python-executable floobits-python-path))
  (switch-to-buffer floobits-message-buffer-name)
  (set-process-filter floobits-python-agent 'floobits-agent-listener)
  (accept-process-output floobits-python-agent 5)
  (set-process-query-on-exit-flag floobits-python-agent nil)
  (floobits-send-debug))

(defun floobits-send-to-agent (req event)
  (if (floobits-process-live-p floobits-conn)
    (progn
      (floo-set-item 'req 'name event)
      ; This works around a bug in Emacs where regions aren't shown or something
      (run-at-time .01 nil
        (lambda (req)
          (process-send-string floobits-conn (concat (json-encode req) "\n")))
        req))
    (progn
      (message "Connection to floobits died :(")
      (floobits-destroy-connection))))

(defun floobits-event-user_input (req)
  ; (minibufferp (current-buffer))
  (if (active-minibuffer-window)
    (push req floobits-user-input-events)
    (let
        ((prompt (floo-get-item req 'prompt))
        (initial (floo-get-item req 'initial))
        (choices (floo-get-item req 'choices))
        (dir (floo-get-item req 'dir)))
      (floo-set-item 'req 'response
        (cond
          (choices (completing-read prompt (mapcar (lambda (x) (append x nil)) choices) nil t initial))
          ((floo-get-item req 'y_or_n) (y-or-n-p prompt))
          (dir (read-directory-name prompt nil initial))
          (t (read-from-minibuffer prompt initial))))
      (floobits-send-to-agent req 'user_input))))

(defun floobits-event-rename_buf (req)
  (let* ((old-path (floo-get-item req 'old_path))
        (new-path (floo-get-item req 'path))
        (buf (get-file-buffer old-path)))
    (rename-file old-path new-path 1)
    (when buf
      (with-current-buffer buf
        (rename-buffer new-path)
        (set-visited-file-name new-path)
        (set-buffer-modified-p nil)))))

(defun floobits-send-highlight (&optional ping)
 (when (floobits-buffer-is-shareable (current-buffer))
    (lexical-let* ((name (buffer-file-name (current-buffer)))
        (point (- (or (point) 0) 1))
        (req (list
          (cons 'ranges (if (use-region-p)
            (vector (vector (- (region-beginning) 1) (- (region-end) 1)))
            (vector (vector point point))))
          (cons 'full_path name)
          (cons 'following floobits-follow-mode)
          (cons 'ping ping))))
      (when (or ping (not (equal req floobits-current-position)))
        (setq floobits-current-position req)
          (floobits-send-to-agent req 'highlight)))))

(defun floobits-buffer-is-shareable (buf)
  (let ((name (buffer-name buf)))
    (cond
      ((eq nil (buffer-file-name buf)) nil)
      ((string= name floobits-message-buffer-name) nil)
      ((string= name "*Messages*") nil)
      (t t))))

(defun floobits-path-is-shared (path)
  (file-in-directory-p path floobits-share-dir))

(defun floobits-is-buffer-shared (buf)
  (floobits-path-is-shared (buffer-file-name buf)))

(defun floobits-get-public-buffers ()
  "returns buffers that aren't internal to emacs"
  (floobits-filter-func 'floobits-buffer-is-shareable (buffer-list)))

(defun floobits-get-text (begin end)
  (buffer-substring-no-properties begin end))

(defun floobits-get-buffer-text (buffer)
  "returns properties free text of buffer with name (name)"
  (floo-when-buf buffer
    (floobits-get-text 1 (+ 1 (buffer-size)))))

(defun floobits-event-disconnect (req)
  (message "Disconnected: %s" (floo-get-item req 'reason)))

(defun floobits-event-room_info (req)
  (let ((floobits-workspace (floo-get-item req 'workspace_name)))
    (message "Successfully joined workspace %s." floobits-workspace)
    (setq floobits-share-dir (floo-get-item req 'project_path))
    (message "Project path is %s." floobits-share-dir)
    (setq floobits-perms (append (floo-get-item req 'perms) nil))
    (mapc
      (lambda (x)
        (when (and (> (length x) 9) (string="floobits-" (substring x 0 9)))
          (bookmark-delete x)))
      (bookmark-all-names))
    (floobits-add-hooks)
    (dired floobits-share-dir)))

(defun floobits-event-join (req)
  (floobits-debug-message "%s" req)
  (message "%s joined the workspace"  (floo-get-item req 'username)))

(defun floobits-event-part (req)
  (floobits-debug-message "%s" req)
  (message "%s left the workspace" (floo-get-item req 'username)))

(defun floobits-event-create_view (req)
  (find-file (floo-get-item req 'full_path))
  (floobits-buffer-list-change))

(defun floobits-event-focus (req)
  (find-file (floo-get-item req 'full_path))
  (goto-char (+ 1 (floo-get-item req 'offset))))

(defun floobits-highlight-apply-f (f highlights)
  ; convert to list :(
  (mapc
    (lambda(x)
      (let ((start (max 1 (min (buffer-size buffer) (+ (elt x 0) 1))))
            (end (+ (elt x 1) 2)))
        (funcall f start end)))
    highlights))

(defun floobits-apply-highlight (user_id buffer ranges)
  (let* ((key (list user_id (buffer-file-name buffer)))
         (previous-ranges (gethash key floobits-user-highlights)))
    (floobits-debug-message "%s key %s" key previous-ranges)
    (when previous-ranges
      (floobits-highlight-apply-f 'hlt-unhighlight-region previous-ranges))
    (floobits-highlight-apply-f 'hlt-highlight-region ranges)
    (puthash key ranges floobits-user-highlights)))

(defun floobits-event-highlight (req)
  (setq floobits-last-highlight req)
  (let* ((ranges (floo-get-item req 'ranges))
        (ranges-length (- (length ranges) 1))
        (user_id (floo-get-item req 'user_id))
        (username (floo-get-item req 'username))
        (pos (+ 1 (elt (elt ranges ranges-length) 0)))
        (path (floo-get-item req 'full_path))
        (buffer (get-file-buffer path))
        (following (floo-get-item req 'following))
        (should-jump (or (floo-get-item req 'ping) (and
          (and floobits-follow-mode (or (not floobits-follow-users)
            (member username floobits-follow-users))) (not following))))
        (buffer (or buffer (and should-jump (find-file path)))))

    (floo-when-buf buffer
      (floobits-apply-highlight user_id buffer ranges)
      (goto-char pos)
      (bookmark-set (format "floobits-%s-%s" username user_id)))

    (when should-jump
      (unless (window-minibuffer-p (get-buffer-window))
        (switch-to-buffer buffer)
        (save-restriction
          (widen)
          (unless (pos-visible-in-window-p pos)
            (condition-case err
              (scroll-up (- (line-number-at-pos pos) (line-number-at-pos)))
              (error))))))))

(defun floobits-event-save (req)
  (floo-when-buf (get-file-buffer (floo-get-item req 'full_path))
    (remove-hook 'after-save-hook 'floobits-after-save-hook)
    (save-buffer)
    (add-hook 'after-save-hook 'floobits-after-save-hook)))

(defun floobits-apply-edit (edit)
  (let* ((inhibit-modification-hooks t)
        (edit-start (max 1 (+ 1 (elt edit 0))))
        (edit-length (elt edit 1))
        (edit-end (min (+ 1 (buffer-size)) (+ edit-start edit-length)))
        (active mark-active)
        (mark (mark))
        (point (point)))
    (delete-region edit-start edit-end)
    (when (eq 3 (length edit))
      (goto-char edit-start)
      (insert (elt edit 2)))
    (goto-char
      (if (>= point edit-start)
        (+ point (- (length (elt edit 2)) edit-length))
      point))
    (when mark
      (pop-mark)
      (push-mark
        (if (>= mark edit-start)
          (+ mark (- (length (elt edit 2)) edit-length))
        mark) t active))))

(defun floobits-event-edit (req)
  (let* ((filename (floo-get-item req "full_path"))
        (buf (get-file-buffer filename))
        (edits (floo-get-item req "edits")))
    (when buf
      (with-current-buffer buf
        (save-restriction
          (widen)
          (atomic-change-group
            (mapc 'floobits-apply-edit edits)))))))

(defun floobits-event-create_buf (req)
  (let ((filename (floo-get-item req "path" ))
        (username (floo-get-item req "username")))
    (floobits-debug-message "User %s created buffer %s" username filename)))

(defun floobits-event-follow_user (req)
    (let ((username (floo-get-item req "username")))
      (setq floobits-follow-mode t)
      (add-to-list 'floobits-follow-users username)
    )
  )

(defun floobits-event-delete_buf (req)
  (let ((filename (floo-get-item req "path" ))
        (username (floo-get-item req "username")))
    (unless (string= filename "FLOOBITS_README.md")
      (message "User %s deleted buffer %s" username filename))))

(defun floobits-event-get_buf (req)
  (floo-when-buf (get-file-buffer (floo-get-item req "full_path"))
    (atomic-change-group
      (delete-region 1 (+ 1 (buffer-size)))
      (insert (floo-get-item req "buf")))))

(defun floobits-event-open_file (req)
  (find-file (floo-get-item req "filename")))

(defun floobits-event-message (req)
  (message "%s" (floo-get-item req "message")))

(defun floobits-event-rename (req)
  (let* ((new-name (floo-get-item req "new_name"))
      (old-name (floo-get-item req "full_path"))
      (buf (get-file-buffer old-name)))
    (when buf
      (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
        (with-current-buffer buf
          (rename-file old-name new-name t)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun floobits-switch (text)
  (floobits-debug-message "%s" text)
  (let* ((json-key-type 'string)
        (json-false 'nil)
        (req (json-read-from-string text))
        (event (floo-get-item req "name"))
        (func (concat "floobits-event-" event)))
    (if (fboundp (intern-soft func))
      (funcall (read func) req)
      (message "func %s doesn't exist" func))))

(defun floobits-after-change (begin end old_length)
  (when (floobits-buffer-is-shareable (current-buffer))
    ; not sure why we're doing with-current-buffer here, but it seems important. Originally added in
    ; https://github.com/Floobits/floobits-emacs/commit/41b6ed9358de6dffa78fa229c347b4b531fc2021
    (with-current-buffer (current-buffer)
      (floobits-send-to-agent
        (list
          (cons 'changed (buffer-substring-no-properties begin end))
          (cons 'begin begin)
          (cons 'end end)
          (cons 'old_length old_length)
          (cons 'full_path (buffer-file-name (current-buffer)))) 'change))))

(defun floobits-after-revert ()
  (when (floobits-buffer-is-shareable (current-buffer))
    ; not sure why we're doing with-current-buffer here, but it seems important. Originally added in
    ; https://github.com/Floobits/floobits-emacs/commit/41b6ed9358de6dffa78fa229c347b4b531fc2021
    (floobits-send-to-agent
      (list
        (cons 'buf (floobits-get-buffer-text (current-buffer)))
        (cons 'full_path (buffer-file-name (current-buffer)))) 'revert)))

(defun floobits-after-save-hook ()
  (when (floobits-is-buffer-shared (current-buffer))
    (floobits-send-to-agent (list (cons 'path (buffer-file-name))) 'saved)))

(defun floobits-get-text-for-path (p)
  (cons (intern p) (floobits-get-buffer-text (find-buffer-visiting p))))

(defun floobits-buffer-list-change ()
  (let* ((current-buffers (mapcar 'buffer-file-name (floobits-get-public-buffers)))
      (added (set-difference current-buffers floobits-open-buffers))
      (deleted (set-difference floobits-open-buffers current-buffers)))
    (when (or added deleted)
      (when (and added (not (member "patch" floobits-perms)))
        (mapc
          (lambda (buf-path)
            (with-current-buffer (find-buffer-visiting buf-path)
              (setq buffer-read-only t)))
          added))
      (setq floobits-open-buffers current-buffers)
      (let* 
          ((added-text (mapcar 'floobits-get-text-for-path added))
          (req (list
            (cons 'current current-buffers)
            (cons 'added added-text)
            (cons 'deleted deleted))))
        (floobits-send-to-agent req 'buffer_list_change)))))

(defun floobits-minibuffer-exit-hook ()
  (when floobits-user-input-events
    (run-at-time 0 nil
      (lambda ()
        (while floobits-user-input-events
          (let ((req (car floobits-user-input-events)))
            (setq floobits-user-input-events (cdr floobits-user-input-events))
            (floobits-event-user_input req)))))))

(defun floobits-get-line-endings ()
  (symbol-name buffer-file-coding-system))

(provide 'floobits)
;;; floobits.el ends here
