;;; git-gutter.el --- Port of Sublime Text plugin GitGutter

;; Copyright (C) 2014 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-git-gutter
;; Version: 0.53

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
;; Port of GitGutter which is a plugin of Sublime Text

;;; Code:

(eval-when-compile
  (require 'cl)
  (defvar global-git-gutter-mode))

(require 'tramp)

(defgroup git-gutter nil
  "Port GitGutter"
  :prefix "git-gutter:"
  :group 'vc)

(defcustom git-gutter:window-width nil
  "Character width of gutter window. Emacs mistakes width of some characters.
It is better to explicitly assign width to this variable, if you use full-width
character for signs of changes"
  :type 'integer
  :group 'git-gutter)

(defcustom git-gutter:diff-option ""
  "Option of 'git diff'"
  :type 'string
  :group 'git-gutter)

(defcustom git-gutter:update-hooks
  '(after-save-hook after-revert-hook window-configuration-change-hook)
  "hook points of updating gutter"
  :type '(list (hook :tag "HookPoint")
               (repeat :inline t (hook :tag "HookPoint")))
  :group 'git-gutter)

(defcustom git-gutter:separator-sign nil
  "Separator sign"
  :type 'string
  :group 'git-gutter)

(defcustom git-gutter:modified-sign "="
  "Modified sign"
  :type 'string
  :group 'git-gutter)

(defcustom git-gutter:added-sign "+"
  "Added sign"
  :type 'string
  :group 'git-gutter)

(defcustom git-gutter:deleted-sign "-"
  "Deleted sign"
  :type 'string
  :group 'git-gutter)

(defcustom git-gutter:unchanged-sign nil
  "Unchanged sign"
  :type 'string
  :group 'git-gutter)

(defcustom git-gutter:hide-gutter nil
  "Hide gutter if there are no changes"
  :type 'boolean
  :group 'git-gutter)

(defcustom git-gutter:lighter " GitGutter"
  "Minor mode lighter in mode-line"
  :type 'string
  :group 'git-gutter)

(defcustom git-gutter:verbosity 4
  "Log/message level. 4 means all, 0 nothing."
  :type 'integer
  :group 'git-gutter)

(defface git-gutter:separator
    '((t (:foreground "cyan" :weight bold)))
  "Face of separator"
  :group 'git-gutter)

(defface git-gutter:modified
    '((t (:foreground "magenta" :weight bold)))
  "Face of modified"
  :group 'git-gutter)

(defface git-gutter:added
    '((t (:foreground "green" :weight bold)))
  "Face of added"
  :group 'git-gutter)

(defface git-gutter:deleted
    '((t (:foreground "red" :weight bold)))
  "Face of deleted"
  :group 'git-gutter)

(defface git-gutter:unchanged
    '((t (:background "yellow")))
  "Face of unchanged"
  :group 'git-gutter)

(defcustom git-gutter:disabled-modes nil
  "A list of modes which `global-git-gutter-mode' should be disabled."
  :type '(repeat symbol)
  :group 'git-gutter)

(defcustom git-gutter:update-threshold 1
  "Minimal update interval for `window-configuration-change-hook'"
  :type 'integer
  :group 'git-gutter)

(defvar git-gutter:view-diff-function 'git-gutter:view-diff-infos
  "Function of viewing changes")

(defvar git-gutter:clear-function 'git-gutter:clear-diff-infos
  "Function of clear changes")

(defvar git-gutter:init-function 'nil
  "Function of initialize")

(defcustom git-gutter-mode-on-hook nil
  "Hook run when git-gutter mode enable"
  :type 'hook
  :group 'git-gutter)

(defcustom git-gutter-mode-off-hook nil
  "Hook run when git-gutter mode disable"
  :type 'hook
  :group 'git-gutter)

(defvar git-gutter:enabled nil)
(defvar git-gutter:toggle-flag t)
(defvar git-gutter:force nil)
(defvar git-gutter:diffinfos nil)
(defvar git-gutter:last-update (make-hash-table :test 'equal))
(defvar git-gutter:base-file-name nil)
(defvar git-gutter:has-indirect-buffers nil)
(defvar git-gutter:from-wcc-hook-p nil)

(defvar git-gutter:popup-buffer "*git-gutter:diff*")

(defmacro git-gutter:awhen (test &rest body)
  "Anaphoric when."
  (declare (indent 1))
  `(let ((it ,test))
     (when it ,@body)))

(defun git-gutter:execute-command (cmd file)
  (if (not (file-remote-p file))
      (call-process-shell-command cmd nil t)
    (process-file-shell-command cmd nil t)))

(defun git-gutter:in-git-repository-p (file)
  (with-temp-buffer
    (let ((cmd "git rev-parse --is-inside-work-tree"))
      (when (zerop (git-gutter:execute-command cmd file))
        (goto-char (point-min))
        (string= "true" (buffer-substring-no-properties
                         (point) (line-end-position)))))))

(defun git-gutter:root-directory (file)
  (with-temp-buffer
    (let* ((cmd "git rev-parse --show-toplevel")
           (ret (git-gutter:execute-command cmd file)))
      (when (zerop ret)
        (goto-char (point-min))
        (let ((root (buffer-substring-no-properties (point) (line-end-position))))
          (unless (string= root "")
            (file-name-as-directory root)))))))

(defsubst git-gutter:changes-to-number (str)
  (if (string= str "")
      1
    (string-to-number str)))

(defsubst git-gutter:make-diffinfo (type content start end)
  (list :type type :content content :start-line start :end-line end))

(defsubst git-gutter:base-file ()
  (buffer-file-name (buffer-base-buffer)))

(defun git-gutter:diff-content ()
  (save-excursion
    (goto-char (line-beginning-position))
    (let ((curpoint (point)))
      (forward-line 1)
      (if (re-search-forward "^@@" nil t)
          (backward-char 3) ;; for '@@'
        (goto-char (point-max)))
      (buffer-substring curpoint (point)))))

(defsubst git-gutter:diff-command (file)
  (format "git --no-pager diff --no-color --no-ext-diff -U0 %s %s"
          git-gutter:diff-option (shell-quote-argument file)))

(defun git-gutter:diff (curfile)
  (let ((cmd (git-gutter:diff-command curfile))
        (regexp "^@@ -\\([0-9]+\\),?\\([0-9]*\\) \\+\\([0-9]+\\),?\\([0-9]*\\) @@")
        (file (git-gutter:base-file))) ;; for tramp
    (when file
      (with-temp-buffer
        (when (zerop (git-gutter:execute-command cmd file))
          (goto-char (point-min))
          (loop while (re-search-forward regexp nil t)
                for orig-line = (string-to-number (match-string 1))
                for new-line  = (string-to-number (match-string 3))
                for orig-changes = (git-gutter:changes-to-number (match-string 2))
                for new-changes = (git-gutter:changes-to-number (match-string 4))
                for type = (cond ((zerop orig-changes) 'added)
                                 ((zerop new-changes) 'deleted)
                                 (t 'modified))
                for end-line = (if (eq type 'deleted)
                                   new-line
                                 (1- (+ new-line new-changes)))
                for content = (git-gutter:diff-content)
                collect
                (let ((start (if (zerop new-line) 1 new-line))
                      (end (if (zerop end-line) 1 end-line)))
                  (git-gutter:make-diffinfo type content start end))))))))

(defun git-gutter:line-to-pos (line)
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (point)))

(defun git-gutter:before-string (sign)
  (let* ((sep-sign git-gutter:separator-sign)
         (sep (when sep-sign
                (propertize sep-sign 'face 'git-gutter:separator)))
         (gutter-sep (concat sign sep)))
    (propertize " " 'display `((margin left-margin) ,gutter-sep))))

(defsubst git-gutter:select-face (type)
  (case type
    (added 'git-gutter:added)
    (modified 'git-gutter:modified)
    (deleted 'git-gutter:deleted)))

(defsubst git-gutter:select-sign (type)
  (case type
    (added git-gutter:added-sign)
    (modified git-gutter:modified-sign)
    (deleted git-gutter:deleted-sign)))

(defun git-gutter:propertized-sign (type)
  (let ((sign (git-gutter:select-sign type))
        (face (git-gutter:select-face type)))
    (propertize sign 'face face)))

(defun git-gutter:view-region (sign start-line end-line)
  (let ((beg (git-gutter:line-to-pos start-line)))
    (goto-char beg)
    (while (and (<= (line-number-at-pos) end-line) (not (eobp)))
      (git-gutter:view-at-pos sign (point))
      (forward-line 1))))

(defun git-gutter:view-at-pos (sign pos)
  (let ((ov (make-overlay pos pos)))
    (overlay-put ov 'before-string (git-gutter:before-string sign))
    (overlay-put ov 'git-gutter t)))

(defun git-gutter:view-diff-info (diffinfo)
  (let* ((start-line (plist-get diffinfo :start-line))
         (end-line (plist-get diffinfo :end-line))
         (type (plist-get diffinfo :type))
         (sign (git-gutter:propertized-sign type)))
    (case type
      ((modified added) (git-gutter:view-region sign start-line end-line))
      (deleted (git-gutter:view-at-pos
                sign (git-gutter:line-to-pos start-line))))))

(defun git-gutter:sign-width (sign)
  (loop for s across sign
        sum (char-width s)))

(defun git-gutter:longest-sign-width ()
  (let ((signs (list git-gutter:modified-sign
                     git-gutter:added-sign
                     git-gutter:deleted-sign)))
    (when git-gutter:unchanged-sign
      (add-to-list 'signs git-gutter:unchanged-sign))
    (+ (apply 'max (mapcar 'git-gutter:sign-width signs))
       (git-gutter:sign-width git-gutter:separator-sign))))

(defun git-gutter:view-for-unchanged ()
  (save-excursion
    (let ((sign (if git-gutter:unchanged-sign
                    (propertize git-gutter:unchanged-sign
                                'face 'git-gutter:unchanged)
                  " ")))
      (goto-char (point-min))
      (while (not (eobp))
        (git-gutter:view-at-pos sign (point))
        (forward-line 1)))))

(defun git-gutter:set-window-margin (width)
  (let ((curwin (get-buffer-window)))
    (set-window-margins curwin width (cdr (window-margins curwin)))))

(defsubst git-gutter:check-file-and-directory ()
  (and (or git-gutter:base-file-name (git-gutter:base-file))
       default-directory (file-directory-p default-directory)))

(defsubst git-gutter:hash-key ()
  (concat git-gutter:base-file-name (buffer-name)))

(defun git-gutter:from-wcc-hook ()
  (let* ((current (float-time))
         (key (git-gutter:hash-key))
         (last-update-time (gethash key git-gutter:last-update))
         (git-gutter:from-wcc-hook-p t))
    (when (or (not last-update-time)
              (not git-gutter:update-threshold)
              (>= current (+ git-gutter:update-threshold last-update-time)))
      (puthash key current git-gutter:last-update)
      (git-gutter))))

(defun git-gutter:at-kill ()
  (remhash (git-gutter:hash-key) git-gutter:last-update))

;;;###autoload
(define-minor-mode git-gutter-mode
  "Git-Gutter mode"
  :group      'git-gutter
  :init-value nil
  :global     nil
  :lighter    git-gutter:lighter
  (if git-gutter-mode
      (let ((basefile (git-gutter:base-file)))
        (if (and (git-gutter:check-file-and-directory)
                 (git-gutter:in-git-repository-p basefile))
            (progn
              (when git-gutter:init-function
                (funcall git-gutter:init-function))
              (make-local-variable 'git-gutter:enabled)
              (set (make-local-variable 'git-gutter:has-indirect-buffers) nil)
              (set (make-local-variable 'git-gutter:base-file-name) basefile)
              (set (make-local-variable 'git-gutter:toggle-flag) t)
              (make-local-variable 'git-gutter:diffinfos)
              (dolist (hook git-gutter:update-hooks)
                (if (eq hook 'window-configuration-change-hook)
                    (add-hook hook 'git-gutter:from-wcc-hook nil t)
                  (add-hook hook 'git-gutter nil t)))
              (add-hook 'kill-buffer-hook 'git-gutter:at-kill nil t)
              (unless global-git-gutter-mode
                (git-gutter)))
          (when (> git-gutter:verbosity 2)
            (message "Here is not Git work tree"))
          (git-gutter-mode -1)))
    (dolist (hook git-gutter:update-hooks)
      (if (eq hook 'window-configuration-change-hook)
          (remove-hook hook 'git-gutter:from-wcc-hook t)
        (remove-hook hook 'git-gutter t)))
    (remove-hook 'kill-buffer-hook t)
    (git-gutter:clear)))

;;;###autoload
(define-global-minor-mode global-git-gutter-mode
  git-gutter-mode
  (lambda ()
    (when (and (buffer-file-name)
               (not (memq major-mode git-gutter:disabled-modes)))
      (git-gutter-mode 1)))
  :group 'git-gutter)

(defsubst git-gutter:show-gutter-p (diffinfos)
  (if git-gutter:hide-gutter
      (or diffinfos git-gutter:unchanged-sign)
    (or global-git-gutter-mode git-gutter:unchanged-sign diffinfos)))

(defun git-gutter:show-gutter (diffinfos)
  (when (git-gutter:show-gutter-p diffinfos)
    (let ((win-width (or git-gutter:window-width
                         (git-gutter:longest-sign-width))))
      (git-gutter:set-window-margin win-width))))

(defun git-gutter:view-diff-infos (diffinfos)
  (when (or git-gutter:unchanged-sign
            git-gutter:separator-sign)
    (git-gutter:view-for-unchanged))
  (when diffinfos
    (save-excursion
      (mapc 'git-gutter:view-diff-info diffinfos)))
  (git-gutter:show-gutter diffinfos))

(defsubst git-gutter:reset-window-margin-p ()
  (or git-gutter:force
      git-gutter:hide-gutter
      (not global-git-gutter-mode)))

(defun git-gutter:clear-diff-infos ()
  (when (git-gutter:reset-window-margin-p)
    (git-gutter:set-window-margin 0))
  (remove-overlays (point-min) (point-max) 'git-gutter t))

(defun git-gutter:update-diffinfo (diffinfos)
  (setq git-gutter:diffinfos diffinfos)
  (save-restriction
    (widen)
    (funcall git-gutter:view-diff-function diffinfos)))

(defun git-gutter:search-near-diff-index (diffinfos is-reverse)
  (loop with current-line = (line-number-at-pos)
        with cmp-fn = (if is-reverse '> '<)
        for diffinfo in (if is-reverse (reverse diffinfos) diffinfos)
        for index = 0 then (1+ index)
        for start-line = (plist-get diffinfo :start-line)
        when (funcall cmp-fn current-line start-line)
        return (if is-reverse
                   (1- (- (length diffinfos) index))
                 index)))

(defun git-gutter:search-here-diffinfo (diffinfos)
  (loop with current-line = (line-number-at-pos)
        for diffinfo in diffinfos
        for start = (plist-get diffinfo :start-line)
        for end   = (or (plist-get diffinfo :end-line) (1+ start))
        when (and (>= current-line start) (<= current-line end))
        return diffinfo))

(defun git-gutter:collect-deleted-line (str)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (loop while (re-search-forward "^-\\(.*?\\)$" nil t)
          collect (match-string 1) into deleted-lines
          finally return deleted-lines)))

(defun git-gutter:delete-added-lines (start-line end-line)
  (forward-line (1- start-line))
  (let ((start-point (point)))
    (forward-line (1+ (- end-line start-line)))
    (delete-region start-point (point))))

(defun git-gutter:insert-deleted-lines (content)
  (dolist (line (git-gutter:collect-deleted-line content))
    (insert (concat line "\n"))))

(defsubst git-gutter:delete-from-first-line-p (start-line end-line)
  (and (not (= start-line 1)) (not (= end-line 1))))

(defun git-gutter:do-revert-hunk (diffinfo)
  (save-excursion
    (goto-char (point-min))
    (let ((start-line (plist-get diffinfo :start-line))
          (end-line (plist-get diffinfo :end-line))
          (content (plist-get diffinfo :content)))
      (case (plist-get diffinfo :type)
        (added (git-gutter:delete-added-lines start-line end-line))
        (deleted (when (git-gutter:delete-from-first-line-p start-line end-line)
                   (forward-line start-line))
                 (git-gutter:insert-deleted-lines content))
        (modified (git-gutter:delete-added-lines start-line end-line)
                  (git-gutter:insert-deleted-lines content))))))

(defsubst git-gutter:popup-buffer-window ()
  (get-buffer-window (get-buffer git-gutter:popup-buffer)))

;;;###autoload
(defun git-gutter:revert-hunk ()
  "Revert current hunk."
  (interactive)
  (git-gutter:awhen (git-gutter:search-here-diffinfo git-gutter:diffinfos)
    (save-window-excursion
      (git-gutter:popup-hunk it)
      (when (yes-or-no-p "Revert current hunk ?")
        (git-gutter:do-revert-hunk it)
        (save-buffer))
      (delete-window (git-gutter:popup-buffer-window)))))

(defun git-gutter:current-file-path ()
  (let ((file (or git-gutter:base-file-name (git-gutter:base-file))))
    (git-gutter:file-path default-directory file)))

(defun git-gutter:diff-header-index-info (path)
  (with-temp-buffer
    (let ((cmd (format "git diff %s" path)))
      (when (zerop (git-gutter:execute-command cmd path))
        (goto-char (point-min))
        (forward-line 4)
        (buffer-substring-no-properties (point-min) (point))))))

(defun git-gutter:hunk-diff-header ()
  (git-gutter:awhen (git-gutter:current-file-path)
    (git-gutter:diff-header-index-info it)))

(defun git-gutter:do-stage-hunk (diff-info)
  (let ((content (plist-get diff-info :content))
        (header (git-gutter:hunk-diff-header))
        (patch (make-temp-name "git-gutter")))
    (when header
      (with-temp-file patch
        (insert header)
        (insert content)
        (insert "\n"))
      (let ((cmd (concat "git apply --unidiff-zero --cached " patch)))
        (unless (zerop (call-process-shell-command cmd))
          (message "Failed: %s" cmd))
        (delete-file patch)))))

;;;###autoload
(defun git-gutter:stage-hunk ()
  "Stage this hunk like 'git add -p'"
  (interactive)
  (git-gutter:awhen (git-gutter:search-here-diffinfo git-gutter:diffinfos)
    (git-gutter:do-stage-hunk it)
    (git-gutter)))

;;;###autoload
(defun git-gutter:popup-hunk (&optional diffinfo)
  "popup current diff hunk"
  (interactive)
  (git-gutter:awhen (or diffinfo
                        (git-gutter:search-here-diffinfo git-gutter:diffinfos))
    (save-selected-window
      (with-current-buffer (get-buffer-create git-gutter:popup-buffer)
        (view-mode -1)
        (erase-buffer)
        (insert (plist-get it :content))
        (insert "\n")
        (goto-char (point-min))
        (diff-mode)
        (view-mode +1)
        (pop-to-buffer (current-buffer))))))

;;;###autoload
(defun git-gutter:next-hunk (arg)
  "Move to next diff hunk"
  (interactive "p")
  (if (not git-gutter:diffinfos)
      (when (> git-gutter:verbosity 3)
        (message "There are no changes!!"))
    (let* ((is-reverse (< arg 0))
           (diffinfos git-gutter:diffinfos)
           (len (length diffinfos))
           (index (git-gutter:search-near-diff-index diffinfos is-reverse))
           (real-index (if index
                           (let ((next (if is-reverse (1+ index) (1- index))))
                             (mod (+ arg next) len))
                         (if is-reverse (1- (length diffinfos)) 0)))
           (diffinfo (nth real-index diffinfos)))
      (goto-char (point-min))
      (forward-line (1- (plist-get diffinfo :start-line)))
      (when (buffer-live-p (get-buffer git-gutter:popup-buffer))
        (save-window-excursion
          (git-gutter:popup-hunk))))))

;;;###autoload
(defun git-gutter:previous-hunk (arg)
  "Move to previous diff hunk"
  (interactive "p")
  (git-gutter:next-diff (- arg)))

(defalias 'git-gutter:next-diff 'git-gutter:next-hunk)
(defalias 'git-gutter:previous-diff 'git-gutter:previous-hunk)
(defalias 'git-gutter:popup-diff 'git-gutter:popup-hunk)

(defun git-gutter:default-directory (dir curfile)
  (if (not (file-remote-p curfile))
      dir
    (let* ((vec (tramp-dissect-file-name curfile))
           (method (aref vec 0))
           (user (aref vec 1))
           (host (aref vec 2)))
      (format "/%s:%s%s:%s" method (if user (concat user "@") "") host dir))))

(defun git-gutter:file-path (dir curfile)
  (if (not (file-remote-p curfile))
      ; Cygwin can't handle Windows absolute path(Case: Mingw Emacs + Cygwin git)
      (if (memq system-type '(windows-nt ms-dos))
          (file-relative-name curfile dir)
        curfile)
    (let ((file (aref (tramp-dissect-file-name curfile) 3)))
      (replace-regexp-in-string (concat "\\`" dir) "" curfile))))

(defun git-gutter:update-indirect-buffers (orig-file)
  (loop with diffinfos = git-gutter:diffinfos
        for win in (window-list)
        for buf  = (window-buffer win)
        for base = (buffer-base-buffer buf)
        when (and base (string= (buffer-file-name base) orig-file))
        do
        (with-current-buffer buf
          (git-gutter:clear)
          (git-gutter:update-diffinfo diffinfos))))

;;;###autoload
(defun git-gutter ()
  "Show diff information in gutter"
  (interactive)
  (git-gutter:clear)
  (when (or git-gutter:force git-gutter:toggle-flag)
    (let ((file (or git-gutter:base-file-name (git-gutter:base-file))))
      (when (and file (file-exists-p file))
        (git-gutter:awhen (git-gutter:root-directory file)
          (let* ((default-directory (git-gutter:default-directory it file))
                 (curfile (git-gutter:file-path default-directory file))
                 (diffinfos (git-gutter:diff curfile)))
            (git-gutter:update-diffinfo diffinfos)
            (when (and git-gutter:has-indirect-buffers
                       (not git-gutter:from-wcc-hook-p))
              (git-gutter:update-indirect-buffers file))
            (setq git-gutter:enabled t)))))))

(defadvice make-indirect-buffer (before git-gutter:has-indirect-buffers activate)
  (when (and git-gutter-mode (not (buffer-base-buffer)))
    (setq git-gutter:has-indirect-buffers t)))

;;;###autoload
(defun git-gutter:clear ()
  "clear diff information in gutter"
  (interactive)
  (save-restriction
    (widen)
    (when git-gutter:clear-function
      (funcall git-gutter:clear-function)))
  (setq git-gutter:enabled nil
        git-gutter:diffinfos nil))

;;;###autoload
(defun git-gutter:toggle ()
  "toggle to show diff information"
  (interactive)
  (let ((git-gutter:force t))
    (if git-gutter:enabled
        (progn
          (git-gutter:clear)
          (setq git-gutter-mode nil))
      (git-gutter)
      (setq git-gutter-mode t))
    (setq git-gutter:toggle-flag git-gutter:enabled)
    (force-mode-line-update)))

(provide 'git-gutter)

;;; git-gutter.el ends here
