;;; git-gutter.el --- Port of Sublime Text 2 plugin GitGutter

;; Copyright (C) 2013 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-git-gutter
;; Version: 0.23

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
;; Port of GitGutter which is a plugin of Sublime Text2

;;; Code:

(eval-when-compile
  (require 'cl))

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
  "Deleted sign"
  :type 'string
  :group 'git-gutter)

(defcustom git-gutter:always-show-gutter nil
  "Always show gutter"
  :type 'boolean
  :group 'git-gutter)

(defcustom git-gutter:lighter " GitGutter"
  "Minor mode lighter in mode-line"
  :type 'string
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

(defvar git-gutter:view-diff-function 'git-gutter:view-diff-infos
  "Function of viewing changes")

(defvar git-gutter:clear-function nil
  "Function of clear changes")

(defvar git-gutter:init-function 'nil
  "Function of initialize")

(defvar git-gutter:enabled nil)
(defvar git-gutter:diffinfos nil)

(defvar git-gutter:popup-buffer "*git-gutter:diff*")

(defmacro git-gutter:awhen (test &rest body)
  "Anaphoric when."
  (declare (indent 1))
  `(let ((it ,test))
     (when it ,@body)))

(defun git-gutter:in-git-repository-p ()
  (with-temp-buffer
    (let ((cmd "git rev-parse --is-inside-work-tree"))
      (when (zerop (call-process-shell-command cmd nil t))
        (goto-char (point-min))
        (string= "true" (buffer-substring-no-properties
                         (point) (line-end-position)))))))

(defun git-gutter:root-directory ()
  (with-temp-buffer
    (let* ((cmd "git rev-parse --show-toplevel")
           (ret (call-process-shell-command cmd nil t)))
      (when (zerop ret)
        (goto-char (point-min))
        (let ((root (buffer-substring-no-properties (point) (line-end-position))))
          (unless (string= root "")
            (file-name-as-directory root)))))))

(defun git-gutter:changes-to-number (str)
  (if (string= str "")
      1
    (string-to-number str)))

(defun git-gutter:make-diffinfo (type content start &optional end)
  (list :type type :content content :start-line start :end-line end))

(defun git-gutter:diff-content ()
  (save-excursion
    (goto-char (line-beginning-position))
    (let ((curpoint (point)))
      (forward-line 1)
      (if (re-search-forward "^@@" nil t)
          (backward-char 3) ;; for '@@'
        (goto-char (point-max)))
      (buffer-substring curpoint (point)))))

(defun git-gutter:diff (curfile)
  (let ((cmd (format "git diff --no-ext-diff -U0 %s %s" git-gutter:diff-option curfile))
        (regexp "^@@ -\\([0-9]+\\),?\\([0-9]*\\) \\+\\([0-9]+\\),?\\([0-9]*\\) @@"))
    (with-temp-buffer
      (when (zerop (call-process-shell-command cmd nil t))
        (goto-char (point-min))
        (loop while (re-search-forward regexp nil t)
              for orig-line = (string-to-number (match-string 1))
              for new-line  = (string-to-number (match-string 3))
              for orig-changes = (git-gutter:changes-to-number (match-string 2))
              for new-changes = (git-gutter:changes-to-number (match-string 4))
              for end-line = (1- (+ new-line new-changes))
              for content = (git-gutter:diff-content)
              collect
              (cond ((zerop orig-changes)
                     (git-gutter:make-diffinfo 'added content new-line end-line))
                    ((zerop new-changes)
                     (git-gutter:make-diffinfo 'deleted content new-line))
                    (t
                     (git-gutter:make-diffinfo
                      'modified content new-line end-line))))))))

(defun git-gutter:line-to-pos (line)
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (point)))

(defmacro git-gutter:before-string (sign)
  `(propertize " " 'display `((margin left-margin) ,sign)))

(defun git-gutter:select-face (type)
  (case type
    (added 'git-gutter:added)
    (modified 'git-gutter:modified)
    (deleted 'git-gutter:deleted)))

(defun git-gutter:select-sign (type)
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
    (apply 'max (mapcar 'git-gutter:sign-width signs))))

(defun git-gutter:view-for-unchanged ()
  (save-excursion
    (let ((sign (propertize git-gutter:unchanged-sign
                            'face 'git-gutter:unchanged)))
      (goto-char (point-min))
      (while (not (eobp))
        (git-gutter:view-at-pos sign (point))
        (forward-line 1)))))

(defun git-gutter:view-diff-infos (diffinfos)
  (let ((curwin (get-buffer-window))
        (win-width (or git-gutter:window-width
                       (git-gutter:longest-sign-width))))
    (when git-gutter:unchanged-sign
      (git-gutter:view-for-unchanged))
    (when diffinfos
      (save-excursion
        (mapc 'git-gutter:view-diff-info diffinfos)))
    (when (or git-gutter:always-show-gutter diffinfos git-gutter:unchanged-sign)
      (set-window-margins curwin win-width (cdr (window-margins curwin))))))

(defun git-gutter:process-diff (curfile)
  (let ((diffinfos (git-gutter:diff curfile)))
    (when diffinfos
      (setq git-gutter:diffinfos diffinfos)
      (funcall git-gutter:view-diff-function diffinfos))))

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

(defun git-gutter:search-here-diff (diffinfos)
  (loop with current-line = (line-number-at-pos)
        for diffinfo in diffinfos
        for start = (plist-get diffinfo :start-line)
        for end   = (or (plist-get diffinfo :end-line) (1+ start))
        when (and (>= current-line start) (<= current-line end))
        return (plist-get diffinfo :content)))

;;;###autoload
(defun git-gutter:popup-diff ()
  (interactive)
  (git-gutter:awhen (git-gutter:search-here-diff git-gutter:diffinfos)
    (save-selected-window
      (with-current-buffer (get-buffer-create git-gutter:popup-buffer)
        (erase-buffer)
        (insert it)
        (insert "\n")
        (goto-char (point-min))
        (diff-mode)
        (pop-to-buffer (current-buffer))))))

;;;###autoload
(defun git-gutter:next-diff (arg)
  (interactive "p")
  (when git-gutter:diffinfos
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
          (git-gutter:popup-diff))))))

;;;###autoload
(defun git-gutter:previous-diff (arg)
  (interactive "p")
  (git-gutter:next-diff (- arg)))

;;;###autoload
(defun git-gutter ()
  (interactive)
  (git-gutter:clear)
  (let ((file (buffer-file-name)))
    (when (and file (file-exists-p file))
      (git-gutter:awhen (git-gutter:root-directory)
        (let ((default-directory it)
              (current-file (file-relative-name file it)))
          (git-gutter:process-diff current-file)
          (setq git-gutter:enabled t))))))

;;;###autoload
(defun git-gutter:clear ()
  (interactive)
  (when git-gutter:clear-function
    (funcall git-gutter:clear-function))
  (remove-overlays (point-min) (point-max) 'git-gutter t)
  (setq git-gutter:enabled nil
        git-gutter:diffinfos nil)
  (unless git-gutter:always-show-gutter
    (let ((curwin (get-buffer-window)))
      (set-window-margins curwin 0 (cdr (window-margins curwin))))))

;;;###autoload
(defun git-gutter:toggle ()
  (interactive)
  (if git-gutter:enabled
      (git-gutter:clear)
    (git-gutter)))

(defun git-gutter:check-file-and-directory ()
  (and (buffer-file-name)
       default-directory (file-directory-p default-directory)))

;;;###autoload
(define-minor-mode git-gutter-mode ()
  "Git-Gutter mode"
  :group      'git-gutter
  :init-value nil
  :global     nil
  :lighter    git-gutter:lighter
  (if git-gutter-mode
      (if (and (git-gutter:check-file-and-directory)
               (git-gutter:in-git-repository-p))
          (progn
            (when git-gutter:init-function
              (funcall git-gutter:init-function))
            (make-local-variable 'git-gutter:enabled)
            (make-local-variable 'git-gutter:diffinfos)
            (add-hook 'after-save-hook 'git-gutter nil t)
            (add-hook 'after-revert-hook 'git-gutter nil t)
            (add-hook 'change-major-mode-hook 'git-gutter nil t)
            (add-hook 'window-configuration-change-hook 'git-gutter nil t)
            (run-with-idle-timer 0 nil 'git-gutter))
        (message "Here is not Git work tree")
        (git-gutter-mode -1))
    (remove-hook 'after-save-hook 'git-gutter t)
    (remove-hook 'after-revert-hook 'git-gutter t)
    (remove-hook 'change-major-mode-hook 'git-gutter t)
    (remove-hook 'window-configuration-change-hook 'git-gutter t)
    (git-gutter:clear)))

;;;###autoload
(define-global-minor-mode global-git-gutter-mode
  git-gutter-mode
  (lambda ()
    (when (and (not (minibufferp)) (buffer-file-name))
      (git-gutter-mode 1)))
  :group 'git-gutter)

(provide 'git-gutter)

;;; git-gutter.el ends here
