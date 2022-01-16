;;; org-persist.el --- Persist data across Emacs sessions         -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022 Free Software Foundation, Inc.

;; Author: Ihor Radchenko <yantar92 at gmail dot com>
;; Keywords: cache, storage

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This file implements persistant data storage across Emacs sessions.
;; Both global and buffer-local data can be stored.

;;; Code:

(require 'org-compat)
(require 'org-id)
(require 'xdg nil t)

(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-next-visible-heading "org" (arg))
(declare-function org-at-heading-p "org" (&optional invisible-not-ok))


(defgroup org-persist nil
  "Persistent cache for Org mode."
  :tag "Org persist"
  :group 'org)

(defcustom org-persist-directory (expand-file-name
                       (org-file-name-concat
                        (let ((cache-dir (when (fboundp 'xdg-cache-home)
                                           (xdg-cache-home))))
                          (if (or (seq-empty-p cache-dir)
                                  (not (file-exists-p cache-dir))
                                  (file-exists-p (org-file-name-concat
                                                  user-emacs-directory
                                                  "org-persist")))
                              user-emacs-directory
                            cache-dir))
                        "org-persist/"))
  "Directory where the data is stored."
  :group 'org-persist
  :type 'directory)

(defvar org-persist-index-file "index"
  "File name used to store the data index.")

(defvar org-persist-before-write-hook nil
  "Abnormal hook ran before saving data for a single variable in a buffer.
The hook must accept the same arguments as `org-persist-write'.
The hooks will be evaluated until a hook returns non-nil.
If any of the hooks return non-nil, do not save the data.")

(defvar org-persist-before-read-hook nil
  "Abnormal hook ran before reading data for a single variable in a buffer.
The hook must accept the same arguments as `org-persist-read'.
The hooks will be evaluated until a hook returns non-nil.
If any of the hooks return non-nil, do not read the data.")

(defvar org-persist-after-read-hook nil
  "Abnormal hook ran after reading data for a single variable in a buffer.
The hook must accept the same arguments as `org-persist-read'.")

(defvar org-persist--index nil
  "Global index.

The index is a list of plists.  Each plist contains information about
a data variable.  Each plist contains the following properties:

  - `:variable'    list of variables to be stored in single file
  - `:persist-file': data file name
  - `:path':       buffer file path, if any
  - `:inode':      buffer file inode, if any
  - `:hash':       buffer hash, if any")

(defvar org-persist--report-time 0.5
  "Whether to report read/write time.

When the value is a number, it is a threshold number of seconds.  If
the read/write time of a single variable exceeds the threashold, a
message is displayed.

When the value is a non-nil non-number, always display the message.
When the value is nil, never diplay the message.")

(defun org-persist--get-index (var &optional buffer)
  "Return plist used to store VAR in BUFFER.
When BUFFER is nil, return plist for global VAR."
  (org-persist--read-index)
  (let* ((buffer-file (when buffer (buffer-file-name (or (buffer-base-buffer buffer)
                                                         buffer))))
         (inode (when buffer-file
                  (and (fboundp 'file-attribute-inode-number)
                       (file-attribute-inode-number (file-attributes buffer-file)))))
         (buffer-hash (when buffer (secure-hash 'md5 buffer))))
    (let ((result (seq-find (lambda (plist)
                              (and (or (memq var (plist-get plist :variable))
                                       (eq var (plist-get plist :variable)))
                                   (or (and inode (equal inode (plist-get plist :inode)))
                                       (and buffer-file (equal buffer-file (plist-get plist :path)))
                                       (and buffer-hash (equal buffer-hash (plist-get plist :hash))))))
                            org-persist--index)))
      (when result
        (unless (equal buffer-file (plist-get result :path))
          (setf result (plist-put result :path buffer-file))))
      (unless result
        (push (list :variable (if (listp var) var (list var))
                    :persist-file (replace-regexp-in-string "^.." "\\&/" (org-id-uuid))
                    :path buffer-file
                    :inode inode
                    :hash buffer-hash)
              org-persist--index)
        (setf result (car org-persist--index)))
      result)))

(defun org-persist--read-index ()
  "Read `org-persist--index'"
  (unless org-persist--index
    (when (file-exists-p (org-file-name-concat org-persist-directory org-persist-index-file))
      (with-temp-buffer
        (insert-file-contents (org-file-name-concat org-persist-directory org-persist-index-file))
        (setq org-persist--index
              (condition-case err
                  (read (current-buffer))
                ;; Recover gracefully if index file is corrupted.
                (error
                 (warn "Emacs reader failed to read data for `org-persist--index' from %S. The error was: %S"
                       (org-file-name-concat org-persist-directory org-persist-index-file)
                       (error-message-string err))
                 nil)))))))

(cl-defun org-persist-register (var &optional buffer &key inherit)
  "Register VAR in BUFFER to be persistent.
Optional key INHERIT make VAR dependent on another variable.  Such
dependency means that data shared between variables will be preserved
(see elisp#Circular Objects)."
  (unless org-persist--index (org-persist--read-index))
  (when inherit
    (let ((inherited-index (org-persist--get-index inherit buffer)))
      (unless (memq var (plist-get inherited-index :variable))
        (setq inherited-index
              (plist-put inherited-index :variable
                         (cons var (plist-get inherited-index :variable)))))))
  (org-persist--get-index var buffer)
  (when buffer
    (add-hook 'kill-buffer-hook #'org-persist-write-all-buffer nil 'local)))

(defun org-persist-unregister (var &optional buffer)
  "Unregister VAR in BUFFER to be persistent.
When BUFFER is `all', unregister VAR in all buffers."
  (unless org-persist--index (org-persist--read-index))
  (setq org-persist--index
        (seq-remove
         (lambda (plist)
           (when (and (memq var (plist-get plist :variable))
                      (or (eq buffer 'all)
                          (string= (buffer-file-name
                                    (or (buffer-base-buffer buffer)
                                        buffer))
                                   (or (plist-get plist :path) ""))))
             (if (> (length (plist-get plist :variable)) 1)
                 (progn
                   (setq plist
                         (plist-put plist :variable
                                    (delq var (plist-get plist :variable))))
                   ;; Do not remove the index though.
                   nil)
               (let ((persist-file (org-file-name-concat org-persist-directory (plist-get plist :persist-file))))
                 (delete-file persist-file)
                 (when (org-directory-empty-p (file-name-directory persist-file))
                   (delete-directory (file-name-directory persist-file))))
               'delete-from-index)))
         org-persist--index))
  (org-persist-gc))

(defun org-persist-write (var &optional buffer)
  "Save buffer-local data in BUFFER for VAR."
  (unless (and buffer (not (get-buffer buffer)))
    (unless (listp var) (setq var (list var)))
    (with-current-buffer (or buffer (current-buffer))
      (let ((index (org-persist--get-index var buffer))
            (start-time (float-time)))
        (setf index (plist-put index :hash (when buffer (secure-hash 'md5 buffer))))
        (let ((print-circle t)
              print-level
              print-length
              print-quoted
              (print-escape-control-characters t)
              (print-escape-nonascii t)
              (print-continuous-numbering t)
              print-number-table)
          (unless (seq-find (lambda (v)
                              (run-hook-with-args-until-success 'org-persist-before-write-hook v buffer))
                            (plist-get index :variable))
            (unless (file-exists-p org-persist-directory)
              (make-directory org-persist-directory))
            (unless (file-exists-p org-persist-directory)
              (warn "Failed to create org-persist storage in %s."
                    org-persist-directory)
              (let ((dir (directory-file-name
                          (file-name-as-directory org-persist-directory))))
                (while (and (not (file-exists-p dir))
                            (not (equal dir (setq dir (directory-file-name
                                                     (file-name-directory dir)))))))
                (unless (file-writable-p dir)
                  (message "Missing write access rights to org-persist-directory: %S"
                           org-persist-directory))))
            (when (file-exists-p org-persist-directory)
              (with-temp-file (org-file-name-concat org-persist-directory org-persist-index-file)
                (prin1 org-persist--index (current-buffer)))
              (let ((file (org-file-name-concat org-persist-directory (plist-get index :persist-file)))
                    (data (mapcar (lambda (s) (cons s (symbol-value s)))
                                  (plist-get index :variable))))
                (unless (file-exists-p (file-name-directory file))
                  (make-directory (file-name-directory file) t))
                (with-temp-file file
                  (prin1 data (current-buffer)))
                (let ((duration (- (float-time) start-time)))
                  (when (or (and org-persist--report-time
                                 (numberp org-persist--report-time)
                                 (>= duration org-persist--report-time))
                            (and org-persist--report-time
                                 (not (numberp org-persist--report-time))))
                    (if buffer
                        (message "org-persist: Writing %S from %S took %.2f sec"
                                 var buffer duration)
                      (message "org-persist: Writing %S took %.2f sec"
                               var duration))))))))))))

(defun org-persist-write-all (&optional buffer)
  "Save all the persistent data."
  (unless (and buffer (not (buffer-file-name buffer)))
    (dolist (index org-persist--index)
      (when (or (and (not (plist-get index :path))
                     (not buffer))
                (and (plist-get index :path)
                     (get-file-buffer (plist-get index :path))
                     (equal (buffer-file-name
                             (or buffer
                                 (get-file-buffer (plist-get index :path))))
                            (plist-get index :path))))
        (org-persist-write (plist-get index :variable)
                (when (plist-get index :path)
                  (get-file-buffer (plist-get index :path))))))))

(defun org-persist-write-all-buffer ()
  "Call `org-persist-write-all' in current buffer."
  (org-persist-write-all (current-buffer)))

(defun org-persist-read (var &optional buffer)
  "Restore VAR data in BUFFER."
  (let* ((index (org-persist--get-index var buffer))
         (persist-file (org-file-name-concat org-persist-directory (plist-get index :persist-file)))
         (data nil)
         (start-time (float-time)))
    (when (and index
               (file-exists-p persist-file)
               (or (not buffer)
                   (equal (secure-hash 'md5 buffer) (plist-get index :hash))))
      (unless (seq-find (lambda (v)
                          (run-hook-with-args-until-success 'org-persist-before-read-hook v buffer))
                        (plist-get index :variable))
        (with-temp-buffer
          (let ((coding-system-for-read 'utf-8)
                (read-circle t))
            (insert-file-contents persist-file))
          ;; FIXME: Reading sometimes fails to read circular objects.
          ;; I suspect that it happens when we have object reference
          ;; #N# read before object definition #N=.  If it is really
          ;; so, it should be Emacs bug - either in `read' or in
          ;; `prin1'.  Meanwhile, just fail silently when `read'
          ;; fails to parse the saved cache object.
          (condition-case err
              (setq data (read (current-buffer)))
            (error
             ;; Do not report the known error to user.
             (unless (string-match-p "Invalid read syntax" (error-message-string err))
               (warn "Emacs reader failed to read data for %S:%S. The error was: %S"
                     (or buffer "global") var (error-message-string err)))
             (setq data nil))))
        (with-current-buffer (or buffer (current-buffer))
          (cl-loop for var1 in (plist-get index :variable)
                   do
                   (when (alist-get var1 data)
                     (setf (symbol-value var1) (alist-get var1 data)))
                   (run-hook-with-args 'org-persist-after-read-hook var1 buffer)))
        (let ((duration (- (float-time) start-time)))
          (when (or (and org-persist--report-time
                         (numberp org-persist--report-time)
                         (>= duration org-persist--report-time))
                    (and org-persist--report-time
                         (not (numberp org-persist--report-time))))
            (if buffer
                (message "org-persist: Reading %S from %S took %.2f sec"
                         var buffer duration)
              (message "org-persist: Reading %S took %.2f sec"
                       var duration))))))))

(defun org-persist-read-all (&optional buffer)
  "Restore all the persistent data in BUFFER."
  (unless org-persist--index (org-persist--read-index))
  (dolist (index org-persist--index)
    (org-persist-read (plist-get index :variable) buffer)))

(defun org-persist-read-all-buffer ()
  "Call `org-persist-read-all' in current buffer."
  (org-persist-read-all (current-buffer)))

(defun org-persist-gc ()
  "Remove stored data for not existing files or unregistered variables."
  (let (new-index)
    (dolist (index org-persist--index)
      (let ((file (plist-get index :path))
            (persist-file (when (plist-get index :persist-file)
                            (org-file-name-concat
                             org-persist-directory
                             (plist-get index :persist-file)))))
        (when (and file persist-file)
          (if (file-exists-p file)
              (push index new-index)
            (when (file-exists-p persist-file)
              (delete-file persist-file)
              (when (org-directory-empty-p (file-name-directory persist-file))
                (delete-directory (file-name-directory persist-file))))))))
    (setq org-persist--index (nreverse new-index))))

;; Automatically write the data, but only when we have write access.
(let ((dir (directory-file-name
            (file-name-as-directory org-persist-directory))))
  (while (and (not (file-exists-p dir))
              (not (equal dir (setq dir (directory-file-name
                                       (file-name-directory dir)))))))
  (if (not (file-writable-p dir))
      (message "Missing write access rights to org-persist-directory: %S"
               org-persist-directory)
    (add-hook 'kill-emacs-hook #'org-persist-write-all)
    ;; `org-persist-gc' should run before `org-persist-write-all'.  So we are adding the
    ;; hook after `org-persist-write-all'.
    (add-hook 'kill-emacs-hook #'org-persist-gc)))

(add-hook 'after-init-hook #'org-persist-read-all)

(provide 'org-persist)

;;; org-persist.el ends here
