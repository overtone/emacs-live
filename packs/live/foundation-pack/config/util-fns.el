;;handy util fns

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
  "Byte-compile all your packs"
  (interactive)
  (byte-recompile-directory-sl live-packs-dir 0 1 1))

(defun insert-date ()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

(defun delete-this-buffer-and-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun what-face (pos)
  "Return the name of the face at point"
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun rotate-windows ()
  "Rotate your windows" (interactive) (cond ((not (> (count-windows) 1)) (message "You can't rotate a single window!"))
 (t
  (setq i 1)
  (setq numWindows (count-windows))
  (while  (< i numWindows)
    (let* (
           (w1 (elt (window-list) i))
           (w2 (elt (window-list) (+ (% i numWindows) 1)))
           (b1 (window-buffer w1))
           (b2 (window-buffer w2))
           (s1 (window-start w1))
           (s2 (window-start w2))
           )
      (set-window-buffer w1  b2)
      (set-window-buffer w2 b1)
      (set-window-start w1 s2)
      (set-window-start w2 s1)
      (setq i (1+ i)))))))
