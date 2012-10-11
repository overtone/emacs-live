;;handy util fns

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

(defun live-delete-whitespace-except-one ()
  (interactive)
  (just-one-space -1))


(defun live-backwards-kill-line ()
  "Kill all characters on current line before point. Same as
  passing 0 as an argument to kill-line"
  (interactive)
  (kill-line 0))

(defun live-end-of-buffer-p ()
  "Predicate fn to determine whether point is at the end of the
   buffer"
  (<= (buffer-size) (point)))
