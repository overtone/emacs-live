;; Personal functions
(defun devn-rdebug ()
  (interactive)
  (insert "require 'ruby-debug'; Debugger.start; Debugger.settings[:autoeval] = 1; Debugger.settings[:autolist] = 1; debugger"))

(defun devn-untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun devn-indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun devn-cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer"
  (interactive)
  (devn-indent-buffer)
  (devn-untabify-buffer)
  (delete-trailing-whitespace))

(defun devn-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end))
    (message "Ah, much better!"))

(defun devn-zap-up-to-char (arg char)
  "Kill up to, but not including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.
Ignores CHAR at point."
  (interactive "p\ncZap up to char: ")
  (let ((direction (if (>= arg 0) 1 -1)))
    (kill-region (point)
                 (progn
                   (forward-char direction)
                   (unwind-protect
                       (search-forward (char-to-string char) nil nil arg)
                     (backward-char direction))
                   (point)))))

(defun devn-recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

;; Personal bindings
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c C-(") 'mark-sexp)
(global-set-key (kbd "M-?") 'hippie-expand)

;; Personal function bindings
(global-set-key (kbd "C-c n") 'devn-cleanup-buffer)
(global-set-key (kbd "C-M-z") 'devn-zap-up-to-char)
(global-set-key (kbd "C-x f") 'devn-recentf-ido-find-file)
