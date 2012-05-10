(require 'comint)
(require 'compile)
(require 'ioke-mode)

;;;; for ioke
(defvar ioke-program-name "ioke"
  "*Program invoked by the run-ioke command")

(defvar inferior-ioke-first-prompt-pattern "^iik>"
  "first prompt regex pattern of ioke interpreter.")

(defvar inferior-ioke-prompt-pattern "^iik>"
  "prompt regex pattern of ioke interpreter.")

;;
;; mode variables
;;
(defvar inferior-ioke-mode-hook nil
  "*Hook for customising inferior-ioke mode.")
(defvar inferior-ioke-mode-map nil
  "*Mode map for inferior-ioke-mode")

(defconst inferior-ioke-error-regexp-alist
       '(("SyntaxError: compile error\n^\\([^\(].*\\):\\([1-9][0-9]*\\):" 1 2)
         ("^\tfrom \\([^\(].*\\):\\([1-9][0-9]*\\)\\(:in `.*'\\)?$" 1 2)))

(cond ((not inferior-ioke-mode-map)
       (setq inferior-ioke-mode-map
             (copy-keymap comint-mode-map))
;       (define-key inferior-ioke-mode-map "\M-\C-x" ;gnu convention
;                  'ioke-send-definition)
;       (define-key inferior-ioke-mode-map "\C-x\C-e" 'ioke-send-last-sexp)
       (define-key inferior-ioke-mode-map "\C-c\C-l" 'ioke-load-file)
))

(defun inf-ioke-keys ()
  "Set local key defs for inf-ioke in ioke-mode"
  (define-key ioke-mode-map "\M-\C-x" 'ioke-send-definition)
;  (define-key ioke-mode-map "\C-x\C-e" 'ioke-send-last-sexp)
  (define-key ioke-mode-map "\C-c\C-b" 'ioke-send-block)
  (define-key ioke-mode-map "\C-c\M-b" 'ioke-send-block-and-go)
  (define-key ioke-mode-map "\C-c\C-x" 'ioke-send-definition)
  (define-key ioke-mode-map "\C-c\M-x" 'ioke-send-definition-and-go)
  (define-key ioke-mode-map "\C-c\C-r" 'ioke-send-region)
  (define-key ioke-mode-map "\C-c\M-r" 'ioke-send-region-and-go)
  (define-key ioke-mode-map "\C-c\C-z" 'switch-to-ioke)
  (define-key ioke-mode-map "\C-c\C-l" 'ioke-load-file)
;  (define-key ioke-mode-map "\C-c\C-s" 'run-ioke)
)

(defvar ioke-buffer nil "current ioke process buffer.")

(defun inferior-ioke-mode ()
  "Major mode for interacting with an inferior ioke (ioke) process.

The following commands are available:
\\{inferior-ioke-mode-map}

A ioke process can be fired up with M-x run-ioke.

Customisation: Entry to this mode runs the hooks on comint-mode-hook and
inferior-ioke-mode-hook (in that order).

You can send text to the inferior ioke process from other buffers containing
Ioke source.
    switch-to-ioke switches the current buffer to the ioke process buffer.
    ioke-send-definition sends the current definition to the ioke process.
    ioke-send-region sends the current region to the ioke process.

    ioke-send-definition-and-go, ioke-send-region-and-go,
        switch to the ioke process buffer after sending their text.
For information on running multiple processes in multiple buffers, see
documentation for variable ioke-buffer.

Commands:
Return after the end of the process' output sends the text from the
    end of process to point.
Return before the end of the process' output copies the sexp ending at point
    to the end of the process' output, and sends it.
Delete converts tabs to spaces as it moves back.
Tab indents for io; with argument, shifts rest
    of expression rigidly with the current line.
C-M-q does Tab on each line starting within following expression.
Paragraphs are separated only by blank lines.  # start comments.
If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it."
  (interactive)
  (comint-mode)
  ;; Customise in inferior-ioke-mode-hook
  ;(setq comint-prompt-regexp "^[^>\n]*>+ *")
  (setq comint-prompt-regexp inferior-ioke-prompt-pattern)
  ;;(scheme-mode-variables)
;;  (ioke-mode-variables)
  (setq major-mode 'inferior-ioke-mode)
  (setq mode-name "Inferior Ioke")
  (setq mode-line-process '(":%s"))
  (use-local-map inferior-ioke-mode-map)
  (setq comint-input-filter (function ioke-input-filter))
  (setq comint-get-old-input (function ioke-get-old-input))
  (compilation-shell-minor-mode t)
  (make-local-variable 'compilation-error-regexp-alist)
  (setq compilation-error-regexp-alist inferior-ioke-error-regexp-alist)
  (run-hooks 'inferior-ioke-mode-hook))

(defvar inferior-ioke-filter-regexp "\\`\\s *\\S ?\\S ?\\s *\\'"
  "*Input matching this regexp are not saved on the history list.
Defaults to a regexp ignoring all inputs of 0, 1, or 2 letters.")

(defun ioke-input-filter (str)
  "Don't save anything matching inferior-ioke-filter-regexp"
  (not (string-match inferior-ioke-filter-regexp str)))

;; adapted from replace-in-string in XEmacs (subr.el)
(defun remove-in-string (str regexp)
  "Remove all matches in STR for REGEXP and returns the new string."
  (let ((rtn-str "") (start 0) match prev-start)
    (while (setq match (string-match regexp str start))
      (setq prev-start start
            start (match-end 0)
            rtn-str (concat rtn-str (substring str prev-start match))))
    (concat rtn-str (substring str start))))

(defun ioke-get-old-input ()
  "Snarf the sexp ending at point"
  (save-excursion
    (let ((end (point)))
      (re-search-backward inferior-ioke-first-prompt-pattern)
      (remove-in-string (buffer-substring (point) end)
                        inferior-ioke-prompt-pattern)
      )))

(defun ioke-args-to-list (string)
  (let ((where (string-match "[ \t]" string)))
    (cond ((null where) (list string))
          ((not (= where 0))
           (cons (substring string 0 where)
                 (ioke-args-to-list (substring string (+ 1 where)
                                                 (length string)))))
          (t (let ((pos (string-match "[^ \t]" string)))
               (if (null pos)
                   nil
                 (ioke-args-to-list (substring string pos
                                                 (length string)))))))))

(defun run-ioke (cmd)
  "Run an inferior Ioke process, input and output via buffer *ioke*.
If there is a process already running in `*ioke*', switch to that buffer.
With argument, allows you to edit the command line (default is value
of `ioke-program-name').  Runs the hooks `inferior-ioke-mode-hook'
\(after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"

  (interactive (list (if current-prefix-arg
                         (read-string "Run Ioke: " ioke-program-name)
                         ioke-program-name)))
  (if (not (comint-check-proc "*ioke*"))
      (let ((cmdlist (ioke-args-to-list cmd)))
        (set-buffer (apply 'make-comint "ioke" (car cmdlist)
                           nil (cdr cmdlist)))
        (inferior-ioke-mode)))
  (setq ioke-program-name cmd)
  (setq ioke-buffer "*ioke*")
  (pop-to-buffer "*ioke*"))

(defconst ioke-send-terminator "--inf-ioke-%x-%d-%d-%d--"
  "Template for ioke here document terminator.
Must not contain ioke meta characters.")

(defconst ioke-eval-separator "")

(defun ioke-send-region (start end)
  "Send the current region to the inferior Ioke process."
  (interactive "r")
  (let (term (file (buffer-file-name)) line)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char start)
        (setq line (+ start (forward-line (- start)) 1))
        (goto-char start)
        (while (progn
                 (setq term (apply 'format ioke-send-terminator (random) (current-time)))
                 (re-search-forward (concat "^" (regexp-quote term) "$") end t)))))
    ;; compilation-parse-errors parses from second line.
    (save-excursion
      (let ((m (process-mark (ioke-proc))))
        (set-buffer (marker-buffer m))
        (goto-char m)
        (insert ioke-eval-separator "\n")
        (set-marker m (point))))
    (comint-send-string (ioke-proc) (format "eval <<'%s', nil, %S, %d\n" term file line))
    (comint-send-region (ioke-proc) start end)
    (comint-send-string (ioke-proc) (concat "\n" term "\n"))))

(defun ioke-send-definition ()
  "Send the current definition to the inferior Ioke process."
  (interactive)
  (save-excursion
    (ioke-end-of-defun)
    (let ((end (point)))
      (ioke-beginning-of-defun)
      (ioke-send-region (point) end))))

;(defun ioke-send-last-sexp ()
;  "Send the previous sexp to the inferior Ioke process."
;  (interactive)
;  (ioke-send-region (save-excursion (backward-sexp) (point)) (point)))

(defun ioke-send-block ()
  "Send the current block to the inferior Ioke process."
  (interactive)
  (save-excursion
    (ioke-end-of-block)
    (end-of-line)
    (let ((end (point)))
      (ioke-beginning-of-block)
      (ioke-send-region (point) end))))

(defun switch-to-ioke (eob-p)
  "Switch to the ioke process buffer.
With argument, positions cursor at end of buffer."
  (interactive "P")
  (if (get-buffer ioke-buffer)
      (pop-to-buffer ioke-buffer)
      (error "No current process buffer. See variable ioke-buffer."))
  (cond (eob-p
         (push-mark)
         (goto-char (point-max)))))

(defun ioke-send-region-and-go (start end)
  "Send the current region to the inferior Ioke process.
Then switch to the process buffer."
  (interactive "r")
  (ioke-send-region start end)
  (switch-to-ioke t))

(defun ioke-send-definition-and-go ()
  "Send the current definition to the inferior Ioke.
Then switch to the process buffer."
  (interactive)
  (ioke-send-definition)
  (switch-to-ioke t))

(defun ioke-send-block-and-go ()
  "Send the current block to the inferior Ioke.
Then switch to the process buffer."
  (interactive)
  (ioke-send-block)
  (switch-to-ioke t))

(defvar ioke-source-modes '(ioke-mode)
  "*Used to determine if a buffer contains Ioke source code.
If it's loaded into a buffer that is in one of these major modes, it's
considered a ioke source file by ioke-load-file.
Used by these commands to determine defaults.")

(defvar ioke-prev-l/c-dir/file nil
  "Caches the last (directory . file) pair.
Caches the last pair used in the last ioke-load-file command.
Used for determining the default in the
next one.")

(defun ioke-load-file (file-name)
  "Load a Ioke file into the inferior Ioke process."
  (interactive (comint-get-source "Load Ioke file: " ioke-prev-l/c-dir/file
                                  ioke-source-modes t)) ; T because LOAD
                                                          ; needs an exact name
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq ioke-prev-l/c-dir/file (cons (file-name-directory    file-name)
                                       (file-name-nondirectory file-name)))
  (comint-send-string (ioke-proc) (concat "(load \""
                                            file-name
                                            "\"\)\n")))

(defun ioke-proc ()
  "Returns the current ioke process. See variable ioke-buffer."
  (let ((proc (get-buffer-process (if (eq major-mode 'inferior-ioke-mode)
                                      (current-buffer)
                                    ioke-buffer))))
    (or proc
        (error "No current process. See variable ioke-buffer"))))

;;; Do the user's customisation...

(defvar inf-ioke-load-hook nil
  "This hook is run when inf-ioke is loaded in.
This is a good place to put keybindings.")

(run-hooks 'inf-ioke-load-hook)

(provide 'inf-ioke)
