(require 'term)

(defcustom eshell-directory-name
  (let* ((dir (concat live-tmp-dir "eshell")))
    (make-directory dir t)
    dir)
  "The directory where Eshell control files should be kept."
  :type 'directory
  :group 'eshell)

;;make sure ansi colour character escapes are honoured
(require 'ansi-color)
(ansi-color-for-comint-mode-on)

;; kill buffer when terminal process is killed
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

(defun live-term-use-utf8 ()
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(add-hook 'term-exec-hook 'live-term-use-utf8)

(defun live-term-paste (&optional string)
 (interactive)
 (process-send-string
  (get-buffer-process (current-buffer))
  (if string string (current-kill 0))))

(defun live-term-hook ()
  (goto-address-mode)
  (define-key term-raw-map "\C-y" 'live-term-paste))

(add-hook 'term-mode-hook 'live-term-hook)

;; rotational ansi-terms

(setq live-current-ansi-term nil)
(setq live-ansi-terminal-path "/usr/local/bin/zsh")

(defun live-ansi-term (program &optional new-buffer-name)
  "Start a terminal-emulator in a new buffer but don't switch to
it. Returns the buffer name of the newly created terminal."
  (interactive (list (read-from-minibuffer "Run program: "
                                           (or explicit-shell-file-name
                                               (getenv "ESHELL")
                                               (getenv "SHELL")
                                               "/bin/sh"))))

  ;; Pick the name of the new buffer.
  (setq term-ansi-buffer-name
        (if new-buffer-name
            new-buffer-name
          (if term-ansi-buffer-base-name
              (if (eq term-ansi-buffer-base-name t)
                  (file-name-nondirectory program)
                term-ansi-buffer-base-name)
            "ansi-term")))

  (setq term-ansi-buffer-name (concat "*" term-ansi-buffer-name "*"))

  ;; In order to have more than one term active at a time
  ;; I'd like to have the term names have the *term-ansi-term<?>* form,
  ;; for now they have the *term-ansi-term*<?> form but we'll see...

  (setq term-ansi-buffer-name (generate-new-buffer-name term-ansi-buffer-name))
  (setq term-ansi-buffer-name (term-ansi-make-term term-ansi-buffer-name program))

  (set-buffer term-ansi-buffer-name)
  (term-mode)
  (term-char-mode)

  ;; I wanna have find-file on C-x C-f -mm
  ;; your mileage may definitely vary, maybe it's better to put this in your
  ;; .emacs ...

  (term-set-escape-char ?\C-x)
  term-ansi-buffer-name)

(defun live-ansi-terminal-buffer-names ()
  (live-filter (lambda (el) (string-match "\\*ansi-term\\.*" el)) (live-list-buffer-names)))

(defun live-show-ansi-terminal ()
  (interactive)
  (when (live-empty-p (live-ansi-terminal-buffer-names))
    (live-ansi-term live-ansi-terminal-path))

  (when (not live-current-ansi-term)
    (setq live-current-ansi-term (car (live-ansi-terminal-buffer-names))))

  (popwin:display-buffer live-current-ansi-term))

(defun live-new-ansi-terminal ()
  (interactive)
  (let* ((term-name (buffer-name (live-ansi-term live-ansi-terminal-path))))
    (setq live-current-ansi-term term-name)
    (popwin:display-buffer live-current-ansi-term)))

;; next need to sort buffer names
;; create a ring of the names
;; find the current terminal in the ring
;; swap the current terminal with the next terminal in the ring
;; popwin display the next terminal
