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
