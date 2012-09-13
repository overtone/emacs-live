;; OS X specific configuration
;; ---------------------------

(setq default-input-method "MacOSX")

;; Make cut and paste work with the OS X clipboard

(defun live-copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun live-paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

 (when (not window-system)
   (setq interprogram-cut-function 'live-paste-to-osx)
   (setq interprogram-paste-function 'live-copy-from-osx))

;; Work around a bug on OS X where system-name is a fully qualified
;; domain name
(setq system-name (car (split-string system-name "\\.")))

;; On OS X Emacs doesn't use the shell PATH if it's not started from
;; the shell.
;; If you're using homebrew or port, modifying the PATH is essential.
(let (osx-paths)
  (dolist (path '("/usr/local/bin" "/opt/local/bin" "/opt/local/sbin") (setenv "PATH" (concat osx-paths (getenv "PATH"))))
    (push path exec-path)
    (setq osx-paths (concat (concat path ":") osx-paths))))

;; Ignore .DS_Store files with ido mode
(add-to-list 'ido-ignore-files "\\.DS_Store")
