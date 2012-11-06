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

;; Ensure the exec-path honours the shell PATH
(live-add-pack-lib "exec-path-from-shell")
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; Ignore .DS_Store files with ido mode
(add-to-list 'ido-ignore-files "\\.DS_Store")
