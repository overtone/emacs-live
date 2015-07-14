;; This code kindly supplied originally by Hector Levesque
;;
;; Put (autoload 'extempore-mode "/path/to/thisfile" "" t) in .emacs.
;;
;; This mode uses the normal Scheme key bindings except that the sequence
;;    control-x control-x sends a top level def to Extempore
;;    control-x control-r sends a region of defs to Extempore
;;
;; Future work: get TAB completion to work!

(require 'scheme)

(defvar extempore-keydef "\C-x\C-x")     ; key sequence to send a definition
(defvar extempore-keyreg "\C-x\C-r")     ; key sequence to send a region
(defvar extempore-port 7099)             ; TCP port to Extempore
;(defvar extempore-process nil)           ; process during TCP connection
;(make-local-variable 'extempore-process)
(make-variable-buffer-local 'extempore-process)
(setq extempore-process nil)

(defun smart-tab ()
  "This smart tab is minibuffer compliant: it acts as usual in
    the minibuffer. Else, if mark is active, indents region. Else if
    point is at the end of a symbol, expands it. Else indents the
    current line."
  (interactive)
  (if (minibufferp)
      (unless (minibuffer-complete)
	(dabbrev-expand nil))
    (if mark-active
	(indent-region (region-beginning)
		       (region-end))
	(if (looking-at "\\_>")
	    (dabbrev-expand nil)
	  (indent-for-tab-command)))))

;(global-set-key (kbd "TAB") 'smart-tab)
(define-key scheme-mode-map (kbd "TAB") 'smart-tab); # only in scheme-mode

(define-key scheme-mode-map (kbd "RET") 'newline-and-indent)

(defun ext:m1 (name duration)
  (interactive "sName: \nsDuration: ")
  (insert (concat "(define " name
		  "\n  (lambda (beat dur)\n    "
		  "(callback (*metro* (+ beat (* .5 " duration "))) '"
		  name " (+ beat " duration ") " duration ")))\n\n"
		  "(" name " (*metro* 'get-beat 4) " duration ")")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add some stuff to scheme mode

(font-lock-add-keywords 'scheme-mode
  '(("definec" . font-lock-keyword-face)
    ("bind-func" . font-lock-keyword-face)
    ("definec:dsp" . font-lock-keyword-face)
    ("bind-instrument" . font-lock-keyword-face)
    ("bind-val" . font-lock-keyword-face)
    ("bind-typevar" . font-lock-keyword-face)
    ("bind-alias" . font-lock-keyword-face)
    ("bind-type" . font-lock-keyword-face)
    ("bind-poly" . font-lock-keyword-face)
    ("dotimes" . font-lock-keyword-face)
    ("memzone" . font-lock-keyword-face)
    ("bind-lib" . font-lock-keyword-face)))

(put 'dotimes 'scheme-indent-function 1)
(put 'bind-func 'scheme-indent-function 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; handle extempore minor mode stuff

(define-minor-mode extempore-mode
   "Toggle the mode for interacting with Scheme in Extempore over TCP"
   :init-value nil :lighter " Extempore" :keymap scheme-mode-map
   (if extempore-mode (extempore-connect "localhost" 7099) (extempore-stop)))

(defun extempore-connect (host port)     ; start connection to Extempore
  (interactive "sHostname: \nnPort: ")
  (define-key scheme-mode-map extempore-keydef 'extempore-send-definition)
  (define-key scheme-mode-map extempore-keyreg 'extempore-send-region)
  (if (not (null extempore-process))
      (delete-process extempore-process))
  (setq extempore-process
	(open-network-stream "extempore" nil
			     (if (null host) "localhost" host)
			     (if (null port) 7099 port)))
  (set-process-filter extempore-process
	'(lambda (proc str) (message (substring str 0 -1)))))

(defun extempore-stop ()                 ; terminate connection to Extempore
  (interactive)
  (delete-process extempore-process)
  (setq extempore-process nil))

(defun extempore-send-definition ()
  "Send the enclosing top-level def to Extempore server for evaluation"
  (interactive)
  (save-excursion
    (mark-defun)
    (let ((str (concat (buffer-substring (point) (mark))
		       "\r\n")))
      (process-send-string extempore-process str)
      ; (process-send-region extempore-process (point) (mark))
      (redisplay)                          ; flash the def like Extempore
      (sleep-for .25))))

(defun extempore-send-region ()
  "Send the current region (or all the buffer) to Extempore for evaluation"
  (interactive)
  (save-excursion
    (if mark-active
	(unless (= (point) (region-beginning)) (exchange-point-and-mark))
      (progn (goto-char (point-min)) (set-mark (point-max))))
    (let ((start (region-beginning)) (end (region-end)))
      (while (re-search-forward "^[^\n;]*(" end t)
	(extempore-send-definition)
	(end-of-defun)))))
