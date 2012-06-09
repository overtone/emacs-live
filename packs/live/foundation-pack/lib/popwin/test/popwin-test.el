(require 'popwin)

(defmacro test (explain &rest body)
  (declare (indent 1))
  `(let ((window-config (current-window-configuration)))
     (unwind-protect
         (progn
           (delete-other-windows)
           (let ((success (progn ,@body)))
             (set-window-configuration window-config)
             (unless success
               (error "failed: %s" ,explain))))
       (popwin:close-popup-window))))

(defmacro ui-test (prompt &rest body)
  (declare (indent 1))
  `(test ,prompt ,@body (yes-or-no-p ,prompt)))

(setq display-buffer-function 'popwin:display-buffer)
(setq popwin:popup-window-position 'bottom)
(defvar buf1 (get-buffer-create "*buf1*"))
(defvar buf2 (get-buffer-create "*buf2*"))
(defvar buf3 (get-buffer-create "*buf3*"))

(test "*buf2* selected?"
  (switch-to-buffer buf1)
  (split-window-horizontally)
  (other-window 1)
  (switch-to-buffer buf2)
  (popwin:popup-buffer buf3)
  (popwin:close-popup-window)
  (message "%s" (current-buffer))
  (eq buf2 (current-buffer)))

(test "select"
  (switch-to-buffer buf1)
  (popwin:popup-buffer buf1)
  (eq popwin:popup-window
      (popwin:select-popup-window)))

(test "stick"
  (switch-to-buffer buf1)
  (popwin:popup-buffer buf1)
  (popwin:stick-popup-window))

(ui-test "popup?"
  (switch-to-buffer buf1)
  (popwin:popup-buffer buf2))

(ui-test "popup?"
  (switch-to-buffer buf1)
  (split-window-horizontally)
  (popwin:popup-buffer buf2))

(ui-test "popup?"
  (switch-to-buffer buf1)
  (split-window-vertically)
  (popwin:popup-buffer buf2))

(ui-test "popup at bottom?"
  (switch-to-buffer buf1)
  (popwin:popup-buffer buf2 :position 'bottom))

(ui-test "popup at left?"
  (switch-to-buffer buf1)
  (popwin:popup-buffer buf2 :position 'left))

(ui-test "popup at top?"
  (switch-to-buffer buf1)
  (popwin:popup-buffer buf2 :position 'top))

(ui-test "popup at right?"
  (switch-to-buffer buf1)
  (popwin:popup-buffer buf2 :position 'right))

(ui-test "popup at bottom with 50%?"
  (switch-to-buffer buf1)
  (popwin:popup-buffer buf2 :height 0.5 :position 'bottom))

(ui-test "popup at left with 50%?"
  (switch-to-buffer buf1)
  (popwin:popup-buffer buf2 :width 0.5 :position 'left))

(ui-test "popup with three columns (bottom)?"
  (switch-to-buffer buf1)
  (split-window-vertically)
  (split-window-vertically)
  (popwin:popup-buffer buf2 :position 'bottom))

(ui-test "popup with three columns (left)?"
  (switch-to-buffer buf1)
  (split-window-horizontally)
  (split-window-horizontally)
  (popwin:popup-buffer buf2 :position 'left))

(ui-test "popup with three columns (top)?"
  (switch-to-buffer buf1)
  (split-window-vertically)
  (split-window-vertically)
  (popwin:popup-buffer buf2 :position 'top))

(ui-test "popup with three columns (right)?"
  (switch-to-buffer buf1)
  (split-window-horizontally)
  (split-window-horizontally)
  (popwin:popup-buffer buf2 :position 'right))

(ui-test "popup *buf1*?"
  (switch-to-buffer buf1)
  (let ((popwin:special-display-config '(("*buf1*"))))
    (popwin:display-buffer buf1)))

(ui-test "popup *buf1*?"
  (switch-to-buffer buf1)
  (let ((popwin:special-display-config '((fundamental-mode))))
    (popwin:display-buffer buf1)))

(ui-test "popup *buf1*?"
  (switch-to-buffer buf1)
  (let ((popwin:special-display-config '(("\\*buf[0-9]\\*" :regexp t))))
    (popwin:display-buffer buf1)))

(ui-test "popup *buf1* again?"
  (switch-to-buffer buf1)
  (popwin:display-last-buffer))

(ui-test "display *buf2* normally?"
  (switch-to-buffer buf1)
  (let (popwin:special-display-config)
    (popwin:display-buffer buf2)))

(ui-test "popup *buf1* tail"
  (switch-to-buffer buf1)
  (popwin:popup-buffer-tail buf1))

(ui-test "popup-buffer interactively?"
  (switch-to-buffer buf1)
  (call-interactively 'popwin:popup-buffer))

(ui-test "display-buffer interactively?"
  (switch-to-buffer buf1)
  (call-interactively 'popwin:display-buffer))

(ui-test "popup-buffer-tail interactively?"
  (switch-to-buffer buf1)
  (call-interactively 'popwin:popup-buffer-tail))

(ui-test "find-file interactively?"
  (switch-to-buffer buf1)
  (call-interactively 'popwin:find-file))

(ui-test "find-file-tail?"
  (switch-to-buffer buf1)
  (call-interactively 'popwin:find-file-tail))

(ui-test "messages?"
  (switch-to-buffer buf1)
  (call-interactively 'popwin:messages))

;; test-case M-x occur and M-x next-error
;; test-case M-x dired and o
;; test-case fixed size popwin
;; test-case partial-completion-mode nil
;; test-case slime-macroexpand-1 loses window focus

(message "Congratulations!")
