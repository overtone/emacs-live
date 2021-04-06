(require 'popwin)
(require 'ert)

;(setq display-buffer-function 'popwin:display-buffer)
(popwin-mode 1)
(setq popwin:popup-window-position 'bottom)

(defun popwin-test:front-buffer-p (buffer)
  (get-window-with-predicate
   (lambda (window)
     (eq (window-buffer window) buffer))))

(defmacro popwin-test:common (&rest body)
  (declare (indent 0) (debug t))
  `(save-excursion
    (save-window-excursion
      (popwin:close-popup-window)
      (delete-other-windows)
      (let ((buf1 (get-buffer-create "*buf1*"))
            (buf2 (get-buffer-create "*buf2*"))
            (buf3 (get-buffer-create "*buf3*"))
            (width (window-width))
            (height (window-height))
            (left   (nth 0 (window-inside-edges)))
            (top    (nth 1 (window-inside-edges)))
            (right  (nth 2 (window-inside-edges)))
            (bottom (nth 3 (window-inside-edges))))
        (switch-to-buffer buf1)
        ,@body
        (popwin:close-popup-window)
        ))))

(defun popwin-test:should-at-top (left top right bottom)
  (should (eq (nth 0 (window-inside-edges)) left))
  (should (eq (nth 1 (window-inside-edges)) top))
  (should (eq (nth 2 (window-inside-edges)) right))
  (should-not (eq (nth 3 (window-inside-edges)) bottom)))

(defun popwin-test:should-at-bottom (left top right bottom)
  (should (eq (nth 0 (window-inside-edges)) left))
  (should-not (eq (nth 1 (window-inside-edges)) top))
  (should (eq (nth 2 (window-inside-edges)) right))
  (should (eq (nth 3 (window-inside-edges)) bottom)))

(defun popwin-test:should-at-left (left top right bottom)
  (should (eq (nth 0 (window-inside-edges)) left))
  (should (eq (nth 1 (window-inside-edges)) top))
  (should-not (eq (nth 2 (window-inside-edges)) right))
  (should (eq (nth 3 (window-inside-edges)) bottom)))

(defun popwin-test:should-at-right (left top right bottom)
  (should-not (eq (nth 0 (window-inside-edges)) left))
  (should (eq (nth 1 (window-inside-edges)) top))
  (should (eq (nth 2 (window-inside-edges)) right))
  (should (eq (nth 3 (window-inside-edges)) bottom)))

(defun popwin-test:store-minibuffer-input (keys)
  (setq unread-command-events
        (append (listify-key-sequence (read-kbd-macro keys))
                unread-command-events)))

(ert-deftest buf2-selected ()
  (popwin-test:common
    (split-window-horizontally)
    (other-window 1)
    (popwin:popup-buffer buf2)
    (switch-to-buffer buf2)
    (popwin:popup-buffer buf3)
    (popwin:close-popup-window)
    (message "%s" (current-buffer))
    (should (eq buf2 (current-buffer)))))

(ert-deftest select ()
  (popwin-test:common
    (popwin:popup-buffer buf2)
    (should (eq popwin:popup-window
                (popwin:select-popup-window)))))

(ert-deftest not-stick ()
  (popwin-test:common
    (popwin:popup-buffer buf2)
    (other-window 1)
    (popwin:close-popup-window-timer)
    (should (one-window-p))
    (should-not (popwin-test:front-buffer-p buf2))))

(ert-deftest stick ()
  (popwin-test:common
    (popwin:popup-buffer buf2)
    (should (popwin:stick-popup-window))
    (other-window 1)
    (popwin:close-popup-window-timer)
    (should (eq (length (window-list)) 2))
    (should (popwin-test:front-buffer-p buf2))))

(ert-deftest dedicated-stick ()
  (popwin-test:common
    (popwin:popup-buffer buf2 :dedicated t :stick t)
    (other-window 1)
    (popwin:close-popup-window-if-necessary)
    (should (eq (length (window-list)) 2))
    (should (popwin-test:front-buffer-p buf2))))

(ert-deftest popup ()
  (popwin-test:common
    (should-not (eq (length (window-list)) 2))
    (should-not (popwin-test:front-buffer-p buf2))
    (popwin:popup-buffer buf2)
    (should (eq (length (window-list)) 2))
    (should (popwin-test:front-buffer-p buf2))))

(ert-deftest popup-when-split-horizontally ()
  (popwin-test:common
    (split-window-horizontally)
    (should-not (popwin-test:front-buffer-p buf2))
    (should-not (eq (length (window-list)) 3))
    (popwin:popup-buffer buf2)
    (should (popwin-test:front-buffer-p buf2))
    (should (eq (length (window-list)) 3))))

(ert-deftest popup-when-split-vertically ()
  (popwin-test:common
    (split-window-vertically)
    (should-not (popwin-test:front-buffer-p buf2))
    (should-not (eq (length (window-list)) 3))
    (popwin:popup-buffer buf2)
    (should (popwin-test:front-buffer-p buf2))
    (should (eq (length (window-list)) 3))))

(ert-deftest popup-at-bottom ()
  (popwin-test:common
    (should-not (popwin-test:front-buffer-p buf2))
    (should-not (eq (length (window-list)) 2))
    (popwin:popup-buffer buf2 :position 'bottom)
    (popwin-test:should-at-bottom left top right bottom)
    (should (popwin-test:front-buffer-p buf2))
    (should (eq (length (window-list)) 2))))

(ert-deftest popup-at-left ()
  (popwin-test:common
    (should-not (popwin-test:front-buffer-p buf2))
    (should-not (eq (length (window-list)) 2))
    (popwin:popup-buffer buf2 :position 'left)
    (popwin-test:should-at-left left top right bottom)
    (should (popwin-test:front-buffer-p buf2))
    (should (eq (length (window-list)) 2))))

(ert-deftest popup-at-top ()
  (popwin-test:common
    (should-not (popwin-test:front-buffer-p buf2))
    (should-not (eq (length (window-list)) 2))
    (popwin:popup-buffer buf2 :position 'top)
    (popwin-test:should-at-top left top right bottom)
    (should (popwin-test:front-buffer-p buf2))
    (should (eq (length (window-list)) 2))))

(ert-deftest popup-at-right ()
  (popwin-test:common
    (should-not (popwin-test:front-buffer-p buf2))
    (should-not (eq (length (window-list)) 2))
    (popwin:popup-buffer buf2 :position 'right)
    (popwin-test:should-at-right left top right bottom)
    (should (popwin-test:front-buffer-p buf2))
    (should (eq (length (window-list)) 2))))

(ert-deftest popup-at-bottom-with-50% ()
  (popwin-test:common
    (should-not (popwin-test:front-buffer-p buf2))
    (should-not (eq (length (window-list)) 2))
    (popwin:popup-buffer buf2 :height 0.5 :position 'bottom)
    (popwin-test:should-at-bottom left top right bottom)
    (popwin:stick-popup-window)
    (call-interactively 'other-window)
    (should (<= (1- (/ height 2)) (window-height)))
    (should (<= (window-height) (1+ (/ height 2))))
    (should (popwin-test:front-buffer-p buf2))
    (should (eq (length (window-list)) 2))))

(ert-deftest popup-at-left-with-50% ()
  (popwin-test:common
    (should-not (popwin-test:front-buffer-p buf2))
    (should-not (eq (length (window-list)) 2))
    (popwin:popup-buffer buf2 :width 0.5 :position 'left)
    (should (popwin-test:front-buffer-p buf2))
    (popwin-test:should-at-left left top right bottom)
    (popwin:stick-popup-window)
    (call-interactively 'other-window)
    (should (<= (1- (/ width 2)) (window-width)))
    (should (<= (window-width) (1+ (/ width 2))))
    (should (eq (length (window-list)) 2))))

(ert-deftest popup-at-bottom-with-three-columes ()
  (popwin-test:common
    (split-window-vertically)
    (split-window-vertically)
    (should-not (popwin-test:front-buffer-p buf2))
    (should-not (eq (length (window-list)) 4))
    (popwin:popup-buffer buf2 :position 'bottom)
    (popwin-test:should-at-bottom left top right bottom)
    (should (popwin-test:front-buffer-p buf2))
    (should (eq (length (window-list)) 4))))

(ert-deftest popup-at-left-with-three-columes ()
  (popwin-test:common
    (split-window-vertically)
    (split-window-vertically)
    (should-not (popwin-test:front-buffer-p buf2))
    (should-not (eq (length (window-list)) 4))
    (popwin:popup-buffer buf2 :position 'left)
    (popwin-test:should-at-left left top right bottom)
    (should (popwin-test:front-buffer-p buf2))
    (should (eq (length (window-list)) 4))))

(ert-deftest popup-at-top-with-three-columes ()
  (popwin-test:common
    (split-window-vertically)
    (split-window-vertically)
    (should-not (popwin-test:front-buffer-p buf2))
    (should-not (eq (length (window-list)) 4))
    (popwin:popup-buffer buf2 :position 'top)
    (popwin-test:should-at-top left top right bottom)
    (should (popwin-test:front-buffer-p buf2))
    (should (eq (length (window-list)) 4))))

(ert-deftest popup-at-right-with-three-columes ()
  (popwin-test:common
    (split-window-vertically)
    (split-window-vertically)
    (should-not (popwin-test:front-buffer-p buf2))
    (should-not (eq (length (window-list)) 4))
    (popwin:popup-buffer buf2 :position 'right)
    (should (popwin-test:front-buffer-p buf2))
    (popwin-test:should-at-right left top right bottom)
    (should (eq (length (window-list)) 4))))

(ert-deftest popup-buf1-by-name ()
  (popwin-test:common
    (switch-to-buffer buf2)
    (should-not (popwin-test:front-buffer-p buf1))
    (should-not (eq (length (window-list)) 2))
    (let ((popwin:special-display-config '(("*buf1*"))))
      (popwin:display-buffer buf1))
    (should (popwin-test:front-buffer-p buf1))
    (should (eq (length (window-list)) 2))))

(ert-deftest popup-buf1-by-mode ()
  (popwin-test:common
    (switch-to-buffer buf2)
    (should-not (popwin-test:front-buffer-p buf1))
    (should-not (eq (length (window-list)) 2))
    (let ((popwin:special-display-config '((fundamental-mode))))
      (popwin:display-buffer buf1))
    (should (popwin-test:front-buffer-p buf1))
    (should (eq (length (window-list)) 2))))

(ert-deftest popup-buf1-by-regexp ()
  (popwin-test:common
    (switch-to-buffer buf2)
    (should-not (popwin-test:front-buffer-p buf1))
    (should-not (eq (length (window-list)) 2))
    (let ((popwin:special-display-config '(("\\*buf[0-9]\\*" :regexp t))))
      (popwin:display-buffer buf1))
    (should (popwin-test:front-buffer-p buf1))
    (should (eq (length (window-list)) 2))))

(ert-deftest popup-buf1-again ()
  (popwin-test:common
    (switch-to-buffer buf2)
    (let ((popwin:special-display-config '(("*buf1*"))))
      (popwin:display-buffer buf1))
    (should (eq (car popwin:popup-last-config) buf1))
    (popwin:close-popup-window)
    (should-not (popwin-test:front-buffer-p buf1))
    (should-not (eq (length (window-list)) 2))
    (popwin:popup-last-buffer)
    (should (popwin-test:front-buffer-p buf1))
    (should (eq (length (window-list)) 2))))

(ert-deftest popup-buf1-normally ()
  (popwin-test:common
    (should-not (popwin-test:front-buffer-p buf2))
    (should-not (eq (length (window-list)) 2))
    (let (popwin:special-display-config)
      (popwin:display-buffer buf2))
    (should (eq (length (window-list)) 2))
    (should (popwin-test:front-buffer-p buf2))))

(ert-deftest popup-buf1-tail ()
  (popwin-test:common
    (switch-to-buffer buf1)
    (insert "\n")
    (beginning-of-buffer)
    (switch-to-buffer buf2)
    (should-not (popwin-test:front-buffer-p buf1))
    (should-not (eq (length (window-list)) 2))
    (popwin:popup-buffer-tail buf1)
    (should (eobp))
    (should (popwin-test:front-buffer-p buf1))
    (should (eq (length (window-list)) 2))))

(ert-deftest popup-buffer-interactively ()
  (popwin-test:common
    (switch-to-buffer buf1)
    (popwin-test:store-minibuffer-input "*buf2* RET")
    (call-interactively 'popwin:popup-buffer)
    ;; Cannot verify whether in minibuffer because call-interactively blocks test
    ;; (should (minibufferp))
    (should (popwin-test:front-buffer-p buf2))
    (should (eq (length (window-list)) 2))
    ))

(ert-deftest display-buffer-interactively ()
  (popwin-test:common
    (switch-to-buffer buf1)
    (popwin-test:store-minibuffer-input "*buf2* RET")
    (call-interactively 'popwin:display-buffer)
    (should (popwin-test:front-buffer-p buf2))
    (should (eq (length (window-list)) 2))
    ))

(ert-deftest popup-buffer-tail-interactively ()
  (popwin-test:common
    (switch-to-buffer buf1)
    (popwin-test:store-minibuffer-input "*buf2* RET")
    (call-interactively 'popwin:popup-buffer-tail)
    (should (eobp))
    (should (popwin-test:front-buffer-p buf2))
    (should (eq (length (window-list)) 2))
    ))

(ert-deftest find-file-interactively ()
  (popwin-test:common
    (switch-to-buffer buf1)
    (popwin-test:store-minibuffer-input "popwin-test.el RET")
    (call-interactively 'popwin:find-file)
    (should (popwin-test:front-buffer-p (get-buffer-create "popwin-test.el")))
    (should (eq (length (window-list)) 2))
    ))

(ert-deftest find-file-tail-interactively ()
  (popwin-test:common
    (switch-to-buffer buf1)
    (popwin-test:store-minibuffer-input "popwin-test.el RET")
    (call-interactively 'popwin:find-file-tail)
    (should (popwin-test:front-buffer-p (get-buffer-create "popwin-test.el")))
    (should (eq (length (window-list)) 2))
    (should (eobp))))

(ert-deftest messages ()
  (popwin-test:common
    (switch-to-buffer buf1)
    (call-interactively 'popwin:messages)
    (should (popwin-test:front-buffer-p (get-buffer-create "*Messages*")))
    (should (eq (length (window-list)) 2))
    ))

(ert-deftest popup-when-split-vertically-move ()
  (popwin-test:common
    (beginning-of-buffer)
    (split-window-vertically)
    (forward-char)
    (let ((points (mapcar 'window-point (window-list))))
      (should (eq (length points) 2))
      (should-not (eq (nth 0 points) (nth 1 points))))
    (popwin:popup-buffer buf2)
    (call-interactively 'other-window)
    (popwin:close-popup-window-timer)
    (let ((points (mapcar 'window-point (window-list))))
      (should (eq (length points) 2))
      (should-not (eq (nth 0 points) (nth 1 points))))
    ))

(ert-deftest popup-when-split-vertically-move-and-other ()
  (popwin-test:common
    (beginning-of-buffer)
    (split-window-vertically)
    (call-interactively 'other-window)
    (forward-char)
    (let ((points (mapcar 'window-point (window-list))))
      (should (eq (length points) 2))
      (should-not (eq (nth 0 points) (nth 1 points))))
    (popwin:popup-buffer buf2)
    (call-interactively 'other-window)
    (popwin:close-popup-window-timer)
    (let ((points (mapcar 'window-point (window-list))))
      (should (eq (length points) 2))
      (should-not (eq (nth 0 points) (nth 1 points))))
    ))

(ert-deftest popwin-will-never-change-the-window-point ()
  (popwin-test:common
   (insert "foobar")
   (let ((old-point (point)))
     (let ((popwin:special-display-config '((".*" :regexp t :position top))))
       (pop-to-buffer buf2))
     (popwin:close-popup-window)
     (should (eq (current-buffer) buf1))
     (should (= old-point (point))))))

(ert-deftest popwin-restore-window-start ()
  (popwin-test:common
   (insert (make-string 100 ?\n))
   (set-window-start nil (point-max))
   (popwin:popup-buffer buf2)
   (set-window-start (get-buffer-window buf1) (point-min))
   (popwin:close-popup-window)
   (should (eq (current-buffer) buf1))
   (should (eq (window-start) (point-max)))))

(ert-deftest popwin-restore-window-start/*help* ()
  (require 'help-mode)
  (describe-function 'popwin:find-file)
  (switch-to-buffer (help-buffer))
  (goto-char (point-min))
  (should (search-forward "popwin.e" (point-max) t))
  (call-interactively 'push-button)
  (goto-char (point-max))
  (describe-function 'popwin:popup-buffer)
  (switch-to-buffer (help-buffer))
  (goto-char (point-min))
  (should (search-forward "popwin.e" (point-max) t))
  (call-interactively 'push-button)
  (sit-for 0)
  (beginning-of-line)
  (should (search-forward "popwin:popup-buffer" (line-end-position) t)))

(ert-deftest popwin-original-display-last-buffer ()
  (popwin-test:common
   (popwin:popup-buffer buf2)
   (should (eq popwin:popup-buffer buf2))
   (popwin:original-display-last-buffer)
   (should (null popwin:popup-buffer))
   (delete-other-windows)
   (popwin:original-display-last-buffer)
   (should (null popwin:popup-buffer))))

(ert-deftest popwin-original-pop-to-last-buffer ()
  (popwin-test:common
   (popwin:popup-buffer buf2)
   (should (eq popwin:popup-buffer buf2))
   (popwin:original-pop-to-last-buffer)
   (should (null popwin:popup-buffer))
   (should (eq (current-buffer) buf2))))

(ert-deftest popwin-switch-to-last-buffer ()
  (popwin-test:common
   (popwin:popup-buffer buf2)
   (should (eq popwin:popup-buffer buf2))
   (popwin:switch-to-last-buffer)
   (should (null popwin:popup-buffer))
   (should (eq (current-buffer) buf2))))

(ert-deftest popwin-display-buffer-reuse-window ()
  (popwin-test:common
   (let ((popwin:special-display-config '(("*buf1*")))
         (window (selected-window)))
     (popwin:display-buffer buf1)
     (should (not (popwin:popup-window-live-p)))
     (should (equal (window-list) (list window))))
   (let ((popwin:special-display-config '(("*buf1*")))
         (popwin:reuse-window nil)
         (window (selected-window)))
     (popwin:display-buffer buf1)
     (should (eq (car popwin:popup-last-config) buf1))
     (should (popwin:popup-window-live-p))
     (popwin:close-popup-window))
   (let ((popwin:special-display-config '(("*buf2*"))))
     (popwin:display-buffer buf2)
     (should (eq (car popwin:popup-last-config) buf2))
     (should (popwin:popup-window-live-p))
     (popwin:close-popup-window))
   (split-window-vertically)
   (switch-to-buffer buf2)
   (popwin:display-buffer buf1)
   (should (not (popwin:popup-window-live-p)))))

(defun do-nothing () (interactive))

(ert-deftest popup-from-minibuffer ()
  (popwin-test:common
   (popwin-test:store-minibuffer-input "do TAB -nothing C-g")
   (should
    (condition-case nil
        (prog1 nil
          (call-interactively 'execute-extended-command))
      (quit t)))))

(ert-deftest popwin-with-selected-window-and-wait ()
  (popwin-test:common
    (popwin:popup-buffer buf2)
    (should (eq popwin:popup-buffer buf2))
    (with-selected-window (get-buffer-window buf1)
      (read-event nil nil (+ popwin:close-popup-window-timer-interval 0.5)))
    (should (popwin:popup-window-live-p))))

(when (fboundp 'display-buffer-in-side-window)
  (ert-deftest popwin-side-window ()
    (popwin-test:common
     (let* ((side-buf (get-buffer-create "*side-buf*"))
            (side-win (display-buffer-in-side-window side-buf nil)))
       (should (eq (length (window-list)) 2))
       (popwin:popup-buffer buf2)
       (should (popwin-test:front-buffer-p buf2))
       (should (eq (length (window-list)) 3))
       (popwin:close-popup-window)
       (should (popwin-test:front-buffer-p buf1))
       (should (eq (length (window-list)) 2))
       (should (window-live-p side-win))))))

;; test-case M-x occur and M-x next-error
;; test-case M-x dired and o
;; test-case fixed size popwin
;; test-case partial-completion-mode nil
;; test-case slime-macroexpand-1 loses window focus

(ert-run-tests-interactively t)
(delete-other-windows)
