(require 'popup)

(defmacro test (explain &rest body)
  (declare (indent 1))
  `(let ((buf "*buf*")
         (window-config (current-window-configuration)))
     (unwind-protect
         (progn
           (delete-other-windows)
           (switch-to-buffer buf)
           (erase-buffer)
           (insert " ")
           (let ((success (progn ,@body)))
             (unless success
               (error "failed: %s" ,explain))))
       (when popup
         (popup-delete popup)
         (setq popup nil))
       (kill-buffer buf)
       (set-window-configuration window-config))))

(defmacro ui-test (prompt &rest body)
  (declare (indent 1))
  `(test ,prompt ,@body (yes-or-no-p ,prompt)))

(defun input (key)
  (push key unread-command-events))

(defvar popup nil)

(test "popup-create"
  (setq popup (popup-create (point) 10 10)))

(test "popup-delete"
  (setq popup (popup-create (point) 10 10))
  (popup-delete popup)
  (not (popup-live-p popup)))

(ui-test "popup?"
  (setq popup (popup-create (point) 10 10))
  (popup-set-list popup '("hello" "world"))
  (popup-draw popup))

(ui-test "hidden?"
  (setq popup (popup-create (point) 10 10))
  (popup-set-list popup '("hello" "world"))
  (popup-draw popup)
  (popup-hide popup))

(ui-test "isearch?"
  (setq popup (popup-create (point) 10 10))
  (popup-set-list popup '("hello" "world"))
  (popup-draw popup)
  (input ?e)
  (popup-isearch popup))

(ui-test "tip?"
  (popup-tip
   "Start isearch on POPUP. This function is synchronized, meaning
event loop waits for quiting of isearch.

CURSOR-COLOR is a cursor color during isearch. The default value
is `popup-isearch-cursor-color'.

KEYMAP is a keymap which is used when processing events during
event loop. The default value is `popup-isearch-keymap'.

CALLBACK is a function taking one argument. `popup-isearch' calls
CALLBACK, if specified, after isearch finished or isearch
canceled. The arguments is whole filtered list of items.

HELP-DELAY is a delay of displaying helps."
   :nowait t))

(ui-test "fold?"
  (let ((s (make-string (- (window-width) 3) ? )))
    (insert s)
    (setq popup (popup-tip "long long long long line" :nowait t))))

(ui-test "fold?"
  (let ((s (make-string (- (window-height) 3) ?\n)))
    (insert s)
    (setq popup (popup-tip "bla\nbla\nbla\nbla\nbla" :nowait t))))

(ui-test "margin?"
  (setq popup (popup-tip "Margin?" :nowait t :margin t)))

(ui-test "two lines?"
  (setq popup (popup-tip "Foo\nBar\nBaz" :nowait t :height 2)))

(ui-test "scroll bar?"
  (setq popup (popup-tip "Foo\nBar\nBaz\nFez\nOz" :nowait t :height 3 :scroll-bar t :margin t)))

(ui-test "min-height?"
  (setq popup (popup-tip "Hello" :nowait t :min-height 10)))

(ui-test "menu?"
  (setq popup (popup-menu* '("Foo" "Bar" "Baz") :nowait t)))

(ui-test "cascade menu?"
  (setq popup (popup-cascade-menu '(("Foo" "Foo1" "Foo2") "Bar" "Baz") :nowait t :margin t)))

(ui-test "next?"
  (setq popup (popup-cascade-menu '("Foo" "Bar" "Baz") :nowait t :margin t))
  (popup-next popup))

(ui-test "previous?"
  (setq popup (popup-cascade-menu '("Foo" "Bar" "Baz") :nowait t :margin t))
  (popup-previous popup))

(ui-test "select?"
  (setq popup (popup-cascade-menu '("Foo" "Bar" "Baz") :nowait t :margin t))
  (popup-select popup 1))

(ui-test "scroll-down?"
  (setq popup (popup-cascade-menu (loop repeat 100 collect "Foo") :nowait t :height 10 :margin t :scroll-bar t))
  (popup-scroll-down popup 10))

(ui-test "scroll-up?"
  (setq popup (popup-cascade-menu (loop repeat 100 collect "Foo") :nowait t :height 10 :margin t :scroll-bar t))
  (popup-scroll-down popup 999)
  (popup-scroll-up popup 10))

(message "Congratulations!")
