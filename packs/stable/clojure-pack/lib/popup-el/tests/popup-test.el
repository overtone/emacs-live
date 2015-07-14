(require 'ert)

(require 'popup)

(when (< (frame-width) (length "long long long long line"))
  (set-frame-size (selected-frame) 80 35))

(defun popup-test-helper-posn-col-row (dummy)
  "This function is workaround. Because `posn-col-row' and `posn-at-point'
can not work well in batch mode."
  (cons (current-column) (line-number-at-pos (point))))

(defmacro popup-test-with-common-setup (&rest body)
  (declare (indent 0) (debug t))
  `(save-excursion
     (with-temp-buffer
       (switch-to-buffer (current-buffer))
       (delete-other-windows)
       (erase-buffer)
       (if noninteractive
           (cl-letf (((symbol-function 'posn-col-row)
                      #'popup-test-helper-posn-col-row))
             ,@body)
         ,@body))))

(defun popup-test-helper-line-move-visual (arg)
  "This function is workaround. Because `line-move-visual' can not work well in
batch mode."
  (let ((cur-col
         (- (current-column)
            (save-excursion (vertical-motion 0) (current-column)))))
    (vertical-motion arg)
    (move-to-column (+ (current-column) cur-col))))

(defun popup-test-helper-rectangle-match (str)
  (let ((buffer-contents (popup-test-helper-buffer-contents)))
    (with-temp-buffer
      (insert buffer-contents)
      (goto-char (point-min))
      (let ((strings (split-string str "\n")))
        (when (search-forward (car strings) nil t)
          (goto-char (match-beginning 0))
          (cl-every
           'identity
           (mapcar
            (lambda (elem)
              (popup-test-helper-line-move-visual 1)
              (looking-at (regexp-quote elem)))
            (cdr strings))))))))

(defun popup-test-helper-buffer-contents ()
  (cl-loop with start = (point-min)
        with contents
        for overlay in (cl-sort (overlays-in (point-min) (point-max))
                              '< :key 'overlay-start)
        for overlay-start = (overlay-start overlay)
        for overlay-end = (overlay-end overlay)
        for prefix = (buffer-substring start overlay-start)
        for befstr = (overlay-get overlay 'before-string)
        for substr = (or (overlay-get overlay 'display)
                         (buffer-substring overlay-start overlay-end))
        for aftstr = (overlay-get overlay 'after-string)
        collect prefix into contents
        unless (overlay-get overlay 'invisible) collect
        (concat befstr substr aftstr) into contents
        do (setq start overlay-end)
        finally (return (concat (apply 'concat contents)
                                (buffer-substring start (point-max))))
        ))

(defun popup-test-helper-create-popup (str)
  (setq popup (popup-create (point) 10 10))
  (popup-set-list popup (split-string str "\n"))
  (popup-draw popup))

(defun popup-test-helper-in-popup-p ()
  (let* ((faces (get-text-property (point) 'face))
         (faces (if (listp faces) faces (list faces))))
    (or (memq 'popup-tip-face faces)
        (memq 'popup-menu-face faces)
        (memq 'popup-menu-selection-face faces)
        (memq 'popup-face faces))))

(defun popup-test-helper-popup-selected-item (str)
  (let ((buffer-contents (popup-test-helper-buffer-contents)))
    (with-temp-buffer
      (insert buffer-contents)
      (goto-char (point-min))
      (goto-char
       (text-property-any (point-min) (point-max)
                          'face 'popup-menu-selection-face))
      (looking-at str)
      )))

(defun popup-test-helper-popup-beginning-line ()
  (let ((buffer-contents (popup-test-helper-buffer-contents)))
    (with-temp-buffer
      (insert buffer-contents)
      (goto-char (point-min))
      (let ((end (point)))
        (while (and (not (eobp))
                    (not (popup-test-helper-in-popup-p)))
          (goto-char (or (next-single-property-change (point) 'face)
                         (point-max))))
        (if (popup-test-helper-in-popup-p)
            ;; todo visual line
            (line-number-at-pos (point)) nil)
        ))))

(defun popup-test-helper-popup-beginning-column ()
  (let ((buffer-contents (popup-test-helper-buffer-contents)))
    (with-temp-buffer
      (insert buffer-contents)
      (goto-char (point-min))
      (let ((end (point)))
        (while (and (not (eobp))
                    (not (popup-test-helper-in-popup-p)))
          (goto-char (or (next-single-property-change (point) 'face)
                         (point-max))))
        (if (popup-test-helper-in-popup-p)
            (current-column) nil)
        ))))

(defun popup-test-helper-popup-end-line ()
  (let ((buffer-contents (popup-test-helper-buffer-contents)))
    (with-temp-buffer
      (insert buffer-contents)
      (goto-char (point-max))
      (let ((end (point)))
        (while (and (not (bobp))
                    (not (popup-test-helper-in-popup-p)))
          (setq end (point))
          (goto-char (or (previous-single-property-change (point) 'face)
                         (point-min))))
        (if (popup-test-helper-in-popup-p)
            ;; todo visual line
            (line-number-at-pos end) nil)
        ))))

(defun popup-test-helper-popup-end-column ()
  (let ((buffer-contents (popup-test-helper-buffer-contents)))
    (with-temp-buffer
      (insert buffer-contents)
      (goto-char (point-max))
      (let ((end (point)))
        (while (and (not (bobp))
                    (not (popup-test-helper-in-popup-p)))
          (setq end (point))
          (goto-char (or (previous-single-property-change (point) 'face)
                         (point-min))))
        (when (popup-test-helper-in-popup-p)
          (goto-char end)
          (current-column))
        ))))

(defun popup-test-helper-debug ()
  (let ((buffer-contents (popup-test-helper-buffer-contents)))
    (with-current-buffer (get-buffer-create "*dump*")
      (erase-buffer)
      (insert buffer-contents)
      (buffer-string)
      )))
;; Test for helper method
(ert-deftest popup-test-no-truncated ()
  (popup-test-with-common-setup
    (insert (make-string (- (window-width) 4) ? )) (insert "Foo\n")
    (insert (make-string (- (window-width) 4) ? )) (insert "Bar\n")
    (insert (make-string (- (window-width) 4) ? )) (insert "Baz\n")
    (should (eq t (popup-test-helper-rectangle-match "\
Foo
Bar
Baz")))
    ))

(ert-deftest popup-test-truncated ()
  (popup-test-with-common-setup
    (insert (make-string (- (window-width) 2) ? )) (insert "Foo\n")
    (insert (make-string (- (window-width) 2) ? )) (insert "Bar\n")
    (insert (make-string (- (window-width) 2) ? )) (insert "Baz\n")
    (should (eq nil (popup-test-helper-rectangle-match "\
Foo
Bar
Baz")))
    ))

(ert-deftest popup-test-misaligned ()
  (popup-test-with-common-setup
    (progn
      (insert (make-string (- (window-width) 5) ? )) (insert "Foo\n")
      (insert (make-string (- (window-width) 4) ? )) (insert "Bar\n")
      (insert (make-string (- (window-width) 3) ? )) (insert "Baz\n"))
    (should (eq nil (popup-test-helper-rectangle-match "\
Foo
Bar
Baz")))
    ))
;; Test for popup-el
(ert-deftest popup-test-simple ()
  (popup-test-with-common-setup
    (popup-test-helper-create-popup "\
foo
bar
baz")
    (should (popup-test-helper-rectangle-match "\
foo
bar
baz"))
    (should (eq (popup-test-helper-popup-beginning-column) 0))))

(ert-deftest popup-test-delete ()
  (popup-test-with-common-setup
    (popup-test-helper-create-popup "\
foo
bar
baz")
    (popup-delete popup)
    (should-not (popup-test-helper-rectangle-match "\
foo
bar
baz"))
    ))

(ert-deftest popup-test-hide ()
  (popup-test-with-common-setup
    (popup-test-helper-create-popup "\
foo
bar
baz")
    (popup-hide popup)
    (should-not (popup-test-helper-rectangle-match "\
foo
bar
baz"))
    ))

(ert-deftest popup-test-at-colum1 ()
  (popup-test-with-common-setup
    (insert " ")
    (popup-test-helper-create-popup "\
foo
bar
baz")
    (should (popup-test-helper-rectangle-match "\
foo
bar
baz"))
    (should (eq (popup-test-helper-popup-beginning-column) 1))
    ))

(ert-deftest popup-test-tip ()
  (popup-test-with-common-setup
    (popup-tip "\
Start isearch on POPUP. This function is synchronized, meaning
event loop waits for quiting of isearch.

CURSOR-COLOR is a cursor color during isearch. The default value
is `popup-isearch-cursor-color'.

KEYMAP is a keymap which is used when processing events during
event loop. The default value is `popup-isearch-keymap'.

CALLBACK is a function taking one argument. `popup-isearch' calls
CALLBACK, if specified, after isearch finished or isearch
canceled. The arguments is whole filtered list of items.

HELP-DELAY is a delay of displaying helps."
               :nowait t)
    (should (popup-test-helper-rectangle-match "\
KEYMAP is a keymap which is used when processing events during
event loop. The default value is `popup-isearch-keymap'."))
    ))

(ert-deftest popup-test-folding-long-line-right-top ()
  (popup-test-with-common-setup
    ;; To use window-width because Emacs 23 does not have window-body-width
    (insert (make-string (- (window-width) 3) ? ))
    (popup-tip "long long long long line" :nowait t)
    (should (popup-test-helper-rectangle-match "long long long long line"))
    (should (eq (popup-test-helper-popup-beginning-line)
                2))
    (should (eq (popup-test-helper-popup-end-line) 2))
    ))

(ert-deftest popup-test-folding-long-line-left-bottom ()
  (popup-test-with-common-setup
    (insert (make-string (- (window-body-height) 1) ?\n))
    (popup-tip "long long long long line" :nowait t)
    (should (popup-test-helper-rectangle-match "long long long long line"))
    (should (eq (popup-test-helper-popup-beginning-line)
                (- (window-body-height) 1)))
    (should (eq (popup-test-helper-popup-end-line) (- (window-body-height) 1)))
    ))

(ert-deftest popup-test-folding-long-line-right-bottom ()
  (popup-test-with-common-setup
    (insert (make-string (- (window-body-height) 1) ?\n))
    (insert (make-string (- (window-width) 3) ? ))
    (popup-tip "long long long long line" :nowait t)
    (should (popup-test-helper-rectangle-match "long long long long line"))
    (should (eq (popup-test-helper-popup-beginning-line)
                (- (window-body-height) 1)))
    (should (eq (popup-test-helper-popup-end-line) (- (window-body-height) 1)))
    ))

(ert-deftest popup-test-folding-short-line-right-top ()
  (popup-test-with-common-setup
    (insert (make-string (- (window-width) 4) ? ))
    (popup-tip "\
bla
bla
bla
bla
bla" :nowait t)
    (should (popup-test-helper-rectangle-match "\
bla
bla
bla
bla
bla"))
    (should (eq (popup-test-helper-popup-beginning-line) 2))
    ))

(ert-deftest popup-test-folding-short-line-left-bottom ()
  (popup-test-with-common-setup
    (insert (make-string (- (window-body-height) 1) ?\n))
    (popup-tip "\
bla
bla
bla
bla
bla" :nowait t)
    (should (popup-test-helper-rectangle-match "\
bla
bla
bla
bla
bla"))
    (should (eq (popup-test-helper-popup-end-line) (- (window-body-height) 1)))))

(ert-deftest popup-test-folding-short-line-right-bottom ()
  (popup-test-with-common-setup
    (insert (make-string (- (window-body-height) 1) ?\n))
    (insert (make-string (- (window-width) 4) ? ))
    (popup-tip "\
bla
bla
bla
bla
bla" :nowait t)
    (should (popup-test-helper-rectangle-match "\
bla
bla
bla
bla
bla"))
    (should (eq (popup-test-helper-popup-end-line) (- (window-body-height) 1)))
    ))

(ert-deftest popup-test-margin-at-column1 ()
  (popup-test-with-common-setup
    (insert " ")
    (popup-tip "Margin?" :nowait t :margin t)
    (should (eq (popup-test-helper-popup-beginning-column)
                0))
    (should (popup-test-helper-rectangle-match " Margin? "))
    ))

(ert-deftest popup-test-margin-left ()
  (popup-test-with-common-setup
   (popup-tip "Margin?" :nowait t :margin t)
   (should (eq (popup-test-helper-popup-beginning-column)
               0))
   ;; Pending: #19
   ;; (should (popup-test-helper-rectangle-match " Margin? "))
   ))

(ert-deftest popup-test-margin-right ()
  (popup-test-with-common-setup
    (insert (make-string (- (window-width) 1) ? ))
    (popup-tip "Margin?" :nowait t :margin t)
    (should (popup-test-helper-rectangle-match " Margin? "))
    ;; Pending: #19
    ;; (should (< (popup-test-helper-popup-end-column) (window-width)))
    ))

(ert-deftest popup-test-height-limit ()
  (popup-test-with-common-setup
    (popup-tip "\
Foo
Bar
Baz" :nowait t :height 2)
    (should (popup-test-helper-rectangle-match "\
Foo
Bar"))
    (should-not (popup-test-helper-rectangle-match "Baz"))
    (should (eq (popup-test-helper-popup-beginning-line) 2))
    (should (eq (popup-test-helper-popup-end-line)  3))
    ))

(ert-deftest popup-test-height-limit-bottom ()
  (popup-test-with-common-setup
    (insert (make-string (- (window-body-height) 1) ?\n))
    (popup-tip "\
Foo
Bar
Baz" :nowait t :height 2)
    (should (popup-test-helper-rectangle-match "\
Foo
Bar"))
    (should-not (popup-test-helper-rectangle-match "Baz"))
    (should (eq (popup-test-helper-popup-end-line) (- (window-body-height) 1)))
    ))

(ert-deftest popup-test-scroll-bar ()
  (popup-test-with-common-setup
    (let ((popup-scroll-bar-foreground-char
           (propertize "f" 'face 'popup-scroll-bar-foreground-face))
          (popup-scroll-bar-background-char
           (propertize "b" 'face 'popup-scroll-bar-background-face)))
      (popup-tip "\
Foo
Bar
Baz
Fez
Oz"
                 :nowait t :height 3 :scroll-bar t :margin t)
      (should (popup-test-helper-rectangle-match "\
Foo f
Bar b
Baz b"))
      (should-not (popup-test-helper-rectangle-match "Fez"))
      (should-not (popup-test-helper-rectangle-match "Oz"))
      (should (eq (popup-test-helper-popup-beginning-line) 2))
      (should (eq (popup-test-helper-popup-end-line)  4))
      )))

(ert-deftest popup-test-scroll-bar-right-no-margin ()
  (popup-test-with-common-setup
    (insert (make-string (- (window-width) 1) ? ))
    (let ((popup-scroll-bar-foreground-char
           (propertize "f" 'face 'popup-scroll-bar-foreground-face))
          (popup-scroll-bar-background-char
           (propertize "b" 'face 'popup-scroll-bar-background-face)))
      (popup-tip "\
Foo
Bar
Baz
Fez
Oz"
                 :nowait t :height 3 :scroll-bar t)
      (should (popup-test-helper-rectangle-match "\
Foof
Barb
Bazb"))
      (should-not (popup-test-helper-rectangle-match "Fez"))
      (should-not (popup-test-helper-rectangle-match "Oz"))
      (should (eq (popup-test-helper-popup-beginning-line) 2))
      (should (eq (popup-test-helper-popup-end-line)  4))
      )))

(ert-deftest popup-test-scroll-bar-right-margin ()
  (popup-test-with-common-setup
    (insert (make-string (- (window-width) 1) ? ))
    (let ((popup-scroll-bar-foreground-char
           (propertize "f" 'face 'popup-scroll-bar-foreground-face))
          (popup-scroll-bar-background-char
           (propertize "b" 'face 'popup-scroll-bar-background-face)))
      (popup-tip "\
Foo
Bar
Baz
Fez
Oz"
                 :nowait t :height 3 :scroll-bar t :margin t)
      (should-not (popup-test-helper-rectangle-match "Fez"))
      (should-not (popup-test-helper-rectangle-match "Oz"))
      (should (eq (popup-test-helper-popup-beginning-line) 2))
      (should (eq (popup-test-helper-popup-end-line)  4))
      ;; Pending: #21
      ;;       (should (popup-test-helper-rectangle-match "\
      ;; Foof
      ;; Barb
      ;; Bazb"))
      )))

(ert-deftest popup-test-min-height ()
  (popup-test-with-common-setup
    (insert (make-string (- (window-width) 1) ? ))
    (popup-tip "Hello" :nowait t :min-height 10)
    (should (popup-test-helper-rectangle-match "Hello"))
    (should (eq (popup-test-helper-popup-beginning-line) 2))
    (should (eq (popup-test-helper-popup-end-line) 11))
    ))

(ert-deftest popup-test-menu ()
  (popup-test-with-common-setup
    (popup-menu* '("Foo" "Bar" "Baz") :nowait t)
    (should (popup-test-helper-rectangle-match "\
Foo
Bar
Baz"))
    (should (eq (popup-test-helper-popup-beginning-line) 2))
    ))

(ert-deftest popup-test-cascade-menu ()
  (popup-test-with-common-setup
    (popup-cascade-menu
     '(("Foo" "Foo1" "Foo2") "Bar" "Baz") :nowait t)
    (should (popup-test-helper-rectangle-match "Foo        >"))
    (should (popup-test-helper-rectangle-match "\
Foo
Bar
Baz"))
    (should-not (popup-test-helper-rectangle-match "Foo1"))
    (should-not (popup-test-helper-rectangle-match "Foo2"))
    (should (eq (popup-test-helper-popup-beginning-line) 2))
    ))

(ert-deftest popup-test-next ()
  (popup-test-with-common-setup
    (setq popup (popup-menu* '("Foo" "Bar" "Baz") :nowait t))
    (should (popup-test-helper-popup-selected-item "Foo"))
    (popup-next popup)
    (should (popup-test-helper-popup-selected-item "Bar"))
    (popup-next popup)
    (should (popup-test-helper-popup-selected-item "Baz"))
    (popup-next popup)
    (should (popup-test-helper-popup-selected-item "Foo"))
    (should (popup-test-helper-rectangle-match "Foo\nBar\nBaz"))
    (should (eq (popup-test-helper-popup-beginning-line) 2))
    ))

(ert-deftest popup-test-previous ()
  (popup-test-with-common-setup
    (setq popup (popup-menu* '("Foo" "Bar" "Baz") :nowait t))
    (should (popup-test-helper-popup-selected-item "Foo"))
    (popup-previous popup)
    (should (popup-test-helper-popup-selected-item "Baz"))
    (popup-previous popup)
    (should (popup-test-helper-popup-selected-item "Bar"))
    (popup-previous popup)
    (should (popup-test-helper-popup-selected-item "Foo"))
    (should (popup-test-helper-rectangle-match "\
Foo
Bar
Baz"))
    (should (eq (popup-test-helper-popup-beginning-line) 2))
    ))

(ert-deftest popup-test-select ()
  (popup-test-with-common-setup
    (setq popup (popup-menu* '("Foo" "Bar" "Baz") :nowait t))
    (should (popup-test-helper-popup-selected-item "Foo"))
    (popup-select popup 1)
    (should (popup-test-helper-popup-selected-item "Bar"))
    (popup-select popup 0)
    (should (popup-test-helper-popup-selected-item "Foo"))
    (popup-select popup 2)
    (should (popup-test-helper-popup-selected-item "Baz"))
    (should (popup-test-helper-rectangle-match "\
Foo
Bar
Baz"))
    (should (eq (popup-test-helper-popup-beginning-line) 2))
    ))

(ert-deftest popup-test-scroll-down ()
  (popup-test-with-common-setup
    (setq popup
          (popup-cascade-menu (cl-loop for x to 100 collect (format "Foo%d" x))
                              :nowait t :height 10 :margin t :scroll-bar t))
    (should (popup-test-helper-rectangle-match "\
Foo0
Foo1
Foo2"))
    (should (popup-test-helper-popup-selected-item "Foo0"))
    (popup-scroll-down popup 10)
    (should (popup-test-helper-popup-selected-item "Foo10"))
    (popup-scroll-down popup 10)
    (should (popup-test-helper-popup-selected-item "Foo20"))
    (popup-scroll-down popup 100)
    (should-not (popup-test-helper-rectangle-match "Foo90"))
    (should (popup-test-helper-rectangle-match "Foo91"))
    (should (popup-test-helper-rectangle-match "Foo100"))
    (should-not (popup-test-helper-rectangle-match "Foo0"))
    (should (eq (popup-test-helper-popup-beginning-line) 2))
    ))

(ert-deftest popup-test-scroll-up ()
  (popup-test-with-common-setup
    (setq popup
          (popup-cascade-menu (cl-loop for x to 100 collect (format "Foo%d" x))
                              :nowait t :height 10 :margin t :scroll-bar t))
    (should (popup-test-helper-rectangle-match "\
Foo0
Foo1
Foo2"))
    (should (popup-test-helper-popup-selected-item "Foo0"))
    (popup-scroll-down popup 100)
    (should (popup-test-helper-popup-selected-item "Foo91"))
    (popup-scroll-up popup 10)
    (should (popup-test-helper-popup-selected-item "Foo81"))
    (popup-scroll-up popup 10)
    (should-not (popup-test-helper-rectangle-match "Foo70"))
    (should (popup-test-helper-rectangle-match "Foo71"))
    (should (popup-test-helper-rectangle-match "Foo80"))
    (should-not (popup-test-helper-rectangle-match "Foo81"))
    (should (eq (popup-test-helper-popup-beginning-line) 2))
    ))

(ert-deftest popup-test-two-tip ()
  (popup-test-with-common-setup
   (popup-tip "\
Foo
Bar" :nowait t)
   (save-excursion (insert "\n"))
   (popup-tip "\
Baz
Qux" :nowait t)
   ;; Pending: #20
   ;;    (should (popup-test-helper-rectangle-match "\
   ;; Foo
   ;; Bar"))
   ;;    (should (popup-test-helper-rectangle-match "\
   ;; Baz
   ;; Qux"))
   ))

(ert-deftest popup-test-initial-index ()
  (popup-test-with-common-setup
    (setq popup (popup-menu* '("Foo" "Bar" "Baz") :initial-index 0 :nowait t))
    (should (popup-test-helper-popup-selected-item "Foo")))

  (popup-test-with-common-setup
    (setq popup (popup-menu* '("Foo" "Bar" "Baz") :initial-index 2 :nowait t))
    (should (popup-test-helper-popup-selected-item "Baz")))

  (popup-test-with-common-setup
    (setq popup (popup-menu* '("Foo" "Bar" "Baz") :initial-index 2 :height 1 :scroll-bar t :nowait t))
    (should (popup-test-helper-popup-selected-item "Baz")))

  (popup-test-with-common-setup
    (setq popup (popup-menu* '("Foo" "Bar" "Baz") :initial-index -1 :nowait t))
    (should (popup-test-helper-popup-selected-item "Foo")))

  (popup-test-with-common-setup
    (setq popup (popup-menu* '("Foo" "Bar" "Baz") :initial-index 100 :nowait t))
    (should (popup-test-helper-popup-selected-item "Baz"))))

(defun popup-test-helper-input (key)
  (push key unread-command-events))

(ert-deftest popup-test-isearch ()
  (popup-test-with-common-setup
    (popup-test-helper-create-popup "\
foo
bar
baz")
    (popup-isearch-update popup 'popup-isearch-filter-list "a")
    (should (popup-test-helper-rectangle-match "\
bar
baz"))
    (should-not (popup-test-helper-rectangle-match "foo"))
    ))
