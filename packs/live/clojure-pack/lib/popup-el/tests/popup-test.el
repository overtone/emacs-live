(require 'ert)

(require 'popup)
;; for "every" function
(require 'cl)

(when (< (frame-width) (length "long long long long line"))
  (set-frame-size (selected-frame) 80 35))

(defmacro popup-test-with-create-popup (&rest body)
  (declare (indent 0) (debug t))
  `(let ((popup (popup-create (point) 10 10)))
     (unwind-protect
         (progn ,@body)
       (when popup
         (popup-delete popup)))
     ))

(defmacro popup-test-with-temp-buffer (&rest body)
  (declare (indent 0) (debug t))
  `(save-excursion
     (with-temp-buffer
       (switch-to-buffer (current-buffer))
       (delete-other-windows)
       (erase-buffer)
       ,@body
       )))

(defmacro popup-test-with-common-setup (&rest body)
  (declare (indent 0) (debug t))
  `(popup-test-with-temp-buffer
     (popup-test-with-create-popup ,@body)))

(defun popup-test-helper-get-overlays-buffer ()
  "Create a new buffer called *text* containing the visible text
of the current buffer, ie. it converts overlays containing text
into real text. Return *text* buffer"
  (interactive)
  (let ((tb (get-buffer-create "*text*"))
        (s (point-min))
        (os (overlays-in (point-min) (point-max))))
    (with-current-buffer tb
      (erase-buffer))
    (setq os (sort os (lambda (o1 o2)
                        (< (overlay-start o1)
                           (overlay-start o2)))))
    (mapc (lambda (o)
            (let ((bt (buffer-substring-no-properties s (overlay-start o)))
                  (b (overlay-get o 'before-string))
                  (text (or (overlay-get o 'display)
                            (buffer-substring-no-properties (overlay-start o) (overlay-end o))))
                  (a (overlay-get o 'after-string))
                  (inv (overlay-get o 'invisible)))
              (with-current-buffer tb
                (insert bt)
                (unless inv
                  (when b (insert b))
                  (insert text)
                  (when a (insert a))))
              (setq s (overlay-end o))))
          os)
    (let ((x (buffer-substring-no-properties s (point-max))))
      (with-current-buffer tb
        (insert x)
        tb))))

(defun popup-test-helper-match-points (strings)
  "Return list of start of first match"
  (when (listp strings)
    (let ((text (buffer-string)))
      (mapcar
       (lambda (content)
         (let ((pos (string-match (regexp-quote content) text)))
           (if (null pos) pos (1+ pos))))
       strings))))

(defun popup-test-helper-points-to-columns (points)
  "Return list of colum"
  (mapcar
   (lambda (point)
     (save-excursion (goto-char point) (current-column)))
   points))

(defun popup-test-helper-same-all-p (seq)
  "Return first element if `eq' every element of SEQ.If not, return nil."
  (reduce #'(lambda (x y) (if (eq x y) x nil)) seq))

(defun popup-test-helper-input (key)
  (push key unread-command-events))

;; Test for helper method
(ert-deftest popup-test-test-helper ()
  (should (eq (popup-test-helper-same-all-p '(0 0 0)) 0))
  (should (eq (popup-test-helper-same-all-p '(1 1 1)) 1))
  (should (eq (popup-test-helper-same-all-p '(0 1 1)) nil))
  )

;; Test for popup-el
(ert-deftest popup-test-simple ()
  (popup-test-with-common-setup
    (popup-set-list popup '("foo" "bar" "baz"))
    (popup-draw popup)
    (should (equal (popup-list popup) '("foo" "bar" "baz")))
    (with-current-buffer (popup-test-helper-get-overlays-buffer)
      (let ((points (popup-test-helper-match-points '("foo" "bar" "baz"))))
        (should (every #'identity points))
        (should (equal (popup-test-helper-points-to-columns points) '(0 0 0)))
        (should (eq (popup-test-helper-same-all-p
                     (popup-test-helper-points-to-columns points)) 0))))))

(ert-deftest popup-test-delete ()
  (popup-test-with-common-setup
    (popup-delete popup)
    (should-not (popup-live-p popup))))

(ert-deftest popup-test-hide ()
  (popup-test-with-common-setup
    (popup-set-list popup '("foo" "bar" "baz"))
    (popup-draw popup)
    (popup-hide popup)
    (should (equal (popup-list popup) '("foo" "bar" "baz")))
    (with-current-buffer (popup-test-helper-get-overlays-buffer)
      (should-not (every #'identity
                         (popup-test-helper-match-points '("foo" "bar" "baz")))))
    ))

(ert-deftest popup-test-tip ()
  (popup-test-with-temp-buffer
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
     :nowait t)
    (with-current-buffer (popup-test-helper-get-overlays-buffer)
      (let ((points (popup-test-helper-match-points
                     '("CURSOR-COLOR is a cursor color during isearch"
                       "KEYMAP is a keymap"))))
        (should (every #'identity points))
        (should (eq (popup-test-helper-same-all-p
                     (popup-test-helper-points-to-columns points)) 0)))
      )))

(ert-deftest popup-test-culumn ()
  (popup-test-with-temp-buffer
    (insert " ")
    (popup-test-with-create-popup
      (popup-set-list popup '("foo" "bar" "baz"))
      (popup-draw popup)
      (should (equal (popup-list popup) '("foo" "bar" "baz")))
      (with-current-buffer (popup-test-helper-get-overlays-buffer)
        (let ((points (popup-test-helper-match-points '("foo" "bar" "baz"))))
          (should (every #'identity points))
          (should (equal (popup-test-helper-points-to-columns points)
                         '(1 1 1)))
          )
        ))))

(ert-deftest popup-test-folding-long-line-right-top ()
  (popup-test-with-temp-buffer
    ;; To use window-width because Emacs 23 does not have window-body-width
    (insert (make-string (- (window-width) 3) ? ))
    (popup-tip "long long long long line" :nowait t)
    (with-current-buffer (popup-test-helper-get-overlays-buffer)
      (let ((points (popup-test-helper-match-points
                     '("long long long long line"))))
        (should (every #'identity points))
        (should (eq (line-number-at-pos (car points)) 2))
        ))))

(ert-deftest popup-test-folding-long-line-left-bottom ()
  (popup-test-with-temp-buffer
    (insert (make-string (- (window-body-height) 1) ?\n))
    (popup-tip "long long long long line" :nowait t)
    (with-current-buffer (popup-test-helper-get-overlays-buffer)
      (let ((points (popup-test-helper-match-points
                     '("long long long long line"))))
        (should (every #'identity points))
        (should (eq (line-number-at-pos (car points))
                    (- (window-body-height) 1)))
        ))))

(ert-deftest popup-test-folding-long-line-right-bottom ()
  (popup-test-with-temp-buffer
    (insert (make-string (- (window-body-height) 1) ?\n))
    (insert (make-string (- (window-width) 3) ? ))
    (popup-tip "long long long long line" :nowait t)
    (with-current-buffer (popup-test-helper-get-overlays-buffer)
      (let ((points (popup-test-helper-match-points
                     '("long long long long line"))))
        (should (every #'identity points))
        (should (eq (line-number-at-pos (car points))
                    (- (window-body-height) 1))))
      )))

(ert-deftest popup-test-folding-short-line-right-top ()
  (popup-test-with-temp-buffer
    (insert (make-string (- (window-width) 3) ? ))
    (popup-tip "bla\nbla\nbla\nbla\nbla" :nowait t)
    (with-current-buffer (popup-test-helper-get-overlays-buffer)
      (let ((points (popup-test-helper-match-points
                     '("bla"))))
        (should (every #'identity points))
        (should (eq (line-number-at-pos (car points)) 2))
        ))))

(ert-deftest popup-test-folding-short-line-left-bottom ()
  (popup-test-with-temp-buffer
    (insert (make-string (- (window-body-height) 1) ?\n))
    (popup-tip "bla\nbla\nbla\nbla\nbla" :nowait t)
    (with-current-buffer (popup-test-helper-get-overlays-buffer)
      (let ((points (popup-test-helper-match-points
                     '("bla"))))
        (should (every #'identity points))
        (should (eq (popup-test-helper-same-all-p
                     (popup-test-helper-points-to-columns points)) 0))
        (should (eq (line-number-at-pos (car points))
                    (- (window-body-height) 5)))
        ))))

(ert-deftest popup-test-folding-short-line-right-bottom ()
  (popup-test-with-temp-buffer
    (insert (make-string (- (window-body-height) 1) ?\n))
    (insert (make-string (- (window-width) 3) ? ))
    (popup-tip "bla\nbla\nbla\nbla\nbla" :nowait t)
    (with-current-buffer (popup-test-helper-get-overlays-buffer)
      (let ((points (popup-test-helper-match-points
                     '("bla"))))
        (should (every #'identity points))
        (should (eq (line-number-at-pos (car points))
                    (- (window-body-height) 5))))
      )))

(ert-deftest popup-test-margin-at-middle ()
  (popup-test-with-temp-buffer
    (insert " ")
    (let ((popup (popup-tip "Margin?" :nowait t :margin t)))
      (with-current-buffer (popup-test-helper-get-overlays-buffer)
        (let ((points (popup-test-helper-match-points '(" Margin? "))))
          (should (every #'identity points))
          (should (equal (car (popup-test-helper-points-to-columns points))
                         0))
          )))))
