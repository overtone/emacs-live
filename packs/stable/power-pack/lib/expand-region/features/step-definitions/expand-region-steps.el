(Given "^mark is inactive by default$"
       (lambda ()
         (setq set-mark-default-inactive t)))

(When "^I expand the region$"
      (lambda ()
        (flet ((message (&rest args) nil))
          (er/expand-region 1))))

(When "^I quit$"
      (lambda ()
        (flet ((signal (&rest args) nil))
          (keyboard-quit))))

(When "^I expand the region \\([0-9]+\\) times$"
      (lambda (arg)
        (flet ((message (&rest args) nil))
          (er/expand-region (string-to-number arg)))))

(And "^I contract the region$"
     (lambda ()
       (er/contract-region 1)))

(When "^I place the cursor after \"\\(.+\\)\"$"
      (lambda (arg)
        (goto-char (point-min))
        (let ((search (search-forward arg nil t))
              (message "Can not place cursor after '%s', because there is no such point: '%s'"))
          (assert search nil message arg (espuds-buffer-contents)))))

(When "^I place the cursor before \"\\(.+\\)\"$"
      (lambda (arg)
        (goto-char (point-max))
        (let ((search (search-backward arg nil t))
              (message "Can not place cursor before '%s', because there is no such point: '%s'"))
          (assert search nil message arg (espuds-buffer-contents)))))

(When "^I pop the mark$"
      (lambda ()
        (set-mark-command 4)))

(When "^I deactivate the mark$"
      (lambda ()
        (deactivate-mark)))

(When "^I activate the mark$"
      (lambda ()
        (activate-mark)))

(Then "^the region should not be active$"
      (lambda ()
        (should
         (not (region-active-p)))))

(Then "^cursor should be at point \"\\(.+\\)\"$"
      (lambda (arg)
        (should
         (=
          (string-to-number arg)
          (point)))))

(And "^autocopy-register is \"\\(.\\)\"$"
      (lambda (reg)
        (setq expand-region-autocopy-register reg)
        (set-register (aref reg 0) nil)))

(Then "^register \"\\(.\\)\" should be \"\\(.+\\)\"$"
      (lambda (reg contents)
        (should
         (equal contents (get-register (aref reg 0))))))

(When "^I go to the \\(front\\|end\\) of the word \"\\(.+\\)\"$"
      (lambda (pos word)
        (goto-char (point-min))
        (let ((search (re-search-forward (format "%s" word) nil t))
              (message "Can not go to character '%s' since it does not exist in the current buffer: %s"))
          (assert search nil message word (espuds-buffer-contents))
          (if (string-equal "front" pos) (backward-word)))))

(When "^I set \\(.+\\) to \\(.+\\)$"
      (lambda (var val)
        (set (intern var) (read val))))
