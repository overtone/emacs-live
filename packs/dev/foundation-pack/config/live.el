;;http://www.emacswiki.org/emacs/IncrementNumber
(defun live-increment-number-at-point ()
  (interactive)
  (save-excursion
    (save-match-data
      (or (looking-at "[0123456789]")
          (error "No number at point"))
      (replace-match (number-to-string (mod (1+ (string-to-number (match-string 0))) 10))))))

(defun live-decrement-number-at-point ()
  (interactive)
  (save-excursion
    (save-match-data
      (or (looking-at "[0123456789]")
          (error "No number at point"))
      (replace-match (number-to-string (mod (1- (string-to-number (match-string 0))) 10))))))
