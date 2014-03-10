;;; test-org-clock.el --- Tests for org-clock.el

;; Copyright (C) 2012, 2014  Nicolas Goaziou

;; Author: Nicolas Goaziou <n.goaziou at gmail dot com>

;; Released under the GNU General Public License version 3
;; see: http://www.gnu.org/licenses/gpl-3.0.html

;;;; Comments



;;; Code:

(defun org-test-clock-create-timestamp (input &optional inactive with-time)
  "Create a timestamp out of a date/time prompt string.

INPUT is a string as expected in a date/time prompt, i.e \"+2d\"
or \"2/5\".

When optional argument INACTIVE is non-nil, return an inactive
timestamp. When optional argument WITH-TIME is non-nil, also
insert hours and minutes.

Return the timestamp as a string."
  (org-element-interpret-data
   (let ((time (decode-time
                (apply 'encode-time
                       (mapcar (lambda (el) (or el 0))
                               (org-read-date-analyze
                                input nil (decode-time (current-time))))))))
     (list 'timestamp
           (list :type (if inactive 'inactive 'active)
                 :minute-start (and with-time (nth 1 time))
                 :hour-start (and with-time (nth 2 time))
                 :day-start (nth 3 time)
                 :month-start (nth 4 time)
                 :year-start (nth 5 time))))))

(defun org-test-clock-create-clock (input1 &optional input2)
  "Create a clock line out of two date/time prompts.

INPUT1 and INPUT2 are strings as expected in a date/time prompt,
i.e \"+2d\" or \"2/5\".  They respectively refer to start and end
range.  INPUT2 can be omitted if clock hasn't finished yet.

Return the clock line as a string."
  (let* ((beg (org-test-clock-create-timestamp input1 t t))
	 (end (and input2 (org-test-clock-create-timestamp input2 t t)))
	 (sec-diff (and input2 (floor (- (org-time-string-to-seconds end)
					 (org-time-string-to-seconds beg))))))
    (concat org-clock-string " " beg
	    (when end
	      (concat "--" end " => "
		      (format "%2d:%02d"
			      (/ sec-diff 3600)
			      (/ (mod sec-diff 3600) 60))))
	    "\n")))

(defun test-org-clock-clocktable-contents-at-point (options)
  "Return contents of a clocktable at point.
OPTIONS is a string of clocktable options.  Caption is ignored in
contents.  The clocktable doesn't appear in the buffer."
  (save-excursion
    (insert "#+BEGIN: clocktable " options "\n")
    (insert "#+END: clocktable\n"))
  (unwind-protect
      (save-excursion
	(org-update-dblock)
	(forward-line)
	;; Skip caption.
	(when (looking-at "#\\+CAPTION:") (forward-line))
	(buffer-substring (point)
			  (progn (search-forward "#+END: clocktable")
				 (match-beginning 0))))
    ;; Remove clocktable.
    (delete-region (point)
		   (progn (search-forward "#+END: clocktable")
			  (forward-line)
			  (point)))))



;;; Clocktable

(ert-deftest test-org-clock/clocktable ()
  "Test clocktable specifications."
  ;; Relative time: Previous two days.
  (should
   (equal
    "| Headline                     | Time    |       |
|------------------------------+---------+-------|
| *Total time*                 | *16:00* |       |
|------------------------------+---------+-------|
| Relative times in clocktable | 16:00   |       |
| Foo                          |         |  5:00 |
| Bar                          |         | 11:00 |
"
    (org-test-with-temp-text "* Relative times in clocktable\n** Foo\n** Bar\n"
      (progn
	;; Install Clock lines in "Foo".
	(search-forward "** Foo")
	(forward-line)
	(insert (org-test-clock-create-clock "-2d 8:00" "-2d 13:00"))
	(insert (org-test-clock-create-clock ". 8:00" "13:00"))
	;; Install Clock lines in "Bar".
	(search-forward "** Bar")
	(forward-line)
	(insert (org-test-clock-create-clock "-2d 15:00" "-2d 18:00"))
	(insert (org-test-clock-create-clock "-1d 8:00" "-1d 13:00"))
	(insert (org-test-clock-create-clock "-1d 15:00" "-1d 18:00"))
	(insert (org-test-clock-create-clock ". 15:00"))
	;; Previous two days.
	(goto-char (point-min))
	(forward-line)
	(test-org-clock-clocktable-contents-at-point
	 ":tstart \"<today-2>\" :tend \"<today>\" :indent nil")))))
  ;; Relative time: Yesterday until now.
  (should
   (equal
    "| Headline                     | Time    |      |
|------------------------------+---------+------|
| *Total time*                 | *13:00* |      |
|------------------------------+---------+------|
| Relative times in clocktable | 13:00   |      |
| Foo                          |         | 5:00 |
| Bar                          |         | 8:00 |
"
    (org-test-with-temp-text "* Relative times in clocktable\n** Foo\n** Bar\n"
      (progn
	;; Install Clock lines in "Foo".
	(search-forward "** Foo")
	(forward-line)
	(insert (org-test-clock-create-clock "-2d 8:00" "-2d 13:00"))
	(insert (org-test-clock-create-clock ". 8:00" "13:00"))
	;; Install Clock lines in "Bar".
	(search-forward "** Bar")
	(forward-line)
	(insert (org-test-clock-create-clock "-2d 15:00" "-2d 18:00"))
	(insert (org-test-clock-create-clock "-1d 8:00" "-1d 13:00"))
	(insert (org-test-clock-create-clock "-1d 15:00" "-1d 18:00"))
	(insert (org-test-clock-create-clock ". 15:00"))
	;; Previous two days.
	(goto-char (point-min))
	(forward-line)
	(test-org-clock-clocktable-contents-at-point
	 ":tstart \"<yesterday>\" :tend \"<tomorrow>\" :indent nil"))))))


(provide 'test-org-clock)
;;; test-org-clock.el end here
