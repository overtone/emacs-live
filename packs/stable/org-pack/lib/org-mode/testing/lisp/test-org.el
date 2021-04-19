;;; test-org.el --- tests for org.el

;; Copyright (c)  David Maus
;; Authors: David Maus

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Template test file for Org tests

;;; Code:

(eval-and-compile (require 'cl-lib))


;;; Comments

(ert-deftest test-org/toggle-comment ()
  "Test `org-toggle-comment' specifications."
  ;; Simple headline.
  (should
   (equal "* Test"
	  (org-test-with-temp-text "* COMMENT Test"
	    (org-toggle-comment)
	    (buffer-string))))
  (should
   (equal "* COMMENT Test"
	  (org-test-with-temp-text "* Test"
	    (org-toggle-comment)
	    (buffer-string))))
  ;; Headline with a regular keyword.
  (should
   (equal "* TODO Test"
	  (org-test-with-temp-text "* TODO COMMENT Test"
	    (org-toggle-comment)
	    (buffer-string))))
  (should
   (equal "* TODO COMMENT Test"
	  (org-test-with-temp-text "* TODO Test"
	    (org-toggle-comment)
	    (buffer-string))))
  ;; Empty headline.
  (should
   (equal "* "
	  (org-test-with-temp-text "* COMMENT"
	    (org-toggle-comment)
	    (buffer-string))))
  (should
   (equal "* COMMENT"
	  (org-test-with-temp-text "* "
	    (org-toggle-comment)
	    (buffer-string))))
  ;; Headline with a single keyword.
  (should
   (equal "* TODO "
	  (org-test-with-temp-text "* TODO COMMENT"
	    (org-toggle-comment)
	    (buffer-string))))
  (should
   (equal "* TODO COMMENT"
	  (org-test-with-temp-text "* TODO"
	    (org-toggle-comment)
	    (buffer-string))))
  ;; Headline with a keyword, a priority cookie and contents.
  (should
   (equal "* TODO [#A] Headline"
	  (org-test-with-temp-text "* TODO [#A] COMMENT Headline"
	    (org-toggle-comment)
	    (buffer-string))))
  (should
   (equal "* TODO [#A] COMMENT Headline"
	  (org-test-with-temp-text "* TODO [#A] Headline"
	    (org-toggle-comment)
	    (buffer-string)))))

(ert-deftest test-org/comment-dwim ()
  "Test `comment-dwim' behaviour in an Org buffer."
  ;; No region selected, no comment on current line and line not
  ;; empty: insert comment on line above.
  (should
   (equal "# \nComment"
	  (org-test-with-temp-text "Comment"
	    (call-interactively #'org-comment-dwim)
	    (buffer-string))))
  ;; No region selected, no comment on current line and line empty:
  ;; insert comment on this line.
  (should
   (equal "# \nParagraph"
	  (org-test-with-temp-text "\nParagraph"
	    (call-interactively #'org-comment-dwim)
	    (buffer-string))))
  ;; No region selected, and a comment on this line: indent it.
  (should
   (equal "* Headline\n  # Comment"
	  (org-test-with-temp-text "* Headline\n# <point>Comment"
	    (let ((org-adapt-indentation t))
	      (call-interactively #'org-comment-dwim))
	    (buffer-string))))
  ;; Also recognize single # at column 0 as comments.
  (should
   (equal "# Comment"
	  (org-test-with-temp-text "# Comment"
	    (call-interactively #'org-comment-dwim)
	    (buffer-string))))
  ;; Region selected and only comments and blank lines within it:
  ;; un-comment all commented lines.
  (should
   (equal "Comment 1\n\nComment 2"
	  (org-test-with-temp-text "# Comment 1\n\n# Comment 2"
	    (transient-mark-mode 1)
	    (push-mark (point) t t)
	    (goto-char (point-max))
	    (call-interactively #'org-comment-dwim)
	    (buffer-string))))
  ;; Region selected without comments: comment all lines if
  ;; `comment-empty-lines' is non-nil, only non-blank lines otherwise.
  (should
   (equal "# Comment 1\n\n# Comment 2"
	  (org-test-with-temp-text "Comment 1\n\nComment 2"
	    (transient-mark-mode 1)
	    (push-mark (point) t t)
	    (goto-char (point-max))
	    (let ((comment-empty-lines nil))
	      (call-interactively #'org-comment-dwim))
	    (buffer-string))))
  (should
   (equal "# Comment 1\n# \n# Comment 2"
	  (org-test-with-temp-text "Comment 1\n\nComment 2"
	    (transient-mark-mode 1)
	    (push-mark (point) t t)
	    (goto-char (point-max))
	    (let ((comment-empty-lines t))
	      (call-interactively #'org-comment-dwim))
	    (buffer-string))))
  ;; In front of a keyword without region, insert a new comment.
  (should
   (equal "# \n#+KEYWORD: value"
	  (org-test-with-temp-text "#+KEYWORD: value"
	    (call-interactively #'org-comment-dwim)
	    (buffer-string))))
  ;; Comment a heading
  (should
   (equal "* COMMENT Test"
	  (org-test-with-temp-text "* Test"
	    (call-interactively #'org-comment-dwim)
	    (buffer-string))))
  ;; In a source block, use appropriate syntax.
  (should
   (equal "  ;; "
	  (org-test-with-temp-text "#+BEGIN_SRC emacs-lisp\n<point>\n#+END_SRC"
	    (let ((org-edit-src-content-indentation 2))
	      (call-interactively #'org-comment-dwim))
	    (buffer-substring-no-properties (line-beginning-position)
					    (point)))))
  (should
   (equal "#+BEGIN_SRC emacs-lisp\n  ;; a\n  ;; b\n#+END_SRC"
	  (org-test-with-temp-text
	      "#+BEGIN_SRC emacs-lisp\n<point>a\nb\n#+END_SRC"
	    (transient-mark-mode 1)
	    (push-mark (point) t t)
	    (forward-line 2)
	    (let ((org-edit-src-content-indentation 2))
	      (call-interactively #'org-comment-dwim))
	    (buffer-string)))))



;;; Date and time analysis

(ert-deftest test-org/org-read-date ()
  "Test `org-read-date' specifications."
  ;; Parse ISO date with abbreviated year and month.
  (should (equal "2012-03-29 16:40"
		 (let ((org-time-was-given t))
		   (org-read-date t nil "12-3-29 16:40"))))
  ;; Parse Europeans dates.
  (should (equal "2012-03-29 16:40"
		 (let ((org-time-was-given t))
		   (org-read-date t nil "29.03.2012 16:40"))))
  ;; Parse Europeans dates without year.
  (should (string-match "2[0-9]\\{3\\}-03-29 16:40"
			(let ((org-time-was-given t))
			  (org-read-date t nil "29.03. 16:40"))))
  ;; Relative date applied to current time if there is single
  ;; plus/minus, or to default date when there are two of them.
  (should
   (equal
    "2015-03-04"
    (org-test-at-time "2014-03-04"
      (org-read-date
       t nil "+1y" nil
       (apply #'encode-time (org-parse-time-string "2012-03-29"))))))
  (should
   (equal
    "2013-03-29"
    (org-test-at-time "2014-03-04"
      (org-read-date
       t nil "++1y" nil
       (apply #'encode-time (org-parse-time-string "2012-03-29"))))))
  ;; When `org-read-date-prefer-future' is non-nil, prefer future
  ;; dates (relatively to now) when incomplete.  Otherwise, use
  ;; default date.
  (should
   (equal
    "2014-04-01"
    (org-test-at-time "2014-03-04"
      (let ((org-read-date-prefer-future t))
	(org-read-date t nil "1")))))
  (should
   (equal
    "2013-03-04"
    (org-test-at-time "2012-03-29"
      (let ((org-read-date-prefer-future t))
	(org-read-date t nil "3-4")))))
  (should
   (equal
    "2012-03-04"
    (org-test-at-time "2012-03-29"
      (let ((org-read-date-prefer-future nil))
	(org-read-date t nil "3-4")))))
  ;; When set to `org-read-date-prefer-future' is set to `time', read
  ;; day is moved to tomorrow if specified hour is before current
  ;; time.  However, it only happens in no other part of the date is
  ;; specified.
  (should
   (equal
    "2012-03-30"
    (org-test-at-time "2012-03-29 16:40"
      (let ((org-read-date-prefer-future 'time))
	(org-read-date t nil "00:40" nil)))))
  (should-not
   (equal
    "2012-03-30"
    (org-test-at-time "2012-03-29 16:40"
      (let ((org-read-date-prefer-future 'time))
	(org-read-date t nil "29 00:40" nil)))))
  ;; Caveat: `org-read-date-prefer-future' always refers to current
  ;; time, not default time, when they differ.
  (should
   (equal
    "2014-04-01"
    (org-test-at-time "2014-03-04"
      (let ((org-read-date-prefer-future t))
	(org-read-date
	 t nil "1" nil
	 (apply #'encode-time (org-parse-time-string "2012-03-29")))))))
  (should
   (equal
    "2014-03-25"
    (org-test-at-time "2014-03-04"
      (let ((org-read-date-prefer-future t))
	(org-read-date
	 t nil "25" nil
	 (apply #'encode-time (org-parse-time-string "2012-03-29"))))))))

(ert-deftest test-org/org-parse-time-string ()
  "Test `org-parse-time-string'."
  (should (equal (org-parse-time-string "2012-03-29 16:40")
		 '(0 40 16 29 3 2012 nil nil nil)))
  (should (equal (org-parse-time-string "[2012-03-29 16:40]")
		 '(0 40 16 29 3 2012 nil nil nil)))
  (should (equal (org-parse-time-string "<2012-03-29 16:40>")
		 '(0 40 16 29 3 2012 nil nil nil)))
  (should (equal (org-parse-time-string "<2012-03-29>")
		 '(0 0 0 29 3 2012 nil nil nil)))
  (should (equal (org-parse-time-string "<2012-03-29>" t)
		 '(0 nil nil 29 3 2012 nil nil nil))))

(ert-deftest test-org/closest-date ()
  "Test `org-closest-date' specifications."
  (require 'calendar)
  ;; Time stamps without a repeater are returned unchanged.
  (should
   (equal
    '(3 29 2012)
    (calendar-gregorian-from-absolute
     (org-closest-date "<2012-03-29>" "<2014-03-04>" nil))))
  ;; Time stamps with a null repeater are returned unchanged.
  (should
   (equal
    '(3 29 2012)
    (calendar-gregorian-from-absolute
     (org-closest-date "<2012-03-29 +0d>" "<2014-03-04>" nil))))
  ;; if PREFER is set to `past' always return a date before, or equal
  ;; to CURRENT.
  (should
   (equal
    '(3 1 2014)
    (calendar-gregorian-from-absolute
     (org-closest-date "<2012-03-29 +1m>" "<2014-03-04>" 'past))))
  (should
   (equal
    '(3 4 2014)
    (calendar-gregorian-from-absolute
     (org-closest-date "<2012-03-04 +1m>" "<2014-03-04>" 'past))))
  ;; if PREFER is set to `future' always return a date before, or equal
  ;; to CURRENT.
  (should
   (equal
    '(3 29 2014)
    (calendar-gregorian-from-absolute
     (org-closest-date "<2012-03-29 +1m>" "<2014-03-04>" 'future))))
  (should
   (equal
    '(3 4 2014)
    (calendar-gregorian-from-absolute
     (org-closest-date "<2012-03-04 +1m>" "<2014-03-04>" 'future))))
  ;; If PREFER is neither `past' nor `future', select closest date.
  (should
   (equal
    '(3 1 2014)
    (calendar-gregorian-from-absolute
     (org-closest-date "<2012-03-29 +1m>" "<2014-03-04>" nil))))
  (should
   (equal
    '(5 4 2014)
    (calendar-gregorian-from-absolute
     (org-closest-date "<2012-03-04 +1m>" "<2014-04-28>" nil))))
  ;; Test "day" repeater.
  (should
   (equal '(3 8 2014)
	  (calendar-gregorian-from-absolute
	   (org-closest-date "<2014-03-04 +2d>" "<2014-03-09>" 'past))))
  (should
   (equal '(3 10 2014)
	  (calendar-gregorian-from-absolute
	   (org-closest-date "<2014-03-04 +2d>" "<2014-03-09>" 'future))))
  ;; Test "month" repeater.
  (should
   (equal '(1 5 2015)
	  (calendar-gregorian-from-absolute
	   (org-closest-date "<2014-03-05 +2m>" "<2015-02-04>" 'past))))
  (should
   (equal '(3 29 2014)
	  (calendar-gregorian-from-absolute
	   (org-closest-date "<2012-03-29 +2m>" "<2014-03-04>" 'future))))
  ;; Test "year" repeater.
  (should
   (equal '(3 5 2014)
	  (calendar-gregorian-from-absolute
	   (org-closest-date "<2014-03-05 +2y>" "<2015-02-04>" 'past))))
  (should
   (equal '(3 29 2014)
	  (calendar-gregorian-from-absolute
	   (org-closest-date "<2012-03-29 +2y>" "<2014-03-04>" 'future)))))

(ert-deftest test-org/deadline-close-p ()
  "Test `org-deadline-close-p' specifications."
  (org-test-at-time "2016-06-03 Fri 01:43"
    ;; Timestamps are close if they are within `ndays' of lead time.
    (org-test-with-temp-text "* Heading"
      (should (org-deadline-close-p "2016-06-03 Fri" 0))
      (should (org-deadline-close-p "2016-06-02 Thu" 0))
      (should-not (org-deadline-close-p "2016-06-04 Sat" 0))
      (should (org-deadline-close-p "2016-06-04 Sat" 1))
      (should (org-deadline-close-p "2016-06-03 Fri 12:00" 0)))
    ;; Read `ndays' from timestamp if argument not given.
    (org-test-with-temp-text "* H"
      (should (org-deadline-close-p "2016-06-04 Sat -1d"))
      (should-not (org-deadline-close-p "2016-06-04 Sat -0d"))
      (should (org-deadline-close-p "2016-06-10 Fri -1w"))
      (should-not (org-deadline-close-p "2016-06-11 Sat -1w")))
    ;; Prefer `ndays' argument over lead time in timestamp.
    (org-test-with-temp-text "* H"
      (should (org-deadline-close-p "2016-06-04 Sat -0d" 1))
      (should-not (org-deadline-close-p "2016-06-04 Sat -0d" 0)))
    ;; Completed tasks are never close.
    (let ((org-todo-keywords '(("TODO" "|" "DONE"))))
      (org-test-with-temp-text "* TODO Heading"
	(should (org-deadline-close-p "2016-06-03")))
      (org-test-with-temp-text "* DONE Heading"
	(should-not (org-deadline-close-p "2016-06-03"))))))


;;; Drawers

(ert-deftest test-org/at-property-p ()
  "Test `org-at-property-p' specifications."
  (should
   (equal 't
	  (org-test-with-temp-text "* H\n:PROPERTIES:\n<point>:PROP: t\n:END:\n"
	    (org-at-property-p))))
  (should
   (equal 't
	  (org-test-with-temp-text ":PROPERTIES:\n<point>:PROP: t\n:END:\n"
	    (org-at-property-p)))))

(ert-deftest test-org/at-property-drawer-p ()
  "Test `org-at-property-drawer-p' specifications."
  (should
   (org-test-with-temp-text "* H\n<point>:PROPERTIES:\n:PROP: t\n:END:\n"
     (org-at-property-drawer-p)))
  (should
   (org-test-with-temp-text ":PROPERTIES:\n:PROP: t\n:END:\n"
     (org-at-property-drawer-p)))
  ;; The function only returns t if point is at the first line of
  ;; a property block.
  (should-not
   (org-test-with-temp-text ":PROPERTIES:\n<point>:PROP: t\n:END:\n"
     (org-at-property-drawer-p)))
  ;; The function ignores incomplete drawers.
  (should-not
   (org-test-with-temp-text ":PROPERTIES:\n<point>:PROP: t\n"
     (org-at-property-drawer-p))))

(ert-deftest test-org/get-property-block ()
  "Test `org-get-property-block' specifications."
  (should
   (equal '(14 . 14)
	  (org-test-with-temp-text ":PROPERTIES:\n:END:\n* H\n"
	    (org-get-property-block))))
  (should
   (equal '(14 . 14)
	  (org-test-with-temp-text ":PROPERTIES:\n:END:\n"
	    (org-get-property-block))))
  ;; Comments above a document property block is ok.
  (should
   (equal '(18 . 18)
	  (org-test-with-temp-text "# C\n:PROPERTIES:\n:END:\n"
	    (org-get-property-block))))
  ;; Keywords above a document property block is ok.
  (should
   (equal '(22 . 22)
	  (org-test-with-temp-text "# C\n# C\n:PROPERTIES:\n:END:\n"
	    (org-get-property-block))))
  ;; Comments and keywords are allowed before a document property block.
  (should
   (equal '(18 . 27)
	  (org-test-with-temp-text "# C\n:PROPERTIES:\n:KEY: V:\n:END:\n"
	    (org-get-property-block))))
  ;; A document property block will not be valid if there are lines
  ;; with whitespace above it
  (should-not
   (org-test-with-temp-text "\n:PROPERTIES:\n:END:\n"
     (org-get-property-block)))
  (should
   (equal '(18 . 18)
	  (org-test-with-temp-text "* H\n:PROPERTIES:\n:END:\n<point>"
	    (org-get-property-block))))
  (should
   (equal "* H\n:PROPERTIES:\n:END:\n"
	  (org-test-with-temp-text "* H"
	    (let ((org-adapt-indentation nil))
	      (org-get-property-block nil 'force))
	    (buffer-string))))
  (should
   (equal ":PROPERTIES:\n:END:\n"
	  (org-test-with-temp-text ""
	    (org-get-property-block nil 'force)
	    (buffer-string))))
  (should
   (equal "* H1\n  :PROPERTIES:\n  :END:\n* H2"
	  (org-test-with-temp-text "* H1\n* H2"
	    (let ((org-adapt-indentation t))
	      (org-get-property-block nil 'force))
	    (buffer-string)))))

(ert-deftest test-org/insert-property-drawer ()
  "Test `org-insert-property-drawer' specifications."
  ;; Insert drawer in empty buffer
  (should
   (equal ":PROPERTIES:\n:END:\n"
	  (org-test-with-temp-text ""
	    (let ((org-adapt-indentation nil)) (org-insert-property-drawer))
	    (buffer-string))))
  ;; Insert drawer in document header with existing comment and
  ;; keyword.
  (should
   (equal "# C\n:PROPERTIES:\n:END:\n#+TITLE: T"
	  (org-test-with-temp-text "# C\n#+TITLE: T"
	    (let ((org-adapt-indentation nil)) (org-insert-property-drawer))
	    (buffer-string))))
  ;; Insert drawer in document header with existing keyword.
  (should
   (equal ":PROPERTIES:\n:END:\n#+TITLE: T"
	  (org-test-with-temp-text "#+TITLE: T"
	    (let ((org-adapt-indentation nil)) (org-insert-property-drawer))
	    (buffer-string))))
  (should
   (equal ":PROPERTIES:\n:END:"
	  (org-test-with-temp-text ":PROPERTIES:\n:END:"
	    (let ((org-adapt-indentation nil)) (org-insert-property-drawer))
	    (buffer-string))))
  ;; Insert drawer in document header with one existing heading in buffer.
  (should
   (equal ":PROPERTIES:\n:END:\n\n* T\n"
	  (org-test-with-temp-text "\n* T\n"
	    (let ((org-adapt-indentation nil)) (org-insert-property-drawer))
	    (buffer-string))))
  ;; Insert drawer right after headline if there is no planning line,
  ;; or after it otherwise.
  (should
   (equal "* H\n:PROPERTIES:\n:END:\nParagraph"
	  (org-test-with-temp-text "* H\nParagraph<point>"
	    (let ((org-adapt-indentation nil)) (org-insert-property-drawer))
	    (buffer-string))))
  (should
   (equal "* H\nDEADLINE: <2014-03-04 tue.>\n:PROPERTIES:\n:END:\nParagraph"
	  (org-test-with-temp-text
	      "* H\nDEADLINE: <2014-03-04 tue.>\nParagraph<point>"
	    (let ((org-adapt-indentation nil)) (org-insert-property-drawer))
	    (buffer-string))))
  ;; Indent inserted drawer.
  (should
   (equal "* H\n  :PROPERTIES:\n  :END:\nParagraph"
	  (org-test-with-temp-text "* H\nParagraph<point>"
	    (let ((org-adapt-indentation t)) (org-insert-property-drawer))
	    (buffer-string))))
  ;; Handle insertion at eob.
  (should
   (equal "* H\n:PROPERTIES:\n:END:\n"
	  (org-test-with-temp-text "* H"
	    (let ((org-adapt-indentation nil)) (org-insert-property-drawer))
	    (buffer-string))))
  ;; Skip inlinetasks before point.
  (when (featurep 'org-inlinetask)
    (should
     (equal "* H\n:PROPERTIES:\n:END:\n*************** I\n*************** END\nP"
	    (org-test-with-temp-text
		"* H\n*************** I\n*************** END\nP<point>"
	      (let ((org-adapt-indentation nil)
		    (org-inlinetask-min-level 15))
		(org-insert-property-drawer))
	      (buffer-string)))))
  ;; Correctly set drawer in an inlinetask.
  (when (featurep 'org-inlinetask)
    (should
     (equal "* H\n*************** I\n:PROPERTIES:\n:END:\nP\n*************** END"
	    (org-test-with-temp-text
		"* H\n*************** I\nP<point>\n*************** END"
	      (let ((org-adapt-indentation nil)
		    (org-inlinetask-min-level 15))
		(org-insert-property-drawer))
	      (buffer-string))))))


;;; Filling

(ert-deftest test-org/fill-element ()
  "Test `org-fill-element' specifications."
  ;; At an Org table, align it.
  (should
   (equal "| a |\n"
	  (org-test-with-temp-text "|a|"
	    (org-fill-element)
	    (buffer-string))))
  (should
   (equal "#+name: table\n| a |\n"
	  (org-test-with-temp-text "#+name: table\n| a |\n"
	    (org-fill-element)
	    (buffer-string))))
  ;; At a paragraph, preserve line breaks.
  (org-test-with-temp-text "some \\\\\nlong\ntext"
    (let ((fill-column 20))
      (org-fill-element)
      (should (equal (buffer-string) "some \\\\\nlong text"))))
  ;; Correctly fill a paragraph when point is at its very end.
  (should
   (equal "A B"
	  (org-test-with-temp-text "A\nB"
	    (let ((fill-column 20))
	      (goto-char (point-max))
	      (org-fill-element)
	      (buffer-string)))))
  ;; Correctly fill the last paragraph of a greater element.
  (should
   (equal "#+BEGIN_CENTER\n- 012345\n  789\n#+END_CENTER"
	  (org-test-with-temp-text "#+BEGIN_CENTER\n- 012345 789\n#+END_CENTER"
	    (let ((fill-column 8))
	      (forward-line)
	      (end-of-line)
	      (org-fill-element)
	      (buffer-string)))))
  ;; Correctly fill an element in a narrowed buffer.
  (should
   (equal "01234\n6"
	  (org-test-with-temp-text "01234 6789"
	    (let ((fill-column 5))
	      (narrow-to-region 1 8)
	      (org-fill-element)
	      (buffer-string)))))
  ;; Handle `adaptive-fill-regexp' in paragraphs.
  (should
   (equal "> a b"
	  (org-test-with-temp-text "> a\n> b"
	    (let ((fill-column 5)
		  (adaptive-fill-regexp "[ \t]*>+[ \t]*"))
	      (org-fill-element)
	      (buffer-string)))))
  ;; Special case: Fill first paragraph when point is at an item or
  ;; a plain-list or a footnote reference.
  (should
   (equal "- A B"
	  (org-test-with-temp-text "- A\n  B"
	    (let ((fill-column 20))
	      (org-fill-element)
	      (buffer-string)))))
  (should
   (equal "[fn:1] A B"
	  (org-test-with-temp-text "[fn:1] A\nB"
	    (let ((fill-column 20))
	      (org-fill-element)
	      (buffer-string)))))
  (org-test-with-temp-text "#+BEGIN_VERSE\nSome \\\\\nlong\ntext\n#+END_VERSE"
    (let ((fill-column 20))
      (org-fill-element)
      (should (equal (buffer-string)
		     "#+BEGIN_VERSE\nSome \\\\\nlong\ntext\n#+END_VERSE"))))
  ;; Fill contents of `comment-block' elements.
  (should
   (equal
    (org-test-with-temp-text "#+BEGIN_COMMENT\nSome\ntext\n#+END_COMMENT"
      (let ((fill-column 20))
	(forward-line)
	(org-fill-element)
	(buffer-string)))
    "#+BEGIN_COMMENT\nSome text\n#+END_COMMENT"))
  ;; Fill `comment' elements.
  (should
   (equal "  # A B"
	  (org-test-with-temp-text "  # A\n  # B"
	    (let ((fill-column 20))
	      (org-fill-element)
	      (buffer-string)))))
  ;; Do not mix consecutive comments when filling one of them.
  (should
   (equal "# A B\n\n# C"
	  (org-test-with-temp-text "# A\n# B\n\n# C"
	    (let ((fill-column 20))
	      (org-fill-element)
	      (buffer-string)))))
  ;; Use commented empty lines as separators when filling comments.
  (should
   (equal "# A B\n#\n# C"
	  (org-test-with-temp-text "# A\n# B\n#\n# C"
	    (let ((fill-column 20))
	      (org-fill-element)
	      (buffer-string)))))
  ;; Handle `adaptive-fill-regexp' in comments.
  (should
   (equal "# > a b"
	  (org-test-with-temp-text "# > a\n# > b"
	    (let ((fill-column 20)
		  (adaptive-fill-regexp "[ \t]*>+[ \t]*"))
	      (org-fill-element)
	      (buffer-string)))))
  ;; Do nothing at affiliated keywords.
  (should
   (equal "#+NAME: para\nSome\ntext."
	  (org-test-with-temp-text "#+NAME: para\nSome\ntext."
	    (let ((fill-column 20))
	      (org-fill-element)
	      (buffer-string)))))
  ;; Do not move point after table when filling a table.
  (should-not
   (org-test-with-temp-text "| a | b |\n| c | d |\n"
     (forward-char)
     (org-fill-element)
     (eobp)))
  ;; Do not fill "n" macro, with or without arguments, followed by
  ;; a dot or a closing parenthesis since it could be confused with
  ;; a numbered bullet.
  (should-not
   (equal "123456789\n{{{n}}}."
	  (org-test-with-temp-text "123456789 {{{n}}}."
	    (let ((fill-column 10))
	      (org-fill-element)
	      (buffer-string)))))
  (should-not
   (equal "123456789\n{{{n}}}\)"
	  (org-test-with-temp-text "123456789 {{{n}}}\)"
	    (let ((fill-column 10))
	      (org-fill-element)
	      (buffer-string)))))
  (should-not
   (equal "123456789\n{{{n()}}}."
	  (org-test-with-temp-text "123456789 {{{n()}}}."
	    (let ((fill-column 10))
	      (org-fill-element)
	      (buffer-string)))))
  (should-not
   (equal "123456789\n{{{n(counter)}}}."
	  (org-test-with-temp-text "123456789 {{{n(counter)}}}."
	    (let ((fill-column 10))
	      (org-fill-element)
	      (buffer-string))))))

(ert-deftest test-org/fill-paragraph ()
  "Test `org-fill-paragraph' specifications."
  ;; Regular test.
  (should
   (equal "012345678\n9"
	  (org-test-with-temp-text "012345678 9"
	    (let ((fill-column 10))
	      (org-fill-paragraph)
	      (buffer-string)))))
  ;; Fill paragraph even at end of buffer.
  (should
   (equal "012345678\n9\n"
	  (org-test-with-temp-text "012345678 9\n<point>"
	    (let ((fill-column 10))
	      (org-fill-paragraph)
	      (buffer-string)))))
  ;; Between two paragraphs, fill the next one.
  (should
   (equal "012345678 9\n\n012345678\n9"
	  (org-test-with-temp-text "012345678 9\n<point>\n012345678 9"
	    (let ((fill-column 10))
	      (org-fill-paragraph)
	      (buffer-string)))))
  (should
   (equal "012345678\n9\n\n012345678 9"
	  (org-test-with-temp-text "012345678 9<point>\n\n012345678 9"
	    (let ((fill-column 10))
	      (org-fill-paragraph)
	      (buffer-string)))))
  ;; Fill paragraph in a comment block.
  (should
   (equal "#+begin_comment\n012345678\n9\n#+end_comment"
	  (org-test-with-temp-text
	      "#+begin_comment\n<point>012345678 9\n#+end_comment"
	    (let ((fill-column 10))
	      (org-fill-paragraph)
	      (buffer-string)))))
  ;; When a region is selected, fill every paragraph in the region.
  (should
   (equal "012345678\n9\n\n012345678\n9"
	  (org-test-with-temp-text "012345678 9\n\n012345678 9"
	    (let ((fill-column 10))
	      (transient-mark-mode 1)
	      (push-mark (point-min) t t)
	      (goto-char (point-max))
	      (call-interactively #'org-fill-paragraph)
	      (buffer-string)))))
  (should
   (equal "012345678\n9\n\n012345678 9"
	  (org-test-with-temp-text "012345678 9\n<point>\n012345678 9"
	    (let ((fill-column 10))
	      (transient-mark-mode 1)
	      (push-mark (point) t t)
	      (goto-char (point-min))
	      (call-interactively #'org-fill-paragraph)
	      (buffer-string)))))
  (should
   (equal "012345678 9\n\n012345678\n9"
	  (org-test-with-temp-text "012345678 9\n<point>\n012345678 9"
	    (let ((fill-column 10))
	      (transient-mark-mode 1)
	      (push-mark (point) t t)
	      (goto-char (point-max))
	      (call-interactively #'org-fill-paragraph)
	      (buffer-string))))))

(ert-deftest test-org/auto-fill-function ()
  "Test auto-filling features."
  ;; Auto fill paragraph.
  (should
   (equal "12345\n7890"
	  (org-test-with-temp-text "12345 7890"
	    (let ((fill-column 5))
	      (end-of-line)
	      (org-auto-fill-function)
	      (buffer-string)))))
  ;; Auto fill first paragraph in an item.
  (should
   (equal "- 12345\n  7890"
	  (org-test-with-temp-text "- 12345 7890"
	    (let ((fill-column 7))
	      (end-of-line)
	      (org-auto-fill-function)
	      (buffer-string)))))
  ;; Auto fill paragraph when `adaptive-fill-regexp' matches.
  (should
   (equal "> 12345\n  7890"
	  (org-test-with-temp-text "> 12345 7890"
	    (let ((fill-column 10)
		  (adaptive-fill-regexp "[ \t]*>+[ \t]*")
		  (adaptive-fill-first-line-regexp "\\`[ 	]*\\'"))
	      (end-of-line)
	      (org-auto-fill-function)
	      (buffer-string)))))
  (should
   (equal "> 12345\n> 12345\n> 7890"
	  (org-test-with-temp-text "> 12345\n> 12345 7890"
	    (let ((fill-column 10)
		  (adaptive-fill-regexp "[ \t]*>+[ \t]*"))
	      (goto-char (point-max))
	      (org-auto-fill-function)
	      (buffer-string)))))
  (should-not
   (equal " 12345\n *12345\n *12345"
	  (org-test-with-temp-text " 12345\n *12345 12345"
	    (let ((fill-column 10)
		  (adaptive-fill-regexp "[ \t]*>+[ \t]*"))
	      (goto-char (point-max))
	      (org-auto-fill-function)
	      (buffer-string)))))
  ;; Auto fill comments.
  (should
   (equal "  # 12345\n  # 7890"
	  (org-test-with-temp-text "  # 12345 7890"
	    (let ((fill-column 10))
	      (end-of-line)
	      (org-auto-fill-function)
	      (buffer-string)))))
  ;; A hash within a line isn't a comment.
  (should-not
   (equal "12345 # 7890\n# 1"
	  (org-test-with-temp-text "12345 # 7890 1"
	    (let ((fill-column 12))
	      (end-of-line)
	      (org-auto-fill-function)
	      (buffer-string)))))
  ;; Correctly interpret empty prefix.
  (should-not
   (equal "# a\n# b\nRegular\n# paragraph"
	  (org-test-with-temp-text "# a\n# b\nRegular paragraph"
	    (let ((fill-column 12))
	      (end-of-line 3)
	      (org-auto-fill-function)
	      (buffer-string)))))
  ;; Comment block: auto fill contents.
  (should
   (equal "#+BEGIN_COMMENT\n12345\n7890\n#+END_COMMENT"
	  (org-test-with-temp-text "#+BEGIN_COMMENT\n12345 7890\n#+END_COMMENT"
	    (let ((fill-column 5))
	      (forward-line)
	      (end-of-line)
	      (org-auto-fill-function)
	      (buffer-string)))))
  (should
   (equal "#+BEGIN_COMMENT\n12345\n7890\n#+END_COMMENT"
	  (org-test-with-temp-text "#+BEGIN_COMMENT\n12345 7890\n#+END_COMMENT"
	    (let ((fill-column 5))
	      (forward-line)
	      (end-of-line)
	      (org-auto-fill-function)
	      (buffer-string)))))
  ;; Do not fill if a new item could be created.
  (should-not
   (equal "12345\n- 90"
	  (org-test-with-temp-text "12345 - 90"
	    (let ((fill-column 5))
	      (end-of-line)
	      (org-auto-fill-function)
	      (buffer-string)))))
  ;; Do not fill if a line break could be introduced.
  (should-not
   (equal "123\\\\\n7890"
	  (org-test-with-temp-text "123\\\\ 7890"
	    (let ((fill-column 6))
	      (end-of-line)
	      (org-auto-fill-function)
	      (buffer-string)))))
  ;; Do not fill affiliated keywords.
  (should-not
   (equal "#+ATTR_LATEX: ABC\nDEFGHIJKL"
	  (org-test-with-temp-text "#+ATTR_LATEX: ABC DEFGHIJKL"
	    (let ((fill-column 20))
	      (end-of-line)
	      (org-auto-fill-function)
	      (buffer-string))))))



;;; Indentation

(ert-deftest test-org/indent-line ()
  "Test `org-indent-line' specifications."
  ;; Do not indent diary sexps, footnote definitions or headlines.
  (should
   (zerop
    (org-test-with-temp-text "%%(org-calendar-holiday)"
      (org-indent-line)
      (org-get-indentation))))
  (should
   (zerop
    (org-test-with-temp-text "[fn:1] fn"
      (let ((org-adapt-indentation t)) (org-indent-line))
      (org-get-indentation))))
  (should
   (zerop
    (org-test-with-temp-text "* H"
      (org-indent-line)
      (org-get-indentation))))
  ;; Do not indent before first headline.
  (should
   (zerop
    (org-test-with-temp-text ""
      (org-indent-line)
      (org-get-indentation))))
  ;; Indent according to headline level otherwise, unless
  ;; `org-adapt-indentation' is nil.
  (should
   (= 2
      (org-test-with-temp-text "* H\n<point>A"
	(let ((org-adapt-indentation t)) (org-indent-line))
	(org-get-indentation))))
  (should
   (= 2
      (org-test-with-temp-text "* H\n<point>\nA"
	(let ((org-adapt-indentation t)) (org-indent-line))
	(org-get-indentation))))
  (should
   (zerop
    (org-test-with-temp-text "* H\n<point>A"
      (let ((org-adapt-indentation nil)) (org-indent-line))
      (org-get-indentation))))
  ;; Indenting preserves point position.
  (should
   (org-test-with-temp-text "* H\nA<point>B"
     (let ((org-adapt-indentation t)) (org-indent-line))
     (looking-at "B")))
  ;; Do not change indentation at an item or a LaTeX environment.
  (should
   (= 1
      (org-test-with-temp-text "* H\n<point> - A"
	(let ((org-adapt-indentation t)) (org-indent-line))
	(org-get-indentation))))
  (should
   (= 1
      (org-test-with-temp-text
	  "\\begin{equation}\n <point>1+1=2\n\\end{equation}"
	(org-indent-line)
	(org-get-indentation))))
  ;; On blank lines at the end of a list, indent like last element
  ;; within it if the line is still in the list.  If the last element
  ;; is an item, indent like its contents.  Otherwise, indent like the
  ;; whole list.
  (should
   (= 4
      (org-test-with-temp-text "* H\n- A\n  - AA\n<point>"
	(let ((org-adapt-indentation t)) (org-indent-line))
	(org-get-indentation))))
  (should
   (= 4
      (org-test-with-temp-text "* H\n- A\n  -\n\n<point>"
	(let ((org-adapt-indentation t)) (org-indent-line))
	(org-get-indentation))))
  (should
   (zerop
    (org-test-with-temp-text "* H\n- A\n  - AA\n\n\n\n<point>"
      (let ((org-adapt-indentation t)) (org-indent-line))
      (org-get-indentation))))
  (should
   (= 4
      (org-test-with-temp-text "* H\n- A\n  - \n<point>"
	(let ((org-adapt-indentation t)) (org-indent-line))
	(org-get-indentation))))
  (should
   (= 4
      (org-test-with-temp-text
	  "* H\n  - \n    #+BEGIN_SRC emacs-lisp\n  t\n    #+END_SRC\n<point>"
	(let ((org-adapt-indentation t)) (org-indent-line))
	(org-get-indentation))))
  (should
   (= 2
      (org-test-with-temp-text "- A\n  B\n\n<point>"
	(let ((org-adapt-indentation nil)) (org-indent-line))
	(org-get-indentation))))
  (should
   (= 2
      (org-test-with-temp-text
	  "- A\n  \begin{cases}    1 + 1\n  \end{cases}\n\n<point>"
	(let ((org-adapt-indentation nil)) (org-indent-line))
	(org-get-indentation))))
  ;; Likewise, on a blank line at the end of a footnote definition,
  ;; indent at column 0 if line belongs to the definition.  Otherwise,
  ;; indent like the definition itself.
  (should
   (zerop
    (org-test-with-temp-text "* H\n[fn:1] Definition\n<point>"
      (let ((org-adapt-indentation t)) (org-indent-line))
      (org-get-indentation))))
  (should
   (zerop
    (org-test-with-temp-text "* H\n[fn:1] Definition\n\n\n\n<point>"
      (let ((org-adapt-indentation t)) (org-indent-line))
      (org-get-indentation))))
  ;; After the end of the contents of a greater element, indent like
  ;; the beginning of the element.
  (should
   (= 1
      (org-test-with-temp-text
	  " #+BEGIN_CENTER\n  Contents\n<point>#+END_CENTER"
	(org-indent-line)
	(org-get-indentation))))
  ;; On blank lines after a paragraph, indent like its last non-empty
  ;; line.
  (should
   (= 1
      (org-test-with-temp-text " Paragraph\n\n<point>"
	(org-indent-line)
	(org-get-indentation))))
  ;; At the first line of an element, indent like previous element's
  ;; first line, ignoring footnotes definitions and inline tasks, or
  ;; according to parent.
  (should
   (= 2
      (org-test-with-temp-text "A\n\n  B\n\nC<point>"
	(org-indent-line)
	(org-get-indentation))))
  (should
   (= 1
      (org-test-with-temp-text " A\n\n[fn:1] B\n\n\nC<point>"
	(org-indent-line)
	(org-get-indentation))))
  (should
   (= 1
      (org-test-with-temp-text
	  " #+BEGIN_CENTER\n<point>  Contents\n#+END_CENTER"
	(org-indent-line)
	(org-get-indentation))))
  ;; Within code part of a source block, use language major mode if
  ;; `org-src-tab-acts-natively' is non-nil.  Otherwise, indent
  ;; according to line above.
  (should
   (= 6
      (org-test-with-temp-text
	  "#+BEGIN_SRC emacs-lisp\n (and A\n<point>B)\n#+END_SRC"
	(let ((org-src-tab-acts-natively t)
	      (org-edit-src-content-indentation 0))
	  (org-indent-line))
	(org-get-indentation))))
  (should
   (= 1
      (org-test-with-temp-text
	  "#+BEGIN_SRC emacs-lisp\n (and A\n<point>B)\n#+END_SRC"
	(let ((org-src-tab-acts-natively nil)
	      (org-edit-src-content-indentation 0))
	  (org-indent-line))
	(org-get-indentation))))
  ;; Otherwise, indent like the first non-blank line above.
  (should
   (zerop
    (org-test-with-temp-text
	"#+BEGIN_CENTER\nline1\n\n<point>  line2\n#+END_CENTER"
      (org-indent-line)
      (org-get-indentation))))
  ;; Align node properties according to `org-property-format'.  Handle
  ;; nicely empty values.
  (should
   (equal "* H\n:PROPERTIES:\n:key:      value\n:END:"
	  (org-test-with-temp-text
	      "* H\n:PROPERTIES:\n<point>:key: value\n:END:"
	    (let ((org-property-format "%-10s %s")) (org-indent-line))
	    (buffer-string))))
  (should
   (equal "* H\n:PROPERTIES:\n:key:\n:END:"
	  (org-test-with-temp-text "* H\n:PROPERTIES:\n<point>:key:\n:END:"
	    (let ((org-property-format "%-10s %s")) (org-indent-line))
	    (buffer-string)))))

(ert-deftest test-org/indent-region ()
  "Test `org-indent-region' specifications."
  ;; Indent paragraph.
  (should
   (equal "A\nB\nC"
	  (org-test-with-temp-text " A\nB\n  C"
	    (org-indent-region (point-min) (point-max))
	    (buffer-string))))
  ;; Indent greater elements along with their contents.
  (should
   (equal "#+BEGIN_CENTER\nA\nB\n#+END_CENTER"
	  (org-test-with-temp-text "#+BEGIN_CENTER\n A\n  B\n#+END_CENTER"
	    (org-indent-region (point-min) (point-max))
	    (buffer-string))))
  ;; Ignore contents of verse blocks.  Only indent block delimiters.
  (should
   (equal "#+BEGIN_VERSE\n A\n  B\n#+END_VERSE"
	  (org-test-with-temp-text "#+BEGIN_VERSE\n A\n  B\n#+END_VERSE"
	    (org-indent-region (point-min) (point-max))
	    (buffer-string))))
  (should
   (equal "#+BEGIN_VERSE\n  A\n   B\n#+END_VERSE"
	  (org-test-with-temp-text " #+BEGIN_VERSE\n  A\n   B\n #+END_VERSE"
	    (org-indent-region (point-min) (point-max))
	    (buffer-string))))
  ;; Indent example blocks as a single block, unless indentation
  ;; should be preserved.  In this case only indent the block markers.
  (should
   (equal "#+BEGIN_EXAMPLE\n A\n  B\n#+END_EXAMPLE"
	  (org-test-with-temp-text "#+BEGIN_EXAMPLE\n A\n  B\n#+END_EXAMPLE"
	    (org-indent-region (point-min) (point-max))
	    (buffer-string))))
  (should
   (equal "#+BEGIN_EXAMPLE\n A\n  B\n#+END_EXAMPLE"
	  (org-test-with-temp-text " #+BEGIN_EXAMPLE\n  A\n   B\n #+END_EXAMPLE"
	    (org-indent-region (point-min) (point-max))
	    (buffer-string))))
  (should
   (equal "#+BEGIN_EXAMPLE -i\n  A\n   B\n#+END_EXAMPLE"
	  (org-test-with-temp-text
	      " #+BEGIN_EXAMPLE -i\n  A\n   B\n #+END_EXAMPLE"
	    (org-indent-region (point-min) (point-max))
	    (buffer-string))))
  (should
   (equal "#+BEGIN_EXAMPLE\n  A\n   B\n#+END_EXAMPLE"
	  (org-test-with-temp-text
	      " #+BEGIN_EXAMPLE\n  A\n   B\n #+END_EXAMPLE"
	    (let ((org-src-preserve-indentation t))
	      (org-indent-region (point-min) (point-max)))
	    (buffer-string))))
  ;; Treat export blocks as a whole.
  (should
   (equal "#+BEGIN_EXPORT latex\n A\n  B\n#+END_EXPORT"
	  (org-test-with-temp-text "#+BEGIN_EXPORT latex\n A\n  B\n#+END_EXPORT"
	    (org-indent-region (point-min) (point-max))
	    (buffer-string))))
  (should
   (equal "#+BEGIN_EXPORT latex\n A\n  B\n#+END_EXPORT"
	  (org-test-with-temp-text
	      " #+BEGIN_EXPORT latex\n  A\n   B\n #+END_EXPORT"
	    (org-indent-region (point-min) (point-max))
	    (buffer-string))))
  ;; Indent according to mode if `org-src-tab-acts-natively' is
  ;; non-nil.  Otherwise, do not indent code at all.
  (should
   (equal "#+BEGIN_SRC emacs-lisp\n(and A\n     B)\n#+END_SRC"
	  (org-test-with-temp-text
	      "#+BEGIN_SRC emacs-lisp\n (and A\nB)\n#+END_SRC"
	    (let ((org-src-tab-acts-natively t)
		  (org-edit-src-content-indentation 0))
	      (org-indent-region (point-min) (point-max)))
	    (buffer-string))))
  (should
   (equal "#+BEGIN_SRC emacs-lisp\n (and A\nB)\n#+END_SRC"
	  (org-test-with-temp-text
	      "#+BEGIN_SRC emacs-lisp\n (and A\nB)\n#+END_SRC"
	    (let ((org-src-tab-acts-natively nil)
		  (org-edit-src-content-indentation 0))
	      (org-indent-region (point-min) (point-max)))
	    (buffer-string))))
  ;; Align node properties according to `org-property-format'.  Handle
  ;; nicely empty values.
  (should
   (equal "* H\n:PROPERTIES:\n:key:      value\n:END:"
	  (org-test-with-temp-text "* H\n<point>:PROPERTIES:\n:key: value\n:END:"
	    (let ((org-property-format "%-10s %s")
		  (org-adapt-indentation nil))
	      (org-indent-region (point) (point-max)))
	    (buffer-string))))
  (should
   (equal "* H\n:PROPERTIES:\n:key:\n:END:"
	  (org-test-with-temp-text "* H\n<point>:PROPERTIES:\n:key:\n:END:"
	    (let ((org-property-format "%-10s %s")
		  (org-adapt-indentation nil))
	      (org-indent-region (point) (point-max)))
	    (buffer-string))))
  ;; Indent plain lists.
  (should
   (equal "- A\n  B\n  - C\n\n    D"
	  (org-test-with-temp-text "- A\n   B\n  - C\n\n     D"
	    (org-indent-region (point-min) (point-max))
	    (buffer-string))))
  (should
   (equal "- A\n\n- B"
	  (org-test-with-temp-text " - A\n\n - B"
	    (org-indent-region (point-min) (point-max))
	    (buffer-string))))
  ;; Indent footnote definitions.
  (should
   (equal "[fn:1] Definition\n\nDefinition"
	  (org-test-with-temp-text "[fn:1] Definition\n\n  Definition"
	    (org-indent-region (point-min) (point-max))
	    (buffer-string))))
  ;; Special case: Start indenting on a blank line.
  (should
   (equal "\nParagraph"
	  (org-test-with-temp-text "\n  Paragraph"
	    (org-indent-region (point-min) (point-max))
	    (buffer-string)))))



;;; Editing

(ert-deftest test-org/delete-indentation ()
  "Test `org-delete-indentation' specifications."
  ;; Regular test.
  (should (equal "foo bar"
		(org-test-with-temp-text
		    "foo \n bar<point>"
		  (org-delete-indentation)
		  (buffer-string))))
  ;; With optional argument.
  (should (equal "foo bar"
		(org-test-with-temp-text
		    "foo<point> \n bar"
		  (org-delete-indentation t)
		  (buffer-string))))
  ;; At headline text should be appended to the headline text.
  (should
   (equal"* foo bar :tag:"
	 (let (org-auto-align-tags)
	   (org-test-with-temp-text
	       "* foo :tag:\n bar<point>"
	     (org-delete-indentation)
	     (buffer-string)))))
  (should
   (equal "* foo bar :tag:"
	  (let (org-auto-align-tags)
	    (org-test-with-temp-text
		"* foo <point>:tag:\n bar"
	      (org-delete-indentation t)
	      (buffer-string))))))

(ert-deftest test-org/return ()
  "Test `org-return' specifications."
  ;; Regular test.
  (should
   (equal "Para\ngraph"
	  (org-test-with-temp-text "Para<point>graph"
	    (org-return)
	    (buffer-string))))
  ;; With optional argument, indent line.
  (should
   (equal "  Para\n  graph"
	  (org-test-with-temp-text "  Para<point>graph"
	    (org-return t)
	    (buffer-string))))
  ;; On a table, call `org-table-next-row'.
  (should
   (org-test-with-temp-text "| <point>a |\n| b |"
     (org-return)
     (looking-at-p "b")))
  ;; Open link or timestamp under point when `org-return-follows-link'
  ;; is non-nil.
  (should
   (org-test-with-temp-text "Link [[target<point>]] <<target>>"
     (let ((org-return-follows-link t)
	   (org-link-search-must-match-exact-headline nil))
       (org-return))
     (looking-at-p "<<target>>")))
  (should-not
   (org-test-with-temp-text "Link [[target<point>]] <<target>>"
     (let ((org-return-follows-link nil)) (org-return))
     (looking-at-p "<<target>>")))
  (should
   (org-test-with-temp-text "* [[b][a<point>]]\n* b"
     (let ((org-return-follows-link t)) (org-return))
     (looking-at-p "* b")))
  (should
   (org-test-with-temp-text "Link [[target][/descipt<point>ion/]] <<target>>"
     (let ((org-return-follows-link t)
	   (org-link-search-must-match-exact-headline nil))
       (org-return))
     (looking-at-p "<<target>>")))
  (should-not
   (org-test-with-temp-text "Link [[target]]<point> <<target>>"
     (let ((org-return-follows-link t)
	   (org-link-search-must-match-exact-headline nil))
       (org-return))
     (looking-at-p "<<target>>")))
  ;; When `org-return-follows-link' is non-nil, tolerate links and
  ;; timestamps in comments, node properties, etc.
  (should
   (org-test-with-temp-text "# Comment [[target<point>]]\n <<target>>"
     (let ((org-return-follows-link t)
	   (org-link-search-must-match-exact-headline nil))
       (org-return))
     (looking-at-p "<<target>>")))
  (should-not
   (org-test-with-temp-text "# Comment [[target<point>]]\n <<target>>"
     (let ((org-return-follows-link nil)) (org-return))
     (looking-at-p "<<target>>")))
  (should-not
   (org-test-with-temp-text "# Comment [[target]]<point>\n <<target>>"
     (let ((org-return-follows-link t)
	   (org-link-search-must-match-exact-headline nil))
       (org-return))
     (looking-at-p "<<target>>")))
  ;; Non-nil `org-return-follows-link' ignores read-only state of
  ;; a buffer.
  (should
   (org-test-with-temp-text "Link [[target<point>]] <<target>>"
     (let ((org-return-follows-link t)
	   (org-link-search-must-match-exact-headline nil))
       (setq buffer-read-only t)
       (call-interactively #'org-return))
     (looking-at-p "<<target>>")))
  ;; `org-return-follows-link' handle multi-line lines.
  (should
   (org-test-with-temp-text
       "[[target][This is a very\n long description<point>]]\n <<target>>"
     (let ((org-return-follows-link t)
	   (org-link-search-must-match-exact-headline nil))
       (org-return))
     (looking-at-p "<<target>>")))
  (should-not
   (org-test-with-temp-text
       "[[target][This is a very\n long description]]<point>\n <<target>>"
     (let ((org-return-follows-link t)
	   (org-link-search-must-match-exact-headline nil))
       (org-return))
     (looking-at-p "<<target>>")))
  ;; However, do not open link when point is in a table.
  (should
   (org-test-with-temp-text "| [[target<point>]] |\n| between |\n| <<target>> |"
     (let ((org-return-follows-link t)) (org-return))
     (looking-at-p "between")))
  ;; Special case: in a list, when indenting, do not break structure.
  (should
   (equal "- A\n  B"
	  (org-test-with-temp-text "- A <point>B"
	    (org-return t)
	    (buffer-string))))
  (should
   (equal "- A\n\n- B"
	  (org-test-with-temp-text "- A\n<point>- B"
	    (org-return t)
	    (buffer-string))))
  ;; On tags part of a headline, add a newline below it instead of
  ;; breaking it.
  (should
   (equal "* H :tag:\n"
	  (org-test-with-temp-text "* H :<point>tag:"
	    (org-return)
	    (buffer-string))))
  ;; Before headline text, add a newline below it instead of breaking
  ;; it.
  (should
   (equal "* TODO H :tag:\n"
	  (org-test-with-temp-text "* <point>TODO H :tag:"
	    (org-return)
	    (buffer-string))))
  (should
   (equal "* TODO [#B] H :tag:\n"
	  (org-test-with-temp-text "* TODO<point> [#B] H :tag:"
	    (org-return)
	    (buffer-string))))
  (should				;TODO are case-sensitive
   (equal "* \nTodo"
	  (org-test-with-temp-text "* <point>Todo"
	    (org-return)
	    (buffer-string))))
  ;; At headline text, break headline text but preserve tags.
  (should
   (equal "* TODO [#B] foo    :tag:\nbar"
	  (let (org-auto-align-tags)
	    (org-test-with-temp-text "* TODO [#B] foo<point>bar :tag:"
	      (org-return)
	      (buffer-string)))))
  ;; At bol of headline insert newline.
  (should
   (equal "\n* h"
	  (org-test-with-temp-text "<point>* h"
	    (org-return)
	    (buffer-string))))
  ;; Refuse to leave invalid headline in buffer.
  (should
   (equal "* h\n"
	  (org-test-with-temp-text "*<point> h"
	    (org-return)
	    (buffer-string))))
  ;; Before first column or after last one in a table, split the
  ;; table.
  (should
   (equal "| a |\n\n| b |"
	  (org-test-with-temp-text "| a |\n<point>| b |"
	    (org-return)
	    (buffer-string))))
  (should
   (equal "| a |\n\n| b |"
	  (org-test-with-temp-text "| a |<point>\n| b |"
	    (org-return)
	    (buffer-string))))
  ;; Do not auto-fill on hitting <RET> inside a property drawer.
  (should
   (equal "* Heading\n:PROPERTIES:\n:SOME_PROP: This is a very long property value that goes beyond the fill-column. But this is inside a property drawer, so the auto-filling should be disabled.\n\n:END:"
	  (org-test-with-temp-text "* Heading\n:PROPERTIES:\n:SOME_PROP: This is a very long property value that goes beyond the fill-column. But this is inside a property drawer, so the auto-filling should be disabled.<point>\n:END:"
	    (setq-local fill-column 10)
	    (auto-fill-mode 1)
	    (org-return)
	    (buffer-string)))))

(ert-deftest test-org/with-electric-indent ()
  "Test RET and C-j specifications with `electric-indent-mode' on."
  ;; Call commands interactively, since this is how `newline' knows it
  ;; must run `post-self-insert-hook'.
  ;;
  ;; RET, like `newline', should indent.
  (should
   (equal "  Para\n  graph"
	  (org-test-with-temp-text "  Para<point>graph"
	    (electric-indent-local-mode 1)
	    (call-interactively 'org-return)
	    (buffer-string))))
  (should
   (equal "- item1\n  item2"
	  (org-test-with-temp-text "- item1<point>item2"
	    (electric-indent-local-mode 1)
	    (call-interactively 'org-return)
	    (buffer-string))))
  (should
   (equal "* heading\n  body"
	  (org-test-with-temp-text "* heading<point>body"
	    (electric-indent-local-mode 1)
	    (call-interactively 'org-return)
	    (buffer-string))))
  ;; C-j, like `electric-newline-and-maybe-indent', should not indent.
  (should
   (equal "  Para\ngraph"
	  (org-test-with-temp-text "  Para<point>graph"
	    (electric-indent-local-mode 1)
	    (call-interactively 'org-return-and-maybe-indent)
	    (buffer-string))))
  (should
   (equal "- item1\nitem2"
	  (org-test-with-temp-text "- item1<point>item2"
	    (electric-indent-local-mode 1)
	    (call-interactively 'org-return-and-maybe-indent)
	    (buffer-string))))
  (should
   (equal "* heading\nbody"
	  (org-test-with-temp-text "* heading<point>body"
	    (electric-indent-local-mode 1)
	    (call-interactively 'org-return-and-maybe-indent)
	    (buffer-string)))))

(ert-deftest test-org/without-electric-indent ()
  "Test RET and C-j specifications with `electric-indent-mode' off."
  ;; Call commands interactively, since this is how `newline' knows it
  ;; must run `post-self-insert-hook'.
  ;;
  ;; RET, like `newline', should not indent.
  (should
   (equal "  Para\ngraph"
	  (org-test-with-temp-text "  Para<point>graph"
	    (electric-indent-local-mode 0)
	    (call-interactively 'org-return)
	    (buffer-string))))
  (should
   (equal "- item1\nitem2"
	  (org-test-with-temp-text "- item1<point>item2"
	    (electric-indent-local-mode 0)
	    (call-interactively 'org-return)
	    (buffer-string))))
  (should
   (equal "* heading\nbody"
	  (org-test-with-temp-text "* heading<point>body"
	    (electric-indent-local-mode 0)
	    (call-interactively 'org-return)
	    (buffer-string))))
  ;; C-j, like `electric-newline-and-maybe-indent', should indent.
  (should
   (equal "  Para\n  graph"
	  (org-test-with-temp-text "  Para<point>graph"
	    (electric-indent-local-mode 0)
	    (call-interactively 'org-return-and-maybe-indent)
	    (buffer-string))))
  (should
   (equal "- item1\n  item2"
	  (org-test-with-temp-text "- item1<point>item2"
	    (electric-indent-local-mode 0)
	    (call-interactively 'org-return-and-maybe-indent)
	    (buffer-string))))
  (should
   (equal "* heading\n  body"
	  (org-test-with-temp-text "* heading<point>body"
	    (electric-indent-local-mode 0)
	    (call-interactively 'org-return-and-maybe-indent)
	    (buffer-string)))))

(ert-deftest test-org/meta-return ()
  "Test M-RET (`org-meta-return') specifications."
  ;; In a table field insert a row above.
  (should
   (org-test-with-temp-text "| a |"
     (forward-char)
     (org-meta-return)
     (forward-line -1)
     (looking-at "|   |$")))
  ;; In a paragraph change current line into a header.
  (should
   (org-test-with-temp-text "a"
     (org-meta-return)
     (beginning-of-line)
     (looking-at "\* a$")))
  ;; In an item insert an item, in this case above.
  (should
   (org-test-with-temp-text "- a"
     (org-meta-return)
     (beginning-of-line)
     (looking-at "- $")))
  ;; In a drawer and item insert an item, in this case above.
  (should
   (org-test-with-temp-text ":MYDRAWER:\n- a\n:END:"
     (forward-line)
     (org-meta-return)
     (beginning-of-line)
     (looking-at "- $"))))

(ert-deftest test-org/insert-heading ()
  "Test `org-insert-heading' specifications."
  ;; In an empty buffer, insert a new headline.
  (should
   (equal "* "
	  (org-test-with-temp-text ""
	    (org-insert-heading)
	    (buffer-string))))
  ;; At the beginning of a line, turn it into a headline.
  (should
   (equal "* P"
	  (org-test-with-temp-text "<point>P"
	    (org-insert-heading)
	    (buffer-string))))
  ;; In the middle of a line, split the line if allowed, otherwise,
  ;; insert the headline at its end.
  (should
   (equal "Para\n* graph"
	  (org-test-with-temp-text "Para<point>graph"
	    (let ((org-M-RET-may-split-line '((default . t))))
	      (org-insert-heading))
	    (buffer-string))))
  (should
   (equal "Paragraph\n* "
	  (org-test-with-temp-text "Para<point>graph"
	    (let ((org-M-RET-may-split-line '((default . nil))))
	      (org-insert-heading))
	    (buffer-string))))
  ;; At the beginning of a headline, create one above.
  (should
   (equal "* \n* H"
	  (org-test-with-temp-text "* H"
	    (org-insert-heading)
	    (buffer-string))))
  ;; In the middle of a headline, split it if allowed.
  (should
   (equal "* H\n* 1"
	  (org-test-with-temp-text "* H<point>1"
	    (let ((org-M-RET-may-split-line '((headline . t))))
	      (org-insert-heading))
	    (buffer-string))))
  (should
   (equal "* H1\n* "
	  (org-test-with-temp-text "* H<point>1"
	    (let ((org-M-RET-may-split-line '((headline . nil))))
	      (org-insert-heading))
	    (buffer-string))))
  ;; However, splitting cannot happen on TODO keywords, priorities or
  ;; tags.
  (should
   (equal "* TODO H1\n* "
	  (org-test-with-temp-text "* TO<point>DO H1"
	    (let ((org-M-RET-may-split-line '((headline . t))))
	      (org-insert-heading))
	    (buffer-string))))
  (should
   (equal "* [#A] H1\n* "
	  (org-test-with-temp-text "* [#<point>A] H1"
	    (let ((org-M-RET-may-split-line '((headline . t))))
	      (org-insert-heading))
	    (buffer-string))))
  (should
   (equal "* H1 :tag:\n* "
	  (org-test-with-temp-text "* H1 :ta<point>g:"
	    (let ((org-M-RET-may-split-line '((headline . t))))
	      (org-insert-heading))
	    (buffer-string))))
  ;; New headline level depends on the level of the headline above.
  (should
   (equal "** H\n** P"
	  (org-test-with-temp-text "** H\n<point>P"
	    (org-insert-heading)
	    (buffer-string))))
  (should
   (equal "** H\nPara\n** graph"
	  (org-test-with-temp-text "** H\nPara<point>graph"
	    (let ((org-M-RET-may-split-line '((default . t))))
	      (org-insert-heading))
	    (buffer-string))))
  (should
   (equal "** \n** H"
	  (org-test-with-temp-text "** H"
	    (org-insert-heading)
	    (buffer-string))))
  ;; When called with one universal argument, insert a new headline at
  ;; the end of the current subtree, independently on the position of
  ;; point.
  (should
   (equal
    "* H1\n** H2\n* "
    (org-test-with-temp-text "* H1\n** H2"
      (let ((org-insert-heading-respect-content nil))
	(org-insert-heading '(4)))
      (buffer-string))))
  (should
   (equal
    "* H1\n** H2\n* "
    (org-test-with-temp-text "* H<point>1\n** H2"
      (let ((org-insert-heading-respect-content nil))
	(org-insert-heading '(4)))
      (buffer-string))))
  ;; When called with two universal arguments, insert a new headline
  ;; at the end of the grandparent subtree.
  (should
   (equal "* H1\n** H3\n- item\n** H2\n** "
	  (org-test-with-temp-text "* H1\n** H3\n- item<point>\n** H2"
	    (let ((org-insert-heading-respect-content nil))
	      (org-insert-heading '(16)))
	    (buffer-string))))
  ;; When optional TOP-LEVEL argument is non-nil, always insert
  ;; a level 1 heading.
  (should
   (equal "* H1\n** H2\n* "
	  (org-test-with-temp-text "* H1\n** H2<point>"
	    (org-insert-heading nil nil t)
	    (buffer-string))))
  (should
   (equal "* H1\n- item\n* "
	  (org-test-with-temp-text "* H1\n- item<point>"
	    (org-insert-heading nil nil t)
	    (buffer-string))))
  ;; Obey `org-blank-before-new-entry'.
  (should
   (equal "* H1\n\n* "
	  (org-test-with-temp-text "* H1<point>"
	    (let ((org-blank-before-new-entry '((heading . t))))
	      (org-insert-heading))
	    (buffer-string))))
  (should
   (equal "* H1\n* "
	  (org-test-with-temp-text "* H1<point>"
	    (let ((org-blank-before-new-entry '((heading . nil))))
	      (org-insert-heading))
	    (buffer-string))))
  (should
   (equal "* H1\n* H2\n* "
	  (org-test-with-temp-text "* H1\n* H2<point>"
	    (let ((org-blank-before-new-entry '((heading . auto))))
	      (org-insert-heading))
	    (buffer-string))))
  (should
   (equal "* H1\n\n* H2\n\n* "
	  (org-test-with-temp-text "* H1\n\n* H2<point>"
	    (let ((org-blank-before-new-entry '((heading . auto))))
	      (org-insert-heading))
	    (buffer-string))))
  ;; Corner case: correctly insert a headline after an empty one.
  (should
   (equal "* \n* "
	  (org-test-with-temp-text "* <point>"
	    (org-insert-heading)
	    (buffer-string))))
  (should
   (org-test-with-temp-text "* <point>\n"
     (org-insert-heading)
     (looking-at-p "\n\\'")))
  ;; Do not insert spurious headlines when inserting a new headline.
  (should
   (equal "* H1\n* H2\n* \n"
	  (org-test-with-temp-text "* H1\n* H2<point>\n"
	    (org-insert-heading)
	    (buffer-string))))
  ;; Preserve visibility at beginning of line.  In particular, when
  ;; removing spurious blank lines, do not visually merge heading with
  ;; the line visible above.
  (should-not
   (org-test-with-temp-text "* H1<point>\nContents\n\n* H2\n"
     (org-overview)
     (let ((org-blank-before-new-entry '((heading . nil))))
       (org-insert-heading '(4)))
     (invisible-p (line-end-position 0))))
  ;; Properly handle empty lines when forcing a headline below current
  ;; one.
  (should
   (equal "* H1\n\n* H\n\n* "
	  (org-test-with-temp-text "* H1\n\n* H<point>"
	    (let ((org-blank-before-new-entry '((heading . t))))
	      (org-insert-heading '(4))
	      (buffer-string))))))

(ert-deftest test-org/insert-todo-heading-respect-content ()
  "Test `org-insert-todo-heading-respect-content' specifications."
  ;; Create a TODO heading.
  (should
   (org-test-with-temp-text "* H1\n Body"
     (org-insert-todo-heading-respect-content)
     (nth 2 (org-heading-components))))
  ;; Add headline at the end of the first subtree
  (should
   (equal
    "* TODO "
    (org-test-with-temp-text "* H1\nH1Body<point>\n** H2\nH2Body"
      (org-insert-todo-heading-respect-content)
      (buffer-substring-no-properties (line-beginning-position) (point-max)))))
  ;; In a list, do not create a new item.
  (should
   (equal
    "* TODO "
    (org-test-with-temp-text "* H\n- an item\n- another one"
      (search-forward "an ")
      (org-insert-todo-heading-respect-content)
      (buffer-substring-no-properties (line-beginning-position) (point-max)))))
  ;; Use the same TODO keyword as current heading.
  (should
   (equal
    "* TODO \n"
    (org-test-with-temp-text "* TODO\n** WAITING\n"
      (org-insert-todo-heading-respect-content)
      (buffer-substring-no-properties (line-beginning-position) (point-max))))))

(ert-deftest test-org/clone-with-time-shift ()
  "Test `org-clone-subtree-with-time-shift'."
  ;; Raise an error before first heading.
  (should-error
   (org-test-with-temp-text ""
     (org-clone-subtree-with-time-shift 1)))
  ;; Raise an error on invalid number of clones.
  (should-error
   (org-test-with-temp-text "* Clone me"
     (org-clone-subtree-with-time-shift -1)))
  ;; Clone non-repeating once.
  (should
   (equal "\
* H1\n<2015-06-21>
* H1\n<2015-06-23>
"
	  (org-test-with-temp-text "* H1\n<2015-06-21 Sun>"
	    (org-clone-subtree-with-time-shift 1 "+2d")
	    (replace-regexp-in-string
	     "\\( [.A-Za-z]+\\)\\( \\+[0-9][hdmwy]\\)?>" "" (buffer-string)
	     nil nil 1))))
  ;; Clone repeating once.
  (should
   (equal "\
* H1\n<2015-06-21>
* H1\n<2015-06-23>
* H1\n<2015-06-25 +1w>
"
	  (org-test-with-temp-text "* H1\n<2015-06-21 Sun +1w>"
	    (org-clone-subtree-with-time-shift 1 "+2d")
	    (replace-regexp-in-string
	     "\\( [.A-Za-z]+\\)\\( \\+[0-9][hdmwy]\\)?>" "" (buffer-string)
	     nil nil 1))))
  ;; Clone repeating once in backward.
  (should
   (equal "\
* H1\n<2015-06-21>
* H1\n<2015-06-19>
* H1\n<2015-06-17 +1w>
"
	  (org-test-with-temp-text "* H1\n<2015-06-21 Sun +1w>"
	    (org-clone-subtree-with-time-shift 1 "-2d")
	    (replace-regexp-in-string
	     "\\( [.A-Za-z]+\\)\\( \\+[0-9][hdmwy]\\)?>" "" (buffer-string)
	     nil nil 1))))
  ;; Clone non-repeating zero times.
  (should
   (equal "\
* H1\n<2015-06-21>
"
	  (org-test-with-temp-text "* H1\n<2015-06-21 Sun>"
	    (org-clone-subtree-with-time-shift 0 "+2d")
	    (replace-regexp-in-string
	     "\\( [.A-Za-z]+\\)\\( \\+[0-9][hdmwy]\\)?>" "" (buffer-string)
	     nil nil 1))))
  ;; Clone repeating "zero" times.
  (should
   (equal "\
* H1\n<2015-06-21>
* H1\n<2015-06-23 +1w>
"
	  (org-test-with-temp-text "* H1\n<2015-06-21 Sun +1w>"
	    (org-clone-subtree-with-time-shift 0 "+2d")
	    (replace-regexp-in-string
	     "\\( [.A-Za-z]+\\)\\( \\+[0-9][hdmwy]\\)?>" "" (buffer-string)
	     nil nil 1))))
  ;; Clone with blank SHIFT argument.
  (should
   (string-prefix-p
    "* H <2012-03-29"
    (org-test-with-temp-text "* H <2012-03-29 Thu><point>"
      (org-clone-subtree-with-time-shift 1 "")
      (buffer-substring-no-properties (line-beginning-position 2)
				      (line-end-position 2)))))
  ;; Find time stamps before point.  If SHIFT is not specified, ask
  ;; for a time shift.
  (should
   (string-prefix-p
    "* H <2012-03-30"
    (org-test-with-temp-text "* H <2012-03-29 Thu><point>"
      (org-clone-subtree-with-time-shift 1 "+1d")
      (buffer-substring-no-properties (line-beginning-position 2)
				      (line-end-position 2)))))
  (should
   (string-prefix-p
    "* H <2014-03-05"
    (org-test-with-temp-text "* H <2014-03-04 Tue><point>"
      (cl-letf (((symbol-function 'read-from-minibuffer)
		 (lambda (&rest args) "+1d")))
	(org-clone-subtree-with-time-shift 1))
      (buffer-substring-no-properties (line-beginning-position 2)
				      (line-end-position 2))))))


;;; Fixed-Width Areas

(ert-deftest test-org/toggle-fixed-width ()
  "Test `org-toggle-fixed-width' specifications."
  ;; No region: Toggle on fixed-width marker in paragraphs.
  (should
   (equal ": A"
	  (org-test-with-temp-text "A"
	    (org-toggle-fixed-width)
	    (buffer-string))))
  ;; No region: Toggle off fixed-width markers in fixed-width areas.
  (should
   (equal "A"
	  (org-test-with-temp-text ": A"
	    (org-toggle-fixed-width)
	    (buffer-string))))
  ;; No region: Toggle on marker in blank lines after elements or just
  ;; after a headline.
  (should
   (equal "* H\n: "
	  (org-test-with-temp-text "* H\n"
	    (forward-line)
	    (org-toggle-fixed-width)
	    (buffer-string))))
  (should
   (equal "#+BEGIN_EXAMPLE\nContents\n#+END_EXAMPLE\n: "
	  (org-test-with-temp-text "#+BEGIN_EXAMPLE\nContents\n#+END_EXAMPLE\n"
	    (goto-char (point-max))
	    (org-toggle-fixed-width)
	    (buffer-string))))
  ;; No region: Toggle on marker in front of one line elements (e.g.,
  ;; headlines, clocks)
  (should
   (equal ": * Headline"
	  (org-test-with-temp-text "* Headline"
	    (org-toggle-fixed-width)
	    (buffer-string))))
  (should
   (equal ": #+KEYWORD: value"
	  (org-test-with-temp-text "#+KEYWORD: value"
	    (org-toggle-fixed-width)
	    (buffer-string))))
  ;; No region: error in other situations.
  (should-error
   (org-test-with-temp-text "#+BEGIN_EXAMPLE\n: A\n#+END_EXAMPLE"
     (forward-line)
     (org-toggle-fixed-width)
     (buffer-string)))
  ;; No region: Indentation is preserved.
  (should
   (equal "- A\n  : B"
	  (org-test-with-temp-text "- A\n  B"
	    (forward-line)
	    (org-toggle-fixed-width)
	    (buffer-string))))
  ;; Region: If it contains only fixed-width elements and blank lines,
  ;; toggle off fixed-width markup.
  (should
   (equal "A\n\nB"
	  (org-test-with-temp-text ": A\n\n: B"
	    (transient-mark-mode 1)
	    (push-mark (point) t t)
	    (goto-char (point-max))
	    (org-toggle-fixed-width)
	    (buffer-string))))
  ;; Region: If it contains anything else, toggle on fixed-width but
  ;; not on fixed-width areas.
  (should
   (equal ": A\n: \n: B\n: \n: C"
	  (org-test-with-temp-text "A\n\n: B\n\nC"
	    (transient-mark-mode 1)
	    (push-mark (point) t t)
	    (goto-char (point-max))
	    (org-toggle-fixed-width)
	    (buffer-string))))
  ;; Region: Ignore blank lines at its end, unless it contains only
  ;; such lines.
  (should
   (equal ": A\n\n"
	  (org-test-with-temp-text "A\n\n"
	    (transient-mark-mode 1)
	    (push-mark (point) t t)
	    (goto-char (point-max))
	    (org-toggle-fixed-width)
	    (buffer-string))))
  (should
   (equal ": \n: \n"
	  (org-test-with-temp-text "\n\n"
	    (transient-mark-mode 1)
	    (push-mark (point) t t)
	    (goto-char (point-max))
	    (org-toggle-fixed-width)
	    (buffer-string)))))

(ert-deftest test-org/kill-line ()
  "Test `org-kill-line' specifications."
  ;; At the beginning of a line, kill whole line.
  (should
   (equal ""
	  (org-test-with-temp-text "abc"
	    (org-kill-line)
	    (buffer-string))))
  ;; In the middle of a line, kill line until its end.
  (should
   (equal "a"
	  (org-test-with-temp-text "a<point>bc"
	    (org-kill-line)
	    (buffer-string))))
  ;; Do not kill newline character.
  (should
   (equal "\n123"
	  (org-test-with-temp-text "abc\n123"
	    (org-kill-line)
	    (buffer-string))))
  (should
   (equal "a\n123"
	  (org-test-with-temp-text "a<point>bc\n123"
	    (org-kill-line)
	    (buffer-string))))
  ;; When `org-special-ctrl-k' is non-nil and point is at a headline,
  ;; kill until tags.
  (should
   (equal "* A :tag:"
	  (org-test-with-temp-text "* A<point>B :tag:"
	    (let ((org-special-ctrl-k t)
		  (org-tags-column 0))
	      (org-kill-line))
	    (buffer-string))))
  ;; If point is on tags, only kill part left until the end of line.
  (should
   (equal "* A :tag:"
	  (org-test-with-temp-text "* A :tag:<point>tag2:"
	    (let ((org-special-ctrl-k t)
		  (org-tags-column 0))
	      (org-kill-line))
	    (buffer-string))))
  ;; However, if point is at the beginning of the line, kill whole
  ;; headline.
  (should
   (equal ""
	  (org-test-with-temp-text "* AB :tag:"
	    (let ((org-special-ctrl-k t)
		  (org-tags-column 0))
	      (org-kill-line))
	    (buffer-string))))
  ;; When `org-ctrl-k-protect-subtree' is non-nil, and point is in
  ;; invisible text, ask before removing it.  When set to `error',
  ;; throw an error.
  (should-error
   (org-test-with-temp-text "* H\n** <point>H2\nContents\n* H3"
     (org-overview)
     (let ((org-special-ctrl-k nil)
	   (org-ctrl-k-protect-subtree t))
       (cl-letf (((symbol-function 'y-or-n-p) 'ignore))
	 (org-kill-line)))))
  (should-error
   (org-test-with-temp-text "* H\n** <point>H2\nContents\n* H3"
     (org-overview)
     (let ((org-special-ctrl-k nil)
	   (org-ctrl-k-protect-subtree 'error))
       (org-kill-line)))))



;;; Headline

(ert-deftest test-org/get-heading ()
  "Test `org-get-heading' specifications."
  ;; Return current heading, even if point is not on it.
  (should
   (equal "H"
	  (org-test-with-temp-text "* H"
	    (org-get-heading))))
  (should
   (equal "H"
	  (org-test-with-temp-text "* H\nText<point>"
	    (org-get-heading))))
  ;; Without any optional argument, return TODO keyword, priority
  ;; cookie, COMMENT keyword and tags.
  (should
   (equal "TODO H"
	  (org-test-with-temp-text "#+TODO: TODO | DONE\n* TODO H<point>"
	    (org-get-heading))))
  (should
   (equal "[#A] H"
	  (org-test-with-temp-text "* [#A] H"
	    (org-get-heading))))
  (should
   (equal "COMMENT H"
	  (org-test-with-temp-text "* COMMENT H"
	    (org-get-heading))))
  (should
   (equal "H :tag:"
	  (org-test-with-temp-text "* H :tag:"
	    (org-get-heading))))
  ;; With NO-TAGS argument, ignore tags.
  (should
   (equal "TODO H"
	  (org-test-with-temp-text "#+TODO: TODO | DONE\n* TODO H<point>"
	    (org-get-heading t))))
  (should
   (equal "H"
	  (org-test-with-temp-text "* H :tag:"
	    (org-get-heading t))))
  ;; With NO-TODO, ignore TODO keyword.
  (should
   (equal "H"
	  (org-test-with-temp-text "#+TODO: TODO | DONE\n* TODO H<point>"
	    (org-get-heading nil t))))
  (should
   (equal "H :tag:"
	  (org-test-with-temp-text "* H :tag:"
	    (org-get-heading nil t))))
  ;; TODO keywords are case-sensitive.
  (should
   (equal "Todo H"
	  (org-test-with-temp-text "#+TODO: TODO | DONE\n* Todo H<point>"
	    (org-get-heading nil t))))
  ;; With NO-PRIORITY, ignore priority.
  (should
   (equal "H"
	  (org-test-with-temp-text "* [#A] H"
	    (org-get-heading nil nil t))))
  (should
   (equal "H"
	  (org-test-with-temp-text "* H"
	    (org-get-heading nil nil t))))
  (should
   (equal "TODO H"
	  (org-test-with-temp-text "* TODO [#A] H"
	    (org-get-heading nil nil t))))
  ;; With NO-COMMENT, ignore COMMENT keyword.
  (should
   (equal "H"
	  (org-test-with-temp-text "* COMMENT H"
	    (org-get-heading nil nil nil t))))
  (should
   (equal "H"
	  (org-test-with-temp-text "* H"
	    (org-get-heading nil nil nil t))))
  (should
   (equal "TODO [#A] H"
	  (org-test-with-temp-text "* TODO [#A] COMMENT H"
	    (org-get-heading nil nil nil t))))
  ;; On an empty headline, return value is consistent.
  (should (equal "" (org-test-with-temp-text "* " (org-get-heading))))
  (should (equal "" (org-test-with-temp-text "* " (org-get-heading t))))
  (should (equal "" (org-test-with-temp-text "* " (org-get-heading nil t))))
  (should (equal "" (org-test-with-temp-text "* " (org-get-heading nil nil t))))
  (should
   (equal "" (org-test-with-temp-text "* " (org-get-heading nil nil nil t)))))

(ert-deftest test-org/in-commented-heading-p ()
  "Test `org-in-commented-heading-p' specifications."
  ;; Commented headline.
  (should
   (org-test-with-temp-text "* COMMENT Headline\nBody"
     (goto-char (point-max))
     (org-in-commented-heading-p)))
  ;; Commented ancestor.
  (should
   (org-test-with-temp-text "* COMMENT Headline\n** Level 2\nBody"
     (goto-char (point-max))
     (org-in-commented-heading-p)))
  ;; Comment keyword is case-sensitive.
  (should-not
   (org-test-with-temp-text "* Comment Headline\nBody"
     (goto-char (point-max))
     (org-in-commented-heading-p)))
  ;; Keyword is standalone.
  (should-not
   (org-test-with-temp-text "* COMMENTHeadline\nBody"
     (goto-char (point-max))
     (org-in-commented-heading-p)))
  ;; Optional argument.
  (should-not
   (org-test-with-temp-text "* COMMENT Headline\n** Level 2\nBody"
     (goto-char (point-max))
     (org-in-commented-heading-p t))))

(ert-deftest test-org/in-archived-heading-p ()
  "Test `org-in-archived-heading-p' specifications."
  ;; Archived headline.
  (should
   (org-test-with-temp-text "* Headline :ARCHIVE:\nBody"
     (goto-char (point-max))
     (org-in-archived-heading-p)))
  ;; Archived ancestor.
  (should
   (org-test-with-temp-text "* Headline :ARCHIVE:\n** Level 2\nBody"
     (goto-char (point-max))
     (org-in-archived-heading-p)))
  ;; Optional argument.
  (should-not
   (org-test-with-temp-text "* Headline :ARCHIVE:\n** Level 2\nBody"
     (goto-char (point-max))
     (org-in-archived-heading-p t)))
   ;; Archive tag containing ARCHIVE as substring
   (should-not
    (org-test-with-temp-text "* Headline :NOARCHIVE:\n** Level 2\nBody"
     (goto-char (point-max))
     (org-in-archived-heading-p))))

(ert-deftest test-org/entry-blocked-p ()
  ;; Check other dependencies.
  (should
   (org-test-with-temp-text "* TODO Blocked\n** DONE one\n** TODO two"
     (let ((org-enforce-todo-dependencies t)
	   (org-blocker-hook
	    '(org-block-todo-from-children-or-siblings-or-parent)))
       (org-entry-blocked-p))))
  (should-not
   (org-test-with-temp-text "* TODO Blocked\n** DONE one\n** DONE two"
     (let ((org-enforce-todo-dependencies t)
	   (org-blocker-hook
	    '(org-block-todo-from-children-or-siblings-or-parent)))
       (org-entry-blocked-p))))
  ;; Entry without a TODO keyword or with a DONE keyword cannot be
  ;; blocked.
  (should-not
   (org-test-with-temp-text "* Blocked\n** TODO one"
     (let ((org-enforce-todo-dependencies t)
	   (org-blocker-hook
	    '(org-block-todo-from-children-or-siblings-or-parent)))
       (org-entry-blocked-p))))
  (should-not
   (org-test-with-temp-text "* DONE Blocked\n** TODO one"
     (let ((org-enforce-todo-dependencies t)
	   (org-blocker-hook
	    '(org-block-todo-from-children-or-siblings-or-parent)))
       (org-entry-blocked-p))))
  ;; Follow :ORDERED: specifications.
  (should
   (org-test-with-temp-text
       "* H\n:PROPERTIES:\n:ORDERED: t\n:END:\n** TODO one\n** <point>TODO two"
     (let ((org-enforce-todo-dependencies t)
	   (org-blocker-hook
	    '(org-block-todo-from-children-or-siblings-or-parent)))
       (org-entry-blocked-p))))
  (should-not
   (org-test-with-temp-text
       "* H\n:PROPERTIES:\n:ORDERED: t\n:END:\n** <point>TODO one\n** DONE two"
     (let ((org-enforce-todo-dependencies t)
	   (org-blocker-hook
	    '(org-block-todo-from-children-or-siblings-or-parent)))
       (org-entry-blocked-p)))))

(ert-deftest test-org/get-outline-path ()
  "Test `org-get-outline-path' specifications."
  ;; Top-level headlines have no outline path.
  (should-not
   (org-test-with-temp-text "* H"
     (org-get-outline-path)))
  ;; Otherwise, outline path is the path leading to the headline.
  (should
   (equal '("H")
	  (org-test-with-temp-text "* H\n** S<point>"
	    (org-get-outline-path))))
  ;; Find path even when point is not on a headline.
  (should
   (equal '("H")
	  (org-test-with-temp-text "* H\n** S\nText<point>"
	    (org-get-outline-path))))
  ;; TODO keywords, tags and statistics cookies are ignored.
  (should
   (equal '("H")
	  (org-test-with-temp-text "* TODO H [0/1] :tag:\n** S<point>"
	    (org-get-outline-path))))
  ;; Links are replaced with their description or their path.
  (should
   (equal '("Org")
	  (org-test-with-temp-text
	      "* [[https://orgmode.org][Org]]\n** S<point>"
	    (org-get-outline-path))))
  (should
   (equal '("https://orgmode.org")
	  (org-test-with-temp-text
	      "* [[https://orgmode.org]]\n** S<point>"
	    (org-get-outline-path))))
  ;; When WITH-SELF is non-nil, include current heading.
  (should
   (equal '("H")
	  (org-test-with-temp-text "* H"
	    (org-get-outline-path t))))
  (should
   (equal '("H" "S")
	  (org-test-with-temp-text "* H\n** S\nText<point>"
	    (org-get-outline-path t))))
  ;; Using cache is transparent to the user.
  (should
   (equal '("H")
	  (org-test-with-temp-text "* H\n** S<point>"
	    (setq org-outline-path-cache nil)
	    (org-get-outline-path nil t))))
  ;; Do not corrupt cache when finding outline path in distant part of
  ;; the buffer.
  (should
   (equal '("H2")
	  (org-test-with-temp-text "* H\n** S<point>\n* H2\n** S2"
	    (setq org-outline-path-cache nil)
	    (org-get-outline-path nil t)
	    (search-forward "S2")
	    (org-get-outline-path nil t))))
  ;; Do not choke on empty headlines.
  (should
   (org-test-with-temp-text "* H\n** <point>"
     (org-get-outline-path)))
  (should
   (org-test-with-temp-text "* \n** H<point>"
     (org-get-outline-path))))

(ert-deftest test-org/format-outline-path ()
  "Test `org-format-outline-path' specifications."
  (should
   (string= (org-format-outline-path (list "one" "two" "three"))
	    "one/two/three"))
  ;; Empty path.
  (should
   (string= (org-format-outline-path '())
	    ""))
  (should
   (string= (org-format-outline-path '(nil))
	    ""))
  ;; Empty path and prefix.
  (should
   (string= (org-format-outline-path '() nil ">>")
	    ">>"))
  ;; Trailing whitespace in headings.
  (should
   (string= (org-format-outline-path (list "one\t" "tw o " "three  "))
	    "one/tw o/three"))
  ;; Non-default prefix and separators.
  (should
   (string= (org-format-outline-path (list "one" "two" "three") nil ">>" "|")
	    ">>|one|two|three"))
  ;; Truncate.
  (should
   (string= (org-format-outline-path (list "one" "two" "three" "four") 10)
	    "one/two/.."))
  ;; Give a very narrow width.
  (should
   (string= (org-format-outline-path (list "one" "two" "three" "four") 2)
	    "on"))
  ;; Give a prefix that extends beyond the width.
  (should
   (string= (org-format-outline-path (list "one" "two" "three" "four") 10
				     ">>>>>>>>>>")
	    ">>>>>>>>..")))

(ert-deftest test-org/map-entries ()
  "Test `org-map-entries' specifications."
  ;; Full match.
  (should
   (equal '(1 11)
	  (org-test-with-temp-text "* Level 1\n** Level 2"
	    (org-map-entries #'point))))
  ;; Level match.
  (should
   (equal '(1)
	  (org-test-with-temp-text "* Level 1\n** Level 2"
	    (let (org-odd-levels-only) (org-map-entries #'point "LEVEL=1")))))
  (should
   (equal '(11)
	  (org-test-with-temp-text "* Level 1\n** Level 2"
	    (let (org-odd-levels-only) (org-map-entries #'point "LEVEL>1")))))
  ;; Tag match.
  (should
   (equal '(11)
	  (org-test-with-temp-text "* H1 :no:\n* H2 :yes:"
	    (org-map-entries #'point "yes"))))
  (should
   (equal '(14)
	  (org-test-with-temp-text "* H1 :yes:a:\n* H2 :yes:b:"
	    (org-map-entries #'point "+yes-a"))))
  (should
   (equal '(11 23)
	  (org-test-with-temp-text "* H1 :no:\n* H2 :yes1:\n* H3 :yes2:"
	    (org-map-entries #'point "{yes?}"))))
  ;; Priority match.
  (should
   (equal '(1)
	  (org-test-with-temp-text "* [#A] H1\n* [#B] H2"
	    (org-map-entries #'point "PRIORITY=\"A\""))))
  ;; Date match.
  (should
   (equal '(36)
	  (org-test-with-temp-text "
* H1
SCHEDULED: <2012-03-29 thu.>
* H2
SCHEDULED: <2014-03-04 tue.>"
	    (org-map-entries #'point "SCHEDULED=\"<2014-03-04 tue.>\""))))
  (should
   (equal '(2)
	  (org-test-with-temp-text "
* H1
SCHEDULED: <2012-03-29 thu.>
* H2
SCHEDULED: <2014-03-04 tue.>"
	    (org-map-entries #'point "SCHEDULED<\"<2013-01-01>\""))))
  ;; Regular property match.
  (should
   (equal '(2)
	  (org-test-with-temp-text "
* H1
:PROPERTIES:
:TEST: 1
:END:
* H2
:PROPERTIES:
:TEST: 2
:END:"
	    (org-map-entries #'point "TEST=1"))))
  ;; Multiple criteria.
  (should
   (equal '(23)
	  (org-test-with-temp-text "* H1 :no:\n** H2 :yes:\n* H3 :yes:"
	    (let (org-odd-levels-only
		  (org-use-tag-inheritance nil))
	      (org-map-entries #'point "yes+LEVEL=1")))))
  ;; "or" criteria.
  (should
   (equal '(12 24)
	  (org-test-with-temp-text "* H1 :yes:\n** H2 :yes:\n** H3 :no:"
	    (let (org-odd-levels-only)
	      (org-map-entries #'point "LEVEL=2|no")))))
  (should
   (equal '(1 12)
	  (org-test-with-temp-text "* H1 :yes:\n* H2 :no:\n* H3 :maybe:"
	    (let (org-odd-levels-only)
	      (org-map-entries #'point "yes|no")))))
  ;; "and" criteria.
  (should
   (equal '(22)
	  (org-test-with-temp-text "* H1 :yes:\n* H2 :no:\n* H3 :yes:no:"
	    (let (org-odd-levels-only)
	      (org-map-entries #'point "yes&no"))))))

(ert-deftest test-org/edit-headline ()
  "Test `org-edit-headline' specifications."
  (should
   (equal "* B"
	  (org-test-with-temp-text "* A"
	    (org-edit-headline "B")
	    (buffer-string))))
  ;; Handle empty headings.
  (should
   (equal "* "
	  (org-test-with-temp-text "* A"
	    (org-edit-headline "")
	    (buffer-string))))
  (should
   (equal "* A"
	  (org-test-with-temp-text "* "
	    (org-edit-headline "A")
	    (buffer-string))))
  ;; Handle TODO keywords and priority cookies.
  (should
   (equal "* TODO B"
	  (org-test-with-temp-text "* TODO A"
	    (org-edit-headline "B")
	    (buffer-string))))
  (should
   (equal "* [#A] B"
	  (org-test-with-temp-text "* [#A] A"
	    (org-edit-headline "B")
	    (buffer-string))))
  (should
   (equal "* TODO [#A] B"
	  (org-test-with-temp-text "* TODO [#A] A"
	    (org-edit-headline "B")
	    (buffer-string))))
  ;; Handle tags.
  (equal "* B :tag:"
	 (org-test-with-temp-text "* A :tag:"
	   (let ((org-tags-column 4)) (org-edit-headline "B"))
	   (buffer-string))))



;;; Keywords

(ert-deftest test-org/set-regexps-and-options ()
  "Test `org-set-regexps-and-options' specifications."
  ;; TAGS keyword.
  (should
   (equal '(("A"))
	  (let ((org-tag-alist '(("A")))
		(org-tag-persistent-alist nil))
	    (org-test-with-temp-text ""
	      (org-mode-restart)
	      org-current-tag-alist))))
  (should
   (equal '(("B"))
	  (let ((org-tag-alist '(("A")))
		(org-tag-persistent-alist nil))
	    (org-test-with-temp-text "#+TAGS: B"
	      (org-mode-restart)
	      org-current-tag-alist))))
  (should
   (equal '(("C") ("B"))
	  (let ((org-tag-alist '(("A")))
		(org-tag-persistent-alist '(("C"))))
	    (org-test-with-temp-text "#+TAGS: B"
	      (org-mode-restart)
	      org-current-tag-alist))))
  (should
   (equal '(("B"))
	  (let ((org-tag-alist '(("A")))
		(org-tag-persistent-alist '(("C"))))
	    (org-test-with-temp-text "#+STARTUP: noptag\n#+TAGS: B"
	      (org-mode-restart)
	      org-current-tag-alist))))
  (should
   (equal '(("A" . ?a) ("B") ("C"))
	  (let ((org-tag-persistant-alist nil))
	    (org-test-with-temp-text "#+TAGS: A(a) B C"
	      (org-mode-restart)
	      org-current-tag-alist))))
  (should
   (equal '(("A") (:newline) ("B"))
	  (let ((org-tag-persistent-alist nil))
	    (org-test-with-temp-text "#+TAGS: A\n#+TAGS: B"
	      (org-mode-restart)
	      org-current-tag-alist))))
  (should
   (equal '((:startgroup) ("A") ("B") (:endgroup) ("C"))
	  (let ((org-tag-persistent-alist nil))
	    (org-test-with-temp-text "#+TAGS: { A B } C"
	      (org-mode-restart)
	      org-current-tag-alist))))
  (should
   (equal '((:startgroup) ("A") (:grouptags) ("B") ("C") (:endgroup))
	  (let ((org-tag-persistent-alist nil))
	    (org-test-with-temp-text "#+TAGS: { A : B C }"
	      (org-mode-restart)
	      org-current-tag-alist))))
  (should
   (equal '(("A" "B" "C"))
	  (let ((org-tag-persistent-alist nil))
	    (org-test-with-temp-text "#+TAGS: { A : B C }"
	      (org-mode-restart)
	      org-tag-groups-alist))))
  (should
   (equal '((:startgrouptag) ("A") (:grouptags) ("B") ("C") (:endgrouptag))
	  (let ((org-tag-persistent-alist nil))
	    (org-test-with-temp-text "#+TAGS: [ A : B C ]"
	      (org-mode-restart)
	      org-current-tag-alist))))
  (should
   (equal '(("A" "B" "C"))
	  (let ((org-tag-persistent-alist nil))
	    (org-test-with-temp-text "#+TAGS: [ A : B C ]"
	      (org-mode-restart)
	      org-tag-groups-alist))))
  (should-not
   (let ((org-tag-alist '(("A"))))
     (org-test-with-temp-text "#+TAGS:"
       (org-mode-restart)
       org-current-tag-alist)))
  ;; FILETAGS keyword.
  (should
   (equal '("A" "B" "C")
	  (org-test-with-temp-text "#+FILETAGS: :A:B:C:"
	    (org-mode-restart)
	    org-file-tags)))
  ;; PROPERTY keyword.  Property names are case-insensitive.
  (should
   (equal "foo=1"
	  (org-test-with-temp-text "#+PROPERTY: var foo=1"
	    (org-mode-restart)
	    (cdr (assoc "var" org-keyword-properties)))))
  (should
   (equal
    "foo=1 bar=2"
    (org-test-with-temp-text "#+PROPERTY: var foo=1\n#+PROPERTY: var+ bar=2"
      (org-mode-restart)
      (cdr (assoc "var" org-keyword-properties)))))
  (should
   (equal
    "foo=1 bar=2"
    (org-test-with-temp-text "#+PROPERTY: var foo=1\n#+PROPERTY: VAR+ bar=2"
      (org-mode-restart)
      (cdr (assoc "var" org-keyword-properties)))))
  ;; ARCHIVE keyword.
  (should
   (equal "%s_done::"
	  (org-test-with-temp-text "#+ARCHIVE: %s_done::"
	    (org-mode-restart)
	    org-archive-location)))
  ;; CATEGORY keyword.
  (should
   (eq 'test
       (org-test-with-temp-text "#+CATEGORY: test"
	 (org-mode-restart)
	 org-category)))
  (should
   (equal "test"
	  (org-test-with-temp-text "#+CATEGORY: test"
	    (org-mode-restart)
	    (cdr (assoc "CATEGORY" org-keyword-properties)))))
  ;; COLUMNS keyword.
  (should
   (equal "%25ITEM %TAGS %PRIORITY %TODO"
	  (org-test-with-temp-text "#+COLUMNS: %25ITEM %TAGS %PRIORITY %TODO"
	    (org-mode-restart)
	    org-columns-default-format)))
  ;; CONSTANTS keyword.  Constants names are case sensitive.
  (should
   (equal '("299792458." "3.14")
	  (org-test-with-temp-text "#+CONSTANTS: c=299792458. pi=3.14"
	    (org-mode-restart)
	    (mapcar
	     (lambda (n) (cdr (assoc n org-table-formula-constants-local)))
	     '("c" "pi")))))
  (should
   (equal "3.14"
	  (org-test-with-temp-text "#+CONSTANTS: pi=22/7 pi=3.14"
	    (org-mode-restart)
	    (cdr (assoc "pi" org-table-formula-constants-local)))))
  (should
   (equal "22/7"
	  (org-test-with-temp-text "#+CONSTANTS: PI=22/7 pi=3.14"
	    (org-mode-restart)
	    (cdr (assoc "PI" org-table-formula-constants-local)))))
  ;; LINK keyword.
  (should
   (equal
    '("url1" "url2")
    (org-test-with-temp-text "#+LINK: a url1\n#+LINK: b url2"
      (org-mode-restart)
      (mapcar (lambda (abbrev) (cdr (assoc abbrev org-link-abbrev-alist-local)))
	      '("a" "b")))))
  ;; PRIORITIES keyword.  Incomplete priorities sets are ignored.
  (should
   (equal
    '(?X ?Z ?Y)
    (org-test-with-temp-text "#+PRIORITIES: X Z Y"
      (org-mode-restart)
      (list org-priority-highest org-priority-lowest org-priority-default))))
  (should
   (equal
    '(?A ?C ?B)
    (org-test-with-temp-text "#+PRIORITIES: X Z"
      (org-mode-restart)
      (list org-priority-highest org-priority-lowest org-priority-default))))
  ;; STARTUP keyword.
  (should
   (equal '(t t)
	  (org-test-with-temp-text "#+STARTUP: fold odd"
	    (org-mode-restart)
	    (list org-startup-folded org-odd-levels-only))))
  ;; TODO keywords.
  (should
   (equal '(("A" "B") ("C"))
	  (org-test-with-temp-text "#+TODO: A B | C"
	    (org-mode-restart)
	    (list org-not-done-keywords org-done-keywords))))
  (should
   (equal '(("A" "C") ("B" "D"))
	  (org-test-with-temp-text "#+TODO: A | B\n#+TODO: C | D"
	    (org-mode-restart)
	    (list org-not-done-keywords org-done-keywords))))
  (should
   (equal '(("A" "B") ("C"))
	  (org-test-with-temp-text "#+TYP_TODO: A B | C"
	    (org-mode-restart)
	    (list org-not-done-keywords org-done-keywords))))
  (should
   (equal '((:startgroup) ("A" . ?a) (:endgroup))
	  (org-test-with-temp-text "#+TODO: A(a)"
	    (org-mode-restart)
	    org-todo-key-alist)))
  (should
   (equal '(("D" note nil) ("C" time nil) ("B" note time))
	  (org-test-with-temp-text "#+TODO: A(a) B(b@/!) | C(c!) D(d@)"
	    (org-mode-restart)
	    org-todo-log-states)))
  ;; Enter SETUPFILE keyword.
  (should
   (equal "1"
	  (org-test-with-temp-text
	      (format "#+SETUPFILE: \"%s/examples/setupfile.org\"" org-test-dir)
	    (org-mode-restart)
	    (cdr (assoc "a" org-keyword-properties))))))



;;; Links

;;;; Coderefs

(ert-deftest test-org/coderef ()
  "Test coderef links specifications."
  (should
   (org-test-with-temp-text "
#+BEGIN_SRC emacs-lisp
\(+ 1 1)                  (ref:sc)
#+END_SRC
\[[(sc)<point>]]"
     (org-open-at-point)
     (looking-at "(ref:sc)")))
  ;; Find coderef even with alternate label format.
  (should
   (org-test-with-temp-text "
#+BEGIN_SRC emacs-lisp -l \"{ref:%s}\"
\(+ 1 1)                  {ref:sc}
#+END_SRC
\[[(sc)<point>]]"
     (org-open-at-point)
     (looking-at "{ref:sc}"))))

;;;; Custom ID

(ert-deftest test-org/custom-id ()
  "Test custom ID links specifications."
  (should
   (org-test-with-temp-text
       "* H1\n:PROPERTIES:\n:CUSTOM_ID: custom\n:END:\n* H2\n[[#custom<point>]]"
     (org-open-at-point)
     (looking-at-p "\\* H1")))
  ;; Handle escape characters.
  (should
   (org-test-with-temp-text
       "* H1\n:PROPERTIES:\n:CUSTOM_ID: [%]\n:END:\n* H2\n[[#\\[%\\]<point>]]"
     (org-open-at-point)
     (looking-at-p "\\* H1")))
  ;; Throw an error on false positives.
  (should-error
   (org-test-with-temp-text
       "* H1\n:DRAWER:\n:CUSTOM_ID: custom\n:END:\n* H2\n[[#custom<point>]]"
     (org-open-at-point)
     (looking-at-p "\\* H1"))))

;;;; Fuzzy Links

;; Fuzzy links [[text]] encompass links to a target (<<text>>), to
;; a named element (#+name: text) and to headlines (* Text).

(ert-deftest test-org/fuzzy-links ()
  "Test fuzzy links specifications."
  ;; Fuzzy link goes in priority to a matching target.
  (should
   (org-test-with-temp-text
       "#+NAME: Test\n|a|b|\n<<Test>>\n* Test\n<point>[[Test]]"
     (let ((org-link-search-must-match-exact-headline nil)) (org-open-at-point))
     (looking-at "<<Test>>")))
  ;; Then fuzzy link points to an element with a given name.
  (should
   (org-test-with-temp-text "Test\n#+NAME: Test\n|a|b|\n* Test\n<point>[[Test]]"
     (let ((org-link-search-must-match-exact-headline nil)) (org-open-at-point))
     (looking-at "#\\+NAME: Test")))
  ;; A target still lead to a matching headline otherwise.
  (should
   (org-test-with-temp-text "* Head1\n* Head2\n*Head3\n<point>[[Head2]]"
     (let ((org-link-search-must-match-exact-headline nil)) (org-open-at-point))
     (looking-at "\\* Head2")))
  ;; With a leading star in link, enforce heading match.
  (should
   (org-test-with-temp-text "* Test\n<<Test>>\n<point>[[*Test]]"
     (let ((org-link-search-must-match-exact-headline nil)) (org-open-at-point))
     (looking-at "\\* Test")))
  ;; With a leading star in link, enforce exact heading match, even
  ;; with `org-link-search-must-match-exact-headline' set to nil.
  (should-error
   (org-test-with-temp-text "* Test 1\nFoo Bar\n<point>[[*Test]]"
     (let ((org-link-search-must-match-exact-headline nil))
       (org-open-at-point))))
  ;; Handle non-nil `org-link-search-must-match-exact-headline'.
  (should
   (org-test-with-temp-text "* Test\nFoo Bar\n<point>[[Test]]"
     (let ((org-link-search-must-match-exact-headline t)) (org-open-at-point))
     (looking-at "\\* Test")))
  (should
   (org-test-with-temp-text "* Test\nFoo Bar\n<point>[[*Test]]"
     (let ((org-link-search-must-match-exact-headline t)) (org-open-at-point))
     (looking-at "\\* Test")))
  ;; Heading match should not care about spaces, cookies, TODO
  ;; keywords, priorities, and tags.  However, TODO keywords are
  ;; case-sensitive.
  (should
   (let ((first-line
	  "** TODO [#A] [/]  Test [1/2] [33%] 1 \t  2 [%] :work:urgent: "))
     (org-test-with-temp-text
	 (concat first-line "\nFoo Bar\n<point>[[*Test 1 2]]")
       (let ((org-link-search-must-match-exact-headline nil)
	     (org-todo-regexp "TODO"))
	 (org-open-at-point))
       (looking-at (regexp-quote first-line)))))
  (should-error
   (org-test-with-temp-text "** todo Test 1 2\nFoo Bar\n<point>[[*Test 1 2]]"
     (let ((org-link-search-must-match-exact-headline nil)
	   (org-todo-regexp "TODO"))
       (org-open-at-point))))
  ;; Heading match should still be exact.
  (should-error
   (org-test-with-temp-text "
** TODO [#A] [/]  Test [1/2] [33%] 1 \t  2 [%] :work:urgent:
Foo Bar
<point>[[*Test 1]]"
     (let ((org-link-search-must-match-exact-headline nil)
	   (org-todo-regexp "TODO"))
       (org-open-at-point))))
  (should
   (org-test-with-temp-text "* Test 1 2 3\n** Test 1 2\n<point>[[*Test 1 2]]"
     (let ((org-link-search-must-match-exact-headline nil)
	   (org-todo-regexp "TODO"))
       (org-open-at-point))
     (looking-at-p (regexp-quote "** Test 1 2"))))
  ;; Heading match ignores COMMENT keyword.
  (should
   (org-test-with-temp-text "[[*Test]]\n* COMMENT Test"
     (org-open-at-point)
     (looking-at "\\* COMMENT Test")))
  (should
   (org-test-with-temp-text "[[*Test]]\n* TODO COMMENT Test"
     (org-open-at-point)
     (looking-at "\\* TODO COMMENT Test")))
  ;; Correctly un-escape fuzzy links.
  (should
   (org-test-with-temp-text "* [foo]\n[[*\\[foo\\]][With escaped characters]]"
     (org-open-at-point)
     (bobp)))
  ;; Match search strings containing newline characters, including
  ;; blank lines.
  (should
   (org-test-with-temp-text-in-file "Paragraph\n\nline1\nline2\n\n"
     (let ((file (buffer-file-name)))
       (goto-char (point-max))
       (insert (format "[[file:%s::line1 line2]]" file))
       (beginning-of-line)
       (let ((org-link-search-must-match-exact-headline nil))
	 (org-open-at-point 0))
       (looking-at-p "line1"))))
  (should
   (org-test-with-temp-text-in-file "Paragraph\n\nline1\n\nline2\n\n"
     (let ((file (buffer-file-name)))
       (goto-char (point-max))
       (insert (format "[[file:%s::line1 line2]]" file))
       (beginning-of-line)
       (let ((org-link-search-must-match-exact-headline nil))
	 (org-open-at-point 0))
       (looking-at-p "line1")))))

;;;; Open at point

(ert-deftest test-org/open-at-point/keyword ()
  "Does `org-open-at-point' open link in a keyword line?"
  (should
   (org-test-with-temp-text
       "<<top>>\n#+KEYWORD: <point>[[top]]"
     (org-open-at-point) t))
  (should
   (org-test-with-temp-text
       "* H\n<<top>>\n#+KEYWORD: <point>[[top]]"
     (org-open-at-point) t)))

(ert-deftest test-org/open-at-point/property ()
  "Does `org-open-at-point' open link in property drawer?"
  (should
   (org-test-with-temp-text
       "* Headline
:PROPERTIES:
:URL: <point>[[*Headline]]
:END:"
     (org-open-at-point) t)))

(ert-deftest test-org/open-at-point/comment ()
  "Does `org-open-at-point' open link in a commented line?"
  (should
   (org-test-with-temp-text
    "<<top>>\n# <point>[[top]]"
    (org-open-at-point) t))
  (should
   (org-test-with-temp-text
    "* H\n<<top>>\n# <point>[[top]]"
    (org-open-at-point) t)))

(ert-deftest test-org/open-at-point/inline-image ()
  "Test `org-open-at-point' on nested links."
  (should
   (org-test-with-temp-text "<<top>>\n[[top][file:<point>unicorn.jpg]]"
     (org-open-at-point)
     (bobp))))

(ert-deftest test-org/open-at-point/radio-target ()
  "Test `org-open-at-point' on radio targets."
  (should
   (org-test-with-temp-text "<<<target>>> <point>target"
     (org-update-radio-target-regexp)
     (org-open-at-point)
     (eq (org-element-type (org-element-context)) 'radio-target))))

(ert-deftest test-org/open-at-point/tag ()
  "Test `org-open-at-point' on tags."
  (should
   (org-test-with-temp-text "* H :<point>tag:"
     (catch :result
       (cl-letf (((symbol-function 'org-tags-view)
		  (lambda (&rest args) (throw :result t))))
	 (org-open-at-point)
	 nil))))
  (should-not
   (org-test-with-temp-text-in-file "* H<point> :tag:"
     (catch :result
       (cl-letf (((symbol-function 'org-tags-view)
		  (lambda (&rest args) (throw :result t))))
	 ;; When point isn't on a tag it's going to try other things,
	 ;; possibly trying to open attachments which will return an
	 ;; error if there isn't an attachment. Suppress that error.
	 (ignore-errors
	     (org-open-at-point))
	 nil)))))


;;; Node Properties

(ert-deftest test-org/accumulated-properties-in-drawers ()
  "Ensure properties accumulate in subtree drawers."
  (org-test-at-id "75282ba2-f77a-4309-a970-e87c149fe125"
    (org-babel-next-src-block)
    (should (equal '(2 1) (org-babel-execute-src-block)))))

(ert-deftest test-org/custom-properties ()
  "Test custom properties specifications."
  ;; Standard test.
  (should
   (let ((org-custom-properties '("FOO")))
     (org-test-with-temp-text "* H\n:PROPERTIES:\n<point>:FOO: val\n:END:\n"
       (org-toggle-custom-properties-visibility)
       (org-invisible-p2))))
  ;; Properties are case-insensitive.
  (should
   (let ((org-custom-properties '("FOO")))
     (org-test-with-temp-text "* H\n:PROPERTIES:\n<point>:foo: val\n:END:\n"
       (org-toggle-custom-properties-visibility)
       (org-invisible-p2))))
  (should
   (let ((org-custom-properties '("foo")))
     (org-test-with-temp-text "* H\n:PROPERTIES:\n<point>:FOO: val\n:END:\n"
       (org-toggle-custom-properties-visibility)
       (org-invisible-p2))))
  ;; Multiple custom properties in the same drawer.
  (should
   (let ((org-custom-properties '("FOO" "BAR")))
     (org-test-with-temp-text
	 "* H\n:PROPERTIES:\n<point>:FOO: val\n:P: 1\n:BAR: baz\n:END:\n"
       (org-toggle-custom-properties-visibility)
       (and (org-invisible-p2)
	    (not (progn (forward-line) (org-invisible-p2)))
	    (progn (forward-line) (org-invisible-p2))))))
  ;; Hide custom properties with an empty value.
  (should
   (let ((org-custom-properties '("FOO")))
     (org-test-with-temp-text "* H\n:PROPERTIES:\n<point>:FOO:\n:END:\n"
       (org-toggle-custom-properties-visibility)
       (org-invisible-p2))))
  ;; Do not hide fake properties.
  (should-not
   (let ((org-custom-properties '("FOO")))
     (org-test-with-temp-text ":FOO: val\n"
       (org-toggle-custom-properties-visibility)
       (org-invisible-p2))))
  (should-not
   (let ((org-custom-properties '("A")))
     (org-test-with-temp-text
	 "* H\n:PROPERTIES:\n:A: 1\n:END:\n\n:PROPERTIES:\n<point>:A: 2\n:END:"
       (org-toggle-custom-properties-visibility)
       (org-invisible-p2)))))



;;; Mark Region

(ert-deftest test-org/mark-element ()
  "Test `org-mark-element' specifications."
  ;; Mark beginning and end of element.
  (should
   (equal '(t t)
	  (org-test-with-temp-text "Para<point>graph"
	    (org-mark-element)
	    (list (bobp) (= (mark) (point-max))))))
  (should
   (equal '(t t)
	  (org-test-with-temp-text "P1\n\nPara<point>graph\n\nP2"
	    (org-mark-element)
	    (list (looking-at "Paragraph")
		  (org-with-point-at (mark) (looking-at "P2"))))))
  ;; Do not set mark past (point-max).
  (should
   (org-test-with-temp-text "Para<point>graph"
     (narrow-to-region 2 6)
     (org-mark-element)
     (= 6 (mark)))))

(ert-deftest test-org/mark-subtree ()
  "Test `org-mark-subtree' specifications."
  ;; Error when point is before first headline.
  (should-error
   (org-test-with-temp-text "Paragraph\n* Headline\nBody"
     (progn (transient-mark-mode 1)
	    (org-mark-subtree))))
  ;; Without argument, mark current subtree.
  (should
   (equal
    '(12 32)
    (org-test-with-temp-text "* Headline\n** Sub-headline\nBody"
      (progn (transient-mark-mode 1)
	     (forward-line 2)
	     (org-mark-subtree)
	     (list (region-beginning) (region-end))))))
  ;; With an argument, move ARG up.
  (should
   (equal
    '(1 32)
    (org-test-with-temp-text "* Headline\n** Sub-headline\nBody"
      (progn (transient-mark-mode 1)
	     (forward-line 2)
	     (org-mark-subtree 1)
	     (list (region-beginning) (region-end))))))
  ;; Do not get fooled by inlinetasks.
  (when (featurep 'org-inlinetask)
    (should
     (= 1
	(org-test-with-temp-text "* Headline\n*************** Task\nContents"
	  (progn (transient-mark-mode 1)
		 (forward-line 1)
		 (let ((org-inlinetask-min-level 15)) (org-mark-subtree))
		 (region-beginning)))))))


 
;;; Miscellaneous

(ert-deftest test-org/sort-entries ()
  "Test `org-sort-entries'."
  ;; Sort alphabetically.
  (should
   (equal "\n* abc\n* def\n* xyz\n"
	  (org-test-with-temp-text "\n* def\n* xyz\n* abc\n"
	    (org-sort-entries nil ?a)
	    (buffer-string))))
  (should
   (equal "\n* xyz\n* def\n* abc\n"
	  (org-test-with-temp-text "\n* def\n* xyz\n* abc\n"
	    (org-sort-entries nil ?A)
	    (buffer-string))))
  (should
   (equal "\n* \n* klm\n* xyz\n"
	  (org-test-with-temp-text "\n* xyz\n* \n* klm\n"
	    (org-sort-entries nil ?a)
	    (buffer-string))))
  ;; Sort numerically.
  (should
   (equal "\n* 1\n* 2\n* 10\n"
	  (org-test-with-temp-text "\n* 10\n* 1\n* 2\n"
	    (org-sort-entries nil ?n)
	    (buffer-string))))
  (should
   (equal "\n* 10\n* 2\n* 1\n"
	  (org-test-with-temp-text "\n* 10\n* 1\n* 2\n"
	    (org-sort-entries nil ?N)
	    (buffer-string))))
  (should
   (equal "\n* \n* 1\n* 2\n"
	  (org-test-with-temp-text "\n* 1\n* \n* 2\n"
	    (org-sort-entries nil ?n)
	    (buffer-string))))
  ;; Sort by custom function.
  (should
   (equal "\n* b\n* aa\n* ccc\n"
	  (org-test-with-temp-text "\n* ccc\n* b\n* aa\n"
	    (org-sort-entries nil ?f
			      (lambda ()
				(length (buffer-substring (point-at-bol)
							  (point-at-eol))))
			      #'<)
	    (buffer-string))))
  (should
   (equal "\n* ccc\n* aa\n* b\n"
	  (org-test-with-temp-text "\n* ccc\n* b\n* aa\n"
	    (org-sort-entries nil ?F
			      (lambda ()
				(length (buffer-substring (point-at-bol)
							  (point-at-eol))))
			      #'<)
	    (buffer-string))))
  ;; Sort by TODO keyword.
  (should
   (equal "\n* TODO h1\n* TODO h3\n* DONE h2\n"
	  (org-test-with-temp-text
	      "\n* TODO h1\n* DONE h2\n* TODO h3\n"
	    (org-sort-entries nil ?o)
	    (buffer-string))))
  (should
   (equal "\n* DONE h2\n* TODO h1\n* TODO h3\n"
	  (org-test-with-temp-text
	      "\n* TODO h1\n* DONE h2\n* TODO h3\n"
	    (org-sort-entries nil ?O)
	    (buffer-string))))
  ;; Sort by priority.
  (should
   (equal "\n* [#A] h2\n* [#B] h3\n* [#C] h1\n"
	  (org-test-with-temp-text
	      "\n* [#C] h1\n* [#A] h2\n* [#B] h3\n"
	    (org-sort-entries nil ?p)
	    (buffer-string))))
  (should
   (equal "\n* [#C] h1\n* [#B] h3\n* [#A] h2\n"
	  (org-test-with-temp-text
	      "\n* [#C] h1\n* [#A] h2\n* [#B] h3\n"
	    (org-sort-entries nil ?P)
	    (buffer-string))))
  ;; Sort by creation time.
  (should
   (equal "
* h3
  [2017-05-08 Mon]
* h2
  [2017-05-09 Tue]
* h1
  [2018-05-09 Wed]
"
	  (org-test-with-temp-text
	      "
* h1
  [2018-05-09 Wed]
* h2
  [2017-05-09 Tue]
* h3
  [2017-05-08 Mon]
"
	    (org-sort-entries nil ?c)
	    (buffer-string))))

  ;; Sort by scheduled date.
  (should
   (equal "
* TODO h4
SCHEDULED: <2017-05-06 Sat>
* TODO h3
SCHEDULED: <2017-05-08 Mon>
* TODO h2
DEADLINE: <2017-05-09 Tue>
* TODO h1
DEADLINE: <2017-05-07 Sun>
"
	  (org-test-with-temp-text
	      "
* TODO h2
DEADLINE: <2017-05-09 Tue>
* TODO h1
DEADLINE: <2017-05-07 Sun>
* TODO h3
SCHEDULED: <2017-05-08 Mon>
* TODO h4
SCHEDULED: <2017-05-06 Sat>
"
	    (org-sort-entries nil ?s)
	    (buffer-string))))
  ;; Sort by deadline date.
  (should
   (equal "
* TODO h1
DEADLINE: <2017-05-07 Sun>
* TODO h2
DEADLINE: <2017-05-09 Tue>
* TODO h3
SCHEDULED: <2017-05-08 Mon>
* TODO h4
SCHEDULED: <2017-05-06 Sat>
"
	  (org-test-with-temp-text
	      "
* TODO h2
DEADLINE: <2017-05-09 Tue>
* TODO h1
DEADLINE: <2017-05-07 Sun>
* TODO h3
SCHEDULED: <2017-05-08 Mon>
* TODO h4
SCHEDULED: <2017-05-06 Sat>
"
	    (org-sort-entries nil ?d)
	    (buffer-string))))
  ;; Sort by any date/time
  (should
   (equal "
* TODO h4
SCHEDULED: <2017-05-06 Sat>
* TODO h1
DEADLINE: <2017-05-07 Sun>
* TODO h3
SCHEDULED: <2017-05-08 Mon>
* TODO h2
DEADLINE: <2017-05-09 Tue>
"
	  (org-test-with-temp-text
	      "
* TODO h2
DEADLINE: <2017-05-09 Tue>
* TODO h1
DEADLINE: <2017-05-07 Sun>
* TODO h3
SCHEDULED: <2017-05-08 Mon>
* TODO h4
SCHEDULED: <2017-05-06 Sat>
"
	    (org-sort-entries nil ?t)
	    (buffer-string))))
  ;; Sort by clocking time.
  (should
   (equal "
* clocked h2
  :LOGBOOK:
  CLOCK: [2017-05-09 Tue 00:15]--[2017-05-09 Tue 00:22] =>  0:07
  CLOCK: [2017-05-09 Tue 00:00]--[2017-05-09 Tue 00:10] =>  0:10
  :END:
* clocked h1
  :LOGBOOK:
  CLOCK: [2017-05-09 Tue 00:15]--[2017-05-09 Tue 00:22] =>  0:07
  CLOCK: [2017-05-09 Tue 00:00]--[2017-05-09 Tue 00:12] =>  0:12
  :END:
"
	  (org-test-with-temp-text
	      "
* clocked h1
  :LOGBOOK:
  CLOCK: [2017-05-09 Tue 00:15]--[2017-05-09 Tue 00:22] =>  0:07
  CLOCK: [2017-05-09 Tue 00:00]--[2017-05-09 Tue 00:12] =>  0:12
  :END:
* clocked h2
  :LOGBOOK:
  CLOCK: [2017-05-09 Tue 00:15]--[2017-05-09 Tue 00:22] =>  0:07
  CLOCK: [2017-05-09 Tue 00:00]--[2017-05-09 Tue 00:10] =>  0:10
  :END:
"
	    (org-sort-entries nil ?k)
	    (buffer-string))))
  ;; Preserve file local variables when sorting.
  (should
   (equal "\n* A\n* B\n# Local Variables:\n# foo: t\n# End:\n"
	  (org-test-with-temp-text
	      "\n* B\n* A\n# Local Variables:\n# foo: t\n# End:"
	    (org-sort-entries nil ?a)
	    (buffer-string)))))

(ert-deftest test-org/string-collate-greaterp ()
  "Test `org-string-collate-greaterp' specifications."
  (should (org-string-collate-greaterp "def" "abc"))
  (should-not (org-string-collate-greaterp "abc" "def")))

(ert-deftest test-org/file-contents ()
  "Test `org-file-contents' specifications."
  ;; Open files.
  (should
   (string= "#+BIND: variable value
#+DESCRIPTION: l2
#+LANGUAGE: en
#+SELECT_TAGS: b
#+TITLE: b
#+PROPERTY: a 1
" (org-file-contents (expand-file-name "setupfile3.org"
				       (concat org-test-dir "examples/")))))
  ;; Throw error when trying to access an invalid file.
  (should-error (org-file-contents "this-file-must-not-exist"))
  ;; Try to access an invalid file, but do not throw an error.
  (should
   (progn (org-file-contents "this-file-must-not-exist" :noerror) t))
  ;; Open URL.
  (should
   (string= "foo"
	    (let ((buffer (generate-new-buffer "url-retrieve-output")))
	      (unwind-protect
		  ;; Simulate successful retrieval of a URL.
		  (cl-letf (((symbol-function 'url-retrieve-synchronously)
			     (lambda (&rest_)
			       (with-current-buffer buffer
				 (insert "HTTP/1.1 200 OK\n\nfoo"))
			       buffer)))
		    (org-file-contents "http://some-valid-url"))
		(kill-buffer buffer)))))
  ;; Throw error when trying to access an invalid URL.
  (should-error
   (let ((buffer (generate-new-buffer "url-retrieve-output")))
     (unwind-protect
	 ;; Simulate unsuccessful retrieval of a URL.
	 (cl-letf (((symbol-function 'url-retrieve-synchronously)
		    (lambda (&rest_)
		      (with-current-buffer buffer
			(insert "HTTP/1.1 404 Not found\n\ndoes not matter"))
		      buffer)))
	   (org-file-contents "http://this-url-must-not-exist"))
       (kill-buffer buffer))))
  ;; Try to access an invalid URL, but do not throw an error.
  (should-error
   (let ((buffer (generate-new-buffer "url-retrieve-output")))
     (unwind-protect
	 ;; Simulate unsuccessful retrieval of a URL.
	 (cl-letf (((symbol-function 'url-retrieve-synchronously)
		    (lambda (&rest_)
		      (with-current-buffer buffer
			(insert "HTTP/1.1 404 Not found\n\ndoes not matter"))
		      buffer)))
	   (org-file-contents "http://this-url-must-not-exist"))
       (kill-buffer buffer))))
  (should
   (let ((buffer (generate-new-buffer "url-retrieve-output")))
     (unwind-protect
	 ;; Simulate unsuccessful retrieval of a URL.
	 (cl-letf (((symbol-function 'url-retrieve-synchronously)
		    (lambda (&rest_)
		      (with-current-buffer buffer
			(insert "HTTP/1.1 404 Not found\n\ndoes not matter"))
		      buffer)))
	   (org-file-contents "http://this-url-must-not-exist" :noerror))
       (kill-buffer buffer))
     t)))


;;; Navigation

(ert-deftest test-org/next-visible-heading ()
  "Test `org-next-visible-heading' specifications."
  ;; Move to the beginning of the next headline, taking into
  ;; consideration ARG.
  (should
   (org-test-with-temp-text "* H1\n* H2"
     (org-next-visible-heading 1)
     (looking-at "\\* H2")))
  (should
   (org-test-with-temp-text "* H1\n* H2\n* H3"
     (org-next-visible-heading 2)
     (looking-at "\\* H3")))
  ;; Ignore invisible headlines.
  (should
   (org-test-with-temp-text "* H1\n** H2\n* H3"
     (org-cycle)
     (org-next-visible-heading 1)
     (looking-at "\\* H3")))
  ;; Move point between headlines, not on blank lines between.
  (should
   (org-test-with-temp-text "* H1\n** H2\n\n\n\n* H3"
     (let ((org-cycle-separator-lines 1))
       (org-cycle)
       (org-next-visible-heading 1))
     (looking-at "\\* H3")))
  ;; Move at end of buffer when there is no more headline.
  (should
   (org-test-with-temp-text "* H1"
     (org-next-visible-heading 1)
     (eobp)))
  (should
   (org-test-with-temp-text "* H1\n* H2"
     (org-next-visible-heading 2)
     (eobp)))
  ;; With a negative argument, move backwards.
  (should
   (org-test-with-temp-text "* H1\n* H2\n<point>* H3"
     (org-next-visible-heading -1)
     (looking-at "\\* H2")))
  (should
   (org-test-with-temp-text "* H1\n* H2\n<point>* H3"
     (org-next-visible-heading -2)
     (looking-at "\\* H1"))))

(ert-deftest test-org/previous-visible-heading ()
  "Test `org-previous-visible-heading' specifications."
  ;; Move to the beginning of the next headline, taking into
  ;; consideration ARG.
  (should
   (org-test-with-temp-text "* H1\n<point>* H2"
     (org-previous-visible-heading 1)
     (looking-at "\\* H1")))
  (should
   (org-test-with-temp-text "* H1\n* H2\n<point>* H3"
     (org-previous-visible-heading 2)
     (looking-at "\\* H1")))
  ;; Ignore invisible headlines.
  (should
   (org-test-with-temp-text "* H1\n** H2\n<point>* H3"
     (org-overview)
     (org-previous-visible-heading 1)
     (looking-at "\\* H1")))
  ;; Move point between headlines, not on blank lines between.
  (should
   (org-test-with-temp-text "* H1\n\n\n\n** H2\n<point>* H3"
     (let ((org-cycle-separator-lines 1))
       (org-overview)
       (org-previous-visible-heading 1))
     (looking-at "\\* H1")))
  ;; Move at end of buffer when there is no more headline.
  (should
   (org-test-with-temp-text "* H1"
     (org-previous-visible-heading 1)
     (bobp)))
  (should
   (org-test-with-temp-text "* H1\n* <point>H2"
     (org-previous-visible-heading 2)
     (bobp)))
  ;; Invisible parts may not start at a headline, i.e., when revealing
  ;; parts of the buffer.  Handle this.
  (should
   (org-test-with-temp-text "* Main\n** H1\nFoo\n** H2\nBar\n** H3\nBaz"
     (org-overview)
     (search-forward "H1")
     (org-show-context 'minimal)
     (org-cycle)
     (search-forward "H3")
     (org-show-context 'minimal)
     ;; At this point, buffer displays, with point at "|",
     ;;
     ;; * Main
     ;; ** H1
     ;;    Foo
     ;; ** H3|
     (org-previous-visible-heading 1)
     (looking-at "\\*+ H1"))))

(ert-deftest test-org/forward-heading-same-level ()
  "Test `org-forward-heading-same-level' specifications."
  ;; Test navigation at top level, forward and backward.
  (should
   (equal "* H2"
	  (org-test-with-temp-text "* H1\n* H2"
	    (org-forward-heading-same-level 1)
	    (buffer-substring-no-properties (point) (line-end-position)))))
  (should
   (equal "* H1"
	  (org-test-with-temp-text "* H1\n<point>* H2"
	    (org-forward-heading-same-level -1)
	    (buffer-substring-no-properties (point) (line-end-position)))))
  ;; Test navigation in a sub-tree, forward and backward.
  (should
   (equal "* H2"
	  (org-test-with-temp-text "* H1\n** H11\n** H12\n* H2"
	    (org-forward-heading-same-level 1)
	    (buffer-substring-no-properties (point) (line-end-position)))))
  (should
   (equal "* H1"
	  (org-test-with-temp-text "* H1\n** H11\n** H12\n<point>* H2"
	    (org-forward-heading-same-level -1)
	    (buffer-substring-no-properties (point) (line-end-position)))))
  ;; Stop at first or last sub-heading.
  (should-not
   (equal "* H2"
	  (org-test-with-temp-text "* H1\n** H11\n<point>** H12\n* H2"
	    (org-forward-heading-same-level 1)
	    (buffer-substring-no-properties (point) (line-end-position)))))
  (should-not
   (equal "* H2"
	  (org-test-with-temp-text "* H1\n<point>** H11\n** H12\n* H2"
	    (org-forward-heading-same-level -1)
	    (buffer-substring-no-properties (point) (line-end-position)))))
  ;; Allow multiple moves.
  (should
   (equal "* H3"
	  (org-test-with-temp-text "* H1\n* H2\n* H3"
	    (org-forward-heading-same-level 2)
	    (buffer-substring-no-properties (point) (line-end-position)))))
  (should
   (equal "* H1"
	  (org-test-with-temp-text "* H1\n* H2\n<point>* H3"
	    (org-forward-heading-same-level -2)
	    (buffer-substring-no-properties (point) (line-end-position)))))
  ;; Ignore spurious moves when first (or last) sibling is reached.
  (should
   (equal "** H3"
	  (org-test-with-temp-text "* First\n<point>** H1\n** H2\n** H3\n* Last"
	    (org-forward-heading-same-level 100)
	    (buffer-substring-no-properties (point) (line-end-position)))))
  (should
   (equal "** H1"
	  (org-test-with-temp-text "* First\n** H1\n** H2\n<point>** H3\n* Last"
	    (org-forward-heading-same-level -100)
	    (buffer-substring-no-properties (point) (line-end-position))))))

(ert-deftest test-org/end-of-meta-data ()
  "Test `org-end-of-meta-data' specifications."
  ;; Skip planning line.
  (should
   (org-test-with-temp-text "* Headline\nSCHEDULED: <2014-03-04 tue.>"
     (org-end-of-meta-data)
     (eobp)))
  ;; Skip properties drawer.
  (should
   (org-test-with-temp-text
       "* Headline\nSCHEDULED: <2014-03-04 tue.>\n:PROPERTIES:\n:A: 1\n:END:"
     (org-end-of-meta-data)
     (eobp)))
  ;; Skip both.
  (should
   (org-test-with-temp-text "* Headline\n:PROPERTIES:\n:A: 1\n:END:"
     (org-end-of-meta-data)
     (eobp)))
  ;; Nothing to skip, go to first line.
  (should
   (org-test-with-temp-text "* Headline\nContents"
     (org-end-of-meta-data)
     (looking-at "Contents")))
  ;; With option argument, skip empty lines, regular drawers and
  ;; clocking lines.
  (should
   (org-test-with-temp-text "* Headline\n\nContents"
     (org-end-of-meta-data t)
     (looking-at "Contents")))
  (should
   (org-test-with-temp-text "* Headline\nCLOCK:\nContents"
     (org-end-of-meta-data t)
     (looking-at "Contents")))
  (should
   (org-test-with-temp-text "* Headline\n:LOGBOOK:\nlogging\n:END:\nContents"
     (org-end-of-meta-data t)
     (looking-at "Contents")))
  ;; Special case: do not skip incomplete drawers.
  (should
   (org-test-with-temp-text "* Headline\n:LOGBOOK:\nlogging\nContents"
     (org-end-of-meta-data t)
     (looking-at ":LOGBOOK:")))
  ;; Special case: Be careful about consecutive headlines.
  (should-not
   (org-test-with-temp-text "* H1\n*H2\nContents"
     (org-end-of-meta-data t)
     (looking-at "Contents"))))

(ert-deftest test-org/shiftright-heading ()
  "Test `org-shiftright' on headings."
  (let ((org-todo-keywords '((sequence "TODO" "DONE"))))
    (should
     (equal "* TODO a1\n** a2\n* DONE b1\n"
	    (org-test-with-temp-text "* a1\n** a2\n* DONE b1\n"
	      (org-shiftright)
	      (buffer-string))))
    (should
     (equal "* TODO a1\n** TODO a2\n* b1\n"
    	    (org-test-with-temp-text "* a1\n** a2\n* DONE b1\n"
    	      (let ((org-loop-over-headlines-in-active-region t))
    		(transient-mark-mode 1)
    		(push-mark (point) t t)
    		(search-forward "* DONE b1")
    		(org-shiftright))
    	      (buffer-string))))
    (should
     (equal "* TODO a1\n** a2\n* b1\n"
    	    (org-test-with-temp-text "* a1\n** a2\n* DONE b1\n"
    	      (let ((org-loop-over-headlines-in-active-region 'start-level))
    		(transient-mark-mode 1)
    		(push-mark (point) t t)
    		(search-forward "* DONE b1")
    		(org-shiftright))
    	      (buffer-string))))))

(ert-deftest test-org/beginning-of-line ()
  "Test `org-beginning-of-line' specifications."
  ;; Move to beginning of line.  If current line in invisible, move to
  ;; beginning of visible line instead.
  (should
   (org-test-with-temp-text "Some text\nSome other text<point>"
     (org-beginning-of-line)
     (bolp)))
  (should
   (org-test-with-temp-text "* H1\n** H2<point>"
     (org-overview)
     (org-beginning-of-line)
     (= (line-beginning-position) 1)))
  ;; With `visual-line-mode' active, move to beginning of visual line.
  (should-not
   (org-test-with-temp-text "A <point>long line of text\nSome other text"
     (visual-line-mode)
     (dotimes (i 1000) (insert "very "))
     (org-beginning-of-line)
     (bolp)))
  ;; In a wide headline, with `visual-line-mode', prefer going to the
  ;; beginning of a visual line than to the logical beginning of line,
  ;; even if special movement is active.
  (should-not
   (org-test-with-temp-text "* A <point>long headline"
     (visual-line-mode)
     (dotimes (i 1000) (insert "very "))
     (goto-char (point-max))
     (org-beginning-of-line)
     (bobp)))
  (should-not
   (org-test-with-temp-text "* A <point>long headline"
     (visual-line-mode)
     (dotimes (i 1000) (insert "very "))
     (goto-char (point-max))
     (let ((org-special-ctrl-a/e t)) (org-beginning-of-line))
     (bobp)))
  ;; At an headline with special movement, first move at beginning of
  ;; title, then at the beginning of line, rinse, repeat.
  (should
   (org-test-with-temp-text "* TODO Headline<point>"
     (let ((org-special-ctrl-a/e t))
       (and (progn (org-beginning-of-line) (looking-at-p "Headline"))
	    (progn (org-beginning-of-line) (bolp))
	    (progn (org-beginning-of-line) (looking-at-p "Headline"))))))
  (should
   (org-test-with-temp-text "* TODO [#A] Headline<point>"
     (let ((org-special-ctrl-a/e t))
       (org-beginning-of-line)
       (looking-at "Headline"))))
  (should
   (org-test-with-temp-text "* TODO [#A] Headline<point>"
     (let ((org-special-ctrl-a/e '(t . nil)))
       (org-beginning-of-line)
       (looking-at "Headline"))))
  (should-not
   (org-test-with-temp-text "* TODO [#A] Headline<point>"
     (let ((org-special-ctrl-a/e '(nil . nil)))
       (org-beginning-of-line)
       (looking-at "Headline"))))
  ;; At an headline with reversed movement, first move to beginning of
  ;; line, then to the beginning of title.
  (should
   (org-test-with-temp-text "* TODO Headline<point>"
     (let ((org-special-ctrl-a/e 'reversed)
	   (this-command last-command))
       (and (progn (org-beginning-of-line) (bolp))
	    (progn (org-beginning-of-line) (looking-at-p "Headline"))))))
  (should
   (org-test-with-temp-text "* TODO Headline<point>"
     (let ((org-special-ctrl-a/e '(reversed . nil))
	   (this-command last-command))
       (and (progn (org-beginning-of-line) (bolp))
	    (progn (org-beginning-of-line) (looking-at-p "Headline"))))))
  (should-not
   (org-test-with-temp-text "* TODO Headline<point>"
     (let ((org-special-ctrl-a/e '(t . nil))
	   (this-command last-command))
       (and (progn (org-beginning-of-line) (bolp))
	    (progn (org-beginning-of-line) (looking-at-p "Headline"))))))
  ;; At an item with special movement, first move after to beginning
  ;; of title, then to the beginning of line, rinse, repeat.
  (should
   (org-test-with-temp-text "- [ ] Item<point>"
     (let ((org-special-ctrl-a/e t))
       (and (progn (org-beginning-of-line) (looking-at-p "Item"))
	    (progn (org-beginning-of-line) (bolp))
	    (progn (org-beginning-of-line) (looking-at-p "Item"))))))
  ;; At an item with reversed movement, first move to beginning of
  ;; line, then to the beginning of title.
  (should
   (org-test-with-temp-text "- [X] Item<point>"
     (let ((org-special-ctrl-a/e 'reversed)
	   (this-command last-command))
       (and (progn (org-beginning-of-line) (bolp))
	    (progn (org-beginning-of-line) (looking-at-p "Item"))))))
  ;; Leave point before invisible characters at column 0.
  (should
   (org-test-with-temp-text "[[https://orgmode.org]]<point>"
     (let ((org-special-ctrl-a/e nil))
       (org-beginning-of-line)
       (bolp))))
  (should
   (org-test-with-temp-text "[[https://orgmode.org]]<point>"
     (let ((org-special-ctrl-a/e t))
       (org-beginning-of-line)
       (bolp))))
  (should
   (org-test-with-temp-text "[[http<point>://orgmode.org]]"
     (visual-line-mode)
     (org-beginning-of-line)
     (bolp)))
  ;; Special case: Do not error when the buffer contains only a single
  ;; asterisk.
  (should
   (org-test-with-temp-text "*<point>"
     (let ((org-special-ctrl-a/e t)) (org-beginning-of-line) t)))
  (should
   (org-test-with-temp-text "*<point>"
     (let ((org-special-ctrl-a/e nil)) (org-beginning-of-line) t))))

(ert-deftest test-org/end-of-line ()
  "Test `org-end-of-line' specifications."
  ;; Standard test.
  (should
   (org-test-with-temp-text "Some text\nSome other text"
     (org-end-of-line)
     (eolp)))
  ;; With `visual-line-mode' active, move to end of visible line.
  ;; However, never go past ellipsis.
  (should-not
   (org-test-with-temp-text "A <point>long line of text\nSome other text"
     (visual-line-mode)
     (dotimes (i 1000) (insert "very "))
     (goto-char (point-min))
     (org-end-of-line)
     (eolp)))
  (should-not
   (org-test-with-temp-text "* A short headline\nSome contents"
     (visual-line-mode)
     (org-overview)
     (org-end-of-line)
     (eobp)))
  ;; In a wide headline, with `visual-line-mode', prefer going to end
  ;; of visible line if tags, or end of line, are farther.
  (should-not
   (org-test-with-temp-text "* A <point>long headline"
     (visual-line-mode)
     (dotimes (i 1000) (insert "very "))
     (goto-char (point-min))
     (org-end-of-line)
     (eolp)))
  (should-not
   (org-test-with-temp-text "* A <point>long headline :tag:"
     (visual-line-mode)
     (dotimes (i 1000) (insert "very "))
     (goto-char (point-min))
     (org-end-of-line)
     (eolp)))
  ;; At an headline without special movement, go to end of line.
  ;; However, never go past ellipsis.
  (should
   (org-test-with-temp-text "* Headline2b :tag:\n"
     (let ((org-special-ctrl-a/e nil))
       (and (progn (org-end-of-line) (eolp))
	    (progn (org-end-of-line) (eolp))))))
  (should
   (org-test-with-temp-text "* Headline2b :tag:\n"
     (let ((org-special-ctrl-a/e '(t . nil)))
       (and (progn (org-end-of-line) (eolp))
	    (progn (org-end-of-line) (eolp))))))
  (should
   (org-test-with-temp-text "* Headline2a :tag:\n** Sub"
     (org-overview)
     (let ((org-special-ctrl-a/e nil))
       (org-end-of-line)
       (= 1 (line-beginning-position)))))
  ;; At an headline with special movement, first move before tags,
  ;; then at the end of line, rinse, repeat.  However, never go past
  ;; ellipsis.
  (should
   (org-test-with-temp-text "* Headline1 :tag:\n"
     (let ((org-special-ctrl-a/e t))
       (and (progn (org-end-of-line) (looking-at-p " :tag:"))
	    (progn (org-end-of-line) (eolp))
	    (progn (org-end-of-line) (looking-at-p " :tag:"))))))
  (should
   (org-test-with-temp-text "* Headline1 :tag:\n"
     (let ((org-special-ctrl-a/e '(nil . t)))
       (and (progn (org-end-of-line) (looking-at-p " :tag:"))
	    (progn (org-end-of-line) (eolp))
	    (progn (org-end-of-line) (looking-at-p " :tag:"))))))
  (should-not
   (org-test-with-temp-text "* Headline1 :tag:\n"
     (let ((org-special-ctrl-a/e '(nil . nil)))
       (and (progn (org-end-of-line) (looking-at-p " :tag:"))
	    (progn (org-end-of-line) (eolp))
	    (progn (org-end-of-line) (looking-at-p " :tag:"))))))
  (should
   (org-test-with-temp-text "* Headline2a :tag:\n** Sub"
     (org-overview)
     (let ((org-special-ctrl-a/e t))
       (org-end-of-line)
       (org-end-of-line)
       (= 1 (line-beginning-position)))))
  ;; At an headline, with reversed movement, first go to end of line,
  ;; then before tags.  However, never go past ellipsis.
  (should
   (org-test-with-temp-text "* Headline3 :tag:\n"
     (let ((org-special-ctrl-a/e 'reversed)
	   (this-command last-command))
       (and (progn (org-end-of-line) (eolp))
	    (progn (org-end-of-line) (looking-at-p " :tag:"))))))
  (should
   (org-test-with-temp-text "* Headline3 :tag:\n"
     (let ((org-special-ctrl-a/e '(nil . reversed))
	   (this-command last-command))
       (and (progn (org-end-of-line) (eolp))
	    (progn (org-end-of-line) (looking-at-p " :tag:"))))))
  (should-not
   (org-test-with-temp-text "* Headline3 :tag:\n"
     (let ((org-special-ctrl-a/e '(nil . t))
	   (this-command last-command))
       (and (progn (org-end-of-line) (eolp))
	    (progn (org-end-of-line) (looking-at-p " :tag:"))))))
  (should
   (org-test-with-temp-text "* Headline2a :tag:\n** Sub"
     (org-overview)
     (let ((org-special-ctrl-a/e 'reversed))
       (org-end-of-line)
       (= 1 (line-beginning-position)))))
  ;; At a block without hidden contents.
  (should
   (org-test-with-temp-text "#+BEGIN_CENTER\nContents\n#+END_CENTER"
     (progn (org-end-of-line) (eolp))))
  ;; At a block with hidden contents.
  (should-not
   (org-test-with-temp-text "#+BEGIN_CENTER\nContents\n#+END_CENTER"
     (let ((org-special-ctrl-a/e t))
       (org-hide-block-toggle)
       (org-end-of-line)
       (eobp))))
  ;; Get past invisible characters at the end of line.
  (should
   (org-test-with-temp-text "[[https://orgmode.org]]"
     (org-end-of-line)
     (eolp))))

(ert-deftest test-org/open-line ()
  "Test `org-open-line' specifications."
  ;; Call `open-line' outside of tables.
  (should
   (equal "\nText"
	  (org-test-with-temp-text "Text"
	    (org-open-line 1)
	    (buffer-string))))
  ;; At a table, create a row above.
  (should
   (equal "\n|   |\n| a |"
	  (org-test-with-temp-text "\n<point>| a |"
	    (org-open-line 1)
	    (buffer-string))))
  ;; At the very first character of the buffer, also call `open-line'.
  (should
   (equal "\n| a |"
	  (org-test-with-temp-text "| a |"
	    (org-open-line 1)
	    (buffer-string))))
  ;; Narrowing does not count.
  (should
   (equal "Text\n|   |\n| a |"
	  (org-test-with-temp-text "Text\n<point>| a |"
	    (narrow-to-region (point) (point-max))
	    (org-open-line 1)
	    (widen)
	    (buffer-string)))))

(ert-deftest test-org/forward-sentence ()
  "Test `org-forward-sentence' specifications."
  ;; At the end of a table cell, move to the end of the next one.
  (should
   (org-test-with-temp-text "| a<point> | b |"
     (org-forward-sentence)
     (looking-at " |$")))
  ;; Elsewhere in a cell, move to its end.
  (should
   (org-test-with-temp-text "| a<point>c | b |"
     (org-forward-sentence)
     (looking-at " | b |$")))
  ;; Otherwise, simply call `forward-sentence'.
  (should
   (org-test-with-temp-text "Sentence<point> 1.  Sentence 2."
     (org-forward-sentence)
     (looking-at "  Sentence 2.")))
  (should
   (org-test-with-temp-text "Sentence<point> 1.  Sentence 2."
     (org-forward-sentence)
     (org-forward-sentence)
     (eobp)))
  ;; At the end of an element, jump to the next one, without stopping
  ;; on blank lines in-between.
  (should
   (org-test-with-temp-text "Paragraph 1.<point>\n\nParagraph 2."
     (org-forward-sentence)
     (eobp)))
  ;; Headlines are considered to be sentences by themselves, even if
  ;; they do not end with a full stop.
  (should
   (equal
    "* Headline"
    (org-test-with-temp-text "* <point>Headline\nSentence."
      (org-forward-sentence)
      (buffer-substring-no-properties (line-beginning-position) (point)))))
  (should
   (org-test-with-temp-text "* Headline<point>\nSentence."
     (org-forward-sentence)
     (eobp)))
  (should
   (org-test-with-temp-text "Sentence.<point>\n\n* Headline\n\nSentence 2."
     (org-forward-sentence)
     (and (org-at-heading-p) (eolp)))))

(ert-deftest test-org/backward-sentence ()
  "Test `org-backward-sentence' specifications."
  ;; At the beginning of a table cell, move to the beginning of the
  ;; previous one.
  (should
   (org-test-with-temp-text "| a | <point>b |"
     (org-backward-sentence)
     (looking-at "a | b |$")))
  ;; Elsewhere in a cell, move to its beginning.
  (should
   (org-test-with-temp-text "| a | b<point>c |"
     (org-backward-sentence)
     (looking-at "bc |$")))
  ;; Otherwise, simply call `backward-sentence'.
  (should
   (org-test-with-temp-text "Sentence 1.  Sentence<point> 2."
     (org-backward-sentence)
     (looking-at "Sentence 2.")))
  (should
   (org-test-with-temp-text "Sentence 1.  Sentence<point> 2."
     (org-backward-sentence)
     (org-backward-sentence)
     (bobp)))
  ;; Make sure to hit the beginning of a sentence on the same line as
  ;; an item.
  (should
   (org-test-with-temp-text "- Line 1\n  line <point>2."
     (org-backward-sentence)
     (looking-at "Line 1"))))

(ert-deftest test-org/forward-paragraph ()
  "Test `org-forward-paragraph' specifications."
  ;; At end of buffer, do not return an error.
  (should
   (org-test-with-temp-text "Paragraph"
     (goto-char (point-max))
     (org-forward-paragraph)
     t))
  ;; Standard test.
  (should
   (= 2
      (org-test-with-temp-text "P1\n\nP2"
	(org-forward-paragraph)
	(org-current-line))))
  (should
   (= 2
      (org-test-with-temp-text "P1\n\nP2\n\nP3"
	(org-forward-paragraph)
	(org-current-line))))
  ;; Enter greater elements.
  (should
   (= 2
      (org-test-with-temp-text "#+begin_center\nP1\n#+end_center\nP2"
	(org-forward-paragraph)
	(org-current-line))))
  ;; Do not enter elements with invisible contents.
  (should
   (= 4
      (org-test-with-temp-text "* H1\n  P1\n\n* H2"
	(org-cycle)
	(org-forward-paragraph)
	(org-current-line))))
  (should
   (= 6
      (org-test-with-temp-text "#+begin_center\nP1\n\nP2\n#+end_center\nP3"
	(org-hide-block-toggle)
	(org-forward-paragraph)
	(org-current-line))))
  ;; On an item or a footnote definition, move past the first element
  ;; inside, if any.
  (should
   (= 2
      (org-test-with-temp-text "- Item1\n\n  Paragraph\n- Item2"
	(org-forward-paragraph)
	(org-current-line))))
  (should
   (= 2
      (org-test-with-temp-text "[fn:1] Def1\n\nParagraph\n\n[fn:2] Def2"
	(org-forward-paragraph)
	(org-current-line))))
  ;; On a table (resp. a property drawer) do not move through table
  ;; rows (resp. node properties).
  (should
   (org-test-with-temp-text "| a | b |\n| c | d |\nParagraph"
     (org-forward-paragraph)
     (looking-at "Paragraph")))
  (should
   (org-test-with-temp-text
       "* H\n<point>:PROPERTIES:\n:prop: value\n:END:\nParagraph"
     (org-forward-paragraph)
     (looking-at "Paragraph")))
  ;; Skip consecutive keywords, clocks and diary S-exps.
  (should
   (org-test-with-temp-text "#+key: val\n  #+key2: val\n#+key3: val\n"
     (org-forward-paragraph)
     (eobp)))
  (should
   (org-test-with-temp-text "CLOCK: val\n  CLOCK: val\nCLOCK: val\n"
     (org-forward-paragraph)
     (eobp)))
  (should
   (org-test-with-temp-text "%%(foo)\n%%(bar)\n%%(baz)\n"
     (org-forward-paragraph)
     (eobp)))
  (should-not
   (org-test-with-temp-text "#+key: val\n  #+key2: val\n\n#+key3: val\n"
     (org-forward-paragraph)
     (eobp)))
  (should-not
   (org-test-with-temp-text "#+key: val\nCLOCK: ...\n"
     (org-forward-paragraph)
     (eobp)))
  ;; In a plain list with one item every line, skip the whole list,
  ;; even with point in the middle of the list.
  (should
   (org-test-with-temp-text "- A\n  - B\n- C\n"
     (org-forward-paragraph)
     (eobp)))
  (should
   (org-test-with-temp-text "- A\n  - <point>B\n- C\n"
     (org-forward-paragraph)
     (eobp)))
  ;; On a comment, verse or source block, stop at "contents"
  ;; boundaries and blank lines.
  (should
   (= 2
      (org-test-with-temp-text "#+begin_src emacs-lisp\nL1\n\nL2\n#+end_src"
	(org-forward-paragraph)
	(org-current-line))))
  (should
   (= 3
      (org-test-with-temp-text "#+begin_verse\n<point>L1\n\nL2\n#+end_verse"
	(org-forward-paragraph)
	(org-current-line))))
  (should
   (= 5
      (org-test-with-temp-text "#+begin_comment\nL1\n\n<point>L2\n#+end_comment"
	(org-forward-paragraph)
	(org-current-line))))
  ;; Being on an affiliated keyword shouldn't make any difference.
  (should
   (org-test-with-temp-text "#+name: para\n#+caption: caption\nPara"
     (org-forward-paragraph)
     (eobp))))

(ert-deftest test-org/backward-paragraph ()
  "Test `org-backward-paragraph' specifications."
  ;; Do not error at beginning of buffer.
  (should
   (org-test-with-temp-text "Paragraph"
     (org-backward-paragraph)
     t))
  ;; At blank lines at the very beginning of a buffer, move to
  ;; point-min.
  (should
   (org-test-with-temp-text "\n\n<point>\n\nParagraph"
     (org-backward-paragraph)
     (bobp)))
  ;; Regular test.
  (should
   (= 2
      (org-test-with-temp-text "P1\n\nP2<point>"
	(org-backward-paragraph)
	(org-current-line))))
  (should
   (= 4
      (org-test-with-temp-text "P1\n\nP2\n\nP3<point>"
	(org-backward-paragraph)
	(org-current-line))))
  ;; Try to move on the line above current element.
  (should
   (= 2
      (org-test-with-temp-text "\n\n<point>Paragraph"
	(org-backward-paragraph)
	(org-current-line))))
  ;; Do not leave point in an invisible area.
  (should
   (org-test-with-temp-text "* H1\n  P1\n\n* H2"
     (org-cycle)
     (goto-char (point-max))
     (beginning-of-line)
     (org-backward-paragraph)
     (bobp)))
  (should
   (org-test-with-temp-text "#+begin_center\nP1\n\nP2\n#+end_center\n"
     (org-hide-block-toggle)
     (goto-char (point-max))
     (org-backward-paragraph)
     (bobp)))
  ;; On the first element in an item or a footnote definition, jump
  ;; before the footnote or the item.
  (should
   (org-test-with-temp-text "- line1<point>"
     (org-backward-paragraph)
     (bobp)))
  (should
   (org-test-with-temp-text "[fn:1] line1n<point>"
     (org-backward-paragraph)
     (bobp)))
  ;; On the second element in an item or a footnote definition, jump
  ;; to item or the definition.
  (should
   (= 2
      (org-test-with-temp-text "- line1\n\n<point>  line2"
	(org-backward-paragraph)
	(org-current-line))))
  (should
   (= 2
      (org-test-with-temp-text "[fn:1] line1\n\n<point>  line2"
	(org-backward-paragraph)
	(org-current-line))))
  ;; On a table (resp. a property drawer), ignore table rows
  ;; (resp. node properties).
  (should
   (org-test-with-temp-text "| a | b |\n| c | d |\n<point>P1"
     (org-backward-paragraph)
     (bobp)))
  (should
   (= 2
      (org-test-with-temp-text
	  "* H\n:PROPERTIES:\n:prop: value\n:END:\n<point>P1"
	(org-backward-paragraph)
	(org-current-line))))
  ;; In a plain list with one item every line, skip the whole list,
  ;; even with point in the middle of the list.
  (should
   (org-test-with-temp-text "- A\n  - B\n- C\n<point>"
     (org-backward-paragraph)
     (bobp)))
  (should
   (org-test-with-temp-text "- A\n  - B\n- <point>C\n"
     (org-backward-paragraph)
     (bobp)))
  ;; Skip consecutive keywords, clocks and diary S-exps.
  (should
   (org-test-with-temp-text "#+key: val\n  #+key2: val\n#+key3: val\n<point>"
     (org-backward-paragraph)
     (bobp)))
  (should
   (org-test-with-temp-text "CLOCK: val\n  CLOCK: val\nCLOCK: val\n<point>"
     (org-backward-paragraph)
     (bobp)))
  (should
   (org-test-with-temp-text "%%(foo)\n%%(bar)\n%%(baz)\n<point>"
     (org-backward-paragraph)
     (bobp)))
  (should-not
   (org-test-with-temp-text "#+key: val\n  #+key2: val\n\n#+key3: val\n<point>"
     (org-backward-paragraph)
     (bobp)))
  (should-not
   (org-test-with-temp-text "#+key: val\nCLOCK: ...\n<point>"
     (org-backward-paragraph)
     (bobp)))
  ;; On a comment, example, source and verse blocks, stop at blank
  ;; lines.
  (should
   (= 1
      (org-test-with-temp-text
	  "#+begin_comment\n<point>L1\n\nL2\n\nL3\n#+end_comment"
	(org-backward-paragraph)
	(org-current-line))))
  (should
   (= 2
      (org-test-with-temp-text
	  "#+begin_verse\nL1\n\n<point>L2\n\nL3\n#+end_verse"
	(org-backward-paragraph)
	(org-current-line))))
  (should
   (= 3
      (org-test-with-temp-text
	  "#+begin_src emacs-lisp\nL1\n\nL2\n\n<point>L3\n#+end_src"
	(org-backward-paragraph)
	(org-current-line))))
  ;; When called from the opening line itself, however, move to
  ;; beginning of block.
  (should
   (org-test-with-temp-text "#+begin_<point>example\nL1\n#+end_example"
     (org-backward-paragraph)
     (bobp)))
  ;; On an empty heading, move above it.
  (should
   (org-test-with-temp-text "\n* <point>"
     (org-backward-paragraph)
     (bobp)))
  (should
   (org-test-with-temp-text "\n* \n<point>"
     (org-backward-paragraph)
     (bobp))))

(ert-deftest test-org/forward-element ()
  "Test `org-forward-element' specifications."
  ;; 1. At EOB: should error.
  (org-test-with-temp-text "Some text\n"
    (goto-char (point-max))
    (should-error (org-forward-element)))
  ;; 2. Standard move: expected to ignore blank lines.
  (org-test-with-temp-text "First paragraph.\n\n\nSecond paragraph."
    (org-forward-element)
    (should (looking-at (regexp-quote "Second paragraph."))))
  ;; 3. Headline tests.
  (org-test-with-temp-text "
* Head 1
** Head 1.1
*** Head 1.1.1
** Head 1.2"
    ;; 3.1. At an headline beginning: move to next headline at the
    ;;      same level.
    (goto-line 3)
    (org-forward-element)
    (should (looking-at (regexp-quote "** Head 1.2")))
    ;; 3.2. At an headline beginning: move to parent headline if no
    ;;      headline at the same level.
    (goto-line 3)
    (org-forward-element)
    (should (looking-at (regexp-quote "** Head 1.2"))))
  ;; 4. Greater element tests.
  (org-test-with-temp-text
      "#+BEGIN_CENTER\nInside.\n#+END_CENTER\n\nOutside."
    ;; 4.1. At a greater element: expected to skip contents.
    (org-forward-element)
    (should (looking-at (regexp-quote "Outside.")))
    ;; 4.2. At the end of greater element contents: expected to skip
    ;;      to the end of the greater element.
    (goto-line 2)
    (org-forward-element)
    (should (looking-at (regexp-quote "Outside."))))
  ;; 5. List tests.
  (org-test-with-temp-text "
- item1

  - sub1

  - sub2

  - sub3

  Inner paragraph.

- item2

Outside."
    ;; 5.1. At list top point: expected to move to the element after
    ;;      the list.
    (goto-line 2)
    (org-forward-element)
    (should (looking-at (regexp-quote "Outside.")))
    ;; 5.2. Special case: at the first line of a sub-list, but not at
    ;;      beginning of line, move to next item.
    (goto-line 2)
    (forward-char)
    (org-forward-element)
    (should (looking-at "- item2"))
    (goto-line 4)
    (forward-char)
    (org-forward-element)
    (should (looking-at "  - sub2"))
    ;; 5.3 At sub-list beginning: expected to move after the sub-list.
    (goto-line 4)
    (org-forward-element)
    (should (looking-at (regexp-quote "  Inner paragraph.")))
    ;; 5.4. At sub-list end: expected to move outside the sub-list.
    (goto-line 8)
    (org-forward-element)
    (should (looking-at (regexp-quote "  Inner paragraph.")))
    ;; 5.5. At an item: expected to move to next item, if any.
    (goto-line 6)
    (org-forward-element)
    (should (looking-at "  - sub3"))))

(ert-deftest test-org/backward-element ()
  "Test `org-backward-element' specifications."
  ;; 1. Should error at BOB.
  (org-test-with-temp-text "    \nParagraph."
    (should-error (org-backward-element)))
  ;; 2. Should move at BOB when called on the first element in buffer.
  (should
   (org-test-with-temp-text "\n#+TITLE: test"
     (progn (forward-line)
	    (org-backward-element)
	    (bobp))))
  ;; 3. Not at the beginning of an element: move at its beginning.
  (org-test-with-temp-text "Paragraph1.\n\nParagraph2."
    (goto-line 3)
    (end-of-line)
    (org-backward-element)
    (should (looking-at (regexp-quote "Paragraph2."))))
  ;; 4. Headline tests.
  (org-test-with-temp-text "
* Head 1
** Head 1.1
*** Head 1.1.1
** Head 1.2"
    ;; 4.1. At an headline beginning: move to previous headline at the
    ;;      same level.
    (goto-line 5)
    (org-backward-element)
    (should (looking-at (regexp-quote "** Head 1.1")))
    ;; 4.2. At an headline beginning: move to parent headline if no
    ;;      headline at the same level.
    (goto-line 3)
    (org-backward-element)
    (should (looking-at (regexp-quote "* Head 1")))
    ;; 4.3. At the first top-level headline: should error.
    (goto-line 2)
    (should-error (org-backward-element)))
  ;; 5. At beginning of first element inside a greater element:
  ;;    expected to move to greater element's beginning.
  (org-test-with-temp-text "Before.\n#+BEGIN_CENTER\nInside.\n#+END_CENTER"
    (goto-line 3)
    (org-backward-element)
    (should (looking-at "#\\+BEGIN_CENTER")))
  ;; 6. At the beginning of the first element in a section: should
  ;;    move back to headline, if any.
  (should
   (org-test-with-temp-text "#+TITLE: test\n* Headline\n\nParagraph"
     (progn (goto-char (point-max))
	    (beginning-of-line)
	    (org-backward-element)
	    (org-at-heading-p))))
  ;; 7. List tests.
  (org-test-with-temp-text "
- item1

  - sub1

  - sub2

  - sub3

  Inner paragraph.

- item2


Outside."
    ;; 7.1. At beginning of sub-list: expected to move to the
    ;;      paragraph before it.
    (goto-line 4)
    (org-backward-element)
    (should (looking-at "item1"))
    ;; 7.2. At an item in a list: expected to move at previous item.
    (goto-line 8)
    (org-backward-element)
    (should (looking-at "  - sub2"))
    (goto-line 12)
    (org-backward-element)
    (should (looking-at "- item1"))
    ;; 7.3. At end of list/sub-list: expected to move to list/sub-list
    ;;      beginning.
    (goto-line 10)
    (org-backward-element)
    (should (looking-at "  - sub1"))
    (goto-line 15)
    (org-backward-element)
    (should (looking-at "- item1"))
    ;; 7.4. At blank-lines before list end: expected to move to top
    ;; item.
    (goto-line 14)
    (org-backward-element)
    (should (looking-at "- item1"))))

(ert-deftest test-org/up-element ()
  "Test `org-up-element' specifications."
  ;; 1. At BOB or with no surrounding element: should error.
  (org-test-with-temp-text "Paragraph."
    (should-error (org-up-element)))
  (org-test-with-temp-text "* Head1\n* Head2"
    (goto-line 2)
    (should-error (org-up-element)))
  (org-test-with-temp-text "Paragraph1.\n\nParagraph2."
    (goto-line 3)
    (should-error (org-up-element)))
  ;; 2. At an headline: move to parent headline.
  (org-test-with-temp-text "* Head1\n** Sub-Head1\n** Sub-Head2"
    (goto-line 3)
    (org-up-element)
    (should (looking-at "\\* Head1")))
  ;; 3. Inside a greater element: move to greater element beginning.
  (org-test-with-temp-text
      "Before.\n#+BEGIN_CENTER\nParagraph1\nParagraph2\n#+END_CENTER\n"
    (goto-line 3)
    (org-up-element)
    (should (looking-at "#\\+BEGIN_CENTER")))
  ;; 4. List tests.
  (org-test-with-temp-text "* Top
- item1

  - sub1

  - sub2

    Paragraph within sub2.

- item2"
    ;; 4.1. Within an item: move to the item beginning.
    (goto-line 8)
    (org-up-element)
    (should (looking-at "  - sub2"))
    ;; 4.2. At an item in a sub-list: move to parent item.
    (goto-line 4)
    (org-up-element)
    (should (looking-at "- item1"))
    ;; 4.3. At an item in top list: move to beginning of whole list.
    (goto-line 10)
    (org-up-element)
    (should (looking-at "- item1"))
    ;; 4.4. Special case.  At very top point: should move to parent of
    ;;      list.
    (goto-line 2)
    (org-up-element)
    (should (looking-at "\\* Top"))))

(ert-deftest test-org/down-element ()
  "Test `org-down-element' specifications."
  ;; Error when the element hasn't got a recursive type.
  (org-test-with-temp-text "Paragraph."
    (should-error (org-down-element)))
  ;; Error when the element has no contents
  (org-test-with-temp-text "* Headline"
    (should-error (org-down-element)))
  ;; When at a plain-list, move to first item.
  (org-test-with-temp-text "- Item 1\n  - Item 1.1\n  - Item 2.2"
    (goto-line 2)
    (org-down-element)
    (should (looking-at " - Item 1.1")))
  (org-test-with-temp-text "#+NAME: list\n- Item 1"
    (org-down-element)
    (should (looking-at " Item 1")))
  ;; When at a table, move to first row
  (org-test-with-temp-text "#+NAME: table\n| a | b |"
    (org-down-element)
    (should (looking-at " a | b |")))
  ;; Otherwise, move inside the greater element.
  (org-test-with-temp-text "#+BEGIN_CENTER\nParagraph.\n#+END_CENTER"
    (org-down-element)
    (should (looking-at "Paragraph"))))

(ert-deftest test-org/drag-element-backward ()
  "Test `org-drag-element-backward' specifications."
  ;; Standard test.
  (should
   (equal
    "#+key2: val2\n#+key1: val1\n#+key3: val3"
    (org-test-with-temp-text "#+key1: val1\n<point>#+key2: val2\n#+key3: val3"
      (org-drag-element-backward)
      (buffer-string))))
  (should
   (equal
    "#+BEGIN_CENTER\n#+B: 2\n#+A: 1\n#+END_CENTER"
    (org-test-with-temp-text
	"#+BEGIN_CENTER\n#+A: 1\n<point>#+B: 2\n#+END_CENTER"
      (org-drag-element-backward)
      (buffer-string))))
  ;; Preserve blank lines.
  (should
   (equal "Paragraph 2\n\n\nPara1\n\nPara3"
	  (org-test-with-temp-text "Para1\n\n\n<point>Paragraph 2\n\nPara3"
	    (org-drag-element-backward)
	    (buffer-string))))
  ;; Preserve column.
  (should
   (org-test-with-temp-text "#+key1: v\n#+key<point>2: v\n#+key3: v"
     (org-drag-element-backward)
     (looking-at-p "2")))
  ;; Error when trying to move first element of buffer.
  (should-error
   (org-test-with-temp-text "Paragraph 1.\n\nParagraph 2."
     (org-drag-element-backward))
   :type 'user-error)
  ;; Error when trying to swap nested elements.
  (should-error
   (org-test-with-temp-text "#+BEGIN_CENTER\n<point>Test.\n#+END_CENTER"
     (org-drag-element-backward))
   :type 'user-error)
  ;; Error when trying to swap an headline element and a non-headline
  ;; element.
  (should-error
   (org-test-with-temp-text "Test.\n<point>* Head 1"
     (org-drag-element-backward))
   :type 'error)
  ;; Error when called before first element.
  (should-error
   (org-test-with-temp-text "\n<point>"
     (org-drag-element-backward))
   :type 'user-error)
  ;; Preserve visibility of elements and their contents.
  (should
   (equal '((63 . 82) (26 . 48))
	  (org-test-with-temp-text "
#+BEGIN_CENTER
Text.
#+END_CENTER
- item 1
  #+BEGIN_QUOTE
  Text.
  #+END_QUOTE"
	    (while (search-forward "BEGIN_" nil t) (org-cycle))
	    (search-backward "- item 1")
	    (org-drag-element-backward)
	    (mapcar (lambda (ov) (cons (overlay-start ov) (overlay-end ov)))
		    (overlays-in (point-min) (point-max))))))
  ;; Pathological case: handle call with point in blank lines right
  ;; after a headline.
  (should
   (equal "* H2\n\n* H1\nText\n"
	  (org-test-with-temp-text "* H1\nText\n* H2\n\n<point>"
	    (org-drag-element-backward)
	    (buffer-string)))))

(ert-deftest test-org/drag-element-forward ()
  "Test `org-drag-element-forward' specifications."
  ;; 1. Error when trying to move first element of buffer.
  (org-test-with-temp-text "Paragraph 1.\n\nParagraph 2."
    (goto-line 3)
    (should-error (org-drag-element-forward)))
  ;; 2. Error when trying to swap nested elements.
  (org-test-with-temp-text "#+BEGIN_CENTER\nTest.\n#+END_CENTER"
    (forward-line)
    (should-error (org-drag-element-forward)))
  ;; 3. Error when trying to swap a non-headline element and an
  ;;    headline.
  (org-test-with-temp-text "Test.\n* Head 1"
    (should-error (org-drag-element-forward)))
  ;; 4. Error when called before first element.
  (should-error
   (org-test-with-temp-text "\n"
     (forward-line)
     (org-drag-element-backward))
   :type 'user-error)
  ;; 5. Otherwise, swap elements, preserving column and blank lines
  ;;    between elements.
  (org-test-with-temp-text "Paragraph 1\n\n\nPara2\n\nPara3"
    (search-forward "graph")
    (org-drag-element-forward)
    (should (equal (buffer-string) "Para2\n\n\nParagraph 1\n\nPara3"))
    (should (looking-at " 1")))
  ;; 5. Preserve visibility of elements and their contents.
  (org-test-with-temp-text "
#+BEGIN_CENTER
Text.
#+END_CENTER
- item 1
  #+BEGIN_QUOTE
  Text.
  #+END_QUOTE"
    (while (search-forward "BEGIN_" nil t) (org-cycle))
    (search-backward "#+BEGIN_CENTER")
    (org-drag-element-forward)
    (should
     (equal
      '((63 . 82) (26 . 48))
      (mapcar (lambda (ov) (cons (overlay-start ov) (overlay-end ov)))
	      (overlays-in (point-min) (point-max)))))))

(ert-deftest test-org/next-block ()
  "Test `org-next-block' specifications."
  ;; Regular test.
  (should
   (org-test-with-temp-text "Paragraph\n#+BEGIN_CENTER\ncontents\n#+END_CENTER"
     (org-next-block 1)
     (looking-at "#\\+BEGIN_CENTER")))
  ;; Ignore case.
  (should
   (org-test-with-temp-text "Paragraph\n#+begin_center\ncontents\n#+end_center"
     (let ((case-fold-search nil))
       (org-next-block 1)
       (looking-at "#\\+begin_center"))))
  ;; Ignore current line.
  (should
   (org-test-with-temp-text
       "#+BEGIN_QUOTE\n#+END_QUOTE\n#+BEGIN_CENTER\n#+END_CENTER"
     (org-next-block 1)
     (looking-at "#\\+BEGIN_CENTER")))
  ;; Throw an error when no block is found.
  (should-error
   (org-test-with-temp-text "Paragraph"
     (org-next-block 1)))
  ;; With an argument, skip many blocks at once.
  (should
   (org-test-with-temp-text
       "Start\n#+BEGIN_CENTER\nA\n#+END_CENTER\n#+BEGIN_QUOTE\nB\n#+END_QUOTE"
     (org-next-block 2)
     (looking-at "#\\+BEGIN_QUOTE")))
  ;; With optional argument BLOCK-REGEXP, filter matched blocks.
  (should
   (org-test-with-temp-text
       "Start\n#+BEGIN_CENTER\nA\n#+END_CENTER\n#+BEGIN_QUOTE\nB\n#+END_QUOTE"
     (org-next-block 1 nil "^[ \t]*#\\+BEGIN_QUOTE")
     (looking-at "#\\+BEGIN_QUOTE")))
  ;; Optional argument is also case-insensitive.
  (should
   (org-test-with-temp-text
       "Start\n#+BEGIN_CENTER\nA\n#+END_CENTER\n#+begin_quote\nB\n#+end_quote"
     (let ((case-fold-search nil))
       (org-next-block 1 nil "^[ \t]*#\\+BEGIN_QUOTE")
       (looking-at "#\\+begin_quote")))))

(ert-deftest test-org/insert-structure-template ()
  "Test `org-insert-structure-template'."
  ;; Test in empty buffer.
  (should
   (string= "#+begin_foo\n#+end_foo\n"
	    (org-test-with-temp-text ""
	      (org-insert-structure-template "foo")
	      (buffer-string))))
  ;; Test with multiple lines in buffer.
  (should
   (string= "#+begin_foo\nI'm a paragraph\n#+end_foo\n\nI'm a second paragraph"
	    (org-test-with-temp-text "I'm a paragraph\n\nI'm a second paragraph"
	      (transient-mark-mode 1)
	      (org-mark-element)
	      (org-insert-structure-template "foo")
	      (buffer-string))))
  ;; Mark only the current line.
  (should
   (string= "#+begin_foo\nI'm a paragraph\n#+end_foo\n\nI'm a second paragraph"
	    (org-test-with-temp-text "I'm a paragraph\n\nI'm a second paragraph"
	      (transient-mark-mode 1)
	      (set-mark (point-min))
	      (end-of-line)
	      (org-insert-structure-template "foo")
	      (buffer-string))))
  ;; Middle of paragraph.
  (should
   (string= "p1\n#+begin_foo\np2\n#+end_foo\np3"
	    (org-test-with-temp-text "p1\n<point>p2\np3"
	      (set-mark (line-beginning-position))
	      (end-of-line)
	      (activate-mark)
	      (org-insert-structure-template "foo")
	      (buffer-string))))
  ;; Test with text in buffer, no region, no final newline.
  (should
   (string= "#+begin_foo\nI'm a paragraph.\n#+end_foo\n"
	    (org-test-with-temp-text "I'm a paragraph."
	      (org-mark-element)
	      (org-insert-structure-template "foo")
	      (buffer-string))))
  ;; Test with text in buffer and region set.
  (should
   (string= "#+begin_foo\nI'm a paragraph\n\nI'm a second paragrah\n#+end_foo\n"
	    (org-test-with-temp-text "I'm a paragraph\n\nI'm a second paragrah"
	      (set-mark (point))
	      (goto-char (point-max))
	      (org-insert-structure-template "foo")
	      (buffer-string))))
  ;; Test with example escaping.
  (should
   (string= "#+begin_example\n,* Heading\n#+end_example\n"
	    (org-test-with-temp-text "* Heading"
	      (org-mark-element)
	      (org-insert-structure-template "example")
	      (buffer-string))))
  ;; Test with indentation.
  (should
   (string= "  #+begin_foo\n  This is a paragraph\n  #+end_foo\n"
	    (org-test-with-temp-text "  This is a paragraph"
	      (org-mark-element)
	      (org-insert-structure-template "foo")
	      (buffer-string))))
  (should
   (string= " #+begin_foo\n Line 1\n  Line2\n #+end_foo\n"
	    (org-test-with-temp-text " Line 1\n  Line2"
	      (org-mark-element)
	      (org-insert-structure-template "foo")
	      (buffer-string))))
  ;; Test point location.
  (should
   (string= "#+begin_foo\n"
	    (org-test-with-temp-text ""
	      (org-insert-structure-template "foo")
	      (buffer-substring (point-min) (point)))))
  (should
   (string= "#+begin_src "
	    (org-test-with-temp-text ""
	      (org-insert-structure-template "src")
	      (buffer-substring (point-min) (point))))))

(ert-deftest test-org/previous-block ()
  "Test `org-previous-block' specifications."
  ;; Regular test.
  (should
   (org-test-with-temp-text "#+BEGIN_CENTER\ncontents\n#+END_CENTER\n<point>"
     (org-previous-block 1)
     (looking-at "#\\+BEGIN_CENTER")))
  ;; Ignore case.
  (should
   (org-test-with-temp-text "#+begin_center\ncontents\n#+end_center\n<point>"
     (let ((case-fold-search nil))
       (org-previous-block 1)
       (looking-at "#\\+begin_center"))))
  ;; Ignore current line.
  (should
   (org-test-with-temp-text
       "#+BEGIN_QUOTE\n#+END_QUOTE\n#+BEGIN_CENTER<point>\n#+END_CENTER"
     (org-previous-block 1)
     (looking-at "#\\+BEGIN_QUOTE")))
  ;; Throw an error when no block is found.
  (should-error
   (org-test-with-temp-text "Paragraph<point>"
     (org-previous-block 1)))
  ;; With an argument, skip many blocks at once.
  (should
   (org-test-with-temp-text
       "#+BEGIN_CENTER\nA\n#+END_CENTER\n#+BEGIN_QUOTE\nB\n#+END_QUOTE\n<point>"
     (org-previous-block 2)
     (looking-at "#\\+BEGIN_CENTER")))
  ;; With optional argument BLOCK-REGEXP, filter matched blocks.
  (should
   (org-test-with-temp-text
       "#+BEGIN_CENTER\nA\n#+END_CENTER\n#+BEGIN_QUOTE\nB\n#+END_QUOTE\n<point>"
     (org-previous-block 1 "^[ \t]*#\\+BEGIN_QUOTE")
     (looking-at "#\\+BEGIN_QUOTE")))
  ;; Optional argument is also case-insensitive.
  (should
   (org-test-with-temp-text
       "#+BEGIN_CENTER\nA\n#+END_CENTER\n#+begin_quote\nB\n#+end_quote\n<point>"
     (let ((case-fold-search nil))
       (org-next-block 1 "^[ \t]*#\\+BEGIN_QUOTE")
       (looking-at "#\\+begin_quote")))))


;;; Outline structure

(ert-deftest test-org/demote ()
  "Test `org-demote' specifications."
  ;; Add correct number of stars according to `org-odd-levels-only'.
  (should
   (= 2
      (org-test-with-temp-text "* H"
	(let ((org-odd-levels-only nil)) (org-demote))
	(org-current-level))))
  (should
   (= 3
      (org-test-with-temp-text "* H"
	(let ((org-odd-levels-only t)) (org-demote))
	(org-current-level))))
  ;; When `org-auto-align-tags' is non-nil, move tags accordingly.
  (should
   (org-test-with-temp-text "* H  :tag:"
     (let ((org-tags-column 10)
	   (org-auto-align-tags t)
	   (org-odd-levels-only nil))
       (org-demote))
     (org-move-to-column 10)
     (looking-at-p ":tag:$")))
  (should-not
   (org-test-with-temp-text "* H  :tag:"
     (let ((org-tags-column 10)
	   (org-auto-align-tags nil)
	   (org-odd-levels-only nil))
       (org-demote))
     (org-move-to-column 10)
     (looking-at-p ":tag:$")))
  ;; When `org-adapt-indentation' is non-nil, always indent planning
  ;; info and property drawers accordingly.
  (should
   (= 3
      (org-test-with-temp-text "* H\n  SCHEDULED: <2014-03-04 tue.>"
	(let ((org-odd-levels-only nil)
	      (org-adapt-indentation t))
	  (org-demote))
	(forward-line)
	(org-get-indentation))))
  (should
   (= 3
      (org-test-with-temp-text "* H\n  :PROPERTIES:\n  :FOO: Bar\n  :END:"
	(let ((org-odd-levels-only nil)
	      (org-adapt-indentation t))
	  (org-demote))
	(forward-line)
	(org-get-indentation))))
  (should-not
   (= 3
      (org-test-with-temp-text "* H\n  SCHEDULED: <2014-03-04 tue.>"
	(let ((org-odd-levels-only nil)
	      (org-adapt-indentation nil))
	  (org-demote))
	(forward-line)
	(org-get-indentation))))
  ;; When `org-adapt-indentation' is non-nil, shift all lines in
  ;; section accordingly.  Ignore, however, footnote definitions and
  ;; inlinetasks boundaries.
  (should
   (= 3
      (org-test-with-temp-text "* H\n  Paragraph"
	(let ((org-odd-levels-only nil)
	      (org-adapt-indentation t))
	  (org-demote))
	(forward-line)
	(org-get-indentation))))
  (should
   (= 2
      (org-test-with-temp-text "* H\n  Paragraph"
	(let ((org-odd-levels-only nil)
	      (org-adapt-indentation nil))
	  (org-demote))
	(forward-line)
	(org-get-indentation))))
  (should
   (zerop
    (org-test-with-temp-text "* H\n[fn:1] def line 1\ndef line 2"
      (let ((org-odd-levels-only nil)
	    (org-adapt-indentation t))
	(org-demote))
      (goto-char (point-max))
      (org-get-indentation))))
  (should
   (= 3
      (org-test-with-temp-text "* H\n[fn:1] Def.\n\n\n  After def."
	(let ((org-odd-levels-only nil)
	      (org-adapt-indentation t))
	  (org-demote))
	(goto-char (point-max))
	(org-get-indentation))))
  (when (featurep 'org-inlinetask)
    (should
     (zerop
      (let ((org-inlinetask-min-level 5)
	    (org-adapt-indentation t))
	(org-test-with-temp-text "* H\n***** I\n***** END"
	  (org-demote)
	  (forward-line)
	  (org-get-indentation))))))
  (when (featurep 'org-inlinetask)
    (should
     (= 3
	(let ((org-inlinetask-min-level 5)
	      (org-adapt-indentation t))
	  (org-test-with-temp-text "* H\n***** I\n  Contents\n***** END"
	    (org-demote)
	    (forward-line 2)
	    (org-get-indentation))))))
  ;; When `org-adapt-indentation' is non-nil, log drawers are
  ;; adjusted.
  (should
   (equal
    "** H\n   :LOGBOOK:\n   - a\n   :END:\n   b"
    (org-test-with-temp-text "* H\n  :LOGBOOK:\n  - a\n  :END:\n  b"
      (let ((org-odd-levels-only nil)
	    (org-adapt-indentation t))
	(org-demote))
      (buffer-string))))
  (should
   (equal
    "** H\n   :LOGBOOK:\n   - a\n   :END:\n  b"
    (org-test-with-temp-text "* H\n  :LOGBOOK:\n  - a\n  :END:\n  b"
      (let ((org-odd-levels-only nil)
	    (org-adapt-indentation 'headline-data))
	(org-demote))
      (buffer-string))))
  (should
   (equal
    "** H\n :LOGBOOK:\n - a\n :END:"
    (org-test-with-temp-text "* H\n:LOGBOOK:\n- a\n:END:"
      (let ((org-odd-levels-only nil)
	    (org-adapt-indentation t))
	(org-demote))
      (buffer-string))))
  ;; Ignore contents of source blocks or example blocks when
  ;; indentation should be preserved (through
  ;; `org-src-preserve-indentation' or "-i" flag).
  (should-not
   (zerop
    (org-test-with-temp-text "* H\n#+BEGIN_SRC emacs-lisp\n(+ 1 1)\n#+END_SRC"
      (let ((org-adapt-indentation t)
	    (org-src-preserve-indentation nil))
	(org-demote))
      (forward-line 2)
      (org-get-indentation))))
  (should
   (zerop
    (org-test-with-temp-text "* H\n#+BEGIN_EXAMPLE\n(+ 1 1)\n#+END_EXAMPLE"
      (let ((org-adapt-indentation t)
	    (org-src-preserve-indentation t))
	(org-demote))
      (forward-line 2)
      (org-get-indentation))))
  (should
   (zerop
    (org-test-with-temp-text "* H\n#+BEGIN_SRC emacs-lisp\n(+ 1 1)\n#+END_SRC"
      (let ((org-adapt-indentation t)
	    (org-src-preserve-indentation t))
	(org-demote))
      (forward-line 2)
      (org-get-indentation))))
  (should
   (zerop
    (org-test-with-temp-text
	"* H\n#+BEGIN_SRC emacs-lisp -i\n(+ 1 1)\n#+END_SRC"
      (let ((org-adapt-indentation t)
	    (org-src-preserve-indentation nil))
	(org-demote))
      (forward-line 2)
      (org-get-indentation)))))

(ert-deftest test-org/promote ()
  "Test `org-promote' specifications."
  ;; Return an error if headline is to be promoted to level 0, unless
  ;; `org-allow-promoting-top-level-subtree' is non-nil, in which case
  ;; headline becomes a comment.
  (should-error
   (org-test-with-temp-text "* H"
     (let ((org-allow-promoting-top-level-subtree nil)) (org-promote))))
  (should
   (equal "# H"
	  (org-test-with-temp-text "* H"
	    (let ((org-allow-promoting-top-level-subtree t)) (org-promote))
	    (buffer-string))))
  ;; Remove correct number of stars according to
  ;; `org-odd-levels-only'.
  (should
   (= 2
      (org-test-with-temp-text "*** H"
	(let ((org-odd-levels-only nil)) (org-promote))
	(org-current-level))))
  (should
   (= 1
      (org-test-with-temp-text "*** H"
	(let ((org-odd-levels-only t)) (org-promote))
	(org-current-level))))
  ;; When `org-auto-align-tags' is non-nil, move tags accordingly.
  (should
   (org-test-with-temp-text "** H :tag:"
     (let ((org-tags-column 10)
	   (org-auto-align-tags t)
	   (org-odd-levels-only nil))
       (org-promote))
     (org-move-to-column 10)
     (looking-at-p ":tag:$")))
  (should-not
   (org-test-with-temp-text "** H :tag:"
     (let ((org-tags-column 10)
	   (org-auto-align-tags nil)
	   (org-odd-levels-only nil))
       (org-promote))
     (org-move-to-column 10)
     (looking-at-p ":tag:$")))
  ;; When `org-adapt-indentation' is non-nil, always indent planning
  ;; info and property drawers.
  (should
   (= 2
      (org-test-with-temp-text "** H\n   SCHEDULED: <2014-03-04 tue.>"
	(let ((org-odd-levels-only nil)
	      (org-adapt-indentation t))
	  (org-promote))
	(forward-line)
	(org-get-indentation))))
  (should
   (= 2
      (org-test-with-temp-text "** H\n   :PROPERTIES:\n   :FOO: Bar\n   :END:"
	(let ((org-odd-levels-only nil)
	      (org-adapt-indentation t))
	  (org-promote))
	(forward-line)
	(org-get-indentation))))
  (should-not
   (= 2
      (org-test-with-temp-text "** H\n   SCHEDULED: <2014-03-04 tue.>"
	(let ((org-odd-levels-only nil)
	      (org-adapt-indentation nil))
	  (org-promote))
	(forward-line)
	(org-get-indentation))))
  ;; When `org-adapt-indentation' is non-nil, shift all lines in
  ;; section accordingly.  Ignore, however, footnote definitions and
  ;; inlinetasks boundaries.
  (should
   (= 2
      (org-test-with-temp-text "** H\n   Paragraph"
	(let ((org-odd-levels-only nil)
	      (org-adapt-indentation t))
	  (org-promote))
	(forward-line)
	(org-get-indentation))))
  (should-not
   (= 2
      (org-test-with-temp-text "** H\n   Paragraph"
	(let ((org-odd-levels-only nil)
	      (org-adapt-indentation nil))
	  (org-promote))
	(forward-line)
	(org-get-indentation))))
  (should
   (= 2
      (org-test-with-temp-text "** H\n   Paragraph\n[fn:1] line1\nline2"
	(let ((org-odd-levels-only nil)
	      (org-adapt-indentation t))
	  (org-promote))
	(forward-line)
	(org-get-indentation))))
  (when (featurep 'org-inlinetask)
    (should
     (zerop
      (let ((org-inlinetask-min-level 5)
	    (org-adapt-indentation t))
	(org-test-with-temp-text "** H\n***** I\n***** END"
	  (org-promote)
	  (forward-line)
	  (org-get-indentation))))))
  (when (featurep 'org-inlinetask)
    (should
     (= 2
	(let ((org-inlinetask-min-level 5)
	      (org-adapt-indentation t))
	  (org-test-with-temp-text "** H\n***** I\n   Contents\n***** END"
	    (org-promote)
	    (forward-line 2)
	    (org-get-indentation))))))
  ;; Give up shifting if it would break document's structure
  ;; otherwise.
  (should
   (= 3
      (org-test-with-temp-text "** H\n   Paragraph\n [fn:1] Def."
	(let ((org-odd-levels-only nil)
	      (org-adapt-indentation t))
	  (org-promote))
	(forward-line)
	(org-get-indentation))))
  (should
   (= 3
      (org-test-with-temp-text "** H\n   Paragraph\n * list."
	(let ((org-odd-levels-only nil)
	      (org-adapt-indentation t))
	  (org-promote))
	(forward-line)
	(org-get-indentation))))
  ;; When `org-adapt-indentation' is non-nil, log drawers are
  ;; adjusted.
  (should
   (equal
    "* H\n  :LOGBOOK:\n  - a\n  :END:\n  b"
    (org-test-with-temp-text "** H\n   :LOGBOOK:\n   - a\n   :END:\n   b"
      (let ((org-odd-levels-only nil)
	    (org-adapt-indentation t))
	(org-promote))
      (buffer-string))))
  (should
   (equal
    "* H\n  :LOGBOOK:\n  - a\n  :END:\n   b"
    (org-test-with-temp-text "** H\n   :LOGBOOK:\n   - a\n   :END:\n   b"
      (let ((org-odd-levels-only nil)
	    (org-adapt-indentation 'headline-data))
	(org-promote))
      (buffer-string))))
  (should
   (equal
    "* H\n:LOGBOOK:\n- a\n:END:"
    (org-test-with-temp-text "** H\n:LOGBOOK:\n- a\n:END:"
      (let ((org-odd-levels-only nil)
	    (org-adapt-indentation t))
	(org-promote))
      (buffer-string))))
  (should
   (equal
    "# H\n:LOGBOOK:\n- a\n:END:"
    (org-test-with-temp-text "* H\n:LOGBOOK:\n- a\n:END:"
      (let ((org-odd-levels-only nil)
	    (org-allow-promoting-top-level-subtree t)
	    (org-adapt-indentation t))
	(org-promote))
      (buffer-string))))
  ;; Ignore contents of source blocks or example blocks when
  ;; indentation should be preserved (through
  ;; `org-src-preserve-indentation' or "-i" flag).
  (should-not
   (zerop
    (org-test-with-temp-text
	"** H\n #+BEGIN_SRC emacs-lisp\n(+ 1 1)\n #+END_SRC"
      (let ((org-adapt-indentation t)
	    (org-src-preserve-indentation nil)
	    (org-odd-levels-only nil))
	(org-promote))
      (forward-line)
      (org-get-indentation))))
  (should
   (zerop
    (org-test-with-temp-text
	"** H\n #+BEGIN_EXAMPLE\nContents\n #+END_EXAMPLE"
      (let ((org-adapt-indentation t)
	    (org-src-preserve-indentation t)
	    (org-odd-levels-only nil))
	(org-promote))
      (forward-line)
      (org-get-indentation))))
  (should
   (zerop
    (org-test-with-temp-text
	"** H\n #+BEGIN_SRC emacs-lisp\n(+ 1 1)\n #+END_SRC"
      (let ((org-adapt-indentation t)
	    (org-src-preserve-indentation t)
	    (org-odd-levels-only nil))
	(org-promote))
      (forward-line)
      (org-get-indentation))))
  (should
   (zerop
    (org-test-with-temp-text
	"** H\n #+BEGIN_SRC emacs-lisp -i\n(+ 1 1)\n #+END_SRC"
      (let ((org-adapt-indentation t)
	    (org-src-preserve-indentation nil)
	    (org-odd-levels-only nil))
	(org-promote))
      (forward-line)
      (org-get-indentation)))))

(ert-deftest test-org/org-get-valid-level ()
  "Test function `org-get-valid-level' specifications."
  (let ((org-odd-levels-only nil))
    (should (equal 1 (org-get-valid-level 0 0)))
    (should (equal 1 (org-get-valid-level 0 1)))
    (should (equal 2 (org-get-valid-level 0 2)))
    (should (equal 3 (org-get-valid-level 0 3)))
    (should (equal 1 (org-get-valid-level 1 0)))
    (should (equal 2 (org-get-valid-level 1 1)))
    (should (equal 23 (org-get-valid-level 1 22)))
    (should (equal 1 (org-get-valid-level 1 -1)))
    (should (equal 1 (org-get-valid-level 2 -1))))
  (let ((org-odd-levels-only t))
    (should (equal 1 (org-get-valid-level 0 0)))
    (should (equal 1 (org-get-valid-level 0 1)))
    (should (equal 3 (org-get-valid-level 0 2)))
    (should (equal 5 (org-get-valid-level 0 3)))
    (should (equal 1 (org-get-valid-level 1 0)))
    (should (equal 3 (org-get-valid-level 1 1)))
    (should (equal 3 (org-get-valid-level 2 1)))
    (should (equal 5 (org-get-valid-level 3 1)))
    (should (equal 5 (org-get-valid-level 4 1)))
    (should (equal 43 (org-get-valid-level 1 21)))
    (should (equal 1 (org-get-valid-level 1 -1)))
    (should (equal 1 (org-get-valid-level 2 -1)))
    (should (equal 1 (org-get-valid-level 3 -1)))
    (should (equal 3 (org-get-valid-level 4 -1)))
    (should (equal 3 (org-get-valid-level 5 -1)))))


;;; Planning

(ert-deftest test-org/at-planning-p ()
  "Test `org-at-planning-p' specifications."
  ;; Regular test.
  (should
   (org-test-with-temp-text "* Headline\n<point>DEADLINE: <2014-03-04 tue.>"
     (org-at-planning-p)))
  (should-not
   (org-test-with-temp-text "DEADLINE: <2014-03-04 tue.>"
     (org-at-planning-p)))
  ;; Correctly find planning attached to inlinetasks.
  (when (featurep 'org-inlinetask)
    (should
     (org-test-with-temp-text
	 "*** Inlinetask\n<point>DEADLINE: <2014-03-04 tue.>\n*** END"
       (let ((org-inlinetask-min-level 3)) (org-at-planning-p))))
    (should-not
     (org-test-with-temp-text
	 "*** Inlinetask\n<point>DEADLINE: <2014-03-04 tue.>"
       (let ((org-inlinetask-min-level 3)) (org-at-planning-p))))
    (should-not
     (org-test-with-temp-text
	 "* Headline\n*** Inlinetask\n<point>DEADLINE: <2014-03-04 tue.>"
       (let ((org-inlinetask-min-level 3)) (org-at-planning-p))))
    (should-not
     (org-test-with-temp-text
	 "* Headline\n*** Inlinetask\n*** END\n<point>DEADLINE: <2014-03-04 tue.>"
       (let ((org-inlinetask-min-level 3)) (org-at-planning-p))))))

(ert-deftest test-org/add-planning-info ()
  "Test `org-add-planning-info'."
  ;; Create deadline when `org-adapt-indentation' is non-nil.
  (should
   (equal "* H\n  DEADLINE: <2015-06-25>\nParagraph"
	  (org-test-with-temp-text "* H\nParagraph<point>"
	    (let ((org-adapt-indentation t))
	      (org-add-planning-info 'deadline "<2015-06-25 Thu>"))
	    (replace-regexp-in-string
	     "\\( [.A-Za-z]+\\)>" "" (buffer-string)
	     nil nil 1))))
  ;; Create deadline when `org-adapt-indentation' is nil.
  (should
   (equal "* H\nDEADLINE: <2015-06-25>\nParagraph"
	  (org-test-with-temp-text "* H\nParagraph<point>"
	    (let ((org-adapt-indentation nil))
	      (org-add-planning-info 'deadline "<2015-06-25 Thu>"))
	    (replace-regexp-in-string
	     "\\( [.A-Za-z]+\\)>" "" (buffer-string)
	     nil nil 1))))
  ;; Update deadline when `org-adapt-indentation' is non-nil.
  (should
   (equal "* H\n  DEADLINE: <2015-06-25>\nParagraph"
	  (org-test-with-temp-text "\
* H
  DEADLINE: <2015-06-24 Wed>
Paragraph<point>"
	    (let ((org-adapt-indentation t))
	      (org-add-planning-info 'deadline "<2015-06-25 Thu>"))
	    (replace-regexp-in-string
	     "\\( [.A-Za-z]+\\)>" "" (buffer-string)
	     nil nil 1))))
  ;; Update deadline when `org-adapt-indentation' is nil.
  (should
   (equal "* H\nDEADLINE: <2015-06-25>\nParagraph"
	  (org-test-with-temp-text "\
* H
DEADLINE: <2015-06-24 Wed>
Paragraph<point>"
	    (let ((org-adapt-indentation nil))
	      (org-add-planning-info 'deadline "<2015-06-25 Thu>"))
	    (replace-regexp-in-string
	     "\\( [.A-Za-z]+\\)>" "" (buffer-string)
	     nil nil 1))))
  ;; Schedule when `org-adapt-indentation' is non-nil.
  (should
   (equal "* H\n  SCHEDULED: <2015-06-25>\nParagraph"
	  (org-test-with-temp-text "* H\nParagraph<point>"
	    (let ((org-adapt-indentation t))
	      (org-add-planning-info 'scheduled "<2015-06-25 Thu>"))
	    (replace-regexp-in-string
	     "\\( [.A-Za-z]+\\)>" "" (buffer-string)
	     nil nil 1))))
  ;; Schedule when `org-adapt-indentation' is nil.
  (should
   (equal "* H\nSCHEDULED: <2015-06-25>\nParagraph"
	  (org-test-with-temp-text "* H\nParagraph<point>"
	    (let ((org-adapt-indentation nil))
	      (org-add-planning-info 'scheduled "<2015-06-25 Thu>"))
	    (replace-regexp-in-string
	     "\\( [.A-Za-z]+\\)>" "" (buffer-string)
	     nil nil 1))))
  ;; Add deadline when scheduled.
  (should
   (equal "\
* H
  DEADLINE: <2015-06-25> SCHEDULED: <2015-06-24>
Paragraph"
	  (org-test-with-temp-text "\
* H
  SCHEDULED: <2015-06-24 Wed>
Paragraph<point>"
	    (let ((org-adapt-indentation t))
	      (org-add-planning-info 'deadline "<2015-06-25 Thu>"))
	    (replace-regexp-in-string
	     "\\( [.A-Za-z]+\\)>" "" (buffer-string)
	     nil nil 1))))
  ;; Remove middle entry.
  (should
   (equal "\
* H
  CLOSED: [2015-06-24] SCHEDULED: <2015-06-24>
Paragraph"
	  (org-test-with-temp-text "\
* H
  CLOSED: [2015-06-24 Wed] DEADLINE: <2015-06-25 Thu> SCHEDULED: <2015-06-24 Wed>
Paragraph<point>"
	    (let ((org-adapt-indentation t))
	      (org-add-planning-info nil nil 'deadline))
	    (replace-regexp-in-string
	     "\\( [.A-Za-z]+\\)[]>]" "" (buffer-string)
	     nil nil 1))))
  ;; Remove last entry and then middle entry (order should not
  ;; matter).
  (should
   (equal "\
* H
  CLOSED: [2015-06-24]
Paragraph"
	  (org-test-with-temp-text "\
* H
  CLOSED: [2015-06-24 Wed] DEADLINE: <2015-06-25 Thu> SCHEDULED: <2015-06-24 Wed>
Paragraph<point>"
	    (let ((org-adapt-indentation t))
	      (org-add-planning-info nil nil 'scheduled 'deadline))
	    (replace-regexp-in-string
	     "\\( [.A-Za-z]+\\)[]>]" "" (buffer-string)
	     nil nil 1))))
  ;; Remove closed when `org-adapt-indentation' is non-nil.
  (should
   (equal "* H\n  DEADLINE: <2015-06-25>\nParagraph"
	  (org-test-with-temp-text "\
* H
  CLOSED: [2015-06-25 Thu] DEADLINE: <2015-06-25 Thu>
Paragraph<point>"
	    (let ((org-adapt-indentation t))
	      (org-add-planning-info nil nil 'closed))
	    (replace-regexp-in-string
	     "\\( [.A-Za-z]+\\)>" "" (buffer-string)
	     nil nil 1))))
  (should
   (equal "* H\n  Paragraph"
	  (org-test-with-temp-text "\
* H
  CLOSED: [2015-06-25 Thu]
  Paragraph<point>"
	    (let ((org-adapt-indentation t))
	      (org-add-planning-info nil nil 'closed))
	    (replace-regexp-in-string
	     "\\( [.A-Za-z]+\\)>" "" (buffer-string)
	     nil nil 1))))
  ;; Remove closed when `org-adapt-indentation' is nil.
  (should
   (equal "* H\nDEADLINE: <2015-06-25>\nParagraph"
	  (org-test-with-temp-text "\
* H
CLOSED: [2015-06-25 Thu] DEADLINE: <2015-06-25 Thu>
Paragraph<point>"
	    (let ((org-adapt-indentation nil))
	      (org-add-planning-info nil nil 'closed))
	    (replace-regexp-in-string
	     "\\( [.A-Za-z]+\\)>" "" (buffer-string)
	     nil nil 1))))
  (should
   (equal "* H\nParagraph"
	  (org-test-with-temp-text "\
* H
  CLOSED: [2015-06-25 Thu]
Paragraph<point>"
	    (let ((org-adapt-indentation nil))
	      (org-add-planning-info nil nil 'closed))
	    (replace-regexp-in-string
	     "\\( [.A-Za-z]+\\)>" "" (buffer-string)
	     nil nil 1))))
  ;; Remove closed entry and delete empty line.
  (should
   (equal "\
* H
Paragraph"
	  (org-test-with-temp-text "\
* H
  CLOSED: [2015-06-24 Wed]
Paragraph<point>"
	    (let ((org-adapt-indentation t))
	      (org-add-planning-info nil nil 'closed))
	    (replace-regexp-in-string
	     "\\( [.A-Za-z]+\\)>" "" (buffer-string)
	     nil nil 1))))
  ;; Remove one entry and update another.
  (should
   (equal "* H\n  DEADLINE: <2015-06-25>\nParagraph"
	  (org-test-with-temp-text "\
* H
  SCHEDULED: <2015-06-23 Tue> DEADLINE: <2015-06-24 Wed>
Paragraph<point>"
	    (let ((org-adapt-indentation t))
	      (org-add-planning-info 'deadline "<2015-06-25 Thu>" 'scheduled))
	    (replace-regexp-in-string
	     "\\( [.A-Za-z]+\\)>" "" (buffer-string)
	     nil nil 1)))))

(ert-deftest test-org/deadline ()
  "Test `org-deadline' specifications."
  ;; Insert a new value or replace existing one.
  (should
   (equal "* H\nDEADLINE: <2012-03-29>"
	  (org-test-with-temp-text "* H"
	    (let ((org-adapt-indentation nil)
		  (org-last-inserted-timestamp nil))
	      (org-deadline nil "<2012-03-29 Tue>"))
	    (replace-regexp-in-string
	     "\\( [.A-Za-z]+\\)>" "" (buffer-string)
	     nil nil 1))))
  (should
   (equal "* H\nDEADLINE: <2014-03-04>"
	  (org-test-with-temp-text "* H\nDEADLINE: <2012-03-29>"
	    (let ((org-adapt-indentation nil)
		  (org-last-inserted-timestamp nil))
	      (org-deadline nil "<2014-03-04 Thu>"))
	    (replace-regexp-in-string
	     "\\( [.A-Za-z]+\\)>" "" (buffer-string)
	     nil nil 1))))
  ;; Accept delta time, e.g., "+2d".
  (should
   (equal "* H\nDEADLINE: <2015-03-04>"
	  (org-test-at-time "2014-03-04"
	    (org-test-with-temp-text "* H"
	      (let ((org-adapt-indentation nil)
		    (org-last-inserted-timestamp nil))
		(org-deadline nil "+1y"))
	      (replace-regexp-in-string
	       "\\( [.A-Za-z]+\\)>" "" (buffer-string) nil nil 1)))))
  ;; Preserve repeater.
  (should
   (equal "* H\nDEADLINE: <2012-03-29 +2y>"
	  (org-test-with-temp-text "* H"
	    (let ((org-adapt-indentation nil)
		  (org-last-inserted-timestamp nil))
	      (org-deadline nil "<2012-03-29 Tue +2y>"))
	    (replace-regexp-in-string
	     "\\( [.A-Za-z]+\\) " "" (buffer-string) nil nil 1))))
  ;; Remove CLOSED keyword, if any.
  (should
   (equal "* H\nDEADLINE: <2012-03-29>"
	  (org-test-with-temp-text "* H\nCLOSED: [2017-01-25 Wed]"
	    (let ((org-adapt-indentation nil)
		  (org-last-inserted-timestamp nil))
	      (org-deadline nil "<2012-03-29 Tue>"))
	    (replace-regexp-in-string
	     "\\( [.A-Za-z]+\\)>" "" (buffer-string) nil nil 1))))
  ;; With C-u argument, remove DEADLINE keyword.
  (should
   (equal "* H\n"
	  (org-test-with-temp-text "* H\nDEADLINE: <2012-03-29>"
	    (let ((org-adapt-indentation nil)
		  (org-last-inserted-timestamp nil))
	      (org-deadline '(4)))
	    (buffer-string))))
  (should
   (equal "* H"
	  (org-test-with-temp-text "* H"
	    (let ((org-adapt-indentation nil)
		  (org-last-inserted-timestamp nil))
	      (org-deadline '(4)))
	    (buffer-string))))
  ;; With C-u C-u argument, prompt for a delay cookie.
  (should
   (equal "* H\nDEADLINE: <2012-03-29 -705d>"
	  (cl-letf (((symbol-function 'org-read-date)
		     (lambda (&rest args)
		       (apply #'encode-time
			      (org-parse-time-string "2014-03-04")))))
	    (org-test-with-temp-text "* H\nDEADLINE: <2012-03-29>"
	      (let ((org-adapt-indentation nil)
		    (org-last-inserted-timestamp nil))
		(org-deadline '(16)))
	      (buffer-string)))))
  (should-error
   (cl-letf (((symbol-function 'org-read-date)
	      (lambda (&rest args)
		(apply #'encode-time
		       (org-parse-time-string "2014-03-04")))))
     (org-test-with-temp-text "* H"
       (let ((org-adapt-indentation nil)
	     (org-last-inserted-timestamp nil))
	 (org-deadline '(16)))
       (buffer-string))))
  ;; When a region is active and
  ;; `org-loop-over-headlines-in-active-region' is non-nil, insert the
  ;; same value in all headlines in region.
  (should
   (equal "* H1\nDEADLINE: <2012-03-29>\n* H2\nDEADLINE: <2012-03-29>"
	  (org-test-with-temp-text "* H1\n* H2"
	    (let ((org-adapt-indentation nil)
		  (org-last-inserted-timestamp nil)
		  (org-loop-over-headlines-in-active-region t))
	      (transient-mark-mode 1)
	      (push-mark (point) t t)
	      (goto-char (point-max))
	      (org-deadline nil "2012-03-29"))
	    (replace-regexp-in-string
	     "\\( [.A-Za-z]+\\)>" "" (buffer-string) nil nil 1))))
  (should-not
   (equal "* H1\nDEADLINE: <2012-03-29>\n* H2\nDEADLINE: <2012-03-29>"
	  (org-test-with-temp-text "* H1\n* H2"
	    (let ((org-adapt-indentation nil)
		  (org-last-inserted-timestamp nil)
		  (org-loop-over-headlines-in-active-region nil))
	      (transient-mark-mode 1)
	      (push-mark (point) t t)
	      (goto-char (point-max))
	      (org-deadline nil "2012-03-29"))
	    (replace-regexp-in-string
	     "\\( [.A-Za-z]+\\)>" "" (buffer-string) nil nil 1)))))

(ert-deftest test-org/schedule ()
  "Test `org-schedule' specifications."
  ;; Insert a new value or replace existing one.
  (should
   (equal "* H\nSCHEDULED: <2012-03-29>"
	  (org-test-with-temp-text "* H"
	    (let ((org-adapt-indentation nil)
		  (org-last-inserted-timestamp nil))
	      (org-schedule nil "<2012-03-29 Tue>"))
	    (replace-regexp-in-string
	     "\\( [.A-Za-z]+\\)>" "" (buffer-string)
	     nil nil 1))))
  (should
   (equal "* H\nSCHEDULED: <2014-03-04>"
	  (org-test-with-temp-text "* H\nSCHEDULED: <2012-03-29>"
	    (let ((org-adapt-indentation nil)
		  (org-last-inserted-timestamp nil))
	      (org-schedule nil "<2014-03-04 Thu>"))
	    (replace-regexp-in-string
	     "\\( [.A-Za-z]+\\)>" "" (buffer-string)
	     nil nil 1))))
  ;; Accept delta time, e.g., "+2d".
  (should
   (equal "* H\nSCHEDULED: <2015-03-04>"
	  (org-test-at-time "2014-03-04"
	    (org-test-with-temp-text "* H"
	      (let ((org-adapt-indentation nil)
		    (org-last-inserted-timestamp nil))
		(org-schedule nil "+1y"))
	      (replace-regexp-in-string
	       "\\( [.A-Za-z]+\\)>" "" (buffer-string) nil nil 1)))))
  ;; Preserve repeater.
  (should
   (equal "* H\nSCHEDULED: <2012-03-29 +2y>"
	  (org-test-with-temp-text "* H"
	    (let ((org-adapt-indentation nil)
		  (org-last-inserted-timestamp nil))
	      (org-schedule nil "<2012-03-29 Tue +2y>"))
	    (replace-regexp-in-string
	     "\\( [.A-Za-z]+\\) " "" (buffer-string) nil nil 1))))
  ;; Remove CLOSED keyword, if any.
  (should
   (equal "* H\nSCHEDULED: <2012-03-29>"
	  (org-test-with-temp-text "* H\nCLOSED: [2017-01-25 Wed]"
	    (let ((org-adapt-indentation nil)
		  (org-last-inserted-timestamp nil))
	      (org-schedule nil "<2012-03-29 Tue>"))
	    (replace-regexp-in-string
	     "\\( [.A-Za-z]+\\)>" "" (buffer-string) nil nil 1))))
  ;; With C-u argument, remove SCHEDULED keyword.
  (should
   (equal "* H\n"
	  (org-test-with-temp-text "* H\nSCHEDULED: <2012-03-29>"
	    (let ((org-adapt-indentation nil)
		  (org-last-inserted-timestamp nil))
	      (org-schedule '(4)))
	    (buffer-string))))
  (should
   (equal "* H"
	  (org-test-with-temp-text "* H"
	    (let ((org-adapt-indentation nil)
		  (org-last-inserted-timestamp nil))
	      (org-schedule '(4)))
	    (buffer-string))))
  ;; With C-u C-u argument, prompt for a delay cookie.
  (should
   (equal "* H\nSCHEDULED: <2012-03-29 -705d>"
	  (cl-letf (((symbol-function 'org-read-date)
		     (lambda (&rest args)
		       (apply #'encode-time
			      (org-parse-time-string "2014-03-04")))))
	    (org-test-with-temp-text "* H\nSCHEDULED: <2012-03-29>"
	      (let ((org-adapt-indentation nil)
		    (org-last-inserted-timestamp nil))
		(org-schedule '(16)))
	      (buffer-string)))))
  (should-error
   (cl-letf (((symbol-function 'org-read-date)
	      (lambda (&rest args)
		(apply #'encode-time
		       (org-parse-time-string "2014-03-04")))))
     (org-test-with-temp-text "* H"
       (let ((org-adapt-indentation nil)
	     (org-last-inserted-timestamp nil))
	 (org-schedule '(16)))
       (buffer-string))))
  ;; When a region is active and
  ;; `org-loop-over-headlines-in-active-region' is non-nil, insert the
  ;; same value in all headlines in region.
  (should
   (equal "* H1\nSCHEDULED: <2012-03-29>\n* H2\nSCHEDULED: <2012-03-29>"
	  (org-test-with-temp-text "* H1\n* H2"
	    (let ((org-adapt-indentation nil)
		  (org-last-inserted-timestamp nil)
		  (org-loop-over-headlines-in-active-region t))
	      (transient-mark-mode 1)
	      (push-mark (point) t t)
	      (goto-char (point-max))
	      (org-schedule nil "2012-03-29"))
	    (replace-regexp-in-string
	     "\\( [.A-Za-z]+\\)>" "" (buffer-string) nil nil 1))))
  (should-not
   (equal "* H1\nSCHEDULED: <2012-03-29>\n* H2\nSCHEDULED: <2012-03-29>"
	  (org-test-with-temp-text "* H1\n* H2"
	    (let ((org-adapt-indentation nil)
		  (org-last-inserted-timestamp nil)
		  (org-loop-over-headlines-in-active-region nil))
	      (transient-mark-mode 1)
	      (push-mark (point) t t)
	      (goto-char (point-max))
	      (org-schedule nil "2012-03-29"))
	    (replace-regexp-in-string
	     "\\( [.A-Za-z]+\\)>" "" (buffer-string) nil nil 1))))
  (should
   ;; check if a repeater survives re-scheduling.
   (string-match-p
    "\\* H\nSCHEDULED: <2017-02-01 [.A-Za-z]* \\+\\+7d>\n"
    (org-test-with-temp-text "* H\nSCHEDULED: <2017-01-19 ++7d>\n"
			     (let ((org-adapt-indentation nil)
				   (org-last-inserted-timestamp nil))
			       (org-schedule nil "2017-02-01"))
			     (buffer-string)))))


;;; Property API

(ert-deftest test-org/buffer-property-keys ()
  "Test `org-buffer-property-keys' specifications."
  ;; Retrieve properties across siblings.
  (should
   (equal '("A" "B")
	  (org-test-with-temp-text "
* H1
:PROPERTIES:
:A: 1
:END:
* H2
:PROPERTIES:
:B: 1
:END:"
	    (org-buffer-property-keys))))
  ;; Retrieve properties across children.
  (should
   (equal '("A" "B")
	  (org-test-with-temp-text "
* H1
:PROPERTIES:
:A: 1
:END:
** H2
:PROPERTIES:
:B: 1
:END:"
	    (org-buffer-property-keys))))
  ;; Retrieve muliple properties in the same drawer.
  (should
   (equal '("A" "B")
	  (org-test-with-temp-text "* H\n:PROPERTIES:\n:A: 1\n:B: 2\n:END:"
	    (org-buffer-property-keys))))
  ;; Ignore extension symbol in property name.
  (should
   (equal '("A")
	  (org-test-with-temp-text "* H\n:PROPERTIES:\n:A: 1\n:A+: 2\n:END:"
	    (org-buffer-property-keys))))
  ;; Add bare property if xxx_ALL property is there
  (should
   (equal '("A" "B" "B_ALL")
	  (org-test-with-temp-text "* H\n:PROPERTIES:\n:A: 1\n:A+: 2\n:B_ALL: foo bar\n:END:"
	    (org-buffer-property-keys))))
  ;; Add bare property if xxx_ALL property is there - check dupes
  (should
   (equal '("A" "B" "B_ALL")
	  (org-test-with-temp-text "* H\n:PROPERTIES:\n:A: 1\n:B: 2\n:B_ALL: foo bar\n:END:"
	    (org-buffer-property-keys))))
  ;; Retrieve properties from #+PROPERTY keyword lines
  (should
   (equal '("A" "C")
	  (org-test-with-temp-text "#+PROPERTY: C foo\n* H\n:PROPERTIES:\n:A: 1\n:A+: 2\n:END:"
	    (org-buffer-property-keys))))
  ;; Retrieve properties from #+PROPERTY keyword lines - make sure an _ALL property also
  ;; adds the bare property
  (should
   (equal '("A" "C" "C_ALL")
	  (org-test-with-temp-text "#+PROPERTY: C_ALL foo bar\n* H\n:PROPERTIES:\n:A: 1\n:A+: 2\n:END:"
	    (org-buffer-property-keys))))
  ;; With non-nil COLUMNS, extract property names from columns.
  (should
   (equal '("A" "B")
	  (org-test-with-temp-text "#+COLUMNS: %25ITEM %A %20B"
	    (org-buffer-property-keys nil nil t))))
  (should
   (equal '("A" "B" "COLUMNS")
	  (org-test-with-temp-text
	      "* H\n:PROPERTIES:\n:COLUMNS: %25ITEM %A %20B\n:END:"
	    (org-buffer-property-keys nil nil t))))
  ;; In COLUMNS, ignore title and summary-type.
  (should
   (equal '("A")
	  (org-test-with-temp-text "#+COLUMNS: %A(Foo)"
	    (org-buffer-property-keys nil nil t))))
  (should
   (equal '("A")
	  (org-test-with-temp-text "#+COLUMNS: %A{Foo}"
	    (org-buffer-property-keys nil nil t))))
  (should
   (equal '("A")
	  (org-test-with-temp-text "#+COLUMNS: %A(Foo){Bar}"
	    (org-buffer-property-keys nil nil t)))))

(ert-deftest test-org/property-values ()
  "Test `org-property-values' specifications."
  ;; Regular test.
  (should
   (equal '("2" "1")
	  (org-test-with-temp-text
	      "* H\n:PROPERTIES:\n:A: 1\n:END:\n* H\n:PROPERTIES:\n:A: 2\n:END:"
	    (org-property-values "A"))))
  ;; Ignore empty values.
  (should-not
   (org-test-with-temp-text
       "* H1\n:PROPERTIES:\n:A:\n:END:\n* H2\n:PROPERTIES:\n:A:  \n:END:"
     (org-property-values "A")))
  ;; Take into consideration extended values.
  (should
   (equal '("1 2")
	  (org-test-with-temp-text "* H\n:PROPERTIES:\n:A: 1\n:A+: 2\n:END:"
	    (org-property-values "A")))))

(ert-deftest test-org/set-property ()
  "Test `org-set-property' specifications."
  (should
   (equal
    ":PROPERTIES:\n:TEST: t\n:END:\n"
    (org-test-with-temp-text ""
      (let ((org-property-format "%s %s"))
	(org-set-property "TEST" "t"))
      (buffer-string))))
  (should
   (equal
    "* H\n:PROPERTIES:\n:TEST: t\n:END:\n"
    (org-test-with-temp-text "* H"
      (let ((org-adapt-indentation nil)
	    (org-property-format "%s %s"))
	(org-set-property "TEST" "t"))
      (buffer-string)))))

(ert-deftest test-org/delete-property ()
  "Test `org-delete-property' specifications."
  (should
   (equal
    ""
    (org-test-with-temp-text ":PROPERTIES:\n:TEST: t\n:END:\n"
      (org-delete-property "TEST")
      (buffer-string))))
  (should
   (equal
    ":PROPERTIES:\n:TEST1: t\n:END:\n"
    (org-test-with-temp-text ":PROPERTIES:\n:TEST1: t\n:TEST2: t\n:END:\n"
      (org-delete-property "TEST2")
      (buffer-string))))
  (should
   (equal
    "* H\n"
    (org-test-with-temp-text "* H\n:PROPERTIES:\n:TEST: t\n:END:\n"
      (org-delete-property "TEST")
      (buffer-string))))
  (should
   (equal
    "* H\n:PROPERTIES:\n:TEST1: t\n:END:\n"
    (org-test-with-temp-text "* H\n:PROPERTIES:\n:TEST1: t\n:TEST2: t\n:END:\n"
      (org-delete-property "TEST2")
      (buffer-string)))))

(ert-deftest test-org/delete-property-globally ()
  "Test `org-delete-property-global' specifications."
  (should
   (equal
    ""
    (org-test-with-temp-text ":PROPERTIES:\n:TEST: t\n:END:\n"
      (org-delete-property-globally "TEST")
      (buffer-string))))
  (should
   (equal
    "* H\n"
    (org-test-with-temp-text ":PROPERTIES:\n:TEST: t\n:END:\n* H\n:PROPERTIES:\n:TEST: nil\n:END:"
      (org-delete-property-globally "TEST")
      (buffer-string)))))

(ert-deftest test-org/find-property ()
  "Test `org-find-property' specifications."
  ;; Regular test.
  (should
   (= 1
      (org-test-with-temp-text "* H\n:PROPERTIES:\n:PROP: value\n:END:"
	(org-find-property "prop"))))
  ;; Find properties in top-level property drawer.
  (should
   (= 1
      (org-test-with-temp-text ":PROPERTIES:\n:PROP: value\n:END:"
	(org-find-property "prop"))))
  ;; Ignore false positives.
  (should
   (= 27
      (org-test-with-temp-text
	  "* H1\n:DRAWER:\n:A: 1\n:END:\n* H2\n:PROPERTIES:\n:A: 1\n:END:"
	(org-find-property "A"))))
  ;; Return first entry found in buffer.
  (should
   (= 1
      (org-test-with-temp-text
	  "* H1\n:PROPERTIES:\n:A: 1\n:END:\n* H2\n:PROPERTIES:\n:<point>A: 1\n:END:"
	(org-find-property "A"))))
  ;; Only search visible part of the buffer.
  (should
   (= 31
      (org-test-with-temp-text
	  "* H1\n:PROPERTIES:\n:A: 1\n:END:\n* H2\n:PROPERTIES:\n:<point>A: 1\n:END:"
	(org-narrow-to-subtree)
	(org-find-property "A"))))
  ;; With optional argument, only find entries with a specific value.
  (should-not
   (org-test-with-temp-text "* H\n:PROPERTIES:\n:A: 1\n:END:"
     (org-find-property "A" "2")))
  (should
   (= 31
      (org-test-with-temp-text
	  "* H1\n:PROPERTIES:\n:A: 1\n:END:\n* H2\n:PROPERTIES:\n:A: 2\n:END:"
	(org-find-property "A" "2"))))
  ;; Use "nil" for explicit nil values.
  (should
   (= 31
      (org-test-with-temp-text
	  "* H1\n:PROPERTIES:\n:A: 1\n:END:\n* H2\n:PROPERTIES:\n:A: nil\n:END:"
	(org-find-property "A" "nil")))))

(ert-deftest test-org/entry-delete ()
  "Test `org-entry-delete' specifications."
  ;; Regular test.
  (should
   (string-match
    " *:PROPERTIES:\n *:B: +2\n *:END:"
    (org-test-with-temp-text "* H\n:PROPERTIES:\n:A: 1\n:B: 2\n:END:"
      (org-entry-delete (point) "A")
      (buffer-string))))
  ;; Also remove accumulated properties.
  (should-not
   (string-match
    ":A"
    (org-test-with-temp-text "* H\n:PROPERTIES:\n:A: 1\n:A+: 2\n:B: 3\n:END:"
      (org-entry-delete (point) "A")
      (buffer-string))))
  ;; When last property is removed, remove the property drawer.
  (should-not
   (string-match
    ":PROPERTIES:"
    (org-test-with-temp-text "* H\n:PROPERTIES:\n:A: 1\n:END:\nParagraph"
      (org-entry-delete (point) "A")
      (buffer-string))))
  ;; Return a non-nil value when some property was removed.
  (should
   (org-test-with-temp-text "* H\n:PROPERTIES:\n:A: 1\n:B: 2\n:END:"
     (org-entry-delete (point) "A")))
  (should-not
   (org-test-with-temp-text "* H\n:PROPERTIES:\n:A: 1\n:B: 2\n:END:"
     (org-entry-delete (point) "C")))
  ;; Special properties cannot be located in a drawer.  Allow to
  ;; remove them anyway, in case of user error.
  (should
   (org-test-with-temp-text "* H\n:PROPERTIES:\n:SCHEDULED: 1\n:END:"
     (org-entry-delete (point) "SCHEDULED"))))

(ert-deftest test-org/entry-get ()
  "Test `org-entry-get' specifications."
  ;; Regular test.
  (should
   (equal "1"
	  (org-test-with-temp-text ":PROPERTIES:\n:A: 1\n:END:"
	    (org-entry-get (point) "A"))))
  (should
   (equal "1"
	  (org-test-with-temp-text "* H\n:PROPERTIES:\n:A: 1\n:END:"
	    (org-entry-get (point) "A"))))
  ;; Ignore case.
  (should
   (equal "1"
	  (org-test-with-temp-text "* H\n:PROPERTIES:\n:A: 1\n:END:"
	    (org-entry-get (point) "a"))))
  ;; Handle extended values, both before and after base value.
  (should
   (equal "1 2 3"
	  (org-test-with-temp-text
	      "* H\n:PROPERTIES:\n:A+: 2\n:A: 1\n:A+: 3\n:END:"
	    (org-entry-get (point) "A"))))
  ;; Empty values are returned as the empty string.
  (should
   (equal ""
	  (org-test-with-temp-text "* H\n:PROPERTIES:\n:A:\n:END:"
	    (org-entry-get (point) "A"))))
  ;; Special nil value.  If LITERAL-NIL is non-nil, return "nil",
  ;; otherwise, return nil.
  (should-not
   (org-test-with-temp-text "* H\n:PROPERTIES:\n:A: nil\n:END:"
     (org-entry-get (point) "A")))
  (should
   (equal "nil"
	  (org-test-with-temp-text "* H\n:PROPERTIES:\n:A: nil\n:END:"
	    (org-entry-get (point) "A" nil t))))
  ;; Return nil when no property is found, independently on the
  ;; LITERAL-NIL argument.
  (should-not
   (org-test-with-temp-text "* H\n:PROPERTIES:\n:A: 1\n:END:"
     (org-entry-get (point) "B")))
  (should-not
   (org-test-with-temp-text "* H\n:PROPERTIES:\n:A: 1\n:END:"
     (org-entry-get (point) "B" nil t)))
  ;; Handle inheritance, when allowed.  Include extended values and
  ;; possibly global values.
  (should
   (equal
    "1"
    (org-test-with-temp-text ":PROPERTIES:\n:A: 1\n:END:\n* H"
      (org-entry-get (point-max) "A" t))))
  (should
   (equal
    "1"
    (org-test-with-temp-text "* H\n:PROPERTIES:\n:A: 1\n:END:\n** H2"
      (org-entry-get (point-max) "A" t))))
  (should
   (equal
    "1"
    (org-test-with-temp-text "* H\n:PROPERTIES:\n:A: 1\n:END:\n** H2"
      (let ((org-use-property-inheritance t))
	(org-entry-get (point-max) "A" 'selective)))))
  (should-not
   (org-test-with-temp-text "* H\n:PROPERTIES:\n:A: 1\n:END:\n** H2"
     (let ((org-use-property-inheritance nil))
       (org-entry-get (point-max) "A" 'selective))))
  (should
   (equal
    "1 2"
    (org-test-with-temp-text
	":PROPERTIES:\n:A: 1\n:END:\n* H\n:PROPERTIES:\n:A+: 2\n:END:"
      (org-entry-get (point-max) "A" t))))
  (should
   (equal
    "1 2"
    (org-test-with-temp-text
	"* H\n:PROPERTIES:\n:A: 1\n:END:\n** H2\n:PROPERTIES:\n:A+: 2\n:END:"
      (org-entry-get (point-max) "A" t))))
  (should
   (equal
    "1 2"
    (org-test-with-temp-text
	":PROPERTIES:\n:A: 1\n:END:\n* H1\n* H2\n:PROPERTIES:\n:A+: 2\n:END:"
      (org-entry-get (point-max) "A" t))))
  (should
   (equal
    "1 2"
    (org-test-with-temp-text
	"* H1\n:PROPERTIES:\n:A: 1\n:END:\n* H2.1\n* H2.2\n:PROPERTIES:\n:A+: 2\n:END:"
      (org-entry-get (point-max) "A" t))))
  (should
   (equal "1"
	  (org-test-with-temp-text
	      "#+PROPERTY: A 0\n* H\n:PROPERTIES:\n:A: 1\n:END:"
	    (org-mode-restart)
	    (org-entry-get (point-max) "A" t))))
  (should
   (equal "0 1"
	  (org-test-with-temp-text
	      "#+PROPERTY: A 0\n* H\n:PROPERTIES:\n:A+: 1\n:END:"
	    (org-mode-restart)
	    (org-entry-get (point-max) "A" t))))
  ;; document level property-drawer has precedance over
  ;; global-property by PROPERTY-keyword.
  (should
   (equal "0 2"
	  (org-test-with-temp-text
	      ":PROPERTIES:\n:A: 0\n:END:\n#+PROPERTY: A 1\n* H\n:PROPERTIES:\n:A+: 2\n:END:"
	    (org-mode-restart)
	    (org-entry-get (point-max) "A" t)))))

(ert-deftest test-org/entry-properties ()
  "Test `org-entry-properties' specifications."
  ;; Get "ITEM" property.
  (should
   (equal "H"
	  (org-test-with-temp-text "* TODO H"
	    (cdr (assoc "ITEM" (org-entry-properties nil "ITEM"))))))
  (should
   (equal "H"
	  (org-test-with-temp-text "* TODO H"
	    (cdr (assoc "ITEM" (org-entry-properties))))))
  ;; Get "TODO" property.  TODO keywords are case sensitive.
  (should
   (equal "TODO"
	  (org-test-with-temp-text "* TODO H"
	    (cdr (assoc "TODO" (org-entry-properties nil "TODO"))))))
  (should
   (equal "TODO"
	  (org-test-with-temp-text "* TODO H"
	    (cdr (assoc "TODO" (org-entry-properties))))))
  (should-not
   (org-test-with-temp-text "* H"
     (assoc "TODO" (org-entry-properties nil "TODO"))))
  (should-not
   (org-test-with-temp-text "* todo H"
     (assoc "TODO" (org-entry-properties nil "TODO"))))
  ;; Get "PRIORITY" property.
  (should
   (equal "A"
	  (org-test-with-temp-text "* [#A] H"
	    (cdr (assoc "PRIORITY" (org-entry-properties nil "PRIORITY"))))))
  (should
   (equal "A"
	  (org-test-with-temp-text "* [#A] H"
	    (cdr (assoc "PRIORITY" (org-entry-properties))))))
  (should
   (equal (char-to-string org-priority-default)
	  (org-test-with-temp-text "* H"
	    (cdr (assoc "PRIORITY" (org-entry-properties nil "PRIORITY"))))))
  ;; Get "FILE" property.
  (should
   (org-test-with-temp-text-in-file "* H\nParagraph"
     (file-equal-p (cdr (assoc "FILE" (org-entry-properties nil "FILE")))
		   (buffer-file-name))))
  (should
   (org-test-with-temp-text-in-file "* H\nParagraph"
     (file-equal-p (cdr (assoc "FILE" (org-entry-properties)))
		   (buffer-file-name))))
  (should-not
   (org-test-with-temp-text "* H\nParagraph"
     (cdr (assoc "FILE" (org-entry-properties nil "FILE")))))
  ;; Get "TAGS" property.
  (should
   (equal ":tag1:tag2:"
	  (org-test-with-temp-text "* H :tag1:tag2:"
	    (cdr (assoc "TAGS" (org-entry-properties nil "TAGS"))))))
  (should
   (equal ":tag1:tag2:"
	  (org-test-with-temp-text "* H :tag1:tag2:"
	    (cdr (assoc "TAGS" (org-entry-properties))))))
  (should-not
   (org-test-with-temp-text "* H"
     (cdr (assoc "TAGS" (org-entry-properties nil "TAGS")))))
  ;; Get "ALLTAGS" property.
  (should
   (equal ":tag1:tag2:"
	  (org-test-with-temp-text "* H :tag1:\n<point>** H2 :tag2:"
	    (cdr (assoc "ALLTAGS" (org-entry-properties nil "ALLTAGS"))))))
  (should
   (equal ":tag1:tag2:"
	  (org-test-with-temp-text "* H :tag1:\n<point>** H2 :tag2:"
	    (cdr (assoc "ALLTAGS" (org-entry-properties))))))
  (should-not
   (org-test-with-temp-text "* H"
     (cdr (assoc "ALLTAGS" (org-entry-properties nil "ALLTAGS")))))
  ;; Get "BLOCKED" property.
  (should
   (equal "t"
	  (org-test-with-temp-text "* TODO Blocked\n** DONE one\n** TODO two"
	    (let ((org-enforce-todo-dependencies t)
		  (org-blocker-hook
		   '(org-block-todo-from-children-or-siblings-or-parent)))
	      (cdr (assoc "BLOCKED" (org-entry-properties nil "BLOCKED")))))))
  (should
   (equal ""
	  (org-test-with-temp-text "* TODO Blocked\n** DONE one\n** DONE two"
	    (let ((org-enforce-todo-dependencies t)
		  (org-blocker-hook
		   '(org-block-todo-from-children-or-siblings-or-parent)))
	      (cdr (assoc "BLOCKED" (org-entry-properties nil "BLOCKED")))))))
  ;; Get "CLOSED", "DEADLINE" and "SCHEDULED" properties.
  (should
   (equal
    "[2012-03-29 thu.]"
    (org-test-with-temp-text "* H\nCLOSED: [2012-03-29 thu.]"
      (cdr (assoc "CLOSED" (org-entry-properties nil "CLOSED"))))))
  (should
   (equal
    "[2012-03-29 thu.]"
    (org-test-with-temp-text "* H\nCLOSED: [2012-03-29 thu.]"
      (cdr (assoc "CLOSED" (org-entry-properties))))))
  (should-not
   (org-test-with-temp-text "* H"
     (cdr (assoc "CLOSED" (org-entry-properties nil "CLOSED")))))
  (should
   (equal
    "<2014-03-04 tue.>"
    (org-test-with-temp-text "* H\nDEADLINE: <2014-03-04 tue.>"
      (cdr (assoc "DEADLINE" (org-entry-properties nil "DEADLINE"))))))
  (should
   (equal
    "<2014-03-04 tue.>"
    (org-test-with-temp-text "* H\nDEADLINE: <2014-03-04 tue.>"
      (cdr (assoc "DEADLINE" (org-entry-properties))))))
  (should-not
   (org-test-with-temp-text "* H"
     (cdr (assoc "DEADLINE" (org-entry-properties nil "DEADLINE")))))
  (should
   (equal
    "<2014-03-04 tue.>"
    (org-test-with-temp-text "* H\nSCHEDULED: <2014-03-04 tue.>"
      (cdr (assoc "SCHEDULED" (org-entry-properties nil "SCHEDULED"))))))
  (should
   (equal
    "<2014-03-04 tue.>"
    (org-test-with-temp-text "* H\nSCHEDULED: <2014-03-04 tue.>"
      (cdr (assoc "SCHEDULED" (org-entry-properties))))))
  (should-not
   (org-test-with-temp-text "* H"
     (cdr (assoc "SCHEDULED" (org-entry-properties nil "SCHEDULED")))))
  ;; Get "CATEGORY"
  (should
   (equal "cat"
	  (org-test-with-temp-text "#+CATEGORY: cat\n<point>* H"
	    (cdr (assoc "CATEGORY" (org-entry-properties))))))
  (should
   (equal "cat"
	  (org-test-with-temp-text "#+CATEGORY: cat\n<point>* H"
	    (cdr (assoc "CATEGORY" (org-entry-properties nil "CATEGORY"))))))
  (should
   (equal "cat"
	  (org-test-with-temp-text "* H\n:PROPERTIES:\n:CATEGORY: cat\n:END:"
	    (cdr (assoc "CATEGORY" (org-entry-properties nil "CATEGORY"))))))
  (should
   (equal "cat2"
	  (org-test-with-temp-text
	      (concat "* H\n:PROPERTIES:\n:CATEGORY: cat1\n:END:"
		      "\n"
		      "** H2\n:PROPERTIES:\n:CATEGORY: cat2\n:END:<point>")
	    (cdr (assoc "CATEGORY" (org-entry-properties nil "CATEGORY"))))))
  ;; Get "TIMESTAMP" and "TIMESTAMP_IA" properties.
  (should
   (equal "<2012-03-29 thu.>"
	  (org-test-with-temp-text "* Entry\n<2012-03-29 thu.>"
	    (cdr (assoc "TIMESTAMP" (org-entry-properties))))))
  (should
   (equal "[2012-03-29 thu.]"
	  (org-test-with-temp-text "* Entry\n[2012-03-29 thu.]"
	    (cdr (assoc "TIMESTAMP_IA" (org-entry-properties))))))
  (should
   (equal "<2012-03-29 thu.>"
	  (org-test-with-temp-text "* Entry\n[2014-03-04 tue.]<2012-03-29 thu.>"
	    (cdr (assoc "TIMESTAMP" (org-entry-properties nil "TIMESTAMP"))))))
  (should
   (equal "[2014-03-04 tue.]"
	  (org-test-with-temp-text "* Entry\n<2012-03-29 thu.>[2014-03-04 tue.]"
	    (cdr (assoc "TIMESTAMP_IA"
			(org-entry-properties nil "TIMESTAMP_IA"))))))
  (should-not
   (equal "<2012-03-29 thu.>"
	  (org-test-with-temp-text "* Current\n* Next\n<2012-03-29 thu.>"
	    (cdr (assoc "TIMESTAMP" (org-entry-properties))))))
  ;; Get standard properties.
  (should
   (equal "1"
	  (org-test-with-temp-text "* H\n:PROPERTIES:\n:A: 1\n:END:"
	    (cdr (assoc "A" (org-entry-properties nil 'standard))))))
  ;; Handle extended properties.
  (should
   (equal "1 2 3"
	  (org-test-with-temp-text
	      "* H\n:PROPERTIES:\n:A+: 2\n:A: 1\n:A+: 3\n:END:"
	    (cdr (assoc "A" (org-entry-properties nil 'standard))))))
  (should
   (equal "1 2 3"
	  (org-test-with-temp-text
	      "* H\n:PROPERTIES:\n:A+: 2\n:A: 1\n:a+: 3\n:END:"
	    (cdr (assoc "A" (org-entry-properties nil 'standard))))))
  ;; Ignore forbidden (special) properties.
  (should-not
   (org-test-with-temp-text "* H\n:PROPERTIES:\n:TODO: foo\n:END:"
     (cdr (assoc "TODO" (org-entry-properties nil 'standard))))))

(ert-deftest test-org/entry-put ()
  "Test `org-entry-put' specifications."
  ;; Error when not a string or nil.
  (should-error
   (org-test-with-temp-text "* H\n:PROPERTIES:\n:test: 1\n:END:"
     (org-entry-put 1 "test" 2)))
  ;; Error when property name is invalid.
  (should-error
   (org-test-with-temp-text "* H\n:PROPERTIES:\n:test: 1\n:END:"
     (org-entry-put 1 "no space" "value")))
  (should-error
   (org-test-with-temp-text "* H\n:PROPERTIES:\n:test: 1\n:END:"
     (org-entry-put 1 "" "value")))
  ;; Set "TODO" property.
  (should
   (string-match (regexp-quote " TODO H")
		 (org-test-with-temp-text "#+TODO: TODO | DONE\n<point>* H"
		   (org-entry-put (point) "TODO" "TODO")
		   (buffer-string))))
  (should
   (string-match (regexp-quote "* H")
		 (org-test-with-temp-text "#+TODO: TODO | DONE\n<point>* H"
		   (org-entry-put (point) "TODO" nil)
		   (buffer-string))))
  ;; Set "PRIORITY" property.
  (should
   (equal "* [#A] H"
	  (org-test-with-temp-text "* [#B] H"
	    (org-entry-put (point) "PRIORITY" "A")
	    (buffer-string))))
  (should
   (equal "* H"
	  (org-test-with-temp-text "* [#B] H"
	    (org-entry-put (point) "PRIORITY" nil)
	    (buffer-string))))
  ;; Set "SCHEDULED" property.
  (should
   (string-match "* H\n *SCHEDULED: <2014-03-04 .*?>"
		 (org-test-with-temp-text "* H"
		   (org-entry-put (point) "SCHEDULED" "2014-03-04")
		   (buffer-string))))
  (should
   (string= "* H\n"
	    (org-test-with-temp-text "* H\nSCHEDULED: <2014-03-04 tue.>"
	      (org-entry-put (point) "SCHEDULED" nil)
	      (buffer-string))))
  (should
   (string-match "* H\n *SCHEDULED: <2014-03-03 .*?>"
		 (org-test-with-temp-text "* H\nSCHEDULED: <2014-03-04 tue.>"
		   (org-entry-put (point) "SCHEDULED" "earlier")
		   (buffer-string))))
  (should
   (string-match "^ *SCHEDULED: <2014-03-05 .*?>"
		 (org-test-with-temp-text "* H\nSCHEDULED: <2014-03-04 tue.>"
		   (org-entry-put (point) "SCHEDULED" "later")
		   (buffer-string))))
  ;; Set "DEADLINE" property.
  (should
   (string-match "^ *DEADLINE: <2014-03-04 .*?>"
		 (org-test-with-temp-text "* H"
		   (org-entry-put (point) "DEADLINE" "2014-03-04")
		   (buffer-string))))
  (should
   (string= "* H\n"
	    (org-test-with-temp-text "* H\nDEADLINE: <2014-03-04 tue.>"
	      (org-entry-put (point) "DEADLINE" nil)
	      (buffer-string))))
  (should
   (string-match "^ *DEADLINE: <2014-03-03 .*?>"
		 (org-test-with-temp-text "* H\nDEADLINE: <2014-03-04 tue.>"
		   (org-entry-put (point) "DEADLINE" "earlier")
		   (buffer-string))))
  (should
   (string-match "^ *DEADLINE: <2014-03-05 .*?>"
		 (org-test-with-temp-text "* H\nDEADLINE: <2014-03-04 tue.>"
		   (org-entry-put (point) "DEADLINE" "later")
		   (buffer-string))))
  ;; Set "CATEGORY" property
  (should
   (string-match "^ *:CATEGORY: cat"
		 (org-test-with-temp-text "* H"
		   (org-entry-put (point) "CATEGORY" "cat")
		   (buffer-string))))
  ;; Regular properties, with or without pre-existing drawer.
  (should
   (string-match "^ *:A: +2$"
		 (org-test-with-temp-text "* H\n:PROPERTIES:\n:A: 1\n:END:"
		   (org-entry-put (point) "A" "2")
		   (buffer-string))))
  (should
   (string-match "^ *:A: +1$"
		 (org-test-with-temp-text "* H"
		   (org-entry-put (point) "A" "1")
		   (buffer-string))))
  ;; Special case: two consecutive headlines.
  (should
   (string-match "\\* A\n *:PROPERTIES:"
		 (org-test-with-temp-text "* A\n** B"
		   (org-entry-put (point) "A" "1")
		   (buffer-string)))))

(ert-deftest test-org/refresh-properties ()
  "Test `org-refresh-properties' specifications."
  (should
   (equal "1"
	  (org-test-with-temp-text "* H\n:PROPERTIES:\n:A: 1\n:END:"
	    (org-refresh-properties "A" 'org-test)
	    (get-text-property (point) 'org-test))))
  (should-not
   (org-test-with-temp-text "* H\n:PROPERTIES:\n:A: 1\n:END:"
     (org-refresh-properties "B" 'org-test)
     (get-text-property (point) 'org-test)))
  ;; Handle properties only defined with extension syntax, i.e.,
  ;; "PROPERTY+".
  (should
   (equal "1"
	  (org-test-with-temp-text "* H\n:PROPERTIES:\n:A+: 1\n:END:"
	    (org-refresh-properties "A" 'org-test)
	    (get-text-property (point) 'org-test))))
  ;; When property is inherited, add text property to the whole
  ;; sub-tree.
  (should
   (equal "1"
	  (org-test-with-temp-text
	      "* H1\n:PROPERTIES:\n:A: 1\n:END:\n<point>** H2"
	    (let ((org-use-property-inheritance t))
	      (org-refresh-properties "A" 'org-test))
	    (get-text-property (point) 'org-test))))
  ;; When a document level property-drawer is used, those properties
  ;; should work exactly like headline-properties as if at a
  ;; headline-level 0.
  (should
   (equal "1"
	  (org-test-with-temp-text
	      ":PROPERTIES:\n:A: 1\n:END:\n"
	    (org-mode-restart)
	    (let ((org-use-property-inheritance t))
	      (org-refresh-properties "A" 'org-test))
	    (get-text-property (point) 'org-test))))
  (should-not
   (equal "1"
	  (org-test-with-temp-text
	      ":PROPERTIES:\n:A: 1\n:END:\n<point>* H1"
	    (org-mode-restart)
	    (let ((org-use-property-inheritance nil))
	      (org-refresh-properties "A" 'org-test))
	    (get-text-property (point) 'org-test))))
  (should
   (equal "1"
	  (org-test-with-temp-text
	      ":PROPERTIES:\n:A: 1\n:END:\n<point>* H1"
	    (org-mode-restart)
	    (let ((org-use-property-inheritance t))
	      (org-refresh-properties "A" 'org-test))
	    (get-text-property (point) 'org-test))))
  (should
   (equal "2"
	  (org-test-with-temp-text
	      ":PROPERTIES:\n:A: 1\n:END:\n<point>* H1\n:PROPERTIES:\n:A: 2\n:END:"
	    (org-mode-restart)
	    (let ((org-use-property-inheritance t))
	      (org-refresh-properties "A" 'org-test))
	    (get-text-property (point) 'org-test))))
  ;; When property is inherited, use global value across the whole
  ;; buffer.  However local values have precedence, as well as the
  ;; document level property-drawer.
  (should-not
   (equal "1"
	  (org-test-with-temp-text "#+PROPERTY: A 1\n<point>* H1"
	    (org-mode-restart)
	    (let ((org-use-property-inheritance nil))
	      (org-refresh-properties "A" 'org-test))
	    (get-text-property (point) 'org-test))))
  (should
   (equal "1"
	  (org-test-with-temp-text "#+PROPERTY: A 1\n<point>* H1"
	    (org-mode-restart)
	    (let ((org-use-property-inheritance t))
	      (org-refresh-properties "A" 'org-test))
	    (get-text-property (point) 'org-test))))
  (should
   (equal "2"
	  (org-test-with-temp-text
	      "#+PROPERTY: A 1\n<point>* H\n:PROPERTIES:\n:A: 2\n:END:"
	    (org-mode-restart)
	    (let ((org-use-property-inheritance t))
	      (org-refresh-properties "A" 'org-test))
	    (get-text-property (point) 'org-test))))
  ;; When both keyword-property and document-level property-block is
  ;; defined, the property-block has precedance.
  (should
   (equal "1"
	  (org-test-with-temp-text
	      ":PROPERTIES:\n:A: 1\n:END:\n#+PROPERTY: A 2\n<point>* H1"
	    (org-mode-restart)
	    (let ((org-use-property-inheritance t))
	      (org-refresh-properties "A" 'org-test))
	    (get-text-property (point) 'org-test)))))

(ert-deftest test-org/refresh-category-properties ()
  "Test `org-refresh-category-properties' specifications"
  (should
   (equal "cat1"
	  (org-test-with-temp-text
	      ":PROPERTIES:\n:CATEGORY: cat1\n:END:"
	    (org-refresh-category-properties)
	    (get-text-property (point) 'org-category))))
  (should
   (equal "cat1"
	  (org-test-with-temp-text
	      "* H\n:PROPERTIES:\n:CATEGORY: cat1\n:END:"
	    (org-refresh-category-properties)
	    (get-text-property (point) 'org-category))))
  ;; Even though property-inheritance is deactivated, category
  ;; property should be inherited.  As described in
  ;; `org-use-property-inheritance'.
  (should
   (equal "cat1"
	  (org-test-with-temp-text
	      ":PROPERTIES:\n:CATEGORY: cat1\n:END:\n<point>* H"
	    (org-mode-restart)
	    (let ((org-use-property-inheritance nil))
	      (org-refresh-category-properties))
	    (get-text-property (point) 'org-category))))
  (should
   (equal "cat1"
	  (org-test-with-temp-text
	      ":PROPERTIES:\n:CATEGORY: cat1\n:END:\n<point>* H"
	    (org-mode-restart)
	    (let ((org-use-property-inheritance t))
	      (org-refresh-category-properties))
	    (get-text-property (point) 'org-category))))
  (should
   (equal "cat2"
	  (org-test-with-temp-text
	      ":PROPERTIES:\n:CATEGORY: cat1\n:END:\n<point>* H\n:PROPERTIES:\n:CATEGORY: cat2\n:END:\n"
	    (org-mode-restart)
	    (let ((org-use-property-inheritance t))
	      (org-refresh-category-properties))
	    (get-text-property (point) 'org-category)))))


;;; Refile

(ert-deftest test-org/refile-get-targets ()
  "Test `org-refile-get-targets' specifications."
  ;; :maxlevel includes all headings above specified value.
  (should
   (equal '("H1" "H2" "H3")
	  (org-test-with-temp-text "* H1\n** H2\n*** H3"
	    (let ((org-refile-use-outline-path nil)
		  (org-refile-targets `((nil :maxlevel . 3))))
	      (mapcar #'car (org-refile-get-targets))))))
  (should
   (equal '("H1" "H2")
	  (org-test-with-temp-text "* H1\n** H2\n*** H3"
	    (let ((org-refile-use-outline-path nil)
		  (org-refile-targets `((nil :maxlevel . 2))))
	      (mapcar #'car (org-refile-get-targets))))))
  ;; :level limits targets to headlines with the specified level.
  (should
   (equal '("H2")
	  (org-test-with-temp-text "* H1\n** H2\n*** H3"
	    (let ((org-refile-use-outline-path nil)
		  (org-refile-targets `((nil :level . 2))))
	      (mapcar #'car (org-refile-get-targets))))))
  ;; :tag limits targets to headlines with specified tag.
  (should
   (equal '("H1")
	  (org-test-with-temp-text "* H1 :foo:\n** H2\n*** H3 :bar:"
	    (let ((org-refile-use-outline-path nil)
		  (org-refile-targets `((nil :tag . "foo"))))
	      (mapcar #'car (org-refile-get-targets))))))
  ;; :todo limits targets to headlines with specified TODO keyword.
  (should
   (equal '("H2")
	  (org-test-with-temp-text "* H1\n** TODO H2\n*** DONE H3"
	    (let ((org-refile-use-outline-path nil)
		  (org-refile-targets `((nil :todo . "TODO"))))
	      (mapcar #'car (org-refile-get-targets))))))
  ;; :regexp filters targets matching provided regexp.
  (should
   (equal '("F2" "F3")
	  (org-test-with-temp-text "* H1\n** F2\n*** F3"
	    (let ((org-refile-use-outline-path nil)
		  (org-refile-targets `((nil :regexp . "F"))))
	      (mapcar #'car (org-refile-get-targets))))))
  ;; A nil `org-refile-targets' includes only top level headlines in
  ;; current buffer.
  (should
   (equal '("H1" "H2")
	  (org-test-with-temp-text "* H1\n** S1\n* H2"
	    (let ((org-refile-use-outline-path nil)
		  (org-refile-targets nil))
	      (mapcar #'car (org-refile-get-targets))))))
  ;; Return value is the union of the targets according to all the
  ;; defined rules.  However, prevent duplicates.
  (should
   (equal '("F2" "F3" "H1")
	  (org-test-with-temp-text "* TODO H1\n** F2\n*** F3"
	    (let ((org-refile-use-outline-path nil)
		  (org-refile-targets `((nil :regexp . "F")
					(nil :todo . "TODO"))))
	      (mapcar #'car (org-refile-get-targets))))))
  (should
   (equal '("F2" "F3" "H1")
	  (org-test-with-temp-text "* TODO H1\n** TODO F2\n*** F3"
	    (let ((org-refile-use-outline-path nil)
		  (org-refile-targets `((nil :regexp . "F")
					(nil :todo . "TODO"))))
	      (mapcar #'car (org-refile-get-targets))))))
  ;; When `org-refile-use-outline-path' is non-nil, provide targets as
  ;; paths.
  (should
   (equal '("H1" "H1/H2" "H1/H2/H3")
	  (org-test-with-temp-text "* H1\n** H2\n*** H3"
	    (let ((org-refile-use-outline-path t)
		  (org-refile-targets `((nil :maxlevel . 3))))
	      (mapcar #'car (org-refile-get-targets))))))
  ;; When providing targets as paths, escape forward slashes in
  ;; headings with backslashes.
  (should
   (equal '("H1\\/foo")
	  (org-test-with-temp-text "* H1/foo"
	    (let ((org-refile-use-outline-path t)
		  (org-refile-targets `((nil :maxlevel . 1))))
	      (mapcar #'car (org-refile-get-targets))))))
  ;; When `org-refile-use-outline-path' is `file', include file name
  ;; without directory in targets.
  (should
   (org-test-with-temp-text-in-file "* H1"
     (let* ((filename (buffer-file-name))
	    (org-refile-use-outline-path 'file)
	    (org-refile-targets `(((,filename) :level . 1))))
       (member (file-name-nondirectory filename)
	       (mapcar #'car (org-refile-get-targets))))))
  ;; When `org-refile-use-outline-path' is `full-file-path', include
  ;; full file name.
  (should
   (org-test-with-temp-text-in-file "* H1"
     (let* ((filename (file-truename (buffer-file-name)))
	    (org-refile-use-outline-path 'full-file-path)
	    (org-refile-targets `(((,filename) :level . 1))))
       (member filename (mapcar #'car (org-refile-get-targets))))))
  ;; When `org-refile-use-outline-path' is `buffer-name', include
  ;; buffer name.
  (should
   (org-test-with-temp-text "* H1"
     (let* ((org-refile-use-outline-path 'buffer-name)
	    (org-refile-targets `((nil :level . 1))))
       (member (buffer-name) (mapcar #'car (org-refile-get-targets)))))))



;;; Sparse trees

(ert-deftest test-org/match-sparse-tree ()
  "Test `org-match-sparse-tree' specifications."
  ;; Match tags.
  (should-not
   (org-test-with-temp-text "* H\n** H1 :tag:"
     (org-match-sparse-tree nil "tag")
     (search-forward "H1")
     (org-invisible-p2)))
  (should
   (org-test-with-temp-text "* H\n** H1 :tag:\n** H2 :tag2:"
     (org-match-sparse-tree nil "tag")
     (search-forward "H2")
     (org-invisible-p2)))
  ;; "-" operator for tags.
  (should-not
   (org-test-with-temp-text "* H\n** H1 :tag1:\n** H2 :tag1:tag2:"
     (org-match-sparse-tree nil "tag1-tag2")
     (search-forward "H1")
     (org-invisible-p2)))
  (should
   (org-test-with-temp-text "* H\n** H1 :tag1:\n** H2 :tag1:tag2:"
     (org-match-sparse-tree nil "tag1-tag2")
     (search-forward "H2")
     (org-invisible-p2)))
  ;; "&" operator for tags.
  (should
   (org-test-with-temp-text "* H\n** H1 :tag1:\n** H2 :tag1:tag2:"
     (org-match-sparse-tree nil "tag1&tag2")
     (search-forward "H1")
     (org-invisible-p2)))
  (should-not
   (org-test-with-temp-text "* H\n** H1 :tag1:\n** H2 :tag1:tag2:"
     (org-match-sparse-tree nil "tag1&tag2")
     (search-forward "H2")
     (org-invisible-p2)))
  ;; "|" operator for tags.
  (should-not
   (org-test-with-temp-text "* H\n** H1 :tag1:\n** H2 :tag1:tag2:"
     (org-match-sparse-tree nil "tag1|tag2")
     (search-forward "H1")
     (org-invisible-p2)))
  (should-not
   (org-test-with-temp-text "* H\n** H1 :tag1:\n** H2 :tag1:tag2:"
     (org-match-sparse-tree nil "tag1|tag2")
     (search-forward "H2")
     (org-invisible-p2)))
  ;; Regexp match on tags.
  (should-not
   (org-test-with-temp-text "* H\n** H1 :tag1:\n** H2 :foo:"
     (org-match-sparse-tree nil "{^tag.*}")
     (search-forward "H1")
     (org-invisible-p2)))
  (should
   (org-test-with-temp-text "* H\n** H1 :tag1:\n** H2 :foo:"
     (org-match-sparse-tree nil "{^tag.*}")
     (search-forward "H2")
     (org-invisible-p2)))
  ;; Match group tags.
  (should-not
   (org-test-with-temp-text
       "#+TAGS: { work : lab }\n* H\n** H1 :work:\n** H2 :lab:"
     (org-match-sparse-tree nil "work")
     (search-forward "H1")
     (org-invisible-p2)))
  (should-not
   (org-test-with-temp-text
       "#+TAGS: { work : lab }\n* H\n** H1 :work:\n** H2 :lab:"
     (org-match-sparse-tree nil "work")
     (search-forward "H2")
     (org-invisible-p2)))
  ;; Match group tags with hard brackets.
  (should-not
   (org-test-with-temp-text
       "#+TAGS: [ work : lab ]\n* H\n** H1 :work:\n** H2 :lab:"
     (org-match-sparse-tree nil "work")
     (search-forward "H1")
     (org-invisible-p2)))
  (should-not
   (org-test-with-temp-text
       "#+TAGS: [ work : lab ]\n* H\n** H1 :work:\n** H2 :lab:"
     (org-match-sparse-tree nil "work")
     (search-forward "H2")
     (org-invisible-p2)))
  ;; Match tags in hierarchies
  (should-not
   (org-test-with-temp-text
       "#+TAGS: [ Lev_1 : Lev_2 ]\n
#+TAGS: [ Lev_2 : Lev_3 ]\n
#+TAGS: { Lev_3 : Lev_4 }\n
* H\n** H1 :Lev_1:\n** H2 :Lev_2:\n** H3 :Lev_3:\n** H4 :Lev_4:"
     (org-match-sparse-tree nil "Lev_1")
     (search-forward "H4")
     (org-invisible-p2)))
  (should-not
   (org-test-with-temp-text
       "#+TAGS: [ Lev_1 : Lev_2 ]\n
#+TAGS: [ Lev_2 : Lev_3 ]\n
#+TAGS: { Lev_3 : Lev_4 }\n
* H\n** H1 :Lev_1:\n** H2 :Lev_2:\n** H3 :Lev_3:\n** H4 :Lev_4:"
     (org-match-sparse-tree nil "Lev_1+Lev_3")
     (search-forward "H4")
     (org-invisible-p2)))
  ;; Match regular expressions in tags
  (should-not
   (org-test-with-temp-text
       "#+TAGS: [ Lev : {Lev_[0-9]} ]\n* H\n** H1 :Lev_1:"
     (org-match-sparse-tree nil "Lev")
     (search-forward "H1")
     (org-invisible-p2)))
  (should
   (org-test-with-temp-text
       "#+TAGS: [ Lev : {Lev_[0-9]} ]\n* H\n** H1 :Lev_n:"
     (org-match-sparse-tree nil "Lev")
     (search-forward "H1")
     (org-invisible-p2)))
  ;; Match properties.
  (should
   (org-test-with-temp-text
       "* H\n** H1\n:PROPERTIES:\n:A: 1\n:END:\n** H2\n:PROPERTIES:\n:A: 2\n:END:"
     (org-match-sparse-tree nil "A=\"1\"")
     (search-forward "H2")
     (org-invisible-p2)))
  (should-not
   (org-test-with-temp-text "* H1\n** H2\n:PROPERTIES:\n:A: 1\n:END:"
     (org-match-sparse-tree nil "A=\"1\"")
     (search-forward "H2")
     (org-invisible-p2)))
  ;; Case is not significant when matching properties.
  (should-not
   (org-test-with-temp-text "* H1\n** H2\n:PROPERTIES:\n:A: 1\n:END:"
     (org-match-sparse-tree nil "a=\"1\"")
     (search-forward "H2")
     (org-invisible-p2)))
  (should-not
   (org-test-with-temp-text "* H1\n** H2\n:PROPERTIES:\n:a: 1\n:END:"
     (org-match-sparse-tree nil "A=\"1\"")
     (search-forward "H2")
     (org-invisible-p2)))
  ;; Match special LEVEL property.
  (should-not
   (org-test-with-temp-text "* H\n** H1\n*** H2"
     (let ((org-odd-levels-only nil)) (org-match-sparse-tree nil "LEVEL=2"))
     (search-forward "H1")
     (org-invisible-p2)))
  (should
   (org-test-with-temp-text "* H\n** H1\n*** H2"
     (let ((org-odd-levels-only nil)) (org-match-sparse-tree nil "LEVEL=2"))
     (search-forward "H2")
     (org-invisible-p2)))
  ;; Comparison operators when matching properties.
  (should
   (org-test-with-temp-text
       "* H\n** H1\nSCHEDULED: <2014-03-04 tue.>\n** H2\nSCHEDULED: <2012-03-29 thu.>"
     (org-match-sparse-tree nil "SCHEDULED<=\"<2013-01-01>\"")
     (search-forward "H1")
     (org-invisible-p2)))
  (should-not
   (org-test-with-temp-text
       "* H\n** H1\nSCHEDULED: <2014-03-04 tue.>\n** H2\nSCHEDULED: <2012-03-29 thu.>"
     (org-match-sparse-tree nil "SCHEDULED<=\"<2013-01-01>\"")
     (search-forward "H2")
     (org-invisible-p2)))
  ;; Regexp match on properties values.
  (should-not
   (org-test-with-temp-text
       "* H\n** H1\n:PROPERTIES:\n:A: foo\n:END:\n** H2\n:PROPERTIES:\n:A: bar\n:END:"
     (org-match-sparse-tree nil "A={f.*}")
     (search-forward "H1")
     (org-invisible-p2)))
  (should
   (org-test-with-temp-text
       "* H\n** H1\n:PROPERTIES:\n:A: foo\n:END:\n** H2\n:PROPERTIES:\n:A: bar\n:END:"
     (org-match-sparse-tree nil "A={f.*}")
     (search-forward "H2")
     (org-invisible-p2)))
  ;; With an optional argument, limit match to TODO entries.
  (should-not
   (org-test-with-temp-text "* H\n** TODO H1 :tag:\n** H2 :tag:"
     (org-match-sparse-tree t "tag")
     (search-forward "H1")
     (org-invisible-p2)))
  (should
   (org-test-with-temp-text "* H\n** TODO H1 :tag:\n** H2 :tag:"
     (org-match-sparse-tree t "tag")
     (search-forward "H2")
     (org-invisible-p2))))

(ert-deftest test-org/occur ()
  "Test `org-occur' specifications."
  ;; Count number of matches.
  (should
   (= 1
      (org-test-with-temp-text "* H\nA\n* H2"
	(org-occur "A"))))
  (should
   (= 2
      (org-test-with-temp-text "* H\nA\n* H2\nA"
	(org-occur "A"))))
  ;; Test CALLBACK optional argument.
  (should
   (= 0
      (org-test-with-temp-text "* H\nA\n* H2"
	(org-occur "A" nil (lambda () (equal (org-get-heading) "H2"))))))
  (should
   (= 1
      (org-test-with-temp-text "* H\nA\n* H2\nA"
	(org-occur "A" nil (lambda () (equal (org-get-heading) "H2"))))))
  ;; Case-fold searches according to `org-occur-case-fold-search'.
  (should
   (= 2
      (org-test-with-temp-text "Aa"
	(let ((org-occur-case-fold-search t)) (org-occur "A")))))
  (should
   (= 2
      (org-test-with-temp-text "Aa"
	(let ((org-occur-case-fold-search t)) (org-occur "a")))))
  (should
   (= 1
      (org-test-with-temp-text "Aa"
	(let ((org-occur-case-fold-search nil)) (org-occur "A")))))
  (should
   (= 1
      (org-test-with-temp-text "Aa"
	(let ((org-occur-case-fold-search nil)) (org-occur "a")))))
  (should
   (= 1
      (org-test-with-temp-text "Aa"
	(let ((org-occur-case-fold-search 'smart)) (org-occur "A")))))
  (should
   (= 2
      (org-test-with-temp-text "Aa"
	(let ((org-occur-case-fold-search 'smart)) (org-occur "a"))))))


;;; Tags

(ert-deftest test-org/tag-string-to-alist ()
  "Test `org-tag-string-to-alist' specifications."
  ;; Tag without selection key.
  (should (equal (org-tag-string-to-alist "tag1") '(("tag1"))))
  ;; Tag with selection key.
  (should (equal (org-tag-string-to-alist "tag1(t)") '(("tag1" . ?t))))
  ;; Tag group.
  (should
   (equal
    (org-tag-string-to-alist "[ group : t1 t2 ]")
    '((:startgrouptag) ("group") (:grouptags) ("t1") ("t2") (:endgrouptag))))
  ;; Mutually exclusive tags.
  (should (equal (org-tag-string-to-alist "{ tag1 tag2 }")
		 '((:startgroup) ("tag1") ("tag2") (:endgroup))))
  (should
   (equal
    (org-tag-string-to-alist "{ group : tag1 tag2 }")
    '((:startgroup) ("group") (:grouptags) ("tag1") ("tag2") (:endgroup)))))

(ert-deftest test-org/tag-alist-to-string ()
  "Test `org-tag-alist-to-string' specifications."
  (should (equal (org-tag-alist-to-string '(("tag1"))) "tag1"))
  (should (equal (org-tag-alist-to-string '(("tag1" . ?t))) "tag1(t)"))
  (should
   (equal
    (org-tag-alist-to-string
     '((:startgrouptag) ("group") (:grouptags) ("t1") ("t2") (:endgrouptag)))
    "[ group : t1 t2 ]"))
  (should
   (equal (org-tag-alist-to-string
	   '((:startgroup) ("tag1") ("tag2") (:endgroup)))
	  "{ tag1 tag2 }"))
  (should
   (equal
    (org-tag-alist-to-string
     '((:startgroup) ("group") (:grouptags) ("tag1") ("tag2") (:endgroup)))
    "{ group : tag1 tag2 }")))

(ert-deftest test-org/tag-alist-to-groups ()
  "Test `org-tag-alist-to-groups' specifications."
  (should
   (equal (org-tag-alist-to-groups
	   '((:startgroup) ("group") (:grouptags) ("t1") ("t2") (:endgroup)))
	  '(("group" "t1" "t2"))))
  (should
   (equal
    (org-tag-alist-to-groups
     '((:startgrouptag) ("group") (:grouptags) ("t1") ("t2") (:endgrouptag)))
    '(("group" "t1" "t2"))))
  (should-not
   (org-tag-alist-to-groups
    '((:startgroup) ("group") ("t1") ("t2") (:endgroup)))))

(ert-deftest test-org/tag-align ()
  "Test tags alignment."
  ;; Test aligning tags with different display width.
  (should
   ;;      12345678901234567890
   (equal "* Test         :abc:"
	  (org-test-with-temp-text "* Test :abc:"
	    (let ((org-tags-column -20)
		  (indent-tabs-mode nil))
	      (org-fix-tags-on-the-fly))
	    (buffer-string))))
  (should
   ;;      12345678901234567890
   (equal "* Test      :日本語:"
	  (org-test-with-temp-text "* Test :日本語:"
	    (let ((org-tags-column -20)
		  (indent-tabs-mode nil))
	      (org-fix-tags-on-the-fly))
	    (buffer-string))))
  ;; Make sure aligning tags do not skip invisible text.
  (should
   (equal "* [[linkx]] :tag:"
	  (org-test-with-temp-text "* [[link<point>]]     :tag:"
	    (let ((org-tags-column 0))
	      (org-fix-tags-on-the-fly)
	      (insert "x")
	      (buffer-string)))))
  ;; Aligning tags preserve position.
  (should
   (= 6 (org-test-with-temp-text "* 345 <point> :tag:"
	  (let ((org-tags-column 78)
		(indent-tabs-mode nil))
	    (org-fix-tags-on-the-fly))
	  (current-column))))
  ;; Aligning all tags in visible buffer.
  (should
   ;;              12345678901234567890
   (equal (concat "* Level 1      :abc:\n"
                  "** Level 2     :def:")
          (org-test-with-temp-text (concat "* Level 1 :abc:\n"
                                           "** Level 2 :def:")
            (let ((org-tags-column -20)
                  (indent-tabs-mode nil))
              ;; (org-align-tags :all) must work even when the point
              ;; is at the end of the buffer.
              (goto-char (point-max))
              (org-align-tags :all))
            (buffer-string)))))

(ert-deftest test-org/get-tags ()
  "Test `org-get-tags' specifications."
  ;; Standard test.
  (should
   (equal '("foo")
	  (org-test-with-temp-text "* Test :foo:" (org-get-tags))))
  (should
   (equal '("foo" "bar")
	  (org-test-with-temp-text "* Test :foo:bar:" (org-get-tags))))
  ;; Return nil when there is no tag.
  (should-not
   (org-test-with-temp-text "* Test" (org-get-tags)))
  ;; Tags are inherited from parent headlines.
  (should
   (equal '("tag")
	  (let ((org-use-tag-inheritance t))
	    (org-test-with-temp-text "* H0 :foo:\n* H1 :tag:\n<point>** H2"
	      (org-get-tags)))))
  ;; Tags are inherited from `org-file-tags'.
  (should
   (equal '("tag")
	  (org-test-with-temp-text "* H1"
	    (let ((org-file-tags '("tag"))
		  (org-use-tag-inheritance t))
	      (org-get-tags)))))
  ;; Only inherited tags have the `inherited' text property.
  (should
   (get-text-property 0 'inherited
		      (org-test-with-temp-text "* H1 :foo:\n** <point>H2 :bar:"
			(let ((org-use-tag-inheritance t))
			  (assoc-string "foo" (org-get-tags))))))
  (should-not
   (get-text-property 0 'inherited
		      (org-test-with-temp-text "* H1 :foo:\n** <point>H2 :bar:"
			(let ((org-use-tag-inheritance t))
			  (assoc-string "bar" (org-get-tags))))))
  ;; Obey to `org-use-tag-inheritance'.
  (should-not
   (org-test-with-temp-text "* H1 :foo:\n** <point>H2 :bar:"
     (let ((org-use-tag-inheritance nil))
       (assoc-string "foo" (org-get-tags)))))
  (should-not
   (org-test-with-temp-text "* H1 :foo:\n** <point>H2 :bar:"
     (let ((org-use-tag-inheritance nil)
	   (org-file-tags '("foo")))
       (assoc-string "foo" (org-get-tags)))))
  (should-not
   (org-test-with-temp-text "* H1 :foo:bar:\n** <point>H2 :baz:"
     (let ((org-use-tag-inheritance '("bar")))
       (assoc-string "foo" (org-get-tags)))))
  (should
   (org-test-with-temp-text "* H1 :foo:bar:\n** <point>H2 :baz:"
     (let ((org-use-tag-inheritance '("bar")))
       (assoc-string "bar" (org-get-tags)))))
  (should-not
   (org-test-with-temp-text "* H1 :foo:bar:\n** <point>H2 :baz:"
     (let ((org-use-tag-inheritance "b.*"))
       (assoc-string "foo" (org-get-tags)))))
  (should
   (org-test-with-temp-text "* H1 :foo:bar:\n** <point>H2 :baz:"
     (let ((org-use-tag-inheritance "b.*"))
       (assoc-string "bar" (org-get-tags)))))
  ;; When optional argument LOCAL is non-nil, ignore tag inheritance.
  (should
   (equal '("baz")
	  (org-test-with-temp-text "* H1 :foo:bar:\n** <point>H2 :baz:"
	    (let ((org-use-tag-inheritance t))
	      (org-get-tags nil t)))))
  ;; When optional argument POS is non-nil, get tags there instead.
  (should
   (equal '("foo")
	  (org-test-with-temp-text "* H1 :foo:\n* <point>H2 :bar:"
	    (org-get-tags 1))))
  ;; Make sure tags excluded from inheritance are returned if local
  (should
   (equal '("foo")
	  (org-test-with-temp-text "* Test :foo:"
            (let ((org-use-tag-inheritance t)
                  (org-tags-exclude-from-inheritance '("foo")))
	      (org-get-tags)))))
  ;; Test the collection of tags from #+filetags and parent tags.
  (should
   (equal '("a" "b" "c" "d")
	  (org-test-with-temp-text (concat "#+filetags: a\n"
					   "* Level 1 :b:\n"
					   "** Level 2 :c:\n"
					   "*** Level 3 :d:\n"
					   "<point>")
            (let ((org-use-tag-inheritance t))
	      (org-mode-restart) ;So that `org-file-tags' get populated from #+filetags
	      (org-get-tags)))))
  ;; Pathological case: tagged headline with an empty body.
  (should (org-test-with-temp-text "* :tag:" (org-get-tags))))

(ert-deftest test-org/set-tags ()
  "Test `org-set-tags' specifications."
  ;; Throw an error on invalid data.
  (should-error
   (org-test-with-temp-text "* H"
     (org-set-tags 'foo)))
  ;; `nil', an empty, and a blank string remove all tags.
  (should
   (equal "* H"
	  (org-test-with-temp-text "* H :tag1:tag2:"
	    (org-set-tags nil)
	    (buffer-string))))
  (should
   (equal "* H"
	  (org-test-with-temp-text "* H :tag1:tag2:"
	    (org-set-tags "")
	    (buffer-string))))
  (should
   (equal "* H"
	  (org-test-with-temp-text "* H :tag1:tag2:"
	    (org-set-tags " ")
	    (buffer-string))))
  ;; If there's nothing to remove, just bail out.
  (should
   (equal "* H"
	  (org-test-with-temp-text "* H"
	    (org-set-tags nil)
	    (buffer-string))))
  (should
   (equal "* "
	  (org-test-with-temp-text "* "
	    (org-set-tags nil)
	    (buffer-string))))
  ;; If DATA is a tag string, set current tags to it, even if it means
  ;; replacing old tags.
  (should
   (equal "* H :tag0:"
	  (org-test-with-temp-text "* H :tag1:tag2:"
	    (let ((org-tags-column 1)) (org-set-tags ":tag0:"))
	    (buffer-string))))
  (should
   (equal "* H :tag0:"
	  (org-test-with-temp-text "* H"
	    (let ((org-tags-column 1)) (org-set-tags ":tag0:"))
	    (buffer-string))))
  ;; If DATA is a list, set tags to this list, even if it means
  ;; replacing old tags.
  (should
   (equal "* H :tag0:"
	  (org-test-with-temp-text "* H :tag1:tag2:"
	    (let ((org-tags-column 1)) (org-set-tags '("tag0")))
	    (buffer-string))))
  (should
   (equal "* H :tag0:"
	  (org-test-with-temp-text "* H"
	    (let ((org-tags-column 1)) (org-set-tags '("tag0")))
	    (buffer-string))))
  ;; When set, apply `org-tags-sort-function'.
  (should
   (equal "* H :a:b:"
	  (org-test-with-temp-text "* H"
	    (let ((org-tags-column 1)
		  (org-tags-sort-function #'string<))
	      (org-set-tags '("b" "a"))
	      (buffer-string)))))
  ;; When new tags are identical to the previous ones, still align.
  (should
   (equal "* H :foo:"
	  (org-test-with-temp-text "* H     :foo:"
	    (let ((org-tags-column 1))
	      (org-set-tags '("foo"))
	      (buffer-string)))))
  ;; When tags have been changed, run `org-after-tags-change-hook'.
  (should
   (catch :return
     (org-test-with-temp-text "* H :foo:"
       (let ((org-after-tags-change-hook (lambda () (throw :return t))))
	 (org-set-tags '("bar"))
	 nil))))
  (should-not
   (catch :return
     (org-test-with-temp-text "* H      :foo:"
       (let ((org-after-tags-change-hook (lambda () (throw :return t))))
	 (org-set-tags '("foo"))
	 nil))))
  ;; Special case: handle empty headlines.
  (should
   (equal "* :tag0:"
	  (org-test-with-temp-text "* "
	    (let ((org-tags-column 1)) (org-set-tags '("tag0")))
	    (buffer-string))))
  ;; Modify buffer only when a tag change happens or alignment is
  ;; done.
  (should-not
   (org-test-with-temp-text "* H :foo:"
     (set-buffer-modified-p nil)
     (let ((org-tags-column 1)) (org-set-tags '("foo")))
     (buffer-modified-p)))
  (should
   (org-test-with-temp-text "* H :foo:"
     (set-buffer-modified-p nil)
     (let ((org-tags-column 10)) (org-set-tags '("foo")))
     (buffer-modified-p)))
  (should
   (org-test-with-temp-text "* H :foo:"
     (set-buffer-modified-p nil)
     (let ((org-tags-column 10)) (org-set-tags '("bar")))
     (buffer-modified-p)))
  ;; Pathological case: when setting tags of a folded headline, do not
  ;; let new tags being sucked into invisibility.
  (should-not
   (org-test-with-temp-text "* H1\nContent\n* H2\n\n Other Content"
     ;; Show only headlines
     (org-content)
     ;; Set NEXT tag on current entry
     (org-set-tags ":NEXT:")
     ;; Move point to that NEXT tag
     (search-forward "NEXT") (backward-word)
     ;; And it should be visible (i.e. no overlays)
     (overlays-at (point)))))

(ert-deftest test-org/set-tags-command ()
  "Test `org-set-tags-command' specifications"
  ;; Set tags at current headline.
  (should
   (equal "* H1 :foo:"
	  (org-test-with-temp-text "* H1"
	    (cl-letf (((symbol-function 'completing-read)
		       (lambda (&rest args) ":foo:")))
	      (let ((org-use-fast-tag-selection nil)
		    (org-tags-column 1))
		(org-set-tags-command)))
	    (buffer-string))))
  ;; Preserve position when called from the section below.
  (should
   (equal "* H1 :foo:\nContents"
	  (org-test-with-temp-text "* H1\n<point>Contents"
	    (cl-letf (((symbol-function 'completing-read)
		       (lambda (&rest args) ":foo:")))
	      (let ((org-use-fast-tag-selection nil)
		    (org-tags-column 1))
		(org-set-tags-command)))
	    (buffer-string))))
  (should-not
   (equal "* H1 :foo:\nContents2"
	  (org-test-with-temp-text "* H1\n<point>Contents2"
	    (cl-letf (((symbol-function 'completing-read)
		       (lambda (&rest args) ":foo:")))
	      (let ((org-use-fast-tag-selection nil)
		    (org-tags-column 1))
		(org-set-tags-command)))
	    (org-at-heading-p))))
  ;; Strip all forbidden characters from user-entered tags.
  (should
   (equal "* H1 :foo:"
	  (org-test-with-temp-text "* H1"
	    (cl-letf (((symbol-function 'completing-read)
		       (lambda (&rest args) ": foo *:")))
	      (let ((org-use-fast-tag-selection nil)
		    (org-tags-column 1))
		(org-set-tags-command)))
	    (buffer-string))))
  ;; When a region is active and
  ;; `org-loop-over-headlines-in-active-region' is non-nil, insert the
  ;; same value in all headlines in region.
  (should
   (equal "* H1 :foo:\nContents\n* H2 :foo:"
	  (org-test-with-temp-text "* H1\nContents\n* H2"
	    (cl-letf (((symbol-function 'completing-read)
		       (lambda (&rest args) ":foo:")))
	      (let ((org-use-fast-tag-selection nil)
		    (org-loop-over-headlines-in-active-region t)
		    (org-tags-column 1))
		(transient-mark-mode 1)
		(push-mark (point) t t)
		(goto-char (point-max))
		(org-set-tags-command)))
	    (buffer-string))))
  (should
   (equal "* H1\nContents\n* H2 :foo:"
	  (org-test-with-temp-text "* H1\nContents\n* H2"
	    (cl-letf (((symbol-function 'completing-read)
		       (lambda (&rest args) ":foo:")))
	      (let ((org-use-fast-tag-selection nil)
		    (org-loop-over-headlines-in-active-region nil)
		    (org-tags-column 1))
		(transient-mark-mode 1)
		(push-mark (point) t t)
		(goto-char (point-max))
		(org-set-tags-command)))
	    (buffer-string))))
  ;; With a C-u prefix argument, align all tags in the buffer.
  (should
   (equal "* H1 :foo:\n* H2 :bar:"
	  (org-test-with-temp-text "* H1    :foo:\n* H2    :bar:"
	    (let ((org-tags-column 1)) (org-set-tags-command '(4)))
	    (buffer-string))))
  ;; Point does not move with empty headline.
  (should
   (equal ":foo:"
	  (org-test-with-temp-text "* <point>"
	    (cl-letf (((symbol-function 'completing-read)
		       (lambda (&rest args) ":foo:")))
	      (let ((org-use-fast-tag-selection nil)
		    (org-tags-column 1))
		(org-set-tags-command)))
	    (buffer-substring (point) (line-end-position)))))
  ;; Point does not move at start of line.
  (should
   (equal "* H1 :foo:"
	  (org-test-with-temp-text "* H1"
	    (cl-letf (((symbol-function 'completing-read)
		       (lambda (&rest args) ":foo:")))
	      (let ((org-use-fast-tag-selection nil)
		    (org-tags-column 1))
		(org-set-tags-command)))
	    (buffer-substring (point) (line-end-position)))))
  ;; Point does not move when within *'s.
  (should
   (equal "* H1 :foo:"
	  (org-test-with-temp-text "*<point>* H1"
	    (cl-letf (((symbol-function 'completing-read)
		       (lambda (&rest args) ":foo:")))
	      (let ((org-use-fast-tag-selection nil)
		    (org-tags-column 1))
		(org-set-tags-command)))
	    (buffer-substring (point) (line-end-position)))))
  ;; Point workaround does not get fooled when looking at a space.
  (should
   (equal " b :foo:"
	  (org-test-with-temp-text "* a<point> b"
	    (cl-letf (((symbol-function 'completing-read)
		       (lambda (&rest args) ":foo:")))
	      (let ((org-use-fast-tag-selection nil)
		    (org-tags-column 1))
		(org-set-tags-command)))
	    (buffer-substring (point) (line-end-position)))))
  ;; Handle tags both set locally and inherited.
  (should
   (equal "b :foo:"
	  (org-test-with-temp-text "* a :foo:\n** <point>b :foo:"
	    (cl-letf (((symbol-function 'completing-read)
		       (lambda (prompt coll &optional pred req initial &rest args)
			 initial)))
	      (let ((org-use-fast-tag-selection nil)
		    (org-tags-column 1))
		(org-set-tags-command)))
	    (buffer-substring (point) (line-end-position))))))

(ert-deftest test-org/toggle-tag ()
  "Test `org-toggle-tag' specifications."
  ;; Insert missing tag.
  (should
   (equal "* H :tag:"
	  (org-test-with-temp-text "* H"
	    (let ((org-tags-column 1)) (org-toggle-tag "tag"))
	    (buffer-string))))
  (should
   (equal "* H :tag1:tag2:"
	  (org-test-with-temp-text "* H :tag1:"
	    (let ((org-tags-column 1)) (org-toggle-tag "tag2"))
	    (buffer-string))))
  ;; Remove existing tag.
  (should
   (equal "* H"
	  (org-test-with-temp-text "* H :tag:"
	    (org-toggle-tag "tag")
	    (buffer-string))))
  (should
   (equal "* H :tag1:"
	  (org-test-with-temp-text "* H :tag1:tag2:"
	    (let ((org-tags-column 1)) (org-toggle-tag "tag2"))
	    (buffer-string))))
  (should
   (equal "* H :tag2:"
	  (org-test-with-temp-text "* H :tag1:tag2:"
	    (let ((org-tags-column 1)) (org-toggle-tag "tag1"))
	    (buffer-string))))
  ;; With optional argument ONOFF set to `on', try to insert the tag,
  ;; even if its already there.
  (should
   (equal "* H :tag:"
	  (org-test-with-temp-text "* H"
	    (let ((org-tags-column 1)) (org-toggle-tag "tag" 'on))
	    (buffer-string))))
  (should
   (equal "* H :tag:"
	  (org-test-with-temp-text "* H :tag:"
	    (let ((org-tags-column 1)) (org-toggle-tag "tag" 'on))
	    (buffer-string))))
  ;; With optional argument ONOFF set to `off', try to remove the tag,
  ;; even if its not there.
  (should
   (equal "* H"
	  (org-test-with-temp-text "* H :tag:"
	    (org-toggle-tag "tag" 'off)
	    (buffer-string))))
  (should
   (equal "* H :tag:"
	  (org-test-with-temp-text "* H :tag:"
	    (let ((org-tags-column 1)) (org-toggle-tag "foo" 'off))
	    (buffer-string))))
  ;; Special case: Handle properly tag inheritance.  In particular, do
  ;; not set inherited tags.
  (should
   (equal "* H1 :tag:\n** H2 :tag2:tag:"
	  (org-test-with-temp-text "* H1 :tag:\n** <point>H2 :tag2:"
	    (let ((org-use-tag-inheritance t)
		  (org-tags-column 1))
	      (org-toggle-tag "tag"))
	    (buffer-string))))
  (should
   (equal "* H1 :tag1:tag2:\n** H2 :foo:"
	  (org-test-with-temp-text "* H1 :tag1:tag2:\n** <point>H2"
	    (let ((org-use-tag-inheritance t)
		  (org-tags-column 1))
	      (org-toggle-tag "foo"))
	    (buffer-string)))))

(ert-deftest test-org/tags-expand ()
  "Test `org-tags-expand' specifications."
  ;; Expand tag groups as a regexp enclosed within curly brackets.
  (should
   (equal "{\\<[ABC]\\>}"
	  (org-test-with-temp-text "#+TAGS: [ A : B C ]"
	    (org-mode-restart)
	    (let ((org-tag-alist-for-agenda nil)) (org-tags-expand "A")))))
  (should
   (equal "{\\<\\(?:Aa\\|Bb\\|Cc\\)\\>}"
	  (org-test-with-temp-text "#+TAGS: [ Aa : Bb Cc ]"
	    (org-mode-restart)
	    (let ((org-tag-alist-for-agenda nil)) (org-tags-expand "Aa")))))
  ;; Preserve operator before the regexp.
  (should
   (equal "+{\\<[ABC]\\>}"
	  (org-test-with-temp-text "#+TAGS: [ A : B C ]"
	    (org-mode-restart)
	    (let ((org-tag-alist-for-agenda nil)) (org-tags-expand "+A")))))
  (should
   (equal "-{\\<[ABC]\\>}"
	  (org-test-with-temp-text "#+TAGS: [ A : B C ]"
	    (org-mode-restart)
	    (let ((org-tag-alist-for-agenda nil)) (org-tags-expand "-A")))))
  ;; Handle "|" syntax.
  (should
   (equal "{\\<[ABC]\\>}|D"
	  (org-test-with-temp-text "#+TAGS: [ A : B C ]"
	    (org-mode-restart)
	    (let ((org-tag-alist-for-agenda nil)) (org-tags-expand "A|D")))))
  ;; Handle nested groups.
  (should
   (equal "{\\<[A-D]\\>}"
	  (org-test-with-temp-text "#+TAGS: [ A : B C ]\n#+TAGS: [ B : D ]"
	    (org-mode-restart)
	    (let ((org-tag-alist-for-agenda nil)) (org-tags-expand "A")))))
  ;; Expand multiple occurrences of the same group.
  (should
   (equal "{\\<[ABC]\\>}|{\\<[ABC]\\>}"
	  (org-test-with-temp-text "#+TAGS: [ A : B C ]"
	    (org-mode-restart)
	    (let ((org-tag-alist-for-agenda nil)) (org-tags-expand "A|A")))))
  ;; Preserve regexp matches.
  (should
   (equal "{A+}"
	  (org-test-with-temp-text "#+TAGS: [ A : B C ]"
	    (org-mode-restart)
	    (let ((org-tag-alist-for-agenda nil)) (org-tags-expand "{A+}")))))
  ;; Uppercase MATCH works with a non-nil DOWNCASED and SINGLE-AS-LIST.
  (should
   (equal (list "a" "b" "c")
	  (org-test-with-temp-text "#+TAGS: [ A : B C ]"
	    (org-mode-restart)
	    (let ((org-tag-alist-for-agenda nil))
	      (sort (org-tags-expand "A" t t) #'string-lessp))))))


;;; TODO keywords

(ert-deftest test-org/auto-repeat-maybe ()
  "Test `org-auto-repeat-maybe' specifications."
  ;; Do not auto repeat when there is no valid time stamp with
  ;; a repeater in the entry.
  (should-not
   (string-prefix-p
    "* TODO H"
    (let ((org-todo-keywords '((sequence "TODO" "DONE"))))
      (org-test-with-temp-text "* TODO H\n<2012-03-29 Thu>"
	(org-todo "DONE")
	(buffer-string)))))
  (should-not
   (string-prefix-p
    "* TODO H"
    (let ((org-todo-keywords '((sequence "TODO" "DONE"))))
      (org-test-with-temp-text "* TODO H\n# <2012-03-29 Thu>"
	(org-todo "DONE")
	(buffer-string)))))
  ;; When switching to DONE state, switch back to first TODO keyword
  ;; in sequence, or the same keyword if they have different types.
  (should
   (string-prefix-p
    "* TODO H"
    (let ((org-todo-keywords '((sequence "TODO" "DONE"))))
      (org-test-with-temp-text "* TODO H\n<2012-03-29 Thu +2y>"
	(org-todo "DONE")
	(buffer-string)))))
  (should
   (string-prefix-p
    "* KWD1 H"
    (let ((org-todo-keywords '((sequence "KWD1" "KWD2" "DONE"))))
      (org-test-with-temp-text "* KWD2 H\n<2012-03-29 Thu +2y>"
	(org-todo "DONE")
	(buffer-string)))))
  (should
   (string-prefix-p
    "* KWD2 H"
    (let ((org-todo-keywords '((type "KWD1" "KWD2" "DONE"))))
      (org-test-with-temp-text "* KWD2 H\n<2012-03-29 Thu +2y>"
	(org-todo "DONE")
	(buffer-string)))))
  ;; If there was no TODO keyword in the first place, do not insert
  ;; any either.
  (should
   (string-prefix-p
    "* H"
    (let ((org-todo-keywords '((sequence "TODO" "DONE"))))
      (org-test-with-temp-text "* H\n<2012-03-29 Thu +2y>"
	(org-todo "DONE")
	(buffer-string)))))
  ;; Revert to REPEAT_TO_STATE, if set.
  (should
   (string-prefix-p
    "* KWD2 H"
    (let ((org-todo-keywords '((sequence "KWD1" "KWD2" "DONE"))))
      (org-test-with-temp-text
	  "* KWD2 H
:PROPERTIES:
:REPEAT_TO_STATE: KWD2
:END:
<2012-03-29 Thu +2y>"
	(org-todo "DONE")
	(buffer-string)))))
  ;; When switching to DONE state, update base date.  If there are
  ;; multiple repeated time stamps, update them all.
  (should
   (string-match-p
    "<2014-03-29 .* \\+2y>"
    (let ((org-todo-keywords '((sequence "TODO" "DONE"))))
      (org-test-with-temp-text "* TODO H\n<2012-03-29 Thu +2y>"
	(org-todo "DONE")
	(buffer-string)))))
  (should
   (string-match-p
    "<2015-03-04 .* \\+1y>"
    (let ((org-todo-keywords '((sequence "TODO" "DONE"))))
      (org-test-with-temp-text
	  "* TODO H\n<2012-03-29 Thu. +2y>\n<2014-03-04 Tue +1y>"
	(org-todo "DONE")
	(buffer-string)))))
  ;; Throw an error if repeater unit is the hour and no time is
  ;; provided in the time-stamp.
  (should-error
   (let ((org-todo-keywords '((sequence "TODO" "DONE"))))
     (org-test-with-temp-text "* TODO H\n<2012-03-29 Thu +2h>"
       (org-todo "DONE")
       (buffer-string))))
  ;; Handle every repeater type using hours step.
  (should
   (string-match-p
    "2014-03-04 .* 02:00"
    (org-test-at-time "<2014-03-04 02:35>"
      (org-test-with-temp-text "* TODO H\n<2014-03-03 18:00 +8h>"
	(org-todo "DONE")
	(buffer-string)))))
  (should
   (string-match-p
    "2014-03-04 .* 10:00"
    (org-test-at-time "<2014-03-04 02:35>"
      (org-test-with-temp-text "* TODO H\n<2014-03-03 18:00 ++8h>"
	(org-todo "DONE")
	(buffer-string)))))
  (should
   (string-match-p
    "2014-03-04 .* 10:35"
    (org-test-at-time "<2014-03-04 02:35>"
      (org-test-with-temp-text "* TODO H\n<2014-03-03 18:00 .+8h>"
	(org-todo "DONE")
	(buffer-string)))))
  ;; Do not repeat inactive time stamps with a repeater.
  (should-not
   (string-match-p
    "\\[2014-03-29 .* \\+2y\\]"
    (let ((org-todo-keywords '((sequence "TODO" "DONE"))))
      (org-test-with-temp-text
	  "* TODO H\n[2012-03-29 Thu. +2y]"
	(org-todo "DONE")
	(buffer-string)))))
  ;; Do not repeat commented time stamps.
  (should-not
   (string-prefix-p
    "<2015-03-04 .* \\+1y>"
    (let ((org-todo-keywords '((sequence "TODO" "DONE"))))
      (org-test-with-temp-text
	  "* TODO H\n<2012-03-29 Thu +2y>\n# <2014-03-04 Tue +1y>"
	(org-todo "DONE")
	(buffer-string)))))
  (should-not
   (string-prefix-p
    "<2015-03-04 .* \\+1y>"
    (let ((org-todo-keywords '((sequence "TODO" "DONE"))))
      (org-test-with-temp-text
	  "* TODO H
<2012-03-29 Thu. +2y>
#+BEGIN_EXAMPLE
<2014-03-04 Tue +1y>
#+END_EXAMPLE"
	(org-todo "DONE")
	(buffer-string)))))
  ;; When `org-log-repeat' is non-nil or there is a CLOCK in the
  ;; entry, record time of last repeat.
  (should-not
   (string-match-p
    ":LAST_REPEAT:"
    (let ((org-todo-keywords '((sequence "TODO" "DONE")))
	  (org-log-repeat nil))
      (cl-letf (((symbol-function 'org-add-log-setup)
		 (lambda (&rest args) nil)))
	(org-test-with-temp-text "* TODO H\n<2012-03-29 Thu. +2y>"
	  (org-todo "DONE")
	  (buffer-string))))))
  (should
   (string-match-p
    ":LAST_REPEAT:"
    (let ((org-todo-keywords '((sequence "TODO" "DONE")))
	  (org-log-repeat t))
      (cl-letf (((symbol-function 'org-add-log-setup)
		 (lambda (&rest args) nil)))
	(org-test-with-temp-text "* TODO H\n<2012-03-29 Thu. +2y>"
	  (org-todo "DONE")
	  (buffer-string))))))
  (should
   (string-match-p
    ":LAST_REPEAT:"
    (let ((org-todo-keywords '((sequence "TODO" "DONE"))))
      (cl-letf (((symbol-function 'org-add-log-setup)
		 (lambda (&rest args) nil)))
	(org-test-with-temp-text
	    "* TODO H\n<2012-03-29 Thu +2y>\nCLOCK: [2012-03-29 Thu 16:40]"
	  (org-todo "DONE")
	  (buffer-string))))))
  ;; When a SCHEDULED entry has no repeater, remove it upon repeating
  ;; the entry as it is no longer relevant.
  (should-not
   (string-match-p
    "^SCHEDULED:"
    (let ((org-todo-keywords '((sequence "TODO" "DONE"))))
      (org-test-with-temp-text
	  "* TODO H\nSCHEDULED: <2014-03-04 Tue>\n<2012-03-29 Thu +2y>"
	(org-todo "DONE")
	(buffer-string)))))
  ;; Properly advance repeater even when a clock entry is specified
  ;; and `org-log-repeat' is nil.
  (should
   (string-match-p
    "SCHEDULED: <2014-03-29"
    (let ((org-log-repeat nil)
	  (org-todo-keywords '((sequence "TODO" "DONE"))))
      (org-test-with-temp-text
	  "* TODO H
SCHEDULED: <2012-03-29 Thu +2y>
CLOCK: [2012-03-29 Thu 10:00]--[2012-03-29 Thu 16:40] =>  6:40"
	(org-todo "DONE")
	(buffer-string))))))


;;; Timestamps API

(ert-deftest test-org/at-timestamp-p ()
  "Test `org-at-timestamp-p' specifications."
  (should
   (org-test-with-temp-text "<2012-03-29 Thu>"
     (org-at-timestamp-p)))
  (should-not
   (org-test-with-temp-text "2012-03-29 Thu"
     (org-at-timestamp-p)))
  ;; Test return values.
  (should
   (eq 'bracket
       (org-test-with-temp-text "<2012-03-29 Thu>"
	 (org-at-timestamp-p))))
  (should
   (eq 'year
       (org-test-with-temp-text "<<point>2012-03-29 Thu>"
	 (org-at-timestamp-p))))
  (should
   (eq 'month
       (org-test-with-temp-text "<2012-<point>03-29 Thu>"
	 (org-at-timestamp-p))))
  (should
   (eq 'day
       (org-test-with-temp-text "<2012-03-<point>29 Thu>"
	 (org-at-timestamp-p))))
  (should
   (eq 'day
       (org-test-with-temp-text "<2012-03-29 T<point>hu>"
	 (org-at-timestamp-p))))
  (should
   (wholenump
    (org-test-with-temp-text "<2012-03-29 Thu +2<point>y>"
      (org-at-timestamp-p))))
  (should
   (eq 'bracket
       (org-test-with-temp-text "<2012-03-29 Thu<point>>"
	 (org-at-timestamp-p))))
  (should
   (eq 'after
       (org-test-with-temp-text "<2012-03-29 Thu><point>»"
	 (org-at-timestamp-p))))
  ;; Test `inactive' optional argument.
  (should
   (org-test-with-temp-text "[2012-03-29 Thu]"
     (org-at-timestamp-p 'inactive)))
  (should-not
   (org-test-with-temp-text "[2012-03-29 Thu]"
     (org-at-timestamp-p)))
  ;; When optional argument is `agenda', recognize time-stamps in
  ;; planning info line, property drawers and clocks.
  (should
   (org-test-with-temp-text "* H\nSCHEDULED: <point><2012-03-29 Thu>"
     (org-at-timestamp-p 'agenda)))
  (should-not
   (org-test-with-temp-text "* H\nSCHEDULED: <point><2012-03-29 Thu>"
     (org-at-timestamp-p)))
  (should
   (org-test-with-temp-text
       "* H\n:PROPERTIES:\n:PROP: <point><2012-03-29 Thu>\n:END:"
     (org-at-timestamp-p 'agenda)))
  (should-not
   (org-test-with-temp-text
       "* H\n:PROPERTIES:\n:PROP: <point><2012-03-29 Thu>\n:END:"
     (org-at-timestamp-p)))
  (should
   (org-test-with-temp-text "CLOCK: <point>[2012-03-29 Thu]"
     (let ((org-agenda-include-inactive-timestamps t))
       (org-at-timestamp-p 'agenda))))
  (should-not
   (org-test-with-temp-text "CLOCK: <point>[2012-03-29 Thu]"
     (let ((org-agenda-include-inactive-timestamps t))
       (org-at-timestamp-p))))
  (should-not
   (org-test-with-temp-text "CLOCK: <point>[2012-03-29 Thu]"
     (let ((org-agenda-include-inactive-timestamps t))
       (org-at-timestamp-p 'inactive))))
  ;; When optional argument is `lax', match any part of the document
  ;; with Org timestamp syntax.
  (should
   (org-test-with-temp-text "# <2012-03-29 Thu><point>"
     (org-at-timestamp-p 'lax)))
  (should-not
   (org-test-with-temp-text "# <2012-03-29 Thu><point>"
     (org-at-timestamp-p)))
  (should
   (org-test-with-temp-text ": <2012-03-29 Thu><point>"
     (org-at-timestamp-p 'lax)))
  (should-not
   (org-test-with-temp-text ": <2012-03-29 Thu><point>"
     (org-at-timestamp-p)))
  (should
   (org-test-with-temp-text
       "#+BEGIN_EXAMPLE\n<2012-03-29 Thu><point>\n#+END_EXAMPLE"
     (org-at-timestamp-p 'lax)))
  (should-not
   (org-test-with-temp-text
       "#+BEGIN_EXAMPLE\n<2012-03-29 Thu><point>\n#+END_EXAMPLE"
     (org-at-timestamp-p)))
  ;; Optional argument `lax' also matches inactive timestamps.
  (should
   (org-test-with-temp-text "# [2012-03-29 Thu]<point>"
     (org-at-timestamp-p 'lax))))

(ert-deftest test-org/time-stamp ()
  "Test `org-time-stamp' specifications."
  ;; Insert chosen time stamp at point.
  (should
   (string-match
    "Te<2014-03-04 .*?>xt"
    (org-test-with-temp-text "Te<point>xt"
      (cl-letf (((symbol-function 'org-read-date)
		 (lambda (&rest args)
		   (apply #'encode-time (org-parse-time-string "2014-03-04")))))
	(org-time-stamp nil)
	(buffer-string)))))
  ;; With a prefix argument, also insert time.
  (should
   (string-match
    "Te<2014-03-04 .*? 00:41>xt"
    (org-test-with-temp-text "Te<point>xt"
      (cl-letf (((symbol-function 'org-read-date)
		 (lambda (&rest args)
		   (apply #'encode-time
			  (org-parse-time-string "2014-03-04 00:41")))))
	(org-time-stamp '(4))
	(buffer-string)))))
  ;; With two universal prefix arguments, insert an active timestamp
  ;; with the current time without prompting the user.
  (should
   (string-match
    "Te<2014-03-04 .*? 00:41>xt"
    (org-test-with-temp-text "Te<point>xt"
      (org-test-at-time "2014-03-04 00:41"
	(org-time-stamp '(16))
	(buffer-string)))))
  ;; When optional argument is non-nil, insert an inactive timestamp.
  (should
   (string-match
    "Te\\[2014-03-04 .*?\\]xt"
    (org-test-with-temp-text "Te<point>xt"
      (cl-letf (((symbol-function 'org-read-date)
		 (lambda (&rest args)
		   (apply #'encode-time (org-parse-time-string "2014-03-04")))))
	(org-time-stamp nil t)
	(buffer-string)))))
  ;; When called from a timestamp, replace existing one.
  (should
   (string-match
    "<2014-03-04 .*?>"
    (org-test-with-temp-text "<2012-03-29<point> thu.>"
      (cl-letf (((symbol-function 'org-read-date)
		 (lambda (&rest args)
		   (apply #'encode-time (org-parse-time-string "2014-03-04")))))
	(org-time-stamp nil)
	(buffer-string)))))
  (should
   (string-match
    "<2014-03-04 .*?>--<2014-03-04 .*?>"
    (org-test-with-temp-text "<2012-03-29<point> thu.>--<2014-03-04 tue.>"
      (cl-letf (((symbol-function 'org-read-date)
		 (lambda (&rest args)
		   (apply #'encode-time (org-parse-time-string "2014-03-04")))))
	(org-time-stamp nil)
	(buffer-string)))))
  ;; When replacing a timestamp, preserve repeater, if any.
  (should
   (string-match
    "<2014-03-04 .*? \\+2y>"
    (org-test-with-temp-text "<2012-03-29<point> thu. +2y>"
      (cl-letf (((symbol-function 'org-read-date)
		 (lambda (&rest args)
		   (apply #'encode-time (org-parse-time-string "2014-03-04")))))
	(org-time-stamp nil)
	(buffer-string)))))
  ;; When called twice in a raw, build a date range.
  (should
   (string-match
    "<2012-03-29 .*?>--<2014-03-04 .*?>"
    (org-test-with-temp-text "<2012-03-29 thu.><point>"
      (cl-letf (((symbol-function 'org-read-date)
		 (lambda (&rest args)
		   (apply #'encode-time (org-parse-time-string "2014-03-04")))))
	(let ((last-command 'org-time-stamp)
	      (this-command 'org-time-stamp))
	  (org-time-stamp nil))
	(buffer-string))))))

(ert-deftest test-org/timestamp-has-time-p ()
  "Test `org-timestamp-has-time-p' specifications."
  ;; With time.
  (should
   (org-test-with-temp-text "<2012-03-29 Thu 16:40>"
     (org-timestamp-has-time-p (org-element-context))))
  ;; Without time.
  (should-not
   (org-test-with-temp-text "<2012-03-29 Thu>"
     (org-timestamp-has-time-p (org-element-context)))))

(ert-deftest test-org/get-repeat ()
  "Test `org-get-repeat' specifications."
  (should
   (org-test-with-temp-text "* H\n<2012-03-29 Thu 16:40 +2y>"
     (org-get-repeat)))
  (should-not
   (org-test-with-temp-text "* H\n<2012-03-29 Thu 16:40>"
     (org-get-repeat)))
  ;; Return proper repeat string.
  (should
   (equal "+2y"
	  (org-test-with-temp-text "* H\n<2014-03-04 Tue 16:40 +2y>"
	    (org-get-repeat))))
  ;; Prevent false positive (commented or verbatim time stamps)
  (should-not
   (org-test-with-temp-text "* H\n# <2012-03-29 Thu 16:40>"
     (org-get-repeat)))
  (should-not
   (org-test-with-temp-text
       "* H\n#+BEGIN_EXAMPLE\n<2012-03-29 Thu 16:40>\n#+END_EXAMPLE"
     (org-get-repeat)))
  ;; Return nil when called before first heading.
  (should-not
   (org-test-with-temp-text "<2012-03-29 Thu 16:40 +2y>"
     (org-get-repeat)))
  ;; When called with an optional argument, extract repeater from that
  ;; string instead.
  (should (equal "+2y" (org-get-repeat "<2012-03-29 Thu 16:40 +2y>")))
  (should-not (org-get-repeat "<2012-03-29 Thu 16:40>")))

(ert-deftest test-org/timestamp-format ()
  "Test `org-timestamp-format' specifications."
  ;; Regular test.
  (should
   (equal
    "2012-03-29 16:40"
    (org-test-with-temp-text "<2012-03-29 Thu 16:40>"
      (org-timestamp-format (org-element-context) "%Y-%m-%d %R"))))
  ;; Range end.
  (should
   (equal
    "2012-03-29"
    (org-test-with-temp-text "[2011-07-14 Thu]--[2012-03-29 Thu]"
      (org-timestamp-format (org-element-context) "%Y-%m-%d" t)))))

(ert-deftest test-org/timestamp-split-range ()
  "Test `org-timestamp-split-range' specifications."
  ;; Extract range start (active).
  (should
   (equal '(2012 3 29)
	  (org-test-with-temp-text "<2012-03-29 Thu>--<2012-03-30 Fri>"
	    (let ((ts (org-timestamp-split-range (org-element-context))))
	      (mapcar (lambda (p) (org-element-property p ts))
		      '(:year-end :month-end :day-end))))))
  ;; Extract range start (inactive)
  (should
   (equal '(2012 3 29)
	  (org-test-with-temp-text "[2012-03-29 Thu]--[2012-03-30 Fri]"
	    (let ((ts (org-timestamp-split-range (org-element-context))))
	      (mapcar (lambda (p) (org-element-property p ts))
		      '(:year-end :month-end :day-end))))))
  ;; Extract range end (active).
  (should
   (equal '(2012 3 30)
	  (org-test-with-temp-text "<2012-03-29 Thu>--<2012-03-30 Fri>"
	    (let ((ts (org-timestamp-split-range
		       (org-element-context) t)))
	      (mapcar (lambda (p) (org-element-property p ts))
		      '(:year-end :month-end :day-end))))))
  ;; Extract range end (inactive)
  (should
   (equal '(2012 3 30)
	  (org-test-with-temp-text "[2012-03-29 Thu]--[2012-03-30 Fri]"
	    (let ((ts (org-timestamp-split-range
		       (org-element-context) t)))
	      (mapcar (lambda (p) (org-element-property p ts))
		      '(:year-end :month-end :day-end))))))
  ;; Return the timestamp if not a range.
  (should
   (org-test-with-temp-text "[2012-03-29 Thu]"
     (let* ((ts-orig (org-element-context))
	    (ts-copy (org-timestamp-split-range ts-orig)))
       (eq ts-orig ts-copy))))
  (should
   (org-test-with-temp-text "<%%(org-float t 4 2)>"
     (let* ((ts-orig (org-element-context))
	    (ts-copy (org-timestamp-split-range ts-orig)))
       (eq ts-orig ts-copy)))))

(ert-deftest test-org/timestamp-translate ()
  "Test `org-timestamp-translate' specifications."
  ;; Translate whole date range.
  (should
   (equal "<29>--<30>"
	  (org-test-with-temp-text "<2012-03-29 Thu>--<2012-03-30 Fri>"
	    (let ((org-display-custom-times t)
		  (org-time-stamp-custom-formats '("<%d>" . "<%d>")))
	      (org-timestamp-translate (org-element-context))))))
  ;; Translate date range start.
  (should
   (equal "<29>"
	  (org-test-with-temp-text "<2012-03-29 Thu>--<2012-03-30 Fri>"
	    (let ((org-display-custom-times t)
		  (org-time-stamp-custom-formats '("<%d>" . "<%d>")))
	      (org-timestamp-translate (org-element-context) 'start)))))
  ;; Translate date range end.
  (should
   (equal "<30>"
	  (org-test-with-temp-text "<2012-03-29 Thu>--<2012-03-30 Fri>"
	    (let ((org-display-custom-times t)
		  (org-time-stamp-custom-formats '("<%d>" . "<%d>")))
	      (org-timestamp-translate (org-element-context) 'end)))))
  ;; Translate time range.
  (should
   (equal "<08>--<16>"
	  (org-test-with-temp-text "<2012-03-29 Thu 8:30-16:40>"
	    (let ((org-display-custom-times t)
		  (org-time-stamp-custom-formats '("<%d>" . "<%H>")))
	      (org-timestamp-translate (org-element-context))))))
  ;; Translate non-range timestamp.
  (should
   (equal "<29>"
	  (org-test-with-temp-text "<2012-03-29 Thu>"
	    (let ((org-display-custom-times t)
		  (org-time-stamp-custom-formats '("<%d>" . "<%d>")))
	      (org-timestamp-translate (org-element-context))))))
  ;; Do not change `diary' timestamps.
  (should
   (equal "<%%(org-float t 4 2)>"
	  (org-test-with-temp-text "<%%(org-float t 4 2)>"
	    (let ((org-display-custom-times t)
		  (org-time-stamp-custom-formats '("<%d>" . "<%d>")))
	      (org-timestamp-translate (org-element-context)))))))

(ert-deftest test-org/timestamp-from-string ()
  "Test `org-timestamp-from-string' specifications."
  ;; Return nil if argument is not a valid Org timestamp.
  (should-not (org-timestamp-from-string ""))
  (should-not (org-timestamp-from-string nil))
  (should-not (org-timestamp-from-string "<2012-03-29"))
  ;; Otherwise, return a valid Org timestamp object.
  (should
   (string-match-p "<2012-03-29 .+>"
		   (org-element-interpret-data
		    (org-timestamp-from-string "<2012-03-29 Thu>"))))
  (should
   (string-match-p "[2014-03-04 .+]"
		   (org-element-interpret-data
		    (org-timestamp-from-string "[2014-03-04 Tue]")))))

(ert-deftest test-org/timestamp-from-time ()
  "Test `org-timestamp-from-time' specifications."
  ;; Standard test.
  (should
   (string-match-p
    "<2012-03-29 .+>"
    (org-element-interpret-data
     (org-timestamp-from-time
      (apply #'encode-time
	     (org-parse-time-string "<2012-03-29 Thu 16:40>"))))))
  ;; When optional argument WITH-TIME is non-nil, provide time
  ;; information.
  (should
   (string-match-p
    "<2012-03-29 .+ 16:40>"
    (org-element-interpret-data
     (org-timestamp-from-time
      (apply #'encode-time
	     (org-parse-time-string "<2012-03-29 Thu 16:40>"))
      t))))
  ;; When optional argument INACTIVE is non-nil, return an inactive
  ;; timestamp.
  (should
   (string-match-p
    "[2012-03-29 .+]"
    (org-element-interpret-data
     (org-timestamp-from-time
      (apply #'encode-time
	     (org-parse-time-string "<2012-03-29 Thu 16:40>"))
      nil t)))))

(ert-deftest test-org/timestamp-to-time ()
  "Test `org-timestamp-to-time' specifications."
  (should
   (equal "2014-03-04"
	  (format-time-string
	   "%Y-%m-%d"
	   (org-timestamp-to-time
	    (org-timestamp-from-string "<2014-03-04 Tue>")))))
  (should
   (equal "2014-03-04"
	  (format-time-string
	   "%Y-%m-%d"
	   (org-timestamp-to-time
	    (org-timestamp-from-string "[2014-03-04 Tue]")))))
  (should
   (equal "2012-03-29 08:30"
	  (format-time-string
	   "%Y-%m-%d %H:%M"
	   (org-timestamp-to-time
	    (org-timestamp-from-string "<2012-03-29 Thu 08:30-16:40>")))))
  (should
   (equal "2012-03-29"
	  (format-time-string
	   "%Y-%m-%d"
	   (org-timestamp-to-time
	    (org-timestamp-from-string "<2012-03-29 Thu>--<2014-03-04 Tue>")))))
  (should
   (equal "2012-03-29"
	  (format-time-string
	   "%Y-%m-%d"
	   (org-timestamp-to-time
	    (org-timestamp-from-string "[2012-03-29 Thu]--[2014-03-04 Tue]")))))
  ;; When optional argument END is non-nil, use end of date range or
  ;; time range.
  (should
   (equal "2012-03-29 16:40"
	  (format-time-string
	   "%Y-%m-%d %H:%M"
	   (org-timestamp-to-time
	    (org-timestamp-from-string "<2012-03-29 Thu 08:30-16:40>")
	    t))))
  (should
   (equal "2014-03-04"
	  (format-time-string
	   "%Y-%m-%d"
	   (org-timestamp-to-time
	    (org-timestamp-from-string "<2012-03-29 Thu>--<2014-03-04 Tue>")
	    t))))
  (should
   (equal "2014-03-04"
	  (format-time-string
	   "%Y-%m-%d"
	   (org-timestamp-to-time
	    (org-timestamp-from-string "[2012-03-29 Thu]--[2014-03-04 Tue]")
	    t)))))


;;; Visibility

(ert-deftest test-org/hide-drawer-toggle ()
  "Test `org-hide-drawer-toggle' specifications."
  ;; Error when not at a drawer.
  (should-error
   (org-test-with-temp-text ":fake-drawer:\ncontents"
     (org-hide-drawer-toggle 'off)
     (get-char-property (line-end-position) 'invisible)))
  (should-error
   (org-test-with-temp-text
       "#+begin_example\n<point>:D:\nc\n:END:\n#+end_example"
     (org-hide-drawer-toggle t)))
  ;; Hide drawer.
  (should
   (org-test-with-temp-text ":drawer:\ncontents\n:end:"
     (org-hide-drawer-toggle)
     (get-char-property (line-end-position) 'invisible)))
  ;; Show drawer unconditionally when optional argument is `off'.
  (should-not
   (org-test-with-temp-text ":drawer:\ncontents\n:end:"
     (org-hide-drawer-toggle)
     (org-hide-drawer-toggle 'off)
     (get-char-property (line-end-position) 'invisible)))
  ;; Hide drawer unconditionally when optional argument is non-nil.
  (should
   (org-test-with-temp-text ":drawer:\ncontents\n:end:"
     (org-hide-drawer-toggle t)
     (get-char-property (line-end-position) 'invisible)))
  ;; Do not hide drawer when called from final blank lines.
  (should-not
   (org-test-with-temp-text ":drawer:\ncontents\n:end:\n\n<point>"
     (org-hide-drawer-toggle)
     (goto-char (point-min))
     (get-char-property (line-end-position) 'invisible)))
  ;; Don't leave point in an invisible part of the buffer when hiding
  ;; a drawer away.
  (should-not
   (org-test-with-temp-text ":drawer:\ncontents\n<point>:end:"
     (org-hide-drawer-toggle)
     (get-char-property (point) 'invisible))))

(ert-deftest test-org/hide-block-toggle ()
  "Test `org-hide-block-toggle' specifications."
  ;; Error when not at a block.
  (should-error
   (org-test-with-temp-text "#+BEGIN_QUOTE\ncontents"
     (org-hide-block-toggle 'off)
     (get-char-property (line-end-position) 'invisible)))
  ;; Hide block.
  (should
   (org-test-with-temp-text "#+BEGIN_CENTER\ncontents\n#+END_CENTER"
     (org-hide-block-toggle)
     (get-char-property (line-end-position) 'invisible)))
  (should
   (org-test-with-temp-text "#+BEGIN_EXAMPLE\ncontents\n#+END_EXAMPLE"
     (org-hide-block-toggle)
     (get-char-property (line-end-position) 'invisible)))
  ;; Show block unconditionally when optional argument is `off'.
  (should-not
   (org-test-with-temp-text "#+BEGIN_QUOTE\ncontents\n#+END_QUOTE"
     (org-hide-block-toggle)
     (org-hide-block-toggle 'off)
     (get-char-property (line-end-position) 'invisible)))
  (should-not
   (org-test-with-temp-text "#+BEGIN_QUOTE\ncontents\n#+END_QUOTE"
     (org-hide-block-toggle 'off)
     (get-char-property (line-end-position) 'invisible)))
  ;; Hide block unconditionally when optional argument is non-nil.
  (should
   (org-test-with-temp-text "#+BEGIN_QUOTE\ncontents\n#+END_QUOTE"
     (org-hide-block-toggle t)
     (get-char-property (line-end-position) 'invisible)))
  (should
   (org-test-with-temp-text "#+BEGIN_QUOTE\ncontents\n#+END_QUOTE"
     (org-hide-block-toggle)
     (org-hide-block-toggle t)
     (get-char-property (line-end-position) 'invisible)))
  ;; Do not hide block when called from final blank lines.
  (should-not
   (org-test-with-temp-text "#+BEGIN_QUOTE\ncontents\n#+END_QUOTE\n\n<point>"
     (org-hide-block-toggle)
     (goto-char (point-min))
     (get-char-property (line-end-position) 'invisible)))
  ;; Don't leave point in an invisible part of the buffer when hiding
  ;; a block away.
  (should-not
   (org-test-with-temp-text "#+BEGIN_QUOTE\ncontents\n<point>#+END_QUOTE"
     (org-hide-block-toggle)
     (get-char-property (point) 'invisible))))

(ert-deftest test-org/hide-block-toggle-maybe ()
  "Test `org-hide-block-toggle-maybe' specifications."
  (should
   (org-test-with-temp-text "#+BEGIN: dynamic\nContents\n#+END:"
     (org-hide-block-toggle-maybe)))
  (should-not
   (org-test-with-temp-text "Paragraph" (org-hide-block-toggle-maybe))))

(ert-deftest test-org/show-set-visibility ()
  "Test `org-show-set-visibility' specifications."
  ;; Do not throw an error before first heading.
  (should
   (org-test-with-temp-text "Preamble\n* Headline"
     (org-show-set-visibility 'tree)
     t))
  ;; Test all visibility spans, both on headline and in entry.
  (let ((list-visible-lines
	 (lambda (state headerp)
	   (org-test-with-temp-text "* Grandmother  (0)
** Uncle              (1)
*** Heir              (2)
** Father             (3)
   Ancestor text      (4)
*** Sister            (5)
    Sibling text      (6)
*** Self              (7)
    Match	      (8)
**** First born	      (9)
     Child text	      (10)
**** The other child  (11)
*** Brother	      (12)
** Aunt               (13)
"
	     (org-cycle t)
	     (search-forward (if headerp "Self" "Match"))
	     (org-show-set-visibility state)
	     (goto-char (point-min))
	     (let (result (line 0))
	       (while (not (eobp))
		 (unless (org-invisible-p2) (push line result))
		 (cl-incf line)
		 (forward-line))
	       (nreverse result))))))
    (should (equal '(0 7) (funcall list-visible-lines 'minimal t)))
    (should (equal '(0 7 8) (funcall list-visible-lines 'minimal nil)))
    (should (equal '(0 7 8 9) (funcall list-visible-lines 'local t)))
    (should (equal '(0 7 8 9) (funcall list-visible-lines 'local nil)))
    (should (equal '(0 3 7) (funcall list-visible-lines 'ancestors t)))
    (should (equal '(0 3 7 8) (funcall list-visible-lines 'ancestors nil)))
    (should (equal '(0 3 5 7 12) (funcall list-visible-lines 'lineage t)))
    (should (equal '(0 3 5 7 8 9 12) (funcall list-visible-lines 'lineage nil)))
    (should (equal '(0 1 3 5 7 12 13) (funcall list-visible-lines 'tree t)))
    (should (equal '(0 1 3 5 7 8 9 11 12 13)
		   (funcall list-visible-lines 'tree nil)))
    (should (equal '(0 1 3 4 5 7 12 13)
		   (funcall list-visible-lines 'canonical t)))
    (should (equal '(0 1 3 4 5 7 8 9 11 12 13)
		   (funcall list-visible-lines 'canonical nil))))
  ;; When point is hidden in a drawer or a block, make sure to make it
  ;; visible.
  (should-not
   (org-test-with-temp-text "#+BEGIN_QUOTE\nText\n#+END_QUOTE"
     (org-hide-block-toggle)
     (search-forward "Text")
     (org-show-set-visibility 'minimal)
     (org-invisible-p2)))
  (should-not
   (org-test-with-temp-text ":DRAWER:\nText\n:END:"
     (org-hide-drawer-toggle)
     (search-forward "Text")
     (org-show-set-visibility 'minimal)
     (org-invisible-p2)))
  (should-not
   (org-test-with-temp-text
       "#+BEGIN_QUOTE\n<point>:DRAWER:\nText\n:END:\n#+END_QUOTE"
     (org-hide-drawer-toggle)
     (forward-line -1)
     (org-hide-block-toggle)
     (search-forward "Text")
     (org-show-set-visibility 'minimal)
     (org-invisible-p2))))

(defun test-org/copy-visible ()
  "Test `org-copy-visible' specifications."
  (should
   (equal "Foo"
	  (org-test-with-temp-text "Foo"
	    (let ((kill-ring nil))
	      (org-copy-visible (point-min) (point-max))
	      (current-kill 0 t)))))
  ;; Skip invisible characters by text property.
  (should
   (equal "Foo"
	  (org-test-with-temp-text #("F<hidden>oo" 1 7 (invisible t))
	    (let ((kill-ring nil))
	      (org-copy-visible (point-min) (point-max))
	      (current-kill 0 t)))))
  ;; Skip invisible characters by overlay.
  (should
   (equal "Foo"
	  (org-test-with-temp-text "F<hidden>oo"
	    (let ((o (make-overlay 2 10)))
	      (overlay-put o 'invisible t))
	    (let ((kill-ring nil))
	      (org-copy-visible (point-min) (point-max))
	      (current-kill 0 t)))))
  ;; Handle invisible characters at the beginning and the end of the
  ;; buffer.
  (should
   (equal "Foo"
	  (org-test-with-temp-text #("<hidden>Foo" 0 8 (invisible t))
	    (let ((kill-ring nil))
	      (org-copy-visible (point-min) (point-max))
	      (current-kill 0 t)))))
  (should
   (equal "Foo"
	  (org-test-with-temp-text #("Foo<hidden>" 3 11 (invisible t))
	    (let ((kill-ring nil))
	      (org-copy-visible (point-min) (point-max))
	      (current-kill 0 t)))))
  ;; Handle multiple visible parts.
  (should
   (equal "abc"
	  (org-test-with-temp-text
	      #("aXbXc" 1 2 (invisible t) 3 4 (invisible t))
	    (let ((kill-ring nil))
	      (org-copy-visible (point-min) (point-max))
	      (current-kill 0 t))))))

(ert-deftest test-org/set-visibility-according-to-property ()
  "Test `org-set-visibility-according-to-property' specifications."
  ;; "folded" state.
  (should
   (org-test-with-temp-text
       "
* a
:PROPERTIES:
:VISIBILITY: folded
:END:
** <point>b"
     (org-set-visibility-according-to-property)
     (invisible-p (point))))
  ;; "children" state.
  (should
   (org-test-with-temp-text
       "
* a
:PROPERTIES:
:VISIBILITY: children
:END:
** b
<point>Contents
** c"
     (org-set-visibility-according-to-property)
     (invisible-p (point))))
  (should
   (org-test-with-temp-text
       "
* a
:PROPERTIES:
:VISIBILITY: children
:END:
** b
Contents
*** <point>c"
     (org-set-visibility-according-to-property)
     (invisible-p (point))))
  ;; "content" state.
  (should
   (org-test-with-temp-text
       "
* a
:PROPERTIES:
:VISIBILITY: content
:END:
** b
<point>Contents
*** c"
     (org-set-visibility-according-to-property)
     (invisible-p (point))))
  (should
   (org-test-with-temp-text
       "
* a
:PROPERTIES:
:VISIBILITY: content
:END:
** b
Contents
*** <point>c"
     (org-set-visibility-according-to-property)
     (not (invisible-p (point)))))
  ;; "showall" state.
  (should
   (org-test-with-temp-text
       "
* a
:PROPERTIES:
:VISIBILITY: showall
:END:
** b
<point>Contents
*** c"
     (org-set-visibility-according-to-property)
     (not (invisible-p (point)))))
  (should
   (org-test-with-temp-text
       "
* a
:PROPERTIES:
:VISIBILITY: showall
:END:
** b
Contents
*** <point>c"
     (org-set-visibility-according-to-property)
     (not (invisible-p (point)))))
  ;; When VISIBILITY properties are nested, ignore inner ones.
  (should
   (org-test-with-temp-text
       "
* A
:PROPERTIES:
:VISIBILITY: folded
:END:
** <point>B
:PROPERTIES:
:VISIBILITY: folded
:END:"
     (org-set-visibility-according-to-property)
     (invisible-p (point)))))

(ert-deftest test-org/visibility-show-branches ()
  "Test visibility of inline archived subtrees."
  (org-test-with-temp-text
   "* Foo<point>
** Bar :ARCHIVE:
*** Baz
"
   (org-kill-note-or-show-branches)
   (should (org-invisible-p (- (point-max) 2)))))


;;; Yank and Kill

(ert-deftest test-org/paste-subtree ()
  "Test `org-paste-subtree' specifications."
  ;; Return an error if text to yank is not a set of subtrees.
  (should-error (org-paste-subtree nil "Text"))
  ;; Adjust level according to current one.
  (should
   (equal "* H\n* Text\n"
	  (org-test-with-temp-text "* H\n<point>"
	    (org-paste-subtree nil "* Text")
	    (buffer-string))))
  (should
   (equal "* H1\n** H2\n** Text\n"
	  (org-test-with-temp-text "* H1\n** H2\n<point>"
	    (org-paste-subtree nil "* Text")
	    (buffer-string))))
  ;; When not on a heading, move to next heading before yanking.
  (should
   (equal "* H1\nParagraph\n* Text\n* H2"
	  (org-test-with-temp-text "* H1\n<point>Paragraph\n* H2"
	    (org-paste-subtree nil "* Text")
	    (buffer-string))))
  ;; If point is between two headings, use the deepest level.
  (should
   (equal "* H1\n\n* Text\n* H2"
	  (org-test-with-temp-text "* H1\n<point>\n* H2"
	    (org-paste-subtree nil "* Text")
	    (buffer-string))))
  (should
   (equal "** H1\n\n** Text\n* H2"
	  (org-test-with-temp-text "** H1\n<point>\n* H2"
	    (org-paste-subtree nil "* Text")
	    (buffer-string))))
  (should
   (equal "* H1\n\n** Text\n** H2"
	  (org-test-with-temp-text "* H1\n<point>\n** H2"
	    (org-paste-subtree nil "* Text")
	    (buffer-string))))
  ;; When on an empty heading, after the stars, deduce the new level
  ;; from the number of stars.
  (should
   (equal "*** Text\n"
	  (org-test-with-temp-text "*** <point>"
	    (org-paste-subtree nil "* Text")
	    (buffer-string))))
  ;; Optional argument LEVEL forces a level for the subtree.
  (should
   (equal "* H\n*** Text\n"
	  (org-test-with-temp-text "* H<point>"
	    (org-paste-subtree 3 "* Text")
	    (buffer-string)))))

(ert-deftest test-org/cut-and-paste-subtree ()
  "Test `org-cut-subtree' and `org-paste-subtree'."
  (should
   (equal
    "* Two
two
* One
"
    (org-test-with-temp-text
     "* One
<point>* Two
two
"
     (call-interactively #'org-cut-subtree)
     (goto-char (point-min))
     (call-interactively #'org-paste-subtree)
     (buffer-string))))
  (should
   (equal
    "* One
* Two
"
    (org-test-with-temp-text
     "* One
<point>* Two
"
     (call-interactively #'org-cut-subtree)
     (backward-char)
     (call-interactively #'org-paste-subtree)
     (buffer-string)))))

(provide 'test-org)

;;; test-org.el ends here
