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

;;; Comments:

;; Template test file for Org-mode tests

;;; Code:


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
	    (progn (call-interactively 'comment-dwim)
		   (buffer-string)))))
  ;; No region selected, no comment on current line and line empty:
  ;; insert comment on this line.
  (should
   (equal "# \nParagraph"
	  (org-test-with-temp-text "\nParagraph"
	    (progn (call-interactively 'comment-dwim)
		   (buffer-string)))))
  ;; No region selected, and a comment on this line: indent it.
  (should
   (equal "* Headline\n  # Comment"
	  (org-test-with-temp-text "* Headline\n# Comment"
	    (progn (forward-line)
		   (let ((org-adapt-indentation t))
		     (call-interactively 'comment-dwim))
		   (buffer-string)))))
  ;; Also recognize single # at column 0 as comments.
  (should
   (equal "# Comment"
	  (org-test-with-temp-text "# Comment"
	    (progn (forward-line)
		   (call-interactively 'comment-dwim)
		   (buffer-string)))))
  ;; Region selected and only comments and blank lines within it:
  ;; un-comment all commented lines.
  (should
   (equal "Comment 1\n\nComment 2"
	  (org-test-with-temp-text "# Comment 1\n\n# Comment 2"
	    (progn
	      (transient-mark-mode 1)
	      (push-mark (point) t t)
	      (goto-char (point-max))
	      (call-interactively 'comment-dwim)
	      (buffer-string)))))
  ;; Region selected without comments: comment all lines if
  ;; `comment-empty-lines' is non-nil, only non-blank lines otherwise.
  (should
   (equal "# Comment 1\n\n# Comment 2"
	  (org-test-with-temp-text "Comment 1\n\nComment 2"
	    (progn
	      (transient-mark-mode 1)
	      (push-mark (point) t t)
	      (goto-char (point-max))
	      (let ((comment-empty-lines nil))
		(call-interactively 'comment-dwim))
	      (buffer-string)))))
  (should
   (equal "# Comment 1\n# \n# Comment 2"
	  (org-test-with-temp-text "Comment 1\n\nComment 2"
	    (progn
	      (transient-mark-mode 1)
	      (push-mark (point) t t)
	      (goto-char (point-max))
	      (let ((comment-empty-lines t))
		(call-interactively 'comment-dwim))
	      (buffer-string)))))
  ;; In front of a keyword without region, insert a new comment.
  (should
   (equal "# \n#+KEYWORD: value"
	  (org-test-with-temp-text "#+KEYWORD: value"
	    (progn (call-interactively 'comment-dwim)
		   (buffer-string)))))
  ;; In a source block, use appropriate syntax.
  (should
   (equal "  ;; "
	  (org-test-with-temp-text "#+BEGIN_SRC emacs-lisp\n\n#+END_SRC"
	    (forward-line)
	    (let ((org-edit-src-content-indentation 2))
	      (call-interactively 'comment-dwim))
	    (buffer-substring-no-properties (line-beginning-position) (point)))))
  (should
   (equal "#+BEGIN_SRC emacs-lisp\n  ;; a\n  ;; b\n#+END_SRC"
	  (org-test-with-temp-text "#+BEGIN_SRC emacs-lisp\na\nb\n#+END_SRC"
	    (forward-line)
	    (transient-mark-mode 1)
	    (push-mark (point) t t)
	    (forward-line 2)
	    (let ((org-edit-src-content-indentation 2))
	      (call-interactively 'comment-dwim))
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
    (flet ((current-time () (apply #'encode-time
				   (org-parse-time-string "2014-03-04"))))
      (org-read-date
       t nil "+1y" nil
       (apply #'encode-time (org-parse-time-string "2012-03-29"))))))
  (should
   (equal
    "2013-03-29"
    (flet ((current-time () (apply #'encode-time
				   (org-parse-time-string "2014-03-04"))))
      (org-read-date
       t nil "++1y" nil
       (apply #'encode-time (org-parse-time-string "2012-03-29"))))))
  ;; When `org-read-date-prefer-future' is non-nil, prefer future
  ;; dates (relatively to now) when incomplete.  Otherwise, use
  ;; default date.
  (should
   (equal
    "2014-04-01"
    (flet ((current-time () (apply #'encode-time
				   (org-parse-time-string "2014-03-04"))))
      (let ((org-read-date-prefer-future t))
	(org-read-date t nil "1")))))
  (should
   (equal
    "2013-03-04"
    (flet ((current-time () (apply #'encode-time
				   (org-parse-time-string "2012-03-29"))))
      (let ((org-read-date-prefer-future t))
	(org-read-date t nil "3-4")))))
  (should
   (equal
    "2012-03-04"
    (flet ((current-time () (apply #'encode-time
				   (org-parse-time-string "2012-03-29"))))
      (let ((org-read-date-prefer-future nil))
	(org-read-date t nil "3-4")))))
  ;; When set to `org-read-date-prefer-future' is set to `time', read
  ;; day is moved to tomorrow if specified hour is before current
  ;; time.  However, it only happens in no other part of the date is
  ;; specified.
  (should
   (equal
    "2012-03-30"
    (flet ((current-time () (apply #'encode-time
				   (org-parse-time-string "2012-03-29 16:40"))))
      (let ((org-read-date-prefer-future 'time))
	(org-read-date t nil "00:40" nil)))))
  (should-not
   (equal
    "2012-03-30"
    (flet ((current-time () (apply #'encode-time
				   (org-parse-time-string "2012-03-29 16:40"))))
      (let ((org-read-date-prefer-future 'time))
	(org-read-date t nil "29 00:40" nil)))))
  ;; Caveat: `org-read-date-prefer-future' always refers to current
  ;; time, not default time, when they differ.
  (should
   (equal
    "2014-04-01"
    (flet ((current-time
	    () (apply #'encode-time (org-parse-time-string "2014-03-04"))))
      (let ((org-read-date-prefer-future t))
	(org-read-date
	 t nil "1" nil
	 (apply #'encode-time (org-parse-time-string "2012-03-29")))))))
  (should
   (equal
    "2014-03-25"
    (flet ((current-time
	    () (apply #'encode-time (org-parse-time-string "2014-03-04"))))
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


;;; Drawers

(ert-deftest test-org/insert-property-drawer ()
  "Test `org-insert-property-drawer' specifications."
  ;; Error before first headline.
  (should-error (org-test-with-temp-text "" (org-insert-property-drawer)))
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

(ert-deftest test-org/fill-paragraph ()
  "Test `org-fill-paragraph' specifications."
  ;; At an Org table, align it.
  (should
   (equal "| a |\n"
	  (org-test-with-temp-text "|a|"
	    (org-fill-paragraph)
	    (buffer-string))))
  (should
   (equal "#+name: table\n| a |\n"
	  (org-test-with-temp-text "#+name: table\n| a |\n"
	    (org-fill-paragraph)
	    (buffer-string))))
  ;; At a paragraph, preserve line breaks.
  (org-test-with-temp-text "some \\\\\nlong\ntext"
    (let ((fill-column 20))
      (org-fill-paragraph)
      (should (equal (buffer-string) "some \\\\\nlong text"))))
  ;; Correctly fill a paragraph when point is at its very end.
  (should
   (equal "A B"
	  (org-test-with-temp-text "A\nB"
	    (let ((fill-column 20))
	      (goto-char (point-max))
	      (org-fill-paragraph)
	      (buffer-string)))))
  ;; Correctly fill the last paragraph of a greater element.
  (should
   (equal "#+BEGIN_CENTER\n- 012345\n  789\n#+END_CENTER"
	  (org-test-with-temp-text "#+BEGIN_CENTER\n- 012345 789\n#+END_CENTER"
	    (let ((fill-column 8))
	      (forward-line)
	      (end-of-line)
	      (org-fill-paragraph)
	      (buffer-string)))))
  ;; Correctly fill an element in a narrowed buffer.
  (should
   (equal "01234\n6"
	  (org-test-with-temp-text "01234 6789"
	    (let ((fill-column 5))
	      (narrow-to-region 1 8)
	      (org-fill-paragraph)
	      (buffer-string)))))
  ;; Handle `adaptive-fill-regexp' in paragraphs.
  (should
   (equal "> a b"
	  (org-test-with-temp-text "> a\n> b"
	    (let ((fill-column 5)
		  (adaptive-fill-regexp "[ \t]*>+[ \t]*"))
	      (org-fill-paragraph)
	      (buffer-string)))))
  ;; Special case: Fill first paragraph when point is at an item or
  ;; a plain-list or a footnote reference.
  (should
   (equal "- A B"
	  (org-test-with-temp-text "- A\n  B"
	    (let ((fill-column 20))
	      (org-fill-paragraph)
	      (buffer-string)))))
  (should
   (equal "[fn:1] A B"
	  (org-test-with-temp-text "[fn:1] A\nB"
	    (let ((fill-column 20))
	      (org-fill-paragraph)
	      (buffer-string)))))
  (org-test-with-temp-text "#+BEGIN_VERSE\nSome \\\\\nlong\ntext\n#+END_VERSE"
    (let ((fill-column 20))
      (org-fill-paragraph)
      (should (equal (buffer-string)
		     "#+BEGIN_VERSE\nSome \\\\\nlong\ntext\n#+END_VERSE"))))
  ;; Fill contents of `comment-block' elements.
  (should
   (equal
    (org-test-with-temp-text "#+BEGIN_COMMENT\nSome\ntext\n#+END_COMMENT"
      (let ((fill-column 20))
	(forward-line)
	(org-fill-paragraph)
	(buffer-string)))
    "#+BEGIN_COMMENT\nSome text\n#+END_COMMENT"))
  ;; Fill `comment' elements.
  (should
   (equal "  # A B"
	  (org-test-with-temp-text "  # A\n  # B"
	    (let ((fill-column 20))
	      (org-fill-paragraph)
	      (buffer-string)))))
  ;; Do not mix consecutive comments when filling one of them.
  (should
   (equal "# A B\n\n# C"
	  (org-test-with-temp-text "# A\n# B\n\n# C"
	    (let ((fill-column 20))
	      (org-fill-paragraph)
	      (buffer-string)))))
  ;; Use commented empty lines as separators when filling comments.
  (should
   (equal "# A B\n#\n# C"
	  (org-test-with-temp-text "# A\n# B\n#\n# C"
	    (let ((fill-column 20))
	      (org-fill-paragraph)
	      (buffer-string)))))
  ;; Handle `adaptive-fill-regexp' in comments.
  (should
   (equal "# > a b"
	  (org-test-with-temp-text "# > a\n# > b"
	    (let ((fill-column 20)
		  (adaptive-fill-regexp "[ \t]*>+[ \t]*"))
	      (org-fill-paragraph)
	      (buffer-string)))))
  ;; Do nothing at affiliated keywords.
  (org-test-with-temp-text "#+NAME: para\nSome\ntext."
    (let ((fill-column 20))
      (org-fill-paragraph)
      (should (equal (buffer-string) "#+NAME: para\nSome\ntext."))))
  ;; Do not move point after table when filling a table.
  (should-not
   (org-test-with-temp-text "| a | b |\n| c | d |\n"
     (forward-char)
     (org-fill-paragraph)
     (eobp))))

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
      (org-test-with-temp-text "* H\nA"
	(forward-line)
	(let ((org-adapt-indentation t)) (org-indent-line))
	(org-get-indentation))))
  (should
   (= 2
      (org-test-with-temp-text "* H\n\nA"
	(forward-line)
	(let ((org-adapt-indentation t)) (org-indent-line))
	(org-get-indentation))))
  (should
   (zerop
    (org-test-with-temp-text "* H\nA"
      (forward-line)
      (let ((org-adapt-indentation nil)) (org-indent-line))
      (org-get-indentation))))
  ;; Indenting preserves point position.
  (should
   (org-test-with-temp-text "* H\nAB"
     (forward-line)
     (forward-char)
     (let ((org-adapt-indentation t)) (org-indent-line))
     (looking-at "B")))
  ;; Do not change indentation at an item.
  (should
   (= 1
      (org-test-with-temp-text "* H\n - A"
	(forward-line)
	(let ((org-adapt-indentation t)) (org-indent-line))
	(org-get-indentation))))
  ;; On blank lines at the end of a list, indent like last element
  ;; within it if the line is still in the list.  If the last element
  ;; is an item, indent like its contents.  Otherwise, indent like the
  ;; whole list.
  (should
   (= 4
      (org-test-with-temp-text "* H\n- A\n  - AA\n"
	(goto-char (point-max))
	(let ((org-adapt-indentation t)) (org-indent-line))
	(org-get-indentation))))
  (should
   (zerop
    (org-test-with-temp-text "* H\n- A\n  - AA\n\n\n\n"
      (goto-char (point-max))
      (let ((org-adapt-indentation t)) (org-indent-line))
      (org-get-indentation))))
  (should
   (= 4
      (org-test-with-temp-text "* H\n- A\n  - \n"
	(goto-char (point-max))
	(let ((org-adapt-indentation t)) (org-indent-line))
	(org-get-indentation))))
  ;; Likewise, on a blank line at the end of a footnote definition,
  ;; indent at column 0 if line belongs to the definition.  Otherwise,
  ;; indent like the definition itself.
  (should
   (zerop
    (org-test-with-temp-text "* H\n[fn:1] Definition\n"
      (goto-char (point-max))
      (let ((org-adapt-indentation t)) (org-indent-line))
      (org-get-indentation))))
  (should
   (zerop
    (org-test-with-temp-text "* H\n[fn:1] Definition\n\n\n\n"
      (goto-char (point-max))
      (let ((org-adapt-indentation t)) (org-indent-line))
      (org-get-indentation))))
  ;; After the end of the contents of a greater element, indent like
  ;; the beginning of the element.
  (should
   (= 1
      (org-test-with-temp-text " #+BEGIN_CENTER\n  Contents\n#+END_CENTER"
	(forward-line 2)
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
      (org-test-with-temp-text "A\n\n  B\n\nC"
	(goto-char (point-max))
	(org-indent-line)
	(org-get-indentation))))
  (should
   (= 1
      (org-test-with-temp-text " A\n\n[fn:1] B\n\n\nC"
	(goto-char (point-max))
	(org-indent-line)
	(org-get-indentation))))
  (should
   (= 1
      (org-test-with-temp-text " #+BEGIN_CENTER\n  Contents\n#+END_CENTER"
	(forward-line 1)
	(org-indent-line)
	(org-get-indentation))))
  ;; Within code part of a source block, use language major mode if
  ;; `org-src-tab-acts-natively' is non-nil.  Otherwise, indent
  ;; according to line above.
  (should
   (= 6
      (org-test-with-temp-text "#+BEGIN_SRC emacs-lisp\n (and A\nB)\n#+END_SRC"
	(forward-line 2)
	(let ((org-src-tab-acts-natively t)
	      (org-edit-src-content-indentation 0))
	  (org-indent-line))
	(org-get-indentation))))
  (should
   (= 1
      (org-test-with-temp-text "#+BEGIN_SRC emacs-lisp\n (and A\nB)\n#+END_SRC"
	(forward-line 2)
	(let ((org-src-tab-acts-natively nil)
	      (org-edit-src-content-indentation 0))
	  (org-indent-line))
	(org-get-indentation))))
  ;; Otherwise, indent like the first non-blank line above.
  (should
   (zerop
    (org-test-with-temp-text "#+BEGIN_CENTER\nline1\n\n  line2\n#+END_CENTER"
      (forward-line 3)
      (org-indent-line)
      (org-get-indentation))))
  ;; Align node properties according to `org-property-format'.  Handle
  ;; nicely empty values.
  (should
   (equal "* H\n:PROPERTIES:\n:key:      value\n:END:"
	  (org-test-with-temp-text "* H\n:PROPERTIES:\n<point>:key: value\n:END:"
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
  ;; Ignore contents of verse blocks and example blocks.
  (should
   (equal "#+BEGIN_VERSE\n A\n  B\n#+END_VERSE"
	  (org-test-with-temp-text "#+BEGIN_VERSE\n A\n  B\n#+END_VERSE"
	    (org-indent-region (point-min) (point-max))
	    (buffer-string))))
  (should
   (equal "#+BEGIN_EXAMPLE\n A\n  B\n#+END_EXAMPLE"
	  (org-test-with-temp-text "#+BEGIN_EXAMPLE\n A\n  B\n#+END_EXAMPLE"
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
     (org-looking-at-p "b")))
  ;; Open link or timestamp under point when `org-return-follows-link'
  ;; is non-nil.
  (should
   (org-test-with-temp-text "Link [[target<point>]] <<target>>"
     (let ((org-return-follows-link t)
	   (org-link-search-must-match-exact-headline nil))
       (org-return))
     (org-looking-at-p "<<target>>")))
  (should-not
   (org-test-with-temp-text "Link [[target<point>]] <<target>>"
     (let ((org-return-follows-link nil)) (org-return))
     (org-looking-at-p "<<target>>")))
  (should
   (org-test-with-temp-text "* [[b][a<point>]]\n* b"
     (let ((org-return-follows-link t)) (org-return))
     (org-looking-at-p "* b")))
  (should
   (org-test-with-temp-text "Link [[target][/descipt<point>ion/]] <<target>>"
     (let ((org-return-follows-link t)
	   (org-link-search-must-match-exact-headline nil))
       (org-return))
     (org-looking-at-p "<<target>>")))
  ;; When `org-return-follows-link' is non-nil, tolerate links and
  ;; timestamps in comments, node properties, etc.
  (should
   (org-test-with-temp-text "# Comment [[target<point>]]\n <<target>>"
     (let ((org-return-follows-link t)
	   (org-link-search-must-match-exact-headline nil))
       (org-return))
     (org-looking-at-p "<<target>>")))
  ;; However, do not open link when point is in a table.
  (should
   (org-test-with-temp-text "| [[target<point>]] |\n| between |\n| <<target>> |"
     (let ((org-return-follows-link t)) (org-return))
     (org-looking-at-p "between")))
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
  ;; At the beginning of a line, turn it into a headline
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
  ;; When on a list, insert an item instead, unless called with an
  ;; universal argument or if list is invisible.  In this case, create
  ;; a new headline after contents.
  (should
   (equal "* H\n- item\n- "
	  (org-test-with-temp-text "* H\n- item<point>"
	    (let ((org-insert-heading-respect-content nil))
	      (org-insert-heading))
	    (buffer-string))))
  (should
   (equal "* H\n- item\n- item 2\n* "
	  (org-test-with-temp-text "* H\n- item<point>\n- item 2"
	    (let ((org-insert-heading-respect-content nil))
	      (org-insert-heading '(4)))
	    (buffer-string))))
  (should
   (equal "* H\n- item\n* "
	  (org-test-with-temp-text "* H\n- item"
	    (org-cycle)
	    (goto-char (point-max))
	    (let ((org-insert-heading-respect-content nil)) (org-insert-heading))
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
  ;; Corner case: correctly insert a headline after an empty one.
  (should
   (equal "* \n* "
	  (org-test-with-temp-text "* <point>"
	    (org-insert-heading)
	    (buffer-string)))))

(ert-deftest test-org/insert-todo-heading-respect-content ()
  "Test `org-insert-todo-heading-respect-content' specifications."
  ;; Create a TODO heading.
  (should
   (org-test-with-temp-text "* H1\n Body"
     (org-insert-todo-heading-respect-content)
     (nth 2 (org-heading-components))))
  ;; Add headline at the end of the first subtree
  (should
   (org-test-with-temp-text "* H1\nH1Body\n** H2\nH2Body"
     (search-forward "H1Body")
     (org-insert-todo-heading-respect-content)
     (and (eobp) (org-at-heading-p))))
  ;; In a list, do not create a new item.
  (should
   (org-test-with-temp-text "* H\n- an item\n- another one"
     (search-forward "an ")
     (org-insert-todo-heading-respect-content)
     (and (eobp) (org-at-heading-p)))))

(ert-deftest test-org/clone-with-time-shift ()
  "Test `org-clone-subtree-with-time-shift'."
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
	     nil nil 1)))))


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



;;; Headline

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

(ert-deftest test-org/format-outline-path ()
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


;;; Keywords

(ert-deftest test-org/set-regexps-and-options ()
  "Test `org-set-regexps-and-options' specifications."
  ;; TAGS keyword.
  (should
   (equal '(("A" . ?a) ("B") ("C"))
	  (org-test-with-temp-text "#+TAGS: A(a) B C"
	    (org-mode-restart)
	    org-tag-alist)))
  (should
   (equal '(("A") (:newline) ("B"))
	  (org-test-with-temp-text "#+TAGS: A\n#+TAGS: B"
	    (org-mode-restart)
	    org-tag-alist)))
  (should
   (equal '((:startgroup) ("A") ("B") (:endgroup) ("C"))
	  (org-test-with-temp-text "#+TAGS: { A B } C"
	    (org-mode-restart)
	    org-tag-alist)))
  (should
   (equal '((:startgroup) ("A") (:grouptags) ("B") ("C") (:endgroup))
	  (org-test-with-temp-text "#+TAGS: { A : B C }"
	    (org-mode-restart)
	    org-tag-alist)))
  (should
   (equal '(("A" "B" "C"))
	  (org-test-with-temp-text "#+TAGS: { A : B C }"
	    (org-mode-restart)
	    org-tag-groups-alist)))
  (should
   (equal '((:startgrouptag) ("A") (:grouptags) ("B") ("C") (:endgrouptag))
	  (org-test-with-temp-text "#+TAGS: [ A : B C ]"
	    (org-mode-restart)
	    org-tag-alist)))
  (should
   (equal '(("A" "B" "C"))
	  (org-test-with-temp-text "#+TAGS: [ A : B C ]"
	    (org-mode-restart)
	    org-tag-groups-alist)))
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
	    (cdr (assoc "var" org-file-properties)))))
  (should
   (equal
    "foo=1 bar=2"
    (org-test-with-temp-text "#+PROPERTY: var foo=1\n#+PROPERTY: var+ bar=2"
      (org-mode-restart)
      (cdr (assoc "var" org-file-properties)))))
  (should
   (equal
    "foo=1 bar=2"
    (org-test-with-temp-text "#+PROPERTY: var foo=1\n#+PROPERTY: VAR+ bar=2"
      (org-mode-restart)
      (cdr (assoc "var" org-file-properties)))))
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
	    (cdr (assoc "CATEGORY" org-file-properties)))))
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
      (list org-highest-priority org-lowest-priority org-default-priority))))
  (should
   (equal
    '(?A ?C ?B)
    (org-test-with-temp-text "#+PRIORITIES: X Z"
      (org-mode-restart)
      (list org-highest-priority org-lowest-priority org-default-priority))))
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
	    (cdr (assoc "a" org-file-properties))))))



;;; Links

;;;; Coderefs

(ert-deftest test-org/coderef ()
  "Test coderef links specifications."
  (should
   (org-test-with-temp-text "
#+BEGIN_SRC emacs-lisp
\(+ 1 1)                  (ref:sc)
#+END_SRC
\[[(sc)]]<point>"
     (org-open-at-point)
     (looking-at "(ref:sc)")))
  ;; Find coderef even with alternate label format.
  (should
   (org-test-with-temp-text "
#+BEGIN_SRC emacs-lisp -l \"{ref:%s}\"
\(+ 1 1)                  {ref:sc}
#+END_SRC
\[[(sc)]]<point>"
     (org-open-at-point)
     (looking-at "{ref:sc}"))))

;;;; Custom ID

(ert-deftest test-org/custom-id ()
  "Test custom ID links specifications."
  (should
   (org-test-with-temp-text
       "* H1\n:PROPERTIES:\n:CUSTOM_ID: custom\n:END:\n* H2\n[[#custom]]<point>"
     (org-open-at-point)
     (org-looking-at-p "\\* H1")))
  ;; Throw an error on false positives.
  (should-error
   (org-test-with-temp-text
       "* H1\n:DRAWER:\n:CUSTOM_ID: custom\n:END:\n* H2\n[[#custom]]<point>"
     (org-open-at-point)
     (org-looking-at-p "\\* H1"))))

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
  ;; keywords, priorities, and tags.
  (should
   (let ((first-line
	  "** TODO [#A] [/]  Test [1/2] [33%] 1 \t  2 [%] :work:urgent: "))
     (org-test-with-temp-text
	 (concat first-line "\nFoo Bar\n<point>[[*Test 1 2]]")
       (let ((org-link-search-must-match-exact-headline nil)
	     (org-todo-regexp "TODO"))
	 (org-open-at-point))
       (looking-at (regexp-quote first-line)))))
  ;; Heading match should still be exact.
  (should-error
   (let ((first-line
	  "** TODO [#A] [/]  Test [1/2] [33%] 1 \t  2 [%] :work:urgent: "))
     (org-test-with-temp-text
	 (concat first-line "\nFoo Bar\n<point>[[*Test 1]]")
       (let ((org-link-search-must-match-exact-headline nil)
	     (org-todo-regexp "TODO"))
	 (org-open-at-point)))))
  ;; Heading match ignores COMMENT keyword.
  (should
   (org-test-with-temp-text "[[*Test]]\n* COMMENT Test"
     (org-open-at-point)
     (looking-at "\\* COMMENT Test")))
  ;; Correctly un-hexify fuzzy links.
  (should
   (org-test-with-temp-text "* With space\n[[*With%20space][With space]]"
     (goto-char (point-max))
     (org-open-at-point)
     (bobp))))

;;;; Link Escaping

(ert-deftest test-org/org-link-escape-ascii-character ()
  "Escape an ascii character."
  (should
   (string=
    "%5B"
    (org-link-escape "["))))

(ert-deftest test-org/org-link-escape-ascii-ctrl-character ()
  "Escape an ascii control character."
  (should
   (string=
    "%09"
    (org-link-escape "\t"))))

(ert-deftest test-org/org-link-escape-multibyte-character ()
  "Escape an unicode multibyte character."
  (should
   (string=
    "%E2%82%AC"
    (org-link-escape "€"))))

(ert-deftest test-org/org-link-escape-custom-table ()
  "Escape string with custom character table."
  (should
   (string=
    "Foo%3A%42ar%0A"
    (org-link-escape "Foo:Bar\n" '(?\: ?\B)))))

(ert-deftest test-org/org-link-escape-custom-table-merge ()
  "Escape string with custom table merged with default table."
  (should
   (string=
    "%5BF%6F%6F%3A%42ar%0A%5D"
    (org-link-escape "[Foo:Bar\n]" '(?\: ?\B ?\o) t))))

(ert-deftest test-org/org-link-unescape-ascii-character ()
  "Unescape an ascii character."
  (should
   (string=
    "["
    (org-link-unescape "%5B"))))

(ert-deftest test-org/org-link-unescape-ascii-ctrl-character ()
  "Unescpae an ascii control character."
  (should
   (string=
    "\n"
    (org-link-unescape "%0A"))))

(ert-deftest test-org/org-link-unescape-multibyte-character ()
  "Unescape unicode multibyte character."
  (should
   (string=
    "€"
    (org-link-unescape "%E2%82%AC"))))

(ert-deftest test-org/org-link-unescape-ascii-extended-char ()
  "Unescape old style percent escaped character."
  (should
   (string=
    "àâçèéêîôùû"
        (decode-coding-string
	 (org-link-unescape "%E0%E2%E7%E8%E9%EA%EE%F4%F9%FB") 'latin-1))))

(ert-deftest test-org/org-link-escape-url-with-escaped-char ()
  "Escape and unescape a URL that includes an escaped char.
http://article.gmane.org/gmane.emacs.orgmode/21459/"
  (should
   (string=
    "http://some.host.com/form?&id=blah%2Bblah25"
    (org-link-unescape
     (org-link-escape "http://some.host.com/form?&id=blah%2Bblah25")))))

(ert-deftest test-org/org-link-escape-chars-browser ()
  "Test of the constant `org-link-escape-chars-browser'.
See there why this test is a candidate to be removed once Org
drops support for Emacs 24.1 and 24.2."
  (should
   (string=
    (concat "http://lists.gnu.org/archive/cgi-bin/namazu.cgi?query="
	    "%22Release%208.2%22&idxname=emacs-orgmode")
    (org-link-escape-browser ; Do not replace with `url-encode-url',
			     ; see docstring above.
     (concat "http://lists.gnu.org/archive/cgi-bin/namazu.cgi?query="
	     "\"Release 8.2\"&idxname=emacs-orgmode")))))

;;;; Open at point

(ert-deftest test-org/open-at-point-in-keyword ()
  "Does `org-open-at-point' open link in a keyword line?"
  (should
   (org-test-with-temp-text
       "#+KEYWORD: <point>[[info:emacs#Top]]"
     (org-open-at-point) t)))

(ert-deftest test-org/open-at-point-in-property ()
  "Does `org-open-at-point' open link in property drawer?"
  (should
   (org-test-with-temp-text
       "* Headline
:PROPERTIES:
:URL: <point>[[info:emacs#Top]]
:END:"
     (org-open-at-point) t)))

(ert-deftest test-org/open-at-point-in-comment ()
  "Does `org-open-at-point' open link in a commented line?"
  (should
   (org-test-with-temp-text
    "# <point>[[info:emacs#Top]]"
    (org-open-at-point) t)))

(ert-deftest test-org/open-at-point/info ()
  "Test `org-open-at-point' on info links."
  (should
   (org-test-with-temp-text
    "<point>[[info:emacs#Top]]"
    (org-open-at-point)
    (and (switch-to-buffer "*info*")
	 (prog1
	     (looking-at "\nThe Emacs Editor")
	   (kill-buffer))))))

(ert-deftest test-org/open-at-point/inline-image ()
  "Test `org-open-at-point' on nested links."
  (should
   (org-test-with-temp-text "[[info:org#Top][info:<point>emacs#Top]]"
     (org-open-at-point)
     (prog1 (with-current-buffer "*info*" (looking-at "\nOrg Mode Manual"))
       (kill-buffer "*info*")))))

(ert-deftest test-org/open-at-point/radio-target ()
  "Test `org-open-at-point' on radio targets."
  (should
   (org-test-with-temp-text "<<<target>>> <point>target"
     (org-update-radio-target-regexp)
     (org-open-at-point)
     (eq (org-element-type (org-element-context)) 'radio-target))))


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


 
;;; Navigation

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

(ert-deftest test-org/beginning-of-line ()
  "Test `org-beginning-of-line' specifications."
  ;; Standard test.
  (should
   (org-test-with-temp-text "Some text\nSome other text"
     (progn (org-beginning-of-line) (bolp))))
  ;; Standard test with `visual-line-mode'.
  (should-not
   (org-test-with-temp-text "A long line of text\nSome other text"
     (progn (visual-line-mode)
	    (forward-char 2)
	    (dotimes (i 1000) (insert "very "))
	    (org-beginning-of-line)
	    (bolp))))
  ;; At an headline with special movement.
  (should
   (org-test-with-temp-text "* TODO Headline"
     (let ((org-special-ctrl-a/e t))
       (org-end-of-line)
       (and (progn (org-beginning-of-line) (looking-at "Headline"))
	    (progn (org-beginning-of-line) (bolp))
	    (progn (org-beginning-of-line) (looking-at "Headline"))))))
  ;; Special case: Do not error when the buffer contains only a single
  ;; asterisk.
  (should
   (org-test-with-temp-text "*<point>"
     (let ((org-special-ctrl-a/e t)) (org-beginning-of-line))))
  (should
   (org-test-with-temp-text "*<point>"
     (let ((org-special-ctrl-a/e nil)) (org-beginning-of-line)))))

(ert-deftest test-org/end-of-line ()
  "Test `org-end-of-line' specifications."
  ;; Standard test.
  (should
   (org-test-with-temp-text "Some text\nSome other text"
     (progn (org-end-of-line) (eolp))))
  ;; Standard test with `visual-line-mode'.
  (should-not
   (org-test-with-temp-text "A long line of text\nSome other text"
     (progn (visual-line-mode)
	    (forward-char 2)
	    (dotimes (i 1000) (insert "very "))
	    (goto-char (point-min))
	    (org-end-of-line)
	    (eolp))))
  ;; At an headline with special movement.
  (should
   (org-test-with-temp-text "* Headline1 :tag:\n"
     (let ((org-special-ctrl-a/e t))
       (and (progn (org-end-of-line) (looking-at " :tag:"))
	    (progn (org-end-of-line) (eolp))
	    (progn (org-end-of-line) (looking-at " :tag:"))))))
  ;; At an headline without special movement.
  (should
   (org-test-with-temp-text "* Headline2 :tag:\n"
     (let ((org-special-ctrl-a/e nil))
       (and (progn (org-end-of-line) (eolp))
	    (progn (org-end-of-line) (eolp))))))
  ;; At an headline, with reversed movement.
  (should
   (org-test-with-temp-text "* Headline3 :tag:\n"
     (let ((org-special-ctrl-a/e 'reversed)
	   (this-command last-command))
       (and (progn (org-end-of-line) (eolp))
	    (progn (org-end-of-line) (looking-at " :tag:"))))))
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
       (eobp)))))

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
     (eobp))))

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
  ;; At end of buffer, return an error.
  (should-error
   (org-test-with-temp-text "Paragraph"
     (goto-char (point-max))
     (org-forward-paragraph)))
  ;; Standard test.
  (should
   (org-test-with-temp-text "P1\n\nP2\n\nP3"
     (org-forward-paragraph)
     (looking-at "P2")))
  ;; Ignore depth.
  (should
   (org-test-with-temp-text "#+BEGIN_CENTER\nP1\n#+END_CENTER\nP2"
     (org-forward-paragraph)
     (looking-at "P1")))
  ;; Do not enter elements with invisible contents.
  (should
   (org-test-with-temp-text "#+BEGIN_CENTER\nP1\n\nP2\n#+END_CENTER\nP3"
     (org-hide-block-toggle)
     (org-forward-paragraph)
     (looking-at "P3")))
  ;; On an affiliated keyword, jump to the beginning of the element.
  (should
   (org-test-with-temp-text "#+name: para\n#+caption: caption\nPara"
     (org-forward-paragraph)
     (looking-at "Para")))
  ;; On an item or a footnote definition, move to the second element
  ;; inside, if any.
  (should
   (org-test-with-temp-text "- Item1\n\n  Paragraph\n- Item2"
     (org-forward-paragraph)
     (looking-at "  Paragraph")))
  (should
   (org-test-with-temp-text "[fn:1] Def1\n\nParagraph\n\n[fn:2] Def2"
     (org-forward-paragraph)
     (looking-at "Paragraph")))
  ;; On an item, or a footnote definition, when the first line is
  ;; empty, move to the first item.
  (should
   (org-test-with-temp-text "- \n\n  Paragraph\n- Item2"
     (org-forward-paragraph)
     (looking-at "  Paragraph")))
  (should
   (org-test-with-temp-text "[fn:1]\n\nParagraph\n\n[fn:2] Def2"
     (org-forward-paragraph)
     (looking-at "Paragraph")))
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
  ;; On a verse or source block, stop after blank lines.
  (should
   (org-test-with-temp-text "#+BEGIN_VERSE\nL1\n\nL2\n#+END_VERSE"
     (org-forward-paragraph)
     (looking-at "L2")))
  (should
   (org-test-with-temp-text "#+BEGIN_SRC\nL1\n\nL2\n#+END_SRC"
     (org-forward-paragraph)
     (looking-at "L2"))))

(ert-deftest test-org/backward-paragraph ()
  "Test `org-backward-paragraph' specifications."
  ;; Error at beginning of buffer.
  (should-error
   (org-test-with-temp-text "Paragraph"
     (org-backward-paragraph)))
  ;; Regular test.
  (should
   (org-test-with-temp-text "P1\n\nP2\n\nP3"
     (goto-char (point-max))
     (org-backward-paragraph)
     (looking-at "P3")))
  (should
   (org-test-with-temp-text "P1\n\nP2\n\nP3"
     (goto-char (point-max))
     (beginning-of-line)
     (org-backward-paragraph)
     (looking-at "P2")))
  ;; Ignore depth.
  (should
   (org-test-with-temp-text "P1\n\n#+BEGIN_CENTER\nP2\n#+END_CENTER\nP3"
     (goto-char (point-max))
     (beginning-of-line)
     (org-backward-paragraph)
     (looking-at "P2")))
  ;; Ignore invisible elements.
  (should
   (org-test-with-temp-text "* H1\n  P1\n* H2"
     (org-cycle)
     (goto-char (point-max))
     (beginning-of-line)
     (org-backward-paragraph)
     (bobp)))
  ;; On an affiliated keyword, jump to the first one.
  (should
   (org-test-with-temp-text "P1\n#+name: n\n#+caption: c1\n#+caption: c2\nP2"
     (search-forward "c2")
     (org-backward-paragraph)
     (looking-at "#\\+name")))
  ;; On the second element in an item or a footnote definition, jump
  ;; to item or the definition.
  (should
   (org-test-with-temp-text "- line1\n\n  line2"
     (goto-char (point-max))
     (beginning-of-line)
     (org-backward-paragraph)
     (looking-at "- line1")))
  (should
   (org-test-with-temp-text "[fn:1] line1\n\n  line2"
     (goto-char (point-max))
     (beginning-of-line)
     (org-backward-paragraph)
     (looking-at "\\[fn:1\\] line1")))
  ;; On a table (resp. a property drawer), ignore table rows
  ;; (resp. node properties).
  (should
   (org-test-with-temp-text "| a | b |\n| c | d |\nP1"
     (goto-char (point-max))
     (beginning-of-line)
     (org-backward-paragraph)
     (bobp)))
  (should
   (org-test-with-temp-text "* H\n:PROPERTIES:\n:prop: value\n:END:\n<point>P1"
     (org-backward-paragraph)
     (looking-at ":PROPERTIES:")))
  ;; On a source or verse block, stop before blank lines.
  (should
   (org-test-with-temp-text "#+BEGIN_VERSE\nL1\n\nL2\n\nL3\n#+END_VERSE"
     (search-forward "L3")
     (beginning-of-line)
     (org-backward-paragraph)
     (looking-at "L2")))
  (should
   (org-test-with-temp-text "#+BEGIN_SRC\nL1\n\nL2\n\nL3#+END_SRC"
     (search-forward "L3")
     (beginning-of-line)
     (org-backward-paragraph)
     (looking-at "L2"))))

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
     (org-looking-at-p "2")))
  ;; Error when trying to move first element of buffer.
  (should-error
   (org-test-with-temp-text "Paragraph 1.\n\nParagraph 2."
     (org-drag-element-backward)))
  ;; Error when trying to swap nested elements.
  (should-error
   (org-test-with-temp-text "#+BEGIN_CENTER\nTest.\n#+END_CENTER"
     (forward-line)
     (org-drag-element-backward)))
  ;; Error when trying to swap an headline element and a non-headline
  ;; element.
  (should-error
   (org-test-with-temp-text "Test.\n* Head 1"
     (forward-line)
     (org-drag-element-backward)))
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
		    (overlays-in (point-min) (point-max)))))))

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
  ;; 4. Otherwise, swap elements, preserving column and blank lines
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
     (org-looking-at-p ":tag:$")))
  (should-not
   (org-test-with-temp-text "* H  :tag:"
     (let ((org-tags-column 10)
	   (org-auto-align-tags nil)
	   (org-odd-levels-only nil))
       (org-demote))
     (org-move-to-column 10)
     (org-looking-at-p ":tag:$")))
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
     (org-looking-at-p ":tag:$")))
  (should-not
   (org-test-with-temp-text "** H :tag:"
     (let ((org-tags-column 10)
	   (org-auto-align-tags nil)
	   (org-odd-levels-only nil))
       (org-promote))
     (org-move-to-column 10)
     (org-looking-at-p ":tag:$")))
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


;;; Property API

(ert-deftest test-org/buffer-property-keys ()
  "Test `org-buffer-property-keys' specifications."
  ;; Retrieve properties accross siblings.
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
  ;; Retrieve properties accross children.
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
  ;; With non-nil COLUMNS, extract property names from columns.
  (should
   (equal '("A" "B")
	  (org-test-with-temp-text "#+COLUMNS: %25ITEM %A %20B"
	    (org-buffer-property-keys nil nil t))))
  (should
   (equal '("A" "B" "COLUMNS")
	  (org-test-with-temp-text
	      "* H\n:PROPERTIES:\n:COLUMNS: %25ITEM %A %20B\n:END:"
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

(ert-deftest test-org/find-property ()
  "Test `org-find-property' specifications."
  ;; Regular test.
  (should
   (= 1
      (org-test-with-temp-text "* H\n:PROPERTIES:\n:PROP: value\n:END:"
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
     (org-entry-delete (point) "C"))))

(ert-deftest test-org/entry-get ()
  "Test `org-entry-get' specifications."
  ;; Regular test.
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
    (org-test-with-temp-text "* H\n:PROPERTIES:\n:A: 1\n:END:\n** <point>H2"
      (org-entry-get (point) "A" t))))
  (should
   (equal
    "1"
    (org-test-with-temp-text "* H\n:PROPERTIES:\n:A: 1\n:END:\n** <point>H2"
      (let ((org-use-property-inheritance t))
	(org-entry-get (point) "A" 'selective)))))
  (should-not
   (org-test-with-temp-text "* H\n:PROPERTIES:\n:A: 1\n:END:\n** <point>H2"
     (let ((org-use-property-inheritance nil))
       (org-entry-get (point) "A" 'selective))))
  (should
   (equal
    "1 2"
    (org-test-with-temp-text
	"* H\n:PROPERTIES:\n:A: 1\n:END:\n** H2\n:PROPERTIES:\n:A+: 2\n:END:"
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
	    (org-entry-get (point-max) "A" t)))))

(ert-deftest test-org/entry-properties ()
  "Test `org-entry-properties' specifications."
  ;; Get "ITEM" property.
  (should
   (equal "* H"
	  (org-test-with-temp-text "* TODO H"
	    (cdr (assoc "ITEM" (org-entry-properties nil "ITEM"))))))
  (should
   (equal "* H"
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
   (equal (char-to-string org-default-priority)
	  (org-test-with-temp-text "* H"
	    (cdr (assoc "PRIORITY" (org-entry-properties nil "PRIORITY"))))))
  ;; Get "FILE" property.
  (should
   (org-test-with-temp-text-in-file "* H\nParagraph"
     (org-file-equal-p (cdr (assoc "FILE" (org-entry-properties nil "FILE")))
		       (buffer-file-name))))
  (should
   (org-test-with-temp-text-in-file "* H\nParagraph"
     (org-file-equal-p (cdr (assoc "FILE" (org-entry-properties)))
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
	    (cdr (assoc "TIMESTAMP_IA" (org-entry-properties nil "TIMESTAMP_IA"))))))
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


;;; Radio Targets

(ert-deftest test-org/update-radio-target-regexp ()
  "Test `org-update-radio-target-regexp' specifications."
  ;; Properly update cache with no previous radio target regexp.
  (should
   (eq 'link
       (org-test-with-temp-text "radio\n\nParagraph\n\nradio"
	 (save-excursion (goto-char (point-max)) (org-element-context))
	 (insert "<<<")
	 (search-forward "o")
	 (insert ">>>")
	 (org-update-radio-target-regexp)
	 (goto-char (point-max))
	 (org-element-type (org-element-context)))))
  ;; Properly update cache with previous radio target regexp.
  (should
   (eq 'link
       (org-test-with-temp-text "radio\n\nParagraph\n\nradio"
	 (save-excursion (goto-char (point-max)) (org-element-context))
	 (insert "<<<")
	 (search-forward "o")
	 (insert ">>>")
	 (org-update-radio-target-regexp)
	 (search-backward "r")
	 (delete-char 5)
	 (insert "new")
	 (org-update-radio-target-regexp)
	 (goto-char (point-max))
	 (delete-region (line-beginning-position) (point))
	 (insert "new")
	 (org-element-type (org-element-context))))))


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


;;; Timestamps API

(ert-deftest test-org/time-stamp ()
  "Test `org-time-stamp' specifications."
  ;; Insert chosen time stamp at point.
  (should
   (string-match
    "Te<2014-03-04 .*?>xt"
    (org-test-with-temp-text "Te<point>xt"
      (flet ((org-read-date
	      (&rest args)
	      (apply #'encode-time (org-parse-time-string "2014-03-04"))))
	(org-time-stamp nil)
	(buffer-string)))))
  ;; With a prefix argument, also insert time.
  (should
   (string-match
    "Te<2014-03-04 .*? 00:41>xt"
    (org-test-with-temp-text "Te<point>xt"
      (flet ((org-read-date
	      (&rest args)
	      (apply #'encode-time (org-parse-time-string "2014-03-04 00:41"))))
	(org-time-stamp '(4))
	(buffer-string)))))
  ;; With two universal prefix arguments, insert an active timestamp
  ;; with the current time without prompting the user.
  (should
   (string-match
    "Te<2014-03-04 .*? 00:41>xt"
    (org-test-with-temp-text "Te<point>xt"
      (flet ((current-time
	      ()
	      (apply #'encode-time (org-parse-time-string "2014-03-04 00:41"))))
	(org-time-stamp '(16))
	(buffer-string)))))
  ;; When optional argument is non-nil, insert an inactive timestamp.
  (should
   (string-match
    "Te\\[2014-03-04 .*?\\]xt"
    (org-test-with-temp-text "Te<point>xt"
      (flet ((org-read-date
	      (&rest args)
	      (apply #'encode-time (org-parse-time-string "2014-03-04"))))
	(org-time-stamp nil t)
	(buffer-string)))))
  ;; When called from a timestamp, replace existing one.
  (should
   (string-match
    "<2014-03-04 .*?>"
    (org-test-with-temp-text "<2012-03-29<point> thu.>"
      (flet ((org-read-date
	      (&rest args)
	      (apply #'encode-time (org-parse-time-string "2014-03-04"))))
	(org-time-stamp nil)
	(buffer-string)))))
  (should
   (string-match
    "<2014-03-04 .*?>--<2014-03-04 .*?>"
    (org-test-with-temp-text "<2012-03-29<point> thu.>--<2014-03-04 tue.>"
      (flet ((org-read-date
	      (&rest args)
	      (apply #'encode-time (org-parse-time-string "2014-03-04"))))
	(org-time-stamp nil)
	(buffer-string)))))
  ;; When replacing a timestamp, preserve repeater, if any.
  (should
   (string-match
    "<2014-03-04 .*? \\+2y>"
    (org-test-with-temp-text "<2012-03-29<point> thu. +2y>"
      (flet ((org-read-date
	      (&rest args)
	      (apply #'encode-time (org-parse-time-string "2014-03-04"))))
	(org-time-stamp nil)
	(buffer-string)))))
  ;; When called twice in a raw, build a date range.
  (should
   (string-match
    "<2012-03-29 .*?>--<2014-03-04 .*?>"
    (org-test-with-temp-text "<2012-03-29 thu.><point>"
      (flet ((org-read-date
	      (&rest args)
	      (apply #'encode-time (org-parse-time-string "2014-03-04"))))
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



;;; Visibility

(ert-deftest test-org/flag-drawer ()
  "Test `org-flag-drawer' specifications."
  ;; Hide drawer.
  (should
   (org-test-with-temp-text ":DRAWER:\ncontents\n:END:"
     (org-flag-drawer t)
     (get-char-property (line-end-position) 'invisible)))
  ;; Show drawer.
  (should-not
   (org-test-with-temp-text ":DRAWER:\ncontents\n:END:"
     (org-flag-drawer t)
     (org-flag-drawer nil)
     (get-char-property (line-end-position) 'invisible)))
  ;; Test optional argument.
  (should
   (org-test-with-temp-text ":D1:\nc1\n:END:\n\n:D2:\nc2\n:END:"
     (let ((drawer (save-excursion (search-forward ":D2")
				   (org-element-at-point))))
       (org-flag-drawer t drawer)
       (get-char-property (progn (search-forward ":D2") (line-end-position))
			  'invisible))))
  (should-not
   (org-test-with-temp-text ":D1:\nc1\n:END:\n\n:D2:\nc2\n:END:"
     (let ((drawer (save-excursion (search-forward ":D2")
				   (org-element-at-point))))
       (org-flag-drawer t drawer)
       (get-char-property (line-end-position) 'invisible))))
  ;; Do not hide fake drawers.
  (should-not
   (org-test-with-temp-text "#+begin_example\n:D:\nc\n:END:\n#+end_example"
     (forward-line 1)
     (org-flag-drawer t)
     (get-char-property (line-end-position) 'invisible)))
  ;; Do not hide incomplete drawers.
  (should-not
   (org-test-with-temp-text ":D:\nparagraph"
     (forward-line 1)
     (org-flag-drawer t)
     (get-char-property (line-end-position) 'invisible)))
  ;; Do not hide drawers when called from final blank lines.
  (should-not
   (org-test-with-temp-text ":DRAWER:\nA\n:END:\n\n"
     (goto-char (point-max))
     (org-flag-drawer t)
     (goto-char (point-min))
     (get-char-property (line-end-position) 'invisible)))
  ;; Don't leave point in an invisible part of the buffer when hiding
  ;; a drawer away.
  (should-not
   (org-test-with-temp-text ":DRAWER:\ncontents\n:END:"
     (goto-char (point-max))
     (org-flag-drawer t)
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
		 (incf line)
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
		   (funcall list-visible-lines 'canonical nil)))))


(provide 'test-org)

;;; test-org.el ends here
