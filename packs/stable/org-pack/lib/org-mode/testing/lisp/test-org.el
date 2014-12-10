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
		   (buffer-string))))))



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
			  (org-read-date t nil "29.03. 16:40")))))

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
	  (org-test-with-temp-text "#+name: table\n| a |"
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



;;; Editing

;;;; Insert elements

(ert-deftest test-org/meta-return ()
  "Test M-RET (`org-meta-return')."
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
   (let ((org-drawers '("MYDRAWER")))
     (org-test-with-temp-text ":MYDRAWER:\n- a\n:END:"
       (forward-line)
       (org-meta-return)
       (beginning-of-line)
       (looking-at "- $")))))

(ert-deftest test-org/insert-heading ()
  "Test `org-insert-heading' specifications."
  ;; FIXME: Test coverage is incomplete yet.
  ;;
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



;;; Links

;;;; Fuzzy Links

;; Fuzzy links [[text]] encompass links to a target (<<text>>), to
;; a named element (#+name: text) and to headlines (* Text).

(ert-deftest test-org/fuzzy-links ()
  "Test fuzzy links specifications."
  ;; 1. Fuzzy link goes in priority to a matching target.
  (should
   (org-test-with-temp-text "#+NAME: Test\n|a|b|\n<<Test>>\n* Test\n[[Test]]"
     (goto-line 5)
     (org-open-at-point)
     (looking-at "<<Test>>")))
  ;; 2. Then fuzzy link points to an element with a given name.
  (should
   (org-test-with-temp-text "Test\n#+NAME: Test\n|a|b|\n* Test\n[[Test]]"
     (goto-line 5)
     (org-open-at-point)
     (looking-at "#\\+NAME: Test")))
  ;; 3. A target still lead to a matching headline otherwise.
  (should
   (org-test-with-temp-text "* Head1\n* Head2\n*Head3\n[[Head2]]"
     (goto-line 4)
     (org-open-at-point)
     (looking-at "\\* Head2")))
  ;; 4. With a leading star in link, enforce heading match.
  (should
   (org-test-with-temp-text "* Test\n<<Test>>\n[[*Test]]"
     (goto-line 3)
     (org-open-at-point)
     (looking-at "\\* Test"))))


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
  "Escape a URL to pass to `browse-url'."
  (should
   (string=
    "http://some.host.com/search?q=%22Org%20mode%22"
    (org-link-escape "http://some.host.com/search?q=\"Org mode\""
		     org-link-escape-chars-browser))))



;;; Node Properties

(ert-deftest test-org/accumulated-properties-in-drawers ()
  "Ensure properties accumulate in subtree drawers."
  (org-test-at-id "75282ba2-f77a-4309-a970-e87c149fe125"
    (org-babel-next-src-block)
    (should (equal '(2 1) (org-babel-execute-src-block)))))



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
	    (progn (org-beginning-of-line) (looking-at "Headline")))))))

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
   (org-test-with-temp-text ":PROPERTIES:\n:prop: value\n:END:\nParagraph"
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
   (org-test-with-temp-text ":PROPERTIES:\n:prop: value\n:END:\nP1"
     (goto-char (point-max))
     (beginning-of-line)
     (org-backward-paragraph)
     (bobp)))
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
  ;; 1. Error when trying to move first element of buffer.
  (org-test-with-temp-text "Paragraph 1.\n\nParagraph 2."
    (should-error (org-drag-element-backward)))
  ;; 2. Error when trying to swap nested elements.
  (org-test-with-temp-text "#+BEGIN_CENTER\nTest.\n#+END_CENTER"
    (forward-line)
    (should-error (org-drag-element-backward)))
  ;; 3. Error when trying to swap an headline element and
  ;;    a non-headline element.
  (org-test-with-temp-text "Test.\n* Head 1"
    (forward-line)
    (should-error (org-drag-element-backward)))
  ;; 4. Otherwise, swap elements, preserving column and blank lines
  ;;    between elements.
  (org-test-with-temp-text "Para1\n\n\nParagraph 2\n\nPara3"
    (search-forward "graph")
    (org-drag-element-backward)
    (should (equal (buffer-string) "Paragraph 2\n\n\nPara1\n\nPara3"))
    (should (looking-at " 2")))
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
    (search-backward "- item 1")
    (org-drag-element-backward)
    (should
     (equal
      '((63 . 82) (26 . 48))
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



;;; Planning

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
       (eq ts-orig ts-copy))))
  ;; Check that parent is the same when a range was split.
  (should
   (org-test-with-temp-text "[2012-03-29 Thu]--[2012-03-30 Fri]"
     (let* ((ts-orig (org-element-context))
	    (ts-copy (org-timestamp-split-range ts-orig)))
       (eq (org-element-property :parent ts-orig)
	   (org-element-property :parent ts-copy))))))

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



;;; Targets and Radio Targets

(ert-deftest test-org/all-targets ()
  "Test `org-all-targets' specifications."
  ;; Without an argument.
  (should
   (equal '("radio-target" "target")
	  (org-test-with-temp-text "<<target>> <<<radio-target>>>\n: <<verb>>"
	    (org-all-targets))))
  (should
   (equal '("radio-target")
	  (org-test-with-temp-text "<<<radio-target>>>!" (org-all-targets))))
  ;; With argument.
  (should
   (equal '("radio-target")
	  (org-test-with-temp-text "<<target>> <<<radio-target>>>"
	    (org-all-targets t)))))


(provide 'test-org)

;;; test-org.el ends here
