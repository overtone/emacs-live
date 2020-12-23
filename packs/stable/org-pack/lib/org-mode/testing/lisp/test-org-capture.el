;;; test-org-capture.el --- Tests for org-capture.el -*- lexical-binding: t; -*-

;; Copyright (C) 2015, 2017, 2019  Nicolas Goaziou

;; Author: Nicolas Goaziou <mail@nicolasgoaziou.fr>

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

;;; Commentary:

;; Unit tests for Org Capture library.

;;; Code:

(require 'org-capture)

(ert-deftest test-org-capture/fill-template ()
  "Test `org-capture-fill-template' specifications."

  ;; When working on these tests consider to also change
  ;; `test-org-feed/fill-template'.

  ;; %(sexp) placeholder.
  (should
   (equal "success!\n"
	  (org-capture-fill-template "%(concat \"success\" \"!\")")))
  ;; It is possible to include other place holders in %(sexp).  In
  ;; that case properly escape \ and " characters.
  (should
   (equal "Nested string \"\\\"\\\"\"\n"
	  (let ((org-store-link-plist nil))
	    (org-capture-fill-template "%(concat \"%i\")"
				       "Nested string \"\\\"\\\"\""))))
  ;; %<...> placeholder.
  (should
   (equal (concat (format-time-string "%Y") "\n")
	  (org-capture-fill-template "%<%Y>")))
  ;; %t and %T placeholders.
  (should
   (equal (concat (format-time-string (org-time-stamp-format nil nil)) "\n")
	  (org-capture-fill-template "%t")))
  (should
   (equal (concat (format-time-string (org-time-stamp-format t nil)) "\n")
	  (org-capture-fill-template "%T")))
  ;; %u and %U placeholders.
  (should
   (equal
    (concat (format-time-string (org-time-stamp-format nil t)) "\n")
    (org-capture-fill-template "%u")))
  (should
   (equal
    (concat (format-time-string (org-time-stamp-format t t)) "\n")
    (org-capture-fill-template "%U")))
  ;; %i placeholder.  Make sure sexp placeholders are not expanded
  ;; when they are inserted through this one.
  (should
   (equal "success!\n"
	  (let ((org-store-link-plist nil))
	    (org-capture-fill-template "%i" "success!"))))
  (should
   (equal "%(concat \"no \" \"evaluation\")\n"
	  (let ((org-store-link-plist nil))
	    (org-capture-fill-template
	     "%i" "%(concat \"no \" \"evaluation\")"))))
  ;; When %i contents span over multiple line, repeat initial leading
  ;; characters over each line.  Also try possibly problematic
  ;; prefixes such as "\\".
  (should
   (equal "> line 1\n> line 2\n"
	  (let ((org-store-link-plist nil))
	    (org-capture-fill-template "> %i" "line 1\nline 2"))))
  (should
   (equal "\\ line 1\n\\ line 2\n"
	  (let ((org-store-link-plist nil))
	    (org-capture-fill-template "\\ %i" "line 1\nline 2"))))
  ;; Test %-escaping with \ character.
  (should
   (equal "%i\n"
	  (let ((org-store-link-plist nil))
	    (org-capture-fill-template "\\%i" "success!"))))
  (should
   (equal "\\success!\n"
	  (let ((org-store-link-plist nil))
	    (org-capture-fill-template "\\\\%i" "success!"))))
  (should
   (equal "\\%i\n"
	  (let ((org-store-link-plist nil))
	    (org-capture-fill-template "\\\\\\%i" "success!"))))
  ;; More than one placeholder in the same template.
  (should
   (equal "success! success! success! success!\n"
	  (let ((org-store-link-plist nil))
	    (org-capture-fill-template "%i %i %i %i" "success!"))))
  ;; %(sexp) placeholder with an input containing the traps %, " and )
  ;; all at once which is complicated to parse.
  (should
   (equal "5 % Less (See Item \"3)\" Somewhere)\n"
	  (let ((org-store-link-plist nil))
	    (org-capture-fill-template
	     "%(capitalize \"%i\")"
	     "5 % less (see item \"3)\" somewhere)")))))

(ert-deftest test-org-capture/refile ()
  "Test `org-capture-refile' specifications."
  ;; When refiling, make sure the headline being refiled is the one
  ;; being captured.  In particular, empty lines after the entry may
  ;; be removed, and we don't want to shift onto the next heading.
  (should
   (string-prefix-p
    "** H1"
    (org-test-with-temp-text-in-file "* A\n* B\n"
      (let* ((file (buffer-file-name))
	     (org-capture-templates
	      `(("t" "Todo" entry (file+headline ,file "A") "** H1 %?"))))
	(org-capture nil "t")
	(insert "\n")
	(cl-letf (((symbol-function 'org-refile)
		   (lambda ()
		     (interactive)
		     (throw :return
			    (buffer-substring-no-properties
			     (line-beginning-position)
			     (line-end-position))))))
	  (catch :return (org-capture-refile)))))))
  ;; When the entry is refiled, `:jump-to-captured' moves point to the
  ;; refile location, not the initial capture target.
  (should
   (org-test-with-temp-text-in-file "* Refile target"
     (let ((file1 (buffer-file-name)))
       (org-test-with-temp-text-in-file "* A"
	 (let* ((file2 (buffer-file-name))
		(org-capture-templates
		 `(("t" "Todo" entry (file+headline ,file2 "A")
		    "** H1 %?" :jump-to-captured t))))
	   (org-capture nil "t")
	   (cl-letf (((symbol-function 'org-refile-get-location)
		      (lambda (&rest args)
			(list (file-name-nondirectory file1) file1 nil nil))))
	     (org-capture-refile)
	     (list file1 file2 (buffer-file-name)))))))))

(ert-deftest test-org-capture/abort ()
  "Test aborting a capture process."
  ;; Test that capture can be aborted after inserting at end of
  ;; capture buffer.
  (should
   (equal
    "* A\n* B\n"
    (org-test-with-temp-text-in-file "* A\n* B\n"
      (let* ((file (buffer-file-name))
	     (org-capture-templates
	      `(("t" "Todo" entry (file+headline ,file "A") "** H1 %?"))))
	(org-capture nil "t")
	(goto-char (point-max))
	(insert "Capture text")
	(org-capture-kill))
      (buffer-string))))
  (should
   (equal "- A\n  - B\n"
	  (org-test-with-temp-text-in-file "- A\n  - B"
	    (let* ((file (buffer-file-name))
		   (org-capture-templates
		    `(("t" "Item" item (file ,file) "- X"))))
	      (org-capture nil "t")
	      (org-capture-kill))
	    (buffer-string))))
  (should
   (equal "| a |\n| b |\n"
	  (org-test-with-temp-text-in-file "| a |\n| b |"
	    (let* ((file (buffer-file-name))
		   (org-capture-templates
		    `(("t" "Table" table-line (file ,file) "| x |"))))
	      (org-capture nil "t")
	      (org-capture-kill))
	    (buffer-string))))
  ;; Test aborting a capture that split the line.
  (should
   (equal
    "* AB\n"
    (org-test-with-temp-text-in-file "* AB\n"
      (let* ((file (buffer-file-name))
	     (org-capture-templates
	      `(("t" "Todo" entry
		 (file+function ,file (lambda () (goto-char 4))) "** H1 %?"))))
	(org-capture nil "t")
	(org-capture-kill))
      (buffer-string)))))

(ert-deftest test-org-capture/entry ()
  "Test `entry' type in capture template."
  ;; Do not break next headline.
  (should
   (equal
    "* A\n** H1 Capture text\n* B\n"
    (org-test-with-temp-text-in-file "* A\n* B\n"
      (let* ((file (buffer-file-name))
	     (org-capture-templates
	      `(("t" "Todo" entry (file+headline ,file "A") "** H1 %?"))))
	(org-capture nil "t")
	(insert "Capture text")
	(org-capture-finalize))
      (buffer-string))))
  ;; Correctly save position of inserted entry.
  (should
   (equal
    "** H"
    (org-test-with-temp-text-in-file "* A"
      (let* ((file (buffer-file-name))
	     (org-capture-templates
	      `(("t" "Test" entry (file+headline ,file "A") "** H\nFoo"
		 :immediate-finish t))))
	(org-capture nil "t")
	(org-capture '(16))
	(buffer-substring (point) (line-end-position))))))
  ;; Do not raise an error on empty entries.
  (should
   (org-test-with-temp-text-in-file ""
     (let* ((file (buffer-file-name))
	    (org-capture-templates
	     `(("t" "Test" entry (file+headline ,file "A") "** "
		:immediate-finish t))))
       (org-capture nil "t")
       (buffer-string))))
  ;; With a 0 prefix argument, ignore surrounding lists.
  (should
   (equal "Foo\n* X\nBar\n"
	  (org-test-with-temp-text-in-file "Foo\nBar"
	    (forward-line)
	    (let* ((file (buffer-file-name))
		   (org-capture-templates
		    `(("t" "Test" entry (file ,file) "* X"
		       :immediate-finish t))))
	      (org-capture 0 "t")
	      (buffer-string)))))
  ;; With a 0 prefix argument, also obey to :empty-lines.
  (should
   (equal "Foo\n\n* X\n\nBar\n"
	  (org-test-with-temp-text-in-file "Foo\nBar"
	    (forward-line)
	    (let* ((file (buffer-file-name))
		   (org-capture-templates
		    `(("t" "Test" entry (file ,file) "* X"
		       :immediate-finish t :empty-lines 1))))
	      (org-capture 0 "t")
	      (buffer-string))))))

(ert-deftest test-org-capture/item ()
  "Test `item' type in capture template."
  ;; Insert item in the first plain list found at the target location.
  (should
   (equal
    "* A\n- list 1\n- X\n\n\n1. list 2\n"
    (org-test-with-temp-text-in-file "* A\n- list 1\n\n\n1. list 2"
      (let* ((file (buffer-file-name))
	     (org-capture-templates
	      `(("t" "Item" item (file+headline ,file "A") "- X"))))
	(org-capture nil "t")
	(org-capture-finalize))
      (buffer-string))))
  (should
   (equal
    "Text\n- list 1\n- X\n\n\n1. list 2\n"
    (org-test-with-temp-text-in-file "Text\n- list 1\n\n\n1. list 2"
      (let* ((file (buffer-file-name))
	     (org-capture-templates
	      `(("t" "Item" item (file ,file) "- X"))))
	(org-capture nil "t")
	(org-capture-finalize))
      (buffer-string))))
  ;; When targeting a specific location, start looking for plain lists
  ;; from there.
  (should
   (equal
    "* A\n- skip\n\n\n1. here\n2. X\n"
    (org-test-with-temp-text-in-file "* A\n- skip\n\n\n1. here"
      (let* ((file (buffer-file-name))
	     (org-capture-templates
	      `(("t" "Item" item (file+regexp ,file "here") "1. X"))))
	(org-capture nil "t")
	(org-capture-finalize))
      (buffer-string))))
  ;; If there is no such list, create it.
  (should
   (equal
    "* A\n- X\n"
    (org-test-with-temp-text-in-file "* A"
      (let* ((file (buffer-file-name))
	     (org-capture-templates
	      `(("t" "Item" item (file+headline ,file "A") "- X"))))
	(org-capture nil "t")
	(org-capture-finalize))
      (buffer-string))))
  ;; When `:prepend' is non-nil, insert new item as the first item.
  (should
   (equal
    "* A\n- X\n- 1\n- 2\n"
    (org-test-with-temp-text-in-file "* A\n- 1\n- 2"
      (let* ((file (buffer-file-name))
	     (org-capture-templates
	      `(("t" "Item" item (file+headline ,file "A") "- X"
		 :prepend t))))
	(org-capture nil "t")
	(org-capture-finalize))
      (buffer-string))))
  ;; If there is no list and `:prepend' is non-nil, insert list at the
  ;; beginning of the entry, or the beginning of the buffer.  However,
  ;; preserve properties drawer and planning info, if any.
  (should
   (equal
    "* A\n- X\nSome text\n"
    (org-test-with-temp-text-in-file "* A\nSome text"
      (let* ((file (buffer-file-name))
	     (org-capture-templates
	      `(("t" "Item" item (file+headline ,file "A") "- X"
		 :prepend t))))
	(org-capture nil "t")
	(org-capture-finalize))
      (buffer-string))))
  (should
   (equal
    "- X\nText\n"
    (org-test-with-temp-text-in-file "Text"
      (let* ((file (buffer-file-name))
	     (org-capture-templates
	      `(("t" "Item" item (file ,file) "- X" :prepend t))))
	(org-capture nil "t")
	(org-capture-finalize))
      (buffer-string))))
  (should
   (equal
    "* A\nSCHEDULED: <2012-03-29 Thu>\n- X\nText\n"
    (org-test-with-temp-text-in-file "* A\nSCHEDULED: <2012-03-29 Thu>\nText"
      (let* ((file (buffer-file-name))
	     (org-capture-templates
	      `(("t" "Item" item (file+headline ,file "A") "- X"
		 :prepend t))))
	(org-capture nil "t")
	(org-capture-finalize))
      (buffer-string))))
  ;; When `:prepend' is nil, insert new item as the last top-level
  ;; item.
  (should
   (equal
    "* A\n- 1\n  - 2\n- X\n"
    (org-test-with-temp-text-in-file "* A\n- 1\n  - 2"
      (let* ((file (buffer-file-name))
	     (org-capture-templates
	      `(("t" "Item" item (file+headline ,file "A") "- X"))))
	(org-capture nil "t")
	(org-capture-finalize))
      (buffer-string))))
  ;; When targeting a specific location, one can insert in a sub-list.
  (should
   (equal
    "* A\n- skip\n  - here\n  - X\n- skip\n"
    (org-test-with-temp-text-in-file "* A\n- skip\n  - here\n- skip"
      (let* ((file (buffer-file-name))
	     (org-capture-templates
	      `(("t" "Item" item (file+regexp ,file "here") "- X"))))
	(org-capture nil "t")
	(org-capture-finalize))
      (buffer-string))))
  ;; Obey `:empty-lines' when creating a new list.
  (should
   (equal
    "\n- X\n\n\n* H\n"
    (org-test-with-temp-text-in-file "\n* H"
      (let* ((file (buffer-file-name))
	     (org-capture-templates
	      `(("t" "Item" item (file ,file) "- X"
		 :empty-lines-before 1 :empty-lines-after 2 :prepend t))))
	(org-capture nil "t")
	(org-capture-finalize))
      (buffer-string))))
  ;; Obey `:empty-lines' in an existing list only between items, and
  ;; only if the value doesn't break the list.
  (should
   (equal
    "- A\n\n- X\nText\n"
    (org-test-with-temp-text-in-file "- A\nText"
      (let* ((file (buffer-file-name))
	     (org-capture-templates
	      `(("t" "Item" item (file ,file) "- X" :empty-lines 1))))
	(org-capture nil "t")
	(org-capture-finalize))
      (buffer-string))))
  (should
   (equal
    "Text\n- X\n\n- A\n"
    (org-test-with-temp-text-in-file "Text\n- A"
      (let* ((file (buffer-file-name))
	     (org-capture-templates
	      `(("t" "Item" item (file ,file) "- X"
		 :prepend t :empty-lines 1))))
	(org-capture nil "t")
	(org-capture-finalize))
      (buffer-string))))
  (should-not
   (equal
    "- A\n\n\n- X\n"
    (org-test-with-temp-text-in-file "- A"
      (let* ((file (buffer-file-name))
	     (org-capture-templates
	      `(("t" "Item" item (file ,file) "- X" :empty-lines 2))))
	(org-capture nil "t")
	(org-capture-finalize))
      (buffer-string))))
  ;; Preserve list type when pre-pending.
  (should
   (equal
    "1. X\n2. A\n"
    (org-test-with-temp-text-in-file "1. A"
      (let* ((file (buffer-file-name))
	     (org-capture-templates
	      `(("t" "Item" item (file ,file) "- X" :prepend t))))
	(org-capture nil "t")
	(org-capture-finalize))
      (buffer-string))))
  ;; Handle indentation.  Handle multi-lines templates.
  (should
   (equal
    "  - A\n  - X\n"
    (org-test-with-temp-text-in-file "  - A"
      (let* ((file (buffer-file-name))
	     (org-capture-templates
	      `(("t" "Item" item (file ,file) "- X"))))
	(org-capture nil "t")
	(org-capture-finalize))
      (buffer-string))))
  (should
   (equal
    "  - A\n  - X\n    Line 2\n"
    (org-test-with-temp-text-in-file "  - A"
      (let* ((file (buffer-file-name))
	     (org-capture-templates
	      `(("t" "Item" item (file ,file) "- X\n  Line 2"))))
	(org-capture nil "t")
	(org-capture-finalize))
      (buffer-string))))
  ;; Handle incomplete templates.
  (should
   (equal
    "- A\n- X\n"
    (org-test-with-temp-text-in-file "- A"
      (let* ((file (buffer-file-name))
	     (org-capture-templates
	      `(("t" "Item" item (file ,file) "X"))))
	(org-capture nil "t")
	(org-capture-finalize))
      (buffer-string))))
  ;; Do not break next headline.
  (should-not
   (equal
    "- A\n- X\nFoo* H"
    (org-test-with-temp-text-in-file "- A\n* H"
      (let* ((file (buffer-file-name))
	     (org-capture-templates
	      `(("t" "Item" item (file ,file) "- X"))))
	(org-capture nil "t")
	(goto-char (point-max))
	(insert "Foo")
	(org-capture-finalize))
      (buffer-string))))
  ;; With a 0 prefix argument, ignore surrounding lists.
  (should
   (equal "- X\nFoo\n\n- A\n"
	  (org-test-with-temp-text-in-file "Foo\n\n- A"
	    (let* ((file (buffer-file-name))
		   (org-capture-templates
		    `(("t" "Test" item (file ,file) "- X"
		       :immediate-finish t))))
	      (org-capture 0 "t")
	      (buffer-string)))))
  ;; With a 0 prefix argument, also obey to `:empty-lines'.
  (should
   (equal "\n- X\n\nFoo\n\n- A\n"
	  (org-test-with-temp-text-in-file "Foo\n\n- A"
	    (let* ((file (buffer-file-name))
		   (org-capture-templates
		    `(("t" "Test" item (file ,file) "- X"
		       :immediate-finish t :empty-lines 1))))
	      (org-capture 0 "t")
	      (buffer-string))))))

(ert-deftest test-org-capture/table-line ()
  "Test `table-line' type in capture template."
  ;; When a only file is specified, use the first table available.
  (should
   (equal "Text

| a |
| x |

| b |
"
	  (org-test-with-temp-text-in-file "Text\n\n| a |\n\n| b |"
	    (let* ((file (buffer-file-name))
		   (org-capture-templates
		    `(("t" "Table" table-line (file ,file) "| x |"
		       :immediate-finish t))))
	      (org-capture nil "t"))
	    (buffer-string))))
  ;; When an entry is specified, find the first table in the
  ;; corresponding section.
  (should
   (equal "* Foo
| a |
* Inbox
| b |
| x |
"
	  (org-test-with-temp-text-in-file "* Foo\n| a |\n* Inbox\n| b |\n"
	    (let* ((file (buffer-file-name))
		   (org-capture-templates
		    `(("t" "Table" table-line (file+headline ,file "Inbox")
		       "| x |" :immediate-finish t))))
	      (org-capture nil "t"))
	    (buffer-string))))
  (should
   (equal "* Inbox
| a |
| x |

| b |
"
	  (org-test-with-temp-text-in-file "* Inbox\n| a |\n\n| b |\n"
	    (let* ((file (buffer-file-name))
		   (org-capture-templates
		    `(("t" "Table" table-line (file+headline ,file "Inbox")
		       "| x |" :immediate-finish t))))
	      (org-capture nil "t"))
	    (buffer-string))))
  ;; When a precise location is specified, find the first table after
  ;; point, down to the end of the section.
  (should
   (equal "| a |


| b |
| x |
"
	  (org-test-with-temp-text-in-file "| a |\n\n\n| b |\n"
	    (let* ((file (buffer-file-name))
		   (org-capture-templates
		    `(("t" "Table" table-line (file+function ,file forward-line)
		       "| x |" :immediate-finish t))))
	      (org-capture nil "t"))
	    (buffer-string))))
  ;; Create a new table with an empty header when none can be found.
  (should
   (equal "|   |   |\n|---+---|\n| a | b |\n"
	  (org-test-with-temp-text-in-file ""
	    (let* ((file (buffer-file-name))
		   (org-capture-templates
		    `(("t" "Table" table-line (file ,file) "| a | b |"
		       :immediate-finish t))))
	      (org-capture nil "t"))
	    (buffer-string))))
  ;; Properly insert row with formulas.
  (should
   (equal "| 1 |\n| 2 |\n#+TBLFM: \n"
	  (org-test-with-temp-text-in-file "| 1 |\n#+TBLFM: "
	    (let* ((file (buffer-file-name))
		   (org-capture-templates
		    `(("t" "Table" table-line (file ,file)
		       "| 2 |" :immediate-finish t))))
	      (org-capture nil "t"))
	    (buffer-string))))
  ;; When `:prepend' is nil, add the row at the end of the table.
  (should
   (equal "| a |\n| x |\n"
	  (org-test-with-temp-text-in-file "| a |"
	    (let* ((file (buffer-file-name))
		   (org-capture-templates
		    `(("t" "Table" table-line (file ,file)
		       "| x |" :immediate-finish t))))
	      (org-capture nil "t"))
	    (buffer-string))))
  ;; When `:prepend' is non-nil, add it as the first row after the
  ;; header, if there is one, or the first row otherwise.
  (should
   (equal "| a |\n|---|\n| x |\n| b |\n"
	  (org-test-with-temp-text-in-file "| a |\n|---|\n| b |"
	    (let* ((file (buffer-file-name))
		   (org-capture-templates
		    `(("t" "Table" table-line (file ,file)
		       "| x |" :immediate-finish t :prepend t))))
	      (org-capture nil "t"))
	    (buffer-string))))
  (should
   (equal "| x |\n| a |\n"
	  (org-test-with-temp-text-in-file "| a |"
	    (let* ((file (buffer-file-name))
		   (org-capture-templates
		    `(("t" "Table" table-line (file ,file)
		       "| x |" :immediate-finish t :prepend t))))
	      (org-capture nil "t"))
	    (buffer-string))))
  ;; When `:table-line-pos' is set and is meaningful, obey it.
  (should
   (equal "| a |\n|---|\n| b |\n| x |\n|---|\n| c |\n"
	  (org-test-with-temp-text-in-file "| a |\n|---|\n| b |\n|---|\n| c |"
	    (let* ((file (buffer-file-name))
		   (org-capture-templates
		    `(("t" "Table" table-line (file ,file)
		       "| x |" :immediate-finish t :table-line-pos "II-1"))))
	      (org-capture nil "t"))
	    (buffer-string))))
  (should
   (equal "| a |\n|---|\n| x |\n| b |\n|---|\n| c |\n"
	  (org-test-with-temp-text-in-file "| a |\n|---|\n| b |\n|---|\n| c |"
	    (let* ((file (buffer-file-name))
		   (org-capture-templates
		    `(("t" "Table" table-line (file ,file)
		       "| x |" :immediate-finish t :table-line-pos "I+1"))))
	      (org-capture nil "t"))
	    (buffer-string))))
  ;; Throw an error on invalid `:table-line-pos' specifications.
  (should-error
   (org-test-with-temp-text-in-file "| a |"
     (let* ((file (buffer-file-name))
	    (org-capture-templates
	     `(("t" "Table" table-line (file ,file)
		"| x |" :immediate-finish t :table-line-pos "II+99"))))
       (org-capture nil "t")
       t)))
  ;; Update formula when capturing one or more rows.
  (should
   (equal
    '(("@3$1" . "9"))
    (org-test-with-temp-text-in-file "| 1 |\n|---|\n| 9 |\n#+tblfm: @2$1=9"
      (let* ((file (buffer-file-name))
	     (org-capture-templates
	      `(("t" "Table" table-line (file ,file)
		 "| 2 |" :immediate-finish t :table-line-pos "I-1"))))
	(org-capture nil "t")
	(org-table-get-stored-formulas)))))
  (should
   (equal
    '(("@4$1" . "9"))
    (org-test-with-temp-text-in-file "| 1 |\n|---|\n| 9 |\n#+tblfm: @2$1=9"
      (let* ((file (buffer-file-name))
	     (org-capture-templates
	      `(("t" "Table" table-line (file ,file)
		 "| 2 |\n| 3 |" :immediate-finish t :table-line-pos "I-1"))))
	(org-capture nil "t")
	(org-table-get-stored-formulas)))))
  ;; Do not update formula when cell in inserted below affected row.
  (should-not
   (equal
    '(("@3$1" . "9"))
    (org-test-with-temp-text-in-file "| 1 |\n|---|\n| 9 |\n#+tblfm: @2$1=9"
      (let* ((file (buffer-file-name))
	     (org-capture-templates
	      `(("t" "Table" table-line (file ,file)
		 "| 2 |" :immediate-finish t))))
	(org-capture nil "t")
	(org-table-get-stored-formulas)))))
  ;; With a 0 prefix argument, ignore surrounding tables.
  (should
   (equal "|   |\n|---|\n| B |\nFoo\n\n| A |\n"
	  (org-test-with-temp-text-in-file "Foo\n\n| A |"
	    (let* ((file (buffer-file-name))
		   (org-capture-templates
		    `(("t" "Test" table-line (file ,file) "| B |"
		       :immediate-finish t))))
	      (org-capture 0 "t")
	      (buffer-string))))))

(ert-deftest test-org-capture/plain ()
  "Test `plain' type in capture template."
  ;; Insert at end of the file, unless `:prepend' is non-nil.
  (should
   (equal "Some text.\nFoo\n"
	  (org-test-with-temp-text-in-file "Some text."
	    (let* ((file (buffer-file-name))
		   (org-capture-templates
		    `(("t" "Text" plain (file ,file) "Foo"
		       :immediate-finish t))))
	      (org-capture nil "t")
	      (buffer-string)))))
  (should
   (equal "Foo\nSome text.\n"
	  (org-test-with-temp-text-in-file "Some text."
	    (let* ((file (buffer-file-name))
		   (org-capture-templates
		    `(("t" "Text" plain (file ,file) "Foo"
		       :immediate-finish t :prepend t))))
	      (org-capture nil "t")
	      (buffer-string)))))
  ;; When a headline is specified, add it at the beginning of the
  ;; entry, past any meta-data, or at its end, depending on
  ;; `:prepend'.
  (should
   (equal "* A\nSCHEDULED: <2012-03-29 Thu>\nSome text.\nFoo\n* B\n"
	  (org-test-with-temp-text-in-file
	      "* A\nSCHEDULED: <2012-03-29 Thu>\nSome text.\n* B"
	    (let* ((file (buffer-file-name))
		   (org-capture-templates
		    `(("t" "Text" plain (file+headline ,file "A") "Foo"
		       :immediate-finish t))))
	      (org-capture nil "t")
	      (buffer-string)))))
  (should
   (equal "* A\nSCHEDULED: <2012-03-29 Thu>\nFoo\nSome text.\n* B\n"
	  (org-test-with-temp-text-in-file
	      "* A\nSCHEDULED: <2012-03-29 Thu>\nSome text.\n* B"
	    (let* ((file (buffer-file-name))
		   (org-capture-templates
		    `(("t" "Text" plain (file+headline ,file "A") "Foo"
		       :immediate-finish t :prepend t))))
	      (org-capture nil "t")
	      (buffer-string)))))
  ;; At an exact position, in the middle of a line, make sure to
  ;; insert text on a line on its own.
  (should
   (equal "A\nX\nB\n"
	  (org-test-with-temp-text-in-file "AB"
	    (let* ((file (buffer-file-name))
		   (org-capture-templates
		    `(("t" "Text" plain (file+function ,file forward-char) "X"
		       :immediate-finish t))))
	      (org-capture nil "t")
	      (buffer-string)))))
  ;; Pathological case: insert an empty template in an empty file.
  (should
   (equal ""
	  (org-test-with-temp-text-in-file ""
	    (let* ((file (buffer-file-name))
		   (org-capture-templates
		    `(("t" "Text" plain (file ,file) ""
		       :immediate-finish t))))
	      (org-capture nil "t")
	      (buffer-string)))))
  ;; Test :unnarrowed property without a "%?" marker.
  (should
   (equal "SUCCESS\n"
	  (org-test-with-temp-text-in-file ""
	    (let* ((file (buffer-file-name))
		   (org-capture-templates
		    `(("t" "Text" plain (file ,file) "SUCCESS"
		       :unnarrowed t :immediate-finish t))))
	      (org-capture nil "t")
	      (buffer-string))))))

(provide 'test-org-capture)
;;; test-org-capture.el ends here
