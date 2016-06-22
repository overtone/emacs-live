;;; test-ox.el --- Tests for ox.el

;; Copyright (C) 2012-2015  Nicolas Goaziou

;; Author: Nicolas Goaziou <n.goaziou at gmail dot com>

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

;;; Code:

(unless (featurep 'ox)
  (signal 'missing-test-dependency "org-export"))

(defun org-test-default-backend ()
  "Return a default export back-end.
This back-end simply returns parsed data as Org syntax."
  (org-export-create-backend
   :transcoders (let (transcode-table)
		  (dolist (type (append org-element-all-elements
					org-element-all-objects)
				transcode-table)
		    (push
		     (cons type
			   (lambda (obj contents info)
			     (funcall
			      (intern (format "org-element-%s-interpreter"
					      type))
			      obj contents)))
		     transcode-table)))))

(defmacro org-test-with-parsed-data (data &rest body)
  "Execute body with parsed data available.
DATA is a string containing the data to be parsed.  BODY is the
body to execute.  Parse tree is available under the `tree'
variable, and communication channel under `info'."
  (declare (debug (form body)) (indent 1))
  `(org-test-with-temp-text ,data
     (org-export--delete-comments)
     (let* ((tree (org-element-parse-buffer))
	    (info (org-export-get-environment)))
       (org-export--prune-tree tree info)
       (org-export--remove-uninterpreted-data tree info)
       (let ((info (org-combine-plists
		    info (org-export-collect-tree-properties tree info))))
	 ,@body))))



;;; Internal Tests

(ert-deftest test-org-export/bind-keyword ()
  "Test reading #+BIND: keywords."
  ;; Test with `org-export-allow-bind-keywords' set to t.
  (should
   (org-test-with-temp-text "#+BIND: test-ox-var value"
     (let ((org-export-allow-bind-keywords t))
       (org-export-get-environment)
       (eq test-ox-var 'value))))
  ;; Test with `org-export-allow-bind-keywords' set to nil.
  (should-not
   (org-test-with-temp-text "#+BIND: test-ox-var value"
     (let ((org-export-allow-bind-keywords nil))
       (org-export-get-environment)
       (boundp 'test-ox-var))))
  ;; BIND keywords are case-insensitive.
  (should
   (org-test-with-temp-text "#+bind: test-ox-var value"
     (let ((org-export-allow-bind-keywords t))
       (org-export-get-environment)
       (eq test-ox-var 'value))))
  ;; Preserve order of BIND keywords.
  (should
   (org-test-with-temp-text "#+BIND: test-ox-var 1\n#+BIND: test-ox-var 2"
     (let ((org-export-allow-bind-keywords t))
       (org-export-get-environment)
       (eq test-ox-var 2))))
  ;; Read BIND keywords in setup files.
  (should
   (org-test-with-temp-text
       (format "#+SETUPFILE: \"%s/examples/setupfile.org\"" org-test-dir)
     (let ((org-export-allow-bind-keywords t))
       (org-export-get-environment)
       (eq variable 'value))))
  ;; Verify that bound variables are seen during export.
  (should
   (equal "Yes\n"
	  (org-test-with-temp-text "#+BIND: test-ox-var value"
	    (let ((org-export-allow-bind-keywords t))
	      (org-export-as
	       (org-export-create-backend
		:transcoders
		'((section . (lambda (s c i)
			       (if (eq test-ox-var 'value) "Yes" "No")))))))))))

(ert-deftest test-org-export/parse-option-keyword ()
  "Test reading all standard #+OPTIONS: items."
  (should
   (let ((options
	  (org-export--parse-option-keyword
	   "H:1 num:t \\n:t timestamp:t arch:t author:t creator:t d:t email:t \
*:t e:t ::t f:t pri:t -:t ^:t toc:t |:t tags:t tasks:t <:t todo:t inline:nil \
stat:t title:t")))
     (and (eq (plist-get options :headline-levels) 1)
	  (eq (plist-get options :section-numbers) t)
	  (eq (plist-get options :preserve-breaks) t)
	  (eq (plist-get options :time-stamp-file) t)
	  (eq (plist-get options :with-archived-trees) t)
	  (eq (plist-get options :with-author) t)
	  (eq (plist-get options :with-drawers) t)
	  (eq (plist-get options :with-email) t)
	  (eq (plist-get options :with-emphasize) t)
	  (eq (plist-get options :with-entities) t)
	  (eq (plist-get options :with-fixed-width) t)
	  (eq (plist-get options :with-footnotes) t)
	  (eq (plist-get options :with-priority) t)
	  (eq (plist-get options :with-special-strings) t)
	  (eq (plist-get options :with-sub-superscript) t)
	  (eq (plist-get options :with-toc) t)
	  (eq (plist-get options :with-tables) t)
	  (eq (plist-get options :with-tags) t)
	  (eq (plist-get options :with-tasks) t)
	  (eq (plist-get options :with-timestamps) t)
	  (eq (plist-get options :with-todo-keywords) t)
	  (eq (plist-get options :with-inlinetasks) nil)
	  (eq (plist-get options :with-statistics-cookies) t)
	  (eq (plist-get options :with-title) t))))
  ;; Test some special values.
  (should
   (let ((options
	  (org-export--parse-option-keyword
	   "arch:headline d:(\"TEST\") ^:{} toc:1 tags:not-in-toc tasks:todo \
num:2 <:active")))
     (and (eq (plist-get options :with-archived-trees) 'headline)
	  (eq (plist-get options :with-sub-superscript) '{})
	  (eq (plist-get options :with-toc) 1)
	  (eq (plist-get options :with-tags) 'not-in-toc)
	  (eq (plist-get options :with-tasks) 'todo)
	  (eq (plist-get options :section-numbers) 2)
	  (eq (plist-get options :with-timestamps) 'active)
	  (equal (plist-get options :with-drawers) '("TEST")))))
  ;; Test back-end specific values.
  (should
   (equal
    (org-export--parse-option-keyword
     "opt:t" (org-export-create-backend :options '((:option nil "opt"))))
    '(:option t)))
  ;; More than one property can refer to the same option item.
  (should
   (equal '(:opt1 t :opt2 t)
	  (org-export--parse-option-keyword
	   "opt:t"
	   (org-export-create-backend
	    :options '((:opt1 nil "opt") (:opt2 nil "opt")))))))

(ert-deftest test-org-export/get-inbuffer-options ()
  "Test reading all standard export keywords."
  ;; Properties should follow buffer order.
  (should
   (equal
    (org-test-with-temp-text "#+LANGUAGE: fr\n#+CREATOR: Me\n#+EMAIL: email"
      (org-export--get-inbuffer-options))
    '(:language "fr" :creator "Me" :email "email")))
  ;; Test `space' behaviour.
  (should
   (equal
    (let ((back-end (org-export-create-backend
		     :options '((:keyword "KEYWORD" nil nil space)))))
      (org-test-with-temp-text "#+KEYWORD: With\n#+KEYWORD: spaces"
	(org-export--get-inbuffer-options back-end)))
    '(:keyword "With spaces")))
  ;; Test `newline' behaviour.
  (should
   (equal
    (let ((back-end (org-export-create-backend
		     :options '((:keyword "KEYWORD" nil nil newline)))))
      (org-test-with-temp-text "#+KEYWORD: With\n#+KEYWORD: two lines"
	(org-export--get-inbuffer-options back-end)))
    '(:keyword "With\ntwo lines")))
  ;; Test `split' behaviour.
  (should
   (equal
    (org-test-with-temp-text "#+SELECT_TAGS: a\n#+SELECT_TAGS: b"
      (org-export--get-inbuffer-options))
    '(:select-tags ("a" "b"))))
  ;; Test `parse' behaviour.  `parse' implies `space' but preserve
  ;; line breaks.  Multi-line objects are allowed.
  (should
   (org-element-map
       (org-test-with-temp-text "#+TITLE: *bold*"
	 (plist-get (org-export--get-inbuffer-options) :title))
       'bold #'identity nil t))
  (should
   (equal
    (org-test-with-temp-text "#+TITLE: Some title\n#+TITLE: with spaces"
      (plist-get (org-export--get-inbuffer-options) :title))
    '("Some title with spaces")))
  (should
   (org-element-map
       (org-test-with-temp-text "#+TITLE: Some title\\\\\n#+TITLE: with spaces"
	 (plist-get (org-export--get-inbuffer-options) :title))
       'line-break #'identity nil t))
  (should
   (org-element-map
       (org-test-with-temp-text "#+TITLE: *bold\n#+TITLE: sentence*"
	 (plist-get (org-export--get-inbuffer-options) :title))
       'bold #'identity nil t))
  ;; Options set through SETUPFILE.
  (should
   (equal
    (org-test-with-temp-text
	(format "#+DESCRIPTION: l1
#+LANGUAGE: es
#+SELECT_TAGS: a
#+TITLE: a
#+SETUPFILE: \"%s/examples/setupfile.org\"
#+LANGUAGE: fr
#+SELECT_TAGS: c
#+TITLE: c"
		org-test-dir)
      (org-export--get-inbuffer-options))
    '(:language "fr" :select-tags ("a" "b" "c") :title ("a b c"))))
  ;; More than one property can refer to the same buffer keyword.
  (should
   (equal '(:k2 "value" :k1 "value")
	  (let ((backend (org-export-create-backend
			  :options '((:k1 "KEYWORD")
				     (:k2 "KEYWORD")))))
	    (org-test-with-temp-text "#+KEYWORD: value"
	      (org-export--get-inbuffer-options backend)))))
  ;; Keywords in commented subtrees are ignored.
  (should-not
   (equal "Me"
	  (org-test-with-parsed-data "* COMMENT H1\n#+AUTHOR: Me"
				     (plist-get info :author))))
  (should-not
   (equal "Mine"
	  (org-test-with-parsed-data "* COMMENT H1\n** H2\n#+EMAIL: Mine"
				     (plist-get info :email)))))

(ert-deftest test-org-export/get-subtree-options ()
  "Test setting options from headline's properties."
  ;; EXPORT_TITLE.
  (should
   (equal '("Subtree Title")
	  (org-test-with-temp-text "#+TITLE: Title
* Headline<point>
  :PROPERTIES:
  :EXPORT_TITLE: Subtree Title
  :END:
Paragraph"
	    (plist-get (org-export-get-environment nil t) :title))))
  ;; EXPORT_OPTIONS.
  (should
   (= 2
      (org-test-with-temp-text "#+OPTIONS: H:1
* Headline<point>
  :PROPERTIES:
  :EXPORT_OPTIONS: H:2
  :END:
Paragraph"
	(plist-get (org-export-get-environment nil t) :headline-levels))))
  ;; EXPORT_DATE.
  (should
   (equal '("29-03-2012")
	  (org-test-with-temp-text "#+DATE: today
* Headline<point>
  :PROPERTIES:
  :EXPORT_DATE: 29-03-2012
  :END:
Paragraph"
	    (plist-get (org-export-get-environment nil t) :date))))
  ;; Properties with `split' behaviour are stored as a list of
  ;; strings.
  (should
   (equal '("a" "b")
	  (org-test-with-temp-text "#+EXCLUDE_TAGS: noexport
* Headline<point>
  :PROPERTIES:
  :EXPORT_EXCLUDE_TAGS: a b
  :END:
Paragraph"
	    (plist-get (org-export-get-environment nil t) :exclude-tags))))
  ;; Handle :PROPERTY+: syntax.
  (should
   (equal '("a" "b")
	  (org-test-with-temp-text "#+EXCLUDE_TAGS: noexport
* Headline<point>
  :PROPERTIES:
  :EXPORT_EXCLUDE_TAGS: a
  :EXPORT_EXCLUDE_TAGS+: b
  :END:
Paragraph"
	    (plist-get (org-export-get-environment nil t) :exclude-tags))))
  ;; Export properties are case-insensitive.
  (should
   (equal '("29-03-2012")
	  (org-test-with-temp-text "* Headline
  :PROPERTIES:
  :EXPORT_Date: 29-03-2012
  :END:
Paragraph"
	    (plist-get (org-export-get-environment nil t) :date))))
  ;; Still grab correct options when section above is empty.
  (should
   (equal '("H1")
	  (org-test-with-temp-text "* H1\n** H11\n** H12<point>"
	    (plist-get (org-export-get-environment nil t) :title))))
  ;; More than one property can refer to the same node property.
  (should
   (equal '("1" "1")
	  (org-test-with-temp-text
	      "* H\n:PROPERTIES:\n:EXPORT_A: 1\n:END:\n<point>"
	    (let* ((backend (org-export-create-backend
			     :options '((:k1 "A")
					(:k2 "A"))))
		   (options (org-export-get-environment backend t)))
	      (list (plist-get options :k1) (plist-get options :k2)))))))

(ert-deftest test-org-export/set-title ()
  "Test title setting."
  ;; Without TITLE keyword.
  (should
   (equal
    ""
    (let (org-export-filter-body-functions
	  org-export-filter-final-output-functions)
      (org-test-with-temp-text "Test"
	(org-export-as
	 (org-export-create-backend
	  :transcoders
	  '((template . (lambda (text info)
			  (org-element-interpret-data
			   (plist-get info :title)))))))))))
  ;; With a blank TITLE keyword.
  (should
   (equal
    ""
    (let (org-export-filter-body-functions
	  org-export-filter-final-output-functions)
      (org-test-with-temp-text "#+TITLE:\nTest"
	(org-export-as
	 (org-export-create-backend
	  :transcoders
	  '((template . (lambda (text info)
			  (org-element-interpret-data
			   (plist-get info :title)))))))))))
  ;; With a non-empty TITLE keyword.
  (should
   (equal
    "Title"
    (org-test-with-temp-text "#+TITLE: Title\nTest"
      (org-export-as
       (org-export-create-backend
	:transcoders
	'((template . (lambda (text info)
			(org-element-interpret-data
			 (plist-get info :title))))))))))
  ;; When exporting a subtree, its heading becomes the headline of the
  ;; document...
  (should
   (equal
    "Headline"
    (org-test-with-temp-text "* Headline\nBody"
      (org-export-as
       (org-export-create-backend
	:transcoders
	'((template . (lambda (text info)
			(org-element-interpret-data
			 (plist-get info :title))))))
       'subtree))))
  ;; ... unless there is an EXPORT_TITLE property at the root of the
  ;; subtree.
  (should
   (equal
    "B"
    (org-test-with-temp-text
	"* A\n  :PROPERTIES:\n  :EXPORT_TITLE: B\n  :END:\nBody"
      (org-export-as
       (org-export-create-backend
	:transcoders
	'((template . (lambda (text info)
			(org-element-interpret-data
			 (plist-get info :title))))))
       'subtree)))))

(ert-deftest test-org-export/handle-options ()
  "Test if export options have an impact on output."
  ;; Test exclude tags for headlines and inlinetasks.
  (should
   (equal ""
	  (let (org-export-filter-body-functions
		org-export-filter-final-output-functions)
	    (org-test-with-temp-text "* Head1 :noexp:"
	      (org-export-as (org-test-default-backend)
			     nil nil nil '(:exclude-tags ("noexp")))))))
  ;; Test include tags for headlines and inlinetasks.
  (should
   (equal "* H2\n** Sub :exp:\n*** Sub Sub\n"
	  (org-test-with-temp-text "* H1\n* H2\n** Sub :exp:\n*** Sub Sub\n* H3"
	    (let ((org-tags-column 0))
	      (org-export-as (org-test-default-backend)
			     nil nil nil '(:select-tags ("exp")))))))
  ;; If there is an include tag, ignore the section before the first
  ;; headline, if any.
  (should
   (equal "* H1 :exp:\nBody\n"
	  (org-test-with-temp-text "First section\n* H1 :exp:\nBody"
	    (let ((org-tags-column 0))
	      (org-export-as (org-test-default-backend)
			     nil nil nil '(:select-tags ("exp")))))))
  (should-not
   (equal "* H1 :exp:\n"
	  (org-test-with-temp-text "* H1 :exp:\nBody"
	    (let ((org-tags-column 0))
	      (org-export-as (org-test-default-backend)
			     nil nil nil '(:select-tags ("exp")))))))
  ;; Test mixing include tags and exclude tags.
  (should
   (string-match
    "\\* Head1[ \t]+:export:\n\\*\\* Sub-Head2\n"
    (org-test-with-temp-text "
* Head1 :export:
** Sub-Head1 :noexport:
** Sub-Head2
* Head2 :noexport:
** Sub-Head1 :export:"
      (org-export-as (org-test-default-backend) nil nil nil
		     '(:select-tags ("export") :exclude-tags ("noexport"))))))
  ;; Ignore tasks.
  (should
   (equal ""
	  (let ((org-todo-keywords '((sequence "TODO" "DONE")))
		org-export-filter-body-functions
		org-export-filter-final-output-functions)
	    (org-test-with-temp-text "* TODO Head1"
	      (org-export-as (org-test-default-backend)
			     nil nil nil '(:with-tasks nil))))))
  (should
   (equal "* TODO Head1\n"
	  (let ((org-todo-keywords '((sequence "TODO" "DONE"))))
	    (org-test-with-temp-text "* TODO Head1"
	      (org-export-as (org-test-default-backend)
			     nil nil nil '(:with-tasks t))))))
  ;; Archived tree.
  (should
   (equal ""
	  (let (org-export-filter-body-functions
		org-export-filter-final-output-functions)
	    (org-test-with-temp-text "* Head1 :archive:"
	      (let ((org-archive-tag "archive"))
		(org-export-as (org-test-default-backend)
			       nil nil nil '(:with-archived-trees nil)))))))
  (should
   (string-match
    "\\* Head1[ \t]+:archive:"
    (org-test-with-temp-text "* Head1 :archive:\nbody\n** Sub-head 2"
      (let ((org-archive-tag "archive"))
	(org-export-as (org-test-default-backend) nil nil nil
		       '(:with-archived-trees headline))))))
  (should
   (string-match
    "\\`\\* Head1[ \t]+:archive:\n\\'"
    (org-test-with-temp-text "* Head1 :archive:"
      (let ((org-archive-tag "archive"))
	(org-export-as (org-test-default-backend)
		       nil nil nil '(:with-archived-trees t))))))
  ;; Clocks.
  (should
   (string-match "CLOCK: \\[2012-04-29 .* 10:45\\]"
		 (org-test-with-temp-text "CLOCK: [2012-04-29 sun. 10:45]"
		   (org-export-as (org-test-default-backend)
				  nil nil nil '(:with-clocks t)))))
  (should
   (equal ""
	  (let (org-export-filter-body-functions
		org-export-filter-final-output-functions)
	    (org-test-with-temp-text "CLOCK: [2012-04-29 sun. 10:45]"
	      (org-export-as (org-test-default-backend)
			     nil nil nil '(:with-clocks nil))))))
  ;; Drawers.
  (should
   (equal ""
	  (let (org-export-filter-body-functions
		org-export-filter-final-output-functions)
	    (org-test-with-temp-text ":TEST:\ncontents\n:END:"
	      (org-export-as (org-test-default-backend)
			     nil nil nil '(:with-drawers nil))))))
  (should
   (equal ":TEST:\ncontents\n:END:\n"
	  (org-test-with-temp-text ":TEST:\ncontents\n:END:"
	    (org-export-as (org-test-default-backend)
			   nil nil nil '(:with-drawers t)))))
  (should
   (equal ":FOO:\nkeep\n:END:\n"
	  (org-test-with-temp-text ":FOO:\nkeep\n:END:\n:BAR:\nremove\n:END:"
	    (org-export-as (org-test-default-backend)
			   nil nil nil '(:with-drawers ("FOO"))))))
  (should
   (equal ":FOO:\nkeep\n:END:\n"
	  (org-test-with-temp-text ":FOO:\nkeep\n:END:\n:BAR:\nremove\n:END:"
	    (org-export-as (org-test-default-backend)
			   nil nil nil '(:with-drawers (not "BAR"))))))
  ;; Fixed-width.
  (should
   (equal ": A\n"
	  (org-test-with-temp-text ": A"
	    (org-export-as (org-test-default-backend) nil nil nil
			   '(:with-fixed-width t)))))
  (should
   (equal ""
	  (let (org-export-filter-body-functions
		org-export-filter-final-output-functions)
	    (org-test-with-temp-text ": A"
	      (org-export-as (org-test-default-backend) nil nil nil
			     '(:with-fixed-width nil))))))
  ;; Footnotes.
  (should
   (equal "Footnote?"
	  (let ((org-footnote-section nil))
	    (org-test-with-temp-text "Footnote?[fn:1]\n\n[fn:1] Def"
	      (org-trim (org-export-as (org-test-default-backend)
				       nil nil nil '(:with-footnotes nil)))))))
  (should
   (equal "Footnote?[fn:1]\n\n[fn:1] Def"
	  (let ((org-footnote-section nil))
	    (org-test-with-temp-text "Footnote?[fn:1]\n\n[fn:1] Def"
	      (org-trim (org-export-as (org-test-default-backend)
				       nil nil nil '(:with-footnotes t)))))))
  ;; Inlinetasks.
  (when (featurep 'org-inlinetask)
    (should
     (equal
      ""
      (let ((org-inlinetask-min-level 15)
	    org-export-filter-body-functions
	    org-export-filter-final-output-functions)
	(org-test-with-temp-text "*************** Task"
	  (org-export-as (org-test-default-backend)
			 nil nil nil '(:with-inlinetasks nil))))))
    (should
     (equal
      ""
      (let ((org-inlinetask-min-level 15)
	    org-export-filter-body-functions
	    org-export-filter-final-output-functions)
	(org-test-with-temp-text
	    "*************** Task\nContents\n*************** END"
	  (org-export-as (org-test-default-backend)
			 nil nil nil '(:with-inlinetasks nil)))))))
  ;; Plannings.
  (should
   (string-match
    "* H\nCLOSED: \\[2012-04-29 .* 10:45\\]"
    (let ((org-closed-string "CLOSED:"))
      (org-test-with-temp-text "* H\nCLOSED: [2012-04-29 sun. 10:45]"
	(org-export-as (org-test-default-backend)
		       nil nil nil '(:with-planning t))))))
  (should
   (equal "* H\n"
	  (let ((org-closed-string "CLOSED:"))
	    (org-test-with-temp-text "* H\nCLOSED: [2012-04-29 sun. 10:45]"
	      (org-export-as (org-test-default-backend)
			     nil nil nil '(:with-planning nil))))))
  ;; Property Drawers.
  (should
   (equal "* H1\n"
	  (org-test-with-temp-text
	      "* H1\n  :PROPERTIES:\n  :PROP: value\n  :END:"
	    (org-export-as (org-test-default-backend)
			   nil nil nil '(:with-properties nil)))))
  (should
   (equal "* H1\n:PROPERTIES:\n:PROP:     value\n:END:\n"
	  (org-test-with-temp-text
	      "* H1\n  :PROPERTIES:\n  :PROP: value\n  :END:"
	    (org-export-as (org-test-default-backend)
			   nil nil nil '(:with-properties t)))))
  (should
   (equal "* H1\n:PROPERTIES:\n:B:        2\n:END:\n"
	  (org-test-with-temp-text
	      "* H1\n  :PROPERTIES:\n  :A: 1\n  :B: 2\n:END:"
	    (org-export-as (org-test-default-backend)
			   nil nil nil '(:with-properties ("B"))))))
  ;; Statistics cookies.
  (should
   (equal "* Stats"
	  (let (org-export-filter-body-functions
		org-export-filter-final-output-functions)
	    (org-trim
	     (org-test-with-temp-text "* Stats [0/0]"
	       (org-export-as (org-test-default-backend)
			      nil nil nil '(:with-statistics-cookies nil)))))))
  ;; Tables.
  (should
   (equal "| A |\n"
	  (org-test-with-temp-text "| A |"
	    (org-export-as (org-test-default-backend) nil nil nil
			   '(:with-tables t)))))
  (should
   (equal ""
	  (let (org-export-filter-body-functions
		org-export-filter-final-output-functions)
	    (org-test-with-temp-text "| A |"
	      (org-export-as (org-test-default-backend) nil nil nil
			     '(:with-tables nil)))))))

(ert-deftest test-org-export/with-timestamps ()
  "Test `org-export-with-timestamps' specifications."
  ;; t value.
  (should
   (string-match
    "\\[2012-04-29 .*? 10:45\\]<2012-04-29 .*? 10:45>"
    (org-test-with-temp-text "[2012-04-29 sun. 10:45]<2012-04-29 sun. 10:45>"
      (org-export-as (org-test-default-backend)
		     nil nil nil '(:with-timestamps t)))))
  ;; nil value.
  (should
   (equal
    ""
    (let (org-export-filter-body-functions
	  org-export-filter-final-output-functions)
      (org-trim
       (org-test-with-temp-text "[2012-04-29 sun. 10:45]<2012-04-29 sun. 10:45>"
	 (org-export-as (org-test-default-backend)
			nil nil nil '(:with-timestamps nil)))))))
  ;; `active' value.
  (should
   (string-match
    "<2012-03-29 .*?>\n\nParagraph <2012-03-29 .*?>\\[2012-03-29 .*?\\]"
    (org-test-with-temp-text
	"<2012-03-29 Thu>[2012-03-29 Thu]

Paragraph <2012-03-29 Thu>[2012-03-29 Thu]"
      (org-export-as (org-test-default-backend)
		     nil nil nil '(:with-timestamps active)))))
  ;; `inactive' value.
  (should
   (string-match
    "\\[2012-03-29 .*?\\]\n\nParagraph <2012-03-29 .*?>\\[2012-03-29 .*?\\]"
    (org-test-with-temp-text
	"<2012-03-29 Thu>[2012-03-29 Thu]

Paragraph <2012-03-29 Thu>[2012-03-29 Thu]"
      (org-export-as (org-test-default-backend)
		     nil nil nil '(:with-timestamps inactive))))))

(ert-deftest test-org-export/comment-tree ()
  "Test if export process ignores commented trees."
  (should
   (equal ""
	  (let (org-export-filter-body-functions
		org-export-filter-final-output-functions)
	    (org-test-with-temp-text "* COMMENT Head1"
	      (org-export-as (org-test-default-backend)))))))

(ert-deftest test-org-export/uninterpreted ()
  "Test handling of uninterpreted elements."
  ;; Entities.
  (should
   (equal "dummy\n"
	  (org-test-with-temp-text "\\alpha"
	    (org-export-as
	     (org-export-create-backend
	      :transcoders '((entity . (lambda (e c i) "dummy"))
			     (paragraph . (lambda (p c i) c))
			     (section . (lambda (s c i) c))))
	     nil nil nil '(:with-entities t)))))
  (should
   (equal "\\alpha\n"
	  (org-test-with-temp-text "\\alpha"
	    (org-export-as
	     (org-export-create-backend
	      :transcoders '((entity . (lambda (e c i) "dummy"))
			     (paragraph . (lambda (p c i) c))
			     (section . (lambda (s c i) c))))
	     nil nil nil '(:with-entities nil)))))
  ;; Emphasis.
  (should
   (equal "dummy\n"
	  (org-test-with-temp-text "*bold*"
	    (org-export-as
	     (org-export-create-backend
	      :transcoders '((bold . (lambda (b c i) "dummy"))
			     (paragraph . (lambda (p c i) c))
			     (section . (lambda (s c i) c))))
	     nil nil nil '(:with-emphasize t)))))
  (should
   (equal "*bold*\n"
	  (org-test-with-temp-text "*bold*"
	    (org-export-as
	     (org-export-create-backend
	      :transcoders '((bold . (lambda (b c i) "dummy"))
			     (paragraph . (lambda (p c i) c))
			     (section . (lambda (s c i) c))))
	     nil nil nil '(:with-emphasize nil)))))
  ;; LaTeX environment.
  (should
   (equal "dummy\n"
	  (org-test-with-temp-text "\\begin{equation}\n1+1=2\n\\end{equation}"
	    (org-export-as
	     (org-export-create-backend
	      :transcoders '((latex-environment . (lambda (l c i) "dummy"))
			     (section . (lambda (s c i) c))))
	     nil nil nil '(:with-latex t)))))
  (should
   (equal "\\begin{equation}\n1+1=2\n\\end{equation}\n"
	  (org-test-with-temp-text "\\begin{equation}\n1+1=2\n\\end{equation}"
	    (org-export-as
	     (org-export-create-backend
	      :transcoders '((latex-environment . (lambda (l c i) "dummy"))
			     (section . (lambda (s c i) c))))
	     nil nil nil '(:with-latex verbatim)))))
  ;; LaTeX fragment.
  (should
   (equal "dummy\n"
	  (org-test-with-temp-text "$1$"
	    (org-export-as
	     (org-export-create-backend
	      :transcoders '((latex-fragment . (lambda (l c i) "dummy"))
			     (paragraph . (lambda (p c i) c))
			     (section . (lambda (s c i) c))))
	     nil nil nil '(:with-latex t)))))
  (should
   (equal "$1$\n"
	  (org-test-with-temp-text "$1$"
	    (org-export-as
	     (org-export-create-backend
	      :transcoders '((latex-fragment . (lambda (l c i) "dummy"))
			     (paragraph . (lambda (p c i) c))
			     (section . (lambda (s c i) c))))
	     nil nil nil '(:with-latex verbatim)))))
  ;; Sub/superscript.
  (should
   (equal "adummy\n"
	  (org-test-with-temp-text "a_b"
	    (org-export-as
	     (org-export-create-backend
	      :transcoders '((subscript . (lambda (s c i) "dummy"))
			     (paragraph . (lambda (p c i) c))
			     (section . (lambda (s c i) c))))
	     nil nil nil '(:with-sub-superscript t)))))
  (should
   (equal "a_b\n"
	  (org-test-with-temp-text "a_b"
	    (org-export-as
	     (org-export-create-backend
	      :transcoders '((subscript . (lambda (s c i) "dummy"))
			     (paragraph . (lambda (p c i) c))
			     (section . (lambda (s c i) c))))
	     nil nil nil '(:with-sub-superscript nil)))))
  (should
   (equal "a_b\n"
	  (org-test-with-temp-text "a_b"
	    (org-export-as
	     (org-export-create-backend
	      :transcoders '((subscript . (lambda (s c i) "dummy"))
			     (paragraph . (lambda (p c i) c))
			     (section . (lambda (s c i) c))))
	     nil nil nil '(:with-sub-superscript {})))))
  (should
   (equal "adummy\n"
	  (org-test-with-temp-text "a_{b}"
	    (org-export-as
	     (org-export-create-backend
	      :transcoders '((subscript . (lambda (s c i) "dummy"))
			     (paragraph . (lambda (p c i) c))
			     (section . (lambda (s c i) c))))
	     nil nil nil '(:with-sub-superscript {})))))
  (should
   (equal "a_entity\n"
	  (org-test-with-temp-text "a_\\alpha"
	    (org-export-as
	     (org-export-create-backend
	      :transcoders '((entity . (lambda (e c i) "entity"))
			     (subscript . (lambda (s c i) "dummy"))
			     (paragraph . (lambda (p c i) c))
			     (section . (lambda (s c i) c))))
	     nil nil nil '(:with-sub-superscript nil)))))
  ;; Also handle uninterpreted objects in title.
  (should
   (equal "a_b"
	  (org-test-with-temp-text "#+TITLE: a_b"
	    (org-export-as
	     (org-export-create-backend
	      :transcoders
	      '((subscript . (lambda (s c i) "dummy"))
		(template . (lambda (c i) (org-export-data
				      (plist-get i :title) i)))
		(section . (lambda (s c i) c))))
	     nil nil nil '(:with-sub-superscript nil)))))
  ;; Handle uninterpreted objects in captions.
  (should
   (equal "adummy\n"
	  (org-test-with-temp-text "#+CAPTION: a_b\nParagraph"
	    (org-export-as
	     (org-export-create-backend
	      :transcoders
	      '((subscript . (lambda (s c i) "dummy"))
		(paragraph . (lambda (p c i)
			       (org-export-data (org-export-get-caption p) i)))
		(section . (lambda (s c i) c))))
	     nil nil nil '(:with-sub-superscript t)))))
  (should
   (equal "a_b\n"
	  (org-test-with-temp-text "#+CAPTION: a_b\nParagraph"
	    (org-export-as
	     (org-export-create-backend
	      :transcoders
	      '((subscript . (lambda (s c i) "dummy"))
		(paragraph . (lambda (p c i)
			       (org-export-data (org-export-get-caption p) i)))
		(section . (lambda (s c i) c))))
	     nil nil nil '(:with-sub-superscript nil)))))
  ;; Special case: multiples uninterpreted objects in a row.
  (should
   (equal "a_b_c_d\n"
	  (org-test-with-temp-text "a_b_c_d"
	    (org-export-as
	     (org-export-create-backend
	      :transcoders '((subscript . (lambda (s c i) "dummy"))
			     (paragraph . (lambda (p c i) c))
			     (section . (lambda (s c i) c))))
	     nil nil nil '(:with-sub-superscript {}))))))

(ert-deftest test-org-export/export-scope ()
  "Test all export scopes."
  (org-test-with-temp-text "
* Head1
** Head2
text
*** Head3"
    ;; Subtree.
    (forward-line 3)
    (should (equal (org-export-as (org-test-default-backend) 'subtree)
		   "text\n*** Head3\n"))
    ;; Visible.
    (goto-char (point-min))
    (forward-line)
    (org-cycle)
    (should (equal (org-export-as (org-test-default-backend) nil 'visible)
		   "* Head1\n"))
    ;; Region.
    (goto-char (point-min))
    (forward-line 3)
    (transient-mark-mode 1)
    (push-mark (point) t t)
    (goto-char (point-at-eol))
    (should (equal (org-export-as (org-test-default-backend)) "text\n")))
  ;; Subtree with a code block calling another block outside.
  (let ((org-export-babel-evaluate t))
    (should
     (equal ": 3\n"
	    (org-test-with-temp-text "
* Head1
#+BEGIN_SRC emacs-lisp :noweb yes :exports results
<<test>>
#+END_SRC
* Head2
#+NAME: test
#+BEGIN_SRC emacs-lisp
\(+ 1 2)
#+END_SRC"
	      (forward-line 1)
	      (org-export-as (org-test-default-backend) 'subtree)))))
  ;; Body only.
  (let ((backend (org-test-default-backend)))
    (setf (org-export-backend-transcoders backend)
	  (cons '(template . (lambda (body i)
			       (format "BEGIN\n%sEND" body)))
		(org-export-backend-transcoders backend)))
    (org-test-with-temp-text "Text"
      (should (equal (org-export-as backend nil nil 'body-only)
		     "Text\n"))
      (should (equal (org-export-as backend) "BEGIN\nText\nEND")))))

(ert-deftest test-org-export/output-file-name ()
  "Test `org-export-output-file-name' specifications."
  ;; Export from a file: name is built from original file name.
  (should
   (org-test-with-temp-text-in-file "Test"
     (equal (concat (file-name-as-directory ".")
		    (file-name-nondirectory
		     (file-name-sans-extension (buffer-file-name))))
	    (file-name-sans-extension (org-export-output-file-name ".ext")))))
  ;; When exporting to subtree, check EXPORT_FILE_NAME property first.
  (should
   (org-test-with-temp-text-in-file
       "* Test\n  :PROPERTIES:\n  :EXPORT_FILE_NAME: test\n  :END:"
     (equal (org-export-output-file-name ".ext" t) "./test.ext")))
  ;; From a buffer not associated to a file, too.
  (should
   (org-test-with-temp-text
       "* Test\n  :PROPERTIES:\n  :EXPORT_FILE_NAME: test\n  :END:"
     (equal (org-export-output-file-name ".ext" t) "./test.ext")))
  ;; When provided name is absolute, preserve it.
  (should
   (org-test-with-temp-text
       (format "* Test\n  :PROPERTIES:\n  :EXPORT_FILE_NAME: %s\n  :END:"
	       (expand-file-name "test"))
     (file-name-absolute-p (org-export-output-file-name ".ext" t))))
  ;; When PUB-DIR argument is provided, use it.
  (should
   (org-test-with-temp-text-in-file "Test"
     (equal (file-name-directory
	     (org-export-output-file-name ".ext" nil "dir/"))
	    "dir/")))
  ;; When returned name would overwrite original file, add EXTENSION
  ;; another time.
  (should
   (org-test-at-id "75282ba2-f77a-4309-a970-e87c149fe125"
     (equal (org-export-output-file-name ".org") "./normal.org.org"))))

(ert-deftest test-org-export/expand-include ()
  "Test file inclusion in an Org buffer."
  ;; Error when file isn't specified.
  (should-error
   (org-test-with-temp-text "#+INCLUDE: dummy.org"
     (org-export-expand-include-keyword)))
  ;; Full insertion with recursive inclusion.
  (should
   (equal
    (with-temp-buffer
      (insert-file
       (expand-file-name "examples/include.org" org-test-dir))
      (replace-regexp-in-string
       (regexp-quote "#+INCLUDE: \"include2.org\"")
       "Success!" (buffer-string)))
    (org-test-with-temp-text
	(format "#+INCLUDE: \"%s/examples/include.org\"" org-test-dir)
      (org-export-expand-include-keyword)
      (buffer-string))))
  ;; Localized insertion.
  (org-test-with-temp-text
      (format "#+INCLUDE: \"%s/examples/include.org\" :lines \"1-2\""
	      org-test-dir)
    (org-export-expand-include-keyword)
    (should (equal (buffer-string)
		   "Small Org file with an include keyword.\n")))
  ;; Insertion with constraints on headlines level.
  (should
   (equal
    "* Top heading\n** Heading\nbody\n"
    (org-test-with-temp-text
	(format
	 "* Top heading\n#+INCLUDE: \"%s/examples/include.org\" :lines \"9-11\""
	 org-test-dir)
      (org-export-expand-include-keyword)
      (buffer-string))))
  (should
   (equal
    "* Top heading\n* Heading\nbody\n"
    (org-test-with-temp-text
	(format
	 "* Top heading\n#+INCLUDE: \"%s/examples/include.org\" :lines \"9-11\" :minlevel 1"
	 org-test-dir)
      (org-export-expand-include-keyword)
      (buffer-string))))
  ;; Inclusion within an example block.
  (should
   (equal
    "#+BEGIN_EXAMPLE\nSmall Org file with an include keyword.\n#+END_EXAMPLE\n"
    (org-test-with-temp-text
	(format "#+INCLUDE: \"%s/examples/include.org\" :lines \"1-2\" EXAMPLE"
		org-test-dir)
      (org-export-expand-include-keyword)
      (buffer-string))))
  ;; Inclusion within a src-block.
  (should
   (equal
    "#+BEGIN_SRC emacs-lisp\n(+ 2 1)\n#+END_SRC\n"
    (org-test-with-temp-text
	(format
	 "#+INCLUDE: \"%s/examples/include.org\" :lines \"4-5\" SRC emacs-lisp"
	 org-test-dir)
      (org-export-expand-include-keyword)
      (buffer-string))))
  ;; Inclusion within an html export-block.
  (should
   (equal
    "#+BEGIN_HTML\n<p>HTML!</p>\n#+END_HTML\n"
    (org-test-with-temp-text
	(format
	 "#+INCLUDE: \"%s/examples/include.html\" HTML"
	 org-test-dir)
      (org-export-expand-include-keyword)
      (buffer-string))))
  ;; Inclusion within an center paragraph
  (should
   (equal
    "#+BEGIN_CENTER\nSuccess!\n#+END_CENTER\n"
    (org-test-with-temp-text
	(format
	 "#+INCLUDE: \"%s/examples/include2.org\" CENTER"
	 org-test-dir)
      (org-export-expand-include-keyword)
      (buffer-string))))
  ;; Footnotes labels are local to each included file.
  (should
   (= 6
      (length
       (delete-dups
	(let ((contents "
Footnotes[fn:1], [fn:test], [fn:test] and [fn:inline:anonymous footnote].
\[fn:1] Footnote 1
\[fn:test] Footnote \"test\""))
	  (org-test-with-temp-text-in-file contents
	    (let ((file1 (buffer-file-name)))
	      (org-test-with-temp-text-in-file contents
		(let ((file2 (buffer-file-name)))
		  (org-test-with-temp-text
		      (format "#+INCLUDE: \"%s\"\n#+INCLUDE: \"%s\""
			      file1 file2)
		    (org-export-expand-include-keyword)
		    (org-element-map (org-element-parse-buffer)
			'footnote-reference
		      (lambda (r) (org-element-property :label r)))))))))))))
  ;; Footnotes labels are not local to each include keyword.
  (should
   (= 4
      (length
       (delete-dups
	(let ((contents "
Footnotes[fn:1], [fn:test], [2] and [fn:inline:anonymous footnote].
\[fn:1] Footnote 1
\[2] Footnote 2
\[fn:test] Footnote \"test\""))
	  (org-test-with-temp-text-in-file contents
	    (let ((file (buffer-file-name)))
	      (org-test-with-temp-text
		  (format "#+INCLUDE: \"%s\"\n#+INCLUDE: \"%s\"" file file)
		(org-export-expand-include-keyword)
		(org-element-map (org-element-parse-buffer)
		    'footnote-reference
		  (lambda (ref) (org-element-property :label ref)))))))))))
  ;; Footnotes are supported by :lines-like elements and unnecessary
  ;; footnotes are dropped.
  (should
   (= 4
      (length
       (delete-dups
	(let ((contents "
* foo
Footnotes[fn:1]
* bar
Footnotes[fn:2], foot[fn:test], digit only[3], and [fn:inline:anonymous footnote]

\[fn:1] Footnote 1
\[fn:2] Footnote 1
* Footnotes
\[fn:test] Footnote \"test\"
\[3] Footnote 3
"))
	  (org-test-with-temp-text-in-file contents
	    (let ((file (buffer-file-name)))
	      (org-test-with-temp-text
		  (format "#+INCLUDE: \"%s::*bar\"
" file)
		(org-export-expand-include-keyword)
		(org-element-map (org-element-parse-buffer)
		    'footnote-definition
		  (lambda (ref) (org-element-property :label ref)))))))))))
  ;; If only-contents is non-nil only include contents of element.
  (should
   (equal
    "body\n"
    (org-test-with-temp-text
	(concat
	 (format "#+INCLUDE: \"%s/examples/include.org::*Heading\" " org-test-dir)
	 ":only-contents t")
      (org-export-expand-include-keyword)
      (buffer-string))))
  ;; Headings can be included via CUSTOM_ID.
  (should
   (org-test-with-temp-text
       (format "#+INCLUDE: \"%s/examples/include.org::#ah\"" org-test-dir)
     (org-export-expand-include-keyword)
     (goto-char (point-min))
     (looking-at "* Another heading")))
  ;; Named objects can be included.
  (should
   (equal
    "| 1 |\n"
    (org-test-with-temp-text
	(format "#+INCLUDE: \"%s/examples/include.org::tbl\" :only-contents t" org-test-dir)
      (org-export-expand-include-keyword)
      (buffer-string))))
  ;; Including non-existing elements should result in an error.
  (should-error
   (org-test-with-temp-text
       (format "#+INCLUDE: \"%s/examples/include.org::*non-existing heading\"" org-test-dir)
     (org-export-expand-include-keyword)))
  ;; Lines work relatively to an included element.
  (should
   (equal
    "2\n3\n"
    (org-test-with-temp-text
	(format "#+INCLUDE: \"%s/examples/include.org::#ah\" :only-contents t :lines \"2-3\"" org-test-dir)
      (org-export-expand-include-keyword)
      (buffer-string))))
  ;; Properties should be dropped from headlines.
  (should
   (equal
    (org-test-with-temp-text
	(format "#+INCLUDE: \"%s/examples/include.org::#ht\" :only-contents t" org-test-dir)
      (org-export-expand-include-keyword)
      (buffer-string))
    (org-test-with-temp-text
	(format "#+INCLUDE: \"%s/examples/include.org::tbl\"" org-test-dir)
      (org-export-expand-include-keyword)
      (buffer-string))))
  ;; Properties should be dropped, drawers should not be.
  (should
   (equal
    ":LOGBOOK:\ndrawer\n:END:\ncontent\n"
    (org-test-with-temp-text
	(format "#+INCLUDE: \"%s/examples/include.org::#dh\" :only-contents t" org-test-dir)
      (org-export-expand-include-keyword)
      (buffer-string))))
  ;; Adjacent INCLUDE-keywords should have the same :minlevel if unspecified.
  (should
   (org-every (lambda (level) (zerop (1- level)))
	      (org-test-with-temp-text
		  (concat
		   (format "#+INCLUDE: \"%s/examples/include.org::#ah\"\n" org-test-dir)
		   (format "#+INCLUDE: \"%s/examples/include.org::*Heading\"" org-test-dir))
		(org-export-expand-include-keyword)
		(org-element-map (org-element-parse-buffer) 'headline
		  (lambda (head) (org-element-property :level head))))))
  ;; INCLUDE does not insert induced :minlevel for src-blocks.
  (should-not
   (equal
    (org-test-with-temp-text
	(format "#+INCLUDE: \"%s/examples/include2.org\" src emacs-lisp" org-test-dir)
      (org-export-expand-include-keyword)
      (buffer-string))
    (org-test-with-temp-text
	(format "#+INCLUDE: \"%s/examples/include2.org\" src emacs-lisp :minlevel 1" org-test-dir)
      (org-export-expand-include-keyword)
      (buffer-string))))
  ;; INCLUDE assigns the relative :minlevel conditional on narrowing.
  (should
   (org-test-with-temp-text-in-file
       (format "* h1\n<point>#+INCLUDE: \"%s/examples/include.org::#ah\"" org-test-dir)
     (narrow-to-region (point) (point-max))
     (org-export-expand-include-keyword)
     (eq 1 (org-current-level))))
  ;; If :minlevel is present do not alter it.
  (should
   (org-test-with-temp-text
       (format "* h1\n<point>#+INCLUDE: \"%s/examples/include.org::#ah\" :minlevel 3" org-test-dir)
     (narrow-to-region (point) (point-max))
     (org-export-expand-include-keyword)
     (eq 3 (org-current-level)))))

(ert-deftest test-org-export/expand-macro ()
  "Test macro expansion in an Org buffer."
  ;; Standard macro expansion.
  (should
   (equal "#+MACRO: macro1 value\nvalue\n"
	  (org-test-with-temp-text "#+MACRO: macro1 value\n{{{macro1}}}"
	    (org-export-as (org-test-default-backend)))))
  ;; Allow macro in parsed keywords and associated properties.
  ;; Standard macro expansion.
  (should
   (string-match
    "#\\+K: value"
    (let ((backend (org-export-create-backend
		    :parent 'org
		    :options '((:k "K" nil nil parse)))))
      (org-test-with-temp-text "#+MACRO: macro value\n#+K: {{{macro}}}"
	(org-export-as backend)))))
  (should
   (string-match
    ":EXPORT_K: v"
    (let ((backend (org-export-create-backend
		    :parent 'org
		    :options '((:k "K" nil nil parse)))))
      (org-test-with-temp-text
	  "#+MACRO: m v\n* H\n:PROPERTIES:\n:EXPORT_K: {{{m}}}\n:END:"
	(org-export-as backend nil nil nil '(:with-properties t))))))
  ;; Expand specific macros.
  (should
   (equal "me 2012-03-29 me@here Title\n"
	  (org-test-with-temp-text
	      "
#+TITLE: Title
#+DATE: 2012-03-29
#+AUTHOR: me
#+EMAIL: me@here
{{{author}}} {{{date}}} {{{email}}} {{{title}}}"
	    (let ((output (org-export-as (org-test-default-backend))))
	      (substring output (string-match ".*\n\\'" output))))))
  ;; Expand specific macros when property contained a regular macro
  ;; already.
  (should
   (equal "value\n"
	  (org-test-with-temp-text "
#+MACRO: macro1 value
#+TITLE: {{{macro1}}}
{{{title}}}"
	    (let ((output (org-export-as (org-test-default-backend))))
	      (substring output (string-match ".*\n\\'" output))))))
  ;; Expand macros with templates in included files.
  (should
   (equal "success\n"
	  (org-test-with-temp-text
	      (format "#+INCLUDE: \"%s/examples/macro-templates.org\"
{{{included-macro}}}" org-test-dir)
	    (let ((output (org-export-as (org-test-default-backend))))
	      (substring output (string-match ".*\n\\'" output))))))
  ;; Date macro takes a optional formatting argument
  (should
   (equal "09-02-15\n"
	  (org-test-with-temp-text "{{{date(%d-%m-%y)}}}\n* d :noexport:\n#+DATE: <2015-02-09>"
	    (org-export-as (org-test-default-backend)))))
  ;; Only single timestamps are formatted
  (should
   (equal "<2015-02x-09>\n"
	  (org-test-with-temp-text "{{{date(%d-%m-%y)}}}\n* d :noexport:\n#+DATE: <2015-02x-09>"
	    (org-export-as (org-test-default-backend)))))
  ;; Throw an error when a macro definition is missing.
  (should-error
   (org-test-with-temp-text "{{{missing}}}"
     (org-export-as (org-test-default-backend))))
  ;; Macros defined in commented subtrees are ignored.
  (should-error
   (org-test-with-temp-text
       "* COMMENT H\n#+MACRO: macro1\n* H2\nvalue\n{{{macro1}}}"
     (org-export-as (org-test-default-backend))))
  (should-error
   (org-test-with-temp-text
       "* COMMENT H\n** H2\n#+MACRO: macro1\n* H3\nvalue\n{{{macro1}}}"
     (org-export-as (org-test-default-backend)))))

(ert-deftest test-org-export/before-processing-hook ()
  "Test `org-export-before-processing-hook'."
  (should
   (equal
    "#+MACRO: mac val\nTest\n"
    (org-test-with-temp-text "#+MACRO: mac val\n{{{mac}}} Test"
      (let ((org-export-before-processing-hook
	     '((lambda (backend)
		 (while (re-search-forward "{{{" nil t)
		   (let ((object (org-element-context)))
		     (when (eq (org-element-type object) 'macro)
		       (delete-region
			(org-element-property :begin object)
			(org-element-property :end object)))))))))
	(org-export-as (org-test-default-backend)))))))

(ert-deftest test-org-export/before-parsing-hook ()
  "Test `org-export-before-parsing-hook'."
  (should
   (equal "Body 1\nBody 2\n"
	  (org-test-with-temp-text "* Headline 1\nBody 1\n* Headline 2\nBody 2"
	    (let ((org-export-before-parsing-hook
		   '((lambda (backend)
		       (goto-char (point-min))
		       (while (re-search-forward org-outline-regexp-bol nil t)
			 (delete-region
			  (point-at-bol) (progn (forward-line) (point))))))))
	      (org-export-as (org-test-default-backend)))))))



;;; Affiliated Keywords

(ert-deftest test-org-export/read-attribute ()
  "Test `org-export-read-attribute' specifications."
  ;; Standard test.
  (should
   (equal
    (org-export-read-attribute
     :attr_html
     (org-test-with-temp-text "#+ATTR_HTML: :a 1 :b 2\nParagraph"
       (org-element-at-point)))
    '(:a "1" :b "2")))
  ;; Return nil on empty attribute.
  (should-not
   (org-export-read-attribute
    :attr_html
    (org-test-with-temp-text "Paragraph" (org-element-at-point))))
  ;; Return nil on "nil" string.
  (should
   (equal '(:a nil :b nil)
	  (org-export-read-attribute
	   :attr_html
	   (org-test-with-temp-text "#+ATTR_HTML: :a nil :b nil\nParagraph"
	     (org-element-at-point)))))
  ;; Return nil on empty string.
  (should
   (equal '(:a nil :b nil)
	  (org-export-read-attribute
	   :attr_html
	   (org-test-with-temp-text "#+ATTR_HTML: :a :b\nParagraph"
	     (org-element-at-point)))))
  ;; Return empty string when value is "".
  (should
   (equal '(:a "")
	  (org-export-read-attribute
	   :attr_html
	   (org-test-with-temp-text "#+ATTR_HTML: :a \"\"\nParagraph"
	     (org-element-at-point)))))
  ;; Return \"\" when value is """".
  (should
   (equal '(:a "\"\"")
	  (org-export-read-attribute
	   :attr_html
	   (org-test-with-temp-text "#+ATTR_HTML: :a \"\"\"\"\nParagraph"
	     (org-element-at-point)))))
  ;; Ignore text before first property.
  (should-not
   (member "ignore"
	   (org-export-read-attribute
	    :attr_html
	    (org-test-with-temp-text "#+ATTR_HTML: ignore :a 1\nParagraph"
	      (org-element-at-point))))))

(ert-deftest test-org-export/get-caption ()
  "Test `org-export-get-caption' specifications."
  ;; Without optional argument, return long caption
  (should
   (equal
    '("l")
    (org-test-with-temp-text "#+CAPTION[s]: l\nPara"
      (org-export-get-caption (org-element-at-point)))))
  ;; With optional argument, return short caption.
  (should
   (equal
    '("s")
    (org-test-with-temp-text "#+CAPTION[s]: l\nPara"
      (org-export-get-caption (org-element-at-point) t))))
  ;; Multiple lines are separated by white spaces.
  (should
   (equal
    '("a" " " "b")
    (org-test-with-temp-text "#+CAPTION: a\n#+CAPTION: b\nPara"
      (org-export-get-caption (org-element-at-point))))))



;;; Back-End Tools

(ert-deftest test-org-export/define-backend ()
  "Test back-end definition and accessors."
  ;; Translate table.
  (should
   (equal '((headline . my-headline-test))
	  (let (org-export-registered-backends)
	    (org-export-define-backend 'test '((headline . my-headline-test)))
	    (org-export-get-all-transcoders 'test))))
  ;; Filters.
  (should
   (equal '((:filter-headline . my-filter))
	  (let (org-export-registered-backends)
	    (org-export-define-backend 'test
	      '((headline . my-headline-test))
	      :filters-alist '((:filter-headline . my-filter)))
	    (org-export-backend-filters (org-export-get-backend 'test)))))
  ;; Options.
  (should
   (equal '((:prop value))
	  (let (org-export-registered-backends)
	    (org-export-define-backend 'test
	      '((headline . my-headline-test))
	      :options-alist '((:prop value)))
	    (org-export-backend-options (org-export-get-backend 'test)))))
  ;; Menu.
  (should
   (equal '(?k "Test Export" test)
	  (let (org-export-registered-backends)
	    (org-export-define-backend 'test
	      '((headline . my-headline-test))
	      :menu-entry '(?k "Test Export" test))
	    (org-export-backend-menu (org-export-get-backend 'test)))))
  ;; Export Blocks.
  (should
   (equal '(("TEST" . org-element-export-block-parser))
	  (let (org-export-registered-backends org-element-block-name-alist)
	    (org-export-define-backend 'test
	      '((headline . my-headline-test))
	      :export-block '("test"))
	    org-element-block-name-alist))))

(ert-deftest test-org-export/define-derived-backend ()
  "Test `org-export-define-derived-backend' specifications."
  ;; Error when parent back-end is not defined.
  (should-error
   (let (org-export-registered-backends)
     (org-export-define-derived-backend 'test 'parent)))
  ;; Append translation table to parent's.
  (should
   (equal '((:headline . test) (:headline . parent))
	  (let (org-export-registered-backends)
	    (org-export-define-backend 'parent '((:headline . parent)))
	    (org-export-define-derived-backend 'test 'parent
	      :translate-alist '((:headline . test)))
	    (org-export-get-all-transcoders 'test))))
  ;; Options defined in the new back have priority over those defined
  ;; in parent.
  (should
   (eq 'test
       (let (org-export-registered-backends)
	 (org-export-define-backend 'parent
	   '((:headline . parent))
	   :options-alist '((:a nil nil 'parent)))
	 (org-export-define-derived-backend 'test 'parent
	   :options-alist '((:a nil nil 'test)))
	 (plist-get (org-export--get-global-options
		     (org-export-get-backend 'test))
		    :a)))))

(ert-deftest test-org-export/derived-backend-p ()
  "Test `org-export-derived-backend-p' specifications."
  ;; Non-nil with direct match.
  (should
   (let (org-export-registered-backends)
     (org-export-define-backend 'test '((headline . test)))
     (org-export-derived-backend-p 'test 'test)))
  (should
   (let (org-export-registered-backends)
     (org-export-define-backend 'test '((headline . test)))
     (org-export-define-derived-backend 'test2 'test)
     (org-export-derived-backend-p 'test2 'test2)))
  ;; Non-nil with a direct parent.
  (should
   (let (org-export-registered-backends)
     (org-export-define-backend 'test '((headline . test)))
     (org-export-define-derived-backend 'test2 'test)
     (org-export-derived-backend-p 'test2 'test)))
  ;; Non-nil with an indirect parent.
  (should
   (let (org-export-registered-backends)
     (org-export-define-backend 'test '((headline . test)))
     (org-export-define-derived-backend 'test2 'test)
     (org-export-define-derived-backend 'test3 'test2)
     (org-export-derived-backend-p 'test3 'test)))
  ;; Nil otherwise.
  (should-not
   (let (org-export-registered-backends)
     (org-export-define-backend 'test '((headline . test)))
     (org-export-define-backend 'test2 '((headline . test2)))
     (org-export-derived-backend-p 'test2 'test)))
  (should-not
   (let (org-export-registered-backends)
     (org-export-define-backend 'test '((headline . test)))
     (org-export-define-backend 'test2 '((headline . test2)))
     (org-export-define-derived-backend 'test3 'test2)
     (org-export-derived-backend-p 'test3 'test))))

(ert-deftest test-org-export/get-all-transcoders ()
  "Test `org-export-get-all-transcoders' specifications."
  ;; Return nil when back-end cannot be found.
  (should-not (org-export-get-all-transcoders nil))
  ;; Same as `org-export-transcoders' if no parent.
  (should
   (equal '((headline . ignore))
	  (org-export-get-all-transcoders
	   (org-export-create-backend
	    :transcoders '((headline . ignore))))))
  ;; But inherit from all ancestors whenever possible.
  (should
   (equal '((section . ignore) (headline . ignore))
	  (let (org-export-registered-backends)
	    (org-export-define-backend 'b1 '((headline . ignore)))
	    (org-export-get-all-transcoders
	     (org-export-create-backend
	      :parent 'b1 :transcoders '((section . ignore)))))))
  (should
   (equal '((paragraph . ignore) (section . ignore) (headline . ignore))
	  (let (org-export-registered-backends)
	    (org-export-define-backend 'b1 '((headline . ignore)))
	    (org-export-define-derived-backend 'b2 'b1
	      :translate-alist '((section . ignore)))
	    (org-export-get-all-transcoders
	     (org-export-create-backend
	      :parent 'b2 :transcoders '((paragraph . ignore)))))))
  ;; Back-end transcoders overrule inherited ones.
  (should
   (eq 'b
       (let (org-export-registered-backends)
	 (org-export-define-backend 'b1 '((headline . a)))
	 (cdr (assq 'headline
		    (org-export-get-all-transcoders
		     (org-export-create-backend
		      :parent 'b1 :transcoders '((headline . b))))))))))

(ert-deftest test-org-export/get-all-options ()
  "Test `org-export-get-all-options' specifications."
  ;; Return nil when back-end cannot be found.
  (should-not (org-export-get-all-options nil))
  ;; Same as `org-export-options' if no parent.
  (should
   (equal '((headline . ignore))
	  (org-export-get-all-options
	   (org-export-create-backend
	    :options '((headline . ignore))))))
  ;; But inherit from all ancestors whenever possible.
  (should
   (equal '((:key2 value2) (:key1 value1))
	  (let (org-export-registered-backends)
	    (org-export-define-backend 'b1 nil :options-alist '((:key1 value1)))
	    (org-export-get-all-options
	     (org-export-create-backend
	      :parent 'b1 :options '((:key2 value2)))))))
  (should
   (equal '((:key3 value3) (:key2 value2) (:key1 value1))
	  (let (org-export-registered-backends)
	    (org-export-define-backend 'b1 nil :options-alist '((:key1 value1)))
	    (org-export-define-derived-backend 'b2 'b1
	      :options-alist '((:key2 value2)))
	    (org-export-get-all-options
	     (org-export-create-backend
	      :parent 'b2 :options '((:key3 value3)))))))
  ;; Back-end options overrule inherited ones.
  (should
   (eq 'b
       (let (org-export-registered-backends)
	 (org-export-define-backend 'b1 nil :options-alist '((:key1 . a)))
	 (cdr (assq :key1
		    (org-export-get-all-options
		     (org-export-create-backend
		      :parent 'b1 :options '((:key1 . b))))))))))

(ert-deftest test-org-export/get-all-filters ()
  "Test `org-export-get-all-filters' specifications."
  ;; Return nil when back-end cannot be found.
  (should-not (org-export-get-all-filters nil))
  ;; Same as `org-export-filters' if no parent.
  (should
   (equal '((:filter-headline . ignore))
	  (org-export-get-all-filters
	   (org-export-create-backend
	    :filters '((:filter-headline . ignore))))))
  ;; But inherit from all ancestors whenever possible.
  (should
   (equal '((:filter-section . ignore) (:filter-headline . ignore))
	  (let (org-export-registered-backends)
	    (org-export-define-backend 'b1
	      nil :filters-alist '((:filter-headline . ignore)))
	    (org-export-get-all-filters
	     (org-export-create-backend
	      :parent 'b1 :filters '((:filter-section . ignore)))))))
  (should
   (equal '((:filter-paragraph . ignore)
	    (:filter-section . ignore)
	    (:filter-headline . ignore))
	  (let (org-export-registered-backends)
	    (org-export-define-backend 'b1
	      nil :filters-alist '((:filter-headline . ignore)))
	    (org-export-define-derived-backend 'b2 'b1
	      :filters-alist '((:filter-section . ignore)))
	    (org-export-get-all-filters
	     (org-export-create-backend
	      :parent 'b2 :filters '((:filter-paragraph . ignore)))))))
  ;; Back-end filters overrule inherited ones.
  (should
   (eq 'b
       (let (org-export-registered-backends)
	 (org-export-define-backend 'b1 '((:filter-headline . a)))
	 (cdr (assq :filter-headline
		    (org-export-get-all-filters
		     (org-export-create-backend
		      :parent 'b1 :filters '((:filter-headline . b))))))))))

(ert-deftest test-org-export/with-backend ()
  "Test `org-export-with-backend' definition."
  ;; Error when calling an undefined back-end
  (should-error (org-export-with-backend nil "Test"))
  ;; Error when called back-end doesn't have an appropriate
  ;; transcoder.
  (should-error
   (org-export-with-backend
    (org-export-create-backend :transcoders '((headline . ignore)))
    "Test"))
  ;; Otherwise, export using correct transcoder
  (should
   (equal "Success"
	  (let (org-export-registered-backends)
	    (org-export-define-backend 'test
	      '((plain-text . (lambda (text contents info) "Failure"))))
	    (org-export-define-backend 'test2
	      '((plain-text . (lambda (text contents info) "Success"))))
	    (org-export-with-backend 'test2 "Test"))))
  ;; Provide correct back-end if transcoder needs to use recursive
  ;; calls anyway.
  (should
   (equal "Success\n"
	  (let ((test-back-end
		 (org-export-create-backend
		  :transcoders
		  '((headline . (lambda (headline contents info)
				  (org-export-data
				   (org-element-property :title headline)
				   info)))
		    (plain-text . (lambda (text info) "Success"))))))
	    (org-export-string-as
	     "* Test"
	     (org-export-create-backend
	      :transcoders
	      '((headline . (lambda (headline contents info)
			      (org-export-with-backend
			       test-back-end headline contents info))))))))))

(ert-deftest test-org-export/data-with-backend ()
  "Test `org-export-data-with-backend' specifications."
  ;; Error when calling an undefined back-end.
  (should-error (org-export-data-with-backend nil "nil" nil))
  ;; Otherwise, export data recursively, using correct back-end.
  (should
   (equal
    "Success!"
    (org-export-data-with-backend
     '(bold nil "Test")
     (org-export-create-backend
      :transcoders
      '((plain-text . (lambda (text info) "Success"))
	(bold . (lambda (bold contents info) (concat contents "!")))))
     '(:with-emphasize t)))))



;;; Comments

(ert-deftest test-org-export/comments ()
  "Test comments handling during export.
In particular, structure of the document mustn't be altered after
comments removal."
  (should
   (equal (org-test-with-temp-text "
Para1
# Comment

# Comment
Para2"
	    (org-export-as (org-test-default-backend)))
	  "Para1\n\nPara2\n"))
  (should
   (equal (org-test-with-temp-text "
Para1
# Comment
Para2"
	    (org-export-as (org-test-default-backend)))
	  "Para1\n\nPara2\n"))
  (should
   (equal (org-test-with-temp-text "
\[fn:1] Para1
# Inside definition


# Outside definition
Para2"
	    (org-export-as (org-test-default-backend)))
	  "[fn:1] Para1\n\n\nPara2\n"))
  (should
   (equal (org-test-with-temp-text "
\[fn:1] Para1

# Inside definition

# Inside definition

Para2"
	    (org-export-as (org-test-default-backend)))
	  "[fn:1] Para1\n\nPara2\n"))
  (should
   (equal (org-test-with-temp-text "
\[fn:1] Para1
# Inside definition

Para2"
	    (org-export-as (org-test-default-backend)))
	  "[fn:1] Para1\n\nPara2\n"))
  (should
   (equal (org-test-with-temp-text "
\[fn:1] Para1

# Inside definition
Para2"
	    (org-export-as (org-test-default-backend)))
	  "[fn:1] Para1\n\nPara2\n")))



;;; Export Snippets

(ert-deftest test-org-export/export-snippet ()
  "Test export snippets transcoding."
  ;; Standard test.
  (org-test-with-temp-text "@@test:A@@@@t:B@@"
    (let ((backend (org-test-default-backend)))
      (setf (org-export-backend-name backend) 'test)
      (setf (org-export-backend-transcoders backend)
	    (cons (cons 'export-snippet
			(lambda (snippet contents info)
			  (when (eq (org-export-snippet-backend snippet) 'test)
			    (org-element-property :value snippet))))
		  (org-export-backend-transcoders backend)))
      (let ((org-export-snippet-translation-alist nil))
	(should (equal (org-export-as backend) "A\n")))
      (let ((org-export-snippet-translation-alist '(("t" . "test"))))
	(should (equal (org-export-as backend) "AB\n")))))
  ;; Ignored export snippets do not remove any blank.
  (should
   (equal "begin  end\n"
	  (org-test-with-parsed-data "begin @@test:A@@ end"
	    (org-export-data-with-backend
	     tree
	     (org-export-create-backend
	      :transcoders
	      '((paragraph . (lambda (paragraph contents info) contents))
		(section . (lambda (section contents info) contents))))
	     info)))))



;;; Footnotes

(ert-deftest test-org-export/footnote-first-reference-p ()
  "Test `org-export-footnote-first-reference-p' specifications."
  (should
   (equal
    '(t nil)
    (org-test-with-temp-text "Text[fn:1][fn:1]\n\n[fn:1] Definition"
      (let (result)
	(org-export-as
	 (org-export-create-backend
	  :transcoders
	  `(,(cons 'footnote-reference
		   (lambda (f c i)
		     (push (org-export-footnote-first-reference-p f info)
			   result)
		     ""))
	    (section . (lambda (s c i) c))
	    (paragraph . (lambda (p c i) c))))
	 nil nil nil '(:with-footnotes t))
	(nreverse result)))))
  ;; Limit check to DATA, when non-nil.
  (should
   (equal
    '(nil t)
    (org-test-with-parsed-data "Text[fn:1]\n* H\nText[fn:1]\n\n[fn:1] D1"
      (let (result)
	(org-element-map tree 'footnote-reference
	  (lambda (ref)
	    (push
	     (org-export-footnote-first-reference-p
	      ref info (org-element-map tree 'headline #'identity info t))
	     result))
	  info)
	(nreverse result)))))
  (should
   (equal
    '(t nil)
    (org-test-with-parsed-data "Text[fn:1]\n* H\nText[fn:1]\n\n[fn:1] D1"
      (let (result)
	(org-element-map tree 'footnote-reference
	  (lambda (ref)
	    (push (org-export-footnote-first-reference-p ref info) result))
	  info)
	(nreverse result)))))
  ;; If optional argument BODY-FIRST is non-nil, first find footnote
  ;; in the main body of the document.  Otherwise, enter footnote
  ;; definitions when they are encountered.
  (should
   (equal
    '(t nil)
    (org-test-with-temp-text
	":BODY:\nText[fn:1][fn:2]\n:END:\n\n[fn:1] Definition[fn:2]\n\n[fn:2] Inner"
      (let (result)
	(org-export-as
	 (org-export-create-backend
	  :transcoders
	  `(,(cons 'footnote-reference
		   (lambda (f c i)
		     (when (org-element-lineage f '(drawer))
		       (push (org-export-footnote-first-reference-p f info nil)
			     result))
		     ""))
	    (drawer . (lambda (d c i) c))
	    (footnote-definition . (lambda (d c i) c))
	    (section . (lambda (s c i) c))
	    (paragraph . (lambda (p c i) c))))
	 nil nil nil '(:with-footnotes t))
	(nreverse result)))))
  (should
   (equal
    '(t t)
    (org-test-with-temp-text
	":BODY:\nText[fn:1][fn:2]\n:END:\n\n[fn:1] Definition[fn:2]\n\n[fn:2] Inner"
      (let (result)
	(org-export-as
	 (org-export-create-backend
	  :transcoders
	  `(,(cons 'footnote-reference
		   (lambda (f c i)
		     (when (org-element-lineage f '(drawer))
		       (push (org-export-footnote-first-reference-p f info nil t)
			     result))
		     ""))
	    (drawer . (lambda (d c i) c))
	    (footnote-definition . (lambda (d c i) c))
	    (section . (lambda (s c i) c))
	    (paragraph . (lambda (p c i) c))))
	 nil nil nil '(:with-footnotes t))
	(nreverse result))))))

(ert-deftest test-org-export/get-footnote-number ()
  "Test `org-export-get-footnote-number' specifications."
  (should
   (equal '(1 2 1)
	  (org-test-with-parsed-data
	      "Text[fn:1][fn:2][fn:1]\n\n[fn:1] Def\n[fn:2] Def"
	    (org-element-map tree 'footnote-reference
	      (lambda (ref) (org-export-get-footnote-number ref info))
	      info))))
  ;; Anonymous footnotes all get a new number.
  (should
   (equal '(1 2)
	  (org-test-with-parsed-data
	      "Text[fn::anon1][fn::anon2]"
	    (org-element-map tree 'footnote-reference
	      (lambda (ref) (org-export-get-footnote-number ref info))
	      info))))
  ;; Test nested footnotes order.
  (should
   (equal
    '((1 . "fn:1") (2 . "fn:2") (3 . "fn:3") (3 . "fn:3") (4))
    (org-test-with-parsed-data
	"Text[fn:1:A[fn:2]] [fn:3].\n\n[fn:2] B [fn:3] [fn::D].\n\n[fn:3] C."
      (org-element-map tree 'footnote-reference
	(lambda (ref)
	  (cons (org-export-get-footnote-number ref info)
		(org-element-property :label ref)))
	info))))
  ;; Limit number to provided DATA, when non-nil.
  (should
   (equal
    '(1)
    (org-test-with-parsed-data
	"Text[fn:1]\n* H\nText[fn:2]\n\n[fn:1] D1\n[fn:2] D2"
      (org-element-map tree 'footnote-reference
	(lambda (ref)
	  (org-export-get-footnote-number
	   ref info (org-element-map tree 'headline #'identity info t)))
	info))))
  (should
   (equal
    '(1 2)
    (org-test-with-parsed-data
	"Text[fn:1]\n* H\nText[fn:2]\n\n[fn:1] D1\n[fn:2]"
      (org-element-map tree 'footnote-reference
	(lambda (ref) (org-export-get-footnote-number ref info))
	info))))
  ;; With a non-nil BODY-FIRST optional argument, first check body,
  ;; then footnote definitions.
  (should
   (equal
    '(("fn:1" . 1) ("fn:2" . 2) ("fn:3" . 3) ("fn:3" . 3))
    (org-test-with-parsed-data
	"Text[fn:1][fn:2][fn:3]\n\n[fn:1] Def[fn:3]\n[fn:2] Def\n[fn:3] Def"
      (org-element-map tree 'footnote-reference
	(lambda (ref)
	  (cons (org-element-property :label ref)
		(org-export-get-footnote-number ref info nil t)))
	info))))
  (should
   (equal
    '(("fn:1" . 1) ("fn:2" . 3) ("fn:3" . 2) ("fn:3" . 2))
    (org-test-with-parsed-data
	"Text[fn:1][fn:2][fn:3]\n\n[fn:1] Def[fn:3]\n[fn:2] Def\n[fn:3] Def"
      (org-element-map tree 'footnote-reference
	(lambda (ref)
	  (cons (org-element-property :label ref)
		(org-export-get-footnote-number ref info nil)))
	info)))))

(ert-deftest test-org-export/collect-footnote-definitions ()
  "Test `org-export-collect-footnote-definitions' specifications."
  (should
   (= 4
      (org-test-with-parsed-data "Text[fn:1:A[fn:2]] [fn:3].

\[fn:2] B [fn:3] [fn::D].

\[fn:3] C."
	(length (org-export-collect-footnote-definitions info)))))
  ;; Limit number to provided DATA, when non-nil.
  (should
   (equal
    '((1 "fn:2"))
    (org-test-with-parsed-data
	"Text[fn:1]\n* H\nText[fn:2]\n\n[fn:1] D1\n[fn:2] D2"
      (mapcar #'butlast
	      (org-export-collect-footnote-definitions
	       info (org-element-map tree 'headline #'identity info t))))))
  (should
   (equal
    '((1 "fn:1") (2 "fn:2"))
    (org-test-with-parsed-data
	"Text[fn:1]\n* H\nText[fn:2]\n\n[fn:1] D1\n[fn:2] D2"
      (mapcar #'butlast (org-export-collect-footnote-definitions info)))))
  ;; With optional argument BODY-FIRST, first check body, then
  ;; footnote definitions.
  (should
   (equal '("fn:1" "fn:3" "fn:2" nil)
	  (org-test-with-parsed-data "Text[fn:1:A[fn:2]] [fn:3].

\[fn:2] B [fn:3] [fn::D].

\[fn:3] C."
	    (mapcar (lambda (e) (nth 1 e))
		    (org-export-collect-footnote-definitions info nil t)))))
  (should-not
   (equal '("fn:1" "fn:3" "fn:2" nil)
	  (org-test-with-parsed-data "Text[fn:1:A[fn:2]] [fn:3].

\[fn:2] B [fn:3] [fn::D].

\[fn:3] C."
	    (mapcar (lambda (e) (nth 1 e))
		    (org-export-collect-footnote-definitions info))))))

(ert-deftest test-org-export/footnotes ()
  "Miscellaneous tests on footnotes."
  (let ((org-footnote-section nil)
	(org-export-with-footnotes t))
    ;; Read every type of footnote.
    (should
     (equal
      '((1 . "A\n") (2 . "B") (3 . "C") (4 . "D"))
      (org-test-with-parsed-data
	  "Text[fn:1] [1] [fn:label:C] [fn::D]\n\n[fn:1] A\n\n[1] B"
	(org-element-map tree 'footnote-reference
	  (lambda (ref)
	    (let ((def (org-export-get-footnote-definition ref info)))
	      (cons (org-export-get-footnote-number ref info)
		    (if (eq (org-element-property :type ref) 'inline) (car def)
		      (car (org-element-contents
			    (car (org-element-contents def))))))))
	  info))))
    ;; Test nested footnote in invisible definitions.
    (should
     (= 2
	(org-test-with-temp-text "Text[1]\n\n[1] B [2]\n\n[2] C."
	  (narrow-to-region (point) (line-end-position))
	  (catch 'exit
	    (org-export-as
	     (org-export-create-backend
	      :transcoders
	      '((section
		 .
		 (lambda (s c i)
		   (throw 'exit (length
				 (org-export-collect-footnote-definitions
				  i))))))))))))
    ;; Test export of footnotes defined outside parsing scope.
    (should
     (equal
      "ParagraphOut of scope\n"
      (org-test-with-temp-text "[fn:1] Out of scope
* Title
<point>Paragraph[fn:1]"
	(let ((backend (org-test-default-backend)))
	  (setf (org-export-backend-transcoders backend)
		(append
		 (list (cons 'footnote-reference
			     (lambda (fn contents info)
			       (org-element-interpret-data
				(org-export-get-footnote-definition fn info))))
		       (cons 'footnote-definition #'ignore)
		       (cons 'headline #'ignore))
		 (org-export-backend-transcoders backend)))
	  (org-export-as backend 'subtree)))))
    ;; Footnotes without a definition should throw an error.
    (should-error
     (org-test-with-parsed-data "Text[fn:1]"
       (org-export-get-footnote-definition
	(org-element-map tree 'footnote-reference #'identity info t) info)))
    ;; Footnote section should be ignored in TOC and in headlines
    ;; numbering.
    (should
     (= 1 (let ((org-footnote-section "Footnotes"))
	    (length (org-test-with-parsed-data "* H1\n* Footnotes\n"
		      (org-export-collect-headlines info))))))
    (should
     (equal '(2)
	    (let ((org-footnote-section "Footnotes"))
	      (org-test-with-parsed-data "* H1\n* Footnotes\n* H2"
		(org-element-map tree 'headline
		  (lambda (hl)
		    (when (equal (org-element-property :raw-value hl) "H2")
		      (org-export-get-headline-number hl info)))
		  info t)))))))



;;; Headlines and Inlinetasks

(ert-deftest test-org-export/get-relative-level ()
  "Test `org-export-get-relative-level' specifications."
  ;; Standard test.
  (should
   (equal '(1 2)
	  (let ((org-odd-levels-only nil))
	    (org-test-with-parsed-data "* Headline 1\n** Headline 2"
	      (org-element-map tree 'headline
		(lambda (h) (org-export-get-relative-level h info))
		info)))))
  ;; Missing levels
  (should
   (equal '(1 3)
	  (let ((org-odd-levels-only nil))
	    (org-test-with-parsed-data "** Headline 1\n**** Headline 2"
	      (org-element-map tree 'headline
		(lambda (h) (org-export-get-relative-level h info))
		info))))))

(ert-deftest test-org-export/low-level-p ()
  "Test `org-export-low-level-p' specifications."
  (should
   (equal
    '(no yes)
    (let ((org-odd-levels-only nil))
      (org-test-with-parsed-data "* Headline 1\n** Headline 2"
	(org-element-map tree 'headline
	  (lambda (h) (if (org-export-low-level-p h info) 'yes 'no))
	  (plist-put info :headline-levels 1)))))))

(ert-deftest test-org-export/get-headline-number ()
  "Test `org-export-get-headline-number' specifications."
  ;; Standard test.
  (should
   (equal
    '((1) (1 1))
    (let ((org-odd-levels-only nil))
      (org-test-with-parsed-data "* Headline 1\n** Headline 2"
	(org-element-map tree 'headline
	  (lambda (h) (org-export-get-headline-number h info))
	  info)))))
  ;; Missing levels are replaced with 0.
  (should
   (equal
    '((1) (1 0 1))
    (let ((org-odd-levels-only nil))
      (org-test-with-parsed-data "* Headline 1\n*** Headline 2"
	(org-element-map tree 'headline
	  (lambda (h) (org-export-get-headline-number h info))
	  info))))))

(ert-deftest test-org-export/numbered-headline-p ()
  "Test `org-export-numbered-headline-p' specifications."
  ;; If `:section-numbers' is nil, never number headlines.
  (should-not
   (org-test-with-parsed-data "* Headline"
     (org-element-map tree 'headline
       (lambda (h) (org-export-numbered-headline-p h info))
       (plist-put info :section-numbers nil))))
  ;; If `:section-numbers' is a number, only number headlines with
  ;; a level greater that it.
  (should
   (equal
    '(yes no)
    (org-test-with-parsed-data "* Headline 1\n** Headline 2"
      (org-element-map tree 'headline
	(lambda (h) (if (org-export-numbered-headline-p h info) 'yes 'no))
	(plist-put info :section-numbers 1)))))
  ;; Otherwise, headlines are always numbered.
  (should
   (org-test-with-parsed-data "* Headline"
     (org-element-map tree 'headline
       (lambda (h) (org-export-numbered-headline-p h info))
       (plist-put info :section-numbers t))))
  ;; With #+OPTIONS: num:nil all headlines are unnumbered.
  (should-not
   (org-test-with-parsed-data "* H\n#+OPTIONS: num:nil"
     (org-export-numbered-headline-p
      (org-element-map tree 'headline 'identity info t)
      info)))
  ;; Headlines with a non-nil UNNUMBERED property are not numbered.
  (should-not
   (org-test-with-parsed-data "* H\n:PROPERTIES:\n:UNNUMBERED: t\n:END:"
     (org-export-numbered-headline-p
      (org-element-map tree 'headline #'identity info t)
      info)))
  ;; UNNUMBERED ignores inheritance.  Any non-nil value among
  ;; ancestors disables numbering.
  (should
   (org-test-with-parsed-data
       "* H
:PROPERTIES:
:UNNUMBERED: t
:END:
** H2
:PROPERTIES:
:UNNUMBERED: nil
:END:
*** H3"
     (org-every
      (lambda (h) (not (org-export-numbered-headline-p h info)))
      (org-element-map tree 'headline #'identity info)))))

(ert-deftest test-org-export/number-to-roman ()
  "Test `org-export-number-to-roman' specifications."
  ;; If number is negative, return it as a string.
  (should (equal (org-export-number-to-roman -1) "-1"))
  ;; Otherwise, return it as a roman number.
  (should (equal (org-export-number-to-roman 1449) "MCDXLIX")))

(ert-deftest test-org-export/get-optional-title ()
  "Test `org-export-get-alt-title' specifications."
  ;; If ALT_TITLE property is defined, use it.
  (should
   (equal '("opt")
	  (org-test-with-parsed-data
	      "* Headline\n:PROPERTIES:\n:ALT_TITLE: opt\n:END:"
	    (org-export-get-alt-title
	     (org-element-map tree 'headline 'identity info t)
	     info))))
  ;; Otherwise, fall-back to regular title.
  (should
   (equal '("Headline")
	  (org-test-with-parsed-data "* Headline"
	    (org-export-get-alt-title
	     (org-element-map tree 'headline 'identity info t)
	     info)))))

(ert-deftest test-org-export/get-tags ()
  "Test `org-export-get-tags' specifications."
  (let ((org-export-exclude-tags '("noexport"))
	(org-export-select-tags '("export")))
    ;; Standard test: tags which are not a select tag, an exclude tag,
    ;; or specified as optional argument shouldn't be ignored.
    (should
     (org-test-with-parsed-data "* Headline :tag:"
       (org-export-get-tags (org-element-map tree 'headline 'identity info t)
			    info)))
    ;; Exclude tags are removed.
    (should-not
     (org-test-with-parsed-data "* Headline :noexport:"
       (org-export-get-tags (org-element-map tree 'headline 'identity info t)
			    info)))
    ;; Select tags are removed.
    (should-not
     (org-test-with-parsed-data "* Headline :export:"
       (org-export-get-tags (org-element-map tree 'headline 'identity info t)
			    info)))
    (should
     (equal
      '("tag")
      (org-test-with-parsed-data "* Headline :tag:export:"
	(org-export-get-tags (org-element-map tree 'headline 'identity info t)
			     info))))
    ;; Tags provided in the optional argument are also ignored.
    (should-not
     (org-test-with-parsed-data "* Headline :ignore:"
       (org-export-get-tags (org-element-map tree 'headline 'identity info t)
			    info '("ignore"))))
    ;; Allow tag inheritance.
    (should
     (equal
      '(("tag") ("tag"))
      (org-test-with-parsed-data "* Headline :tag:\n** Sub-heading"
	(org-element-map tree 'headline
	  (lambda (hl) (org-export-get-tags hl info nil t)) info))))
    ;; Tag inheritance checks FILETAGS keywords.
    (should
     (equal
      '(("a" "b" "tag"))
      (org-test-with-parsed-data "#+FILETAGS: :a:b:\n* Headline :tag:"
	(org-element-map tree 'headline
	  (lambda (hl) (org-export-get-tags hl info nil t)) info))))))

(ert-deftest test-org-export/get-node-property ()
  "Test`org-export-get-node-property' specifications."
  ;; Standard test.
  (should
   (equal "value"
	  (org-test-with-parsed-data "* Headline
  :PROPERTIES:
  :prop:     value
  :END:"
	    (org-export-get-node-property
	     :PROP (org-element-map tree 'headline 'identity nil t)))))
  ;; Test inheritance.
  (should
   (equal "value"
	  (org-test-with-parsed-data "* Parent
  :PROPERTIES:
  :prop:     value
  :END:
** Headline
   Paragraph"
	    (org-export-get-node-property
	     :PROP (org-element-map tree 'paragraph 'identity nil t) t))))
  ;; Cannot return a value before the first headline.
  (should-not
   (org-test-with-parsed-data "Paragraph
* Headline
  :PROPERTIES:
  :prop:     value
  :END:"
     (org-export-get-node-property
      :PROP (org-element-map tree 'paragraph 'identity nil t)))))

(ert-deftest test-org-export/get-category ()
  "Test `org-export-get-category' specifications."
  ;; Standard test.
  (should
   (equal "value"
	  (org-test-with-parsed-data "* Headline
  :PROPERTIES:
  :CATEGORY:     value
  :END:"
	    (org-export-get-category
	     (org-element-map tree 'headline 'identity nil t) info))))
  ;; Test inheritance from a parent headline.
  (should
   (equal '("value" "value")
	  (org-test-with-parsed-data "* Headline1
  :PROPERTIES:
  :CATEGORY:     value
  :END:
** Headline2"
	    (org-element-map tree 'headline
	      (lambda (hl) (org-export-get-category hl info)) info))))
  ;; Test inheritance from #+CATEGORY keyword
  (should
   (equal "value"
	  (org-test-with-parsed-data "#+CATEGORY: value
* Headline"
	    (org-export-get-category
	     (org-element-map tree 'headline 'identity nil t) info))))
  ;; Test inheritance from file name.
  (should
   (equal "test"
	  (org-test-with-parsed-data "* Headline"
	    (let ((info (plist-put info :input-file "~/test.org")))
	      (org-export-get-category
	       (org-element-map tree 'headline 'identity nil t) info)))))
  ;; Fall-back value.
  (should
   (equal "???"
	  (org-test-with-parsed-data "* Headline"
	    (org-export-get-category
	     (org-element-map tree 'headline 'identity nil t) info)))))

(ert-deftest test-org-export/first-sibling-p ()
  "Test `org-export-first-sibling-p' specifications."
  ;; Standard test.
  (should
   (equal
    '(yes yes no)
    (org-test-with-parsed-data "* H\n** H 2\n** H 3"
      (org-element-map tree 'headline
	(lambda (h) (if (org-export-first-sibling-p h info) 'yes 'no))
	info))))
  (should
   (equal '(yes no)
	  (org-test-with-parsed-data "- item\n\n  para"
	    (org-element-map tree 'paragraph
	      (lambda (h) (if (org-export-first-sibling-p h info) 'yes 'no))
	      info))))
  ;; Ignore sections for headlines.
  (should
   (equal '(yes yes)
	  (org-test-with-parsed-data "* H\nSection\n** H 2"
	    (org-element-map tree 'headline
	      (lambda (h) (if (org-export-first-sibling-p h info) 'yes 'no))
	      info))))
  ;; Ignore headlines not exported.
  (should
   (equal
    '(yes)
    (let ((org-export-exclude-tags '("ignore")))
      (org-test-with-parsed-data "* Headline :ignore:\n* Headline 2"
	(org-element-map tree 'headline
	  (lambda (h) (if (org-export-first-sibling-p h info) 'yes 'no))
	  info))))))

(ert-deftest test-org-export/last-sibling-p ()
  "Test `org-export-last-sibling-p' specifications."
  ;; Standard test.
  (should
   (equal
    '(yes no yes)
    (org-test-with-parsed-data "* Headline\n** Headline 2\n** Headline 3"
      (org-element-map tree 'headline
	(lambda (h) (if (org-export-last-sibling-p h info) 'yes 'no))
	info))))
  (should
   (equal '(no yes)
	  (org-test-with-parsed-data "- item\n\n  para"
	    (org-element-map tree 'paragraph
	      (lambda (h) (if (org-export-last-sibling-p h info) 'yes 'no))
	      info))))
  ;; Ignore headlines not exported.
  (should
   (equal
    '(yes)
    (let ((org-export-exclude-tags '("ignore")))
      (org-test-with-parsed-data "* Headline\n* Headline 2 :ignore:"
	(org-element-map tree 'headline
	  (lambda (h) (if (org-export-last-sibling-p h info) 'yes 'no))
	  info))))))

(ert-deftest test-org-export/handle-inlinetasks ()
  "Test inlinetask export."
  ;; Inlinetask with an exclude tag.
  (when (featurep 'org-inlinetask)
    (should
     (equal
      ""
      (let ((org-inlinetask-min-level 3)
	    org-export-filter-body-functions
	    org-export-filter-final-output-functions)
	(org-test-with-temp-text "*** Inlinetask :noexp:\nContents\n*** end"
	  (org-export-as (org-test-default-backend)
			 nil nil nil '(:exclude-tags ("noexp")))))))
    ;; Inlinetask with an include tag.
    (should
     (equal
      "* H2\n*** Inline :exp:\n"
      (let ((org-inlinetask-min-level 3)
	    (org-tags-column 0))
	(org-test-with-temp-text "* H1\n* H2\n*** Inline :exp:"
	  (org-export-as (org-test-default-backend)
			 nil nil nil '(:select-tags ("exp")))))))
    ;; Ignore inlinetask with a TODO keyword and tasks excluded.
    (should
     (equal ""
	    (let ((org-todo-keywords '((sequence "TODO" "DONE")))
		  (org-inlinetask-min-level 3)
		  org-export-filter-body-functions
		  org-export-filter-final-output-functions)
	      (org-test-with-temp-text "*** TODO Inline"
		(org-export-as (org-test-default-backend)
			       nil nil nil '(:with-tasks nil))))))))



;;; Keywords

(ert-deftest test-org-export/get-date ()
  "Test `org-export-get-date' specifications."
  ;; Return a properly formatted string when
  ;; `org-export-date-timestamp-format' is non-nil and DATE keyword
  ;; consists in a single timestamp.
  (should
   (equal "29 03 2012"
	  (let ((org-export-date-timestamp-format "%d %m %Y"))
	    (org-test-with-parsed-data "#+DATE: <2012-03-29 Thu>"
	      (org-export-get-date info)))))
  ;; Return a secondary string otherwise.
  (should-not
   (stringp
    (let ((org-export-date-timestamp-format nil))
      (org-test-with-parsed-data "#+DATE: <2012-03-29 Thu>"
	(org-export-get-date info)))))
  (should
   (equal '("Date")
	  (org-test-with-parsed-data "#+DATE: Date"
	    (org-export-get-date info))))
  ;; Optional argument has precedence over
  ;; `org-export-date-timestamp-format'.
  (should
   (equal "29 03"
	  (let ((org-export-date-timestamp-format "%d %m %Y"))
	    (org-test-with-parsed-data "#+DATE: <2012-03-29 Thu>"
	      (org-export-get-date info "%d %m"))))))



;;; Links

(ert-deftest test-org-export/custom-protocol-maybe ()
  "Test `org-export-custom-protocol-maybe' specifications."
  (should
   (string-match
    "success"
    (let ((org-link-types (copy-sequence org-link-types)))
      (org-add-link-type "foo" nil (lambda (p d f) "success"))
      (org-export-string-as
       "[[foo:path]]"
       (org-export-create-backend
	:name 'test
	:transcoders
	'((section . (lambda (s c i) c))
	  (paragraph . (lambda (p c i) c))
	  (link . (lambda (l c i)
		    (or (org-export-custom-protocol-maybe l c 'test)
			"failure")))))))))
  (should-not
   (string-match
    "success"
    (let ((org-link-types (copy-sequence org-link-types)))
      (org-add-link-type
       "foo" nil (lambda (p d f) (and (eq f 'test) "success")))
      (org-export-string-as
       "[[foo:path]]"
       (org-export-create-backend
	:name 'no-test
	:transcoders
	'((section . (lambda (s c i) c))
	  (paragraph . (lambda (p c i) c))
	  (link . (lambda (l c i)
		    (or (org-export-custom-protocol-maybe l c 'no-test)
			"failure")))))))))
  ;; Ignore anonymous back-ends.
  (should-not
   (string-match
    "success"
    (let ((org-link-types (copy-sequence org-link-types)))
      (org-add-link-type
       "foo" nil (lambda (p d f) (and (eq f 'test) "success")))
      (org-export-string-as
       "[[foo:path]]"
       (org-export-create-backend
	:transcoders
	'((section . (lambda (s c i) c))
	  (paragraph . (lambda (p c i) c))
	  (link . (lambda (l c i)
		    (or (org-export-custom-protocol-maybe l c nil)
			"failure"))))))))))

(ert-deftest test-org-export/get-coderef-format ()
  "Test `org-export-get-coderef-format' specifications."
  ;; A link without description returns "%s"
  (should (equal (org-export-get-coderef-format "(ref:line)" nil)
		 "%s"))
  ;; Return "%s" when path is matched within description.
  (should (equal (org-export-get-coderef-format "path" "desc (path)")
		 "desc %s"))
  ;; Otherwise return description.
  (should (equal (org-export-get-coderef-format "path" "desc")
		 "desc")))

(ert-deftest test-org-export/inline-image-p ()
  "Test `org-export-inline-image-p' specifications."
  (should
   (org-export-inline-image-p
    (org-test-with-temp-text "[[#id]]"
      (org-element-map (org-element-parse-buffer) 'link 'identity nil t))
    '(("custom-id" . "id")))))

(ert-deftest test-org-export/fuzzy-link ()
  "Test fuzzy links specifications."
  ;; Link to an headline should return headline's number.
  (org-test-with-parsed-data
      "Paragraph.\n* Head1\n* Head2\n* Head3\n[[Head2]]"
    (should
     ;; Note: Headline's number is in fact a list of numbers.
     (equal '(2)
	    (org-element-map tree 'link
	      (lambda (link)
		(org-export-get-ordinal
		 (org-export-resolve-fuzzy-link link info) info)) info t))))
  ;; Link to a target in an item should return item's number.
  (org-test-with-parsed-data
      "- Item1\n  - Item11\n  - <<test>>Item12\n- Item2\n\n\n[[test]]"
    (should
     ;; Note: Item's number is in fact a list of numbers.
     (equal '(1 2)
	    (org-element-map tree 'link
	      (lambda (link)
		(org-export-get-ordinal
		 (org-export-resolve-fuzzy-link link info) info)) info t))))
  ;; Link to a target in a footnote should return footnote's number.
  (org-test-with-parsed-data "
Paragraph[1][2][fn:lbl3:C<<target>>][[test]][[target]]\n[1] A\n\n[2] <<test>>B"
    (should
     (equal '(2 3)
	    (org-element-map tree 'link
	      (lambda (link)
		(org-export-get-ordinal
		 (org-export-resolve-fuzzy-link link info) info)) info))))
  ;; Link to a named element should return sequence number of that
  ;; element.
  (org-test-with-parsed-data
      "#+NAME: tbl1\n|1|2|\n#+NAME: tbl2\n|3|4|\n#+NAME: tbl3\n|5|6|\n[[tbl2]]"
    (should
     (= 2
	(org-element-map tree 'link
	  (lambda (link)
	    (org-export-get-ordinal
	     (org-export-resolve-fuzzy-link link info) info)) info t))))
  ;; Link to a target not within an item, a table, a footnote
  ;; reference or definition should return section number.
  (org-test-with-parsed-data
      "* Head1\n* Head2\nParagraph<<target>>\n* Head3\n[[target]]"
    (should
     (equal '(2)
	    (org-element-map tree 'link
	      (lambda (link)
		(org-export-get-ordinal
		 (org-export-resolve-fuzzy-link link info) info)) info t))))
  ;; Space are not significant when matching a fuzzy link.
  (should
   (org-test-with-parsed-data "* Head 1\n[[Head\n  1]]"
     (org-element-map tree 'link
       (lambda (link) (org-export-resolve-fuzzy-link link info))
       info t)))
  ;; Statistics cookies are ignored for headline match.
  (should
   (org-test-with-parsed-data "* Head [0/0]\n[[Head]]"
     (org-element-map tree 'link
       (lambda (link) (org-export-resolve-fuzzy-link link info))
       info t)))
  (should
   (org-test-with-parsed-data "* Head [100%]\n[[Head]]"
     (org-element-map tree 'link
       (lambda (link) (org-export-resolve-fuzzy-link link info))
       info t))))

(ert-deftest test-org-export/resolve-coderef ()
  "Test `org-export-resolve-coderef' specifications."
  (let ((org-coderef-label-format "(ref:%s)"))
    ;; A link to a "-n -k -r" block returns line number.
    (should
     (= 1
	(org-test-with-parsed-data
	    "#+BEGIN_EXAMPLE -n -k -r\nText (ref:coderef)\n#+END_EXAMPLE"
	  (org-export-resolve-coderef "coderef" info))))
    (should
     (= 1
	(org-test-with-parsed-data
	    "#+BEGIN_SRC emacs-lisp -n -k -r\n(+ 1 1) (ref:coderef)\n#+END_SRC"
	  (org-export-resolve-coderef "coderef" info))))
    ;; A link to a "-n -r" block returns line number.
    (should
     (= 1
	(org-test-with-parsed-data
	    "#+BEGIN_EXAMPLE -n -r\nText (ref:coderef)\n#+END_EXAMPLE"
	  (org-export-resolve-coderef "coderef" info))))
    (should
     (= 1
	(org-test-with-parsed-data
	    "#+BEGIN_SRC emacs-lisp -n -r\n(+ 1 1) (ref:coderef)\n#+END_SRC"
	  (org-export-resolve-coderef "coderef" info))))
    ;; A link to a "-n" block returns coderef.
    (should
     (equal "coderef"
	    (org-test-with-parsed-data
		"#+BEGIN_SRC emacs-lisp -n\n(+ 1 1) (ref:coderef)\n#+END_SRC"
	      (org-export-resolve-coderef "coderef" info))))
    (should
     (equal "coderef"
	    (org-test-with-parsed-data
		"#+BEGIN_EXAMPLE -n\nText (ref:coderef)\n#+END_EXAMPLE"
	      (org-export-resolve-coderef "coderef" info))))
    ;; A link to a "-r" block returns line number.
    (should
     (= 1
	(org-test-with-parsed-data
	    "#+BEGIN_SRC emacs-lisp -r\n(+ 1 1) (ref:coderef)\n#+END_SRC"
	  (org-export-resolve-coderef "coderef" info))))
    (should
     (= 1
	(org-test-with-parsed-data
	    "#+BEGIN_EXAMPLE -r\nText (ref:coderef)\n#+END_EXAMPLE"
	  (org-export-resolve-coderef "coderef" info))))
    ;; A link to a block without a switch returns coderef.
    (should
     (equal "coderef"
	    (org-test-with-parsed-data
		"#+BEGIN_SRC emacs-lisp\n(+ 1 1) (ref:coderef)\n#+END_SRC"
	      (org-export-resolve-coderef "coderef" info))))
    (org-test-with-parsed-data
	"#+BEGIN_EXAMPLE\nText (ref:coderef)\n#+END_EXAMPLE"
      (should (equal (org-export-resolve-coderef "coderef" info) "coderef")))
    ;; Correctly handle continued line numbers.  A "+n" switch should
    ;; resume numbering from previous block with numbered lines,
    ;; ignoring blocks not numbering lines in the process.  A "-n"
    ;; switch resets count.
    (should
     (equal '(2 1)
	    (org-test-with-parsed-data "
#+BEGIN_EXAMPLE -n
Text.
#+END_EXAMPLE

#+BEGIN_SRC emacs-lisp
\(- 1 1)
#+END_SRC

#+BEGIN_SRC emacs-lisp +n -r
\(+ 1 1) (ref:addition)
#+END_SRC

#+BEGIN_EXAMPLE -n -r
Another text. (ref:text)
#+END_EXAMPLE"
	      (list (org-export-resolve-coderef "addition" info)
		    (org-export-resolve-coderef "text" info)))))
    ;; Recognize coderef with user-specified syntax.
    (should
     (equal "text"
	    (org-test-with-parsed-data
		"#+BEGIN_EXAMPLE -l \"[ref:%s]\"\nText. [ref:text]\n#+END_EXAMPLE"
	      (org-export-resolve-coderef "text" info))))
    ;; Unresolved coderefs throw an error.
    (should-error
     (org-test-with-parsed-data "#+BEGIN_SRC emacs-lisp\n(+ 1 1)\n#+END_SRC"
       (org-export-resolve-coderef "unknown" info)))))

(ert-deftest test-org-export/resolve-fuzzy-link ()
  "Test `org-export-resolve-fuzzy-link' specifications."
  ;; Match target objects.
  (should
   (org-test-with-parsed-data "<<target>> [[target]]"
     (org-export-resolve-fuzzy-link
      (org-element-map tree 'link 'identity info t) info)))
  ;; Match named elements.
  (should
   (org-test-with-parsed-data "#+NAME: target\nParagraph\n\n[[target]]"
     (org-export-resolve-fuzzy-link
      (org-element-map tree 'link 'identity info t) info)))
  ;; Match exact headline's name.
  (should
   (org-test-with-parsed-data "* My headline\n[[My headline]]"
     (org-export-resolve-fuzzy-link
      (org-element-map tree 'link 'identity info t) info)))
  ;; Targets objects have priority over named elements and headline
  ;; titles.
  (should
   (eq 'target
       (org-test-with-parsed-data
	   "* target\n#+NAME: target\n<<target>>\n\n[[target]]"
	 (org-element-type
	  (org-export-resolve-fuzzy-link
	   (org-element-map tree 'link 'identity info t) info)))))
  ;; Named elements have priority over headline titles.
  (should
   (eq 'paragraph
       (org-test-with-parsed-data
	   "* target\n#+NAME: target\nParagraph\n\n[[target]]"
	 (org-element-type
	  (org-export-resolve-fuzzy-link
	   (org-element-map tree 'link 'identity info t) info)))))
  ;; If link's path starts with a "*", only match headline titles,
  ;; though.
  (should
   (eq 'headline
       (org-test-with-parsed-data
	   "* target\n#+NAME: target\n<<target>>\n\n[[*target]]"
	 (org-element-type
	  (org-export-resolve-fuzzy-link
	   (org-element-map tree 'link 'identity info t) info)))))
  ;; Error if no match.
  (should-error
   (org-test-with-parsed-data "[[target]]"
     (org-export-resolve-fuzzy-link
      (org-element-map tree 'link 'identity info t) info)))
  ;; Match fuzzy link even when before first headline.
  (should
   (eq 'headline
       (org-test-with-parsed-data "[[hl]]\n* hl"
	 (org-element-type
	  (org-export-resolve-fuzzy-link
	   (org-element-map tree 'link 'identity info t) info)))))
  ;; Handle url-encoded fuzzy links.
  (should
   (org-test-with-parsed-data "* A B\n[[A%20B]]"
     (org-export-resolve-fuzzy-link
      (org-element-map tree 'link #'identity info t) info))))

(ert-deftest test-org-export/resolve-id-link ()
  "Test `org-export-resolve-id-link' specifications."
  ;; Regular test for custom-id link.
  (should
   (equal '("Headline1")
	  (org-test-with-parsed-data "* Headline1
:PROPERTIES:
:CUSTOM_ID: test
:END:
* Headline 2
\[[#test]]"
	    (org-element-property
	     :title
	     (org-export-resolve-id-link
	      (org-element-map tree 'link 'identity info t) info)))))
  ;; Throw an error on failing searches.
  (should-error
   (org-test-with-parsed-data "* Headline1
:PROPERTIES:
:CUSTOM_ID: test
:END:
* Headline 2
\[[#no-match]]"
     (org-export-resolve-id-link
      (org-element-map tree 'link 'identity info t) info)))
  ;; Test for internal id target.
  (should
   (equal '("Headline1")
	  (org-test-with-parsed-data "* Headline1
:PROPERTIES:
:ID: aaaa
:END:
* Headline 2
\[[id:aaaa]]"
	    (org-element-property
	     :title
	     (org-export-resolve-id-link
	      (org-element-map tree 'link 'identity info t) info)))))
  ;; Test for external id target.
  (should
   (equal
    "external-file"
    (org-test-with-parsed-data "[[id:aaaa]]"
      (org-export-resolve-id-link
       (org-element-map tree 'link 'identity info t)
       (org-combine-plists info '(:id-alist (("aaaa" . "external-file")))))))))

(ert-deftest test-org-export/resolve-radio-link ()
  "Test `org-export-resolve-radio-link' specifications."
  ;; Standard test.
  (should
   (org-test-with-temp-text "<<<radio>>> radio"
     (org-update-radio-target-regexp)
     (let* ((tree (org-element-parse-buffer))
	    (info `(:parse-tree ,tree)))
       (org-export-resolve-radio-link
	(org-element-map tree 'link 'identity info t)
	info))))
  ;; Radio targets are case-insensitive.
  (should
   (org-test-with-temp-text "<<<RADIO>>> radio"
     (org-update-radio-target-regexp)
     (let* ((tree (org-element-parse-buffer))
	    (info `(:parse-tree ,tree)))
       (org-export-resolve-radio-link
	(org-element-map tree 'link 'identity info t)
	info))))
  ;; Radio target with objects.
  (should
   (org-test-with-temp-text "<<<radio \\alpha>>> radio \\alpha"
     (org-update-radio-target-regexp)
     (let* ((tree (org-element-parse-buffer))
	    (info `(:parse-tree ,tree)))
       (org-export-resolve-radio-link
	(org-element-map tree 'link 'identity info t)
	info))))
  ;; Radio target with objects at its beginning.
  (should
   (org-test-with-temp-text "<<<\\alpha radio>>> \\alpha radio"
     (org-update-radio-target-regexp)
     (let* ((tree (org-element-parse-buffer))
	    (info `(:parse-tree ,tree)))
       (org-export-resolve-radio-link
	(org-element-map tree 'link 'identity info t)
	info))))
  ;; Radio link next to an apostrophe.
  (should
   (org-test-with-temp-text "<<<radio>>> radio's"
     (org-update-radio-target-regexp)
     (let* ((tree (org-element-parse-buffer))
	    (info `(:parse-tree ,tree)))
       (org-export-resolve-radio-link
	(org-element-map tree 'link 'identity info t)
	info))))
  ;; Multiple radio targets.
  (should
   (equal '("radio1" "radio2")
	  (org-test-with-temp-text "<<<radio1>>> <<<radio2>>> radio1 radio2"
	    (org-update-radio-target-regexp)
	    (let* ((tree (org-element-parse-buffer))
		   (info `(:parse-tree ,tree)))
	      (org-element-map tree 'link
		(lambda (link)
		  (org-element-property
		   :value (org-export-resolve-radio-link link info)))
		info)))))
  ;; Radio target is whitespace insensitive.
  (should
   (org-test-with-temp-text "<<<a radio>>> a\n  radio"
     (org-update-radio-target-regexp)
     (let* ((tree (org-element-parse-buffer))
	    (info `(:parse-tree ,tree)))
       (org-element-map tree 'link
	 (lambda (link) (org-export-resolve-radio-link link info)) info t)))))

(ert-deftest test-org-export/file-uri ()
  "Test `org-export-file-uri' specifications."
  ;; Preserve relative filenames.
  (should (equal "relative.org" (org-export-file-uri "relative.org")))
  ;; Local files start with "file:///"
  (should (equal "file:///local.org" (org-export-file-uri "/local.org")))
  ;; Remote files start with "file://"
  (should (equal "file://myself@some.where:papers/last.pdf"
		 (org-export-file-uri "/myself@some.where:papers/last.pdf")))
  (should (equal "file://localhost/etc/fstab"
		 (org-export-file-uri "//localhost/etc/fstab")))
  ;; Expand filename starting with "~".
  (should (equal (org-export-file-uri "~/file.org")
		 (concat "file://" (expand-file-name "~/file.org")))))



;;; Src-block and example-block

(ert-deftest test-org-export/unravel-code ()
  "Test `org-export-unravel-code' function."
  ;; Code without reference.
  (should
   (equal '("(+ 1 1)\n")
	  (org-test-with-temp-text "#+BEGIN_EXAMPLE\n(+ 1 1)\n#+END_EXAMPLE"
	    (org-export-unravel-code (org-element-at-point)))))
  ;; Code with reference.
  (should
   (equal '("(+ 1 1)\n" (1 . "test"))
	  (org-test-with-temp-text
	      "#+BEGIN_EXAMPLE\n(+ 1 1) (ref:test)\n#+END_EXAMPLE"
	    (let  ((org-coderef-label-format "(ref:%s)"))
	      (org-export-unravel-code (org-element-at-point))))))
  ;; Code with user-defined reference.
  (should
   (equal
    '("(+ 1 1)\n" (1 . "test"))
    (org-test-with-temp-text
	"#+BEGIN_EXAMPLE -l \"[ref:%s]\"\n(+ 1 1) [ref:test]\n#+END_EXAMPLE"
      (let ((org-coderef-label-format "(ref:%s)"))
	(org-export-unravel-code (org-element-at-point))))))
  ;; Code references keys are relative to the current block.
  (should
   (equal '("(+ 2 2)\n(+ 3 3)\n" (2 . "one"))
	  (org-test-with-temp-text "
#+BEGIN_EXAMPLE -n
\(+ 1 1)
#+END_EXAMPLE
#+BEGIN_EXAMPLE +n
\(+ 2 2)
\(+ 3 3) (ref:one)
#+END_EXAMPLE"
	    (goto-line 5)
	    (let ((org-coderef-label-format "(ref:%s)"))
	      (org-export-unravel-code (org-element-at-point)))))))

(ert-deftest test-org-export/format-code-default ()
  "Test `org-export-format-code-default' specifications."
  ;; Return the empty string when code is empty.
  (should
   (equal ""
	  (org-test-with-parsed-data "#+BEGIN_SRC emacs-lisp\n\n\n#+END_SRC"
	    (org-export-format-code-default
	     (org-element-map tree 'src-block 'identity info t) info))))
  ;; Number lines, two whitespace characters before the actual loc.
  (should
   (equal "1  a\n2  b\n"
	  (org-test-with-parsed-data
	      "#+BEGIN_SRC emacs-lisp +n\na\nb\n#+END_SRC"
	    (org-export-format-code-default
	     (org-element-map tree 'src-block 'identity info t) info))))
  ;; Put references 6 whitespace characters after the widest line,
  ;; wrapped within parenthesis.
  (should
   (equal "123      (a)\n1        (b)\n"
	  (let ((org-coderef-label-format "(ref:%s)"))
	    (org-test-with-parsed-data
		"#+BEGIN_SRC emacs-lisp\n123 (ref:a)\n1 (ref:b)\n#+END_SRC"
	      (org-export-format-code-default
	       (org-element-map tree 'src-block 'identity info t) info))))))



;;; Smart Quotes

(ert-deftest test-org-export/activate-smart-quotes ()
  "Test `org-export-activate-smart-quotes' specifications."
  ;; Double quotes: standard test.
  (should
   (equal
    '("some &ldquo;quoted&rdquo; text")
    (let ((org-export-default-language "en"))
      (org-test-with-parsed-data "some \"quoted\" text"
	(org-element-map tree 'plain-text
	  (lambda (s) (org-export-activate-smart-quotes s :html info))
	  info)))))
  ;; Opening quotes: at the beginning of a paragraph.
  (should
   (equal
    '("&ldquo;begin")
    (let ((org-export-default-language "en"))
      (org-test-with-parsed-data "\"begin"
	(org-element-map tree 'plain-text
	  (lambda (s) (org-export-activate-smart-quotes s :html info))
	  info)))))
  ;; Opening quotes: after an object.
  (should
   (equal
    '("&ldquo;quoted&rdquo; text")
    (let ((org-export-default-language "en"))
      (org-test-with-parsed-data "=verb= \"quoted\" text"
	(org-element-map tree 'plain-text
	  (lambda (s) (org-export-activate-smart-quotes s :html info))
	  info)))))
  ;; Closing quotes: at the end of a paragraph.
  (should
   (equal
    '("Quoted &ldquo;text&rdquo;")
    (let ((org-export-default-language "en"))
      (org-test-with-parsed-data "Quoted \"text\""
	(org-element-map tree 'plain-text
	  (lambda (s) (org-export-activate-smart-quotes s :html info))
	  info)))))
  ;; Inner quotes: standard test.
  (should
   (equal '("« outer « inner » outer »")
	  (let ((org-export-default-language "fr"))
	    (org-test-with-parsed-data "\"outer 'inner' outer\""
	      (org-element-map tree 'plain-text
		(lambda (s) (org-export-activate-smart-quotes s :utf-8 info))
		info)))))
  ;; Inner quotes: close to special symbols.
  (should
   (equal '("« outer (« inner ») outer »")
	  (let ((org-export-default-language "fr"))
	    (org-test-with-parsed-data "\"outer ('inner') outer\""
	      (org-element-map tree 'plain-text
		(lambda (s) (org-export-activate-smart-quotes s :utf-8 info))
		info)))))
  (should
   (equal '("« « inner » »")
	  (let ((org-export-default-language "fr"))
	    (org-test-with-parsed-data "\"'inner'\""
	      (org-element-map tree 'plain-text
		(lambda (s) (org-export-activate-smart-quotes s :utf-8 info))
		info)))))
  ;; Apostrophe: standard test.
  (should
   (equal '("It « shouldn’t » fail")
	  (let ((org-export-default-language "fr"))
	    (org-test-with-parsed-data "It \"shouldn't\" fail"
	      (org-element-map tree 'plain-text
		(lambda (s) (org-export-activate-smart-quotes s :utf-8 info))
		info)))))
  (should
   (equal '("It shouldn’t fail")
	  (let ((org-export-default-language "fr"))
	    (org-test-with-parsed-data "It shouldn't fail"
	      (org-element-map tree 'plain-text
		(lambda (s) (org-export-activate-smart-quotes s :utf-8 info))
		info)))))
  ;; Apostrophe: before an object.
  (should
   (equal
    '("« a’" " »")
    (let ((org-export-default-language "fr"))
      (org-test-with-parsed-data "\"a'=b=\""
	(org-element-map tree 'plain-text
	  (lambda (s) (org-export-activate-smart-quotes s :utf-8 info))
	  info)))))
  ;; Apostrophe: after an object.
  (should
   (equal '("« " "’s »")
	  (let ((org-export-default-language "fr"))
	    (org-test-with-parsed-data "\"=code='s\""
	      (org-element-map tree 'plain-text
		(lambda (s) (org-export-activate-smart-quotes s :utf-8 info))
		info)))))
  ;; Special case: isolated quotes.
  (should
   (equal '("&ldquo;" "&rdquo;")
	  (let ((org-export-default-language "en"))
	    (org-test-with-parsed-data "\"$x$\""
	      (org-element-map tree 'plain-text
		(lambda (s) (org-export-activate-smart-quotes s :html info))
		info)))))
  ;; Smart quotes in secondary strings.
  (should
   (equal '("&ldquo;" "&rdquo;")
	  (let ((org-export-default-language "en"))
	    (org-test-with-parsed-data "* \"$x$\""
	      (org-element-map tree 'plain-text
		(lambda (s) (org-export-activate-smart-quotes s :html info))
		info)))))
  ;; Smart quotes in document keywords.
  (should
   (equal '("&ldquo;" "&rdquo;")
	  (let ((org-export-default-language "en"))
	    (org-test-with-parsed-data "#+TITLE: \"$x$\""
	      (org-element-map (plist-get info :title) 'plain-text
		(lambda (s) (org-export-activate-smart-quotes s :html info))
		info)))))
  ;; Smart quotes in parsed affiliated keywords.
  (should
   (equal '("&ldquo;" "&rdquo;" "Paragraph")
	  (let ((org-export-default-language "en"))
	    (org-test-with-parsed-data "#+CAPTION: \"$x$\"\nParagraph"
	      (org-element-map tree 'plain-text
		(lambda (s) (org-export-activate-smart-quotes s :html info))
		info nil nil t))))))



;;; Tables

(ert-deftest test-org-export/special-column ()
  "Test if the table's special column is properly recognized."
  ;; 1. First column is special if it contains only a special marking
  ;;    characters or empty cells.
  (org-test-with-temp-text "
| ! | 1 |
|   | 2 |"
    (should
     (org-export-table-has-special-column-p
      (org-element-map
	  (org-element-parse-buffer) 'table 'identity nil 'first-match))))
  ;; 2. If the column contains anything else, it isn't special.
  (org-test-with-temp-text "
| ! | 1 |
| b | 2 |"
    (should-not
     (org-export-table-has-special-column-p
      (org-element-map
	  (org-element-parse-buffer) 'table 'identity nil 'first-match))))
  ;; 3. Special marking characters are "#", "^", "*", "_", "/", "$"
  ;;    and "!".
  (org-test-with-temp-text "
| # | 1 |
| ^ | 2 |
| * | 3 |
| _ | 4 |
| / | 5 |
| $ | 6 |
| ! | 7 |"
    (should
     (org-export-table-has-special-column-p
      (org-element-map
	  (org-element-parse-buffer) 'table 'identity nil 'first-match))))
  ;; 4. A first column with only empty cells isn't considered as
  ;;    special.
  (org-test-with-temp-text "
|   | 1 |
|   | 2 |"
    (should-not
     (org-export-table-has-special-column-p
      (org-element-map
	  (org-element-parse-buffer) 'table 'identity nil 'first-match)))))

(ert-deftest test-org-export/table-row-is-special-p ()
  "Test `org-export-table-row-is-special-p' specifications."
  ;; 1. A row is special if it has a special marking character in the
  ;;    special column.
  (org-test-with-parsed-data "| ! | 1 |"
    (should
     (org-export-table-row-is-special-p
      (org-element-map tree 'table-row 'identity nil 'first-match) info)))
  ;; 2. A row is special when its first field is "/"
  (org-test-with-parsed-data "
| / | 1 |
| a | b |"
    (should
     (org-export-table-row-is-special-p
      (org-element-map tree 'table-row 'identity nil 'first-match) info)))
  ;; 3. A row only containing alignment cookies is also considered as
  ;;    special.
  (org-test-with-parsed-data "| <5> |   | <l> | <l22> |"
    (should
     (org-export-table-row-is-special-p
      (org-element-map tree 'table-row 'identity nil 'first-match) info)))
  ;; 4. Everything else isn't considered as special.
  (org-test-with-parsed-data "| \alpha |   | c |"
    (should-not
     (org-export-table-row-is-special-p
      (org-element-map tree 'table-row 'identity nil 'first-match) info)))
  ;; 5. Table's rules are never considered as special rows.
  (org-test-with-parsed-data "|---+---|"
    (should-not
     (org-export-table-row-is-special-p
      (org-element-map tree 'table-row 'identity nil 'first-match) info))))

(ert-deftest test-org-export/has-header-p ()
  "Test `org-export-table-has-header-p' specifications."
  ;; 1. With an header.
  (org-test-with-parsed-data "
| a | b |
|---+---|
| c | d |"
    (should
     (org-export-table-has-header-p
      (org-element-map tree 'table 'identity info 'first-match)
      info)))
  ;; 2. Without an header.
  (org-test-with-parsed-data "
| a | b |
| c | d |"
    (should-not
     (org-export-table-has-header-p
      (org-element-map tree 'table 'identity info 'first-match)
      info)))
  ;; 3. Don't get fooled with starting and ending rules.
  (org-test-with-parsed-data "
|---+---|
| a | b |
| c | d |
|---+---|"
    (should-not
     (org-export-table-has-header-p
      (org-element-map tree 'table 'identity info 'first-match)
      info))))

(ert-deftest test-org-export/table-row-group ()
  "Test `org-export-table-row-group' specifications."
  ;; 1. A rule creates a new group.
  (should
   (equal '(1 rule 2)
	  (org-test-with-parsed-data "
| a | b |
|---+---|
| 1 | 2 |"
	    (org-element-map tree 'table-row
	      (lambda (row)
		(if (eq (org-element-property :type row) 'rule) 'rule
		  (org-export-table-row-group row info)))))))
  ;; 2. Special rows are ignored in count.
  (should
   (equal
    '(rule 1)
    (org-test-with-parsed-data "
| / | < | > |
|---|---+---|
|   | 1 | 2 |"
      (org-element-map tree 'table-row
	(lambda (row)
	  (if (eq (org-element-property :type row) 'rule) 'rule
	    (org-export-table-row-group row info)))
	info))))
  ;; 3. Double rules also are ignored in count.
  (should
   (equal '(1 rule rule 2)
	  (org-test-with-parsed-data "
| a | b |
|---+---|
|---+---|
| 1 | 2 |"
	    (org-element-map tree 'table-row
	      (lambda (row)
		(if (eq (org-element-property :type row) 'rule) 'rule
		  (org-export-table-row-group row info))))))))

(ert-deftest test-org-export/table-row-number ()
  "Test `org-export-table-row-number' specifications."
  ;; Standard test.  Number is 0-indexed.
  (should
   (equal '(0 1)
	  (org-test-with-parsed-data "| a | b | c |\n| d | e | f |"
	    (org-element-map tree 'table-row
	      (lambda (row) (org-export-table-row-number row info)) info))))
  ;; Number ignores separators.
  (should
   (equal '(0 1)
	  (org-test-with-parsed-data "
| a | b | c |
|---+---+---|
| d | e | f |"
	    (org-element-map tree 'table-row
	      (lambda (row) (org-export-table-row-number row info)) info))))
  ;; Number ignores special rows.
  (should
   (equal '(0 1)
	  (org-test-with-parsed-data "
| / | <   | >   |
|   | b   | c   |
|---+-----+-----|
|   | <c> | <c> |
|   | e   | f   |"
	    (org-element-map tree 'table-row
	      (lambda (row) (org-export-table-row-number row info)) info)))))

(ert-deftest test-org-export/table-cell-width ()
  "Test `org-export-table-cell-width' specifications."
  ;; 1. Width is primarily determined by width cookies.  If no cookie
  ;;    is found, cell's width is nil.
  (org-test-with-parsed-data "
| / | <l> | <6> | <l7> |
|   |  a  |  b  |  c   |"
    (should
     (equal
      '(nil 6 7)
      (mapcar (lambda (cell) (org-export-table-cell-width cell info))
	      (org-element-map tree 'table-cell 'identity info)))))
  ;; 2. The last width cookie has precedence.
  (org-test-with-parsed-data "
| <6> |
| <7> |
|  a  |"
    (should
     (equal
      '(7)
      (mapcar (lambda (cell) (org-export-table-cell-width cell info))
	      (org-element-map tree 'table-cell 'identity info)))))
  ;; 3. Valid width cookies must have a specific row.
  (org-test-with-parsed-data "| <6> | cell |"
    (should
     (equal
      '(nil nil)
      (mapcar (lambda (cell) (org-export-table-cell-width cell info))
	      (org-element-map tree 'table-cell 'identity))))))

(ert-deftest test-org-export/table-cell-alignment ()
  "Test `org-export-table-cell-alignment' specifications."
  ;; 1. Alignment is primarily determined by alignment cookies.
  (should
   (equal '(left center right)
	  (let ((org-table-number-fraction 0.5)
		(org-table-number-regexp "^[0-9]+$"))
	    (org-test-with-parsed-data "| <l> | <c> | <r> |"
	      (mapcar (lambda (cell)
			(org-export-table-cell-alignment cell info))
		      (org-element-map tree 'table-cell 'identity))))))
  ;; 2. The last alignment cookie has precedence.
  (should
   (equal '(right right right)
	  (org-test-with-parsed-data "
| <l8> |
| cell |
| <r9> |"
	    (mapcar (lambda (cell) (org-export-table-cell-alignment cell info))
		    (org-element-map tree 'table-cell 'identity)))))
  ;; 3. If there's no cookie, cell's contents determine alignment.
  ;;    A column mostly made of cells containing numbers will align
  ;;    its cells to the right.
  (should
   (equal '(right right right)
	  (let ((org-table-number-fraction 0.5)
		(org-table-number-regexp "^[0-9]+$"))
	    (org-test-with-parsed-data "
| 123       |
| some text |
| 12345     |"
	      (mapcar (lambda (cell)
			(org-export-table-cell-alignment cell info))
		      (org-element-map tree 'table-cell 'identity))))))
  ;; 4. Otherwise, they will be aligned to the left.
  (should
   (equal '(left left left)
	  (org-test-with-parsed-data "
| text      |
| some text |
| \alpha    |"
	    (mapcar (lambda (cell)
		      (org-export-table-cell-alignment cell info))
		    (org-element-map tree 'table-cell 'identity info))))))

(ert-deftest test-org-export/table-cell-borders ()
  "Test `org-export-table-cell-borders' specifications."
  ;; 1. Recognize various column groups indicators.
  (org-test-with-parsed-data "| / | < | > | <> |"
    (should
     (equal
      '((right bottom top) (left bottom top) (right bottom top)
	(right left bottom top))
      (mapcar (lambda (cell)
		(org-export-table-cell-borders cell info))
	      (org-element-map tree 'table-cell 'identity)))))
  ;; 2. Accept shortcuts to define column groups.
  (org-test-with-parsed-data "| / | < | < |"
    (should
     (equal
      '((right bottom top) (right left bottom top) (left bottom top))
      (mapcar (lambda (cell)
		(org-export-table-cell-borders cell info))
	      (org-element-map tree 'table-cell 'identity)))))
  ;; 3. A valid column groups row must start with a "/".
  (org-test-with-parsed-data "
|   | < |
| a | b |"
    (should
     (equal '((top) (top) (bottom) (bottom))
	    (mapcar (lambda (cell)
		      (org-export-table-cell-borders cell info))
		    (org-element-map tree 'table-cell 'identity)))))
  ;; 4. Take table rules into consideration.
  (org-test-with-parsed-data "
| 1 |
|---|
| 2 |"
    (should
     (equal '((below top) (bottom above))
	    (mapcar (lambda (cell)
		      (org-export-table-cell-borders cell info))
		    (org-element-map tree 'table-cell 'identity)))))
  ;; 5. Top and (resp. bottom) rules induce both `top' and `above'
  ;;    (resp. `bottom' and `below') borders.  Any special row is
  ;;    ignored.
  (org-test-with-parsed-data "
|---+----|
| / |    |
|   |  1 |
|---+----|"
    (should
     (equal '((bottom below top above))
	    (last
	     (mapcar (lambda (cell)
		       (org-export-table-cell-borders cell info))
		     (org-element-map tree 'table-cell 'identity)))))))

(ert-deftest test-org-export/table-dimensions ()
  "Test `org-export-table-dimensions' specifications."
  ;; 1. Standard test.
  (org-test-with-parsed-data "
| 1 | 2 | 3 |
| 4 | 5 | 6 |"
    (should
     (equal '(2 . 3)
	    (org-export-table-dimensions
	     (org-element-map tree 'table 'identity info 'first-match) info))))
  ;; 2. Ignore horizontal rules and special columns.
  (org-test-with-parsed-data "
| / | < | > |
| 1 | 2 | 3 |
|---+---+---|
| 4 | 5 | 6 |"
    (should
     (equal '(2 . 3)
	    (org-export-table-dimensions
	     (org-element-map tree 'table 'identity info 'first-match) info)))))

(ert-deftest test-org-export/table-cell-address ()
  "Test `org-export-table-cell-address' specifications."
  ;; 1. Standard test: index is 0-based.
  (org-test-with-parsed-data "| a | b |"
    (should
     (equal '((0 . 0) (0 . 1))
	    (org-element-map tree 'table-cell
	      (lambda (cell) (org-export-table-cell-address cell info))
	      info))))
  ;; 2. Special column isn't counted, nor are special rows.
  (org-test-with-parsed-data "
| / | <> |
|   | c  |"
    (should
     (equal '(0 . 0)
	    (org-export-table-cell-address
	     (car (last (org-element-map tree 'table-cell 'identity info)))
	     info))))
  ;; 3. Tables rules do not count either.
  (org-test-with-parsed-data "
| a |
|---|
| b |
|---|
| c |"
    (should
     (equal '(2 . 0)
	    (org-export-table-cell-address
	     (car (last (org-element-map tree 'table-cell 'identity info)))
	     info))))
  ;; 4. Return nil for special cells.
  (org-test-with-parsed-data "| / | a |"
    (should-not
     (org-export-table-cell-address
      (org-element-map tree 'table-cell 'identity nil 'first-match)
      info))))

(ert-deftest test-org-export/get-table-cell-at ()
  "Test `org-export-get-table-cell-at' specifications."
  ;; 1. Address ignores special columns, special rows and rules.
  (org-test-with-parsed-data "
| / | <> |
|   | a  |
|---+----|
|   | b  |"
    (should
     (equal '("b")
	    (org-element-contents
	     (org-export-get-table-cell-at
	      '(1 . 0)
	      (org-element-map tree 'table 'identity info 'first-match)
	      info)))))
  ;; 2. Return value for a non-existent address is nil.
  (org-test-with-parsed-data "| a |"
    (should-not
     (org-export-get-table-cell-at
      '(2 . 2)
      (org-element-map tree 'table 'identity info 'first-match)
      info)))
  (org-test-with-parsed-data "| / |"
    (should-not
     (org-export-get-table-cell-at
      '(0 . 0)
      (org-element-map tree 'table 'identity info 'first-match)
      info))))

(ert-deftest test-org-export/table-cell-starts-colgroup-p ()
  "Test `org-export-table-cell-starts-colgroup-p' specifications."
  ;; 1. A cell at a beginning of a row always starts a column group.
  (org-test-with-parsed-data "| a |"
    (should
     (org-export-table-cell-starts-colgroup-p
      (org-element-map tree 'table-cell 'identity info 'first-match)
      info)))
  ;; 2. Special column should be ignored when determining the
  ;;    beginning of the row.
  (org-test-with-parsed-data "
| / |   |
|   | a |"
    (should
     (org-export-table-cell-starts-colgroup-p
      (org-element-map tree 'table-cell 'identity info 'first-match)
      info)))
  ;; 2. Explicit column groups.
  (org-test-with-parsed-data "
| / |   | < |
| a | b | c |"
    (should
     (equal
      '(yes no yes)
      (org-element-map tree 'table-cell
	(lambda (cell)
	  (if (org-export-table-cell-starts-colgroup-p cell info) 'yes 'no))
	info)))))

(ert-deftest test-org-export/table-cell-ends-colgroup-p ()
  "Test `org-export-table-cell-ends-colgroup-p' specifications."
  ;; 1. A cell at the end of a row always ends a column group.
  (org-test-with-parsed-data "| a |"
    (should
     (org-export-table-cell-ends-colgroup-p
      (org-element-map tree 'table-cell 'identity info 'first-match)
      info)))
  ;; 2. Special column should be ignored when determining the
  ;;    beginning of the row.
  (org-test-with-parsed-data "
| / |   |
|   | a |"
    (should
     (org-export-table-cell-ends-colgroup-p
      (org-element-map tree 'table-cell 'identity info 'first-match)
      info)))
  ;; 3. Explicit column groups.
  (org-test-with-parsed-data "
| / | < |   |
| a | b | c |"
    (should
     (equal
      '(yes no yes)
      (org-element-map tree 'table-cell
	(lambda (cell)
	  (if (org-export-table-cell-ends-colgroup-p cell info) 'yes 'no))
	info)))))

(ert-deftest test-org-export/table-row-starts-rowgroup-p ()
  "Test `org-export-table-row-starts-rowgroup-p' specifications."
  ;; 1. A row at the beginning of a table always starts a row group.
  ;;    So does a row following a table rule.
  (org-test-with-parsed-data "
| a |
|---|
| b |"
    (should
     (equal
      '(yes no yes)
      (org-element-map tree 'table-row
	(lambda (row)
	  (if (org-export-table-row-starts-rowgroup-p row info) 'yes 'no))
	info))))
  ;; 2. Special rows should be ignored when determining the beginning
  ;;    of the row.
  (org-test-with-parsed-data "
| / | < |
|   | a |
|---+---|
| / | < |
|   | b |"
    (should
     (equal
      '(yes no yes)
      (org-element-map tree 'table-row
	(lambda (row)
	  (if (org-export-table-row-starts-rowgroup-p row info) 'yes 'no))
	info)))))

(ert-deftest test-org-export/table-row-ends-rowgroup-p ()
  "Test `org-export-table-row-ends-rowgroup-p' specifications."
  ;; 1. A row at the end of a table always ends a row group.  So does
  ;;    a row preceding a table rule.
  (org-test-with-parsed-data "
| a |
|---|
| b |"
    (should
     (equal
      '(yes no yes)
      (org-element-map tree 'table-row
	(lambda (row)
	  (if (org-export-table-row-ends-rowgroup-p row info) 'yes 'no))
	info))))
  ;; 2. Special rows should be ignored when determining the beginning
  ;;    of the row.
  (org-test-with-parsed-data "
|   | a |
| / | < |
|---+---|
|   | b |
| / | < |"
    (should
     (equal
      '(yes no yes)
      (org-element-map tree 'table-row
	(lambda (row)
	  (if (org-export-table-row-ends-rowgroup-p row info) 'yes 'no))
	info)))))

(ert-deftest test-org-export/table-row-in-header-p ()
  "Test `org-export-table-row-in-header-p' specifications."
  ;; Standard test.  Separators are always nil.
  (should
   (equal
    '(yes no no)
    (org-test-with-parsed-data "| a |\n|---|\n| b |"
      (org-element-map tree 'table-row
	(lambda (row)
	  (if (org-export-table-row-in-header-p row info) 'yes 'no)) info))))
  ;; Nil when there is no header.
  (should
   (equal
    '(no no)
    (org-test-with-parsed-data "| a |\n| b |"
      (org-element-map tree 'table-row
	(lambda (row)
	  (if (org-export-table-row-in-header-p row info) 'yes 'no)) info)))))

(ert-deftest test-org-export/table-row-starts-header-p ()
  "Test `org-export-table-row-starts-header-p' specifications."
  ;; 1. Only the row starting the first row group starts the table
  ;;    header.
  (org-test-with-parsed-data "
| a |
| b |
|---|
| c |"
    (should
     (equal
      '(yes no no no)
      (org-element-map tree 'table-row
	(lambda (row)
	  (if (org-export-table-row-starts-header-p row info) 'yes 'no))
	info))))
  ;; 2. A row cannot start an header if there's no header in the
  ;;    table.
  (org-test-with-parsed-data "
| a |
|---|"
    (should-not
     (org-export-table-row-starts-header-p
      (org-element-map tree 'table-row 'identity info 'first-match)
      info))))

(ert-deftest test-org-export/table-row-ends-header-p ()
  "Test `org-export-table-row-ends-header-p' specifications."
  ;; 1. Only the row starting the first row group starts the table
  ;;    header.
  (org-test-with-parsed-data "
| a |
| b |
|---|
| c |"
    (should
     (equal
      '(no yes no no)
      (org-element-map tree 'table-row
	(lambda (row)
	  (if (org-export-table-row-ends-header-p row info) 'yes 'no))
	info))))
  ;; 2. A row cannot start an header if there's no header in the
  ;;    table.
  (org-test-with-parsed-data "
| a |
|---|"
    (should-not
     (org-export-table-row-ends-header-p
      (org-element-map tree 'table-row 'identity info 'first-match)
      info))))



;;; Tables of Contents

(ert-deftest test-org-export/collect-headlines ()
  "Test `org-export-collect-headlines' specifications."
  ;; Standard test.
  (should
   (= 2
      (length
       (org-test-with-parsed-data "* H1\n** H2"
	 (org-export-collect-headlines info)))))
  ;; Do not collect headlines below optional argument.
  (should
   (= 1
      (length
       (org-test-with-parsed-data "* H1\n** H2"
	 (org-export-collect-headlines info 1)))))
  ;; Never collect headlines below maximum headline level.
  (should
   (= 1
      (length
       (org-test-with-parsed-data "#+OPTIONS: H:1\n* H1\n** H2"
	 (org-export-collect-headlines info)))))
  (should
   (= 1
      (length
       (org-test-with-parsed-data "#+OPTIONS: H:1\n* H1\n** H2"
	 (org-export-collect-headlines info 2)))))
  ;; Collect headlines locally.
  (should
   (= 2
      (org-test-with-parsed-data "* H1\n** H2\n** H3"
	(let ((scope (org-element-map tree 'headline #'identity info t)))
	  (length (org-export-collect-headlines info nil scope))))))
  ;; When collecting locally, optional level is relative.
  (should
   (= 1
      (org-test-with-parsed-data "* H1\n** H2\n*** H3"
	(let ((scope (org-element-map tree 'headline #'identity info t)))
	  (length (org-export-collect-headlines info 1 scope)))))))



;;; Templates

(ert-deftest test-org-export/inner-template ()
  "Test `inner-template' translator specifications."
  (should
   (equal "Success!"
	  (org-test-with-temp-text "* Headline"
	    (org-export-as
	     (org-export-create-backend
	      :transcoders
	      '((inner-template . (lambda (contents info) "Success!"))
		(headline . (lambda (h c i) "Headline"))))))))
  ;; Inner template is applied even in a "body-only" export.
  (should
   (equal "Success!"
	  (org-test-with-temp-text "* Headline"
	    (org-export-as
	     (org-export-create-backend
	      :transcoders '((inner-template . (lambda (c i) "Success!"))
			     (headline . (lambda (h c i) "Headline"))))
	     nil nil 'body-only)))))

(ert-deftest test-org-export/template ()
  "Test `template' translator specifications."
  (should
   (equal "Success!"
	  (org-test-with-temp-text "* Headline"
	    (org-export-as
	     (org-export-create-backend
	      :transcoders '((template . (lambda (contents info) "Success!"))
			     (headline . (lambda (h c i) "Headline"))))))))
  ;; Template is not applied in a "body-only" export.
  (should-not
   (equal "Success!"
	  (org-test-with-temp-text "* Headline"
	    (org-export-as
	     (org-export-create-backend
	      :transcoders '((template . (lambda (contents info) "Success!"))
			     (headline . (lambda (h c i) "Headline"))))
	     nil nil 'body-only)))))



;;; Topology

(ert-deftest test-org-export/get-next-element ()
  "Test `org-export-get-next-element' specifications."
  ;; Standard test.
  (should
   (equal "b"
	  (org-test-with-parsed-data "* Headline\n*a* b"
	    (org-export-get-next-element
	     (org-element-map tree 'bold 'identity info t) info))))
  ;; Return nil when no previous element.
  (should-not
   (org-test-with-parsed-data "* Headline\na *b*"
     (org-export-get-next-element
      (org-element-map tree 'bold 'identity info t) info)))
  ;; Non-exportable elements are ignored.
  (should-not
   (let ((org-export-with-timestamps nil))
     (org-test-with-parsed-data "\alpha <2012-03-29 Thu>"
       (org-export-get-next-element
	(org-element-map tree 'entity 'identity info t) info))))
  ;; Find next element in secondary strings.
  (should
   (eq 'verbatim
       (org-test-with-parsed-data "* a =verb="
	 (org-element-type
	  (org-export-get-next-element
	   (org-element-map tree 'plain-text 'identity info t) info)))))
  (should
   (eq 'verbatim
       (org-test-with-parsed-data "* /italic/ =verb="
	 (org-element-type
	  (org-export-get-next-element
	   (org-element-map tree 'italic 'identity info t) info)))))
  ;; Find next element in document keywords.
  (should
   (eq 'verbatim
       (org-test-with-parsed-data "#+TITLE: a =verb="
	 (org-element-type
	  (org-export-get-next-element
	   (org-element-map
	       (plist-get info :title) 'plain-text 'identity info t) info)))))
  ;; Find next element in parsed affiliated keywords.
  (should
   (eq 'verbatim
       (org-test-with-parsed-data "#+CAPTION: a =verb=\nParagraph"
	 (org-element-type
	  (org-export-get-next-element
	   (org-element-map tree 'plain-text 'identity info t nil t) info)))))
  ;; With optional argument N, return a list containing all the
  ;; following elements.
  (should
   (equal
    '(bold code underline)
    (org-test-with-parsed-data "_a_ /b/ *c* ~d~ _e_"
      (mapcar 'car
	      (org-export-get-next-element
	       (org-element-map tree 'italic 'identity info t) info t)))))
  ;; When N is a positive integer, return a list containing up to
  ;; N following elements.
  (should
   (equal
    '(bold code)
    (org-test-with-parsed-data "_a_ /b/ *c* ~d~ _e_"
      (mapcar 'car
	      (org-export-get-next-element
	       (org-element-map tree 'italic 'identity info t) info 2))))))

(ert-deftest test-org-export/get-previous-element ()
  "Test `org-export-get-previous-element' specifications."
  ;; Standard test.
  (should
   (equal "a "
	  (org-test-with-parsed-data "* Headline\na *b*"
	    (org-export-get-previous-element
	     (org-element-map tree 'bold 'identity info t) info))))
  ;; Return nil when no previous element.
  (should-not
   (org-test-with-parsed-data "* Headline\n*a* b"
     (org-export-get-previous-element
      (org-element-map tree 'bold 'identity info t) info)))
  ;; Non-exportable elements are ignored.
  (should-not
   (let ((org-export-with-timestamps nil))
     (org-test-with-parsed-data "<2012-03-29 Thu> \alpha"
       (org-export-get-previous-element
	(org-element-map tree 'entity 'identity info t) info))))
  ;; Find previous element in secondary strings.
  (should
   (eq 'verbatim
       (org-test-with-parsed-data "* =verb= a"
	 (org-element-type
	  (org-export-get-previous-element
	   (org-element-map tree 'plain-text 'identity info t) info)))))
  (should
   (eq 'verbatim
       (org-test-with-parsed-data "* =verb= /italic/"
	 (org-element-type
	  (org-export-get-previous-element
	   (org-element-map tree 'italic 'identity info t) info)))))
  ;; Find previous element in document keywords.
  (should
   (eq 'verbatim
       (org-test-with-parsed-data "#+TITLE: =verb= a"
	 (org-element-type
	  (org-export-get-previous-element
	   (org-element-map
	       (plist-get info :title) 'plain-text 'identity info t) info)))))
  ;; Find previous element in parsed affiliated keywords.
  (should
   (eq 'verbatim
       (org-test-with-parsed-data "#+CAPTION: =verb= a\nParagraph"
	 (org-element-type
	  (org-export-get-previous-element
	   (org-element-map tree 'plain-text 'identity info t nil t) info)))))
  ;; With optional argument N, return a list containing up to
  ;; N previous elements.
  (should
   (equal '(underline italic bold)
	  (org-test-with-parsed-data "_a_ /b/ *c* ~d~"
	    (mapcar 'car
		    (org-export-get-previous-element
		     (org-element-map tree 'code 'identity info t) info t)))))
  ;; When N is a positive integer, return a list containing up to
  ;; N previous elements.
  (should
   (equal '(italic bold)
	  (org-test-with-parsed-data "_a_ /b/ *c* ~d~"
	    (mapcar 'car
		    (org-export-get-previous-element
		     (org-element-map tree 'code 'identity info t) info 2))))))


(provide 'test-ox)
;;; test-org-export.el end here
