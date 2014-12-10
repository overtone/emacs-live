;;; test-org-element.el --- Tests for org-element.el

;; Copyright (C) 2012, 2013, 2014  Nicolas Goaziou

;; Author: Nicolas Goaziou <n.goaziou at gmail dot com>

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

(unless (featurep 'org-element)
  (signal 'missing-test-dependency "org-element"))

(defun org-test-parse-and-interpret (text)
  "Parse TEXT as Org syntax and interpret it.
Return interpreted string."
  (with-temp-buffer
    (org-mode)
    (insert text)
    (org-element-interpret-data (org-element-parse-buffer))))



;;; Test `org-element-map'

(ert-deftest test-org-element/map ()
  "Test `org-element-map'."
  ;; Can map to `plain-text' objects.
  (should
   (= 2
      (org-test-with-temp-text "Some text \alpha
#+BEGIN_CENTER
Some other text
#+END_CENTER"
	(let ((count 0))
	  (org-element-map
	   (org-element-parse-buffer) 'plain-text
	   (lambda (s) (when (string-match "text" s) (incf count))))
	  count))))
  ;; Applies to secondary strings
  (should
   (org-element-map '("some " (bold nil "bold") "text") 'bold 'identity))
  ;; Enter secondary strings before entering contents.
  (should
   (equal
    "alpha"
    (org-element-property
     :name
     (org-test-with-temp-text "* Some \\alpha headline\n\\beta entity."
       (org-element-map (org-element-parse-buffer) 'entity 'identity nil t)))))
  ;; Apply NO-RECURSION argument.
  (should-not
   (org-test-with-temp-text "#+BEGIN_CENTER\n\\alpha\n#+END_CENTER"
     (org-element-map
      (org-element-parse-buffer) 'entity 'identity nil nil 'center-block)))
  ;; Use WITH-AFFILIATED argument.
  (should
   (equal
    '("a" "1" "b" "2")
    (org-test-with-temp-text "#+CAPTION[a]: 1\n#+CAPTION[b]: 2\nParagraph"
      (org-element-map
       (org-element-at-point) 'plain-text 'identity nil nil nil t)))))



;;; Test Setters

(ert-deftest test-org-element/put-property ()
  "Test `org-element-put-property' specifications."
  ;; Standard test.
  (org-test-with-temp-text "* Headline\n *a*"
    (let ((tree (org-element-parse-buffer)))
      (org-element-put-property
       (org-element-map tree 'bold 'identity nil t) :test 1)
      (should (org-element-property
	       :test (org-element-map tree 'bold 'identity nil t)))))
  ;; Put property on a string.
  (should
   (org-element-property :test (org-element-put-property "Paragraph" :test t))))

(ert-deftest test-org-element/set-contents ()
  "Test `org-element-set-contents' specifications."
  ;; Accept multiple entries.
  (should
   (equal '("b" (italic nil "a"))
	  (org-test-with-temp-text "* Headline\n *a*"
	    (let ((tree (org-element-parse-buffer)))
	      (org-element-set-contents
	       (org-element-map tree 'bold 'identity nil t) "b" '(italic nil "a"))
	      (org-element-contents
	       (org-element-map tree 'bold 'identity nil t))))))
  ;; Accept atoms and elements.
  (should
   (equal '("b")
	  (org-test-with-temp-text "* Headline\n *a*"
	    (let ((tree (org-element-parse-buffer)))
	      (org-element-set-contents
	       (org-element-map tree 'bold 'identity nil t) "b")
	      (org-element-contents
	       (org-element-map tree 'bold 'identity nil t))))))
  (should
   (equal '((italic nil "b"))
	  (org-test-with-temp-text "* Headline\n *a*"
	    (let ((tree (org-element-parse-buffer)))
	      (org-element-set-contents
	       (org-element-map tree 'bold 'identity nil t) '(italic nil "b"))
	      (org-element-contents
	       (org-element-map tree 'bold 'identity nil t))))))
  ;; Allow nil contents.
  (should-not
   (org-test-with-temp-text "* Headline\n *a*"
     (let ((tree (org-element-parse-buffer)))
       (org-element-set-contents (org-element-map tree 'bold 'identity nil t))
       (org-element-contents (org-element-map tree 'bold 'identity nil t))))))

(ert-deftest test-org-element/set-element ()
  "Test `org-element-set-element' specifications."
  (org-test-with-temp-text "* Headline\n*a*"
    (let ((tree (org-element-parse-buffer)))
      (org-element-set-element
       (org-element-map tree 'bold 'identity nil t)
       '(italic nil "b"))
      ;; Check if object is correctly replaced.
      (should (org-element-map tree 'italic 'identity))
      (should-not (org-element-map tree 'bold 'identity))
      ;; Check if new object's parent is correctly set.
      (should
       (eq
	(org-element-property :parent
			      (org-element-map tree 'italic 'identity nil t))
	(org-element-map tree 'paragraph 'identity nil t))))))

(ert-deftest test-org-element/adopt-elements ()
  "Test `org-element-adopt-elements' specifications."
  ;; Adopt an element.
  (should
   (equal '(plain-text italic)
	  (org-test-with-temp-text "* Headline\n *a*"
	    (let ((tree (org-element-parse-buffer)))
	      (org-element-adopt-elements
	       (org-element-map tree 'bold 'identity nil t) '(italic nil "a"))
	      (mapcar (lambda (blob) (org-element-type blob))
		      (org-element-contents
		       (org-element-map tree 'bold 'identity nil t)))))))
  ;; Adopt a string.
  (should
   (equal '("a" "b")
	  (org-test-with-temp-text "* Headline\n *a*"
	    (let ((tree (org-element-parse-buffer)))
	      (org-element-adopt-elements
	       (org-element-map tree 'bold 'identity nil t) "b")
	      (org-element-contents
	       (org-element-map tree 'bold 'identity nil t)))))))



;;; Test Parsers

;;;; Affiliated Keywords

(ert-deftest test-org-element/affiliated-keywords-parser ()
  "Test affiliated keywords parsing."
  ;; Read simple keywords.
  (should
   (equal "para"
	  (org-element-property
	   :name
	   (org-test-with-temp-text "#+NAME: para\nParagraph"
	     (org-element-at-point)))))
  (should
   (= 1
      (org-element-property
       :begin
       (org-test-with-temp-text "#+NAME: para\nParagraph"
	 (org-element-at-point)))))
  ;; Parse multiple keywords.
  (should
   (equal
    '("line2" "line1")
    (org-element-property
     :attr_ascii
     (org-test-with-temp-text
	 "#+ATTR_ASCII: line1\n#+ATTR_ASCII: line2\nParagraph"
       (org-element-at-point)))))
  ;; Parse "parsed" keywords.
  (should
   (equal
    '(("caption"))
    (org-test-with-temp-text "#+CAPTION: caption\nParagraph"
      (car (org-element-property :caption (org-element-at-point))))))
  ;; Parse dual keywords.
  (should
   (equal
    '((("long") "short"))
    (org-test-with-temp-text "#+CAPTION[short]: long\nParagraph"
      (org-element-property :caption (org-element-at-point)))))
  ;; Allow multiple caption keywords.
  (should
   (equal
    '((("l2") "s2") (("l1") "s1"))
    (org-test-with-temp-text "#+CAPTION[s1]: l1\n#+CAPTION[s2]: l2\nParagraph"
      (org-element-property :caption (org-element-at-point)))))
  (should
   (equal
    '((("l1")) (nil "s1"))
    (org-test-with-temp-text "#+CAPTION[s1]:\n#+CAPTION: l1\nParagraph"
      (org-element-property :caption (org-element-at-point)))))
  ;; Corner case: orphaned keyword at the end of an element.
  (should
   (eq 'keyword
       (org-test-with-temp-text "- item\n  #+name: name\nSome paragraph"
	 (progn (search-forward "name")
		(org-element-type (org-element-at-point))))))
  (should-not
   (org-test-with-temp-text "- item\n  #+name: name\nSome paragraph"
     (progn (search-forward "Some")
	    (org-element-property :name (org-element-at-point))))))


;;;; Babel Call

(ert-deftest test-org-element/babel-call-parser ()
  "Test `babel-call' parsing."
  ;; Standard test.
  (should
   (org-test-with-temp-text "#+CALL: test()"
     (org-element-map (org-element-parse-buffer) 'babel-call 'identity)))
  ;; Ignore case.
  (should
   (org-test-with-temp-text "#+call: test()"
     (org-element-map (org-element-parse-buffer) 'babel-call 'identity)))
  ;; Handle non-empty blank line at the end of buffer.
  (should
   (org-test-with-temp-text "#+CALL: test()\n "
     (= (org-element-property :end (org-element-at-point)) (point-max)))))


;;;; Bold

(ert-deftest test-org-element/bold-parser ()
  "Test `bold' parser."
  ;; Standard test.
  (should
   (let ((org-emph-re "\\([ 	('\"{]\\|^\\)\\(\\([+*/_=~]\\)\\([^ 	\n,\"']\\|[^ 	\n,\"'].*?\\(?:\n.*?\\)\\{0,1\\}[^ 	\n,\"']\\)\\3\\)\\([- 	.,:!?;'\")}\\]\\|$\\)"))
     (org-test-with-temp-text "*bold*"
       (org-element-map (org-element-parse-buffer) 'bold 'identity nil t))))
  ;; Multi-line markup.
  (should
   (equal
    (org-element-contents
     (let ((org-emph-re "\\([ 	('\"{]\\|^\\)\\(\\([+*/_=~]\\)\\([^ 	\n,\"']\\|[^ 	\n,\"'].*?\\(?:\n.*?\\)\\{0,1\\}[^ 	\n,\"']\\)\\3\\)\\([- 	.,:!?;'\")}\\]\\|$\\)"))
       (org-test-with-temp-text "*first line\nsecond line*"
	 (org-element-map (org-element-parse-buffer) 'bold 'identity nil t))))
    '("first line\nsecond line"))))


;;;; Center Block

(ert-deftest test-org-element/center-block-parser ()
  "Test `center-block' parser."
  ;; Standard test.
  (should
   (org-test-with-temp-text "#+BEGIN_CENTER\nText\n#+END_CENTER"
     (org-element-map (org-element-parse-buffer) 'center-block 'identity)))
  ;; Ignore case.
  (should
   (org-test-with-temp-text "#+begin_center\nText\n#+end_center"
     (org-element-map (org-element-parse-buffer) 'center-block 'identity)))
  ;; Test folded block.
  (org-test-with-temp-text "#+BEGIN_CENTER\nText\n#+END_CENTER"
    (org-cycle)
    (should
     (org-element-property
      :hiddenp
      (org-element-map (org-element-parse-buffer) 'center-block
	'identity nil t))))
  ;; Ignore incomplete block.
  (should-not
   (org-test-with-temp-text "#+BEGIN_CENTER"
     (org-element-map (org-element-parse-buffer) 'center-block
       'identity nil t)))
  ;; Handle non-empty blank line at the end of buffer.
  (should
   (org-test-with-temp-text "#+BEGIN_CENTER\nC\n#+END_CENTER\n "
     (= (org-element-property :end (org-element-at-point)) (point-max)))))


;;;; Clock

(ert-deftest test-org-element/clock-parser ()
  "Test `clock' parser."
  ;; Running clock.
  (let* ((org-clock-string "CLOCK:")
	 (clock (org-test-with-temp-text "CLOCK: [2012-01-01 sun. 00:01]"
		  (org-element-at-point))))
    (should (eq (org-element-property :status clock) 'running))
    (should
     (equal (org-element-property :raw-value
				  (org-element-property :value clock))
	    "[2012-01-01 sun. 00:01]"))
    (should-not (org-element-property :duration clock)))
  ;; Closed clock.
  (let* ((org-clock-string "CLOCK:")
	 (clock
	  (org-test-with-temp-text
	      "CLOCK: [2012-01-01 sun. 00:01]--[2012-01-01 sun. 00:02] =>  0:01"
	    (org-element-at-point))))
    (should (eq (org-element-property :status clock) 'closed))
    (should (equal (org-element-property :raw-value
					 (org-element-property :value clock))
		   "[2012-01-01 sun. 00:01]--[2012-01-01 sun. 00:02]"))
    (should (equal (org-element-property :duration clock) "0:01"))))


;;;; Code

(ert-deftest test-org-element/code-parser ()
  "Test `code' parser."
  ;; Regular test.
  (should
   (let ((org-emph-re "\\([ 	('\"{]\\|^\\)\\(\\([+*/_=~]\\)\\([^ 	\n,\"']\\|[^ 	\n,\"'].*?\\(?:\n.*?\\)\\{0,1\\}[^ 	\n,\"']\\)\\3\\)\\([- 	.,:!?;'\")}\\]\\|$\\)"))
     (org-test-with-temp-text "~code~"
       (org-element-map (org-element-parse-buffer) 'code 'identity))))
  ;; Multi-line markup.
  (should
   (equal
    (org-element-property
     :value
     (let ((org-emph-re "\\([ 	('\"{]\\|^\\)\\(\\([+*/_=~]\\)\\([^ 	\n,\"']\\|[^ 	\n,\"'].*?\\(?:\n.*?\\)\\{0,1\\}[^ 	\n,\"']\\)\\3\\)\\([- 	.,:!?;'\")}\\]\\|$\\)"))
       (org-test-with-temp-text "~first line\nsecond line~"
	 (org-element-map
	  (org-element-parse-buffer) 'code 'identity nil t))))
    "first line\nsecond line")))


;;;; Comment

(ert-deftest test-org-element/comment-parser ()
  "Test `comment' parser."
  ;; Regular comment.
  (should
   (org-test-with-temp-text "# Comment"
     (org-element-map (org-element-parse-buffer) 'comment 'identity)))
  ;; Inline comment.
  (should
   (org-test-with-temp-text "  # Comment"
     (org-element-map (org-element-parse-buffer) 'comment 'identity)))
  ;; Preserve indentation.
  (should
   (equal
    (org-element-property
     :value
     (org-test-with-temp-text "# No blank\n#  One blank"
       (org-element-map (org-element-parse-buffer) 'comment 'identity nil t)))
    "No blank\n One blank"))
  ;; Comment with blank lines.
  (should
   (equal
    (org-element-property
     :value
     (org-test-with-temp-text "# First part\n# \n#\n# Second part"
       (org-element-map (org-element-parse-buffer) 'comment 'identity nil t)))
    "First part\n\n\nSecond part"))
  ;; Do not mix comments and keywords.
  (should
   (eq 1
       (org-test-with-temp-text "#+keyword: value\n# comment\n#+keyword: value"
	 (length (org-element-map (org-element-parse-buffer) 'comment
		   'identity)))))
  (should
   (equal "comment"
	  (org-test-with-temp-text "#+keyword: value\n# comment\n#+keyword: value"
	    (org-element-property
	     :value
	     (org-element-map (org-element-parse-buffer) 'comment
	       'identity nil t)))))
  ;; Correctly handle non-empty blank lines at the end of buffer.
  (should
   (org-test-with-temp-text "# A\n "
     (= (org-element-property :end (org-element-at-point)) (point-max)))))


;;;; Comment Block

(ert-deftest test-org-element/comment-block-parser ()
  "Test `comment-block' parser."
  ;; Standard test.
  (should
   (org-test-with-temp-text "#+BEGIN_COMMENT\nText\n#+END_COMMENT"
     (org-element-map (org-element-parse-buffer) 'comment-block 'identity)))
  ;; Ignore case.
  (should
   (org-test-with-temp-text "#+begin_comment\nText\n#+end_comment"
     (org-element-map (org-element-parse-buffer) 'comment-block 'identity)))
  ;; Test folded block.
  (org-test-with-temp-text "#+BEGIN_COMMENT\nText\n#+END_COMMENT"
    (org-cycle)
    (should
     (org-element-property
      :hiddenp
      (org-element-map (org-element-parse-buffer) 'comment-block
	'identity nil t))))
  ;; Ignore incomplete block.
  (should-not
   (org-test-with-temp-text "#+BEGIN_COMMENT"
     (org-element-map (org-element-parse-buffer) 'comment-block
       'identity nil t)))
  ;; Handle non-empty blank line at the end of buffer.
  (should
   (org-test-with-temp-text "#+BEGIN_COMMENT\nC\n#+END_COMMENT\n "
     (= (org-element-property :end (org-element-at-point)) (point-max)))))


;;;; Diary Sexp

(ert-deftest test-org-element/diary-sexp-parser ()
  "Test `diary-sexp' parser."
  ;; Standard test.
  (should
   (eq 'diary-sexp
       (org-test-with-temp-text
	   "%%(org-anniversary 1956  5 14)(2) Arthur Dent is %d years old"
	 (org-element-type (org-element-at-point)))))
  ;; Diary sexp must live at beginning of line
  (should-not
   (eq 'diary-sexp
       (org-test-with-temp-text " %%(org-bbdb-anniversaries)"
	 (org-element-type (org-element-at-point)))))
  ;; Handle non-empty blank line at the end of buffer.
  (should
   (org-test-with-temp-text "%%(org-bbdb-anniversaries)\n "
     (= (org-element-property :end (org-element-at-point)) (point-max)))))


;;;; Drawer

(ert-deftest test-org-element/drawer-parser ()
  "Test `drawer' parser."
  ;; Standard test.
  (should
   (let ((org-drawers '("TEST")))
     (org-test-with-temp-text ":TEST:\nText\n:END:"
       (org-element-map (org-element-parse-buffer) 'drawer 'identity))))
  ;; Do not mix regular drawers and property drawers.
  (should-not
   (let ((org-drawers '("PROPERTIES")))
     (org-test-with-temp-text ":PROPERTIES:\n:prop: value\n:END:"
       (org-element-map (org-element-parse-buffer) 'drawer 'identity nil t))))
  ;; Ignore incomplete drawer.
  (should-not
   (let ((org-drawers '("TEST")))
     (org-test-with-temp-text ":TEST:"
       (org-element-map (org-element-parse-buffer) 'drawer 'identity nil t))))
  ;; Handle non-empty blank line at the end of buffer.
  (should
   (org-test-with-temp-text ":TEST:\nC\n:END:\n "
     (= (org-element-property :end (org-element-at-point)) (point-max)))))


;;;; Dynamic Block

(ert-deftest test-org-element/dynamic-block-parser ()
  "Test `dynamic-block' parser."
  ;; Standard test.
  (should
   (org-test-with-temp-text
       "#+BEGIN: myblock :param1 val1 :param2 val2\nText\n#+END:"
     (org-element-map (org-element-parse-buffer) 'dynamic-block 'identity)))
  ;; Folded view
  (org-test-with-temp-text
      "#+BEGIN: myblock :param1 val1 :param2 val2\nText\n#+END:"
    (org-cycle)
    (should
     (org-element-property
      :hiddenp
      (org-element-map (org-element-parse-buffer) 'dynamic-block
	'identity nil t))))
  ;; Ignore case.
  (should
   (org-test-with-temp-text
       "#+begin: myblock :param1 val1 :param2 val2\nText\n#+end:"
     (org-element-map (org-element-parse-buffer) 'dynamic-block 'identity)))
  ;; Ignore incomplete block.
  (should-not
   (org-test-with-temp-text "#+BEGIN: myblock :param1 val1 :param2 val2"
     (org-element-map (org-element-parse-buffer) 'dynamic-block
       'identity nil t)))
  ;; Handle non-empty blank line at the end of buffer.
  (should
   (org-test-with-temp-text "#+BEGIN: myblock :param val1\nC\n#+END:\n  "
     (= (org-element-property :end (org-element-at-point)) (point-max)))))


;;;; Entity

(ert-deftest test-org-element/entity-parser ()
  "Test `entity' parser."
  ;; Without brackets.
  (should
   (org-test-with-temp-text "\\sin"
     (org-element-map (org-element-parse-buffer) 'entity 'identity)))
  ;; With brackets.
  (should
   (org-element-property
    :use-brackets-p
    (org-test-with-temp-text "\\alpha{}text"
      (org-element-map (org-element-parse-buffer) 'entity 'identity nil t))))
  ;; User-defined entity.
  (should
   (equal
    (org-element-property
     :name
     (let ((org-entities-user
	    '(("test" "test" nil "test" "test" "test" "test"))))
       (org-test-with-temp-text "\\test"
	 (org-element-map (org-element-parse-buffer) 'entity 'identity nil t))))
    "test"))
  ;; Special case: entity at the end of a container.
  (should
   (eq 'entity
       (org-test-with-temp-text "*\\alpha \\beta*"
	 (search-forward "be")
	 (org-element-type (org-element-context))))))


;;;; Example Block

(ert-deftest test-org-element/example-block-parser ()
  "Test `example-block' parser."
  ;; Standard test.
  (should
   (org-test-with-temp-text "#+BEGIN_EXAMPLE\nText\n#+END_EXAMPLE"
     (org-element-map (org-element-parse-buffer) 'example-block 'identity)))
  ;; Test folded block.
  (should
   (org-test-with-temp-text "#+BEGIN_EXAMPLE\nText\n#+END_EXAMPLE"
     (org-cycle)
     (org-element-property :hiddenp (org-element-at-point))))
  ;; Ignore incomplete block.
  (should-not
   (eq 'example-block
       (org-test-with-temp-text "#+BEGIN_EXAMPLE"
	 (org-element-type (org-element-at-point)))))
  ;; Properly un-escape code.
  (should
   (equal "* Headline\n #+keyword\nText\n"
	  (org-test-with-temp-text
	      "#+BEGIN_EXAMPLE\n,* Headline\n ,#+keyword\nText\n#+END_EXAMPLE"
	    (org-element-property :value (org-element-at-point)))))
  ;; Nil `org-src-preserve-indentation': Remove maximum common
  ;; indentation.
  (should
   (equal " L1\nL2\n"
	  (org-test-with-temp-text "#+BEGIN_EXAMPLE\n  L1\n L2\n#+END_EXAMPLE"
	    (let ((org-src-preserve-indentation nil))
	      (org-element-property :value (org-element-at-point))))))
  ;; Non-nil `org-src-preserve-indentation': Remove block indentation
  ;; only, unless block contents are less indented than block
  ;; boundaries.
  (should
   (equal " L1\nL2\n"
	  (org-test-with-temp-text " #+BEGIN_EXAMPLE\n  L1\n L2\n #+END_EXAMPLE"
	    (let ((org-src-preserve-indentation t))
	      (org-element-property :value (org-element-at-point))))))
  (should
   (equal
    "  L1\n L2\n"
    (org-test-with-temp-text "  #+BEGIN_EXAMPLE\n  L1\n L2\n  #+END_EXAMPLE"
      (let ((org-src-preserve-indentation t))
	(org-element-property :value (org-element-at-point))))))
  ;; Handle non-empty blank line at the end of buffer.
  (should
   (org-test-with-temp-text "#+BEGIN_EXAMPLE\nC\n#+END_EXAMPLE\n "
     (= (org-element-property :end (org-element-at-point)) (point-max)))))

(ert-deftest test-org-element/block-switches ()
  "Test `example-block' and `src-block' switches parsing."
  (let ((org-coderef-label-format "(ref:%s)"))
    ;; 1. Test "-i" switch.
    (should-not
     (org-test-with-temp-text "#+BEGIN_SRC emacs-lisp\n(+ 1 1)\n#+END_SRC"
       (org-element-property :preserve-indent (org-element-at-point))))
    (should
     (org-test-with-temp-text "#+BEGIN_SRC emacs-lisp -i\n(+ 1 1)\n#+END_SRC"
       (org-element-property :preserve-indent (org-element-at-point))))
    (should-not
     (org-test-with-temp-text "#+BEGIN_EXAMPLE\nText.\n#+END_EXAMPLE"
       (org-element-property :preserve-indent (org-element-at-point))))
    (should
     (org-test-with-temp-text "#+BEGIN_EXAMPLE -i\nText.\n#+END_EXAMPLE"
       (org-element-property :preserve-indent (org-element-at-point))))
    ;; 2. "-n -r -k" combination should number lines, retain labels but
    ;;    not use them in coderefs.
    (should
     (org-test-with-temp-text "#+BEGIN_EXAMPLE -n -r -k\nText.\n#+END_EXAMPLE"
       (let ((element (org-element-at-point)))
	 (and (org-element-property :number-lines element)
	      (org-element-property :retain-labels element)
	      (not (org-element-property :use-labels element))))))
    (should
     (org-test-with-temp-text
	 "#+BEGIN_SRC emacs-lisp -n -r -k\n(+ 1 1)\n#+END_SRC"
       (let ((element (org-element-at-point)))
	 (and (org-element-property :number-lines element)
	      (org-element-property :retain-labels element)
	      (not (org-element-property :use-labels element))))))
    ;; 3. "-n -r" combination should number-lines remove labels and not
    ;;    use them in coderefs.
    (should
     (org-test-with-temp-text "#+BEGIN_EXAMPLE -n -r\nText.\n#+END_EXAMPLE"
       (let ((element (org-element-at-point)))
	 (and (org-element-property :number-lines element)
	      (not (org-element-property :retain-labels element))
	      (not (org-element-property :use-labels element))))))
    (should
     (org-test-with-temp-text "#+BEGIN_SRC emacs-lisp -n -r\n(+ 1 1)\n#+END_SRC"
       (let ((element (org-element-at-point)))
	 (and (org-element-property :number-lines element)
	      (not (org-element-property :retain-labels element))
	      (not (org-element-property :use-labels element))))))
    ;; 4. "-n" or "+n" should number lines, retain labels and use them
    ;;    in coderefs.
    (should
     (org-test-with-temp-text "#+BEGIN_EXAMPLE -n\nText.\n#+END_EXAMPLE"
       (let ((element (org-element-at-point)))
	 (and (org-element-property :number-lines element)
	      (org-element-property :retain-labels element)
	      (org-element-property :use-labels element)))))
    (should
     (org-test-with-temp-text "#+BEGIN_SRC emacs-lisp -n\n(+ 1 1)\n#+END_SRC"
       (let ((element (org-element-at-point)))
	 (and (org-element-property :number-lines element)
	      (org-element-property :retain-labels element)
	      (org-element-property :use-labels element)))))
    (should
     (org-test-with-temp-text "#+BEGIN_EXAMPLE +n\nText.\n#+END_EXAMPLE"
       (let ((element (org-element-at-point)))
	 (and (org-element-property :number-lines element)
	      (org-element-property :retain-labels element)
	      (org-element-property :use-labels element)))))
    (should
     (org-test-with-temp-text "#+BEGIN_SRC emacs-lisp +n\n(+ 1 1)\n#+END_SRC"
       (let ((element (org-element-at-point)))
	 (and (org-element-property :number-lines element)
	      (org-element-property :retain-labels element)
	      (org-element-property :use-labels element)))))
    ;; 5. No switch should not number lines, but retain labels and use
    ;;    them in coderefs.
    (should
     (org-test-with-temp-text "#+BEGIN_EXAMPLE\nText.\n#+END_EXAMPLE"
       (let ((element (org-element-at-point)))
	 (and (not (org-element-property :number-lines element))
	      (org-element-property :retain-labels element)
	      (org-element-property :use-labels element)))))
    (should
     (org-test-with-temp-text "#+BEGIN_SRC emacs-lisp\n(+ 1 1)\n#+END_SRC"
       (let ((element (org-element-at-point)))
	 (and (not (org-element-property :number-lines element))
	      (org-element-property :retain-labels element)
	      (org-element-property :use-labels element)))))
    ;; 6. "-r" switch only: do not number lines, remove labels, and
    ;;    don't use labels in coderefs.
    (should
     (org-test-with-temp-text "#+BEGIN_EXAMPLE -r\nText.\n#+END_EXAMPLE"
       (let ((element (org-element-at-point)))
	 (and (not (org-element-property :number-lines element))
	      (not (org-element-property :retain-labels element))
	      (not (org-element-property :use-labels element))))))
    (should
     (org-test-with-temp-text "#+BEGIN_SRC emacs-lisp -r\n(+ 1 1)\n#+END_SRC"
       (let ((element (org-element-at-point)))
	 (and (not (org-element-property :number-lines element))
	      (not (org-element-property :retain-labels element))
	      (not (org-element-property :use-labels element))))))
    ;; 7. Recognize coderefs with user-defined syntax.
    (should
     (equal
      "[ref:%s]"
      (org-test-with-temp-text
	  "#+BEGIN_EXAMPLE -l \"[ref:%s]\"\nText [ref:text]\n#+END_EXAMPLE"
	(org-element-property :label-fmt (org-element-at-point)))))
    (should
     (equal
      "[ref:%s]"
      (org-test-with-temp-text
	  "#+BEGIN_SRC emacs-lisp -l \"[ref:%s]\"\n(+ 1 1) [ref:text]\n#+END_SRC"
	(org-element-property :label-fmt (org-element-at-point)))))))


;;;; Export Block

(ert-deftest test-org-element/export-block-parser ()
  "Test `export-block' parser."
  ;; Standard test.
  (should
   (let ((org-element-block-name-alist
	  '(("LATEX" . org-element-export-block-parser))))
     (org-test-with-temp-text "#+BEGIN_LATEX\nText\n#+END_LATEX"
       (org-element-map (org-element-parse-buffer) 'export-block 'identity))))
  ;; Test folded block.
  (org-test-with-temp-text "#+BEGIN_LATEX\nText\n#+END_LATEX"
    (org-cycle)
    (should
     (org-element-property
      :hiddenp
      (org-element-map
	  (let ((org-element-block-name-alist
		 '(("LATEX" . org-element-export-block-parser))))
	    (org-element-parse-buffer))
	  'export-block 'identity nil t))))
  ;; Ignore case.
  (should
   (let ((org-element-block-name-alist
	  '(("LATEX" . org-element-export-block-parser))))
     (org-test-with-temp-text "#+begin_latex\nText\n#+end_latex"
       (org-element-map (org-element-parse-buffer) 'export-block 'identity))))
  ;; Ignore incomplete block.
  (should-not
   (let ((org-element-block-name-alist
	  '(("LATEX" . org-element-export-block-parser))))
     (org-test-with-temp-text "#+BEGIN_LATEX"
       (org-element-map (org-element-parse-buffer) 'export-block
	 'identity nil t))))
  ;; Handle non-empty blank line at the end of buffer.
  (should
   (let ((org-element-block-name-alist
	  '(("LATEX" . org-element-export-block-parser))))
     (org-test-with-temp-text "#+BEGIN_LATEX\nC\n#+END_LATEX\n "
       (= (org-element-property :end (org-element-at-point)) (point-max))))))


;;;; Export Snippet

(ert-deftest test-org-element/export-snippet-parser ()
  "Test `export-snippet' parser."
  (should
   (equal
    '("back-end" . "contents")
    (org-test-with-temp-text "@@back-end:contents@@"
      (org-element-map
       (org-element-parse-buffer) 'export-snippet
       (lambda (snippet) (cons (org-element-property :back-end snippet)
			  (org-element-property :value snippet)))
       nil t)))))


;;;; Fixed Width

(ert-deftest test-org-element/fixed-width-parser ()
  "Test fixed-width area parsing."
  ;; Preserve indentation.
  (should
   (org-test-with-temp-text ": no blank\n:  one blank"
     (org-element-map (org-element-parse-buffer) 'fixed-width 'identity)))
  ;; Fixed-width with empty lines.
  (should
   (org-test-with-temp-text ": first part\n:\n: \n: second part"
     (org-element-map (org-element-parse-buffer) 'fixed-width 'identity)))
  ;; Parse indented fixed-width markers.
  (should
   (org-test-with-temp-text "Text\n  : no blank\n  :  one blank"
     (org-element-map (org-element-parse-buffer) 'fixed-width 'identity)))
  ;; Distinguish fixed-width areas within a list and outside of it.
  (should
   (= 2
      (length
       (org-test-with-temp-text "
- Item
  : fixed-width inside
: fixed-width outside"
	 (org-element-map (org-element-parse-buffer) 'fixed-width 'identity)))))
  ;; Handle non-empty blank line at the end of buffer.
  (should
   (org-test-with-temp-text ": A\n "
     (= (org-element-property :end (org-element-at-point)) (point-max)))))


;;;; Footnote Definition

(ert-deftest test-org-element/footnote-definition-parser ()
  "Test `footnote-definition' parser."
  (should
   (org-test-with-temp-text "[fn:1] Definition"
     (org-element-map (org-element-parse-buffer) 'footnote-definition
       'identity nil t)))
  ;; Footnote with more contents
  (should
   (= 29
      (org-element-property
       :end
       (org-test-with-temp-text "[fn:1] Definition\n\n| a | b |"
	 (org-element-map (org-element-parse-buffer) 'footnote-definition
	   'identity nil t)))))
  ;; Footnote starting with special syntax.
  (should-not
   (org-test-with-temp-text "[fn:1] - no item"
     (org-element-map (org-element-parse-buffer) 'item 'identity)))
  ;; Correctly handle footnote starting with an empty line.
  (should
   (= 9
      (org-test-with-temp-text "[fn:1]\n\n  Body"
	(org-element-property :contents-begin (org-element-at-point)))))
  ;; Handle non-empty blank line at the end of buffer.
  (should
   (org-test-with-temp-text "[fn:1] Definition\n "
     (= (org-element-property :end (org-element-at-point)) (point-max)))))


;;;; Footnotes Reference.

(ert-deftest test-org-element/footnote-reference-parser ()
  "Test `footnote-reference' parser."
  ;; 1. Parse a standard reference.
  (org-test-with-temp-text "Text[fn:label]"
    (should
     (org-element-map
      (org-element-parse-buffer) 'footnote-reference 'identity)))
  ;; 2. Parse a normalized reference.
  (org-test-with-temp-text "Text[1]"
    (should
     (org-element-map
      (org-element-parse-buffer) 'footnote-reference 'identity)))
  ;; 3. Parse an inline reference.
  (org-test-with-temp-text "Text[fn:test:def]"
    (should
     (org-element-map
      (org-element-parse-buffer) 'footnote-reference 'identity)))
  ;; 4. Parse an anonymous reference.
  (org-test-with-temp-text "Text[fn::def]"
    (should
     (org-element-map
      (org-element-parse-buffer) 'footnote-reference 'identity)))
  ;; 5. Parse nested footnotes.
  (org-test-with-temp-text "Text[fn::def [fn:label]]"
    (should
     (= 2
	(length
	 (org-element-map
	  (org-element-parse-buffer) 'footnote-reference 'identity)))))
  ;; 6. Parse adjacent footnotes.
  (org-test-with-temp-text "Text[fn:label1][fn:label2]"
    (should
     (= 2
	(length
	 (org-element-map
	  (org-element-parse-buffer) 'footnote-reference 'identity)))))
  ;; 7. Only properly closed footnotes are recognized as such.
  (org-test-with-temp-text "Text[fn:label"
    (should-not
     (org-element-map
      (org-element-parse-buffer) 'footnote-reference 'identity))))


;;;; Headline

(ert-deftest test-org-element/headline-quote-keyword ()
  "Test QUOTE keyword recognition."
  ;; Reference test.
  (org-test-with-temp-text "* Headline"
    (let ((org-quote-string "QUOTE"))
      (should-not (org-element-property :quotedp (org-element-at-point)))))
  ;; Standard position.
  (org-test-with-temp-text "* QUOTE Headline"
    (let* ((org-quote-string "QUOTE")
	   (headline (org-element-at-point)))
      (should (org-element-property :quotedp headline))
      ;; Test removal from raw value.
      (should (equal (org-element-property :raw-value headline) "Headline"))))
  ;; Case sensitivity.
  (org-test-with-temp-text "* QUOTE Headline"
    (let* ((org-quote-string "Quote")
	   (headline (org-element-at-point)))
      (should-not (org-element-property :quotedp headline))
      (should (equal (org-element-property :raw-value headline)
		     "QUOTE Headline"))))
  ;; With another keyword.
  (org-test-with-temp-text "* TODO QUOTE Headline"
    (let* ((org-quote-string "QUOTE")
	   (org-todo-keywords '((sequence "TODO" "DONE")))
	   (headline (org-element-at-point)))
      (should (org-element-property :quotedp headline))
      (should (equal (org-element-property :raw-value headline) "Headline"))))
  ;; With the keyword only.
  (org-test-with-temp-text "* QUOTE"
    (let* ((org-quote-string "QUOTE")
	   (headline (org-element-at-point)))
      (should (org-element-property :quotedp headline))
      (should (equal (org-element-property :raw-value headline) "")))))

(ert-deftest test-org-element/headline-comment-keyword ()
  "Test COMMENT keyword recognition."
  ;; Reference test.
  (org-test-with-temp-text "* Headline"
    (let ((org-comment-string "COMMENT"))
      (should-not (org-element-property :commentedp (org-element-at-point)))))
  ;; Standard position.
  (org-test-with-temp-text "* COMMENT Headline"
    (let ((org-comment-string "COMMENT")
	  (headline (org-element-at-point)))
      (should (org-element-property :commentedp headline))
      (should (equal (org-element-property :raw-value headline) "Headline"))))
  ;; Case sensitivity.
  (org-test-with-temp-text "* COMMENT Headline"
    (let* ((org-comment-string "Comment")
	   (headline (org-element-at-point)))
      (should-not (org-element-property :commentedp headline))
      (should (equal (org-element-property :raw-value headline)
		     "COMMENT Headline"))))
  ;; With another keyword.
  (org-test-with-temp-text "* TODO COMMENT Headline"
    (let* ((org-comment-string "COMMENT")
	   (org-todo-keywords '((sequence "TODO" "DONE")))
	   (headline (org-element-at-point)))
      (should (org-element-property :commentedp headline))
      (should (equal (org-element-property :raw-value headline) "Headline"))))
  ;; With the keyword only.
  (org-test-with-temp-text "* COMMENT"
    (let* ((org-comment-string "COMMENT")
	   (headline (org-element-at-point)))
      (should (org-element-property :commentedp headline))
      (should (equal (org-element-property :raw-value headline) "")))))

(ert-deftest test-org-element/headline-archive-tag ()
  "Test ARCHIVE tag recognition."
  ;; Reference test.
  (org-test-with-temp-text "* Headline"
    (let ((org-archive-tag "ARCHIVE"))
      (should-not (org-element-property :archivedp (org-element-at-point)))))
  ;; Single tag.
  (org-test-with-temp-text "* Headline :ARCHIVE:"
    (let ((org-archive-tag "ARCHIVE"))
      (let ((headline (org-element-at-point)))
	(should (org-element-property :archivedp headline))
	;; Test tag removal.
	(should-not (org-element-property :tags headline))))
    (let ((org-archive-tag "Archive"))
      (should-not (org-element-property :archivedp (org-element-at-point)))))
  ;; Multiple tags.
  (org-test-with-temp-text "* Headline :test:ARCHIVE:"
    (let ((org-archive-tag "ARCHIVE"))
      (let ((headline (org-element-at-point)))
	(should (org-element-property :archivedp headline))
	;; Test tag removal.
	(should (equal (org-element-property :tags headline) '("test")))))))

(ert-deftest test-org-element/headline-properties ()
  "Test properties from property drawer."
  ;; All properties from property drawer have their symbol upper
  ;; cased.
  (should
   (org-test-with-temp-text "* Headline\n:PROPERTIES:\n:foo: bar\n:END:"
     (org-element-property :FOO (org-element-at-point))))
  (should-not
   (org-test-with-temp-text "* Headline\n:PROPERTIES:\n:foo: bar\n:END:"
     (org-element-property :foo (org-element-at-point)))))


;;;; Horizontal Rule

(ert-deftest test-org-element/horizontal-rule-parser ()
  "Test `horizontal-rule' parser."
  ;; Standard.
  (should
   (org-test-with-temp-text "-----"
     (org-element-map (org-element-parse-buffer) 'horizontal-rule 'identity)))
  ;; Indented.
  (should
   (org-test-with-temp-text "  -----"
     (org-element-map (org-element-parse-buffer) 'horizontal-rule 'identity)))
  ;; Hyphen must be alone on the line.
  (should-not
   (org-test-with-temp-text "-----wrong"
     (org-element-map (org-element-parse-buffer) 'horizontal-rule 'identity)))
  ;; 4 hyphens is too small.
  (should-not
   (org-test-with-temp-text "----"
     (org-element-map (org-element-parse-buffer) 'horizontal-rule 'identity)))
  ;; Handle non-empty blank line at the end of buffer.
  (should
   (org-test-with-temp-text "-----\n "
     (= (org-element-property :end (org-element-at-point)) (point-max)))))


;;;; Inline Babel Call

(ert-deftest test-org-element/inline-babel-call-parser ()
  "Test `inline-babel-call' parser."
  (should
   (org-test-with-temp-text "call_test()"
     (org-element-map
      (org-element-parse-buffer) 'inline-babel-call 'identity))))


;;;; Inline Src Block

(ert-deftest test-org-element/inline-src-block-parser ()
  "Test `inline-src-block' parser."
  (should
   (org-test-with-temp-text "src_emacs-lisp{(+ 1 1)}"
     (org-element-map (org-element-parse-buffer) 'inline-src-block 'identity)))
  ;; Test parsing at the beginning of an item.
  (should
   (org-test-with-temp-text "- src_emacs-lisp{(+ 1 1)}"
     (org-element-map (org-element-parse-buffer) 'inline-src-block 'identity))))


;;;; Inlinetask

(ert-deftest test-org-element/inlinetask-parser ()
  "Test `inlinetask' parser."
  (when (featurep 'org-inlinetask)
    (let ((org-inlinetask-min-level 15))
      ;; Regular inlinetask.
      (should
       (eq 'inlinetask
	   (org-test-with-temp-text
	       "*************** Task\nTest\n*************** END"
	     (org-element-type (org-element-at-point)))))
      ;; Degenerate inlinetask.
      (should
       (eq 'inlinetask
	   (org-test-with-temp-text "*************** Task"
	     (org-element-type (org-element-at-point)))))
      ;; Mixed inlinetasks.
      (should-not
       (org-test-with-temp-text
	   "
*************** Task
*************** Task2
Contents
*************** END"
	 (forward-line)
	 (goto-char (org-element-property :end (org-element-at-point)))
	 (eobp)))
      ;; TODO keyword.
      (should
       (equal
	"TODO"
	(let ((org-todo-keywords '((sequence "TODO" "DONE"))))
	  (org-test-with-temp-text "*************** TODO Task"
	    (org-element-property :todo-keyword (org-element-at-point))))))
      ;; Planning info.
      (should
       (org-test-with-temp-text "
*************** Task
DEADLINE: <2012-03-29 thu.>"
	 (forward-line)
	 (org-element-property :deadline (org-element-at-point))))
      ;; Priority.
      (should
       (eq
	?A
	(org-test-with-temp-text "
*************** [#A] Task"
	  (forward-line)
	  (org-element-property :priority (org-element-at-point)))))
      ;; Tags.
      (should
       (equal
	'("test")
	(org-test-with-temp-text "
*************** Task :test:"
	  (forward-line)
	  (org-element-property :tags (org-element-at-point)))))
      ;; Regular properties are accessed through upper case keywords.
      (should
       (org-test-with-temp-text "
*************** Task
:PROPERTIES:
:foo: bar
:END:
*************** END"
	 (forward-line)
	 (org-element-property :FOO (org-element-at-point))))
      (should-not
       (org-test-with-temp-text "
*************** Task
:PROPERTIES:
:foo: bar
:END:
*************** END"
	 (forward-line)
	 (org-element-property :foo (org-element-at-point))))
      ;; Handle non-empty blank line at the end of buffer.
      (should
       (org-test-with-temp-text "*************** Task\n*************** END\n "
	 (= (org-element-property :end (org-element-at-point)) (point-max)))))))


;;;; Italic

(ert-deftest test-org-element/italic-parser ()
  "Test `italic' parser."
  ;; Regular test.
  (should
   (let ((org-emph-re "\\([ 	('\"{]\\|^\\)\\(\\([+*/_=~]\\)\\([^ 	\n,\"']\\|[^ 	\n,\"'].*?\\(?:\n.*?\\)\\{0,1\\}[^ 	\n,\"']\\)\\3\\)\\([- 	.,:!?;'\")}\\]\\|$\\)"))
     (org-test-with-temp-text "/italic/"
       (org-element-map (org-element-parse-buffer) 'italic 'identity nil t))))
  ;; Multi-line markup.
  (should
   (equal
    (org-element-contents
     (let ((org-emph-re "\\([ 	('\"{]\\|^\\)\\(\\([+*/_=~]\\)\\([^ 	\n,\"']\\|[^ 	\n,\"'].*?\\(?:\n.*?\\)\\{0,1\\}[^ 	\n,\"']\\)\\3\\)\\([- 	.,:!?;'\")}\\]\\|$\\)"))
       (org-test-with-temp-text "/first line\nsecond line/"
	 (org-element-map (org-element-parse-buffer) 'italic 'identity nil t))))
    '("first line\nsecond line"))))


;;;; Item

(ert-deftest test-org-element/item-parser ()
  "Test `item' parser."
  ;; Standard test.
  (should
   (org-test-with-temp-text "- item"
     (org-element-map (org-element-parse-buffer) 'item 'identity)))
  ;; Counter.
  (should
   (= 6
      (org-element-property
       :counter
       (org-test-with-temp-text "6. [@6] item"
	 (org-element-map (org-element-parse-buffer) 'item 'identity nil t)))))
  ;; Tag
  (should
   (equal
    '("tag")
    (org-element-property
     :tag
     (org-test-with-temp-text "- tag :: description"
       (org-element-map (org-element-parse-buffer) 'item 'identity nil t)))))
  ;; No tags in ordered lists.
  (should-not
   (org-element-property
    :tag
    (org-test-with-temp-text "1. tag :: description"
      (org-element-map (org-element-parse-buffer) 'item 'identity nil t))))
  ;; Check-boxes
  (should
   (equal
    '(trans on off)
    (org-test-with-temp-text "
- [-] item 1
  - [X] item 1.1
  - [ ] item 1.2"
      (org-element-map (org-element-parse-buffer) 'item
	(lambda (item) (org-element-property :checkbox item))))))
  ;; Folded state.
  (org-test-with-temp-text "* Headline
- item

  paragraph below"
    (forward-line)
    (let ((org-cycle-include-plain-lists t)) (org-cycle))
    (should
     (org-element-property
      :hiddenp
      (org-element-map (org-element-parse-buffer) 'item 'identity nil t))))
  ;; Item starting with special syntax.
  (should
   (equal '(("- item"))
	  (org-test-with-temp-text "- - item"
	    (org-element-map (org-element-parse-buffer) 'paragraph
	      'org-element-contents))))
  ;; Block in an item: ignore indentation within the block.
  (should
   (org-test-with-temp-text "- item\n  #+begin_src emacs-lisp\n(+ 1 1)\n  #+end_src"
     (forward-char)
     (= (org-element-property :end (org-element-at-point)) (point-max)))))


;;;; Keyword

(ert-deftest test-org-element/keyword-parser ()
  "Test `keyword' parser."
  ;; Standard test.
  (should
   (org-test-with-temp-text "#+KEYWORD: value"
     (org-element-map (org-element-parse-buffer) 'keyword 'identity)))
  ;; Keywords are case-insensitive.
  (should
   (org-test-with-temp-text "#+keyword: value"
     (org-element-map (org-element-parse-buffer) 'keyword 'identity)))
  ;; Affiliated keywords are not keywords.
  (should-not
   (org-test-with-temp-text "#+NAME: value
Paragraph"
     (org-element-map (org-element-parse-buffer) 'keyword 'identity)))
  ;; Do not mix keywords with Babel calls and dynamic blocks.
  (should-not
   (org-test-with-temp-text "#+CALL: fun()"
     (org-element-map (org-element-parse-buffer) 'keyword 'identity)))
  (should-not
   (org-test-with-temp-text "#+BEGIN: my-fun\nBody\n#+END:"
     (org-element-map (org-element-parse-buffer) 'keyword 'identity)))
  ;; Handle non-empty blank line at the end of buffer.
  (should
   (org-test-with-temp-text "#+KEYWORD: value\n "
     (= (org-element-property :end (org-element-at-point)) (point-max)))))


;;;; Latex Environment

(ert-deftest test-org-element/latex-environment-parser ()
  "Test `latex-environment' parser."
  (should
   (org-test-with-temp-text "\\begin{equation}\ne^{i\\pi}+1=0\n\\end{equation}"
     (org-element-map (org-element-parse-buffer) 'latex-environment 'identity)))
  ;; Allow nested environments.
  (should
   (equal
    "\\begin{outer}
\\begin{inner}
e^{i\\pi}+1=0
\\end{inner}
\\end{outer}"
    (org-test-with-temp-text "
\\begin{outer}
\\begin{inner}
e^{i\\pi}+1=0
\\end{inner}
\\end{outer}"
      (org-element-property
       :value
       (org-element-map
	   (org-element-parse-buffer) 'latex-environment 'identity nil t)))))
  ;; Allow environments with options and arguments.
  (should
   (eq 'latex-environment
       (org-test-with-temp-text
	   "\\begin{theorem}[Euler]\ne^{i\\pi}+1=0\n\\end{theorem}"
	 (org-element-type (org-element-at-point)))))
  (should
   (eq 'latex-environment
       (org-test-with-temp-text "\\begin{env}{arg}\nvalue\n\\end{env}"
	 (org-element-type (org-element-at-point)))))
  (should-not
   (eq 'latex-environment
       (org-test-with-temp-text "\\begin{env}{arg} something\nvalue\n\\end{env}"
	 (org-element-type (org-element-at-point)))))
  ;; Handle non-empty blank line at the end of buffer.
  (should
   (org-test-with-temp-text "\\begin{env}\n\\end{env}\n "
     (= (org-element-property :end (org-element-at-point)) (point-max)))))


;;;; Latex Fragment

(ert-deftest test-org-element/latex-fragment-parser ()
  "Test `latex-fragment' parser."
  (should
   (org-test-with-temp-text "$a$"
     (org-element-map (org-element-parse-buffer) 'latex-fragment 'identity)))
  (should
   (org-test-with-temp-text "$$a$$"
     (org-element-map (org-element-parse-buffer) 'latex-fragment 'identity)))
  (should
   (org-test-with-temp-text "\\(a\\)"
     (org-element-map (org-element-parse-buffer) 'latex-fragment 'identity)))
  (should
   (org-test-with-temp-text "\\[a\\]"
     (org-element-map
	 (org-element-parse-buffer) 'latex-fragment 'identity)))
  ;; Test fragment at the beginning of an item.
  (should
   (eq 'latex-fragment
       (org-test-with-temp-text "- $x$"
	 (progn (search-forward "$")
		(org-element-type (org-element-context)))))))


;;;; Line Break

(ert-deftest test-org-element/line-break-parser ()
  "Test `line-break' parser."
  ;; Regular test.
  (should
   (org-test-with-temp-text "Text \\\\"
     (org-element-map (org-element-parse-buffer) 'line-break 'identity)))
  ;; Line break with trailing white spaces.
  (should
   (org-test-with-temp-text "Text \\\\  "
     (org-element-map (org-element-parse-buffer) 'line-break 'identity)))
  ;; Three backslashes are too much.
  (should-not
   (org-test-with-temp-text "Text \\\\\\"
     (org-element-map (org-element-parse-buffer) 'line-break 'identity))))


;;;; Link

(ert-deftest test-org-element/link-parser ()
  "Test `link' parser."
  ;; Radio target.
  (should
   (equal
    "radio"
    (org-test-with-temp-text "A radio link"
      (org-element-property
       :type
       (org-element-map
	   (let ((org-target-link-regexp "radio")) (org-element-parse-buffer))
	   'link 'identity nil t)))))
  ;; Standard link.
  ;;
  ;; ... with description.
  (should
   (equal
    '("Orgmode.org")
    (org-test-with-temp-text "[[http://orgmode.org][Orgmode.org]]"
      (org-element-contents
       (org-element-map (org-element-parse-buffer) 'link 'identity nil t)))))
  ;; ... without description.
  (should
   (equal
    "http"
    (org-test-with-temp-text "[[http://orgmode.org]]"
      (org-element-property
       :type
       (org-element-map (org-element-parse-buffer) 'link 'identity nil t)))))
  ;; ... with expansion.
  (should
   (equal
    "//orgmode.org/worg"
    (org-test-with-temp-text "[[Org:worg]]"
      (let ((org-link-abbrev-alist '(("Org" . "http://orgmode.org/"))))
	(org-element-property
	 :path
	 (org-element-map (org-element-parse-buffer) 'link 'identity nil t))))))
  ;; ... with translation.
  (should
   (equal
    "127.0.0.1"
    (org-test-with-temp-text "[[http://orgmode.org]]"
      (flet ((link-translate (type path) (cons type "127.0.0.1")))
	(let ((org-link-translation-function 'link-translate))
	  (org-element-property
	   :path
	   (org-element-map (org-element-parse-buffer) 'link
	     'identity nil t)))))))
  ;; ... id link.
  (should
   (equal
    "id"
    (org-test-with-temp-text "[[id:aaaa]]"
      (org-element-property
       :type
       (org-element-map (org-element-parse-buffer) 'link 'identity nil t)))))
  ;; ... custom-id link.
  (should
   (equal
    "custom-id"
    (org-test-with-temp-text "[[#some-id]]"
      (org-element-property
       :type
       (org-element-map (org-element-parse-buffer) 'link 'identity nil t)))))
  ;; ... coderef link.
  (should
   (equal
    "coderef"
    (org-test-with-temp-text "[[(reference)]]"
      (org-element-property
       :type
       (org-element-map (org-element-parse-buffer) 'link 'identity nil t)))))
  ;; ... fuzzy link.
  (should
   (equal
    "fuzzy"
    (org-test-with-temp-text "[[target-or-title]]"
      (org-element-property
       :type
       (org-element-map (org-element-parse-buffer) 'link 'identity nil t)))))
  ;; ... file-type link with search option.
  (should
   (equal
    '(("file" "projects.org" "*task title"))
    (org-test-with-temp-text "[[file:projects.org::*task title]]"
      (org-element-map (org-element-parse-buffer) 'link
	(lambda (l) (list (org-element-property :type l)
			  (org-element-property :path l)
			  (org-element-property :search-option l)))))))
  ;; ... file-type link with application...
  (should
   (equal
    '("file" "projects.org" "docview")
    (org-test-with-temp-text "[[docview:projects.org]]"
      (let ((l (org-element-context)))
	(list (org-element-property :type l)
	      (org-element-property :path l)
	      (org-element-property :application l))))))
  ;; ... multi-line link.
  (should
   (equal "//orgmode.org"
	  (org-test-with-temp-text "[[http://orgmode.\norg]]"
	    (org-element-property :path (org-element-context)))))
  ;; Plain link.
  (should
   (org-test-with-temp-text "A link: http://orgmode.org"
     (org-element-map (org-element-parse-buffer) 'link 'identity)))
  ;; Angular link.
  (should
   (org-test-with-temp-text "A link: <http://orgmode.org>"
     (org-element-map (org-element-parse-buffer) 'link 'identity nil t)))
  ;; Link abbreviation.
  (should
   (equal "http"
	  (org-test-with-temp-text
	      "#+LINK: orgmode http://www.orgmode.org/\n[[orgmode:#docs]]"
	    (progn (org-mode-restart)
		   (goto-char (1- (point-max)))
		   (org-element-property :type (org-element-context))))))
  ;; Link abbreviation in a secondary string.
  (should
   (equal "http"
	  (org-test-with-temp-text
	      "#+LINK: orgmode http://www.orgmode.org/\n* H [[orgmode:#docs]]"
	    (progn (org-mode-restart)
		   (org-element-map (org-element-parse-buffer) 'link
		     (lambda (link) (org-element-property :type link))
		     nil t nil t)))))
  ;; Plain links are allowed as description of regular links.
  (should
   (equal "file"
	  (org-test-with-temp-text "[[http://orgmode.org][file:unicorn.jpg]]"
	    (search-forward "file:")
	    (org-element-property :type (org-element-context))))))


;;;; Macro

(ert-deftest test-org-element/macro-parser ()
  "Test `macro' parser."
  ;; Without arguments.
  (should
   (org-test-with-temp-text "{{{macro}}}"
     (org-element-map (org-element-parse-buffer) 'macro 'identity)))
  ;; With arguments.
  (should
   (org-test-with-temp-text "{{{macro(arg1,arg2)}}}"
     (org-element-map (org-element-parse-buffer) 'macro 'identity)))
  ;; Properly handle protected commas in arguments...
  (should
   (= 2
      (length
       (org-test-with-temp-text "{{{macro(arg1\\,arg1,arg2)}}}"
	 (org-element-property :args (org-element-context))))))
  ;; ... even when last argument ends with a protected comma.
  (should
   (equal '("C-,")
	  (org-test-with-temp-text "{{{macro(C-\\,)}}}"
	    (org-element-property :args (org-element-context)))))
  ;; Allow to escape escaping character.
  (should
   (equal '("C-\\" "")
	  (org-test-with-temp-text "{{{macro(C-\\\\,)}}}"
	    (org-element-property :args (org-element-context)))))
  ;; No need to escape backslashes elsewhere.
  (should
   (equal '("\\")
	  (org-test-with-temp-text "{{{macro(\\)}}}"
	    (org-element-property :args (org-element-context))))))


;;;; Node Property

(ert-deftest test-org-element/node-property ()
  "Test `node-property' parser."
  ;; Standard test.
  (should
   (equal '("abc" "value")
	  (org-test-with-temp-text ":PROPERTIES:\n<point>:abc: value\n:END:"
	    (let ((element (org-element-at-point)))
	      (list (org-element-property :key element)
		    (org-element-property :value element))))))
  ;; Value should be trimmed.
  (should
   (equal "value"
	  (org-test-with-temp-text ":PROPERTIES:\n<point>:abc: value  \n:END:"
	    (org-element-property :value (org-element-at-point)))))
  ;; A node property requires to be wrapped within a property drawer.
  (should-not
   (eq 'node-property
       (org-test-with-temp-text ":abc: value"
	 (org-element-type (org-element-at-point)))))
  ;; Accept empty properties.
  (should
   (equal '(("foo" "value") ("bar" ""))
	  (org-test-with-temp-text ":PROPERTIES:\n:foo: value\n:bar:\n:END:"
	    (org-element-map (org-element-parse-buffer) 'node-property
	      (lambda (p)
		(list (org-element-property :key p)
		      (org-element-property :value p)))))))
  ;; Ignore all non-property lines in property drawers.
  (should
   (equal
    '(("foo" "value"))
    (org-test-with-temp-text ":PROPERTIES:\nWrong1\n:foo: value\nWrong2\n:END:"
      (org-element-map (org-element-parse-buffer) 'node-property
	(lambda (p)
	  (list (org-element-property :key p)
		(org-element-property :value p))))))))


;;;; Paragraph

(ert-deftest test-org-element/paragraph-parser ()
  "Test `paragraph' parser."
  ;; Standard test.
  (should
   (org-test-with-temp-text "Paragraph"
     (org-element-map (org-element-parse-buffer) 'paragraph 'identity nil t)))
  ;; Property find end of a paragraph stuck to another element.
  (should
   (eq ?#
       (org-test-with-temp-text "Paragraph\n# Comment"
	 (org-element-map (org-element-parse-buffer) 'paragraph
	   (lambda (p) (char-after (org-element-property :end p)))
	   nil t))))
  ;; Include ill-formed Keywords.
  (should
   (org-test-with-temp-text "#+wrong_keyword something"
     (org-element-map (org-element-parse-buffer) 'paragraph 'identity)))
  ;; Include incomplete-drawers.
  (should
   (let ((org-drawers '("TEST")))
     (org-test-with-temp-text ":TEST:\nParagraph"
       (let ((elem (org-element-at-point)))
	 (and (eq (org-element-type elem) 'paragraph)
	      (= (point-max) (org-element-property :end elem)))))))
  ;; Include non-existent drawers.
  (should
   (let ((org-drawers '("TEST")))
     (org-test-with-temp-text ":NONAME:"
       (org-element-map (org-element-parse-buffer) 'paragraph 'identity))))
  ;; Include incomplete blocks.
  (should
   (org-test-with-temp-text "#+BEGIN_CENTER\nParagraph"
     (let ((elem (org-element-at-point)))
       (and (eq (org-element-type elem) 'paragraph)
	    (= (point-max) (org-element-property :end elem))))))
  ;; Include incomplete dynamic blocks.
  (should
   (org-test-with-temp-text "#+BEGIN: \nParagraph"
     (let ((elem (org-element-at-point)))
       (and (eq (org-element-type elem) 'paragraph)
	    (= (point-max) (org-element-property :end elem))))))
  ;; Include incomplete latex environments.
  (should
   (org-test-with-temp-text "\begin{equation}\nParagraph"
     (let ((elem (org-element-at-point)))
       (and (eq (org-element-type elem) 'paragraph)
	    (= (point-max) (org-element-property :end elem))))))
  ;; Do not steal affiliated keywords from container.
  (should
   (org-test-with-temp-text "#+ATTR_LATEX: test\n- item 1"
     (let ((elem (progn (search-forward "item") (org-element-at-point))))
       (and (eq (org-element-type elem) 'paragraph)
	    (not (org-element-property :attr_latex elem))
	    (/= (org-element-property :begin elem) 1)))))
  ;; Handle non-empty blank line at the end of buffer.
  (should
   (org-test-with-temp-text "#+BEGIN_CENTER\nC\n#+END_CENTER\n  "
     (= (org-element-property :end (org-element-at-point)) (point-max)))))


;;;; Plain List

(ert-deftest test-org-element/plain-list-parser ()
  "Test `plain-list' parser."
  (org-test-with-temp-text "- item"
    (should (org-element-map (org-element-parse-buffer) 'plain-list 'identity)))
  ;; Blank lines after the list only belong to outer plain list.
  (should
   (equal
    '(t t)
    (org-test-with-temp-text "
- outer
  - inner

Outside list"
      (let ((endings (org-element-map (org-element-parse-buffer) 'plain-list
		       (lambda (pl) (org-element-property :end pl)))))
	(list
	 ;; Move to ending of outer list.
	 (progn (goto-char (car endings)) (looking-at "Outside list"))
	 ;; Move to ending of inner list.
	 (progn (goto-char (nth 1 endings)) (looking-at "^$")))))))
  ;; Correctly compute end of list if it doesn't end at a line
  ;; beginning.
  (should
   (org-test-with-temp-text "- list\n   \n   "
     (= (org-element-property :end (org-element-at-point)) (point-max)))))


;;;; Planning

(ert-deftest test-org-element/planning-parser ()
  "Test `planning' parser."
  (should
   (equal "[2012-03-29 thu.]"
	  (org-element-property
	   :raw-value
	   (org-element-property
	    :closed
	    (org-test-with-temp-text "CLOSED: [2012-03-29 thu.]"
	      (org-element-at-point))))))
  (should
   (equal "<2012-03-29 thu.>"
	  (org-element-property
	   :raw-value
	   (org-element-property
	    :deadline
	    (org-test-with-temp-text "DEADLINE: <2012-03-29 thu.>"
	      (org-element-at-point))))))
  (should
   (equal "<2012-03-29 thu.>"
	  (org-element-property
	   :raw-value
	   (org-element-property
	    :scheduled
	    (org-test-with-temp-text "SCHEDULED: <2012-03-29 thu.>"
	      (org-element-at-point)))))))


;;;; Property Drawer

(ert-deftest test-org-element/property-drawer-parser ()
  "Test `property-drawer' parser."
  ;; Standard test.
  (should
   (let ((org-drawers '("PROPERTIES")))
     (org-test-with-temp-text ":PROPERTIES:\n:prop: value\n:END:"
       (org-element-map (org-element-parse-buffer) 'property-drawer
	 'identity nil t))))
  ;; Do not mix property drawers and regular drawers.
  (should-not
   (let ((org-drawers '("TEST")))
     (org-test-with-temp-text ":TEST:\n:prop: value\n:END:"
       (org-element-map (org-element-parse-buffer) 'property-drawer
	 'identity nil t))))
  ;; Ignore incomplete drawer.
  (should-not
   (let ((org-drawers '("PROPERTIES")))
     (org-test-with-temp-text ":PROPERTIES:\n:prop: value"
       (org-element-map (org-element-parse-buffer) 'property-drawer
	 'identity nil t))))
  ;; Handle non-empty blank line at the end of buffer.
  (should
   (org-test-with-temp-text ":PROPERTIES:\n:END:\n "
     (= (org-element-property :end (org-element-at-point)) (point-max)))))


;;;; Quote Block

(ert-deftest test-org-element/quote-block-parser ()
  "Test `quote-block' parser."
  ;; Regular test.
  (should
   (org-test-with-temp-text "#+BEGIN_QUOTE\nText\n#+END_QUOTE"
     (org-element-map (org-element-parse-buffer) 'quote-block 'identity)))
  ;; Test folded block.
  (org-test-with-temp-text "#+BEGIN_QUOTE\nText\n#+END_QUOTE"
    (org-cycle)
    (should
     (org-element-property
      :hiddenp
      (org-element-map (org-element-parse-buffer) 'quote-block
	'identity nil t))))
  ;; Ignore incomplete block.
  (should-not
   (org-test-with-temp-text "#+BEGIN_QUOTE"
     (org-element-map (org-element-parse-buffer) 'quote-block 'identity nil t)))
  ;; Handle non-empty blank line at the end of buffer.
  (should
   (org-test-with-temp-text "#+BEGIN_QUOTE\nC\n#+END_QUOTE\n  "
     (= (org-element-property :end (org-element-at-point)) (point-max)))))


;;;; Quote Section

(ert-deftest test-org-element/quote-section-parser ()
  "Test `quote-section' parser."
  (should
   (let ((org-quote-string "QUOTE"))
     (org-test-with-temp-text "* QUOTE Headline\nBody"
       (org-element-map (org-element-parse-buffer) 'quote-section 'identity))))
  (should-not
   (let ((org-quote-string "TEST"))
     (org-test-with-temp-text "* QUOTE Headline\nBody"
       (org-element-map (org-element-parse-buffer) 'quote-section 'identity)))))


;;;; Radio Target

(ert-deftest test-org-element/radio-target-parser ()
  "Test `radio-target' parser."
  ;; Standard test.
  (should
   (eq 'radio-target
       (org-test-with-temp-text "<<<radio>>>"
	 (org-element-type (org-element-context)))))
  ;; Radio targets with objects.
  (should
   (eq 'radio-target
       (org-test-with-temp-text "<<<radio \\alpha>>>"
	 (org-element-type (org-element-context)))))
  ;; Radio targets starting with an object.
  (should
   (eq 'radio-target
       (org-test-with-temp-text "<<<\\alpha radio>>>"
	 (org-element-type (org-element-context))))))


;;;; Section

(ert-deftest test-org-element/section-parser ()
  "Test `section' parser."
  ;; Standard test.
  (should
   (org-test-with-temp-text "* Headline\nText"
     (org-element-map (org-element-parse-buffer) 'section 'identity)))
  ;; There's a section before the first headline.
  (should
   (org-test-with-temp-text "Text"
     (org-element-map (org-element-parse-buffer) 'section 'identity)))
  ;; A section cannot be empty.
  (should-not
   (org-test-with-temp-text "* Headline 1\n* Headline 2"
     (org-element-map (org-element-parse-buffer) 'section 'identity)))
  ;; A section doesn't contain sub-trees.
  (should-not
   (org-test-with-temp-text "* Head\nText\n** Sub-Head"
     (org-element-map
      (org-element-map (org-element-parse-buffer) 'section 'identity nil t)
      'headline 'identity))))


;;;; Special Block

(ert-deftest test-org-element/special-block-parser ()
  "Test `special-block' parser."
  ;; Standard test.
  (should
   (equal "SPECIAL"
	  (org-test-with-temp-text "#+BEGIN_SPECIAL\nText\n#+END_SPECIAL"
	    (org-element-property :type (org-element-at-point)))))
  ;; Special blocks can contain paragraphs.
  (should
   (eq 'paragraph
       (org-test-with-temp-text "#+BEGIN_SPECIAL\nText\n#+END_SPECIAL"
	 (forward-line)
	 (org-element-type (org-element-at-point)))))
  ;; Test folded block.
  (should
   (org-test-with-temp-text "#+BEGIN_SPECIAL\nText\n#+END_SPECIAL"
     (org-cycle)
     (org-element-property :hiddenp (org-element-at-point))))
  ;; Ignore incomplete block.
  (should-not
   (eq 'special-block
       (org-test-with-temp-text "#+BEGIN_SPECIAL"
	 (org-element-type (org-element-at-point)))))
  ;; Allow special characters in type.
  (should
   (equal '(special-block "SPECIAL*")
	  (org-test-with-temp-text "#+BEGIN_SPECIAL*\nContents\n#+END_SPECIAL*"
	    (let ((element (org-element-at-point)))
	      (list (org-element-type element)
		    (org-element-property :type element))))))
  ;; Handle non-empty blank line at the end of buffer.
  (should
   (org-test-with-temp-text "#+BEGIN_SPECIAL\nC\n#+END_SPECIAL\n "
     (= (org-element-property :end (org-element-at-point)) (point-max)))))


;;;; Src Block

(ert-deftest test-org-element/src-block-parser ()
  "Test `src-block' parser."
  ;; Regular tests.
  (should
   (org-test-with-temp-text "#+BEGIN_SRC org\nText\n#+END_SRC"
     (org-element-map (org-element-parse-buffer) 'src-block 'identity)))
  ;; Test folded block.
  (should
   (org-test-with-temp-text "#+BEGIN_SRC org\nText\n#+END_SRC"
     (org-cycle)
     (org-element-property :hiddenp (org-element-at-point))))
  ;; Ignore incomplete block.
  (should-not
   (org-test-with-temp-text "#+BEGIN_SRC"
     (org-element-map (org-element-parse-buffer) 'src-block 'identity)))
  ;; Properly un-escape code.
  (should
   (equal "* Headline\n #+keyword\nText\n"
	  (org-test-with-temp-text
	      "#+BEGIN_SRC org\n,* Headline\n ,#+keyword\nText\n#+END_SRC"
	    (org-element-property :value (org-element-at-point)))))
  ;; Nil `org-src-preserve-indentation': Remove maximum common
  ;; indentation.
  (should
   (equal " L1\nL2\n"
	  (org-test-with-temp-text "#+BEGIN_SRC org\n  L1\n L2\n#+END_SRC"
	    (let ((org-src-preserve-indentation nil))
	      (org-element-property :value (org-element-at-point))))))
  ;; Non-nil `org-src-preserve-indentation': Remove block indentation
  ;; only, unless block contents are less indented than block
  ;; boundaries.
  (should
   (equal " L1\nL2\n"
	  (org-test-with-temp-text " #+BEGIN_SRC org\n  L1\n L2\n #+END_SRC"
	    (let ((org-src-preserve-indentation t))
	      (org-element-property :value (org-element-at-point))))))
  (should
   (equal
    "  L1\n L2\n"
    (org-test-with-temp-text "  #+BEGIN_SRC org\n  L1\n L2\n  #+END_SRC"
      (let ((org-src-preserve-indentation t))
	(org-element-property :value (org-element-at-point))))))
  ;; Handle non-empty blank line at the end of buffer.
  (should
   (org-test-with-temp-text "#+BEGIN_SRC emacs-lisp\nC\n#+END_SRC\n "
     (= (org-element-property :end (org-element-at-point)) (point-max)))))


;;;; Statistics Cookie

(ert-deftest test-org-element/statistics-cookie ()
  "Test `statistics-cookie' parser."
  ;; With numbers.
  (should
   (org-test-with-temp-text "[1/2]"
     (org-element-map (org-element-parse-buffer) 'statistics-cookie 'identity)))
  ;; With percents.
  (should
   (org-test-with-temp-text "[33%]"
     (org-element-map
      (org-element-parse-buffer) 'statistics-cookie 'identity))))


;;;; Strike Through

(ert-deftest test-org-element/strike-through-parser ()
  "Test `strike-through' parser."
  ;; Regular test.
  (should
   (let ((org-emph-re "\\([ 	('\"{]\\|^\\)\\(\\([+*/_=~]\\)\\([^ 	\n,\"']\\|[^ 	\n,\"'].*?\\(?:\n.*?\\)\\{0,1\\}[^ 	\n,\"']\\)\\3\\)\\([- 	.,:!?;'\")}\\]\\|$\\)"))
     (org-test-with-temp-text "+strike-through+"
       (org-element-map (org-element-parse-buffer) 'strike-through 'identity))))
  ;; Multi-line markup.
  (should
   (equal
    (org-element-contents
     (let ((org-emph-re "\\([ 	('\"{]\\|^\\)\\(\\([+*/_=~]\\)\\([^ 	\n,\"']\\|[^ 	\n,\"'].*?\\(?:\n.*?\\)\\{0,1\\}[^ 	\n,\"']\\)\\3\\)\\([- 	.,:!?;'\")}\\]\\|$\\)"))
       (org-test-with-temp-text "+first line\nsecond line+"
	 (org-element-map
	  (org-element-parse-buffer) 'strike-through 'identity nil t))))
    '("first line\nsecond line"))))


;;;; Subscript

(ert-deftest test-org-element/subscript-parser ()
  "Test `subscript' parser."
  ;; Without braces.
  (should
   (org-test-with-temp-text "a_b"
     (org-element-map (org-element-parse-buffer) 'subscript 'identity)))
  ;; With braces.
  (should
   (org-test-with-temp-text "a_{b}"
     (org-element-map (org-element-parse-buffer) 'subscript 'identity)))
  ;; Multiple subscripts in a paragraph.
  (should
   (= 2
      (org-test-with-temp-text "a_b and c_d"
	(length
	 (org-element-map (org-element-parse-buffer) 'subscript 'identity))))))


;;;; Superscript

(ert-deftest test-org-element/superscript-parser ()
  "Test `superscript' parser."
  ;; Without braces.
  (should
   (org-test-with-temp-text "a^b"
     (org-element-map (org-element-parse-buffer) 'superscript 'identity)))
  ;; With braces.
  (should
   (org-test-with-temp-text "a^{b}"
     (org-element-map (org-element-parse-buffer) 'superscript 'identity)))
  ;; Multiple superscript in a paragraph.
  (should
   (= 2
      (org-test-with-temp-text "a^b and c^d"
	(length
	 (org-element-map
	  (org-element-parse-buffer) 'superscript 'identity))))))


;;;; Table

(ert-deftest test-org-element/table-parser ()
  "Test `table' parser."
  (should
   (org-test-with-temp-text "| a |"
     (org-element-map (org-element-parse-buffer) 'table 'identity)))
  ;; TBLFM keyword is case insensitive.
  (should
   (org-test-with-temp-text "| a |\n#+tblfm: test"
     (org-element-property
      :tblfm
      (org-element-map (org-element-parse-buffer) 'table 'identity nil t))))
  ;; Handle multiple TBLFM lines.
  (should
   (= 2
      (org-test-with-temp-text "| a |\n#+TBLFM: test1\n#+TBLFM: test2"
	(length (org-element-property
		 :tblfm
		 (org-element-map
		     (org-element-parse-buffer) 'table 'identity nil t))))))
  ;; Handle non-empty blank line at the end of buffer.
  (should
   (org-test-with-temp-text "| a |\n "
     (= (org-element-property :end (org-element-at-point)) (point-max)))))


;;;; Table Cell

(ert-deftest test-org-element/table-cell-parser ()
  "Test `table-cell' parser."
  ;; Regular table cell.
  (should
   (org-test-with-temp-text "| a |"
     (org-element-map (org-element-parse-buffer) 'table-cell 'identity)))
  ;; Last vertical bar may be omitted.
  (should
   (org-test-with-temp-text "| a "
     (org-element-map (org-element-parse-buffer) 'table-cell 'identity))))


;;;; Table Row

(ert-deftest test-org-element/table-row-parser ()
  "Test `table-row' parser."
  (should
   (equal '(standard rule)
	  (org-test-with-temp-text "| a |\n|---|"
	    (org-element-map
	     (org-element-parse-buffer) 'table-row
	     (lambda (row) (org-element-property :type row)))))))


;;;; Target

(ert-deftest test-org-element/target-parser ()
  "Test `target' parser."
  (should
   (org-test-with-temp-text "<<target>>"
     (org-element-map (org-element-parse-buffer) 'target 'identity))))


;;;; Timestamp

(ert-deftest test-org-element/timestamp ()
  "Test `timestamp' parser."
  ;; Active timestamp.
  (should
   (org-test-with-temp-text "<2012-03-29 16:40>"
     (eq (org-element-property :type (org-element-context)) 'active)))
  (should-not
   (org-test-with-temp-text "<2012-03-29 Thu>"
     (let ((timestamp (org-element-context)))
       (or (org-element-property :hour-start timestamp)
	   (org-element-property :minute-start timestamp)))))
  (should
   (equal '(2012 3 29 16 40)
	  (org-test-with-temp-text "<2012-03-29 Thu 16:40>"
	    (let ((object (org-element-context)))
	      (list (org-element-property :year-start object)
		    (org-element-property :month-start object)
		    (org-element-property :day-start object)
		    (org-element-property :hour-start object)
		    (org-element-property :minute-start object))))))
  ;; Inactive timestamp.
  (should
   (org-test-with-temp-text "[2012-03-29 Thu 16:40]"
     (eq (org-element-property :type (org-element-context)) 'inactive)))
  ;; Time range.
  (should
   (equal '(2012 3 29 16 40 7 30)
	  (org-test-with-temp-text "<2012-03-29 Thu 7:30-16:40>"
	    (let ((object (org-element-context)))
	      (list (org-element-property :year-end object)
		    (org-element-property :month-end object)
		    (org-element-property :day-end object)
		    (org-element-property :hour-end object)
		    (org-element-property :minute-end object)
		    (org-element-property :hour-start object)
		    (org-element-property :minute-start object))))))
  (should
   (eq 'active-range
       (org-test-with-temp-text "<2012-03-29 Thu 7:30-16:40>"
	 (org-element-property :type (org-element-context)))))
  ;; Date range.
  (should
   (org-test-with-temp-text "[2012-03-29 Thu 16:40]--[2012-03-29 Thu 16:41]"
     (eq (org-element-property :type (org-element-context)) 'inactive-range)))
  (should-not
   (org-test-with-temp-text "[2011-07-14 Thu]--[2012-03-29 Thu]"
     (let ((timestamp (org-element-context)))
       (or (org-element-property :hour-end timestamp)
	   (org-element-property :minute-end timestamp)))))
  ;; With repeater, warning delay and both.
  (should
   (eq 'catch-up
       (org-test-with-temp-text "<2012-03-29 Thu ++1y>"
	 (org-element-property :repeater-type (org-element-context)))))
  (should
   (eq 'first
       (org-test-with-temp-text "<2012-03-29 Thu --1y>"
	 (org-element-property :warning-type (org-element-context)))))
  (should
   (equal '(cumulate all)
	  (org-test-with-temp-text "<2012-03-29 Thu +1y -1y>"
	    (let ((ts (org-element-context)))
	      (list (org-element-property :repeater-type ts)
		    (org-element-property :warning-type ts))))))
  ;; Timestamps are not planning elements.
  (should-not
   (org-test-with-temp-text "SCHEDULED: <2012-03-29 Thu 16:40>"
     (org-element-map (org-element-parse-buffer) 'timestamp 'identity))))


;;;; Underline

(ert-deftest test-org-element/underline-parser ()
  "Test `underline' parser."
  ;; Regular test.
  (should
   (let ((org-emph-re "\\([ 	('\"{]\\|^\\)\\(\\([+*/_=~]\\)\\([^ 	\n,\"']\\|[^ 	\n,\"'].*?\\(?:\n.*?\\)\\{0,1\\}[^ 	\n,\"']\\)\\3\\)\\([- 	.,:!?;'\")}\\]\\|$\\)"))
     (org-test-with-temp-text "_underline_"
       (org-element-map (org-element-parse-buffer) 'underline 'identity))))
  ;; Multi-line markup.
  (should
   (equal
    (org-element-contents
     (let ((org-emph-re "\\([ 	('\"{]\\|^\\)\\(\\([+*/_=~]\\)\\([^ 	\n,\"']\\|[^ 	\n,\"'].*?\\(?:\n.*?\\)\\{0,1\\}[^ 	\n,\"']\\)\\3\\)\\([- 	.,:!?;'\")}\\]\\|$\\)"))
       (org-test-with-temp-text "_first line\nsecond line_"
	 (org-element-map
	  (org-element-parse-buffer) 'underline 'identity nil t))))
    '("first line\nsecond line"))))


;;;; Verbatim

(ert-deftest test-org-element/verbatim-parser ()
  "Test `verbatim' parser."
  ;; Regular test.
  (should
   (let ((org-emph-re "\\([ 	('\"{]\\|^\\)\\(\\([+*/_=~]\\)\\([^ 	\n,\"']\\|[^ 	\n,\"'].*?\\(?:\n.*?\\)\\{0,1\\}[^ 	\n,\"']\\)\\3\\)\\([- 	.,:!?;'\")}\\]\\|$\\)"))
     (org-test-with-temp-text "=verbatim="
       (org-element-map (org-element-parse-buffer) 'verbatim 'identity))))
  ;; Multi-line markup.
  (should
   (equal
    (org-element-property
     :value
     (let ((org-emph-re "\\([ 	('\"{]\\|^\\)\\(\\([+*/_=~]\\)\\([^ 	\n,\"']\\|[^ 	\n,\"'].*?\\(?:\n.*?\\)\\{0,1\\}[^ 	\n,\"']\\)\\3\\)\\([- 	.,:!?;'\")}\\]\\|$\\)"))
       (org-test-with-temp-text "=first line\nsecond line="
	 (org-element-map
	  (org-element-parse-buffer) 'verbatim 'identity nil t))))
    "first line\nsecond line")))


;;;; Verse Block

(ert-deftest test-org-element/verse-block-parser ()
  "Test `verse-block' parser."
  ;; Standard test.
  (org-test-with-temp-text "#+BEGIN_VERSE\nVerse block\n#+END_VERSE"
    (should
     (org-element-map (org-element-parse-buffer) 'verse-block 'identity)))
  ;; Ignore case.
  (org-test-with-temp-text "#+begin_verse\nVerse block\n#+end_verse"
    (should
     (org-element-map (org-element-parse-buffer) 'verse-block 'identity)))
  ;; Parse folding.
  (org-test-with-temp-text "#+BEGIN_VERSE\nVerse block\n#+END_VERSE"
    (org-hide-block-all)
    (should
     (org-element-property
      :hiddenp
      (org-element-map (org-element-parse-buffer) 'verse-block
	'identity nil t))))
  ;; Parse objects in verse blocks.
  (org-test-with-temp-text "#+BEGIN_VERSE\nVerse \\alpha\n#+END_VERSE"
    (should (org-element-map (org-element-parse-buffer) 'entity 'identity)))
  ;; Ignore incomplete verse block.
  (should-not
   (org-test-with-temp-text "#+BEGIN_VERSE"
     (org-element-map (org-element-parse-buffer) 'verse-block 'identity nil t)))
  ;; Handle non-empty blank line at the end of buffer.
  (should
   (org-test-with-temp-text "#+BEGIN_VERSE\nC\n#+END_VERSE\n "
     (= (org-element-property :end (org-element-at-point)) (point-max)))))



;;; Test Interpreters.

(ert-deftest test-org-element/affiliated-keywords-interpreter ()
  "Test if affiliated keywords are correctly interpreted."
  ;; Interpret simple keywords.
  (should
   (equal
    (org-element-interpret-data
     '(org-data nil (paragraph (:name "para") "Paragraph")))
    "#+NAME: para\nParagraph\n"))
  ;; Interpret multiple keywords.
  (should
   (equal
    (org-element-interpret-data
     '(org-data nil (paragraph (:attr_ascii ("line2" "line1")) "Paragraph")))
    "#+ATTR_ASCII: line1\n#+ATTR_ASCII: line2\nParagraph\n"))
  ;; Interpret parsed keywords.
  (should
   (equal
    (org-element-interpret-data
     '(org-data nil (paragraph (:caption (("caption"))) "Paragraph")))
    "#+CAPTION: caption\nParagraph\n"))
  ;; Interpret dual keywords.
  (should
   (equal
    (org-element-interpret-data
     '(org-data nil (paragraph (:caption ((("long") "short"))) "Paragraph")))
    "#+CAPTION[short]: long\nParagraph\n"))
  ;; Interpret multiple parsed dual keywords.
  (should
   (equal
    (org-element-interpret-data
     '(org-data nil (paragraph
		     (:caption ((("l2") "s2") (("l1") "s1"))) "Paragraph")))
    "#+CAPTION[s1]: l1\n#+CAPTION[s2]: l2\nParagraph\n")))

(ert-deftest test-org-element/center-block-interpreter ()
  "Test center block interpreter."
  (should
   (equal (org-test-parse-and-interpret "#+BEGIN_CENTER\nTest\n#+END_CENTER")
	  "#+BEGIN_CENTER\nTest\n#+END_CENTER\n")))

(ert-deftest test-org-element/drawer-interpreter ()
  "Test drawer interpreter."
  (should
   (equal (let ((org-drawers '("TEST")))
	    (org-test-parse-and-interpret ":TEST:\nTest\n:END:"))
	  ":TEST:\nTest\n:END:\n")))

(ert-deftest test-org-element/dynamic-block-interpreter ()
  "Test dynamic block interpreter."
  (should
   (equal (org-test-parse-and-interpret
	   "#+BEGIN: myblock :parameter value1\nTest\n#+END:")
	  "#+BEGIN: myblock :parameter value1\nTest\n#+END:\n")))

(ert-deftest test-org-element/footnote-definition-interpreter ()
  "Test footnote definition interpreter."
  (should (equal (org-test-parse-and-interpret "[fn:1] Test") "[fn:1] Test\n")))

(ert-deftest test-org-element/headline-interpreter ()
  "Test headline and section interpreters."
  ;; 1. Standard test.
  (should (equal (org-test-parse-and-interpret "* Headline") "* Headline\n"))
  ;; 2. With TODO keywords.
  (should
   (equal (let ((org-todo-keywords '((sequence "TODO" "DONE"))))
	    (org-test-parse-and-interpret "* TODO Headline"))
	  "* TODO Headline\n"))
  ;; 3. With tags...
  ;;
  ;; 3.1. ... and a positive `org-tags-column' value.
  (should
   (equal (let ((org-tags-column 20))
	    (org-test-parse-and-interpret "* Headline :tag:"))
	  "* Headline          :tag:\n"))
  ;; 3.2. ... and a negative `org-tags-column' value.
  (should
   (equal (let ((org-tags-column -20))
	    (org-test-parse-and-interpret "* Headline :tag:"))
	  "* Headline     :tag:\n"))
  ;; 3.3. ... and a null `org-tags-column' value.
  (should
   (equal (let ((org-tags-column 0))
	    (org-test-parse-and-interpret "* Headline     :tag:"))
	  "* Headline :tag:\n"))
  ;; 4. With priority cookie.
  (should
   (equal (org-test-parse-and-interpret "* [#B] Headline")
	  "* [#B] Headline\n"))
  ;; 5. With comment keyword.
  (should
   (equal (let ((org-comment-string "COMMENT"))
	    (org-test-parse-and-interpret "* COMMENT Headline"))
	  "* COMMENT Headline\n"))
  ;; 6. With quote section.
  (should
   (equal (let ((org-quote-string "QUOTE"))
	    (org-test-parse-and-interpret "* QUOTE Headline"))
	  "* QUOTE Headline\n"))
  ;; 7. Keep same number of blank lines before body.
  (should
   (equal (org-test-parse-and-interpret
	   "* Headline\n\n\nText after two blank lines.")
	  "* Headline\n\n\nText after two blank lines.\n"))
  ;; 8. Preserve `org-odd-levels-only' state.
  (should
   (equal "* H\n*** H2\n"
	  (let ((org-odd-levels-only t))
	    (org-test-parse-and-interpret "* H\n*** H2")))))

(ert-deftest test-org-element/inlinetask-interpreter ()
  "Test inlinetask interpretation."
  (when (featurep 'org-inlinetask)
    (let ((org-inlinetask-min-level 15))
      ;; 1. Regular inlinetask.
     (should (equal (org-test-parse-and-interpret
		     "*************** Task\nTest\n*************** END")
		    "*************** Task\nTest\n*************** END\n"))
     ;; 2. Degenerate inlinetask.
     (should (equal (org-test-parse-and-interpret "*************** Task")
		    "*************** Task\n"))
     ;; 3. Prefer degenerate form when there are no contents.
     (should (equal (org-test-parse-and-interpret
		     "*************** Task\n*************** END")
		    "*************** Task\n"))
     ;; 4. With TODO keywords.
     (should
      (equal (let ((org-todo-keywords '((sequence "TODO" "DONE"))))
	       (org-test-parse-and-interpret "*************** TODO Task"))
	     "*************** TODO Task\n"))
     ;; 5. With tags...
     ;;
     ;; 5.1. ... and a positive `org-tags-column' value.
     (should
      (equal (let ((org-tags-column 30))
	       (org-test-parse-and-interpret "*************** Task :tag:"))
	     "*************** Task          :tag:\n"))
     ;; 5.2. ... and a negative `org-tags-column' value.
     (should
      (equal (let ((org-tags-column -30))
	       (org-test-parse-and-interpret "*************** Task :tag:"))
	     "*************** Task     :tag:\n"))
     ;; 5.3. ... and a null `org-tags-column' value.
     (should
      (equal (let ((org-tags-column 0))
	       (org-test-parse-and-interpret "*************** Task     :tag:"))
	     "*************** Task :tag:\n"))
     ;; 6. With priority cookie.
     (should
      (equal (org-test-parse-and-interpret "*************** [#B] Task")
	     "*************** [#B] Task\n")))))

(ert-deftest test-org-element/plain-list-interpreter ()
  "Test plain-list and item interpreters."
  (let ((org-list-two-spaces-after-bullet-regexp nil))
    ;; Unordered list.
    (should (equal (org-test-parse-and-interpret "- item 1") "- item 1\n"))
    ;; Description list.
    (should
     (equal (org-test-parse-and-interpret "- tag :: desc") "- tag :: desc\n"))
    ;; Ordered list.
    (should
     (equal (let ((org-plain-list-ordered-item-terminator t))
	      (org-test-parse-and-interpret "1. Item"))
	    "1. Item\n"))
    (should
     (equal (let ((org-plain-list-ordered-item-terminator ?\)))
	      (org-test-parse-and-interpret "1) Item"))
	    "1) Item\n"))
    ;; Ordered list with counter.
    (should
     (equal (let ((org-plain-list-ordered-item-terminator t))
	      (org-test-parse-and-interpret "1. [@5] Item"))
	    "5. [@5] Item\n"))
    ;; List with check-boxes.
    (should
     (equal (org-test-parse-and-interpret
	     "- [-] Item 1\n  - [X] Item 2\n  - [ ] Item 3")
	    "- [-] Item 1\n  - [X] Item 2\n  - [ ] Item 3\n"))
    ;; Item not starting with a paragraph.
    (should
     (equal (org-test-parse-and-interpret "-\n  | a | b |")
	    "- \n  | a | b |\n"))
    ;; Special case: correctly handle "*" bullets.
    (should (org-test-parse-and-interpret " * item"))
    ;; Special case: correctly handle empty items.
    (should (org-test-parse-and-interpret "-"))))

(ert-deftest test-org-element/quote-block-interpreter ()
  "Test quote block interpreter."
  (should (equal (org-test-parse-and-interpret
		  "#+BEGIN_QUOTE\nTest\n#+END_QUOTE")
		 "#+BEGIN_QUOTE\nTest\n#+END_QUOTE\n")))

(ert-deftest test-org-element/special-block-interpreter ()
  "Test special block interpreter."
  (should (equal (org-test-parse-and-interpret
		  "#+BEGIN_SPECIAL\nTest\n#+END_SPECIAL")
		 "#+BEGIN_SPECIAL\nTest\n#+END_SPECIAL\n")))

(ert-deftest test-org-element/babel-call-interpreter ()
  "Test babel call interpreter."
  ;; 1. Without argument.
  (should (equal (org-test-parse-and-interpret "#+CALL: test()")
		 "#+CALL: test()\n"))
  ;; 2. With argument.
  (should (equal (org-test-parse-and-interpret "#+CALL: test(x=2)")
		 "#+CALL: test(x=2)\n"))
  ;; 3. With header arguments.
  (should (equal (org-test-parse-and-interpret
		  "#+CALL: test[:results output]()[:results html]")
		 "#+CALL: test[:results output]()[:results html]\n")))

(ert-deftest test-org-element/clock-interpreter ()
  "Test clock interpreter."
  ;; Running clock.
  (should
   (equal (let ((org-clock-string "CLOCK:"))
	    (org-test-parse-and-interpret "CLOCK: [2012-01-01 sun. 00:01]"))
	  "CLOCK: [2012-01-01 sun. 00:01]\n"))
  ;; Closed clock.
  (should
   (equal
    (let ((org-clock-string "CLOCK:"))
      (org-test-parse-and-interpret "
CLOCK: [2012-01-01 sun. 00:01]--[2012-01-01 sun. 00:02] =>  0:01"))
    "CLOCK: [2012-01-01 sun. 00:01]--[2012-01-01 sun. 00:02] =>  0:01\n")))

(ert-deftest test-org-element/comment-interpreter ()
  "Test comment interpreter."
  ;; Regular comment.
  (should (equal (org-test-parse-and-interpret "# Comment") "# Comment\n"))
  ;; Inline comment.
  (should (equal (org-test-parse-and-interpret "  # Comment")
		 "# Comment\n"))
  ;; Preserve indentation.
  (should (equal (org-test-parse-and-interpret "  # No blank\n#  One blank")
		 "# No blank\n#  One blank\n")))

(ert-deftest test-org-element/comment-block-interpreter ()
  "Test comment block interpreter."
  (should (equal (org-test-parse-and-interpret
		  "#+BEGIN_COMMENT\nTest\n#+END_COMMENT")
		 "#+BEGIN_COMMENT\nTest\n#+END_COMMENT\n")))

(ert-deftest test-org-element/diary-sexp ()
  "Test diary-sexp interpreter."
  (should
   (equal
    (org-test-parse-and-interpret
     "%%(org-anniversary 1956  5 14)(2) Arthur Dent is %d years old")
    "%%(org-anniversary 1956  5 14)(2) Arthur Dent is %d years old\n")))

(ert-deftest test-org-element/example-block-interpreter ()
  "Test example block interpreter."
  ;; Without switches.
  (should (equal (org-test-parse-and-interpret
		  "#+BEGIN_EXAMPLE\nTest\n#+END_EXAMPLE")
		 "#+BEGIN_EXAMPLE\nTest\n#+END_EXAMPLE\n"))
  ;; With switches.
  (should
   (equal (org-test-parse-and-interpret
	   "#+BEGIN_EXAMPLE -n -k\n(+ 1 1)\n#+END_EXAMPLE")
	  "#+BEGIN_EXAMPLE -n -k\n(+ 1 1)\n#+END_EXAMPLE\n"))
  ;; Preserve code escaping.
  (should
   (equal (org-test-parse-and-interpret
	   "#+BEGIN_EXAMPLE\n,* Headline\n ,#+keyword\nText #+END_EXAMPLE")
	  "#+BEGIN_EXAMPLE\n,* Headline\n ,#+keyword\nText #+END_EXAMPLE\n")))

(ert-deftest test-org-element/export-block-interpreter ()
  "Test export block interpreter."
  (should (equal (org-test-parse-and-interpret
		  "#+BEGIN_HTML\nTest\n#+END_HTML")
		 "#+BEGIN_HTML\nTest\n#+END_HTML\n")))

(ert-deftest test-org-element/fixed-width-interpreter ()
  "Test fixed width interpreter."
  ;; Standard test.
  (should (equal (org-test-parse-and-interpret ": Test") ": Test\n"))
  ;; Preserve indentation.
  (should (equal (org-test-parse-and-interpret ":  2 blanks\n: 1 blank")
		 ":  2 blanks\n: 1 blank\n"))
  ;; Remove last newline character
  (should
   (equal (org-element-fixed-width-interpreter
	   '(fixed-width (:value "Test\n")) nil)
	  ": Test"))
  (should
   (equal (org-element-fixed-width-interpreter
	   '(fixed-width (:value "Test")) nil)
	  ": Test"))
  ;; Handle empty string.
  (should
   (equal (org-element-fixed-width-interpreter
	   '(fixed-width (:value "")) nil)
	  ""))
  ;; Handle nil value.
  (should-not
   (org-element-fixed-width-interpreter
    '(fixed-width (:value nil)) nil)))

(ert-deftest test-org-element/horizontal-rule-interpreter ()
  "Test horizontal rule interpreter."
  (should (equal (org-test-parse-and-interpret "-------") "-----\n")))

(ert-deftest test-org-element/keyword-interpreter ()
  "Test keyword interpreter."
  (should (equal (org-test-parse-and-interpret "#+KEYWORD: value")
		 "#+KEYWORD: value\n")))

(ert-deftest test-org-element/latex-environment-interpreter ()
  "Test latex environment interpreter."
  (should (equal (org-test-parse-and-interpret
		  "\\begin{equation}\n1+1=2\n\\end{equation}")
		 "\\begin{equation}\n1+1=2\n\\end{equation}\n"))
  (should (equal (org-test-parse-and-interpret
		  "\\begin{theorem}[me]\n1+1=2\n\\end{theorem}")
		 "\\begin{theorem}[me]\n1+1=2\n\\end{theorem}\n")))

(ert-deftest test-org-element/planning-interpreter ()
  "Test planning interpreter."
  (let ((org-closed-string "CLOSED:")
	(org-deadline-string "DEADLINE:")
	(org-scheduled-string "SCHEDULED:"))
    (should
     (equal
      (org-test-parse-and-interpret
       "* Headline
DEADLINE: <2012-01-01> SCHEDULED: <2012-01-01> CLOSED: [2012-01-01]")
      "* Headline
DEADLINE: <2012-01-01> SCHEDULED: <2012-01-01> CLOSED: [2012-01-01]\n"))))

(ert-deftest test-org-element/property-drawer-interpreter ()
  "Test property drawer interpreter."
  (should (equal (let ((org-property-format "%-10s %s"))
		   (org-test-parse-and-interpret
		    ":PROPERTIES:\n:prop: value\n:END:"))
		 ":PROPERTIES:\n:prop:     value\n:END:\n")))

(ert-deftest test-org-element/src-block-interpreter ()
  "Test src block interpreter."
  ;; With arguments.
  (should
   (equal (let ((org-edit-src-content-indentation 2))
	    (org-test-parse-and-interpret
	     "#+BEGIN_SRC emacs-lisp :results silent\n(+ 1 1)\n#+END_SRC"))
	  "#+BEGIN_SRC emacs-lisp :results silent\n  (+ 1 1)\n#+END_SRC\n"))
  ;; With switches.
  (should
   (equal (let ((org-edit-src-content-indentation 2))
	    (org-test-parse-and-interpret
	     "#+BEGIN_SRC emacs-lisp -n -k\n(+ 1 1)\n#+END_SRC"))
	  "#+BEGIN_SRC emacs-lisp -n -k\n  (+ 1 1)\n#+END_SRC\n"))
  ;; Preserve code escaping.
  (should
   (equal (let ((org-edit-src-content-indentation 2))
	    (org-test-parse-and-interpret
	     "#+BEGIN_SRC org\n,* Headline\n ,#+keyword\nText #+END_SRC"))
	  "#+BEGIN_SRC org\n,* Headline\n ,#+keyword\nText #+END_SRC\n"))
  ;; Do not apply `org-edit-src-content-indentation' when preserving
  ;; indentation.
  (should
   (equal (let ((org-edit-src-content-indentation 2)
		(org-src-preserve-indentation t))
	    (org-test-parse-and-interpret
	     "#+BEGIN_SRC emacs-lisp\n(+ 1 1)\n#+END_SRC"))
	  "#+BEGIN_SRC emacs-lisp\n(+ 1 1)\n#+END_SRC\n"))
  (should
   (equal (let ((org-edit-src-content-indentation 2)
		(org-src-preserve-indentation nil))
	    (org-test-parse-and-interpret
	     "#+BEGIN_SRC emacs-lisp -i\n(+ 1 1)\n#+END_SRC"))
	  "#+BEGIN_SRC emacs-lisp -i\n(+ 1 1)\n#+END_SRC\n")))

(ert-deftest test-org-element/table-interpreter ()
  "Test table, table-row and table-cell interpreters."
  ;; 1. Simple table.
  (should (equal (org-test-parse-and-interpret "| a | b |\n| c | d |")
		 "| a | b |\n| c | d |\n"))
  ;; 2. With horizontal rules.
  (should (equal (org-test-parse-and-interpret
		  "| a | b |\n|---+---|\n| c | d |")
		 "| a | b |\n|---+---|\n| c | d |\n"))
  ;; 3. With meta-data.
  (should (equal (org-test-parse-and-interpret "| / | < | > |\n| * | 1 | 2 |")
		 "| / | < | > |\n| * | 1 | 2 |\n"))
  ;; 4. With a formula.
  (should
   (equal (org-test-parse-and-interpret
	   "| 2 |\n| 4 |\n| 3 |\n#+TBLFM: @3=vmean(@1..@2)")
	  "| 2 |\n| 4 |\n| 3 |\n#+TBLFM: @3=vmean(@1..@2)\n"))
  ;; 5. With multiple formulas.
  (should
   (equal (org-test-parse-and-interpret
	   "| 2 |\n| 4 |\n| 3 |\n#+TBLFM: test1\n#+TBLFM: test2")
	  "| 2 |\n| 4 |\n| 3 |\n#+TBLFM: test1\n#+TBLFM: test2\n")))

(ert-deftest test-org-element/timestamp-interpreter ()
  "Test timestamp interpreter."
  ;; Active.
  (should (equal (org-test-parse-and-interpret "<2012-03-29 thu. 16:40>")
		 "<2012-03-29 thu. 16:40>\n"))
  (should
   (string-match "<2012-03-29 .* 16:40>"
		 (org-element-timestamp-interpreter
		  '(timestamp
		    (:type active :year-start 2012 :month-start 3 :day-start 29
			   :hour-start 16 :minute-start 40)) nil)))
  ;; Inactive.
  (should (equal (org-test-parse-and-interpret "[2012-03-29 thu. 16:40]")
		 "[2012-03-29 thu. 16:40]\n"))
  (should
   (string-match
    "\\[2012-03-29 .* 16:40\\]"
    (org-element-timestamp-interpreter
     '(timestamp
       (:type inactive :year-start 2012 :month-start 3 :day-start 29
	      :hour-start 16 :minute-start 40)) nil)))
  ;; Active range.
  (should (equal (org-test-parse-and-interpret
		  "<2012-03-29 thu. 16:40>--<2012-03-29 thu. 16:41>")
		 "<2012-03-29 thu. 16:40>--<2012-03-29 thu. 16:41>\n"))
  (should
   (string-match
    "<2012-03-29 .* 16:40>--<2012-03-29 .* 16:41>"
    (org-element-timestamp-interpreter
     '(timestamp
       (:type active-range :year-start 2012 :month-start 3 :day-start 29
	      :hour-start 16 :minute-start 40 :year-end 2012 :month-end 3
	      :day-end 29 :hour-end 16 :minute-end 41)) nil)))
  ;; Inactive range.
  (should (equal (org-test-parse-and-interpret
		  "[2012-03-29 thu. 16:40]--[2012-03-29 thu. 16:41]")
		 "[2012-03-29 thu. 16:40]--[2012-03-29 thu. 16:41]\n"))
  (should
   (string-match
    "\\[2012-03-29 .* 16:40\\]--\\[2012-03-29 .* 16:41\\]"
    (org-element-timestamp-interpreter
     '(timestamp
       (:type inactive-range :year-start 2012 :month-start 3 :day-start 29
	      :hour-start 16 :minute-start 40 :year-end 2012 :month-end 3
	      :day-end 29 :hour-end 16 :minute-end 41)) nil)))
  ;; Diary.
  (should (equal (org-test-parse-and-interpret "<%%diary-float t 4 2>")
		 "<%%diary-float t 4 2>\n"))
  ;; Timestamp with repeater interval, with delay, with both.
  (should (equal (org-test-parse-and-interpret "<2012-03-29 thu. +1y>")
		 "<2012-03-29 thu. +1y>\n"))
  (should
   (string-match
    "<2012-03-29 .* \\+1y>"
    (org-element-timestamp-interpreter
     '(timestamp
       (:type active :year-start 2012 :month-start 3 :day-start 29
	      :repeater-type cumulate :repeater-value 1 :repeater-unit year))
     nil)))
  (should
   (string-match
    "<2012-03-29 .* -1y>"
    (org-element-timestamp-interpreter
     '(timestamp
       (:type active :year-start 2012 :month-start 3 :day-start 29
	      :warning-type all :warning-value 1 :warning-unit year))
     nil)))
  (should
   (string-match
    "<2012-03-29 .* \\+1y -1y>"
    (org-element-timestamp-interpreter
     '(timestamp
       (:type active :year-start 2012 :month-start 3 :day-start 29
	      :warning-type all :warning-value 1 :warning-unit year
	      :repeater-type cumulate :repeater-value 1 :repeater-unit year))
     nil)))
  ;; Timestamp range with repeater interval
  (should (equal (org-test-parse-and-interpret
		  "<2012-03-29 Thu +1y>--<2012-03-30 Thu +1y>")
		 "<2012-03-29 Thu +1y>--<2012-03-30 Thu +1y>\n"))
  (should
   (string-match
    "<2012-03-29 .* \\+1y>--<2012-03-30 .* \\+1y>"
    (org-element-timestamp-interpreter
     '(timestamp
       (:type active-range :year-start 2012 :month-start 3 :day-start 29
	      :year-end 2012 :month-end 3 :day-end 30 :repeater-type cumulate
	      :repeater-value 1 :repeater-unit year))
     nil))))

(ert-deftest test-org-element/verse-block-interpreter ()
  "Test verse block interpretation."
  (should
   (equal (org-test-parse-and-interpret "#+BEGIN_VERSE\nTest\n#+END_VERSE")
	  "#+BEGIN_VERSE\nTest\n#+END_VERSE\n")))

(ert-deftest test-org-element/bold-interpreter ()
  "Test bold interpreter."
  (should (equal (org-test-parse-and-interpret "*text*") "*text*\n")))

(ert-deftest test-org-element/code-interpreter ()
  "Test code interpreter."
  (should (equal (org-test-parse-and-interpret "~text~") "~text~\n")))

(ert-deftest test-org-element/entity-interpreter ()
  "Test entity interpreter."
  ;; 1. Without brackets.
  (should
   (equal (org-test-parse-and-interpret "\\alpha text") "\\alpha text\n"))
  ;; 2. With brackets.
  (should
   (equal (org-test-parse-and-interpret "\\alpha{}text") "\\alpha{}text\n")))

(ert-deftest test-org-element/export-snippet-interpreter ()
  "Test export snippet interpreter."
  (should (equal (org-test-parse-and-interpret "@@back-end:contents@@")
		 "@@back-end:contents@@\n")))

(ert-deftest test-org-element/footnote-reference-interpreter ()
  "Test footnote reference interpreter."
  ;; 1. Regular reference.
  (should (equal (org-test-parse-and-interpret "Text[fn:1]") "Text[fn:1]\n"))
  ;; 2. Normalized reference.
  (should (equal (org-test-parse-and-interpret "Text[1]") "Text[1]\n"))
  ;; 3. Named reference.
  (should (equal (org-test-parse-and-interpret "Text[fn:label]")
		 "Text[fn:label]\n"))
  ;; 4. Inline reference.
  (should (equal (org-test-parse-and-interpret "Text[fn:label:def]")
		 "Text[fn:label:def]\n"))
  ;; 5. Anonymous reference.
  (should (equal (org-test-parse-and-interpret "Text[fn::def]")
		 "Text[fn::def]\n")))

(ert-deftest test-org-element/inline-babel-call-interpreter ()
  "Test inline babel call interpreter."
  ;; 1. Without arguments.
  (should (equal (org-test-parse-and-interpret "call_test()") "call_test()\n"))
  ;; 2. With arguments.
  (should (equal (org-test-parse-and-interpret "call_test(x=2)")
		 "call_test(x=2)\n"))
  ;; 3. With header arguments.
  (should (equal (org-test-parse-and-interpret
		  "call_test[:results output]()[:results html]")
		 "call_test[:results output]()[:results html]\n")))

(ert-deftest test-org-element/inline-src-block-interpreter ()
  "Test inline src block interpreter."
  ;; 1. Without header argument.
  (should (equal (org-test-parse-and-interpret "src_emacs-lisp{(+ 1 1)}")
		 "src_emacs-lisp{(+ 1 1)}\n"))
  ;; 2. With header arguments.
  (should (equal (org-test-parse-and-interpret
		  "src_emacs-lisp[:results silent]{(+ 1 1)}")
		 "src_emacs-lisp[:results silent]{(+ 1 1)}\n")))

(ert-deftest test-org-element/italic-interpreter ()
  "Test italic interpreter."
  (should (equal (org-test-parse-and-interpret "/text/") "/text/\n")))

(ert-deftest test-org-element/latex-fragment-interpreter ()
  "Test latex fragment interpreter."
  (let ((org-latex-regexps
	 '(("begin" "^[ 	]*\\(\\\\begin{\\([a-zA-Z0-9\\*]+\\)[^ ]+?\\\\end{\\2}\\)" 1 t)
	   ("$1" "\\([^$]\\|^\\)\\(\\$[^ 	\n,;.$]\\$\\)\\([- 	.,?;:'\") ]\\|$\\)" 2 nil)
	   ("$" "\\([^$]\\|^\\)\\(\\(\\$\\([^ 	\n,;.$][^$\n]*?\\(\n[^$\n]*?\\)\\{0,2\\}[^ 	\n,.$]\\)\\$\\)\\)\\([- 	.,?;:'\") ]\\|$\\)" 2 nil)
	   ("\\(" "\\\\([^ ]*?\\\\)" 0 nil)
	   ("\\[" "\\\\\\[[^ ]*?\\\\\\]" 0 nil)
	   ("$$" "\\$\\$[^ ]*?\\$\\$" 0 nil))))
    (should (equal (org-test-parse-and-interpret "\\command{}")
		   "\\command{}\n"))
    (should (equal (org-test-parse-and-interpret "$x$") "$x$\n"))
    (should (equal (org-test-parse-and-interpret "$x+y$") "$x+y$\n"))
    (should (equal (org-test-parse-and-interpret "$$x+y$$") "$$x+y$$\n"))
    (should (equal (org-test-parse-and-interpret "\\(x+y\\)") "\\(x+y\\)\n"))
    (should (equal (org-test-parse-and-interpret "\\[x+y\\]") "\\[x+y\\]\n"))))

(ert-deftest test-org-element/line-break-interpreter ()
  "Test line break interpreter."
  (should (equal (org-test-parse-and-interpret "First line \\\\ \nSecond line")
		 "First line \\\\\nSecond line\n")))

(ert-deftest test-org-element/link-interpreter ()
  "Test link interpreter."
  ;; 1. Links targeted from a radio target.
  (should (equal (let ((org-target-link-regexp "radio-target"))
		   (org-test-parse-and-interpret "a radio-target"))
		 "a radio-target\n"))
  ;; 2. Regular links.
  ;;
  ;; 2.1. Without description.
  (should (equal (org-test-parse-and-interpret "[[http://orgmode.org]]")
		 "[[http://orgmode.org]]\n"))
  ;; 2.2. With a description.
  (should (equal (org-test-parse-and-interpret
		  "[[http://orgmode.org][Org mode]]")
		 "[[http://orgmode.org][Org mode]]\n"))
  ;; 2.3. Id links.
  (should (equal (org-test-parse-and-interpret "[[id:aaaa]]") "[[id:aaaa]]\n"))
  ;; 2.4. Custom-id links.
  (should (equal (org-test-parse-and-interpret "[[#id]]") "[[#id]]\n"))
  ;; 2.5 Code-ref links.
  (should (equal (org-test-parse-and-interpret "[[(ref)]]") "[[(ref)]]\n"))
  ;; 3. Normalize plain links.
  (should (equal (org-test-parse-and-interpret "http://orgmode.org")
		 "[[http://orgmode.org]]\n"))
  ;; 4. Normalize angular links.
  (should (equal (org-test-parse-and-interpret "<http://orgmode.org>")
		 "[[http://orgmode.org]]\n")))

(ert-deftest test-org-element/macro-interpreter ()
  "Test macro interpreter."
  ;; 1. Without argument.
  (should (equal (org-test-parse-and-interpret "{{{test}}}") "{{{test}}}\n"))
  ;; 2. With arguments.
  (should (equal (org-test-parse-and-interpret "{{{test(arg1,arg2)}}}")
		 "{{{test(arg1,arg2)}}}\n")))

(ert-deftest test-org-element/radio-target-interpreter ()
  "Test radio target interpreter."
  (should (equal (org-test-parse-and-interpret "<<<some text>>>")
		 "<<<some text>>>\n")))

(ert-deftest test-org-element/statistics-cookie-interpreter ()
  "Test statistics cookie interpreter."
  ;; 1. Without percent
  (should (equal (org-test-parse-and-interpret "[0/1]") "[0/1]\n"))
  ;; 2. With percent.
  (should (equal (org-test-parse-and-interpret "[66%]") "[66%]\n")))

(ert-deftest test-org-element/strike-through-interpreter ()
  "Test strike through interpreter."
  (should (equal (org-test-parse-and-interpret "+target+") "+target+\n")))

(ert-deftest test-org-element/subscript-interpreter ()
  "Test subscript interpreter."
  ;; 1. Without brackets.
  (should (equal (org-test-parse-and-interpret "a_b") "a_b\n"))
  ;; 2. With brackets.
  (should (equal (org-test-parse-and-interpret "a_{b}") "a_{b}\n")))

(ert-deftest test-org-element/superscript-interpreter ()
  "Test superscript interpreter."
  ;; 1. Without brackets.
  (should (equal (org-test-parse-and-interpret "a^b") "a^b\n"))
  ;; 2. With brackets.
  (should (equal (org-test-parse-and-interpret "a^{b}") "a^{b}\n")))

(ert-deftest test-org-element/target-interpreter ()
  "Test target interpreter."
  (should (equal (org-test-parse-and-interpret "<<target>>") "<<target>>\n")))

(ert-deftest test-org-element/underline-interpreter ()
  "Test underline interpreter."
  (should (equal (org-test-parse-and-interpret "_text_") "_text_\n")))

(ert-deftest test-org-element/verbatim-interpreter ()
  "Test verbatim interpreter."
  (should (equal (org-test-parse-and-interpret "=text=") "=text=\n")))



;;; Test Granularity

(ert-deftest test-org-element/granularity ()
  "Test granularity impact on buffer parsing."
  (org-test-with-temp-text "
* Head 1
** Head 2
#+BEGIN_CENTER
Centered paragraph.
#+END_CENTER
Paragraph \\alpha."
    ;; 1.1. Granularity set to `headline' should parse every headline
    ;;      in buffer, and only them.
    (let ((tree (org-element-parse-buffer 'headline)))
      (should (= 2 (length (org-element-map tree 'headline 'identity))))
      (should-not (org-element-map tree 'paragraph 'identity)))
    ;; 1.2. Granularity set to `greater-element' should not enter
    ;;      greater elements excepted headlines and sections.
    (let ((tree (org-element-parse-buffer 'greater-element)))
      (should (= 1 (length (org-element-map tree 'center-block 'identity))))
      (should (= 1 (length (org-element-map tree 'paragraph 'identity))))
      (should-not (org-element-map tree 'entity 'identity)))
    ;; 1.3. Granularity set to `element' should enter every
    ;;      greater-element.
    (let ((tree (org-element-parse-buffer 'element)))
      (should (= 2 (length (org-element-map tree 'paragraph 'identity))))
      (should-not (org-element-map tree 'entity 'identity)))
    ;; 1.4. Granularity set to `object' can see everything.
    (let ((tree (org-element-parse-buffer 'object)))
      (should (= 1 (length (org-element-map tree 'entity 'identity)))))))

(ert-deftest test-org-element/secondary-string-parsing ()
  "Test if granularity correctly toggles secondary strings parsing."
  ;; With a granularity bigger than `object', no secondary string
  ;; should be parsed.
  (should
   (stringp
    (org-test-with-temp-text "* Headline"
      (let ((headline
	     (org-element-map (org-element-parse-buffer 'headline) 'headline
	       #'identity nil 'first-match)))
	(org-element-property :title headline)))))
  (should
   (stringp
    (org-test-with-temp-text "* Headline\n- tag :: item"
      (let ((item (org-element-map (org-element-parse-buffer 'element) 'item
		    #'identity nil 'first-match)))
	(org-element-property :tag item)))))
  (when (featurep 'org-inlinetask)
    (should
     (stringp
      (let ((org-inlinetask-min-level 15))
	(org-test-with-temp-text "*************** Inlinetask"
	  (let ((inlinetask (org-element-map (org-element-parse-buffer 'element)
				'inlinetask
			      #'identity nil 'first-match)))
	    (org-element-property :title inlinetask)))))))
  ;; With a default granularity, secondary strings should be parsed.
  (should
   (listp
    (org-test-with-temp-text "* Headline"
      (let ((headline
	     (org-element-map (org-element-parse-buffer) 'headline
	       #'identity nil 'first-match)))
	(org-element-property :title headline)))))
  ;; `org-element-at-point' should never parse a secondary string.
  (should-not
   (listp
    (org-test-with-temp-text "* Headline"
      (org-element-property :title (org-element-at-point)))))
  ;; Preserve current local variables when parsing a secondary string.
  (should
   (let ((org-entities nil)
	 (org-entities-user nil))
     (org-test-with-temp-text "
#+CAPTION: \\foo
Text
# Local Variables:
# org-entities-user: ((\"foo\"))
# End:"
       (let ((safe-local-variable-values '((org-entities-user . (("foo"))))))
	 (hack-local-variables))
       (org-element-map (org-element-parse-buffer) 'entity
	 #'identity nil nil nil t)))))



;;; Test Visible Only Parsing

(ert-deftest test-org-element/parse-buffer-visible ()
  "Test `org-element-parse-buffer' with visible only argument."
  (should
   (equal '("H1" "H3" "H5")
      (org-test-with-temp-text
	  "* H1\n** H2\n** H3 :visible:\n** H4\n** H5 :visible:"
	(org-occur ":visible:")
	(org-element-map (org-element-parse-buffer nil t) 'headline
	  (lambda (hl) (org-element-property :raw-value hl)))))))



;;; Test `:parent' Property

(ert-deftest test-org-element/parent-property ()
  "Test `:parent' property."
  ;; Elements.
  (org-test-with-temp-text "#+BEGIN_CENTER\nText\n#+END_CENTER"
    (let* ((tree (org-element-parse-buffer))
	   (parent (org-element-property
		    :parent
		    (org-element-map tree 'paragraph 'identity nil t))))
      (should parent)
      (should (eq (org-element-map tree 'center-block 'identity nil t)
		  parent))))
  ;; Objects.
  (org-test-with-temp-text "a_{/b/}"
    (let* ((tree (org-element-parse-buffer))
	   (parent (org-element-property
		    :parent
		    (org-element-map tree 'italic 'identity nil t))))
      (should parent)
      (should (eq parent
		  (org-element-map tree 'subscript 'identity nil t)))))
  ;; Secondary strings
  (org-test-with-temp-text "* /italic/"
    (let* ((tree (org-element-parse-buffer))
	   (parent (org-element-property
		    :parent (org-element-map tree 'italic 'identity nil t))))
      (should parent)
      (should (eq parent
		  (org-element-map tree 'headline 'identity nil t))))))



;;; Test Normalize Contents

(ert-deftest test-org-element/normalize-contents ()
  "Test `org-element-normalize-contents' specifications."
  ;; Remove maximum common indentation from element's contents.
  (should
   (equal
    (org-element-normalize-contents
     '(paragraph nil "  Two spaces\n   Three spaces"))
    '(paragraph nil "Two spaces\n Three spaces")))
  ;; Ignore objects within contents when computing maximum common
  ;; indentation.
  (should
   (equal
    (org-element-normalize-contents
     '(paragraph nil " One " (emphasis nil "space") "\n  Two spaces"))
    '(paragraph nil "One " (emphasis nil "space") "\n Two spaces")))
  ;; Ignore blank lines.
  (should
   (equal
    (org-element-normalize-contents
     '(paragraph nil "  Two spaces\n\n \n  Two spaces"))
    '(paragraph nil "Two spaces\n\n \nTwo spaces")))
  (should
   (equal
    '(paragraph nil " Two spaces\n" (verbatim nil "V") "\n Two spaces")
    (org-element-normalize-contents
     '(paragraph nil "  Two spaces\n " (verbatim nil "V") "\n  Two spaces"))))
  (should
   (equal
    '(verse-block nil "line 1\n\nline 2")
    (org-element-normalize-contents
     '(verse-block nil "  line 1\n\n  line 2"))))
  ;; Recursively enter objects in order to compute common indentation.
  (should
   (equal
    (org-element-normalize-contents
     '(paragraph nil "  Two spaces " (bold nil " and\n One space")))
    '(paragraph nil " Two spaces " (bold nil " and\nOne space"))))
  ;; When optional argument is provided, ignore first line
  ;; indentation.
  (should
   (equal
    (org-element-normalize-contents
     '(paragraph nil "No space\n  Two spaces\n   Three spaces") t)
    '(paragraph nil "No space\nTwo spaces\n Three spaces"))))



;;; Test Navigation Tools.

(ert-deftest test-org-element/at-point ()
  "Test `org-element-at-point' specifications."
  ;; Return closest element containing point.
  (should
   (eq 'paragraph
       (org-test-with-temp-text "#+BEGIN_CENTER\nA\n#+END_CENTER"
	 (progn (search-forward "A")
		(org-element-type (org-element-at-point))))))
  ;; Correctly set `:parent' property.
  (should
   (eq 'center-block
       (org-test-with-temp-text "#+BEGIN_CENTER\nA\n#+END_CENTER"
	 (progn (search-forward "A")
		(org-element-type
		 (org-element-property :parent (org-element-at-point)))))))
  ;; Special case: at a blank line just below a headline, return that
  ;; headline.
  (should
   (equal "H1" (org-test-with-temp-text "* H1\n  \n* H2\n"
		 (forward-line)
		 (org-element-property :title (org-element-at-point)))))
  ;; Special case: at the very beginning of a table, return `table'
  ;; object instead of `table-row'.
  (should
   (eq 'table
       (org-test-with-temp-text "| a | b |"
	 (org-element-type (org-element-at-point)))))
  ;; Special case: at the very beginning of a list or sub-list, return
  ;; `plain-list' object instead of `item'.
  (should
   (eq 'plain-list
       (org-test-with-temp-text "- item"
	 (org-element-type (org-element-at-point)))))
  ;; Special case: at the closing line of a greater element, be sure
  ;; to return it instead of the last element in its contents.
  (should
   (eq 'center-block
       (org-test-with-temp-text "#+BEGIN_CENTER\nParagraph\n#+END_CENTER"
	 (progn (forward-line 2)
		(org-element-type (org-element-at-point))))))
  ;; Special case: at a blank line between two items, be sure to
  ;; return item above instead of the last element of its contents.
  (should
   (eq 'item
       (org-test-with-temp-text "- Para1\n\n- Para2"
	 (progn (forward-line)
		(org-element-type
		 (let ((org-list-empty-line-terminates-plain-lists nil))
		   (org-element-at-point)))))))
  ;; Special case: at the last blank line in a plain list, return it
  ;; instead of the last item.
  (should
   (eq 'plain-list
       (org-test-with-temp-text "- Para1\n- Para2\n\nPara3"
	 (progn (forward-line 2)
		(org-element-type (org-element-at-point))))))
  ;; Special case: when a list ends at the end of buffer and there's
  ;; no final newline, return last element in last item.
  (should
   (eq 'paragraph
       (org-test-with-temp-text "- a"
	 (end-of-line)
	 (org-element-type (org-element-at-point)))))
  ;; With an optional argument, return trail.
  (should
   (equal '(paragraph center-block)
	  (org-test-with-temp-text "#+BEGIN_CENTER\nA\n#+END_CENTER\nZ"
	    (progn (search-forward "Z")
		   (mapcar 'org-element-type (org-element-at-point t))))))
  ;; Parse a list within a block itself contained in a list.
  (should
   (eq 'plain-list
       (org-test-with-temp-text
	   "- outer\n  #+begin_center\n  - inner\n  #+end_center"
	 (search-forward "inner")
	 (beginning-of-line)
	 (org-element-type (org-element-at-point)))))
  ;; Do not error at eob on an empty line.
  (should
   (org-test-with-temp-text "* H\n"
     (forward-line)
     (or (org-element-at-point) t))))

(ert-deftest test-org-element/context ()
  "Test `org-element-context' specifications."
  ;; Return closest object containing point.
  (should
   (eq 'underline
       (org-test-with-temp-text "Some *text with _underline_ text*"
	 (progn (search-forward "under")
		(org-element-type (org-element-context))))))
  ;; Find objects in secondary strings.
  (should
   (eq 'underline
       (org-test-with-temp-text "* Headline _with_ underlining"
	 (progn (search-forward "w")
		(org-element-type (org-element-context))))))
  ;; Find objects in objects.
  (should
   (eq 'macro
       (org-test-with-temp-text "| a | {{{macro}}} |"
	 (progn (search-forward "{")
		(org-element-type (org-element-context))))))
  (should
   (eq 'table-cell
       (org-test-with-temp-text "| a | b {{{macro}}} |"
	 (progn (search-forward "b")
		(org-element-type (org-element-context))))))
  ;; Find objects in planning lines.
  (should
   (eq 'timestamp
       (org-test-with-temp-text "* H\n  SCHEDULED: <2012-03-29 thu.>"
	 (search-forward "2012")
	 (org-element-type (org-element-context)))))
  (should-not
   (eq 'timestamp
       (org-test-with-temp-text "* H\n  SCHEDULED: <2012-03-29 thu.>"
	 (search-forward "SCHEDULED")
	 (org-element-type (org-element-context)))))
  ;; Find objects in document keywords.
  (should
   (eq 'macro
       (org-test-with-temp-text "#+DATE: {{{macro}}}"
	 (progn (search-forward "{")
		(org-element-type (org-element-context))))))
  ;; Do not find objects in table rules.
  (should
   (eq 'table-row
       (org-test-with-temp-text "| a | b |\n+---+---+\n| c | d |"
	 (forward-line)
	 (org-element-type (org-element-context)))))
  ;; Find objects in parsed affiliated keywords.
  (should
   (eq 'macro
       (org-test-with-temp-text "#+CAPTION: {{{macro}}}\n| a | b |."
	 (progn (search-forward "{")
		(org-element-type (org-element-context))))))
  (should
   (eq 'bold
       (org-test-with-temp-text "#+caption: *bold*\nParagraph"
	 (progn (search-forward "*")
		(org-element-type (org-element-context))))))
  ;; Correctly set `:parent' property.
  (should
   (eq 'paragraph
       (org-test-with-temp-text "Some *bold* text"
	 (progn (search-forward "bold")
		(org-element-type
		 (org-element-property :parent (org-element-context)))))))
  ;; Between two objects, return the second one.
  (should
   (eq 'macro
       (org-test-with-temp-text "<<target>>{{{test}}}"
	 (progn (search-forward "{")
		(backward-char)
		(org-element-type (org-element-context))))))
  ;; Test optional argument.
  (should
   (eq 'underline
       (org-test-with-temp-text "Some *text with _underline_ text*"
	 (progn
	   (search-forward "under")
	   (org-element-type (org-element-context (org-element-at-point)))))))
  ;; Special case: bold object at the beginning of a headline.
  (should
   (eq 'bold
       (org-test-with-temp-text "* *bold*"
	 (search-forward "bo")
	 (org-element-type (org-element-context))))))


(provide 'test-org-element)
;;; test-org-element.el ends here
