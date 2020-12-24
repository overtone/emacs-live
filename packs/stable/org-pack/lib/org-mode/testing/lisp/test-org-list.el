;;; test-org-list.el --- Tests for org-list.el

;; Copyright (C) 2012, 2013, 2014, 2018, 2019  Nicolas Goaziou

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

(ert-deftest test-org-list/list-ending ()
  "Test if lists end at the right place."
  ;; With two blank lines.
  (org-test-with-temp-text "- item\n\n\n  Text"
    (goto-line 4)
    (should-not (org-in-item-p)))
  ;; With text less indented than top items.
  (org-test-with-temp-text "- item\nText"
    (goto-line 2)
    (should-not (org-in-item-p)))
  ;; Though, blank lines and text indentation is ignored in blocks.
  (org-test-with-temp-text
      "- item\n  #+begin_quote\n\n\nText at column 0\n  #+end_quote\n Text"
    (goto-line 7)
    (should (org-in-item-p))))

(ert-deftest test-org-list/list-navigation ()
  "Test list navigation specifications."
  (org-test-with-temp-text "
- item A
- item B


- item 1
  - item 1.1
  - item 1.2
  - item 1.3
- item 2


- item X
- item Y"
    (let ((org-list-use-circular-motion nil))
      ;; 1. Test `org-next-item'.
      ;;
      ;; 1.1. Should return an error if at last item in
      ;;      a list/sub-list, unless `org-list-use-circular-motion'
      ;;      is non-nil.
      (goto-line 9)
      (should-error (org-next-item))
      (let ((org-list-use-circular-motion t))
	(should (progn (org-next-item) t)))
      (goto-line 14)
      (should-error (org-next-item))
      (let ((org-list-use-circular-motion t))
	(should (progn (org-next-item) t)))
      ;; 1.2. Should jump over sub-lists.
      (goto-line 6)
      (org-next-item)
      (should (looking-at "- item 2"))
      ;; 1.3. Shouldn't move to another list.
      (goto-line 3)
      (should-error (org-next-item))
      (should-not (looking-at "- item 1"))
      ;; 1.4. Should move to the list/sub-list first item when
      ;;     `org-list-use-circular-motion' is non-nil.
      (let ((org-list-use-circular-motion t))
	(goto-line 10)
	(org-next-item)
	(should (looking-at "- item 1"))
	(goto-line 9)
	(org-next-item)
	(should (looking-at "  - item 1.1")))
      ;; 2. Test `org-previous-item'.
      ;;
      ;; 2.1. Should return an error if at first item in
      ;;      a list/sub-list, unless `org-list-use-circular-motion is
      ;;      non-nil.
      (goto-line 7)
      (should-error (org-previous-item))
      (let ((org-list-use-circular-motion t))
	(should (progn (org-previous-item) t)))
      (goto-line 13)
      (should-error (org-previous-item))
      (let ((org-list-use-circular-motion t))
	(should (progn (org-previous-item) t)))
      ;; 2.2. Should ignore sub-lists.
      (goto-line 10)
      (org-previous-item)
      (should (looking-at "- item 1"))
      ;; 2.3. Shouldn't move to another list.
      (goto-line 6)
      (should-error (org-previous-item))
      (should-not (looking-at "- item B"))
      ;; 2.4. Should move to the list/sub-list last item when
      ;;      `org-list-use-circular-motion' is non-nil.
      (let ((org-list-use-circular-motion t))
	(goto-line 6)
	(org-previous-item)
	(should (looking-at "- item 2"))
	(goto-line 7)
	(org-previous-item)
	(should (looking-at "  - item 1.3"))))))

(ert-deftest test-org-list/cycle-bullet ()
  "Test `org-cycle-list-bullet' specifications."
  ;; Error when not at an item.
  (should-error
   (org-test-with-temp-text "Paragraph"
     (org-cycle-list-bullet)))
  ;; Cycle through "-", "+", "*", "1.", "1)".
  (org-test-with-temp-text "  - item"
    (org-cycle-list-bullet)
    (should (looking-at "[ \t]+\\+"))
    (org-cycle-list-bullet)
    (should (looking-at "[ \t]+\\*"))
    (let ((org-plain-list-ordered-item-terminator t))
      (org-cycle-list-bullet))
    (should (looking-at "[ \t]+1\\."))
    (let ((org-plain-list-ordered-item-terminator t))
      (org-cycle-list-bullet))
    (should (looking-at "[ \t]+1)")))
  ;; Argument is a valid bullet: cycle to that bullet directly.
  (should
   (equal "1. item"
	  (org-test-with-temp-text "- item"
	    (let ((org-plain-list-ordered-item-terminator t))
	      (org-cycle-list-bullet "1.")
	      (buffer-string)))))
  ;; Argument is an integer N: cycle to the Nth allowed bullet.
  (should
   (equal "+ item"
	  (org-test-with-temp-text "1. item"
	    (let ((org-plain-list-ordered-item-terminator t))
	      (org-cycle-list-bullet 1)
	      (buffer-string)))))
  ;; Argument is `previous': cycle backwards.
  (should
   (equal "- item"
	  (org-test-with-temp-text "+ item"
	    (let ((org-plain-list-ordered-item-terminator t))
	      (org-cycle-list-bullet 'previous)
	      (buffer-string)))))
  ;; Do not cycle to "*" bullets when item is at column 0.
  (should
   (equal "1. item"
	  (org-test-with-temp-text "+ item"
	    (let ((org-plain-list-ordered-item-terminator t))
	      (org-cycle-list-bullet)
	      (buffer-string)))))
  ;; Do not cycle to numbered bullets in a description list.
  (should-not
   (equal "1. tag :: item"
	  (org-test-with-temp-text "+ tag :: item"
	    (let ((org-plain-list-ordered-item-terminator t))
	      (org-cycle-list-bullet)
	      (buffer-string)))))
  ;; Do not cycle to ordered item terminators if they are not allowed
  ;; in `org-plain-list-ordered-item-terminator'.
  (should
   (equal "  1) item"
	  (org-test-with-temp-text "  * item"
	    (let ((org-plain-list-ordered-item-terminator 41))
	      (org-cycle-list-bullet)
	      (buffer-string)))))
  ;; When `org-list-allow-alphabetical' is non-nil, cycle to alpha bullets.
  (should
   (equal "a. item"
	  (org-test-with-temp-text "1) item"
	    (let ((org-plain-list-ordered-item-terminator t)
		  (org-list-allow-alphabetical t))
	      (org-cycle-list-bullet)
	      (buffer-string)))))
  ;; Do not cycle to alpha bullets when list has more than 26
  ;; elements.
  (should-not
   (equal "a. item 1"
	  (org-test-with-temp-text "1) item 1
2) item 2
3) item 3
4) item 4
5) item 5
6) item 6
7) item 7
8) item 8
9) item 9
10) item 10
11) item 11
12) item 12
13) item 13
14) item 14
15) item 15
16) item 16
17) item 17
18) item 18
19) item 19
20) item 20
21) item 21
22) item 22
23) item 23
24) item 24
25) item 25
26) item 26
27) item 27"
	    (let ((org-plain-list-ordered-item-terminator t)
		  (org-list-allow-alphabetical t))
	      (org-cycle-list-bullet)
	      (buffer-substring (point) (line-end-position)))))))

(ert-deftest test-org-list/indent-item ()
  "Test `org-indent-item' specifications."
  ;; Error when not at an item.
  (org-test-with-temp-text "Paragraph."
    (should-error (org-indent-item)))
  ;; Error when trying to move first item of a list.
  (should-error
   (org-test-with-temp-text "
- Item 1
- Item 2"
     (forward-line)
     (org-indent-item)))
  (should-error
   (org-test-with-temp-text "
- Item 1
- Item 2"
     (forward-line)
     (let ((org-list-automatic-rules nil)) (org-indent-item))))
  ;; Indent a single item, not its children.
  (should
   (equal "
- Item 1
  - Item 2
  - Item 2.1"
	  (org-test-with-temp-text "
- Item 1
- Item 2<point>
  - Item 2.1"
	    (let (org-list-demote-modify-bullet) (org-indent-item))
	    (buffer-string))))
  ;; Follow `org-list-demote-modify-bullet' specifications.
  (should
   (equal "
- Item 1
  + Item 2"
	  (org-test-with-temp-text "
- Item 1
- Item 2<point>"
	    (let ((org-list-demote-modify-bullet '(("-" . "+"))))
	      (org-indent-item))
	    (buffer-string))))
  (should
   (equal "
1. Item 1
   + Item 2"
	  (org-test-with-temp-text "
1. Item 1
2. Item 2<point>"
	    (let ((org-plain-list-ordered-item-terminator t)
		  (org-list-demote-modify-bullet '(("1." . "+"))))
	      (org-indent-item))
	    (buffer-string))))
  (should
   (equal "
a. Item 1
   - Item 2"
	  (org-test-with-temp-text "
a. Item 1
b. Item 2<point>"
	    (let ((org-plain-list-ordered-item-terminator t)
		  (org-list-allow-alphabetical t)
		  (org-list-demote-modify-bullet '(("A." . "a.") ("a." . "-"))))
	      (org-indent-item))
	    (buffer-string))))
  ;; When a region is selected, indent every item within.
  (should
   (equal "
- Item 1
  - Item 2
  - Item 3
"
	  (org-test-with-temp-text "
- Item 1
<point>- Item 2
- Item 3
"
	    (transient-mark-mode 1)
	    (push-mark (point) t t)
	    (goto-char (point-max))
	    (let (org-list-demote-modify-bullet) (org-indent-item))
	    (buffer-string)))))

(ert-deftest test-org-list/indent-item-tree ()
  "Test `org-indent-item-tree' specifications."
  ;; 1. Error when not at an item.
  (org-test-with-temp-text "Paragraph."
    (should-error (org-indent-item-tree)))
  ;; 2. Indent item along with its children.
  (org-test-with-temp-text "
- Item 1
- Item 2
  - Item 2.1"
    (search-forward "- Item 2")
    (let (org-list-demote-modify-bullet) (org-indent-item-tree))
    (should (equal (buffer-string)
		   "
- Item 1
  - Item 2
    - Item 2.1")))
  ;; 3. Special case: When indenting top item, move the whole list.
  (org-test-with-temp-text "
- Item 1
- Item 2"
    (search-forward "- Item 1")
    (let (org-list-demote-modify-bullet org-odd-levels-only)
      (org-indent-item-tree))
    (should (equal (buffer-string)
		   "
 - Item 1
 - Item 2")))
  ;; 4. Follow `org-list-demote-modify-bullet' specifications.
  ;;
  ;; 4.1. With unordered lists.
  (org-test-with-temp-text "
- Item 1
- Item 2
  + Item 2.1"
    (search-forward "- Item 2")
    (let ((org-list-demote-modify-bullet '(("-" . "+") ("+" . "-"))))
      (org-indent-item-tree))
    (should (equal (buffer-string)
		   "
- Item 1
  + Item 2
    - Item 2.1")))
  ;; 4.2. and ordered lists.
  (org-test-with-temp-text "
1. Item 1
2. Item 2
   + Item 2.1"
    (search-forward "2. Item 2")
    (let ((org-plain-list-ordered-item-terminator t)
	  (org-list-demote-modify-bullet '(("1." . "+") ("+" . "1."))))
      (org-indent-item-tree))
    (should (equal (buffer-string)
		   "
1. Item 1
   + Item 2
     1. Item 2.1")))
  ;; 5. When a region is selected, indent every item within.
  (org-test-with-temp-text "
- Item 1
- Item 2
  - Item 2.1
- Item 3
  - Item 3.1
"
    (search-forward "- Item 2")
    (beginning-of-line)
    (transient-mark-mode 1)
    (push-mark (point) t t)
    (goto-char (point-max))
    (let (org-list-demote-modify-bullet) (org-indent-item-tree))
    (should (equal (buffer-string)
		   "
- Item 1
  - Item 2
    - Item 2.1
  - Item 3
    - Item 3.1
"))))

(ert-deftest test-org-list/outdent-item ()
  "Test `org-outdent-item' specifications."
  ;; 1. Error when not at an item.
  (org-test-with-temp-text "Paragraph."
    (should-error (org-outdent-item)))
  ;; 2. Error when trying to move first item of a list.
  (org-test-with-temp-text "
- Item 1
- Item 2"
    (forward-line)
    (should-error (org-outdent-item)))
  ;; 3. Error when trying to outdent an item without its children.
  (org-test-with-temp-text "
- Item 1
  - Item 1.1
    - Item 1.1.1"
    (search-forward "- Item 1.1")
    (should-error (org-outdent-item)))
  ;; 4. Error when trying to outdent before top item.
  (org-test-with-temp-text "
  - Item 1
  - Item 2"
    (search-forward "- Item 2")
    (should-error (org-outdent-item)))
  ;; 5. When a region is selected, outdent every item within.
  (org-test-with-temp-text "
- Item 1
  - Item 2
  - Item 3
"
    (search-forward "- Item 2")
    (beginning-of-line)
    (transient-mark-mode 1)
    (push-mark (point) t t)
    (goto-char (point-max))
    (let (org-list-demote-modify-bullet) (org-outdent-item))
    (should (equal (buffer-string)
		   "
- Item 1
- Item 2
- Item 3
"))))

(ert-deftest test-org-list/outdent-item-tree ()
  "Test `org-outdent-item-tree' specifications."
  ;; 1. Error when not at an item.
  (org-test-with-temp-text "Paragraph."
    (should-error (org-outdent-item-tree)))
  ;; 2. Error when trying to outdent before top item.
  (org-test-with-temp-text "
  - Item 1
  - Item 2"
    (search-forward "- Item 2")
    (should-error (org-outdent-item-tree)))
  ;; 3. Outdent item along with its children.
  (org-test-with-temp-text "
- Item 1
  - Item 2
    - Item 2.1"
    (search-forward "- Item 2")
    (org-outdent-item-tree)
    (should (equal (buffer-string)
		   "
- Item 1
- Item 2
  - Item 2.1")))
  ;; 3. Special case: When outdenting top item, move the whole list.
  (org-test-with-temp-text "
 - Item 1
 - Item 2"
    (search-forward "- Item 1")
    (let (org-odd-levels-only) (org-outdent-item-tree))
    (should (equal (buffer-string)
		   "
- Item 1
- Item 2")))
  ;; 5. When a region is selected, outdent every item within.
  (org-test-with-temp-text "
- Item 1
  - Item 2
    - Item 2.1
  - Item 3
    - Item 3.1
"
    (search-forward "- Item 2")
    (beginning-of-line)
    (transient-mark-mode 1)
    (push-mark (point) t t)
    (goto-char (point-max))
    (org-outdent-item-tree)
    (should (equal (buffer-string)
		   "
- Item 1
- Item 2
  - Item 2.1
- Item 3
  - Item 3.1
"))))

(ert-deftest test-org-list/cycle-item-identation ()
  "Test `org-list-cycle-item-indentation' specifications."
  ;; Refuse to indent non-empty items.
  (should-not
   (org-test-with-temp-text "- item - item2<point>"
     (org-cycle-item-indentation)))
  ;; First try to indent item.
  (should
   (equal "- item\n  - sub-item\n    - "
	  (org-test-with-temp-text "- item\n  - sub-item\n  - <point>"
	    (org-cycle-item-indentation)
	    (buffer-string))))
  ;; If first indentation is not possible, outdent item.
  (should
   (equal "- item\n- "
	  (org-test-with-temp-text "- item\n  - <point>"
	    (org-cycle-item-indentation)
	    (buffer-string))))
  ;; Throw an error when item cannot move either way.
  (should-error
   (org-test-with-temp-text "- "
     (org-cycle-item-indentation)))
  ;; On repeated commands, cycle through all the indented positions,
  ;; then through all the outdented ones, then move back to initial
  ;; position.
  (should
   (equal '(4 6 0 2)
	  (org-test-with-temp-text "- i0\n  - i1\n    - s1\n  - <point>"
	    (let ((indentations nil))
	      (org-cycle-item-indentation)
	      (dotimes (_ 3)
		(let ((last-command 'org-cycle-item-indentation))
		  (push (current-indentation) indentations)
		  (org-cycle-item-indentation)))
	      (reverse (cons (current-indentation) indentations))))))
  ;; Refuse to indent the first item in a sub-list.  Also refuse to
  ;; outdent an item with a next sibling.
  (should-error
   (org-test-with-temp-text "- item\n  - <point>\n  - sub-item 2"
     (org-cycle-item-indentation)))
  ;; When cycling back into initial position, preserve bullet type.
  (should
   (equal "1. item\n   - "
	  (org-test-with-temp-text "1. item\n  - <point>"
	    (org-cycle-item-indentation)
	    (let ((last-command 'org-cycle-item-indentation))
	      (org-cycle-item-indentation))
	    (buffer-string))))
  (should
   (equal "1. item\n   - tag :: "
	  (org-test-with-temp-text "1. item\n  - tag :: <point>"
	    (org-cycle-item-indentation)
	    (let ((last-command 'org-cycle-item-indentation))
	      (org-cycle-item-indentation))
	    (buffer-string))))
  ;; When starting at top level, never outdent.
  (should
   (org-test-with-temp-text "- item\n- <point>"
     (org-cycle-item-indentation)
     (let ((last-command 'org-cycle-item-indentation))
       (org-cycle-item-indentation))
     (buffer-string))))

(ert-deftest test-org-list/move-item-down ()
  "Test `org-move-item-down' specifications."
  ;; Standard test.
  (should
   (equal "- item 2\n- item 1"
	  (org-test-with-temp-text "- item 1\n- item 2"
	    (org-move-item-down)
	    (buffer-string))))
  ;; Keep same column in item.
  (should
   (org-test-with-temp-text "- it<point>em 1\n- item 2"
     (org-move-item-down)
     (looking-at "em 1")))
  ;; Move sub-items.
  (org-test-with-temp-text "- item 1\n  - sub-item 1\n- item 2"
    (org-move-item-down)
    (should (equal (buffer-string)
		   "- item 2\n- item 1\n  - sub-item 1")))
  ;; Preserve blank lines.
  (should
   (equal
    "- item 2\n\n- item 1"
    (org-test-with-temp-text "- item 1\n\n- item 2"
      (org-move-item-down)
      (buffer-string))))
  ;; Error when trying to move the last item...
  (should-error
   (org-test-with-temp-text "- item 1\n- item 2"
     (forward-line)
     (org-move-item-down)))
  ;; ... unless `org-list-use-circular-motion' is non-nil.  In this
  ;; case, move to the first item.
  (should
   (equal  "- item 3\n- item 1\n- item 2\n"
	   (org-test-with-temp-text "- item 1\n- item 2\n<point>- item 3"
	     (let ((org-list-use-circular-motion t)) (org-move-item-down))
	     (buffer-string))))
  ;; Preserve item visibility.
  (should
   (equal
    '(outline outline)
    (org-test-with-temp-text
	"* Headline\n<point>- item 1\n  body 1\n- item 2\n  body 2"
      (let ((org-cycle-include-plain-lists t))
	(org-cycle)
	(search-forward "- item 2")
	(org-cycle))
      (search-backward "- item 1")
      (org-move-item-down)
      (forward-line)
      (list (org-invisible-p2)
	    (progn
	      (search-backward " body 2")
	      (org-invisible-p2))))))
  ;; Preserve children visibility.
  (org-test-with-temp-text "* Headline
- item 1
  - sub-item 1
    sub-body 1
- item 2
  - sub-item 2
    sub-body 2"
    (let ((org-cycle-include-plain-lists t))
      (search-forward "- sub-item 1")
      (org-cycle)
      (search-forward "- sub-item 2")
      (org-cycle))
    (search-backward "- item 1")
    (org-move-item-down)
    (search-forward "sub-body 1")
    (should (org-invisible-p2))
    (search-backward "sub-body 2")
    (should (org-invisible-p2))))

(ert-deftest test-org-list/move-item-down-contents-visibility ()
  "Preserve contents visibility."
  (org-test-with-temp-text "
- item 1
  #+BEGIN_CENTER
  Text1
  #+END_CENTER
- item 2
  #+BEGIN_CENTER
  Text2
  #+END_CENTER"
    (org-hide-block-all)
    (let ((invisible-property-1
	   (progn
	     (search-forward "Text1")
	     (get-char-property (point) 'invisible)))
	  (invisible-property-2
	   (progn
	     (search-forward "Text2")
	     (get-char-property (point) 'invisible))))
      (goto-char (point-min))
      (search-forward "- item 1")
      (org-move-item-down)
      (search-forward "Text1")
      (should (eq invisible-property-1 (get-char-property (point) 'invisible)))
      (search-backward "Text2")
      (should (eq invisible-property-2 (get-char-property (point) 'invisible))))))

(ert-deftest test-org-list/move-item-up ()
  "Test `org-move-item-up' specifications."
  ;; Standard test.
  (org-test-with-temp-text "- item 1\n- item 2"
    (forward-line)
    (org-move-item-up)
    (should (equal (buffer-string)
		   "- item 2\n- item 1")))
  ;; Keep same column in item.
  (org-test-with-temp-text "- item 1\n- item 2"
    (forward-line)
    (forward-char 4)
    (org-move-item-up)
    (should (looking-at "em 2")))
  ;; Move sub-items.
  (org-test-with-temp-text "- item 1\n- item 2\n  - sub-item 2"
    (forward-line)
    (org-move-item-up)
    (should (equal (buffer-string)
		   "- item 2\n  - sub-item 2\n- item 1")))
  ;; Preserve blank lines.
  (should
   (equal
    "- item 2\n\n- item 1"
    (org-test-with-temp-text "- item 1\n\n- item 2"
      (search-forward "- item 2")
      (org-move-item-up)
      (buffer-string))))
  ;; Error when trying to move the first item...
  (org-test-with-temp-text "- item 1\n- item 2"
    (should-error (org-move-item-up)))
  ;; ... unless `org-list-use-circular-motion' is non-nil.  In this
  ;; case, move to the first item.
  (should
   (equal "- item 2\n- item 3\n- item 1"
	  (org-test-with-temp-text "- item 1\n- item 2\n- item 3"
	    (let ((org-list-use-circular-motion t)) (org-move-item-up))
	    (buffer-string))))
  ;; Preserve item visibility.
  (org-test-with-temp-text "* Headline\n- item 1\n  body 1\n- item 2\n  body 2"
    (let ((org-cycle-include-plain-lists t))
      (search-forward "- item 1")
      (org-cycle)
      (search-forward "- item 2")
      (org-cycle))
    (org-move-item-up)
    (forward-line)
    (should (org-invisible-p2))
    (search-forward " body 1")
    (should (org-invisible-p2)))
  ;; Preserve children visibility.
  (org-test-with-temp-text "* Headline
- item 1
  - sub-item 1
    sub-body 1
- item 2
  - sub-item 2
    sub-body 2"
    (let ((org-cycle-include-plain-lists t))
      (search-forward "- sub-item 1")
      (org-cycle)
      (search-forward "- sub-item 2")
      (org-cycle))
    (search-backward "- item 2")
    (org-move-item-up)
    (search-forward "sub-body 2")
    (should (org-invisible-p2))
    (search-forward "sub-body 1")
    (should (org-invisible-p2))))

(ert-deftest test-org-list/move-item-up-contents-visibility ()
  (org-test-with-temp-text "
- item 1
  #+BEGIN_CENTER
  Text1
  #+END_CENTER
- item 2
  #+BEGIN_CENTER
  Text2
  #+END_CENTER"
    (org-hide-block-all)
    (let ((invisible-property-1
	   (progn
	     (search-forward "Text1")
	     (get-char-property (point) 'invisible)))
          (invisible-property-2
	   (progn
	     (search-forward "Text2")
	     (get-char-property (point) 'invisible))))
      (goto-char (point-min))
      (search-forward "- item 2")
      (org-move-item-up)
      (search-forward "Text2")
      (should (eq invisible-property-2 (get-char-property (point) 'invisible)))
      (search-forward "Text1")
      (should (eq invisible-property-1 (get-char-property (point) 'invisible))))))

(ert-deftest test-org-list/insert-item ()
  "Test item insertion."
  ;; Blank lines specifications.
  ;;
  ;; Non-nil `org-blank-before-new-entry': insert a blank line.
  (should
   (org-test-with-temp-text "- a"
     (let ((org-blank-before-new-entry '((plain-list-item . t))))
       (end-of-line)
       (org-insert-item)
       (forward-line -1)
       (looking-at "$"))))
  ;; Nil `org-blank-before-new-entry': do not insert a blank line.
  (should-not
   (org-test-with-temp-text "- a"
     (let ((org-blank-before-new-entry '((plain-list-item . nil))))
       (end-of-line)
       (org-insert-item)
       (forward-line -1)
       (looking-at "$"))))
  ;; `org-blank-before-new-entry' set to auto: if there's no blank
  ;; line already in the sole item, do not insert one.
  (should-not
   (org-test-with-temp-text "- a"
     (let ((org-blank-before-new-entry '((plain-list-item . auto))))
       (end-of-line)
       (org-insert-item)
       (forward-line -1)
       (looking-at "$"))))
  ;; `org-blank-before-new-entry' set to `auto': if there's a blank
  ;; line in the sole item, insert another one.
  (should
   (org-test-with-temp-text "- a\n\n  b<point>"
     (let ((org-blank-before-new-entry '((plain-list-item . auto))))
       (org-insert-item)
       (forward-line -1)
       (looking-at "$"))))
  ;; `org-blank-before-new-entry' set to `auto': if the user specified
  ;; a blank line, preserve it.
  (should
   (org-test-with-temp-text "- a\n\n<point>"
     (let ((org-blank-before-new-entry '((plain-list-item . auto))))
       (org-insert-item)
       (forward-line -1)
       (looking-at "$"))))
  ;; `org-blank-before-new-entry' set to `auto': if some items in list
  ;; are already separated by blank lines, insert one.
  (should
   (org-test-with-temp-text "- a\n\n- b<point>"
     (let ((org-blank-before-new-entry '((plain-list-item . auto))))
       (org-insert-item)
       (forward-line -1)
       (looking-at "$"))))
  (should
   (org-test-with-temp-text "- a\n\n- b"
     (let ((org-blank-before-new-entry '((plain-list-item . auto))))
       (org-insert-item)
       (forward-line)
       (looking-at "$"))))
  (should
   (org-test-with-temp-text
       "- a\n  #+BEGIN_EXAMPLE\n\n  x\n  #+END_EXAMPLE<point>"
     (let ((org-blank-before-new-entry '((plain-list-item . auto))))
       (org-insert-item)
       (forward-line -1)
       (looking-at "$"))))
  ;; When called before or on the bullet, insert new item before
  ;; current one.
  (should
   (equal "- \n- item"
	  (org-test-with-temp-text "- item"
	    (org-insert-item)
	    (buffer-string))))
  (should
   (equal "- \n- item"
	  (org-test-with-temp-text "- <point>item"
	    (org-insert-item)
	    (buffer-string))))
  ;; When called at the very end of the list, insert new item as
  ;; a sibling of the very last one.
  (should
   (equal "- A\n\n  - B\n\n  - "
	  (org-test-with-temp-text "- A\n\n - B\n\n<point>"
	    (org-insert-item)
	    (buffer-string))))
  (should
   (equal "- A\n\n  - B\n\n  - "
	  (org-test-with-temp-text "- A\n\n  - B\n\n  <point>"
	    (org-insert-item)
	    (buffer-string))))
  ;; When called on tag in a descriptive list, insert new item before
  ;; current one too.
  (should
   (equal "-  :: \n- tag :: item"
	  (org-test-with-temp-text "- tag <point>:: item"
	    (org-insert-item)
	    (buffer-string))))
  (should
   (equal "-  :: \n- tag :: item"
	  (org-test-with-temp-text "- ta<point>g :: item"
	    (org-insert-item)
	    (buffer-string))))
  ;; Further, it splits the line or add a blank new item after it,
  ;; according to `org-M-RET-may-split-line'.
  (should
   (equal "- it\n- em"
	  (org-test-with-temp-text "- it<point>em"
	    (let ((org-M-RET-may-split-line  '((default . t))))
	      (org-insert-item))
	    (buffer-string))))
  (should
   (equal "- item\n- "
	  (org-test-with-temp-text "- it<point>em"
	    (let ((org-M-RET-may-split-line  '((default . nil))))
	      (org-insert-item))
	    (buffer-string))))
  ;; Re-order automatically.
  (should
   (equal "1. A\n\n2. \n\n3. \n\n4. B"
	  (org-test-with-temp-text "1. A<point>\n\n2. \n\n3. B"
	    (org-insert-item)
	    (buffer-string))))
  (should
   (equal "1. a\n2. \n   b\n3. c"
	  (org-test-with-temp-text "1. a<point>\n   b\n2. c"
	    (org-insert-item)
	    (buffer-string))))
  ;; Preserve list visibility when inserting an item.
  (should
   (equal
    '(outline outline)
    (org-test-with-temp-text "- A\n  - B\n- C\n  - D"
      (let ((org-cycle-include-plain-lists t))
	(org-cycle)
	(forward-line 2)
	(org-cycle)
	(org-insert-item)
	(list (get-char-property (line-beginning-position 0) 'invisible)
	      (get-char-property (line-end-position 2) 'invisible))))))
  ;; Test insertion in area after a sub-list.  In particular, if point
  ;; is right at the end of the previous sub-list, still insert
  ;; a sub-item in that list.
  (should
   (= 2
      (org-test-with-temp-text "- item\n  - sub-list\n<point>  resume item"
	(org-insert-item)
	(current-indentation))))
  (should
   (= 0
      (org-test-with-temp-text "- item\n  - sub-list\n  resume item<point>"
	(org-insert-item)
	(current-indentation))))
  ;; Test splitting with blanks around.
  (should
   (equal "- A\n  B\n- C\n  - D\n- [ ] E"
    (org-test-with-temp-text "- A\n  B <point> C\n  - D\n- [ ] E"
      (org-insert-item)
      (buffer-string)))))

(ert-deftest test-org-list/repair ()
  "Test `org-list-repair' specifications."
  ;; Repair indentation.
  (should
   (equal "- item\n  - child"
	  (org-test-with-temp-text "- item\n - child"
	    (let ((org-list-indent-offset 0)) (org-list-repair))
	    (buffer-string))))
  ;; Repair bullets and numbering.
  (should
   (equal "- a\n- b"
	  (org-test-with-temp-text "- a\n+ b"
	    (let ((org-list-indent-offset 0))
	      (org-list-repair))
	    (buffer-string))))
  (should
   (equal "1. a\n2. b"
	  (org-test-with-temp-text "1. a\n1. b"
	    (let ((org-list-indent-offset 0)
		  (org-plain-list-ordered-item-terminator t))
	      (org-list-repair))
	    (buffer-string))))
  ;; Repair check-boxes.
  (should
   (equal "- [X] item\n  - [X] child"
	  (org-test-with-temp-text "- [ ] item\n  - [X] child"
	    (let ((org-list-indent-offset 0))
	      (org-list-repair))
	    (buffer-string))))
  ;; Special case: do not move contents of an item within its child.
  ;; Yet, preserve indentation differences within contents.
  (should
   (equal "- item\n  - child\n  within item"
	  (org-test-with-temp-text "- item\n    - child\n    within item"
	    (let ((org-list-indent-offset 0)) (org-list-repair))
	    (buffer-string))))
  (should
   (equal
    "- item\n  - child\n  within item\n    indented"
    (org-test-with-temp-text
	"- item\n    - child\n   within item\n     indented"
      (let ((org-list-indent-offset 0)) (org-list-repair))
      (buffer-string)))))

(ert-deftest test-org-list/update-checkbox-count ()
  "Test `org-update-checkbox-count' specifications."
  ;; From a headline.
  (should
   (string-match "\\[0/1\\]"
		 (org-test-with-temp-text "* [/]\n- [ ] item"
		   (org-update-checkbox-count)
		   (buffer-string))))
  (should
   (string-match "\\[1/1\\]"
		 (org-test-with-temp-text "* [/]\n- [X] item"
		   (org-update-checkbox-count)
		   (buffer-string))))
  (should
   (string-match "\\[100%\\]"
		 (org-test-with-temp-text "* [%]\n- [X] item"
		   (org-update-checkbox-count)
		   (buffer-string))))
  ;; From a list or a sub-list.
  (should
   (string-match "\\[0/1\\]"
		 (org-test-with-temp-text "- [/]\n  - [ ] item"
		   (org-update-checkbox-count)
		   (buffer-string))))
  (should
   (string-match "\\[1/1\\]"
		 (org-test-with-temp-text "- [/]\n  - [X] item"
		   (org-update-checkbox-count)
		   (buffer-string))))
  (should
   (string-match "\\[100%\\]"
		 (org-test-with-temp-text "- [%]\n  - [X] item"
		   (org-update-checkbox-count)
		   (buffer-string))))
  (should
   (string-match
    "\\[1/1\\]"
    (org-test-with-temp-text "- [ ] item 1\n- [ ] item 2 [/]\n  - [X] sub 1"
      (org-update-checkbox-count)
      (buffer-string))))
  ;; Count do not apply to sub-lists unless count is not hierarchical.
  ;; This state can be achieved with COOKIE_DATA node property set to
  ;; "recursive".
  (should
   (string-match "\\[1/1\\]"
		 (org-test-with-temp-text "- [/]\n  - item\n    - [X] sub-item"
		   (let ((org-checkbox-hierarchical-statistics nil))
		     (org-update-checkbox-count))
		   (buffer-string))))
  (should
   (string-match "\\[1/1\\]"
		 (org-test-with-temp-text "
<point>* H
:PROPERTIES:
:COOKIE_DATA: recursive
:END:
- [/]
  - item
    - [X] sub-item"
		   (org-update-checkbox-count)
		   (buffer-string))))
  (should
   (string-match "\\[0/0\\]"
		 (org-test-with-temp-text "- [/]\n  - item\n    - [ ] sub-item"
		   (org-update-checkbox-count)
		   (buffer-string))))
  ;; With optional argument ALL, update all buffer.
  (should
   (= 2
      (org-test-with-temp-text "* [/]\n- [X] item\n* [/]\n- [X] item"
	(org-update-checkbox-count t)
	(count-matches "\\[1/1\\]"))))
  ;; Ignore boxes in drawers, blocks or inlinetasks when counting from
  ;; outside.
  (should
   (string-match "\\[2/2\\]"
		 (org-test-with-temp-text "
- [/]
  - [X] item1
    :DRAWER:
    - [X] item
    :END:
  - [X] item2"
		   (let ((org-checkbox-hierarchical-statistics nil))
		     (org-update-checkbox-count))
		   (buffer-string)))))


;;; API

(ert-deftest test-org-list/at-radio-list-p ()
  "Test `org-at-radio-list-p' specifications."
  (should
   (org-test-with-temp-text "#+attr_org: :radio t\n<point>- foo"
     (org-at-radio-list-p)))
  (should
   (org-test-with-temp-text "#+attr_org: :radio t\n- foo\n<point>- bar"
     (org-at-radio-list-p)))
  (should
   (org-test-with-temp-text "#+ATTR_ORG: :radio t\n<point>- foo"
     (org-at-radio-list-p)))
  (should
   (org-test-with-temp-text "#+attr_org: :radio bar\n<point>- foo"
     (org-at-radio-list-p)))
  (should-not
   (org-test-with-temp-text "#+attr_org: :radio nil\n<point>- foo"
     (org-at-radio-list-p)))
  (should-not
   (org-test-with-temp-text "<point>- foo"
     (org-at-radio-list-p)))
  (should-not
   (org-test-with-temp-text "#+attr_org: :radio t\n- foo\n  <point>bar"
     (org-at-radio-list-p)))
  (should-not
   (org-test-with-temp-text
       "#+attr_org: :radio t\n#+begin_example\n<point>- foo\n#+end_example"
     (org-at-radio-list-p))))


;;; Miscellaneous

(ert-deftest test-org-list/toggle-item ()
  "Test `org-toggle-item' specifications."
  ;; Convert normal lines to items.
  (should
   (equal "- line"
	  (org-test-with-temp-text "line"
	    (org-toggle-item nil)
	    (buffer-string))))
  ;; Convert items to normal lines.
  (should
   (equal "line"
	  (org-test-with-temp-text "- line"
	    (org-toggle-item nil)
	    (buffer-string))))
  ;; Convert headlines to items.
  (should
   (equal "- line"
	  (org-test-with-temp-text "* line"
	    (org-toggle-item nil)
	    (buffer-string))))
  ;; When converting a headline to a list item, TODO keywords become
  ;; checkboxes.
  (should
   (equal "- [X] line"
	  (org-test-with-temp-text "* DONE line"
	    (org-toggle-item nil)
	    (buffer-string))))
  (should
   (equal "- [ ] line"
	  (org-test-with-temp-text "* TODO line"
	    (org-toggle-item nil)
	    (buffer-string))))
  ;; When turning headlines into items, make sure planning info line
  ;; and properties drawers are removed.  This also includes empty
  ;; lines following them.
  (should
   (equal "- H\n"
	  (org-test-with-temp-text "* H\nSCHEDULED: <2012-03-29 Thu>"
	    (org-toggle-item nil)
	    (buffer-string))))
  (should
   (equal "- H\n"
	  (org-test-with-temp-text "* H\n:PROPERTIES:\n:A: 1\n:END:"
	    (org-toggle-item nil)
	    (buffer-string))))
  (should
   (equal "- H\nText"
	  (org-test-with-temp-text "* H\n:PROPERTIES:\n:A: 1\n:END:\n\n\nText"
	    (org-toggle-item nil)
	    (buffer-string))))
  ;; When no region is marked and point is on a blank line
  ;; only operate on current line.
  (should
   (equal " \n* H :tag:"
	  (org-test-with-temp-text "<point> \n* H :tag:"
	    (org-toggle-item nil)
	    (buffer-string))))
  ;; When a region is marked and first line is a headline, all
  ;; headlines are turned into items.
  (should
   (equal "- H1\n  - H2"
	  (org-test-with-temp-text "* H1\n** H2"
	    (transient-mark-mode 1)
	    (push-mark (point) t t)
	    (goto-char (point-max))
	    (org-toggle-item nil)
	    (buffer-string))))
  (should
   (equal "- [ ] H1\n  - [ ] H2"
	  (org-test-with-temp-text "* TODO H1\n** TODO H2"
	    (transient-mark-mode 1)
	    (push-mark (point) t t)
	    (goto-char (point-max))
	    (org-toggle-item nil)
	    (buffer-string))))
  ;; When turning headlines into items, make sure headings contents
  ;; are kept within items.
  (should
   (equal "- H1\n  Text"
	  (org-test-with-temp-text "* H1\nText"
	    (transient-mark-mode 1)
	    (push-mark (point) t t)
	    (goto-char (point-max))
	    (org-toggle-item nil)
	    (buffer-string))))
  ;; When a region is marked and first line is an item, all items are
  ;; turned into normal lines.
  (should
   (equal "1\n  2"
	  (org-test-with-temp-text "- 1\n  - 2"
	    (transient-mark-mode 1)
	    (push-mark (point) t t)
	    (goto-char (point-max))
	    (org-toggle-item nil)
	    (buffer-string))))
  (should
   (equal "1\n2"
	  (org-test-with-temp-text "- 1\n2"
	    (transient-mark-mode 1)
	    (push-mark (point) t t)
	    (goto-char (point-max))
	    (org-toggle-item nil)
	    (buffer-string))))
  ;; When a region is marked and first line is an item, all normal
  ;; lines are turned into items.
  (should
   (equal "- line 1\n- line 2"
	  (org-test-with-temp-text "line 1\nline 2"
	    (transient-mark-mode 1)
	    (push-mark (point) t t)
	    (goto-char (point-max))
	    (org-toggle-item nil)
	    (buffer-string))))
  (should
   (equal "- line 1\n- line 2"
	  (org-test-with-temp-text "line 1\n- line 2"
	    (transient-mark-mode 1)
	    (push-mark (point) t t)
	    (goto-char (point-max))
	    (org-toggle-item nil)
	    (buffer-string))))
  ;; When argument ARG is non-nil, change the whole region into
  ;; a single item.
  (should
   (equal "- line 1\n  line 2"
	  (org-test-with-temp-text "line 1\nline 2"
	    (transient-mark-mode 1)
	    (push-mark (point) t t)
	    (goto-char (point-max))
	    (org-toggle-item t)
	    (buffer-string)))))

(ert-deftest test-org-list/sort ()
  "Test `org-sort-list'."
  ;; Sort alphabetically.
  (let ((original-string-collate-lessp (symbol-function 'string-collate-lessp)))
    (cl-letf (((symbol-function 'string-collate-lessp)
	       (lambda (s1 s2 &optional locale ignore-case)
		 (funcall original-string-collate-lessp
			  s1 s2 "C" ignore-case))))
      (should
       (equal "- abc\n- def\n- XYZ\n"
	      (org-test-with-temp-text "- def\n- XYZ\n- abc\n"
		(org-sort-list nil ?a)
		(buffer-string))))
      (should
       (equal "- XYZ\n- def\n- abc\n"
	      (org-test-with-temp-text "- def\n- XYZ\n- abc\n"
		(org-sort-list nil ?A)
		(buffer-string))))
      ;; Sort alphabetically (with case).
      (should
       (equal "- C\n- a\n- b\n"
	      (org-test-with-temp-text "- b\n- C\n- a\n"
		(org-sort-list t ?a)
		(buffer-string))))
      (should
       (equal "- b\n- a\n- C\n"
	      (org-test-with-temp-text "- b\n- C\n- a\n"
		(org-sort-list t ?A)
		(buffer-string))))))
  ;; Sort numerically.
  (should
   (equal "- 1\n- 2\n- 10\n"
	  (org-test-with-temp-text "- 10\n- 1\n- 2\n"
	    (org-sort-list nil ?n)
	    (buffer-string))))
  (should
   (equal "- 10\n- 2\n- 1\n"
	  (org-test-with-temp-text "- 10\n- 1\n- 2\n"
	    (org-sort-list nil ?N)
	    (buffer-string))))
  ;; Sort by checked status.
  (should
   (equal "- [ ] xyz\n- [ ] def\n- [X] abc\n"
	  (org-test-with-temp-text "- [X] abc\n- [ ] xyz\n- [ ] def\n"
	    (org-sort-list nil ?x)
	    (buffer-string))))
  (should
   (equal "- [X] abc\n- [ ] xyz\n- [ ] def\n"
	  (org-test-with-temp-text "- [X] abc\n- [ ] xyz\n- [ ] def\n"
	    (org-sort-list nil ?X)
	    (buffer-string))))
  ;; Sort by time stamp.
  (should
   (equal "- <2017-05-08 Mon>\n- <2017-05-09 Tue>\n- <2018-05-09 Wed>\n"
	  (org-test-with-temp-text
	      "- <2018-05-09 Wed>\n- <2017-05-09 Tue>\n- <2017-05-08 Mon>\n"
	    (org-sort-list nil ?t)
	    (buffer-string))))
  (should
   (equal "- <2018-05-09 Wed>\n- <2017-05-09 Tue>\n- <2017-05-08 Mon>\n"
	  (org-test-with-temp-text
	      "- <2018-05-09 Wed>\n- <2017-05-09 Tue>\n- <2017-05-08 Mon>\n"
	    (org-sort-list nil ?T)
	    (buffer-string))))
  ;; Sort by custom function.
  (should
   (equal "- b\n- aa\n- ccc\n"
	  (org-test-with-temp-text "- ccc\n- b\n- aa\n"
	    (org-sort-list nil ?f
			   (lambda ()
			     (length (buffer-substring (point-at-bol)
						       (point-at-eol))))
			   #'<)
	    (buffer-string))))
  (should
   (equal "- ccc\n- aa\n- b\n"
	  (org-test-with-temp-text "- ccc\n- b\n- aa\n"
	    (org-sort-list nil ?F
			   (lambda ()
			     (length (buffer-substring (point-at-bol)
						       (point-at-eol))))
			   #'<)
	    (buffer-string)))))


;;; List transformations

(ert-deftest test-org-list/to-generic ()
  "Test `org-list-to-generic' specifications."
  ;; Test `:ustart' and `:uend' parameters.
  (should
   (equal
    "begin\na"
    (org-test-with-temp-text "- a"
      (org-list-to-generic (org-list-to-lisp) '(:ustart "begin")))))
  (should-not
   (equal
    "begin\na"
    (org-test-with-temp-text "1. a"
      (org-list-to-generic (org-list-to-lisp) '(:ustart "begin")))))
  (should
   (equal
    "a\nend"
    (org-test-with-temp-text "- a"
      (org-list-to-generic (org-list-to-lisp) '(:uend "end")))))
  (should-not
   (equal
    "a\nend"
    (org-test-with-temp-text "1. a"
      (org-list-to-generic (org-list-to-lisp) '(:uend "end")))))
  (should
   (equal
    "begin l1\na\nbegin l2\nb\nend l2\nend l1"
    (org-test-with-temp-text "- a\n  - b"
      (org-list-to-generic
       (org-list-to-lisp)
       (list :ustart (lambda (l)  (format "begin l%d" l))
	     :uend (lambda (l)  (format "end l%d" l)))))))
  ;; Test `:ostart' and `:oend' parameters.
  (should
   (equal
    "begin\na"
    (org-test-with-temp-text "1. a"
      (org-list-to-generic (org-list-to-lisp) '(:ostart "begin")))))
  (should-not
   (equal
    "begin\na"
    (org-test-with-temp-text "- a"
      (org-list-to-generic (org-list-to-lisp) '(:ostart "begin")))))
  (should
   (equal
    "a\nend"
    (org-test-with-temp-text "1. a"
      (org-list-to-generic (org-list-to-lisp) '(:oend "end")))))
  (should-not
   (equal
    "a\nend"
    (org-test-with-temp-text "- a"
      (org-list-to-generic (org-list-to-lisp) '(:oend "end")))))
  (should
   (equal
    "begin l1\na\nbegin l2\nb\nend l2\nend l1"
    (org-test-with-temp-text "1. a\n  1. b"
      (org-list-to-generic
       (org-list-to-lisp)
       (list :ostart (lambda (l)  (format "begin l%d" l))
	     :oend (lambda (l)  (format "end l%d" l)))))))
  ;; Test `:dstart' and `:dend' parameters.
  (should
   (equal
    "begin\ntaga"
    (org-test-with-temp-text "- tag :: a"
      (org-list-to-generic (org-list-to-lisp) '(:dstart "begin")))))
  (should-not
   (equal
    "begin\na"
    (org-test-with-temp-text "- a"
      (org-list-to-generic (org-list-to-lisp) '(:dstart "begin")))))
  (should
   (equal
    "taga\nend"
    (org-test-with-temp-text "- tag :: a"
      (org-list-to-generic (org-list-to-lisp) '(:dend "end")))))
  (should-not
   (equal
    "a\nend"
    (org-test-with-temp-text "- a"
      (org-list-to-generic (org-list-to-lisp) '(:dend "end")))))
  (should
   (equal
    "begin l1\ntag1a\nbegin l2\ntag2b\nend l2\nend l1"
    (org-test-with-temp-text "- tag1 :: a\n  - tag2 :: b"
      (org-list-to-generic
       (org-list-to-lisp)
       (list :dstart (lambda (l)  (format "begin l%d" l))
	     :dend (lambda (l)  (format "end l%d" l)))))))
  ;; Test `:dtstart', `:dtend', `:ddstart' and `:ddend' parameters.
  (should
   (equal
    ">tag<a"
    (org-test-with-temp-text "- tag :: a"
      (org-list-to-generic (org-list-to-lisp) '(:dtstart ">" :dtend "<")))))
  (should
   (equal
    "tag>a<"
    (org-test-with-temp-text "- tag :: a"
      (org-list-to-generic (org-list-to-lisp) '(:ddstart ">" :ddend "<")))))
  ;; Test `:istart' and `:iend' parameters.
  (should
   (equal
    "starta"
    (org-test-with-temp-text "- a"
      (org-list-to-generic (org-list-to-lisp) '(:istart "start")))))
  (should
   (equal
    "level1 a\nlevel2 b"
    (org-test-with-temp-text "- a\n  - b"
      (org-list-to-generic (org-list-to-lisp)
			   '(:istart (lambda (type l) (format "level%d "l)))))))
  (should
   (equal
    "a\nblevel2level1"
    (org-test-with-temp-text "- a\n  - b"
      (org-list-to-generic (org-list-to-lisp)
			   '(:iend (lambda (type l) (format "level%d" l)))))))
  ;; Test `:icount' parameter.
  (should
   (equal
    "counta"
    (org-test-with-temp-text "1. [@3] a"
      (org-list-to-generic (org-list-to-lisp) '(:icount "count")))))
  (should-not
   (equal
    "counta"
    (org-test-with-temp-text "1. a"
      (org-list-to-generic (org-list-to-lisp) '(:icount "count")))))
  (should
   (equal
    "counta"
    (org-test-with-temp-text "1. [@3] a"
      (org-list-to-generic (org-list-to-lisp)
			   '(:icount "count" :istart "start")))))
  (should
   (equal
    "level:1, counter:3 a"
    (org-test-with-temp-text "1. [@3] a"
      (org-list-to-generic
       (org-list-to-lisp)
       '(:icount (lambda (type l c) (format "level:%d, counter:%d " l c)))))))
  ;; Test `:isep' parameter.
  (should
   (equal
    "a\n--\nb"
    (org-test-with-temp-text "- a\n- b"
      (org-list-to-generic (org-list-to-lisp) '(:isep "--")))))
  (should-not
   (equal
    "a\n--\nb"
    (org-test-with-temp-text "- a\n  - b"
      (org-list-to-generic (org-list-to-lisp) '(:isep "--")))))
  (should
   (equal
    "a\n- 1 -\nb"
    (org-test-with-temp-text "- a\n- b"
      (org-list-to-generic
       (org-list-to-lisp)
       '(:isep (lambda (type depth) (format "- %d -" depth)))))))
  ;; Test `:ifmt' parameter.
  (should
   (equal
    ">> a <<"
    (org-test-with-temp-text "1. [@3] a"
      (org-list-to-generic
       (org-list-to-lisp)
       '(:ifmt (lambda (type c) (format ">> %s <<" c)))))))
  ;; Test `:cbon', `:cboff', `:cbtrans'
  (should
   (equal
    "!a"
    (org-test-with-temp-text "- [X] a"
      (org-list-to-generic (org-list-to-lisp) '(:cbon "!")))))
  (should-not
   (equal
    "!a"
    (org-test-with-temp-text "- [X] a"
      (org-list-to-generic (org-list-to-lisp) '(:cboff "!" :cbtrans "!")))))
  (should
   (equal
    "!a"
    (org-test-with-temp-text "- [ ] a"
      (org-list-to-generic (org-list-to-lisp) '(:cboff "!")))))
  (should-not
   (equal
    "!a"
    (org-test-with-temp-text "- [ ] a"
      (org-list-to-generic (org-list-to-lisp) '(:cbon "!" :cbtrans "!")))))
  (should
   (equal
    "!a"
    (org-test-with-temp-text "- [-] a"
      (org-list-to-generic (org-list-to-lisp) '(:cbtrans "!")))))
  (should-not
   (equal
    "!a"
    (org-test-with-temp-text "- [-] a"
      (org-list-to-generic (org-list-to-lisp) '(:cbon "!" :cboff "!")))))
  ;; Test `:splice' parameter.
  (should
   (equal
    "a"
    (org-test-with-temp-text "- a"
      (org-list-to-generic (org-list-to-lisp)
			   '(:ustart "begin" :uend "end" :splice t)))))
  ;; No error on empty lists.
  (should
   (org-test-with-temp-text "-" (org-list-to-generic (org-list-to-lisp) nil))))

(ert-deftest test-org-list/to-html ()
  "Test `org-list-to-html' specifications."
  (should
   (equal "<ul class=\"org-ul\">\n<li>a</li>\n</ul>"
	  (org-test-with-temp-text "- a"
	    (org-list-to-html (org-list-to-lisp) nil)))))

(ert-deftest test-org-list/to-latex ()
  "Test `org-list-to-latex' specifications."
  (should
   (equal "\\begin{itemize}\n\\item a\n\\end{itemize}"
	  (org-test-with-temp-text "- a"
	    (org-list-to-latex (org-list-to-lisp) nil)))))

(ert-deftest test-org-list/to-texinfo ()
  "Test `org-list-to-texinfo' specifications."
  (should
   (equal "@itemize\n@item\na\n@end itemize"
	  (org-test-with-temp-text "- a"
	    (org-list-to-texinfo (org-list-to-lisp) nil)))))

(ert-deftest test-org-list/to-org ()
  "Test `org-list-to-org' specifications."
  ;; Un-ordered list.
  (should
   (equal "- a"
	  (org-test-with-temp-text "- a"
	    (org-list-to-org (org-list-to-lisp) nil))))
  ;; Ordered list.
  (should
   (equal "1. a"
	  (org-test-with-temp-text "1. a"
	    (org-list-to-org (org-list-to-lisp) nil))))
  ;; Descriptive list.
  (should
   (equal "- a :: b"
	  (org-test-with-temp-text "- a :: b"
	    (org-list-to-org (org-list-to-lisp) nil))))
  ;; Nested list.
  (should
   (equal "- a\n  - b"
	  (org-test-with-temp-text "- a\n  - b"
	    (org-list-to-org (org-list-to-lisp) nil))))
  ;; Item spanning over multiple lines.
  (should
   (equal "- a\n  b"
	  (org-test-with-temp-text "- a\n  b"
	    (org-list-to-org (org-list-to-lisp) nil))))
  ;; Item with continuation text after a sub-list.
  (should
   (equal "- a\n  - b\n  c"
	  (org-test-with-temp-text "- a\n  - b\n  c"
	    (org-list-to-org (org-list-to-lisp) nil)))))


(provide 'test-org-list)
;;; test-org-list.el ends here
