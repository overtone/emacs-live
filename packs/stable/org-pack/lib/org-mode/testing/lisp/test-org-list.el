;;; test-org-list.el --- Tests for org-list.el

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
  ;; 1. Error when not at an item.
  (org-test-with-temp-text "Paragraph."
    (should-error (org-indent-item)))
  ;; 2. Error when trying to move first item of a list.
  (org-test-with-temp-text "
- Item 1
- Item 2"
    (forward-line)
    (should-error (org-indent-item)))
  ;; 3. Indent a single item, not its children.
  (org-test-with-temp-text "
- Item 1
- Item 2
  - Item 2.1"
    (search-forward "- Item 2")
    (let (org-list-demote-modify-bullet) (org-indent-item))
    (should (equal (buffer-string)
		   "
- Item 1
  - Item 2
  - Item 2.1")))
  ;; 4. Follow `org-list-demote-modify-bullet' specifications.
  ;;
  ;; 4.1. With unordered lists.
  (org-test-with-temp-text "
- Item 1
- Item 2"
    (search-forward "- Item 2")
    (let ((org-list-demote-modify-bullet '(("-" . "+")))) (org-indent-item))
    (should (equal (buffer-string)
		   "
- Item 1
  + Item 2")))
  ;; 4.2. and ordered lists.
  (org-test-with-temp-text "
1. Item 1
2. Item 2"
    (search-forward "2. Item 2")
    (let ((org-plain-list-ordered-item-terminator t)
	  (org-list-demote-modify-bullet '(("1." . "+"))))
      (org-indent-item))
    (should (equal (buffer-string)
		   "
1. Item 1
   + Item 2")))
  ;; 5. When a region is selected, indent every item within.
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
    (let (org-list-demote-modify-bullet) (org-indent-item))
    (should (equal (buffer-string)
		   "
- Item 1
  - Item 2
  - Item 3
"))))

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

(ert-deftest test-org-list/move-item-down ()
  "Test `org-move-item-down' specifications."
  ;; Standard test.
  (org-test-with-temp-text "- item 1\n- item 2"
    (org-move-item-down)
    (should (equal (buffer-string)
		   "- item 2\n- item 1")))
  ;; Keep same column in item.
  (org-test-with-temp-text "- item 1\n- item 2"
    (forward-char 4)
    (org-move-item-down)
    (should (looking-at "em 1")))
  ;; Move sub-items.
  (org-test-with-temp-text "- item 1\n  - sub-item 1\n- item 2"
    (org-move-item-down)
    (should (equal (buffer-string)
		   "- item 2\n- item 1\n  - sub-item 1")))
  ;; Preserve blank lines.
  (org-test-with-temp-text "- item 1\n\n- item 2"
    (let ((org-list-empty-line-terminates-plain-lists nil)) (org-move-item-down))
    (should (equal (buffer-string) "- item 2\n\n- item 1")))
  ;; Error when trying to move the last item...
  (org-test-with-temp-text "- item 1\n- item 2"
    (forward-line)
    (should-error (org-move-item-down)))
  ;; ... unless `org-list-use-circular-motion' is non-nil.  In this
  ;; case, move to the first item.
  (org-test-with-temp-text "- item 1\n- item 2\n- item 3"
    (forward-line 2)
    (let ((org-list-use-circular-motion t)) (org-move-item-down))
    (should (equal (buffer-string) "- item 3\n- item 1\n- item 2\n")))
  ;; Preserve item visibility.
  (org-test-with-temp-text "* Headline\n- item 1\n  body 1\n- item 2\n  body 2"
    (let ((org-cycle-include-plain-lists t))
      (search-forward "- item 1")
      (org-cycle)
      (search-forward "- item 2")
      (org-cycle))
    (search-backward "- item 1")
    (org-move-item-down)
    (forward-line)
    (should (org-invisible-p2))
    (search-backward " body 2")
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
    (search-backward "- item 1")
    (org-move-item-down)
    (search-forward "sub-body 1")
    (should (org-invisible-p2))
    (search-backward "sub-body 2")
    (should (org-invisible-p2)))
  ;; Preserve contents visibility.
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
    (search-forward "- item 1")
    (org-move-item-down)
    (search-forward "Text1")
    (should (org-invisible-p2))
    (search-backward "Text2")
    (should (org-invisible-p2))))

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
  (org-test-with-temp-text "- item 1\n\n- item 2"
    (search-forward "- item 2")
    (let ((org-list-empty-line-terminates-plain-lists nil)) (org-move-item-up))
    (should (equal (buffer-string) "- item 2\n\n- item 1")))
  ;; Error when trying to move the first item...
  (org-test-with-temp-text "- item 1\n- item 2"
    (should-error (org-move-item-up)))
  ;; ... unless `org-list-use-circular-motion' is non-nil.  In this
  ;; case, move to the first item.
  (org-test-with-temp-text "- item 1\n- item 2\n- item 3"
    (let ((org-list-use-circular-motion t)) (org-move-item-up))
    (should (equal (buffer-string) "- item 2\n- item 3\n- item 1")))
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
    (should (org-invisible-p2)))
  ;; Preserve contents visibility.
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
    (search-forward "- item 2")
    (org-move-item-up)
    (search-forward "Text2")
    (should (org-invisible-p2))
    (search-forward "Text1")
    (should (org-invisible-p2))))

(ert-deftest test-org-list/insert-item ()
  "Test item insertion."
  ;; Blank lines specifications.
  ;;
  ;; Non-nil `org-blank-before-new-entry': insert a blank line, unless
  ;; `org-list-empty-line-terminates-plain-lists' is non-nil.
  (should
   (org-test-with-temp-text "- a"
     (let ((org-list-empty-line-terminates-plain-lists nil)
	   (org-blank-before-new-entry '((plain-list-item . t))))
       (end-of-line)
       (org-insert-item)
       (forward-line -1)
       (looking-at "$"))))
  (should-not
   (org-test-with-temp-text "- a"
     (let ((org-list-empty-line-terminates-plain-lists t)
	   (org-blank-before-new-entry '((plain-list-item . t))))
       (end-of-line)
       (org-insert-item)
       (forward-line -1)
       (looking-at "$"))))
  ;; Nil `org-blank-before-new-entry': do not insert a blank line.
  (should-not
   (org-test-with-temp-text "- a"
     (let ((org-list-empty-line-terminates-plain-lists nil)
	   (org-blank-before-new-entry '((plain-list-item . nil))))
       (end-of-line)
       (org-insert-item)
       (forward-line -1)
       (looking-at "$"))))
  ;; `org-blank-before-new-entry' set to auto: if there's no blank
  ;; line already in the sole item, do not insert one.
  (should-not
   (org-test-with-temp-text "- a"
     (let ((org-list-empty-line-terminates-plain-lists nil)
	   (org-blank-before-new-entry '((plain-list-item . auto))))
       (end-of-line)
       (org-insert-item)
       (forward-line -1)
       (looking-at "$"))))
  ;; `org-blank-before-new-entry' set to `auto': if there's a blank
  ;; line in the sole item, insert another one.
  (should
   (org-test-with-temp-text "- a\n\n  b"
     (let ((org-list-empty-line-terminates-plain-lists nil)
	   (org-blank-before-new-entry '((plain-list-item . auto))))
       (goto-char (point-max))
       (org-insert-item)
       (forward-line -1)
       (looking-at "$"))))
  ;; `org-blank-before-new-entry' set to `auto': if the user specified
  ;; a blank line, preserve it.
  (should
   (org-test-with-temp-text "- a\n\n"
     (let ((org-list-empty-line-terminates-plain-lists nil)
	   (org-blank-before-new-entry '((plain-list-item . auto))))
       (goto-char (point-max))
       (org-insert-item)
       (forward-line -1)
       (looking-at "$"))))
  ;; `org-blank-before-new-entry' set to `auto': if some items in list
  ;; are already separated by blank lines, insert one.
  (should
   (org-test-with-temp-text "- a\n\n- b"
     (let ((org-list-empty-line-terminates-plain-lists nil)
	   (org-blank-before-new-entry '((plain-list-item . auto))))
       (goto-char (point-max))
       (org-insert-item)
       (forward-line -1)
       (looking-at "$"))))
  (should
   (org-test-with-temp-text "- a\n\n- b"
     (let ((org-list-empty-line-terminates-plain-lists nil)
	   (org-blank-before-new-entry '((plain-list-item . auto))))
       (org-insert-item)
       (forward-line)
       (looking-at "$"))))
  (should
   (org-test-with-temp-text "- a\n  #+BEGIN_EXAMPLE\n\n  x\n  #+END_EXAMPLE"
     (let ((org-list-empty-line-terminates-plain-lists nil)
	   (org-blank-before-new-entry '((plain-list-item . auto))))
       (goto-char (point-max))
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



;;; Radio Lists

(ert-deftest test-org-list/send-list ()
  "Test various checks for `org-list-send-list'."
  ;; Error when not at a list item.
  (should-error
   (org-test-with-temp-text "Not a list item"
     (org-list-send-list)))
  ;; Error when ORGLST line is not provided.
  (should-error
   (org-test-with-temp-text "- item"
     (org-list-send-list)))
  ;; Error when transformation function is unknown.
  (should-error
   (org-test-with-temp-text "@ignore
#+ORGLST: SEND list unknown-function
- item
@end ignore"
     (forward-line 2)
     (org-list-send-list)))
  ;; Error when receiving location is not defined.
  (should-error
   (org-test-with-temp-text "@ignore
#+ORGLST: SEND list org-list-to-texinfo
- item
@end ignore"
     (forward-line 2)
     (org-list-send-list)))
  ;; Error when insertion region is ill-formed.
  (should-error
   (org-test-with-temp-text "@c BEGIN RECEIVE ORGLST list
@ignore
#+ORGLST: SEND list org-list-to-texinfo
- item
@end ignore"
     (forward-line 3)
     (org-list-send-list))))

(ert-deftest test-org-list/to-html ()
  "Test `org-list-to-html' specifications."
  (should
   (equal "<ul class=\"org-ul\">\n<li>a</li>\n</ul>"
	  (let (org-html-indent)
	    (with-temp-buffer
	      (insert "<!-- BEGIN RECEIVE ORGLST name -->
<!-- END RECEIVE ORGLST name -->
<!--
#+ORGLST: SEND name org-list-to-html
- a
-->")
	      (goto-char (point-min))
	      (re-search-forward "^- a" nil t)
	      (beginning-of-line)
	      (org-list-send-list)
	      (goto-line 2)
	      (buffer-substring-no-properties
	       (point)
	       (progn (re-search-forward "^<!-- END" nil t)
		      (beginning-of-line)
		      (skip-chars-backward " \r\t\n")
		      (point))))))))

(ert-deftest test-org-list/to-latex ()
  "Test `org-list-to-latex' specifications."
  (should
   (equal "\\begin{itemize}\n\\item a\n\\end{itemize}"
	  (with-temp-buffer
	    (insert "% BEGIN RECEIVE ORGLST name
% END RECEIVE ORGLST name
\\begin{comment}
#+ORGLST: SEND name org-list-to-latex
- a
\\end{comment}")
	    (goto-char (point-min))
	    (re-search-forward "^- a" nil t)
	    (beginning-of-line)
	    (org-list-send-list)
	    (goto-line 2)
	    (buffer-substring-no-properties
	     (point)
	     (progn (re-search-forward "^% END" nil t)
		    (beginning-of-line)
		    (skip-chars-backward " \r\t\n")
		    (point)))))))

(ert-deftest test-org-list/to-texinfo ()
  "Test `org-list-to-texinfo' specifications."
  (should
   (equal "@itemize\n@item\na\n@end itemize"
	  (with-temp-buffer
	    (insert "@c BEGIN RECEIVE ORGLST name
@c END RECEIVE ORGLST name
@ignore
#+ORGLST: SEND name org-list-to-texinfo
- a
@end ignore")
	    (goto-char (point-min))
	    (re-search-forward "^- a" nil t)
	    (beginning-of-line)
	    (org-list-send-list)
	    (goto-line 2)
	    (buffer-substring-no-properties
	     (point)
	     (progn (re-search-forward "^@c END" nil t)
		    (beginning-of-line)
		    (skip-chars-backward " \r\t\n")
		    (point)))))))


(provide 'test-org-list)
;;; test-org-list.el ends here
