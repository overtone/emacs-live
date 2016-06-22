;;; test-org-footnote.el --- Tests for org-footnote.el

;; Copyright (C) 2012-2015  Nicolas Goaziou

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

(ert-deftest test-org-footnote/new ()
  "Test `org-footnote-new' specifications."
  ;; `org-footnote-auto-label' is t.
  (should
   (string-match-p
    "Test\\[fn:1\\]\n+\\[fn:1\\]"
    (org-test-with-temp-text "Test<point>"
      (let ((org-footnote-auto-label t)
	    (org-footnote-section nil))
	(org-footnote-new))
      (buffer-string))))
  ;; `org-footnote-auto-label' is `plain'.
  (should
   (string-match-p
    "Test\\[1\\]\n+\\[1\\]"
    (org-test-with-temp-text "Test<point>"
      (let ((org-footnote-auto-label 'plain)
	    (org-footnote-section nil))
	(org-footnote-new))
      (buffer-string))))
  ;; `org-footnote-auto-label' is `random'.
  (should
   (string-match-p
    "Test\\[fn:\\(.+?\\)\\]\n+\\[fn:\\1\\]"
    (org-test-with-temp-text "Test<point>"
      (let ((org-footnote-auto-label 'random)
	    (org-footnote-section nil))
	(org-footnote-new))
      (buffer-string))))
  ;; Error at beginning of line.
  (should-error
   (org-test-with-temp-text "<point>Test"
     (org-footnote-new)))
  ;; Error at keywords.
  (should-error
   (org-test-with-temp-text "#+TIT<point>LE: value"
     (org-footnote-new)))
  (should-error
   (org-test-with-temp-text "#+CAPTION: <point>\nParagraph"
     (org-footnote-new)))
  ;; Allow new footnotes in blank lines at the beginning of the
  ;; document.
  (should
   (string-match-p
    " \\[fn:1\\]"
    (org-test-with-temp-text " <point>"
      (let ((org-footnote-auto-label t)) (org-footnote-new))
      (buffer-string))))
  ;; In an headline or inlinetask, point must be either on the
  ;; heading itself or on the blank lines below.
  (should (org-test-with-temp-text "* H<point>" (org-footnote-new) t))
  (should
   (org-test-with-temp-text "* H\n <point>\nParagraph" (org-footnote-new) t))
  (should-error (org-test-with-temp-text "*<point> H" (org-footnote-new) t))
  (should-error
   (org-test-with-temp-text "* H <point>:tag:" (org-footnote-new) t))
  ;; Allow new footnotes within recursive objects, but not in links.
  (should
   (string-match-p
    " \\*bold\\[fn:1\\]\\*"
    (org-test-with-temp-text " *bold<point>*"
      (let ((org-footnote-auto-label t)) (org-footnote-new))
      (buffer-string))))
  (should-error
   (org-test-with-temp-text " [[http://orgmode.org][Org mode<point>]]"
     (org-footnote-new)))
  ;; Allow new footnotes in blank lines after an element or white
  ;; spaces after an object.
  (should
   (string-match-p
    " \\[fn:1\\]"
    (org-test-with-temp-text "#+BEGIN_EXAMPLE\nA\n#+END_EXAMPLE\n <point>"
      (let ((org-footnote-auto-label t)) (org-footnote-new))
      (buffer-string))))
  (should
   (string-match-p
    " \\*bold\\*\\[fn:1\\]"
    (org-test-with-temp-text " *bold*<point>"
      (let ((org-footnote-auto-label t)) (org-footnote-new))
      (buffer-string))))
  ;; When creating a new footnote, move to its definition.
  (should
   (string=
    "[fn:1] "
    (org-test-with-temp-text "Text<point>"
      (let ((org-footnote-auto-label t)
	    (org-footnote-auto-adjust nil))
	(org-footnote-new))
      (buffer-substring-no-properties (line-beginning-position) (point)))))
  ;; Re-order and re-label footnotes properly when
  ;; `org-footnote-auto-adjust' is non-nil.
  (should
   (string=
    "[fn:1] 1\n\n[fn:2] \n\n[fn:3] 2\n"
    (org-test-with-temp-text
	"Text[fn:1]Text<point>Text[fn:2]\n\n[fn:1] 1\n\n[fn:2] 2"
      (let ((org-footnote-auto-label t)
	    (org-footnote-auto-adjust t)
	    (org-footnote-section nil))
	(org-footnote-new))
      (buffer-substring-no-properties
       (line-beginning-position -1)
       (line-beginning-position 4))))))

(ert-deftest test-org-footnote/delete ()
  "Test `org-footnote-delete' specifications."
  ;; Regular test.
  (should
   (equal "Paragraph"
	  (org-test-with-temp-text "Paragraph[1]\n\n[1] Definition"
	    (search-forward "[")
	    (org-footnote-delete)
	    (org-trim (buffer-string)))))
  ;; Remove multiple definitions and references.
  (should
   (equal "Paragraph and another"
	  (org-test-with-temp-text
	      "Paragraph[1] and another[1]\n\n[1] def\n\n[1] def"
	    (search-forward "[")
	    (org-footnote-delete)
	    (org-trim (buffer-string)))))
  ;; Delete inline footnotes and all references.
  (should
   (equal "Para and"
	  (org-test-with-temp-text "Para[fn:label:def] and[fn:label]"
	    (search-forward "[")
	    (org-footnote-delete)
	    (org-trim (buffer-string)))))
  ;; Delete anonymous footnotes.
  (should
   (equal "Para"
	  (org-test-with-temp-text "Para[fn::def]"
	    (search-forward "[")
	    (org-footnote-delete)
	    (org-trim (buffer-string)))))
  ;; With an argument, delete footnote with specified label.
  (should
   (equal "Paragraph[1] and another\n\n[1] def"
	  (let ((org-footnote-section nil))
	    (org-test-with-temp-text
		"Paragraph[1] and another[2]\n\n[1] def\n\n[2] def2"
	      (org-footnote-delete "2")
	      (org-trim (buffer-string))))))
  ;; Error when no argument is specified at point is not at a footnote
  ;; reference.
  (should-error
   (org-test-with-temp-text "Para[1]\n\n[1] Def"
     (org-footnote-delete)))
  ;; Correctly delete footnotes with multiple paragraphs.
  (should
   (equal "Para\n\n\nOutside footnote."
	  (org-test-with-temp-text
	      "Para[1]\n\n[1] para1\n\npara2\n\n\nOutside footnote."
	    (org-footnote-delete "1")
	    (org-trim (buffer-string))))))

(ert-deftest test-org-footnote/goto-definition ()
  "Test `org-footnote-goto-definition' specifications."
  ;; Error on unknown definitions.
  (should-error
   (org-test-with-temp-text "No footnote definition"
     (org-footnote-goto-definition "fn:1")))
  ;; Error when trying to reach a definition outside narrowed part of
  ;; buffer.
  (should-error
   (org-test-with-temp-text "Some text<point>\n[fn:1] Definition."
     (narrow-to-region (point-min) (point))
     (org-footnote-goto-definition "fn:1")))
  (should-error
   (org-test-with-temp-text "[fn:1] Definition.\n<point>Some text"
     (narrow-to-region (point) (point-max))
     (org-footnote-goto-definition "fn:1")))
  ;; Otherwise, move at the beginning of the definition, including
  ;; anonymous footnotes.
  (should
   (equal
    "Definition."
    (org-test-with-temp-text "Some text\n[fn:1] Definition."
      (org-footnote-goto-definition "fn:1")
      (buffer-substring (point) (point-max)))))
  (should
   (equal
    "definition]"
    (org-test-with-temp-text "Some text[fn:label:definition]"
      (org-footnote-goto-definition "fn:label")
      (buffer-substring (point) (point-max))))))

(ert-deftest test-org-footnote/normalize-in-org ()
  "Test specifications for `org-footnote-normalize' in an Org buffer."
  ;; With a non-nil `org-footnote-section', normalize each type of
  ;; footnote: standard, labelled, numbered, inline, anonymous.
  (should
   (equal "Paragraph[1][2][3][4][5]

* Footnotes

\[1] Standard

\[2] Labelled

\[3] Numbered

\[4] Inline

\[5] Anonymous


"
	  (let ((org-footnote-section "Footnotes")
		(org-blank-before-new-entry '((heading . auto))))
	    (org-test-with-temp-text
		"Paragraph[fn:1][fn:label][1][fn:inline:Inline][fn::Anonymous]

* Footnotes

\[fn:1] Standard

\[fn:label] Labelled

\[1] Numbered"
	      (org-footnote-normalize)
	      (buffer-string)))))
  ;; When no footnote section is present, create it.  Follow
  ;; `org-blank-before-new-entry' specifications when doing so.
  (should
   (equal "Paragraph[1]\n\n* Footnotes\n\n[1] Definition"
	  (let ((org-footnote-section "Footnotes")
		(org-blank-before-new-entry '((heading . auto))))
	    (org-test-with-temp-text "Paragraph[fn:1]\n\n[fn:1] Definition"
	      (org-footnote-normalize)
	      (buffer-string)))))
  (should
   (equal
    "Paragraph[1]\n* Head1\n* Footnotes\n\n[1] Definition"
    (let ((org-footnote-section "Footnotes")
	  (org-blank-before-new-entry '((heading))))
      (org-test-with-temp-text "Paragraph[fn:1]\n* Head1\n[fn:1] Definition"
	(org-footnote-normalize)
	(buffer-string)))))
  ;; When the footnote section is misplaced, move it at the end of
  ;; the buffer.
  (should
   (equal
    "* Head1
Body[1]
* Head2

* Footnotes

\[1] Definition 1"
    (let ((org-footnote-section "Footnotes")
	  (org-blank-before-new-entry '((heading . auto))))
      (org-test-with-temp-text "* Head1
Body[fn:1]
* Footnotes
\[fn:1] Definition 1
* Head2"
	(org-footnote-normalize)
	(buffer-string)))))
  ;; With a nil `org-footnote-section', normalize each type of
  ;; footnote: standard, labelled, numbered, inline, anonymous.
  (should
   (equal "Paragraph[1][2][3][4][5]

\[1] Standard

\[2] Labelled

\[3] Numbered

\[4] Inline

\[5] Anonymous
"
	  (let ((org-footnote-section nil))
	    (org-test-with-temp-text
		"Paragraph[fn:1][fn:label][1][fn:inline:Inline][fn::Anonymous]

\[fn:1] Standard

\[fn:label] Labelled

\[1] Numbered"
	      (org-footnote-normalize)
	      (buffer-string)))))
  ;; Also put each footnote definition at the end of the section
  ;; containing its first reference.
  (should
   (equal "* Head 1
Text[1]

\[1] Def1
* Head 2
Text[1]
* Head 3
Text[2]

\[2] Def2
"
	  (let ((org-footnote-section nil))
	    (org-test-with-temp-text
		"* Head 1
Text[fn:1:Def1]
* Head 2
Text[fn:1]
* Head 3
Text[fn:2:Def2]"
	      (org-footnote-normalize)
	      (buffer-string))))))

(ert-deftest test-org-footnote/normalize-outside-org ()
  "Test `org-footnote-normalize' specifications for buffers not in Org mode."
  ;; 1. In a non-Org buffer, footnotes definitions are always put at
  ;;    its end.
  (should
   (equal
    "Paragraph[1][2][3][4][5]


Some additional text.

\[1] Standard

\[2] Labelled

\[3] Numbered

\[4] Inline

\[5] Anonymous"
    (let ((org-footnote-tag-for-non-org-mode-files nil))
      (with-temp-buffer
	(insert "Paragraph[fn:1][fn:label][1][fn:inline:Inline][fn::Anonymous]

\[fn:1] Standard

\[fn:label] Labelled

\[1] Numbered


Some additional text.")
	(org-footnote-normalize)
	(buffer-string)))))
  ;; 2. With a special tag.
  (let ((org-footnote-tag-for-non-org-mode-files "Footnotes:"))
    ;; 2.1. The tag must be inserted before the footnotes, separated
    ;;      from the rest of the text with a blank line.
    (with-temp-buffer
      (insert "Paragraph[fn:1][fn::Anonymous]

\[fn:1] Standard


Some additional text.")
      (org-footnote-normalize)
      (should
       (equal (buffer-string)
	      "Paragraph[1][2]


Some additional text.

Footnotes:

\[1] Standard

\[2] Anonymous")))
    ;; 2.2. Any tag already inserted in the buffer should be removed
    ;;      prior to footnotes insertion.
    (with-temp-buffer
      (insert "Text[fn:1]
Footnotes:

Additional text.

Footnotes:

\[fn:1] Definition")
      (org-footnote-normalize)
      (should
       (equal (buffer-string)
	      "Text[1]

Additional text.

Footnotes:

\[1] Definition"))))
  ;; 3. As an exception, in `message-mode' buffer, if a signature is
  ;;    present, insert footnotes before it.n
  (let ((org-footnote-tag-for-non-org-mode-files nil))
    (with-temp-buffer
      (insert "Body[fn::def]
-- 
Fake signature
-- 
Signature")
      ;; Mimic `message-mode'.
      (let ((major-mode 'message-mode)
	    (message-cite-prefix-regexp "\\([ 	]*[_.[:word:]]+>+\\|[ 	]*[]>|]\\)+")
	    (message-signature-separator "^-- $"))
	(flet ((message-point-in-header-p nil nil))
	  (org-footnote-normalize)))
      (should
       (equal (buffer-string)
              "Body[1]
-- 
Fake signature

\[1] def

-- 
Signature")))))

(ert-deftest test-org-footnote/sort ()
  "Test footnotes definitions sorting."
  (let ((org-footnote-section nil))
    (org-test-with-temp-text
        "Text[fn:1][fn::inline][fn:2][fn:label]

\[fn:label] C

\[fn:1] A

\[fn:2] B"
    (org-footnote-normalize 'sort)
    (should
     (equal (buffer-string)
            "Text[fn:1][fn::inline][fn:2][fn:label]

\[fn:1] A

\[fn:2] B

\[fn:label] C
")))))


(provide 'test-org-footnote)
;;; test-org-footnote.el ends here
