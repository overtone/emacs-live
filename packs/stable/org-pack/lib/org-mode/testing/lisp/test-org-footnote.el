;;; test-org-footnote.el --- Tests for org-footnote.el

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

(ert-deftest test-org-footnote/normalize-in-org ()
  "Test specifications for `org-footnote-normalize' in an Org buffer."
  ;; 1. With a non-nil `org-footnote-section'.
  (let ((org-footnote-section "Footnotes")
	(org-blank-before-new-entry '((heading . auto))))
    ;; 1.1. Normalize each type of footnote: standard, labelled,
    ;;      numbered, inline, anonymous.
    (org-test-with-temp-text
	"Paragraph[fn:1][fn:label][1][fn:inline:Inline][fn::Anonymous]

* Footnotes

\[fn:1] Standard

\[fn:label] Labelled

\[1] Numbered"
      (org-footnote-normalize)
      (should
       (equal (buffer-string)
	      "Paragraph[1][2][3][4][5]

* Footnotes

\[1] Standard

\[2] Labelled

\[3] Numbered

\[4] Inline

\[5] Anonymous


")))
    ;; 1.2. When no footnote section is present, create it.  Follow
    ;;      `org-blank-before-new-entry' specifications when doing so.
    (org-test-with-temp-text "Paragraph[fn:1]\n\n[fn:1] Definition"
      (org-footnote-normalize)
      (should (equal (buffer-string)
		     "Paragraph[1]\n\n* Footnotes\n\n[1] Definition")))
    (org-test-with-temp-text "Paragraph[fn:1]\n* Head1\n[fn:1] Definition"
      (let ((org-blank-before-new-entry '((heading))))
	(org-footnote-normalize))
      (should (equal (buffer-string)
		     "Paragraph[1]\n* Head1\n* Footnotes\n\n[1] Definition")))
    ;; 1.3. When the footnote section is misplaced, move it at the end
    ;;      of the buffer.
    (org-test-with-temp-text "* Head1
Body[fn:1]
* Footnotes
\[fn:1] Definition 1
* Head2"
      (org-footnote-normalize)
      (should
       (equal (buffer-string)
	      "* Head1
Body[1]
* Head2

* Footnotes

\[1] Definition 1"))))
  ;; 2. With a nil `org-footnote-section'.
  (let ((org-footnote-section nil))
    ;; 2.1. Normalize each type of footnote: standard, labelled,
    ;;      numbered, inline, anonymous.
    (org-test-with-temp-text
	"Paragraph[fn:1][fn:label][1][fn:inline:Inline][fn::Anonymous]

\[fn:1] Standard

\[fn:label] Labelled

\[1] Numbered"
      (org-footnote-normalize)
      (should
       (equal (buffer-string)
	      "Paragraph[1][2][3][4][5]

\[1] Standard

\[2] Labelled

\[3] Numbered

\[4] Inline

\[5] Anonymous
")))
    ;; 2.2. Put each footnote definition at the end of the section
    ;;      containing its first reference.
    (org-test-with-temp-text
	"* Head 1
Text[fn:1:Def1]
* Head 2
Text[fn:1]
* Head 3
Text[fn:2:Def2]"
      (org-footnote-normalize)
      (should
       (equal (buffer-string)
	      "* Head 1
Text[1]

\[1] Def1
* Head 2
Text[1]
* Head 3
Text[2]

\[2] Def2
")))))

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
