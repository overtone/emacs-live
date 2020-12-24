;;; test-org-footnote.el --- Tests for org-footnote.el

;; Copyright (C) 2012-2015, 2019  Nicolas Goaziou

;; Author: Nicolas Goaziou <mail at nicolasgoaziou dot fr>

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
   (org-test-with-temp-text " [[https://orgmode.org][Org mode<point>]]"
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
    "[fn:1]"
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
       (line-beginning-position 4)))))
  ;; Do not alter file local variables when inserting new definition
  ;; label.
  (should
   (equal "Paragraph[fn:1]

\[fn:1] 
# Local Variables:
# foo: t
# End:"
	  (org-test-with-temp-text
	      "Paragraph<point>\n# Local Variables:\n# foo: t\n# End:"
	    (let ((org-footnote-section nil)) (org-footnote-new))
	    (buffer-string))))
  (should
   (equal "Paragraph[fn:1]

* Footnotes

\[fn:1] 
# Local Variables:
# foo: t
# End:"
	  (org-test-with-temp-text
	      "Paragraph<point>\n# Local Variables:\n# foo: t\n# End:"
	    (let ((org-footnote-section "Footnotes")) (org-footnote-new))
	    (buffer-string)))))

(ert-deftest test-org-footnote/delete ()
  "Test `org-footnote-delete' specifications."
  ;; Regular test.
  (should
   (equal "Paragraph"
	  (org-test-with-temp-text "Paragraph<point>[fn:1]\n\n[fn:1] Definition"
	    (org-footnote-delete)
	    (org-trim (buffer-string)))))
  ;; Remove multiple definitions and references.
  (should
   (equal "Paragraph and another"
	  (org-test-with-temp-text
	      "Paragraph<point>[fn:1] and another[fn:1]

\[fn:1] def

\[fn:1] def"
	    (org-footnote-delete)
	    (org-trim (buffer-string)))))
  ;; Delete inline footnotes and all references.
  (should
   (equal "Para and"
	  (org-test-with-temp-text "Para<point>[fn:label:def] and[fn:label]"
	    (org-footnote-delete)
	    (org-trim (buffer-string)))))
  ;; Delete anonymous footnotes.
  (should
   (equal "Para"
	  (let ((org-footnote-section nil))
	    (org-test-with-temp-text "Para<point>[fn::def]"
	      (org-footnote-delete)
	      (org-trim (buffer-string))))))
  ;; With an argument, delete footnote with specified label.
  (should
   (equal "Paragraph[fn:1] and another\n\n[fn:1] def"
	  (let ((org-footnote-section nil))
	    (org-test-with-temp-text
		"Paragraph[fn:1] and another[fn:2]\n\n[fn:1] def\n\n[fn:2] def2"
	      (org-footnote-delete "2")
	      (org-trim (buffer-string))))))
  ;; Error when no argument is specified at point is not at a footnote
  ;; reference.
  (should-error
   (org-test-with-temp-text "Para[fn:1]\n\n[fn:1] Def"
     (org-footnote-delete)))
  ;; Correctly delete footnotes with multiple paragraphs.
  (should
   (equal "Para\n\n\nOutside footnote."
	  (let ((org-footnote-section nil))
	    (org-test-with-temp-text
		"Para[fn:1]\n\n[fn:1] para1\n\npara2\n\n\nOutside footnote."
	      (org-footnote-delete "1")
	      (org-trim (buffer-string))))))
  ;; Remove blank lines above the footnote but preserve those after
  ;; it.
  (should
   (equal "Text\n\n\nOther text."
	  (let ((org-footnote-section nil))
	    (org-test-with-temp-text
		"Text[fn:1]\n\n[fn:1] Definition.\n\n\nOther text."
	      (org-footnote-delete "1")
	      (buffer-string)))))
  ;; Preserve file local variables when deleting a footnote.
  (should
   (equal
    "Paragraph\n# Local Variables:\n# foo: t\n# End:"
    (org-test-with-temp-text
	"Paragraph[fn:1]\n[fn:1] Def 1\n# Local Variables:\n# foo: t\n# End:"
      (let ((org-footnote-section nil)) (org-footnote-delete "1"))
      (buffer-string)))))

(ert-deftest test-org-footnote/goto-definition ()
  "Test `org-footnote-goto-definition' specifications."
  ;; Error on unknown definitions.
  (should-error
   (org-test-with-temp-text "No footnote definition"
     (org-footnote-goto-definition "1")))
  ;; Error when trying to reach a definition outside narrowed part of
  ;; buffer.
  (should-error
   (org-test-with-temp-text "Some text<point>\n[fn:1] Definition."
     (narrow-to-region (point-min) (point))
     (org-footnote-goto-definition "1")))
  (should-error
   (org-test-with-temp-text "[fn:1] Definition.\n<point>Some text"
     (narrow-to-region (point) (point-max))
     (org-footnote-goto-definition "1")))
  ;; Otherwise, move at the beginning of the definition, including
  ;; anonymous footnotes.
  (should
   (equal
    " Definition."
    (org-test-with-temp-text "Some text\n[fn:1] Definition."
      (org-footnote-goto-definition "1")
      (buffer-substring (point) (point-max)))))
  (should
   (equal
    "definition]"
    (org-test-with-temp-text "Some text[fn:label:definition]"
      (org-footnote-goto-definition "label")
      (buffer-substring (point) (point-max))))))

(ert-deftest test-org-footnote/goto-previous-reference ()
  "Test `org-footnote-goto-previous-reference' specifications."
  ;; Error on unknown reference.
  (should-error
   (org-test-with-temp-text "No footnote reference"
     (org-footnote-goto-previous-reference "1")))
  ;; Error when trying to reach a reference outside narrowed part of
  ;; buffer.
  (should-error
   (org-test-with-temp-text "Some text<point>\nReference[fn:1]."
     (narrow-to-region (point-min) (point))
     (org-footnote-goto-previous-reference "1")))
  ;; Otherwise, move to closest reference from point.
  (should
   (org-test-with-temp-text "First reference[fn:1]\nReference[fn:1].<point>"
     (org-footnote-goto-previous-reference "1")
     (= (line-end-position) (point-max))))
  (should
   (org-test-with-temp-text "First reference[fn:1]\nReference[fn:1]."
     (org-footnote-goto-previous-reference "1")
     (= (line-beginning-position) (point-min)))))

(ert-deftest test-org-footnote/sort ()
  "Test `org-footnote-sort' specifications."
  ;; Reorder definitions with a nil `org-footnote-section'.  In this
  ;; case each definition is written at the end of the section
  ;; containing its first reference.
  (should
   (equal
    "
Text[fn:1][fn:2]

\[fn:1] Def 1

\[fn:2] Def 2
"
    (org-test-with-temp-text "
Text[fn:1][fn:2]

\[fn:2] Def 2

\[fn:1] Def 1"
      (let ((org-footnote-section nil)) (org-footnote-sort))
      (buffer-string))))
  (should
   (equal
    "
* H1
Text[fn:1]

\[fn:1] Def 1
* H2
Text[fn:2]

\[fn:2] Def 2
"
    (org-test-with-temp-text "
* H1
Text[fn:1]
* H2
Text[fn:2]

\[fn:1] Def 1

\[fn:2] Def 2
"
      (let ((org-footnote-section nil)) (org-footnote-sort))
      (buffer-string))))
  ;; Reorder definitions with a non-nil `org-footnote-section'.
  (should
   (equal
    "
Text[fn:1][fn:2]

* Footnotes

\[fn:1] Def 1

\[fn:2] Def 2
"
    (org-test-with-temp-text "
Text[fn:1][fn:2]

\[fn:2] Def 2

\[fn:1] Def 1"
      (let ((org-footnote-section "Footnotes")) (org-footnote-sort))
      (buffer-string))))
  ;; When `org-footnote-section' is non-nil, clear previous footnote
  ;; sections.
  (should
   (equal
    "
Text[fn:1]

* Headline

* Other headline

* Footnotes

\[fn:1] Def 1
"
    (org-test-with-temp-text "
Text[fn:1]

* Footnotes

\[fn:1] Def 1

* Headline

** Footnotes

* Other headline"
      (let ((org-footnote-section "Footnotes")) (org-footnote-sort))
      (buffer-string))))
  ;; Ignore anonymous footnotes.
  (should
   (equal
    "
Text[fn:1][fn::inline][fn:2]

\[fn:1] Def 1

\[fn:2] Def 2
"
    (org-test-with-temp-text
	"
Text[fn:1][fn::inline][fn:2]

\[fn:2] Def 2

\[fn:1] Def 1"
      (let ((org-footnote-section nil)) (org-footnote-sort))
      (buffer-string))))
  ;; Ignore inline footnotes.
  (should
   (equal
    "
Text[fn:1][fn:label:inline][fn:2]

\[fn:1] Def 1

\[fn:2] Def 2
"
    (org-test-with-temp-text
	"
Text[fn:1][fn:label:inline][fn:2]

\[fn:2] Def 2

\[fn:1] Def 1"
      (let ((org-footnote-section nil)) (org-footnote-sort))
      (buffer-string))))
  ;; Handle (deeply) nested footnotes.
  (should
   (equal
    "
Text[fn:1][fn:3]

\[fn:1] Def 1[fn:2]

\[fn:2] Def 2

\[fn:3] Def 3
"
    (org-test-with-temp-text "
Text[fn:1][fn:3]

\[fn:1] Def 1[fn:2]

\[fn:3] Def 3

\[fn:2] Def 2
"
      (let ((org-footnote-section nil)) (org-footnote-sort))
      (buffer-string))))
  (should
   (equal
    "
Text[fn:1][fn:4]

\[fn:1] Def 1[fn:2]

\[fn:2] Def 2[fn:3]

\[fn:3] Def 3

\[fn:4] Def 4
"
    (org-test-with-temp-text "
Text[fn:1][fn:4]

\[fn:1] Def 1[fn:2]

\[fn:3] Def 3

\[fn:2] Def 2[fn:3]

\[fn:4] Def 4
"
      (let ((org-footnote-section nil)) (org-footnote-sort))
      (buffer-string))))
  ;; When multiple (nested) references are used, make sure to insert
  ;; definition only once.
  (should
   (equal
    "
* Section 1

Text[fn:1]

\[fn:1] Def 1

* Section 2

Text[fn:1]"
    (org-test-with-temp-text
	"
* Section 1

Text[fn:1]

\[fn:1] Def 1

* Section 2

Text[fn:1]"
      (let ((org-footnote-section nil)) (org-footnote-sort))
      (buffer-string))))
  (should
   (equal
    "
Text[fn:1][fn:4]

\[fn:1] Def 1[fn:2][fn:3]

\[fn:2] Def 2[fn:3]

\[fn:3] Def 3

\[fn:4] Def 4
"
    (org-test-with-temp-text "
Text[fn:1][fn:4]

\[fn:1] Def 1[fn:2][fn:3]

\[fn:3] Def 3

\[fn:2] Def 2[fn:3]

\[fn:4] Def 4
"
      (let ((org-footnote-section nil)) (org-footnote-sort))
      (buffer-string))))
  ;; Insert un-referenced definitions at the end.
  (should
   (equal
    "Text[fn:9]

\[fn:9] B

\[fn:1] A
"
    (org-test-with-temp-text "Text[fn:9]\n\n[fn:1] A\n[fn:9] B"
      (let ((org-footnote-section nil)) (org-footnote-sort))
      (buffer-string))))
  ;; When sorting, preserve file local variables.
  (should
   (equal "
Paragraph[fn:1][fn:2]

\[fn:1] Def 1

\[fn:2] Def 2

# Local Variables:
# foo: t
# End:"
	  (org-test-with-temp-text
	      "
Paragraph[fn:1][fn:2]

\[fn:2] Def 2

\[fn:1] Def 1

# Local Variables:
# foo: t
# End:"
	    (let ((org-footnote-section nil)) (org-footnote-sort))
	    (buffer-string)))))

(ert-deftest test-org-footnote/renumber-fn:N ()
  "Test `org-footnote-renumber-fn:N' specifications."
  ;; Renumber (inline) references and definitions.
  (should
   (equal
    "Test[fn:1]"
    (org-test-with-temp-text "Test[fn:99]"
      (org-footnote-renumber-fn:N)
      (buffer-string))))
  (should
   (equal
    "Test[fn:1]\n\n[fn:1] 99"
    (org-test-with-temp-text "Test[fn:99]\n\n[fn:99] 99"
      (org-footnote-renumber-fn:N)
      (buffer-string))))
  (should
   (equal
    "Test[fn:1:99]"
    (org-test-with-temp-text "Test[fn:99:99]"
      (org-footnote-renumber-fn:N)
      (buffer-string))))
  ;; No-op if there's no numbered footnote.
  (should
   (equal
    "Test[fn:label]\n\n[fn:label] Def"
    (org-test-with-temp-text "Test[fn:label]\n\n[fn:label] Def"
      (org-footnote-renumber-fn:N)
      (buffer-string))))
  ;; Definitions without a reference get the highest numbers.
  (should
   (equal
    "Test[fn:1]\n[fn:1] 1\n[fn:2] 99"
    (org-test-with-temp-text "Test[fn:1]\n[fn:1] 1\n[fn:99] 99"
      (org-footnote-renumber-fn:N)
      (buffer-string))))
  ;; Sort labels in sequence.  Anonymous footnotes are ignored.
  (should
   (equal
    "Test[fn:1][fn:2:def][fn:3]"
    (org-test-with-temp-text "Test[fn:4][fn:3:def][fn:2]"
      (org-footnote-renumber-fn:N)
      (buffer-string))))
  (should
   (equal
    "Test[fn:1][fn::def][fn:2]"
    (org-test-with-temp-text "Test[fn:4][fn::def][fn:2]"
      (org-footnote-renumber-fn:N)
      (buffer-string)))))

(ert-deftest test-org-footnote/normalize ()
  "Test `org-footnote-normalize' specifications."
  ;; Normalize regular, inline and anonymous references.
  (should
   (equal
    "Test[fn:1]\n\n[fn:1] def\n"
    (org-test-with-temp-text "Test[fn:label]\n[fn:label] def"
      (let ((org-footnote-section nil)) (org-footnote-normalize))
      (buffer-string))))
  (should
   (equal
    "Test[fn:1]\n\n[fn:1] def\n"
    (org-test-with-temp-text "Test[fn:label:def]"
      (let ((org-footnote-section nil)) (org-footnote-normalize))
      (buffer-string))))
  (should
   (equal
    "Test[fn:1]\n\n[fn:1] def\n"
    (org-test-with-temp-text "Test[fn::def]"
      (let ((org-footnote-section nil)) (org-footnote-normalize))
      (buffer-string))))
  ;; Normalization includes sorting.
  (should
   (equal
    "Test[fn:1][fn:2]\n\n[fn:1] def2\n\n[fn:2] def\n"
    (org-test-with-temp-text "Test[fn:2][fn:1]\n\n[fn:2] def2\n[fn:1] def"
      (let ((org-footnote-section nil)) (org-footnote-normalize))
      (buffer-string))))
  (should
   (equal
    "Test[fn:1][fn:2]\n\n[fn:1] def\n\n[fn:2] inline\n"
    (org-test-with-temp-text "Test[fn:2][fn::inline]\n[fn:2] def\n"
      (let ((org-footnote-section nil)) (org-footnote-normalize))
      (buffer-string))))
  (should
   (equal
    "Test[fn:1][fn:3]

\[fn:1] def[fn:2]

\[fn:2] inline

\[fn:3] last
"
    (org-test-with-temp-text
	"Test[fn:lab1][fn:lab2]\n[fn:lab1] def[fn::inline]\n[fn:lab2] last"
      (let ((org-footnote-section nil)) (org-footnote-normalize))
      (buffer-string))))
  ;; When normalizing an inline reference, fill paragraph whenever the
  ;; `org-footnote-fill-after-inline-note-extraction' is non-nil.
  (should
   (equal
    "Test[fn:1] Next\n\n[fn:1] def\n"
    (org-test-with-temp-text "Test[fn::def]\nNext"
      (let ((org-footnote-section nil)
	    (org-footnote-fill-after-inline-note-extraction t))
	(org-footnote-normalize))
      (buffer-string))))
  ;; Insert un-referenced definitions at the end.
  (should
   (equal
    "Test[fn:1]\nNext\n\n[fn:1] def\n\n[fn:2] A\n"
    (org-test-with-temp-text "Test[fn::def]\nNext\n[fn:unref] A"
      (let ((org-footnote-section nil)) (org-footnote-normalize))
      (buffer-string))))
  ;; Preserve file local variables when normalizing.
  (should
   (equal "
Paragraph[fn:1][fn:2]

\[fn:1] Def 1

\[fn:2] Def 2

# Local Variables:
# foo: t
# End:"
	  (org-test-with-temp-text
	      "
Paragraph[fn:foo][fn:bar]

\[fn:bar] Def 2

\[fn:foo] Def 1

# Local Variables:
# foo: t
# End:"
	    (let ((org-footnote-section nil)) (org-footnote-normalize))
	    (buffer-string)))))


(provide 'test-org-footnote)
;;; test-org-footnote.el ends here
