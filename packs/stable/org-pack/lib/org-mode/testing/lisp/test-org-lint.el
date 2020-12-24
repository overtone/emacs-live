;;; test-org-lint.el --- Tests for Org Lint          -*- lexical-binding: t; -*-

;; Copyright (C) 2016, 2019  Nicolas Goaziou

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

;;; Code:

(ert-deftest test-org-lint/duplicate-custom-id ()
  "Test `org-lint-duplicate-custom-id' checker."
  (should
   (org-test-with-temp-text "
* H1
:PROPERTIES:
:CUSTOM_ID: foo
:END:

* H2
:PROPERTIES:
:CUSTOM_ID: foo
:END:"
     (org-lint '(duplicate-custom-id))))
  (should-not
   (org-test-with-temp-text "
* H1
:PROPERTIES:
:CUSTOM_ID: foo
:END:

* H2
:PROPERTIES:
:CUSTOM_ID: bar
:END:"
     (org-lint '(duplicate-custom-id)))))

(ert-deftest test-org-lint/duplicate-name ()
  "Test `org-lint-duplicate-name' checker."
  (should
   (org-test-with-temp-text "
#+name: foo
Paragraph1

#+name: foo
Paragraph 2"
     (org-lint '(duplicate-name))))
  (should-not
   (org-test-with-temp-text "
#+name: foo
Paragraph1

#+name: bar
Paragraph 2"
     (org-lint '(duplicate-name)))))

(ert-deftest test-org-lint/duplicate-target ()
  "Test `org-lint-duplicate-target' checker."
  (should
   (org-test-with-temp-text "<<foo>> <<foo>>"
     (org-lint '(duplicate-target))))
  (should-not
   (org-test-with-temp-text "<<foo>> <<bar>>"
     (org-lint '(duplicate-target)))))

(ert-deftest test-org-lint/duplicate-footnote-definition ()
  "Test `org-lint-duplicate-footnote-definition' checker."
  (should
   (org-test-with-temp-text "
\[fn:1] Definition 1

\[fn:1] Definition 2"
     (org-lint '(duplicate-footnote-definition))))
  (should-not
   (org-test-with-temp-text "
\[fn:1] Definition 1

\[fn:2] Definition 2"
     (org-lint '(duplicate-footnote-definition)))))

(ert-deftest test-org-lint/orphaned-affiliated-keywords ()
  "Test `org-lint-orphaned-affiliated-keywords' checker."
  (should
   (org-test-with-temp-text "#+name: foo"
     (org-lint '(orphaned-affiliated-keywords)))))

(ert-deftest test-org-lint/deprecated-export-blocks ()
  "Test `org-lint-deprecated-export-blocks' checker."
  (should
   (org-test-with-temp-text "
#+begin_latex
...
#+end_latex"
     (org-lint '(deprecated-export-blocks)))))

(ert-deftest test-org-lint/deprecated-header-syntax ()
  "Test `org-lint-deprecated-header-syntax' checker."
  (should
   (org-test-with-temp-text "#+property: cache yes"
     (org-lint '(deprecated-header-syntax))))
  (should
   (org-test-with-temp-text "
* H
:PROPERTIES:
:cache: yes
:END:"
     (org-lint '(deprecated-header-syntax)))))

(ert-deftest test-org-lint/missing-language-in-src-block ()
  "Test `org-lint-missing-language-in-src-block' checker."
  (should
   (org-test-with-temp-text "
#+begin_src
...
#+end_src"
     (org-lint '(missing-language-in-src-block)))))

(ert-deftest test-org-lint/missing-backend-in-export-block ()
  "Test `org-lint-missing-backend-in-export-block' checker."
  (should
   (org-test-with-temp-text "
#+begin_export
...
#+end_export"
     (org-lint '(missing-backend-in-export-block)))))

(ert-deftest test-org-lint/invalid-babel-call-block ()
  "Test `org-lint-invalid-babel-call-block' checker."
  (should
   (org-test-with-temp-text "#+call:"
     (org-lint '(invalid-babel-call-block))))
  (should
   (org-test-with-temp-text "#+call: foo() [:exports code]"
     (org-lint '(invalid-babel-call-block)))))

(ert-deftest test-org-lint/deprecated-category-setup ()
  "Test `org-lint-deprecated-category-setup' checker."
  (should
   (org-test-with-temp-text "#+category: foo\n#+category: bar"
     (org-lint '(deprecated-category-setup)))))

(ert-deftest test-org-lint/invalid-coderef-link ()
  "Test `org-lint-invalid-coderef-link' checker."
  (should
   (org-test-with-temp-text "[[(unknown)]]"
     (org-lint '(invalid-coderef-link))))
  (should-not
   (org-test-with-temp-text "[[(foo)]]
#+begin_src emacs-lisp -l \"; ref:%s\"
(+ 1 1) ; ref:foo
#+end_src"
     (org-lint '(invalid-coderef-link)))))

(ert-deftest test-org-lint/invalid-custom-id-link ()
  "Test `org-lint-invalid-custom-id-link' checker."
  (should
   (org-test-with-temp-text "[[#unknown]]"
     (org-lint '(invalid-custom-id-link))))
  (should-not
   (org-test-with-temp-text "[[#foo]]
* H
:PROPERTIES:
:CUSTOM_ID: foo
:END:"
     (org-lint '(invalid-custom-id-link)))))

(ert-deftest test-org-lint/invalid-fuzzy-link ()
  "Test `org-lint-invalid-fuzzy-link' checker."
  (should
   (org-test-with-temp-text "[[*unknown]]"
     (org-lint '(invalid-fuzzy-link))))
  (should-not
   (org-test-with-temp-text "[[*foo]]\n* foo"
     (org-lint '(invalid-fuzzy-link))))
  (should
   (org-test-with-temp-text "[[unknown]]"
     (org-lint '(invalid-fuzzy-link))))
  (should-not
   (org-test-with-temp-text "[[foo]]\n#+name: foo\nParagraph"
     (org-lint '(invalid-fuzzy-link))))
  (should-not
   (org-test-with-temp-text "[[foo]]\n<<foo>>"
     (org-lint '(invalid-fuzzy-link)))))

(ert-deftest test-org-lint/special-property-in-properties-drawer ()
  "Test `org-lint-special-property-in-properties-drawer' checker."
  (should
   (org-test-with-temp-text "
* H
:PROPERTIES:
:TODO: foo
:END:"
     (org-lint '(special-property-in-properties-drawer)))))

(ert-deftest test-org-lint/obsolete-properties-drawer ()
  "Test `org-lint-obsolete-properties-drawer' checker."
  (should-not
   (org-test-with-temp-text "
* H
:PROPERTIES:
:SOMETHING: foo
:END:"
     (org-lint '(obsolete-properties-drawer))))
  (should-not
   (org-test-with-temp-text "
* H
SCHEDULED: <2012-03-29>
:PROPERTIES:
:SOMETHING: foo
:END:"
     (org-lint '(obsolete-properties-drawer))))
  (should-not
   (org-test-with-temp-text ":PROPERTIES:
:SOMETHING: foo
:END:"
     (org-lint '(obsolete-properties-drawer))))
  (should-not
   (org-test-with-temp-text "# Comment
:PROPERTIES:
:SOMETHING: foo
:END:"
     (org-lint '(obsolete-properties-drawer))))
  (should
   (org-test-with-temp-text "
* H
Paragraph
:PROPERTIES:
:SOMETHING: foo
:END:"
     (org-lint '(obsolete-properties-drawer))))
  (should
   (org-test-with-temp-text "
* H
:PROPERTIES:
This is not a node property
:END:"
     (org-lint '(obsolete-properties-drawer))))
  (should
   (org-test-with-temp-text "Paragraph
:PROPERTIES:
:FOO: bar
:END:"
     (org-lint '(obsolete-properties-drawer)))))

(ert-deftest test-org-lint/invalid-effort-property ()
  "Test `org-lint-invalid-effort-property' checker."
  (should
   (org-test-with-temp-text "* H\n:PROPERTIES:\n:EFFORT: something\n:END:"
     (org-lint '(invalid-effort-property))))
  (should-not
   (org-test-with-temp-text "* H\n:PROPERTIES:\n:EFFORT: 1:23\n:END:"
     (org-lint '(invalid-effort-property)))))

(ert-deftest test-org-lint/link-to-local-file ()
  "Test `org-lint-link-to-local-file' checker."
  (should
   (org-test-with-temp-text "[[file:/Idonotexist.org]]"
     (org-lint '(link-to-local-file)))))

(ert-deftest test-org-lint/non-existent-setupfile-parameter ()
  "Test `org-lint-non-existent-setupfile-parameter' checker."
  (should
   (org-test-with-temp-text "#+setupfile: Idonotexist.org"
     (org-lint '(non-existent-setupfile-parameter))))
  (should-not
   (org-test-with-temp-text "#+setupfile: https://I.do/not.exist.org"
     (org-lint '(non-existent-setupfile-parameter)))))

(ert-deftest test-org-lint/wrong-include-link-parameter ()
  "Test `org-lint-wrong-include-link-parameter' checker."
  (should
   (org-test-with-temp-text "#+include:"
     (org-lint '(wrong-include-link-parameter))))
  (should
   (org-test-with-temp-text "#+include: Idonotexist.org"
     (org-lint '(wrong-include-link-parameter))))
  (should
   (org-test-with-temp-text-in-file ""
     (let ((file (buffer-file-name)))
       (org-test-with-temp-text (format "#+include: \"%s::#foo\"" file)
	 (org-lint '(wrong-include-link-parameter))))))
  (should-not
   (org-test-with-temp-text-in-file "* foo"
     (let ((file (buffer-file-name)))
       (org-test-with-temp-text (format "#+include: \"%s::*foo\"" file)
	 (org-lint '(wrong-include-link-parameter)))))))

(ert-deftest test-org-lint/obsolete-include-markup ()
  "Test `org-lint-obsolete-include-markup' checker."
  (should
   (org-test-with-temp-text-in-file ""
     (let ((file (buffer-file-name)))
       (org-test-with-temp-text (format "#+include: \"%s\" html" file)
	 (org-lint '(obsolete-include-markup))))))
  (should-not
   (org-test-with-temp-text-in-file ""
     (let ((file (buffer-file-name)))
       (org-test-with-temp-text (format "#+include: \"%s\" export html" file)
	 (org-lint '(obsolete-include-markup)))))))

(ert-deftest test-org-lint/unknown-options-item ()
  "Test `org-lint-unknown-options-item' checker."
  (should
   (org-test-with-temp-text "#+options: foobarbaz:t"
     (org-lint '(unknown-options-item)))))

(ert-deftest test-org-lint/invalid-macro-argument-and-template ()
  "Test `org-lint-invalid-macro-argument-and-template' checker."
  (should
   (org-test-with-temp-text "{{{undefined()}}}"
     (org-lint '(invalid-macro-argument-and-template))))
  (should
   (org-test-with-temp-text
       "#+macro: wrongsignature $1 $2\n{{{wrongsignature(1, 2, 3)}}}"
     (org-lint '(invalid-macro-argument-and-template))))
  (should
   (org-test-with-temp-text "#+macro:"
     (org-lint '(invalid-macro-argument-and-template))))
  (should
   (org-test-with-temp-text "#+macro: missingtemplate"
     (org-lint '(invalid-macro-argument-and-template))))
  (should
   (org-test-with-temp-text "#+macro: unusedplaceholders $1 $3"
     (org-lint '(invalid-macro-argument-and-template))))
  (should-not
   (org-test-with-temp-text
       "#+macro: valid $1 $2\n{{{valid(1, 2)}}}"
     (org-lint '(invalid-macro-argument-and-template)))))

(ert-deftest test-org-lint/undefined-footnote-reference ()
  "Test `org-lint-undefined-footnote-reference' checker."
  (should
   (org-test-with-temp-text "Text[fn:1]"
     (org-lint '(undefined-footnote-reference))))
  (should-not
   (org-test-with-temp-text "Text[fn:1]\n[fn:1] Definition"
     (org-lint '(undefined-footnote-reference))))
  (should-not
   (org-test-with-temp-text "Text[fn:1:inline reference]"
     (org-lint '(undefined-footnote-reference))))
  (should-not
   (org-test-with-temp-text "Text[fn::anonymous reference]"
     (org-lint '(undefined-footnote-reference)))))

(ert-deftest test-org-lint/unreferenced-footnote-definition ()
  "Test `org-lint-unreferenced-footnote-definition' checker."
  (should
   (org-test-with-temp-text "[fn:1] Definition"
     (org-lint '(unreferenced-footnote-definition))))
  (should-not
   (org-test-with-temp-text "Text[fn:1]\n[fn:1] Definition"
     (org-lint '(unreferenced-footnote-definition)))))

(ert-deftest test-org-lint/colon-in-name ()
  "Test `org-lint-colon-in-name' checker."
  (should
   (org-test-with-temp-text "#+name: tab:name\n| a |"
     (org-lint '(colon-in-name))))
  (should-not
   (org-test-with-temp-text "#+name: name\n| a |"
     (org-lint '(colon-in-name)))))

(ert-deftest test-org-lint/misplaced-planning-info ()
  "Test `org-lint-misplaced-planning-info' checker."
  (should
   (org-test-with-temp-text "SCHEDULED: <2012-03-29 thu.>"
     (org-lint '(misplaced-planning-info))))
  (should
   (org-test-with-temp-text "
* H
Text
SCHEDULED: <2012-03-29 thu.>"
     (org-lint '(misplaced-planning-info))))
  (should-not
   (org-test-with-temp-text "
* H
SCHEDULED: <2012-03-29 thu.>"
     (org-lint '(misplaced-planning-info)))))

(ert-deftest test-org-lint/incomplete-drawer ()
  "Test `org-lint-incomplete-drawer' checker."
  (should
   (org-test-with-temp-text ":DRAWER:"
     (org-lint '(incomplete-drawer))))
  (should
   (org-test-with-temp-text ":DRAWER:\n:ODD:\n:END:"
     (org-lint '(incomplete-drawer))))
  (should-not
   (org-test-with-temp-text ":DRAWER:\n:END:"
     (org-lint '(incomplete-drawer)))))

(ert-deftest test-org-lint/indented-diary-sexp ()
  "Test `org-lint-indented-diary-sexp' checker."
  (should
   (org-test-with-temp-text "  %%(foo)"
     (org-lint '(indented-diary-sexp))))
  (should-not
   (org-test-with-temp-text "%%(foo)"
     (org-lint '(indented-diary-sexp)))))

(ert-deftest test-org-lint/invalid-block ()
  "Test `org-lint-invalid-block' checker."
  (should
   (org-test-with-temp-text "#+begin_foo"
     (org-lint '(invalid-block))))
  (should-not
   (org-test-with-temp-text "#+begin_foo\n#+end_foo"
     (org-lint '(invalid-block)))))

(ert-deftest test-org-lint/invalid-keyword-syntax ()
  "Test `org-lint-invalid-keyword-syntax' checker."
  (should
   (org-test-with-temp-text "#+keyword"
     (org-lint '(invalid-keyword-syntax))))
  (should-not
   (org-test-with-temp-text "#+keyword:"
     (org-lint '(invalid-keyword-syntax)))))

(ert-deftest test-org-lint/extraneous-element-in-footnote-section ()
  "Test `org-lint-extraneous-element-in-footnote-section' checker."
  (should
   (org-test-with-temp-text "* Footnotes\nI'm not a footnote definition"
     (let ((org-footnote-section "Footnotes"))
       (org-lint '(extraneous-element-in-footnote-section)))))
  (should-not
   (org-test-with-temp-text "* Footnotes\n[fn:1] I'm a footnote definition"
     (let ((org-footnote-section "Footnotes"))
       (org-lint '(extraneous-element-in-footnote-section))))))

(ert-deftest test-org-lint/quote-section ()
  "Test `org-lint-quote-section' checker."
  (should
   (org-test-with-temp-text "* QUOTE H"
     (org-lint '(quote-section))))
  (should
   (org-test-with-temp-text "* COMMENT QUOTE H"
     (org-lint '(quote-section)))))

(ert-deftest test-org-lint/file-application ()
  "Test `org-lint-file-application' checker."
  (should
   (org-test-with-temp-text "[[file+emacs:foo.org]]"
     (org-lint '(file-application)))))

(ert-deftest test-org-lint/percenc-encoding-link-escape ()
  "Test `org-lint-percent-encoding-link-escape' checker."
  (should
   (org-test-with-temp-text "[[A%20B]]"
     (org-lint '(percent-encoding-link-escape))))
  (should
   (org-test-with-temp-text "[[%5Bfoo%5D]]"
     (org-lint '(percent-encoding-link-escape))))
  (should
   (org-test-with-temp-text "[[A%2520B]]"
     (org-lint '(percent-encoding-link-escape))))
  (should-not
   (org-test-with-temp-text "[[A B]]"
     (org-lint '(percent-encoding-link-escape))))
  (should-not
   (org-test-with-temp-text "[[A%30B]]"
     (org-lint '(percent-encoding-link-escape))))
  (should-not
   (org-test-with-temp-text "[[A%20%30B]]"
     (org-lint '(percent-encoding-link-escape))))
  (should-not
   (org-test-with-temp-text "<file:A%20B>"
     (org-lint '(percent-encoding-link-escape))))
  (should-not
   (org-test-with-temp-text "[[A B%]]"
     (org-lint '(percent-encoding-link-escape)))))

(ert-deftest test-org-lint/wrong-header-argument ()
  "Test `org-lint-wrong-header-argument' checker."
  (should
   (org-test-with-temp-text "#+call: foo() barbaz yes"
     (org-lint '(wrong-header-argument))))
  (should
   (org-test-with-temp-text "#+call: foo() :barbaz yes"
     (org-lint '(wrong-header-argument))))
  (should
   (org-test-with-temp-text "call_foo[barbaz yes]()"
     (org-lint '(wrong-header-argument))))
  (should
   (org-test-with-temp-text "call_foo[:barbaz yes]()"
     (org-lint '(wrong-header-argument))))
  (should
   (org-test-with-temp-text "#+property: header-args barbaz yes"
     (org-lint '(wrong-header-argument))))
  (should
   (org-test-with-temp-text "#+property: header-args :barbaz yes"
     (org-lint '(wrong-header-argument))))
  (should
   (org-test-with-temp-text "
* H
:PROPERTIES:
:HEADER-ARGS: barbaz yes
:END:"
     (org-lint '(wrong-header-argument))))
  (should
   (org-test-with-temp-text "
* H
:PROPERTIES:
:HEADER-ARGS: :barbaz yes
:END:"
     (org-lint '(wrong-header-argument))))
  (should
   (org-test-with-temp-text "
#+header: :barbaz yes
#+begin_src emacs-lisp
\(+ 1 1)
#+end_src"
     (org-lint '(wrong-header-argument))))
  (should
   (org-test-with-temp-text "src_emacs-lisp[barbaz yes]{}"
     (org-lint '(wrong-header-argument))))
  (should
   (org-test-with-temp-text "src_emacs-lisp[:barbaz yes]{}"
     (org-lint '(wrong-header-argument)))))

(ert-deftest test-org-lint/wrong-header-value ()
  "Test `org-lint-wrong-header-value' checker."
  (should
   (org-test-with-temp-text "
#+header: :cache maybe
#+begin_src emacs-lisp
\(+ 1 1)
#+end_src"
     (org-lint '(wrong-header-value))))
  (should
   (org-test-with-temp-text "
#+header: :exports both none
#+begin_src emacs-lisp
\(+ 1 1)
#+end_src"
     (org-lint '(wrong-header-value))))
  (should-not
   (org-test-with-temp-text "
#+header: :cache yes
#+begin_src emacs-lisp
\(+ 1 1)
#+end_src"
     (org-lint '(wrong-header-value)))))

(ert-deftest test-org/spurious-colons ()
  "Test `org-list-spurious-colons' checker."
  (should-not
   (org-test-with-temp-text "* H :tag:tag2:"
     (org-lint '(spurious-colons))))
  (should
   (org-test-with-temp-text "* H :tag::tag2:"
     (org-lint '(spurious-colons))))
  (should
   (org-test-with-temp-text "* H :tag::"
     (org-lint '(spurious-colons)))))


(provide 'test-org-lint)
;;; test-org-lint.el ends here
