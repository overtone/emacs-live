;;; test-org-pcomplete.el --- test pcomplete integration

;; Copyright (C) 2015-2016, 2019  Alexey Lebedeff
;; Authors: Alexey Lebedeff

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



;;; Code:

(ert-deftest test-org-pcomplete/clocktable ()
  "Test completion of clock table parameters."
  (should
   (equal "#+begin: clocktable :scope"
	  (org-test-with-temp-text "#+begin: clocktable :sco<point>"
	    (pcomplete)
	    (buffer-string)))))

(ert-deftest test-org-pcomplete/drawer ()
  "Test drawer completion."
  (should
   (equal "* Foo\n:PROPERTIES:"
	  (org-test-with-temp-text "* Foo\n:<point>"
	    (pcomplete)
	    (buffer-string))))
  (should
   (equal ":DRAWER:\nContents\n:END:\n* Foo\n:DRAWER:"
	  (org-test-with-temp-text ":DRAWER:\nContents\n:END:\n* Foo\n:D<point>"
	    (pcomplete)
	    (buffer-string)))))

(ert-deftest test-org-pcomplete/entity ()
  "Test entity completion."
  (should
   (equal "\\alpha"
	  (org-test-with-temp-text "\\alp<point>"
	    (pcomplete)
	    (buffer-string))))
  (should
   (equal "\\frac12"
	  (org-test-with-temp-text "\\frac1<point>"
	    (pcomplete)
	    (buffer-string)))))

(ert-deftest test-org-pcomplete/keyword ()
  "Test keyword and block completion."
  (should
   (string-prefix-p
    "#+startup: "
    (org-test-with-temp-text "#+start<point>"
      (pcomplete)
      (buffer-string))
    t))
  (should
   (string-prefix-p
    "#+begin_center"
    (org-test-with-temp-text "#+begin_ce<point>"
      (pcomplete)
      (buffer-string))
    t)))

(ert-deftest test-org-pcomplete/link ()
  "Test link completion"
  (should
   (equal "[[org:"
	  (org-test-with-temp-text "[[o<point>"
	    (let ((org-link-abbrev-alist '(("org" . "https://orgmode.org/"))))
	      (pcomplete))
	    (buffer-string))))
  (should-not
   (equal "[org:"
	  (org-test-with-temp-text "[[o<point>"
	    (let ((org-link-abbrev-alist '(("org" . "https://orgmode.org/"))))
	      (pcomplete))
	    (buffer-string)))))

(ert-deftest test-org-pcomplete/prop ()
  "Test property completion."
  (should
   (equal
    "
* a
:PROPERTIES:
:pname:\s
:END:
* b
:PROPERTIES:
:pname: pvalue
:END:
"
    (org-test-with-temp-text "
* a
:PROPERTIES:
:pna<point>
:END:
* b
:PROPERTIES:
:pname: pvalue
:END:
"
      (pcomplete)
      (buffer-string)))))

(ert-deftest test-org-pcomplete/search-heading ()
  "Test search heading completion."
  (should
   (equal "* Foo\n[[*Foo"
	  (org-test-with-temp-text "* Foo\n[[*<point>"
	    (pcomplete)
	    (buffer-string)))))

(ert-deftest test-org-pcomplete/tag ()
  "Test tag completion."
  ;; Complete at end of line, according to `org-current-tag-alist'.
  (should
   (equal "* H :foo:"
	  (org-test-with-temp-text "* H :<point>"
	    (let ((org-current-tag-alist '(("foo")))) (pcomplete))
	    (buffer-string))))
  (should
   (equal "* H :foo:bar:"
	  (org-test-with-temp-text "* H :foo:b<point>"
	    (let ((org-current-tag-alist '(("bar")))) (pcomplete))
	    (buffer-string))))
  ;; If `org-current-tag-alist' is non-nil, complete against tags in
  ;; buffer.
  (should
   (equal "* H1 :bar:\n* H2 :bar:"
	  (org-test-with-temp-text "* H1 :bar:\n* H2 :<point>"
	    (let ((org-current-tag-alist nil)) (pcomplete))
	    (buffer-string))))
  ;; Do not complete in the middle of a line.
  (should
   (equal "* H :notag: :real:tags:"
	  (org-test-with-temp-text "* H :notag:<point> :real:tags:"
	    (let ((org-current-tag-alist '(("foo")))) (pcomplete))
	    (buffer-string))))
  ;; Complete even when there's a match on the line.
  (should
   (equal "* foo: :foo:"
	  (org-test-with-temp-text "* foo: :<point>"
	    (let ((org-current-tag-alist '(("foo")))) (pcomplete))
	    (buffer-string)))))

(ert-deftest test-org-pcomplete/todo ()
  "Test TODO completion."
  (should
   (equal "* TODO"
	  (org-test-with-temp-text "* T<point>"
	    (pcomplete)
	    (buffer-string)))))

(provide 'test-org-pcomplete)
;;; test-org-pcomplete.el ends here
