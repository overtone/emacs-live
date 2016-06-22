;;; test-org-macro.el --- Tests for org-macro.el

;; Copyright (C) 2013, 2014  Nicolas Goaziou

;; Author: Nicolas Goaziou <n.goaziou@gmail.com>

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


;;; Macros

(ert-deftest test-org/macro-replace-all ()
  "Test `org-macro-replace-all' specifications."
  ;; Standard test.
  (should
   (equal
    "#+MACRO: A B\n1 B 3"
    (org-test-with-temp-text "#+MACRO: A B\n1 {{{A}}} 3"
      (progn (org-macro-initialize-templates)
	     (org-macro-replace-all org-macro-templates)
	     (buffer-string)))))
  ;; Macro with arguments.
  (should
   (equal
    "#+MACRO: macro $1 $2\nsome text"
    (org-test-with-temp-text "#+MACRO: macro $1 $2\n{{{macro(some,text)}}}"
      (progn (org-macro-initialize-templates)
	     (org-macro-replace-all org-macro-templates)
	     (buffer-string)))))
  ;; Macro with "eval".
  (should
   (equal
    "#+MACRO: add (eval (+ $1 $2))\n3"
    (org-test-with-temp-text "#+MACRO: add (eval (+ $1 $2))\n{{{add(1,2)}}}"
      (progn (org-macro-initialize-templates)
	     (org-macro-replace-all org-macro-templates)
	     (buffer-string)))))
  ;; Nested macros.
  (should
   (equal
    "#+MACRO: in inner\n#+MACRO: out {{{in}}} outer\ninner outer"
    (org-test-with-temp-text
	"#+MACRO: in inner\n#+MACRO: out {{{in}}} outer\n{{{out}}}"
      (progn (org-macro-initialize-templates)
	     (org-macro-replace-all org-macro-templates)
	     (buffer-string)))))
  ;; Error out when macro expansion is circular.
  (should-error
   (org-test-with-temp-text
       "#+MACRO: mac1 {{{mac2}}}\n#+MACRO: mac2 {{{mac1}}}\n{{{mac1}}}"
     (org-macro-initialize-templates)
     (org-macro-replace-all org-macro-templates)))
  ;; Macros in setup file.
  (should
   (string-match
    "success success\\'"
    (org-test-with-temp-text
	(format "#+MACRO: other-macro success
#+SETUPFILE: \"%sexamples/macro-templates.org\"
{{{included-macro}}} {{{other-macro}}}"
		org-test-dir)
      (org-macro-initialize-templates)
      (org-macro-replace-all org-macro-templates)
      (buffer-string))))
  ;; Test special "property" macro.  With only one argument, retrieve
  ;; property from current headline.  Otherwise, the second argument
  ;; is a search option to get the property from another headline.
  (should
   (equal "1"
	  (org-test-with-temp-text
	      "* H\n:PROPERTIES:\n:A: 1\n:END:\n{{{property(A)}}}<point>"
	    (org-macro-initialize-templates)
	    (org-macro-replace-all org-macro-templates)
	    (buffer-substring-no-properties
	     (line-beginning-position) (line-end-position)))))
  (should
   (equal "1"
	  (org-test-with-temp-text
	      "* H\n:PROPERTIES:\n:A: 1\n:END:\n{{{property(A,)}}}<point>"
	    (org-macro-initialize-templates)
	    (org-macro-replace-all org-macro-templates)
	    (buffer-substring-no-properties
	     (line-beginning-position) (line-end-position)))))
  (should
   (equal
    "1"
    (org-test-with-temp-text
	"* H1\n:PROPERTIES:\n:A: 1\n:END:\n* H2\n{{{property(A,*H1)}}}<point>"
      (org-macro-initialize-templates)
      (org-macro-replace-all org-macro-templates)
      (buffer-substring-no-properties
       (line-beginning-position) (line-end-position)))))
  (should-error
   (org-test-with-temp-text
       "* H1\n:PROPERTIES:\n:A: 1\n:END:\n* H2\n{{{property(A,*???)}}}<point>"
     (org-macro-initialize-templates)
     (org-macro-replace-all org-macro-templates)))
  ;; Macro expansion ignores narrowing.
  (should
   (string-match
    "expansion"
    (org-test-with-temp-text
	"#+MACRO: macro expansion\n{{{macro}}}\n<point>Contents"
      (narrow-to-region (point) (point-max))
      (org-macro-initialize-templates)
      (org-macro-replace-all org-macro-templates)
      (org-with-wide-buffer (buffer-string))))))

(ert-deftest test-org-macro/escape-arguments ()
  "Test `org-macro-escape-arguments' specifications."
  ;; Regular tests.
  (should (equal "a" (org-macro-escape-arguments "a")))
  (should (equal "a,b" (org-macro-escape-arguments "a" "b")))
  ;; Handle empty arguments.
  (should (equal "a,,b" (org-macro-escape-arguments "a" "" "b")))
  ;; Properly escape commas and backslashes preceding them.
  (should (equal "a\\,b" (org-macro-escape-arguments "a,b")))
  (should (equal "a\\\\,b" (org-macro-escape-arguments "a\\" "b")))
  (should (equal "a\\\\\\,b" (org-macro-escape-arguments "a\\,b"))))

(ert-deftest test-org-macro/extract-arguments ()
  "Test `org-macro-extract-arguments' specifications."
  ;; Regular tests.
  (should (equal '("a") (org-macro-extract-arguments "a")))
  (should (equal '("a" "b") (org-macro-extract-arguments "a,b")))
  ;; Handle empty arguments.
  (should (equal '("a" "" "b") (org-macro-extract-arguments "a,,b")))
  ;; Handle escaped commas and backslashes.
  (should (equal '("a,b") (org-macro-extract-arguments "a\\,b")))
  (should (equal '("a\\" "b") (org-macro-extract-arguments "a\\\\,b")))
  (should (equal '("a\\,b") (org-macro-extract-arguments "a\\\\\\,b"))))


(provide 'test-org-macro)
;;; test-org-macro.el ends here
