;;; test-org-macro.el --- Tests for org-macro.el

;; Copyright (C) 2013, 2014, 2019  Nicolas Goaziou

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
      (org-macro-initialize-templates)
      (org-macro-replace-all org-macro-templates)
      (buffer-string))))
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
    "3"
    (org-test-with-temp-text
	"#+MACRO: add (eval (+ (string-to-number $1) (string-to-number $2)))
<point>{{{add(1,2)}}}"
      (org-macro-initialize-templates)
      (org-macro-replace-all org-macro-templates)
      (buffer-substring-no-properties (point) (line-end-position)))))
  ;; Nested macros.
  (should
   (equal
    "#+MACRO: in inner\n#+MACRO: out {{{in}}} outer\ninner outer"
    (org-test-with-temp-text
        "#+MACRO: in inner\n#+MACRO: out {{{in}}} outer\n{{{out}}}"
      (org-macro-initialize-templates)
      (org-macro-replace-all org-macro-templates)
      (buffer-string))))
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
  ;; Macro expansion ignores narrowing.
  (should
   (string-match
    "expansion"
    (org-test-with-temp-text
        "#+MACRO: macro expansion\n{{{macro}}}\n<point>Contents"
      (narrow-to-region (point) (point-max))
      (org-macro-initialize-templates)
      (org-macro-replace-all org-macro-templates)
      (org-with-wide-buffer (buffer-string)))))
  ;; Macros in a commented tree are not expanded.
  (should
   (string-match-p
    "{{{macro}}}"
    (org-test-with-temp-text
        "#+MACRO: macro expansion\n* COMMENT H\n<point>{{{macro}}}"
      (org-macro-initialize-templates)
      (org-macro-replace-all org-macro-templates)
      (org-with-wide-buffer (buffer-string)))))
  (should
   (string-match-p
    "{{{macro}}}"
    (org-test-with-temp-text
        "#+MACRO: macro expansion\n* COMMENT H1\n** H2\n<point>{{{macro}}}"
      (org-macro-initialize-templates)
      (org-macro-replace-all org-macro-templates)
      (org-with-wide-buffer (buffer-string))))))

(ert-deftest test-org-macro/property ()
  "Test {{{property}}} macro."
  ;; With only one argument, retrieve property from current headline.
  ;; Otherwise, the second argument is a search option to get the
  ;; property from another headline.
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
     (org-macro-replace-all org-macro-templates))))

(ert-deftest test-org-macro/n ()
  "Test {{{n}}} macro."
  ;; Standard test with default counter.
  (should
   (equal "1 2"
          (org-test-with-temp-text "{{{n}}} {{{n}}}"
            (org-macro-initialize-templates)
            (org-macro-replace-all org-macro-templates)
            (buffer-substring-no-properties
             (line-beginning-position) (line-end-position)))))
  (should
   (equal "1 2"
          (org-test-with-temp-text "{{{n()}}} {{{n}}}"
            (org-macro-initialize-templates)
            (org-macro-replace-all org-macro-templates)
            (buffer-substring-no-properties
             (line-beginning-position) (line-end-position)))))
  ;; Test alternative counters.
  (should
   (equal "1 1 1 2"
          (org-test-with-temp-text "{{{n}}} {{{n(c1)}}} {{{n(c2)}}} {{{n(c1)}}}"
            (org-macro-initialize-templates)
            (org-macro-replace-all org-macro-templates)
            (buffer-substring-no-properties
             (line-beginning-position) (line-end-position)))))
  ;; Second argument set a counter to a given value.  A non-numeric
  ;; value resets the counter to 1.
  (should
   (equal "9 10"
          (org-test-with-temp-text "{{{n(c,9)}}} {{{n(c)}}}"
            (org-macro-initialize-templates)
            (org-macro-replace-all org-macro-templates)
            (buffer-substring-no-properties
             (line-beginning-position) (line-end-position)))))
  (should
   (equal "9 1"
          (org-test-with-temp-text "{{{n(c,9)}}} {{{n(c,reset)}}}"
            (org-macro-initialize-templates)
            (org-macro-replace-all org-macro-templates)
            (buffer-substring-no-properties
             (line-beginning-position) (line-end-position)))))
  ;; Check that reset happens when the second argument is neither "-"
  ;; nor a number.
  (should
   (equal "9 1 1 1"
          (org-test-with-temp-text
	      (concat "{{{n(c,9)}}} {{{n(c,reiniciar)}}}"
		      " {{{n(c,réinitialiser)}}} {{{n(c,zurückstellen)}}}")
            (org-macro-initialize-templates)
            (org-macro-replace-all org-macro-templates)
            (buffer-substring-no-properties
             (line-beginning-position) (line-end-position)))))
  ;; Tolerate spaces in first argument.
  (should
   (equal "1 2 3 4"
          (org-test-with-temp-text "{{{n(c)}}} {{{n(c )}}} {{{n( c)}}} {{{n( c )}}}"
            (org-macro-initialize-templates)
            (org-macro-replace-all org-macro-templates)
            (buffer-substring-no-properties
             (line-beginning-position) (line-end-position)))))
  ;; Tolerate spaces when second argument is an integer.
  (should
   (equal "2 3 5 7"
          (org-test-with-temp-text
	      (concat "{{{n(c,2)}}} {{{n(c, 3)}}}"
		      " {{{n(c,5 )}}} {{{n(c, 7 )}}}")
            (org-macro-initialize-templates)
            (org-macro-replace-all org-macro-templates)
            (buffer-substring-no-properties
             (line-beginning-position) (line-end-position)))))
  ;; Tolerate spaces when second argument is the hold argument.
  (should
   (equal "7 7 8 8 9 9"
          (org-test-with-temp-text
	      (concat "{{{n(,7)}}} {{{n(, -)}}}"
		      " {{{n}}} {{{n(,- )}}} {{{n}}} {{{n(, - )}}}")
            (org-macro-initialize-templates)
            (org-macro-replace-all org-macro-templates)
            (buffer-substring-no-properties
             (line-beginning-position) (line-end-position)))))
  ;; Tolerate spaces when second argument is used to reset the counter.
  (should
   (equal "9 1 1 1 1"
          (org-test-with-temp-text
	      (concat "{{{n(c,9)}}} {{{n(c,reset)}}} {{{n(c, reset)}}}"
		      " {{{n(c,reset )}}} {{{n(c, reset )}}}")
            (org-macro-initialize-templates)
            (org-macro-replace-all org-macro-templates)
            (buffer-substring-no-properties
             (line-beginning-position) (line-end-position)))))
  ;; Second argument also applies to default counter.
  (should
   (equal "9 10 1"
          (org-test-with-temp-text "{{{n(,9)}}} {{{n}}} {{{n(,reset)}}}"
            (org-macro-initialize-templates)
            (org-macro-replace-all org-macro-templates)
            (buffer-substring-no-properties
             (line-beginning-position) (line-end-position)))))
  ;; An empty second argument is equivalent to no argument.
  (should
   (equal "2 3"
          (org-test-with-temp-text "{{{n(c,2)}}} {{{n(c,)}}}"
            (org-macro-initialize-templates)
            (org-macro-replace-all org-macro-templates)
            (buffer-substring-no-properties
             (line-beginning-position) (line-end-position)))))
  ;; Hold value at reset value of 1 if the counter hasn't yet started.
  (should
   (equal "1"
          (org-test-with-temp-text "{{{n(,-)}}}"
            (org-macro-initialize-templates)
            (org-macro-replace-all org-macro-templates)
            (buffer-substring-no-properties
             (line-beginning-position) (line-end-position)))))
  ;; Increment counter following a hold.
  (should
   (equal "1 1 2"
          (org-test-with-temp-text "{{{n}}} {{{n(,-)}}} {{{n}}}"
            (org-macro-initialize-templates)
            (org-macro-replace-all org-macro-templates)
            (buffer-substring-no-properties
             (line-beginning-position) (line-end-position)))))
  ;; Hold counter value following a counter value set.
  (should
   (equal "1 10 10"
          (org-test-with-temp-text "{{{n}}} {{{n(,10)}}} {{{n(,-)}}}"
            (org-macro-initialize-templates)
            (org-macro-replace-all org-macro-templates)
            (buffer-substring-no-properties
             (line-beginning-position) (line-end-position)))))
  ;; Hold counter value in a multiple-counter situation.
  (should
   (equal "1.1 1.2 1.3"
          (org-test-with-temp-text
	      "{{{n}}}.{{{n(c)}}} {{{n(,-)}}}.{{{n(c)}}} {{{n(,-)}}}.{{{n(c)}}}"
            (org-macro-initialize-templates)
            (org-macro-replace-all org-macro-templates)
            (buffer-substring-no-properties
             (line-beginning-position) (line-end-position)))))
  ;; Hold counter values on one or multiple counters at the same time.
  (should
   (equal "1.1 1.2 2.2 2.2"
          (org-test-with-temp-text
	      (concat "{{{n}}}.{{{n(c)}}} {{{n(,-)}}}.{{{n(c)}}}"
		      " {{{n}}}.{{{n(c,-)}}} {{{n(,-)}}}.{{{n(c,-)}}}")
            (org-macro-initialize-templates)
            (org-macro-replace-all org-macro-templates)
            (buffer-substring-no-properties
             (line-beginning-position) (line-end-position))))))

(ert-deftest test-org-macro/keyword ()
  "Test {{{keyword}}} macro."
  ;; Replace macro with keyword's value.
  (should
   (equal
    "value"
    (org-test-with-temp-text
	"#+keyword: value\n<point>{{{keyword(KEYWORD)}}}"
      (org-macro-initialize-templates)
      (org-macro-replace-all org-macro-templates)
      (buffer-substring-no-properties
       (line-beginning-position) (point-max))))))

(ert-deftest test-org-macro/author ()
  "Test {{{author}}} macro."
  ;; Return AUTHOR keyword value.
  (should
   (equal "me"
	  (org-test-with-temp-text "#+author: me\n<point>{{{author}}}"
	    (org-macro-initialize-templates)
	    (org-macro-replace-all org-macro-templates)
	    (buffer-substring-no-properties
	     (line-beginning-position) (point-max)))))
  ;; When AUTHOR keyword is missing, return the empty string.
  (should
   (equal ""
	  (org-test-with-temp-text "{{{author}}}"
	    (org-macro-initialize-templates)
	    (org-macro-replace-all org-macro-templates)
	    (buffer-substring-no-properties
	     (line-beginning-position) (point-max))))))

(ert-deftest test-org-macro/email ()
  "Test {{{email}}} macro."
  ;; Return EMAIL keyword value.
  (should
   (equal "me@home"
	  (org-test-with-temp-text "#+email: me@home\n<point>{{{email}}}"
	    (org-macro-initialize-templates)
	    (org-macro-replace-all org-macro-templates)
	    (buffer-substring-no-properties
	     (line-beginning-position) (point-max)))))
  ;; When EMAIL keyword is missing, return the empty string.
  (should
   (equal ""
	  (org-test-with-temp-text "{{{email}}}"
	    (org-macro-initialize-templates)
	    (org-macro-replace-all org-macro-templates)
	    (buffer-substring-no-properties
	     (line-beginning-position) (point-max))))))

(ert-deftest test-org-macro/title ()
  "Test {{{title}}} macro."
  ;; Return TITLE keyword value.
  (should
   (equal "Foo!"
	  (org-test-with-temp-text "#+title: Foo!\n<point>{{{title}}}"
	    (org-macro-initialize-templates)
	    (org-macro-replace-all org-macro-templates)
	    (buffer-substring-no-properties
	     (line-beginning-position) (point-max)))))
  ;; When TITLE keyword is missing, return the empty string.
  (should
   (equal ""
	  (org-test-with-temp-text "{{{title}}}"
	    (org-macro-initialize-templates)
	    (org-macro-replace-all org-macro-templates)
	    (buffer-substring-no-properties
	     (line-beginning-position) (point-max)))))
  ;; When multiple TITLE keywords are used, concatenate them.
  (should
   (equal "Foo Bar!"
	  (org-test-with-temp-text
	      "#+title: Foo\n#+title: Bar!\n<point>{{{title}}}"
	    (org-macro-initialize-templates)
	    (org-macro-replace-all org-macro-templates)
	    (buffer-substring-no-properties
	     (line-beginning-position) (point-max))))))

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
