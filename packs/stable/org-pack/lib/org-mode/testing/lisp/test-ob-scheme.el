;;; test-ob-scheme.el --- Tests for Babel scheme     -*- lexical-binding: t; -*-

;; Copyright (C) 2017, 2019  Nicolas Goaziou

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Unit tests for Org Babel Scheme.

;;; Code:

(unless (featurep 'ob-scheme)
  (signal 'missing-test-dependency "Support for Scheme code blocks"))

(ert-deftest test-ob-scheme/tables ()
  "Test table output."
  (equal "#+begin_src scheme
'(1 2 3)
#+end_src

#+RESULTS:
| 1 | 2 | 3 |
"
	 (org-test-with-temp-text "#+begin_src scheme\n'(1 2 3)\n#+end_src"
	   (org-babel-execute-maybe)
	   (buffer-string))))

(ert-deftest test-ob-scheme/verbatim ()
  "Test verbatim output."
  (should
   (equal ": (1 2 3)\n"
	  (org-test-with-temp-text "#+begin_src scheme :results verbatim\n'(1 2 3)\n#+end_src"
	    (org-babel-execute-src-block)
	    (let ((case-fold-search t)) (search-forward "#+results"))
	    (buffer-substring-no-properties (line-beginning-position 2)
					    (point-max))))))

(ert-deftest test-ob-scheme/list ()
  "Test list output."
  (should
   (equal "- 1\n- 2\n- 3\n"
	  (org-test-with-temp-text "#+begin_src scheme :results list\n'(1 2 3)\n#+end_src"
	    (org-babel-execute-maybe)
	    (let ((case-fold-search t)) (search-forward "#+results"))
	    (buffer-substring-no-properties (line-beginning-position 2)
					    (point-max))))))

(ert-deftest test-ob-scheme/prologue ()
  "Test :prologue parameter."
  (should
   (equal "#+begin_src scheme :prologue \"(define x 2)\"
x
#+end_src

#+RESULTS:
: 2
"
	  (org-test-with-temp-text
	      "#+begin_src scheme :prologue \"(define x 2)\"\nx\n#+end_src"
	    (org-babel-execute-maybe)
	    (buffer-string))))
  (should
   (equal
    "#+begin_src scheme :prologue \"(define x 2)\" :var y=1
x
#+end_src

#+RESULTS:
: 2
"
    (org-test-with-temp-text
	"#+begin_src scheme :prologue \"(define x 2)\" :var y=1\nx\n#+end_src"
      (org-babel-execute-maybe)
      (buffer-string)))))

(ert-deftest test-ob-scheme/unspecified ()
  "Test <#unspecified> return value."
  (should
   (equal "#+begin_src scheme
\(define (mysquare x)
  (* x x))
#+end_src

#+RESULTS:
: #<unspecified>
"
	  (org-test-with-temp-text
	      "#+begin_src scheme
(define (mysquare x)
  (* x x))
#+end_src"
	    (org-babel-execute-maybe)
	    (buffer-string)))))


(provide 'test-ob-scheme)
;;; test-ob-scheme.el ends here
