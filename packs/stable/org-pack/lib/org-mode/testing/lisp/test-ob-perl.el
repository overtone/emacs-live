;;; test-ob-perl.el --- tests for ob-perl.el

;; Copyright (c) 2013, 2014 Achim Gratz
;; Authors: Achim Gratz

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

;;; Code:
(org-test-for-executable "perl")
(unless (featurep 'ob-perl)
  (signal 'missing-test-dependency "Support for perl code blocks"))

(ert-deftest test-ob-perl/simple-output ()
  (org-test-with-temp-text "
#+header: :results output
#+begin_src perl
  print qq(Hi Mom!$/I'm home.);
#+end_src"
    (org-babel-next-src-block)
    (should (equal "Hi Mom!\nI'm home."
		   (org-babel-execute-src-block)))))

(ert-deftest test-ob-perl/simple-value ()
  (org-test-with-temp-text "
#+header: :results value
#+begin_src perl
  qq(Hi Mom!$/I'm home.);
#+end_src"
    (org-babel-next-src-block)
    (should (equal '(("Hi Mom!") ("I'm home."))
		   (org-babel-execute-src-block)))))

(ert-deftest test-ob-perl/table-passthrough-colnames-nil ()
  (org-test-with-temp-text "#+name: eg
| col1 | col2 |
|------+------|
| a    | 1    |
| b    | 2.0  |

#+header: :colnames nil
#+header: :var x = eg
#+begin_src perl
#+end_src"
    (org-babel-next-src-block)
    (should (equal '(("col1" "col2") hline ("a" 1) ("b" 2.0))
		   (org-babel-execute-src-block)))))

(ert-deftest test-ob-perl/table-passthrough-colnames-no ()
  (org-test-with-temp-text "#+name: eg
| col1 | col2 |
|------+------|
| a    | 1    |
| b    | 2.0  |

#+header: :colnames no
#+header: :var x = eg
#+begin_src perl
#+end_src"
    (org-babel-next-src-block)
    (should (equal '(("col1" "col2") ("a" 1) ("b" 2.0))
		   (org-babel-execute-src-block)))))

(provide 'test-ob-perl)

;;; test-ob-perl.el ends here
