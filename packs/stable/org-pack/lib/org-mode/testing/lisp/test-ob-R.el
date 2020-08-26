;;; test-ob-R.el --- tests for ob-R.el

;; Copyright (c) 2011-2014, 2019 Eric Schulte
;; Authors: Eric Schulte

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
(org-test-for-executable "R")
(unless (featurep 'ess)
  (signal 'missing-test-dependency "ESS"))
(unless (featurep 'ob-R)
  (signal 'missing-test-dependency "Support for R code blocks"))

(ert-deftest test-ob-R/simple-session ()
  (let (ess-ask-for-ess-directory ess-history-file)
    (org-test-with-temp-text
     "#+begin_src R :session R\n  paste(\"Yep!\")\n#+end_src\n"
     (should (string= "Yep!" (org-babel-execute-src-block))))))

(ert-deftest test-ob-R/colnames-yes-header-argument ()
  (org-test-with-temp-text "#+name: eg
| col |
|-----|
| a   |
| b   |

#+header: :colnames yes
#+header: :var x = eg
#+begin_src R
x
#+end_src"
    (org-babel-next-src-block)
    (should (equal '(("col") hline ("a") ("b"))
		   (org-babel-execute-src-block)))))

(ert-deftest test-ob-R/colnames-nil-header-argument ()
  (org-test-with-temp-text "#+name: eg
| col |
|-----|
| a   |
| b   |

#+header: :colnames nil
#+header: :var x = eg
#+begin_src R
x
#+end_src"
    (org-babel-next-src-block)
    (should (equal '(("col") hline ("a") ("b"))
		   (org-babel-execute-src-block)))))

(ert-deftest test-ob-R/colnames-no-header-argument ()
  (org-test-with-temp-text "#+name: eg
| col |
|-----|
| a   |
| b   |

#+header: :colnames no
#+header: :var x = eg
#+begin_src R
x
#+end_src"
    (org-babel-next-src-block)
    (should (equal '(("col") ("a") ("b"))
		   (org-babel-execute-src-block)))))

(ert-deftest test-ob-R/results-file ()
  (let (ess-ask-for-ess-directory ess-history-file)
    (org-test-with-temp-text
     "#+NAME: TESTSRC
#+BEGIN_SRC R :results file
  a <- file.path(\"junk\", \"test.org\")
  a
#+END_SRC"
     (goto-char (point-min)) (org-babel-execute-maybe)
     (org-babel-goto-named-result "TESTSRC") (forward-line 1)
     (should (string= "[[file:junk/test.org]]"
		      (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
     (goto-char (point-min)) (forward-line 1)
     (insert "#+header: :session\n")
     (goto-char (point-min)) (org-babel-execute-maybe)
     (org-babel-goto-named-result "TESTSRC") (forward-line 1)
     (should (string= "[[file:junk/test.org]]"
		      (buffer-substring-no-properties (point-at-bol) (point-at-eol)))))))
(provide 'test-ob-R)

;;; test-ob-R.el ends here
