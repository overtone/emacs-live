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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

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



(ert-deftest test-ob-r/output-with-<> ()
  "make sure angle brackets are well formatted"
    (let (ess-ask-for-ess-directory ess-history-file)
      (should (string="[1] \"<X> <Y> <!>\"
[1] \"one <two> three\"
[1] \"end35\"
"
  (org-test-with-temp-text "#+begin_src R :results output
     print(\"<X> <Y> <!>\")
     print(\"one <two> three\")
     print(\"end35\")
   #+end_src
"
    (org-babel-execute-src-block))
))))




;; (ert-deftest test-ob-r/output-with-error ()
;;   "make sure angle brackets are well formatted"
;;     (let (ess-ask-for-ess-directory ess-history-file)
;;       (should (string="Error in print(1/a) : object 'a' not found"
;;   (org-test-with-temp-text "#+begin_src R :results output
;;   print(1/a)
;;  #+end_src
;; "
;;     (org-babel-execute-src-block))
;; ))))


(ert-deftest test-ob-R/output-nonprinted ()
  (let (ess-ask-for-ess-directory ess-history-file)
    (org-test-with-temp-text
     "#+begin_src R :results output
4.0 * 3.5
log(10)
log10(10)
(3 + 1) * 5
3^-1
1/0
#+end_src"
     (should (string= "[1] 14\n[1] 2.302585\n[1] 1\n[1] 20\n[1] 0.3333333\n[1] Inf\n" (org-babel-execute-src-block))))))

(ert-deftest test-ob-r/NA-blank ()
  "For :results value, NAs should be empty"
  (let (ess-ask-for-ess-directory ess-history-file)
    (should (equal '(("A" "B") hline ("" 1) (1 2) (1 "") (1 4) (1 4))
  (org-test-with-temp-text "#+BEGIN_SRC R :results value :colnames yes
  data.frame(A=c(NA,1,1,1,1),B=c(1,2,NA,4,4))
#+end_src"     
  (org-babel-execute-src-block))))))


(ert-deftest ob-session-async-R-simple-session-async-value ()
  (let (ess-ask-for-ess-directory
        ess-history-file
        (org-babel-temporary-directory "/tmp")
        (org-confirm-babel-evaluate nil))
    (org-test-with-temp-text
     "#+begin_src R :session R :async yes\n  Sys.sleep(.1)\n  paste(\"Yep!\")\n#+end_src\n"
     (should (let ((expected "Yep!"))
	       (and (not (string= expected (org-babel-execute-src-block)))
		    (string= expected
			     (progn
			       (sleep-for 0 200)
			       (goto-char (org-babel-where-is-src-block-result))
			       (org-babel-read-result)))))))))

(ert-deftest ob-session-async-R-simple-session-async-output ()
  (let (ess-ask-for-ess-directory
        ess-history-file
        (org-babel-temporary-directory "/tmp")
        (org-confirm-babel-evaluate nil))
    (org-test-with-temp-text
     "#+begin_src R :session R :results output :async yes\n  Sys.sleep(.1)\n  1:5\n#+end_src\n"
     (should (let ((expected "[1] 1 2 3 4 5"))
	       (and (not (string= expected (org-babel-execute-src-block)))
		    (string= expected
			     (progn
			       (sleep-for 0 200)
			       (goto-char (org-babel-where-is-src-block-result))
			       (org-babel-read-result)))))))))

(ert-deftest ob-session-async-R-named-output ()
  (let (ess-ask-for-ess-directory
        ess-history-file
        (org-babel-temporary-directory "/tmp")
        org-confirm-babel-evaluate
        (src-block "#+begin_src R :async :session R :results output\n  1:5\n#+end_src")
        (results-before "\n\n#+NAME: foobar\n#+RESULTS:\n: [1] 1")
        (results-after "\n\n#+NAME: foobar\n#+RESULTS:\n: [1] 1 2 3 4 5\n"))
    (org-test-with-temp-text
     (concat src-block results-before)
     (should (progn (org-babel-execute-src-block)
                    (sleep-for 0 200)
                    (string= (concat src-block results-after)
                             (buffer-string)))))))

(ert-deftest ob-session-async-R-named-value ()
  (let (ess-ask-for-ess-directory
        ess-history-file
        org-confirm-babel-evaluate
        (org-babel-temporary-directory "/tmp")
        (src-block "#+begin_src R :async :session R :results value\n  paste(\"Yep!\")\n#+end_src")
        (results-before "\n\n#+NAME: foobar\n#+RESULTS:\n: [1] 1")
        (results-after "\n\n#+NAME: foobar\n#+RESULTS:\n: Yep!\n"))
    (org-test-with-temp-text
     (concat src-block results-before)
     (should (progn (org-babel-execute-src-block)
                    (sleep-for 0 200)
                    (string= (concat src-block results-after)
                             (buffer-string)))))))

(ert-deftest ob-session-async-R-output-drawer ()
  (let (ess-ask-for-ess-directory
        ess-history-file
        org-confirm-babel-evaluate
        (org-babel-temporary-directory "/tmp")
        (src-block "#+begin_src R :async :session R :results output drawer\n  1:5\n#+end_src")
        (result "\n\n#+RESULTS:\n:results:\n[1] 1 2 3 4 5\n:end:\n"))
    (org-test-with-temp-text
     src-block
     (should (progn (org-babel-execute-src-block)
                    (sleep-for 0 200)
                    (string= (concat src-block result)
                             (buffer-string)))))))

(ert-deftest ob-session-async-R-value-drawer ()
  (let (ess-ask-for-ess-directory
        ess-history-file
        org-confirm-babel-evaluate
        (org-babel-temporary-directory "/tmp")
        (src-block "#+begin_src R :async :session R :results value drawer\n  1:3\n#+end_src")
        (result "\n\n#+RESULTS:\n:results:\n1\n2\n3\n:end:\n"))
    (org-test-with-temp-text
     src-block
     (should (progn (org-babel-execute-src-block)
                    (sleep-for 0 200)
                    (string= (concat src-block result)
                             (buffer-string)))))))




(provide 'test-ob-R)

;;; test-ob-R.el ends here
 
