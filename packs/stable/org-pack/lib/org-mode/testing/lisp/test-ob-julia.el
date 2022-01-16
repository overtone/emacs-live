;;; test-ob-python.el --- tests for ob-python.el

;; Copyright (c) 2011-2014, 2019, 2021 Eric Schulte
;; Authors: Pedro Bruel, based on test-ob-python.el by Eric Schulte
;; Maintainer: Pedro Bruel <pedro.bruel@gmail.com>

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
(org-test-for-executable "julia")
(unless (featurep 'ob-julia)
  (signal 'missing-test-dependency "Support for julia code blocks"))

(ert-deftest test-ob-julia/colnames-yes-header-argument ()
  (should
   (equal '(("col") hline ("a") ("b"))
	  (org-test-with-temp-text "#+name: eg
| col |
|-----|
| a   |
| b   |

#+header: :colnames yes
#+header: :var x = eg
<point>#+begin_src julia
return x
#+end_src"
	    (org-babel-execute-src-block)))))

(ert-deftest test-ob-julia/colnames-yes-header-argument-again ()
  (should
   (equal '(("a") hline ("b*") ("c*"))
	  (org-test-with-temp-text "#+name: less-cols
| a |
|---|
| b |
| c |

#+header: :colnames yes
<point>#+begin_src julia :var tab=less-cols
  return [[val + '*' for val in row] for row in tab]
#+end_src"
	    (org-babel-execute-src-block)))))

(ert-deftest test-ob-julia/colnames-nil-header-argument ()
  (should
   (equal '(("col") hline ("a") ("b"))
	  (org-test-with-temp-text "#+name: eg
| col |
|-----|
| a   |
| b   |

#+header: :colnames nil
#+header: :var x = eg
<point>#+begin_src julia
return x
#+end_src"
	    (org-babel-execute-src-block)))))

(ert-deftest test-ob-julia/colnames-no-header-argument-again ()
  (should
   (equal '(("a*") ("b*") ("c*"))
	  (org-test-with-temp-text "#+name: less-cols
| a |
|---|
| b |
| c |

#+header: :colnames no
<point>#+begin_src julia :var tab=less-cols
  return [[val + '*' for val in row] for row in tab]
#+end_src"
	    (org-babel-execute-src-block)))))

(ert-deftest test-ob-julia/colnames-no-header-argument ()
  (should
   (equal '(("col") ("a") ("b"))
	  (org-test-with-temp-text "#+name: eg
| col |
|-----|
| a   |
| b   |

#+header: :colnames no
#+header: :var x = eg
<point>#+begin_src julia
return x
#+end_src"
	    (org-babel-execute-src-block)))))

(ert-deftest test-ob-julia/session-multiline ()
  (should
   (equal "20"
	  (org-test-with-temp-text "#+begin_src julia :session :results output
  foo = 0
  for _ in range(10):
      foo += 1

      foo += 1

  print(foo)
#+end_src"
	    (org-babel-execute-src-block)))))

(ert-deftest test-ob-julia/insert-necessary-blank-line-when-sending-code-to-interpreter ()
  (should
   (equal 2 (org-test-with-temp-text "#+begin_src julia :session :results value
if True:
    1
2
#+end_src"
	      ;; Previously, while adding `:session' to a normal code
	      ;; block, also need to add extra blank lines to end
	      ;; indent block or indicate logical sections. Now, the
	      ;; `org-babel-julia-evaluate-session' can do it
	      ;; automatically:
	      ;;
	      ;; >>> if True:
	      ;; >>>     1
	      ;; >>> <insert_blank_line_here>
	      ;; >>> 2
	      (org-babel-execute-maybe)
	      (org-babel-execute-src-block)))))

(ert-deftest test-ob-julia/if-else-block ()
  (should
   (equal "success" (org-test-with-temp-text "#+begin_src julia :session :results value
value = 'failure'
if False:
    pass
else:
    value = 'success'
value
#+end_src"
	      (org-babel-execute-src-block)))))

(ert-deftest test-ob-julia/indent-block-with-blank-lines ()
  (should
   (equal 20
	  (org-test-with-temp-text "#+begin_src julia :session :results value
  foo = 0
  for i in range(10):
      foo += 1

      foo += 1

  foo
#+end_src"
	    (org-babel-execute-src-block)))))

(ert-deftest test-ob-julia/assign-underscore ()
  (should
   (equal "success"
	  (org-test-with-temp-text "#+begin_src julia :session :results value
_ = 'failure'
'success'
#+end_src"
	    (org-babel-execute-src-block)))))

(ert-deftest test-ob-julia/multiline-var ()
  (should
   (equal "a\nb\nc"
	  (org-test-with-temp-text "#+begin_src julia :var text=\"a\\nb\\nc\"
return text
#+end_src"
	    (org-babel-execute-src-block)))))

(ert-deftest test-ob-julia/multiline-str ()
  (should
   (equal "a\nb\nc"
	  (org-test-with-temp-text "#+begin_src julia
text=\"a\\nb\\nc\"
return text
#+end_src"
	    (org-babel-execute-src-block)))))

(ert-deftest test-ob-julia/header-var-assignment ()
  (should
   (equal "success"
	  (org-test-with-temp-text "#+begin_src julia :var text=\"failure\"
text
text=\"success\"
return text
#+end_src"
	    (org-babel-execute-src-block)))))

(ert-deftest test-ob-julia/session-value-sleep ()
  (should
   (equal "success"
	  (org-test-with-temp-text "#+begin_src julia :session :results value
import time
time.sleep(.1)
'success'
#+end_src"
	    (org-babel-execute-src-block)))))

(ert-deftest test-ob-julia/async-simple-session-output ()
  (let ((org-babel-temporary-directory temporary-file-directory)
        (org-confirm-babel-evaluate nil))
    (org-test-with-temp-text
     "#+begin_src julia :session :async yes :results output
import time
time.sleep(.1)
print('Yep!')
#+end_src\n"
     (should (let ((expected "Yep!"))
	       (and (not (string= expected (org-babel-execute-src-block)))
		    (string= expected
			     (progn
			       (sleep-for 0 200)
			       (goto-char (org-babel-where-is-src-block-result))
			       (org-babel-read-result)))))))))

(ert-deftest test-ob-julia/async-named-output ()
  (let (org-confirm-babel-evaluate
        (org-babel-temporary-directory temporary-file-directory)
        (src-block "#+begin_src julia :async :session :results output
print(\"Yep!\")
#+end_src")
        (results-before "

#+NAME: foobar
#+RESULTS:
: Nope!")
        (results-after "

#+NAME: foobar
#+RESULTS:
: Yep!
"))
    (org-test-with-temp-text
     (concat src-block results-before)
     (should (progn (org-babel-execute-src-block)
                    (sleep-for 0 200)
                    (string= (concat src-block results-after)
                             (buffer-string)))))))

(ert-deftest test-ob-julia/async-output-drawer ()
  (let (org-confirm-babel-evaluate
        (org-babel-temporary-directory temporary-file-directory)
        (src-block "#+begin_src julia :async :session :results output drawer
print(list(range(3)))
#+end_src")
        (result "

#+RESULTS:
:results:
[0, 1, 2]
:end:
"))
    (org-test-with-temp-text
     src-block
     (should (progn (org-babel-execute-src-block)
                    (sleep-for 0 200)
                    (string= (concat src-block result)
                             (buffer-string)))))))

(provide 'test-ob-julia)

;;; test-ob-julia.el ends here
