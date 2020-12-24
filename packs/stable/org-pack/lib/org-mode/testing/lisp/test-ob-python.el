;;; test-ob-python.el --- tests for ob-python.el

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
(org-test-for-executable "python")
(unless (featurep 'ob-python)
  (signal 'missing-test-dependency "Support for Python code blocks"))

(ert-deftest test-ob-python/colnames-yes-header-argument ()
  (should
   (equal '(("col") hline ("a") ("b"))
	  (org-test-with-temp-text "#+name: eg
| col |
|-----|
| a   |
| b   |

#+header: :colnames yes
#+header: :var x = eg
<point>#+begin_src python
return x
#+end_src"
	    (org-babel-execute-src-block)))))

(ert-deftest test-ob-python/colnames-yes-header-argument-again ()
  (should
   (equal '(("a") hline ("b*") ("c*"))
	  (org-test-with-temp-text "#+name: less-cols
| a |
|---|
| b |
| c |

#+header: :colnames yes
<point>#+begin_src python :var tab=less-cols
  return [[val + '*' for val in row] for row in tab]
#+end_src"
	    (org-babel-execute-src-block)))))

(ert-deftest test-ob-python/colnames-nil-header-argument ()
  (should
   (equal '(("col") hline ("a") ("b"))
	  (org-test-with-temp-text "#+name: eg
| col |
|-----|
| a   |
| b   |

#+header: :colnames nil
#+header: :var x = eg
<point>#+begin_src python
return x
#+end_src"
	    (org-babel-execute-src-block)))))

(ert-deftest test-ob-python/colnames-no-header-argument-again ()
  (should
   (equal '(("a*") ("b*") ("c*"))
	  (org-test-with-temp-text "#+name: less-cols
| a |
|---|
| b |
| c |

#+header: :colnames no
<point>#+begin_src python :var tab=less-cols
  return [[val + '*' for val in row] for row in tab]
#+end_src"
	    (org-babel-execute-src-block)))))

(ert-deftest test-ob-python/colnames-no-header-argument ()
  (should
   (equal '(("col") ("a") ("b"))
	  (org-test-with-temp-text "#+name: eg
| col |
|-----|
| a   |
| b   |

#+header: :colnames no
#+header: :var x = eg
<point>#+begin_src python
return x
#+end_src"
	    (org-babel-execute-src-block)))))

(ert-deftest test-ob-python/session-multiline ()
  (should
   (equal "20"
	  (org-test-with-temp-text "#+begin_src python :session :results output
  foo = 0
  for _ in range(10):
      foo += 1

      foo += 1

  print(foo)
#+end_src"
	    (org-babel-execute-src-block)))))

(ert-deftest test-ob-python/insert-necessary-blank-line-when-sending-code-to-interpreter ()
  (should
   (equal 2 (org-test-with-temp-text "#+begin_src python :session :results value
if True:
    1
2
#+end_src"
	      ;; Previously, while adding `:session' to a normal code
	      ;; block, also need to add extra blank lines to end
	      ;; indent block or indicate logical sections. Now, the
	      ;; `org-babel-python-evaluate-session' can do it
	      ;; automatically:
	      ;;
	      ;; >>> if True:
	      ;; >>>     1
	      ;; >>> <insert_blank_line_here>
	      ;; >>> 2
	      (org-babel-execute-maybe)
	      (org-babel-execute-src-block)))))

(ert-deftest test-ob-python/if-else-block ()
  (should
   (equal "success" (org-test-with-temp-text "#+begin_src python :session :results value
value = 'failure'
if False:
    pass
else:
    value = 'success'
value
#+end_src"
	      (org-babel-execute-src-block)))))

(ert-deftest test-ob-python/indent-block-with-blank-lines ()
  (should
   (equal 20
	  (org-test-with-temp-text "#+begin_src python :session :results value
  foo = 0
  for i in range(10):
      foo += 1

      foo += 1

  foo
#+end_src"
	    (org-babel-execute-src-block)))))

(ert-deftest test-ob-python/assign-underscore ()
  (should
   (equal "success"
	  (org-test-with-temp-text "#+begin_src python :session :results value
_ = 'failure'
'success'
#+end_src"
	    (org-babel-execute-src-block)))))

(ert-deftest test-ob-python/multiline-var ()
  (should
   (equal "a\nb\nc"
	  (org-test-with-temp-text "#+begin_src python :var text=\"a\\nb\\nc\"
return text
#+end_src"
	    (org-babel-execute-src-block)))))

(ert-deftest test-ob-python/multiline-str ()
  (should
   (equal "a\nb\nc"
	  (org-test-with-temp-text "#+begin_src python
text=\"a\\nb\\nc\"
return text
#+end_src"
	    (org-babel-execute-src-block)))))

(ert-deftest test-ob-python/header-var-assignment ()
  (should
   (equal "success"
	  (org-test-with-temp-text "#+begin_src python :var text=\"failure\"
text
text=\"success\"
return text
#+end_src"
	    (org-babel-execute-src-block)))))

(ert-deftest test-ob-python/session-value-sleep ()
  (should
   (equal "success"
	  (org-test-with-temp-text "#+begin_src python :session :results value
import time
time.sleep(.1)
'success'
#+end_src"
	    (org-babel-execute-src-block)))))

(provide 'test-ob-python)

;;; test-ob-python.el ends here
