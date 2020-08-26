;;; test-ob-lua.el --- tests for ob-lua.el

;; Copyright (c) 2016, 2019 Thibault Marin
;; Authors: Thibault Marin

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
(unless (featurep 'ob-lua)
  (signal 'missing-test-dependency "Support for Lua code blocks"))

(ert-deftest test-ob-lua/simple-value ()
  "Test associative array return by value."
  (should
   (= 2
      (org-test-with-temp-text
	  "#+name: eg
| a   | 1 |
| b   | 2 |

#+header: :results value
#+header: :var x = eg
#+begin_src lua
return x['b']
#+end_src"
        (org-babel-next-src-block)
        (org-babel-execute-src-block)))))

(ert-deftest test-ob-lua/simple-output ()
  "Test text output from table."
  (should
   (equal "result: c\n"
	  (org-test-with-temp-text
	      "#+name: eg
| a | b | c | d |

#+header: :results output
#+header: :var x = eg
#+begin_src lua
print('result: ' .. x[1][3])
#+end_src"
	    (org-babel-next-src-block)
	    (org-babel-execute-src-block)))))


(ert-deftest test-ob-lua/colnames-yes-header-argument ()
  "Test table passing with `colnames' header."
  (should
   (equal "a"
	  (org-test-with-temp-text
	      "#+name: eg
| col |
|-----|
| a   |
| b   |

#+header: :colnames yes
#+header: :var x = eg
#+begin_src lua
return x[1]
#+end_src"
	    (org-babel-next-src-block)
	    (org-babel-execute-src-block)))))


(ert-deftest test-ob-lua/colnames-yes-header-argument-pp ()
  "Test table passing with `colnames' header and pp option."
  (should
   (equal "a = 12\nb = 13\n"
	  (org-test-with-temp-text
	      "#+name: eg
| col | val |
|-----+-----|
| a   |  12 |
| b   |  13 |

#+header: :results value pp
#+header: :colnames yes
#+header: :var x = eg
#+begin_src lua
return x
#+end_src"
	    (org-babel-next-src-block)
	    (org-babel-execute-src-block)))))

(ert-deftest test-ob-lua/colnames-nil-header-argument ()
  "Test table with `colnames' set to `nil'."
  (should
   (equal "1 = a\n2 = b\n"
	  (org-test-with-temp-text
	      "#+name: eg
| col |
|-----|
| a   |
| b   |

#+header: :colnames nil
#+header: :var x = eg
#+header: :results value pp
#+begin_src lua
return x
#+end_src"
	    (org-babel-next-src-block)
	    (org-babel-execute-src-block)))))

(ert-deftest test-ob-lua/colnames-no-header-argument ()
  "Test table passing without `colnames'."
  (should
   (equal "1 = col\n2 = a\n3 = b\n"
	  (org-test-with-temp-text
	      "#+name: eg
| col |
|-----|
| a   |
| b   |

#+header: :colnames no
#+header: :var x = eg
#+header: :results value pp
#+begin_src lua
return x
#+end_src"
	    (org-babel-next-src-block)
	    (org-babel-execute-src-block)))))

(provide 'test-ob-lua)

;;; test-ob-lua.el ends here
