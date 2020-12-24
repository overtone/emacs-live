;;; test-ob-emacs-lisp.el

;; Copyright (c) 2012-2020 Free Software Foundation, Inc.
;; Authors: Eric Schulte, Martyn Jago

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

;; Org tests for ob-emacs-lisp.el live here

;;; Code:
(ert-deftest ob-emacs-lisp/commented-last-block-line-no-var ()
  (org-test-with-temp-text-in-file "
#+begin_src emacs-lisp
;;
#+end_src"
    (org-babel-next-src-block)
    (org-babel-execute-maybe)
    (should (re-search-forward "results:" nil t))
    (forward-line)
    (should
     (string=
      ""
      (buffer-substring-no-properties (point-at-bol) (point-at-eol)))))
  (org-test-with-temp-text-in-file "
#+begin_src emacs-lisp
\"some text\";;
#+end_src"

    (org-babel-next-src-block)
    (org-babel-execute-maybe)
    (should (re-search-forward "results:" nil t))
    (forward-line)
    (should
     (string=
      ": some text"
      (buffer-substring-no-properties (point-at-bol) (point-at-eol))))))

(ert-deftest ob-emacs-lisp/commented-last-block-line-with-var ()
  (org-test-with-temp-text-in-file "
#+begin_src emacs-lisp :var a=1
;;
#+end_src"
    (org-babel-next-src-block)
    (org-babel-execute-maybe)
    (re-search-forward "results" nil t)
    (forward-line)
    (should (string=
	     ""
	     (buffer-substring-no-properties (point-at-bol) (point-at-eol))))))

(ert-deftest ob-emacs-lisp/commented-last-block-line ()
  (should
   (string= ": 2"
	    (org-test-with-temp-text-in-file "
#+begin_src emacs-lisp :var a=2
2;;
#+end_src"
	      (org-babel-next-src-block)
	      (org-babel-execute-maybe)
	      (re-search-forward "results" nil t)
	      (buffer-substring-no-properties (line-beginning-position 2)
					      (line-end-position 2))))))

(ert-deftest ob-emacs-lisp/dynamic-lexical-execute ()
  (cl-flet ((execute (text)
              (org-test-with-temp-text-in-file text
		(org-babel-next-src-block)
		(org-babel-execute-maybe)
		(re-search-forward "results" nil t)
		(re-search-forward ": " nil t)
		(buffer-substring-no-properties (point) (point-at-eol)))))

    (should (string= "dynamic" (execute "
#+begin_src emacs-lisp :lexical no :results verbatim
(let ((x 'dynamic)) (funcall (let ((x 'lexical)) (lambda () x))))
#+end_src")))

    (should (string= "lexical" (execute "
#+begin_src emacs-lisp :lexical yes :results verbatim
(let ((x 'dynamic)) (funcall (let ((x 'lexical)) (lambda () x))))
#+end_src")))

    (should (string= "dynamic" (let ((x 'dynamic)) (execute "
#+begin_src emacs-lisp :lexical no :results verbatim
x
#+end_src"))))

    (should (string= "lexical" (let ((x 'dynamic)) (execute "
#+begin_src emacs-lisp :lexical '((x . lexical)) :results verbatim
x
#+end_src"))))

    ;; Src block execution uses `eval'. As of 2019-02-26, `eval' does
    ;; not dynamically bind `lexical-binding' to the value of its
    ;; LEXICAL parameter. Hence, (eval 'lexical-binding LEXICAL)
    ;; evaluates to the same value that just `lexical-binding'
    ;; evaluates to, even if LEXICAL is different. So tests like the
    ;; following do not work here:
    ;;
    ;; (should (string= "t" (execute "
    ;; #+begin_src emacs-lisp :lexical yes :results verbatim
    ;; lexical-binding
    ;; #+end_src")))
    ;;
    ;; However, the corresponding test in
    ;; `ob-emacs-lisp/dynamic-lexical-edit' does work.
    ))

(ert-deftest ob-emacs-lisp/dynamic-lexical-edit ()
  (cl-flet ((execute (text)
              (org-test-with-temp-text-in-file text
		(org-babel-next-src-block)
		(org-edit-src-code)
		(goto-char (point-max))
		(prog1 (eval-last-sexp 0)
		  (org-edit-src-exit)))))

    (should (eq 'dynamic (execute "
#+begin_src emacs-lisp :lexical no :results verbatim
(let ((x 'dynamic)) (funcall (let ((x 'lexical)) (lambda () x))))
#+end_src")))

    (should (eq 'lexical (execute "
#+begin_src emacs-lisp :lexical yes :results verbatim
(let ((x 'dynamic)) (funcall (let ((x 'lexical)) (lambda () x))))
#+end_src")))

    (should (eq 'dynamic (let ((x 'dynamic)) (execute "
#+begin_src emacs-lisp :lexical no :results verbatim
x
#+end_src"))))

    (should (eq 'lexical (let ((x 'dynamic)) (execute "
#+begin_src emacs-lisp :lexical '((x . lexical)) :results verbatim
x
#+end_src"))))

    (should (equal nil (execute "
#+begin_src emacs-lisp :lexical no :results verbatim
lexical-binding
#+end_src")))

    (should (equal t (execute "
#+begin_src emacs-lisp :lexical yes :results verbatim
lexical-binding
#+end_src")))

    (should (equal '((x . 0)) (execute "
#+begin_src emacs-lisp :lexical '((x . 0)) :results verbatim
lexical-binding
#+end_src")))))

(provide 'test-ob-emacs-lisp)

 ;;; test-ob-emacs-lisp.el ends here
