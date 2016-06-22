;;; test-ob-emacs-lisp.el

;; Copyright (c) 2012-2016 Free Software Foundation, Inc.
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

;; Org-mode tests for ob-emacs-lisp.el live here

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
  (org-test-with-temp-text-in-file "
#+begin_src emacs-lisp :var a=2
2;;
#+end_src"
    (org-babel-next-src-block)
    (org-babel-execute-maybe)
    (re-search-forward "results" nil t)
    (forward-line)
    (should (string=
	     ": 2"
	     (buffer-substring-no-properties (point-at-bol) (point-at-eol))))))

(provide 'test-ob-emacs-lisp)

 ;;; test-ob-emacs-lisp.el ends here
