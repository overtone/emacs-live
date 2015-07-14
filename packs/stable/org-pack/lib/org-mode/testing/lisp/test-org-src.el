;;; test-org-src.el --- tests for org-src.el

;; Copyright (C) 2012-2015  Le Wang

;; Author: Le Wang <l26wang at gmail dot com>

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

(require 'org-test)



(ert-deftest test-org-src/basic ()
  "Editing regular block works, with point on source block."
  (org-test-with-temp-text
      "
<point>#+begin_src emacs-lisp
  (message hello)
#+end_src
"
    (let ((org-edit-src-content-indentation 2)
	  (org-src-preserve-indentation nil))
      (org-edit-special)
      (insert "blah")
      (org-edit-src-exit)
      (should (equal (buffer-string) "
#+begin_src emacs-lisp
  blah(message hello)
#+end_src
"))
      (should (org-looking-at-p "(message hello)")))))

(ert-deftest test-org-src/point-outside-block ()
  "Editing with point before/after block signals expected error."
  (org-test-with-temp-text
      "
#+begin_src emacs-lisp
  (message hello)
#+end_src
"
    (goto-line 1)
    (should-error (org-edit-special))
    (goto-char (point-max))
    (should-error (org-edit-special))))

(ert-deftest test-org-src/empty-block ()
  "Editing empty block."
  (org-test-with-temp-text
      "
<point>#+begin_src emacs-lisp
#+end_src
"
    (let ((org-edit-src-content-indentation 0)
	  (org-src-preserve-indentation nil))
      (org-edit-special)
      (insert "blah")
      (org-edit-src-exit)
      (should (equal (buffer-string) "
#+begin_src emacs-lisp
blah
#+end_src
"))
      (should
       (equal (buffer-substring (line-beginning-position) (point)) "blah")))))

(ert-deftest test-org-src/blank-line-block ()
  "Editing block with just a blank line."
  (org-test-with-temp-text-in-file
      "
#+begin_src emacs-lisp

#+end_src
"
    (let ((org-edit-src-content-indentation 2)
	  (org-src-preserve-indentation nil))
      (goto-line 2)
      (org-edit-special)
      (insert "blah")
      (org-edit-src-exit)
      (should (equal (buffer-string) "
#+begin_src emacs-lisp
  blah
#+end_src
")))))

(provide 'test-org-src)
;;; test-org-src.el ends here
