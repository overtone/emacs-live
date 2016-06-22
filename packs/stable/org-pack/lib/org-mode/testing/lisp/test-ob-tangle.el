;;; test-ob-tangle.el --- tests for ob-tangle.el

;; Copyright (c) 2010-2016 Eric Schulte
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

;;; Comments:

;; Template test file for Org-mode tests


;;; Code:

;; TODO
;; (ert-deftest ob-tangle/noweb-on-tangle ()
;;   "Noweb header arguments tangle correctly.
;; - yes      expand on both export and tangle
;; - no       expand on neither export or tangle
;; - tangle   expand on only tangle not export"
;;   (let ((target-file (make-temp-file "ob-tangle-test-")))
;;     (org-test-at-id "eb1f6498-5bd9-45e0-9c56-50717053e7b7"
;;       (org-narrow-to-subtree)
;;       (org-babel-tangle target-file))
;;     (let ((tang (with-temp-buffer
;;                (insert-file-contents target-file)
;;                (buffer-string))))
;;       (flet ((exp-p (arg)
;;                  (and
;;                   (string-match
;;                    (format "noweb-%s-start\\([^\000]*\\)noweb-%s-end" arg arg)
;;                    tang)
;;                   (string-match "expanded" (match-string 1 tang)))))
;;      (should (exp-p "yes"))
;;      (should-not (exp-p "no"))
;;      (should (exp-p "tangle"))))))

(ert-deftest ob-tangle/no-excessive-id-insertion-on-tangle ()
  "Don't add IDs to headings without tangling code blocks."
  (org-test-at-id "ef06fd7f-012b-4fde-87a2-2ae91504ea7e"
    (org-babel-next-src-block)
    (org-narrow-to-subtree)
    (org-babel-tangle)
    (should (null (org-id-get)))))

(ert-deftest ob-tangle/continued-code-blocks-w-noweb-ref ()
  "Test that the :noweb-ref header argument is used correctly."
  (org-test-at-id "54d68d4b-1544-4745-85ab-4f03b3cbd8a0"
    (let ((tangled
           "df|sed '1d'|awk '{print $5 \" \" $6}'|sort -n |tail -1|awk '{print $2}'"))
      (org-narrow-to-subtree)
      (org-babel-tangle)
      (with-temp-buffer
        (insert-file-contents "babel.sh")
        (goto-char (point-min))
        (should (re-search-forward (regexp-quote tangled) nil t)))
      (delete-file "babel.sh"))))

(ert-deftest ob-tangle/expand-headers-as-noweb-references ()
  "Test that references to headers are expanded during noweb expansion."
  (org-test-at-id "2409e8ba-7b5f-4678-8888-e48aa02d8cb4"
    (org-babel-next-src-block 2)
    (let ((expanded (org-babel-expand-noweb-references)))
      (should (string-match (regexp-quote "simple") expanded))
      (should (string-match (regexp-quote "length 14") expanded)))))

(ert-deftest ob-tangle/comment-links-at-left-margin ()
  "Test commenting of links at left margin."
  (should
   (string-match
    (regexp-quote "# [[http://orgmode.org][Org mode]]")
    (org-test-with-temp-text-in-file
        "[[http://orgmode.org][Org mode]]
#+header: :comments org :tangle \"test-ob-tangle.sh\"
#+begin_src sh
echo 1
#+end_src"
      (unwind-protect
          (progn (org-babel-tangle)
                 (with-temp-buffer (insert-file-contents "test-ob-tangle.sh")
                                   (buffer-string)))
        (delete-file "test-ob-tangle.sh"))))))

(ert-deftest ob-tangle/comment-links-numbering ()
  "Test numbering of source blocks when commenting with links."
  (should
   (org-test-with-temp-text-in-file
       "* H
#+header: :tangle \"test-ob-tangle.el\" :comments link
#+begin_src emacs-lisp
1
#+end_src

#+header: :tangle \"test-ob-tangle.el\" :comments link
#+begin_src emacs-lisp
2
#+end_src"
     (unwind-protect
         (progn
           (org-babel-tangle)
           (with-temp-buffer
             (insert-file-contents "test-ob-tangle.el")
             (buffer-string)
             (goto-char (point-min))
             (and (search-forward "[H:1]]" nil t)
                  (search-forward "[H:2]]" nil t))))
       (delete-file "test-ob-tangle.el")))))

(ert-deftest ob-tangle/jump-to-org ()
  "Test `org-babel-tangle-jump-to-org' specifications."
  ;; Standard test.
  (should
   (equal
    "* H\n#+begin_src emacs-lisp\n1\n#+end_src"
    (org-test-with-temp-text-in-file
        "* H\n#+begin_src emacs-lisp\n1\n#+end_src"
      (let ((file (buffer-file-name)))
        (org-test-with-temp-text
            (format ";; [[file:%s][H:1]]\n<point>1\n;; H:1 ends here\n"
                    (file-name-nondirectory file))
          (org-babel-tangle-jump-to-org)
          (buffer-string))))))
  ;; Multiple blocks in the same section.
  (should
   (equal
    "2"
    (org-test-with-temp-text-in-file
        "* H

first block

#+begin_src emacs-lisp
1
#+end_src

another block

#+begin_src emacs-lisp
2
#+end_src
"
      (let ((file (buffer-file-name)))
        (org-test-with-temp-text
            (format ";; [[file:%s][H:2]]\n<point>2\n;; H:2 ends here\n"
                    (file-name-nondirectory file))
          (org-babel-tangle-jump-to-org)
          (buffer-substring (line-beginning-position)
                            (line-end-position)))))))
  ;; Preserve position within the source code.
  (should
   (equal
    "1)"
    (org-test-with-temp-text-in-file
        "* H\n#+begin_src emacs-lisp\n(+ 1 1)\n#+end_src"
      (let ((file (buffer-file-name)))
        (org-test-with-temp-text
            (format ";; [[file:%s][H:1]]\n(+ 1 <point>1)\n;; H:1 ends here\n"
                    (file-name-nondirectory file))
          (org-babel-tangle-jump-to-org)
          (buffer-substring-no-properties (point) (line-end-position)))))))
  ;; Blocks before first heading.
  (should
   (equal
    "Buffer start\n#+begin_src emacs-lisp\n1\n#+end_src\n* H"
    (org-test-with-temp-text-in-file
        "Buffer start\n#+begin_src emacs-lisp\n1\n#+end_src\n* H"
      (let ((file (buffer-file-name)))
        (org-test-with-temp-text
            (format ";; [[file:%s][H:1]]\n<point>1\n;; H:1 ends here\n"
                    (file-name-nondirectory file))
          (org-babel-tangle-jump-to-org)
          (buffer-string))))))
  ;; Special case: buffer starts with a source block.
  (should
   (equal
    "#+begin_src emacs-lisp\n1\n#+end_src\n* H"
    (org-test-with-temp-text-in-file
        "#+begin_src emacs-lisp\n1\n#+end_src\n* H"
      (let ((file (buffer-file-name)))
        (org-test-with-temp-text
            (format ";; [[file:%s][H:1]]\n<point>1\n;; H:1 ends here\n"
                    (file-name-nondirectory file))
          (org-babel-tangle-jump-to-org)
          (buffer-string)))))))

(provide 'test-ob-tangle)

;;; test-ob-tangle.el ends here
