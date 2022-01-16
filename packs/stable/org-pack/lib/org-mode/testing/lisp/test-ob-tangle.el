;;; test-ob-tangle.el --- tests for ob-tangle.el

;; Copyright (c) 2010-2016, 2019 Eric Schulte
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

;;; Comments:

;; Template test file for Org tests


;;; Code:

(require 'subr-x)

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
      (should (unwind-protect
		  (with-temp-buffer
		    (insert-file-contents "babel.sh")
		    (goto-char (point-min))
		    (re-search-forward (regexp-quote tangled) nil t))
		(when (file-exists-p "babel.sh") (delete-file "babel.sh")))))))

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
    (regexp-quote "# [[https://orgmode.org][Org mode]]")
    (org-test-with-temp-text-in-file
        "[[https://orgmode.org][Org mode]]
#+header: :comments org :results output :tangle \"test-ob-tangle.sh\"
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

(ert-deftest ob-tangle/comment-links-relative-file ()
  "Test relative file name handling when commenting with links."
  (should
   (org-test-with-temp-text-in-file
       "* H
#+header: :tangle \"test-ob-tangle.el\" :comments link
#+begin_src emacs-lisp
1
#+end_src"
     (unwind-protect
	 (let ((org-babel-tangle-use-relative-file-links t))
	   (org-babel-tangle)
	   (with-temp-buffer
	     (insert-file-contents "test-ob-tangle.el")
	     (buffer-string)
	     (goto-char (point-min))
	     (search-forward
	      (concat "[file:" (file-name-nondirectory file))
	      nil t)))
       (delete-file "test-ob-tangle.el"))))
  (should
   (org-test-with-temp-text-in-file
       "* H
#+header: :tangle \"test-ob-tangle.el\" :comments link
#+begin_src emacs-lisp
1
#+end_src"
     (unwind-protect
	 (let ((org-babel-tangle-use-relative-file-links nil))
	   (org-babel-tangle)
	   (with-temp-buffer
	     (insert-file-contents "test-ob-tangle.el")
	     (buffer-string)
	     (goto-char (point-min))
	     (search-forward (concat "[file:" file) nil t)))
       (delete-file "test-ob-tangle.el")))))

(ert-deftest ob-tangle/jump-to-org ()
  "Test `org-babel-tangle-jump-to-org' specifications."
  ;; Standard test.
  (let ((org-file-apps '((t . emacs))))
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
            (buffer-string))))))))

(ert-deftest ob-tangle/nested-block ()
  "Test tangling of org file with nested block."
  (should
   (string=
    "#+begin_src org
,#+begin_src emacs-lisp
1
,#+end_src
#+end_src
"
    (org-test-with-temp-text-in-file
        "#+header: :tangle \"test-ob-tangle.org\"
#+begin_src org
,#+begin_src org
,,#+begin_src emacs-lisp
1
,,#+end_src
,#+end_src
#+end_src"
      (unwind-protect
          (progn (org-babel-tangle)
                 (with-temp-buffer (insert-file-contents "test-ob-tangle.org")
                                   (buffer-string)))
        (delete-file "test-ob-tangle.org"))))))

(ert-deftest ob-tangle/block-order ()
  "Test order of tangled blocks."
  ;; Order per language.
  (should
   (equal '("1" "2")
	  (let ((file (make-temp-file "org-tangle-")))
	    (unwind-protect
		(progn
		  (org-test-with-temp-text-in-file
		      (format "#+property: header-args :tangle %S
#+begin_src emacs-lisp
1
#+end_src

#+begin_src emacs-lisp
2
#+end_src"
			      file)
		    (org-babel-tangle))
		  (with-temp-buffer
		    (insert-file-contents file)
		    (org-split-string (buffer-string))))
	      (delete-file file)))))
  ;; Order per source block.
  (should
   (equal '("1" "2")
	  (let ((file (make-temp-file "org-tangle-")))
	    (unwind-protect
		(progn
		  (org-test-with-temp-text-in-file
		      (format "#+property: header-args :tangle %S
#+begin_src foo
1
#+end_src

#+begin_src bar
2
#+end_src"
			      file)
		    (org-babel-tangle))
		  (with-temp-buffer
		    (insert-file-contents file)
		    (org-split-string (buffer-string))))
	      (delete-file file)))))
  ;; Preserve order with mixed languages.
  (should
   (equal '("1" "2" "3" "4")
	  (let ((file (make-temp-file "org-tangle-")))
	    (unwind-protect
		(progn
		  (org-test-with-temp-text-in-file
		      (format "#+property: header-args :tangle %S
#+begin_src foo
1
#+end_src

#+begin_src bar
2
#+end_src

#+begin_src foo
3
#+end_src

#+begin_src bar
4
#+end_src"
			      file)
		    (org-babel-tangle))
		  (with-temp-buffer
		    (insert-file-contents file)
		    (org-split-string (buffer-string))))
	      (delete-file file))))))

(ert-deftest ob-tangle/commented-src-blocks ()
  "Test omission of commented src blocks."
  (should
   (equal '("A")
	  (let ((file (make-temp-file "org-tangle-")))
	    (unwind-protect
		(progn
		  (org-test-with-temp-text-in-file
		      (format "#+property: header-args :tangle %S
* A

  #+begin_src emacs-lisp
  A
  #+end_src

* COMMENT B

  #+begin_src emacs-lisp
  B
  #+end_src

* C

  # #+begin_src emacs-lisp
  # C
  # #+end_src

* D

  #+begin_comment
  #+begin_src emacs-lisp
  D
  #+end_src
  #+end_comment"
			      file)
		    (org-babel-tangle))
		  (with-temp-buffer
		    (insert-file-contents file)
		    (org-split-string (buffer-string))))
	      (delete-file file)))))
  (should
   (equal '("A")
	  (let ((file (make-temp-file "org-tangle-")))
	    (unwind-protect
		(progn
		  (org-test-with-temp-text-in-file
		      (format "#+property: header-args :tangle %S
* A

  #+begin_src elisp :noweb yes
  A
  <<B>>
  <<C>>
  <<D>>
  #+end_src

* COMMENT B

  #+begin_src elisp :noweb-ref B
  B
  #+end_src

* C

  # #+begin_src elisp :noweb-ref C
  # C
  # #+end_src

* D

  #+begin_comment
  #+begin_src elisp :noweb-ref D
  D
  #+end_src
  #+end_comment"
			      file)
		    (let (org-babel-noweb-error-all-langs
			  org-babel-noweb-error-langs)
		      (org-babel-tangle)))
		  (with-temp-buffer
		    (insert-file-contents file)
		    (org-split-string (buffer-string))))
	      (delete-file file))))))

(ert-deftest ob-tangle/multiple-noweb-in-line ()
  "Test handling of multiple noweb references in a single line."
  (should
   (equal '("1" "2" "1")
	  (let ((file (make-temp-file "org-tangle-")))
	    (unwind-protect
		(progn
		  (org-test-with-temp-text-in-file
		      (format "
#+name: block1
#+begin_src elisp
1
#+end_src

#+name: block2
#+begin_src elisp
2
#+end_src

#+name: block3
#+begin_src elisp :noweb yes :tangle %s
<<block1>> <<block2>> <<block1>>
#+end_src"
			      file)
		    (let ((org-babel-noweb-error-all-langs nil)
			  (org-babel-noweb-error-langs nil))
		      (org-babel-tangle)))
		  (with-temp-buffer
		    (insert-file-contents file)
		    (org-split-string (buffer-string))))
	      (delete-file file))))))

(ert-deftest ob-tangle/detangle-false-positive ()
  "Test handling of false positive link during detangle."
  (let (buffer)
    (unwind-protect
	(org-test-in-example-file (expand-file-name "babel.el" org-test-example-dir)
	  (org-babel-detangle)
	  (org-test-at-id "73115FB0-6565-442B-BB95-50195A499EF4"
	    (setq buffer (current-buffer))
	    (org-babel-next-src-block)
	    (should (equal (string-trim (org-element-property
					 :value (org-element-at-point)))
			   ";; detangle changes"))))
      (kill-buffer buffer))))

(provide 'test-ob-tangle)

;;; test-ob-tangle.el ends here
