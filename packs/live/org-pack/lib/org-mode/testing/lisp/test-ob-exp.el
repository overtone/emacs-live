;;; test-ob-exp.el

;; Copyright (c) 2010-2013 Eric Schulte
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

(defmacro org-test-with-expanded-babel-code (&rest body)
  "Execute BODY while in a buffer with all Babel code evaluated.
Current buffer is a copy of the original buffer."
  `(let ((string (buffer-string))
	 (buf (current-buffer)))
     (with-temp-buffer
       (org-mode)
       (insert string)
       (let ((org-current-export-file buf))
	 (org-babel-exp-process-buffer))
       (goto-char (point-min))
       (progn ,@body))))

(ert-deftest test-ob-exp/org-babel-exp-src-blocks/w-no-headers ()
  "Testing export without any headlines in the Org mode file."
  (require 'ox-html)
  (let ((html-file (concat (file-name-sans-extension org-test-no-heading-file)
			   ".html")))
    (when (file-exists-p html-file) (delete-file html-file))
    (org-test-in-example-file org-test-no-heading-file
      ;; Export the file to HTML.
      (org-export-to-file 'html html-file))
    ;; should create a .html file
    (should (file-exists-p html-file))
    ;; should not create a file with "::" appended to it's name
    (should-not (file-exists-p (concat org-test-no-heading-file "::")))
    (when (file-exists-p html-file) (delete-file html-file))))

(ert-deftest test-ob-exp/org-babel-exp-src-blocks/w-no-file ()
  "Testing export from buffers which are not visiting any file."
  (require 'ox-html)
  (let ((name (generate-new-buffer-name "*Org HTML Export*")))
    (org-test-in-example-file nil
      (org-export-to-buffer 'html name nil nil t))
    ;; Should create a HTML buffer.
    (should (buffer-live-p (get-buffer name)))
    ;; Should contain the content of the buffer.
    (with-current-buffer (get-buffer name)
      (should (string-match (regexp-quote org-test-file-ob-anchor)
			    (buffer-string))))
    (when (get-buffer name) (kill-buffer name))))

(ert-deftest test-ob-exp/org-babel-exp-src-blocks/w-no-headers2 ()
  "Testing export without any headlines in the org-mode file."
  (let ((html-file (concat (file-name-sans-extension
			    org-test-link-in-heading-file)
			   ".html")))
    (when (file-exists-p html-file) (delete-file html-file))
    (org-test-in-example-file org-test-link-in-heading-file
      ;; export the file to html
      (org-export-to-file 'html html-file))
    ;; should create a .html file
    (should (file-exists-p html-file))
    ;; should not create a file with "::" appended to it's name
    (should-not (file-exists-p (concat org-test-link-in-heading-file "::")))
    (when (file-exists-p html-file) (delete-file html-file))))

(ert-deftest ob-exp/noweb-on-export ()
  "Noweb header arguments export correctly.
- yes      expand on both export and tangle
- no       expand on neither export or tangle
- tangle   expand on only tangle not export"
  (should
   (equal
    '("(message \"expanded1\")" "(message \"expanded2\")" ";; noweb-1-yes-start
  (message \"expanded1\")
  (message \"expanded1\")" ";; noweb-no-start
  <<noweb-example1>>" ";; noweb-2-yes-start
  (message \"expanded2\")
  (message \"expanded2\")" ";; noweb-tangle-start
<<noweb-example1>>
<<noweb-example2>>")
    (org-test-at-id "eb1f6498-5bd9-45e0-9c56-50717053e7b7"
      (org-narrow-to-subtree)
      (org-element-map
	  (org-test-with-expanded-babel-code (org-element-parse-buffer))
	  'src-block
	(lambda (src) (org-trim (org-element-property :value src))))))))

(ert-deftest ob-exp/noweb-on-export-with-exports-results ()
  "Noweb header arguments export correctly using :exports results.
- yes      expand on both export and tangle
- no       expand on neither export or tangle
- tangle   expand on only tangle not export"
  (should
   (equal
    '(";; noweb-no-start
  <<noweb-example1>>" "<<noweb-example1>>
<<noweb-example2>>")
    (org-test-at-id "8701beb4-13d9-468c-997a-8e63e8b66f8d"
      (org-narrow-to-subtree)
      (org-element-map
	  (org-test-with-expanded-babel-code (org-element-parse-buffer))
	  'src-block
	(lambda (src) (org-trim (org-element-property :value src))))))))

(ert-deftest ob-exp/exports-both ()
  "Test the \":exports both\" header argument.
The code block evaluation should create both a code block and
a table."
  (org-test-at-id "92518f2a-a46a-4205-a3ab-bcce1008a4bb"
    (org-narrow-to-subtree)
    (let ((tree (org-test-with-expanded-babel-code (org-element-parse-buffer))))
      (should (and (org-element-map tree 'src-block 'identity)
		   (org-element-map tree 'table 'identity))))))

(ert-deftest ob-exp/mixed-blocks-with-exports-both ()
  (should
   (equal
    '(property-drawer plain-list src-block fixed-width src-block plain-list)
    (org-test-at-id "5daa4d03-e3ea-46b7-b093-62c1b7632df3"
      (org-narrow-to-subtree)
      (mapcar 'org-element-type
	      (org-element-map
		  (org-test-with-expanded-babel-code
		   (org-element-parse-buffer 'greater-element))
		  'section 'org-element-contents nil t))))))

(ert-deftest ob-exp/export-with-name ()
  (should
   (string-match
    "=qux="
    (let ((org-babel-exp-code-template
	   "=%name=\n#+BEGIN_SRC %lang%flags\nbody\n#+END_SRC"))
      (org-test-at-id "b02ddd8a-eeb8-42ab-8664-8a759e6f43d9"
	(org-narrow-to-subtree)
	(org-test-with-expanded-babel-code
	 (buffer-string)))))))

(ert-deftest ob-exp/export-with-header-argument ()
  (let ((org-babel-exp-code-template
	 "
| header  | value    |
|---------+----------|
| foo     | %foo     |
| results | %results |
#+BEGIN_SRC %lang%flags\nbody\n#+END_SRC"))
    (org-test-at-id "b02ddd8a-eeb8-42ab-8664-8a759e6f43d9"
      (org-narrow-to-subtree)
      (org-test-with-expanded-babel-code
       (should (string-match "baz" (buffer-string)))
       (should (string-match "replace" (buffer-string)))))))

(ert-deftest ob-exp/noweb-no-export-and-exports-both ()
  (should
   (string-match
    "<<noweb-no-export-and-exports-both-1>>"
    (org-test-at-id "8a820f6c-7980-43db-8a24-0710d33729c9"
      (org-narrow-to-subtree)
      (org-test-with-expanded-babel-code
       (org-element-map (org-element-parse-buffer) 'src-block
	 (lambda (src-block) (org-element-property :value src-block))
	 nil t))))))

(ert-deftest ob-exp/evaluate-all-executables-in-order ()
  (should
   (equal '(5 4 3 2 1)
	  (let (*evaluation-collector*)
	    (org-test-at-id "96cc7073-97ec-4556-87cf-1f9bffafd317"
	      (org-narrow-to-subtree)
	      (buffer-string)
	      (fboundp 'org-export-execute-babel-code)
	      (org-test-with-expanded-babel-code *evaluation-collector*))))))

(ert-deftest ob-exp/exports-inline ()
  (should
   (string-match
    (regexp-quote "Here is one in the middle =1= of a line.
Here is one at the end of a line. =2=
=3= Here is one at the beginning of a line.")
    (org-test-at-id "54cb8dc3-298c-4883-a933-029b3c9d4b18"
      (org-narrow-to-subtree)
      (org-test-with-expanded-babel-code (buffer-string))))))

(ert-deftest ob-exp/export-call-line-information ()
  (org-test-at-id "bec63a04-491e-4caa-97f5-108f3020365c"
    (org-narrow-to-subtree)
    (let ((org-babel-exp-call-line-template "\n: call: %line special-token"))
      (org-test-with-expanded-babel-code
       (should (string-match "double" (buffer-string)))
       (should (string-match "16" (buffer-string)))
       (should (string-match "special-token" (buffer-string)))))))

(ert-deftest ob-exp/noweb-strip-export-ensure-strips ()
  (org-test-at-id "8e7bd234-99b2-4b14-8cd6-53945e409775"
    (org-narrow-to-subtree)
    (org-babel-next-src-block 2)
    (should (= 110 (org-babel-execute-src-block)))
    (let ((result (org-test-with-expanded-babel-code (buffer-string))))
      (should-not (string-match (regexp-quote "<<strip-export-1>>") result))
      (should-not (string-match (regexp-quote "i=\"10\"") result)))))

(ert-deftest ob-exp/export-from-a-temp-buffer ()
  :expected-result :failed
  (org-test-with-temp-text
      "
#+Title: exporting from a temporary buffer

#+name: foo
#+BEGIN_SRC emacs-lisp
  :foo
#+END_SRC

#+name: bar
#+BEGIN_SRC emacs-lisp
  :bar
#+END_SRC

#+BEGIN_SRC emacs-lisp :var foo=foo :noweb yes :exports results
  (list foo <<bar>>)
#+END_SRC
"
    (let* ((ascii (org-export-as 'ascii)))
      (should (string-match (regexp-quote (format nil "%S" '(:foo :bar)))
			    ascii)))))


(provide 'test-ob-exp)

;;; test-ob-exp.el ends here
