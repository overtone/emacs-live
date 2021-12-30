;;; test-ob-lob.el --- test for ob-lob.el

;; Copyright (c) 2010-2015, 2019 Eric Schulte
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

(eval-and-compile (require 'cl-lib))


;;; Tests
(org-babel-lob-ingest
 (expand-file-name
  "library-of-babel.org"
  (expand-file-name
   "doc"
    (expand-file-name
     ".."
     (expand-file-name
      ".."
      (file-name-directory
       (or load-file-name buffer-file-name)))))))

(ert-deftest test-ob-lob/ingest ()
  "Test the ingestion of an Org file."
  (should (< 0 (org-babel-lob-ingest
		(expand-file-name "babel.org" org-test-example-dir)))))

(ert-deftest test-ob-lob/call-with-header-arguments ()
  "Test the evaluation of a library of babel #+call: line."
  (cl-letf (((symbol-function 'org-babel-insert-result)
	  (symbol-function 'ignore)))
    (let ((org-babel-library-of-babel
	   (org-test-with-temp-text-in-file
	       "
#+name: echo
#+begin_src emacs-lisp :var input=\"echo'd\"
  input
#+end_src

#+name: lob-minus
#+begin_src emacs-lisp :var a=0 :var b=0
  (- a b)
#+end_src"
	     (org-babel-lob-ingest)
	     org-babel-library-of-babel)))
      (org-test-at-id "fab7e291-fde6-45fc-bf6e-a485b8bca2f0"
	(move-beginning-of-line 1)
	(forward-line 6)
	(message (buffer-substring (point-at-bol) (point-at-eol)))
	(should
	 (string= "testing" (org-babel-execute-src-block
			     nil (org-babel-lob-get-info))))
	(forward-line 1)
	(should
	 (string= "testing" (caar (org-babel-execute-src-block
				   nil (org-babel-lob-get-info)))))
	(forward-line 1)
	(should
	 (string= "testing" (org-babel-execute-src-block
			     nil (org-babel-lob-get-info))))
	(forward-line 1)
	(should
	 (string= "testing" (caar (org-babel-execute-src-block
				   nil (org-babel-lob-get-info)))))
	(forward-line 1)
	(should
	 (string= "testing" (org-babel-execute-src-block
			     nil (org-babel-lob-get-info))))
	(forward-line 1)
	(should
	 (string= "testing" (caar (org-babel-execute-src-block
				   nil (org-babel-lob-get-info)))))
	(forward-line 1) (beginning-of-line) (forward-char 27)
	(should
	 (string= "testing" (org-babel-execute-src-block
			     nil (org-babel-lob-get-info))))
	(forward-line 1) (beginning-of-line) (forward-char 27)
	(should
	 (string= "testing" (caar (org-babel-execute-src-block
				   nil (org-babel-lob-get-info)))))
	(forward-line 1) (beginning-of-line)
	(should
	 (= 4 (org-babel-execute-src-block nil (org-babel-lob-get-info))))
	(forward-line 1)
	(should
	 (string= "testing" (org-babel-execute-src-block
			     nil (org-babel-lob-get-info))))
	(forward-line 1)
	(should (string= "123" (org-babel-execute-src-block
				nil (org-babel-lob-get-info))))))))

(ert-deftest test-ob-lob/export-lob-lines ()
  "Test the export of a variety of library babel call lines."
  (let ((org-babel-inline-result-wrap "=%s=")
	(org-export-use-babel t))
    (org-test-at-id "72ddeed3-2d17-4c7f-8192-a575d535d3fc"
      (org-narrow-to-subtree)
      (let ((string (org-with-wide-buffer (buffer-string)))
	    (narrowing (list (point-min) (point-max))))
	(with-temp-buffer
	  (org-mode)
	  (insert string)
	  (apply #'narrow-to-region narrowing)
	  (org-babel-exp-process-buffer)
	  (message (buffer-string))
	  (goto-char (point-min))
	  (should (re-search-forward "^: 0" nil t))
	  (should (re-search-forward "call {{{results(=2=)}}} stuck" nil t))
	  (should (re-search-forward
		   "exported =call_double(it=2)= because" nil t))
	  (should (re-search-forward "^{{{results(=6=)}}} because" nil t))
	  (should (re-search-forward "results 8 should" nil t))
	  (should (re-search-forward "following 2\\*5={{{results(=10=)}}} should" nil t)))))))

(ert-deftest test-ob-lob/do-not-eval-lob-lines-in-example-blocks-on-export ()
  (require 'ox)
  (org-test-with-temp-text-in-file "
for export
#+begin_example
#+call: rubbish()
#+end_example"
    (should (progn (org-babel-exp-process-buffer) t))))

(ert-deftest test-ob-lob/caching-call-line ()
  (let ((temporary-value-for-test 0))
    (org-test-with-temp-text "
#+name: call-line-caching-example
#+begin_src emacs-lisp :var bar=\"baz\"
  (setq temporary-value-for-test (+ 1 temporary-value-for-test))
#+end_src

<point>#+call: call-line-caching-example(\"qux\") :cache yes
"
      ;; first execution should flip value to t
      (should
       (eq (org-babel-execute-src-block nil (org-babel-lob-get-info)) 1))
      ;; if cached, second evaluation will retain the t value
      (should
       (eq (org-babel-execute-src-block nil (org-babel-lob-get-info)) 1)))))

(ert-deftest test-ob-lob/named-caching-call-line ()
  (let ((temporary-value-for-test 0))
    (org-test-with-temp-text "
#+name: call-line-caching-example
#+begin_src emacs-lisp :var bar=\"baz\"
  (setq temporary-value-for-test (+ 1 temporary-value-for-test))
#+end_src

#+name: call-line-caching-called
<point>#+call: call-line-caching-example(\"qux\") :cache yes
"
      ;; first execution should flip value to t
      (should
       (eq (org-babel-execute-src-block nil (org-babel-lob-get-info)) 1))
      ;; if cached, second evaluation will retain the t value
      (should
       (eq (org-babel-execute-src-block nil (org-babel-lob-get-info)) 1)))))

(ert-deftest test-ob-lob/assignment-with-newline ()
  "Test call lines with an argument containing a newline character."
  (should
   (equal " foo"
	  (org-test-with-temp-text "
#+name: test-newline
#+begin_src emacs-lisp :var x=\"a\"
'foo
#+end_src

call_test-newline[:eval yes :results raw](\"a\nb\")<point>"
	    (org-babel-execute-src-block nil (org-babel-lob-get-info))
	    (buffer-substring (point) (point-max)))))
  (should
   (equal " bar"
	  (org-test-with-temp-text "
#+name: test-newline
#+begin_src emacs-lisp :var x=\"a\"
'bar
#+end_src

call_test-newline[:eval yes :results raw]('(1\n2))<point>"
	    (org-babel-execute-src-block nil (org-babel-lob-get-info))
	    (buffer-substring (point) (point-max))))))

(ert-deftest test-ob-lob/external-reference-syntax ()
  "Test external reference syntax for Babel calls."
  (should
   (= 2
      (org-test-with-temp-text-in-file
	  "#+name: foo\n#+begin_src emacs-lisp\n(+ 1 1)\n#+end_src"
	(let ((file (buffer-file-name)))
	  (org-test-with-temp-text (format "#+call: %s:foo()" file)
	    (org-babel-execute-src-block nil (org-babel-lob-get-info))))))))

(ert-deftest test-ob-lob/call-with-indirection ()
  "Test calling code with indirection."
  (should
   (= 2
      (org-test-with-temp-text
	  "
#+name: foo
#+begin_src emacs-lisp
\(+ 1 1)
#+end_src

#+name: bar
#+call: foo()

<point>#+call: bar()"
	(org-babel-execute-src-block nil (org-babel-lob-get-info)))))
  (should
   (= 10
      (org-test-with-temp-text
	  "
#+name: foo
#+begin_src emacs-lisp :var x=1
\(* 2 x)
#+end_src

#+name: bar
#+call: foo(x=3)

<point>#+call: bar(x=5)"
	(org-babel-execute-src-block nil (org-babel-lob-get-info)))))
  (should
   (= 6
      (org-test-with-temp-text
	  "
#+name: foo
#+begin_src emacs-lisp :var x=1
\(* 2 x)
#+end_src

#+name: bar
#+call: foo(x=3)

<point>#+call: bar()"
	(org-babel-execute-src-block nil (org-babel-lob-get-info))))))

(ert-deftest test-ob-lob/confirm-evaluate ()
  "Test confirmation when exporting lob calls."
  ;; With the default `org-confirm-babel-evaluate' of t, the caller is
  ;; queried one time.
  (should
   (= 1
      (let ((org-export-use-babel t)
	    (org-confirm-babel-evaluate t)
	    (confirm-evaluate-calls 0))
	(cl-letf (((symbol-function 'yes-or-no-p)
		   (lambda (&rest _ignore)
		     (cl-incf confirm-evaluate-calls)
		     t)))
	  (org-test-with-temp-text
	      "
#+name: foo
#+begin_src emacs-lisp
  nil
#+end_src

#+call: foo()"
	    (let ((string (buffer-string)))
	      (with-temp-buffer
		(org-mode)
		(insert string)
		(org-babel-exp-process-buffer)
		confirm-evaluate-calls))))))))

(provide 'test-ob-lob)

;;; test-ob-lob.el ends here
