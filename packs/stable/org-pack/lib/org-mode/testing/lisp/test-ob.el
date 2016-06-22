;;; test-ob.el --- tests for ob.el

;; Copyright (c) 2010-2015 Eric Schulte
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

;;; Code:

(ert-deftest test-org-babel/indented-cached-org-bracket-link ()
  "When the result of a source block is a cached indented link it
should still return the link."
  (should
   (let ((default-directory temporary-file-directory))
     (org-test-with-temp-text
      "
* Test
  #+<point>BEGIN_SRC emacs-lisp :file test.txt :cache yes
    (message \"test\")
  #+END_SRC"
      ;; Execute twice as the first time creates the cache.
      (org-babel-execute-src-block)
      (string= (concat default-directory "test.txt")
	       (org-babel-execute-src-block))))))

(ert-deftest test-org-babel/multi-line-header-regexp ()
  (should(equal "^[ \t]*#\\+headers?:[ \t]*\\([^\n]*\\)$"
		org-babel-multi-line-header-regexp))
  ;;TODO can be optimised - and what about blah4 blah5 blah6?
  (should (string-match
	   org-babel-multi-line-header-regexp
	   "   \t #+headers: blah1 blah2 blah3 \t\n\t\n blah4 blah5 blah6 \n"))
  (should
   (equal
    "blah1 blah2 blah3 \t"
    (match-string
     1
     "   \t #+headers: blah1 blah2 blah3 \t\n\t\n blah4 blah5 blah6 \n")))

  ;;TODO Check - should this fail?
  (should
   (not (org-test-string-exact-match
	 org-babel-multi-line-header-regexp
	 "   \t #+headers : blah1 blah2 blah3 \t\n\t\n blah4 blah5 blah6 \n"))))

(ert-deftest test-org-babel/src-block-regexp ()
  (let ((test-block
	 (concat
	  "#+begin_src language -n-r-a-b -c :argument-1 yes :argument-2 no\n"
	  "echo this is a test\n"
	  "echo Currently in ' $PWD\n"
	  "#+end_src"))
	(language "language")
	(flags "-n-r-a-b -c ")
	(arguments ":argument-1 yes :argument-2 no")
	(body "echo this is a test\necho Currently in ' $PWD\n"))
    (should (string-match org-babel-src-block-regexp test-block))
    (should (string-match org-babel-src-block-regexp (upcase test-block)))
    (should (equal language (match-string 2 test-block)))
    ;;TODO Consider refactoring
    (should (equal flags (match-string 3 test-block)))
    (should (equal arguments (match-string 4 test-block)))
    (should (equal body (match-string 5 test-block)))
    ;;no switches
    (should (org-test-string-exact-match
     	     org-babel-src-block-regexp
     	     (replace-regexp-in-string flags "" test-block)))
    ;;no header arguments
    (should (org-test-string-exact-match
     	     org-babel-src-block-regexp
	     (replace-regexp-in-string arguments "" test-block)))
    ;; should be valid with no body
    (should (org-test-string-exact-match
	     org-babel-src-block-regexp
	     (replace-regexp-in-string body "" test-block)))))

(ert-deftest test-org-babel/get-header ()
  (should (not (org-babel-get-header
		org-babel-default-header-args :doesnt-exist)))
  (should(equal '((:session . "none"))
		(org-babel-get-header
		 org-babel-default-header-args :session)))
  (should(equal '((:session . "none"))
		(org-babel-get-header
		 org-babel-default-header-args :session nil)))
  (should (not (org-babel-get-header
		org-babel-default-header-args :SESSION)))
  (should (equal '((:tangle . "no"))
		 (org-babel-get-header
		  org-babel-default-header-args :tangle)))
  ;; with OTHERS option
  (should (equal org-babel-default-header-args
		 (org-babel-get-header
		  org-babel-default-header-args :doesnt-exist 'others)))
  (should (equal org-babel-default-header-args
		 (org-babel-get-header
		  org-babel-default-header-args nil 'others)))
  (should (null
	   (assoc :noweb
		  (org-babel-get-header
		   org-babel-default-header-args :noweb 'others)))))

(ert-deftest test-org-babel/default-inline-header-args ()
  (should(equal
	  '((:session . "none")
	    (:results . "replace")
	    (:exports . "results")
	    (:hlines  . "yes"))
	  org-babel-default-inline-header-args)))

(ert-deftest ob-test/org-babel-combine-header-arg-lists ()
  (let ((results (org-babel-combine-header-arg-lists
                  '((foo  . :any)
                    (bar)
                    (baz  . ((foo bar) (baz)))
                    (qux  . ((foo bar baz qux)))
                    (quux . ((foo bar))))
                  '((bar)
                    (baz  . ((baz)))
                    (quux . :any)))))
    (dolist (pair '((foo  . :any)
		    (bar)
		    (baz  . ((baz)))
		    (quux . :any)
		    (qux  . ((foo bar baz qux)))))
      (should (equal (cdr pair)
                     (cdr (assoc (car pair) results)))))))

;;; ob-get-src-block-info
(ert-deftest test-org-babel/get-src-block-info-language ()
  (org-test-at-marker nil org-test-file-ob-anchor
    (let ((info (org-babel-get-src-block-info)))
      (should (string= "emacs-lisp" (nth 0 info))))))

(ert-deftest test-org-babel/get-src-block-info-body ()
  (org-test-at-marker nil org-test-file-ob-anchor
    (let ((info (org-babel-get-src-block-info)))
      (should (string-match (regexp-quote org-test-file-ob-anchor)
			    (nth 1 info))))))

(ert-deftest test-org-babel/get-src-block-info-tangle ()
  (org-test-at-marker nil org-test-file-ob-anchor
    (let ((info (org-babel-get-src-block-info)))
      (should (string= "no" (cdr (assoc :tangle (nth 2 info))))))))

(ert-deftest test-org-babel/elisp-in-header-arguments ()
  "Test execution of elisp forms in header arguments."
  (org-test-with-temp-text-in-file "

* elisp forms in header arguments
  :PROPERTIES:
  :var:      prop = (* 7 6)
  :END:
#+begin_src emacs-lisp
  prop
#+end_src"
    (goto-char (point-min))
    (org-babel-next-src-block)
    (let ((info (org-babel-get-src-block-info)))
      (should (= 42 (org-babel-execute-src-block))))))

(ert-deftest test-org-babel/simple-named-code-block ()
  "Test that simple named code blocks can be evaluated."
  (org-test-with-temp-text-in-file "

#+name: i-have-a-name
#+begin_src emacs-lisp
  42
#+end_src"
    (org-babel-next-src-block 1)
    (should (= 42 (org-babel-execute-src-block)))))

(ert-deftest test-org-babel/simple-variable-resolution ()
  "Test that simple variable resolution is working."
  (org-test-with-temp-text-in-file "

#+name: four
#+begin_src emacs-lisp
  (list 1 2 3 4)
#+end_src

#+begin_src emacs-lisp :var four=four
  (length four)
#+end_src"

    (org-babel-next-src-block 2)
    (should (= 4 (org-babel-execute-src-block)))
    (forward-line 5)
    (should (string= ": 4" (buffer-substring
			    (point-at-bol)
			    (point-at-eol))))))

(ert-deftest test-org-babel/multi-line-header-arguments ()
  "Test that multi-line header arguments and can be read."
  (org-test-with-temp-text-in-file "

#+headers: :var letters='(a b c d e f g)
#+begin_src emacs-lisp :var numbers='(1 2 3 4 5 6 7)
  (require 'cl)
  (defalias 'my-map (if (org-version-check \"24.2.50\" \"cl\" :predicate)
                        'cl-map
                      'map))
  (my-map 'list #'list numbers letters)
#+end_src"

    (org-babel-next-src-block)
    (let ((results (org-babel-execute-src-block)))
      (should(equal 'a (cadr (assoc 1 results))))
      (should(equal 'd (cadr (assoc 4 results)))))))

(ert-deftest test-org-babel/parse-header-args ()
  (org-test-with-temp-text-in-file "

#+begin_src example-lang :session     :results output :var num=9
  the body
#+end_src"

    (org-babel-next-src-block)
    (let* ((info (org-babel-get-src-block-info))
	   (params (nth 2 info)))
      (message "%S" params)
      (should (equal "example-lang" (nth 0 info)))
      (should (string= "the body" (org-babel-trim (nth 1 info))))
      (should-not (member '(:session\ \ \ \ ) params))
      (should (equal '(:session) (assoc :session params)))
      (should (equal '(:result-type . output) (assoc :result-type params)))
      (should (equal '(num . 9) (cdr (assoc :var params)))))))

(ert-deftest test-org-babel/parse-header-args2 ()
  (org-test-with-temp-text-in-file "

* resolving sub-trees as references

#+begin_src emacs-lisp :var text=d4faa7b3-072b-4dcf-813c-dd7141c633f3
  (length text)
#+end_src

#+begin_src org :noweb yes
  <<simple-subtree>>
  <<d4faa7b3-072b-4dcf-813c-dd7141c633f3>>
#+end_src

** simple subtree with custom ID
   :PROPERTIES:
   :CUSTOM_ID: simple-subtree
   :END:
this is simple"

    (should (string-match (regexp-quote "this is simple")
			  (org-babel-ref-resolve "simple-subtree")))
    (org-babel-next-src-block)
    (should (= 14 (org-babel-execute-src-block)))))

(ert-deftest test-org-babel/inline-src-blocks ()
  (should
   (= 1
      (org-test-with-temp-text
	  "In the middle <point>src_emacs-lisp{(+ 0 1)} of a line"
	(org-babel-execute-src-block))))
  (should
   (= 2
      (org-test-with-temp-text
	  "One at the end of a line: <point>src_emacs-lisp{(+ 1 1)}"
	(org-babel-execute-src-block))))
  (should
   (= 3
      (org-test-with-temp-text
	  "src_emacs-lisp{(+ 2 1)} at the beginning of a line."
	(org-babel-execute-src-block))))
  (should
   (= 4
      (org-test-with-temp-text
	  "In the middle <point>src_emacs-lisp[:results silent\
 :exports code]{(+ 3 1)} of a line"
	(org-babel-execute-src-block))))
  (should
   (= 5
      (org-test-with-temp-text
	  "One at the end of a line: <point>src_emacs-lisp[:results silent\
 :exports code]{(+ 4 1)}"
	(org-babel-execute-src-block))))
  (should
   (= 6
      (org-test-with-temp-text
	  "src_emacs-lisp[:results silent :exports code]{(+ 5 1)}\
at the beginning of a line."
	(org-babel-execute-src-block))))
  (should
   (= 7
      (org-test-with-temp-text
	  "One also evaluated: <point>src_emacs-lisp[:exports both\
 :results silent]{(+ 6 1)}"
	(org-babel-execute-src-block)))))

(ert-deftest test-org-babel/org-babel-get-inline-src-block-matches ()
  (flet ((test-at-id (id)
	   (org-test-at-id
	    id
	    (let ((test-point (point)))
	      (should (fboundp 'org-babel-get-inline-src-block-matches))
	      (should (re-search-forward "src_" nil t)) ;; 1
	      (should (org-babel-get-inline-src-block-matches))
	      (should (re-search-forward " b" nil (point-at-bol))) ;; 1
	      (should-not (org-babel-get-inline-src-block-matches))
	      (should (re-search-forward "in" nil t)) ;; 2
	      (should-not (org-babel-get-inline-src-block-matches))
	      (should (re-search-forward "echo" nil t)) ;; 2
	      (should (org-babel-get-inline-src-block-matches))
	      (should (re-search-forward "blocks" nil t)) ;; 3
	      (backward-char 7) ;; 3
	      (should (org-babel-get-inline-src-block-matches))
	      (forward-char 1) ;;3
	      (should-not (org-babel-get-inline-src-block-matches))
	      (should (re-search-forward ":results" nil t)) ;; 4
	      (should (org-babel-get-inline-src-block-matches))
	      (end-of-line)
	      (should-not (org-babel-get-inline-src-block-matches))))))
    (test-at-id "0D0983D4-DE33-400A-8A05-A225A567BC74")
    (test-at-id "d55dada7-de0e-4340-8061-787cccbedee5")))

(ert-deftest test-org-babel/inline-src_blk-default-results-replace-line-1 ()
  (let ((test-line "src_sh{echo 1}")
	(org-babel-inline-result-wrap "=%s="))
    ;; src_ at bol line 1...
    (org-test-with-temp-text
	test-line
      (goto-char (point-min)) (org-babel-execute-maybe)
      (should (string=
                      (concat test-line " {{{results(=1=)}}}")
       	       (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
      (forward-char) (org-babel-execute-maybe)
      (should (string=
                      (concat test-line " {{{results(=1=)}}}")
       	       (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
      (re-search-forward "{{{")
     ;;(should-error (org-ctrl-c-ctrl-c))
      (backward-char 4) ;; last char of block body
      (org-babel-execute-maybe)
      (should (string=
                      (concat test-line " {{{results(=1=)}}}")
       	       (buffer-substring-no-properties (point-at-bol) (point-at-eol)))))
    ;; src_ follows space line 1...
    (let ((test-line " src_emacs-lisp{ 1 }"))
      (org-test-with-temp-text
	  test-line
	(should-error (org-ctrl-c-ctrl-c))
	(forward-char) (org-babel-execute-maybe)
	(should (string=
		 (concat test-line " {{{results(=1=)}}}")
		 (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
	(re-search-forward "{ 1 ") (org-babel-execute-maybe)
	(should (string=
		 (concat test-line " {{{results(=1=)}}}")
		 (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
	(forward-char 6)
	(should-error (org-ctrl-c-ctrl-c))
	))))

(ert-deftest test-org-babel/inline-src_blk-default-results-replace-line-2 ()
  ;; src_ at bol line 2...
  (let ((test-line " src_emacs-lisp{ \"x\" }")
	(org-babel-inline-result-wrap "=%s="))
    (org-test-with-temp-text
	(concat "\n" test-line)
      (should-error (org-ctrl-c-ctrl-c))
      (goto-char (point-min))
      (should-error (org-ctrl-c-ctrl-c))
      (forward-line)
      (should-error (org-ctrl-c-ctrl-c))
      (forward-char) (org-babel-execute-maybe)
      (should (string=
	       (concat test-line " {{{results(=x=)}}}")
	       (buffer-substring-no-properties
		(point-at-bol) (point-at-eol))))))
  (let ((test-line "Some text prior to block src_emacs-lisp{ \"y\" }")
	(org-babel-inline-result-wrap "=%s="))
    (org-test-with-temp-text
	test-line
      (goto-char (point-max))
      (insert (concat "\n" test-line " end"))
      (re-search-backward "src") (org-babel-execute-maybe)
      (should (string=
	       (concat test-line " {{{results(=y=)}}} end")
	       (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
      (re-search-forward "\" ") (org-babel-execute-maybe)
      (should (string=
	       (concat test-line " {{{results(=y=)}}} end")
	       (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
      (forward-char 3)
      (should-error (org-ctrl-c-ctrl-c)))))

(ert-deftest test-org-babel/inline-src_blk-manual-results-replace ()
  (let ((test-line " src_emacs-lisp[:results replace]{ \"x\" }")
	(org-babel-inline-result-wrap "=%s="))
    (org-test-with-temp-text
	(concat "\n" test-line)
      (should-error (org-ctrl-c-ctrl-c))
      (goto-char (point-max))
      (org-babel-execute-maybe)
      (beginning-of-line)
      (should-error (org-ctrl-c-ctrl-c))
      (forward-char) (org-babel-execute-maybe)
      (should (string=
              (concat test-line " {{{results(=x=)}}}")
      	       (buffer-substring-no-properties
		(point-at-bol) (point-at-eol))))))
  (let ((test-line (concat " Some text prior to block "
			   "src_emacs-lisp[:results replace]{ \"y\" }"))
	(org-babel-inline-result-wrap "=%s="))
    (org-test-with-temp-text test-line
      (goto-char (point-max))
      (insert (concat "\n" test-line " end"))
      (re-search-backward "src") (org-babel-execute-maybe)
      (should (string=
              (concat test-line " {{{results(=y=)}}} end")
    	       (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
      (re-search-forward "\" ") (org-babel-execute-maybe)
      (should (string=
              (concat test-line " {{{results(=y=)}}} end")
    	       (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
      (forward-char 3)
      (should-error (org-ctrl-c-ctrl-c)))))

(ert-deftest test-org-babel/inline-src_blk-results-silent ()
  (let ((test-line "src_emacs-lisp[ :results silent ]{ \"x\" }"))
    (org-test-with-temp-text test-line
      (org-babel-execute-maybe)
      (should (string= test-line
		       (buffer-substring-no-properties
			(point-at-bol) (point-at-eol))))))
  (let ((test-line (concat " Some text prior to block src_emacs-lisp"
			   "[ :results silent ]{ \"y\" }")))
    (org-test-with-temp-text
	test-line
      (goto-char (point-max))
      (insert (concat "\n" test-line " end"))
      (re-search-backward "src_") (org-babel-execute-maybe)
      (should (string= (concat test-line " end")
		       (buffer-substring-no-properties
			(point-at-bol) (point-at-eol))))
      (re-search-forward "\" ") (org-babel-execute-maybe)
      (should (string= (concat test-line " end")
		       (buffer-substring-no-properties
			(point-at-bol) (point-at-eol))))
      (forward-char 2)
      (should-error (org-ctrl-c-ctrl-c)))))

(ert-deftest test-org-babel/inline-src_blk-results-raw ()
  (let ((test-line "src_emacs-lisp[ :results raw ]{ \"x\" }"))
    (org-test-with-temp-text test-line
      (org-babel-execute-maybe)
      (should (string= (concat test-line " x")
		       (buffer-string)))))
  (let ((test-line (concat " Some text prior to block "
			   "src_emacs-lisp[ :results raw ]{ \"the\" }")))
    (org-test-with-temp-text (concat test-line " end")
      (re-search-forward "src_") (org-babel-execute-maybe)
      (should (string= (concat test-line " the end")
		       (buffer-substring-no-properties
			(point-at-bol) (point-at-eol))))
      (re-search-forward "\" ") (org-babel-execute-maybe)
      (should (string= (concat test-line " the the end")
		       (buffer-substring-no-properties
			(point-at-bol) (point-at-eol))))
      (forward-char 2)
      (should-error (org-ctrl-c-ctrl-c)))))

(ert-deftest test-org-babel/inline-src_blk-results-file ()
  (let ((test-line "src_emacs-lisp[ :results file ]{ \"~/test-file\"  }"))
    (org-test-with-temp-text
	test-line
      (org-babel-execute-maybe)
      (should (string= (concat test-line " {{{results([[file:~/test-file]])}}}")
		       (buffer-substring-no-properties
			(point-min) (point-max)))))))

(ert-deftest test-org-babel/inline-src_blk-results-scalar ()
  (let ((test-line "src_emacs-lisp[ :results scalar ]{ \"x\"  }")
	(org-babel-inline-result-wrap "=%s="))
    (org-test-with-temp-text
	test-line
      (org-babel-execute-maybe)
      (should (string= (concat test-line  " {{{results(=\"x\"=)}}}")
		       (buffer-substring-no-properties
			(point-min) (point-max)))))))

(ert-deftest test-org-babel/inline-src_blk-results-verbatim ()
  (let ((test-line "src_emacs-lisp[ :results verbatim ]{ \"x\"  }")
	(org-babel-inline-result-wrap "=%s="))
    (org-test-with-temp-text
	test-line
      (org-babel-execute-maybe)
      (should (string= (concat test-line " {{{results(=\"x\"=)}}}")
		       (buffer-substring-no-properties
			(point-min) (point-max)))))))

(ert-deftest test-org-babel/combining-scalar-and-raw-result-types ()
  (org-test-with-temp-text-in-file "

#+begin_src sh :results scalar
echo \"[[file:./cv.cls]]\"
#+end_src

#+name:
: [[file:./cv.cls]]

#+begin_src sh :results raw scalar
   echo \"[[file:./cv.cls]]\"
#+end_src
"
    (flet ((next-result ()
			(org-babel-next-src-block)
			(org-babel-execute-src-block)
			(goto-char (org-babel-where-is-src-block-result))
			(forward-line 1)))
      (goto-char (point-min))
      (next-result)
      (should (org-babel-in-example-or-verbatim))
      (next-result)
      (should (not (org-babel-in-example-or-verbatim))))))

(ert-deftest test-org-babel/no-defaut-value-for-var ()
  "Test that the absence of a default value for a variable DOES THROW
  a proper error."
  (org-test-at-id "f2df5ba6-75fa-4e6b-8441-65ed84963627"
    (org-babel-next-src-block)
    (let ((err
	   (should-error (org-babel-execute-src-block) :type 'error)))
      (should
       (equal
	'(error
	  "Variable \"x\" must be assigned a default value")
	err)))))

(ert-deftest test-org-babel/just-one-results-block ()
  "Test that evaluating two times the same code block does not result in a
duplicate results block."
  (org-test-with-temp-text "#+begin_src sh\necho Hello\n#+end_src\n"
    (org-babel-execute-src-block)
    (org-babel-execute-src-block)     ; second code block execution
    (should (search-forward "Hello")) ; the string inside the source code block
    (should (search-forward "Hello")) ; the same string in the results block
    (should-error (search-forward "Hello"))))

(ert-deftest test-org-babel/nested-code-block ()
  "Test nested code blocks inside code blocks don't cause problems."
  (should
   (string= "#+begin_src emacs-lisp\n  'foo\n#+end_src"
	    (org-test-with-temp-text "#+begin_src org :results silent
  ,#+begin_src emacs-lisp
  ,  'foo
  ,#+end_src
#+end_src"
	      (let ((org-edit-src-content-indentation 2)
		    (org-src-preserve-indentation nil))
		(org-babel-execute-src-block))))))

(ert-deftest test-org-babel/partial-nested-code-block ()
  "Test nested code blocks inside code blocks don't cause problems."
  (org-test-with-temp-text "#+begin_src org :results silent
  ,#+begin_src emacs-lisp
#+end_src"
    (should (string= "#+begin_src emacs-lisp" (org-babel-execute-src-block)))))

(ert-deftest test-ob/does-not-replace-a-block-with-the-results ()
  (org-test-with-temp-text "#+NAME: foo
#+BEGIN_SRC emacs-lisp
 'foo
#+END_SRC\n"
    (org-babel-next-src-block 1)
    (should (eq 'foo (org-babel-execute-src-block)))
    (goto-char (point-min))
    (org-babel-next-src-block 1)
    (should (looking-at org-babel-src-block-regexp))))

(ert-deftest test-ob/catches-all-references ()
  (org-test-with-temp-text "
#+NAME: literal-example
#+BEGIN_EXAMPLE
A literal example
on two lines
#+END_EXAMPLE

#+NAME: read-literal-example
#+BEGIN_SRC emacs-lisp :var x=literal-example
  (concatenate 'string x \" for me.\")
#+END_SRC"
    (org-babel-next-src-block 1)
    (should (string= (org-babel-execute-src-block)
		     "A literal example\non two lines\n for me."))))

(ert-deftest test-ob/resolve-code-blocks-before-data-blocks ()
  (org-test-with-temp-text "
#+name: foo
: bar

#+name: foo
#+begin_src emacs-lisp
  \"baz\"
#+end_src

#+begin_src emacs-lisp :var foo=foo
  foo
#+end_src"
    (org-babel-next-src-block 2)
    (should (string= (org-babel-execute-src-block) "baz"))))

(ert-deftest test-ob/do-not-resolve-to-partial-names-data ()
  (org-test-with-temp-text "
#+tblname: base_plus
| 1 |
| 2 |

#+tblname: base
| 3 |
| 4 |

#+begin_src emacs-lisp :var x=base
  x
#+end_src"
    (org-babel-next-src-block 1)
    (should (equal (org-babel-execute-src-block) '((3) (4))))))

(ert-deftest test-ob/do-not-resolve-to-partial-names-code ()
  (org-test-with-temp-text "
#+name: base_plus
#+begin_src emacs-lisp
  'bar
#+end_src

#+name: base
#+begin_src emacs-lisp
  'foo
#+end_src

#+begin_src emacs-lisp :var x=base
  x
#+end_src"
    (org-babel-next-src-block 3)
    (should (equal (org-babel-execute-src-block) "foo"))))

(ert-deftest test-ob/allow-spaces-around-=-in-var-specs ()
  (org-test-with-temp-text "#+begin_src emacs-lisp :var a = 1 b = 2 c= 3 d =4
  (+ a b c d)
#+end_src
"
    (should (= 10 (org-babel-execute-src-block)))))

(ert-deftest test-ob/org-babel-update-intermediate ()
  (org-test-with-temp-text "#+name: foo
#+begin_src emacs-lisp
  2
#+end_src

#+results: foo
: 4

#+begin_src emacs-lisp :var it=foo
  (+ it 1)
#+end_src"
    (let ((org-babel-update-intermediate nil))
      (goto-char (point-min))
      (org-babel-next-src-block 2)
      (should (= 3 (org-babel-execute-src-block)))
      (goto-char (point-min))
      (forward-line 6)
      (should (looking-at ": 4")))
    (let ((org-babel-update-intermediate t))
      (goto-char (point-min))
      (org-babel-next-src-block 2)
      (should (= 3 (org-babel-execute-src-block)))
      (goto-char (point-min))
      (forward-line 6)
      (should (looking-at ": 2")))))

(ert-deftest test-ob/eval-header-argument ()
  (flet ((check-eval (eval runp)
		     (org-test-with-temp-text (format "#+begin_src emacs-lisp :eval %s
  (setq foo :evald)
#+end_src" eval)
		       (let ((foo :not-run))
			 (if runp
			     (progn (should (org-babel-execute-src-block))
				    (should (eq foo :evald)))
			   (progn (should-not (org-babel-execute-src-block))
				  (should-not (eq foo :evald))))))))
    (check-eval "never" nil)
    (check-eval "no" nil)
    (check-eval "never-export" t)
    (check-eval "no-export" t)
    (let ((org-babel-exp-reference-buffer (current-buffer)))
      (check-eval "never" nil)
      (check-eval "no" nil)
      (check-eval "never-export" nil)
      (check-eval "no-export" nil))))

(ert-deftest test-ob/noweb-expansion-1 ()
  (org-test-with-temp-text "#+begin_src sh :results output :tangle yes
  <<foo>>
#+end_src

#+name: foo
#+begin_src sh
  bar
#+end_src"
    (should (string= (org-babel-expand-noweb-references) "bar"))))

(ert-deftest test-ob/noweb-expansion-2 ()
  (org-test-with-temp-text "#+begin_src sh :results output :tangle yes
  <<foo>>
#+end_src

#+name: foo
#+begin_src sh :noweb-sep \"\"
  bar
#+end_src

#+begin_src sh :noweb-ref foo :noweb-sep \"\"
  baz
#+end_src"
    (should (string= (org-babel-expand-noweb-references) "barbaz"))))

(ert-deftest test-ob/splitting-variable-lists-in-references ()
  (org-test-with-temp-text ""
    (should (= 1 (length (org-babel-ref-split-args
			  "a=\"this, no work\""))))
    (should (= 2 (length (org-babel-ref-split-args
			  "a=\"this, no work\", b=1"))))))

(ert-deftest test-ob/org-babel-balanced-split ()
  (should (equal
	   '(":a 1" "b [2 3]" "c (4 :d (5 6))")
	   (org-babel-balanced-split ":a 1 :b [2 3] :c (4 :d (5 6))"
				     '((32 9) . 58)))))

(ert-deftest test-ob/commented-last-block-line-no-var ()
  (org-test-with-temp-text-in-file "
#+begin_src emacs-lisp
;;
#+end_src"
    (org-babel-next-src-block)
    (org-babel-execute-maybe)
    (should (re-search-forward "\\#\\+results:" nil t))
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
    (should (re-search-forward "\\#\\+results:" nil t))
    (forward-line)
    (should
     (string=
      ": some text"
      (buffer-substring-no-properties (point-at-bol) (point-at-eol))))))

(ert-deftest test-ob/commented-last-block-line-with-var ()
  (org-test-with-temp-text-in-file "
#+begin_src emacs-lisp :var a=1
;;
#+end_src"
    (org-babel-next-src-block)
    (org-babel-execute-maybe)
    (re-search-forward "\\#\\+results:" nil t)
    (forward-line)
    (should (string=
	     ""
	     (buffer-substring-no-properties (point-at-bol) (point-at-eol)))))
  (org-test-with-temp-text-in-file "
#+begin_src emacs-lisp :var a=2
2;;
#+end_src"
    (org-babel-next-src-block)
    (org-babel-execute-maybe)
    (re-search-forward "\\#\\+results:" nil t)
    (forward-line)
    (should (string=
	     ": 2"
	     (buffer-substring-no-properties (point-at-bol) (point-at-eol))))))

(ert-deftest test-ob/org-babel-insert-result--improper-lists ()
  "Test `org-babel-insert-result' with improper lists."
  ;; Do not error when output is an improper list.
  (should
   (org-test-with-temp-text
       "
<point>#+BEGIN_SRC emacs-lisp
'((1 . nil) (2 . 3))
#+END_SRC
"
     (org-babel-execute-maybe) t)))

(ert-deftest test-ob/remove-inline-result ()
  "Test `org-babel-remove-inline-result' honors whitespace."
  (let*
      ((inline-sb "src_emacs-lisp{(+ 1 2)}")
       (inline-res " {{{results(=3=)}}}")
       (inline-sb-dot (concat inline-sb "."))
       (inline-sb-res-dot (concat inline-sb inline-res ".")))
    (org-test-with-temp-text
	;; Insert inline_src_block followed by dot.
	inline-sb-dot
      ;; Insert result before dot.
      (org-babel-execute-maybe)
      (should (string= inline-sb-res-dot
		       (buffer-substring-no-properties
			(point-at-bol) (point-at-eol))))
      ;; Delete whitespace and result.
      (org-babel-remove-inline-result)
      (should (string= inline-sb-dot
		       (buffer-substring-no-properties
			(point-at-bol) (point-at-eol))))
      ;; Add whitespace and result before dot.
      (search-forward inline-sb)
      (insert "     " inline-res)
      (goto-char (point-at-bol))
      ;; Remove whitespace and result.
      (org-babel-remove-inline-result)
      (should (string= inline-sb-dot
		       (buffer-substring-no-properties
			(point-at-bol) (point-at-eol))))
      ;; Add whitespace before dot.
      (search-forward inline-sb)
      (insert "     ")
      (goto-char (point-at-bol))
      ;; Add result before whitespace.
      (org-babel-execute-maybe)
      ;; Remove result - leave trailing whitespace and dot.
      (org-babel-remove-inline-result)
      (should (string= (concat inline-sb "     .")
		       (buffer-substring-no-properties
			(point-at-bol) (point-at-eol)))))))

(defun test-ob-verify-result-and-removed-result (result buffer-text)
  "Test helper function to test `org-babel-remove-result'.
A temp buffer is populated with BUFFER-TEXT, the first block is executed,
and the result of execution is verified against RESULT.

The block is actually executed /twice/ to ensure result
replacement happens correctly."
  (org-test-with-temp-text
      buffer-text
    (org-babel-next-src-block) (org-babel-execute-maybe) (org-babel-execute-maybe)
    (should (re-search-forward "\\#\\+results:" nil t))
    (forward-line)
    (should (string= result
		     (buffer-substring-no-properties
		      (point-at-bol)
		      (- (point-max) 16))))
    (org-babel-previous-src-block) (org-babel-remove-result)
    (should (string= buffer-text
		     (buffer-substring-no-properties
		      (point-min) (point-max))))))

(ert-deftest test-ob/org-babel-remove-result--results-default ()
  "Test `org-babel-remove-result' with default :results."
  (mapcar (lambda (language)
	    (test-ob-verify-result-and-removed-result
	     "\n"
	     (concat
	      "* org-babel-remove-result
#+begin_src " language "
#+end_src

* next heading")))
	  '("sh" "emacs-lisp")))

(ert-deftest test-ob/org-babel-remove-result--results-list ()
  "Test `org-babel-remove-result' with :results list."
  (test-ob-verify-result-and-removed-result
   "- 1
- 2
- 3
- (quote (4 5))"

   "* org-babel-remove-result
#+begin_src emacs-lisp :results list
'(1 2 3 '(4 5))
#+end_src

* next heading"))

(ert-deftest test-ob/org-babel-results-indented-wrap ()
  "Ensure that wrapped results are inserted correction when indented.
If not inserted correctly then the second evaluation will fail
trying to find the :END: marker."
  (org-test-with-temp-text
      "- indented
  #+begin_src sh :results file wrap
    echo test.txt
  #+end_src"
    (org-babel-next-src-block 1)
    (org-babel-execute-src-block)
    (org-babel-execute-src-block)))

(ert-deftest test-ob/file-desc-header-argument ()
  "Test that the :file-desc header argument is used."
  (org-test-with-temp-text "#+begin_src emacs-lisp :results file :file-desc bar
  \"foo\"
#+end_src

#+begin_src emacs-lisp :results file :file-desc
  \"foo\"
#+end_src"
    (org-babel-execute-src-block)
    (org-babel-next-src-block 1)
    (org-babel-execute-src-block)
    (goto-char (point-min))
    (should (search-forward "[[file:foo][bar]]" nil t))
    (should (search-forward "[[file:foo][foo]]" nil t))))

(ert-deftest test-ob/org-babel-remove-result--results-pp ()
  "Test `org-babel-remove-result' with :results pp."
  (test-ob-verify-result-and-removed-result
   ": \"I /am/ working!\""

   "* org-babel-remove-result
#+begin_src emacs-lisp :results pp
\"I /am/ working!\")
#+end_src

* next heading"))

(ert-deftest test-org-babel/inline-src_blk-preceded-punct-preceded-by-point ()
  (let ((test-line ".src_emacs-lisp[ :results verbatim ]{ \"x\"  }")
	(org-babel-inline-result-wrap "=%s="))
    (org-test-with-temp-text
	test-line
      (forward-char 1)
      (org-babel-execute-maybe)
      (should (re-search-forward "=\"x\"=" nil t))
      (forward-line))))

(ert-deftest test-ob/commented-last-block-line-with-var ()
  (org-test-with-temp-text-in-file "
#+begin_src emacs-lisp :var a=1
;;
#+end_src"
    (org-babel-next-src-block)
    (org-babel-execute-maybe)
    (re-search-forward "\\#\\+results:" nil t)
    (forward-line)
    (should (string=
	     ""
	     (buffer-substring-no-properties (point-at-bol) (point-at-eol)))))
  (org-test-with-temp-text-in-file "
#+begin_src emacs-lisp :var a=2
2;;
#+end_src"
    (org-babel-next-src-block)
    (org-babel-execute-maybe)
    (re-search-forward "\\#\\+results:" nil t)
    (forward-line)
    (should (string=
	     ": 2"
	     (buffer-substring-no-properties (point-at-bol) (point-at-eol))))))

(defun test-ob-verify-result-and-removed-result (result buffer-text)
  "Test helper function to test `org-babel-remove-result'.
A temp buffer is populated with BUFFER-TEXT, the first block is executed,
and the result of execution is verified against RESULT.

The block is actually executed /twice/ to ensure result
replacement happens correctly."
  (org-test-with-temp-text
      buffer-text
    (org-babel-next-src-block) (org-babel-execute-maybe) (org-babel-execute-maybe)
    (should (re-search-forward "\\#\\+results:" nil t))
    (forward-line)
    (should (string= result
		     (buffer-substring-no-properties
		      (point-at-bol)
		      (- (point-max) 16))))
    (org-babel-previous-src-block) (org-babel-remove-result)
    (should (string= buffer-text
		     (buffer-substring-no-properties
		      (point-min) (point-max))))))

(ert-deftest test-ob/org-babel-remove-result--results-default ()
  "Test `org-babel-remove-result' with default :results."
  (mapcar (lambda (language)
	    (test-ob-verify-result-and-removed-result
	     "\n"
	     (concat
	      "* org-babel-remove-result
#+begin_src " language "
#+end_src

* next heading")))
	  '("sh" "emacs-lisp")))

(ert-deftest test-ob/org-babel-remove-result--results-list ()
  "Test `org-babel-remove-result' with :results list."
  (test-ob-verify-result-and-removed-result
   "- 1
- 2
- 3
- (quote (4 5))"

   "* org-babel-remove-result
#+begin_src emacs-lisp :results list
'(1 2 3 '(4 5))
#+end_src

* next heading"))

(ert-deftest test-ob/org-babel-remove-result--results-wrap ()
  "Test `org-babel-remove-result' with :results wrap."
  (test-ob-verify-result-and-removed-result
   ":RESULTS:
hello there
:END:"

   "* org-babel-remove-result

#+begin_src emacs-lisp :results wrap
\"hello there\"
#+end_src

* next heading"))

(ert-deftest test-ob/org-babel-remove-result--results-org ()
  "Test `org-babel-remove-result' with :results org."
  (test-ob-verify-result-and-removed-result
   "#+BEGIN_SRC org
,* heading
,** subheading
content
#+END_SRC"

   "* org-babel-remove-result
#+begin_src emacs-lisp :results org
\"* heading
,** subheading
content\"
#+end_src

* next heading"))

(ert-deftest test-ob/org-babel-remove-result--results-html ()
  "Test `org-babel-remove-result' with :results html."
  (test-ob-verify-result-and-removed-result
   "#+BEGIN_HTML
<head><body></body></head>
#+END_HTML"

   "* org-babel-remove-result
#+begin_src emacs-lisp :results html
\"<head><body></body></head>\"
#+end_src

* next heading"))

(ert-deftest test-ob/org-babel-remove-result--results-latex ()
  "Test `org-babel-remove-result' with :results latex."
  (test-ob-verify-result-and-removed-result
   "#+BEGIN_LaTeX
Line 1
Line 2
Line 3
#+END_LaTeX"

   "* org-babel-remove-result
#+begin_src emacs-lisp :results latex
\"Line 1
Line 2
Line 3\"
#+end_src

* next heading"))

(ert-deftest test-ob/org-babel-remove-result--results-code ()
  "Test `org-babel-remove-result' with :results code."

  (test-ob-verify-result-and-removed-result
   "#+BEGIN_SRC emacs-lisp
\"I am working!\"
#+END_SRC"

   "* org-babel-remove-result
#+begin_src emacs-lisp :results code
(message \"I am working!\")
#+end_src

* next heading"))

(ert-deftest test-ob/org-babel-remove-result--results-pp ()
  "Test `org-babel-remove-result' with :results pp."
  (test-ob-verify-result-and-removed-result
   ": \"I /am/ working!\""

   "* org-babel-remove-result
#+begin_src emacs-lisp :results pp
\"I /am/ working!\")
#+end_src

* next heading"))

(ert-deftest test-ob/results-do-not-replace-code-blocks ()
  (org-test-with-temp-text "Block two has a space after the name.

  #+name: foo
  #+begin_src emacs-lisp
    1
  #+end_src

#+name: foo
#+begin_src emacs-lisp
  2
#+end_src

#+name: foo
#+begin_src emacs-lisp
  3
#+end_src

#+RESULTS: foo
: foo
"
    (dolist (num '(1 2 3))
      ;; execute the block
      (goto-char (point-min))
      (org-babel-next-src-block num) (org-babel-execute-src-block)
      ;; check the results
      (goto-char (point-max))
      (move-beginning-of-line 0)
      (should (looking-at (format ": %d" num))))))

(ert-deftest test-ob/blocks-with-spaces ()
  "Test expansion of blocks followed by blank lines."
  (should
   (equal "#+BEGIN_SRC emacs-lisp
\(+ 1 2)
#+END_SRC

#+RESULTS:
: 3\n\n\n"
	  (org-test-with-temp-text "#+BEGIN_SRC emacs-lisp
\(+ 1 2)
#+END_SRC\n\n\n"
	    (let ((org-babel-next-src-block "RESULTS"))
	      (org-babel-execute-src-block))
	    (buffer-string)))))

(ert-deftest test-ob/results-in-narrowed-buffer ()
  "Test block execution in a narrowed buffer."
  ;; If results don't exist, they should be inserted in visible part
  ;; of the buffer.
  (should
   (equal
    "#+BEGIN_SRC emacs-lisp\n(+ 1 2)\n#+END_SRC\n\n#+RESULTS:\n: 3"
    (org-test-with-temp-text
	"#+BEGIN_SRC emacs-lisp\n(+ 1 2)\n#+END_SRC\n\nParagraph"
      (narrow-to-region (point) (save-excursion (forward-line 3) (point)))
      (let ((org-babel-results-keyword "RESULTS"))
	(org-babel-execute-src-block))
      (org-trim (buffer-string)))))
  (should
   (equal
    "#+NAME: test\n#+BEGIN_SRC emacs-lisp\n(+ 1 2)\n#+END_SRC\n\n#+RESULTS: test\n: 3"
    (org-test-with-temp-text
	"#+NAME: test\n#+BEGIN_SRC emacs-lisp\n(+ 1 2)\n#+END_SRC\n\nParagraph"
      (narrow-to-region (point) (save-excursion (forward-line 4) (point)))
      (let ((org-babel-results-keyword "RESULTS"))
	(org-babel-execute-src-block))
      (org-trim (buffer-string)))))
  ;; Results in visible part of buffer, should be updated here.
  (should
   (equal
    "#+NAME: test
#+BEGIN_SRC emacs-lisp
\(+ 1 2)
#+END_SRC

#+RESULTS: test
: 3"
    (org-test-with-temp-text
	"#+NAME: test
#+BEGIN_SRC emacs-lisp
\(+ 1 2)
#+END_SRC

#+RESULTS: test
: 4

Paragraph"
      (narrow-to-region (point) (save-excursion (forward-line 7) (point)))
      (let ((org-babel-results-keyword "RESULTS"))
	(org-babel-execute-src-block))
      (org-trim (buffer-string)))))
  ;; Results in invisible part of buffer, should be updated there.
  (org-test-with-temp-text
      "#+NAME: test
#+BEGIN_SRC emacs-lisp
\(+ 1 2)
#+END_SRC

#+RESULTS: test
: 4

Paragraph"
    (narrow-to-region (point) (save-excursion (forward-line 4) (point)))
    (let ((org-babel-results-keyword "RESULTS"))
      (org-babel-execute-src-block))
    (should-not (re-search-forward "^#\\+RESULTS:" nil t))
    (widen)
    (should (should (re-search-forward "^: 3" nil t)))))

(ert-deftest test-ob/specific-colnames ()
  "Test passing specific column names."
  (should
   (equal "#+name: input-table
| id | var1 |
|----+------|
|  1 | bar  |
|  2 | baz  |

#+begin_src sh :var data=input-table :exports results :colnames '(Rev Author)
echo \"$data\"
#+end_src

#+RESULTS:
| Rev | Author |
|-----+--------|
|   1 | bar    |
|   2 | baz    |

"
	  (org-test-with-temp-text
	      "#+name: input-table
| id | var1 |
|----+------|
|  1 | bar  |
|  2 | baz  |

#+begin_src sh :var data=input-table :exports results :colnames '(Rev Author)
echo \"$data\"
#+end_src
"
	    ;; we should find a code block
	    (should (re-search-forward org-babel-src-block-regexp nil t))
	    (goto-char (match-beginning 0))
	    ;; now that we've located the code block, it may be evaluated
	    (let ((org-babel-execute-src-block "RESULTS"))
	      (org-babel-execute-src-block))
	    (buffer-string)))))

(ert-deftest test-ob/location-of-header-arg-eval ()
  "Test location of header argument evaluation."
  (org-test-with-temp-text "
#+name: top-block
#+begin_src emacs-lisp :var pt=(point)
  pt
#+end_src

#+name: bottom-block
#+begin_src emacs-lisp :var pt=top-block()
  pt
#+end_src
"
    ;; the value of the second block should be greater than the first
    (should
     (< (progn (re-search-forward org-babel-src-block-regexp nil t)
	       (goto-char (match-beginning 0))
	       (prog1 (save-match-data (org-babel-execute-src-block))
		 (goto-char (match-end 0))))
	(progn (re-search-forward org-babel-src-block-regexp nil t)
	       (goto-char (match-beginning 0))
	       (org-babel-execute-src-block))))))

(ert-deftest test-ob/preserve-results-indentation ()
  "Preserve indentation when executing a src block."
  (should
   (equal
    '(2 2)
    (org-test-with-temp-text "  #+begin_src emacs-lisp\n(+ 1 1)\n  #+end_src"
      (org-babel-execute-src-block)
      (let ((case-fold-search t)) (search-forward "RESULTS"))
      (list (org-get-indentation)
	    (progn (forward-line) (org-get-indentation))))))
  (should
   (equal
    '(2 2)
    (org-test-with-temp-text
	"  #+name: block\n  #+begin_src emacs-lisp\n(+ 1 1)\n  #+end_src"
      (org-babel-execute-src-block)
      (let ((case-fold-search t)) (search-forward "RESULTS"))
      (list (org-get-indentation)
	    (progn (forward-line) (org-get-indentation))))))
  ;; Don't get fooled by TAB-based indentation.
  (should
   (equal
    '(6 6)
    (org-test-with-temp-text
	"\t  #+begin_src emacs-lisp\n\t  (+ 1 1)\n\t  #+end_src"
      (setq tab-width 4)
      (org-babel-execute-src-block)
      (let ((case-fold-search t)) (search-forward "RESULTS"))
      (list (org-get-indentation)
	    (progn (forward-line) (org-get-indentation)))))))

(ert-deftest test-ob/safe-header-args ()
  "Detect safe and unsafe header args."
  (let ((safe-args '((:cache . "foo")
		     (:results . "output")
		     (:eval . "never")
		     (:eval . "query")))
	(unsafe-args '((:eval . "yes")
		       (:results . "output file")
		       (:foo . "bar")))
	(malformed-args '((bar . "foo")
			  ("foo" . "bar")
			  :foo))
	(safe-p (org-babel-header-args-safe-fn org-babel-safe-header-args)))
    (dolist (arg safe-args)
      (should (org-babel-one-header-arg-safe-p arg org-babel-safe-header-args)))
    (dolist (arg unsafe-args)
      (should (not (org-babel-one-header-arg-safe-p arg org-babel-safe-header-args))))
    (dolist (arg malformed-args)
      (should (not (org-babel-one-header-arg-safe-p arg org-babel-safe-header-args))))
    (should (not (funcall safe-p (append safe-args unsafe-args))))))

(ert-deftest test-ob/noweb-expansions-in-cache ()
  "Ensure that noweb expansions are expanded before caching."
  (let ((noweb-expansions-in-cache-var 0))
    (org-test-with-temp-text "
#+name: foo
#+begin_src emacs-lisp
  \"I said\"
#+end_src

#+name: bar
#+begin_src emacs-lisp :noweb yes :cache yes
  (setq noweb-expansions-in-cache-var
        (+ 1 noweb-expansions-in-cache-var))
  (concat <<foo>> \" check noweb expansions\")
#+end_src
"
      ;; run the second block to create the cache
      (goto-char (point-min))
      (re-search-forward (regexp-quote "#+name: bar"))
      (should (string= "I said check noweb expansions"
		       (org-babel-execute-src-block)))
      (should (= noweb-expansions-in-cache-var 1))
      ;; change the value of the first block
      (goto-char (point-min))
      (re-search-forward (regexp-quote "said"))
      (goto-char (match-beginning 0))
      (insert "haven't ")
      (re-search-forward (regexp-quote "#+name: bar"))
      (should (string= "I haven't said check noweb expansions"
		       (org-babel-execute-src-block)))
      (should (= noweb-expansions-in-cache-var 2)))))

(ert-deftest test-org-babel/file-ext-and-output-dir ()
  (org-test-at-id "93573e1d-6486-442e-b6d0-3fedbdc37c9b"
    (org-babel-next-src-block)
    (should (equal  "file-ext-basic.txt"
		   (cdr (assq :file (nth 2 (org-babel-get-src-block-info t))))))
    (org-babel-next-src-block)
    (should (equal "foo/file-ext-dir-relative.txt"
		   (cdr (assq :file (nth 2 (org-babel-get-src-block-info t))))))
    (org-babel-next-src-block)
    (should (equal  "foo/file-ext-dir-relative-slash.txt"
		   (cdr (assq :file (nth 2 (org-babel-get-src-block-info t))))))
    (org-babel-next-src-block)
    (should (equal  "/tmp/file-ext-dir-absolute.txt"
		   (cdr (assq :file (nth 2 (org-babel-get-src-block-info t))))))
    (org-babel-next-src-block)
    (should (equal  "foo.bar"
		   (cdr (assq :file (nth 2 (org-babel-get-src-block-info t))))))
    (org-babel-next-src-block)
    (should (equal "xxx/foo.bar"
		   (cdr (assq :file (nth 2 (org-babel-get-src-block-info t))))))
    ))

(ert-deftest test-org-babel/script-escape ()
  ;; Delimited lists of numbers
  (should (equal '(1 2 3)
		 (org-babel-script-escape "[1 2 3]")))
  (should (equal '(1 2 3)
		 (org-babel-script-escape "{1 2 3}")))
  (should (equal '(1 2 3)
		 (org-babel-script-escape "(1 2 3)")))
  ;; Delimited lists of double-quoted strings
  (should (equal '("foo" "bar")
		 (org-babel-script-escape "(\"foo\" \"bar\")")))
  (should (equal '("foo" "bar")
		 (org-babel-script-escape "[\"foo\" \"bar\"]")))
  (should (equal '("foo" "bar")
		 (org-babel-script-escape "{\"foo\" \"bar\"}")))
  ;; ... with commas
  (should (equal '("foo" "bar")
		 (org-babel-script-escape "(\"foo\", \"bar\")")))
  (should (equal '("foo" "bar")
		 (org-babel-script-escape "[\"foo\", \"bar\"]")))
  (should (equal '("foo" "bar")
		 (org-babel-script-escape "{\"foo\", \"bar\"}")))
  ;; Delimited lists of single-quoted strings
  (should (equal '("foo" "bar")
		 (org-babel-script-escape "('foo' 'bar')")))
  (should (equal '("foo" "bar")
		 (org-babel-script-escape "['foo' 'bar']")))
  (should (equal '("foo" "bar")
		 (org-babel-script-escape "{'foo' 'bar'}")))
  ;; ... with commas
  (should (equal '("foo" "bar")
		 (org-babel-script-escape "('foo', 'bar')")))
  (should (equal '("foo" "bar")
		 (org-babel-script-escape "['foo', 'bar']")))
  (should (equal '("foo" "bar")
		 (org-babel-script-escape "{'foo', 'bar'}")))
  ;; Single quoted strings
  (should (equal "foo"
		 (org-babel-script-escape "'foo'")))
  ;; ... with internal double quote
  (should (equal "foo\"bar"
		 (org-babel-script-escape "'foo\"bar'")))
  ;; ... with internal backslash
  (should (equal "foo\\bar"
		 (org-babel-script-escape "'foo\\bar'")))
  ;; ... with internal escaped backslash
  (should (equal "foo\\bar"
		 (org-babel-script-escape "'foo\\\\bar'")))
  ;; ... with internal backslash-double quote
  (should (equal "foo\\\"bar"
		 (org-babel-script-escape "'foo\\\"bar'")))
  ;; ... with internal escaped backslash-double quote
  (should (equal "foo\\\"bar"
		 (org-babel-script-escape "'foo\\\\\"bar'")))
  ;; ... with internal escaped single quote
  (should (equal "foo'bar"
		 (org-babel-script-escape "'foo\\'bar'")))
  ;; ... with internal escaped backslash-escaped single quote
  (should (equal "foo\\'bar"
		 (org-babel-script-escape "'foo\\\\\\'bar'")))
  ;; Double quoted strings
  (should (equal "foo"
		 (org-babel-script-escape "\"foo\"")))
  ;; ... with internal single quote
  (should (equal "foo'bar"
		 (org-babel-script-escape "\"foo'bar\"")))
  ;; ... with internal backslash
  (should (equal "foo\\bar"
		 (org-babel-script-escape "\"foo\\bar\"")))
  ;; ... with internal escaped backslash
  (should (equal "foo\\bar"
		 (org-babel-script-escape "\"foo\\\\bar\"")))
  ;; ... with internal backslash-single quote
  (should (equal "foo\\'bar"
		 (org-babel-script-escape "\"foo\\'bar\"")))
  ;; ... with internal escaped backslash-single quote
  (should (equal "foo\\'bar"
		 (org-babel-script-escape "\"foo\\\\'bar\"")))
  ;; ... with internal escaped double quote
  (should (equal "foo\"bar"
		 (org-babel-script-escape "\"foo\\\"bar\"")))
  ;; ... with internal escaped backslash-escaped double quote
  (should (equal "foo\\\"bar"
		 (org-babel-script-escape "\"foo\\\\\\\"bar\""))))

(ert-deftest ob/process-params-no-duplicates ()
    (should (equal (org-babel-process-params '((:colname-names . 1)
                                               (:rowname-names . 1)
                                               (:result-params . 1)
                                               (:result-type . 1)
                                               (:var . "\"foo\"")))
                   '((:var)
		     (:colname-names . 1)
		     (:rowname-names . 1)
		     (:result-params . 1)
		     (:result-type . value)))))

(defun org-test-ob/update-block-body ()
  "Test `org-babel-update-block-body' specifications."
  (should
   (equal "#+begin_src elisp\n  2\n#+end_src"
	  (let ((org-edit-src-content-indentation 2))
	    (org-test-with-temp-text "#+begin_src elisp\n(+ 1 1)\n#+end_src"
	      (org-babel-update-block-body "2")
	      (buffer-string)))))
  ;; Preserve block indentation.
  (should
   (equal "  #+begin_src elisp\n   2\n  #+end_src"
	  (let ((org-edit-src-content-indentation 1))
	    (org-test-with-temp-text
		"  #+begin_src elisp\n  (+ 1 1)\n  #+end_src"
	      (org-babel-update-block-body "2")
	      (buffer-string)))))
  ;; Ignore NEW-BODY global indentation.
  (should
   (equal "#+begin_src elisp\n  2\n#+end_src"
	  (let ((org-edit-src-content-indentation 2))
	    (org-test-with-temp-text "#+begin_src elisp\n(+ 1 1)\n#+end_src"
	      (org-babel-update-block-body "      2")
	      (buffer-string)))))
  ;; When indentation should be preserved ignore the two rules above.
  (should
   (equal "  #+begin_src elisp\n2\n  #+end_src"
	  (let ((org-edit-src-content-indentation 1)
		(org-src-preserve-indentation t))
	    (org-test-with-temp-text
		"  #+begin_src elisp\n  (+ 1 1)\n  #+end_src"
	      (org-babel-update-block-body "2")
	      (buffer-string)))))
  (should
   (equal "  #+begin_src elisp -i\n2\n  #+end_src"
	  (let ((org-edit-src-content-indentation 1))
	    (org-test-with-temp-text
		"  #+begin_src elisp -i\n  (+ 1 1)\n  #+end_src"
	      (org-babel-update-block-body "2")
	      (buffer-string)))))
  (should
   (equal "#+begin_src elisp\n      2\n#+end_src"
	  (let ((org-edit-src-content-indentation 2)
		(org-src-preserve-indentation t))
	    (org-test-with-temp-text "#+begin_src elisp\n(+ 1 1)\n#+end_src"
	      (org-babel-update-block-body "      2")
	      (buffer-string)))))
  (should
   (equal "#+begin_src elisp -i\n      2\n#+end_src"
	  (let ((org-edit-src-content-indentation 2)
		(org-src-preserve-indentation t))
	    (org-test-with-temp-text "#+begin_src elisp -i\n(+ 1 1)\n#+end_src"
	      (org-babel-update-block-body "      2")
	      (buffer-string))))))

(provide 'test-ob)

;;; test-ob ends here
