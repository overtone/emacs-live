;;; test-ob-exp.el

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Comments:

;; Template test file for Org tests

;;; Code:

(defmacro org-test-with-expanded-babel-code (&rest body)
  "Execute BODY while in a buffer with all Babel code evaluated.
Current buffer is a copy of the original buffer."
  `(let ((string (org-with-wide-buffer (buffer-string)))
	 (narrowing (list (point-min) (point-max)))
	 (org-export-use-babel t))
     (with-temp-buffer
       (org-mode)
       (insert string)
       (apply #'narrow-to-region narrowing)
       (org-babel-exp-process-buffer)
       (goto-char (point-min))
       (progn ,@body))))

(ert-deftest test-ob-exp/org-babel-exp-src-blocks/w-no-headers ()
  "Testing export without any headlines in the Org mode file."
  (require 'ox-ascii)
  (let ((text-file (concat (file-name-sans-extension org-test-no-heading-file)
			   ".txt")))
    (when (file-exists-p text-file) (delete-file text-file))
    (org-test-in-example-file org-test-no-heading-file
      ;; Export the file to HTML.
      (org-export-to-file 'ascii text-file))
    ;; should create a ".txt" file
    (should (file-exists-p text-file))
    ;; should not create a file with "::" appended to its name
    (should-not (file-exists-p (concat org-test-no-heading-file "::")))
    (when (file-exists-p text-file) (delete-file text-file))))

(ert-deftest test-ob-exp/org-babel-exp-src-blocks/w-no-file ()
  "Testing export from buffers which are not visiting any file."
  (require 'ox-ascii)
  (let ((name (generate-new-buffer-name "*Org ASCII Export*")))
    (org-test-in-example-file nil
      (org-export-to-buffer 'ascii name nil nil nil t))
    ;; Should create a new buffer.
    (should (buffer-live-p (get-buffer name)))
    ;; Should contain the content of the buffer.
    (with-current-buffer (get-buffer name)
      (should (string-match (regexp-quote org-test-file-ob-anchor)
			    (buffer-string))))
    (when (get-buffer name) (kill-buffer name))))

(ert-deftest test-ob-exp/org-babel-exp-src-blocks/w-no-headers2 ()
  "Testing export without any headlines in the Org file."
  (let ((html-file (concat (file-name-sans-extension
			    org-test-link-in-heading-file)
			   ".html")))
    (when (file-exists-p html-file) (delete-file html-file))
    (org-test-in-example-file org-test-link-in-heading-file
      ;; export the file to html
      (org-export-to-file 'html html-file))
    ;; should create a .html file
    (should (file-exists-p html-file))
    ;; should not create a file with "::" appended to its name
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
  (message \"expanded1\")" ";; noweb-no-start
  <<noweb-example1>>" ";; noweb-2-yes-start
  (message \"expanded2\")"
  ";; noweb-tangle-start
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
	  (let ((org-export-use-babel t) *evaluation-collector*)
	    (org-test-at-id "96cc7073-97ec-4556-87cf-1f9bffafd317"
	      (org-narrow-to-subtree)
	      (buffer-string)
	      (org-test-with-expanded-babel-code *evaluation-collector*))))))

(ert-deftest ob-exp/exports-inline ()
  (should
   (string-match
    (regexp-quote "Here is one in the middle {{{results(=1=)}}} of a line.
Here is one at the end of a line. {{{results(=2=)}}}
{{{results(=3=)}}} Here is one at the beginning of a line.")
    (org-test-at-id "54cb8dc3-298c-4883-a933-029b3c9d4b18"
      (org-narrow-to-subtree)
      (let ((org-babel-inline-result-wrap "=%s="))
	(org-test-with-expanded-babel-code (buffer-string)))))))

(ert-deftest ob-exp/exports-inline-code ()
  (should
   (equal "src_emacs-lisp[]{(+ 1 1)}"
	  (org-test-with-temp-text "src_emacs-lisp[:exports code]{(+ 1 1)}"
	    (let ((org-babel-inline-result-wrap "=%s=")
		  (org-export-use-babel t))
	      (org-babel-exp-process-buffer))
	    (buffer-string))))
  (should
   (equal "src_emacs-lisp[]{(+ 1 1)}"
	  (org-test-with-temp-text "src_emacs-lisp[ :exports code ]{(+ 1 1)}"
	    (let ((org-babel-inline-result-wrap "=%s=")
		  (org-export-use-babel t))
	      (org-babel-exp-process-buffer))
	    (buffer-string))))
  (should
   (equal "src_emacs-lisp[]{(+ 1 1)} {{{results(=2=)}}}"
	  (org-test-with-temp-text "src_emacs-lisp[:exports both]{(+ 1 1)}"
	    (let ((org-babel-inline-result-wrap "=%s=")
		  (org-export-use-babel t))
	      (org-babel-exp-process-buffer))
	    (buffer-string))))
  (should
   (equal "{{{results(=2=)}}}"
	  (org-test-with-temp-text
	      "src_emacs-lisp[:exports results :results scalar]{(+ 1 1)}"
	    (let ((org-babel-inline-result-wrap "=%s=")
		  (org-export-use-babel t))
	      (org-babel-exp-process-buffer))
	    (buffer-string))))
  (should
   (equal "foosrc_emacs-lisp[:exports code]{(+ 1 1)}"
	  (org-test-with-temp-text
	      "foosrc_emacs-lisp[:exports code]{(+ 1 1)}"
	    (let ((org-babel-inline-result-wrap "=%s=")
		  (org-export-use-babel t))
	      (org-babel-exp-process-buffer))
	    (buffer-string))))
  (should
   (let ((text "src_emacs lisp{(+ 1 1)}"))
     (string-match (regexp-quote text)
		   (org-test-with-temp-text
		       text
		     (let ((org-babel-inline-result-wrap "=%s=")
			   (org-export-use-babel t))
		       (org-babel-exp-process-buffer))
		     (buffer-string)))))
  (should
   (string-match
    (replace-regexp-in-string
     "\\\\\\[]{" "\\(?:\\[]\\)?{" ;accept both src_sh[]{...} or src_sh{...}
     (regexp-quote "Here is one in the middle src_sh[]{echo 1} of a line.
Here is one at the end of a line. src_sh[]{echo 2}
src_sh[]{echo 3} Here is one at the beginning of a line.
Here is one that is also evaluated: src_sh[]{echo 4} {{{results(=4=)}}}")
     nil t)
    (org-test-at-id "cd54fc88-1b6b-45b6-8511-4d8fa7fc8076"
      (org-narrow-to-subtree)
      (let ((org-babel-inline-result-wrap "=%s=")
	    (org-export-use-babel t))
	(org-test-with-expanded-babel-code (buffer-string)))))))

(ert-deftest ob-exp/exports-inline-code-double-eval ()
  "Based on default header arguments for inline code blocks (:exports
results), the resulting code block `src_emacs-lisp{2}' should also be
evaluated."
  (let ((org-babel-inline-result-wrap "=%s=")
	(org-export-use-babel t))
    (should
     (string-match "\\`{{{results(src_emacs-lisp\\[\\]{2})}}}$"
		   (org-test-with-temp-text
		       "src_emacs-lisp[:exports results :results code]{(+ 1 1)}"
		     (org-babel-exp-process-buffer)
		     (buffer-string))))))

(ert-deftest ob-exp/exports-inline-code-eval-code-once ()
  "Ibid above, except that the resulting inline code block should not
be evaluated."
  (let ((org-export-use-babel t))
    (should
     (string-match "{{{results(src_emacs-lisp\\(?:\\[[: a-zA-Z]+]\\)?{2})}}}$"
		   (org-test-with-temp-text
		       (concat "src_emacs-lisp[:exports results :results code "
			       ":results_switches \":exports code\"]{(+ 1 1)}")
		     (org-babel-exp-process-buffer)
		     (buffer-string))))))

(ert-deftest ob-exp/exports-inline-code-double-eval-exports-both ()
  (let ((org-export-use-babel t))
    (should
     (string-match (concat "\\`src_emacs-lisp\\(?:\\[]\\)?{(\\+ 1 1)} "
			   "{{{results(src_emacs-lisp\\[ :exports code\\]{2})}}}$")
		   (org-test-with-temp-text
		       (concat "src_emacs-lisp[:exports both :results code "
			       ":results_switches \":exports code\"]{(+ 1 1)}")
		     (org-babel-exp-process-buffer)
		     (buffer-string))))))

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

(ert-deftest ob-exp/use-case-of-reading-entry-properties ()
  (org-test-at-id "cc5fbc20-bca5-437a-a7b8-2b4d7a03f820"
    (org-narrow-to-subtree)
    (let* ((case-fold-search nil)
	   (result (org-test-with-expanded-babel-code (buffer-string)))
	   (sect "a:1, b:0, c:3, d:0, e:0")
	   (sub0 "a:1, b:2, c:4, d:0, e:0")
	   (sub1 "a:1, b:2, c:5, d:0, e:6")
	   (func sub0))
      ;; entry "section"
      (should (string-match (concat "_shell-sect-call\n: shell " sect "\n")
			    result))
      (should (string-match (concat "_elisp-sect-call\n: elisp " sect "\n")
			    result))
      (should (string-match (concat "\n- sect inline shell " sect "\n")
			    result))
      (should (string-match (concat "\n- sect inline elisp " sect "\n")
			    result))
            ;; entry "subsection", call without arguments
      (should (string-match (concat "_shell-sub0-call\n: shell " sub0 "\n")
			    result))
      (should (string-match (concat "_elisp-sub0-call\n: elisp " sub0 "\n")
			    result))
      (should (string-match (concat "\n- sub0 inline shell " sub0 "\n")
			    result))
      (should (string-match (concat "\n- sub0 inline elisp " sub0 "\n")
			    result))
      ;; entry "subsection", call with arguments
      (should (string-match (concat "_shell-sub1-call\n: shell " sub1 "\n")
			    result))
      (should (string-match (concat "_elisp-sub1-call\n: elisp " sub1 "\n")
			    result))
      (should (string-match (concat "\n- sub1 inline shell " sub1 "\n")
			    result))
      (should (string-match (concat "\n- sub1 inline elisp " sub1 "\n")
			    result))
      ;; entry "function definition"
      (should (string-match (concat "_location_shell\n: shell " func "\n")
			    result))
      (should (string-match (concat "_location_elisp\n: elisp " func "\n")
			    result)))))

(ert-deftest ob-exp/export-from-a-temp-buffer ()
  (let ((org-export-use-babel t))
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
	(should (string-match
		 (regexp-quote " :foo  :bar \n")
		 ascii))))))

(ert-deftest ob-export/export-with-results-before-block ()
  "Test export when results are inserted before source block."
  (let ((org-export-use-babel t))
    (should
     (equal
      "#+RESULTS: src1
: 2

#+NAME: src1
#+BEGIN_SRC emacs-lisp
\(+ 1 1)
#+END_SRC"
      (org-test-with-temp-text
	  "#+RESULTS: src1

#+NAME: src1
#+BEGIN_SRC emacs-lisp :exports both
\(+ 1 1)
#+END_SRC"
	(org-babel-exp-process-buffer)
	(org-trim (org-no-properties (buffer-string))))))))

(ert-deftest ob-export/export-src-block-with-switches ()
  "Test exporting a source block with switches."
  (should
   (string-match
    "\\`#\\+BEGIN_SRC emacs-lisp -n -r$"
    (org-test-with-temp-text
	"#+BEGIN_SRC emacs-lisp -n -r\n\(+ 1 1)\n#+END_SRC"
      (org-babel-exp-process-buffer)
      (buffer-string)))))

(ert-deftest ob-export/export-src-block-with-flags ()
  "Test exporting a source block with a flag."
  (should
   (string-match
    "\\`#\\+BEGIN_SRC emacs-lisp -some-flag$"
    (org-test-with-temp-text
	"#+BEGIN_SRC emacs-lisp :flags -some-flag\n\(+ 1 1)\n#+END_SRC"
      (org-babel-exp-process-buffer)
      (buffer-string)))))

(ert-deftest ob-export/export-and-indentation ()
  "Test indentation of evaluated source blocks during export."
  ;; No indentation.
  (should
   (string-match
    "^t"
    (org-test-with-temp-text "#+BEGIN_SRC emacs-lisp\n t\n#+END_SRC"
      (let ((indent-tabs-mode t)
	    (tab-width 1)
	    (org-src-preserve-indentation nil))
	(org-babel-exp-process-buffer)
	(buffer-string)))))
  ;; Preserve indentation with "-i" flag.
  (should
   (string-match
    "^ t"
    (org-test-with-temp-text "#+BEGIN_SRC emacs-lisp -i\n t\n#+END_SRC"
      (let ((indent-tabs-mode t)
	    (tab-width 1))
	(org-babel-exp-process-buffer)
	(buffer-string)))))
  ;; Preserve indentation with a non-nil
  ;; `org-src-preserve-indentation'.
  (should
   (string-match
    "^ t"
    (org-test-with-temp-text "#+BEGIN_SRC emacs-lisp\n t\n#+END_SRC"
      (let ((indent-tabs-mode t)
	    (tab-width 1)
	    (org-src-preserve-indentation t))
	(org-babel-exp-process-buffer)
	(buffer-string))))))

(ert-deftest ob-export/export-under-commented-headline ()
  "Test evaluation of code blocks under COMMENT headings."
  (let ((org-export-use-babel t)
	(org-babel-inline-result-wrap "=%s="))
    ;; Do not eval block in a commented headline.
    (should
     (string-match
      ": 2"
      (org-test-with-temp-text "* Headline
#+BEGIN_SRC emacs-lisp :exports results
\(+ 1 1)
#+END_SRC"
	(org-babel-exp-process-buffer)
	(buffer-string))))
    (should-not
     (string-match
      ": 2"
      (org-test-with-temp-text "* COMMENT Headline
#+BEGIN_SRC emacs-lisp :exports results
\(+ 1 1)
#+END_SRC"
	(org-babel-exp-process-buffer)
	(buffer-string))))
    ;; Do not eval inline blocks either.
    (should
     (string-match
      "=2="
      (org-test-with-temp-text "* Headline
src_emacs-lisp{(+ 1 1)}"
	(org-babel-exp-process-buffer)
	(buffer-string))))
    (should-not
     (string-match
      "=2="
      (org-test-with-temp-text "* COMMENT Headline
src_emacs-lisp{(+ 1 1)}"
	(org-babel-exp-process-buffer)
	(buffer-string))))
    ;; Also check parent headlines.
    (should-not
     (string-match
      ": 2"
      (org-test-with-temp-text "
* COMMENT Headline
** Children
#+BEGIN_SRC emacs-lisp :exports results
\(+ 1 1)
#+END_SRC"
	(org-babel-exp-process-buffer)
	(buffer-string))))))

(ert-deftest ob-export/reference-in-post-header ()
  "Test references in :post header during export."
  (should
   (org-test-with-temp-text "
#+NAME: foo
#+BEGIN_SRC emacs-lisp :exports none :var bar=\"baz\"
  (concat \"bar\" bar)
#+END_SRC

#+NAME: nofun
#+BEGIN_SRC emacs-lisp :exports results :post foo(\"nofun\")
#+END_SRC"
     (org-babel-exp-process-buffer) t)))

(ert-deftest ob-export/babel-evaluate ()
  "Test `org-export-use-babel' effect."
  ;; When nil, no Babel code is executed.
  (should-not
   (string-match-p
    "2"
    (org-test-with-temp-text
	"#+BEGIN_SRC emacs-lisp :exports results\n(+ 1 1)\n#+END_SRC"
      (let ((org-export-use-babel nil)) (org-babel-exp-process-buffer))
      (buffer-string))))
  (should-not
   (string-match-p
    "2"
    (org-test-with-temp-text
	"src_emacs-lisp{(+ 1 1)}"
      (let ((org-export-use-babel nil)) (org-babel-exp-process-buffer))
      (buffer-string))))
  ;; When non-nil, all Babel code types are executed.
  (should
   (string-match-p
    "2"
    (org-test-with-temp-text
	"#+BEGIN_SRC emacs-lisp :exports results\n(+ 1 1)\n#+END_SRC"
      (let ((org-export-use-babel t)) (org-babel-exp-process-buffer))
      (buffer-string))))
  (should
   (string-match-p
    "2"
    (org-test-with-temp-text
	"src_emacs-lisp{(+ 1 1)}"
      (let ((org-export-use-babel t)) (org-babel-exp-process-buffer))
      (buffer-string)))))

(ert-deftest ob-export/body-with-coderef ()
  "Test exporting a code block with coderefs."
  (should
   (equal "#+BEGIN_SRC emacs-lisp\n0 (ref:foo)\n#+END_SRC"
	  (org-test-with-temp-text
	      "#+BEGIN_SRC emacs-lisp :exports code\n0 (ref:foo)\n#+END_SRC"
	    (let ((org-export-use-babel t)
		  (org-coderef-label-format "(ref:foo)"))
	      (org-babel-exp-process-buffer))
	    (buffer-string))))
  (should
   (equal
    "#+BEGIN_SRC emacs-lisp -l \"r:%s\"\n1 r:foo\n#+END_SRC"
    (org-test-with-temp-text
	"#+BEGIN_SRC emacs-lisp -l \"r:%s\" -lisp :exports code\n1 r:foo\n#+END_SRC"
      (let ((org-export-use-babel t))
	(org-babel-exp-process-buffer))
      (buffer-string)))))

(ert-deftest ob-exp/src-block-with-affiliated-keyword ()
  "Test exporting a code block with affiliated keywords."
  ;; Pathological case: affiliated keyword matches inline source block
  ;; syntax.
  (should
   (equal "#+name: call_foo\n#+BEGIN_SRC emacs-lisp\n42\n#+END_SRC"
	  (org-test-with-temp-text
	      "#+name: call_foo\n#+BEGIN_SRC emacs-lisp\n42\n#+END_SRC"
	    (let ((org-export-use-babel t))
	      (org-babel-exp-process-buffer))
	    (buffer-string)))))


(provide 'test-ob-exp)

;;; test-ob-exp.el ends here
