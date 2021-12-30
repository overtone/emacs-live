;;; test-org-src.el --- tests for org-src.el

;; Copyright (C) 2012-2015, 2019  Le Wang

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'org-test "../testing/org-test")



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
      (should (looking-at-p "(message hello)")))))

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

(ert-deftest test-org-src/undo ()
  "Undo-ing an edit buffer should not go back to empty state."
  (org-test-with-temp-text "
#+begin_src emacs-lisp<point>
  (message hello)
#+end_src
"
    (org-edit-special)
    (should-error (undo))
    (org-edit-src-exit)))

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

(ert-deftest test-org-src/preserve-tabs ()
  "Editing block preserve tab characters."
  ;; With `org-src-preserve-indentation' set to nil.
  (should
   (equal "
#+begin_src emacs-lisp
  This is a tab:\t.
#+end_src"
          (org-test-with-temp-text
              "
#+begin_src emacs-lisp
<point>This is a tab:\t.
#+end_src"
            (let ((org-edit-src-content-indentation 2)
                  (org-src-preserve-indentation nil))
              (org-edit-special)
              (org-edit-src-exit)
              (buffer-string)))))
  ;; With `org-src-preserve-indentation' set to t.
  (should
   (equal "
#+begin_src emacs-lisp
This is a tab:\t.
#+end_src"
          (org-test-with-temp-text
              "
#+begin_src emacs-lisp
<point>This is a tab:\t.
#+end_src"
            (let ((org-edit-src-content-indentation 2)
                  (org-src-preserve-indentation t))
              (org-edit-special)
              (org-edit-src-exit)
              (buffer-string))))))

(ert-deftest test-org-src/coderef-format ()
  "Test `org-src-coderef-format' specifications."
  ;; Regular tests in a src block, an example block and an edit
  ;; buffer.
  (should
   (equal "foo"
          (let ((org-coderef-label-format "foo"))
            (org-test-with-temp-text "#+BEGIN_SRC emacs-lisp\n0\n#+END_SRC"
              (org-src-coderef-format)))))
  (should
   (equal "foo"
          (let ((org-coderef-label-format "foo"))
            (org-test-with-temp-text "#+BEGIN_EXAMPLE\n0\n#+END_EXAMPLE"
              (org-src-coderef-format)))))
  (should
   (equal "foo"
          (let ((org-coderef-label-format "foo") result)
            (org-test-with-temp-text "#+BEGIN_SRC emacs-lisp\n0\n#+END_SRC"
              (org-edit-special)
              (setq result (org-src-coderef-format))
              (org-edit-src-exit)
              result))))
  ;; When a local variable in the source buffer is available, use it.
  (should
   (equal "bar"
          (let ((org-coderef-label-format "foo"))
            (org-test-with-temp-text "#+BEGIN_SRC emacs-lisp\n0\n#+END_SRC"
              (setq-local org-coderef-label-format "bar")
              (org-src-coderef-format)))))
  (should
   (equal "bar"
          (let ((org-coderef-label-format "foo") result)
            (org-test-with-temp-text "#+BEGIN_SRC emacs-lisp\n0\n#+END_SRC"
              (setq-local org-coderef-label-format "bar")
              (org-edit-special)
              (setq result (org-src-coderef-format))
              (org-edit-src-exit)
              result))))
  ;; Use provided local format even if in an edit buffer.
  (should
   (equal "bar"
          (let ((org-coderef-label-format "foo"))
            (org-test-with-temp-text
		"#+BEGIN_SRC emacs-lisp -l \"bar\"\n0\n#+END_SRC"
              (org-src-coderef-format)))))
  (should
   (equal "bar"
          (let ((org-coderef-label-format "foo") result)
            (org-test-with-temp-text
                "#+BEGIN_SRC emacs-lisp -l \"bar\"\n0\n#+END_SRC"
              (org-edit-special)
              (setq result (org-src-coderef-format))
              (org-edit-src-exit)
              result))))
  ;; Local format has precedence over local variables.
  (should
   (equal "bar"
          (let ((org-coderef-label-format "foo"))
            (org-test-with-temp-text
		"#+BEGIN_SRC emacs-lisp -l \"bar\"\n0\n#+END_SRC"
	      (setq-local org-coderef-label-format "foo")
              (org-src-coderef-format)))))
  (should
   (equal "bar"
          (let ((org-coderef-label-format "foo") result)
            (org-test-with-temp-text
                "#+BEGIN_SRC emacs-lisp -l \"bar\"\n0\n#+END_SRC"
	      (setq-local org-coderef-label-format "foo")
              (org-edit-special)
              (setq result (org-src-coderef-format))
              (org-edit-src-exit)
              result))))
  ;; When optional argument provides a coderef format string, use it.
  (should
   (equal "bar"
	  (let ((org-coderef-label-format "foo")
		(element (org-element-create 'src-block '(:label-fmt "bar"))))
	    (org-test-with-temp-text "#+BEGIN_SRC emacs-lisp\n0\n#+END_SRC"
	      (org-src-coderef-format element)))))
  (should
   (equal "baz"
          (let ((org-coderef-label-format "foo")
		(element (org-element-create 'src-block '(:label-fmt "baz"))))
            (org-test-with-temp-text
		"#+BEGIN_SRC emacs-lisp -l \"bar\"\n0\n#+END_SRC"
	      (setq-local org-coderef-label-format "foo")
              (org-src-coderef-format element)))))
  ;; If it doesn't provide any label format string, fall back to
  ;; regular checks.
  (should
   (equal "foo"
	  (let ((org-coderef-label-format "foo")
		(element (org-element-create 'src-block)))
	    (org-test-with-temp-text "#+BEGIN_SRC emacs-lisp\n0\n#+END_SRC"
	      (org-src-coderef-format element)))))
  (should
   (equal "bar"
          (let ((org-coderef-label-format "foo")
		(element (org-element-create 'src-block)))
            (org-test-with-temp-text
		"#+BEGIN_SRC emacs-lisp -l \"bar\"\n0\n#+END_SRC"
	      (setq-local org-coderef-label-format "foo")
              (org-src-coderef-format element))))))

(ert-deftest test-org-src/coderef-regexp ()
  "Test `org-src-coderef-regexp' specifications."
  ;; Regular test.
  (should
   (string-match-p (org-src-coderef-regexp "; ref:%s")
		   "#+BEGIN_SRC emacs-lisp\n0; ref:label\n#+END_SRC"))
  ;; Ignore white space around the coderef.
  (should
   (string-match-p (org-src-coderef-regexp "; ref:%s")
		   "#+BEGIN_SRC emacs-lisp\n0 ; ref:label\n#+END_SRC"))
  (should
   (string-match-p (org-src-coderef-regexp "; ref:%s")
		   "#+BEGIN_SRC emacs-lisp\n0 ; ref:label  \n#+END_SRC"))
  ;; Only match regexp at the end of the line.
  (should-not
   (string-match-p (org-src-coderef-regexp "; ref:%s")
		   "#+BEGIN_SRC emacs-lisp\n0; ref:label (+ 1 2)\n#+END_SRC"))
  ;; Do not match an empty label.
  (should-not
   (string-match-p (org-src-coderef-regexp "; ref:%s")
		   "#+BEGIN_SRC emacs-lisp\n0; ref:\n#+END_SRC"))
  ;; When optional argument LABEL is provided, match given label only.
  (should
   (string-match-p (org-src-coderef-regexp "; ref:%s" "label")
		   "#+BEGIN_SRC emacs-lisp\n0; ref:label\n#+END_SRC"))
  (should-not
   (string-match-p (org-src-coderef-regexp "; ref:%s" "label2")
		   "#+BEGIN_SRC emacs-lisp\n0; ref:label\n#+END_SRC")))

(ert-deftest test-org-src/indented-blocks ()
  "Test editing indented blocks."
  ;; Editing a block should preserve its global indentation, unless
  ;; `org-src-preserve-indentation' is non-nil.
  (should
   (equal
    "- Item\n  #+BEGIN_SRC emacs-lisp\n    Foo\n  #+END_SRC"
    (org-test-with-temp-text
	"- Item\n<point>  #+BEGIN_SRC emacs-lisp\n    (+ 1 1)\n  #+END_SRC"
      (let ((org-edit-src-content-indentation 2)
	    (org-src-preserve-indentation nil))
	(org-edit-special)
	(erase-buffer)
	(insert "Foo")
	(org-edit-src-exit)
	(buffer-string)))))
  (should
   (equal
    "- Item\n  #+BEGIN_SRC emacs-lisp\n Foo\n  #+END_SRC"
    (org-test-with-temp-text
	"- Item\n<point>  #+BEGIN_SRC emacs-lisp\n    (+ 1 1)\n  #+END_SRC"
      (let ((org-src-preserve-indentation t))
	(org-edit-special)
	(erase-buffer)
	(insert " Foo")
	(org-edit-src-exit)
	(buffer-string)))))
  ;; Global indentation obeys `indent-tabs-mode' from the original
  ;; buffer.
  (should
   (string-match-p
    "^\t+\s*argument2"
    (org-test-with-temp-text
	"
- Item
  #+BEGIN_SRC emacs-lisp<point>
  (progn
    (function argument1
              argument2))
  #+END_SRC"
      (setq-local indent-tabs-mode t)
      (let ((org-edit-src-content-indentation 2)
	    (org-src-preserve-indentation nil))
	(org-edit-special)
	(org-edit-src-exit)
	(buffer-string)))))
  (should
   (string-match-p
    "^\s+argument2"
    (org-test-with-temp-text
	"
- Item
  #+BEGIN_SRC emacs-lisp<point>
    (progn\n      (function argument1\n\t\targument2))
  #+END_SRC"
      (setq-local indent-tabs-mode nil)
      (let ((org-edit-src-content-indentation 2)
	    (org-src-preserve-indentation nil))
	(org-edit-special)
	(org-edit-src-exit)
	(buffer-string)))))
  ;; Global indentation also obeys `tab-width' from original buffer.
  (should
   (string-match-p
    "^\t\\{3\\}\s\\{2\\}argument2"
    (org-test-with-temp-text
	"
- Item
  #+BEGIN_SRC emacs-lisp<point>
  (progn
    (function argument1
              argument2))
  #+END_SRC"
      (setq-local indent-tabs-mode t)
      (setq-local tab-width 4)
      (let ((org-edit-src-content-indentation 0)
	    (org-src-preserve-indentation nil))
	(org-edit-special)
	(org-edit-src-exit)
	(buffer-string)))))
  (should
   (string-match-p
    "^\t\s\\{6\\}argument2"
    (org-test-with-temp-text
	"
- Item
  #+BEGIN_SRC emacs-lisp<point>
  (progn
    (function argument1
              argument2))
  #+END_SRC"
      (setq-local indent-tabs-mode t)
      (setq-local tab-width 8)
      (let ((org-edit-src-content-indentation 0)
	    (org-src-preserve-indentation nil))
	(org-edit-special)
	(org-edit-src-exit)
	(buffer-string))))))

(ert-deftest test-org-src/footnote-references ()
  "Test editing footnote references."
  ;; Error when there is no definition to edit.
  (should-error
   (org-test-with-temp-text "A footnote<point>[fn:1]"
     (org-edit-special)))
  ;; Error when trying to edit an anonymous footnote.
  (should-error
   (org-test-with-temp-text "A footnote[fn::<point>edit me!]"
     (org-edit-special)))
  ;; Edit a regular definition.
  (should
   (equal "[fn:1] Definition"
	  (org-test-with-temp-text "A footnote<point>[fn:1]\n[fn:1] Definition"
	    (org-edit-special)
	    (prog1 (buffer-string) (org-edit-src-exit)))))
  ;; Label should be protected against editing.
  (should
   (org-test-with-temp-text "A footnote<point>[fn:1]\n[fn:1] Definition"
     (org-edit-special)
     (prog1 (get-text-property 0 'read-only (buffer-string))
       (org-edit-src-exit))))
  (should
   (org-test-with-temp-text "A footnote<point>[fn:1]\n[fn:1] Definition"
     (org-edit-special)
     (prog1 (get-text-property 5 'read-only (buffer-string))
       (org-edit-src-exit))))
  ;; Edit a regular definition.
  (should
   (equal
    "A footnote[fn:1][fn:2]\n[fn:1] D1\n\n[fn:2] D2"
    (org-test-with-temp-text
	"A footnote<point>[fn:1][fn:2]\n[fn:1] D1\n\n[fn:2] D2"
      (org-edit-special)
      (org-edit-src-exit)
      (buffer-string))))
  ;; Edit an inline definition.
  (should
   (equal
    "[fn:1:definition]"
    (org-test-with-temp-text
	"An inline<point>[fn:1] footnote[fn:1:definition]"
      (org-edit-special)
      (prog1 (buffer-string) (org-edit-src-exit)))))
  ;; Label and closing square bracket should be protected against
  ;; editing.
  (should
   (org-test-with-temp-text "An inline<point>[fn:1] footnote[fn:1:definition]"
     (org-edit-special)
     (prog1 (get-text-property 0 'read-only (buffer-string))
       (org-edit-src-exit))))
  (should
   (org-test-with-temp-text "An inline<point>[fn:1] footnote[fn:1:definition]"
     (org-edit-special)
     (prog1 (get-text-property 5 'read-only (buffer-string))
       (org-edit-src-exit))))
  (should
   (org-test-with-temp-text "An inline<point>[fn:1] footnote[fn:1:definition]"
     (org-edit-special)
     (prog1 (get-text-property 16 'read-only (buffer-string))
       (org-edit-src-exit))))
  ;; Do not include trailing white spaces when displaying the inline
  ;; footnote definition.
  (should
   (equal
    "[fn:1:definition]"
    (org-test-with-temp-text
	"An inline<point>[fn:1] footnote[fn:1:definition]    and some text"
      (org-edit-special)
      (prog1 (buffer-string) (org-edit-src-exit)))))
  ;; Preserve local variables when editing a footnote definition.
  (should
   (eq 'bar
       (org-test-with-temp-text "A footnote<point>[fn:1]\n[fn:1] Definition"
	 (setq-local foo 'bar)
	 (org-edit-special)
	 (prog1 foo (org-edit-src-exit))))))

;;; Code escaping

(ert-deftest test-org-src/escape-code-in-string ()
  "Test `org-escape-code-in-string' specifications."
  ;; Escape lines starting with "*" or "#+".
  (should (equal ",*" (org-escape-code-in-string "*")))
  (should (equal ",#+" (org-escape-code-in-string "#+")))
  ;; Escape lines starting with ",*" and ",#+".  Number of leading
  ;; commas does not matter.
  (should (equal ",,*" (org-escape-code-in-string ",*")))
  (should (equal ",,#+" (org-escape-code-in-string ",#+")))
  (should (equal ",,,*" (org-escape-code-in-string ",,*")))
  (should (equal ",,,#+" (org-escape-code-in-string ",,#+")))
  ;; Indentation does not matter.
  (should (equal " ,*" (org-escape-code-in-string " *")))
  (should (equal " ,#+" (org-escape-code-in-string " #+")))
  ;; Do nothing on other cases.
  (should (equal "a" (org-escape-code-in-string "a")))
  (should (equal "#" (org-escape-code-in-string "#")))
  (should (equal "," (org-escape-code-in-string ","))))

(ert-deftest test-org-src/unescape-code-in-string ()
  "Test `org-unescape-code-in-string' specifications."
  ;; Unescape lines starting with ",*" or ",#+".  Number of leading
  ;; commas does not matter.
  (should (equal "*" (org-unescape-code-in-string ",*")))
  (should (equal "#+" (org-unescape-code-in-string ",#+")))
  (should (equal ",*" (org-unescape-code-in-string ",,*")))
  (should (equal ",#+" (org-unescape-code-in-string ",,#+")))
  ;; Indentation does not matter.
  (should (equal " *" (org-unescape-code-in-string " ,*")))
  (should (equal " #+" (org-unescape-code-in-string " ,#+")))
  ;; Do nothing on other cases.
  (should (equal "a" (org-unescape-code-in-string "a")))
  (should (equal "#" (org-unescape-code-in-string "#")))
  (should (equal "," (org-unescape-code-in-string ","))))


(provide 'test-org-src)
;;; test-org-src.el ends here
