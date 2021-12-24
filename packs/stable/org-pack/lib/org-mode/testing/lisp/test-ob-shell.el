;;; test-ob-shell.el

;; Copyright (c) 2010-2014, 2019 Eric Schulte
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

;;; Comment:

;; Template test file for Org tests

;;; Code:
(org-test-for-executable "sh")
(unless (featurep 'ob-shell)
  (signal 'missing-test-dependency "Support for Shell code blocks"))

(ert-deftest test-ob-shell/dont-insert-spaces-on-expanded-bodies ()
  "Expanded shell bodies should not start with a blank line
unless the body of the tangled block does."
  (should-not (string-match "^[\n\r][\t ]*[\n\r]"
			    (org-babel-expand-body:generic "echo 2" '())))
  (should (string-match "^[\n\r][\t ]*[\n\r]"
			(org-babel-expand-body:generic "\n\necho 2" '()))))

(ert-deftest test-ob-shell/dont-error-on-empty-results ()
  "Was throwing an elisp error when shell blocks threw errors and
returned empty results."
  (should (null (org-babel-execute:sh "ls NoSuchFileOrDirectory.txt" nil))))

(ert-deftest test-ob-shell/session ()
  "This also tests `org-babel-comint-with-output' in
ob-comint.el, which was not previously tested."
  (let ((res (org-babel-execute:sh "echo 1; echo 2" '((:session . "yes")))))
    (should res)
    (should (listp res))))

; A list of tests using the samples in ob-shell-test.org
(ert-deftest ob-shell/generic-uses-no-arrays ()
  "No arrays for generic"
  (org-test-at-id "0ba56632-8dc1-405c-a083-c204bae477cf"
    (org-babel-next-src-block)
    (should (equal "one two three" (org-trim (org-babel-execute-src-block))))))

(ert-deftest ob-shell/bash-uses-arrays ()
  "Bash arrays"
  (org-test-at-id "0ba56632-8dc1-405c-a083-c204bae477cf"
    (org-babel-next-src-block 2)
    (should (equal "one" (org-trim (org-babel-execute-src-block))))))

(ert-deftest ob-shell/generic-uses-no-assoc-arrays ()
  "No associative arrays for generic"
  (should
   (equal "first one second two third three"
	  (org-test-at-id
	   "bec1a5b0-4619-4450-a8c0-2a746b44bf8d"
	   (org-babel-next-src-block)
	   (org-trim (org-babel-execute-src-block)))))
  (should
   (equal "bread 2 kg spaghetti 20 cm milk 50 dl"
	  (org-test-at-id
	   "82320a48-3409-49d7-85c9-5de1c6d3ff87"
	   (org-babel-next-src-block)
	   (org-trim (org-babel-execute-src-block))))))

(ert-deftest ob-shell/bash-uses-assoc-arrays ()
  "Bash associative arrays"
  (should
   (equal "two"
	  (org-test-at-id
	   "bec1a5b0-4619-4450-a8c0-2a746b44bf8d"
	   (org-babel-next-src-block 2)
	   (org-trim (org-babel-execute-src-block)))))
  ;; Bash associative arrays as strings for the row.
  (should
   (equal "20 cm"
	  (org-test-at-id
	   "82320a48-3409-49d7-85c9-5de1c6d3ff87"
	   (org-babel-next-src-block 2)
	   (org-trim (org-babel-execute-src-block))))))

(ert-deftest ob-shell/simple-list ()
  "Test list variables in shell."
  ;; With bash, a list is turned into an array.
  (should
   (equal "2"
	  (org-test-with-temp-text
	   "#+BEGIN_SRC bash :results output :var l='(1 2)\necho ${l[1]}\n#+END_SRC"
	   (org-trim (org-babel-execute-src-block)))))
  ;; On sh, it is a string containing all values.
  (should
   (equal "1 2"
	  (org-test-with-temp-text
	   "#+BEGIN_SRC sh :results output :var l='(1 2)\necho ${l}\n#+END_SRC"
	   (org-trim (org-babel-execute-src-block))))))

(provide 'test-ob-shell)

;;; test-ob-shell.el ends here
