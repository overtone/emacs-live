;;; test-ob-sh.el

;; Copyright (c) 2010-2014 Eric Schulte
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

;; Template test file for Org-mode tests

;;; Code:
(org-test-for-executable "sh")
(unless (featurep 'ob-sh)
  (signal 'missing-test-dependency "Support for Sh code blocks"))

(ert-deftest test-ob-sh/dont-insert-spaces-on-expanded-bodies ()
  "Expanded shell bodies should not start with a blank line
unless the body of the tangled block does."
  (should-not (string-match "^[\n\r][\t ]*[\n\r]"
			    (org-babel-expand-body:generic "echo 2" '())))
  (should (string-match "^[\n\r][\t ]*[\n\r]"
			(org-babel-expand-body:generic "\n\necho 2" '()))))

(ert-deftest test-ob-sh/dont-error-on-empty-results ()
  "Was throwing an elisp error when shell blocks threw errors and
returned empty results."
  (should (null (org-babel-execute:sh "ls NoSuchFileOrDirectory.txt" nil))))

(ert-deftest test-ob-sh/session ()
  "This also tests `org-babel-comint-with-output' in
ob-comint.el, which was not previously tested."
  (let ((res (org-babel-execute:sh "echo 1; echo 2" '((:session . "yes")))))
    (should res)
    (should (listp res))))

(provide 'test-ob-sh)

;;; test-ob-sh.el ends here
