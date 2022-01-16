;;; test-ob-sed.el --- tests for ob-sed.el

;; Copyright (c) 2015, 2019 Bjarte Johansen
;; Authors: Bjarte Johansen

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

(require 'ob-sed)
(org-test-for-executable "sed")
(unless (featurep 'ob-sed)
  (signal 'missing-test-dependency "Support for Sed code blocks"))

(ert-deftest ob-sed-test/simple-execution-of-script ()
  "Test simple execution of script."
  (org-test-at-id "C7E7CA6A-2601-42C9-B534-4102D62E458D"
    (org-babel-next-src-block)
    (should (string= "A processed sentence."
		     (org-babel-execute-src-block)))))

(ert-deftest ob-sed-test/in-file-header-argument ()
  "Test :in-file header argument."
  (org-test-at-id "54EC49AA-FE9F-4D58-812E-00FC87FAF562"
    (let ((default-directory temporary-file-directory))
      (with-temp-buffer
	(insert "A test file.")
	(write-file "test1.txt"))
      (org-babel-next-src-block)
      (should (string= "A tested file."
		       (org-babel-execute-src-block))))))

(ert-deftest ob-sed-test/cmd-line-header-argument ()
  "Test :cmd-line header argument."
  (org-test-at-id "E3C6A8BA-39FF-4840-BA8E-90D5C4365AB1"
    (let ((default-directory temporary-file-directory))
      (with-temp-buffer
	(insert "A test file.")
	(write-file "test2.txt"))
      (org-babel-next-src-block)
      (org-babel-execute-src-block)
      (should (string= "A tested again file.\n"
		       (with-temp-buffer
			 (insert-file-contents "test2.txt")
			 (buffer-string)))))))


(provide 'test-ob-sed)
;;; test-ob-sed ends here
