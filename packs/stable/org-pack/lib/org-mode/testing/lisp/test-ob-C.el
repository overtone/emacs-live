;;; test-ob-awk.el --- tests for ob-awk.el

;; Copyright (c) 2010-2014 Sergey Litvinov
;; Authors: Sergey Litvinov

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
(unless (featurep 'ob-C)
  (signal 'missing-test-dependency "Support for C code blocks"))

(ert-deftest ob-C/assert ()
  (should t))

(ert-deftest ob-C/simple-program ()
  "Hello world program."
  (org-test-at-id "fa6db330-e960-4ea2-ac67-94bb845b8577"
    (org-babel-next-src-block)
    (should (= 42 (org-babel-execute-src-block)))))

(ert-deftest ob-C/integer-var ()
  "Test of an integer variable."
  (org-test-at-id "fa6db330-e960-4ea2-ac67-94bb845b8577"
    (org-babel-next-src-block 2)
    (should (= 12 (org-babel-execute-src-block)))))

(ert-deftest ob-C/two-integer-var ()
  "Test of two input variables"
  (org-test-at-id "fa6db330-e960-4ea2-ac67-94bb845b8577"
    (org-babel-next-src-block 3)
    (should (= 22 (org-babel-execute-src-block)))))

(ert-deftest ob-C/string-var ()
  "Test of a string input variable"
  (org-test-at-id "fa6db330-e960-4ea2-ac67-94bb845b8577"
    (org-babel-next-src-block 4)
    (should (equal "word 4" (org-babel-execute-src-block)))))

(ert-deftest ob-C/preprocessor ()
  "Test of a string variable"
  (org-test-at-id "fa6db330-e960-4ea2-ac67-94bb845b8577"
    (org-babel-next-src-block 5)
    (should (= 42 (org-babel-execute-src-block)))))

(ert-deftest ob-C/table ()
  "Test of a table output"
  (org-test-at-id "2df1ab83-3fa3-462a-a1f3-3aef6044a874"
    (org-babel-next-src-block)
    (should (equal '((1) (2)) (org-babel-execute-src-block)))))

(ert-deftest ob-C/list-var ()
"Test of a list input variable"
  (org-test-at-id "cc65d6b3-8e8e-4f9c-94cd-f5a00cdeceb5"
    (org-babel-next-src-block 1)
    (should (string= "abcdef2" (org-babel-execute-src-block)))))

(ert-deftest ob-C/vector-var ()
"Test of a vector input variable"
  (org-test-at-id "cc65d6b3-8e8e-4f9c-94cd-f5a00cdeceb5"
    (org-babel-next-src-block 2)
    (should (equal 122 (org-babel-execute-src-block)))))

(ert-deftest ob-C/list-list-var ()
  "Test of a list list input variable"
  (org-test-at-id "cc65d6b3-8e8e-4f9c-94cd-f5a00cdeceb5"
    (org-babel-next-src-block 3)
    (should (equal '((1 3) (2 4)) (org-babel-execute-src-block)))))

;;; test-ob-C.el ends here
