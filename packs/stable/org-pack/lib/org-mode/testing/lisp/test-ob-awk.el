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
(org-test-for-executable "awk")
(unless (featurep 'ob-awk)
  (signal 'missing-test-dependency "Support for Awk code blocks"))

(ert-deftest ob-awk/input-none ()
  "Test with no input file"
  (org-test-at-id "9e998b2a-3581-43fe-b26d-07d3c507b86a"
    (org-babel-next-src-block)
    (should (= 42 (org-babel-execute-src-block)))))

(ert-deftest ob-awk/input-src-block-1 ()
  "Test a code block as an input"
  (org-test-at-id "9e998b2a-3581-43fe-b26d-07d3c507b86a"
    (org-babel-next-src-block 2)
    (should (= 43 (org-babel-execute-src-block)))))

(ert-deftest ob-awk/input-src-block-2 ()
  "Test a code block as an input"
  (org-test-at-id "9e998b2a-3581-43fe-b26d-07d3c507b86a"
    (org-babel-next-src-block 3)
    (should (= 150 (org-babel-execute-src-block)))))

(ert-deftest ob-awk/tabular-input ()
  "Test a code block as an input"
  (org-test-at-id "9e998b2a-3581-43fe-b26d-07d3c507b86a"
    (org-babel-next-src-block 4)
    (should (equal '(("a" "b" "c")) (org-babel-execute-src-block)))))
