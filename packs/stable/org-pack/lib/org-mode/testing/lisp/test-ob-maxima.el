;;; test-ob-maxima.el --- tests for ob-maxima.el

;; Copyright (c) 2010-2014, 2019 Sergey Litvinov
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(org-test-for-executable "maxima")
(unless (featurep 'ob-maxima)
  (signal 'missing-test-dependency "Support for Maxima code blocks"))

(ert-deftest ob-maxima/assert ()
  (should t))

(ert-deftest ob-maxima/integer-input ()
  "Test of integer input"
  (org-test-at-id "b5842ed4-8e8b-4b18-a1c9-cef006b6a6c8"
    (org-babel-next-src-block)
    (should (equal 4 (org-babel-execute-src-block)))))

(ert-deftest ob-maxima/string-input ()
  "Test of string input"
  (org-test-at-id "b5842ed4-8e8b-4b18-a1c9-cef006b6a6c8"
    (org-babel-next-src-block 2)
    (should (equal "- sin(x)" (org-babel-execute-src-block)))))

(ert-deftest ob-maxima/simple-list-input ()
  "Test of flat list input"
  (org-test-at-id "b5561c6a-73cd-453a-ba5e-62ad84844de6"
    (org-babel-next-src-block)
    (should (equal "[1, 2, 3] " (org-babel-execute-src-block)))))

(ert-deftest ob-maxima/list-input ()
  "Test of list input"
  (org-test-at-id "b5561c6a-73cd-453a-ba5e-62ad84844de6"
    (org-babel-next-src-block 2)
    (should (equal "[2, [2, 3], 4] " (org-babel-execute-src-block)))))

(ert-deftest ob-maxima/table-input1 ()
  "Test of table input"
  (org-test-at-id "400ee228-6b12-44fd-8097-7986f0f0db43"
    (org-babel-next-src-block)
    (should (equal "[[2.0], [3.0]] " (org-babel-execute-src-block)))))

(ert-deftest ob-maxima/table-input2 ()
  "Test of table input"
  (org-test-at-id "400ee228-6b12-44fd-8097-7986f0f0db43"
    (org-babel-next-src-block 2)
    (should (equal "[[2.0, 3.0]] " (org-babel-execute-src-block)))))

(ert-deftest ob-maxima/matrix-output ()
  "Test of table output"
  (org-test-at-id "cc158527-b867-4b1d-8ae0-b8c713a90fd7"
    (org-babel-next-src-block)
    (should
     (equal
      '((1 2 3) (2 3 4) (3 4 5)) (org-babel-execute-src-block)))))

(provide 'test-ob-maxima)

;;; test-ob-maxima.el ends here
