;;; test-ob-fortran.el --- tests for ob-fortran.el

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

;;; Code:
(org-test-for-executable "gfortran")
(unless (featurep 'ob-fortran)
  (signal 'missing-test-dependency "Support for Fortran code blocks"))

(ert-deftest ob-fortran/assert ()
  (should t))

(ert-deftest ob-fortran/simple-program ()
  "Test of hello world program."
  (org-test-at-id "459384e8-1797-4f11-867e-dde0473ea7cc"
    (org-babel-next-src-block)
    (should (equal "Hello world" (org-babel-execute-src-block)))))

(ert-deftest ob-fortran/fortran-var-program ()
  "Test a fortran variable"
  (org-test-at-id "459384e8-1797-4f11-867e-dde0473ea7cc"
    (org-babel-next-src-block 2)
    (should (= 10 (org-babel-execute-src-block)))))

(ert-deftest ob-fortran/input-var ()
  "Test :var"
  (org-test-at-id "d8d1dfd3-5f0c-48fe-b55d-777997e02242"
    (org-babel-next-src-block)
    (should (= 15 (org-babel-execute-src-block)))))

(ert-deftest ob-fortran/preprocessor-var ()
  "Test preprocessed fortran"
  (org-test-at-id "d8d1dfd3-5f0c-48fe-b55d-777997e02242"
    (org-babel-next-src-block 2)
    (should (= 42 (org-babel-execute-src-block)))))

(ert-deftest ob-fortran/character-var ()
  "Test string input"
  (org-test-at-id "d8d1dfd3-5f0c-48fe-b55d-777997e02242"
    (org-babel-next-src-block 3)
    (should (equal "word" (org-babel-execute-src-block)))))

(ert-deftest ob-fortran/list-var ()
  "Test real array input"
  (org-test-at-id "c28569d9-04ce-4cad-ab81-1ea29f691465"
    (org-babel-next-src-block)
    (should (equal "1.00 2.00 3.00" (org-babel-execute-src-block)))))

(ert-deftest ob-fortran/list-var-from-table ()
  "Test real array from a table"
  (org-test-at-id "c28569d9-04ce-4cad-ab81-1ea29f691465"
    (org-babel-next-src-block 2)
    (should (equal "1.00 2.00" (org-babel-execute-src-block)))))

(ert-deftest ob-fortran/list-matrix-from-table1 ()
  "Test real matrix from a table"
  (org-test-at-id "3f73ab19-d25a-428d-8c26-e8c6aa933976"
    (org-babel-next-src-block 1)
    (should (= 42 (org-babel-execute-src-block)))))

(ert-deftest ob-fortran/list-matrix-from-table2 ()
  "Test real matrix from a table"
  (org-test-at-id "3f73ab19-d25a-428d-8c26-e8c6aa933976"
    (org-babel-next-src-block 2)
    (should (= 42 (org-babel-execute-src-block)))))

(ert-deftest ob-fortran/no-variables-with-main ()
  "Test :var with explicit 'program'"
  (org-test-at-id "891ead4a-f87a-473c-9ae0-1cf348bcd04f"
    (org-babel-next-src-block)
    (should-error (org-babel-execute-src-block))
    :type 'error))

;; (ert-deftest ob-fortran/wrong-list ()
;;   "Test wrong input list"
;;   (org-test-at-id "891ead4a-f87a-473c-9ae0-1cf348bcd04f"
;;     (org-babel-next-src-block 2)
;;     (should-error (org-babel-execute-src-block))
;;     :type 'error))

;; (ert-deftest ob-fortran/compiler-flags ()
;;   "Test compiler's flags"
;;   (org-test-at-id "891ead4a-f87a-473c-9ae0-1cf348bcd04f"
;;     (org-babel-next-src-block 3)
;;     (should-error (org-babel-execute-src-block))
;;     :type 'error))

(ert-deftest ob-fortran/command-arguments ()
  "Test real array from a table"
  (org-test-at-id "2d5330ea-9934-4737-9ed6-e1d3dae2dfa4"
    (org-babel-next-src-block)
    (should (= 23 (org-babel-execute-src-block)))))

(provide 'test-ob-fortran)

;;; test-ob-fortran.el ends here
 
