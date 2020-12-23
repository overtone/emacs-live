;;; test-ob-table.el

;; Copyright (c) 2011-2014, 2019 Eric Schulte
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

;; TODO Test Broken (wrong-type-argument number-or-marker-p "2.0")
;; (ert-deftest test-ob-table/sbe ()
;;   "Test that `sbe' can be used to call code blocks from inside tables."
;;   (org-test-at-id "6d2ff4ce-4489-4e2a-9c65-e3f71f77d975"
;;     (should (= 2 (sbe take-sqrt (n "4"))))))

(provide 'test-ob-table)

;;; test-ob-table.el ends here
