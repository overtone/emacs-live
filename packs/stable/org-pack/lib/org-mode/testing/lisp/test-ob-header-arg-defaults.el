;;; test-ob-header-arg-defaults.el --- tests for default header args from properties

;; Copyright (c) 2013, 2014, 2019 Achim Gratz
;; Authors: Achim Gratz

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

(ert-deftest test-ob-header-arg-defaults/global/call ()
  (org-test-at-id "3fdadb69-5d15-411e-aad0-f7860cdd7816"
   (org-babel-next-src-block 1)
   (forward-line -1)
   (should (equal "ge1/gh2/--3/ge4/ge5/--6/--7/--8/--9"
		  (org-babel-execute-src-block nil (org-babel-lob-get-info))))))

(ert-deftest test-ob-header-arg-defaults/global/noweb ()
  (org-test-at-id "3fdadb69-5d15-411e-aad0-f7860cdd7816"
   (org-babel-next-src-block 1)
   (should (equal "ge1/gh2/--3/ge4/ge5/--6/--7"
		  (org-babel-execute-src-block)))))

(ert-deftest test-ob-header-arg-defaults/tree/overwrite/call ()
  (should
   (equal "ge1/gh2/--3/ge4/ge5/--6/th7/te8/--9"
	  (org-test-at-id "a9cdfeda-9f31-4bb5-b694-2cf452f07dfd"
	    (org-babel-next-src-block 1)
	    (forward-line -1)
	    (org-babel-execute-src-block nil (org-babel-lob-get-info))))))

(ert-deftest test-ob-header-arg-defaults/tree/overwrite/noweb ()
  (should
   (equal "--1/--2/--3/--4/--5/--6/th7/te8/--9"
	  (org-test-at-id "a9cdfeda-9f31-4bb5-b694-2cf452f07dfd"
	    (org-babel-next-src-block 1)
	    (org-babel-execute-src-block)))))

(ert-deftest test-ob-header-arg-defaults/tree/accumulate/call ()
  (should
   (equal "ge1/th2/th3/ge4/te5/--6"
	  (org-test-at-id "1d97d258-fd50-4107-a095-e4625bffc57b"
	    (org-babel-next-src-block 1)
	    (forward-line -1)
	    (org-babel-execute-src-block nil (org-babel-lob-get-info))))))

(ert-deftest test-ob-header-arg-defaults/tree/accumulate/noweb ()
  (should
   (equal "ge1/th2/th3/ge4/te5/--6/--7/--8"
	  (org-test-at-id "1d97d258-fd50-4107-a095-e4625bffc57b"
	    (org-babel-next-src-block 1)
	    (org-babel-execute-src-block)))))

(ert-deftest test-ob-header-arg-defaults/tree/complex/call ()
  (should
   (equal "gh1/th2/--3/gh4/te5/--6"
	  (org-test-at-id "fa0e912d-d9b4-47b0-9f9e-1cbb39f7cbc2"
	    (org-babel-next-src-block 1)
	    (forward-line -1)
	    (org-babel-execute-src-block nil (org-babel-lob-get-info))))))

(ert-deftest test-ob-header-arg-defaults/tree/complex/noweb ()
  (should
   (equal "gh1/th2/--3/gh4/te5/--6/--7/--8/--9"
	  (org-test-at-id "fa0e912d-d9b4-47b0-9f9e-1cbb39f7cbc2"
	    (org-babel-next-src-block 1)
	    (org-babel-execute-src-block)))))

(provide 'test-ob-header-arg-defaults)

;;; test-ob-header-arg-defaults.el ends here
