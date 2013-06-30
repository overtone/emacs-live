;;; test-ob-R.el --- tests for ob-R.el

;; Copyright (c) 2011-2013 Eric Schulte
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

;;; Code:
(defmacro test-org-in-property-buffer (&rest body)
  `(with-temp-buffer
     (insert-file-contents (expand-file-name "property-inheritance.org"
					     org-test-example-dir))
     (org-mode)
     ,@body))
(def-edebug-spec test-org-in-property-buffer (body))

(ert-deftest test-org-property-accumulation-top-use ()
  (test-org-in-property-buffer
   (goto-char (point-min))
   (org-babel-next-src-block 1)
   (should (equal 3 (org-babel-execute-src-block)))))

(ert-deftest test-org-property-accumulation-top-val ()
  (test-org-in-property-buffer
   (goto-char (point-min))
   (org-babel-next-src-block 2)
   (should (string= "foo=1 bar=2" (org-babel-execute-src-block)))))

(ert-deftest test-org-property-accumulation-overwrite-use ()
  (test-org-in-property-buffer
   (goto-char (point-min))
   (org-babel-next-src-block 3)
   (should (= 7 (org-babel-execute-src-block)))))

(ert-deftest test-org-property-accumulation-overwrite-val ()
  (test-org-in-property-buffer
   (goto-char (point-min))
   (org-babel-next-src-block 4)
   (should (string= "foo=7" (org-babel-execute-src-block)))))

(ert-deftest test-org-property-accumulation-append-use ()
  (test-org-in-property-buffer
   (goto-char (point-min))
   (org-babel-next-src-block 5)
   (should (= 6 (org-babel-execute-src-block)))))

(ert-deftest test-org-property-accumulation-append-val ()
  (test-org-in-property-buffer
   (goto-char (point-min))
   (org-babel-next-src-block 6)
   (should (string= "foo=1 bar=2 baz=3" (org-babel-execute-src-block)))))

(provide 'test-ob-R)

;;; test-ob-R.el ends here
