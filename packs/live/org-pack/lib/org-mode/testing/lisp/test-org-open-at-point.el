;;; test-org-open-at-point.el

;; Copyright (c) Samuel Loury
;; Authors: Samuel Loury

;; Released under the GNU General Public License version 3
;; see: http://www.gnu.org/licenses/gpl-3.0.html

;;;; Comments:

;; Test for the org-open-at-point function

;;; Code:

(save-excursion
  (set-buffer (get-buffer-create "test-org-open-at-point.el"))
  (setq ly-here
        (file-name-directory
         (or load-file-name (buffer-file-name)))))

(defun test-org-open-at-point/goto-fixture ()
  (find-file-other-window
   (concat ly-here "../examples/open-at-point.org"))
  (set-buffer "open-at-point.org"))

(ert-deftest test-org-open-at-point/bracket-link-inside ()
  "Test `org-open-at-point' from inside a bracket link."
  (test-org-open-at-point/goto-fixture)
  ;; go inside the bracket link
  (goto-char 113)
  (org-open-at-point)
  ;; should now be in front of the header
  (should (equal (point) 2)))

(ert-deftest test-org-open-at-point/plain-link-inside ()
  "Test `org-open-at-point' from inside a plain link."
  (test-org-open-at-point/goto-fixture)
  ;; go inside the plain link
  (goto-char 126)
  (org-open-at-point)
  ;; should now be in front of the header
  (should (equal (point) 2)))

(ert-deftest test-org-open-at-point/bracket-link-before ()
  "Test `org-open-at-point' from before a bracket link but in the same line."
  (test-org-open-at-point/goto-fixture)
  ;; go before the bracket link
  (goto-char 83)
  (message "point %s" (point))
  (org-open-at-point)
  ;; should now be in front of the header
  (should (equal (point) 2)))

(ert-deftest test-org-open-at-point/plain-link-before ()
  "Test `org-open-at-point' from before a plain link but in the same line."
  (test-org-open-at-point/goto-fixture)
  ;; go before the plain link
  (goto-char 124)
  (org-open-at-point)
  ;; should now be in front of the header
  (should (equal (point) 2)))
