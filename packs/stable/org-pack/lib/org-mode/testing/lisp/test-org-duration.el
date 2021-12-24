;;; test-org-duration.el --- Tests for org-duration.el -*- lexical-binding: t; -*-

;; Copyright (C) 2017, 2019  Nicolas Goaziou

;; Author: Nicolas Goaziou <mail@nicolasgoaziou.fr>

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

(ert-deftest test-org-duration/to-minutes ()
  "Test `org-duration-to-minutes' specifications."
  ;; Raise an error for unknown duration format.
  (should-error (org-duration-to-minutes "1:2"))
  ;; Return number of minutes, as a float.
  (should (= (org-duration-to-minutes "1:01") 61))
  (should (floatp (org-duration-to-minutes "1:01")))
  ;; Handle various duration formats.
  (should (= (org-duration-to-minutes "1:20:30") 80.5))
  (should (= (org-duration-to-minutes "2h 10min") 130))
  (should (= (org-duration-to-minutes "1d 1:02") 1502))
  (should (= (org-duration-to-minutes "2.5h") 150))
  ;; Special case: a bare number is treated as minutes.
  (should (= (org-duration-to-minutes "2") 2))
  (should (= (org-duration-to-minutes "2.5") 2.5))
  (should (= (org-duration-to-minutes 1) 1))
  ;; Special case: the empty string is 0.0.
  (should (= (org-duration-to-minutes "") 0.0))
  ;; Support custom units.
  (should (= 4
	     (let ((org-duration-units '(("longmin" . 2)))
		   org-duration--unit-re
		   org-duration--full-re
		   org-duration--mixed-re)
	       (org-duration-set-regexps)
	       (org-duration-to-minutes "2longmin"))))
  (should (= 61
	     (let ((org-duration-units '(("h" . 61)))
		   org-duration--unit-re
		   org-duration--full-re
		   org-duration--mixed-re)
	       (org-duration-set-regexps)
	       (org-duration-to-minutes "1h"))))
  ;; When CANONICAL is non-nil, ignore custom units and only recognize
  ;; units defined in `org-duration-canonical-units'.
  (should (= 60
	     (let ((org-duration-units '(("h" . 61)))
		   org-duration--unit-re
		   org-duration--full-re
		   org-duration--mixed-re)
	       (org-duration-set-regexps)
	       (org-duration-to-minutes "1h" t))))
  (should-error (let ((org-duration-units '(("longmin" . 2)))
		      org-duration--unit-re
		      org-duration--full-re
		      org-duration--mixed-re)
		  (org-duration-set-regexps)
		  (org-duration-to-minutes "2longmin" t))))

(ert-deftest test-org-duration/from-minutes ()
  "Test `org-duration-from-minutes' specifications."
  ;; Format number of minutes according to `org-duration-format'.
  (should (equal "1:00"
		 (let ((org-duration-format 'h:mm))
		   (org-duration-from-minutes 60))))
  (should (equal "1:01:30"
		 (let ((org-duration-format 'h:mm:ss))
		   (org-duration-from-minutes 61.5))))
  (should (equal "1:01"
		 (let ((org-duration-format 'h:mm))
		   (org-duration-from-minutes 61.5))))
  ;; Handle required parameter in advanced format specifications.
  (should (equal "1h"
		 (let ((org-duration-format '(("h" . nil) ("min" . nil))))
		   (org-duration-from-minutes 60))))
  (should (equal "1h 0min"
		 (let ((org-duration-format '(("h" . nil) ("min" . t))))
		   (org-duration-from-minutes 60))))
  (should (equal "50min"
		 (let ((org-duration-format '(("h" . nil) ("min" . nil))))
		   (org-duration-from-minutes 50))))
  (should (equal "0h 50min"
		 (let ((org-duration-format '(("h" . t) ("min" . t))))
		   (org-duration-from-minutes 50))))
  ;; Handle mixed mode.
  (should (equal "1d 0:10"
		 (let ((org-duration-format '(("d" . nil) (special . h:mm))))
		   (org-duration-from-minutes (+ (* 24 60) 10)))))
  (should (equal "1d 0:12:30"
		 (let ((org-duration-format '(("d" . nil) (special . h:mm:ss))))
		   (org-duration-from-minutes (+ (* 24 60) 12.5)))))
  ;; Handle fractional duration.  Parameter is the precision.
  (should (equal "1.5h"
		 (let ((org-duration-format '(("h" . nil) (special . 1))))
		   (org-duration-from-minutes 90))))
  (should (equal "1.50h"
		 (let ((org-duration-format '(("h" . nil) (special . 2))))
		   (org-duration-from-minutes 90))))
  ;; When using fractional duration, use first required unit or the
  ;; first with a non-zero integer part.  If none is found, refer to
  ;; smallest unit specified in format.
  (should (equal "0.7h"
		 (let ((org-duration-format
			'(("h" . t) ("min" . nil) (special . 1))))
		   (org-duration-from-minutes 40))))
  (should (equal "40.0min"
		 (let ((org-duration-format
			'(("h" . nil) ("min" . nil) (special . 1))))
		   (org-duration-from-minutes 40))))
  (should (equal "0.5min"
		 (let ((org-duration-format
			'(("h" . nil) ("min" . nil) (special . 1))))
		   (org-duration-from-minutes 0.5))))
  ;; Handle compact form.
  (should (equal "0h50min"
		 (let ((org-duration-format '(("h" . t) ("min" . t) compact)))
		   (org-duration-from-minutes 50))))
  (should (equal "1d0:10"
		 (let ((org-duration-format
			'(("d" . nil) (special . h:mm) compact)))
		   (org-duration-from-minutes (+ (* 24 60) 10))))))

(ert-deftest test-org-duration/p ()
  "Test `org-duration-p' specifications."
  ;; Test all duration formats.
  (should (org-duration-p "3:12"))
  (should (org-duration-p "123:12"))
  (should (org-duration-p "1:23:45"))
  (should (org-duration-p "3d 3h 4min"))
  (should (org-duration-p "3d3h4min"))
  (should (org-duration-p "3d 13:35"))
  (should (org-duration-p "3d13:35"))
  (should (org-duration-p "2.35h"))
  ;; Handle custom units, but return nil for unknown units.
  (should-not (org-duration-p "1minute"))
  (should (let ((org-duration-units '(("minute" . 1)))
		org-duration--unit-re
		org-duration--full-re
		org-duration--mixed-re)
	    (org-duration-set-regexps)
	    (org-duration-p "2minute")))
  ;; Tolerate white space between the number and the unit.
  (should (org-duration-p "2 h"))
  ;; Return nil for ill-formed H:MM:SS strings.
  (should-not (org-duration-p "3::12"))
  (should-not (org-duration-p "3:2"))
  (should-not (org-duration-p "3:12:4"))
  ;; Return nil in mixed mode if H:MM:SS part is not the last one.
  (should-not (org-duration-p "3d 13:35 13h")))

(ert-deftest test-org-duration/h:mm-only-p ()
  "Test `org-duration-h:mm-only-p' specifications."
  (should (org-duration-h:mm-only-p '("123:31" "1:00")))
  (should-not (org-duration-h:mm-only-p '("123:32" "1h")))
  (should (eq 'h:mm:ss (org-duration-h:mm-only-p '("3:33" "1:23:45")))))


(provide 'test-org-duration)
;;; test-org-duration.el ends here
