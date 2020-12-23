;;; test-org-timer.el --- Tests for org-timer.el

;; Copyright (C) 2014-2015, 2019  Kyle Meyer

;; Author: Kyle Meyer <kyle@kyleam.com>

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

(eval-and-compile (require 'cl-lib))

(defmacro test-org-timer/with-temp-text (text &rest body)
  "Like `org-test-with-temp-text', but set timer-specific variables.
Also, mute output from `message'."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'message) (lambda (&rest args) nil)))
     (org-test-with-temp-text ,text
       (let (org-timer-start-time
	     org-timer-pause-time
	     org-timer-countdown-timer
	     org-timer-display)
	 (unwind-protect (progn ,@body)
	   (when (timerp org-timer-countdown-timer)
	     (cancel-timer org-timer-countdown-timer)))))))

(defmacro test-org-timer/with-current-time (time &rest body)
  "Run BODY, setting `current-time' output to TIME."
  (declare (indent 1))
  `(org-test-at-time ,time ,@body))


;;; Time conversion and formatting

(ert-deftest test-org-timer/secs-to-hms ()
  "Test conversion between HMS format and seconds."
  ;; Seconds to HMS, and back again
  (should
   (equal "0:00:30"
	  (org-timer-secs-to-hms 30)))
  (should
   (equal 30
	  (org-timer-hms-to-secs (org-timer-secs-to-hms 30))))
  ;; Minutes to HMS, and back again
  (should
   (equal "0:02:10"
	  (org-timer-secs-to-hms 130)))
  (should
   (equal 130
	  (org-timer-hms-to-secs (org-timer-secs-to-hms 130))))
  ;; Hours to HMS, and back again
  (should
   (equal "1:01:30"
	  (org-timer-secs-to-hms 3690)))
  (should
   (equal 3690
	  (org-timer-hms-to-secs (org-timer-secs-to-hms 3690))))
  ;; Negative seconds to HMS, and back again
  (should
   (equal "-1:01:30"
	  (org-timer-secs-to-hms -3690)))
  (should
   (equal -3690
	  (org-timer-hms-to-secs (org-timer-secs-to-hms -3690)))))

(ert-deftest test-org-timer/fix-incomplete ()
  "Test conversion to complete HMS format."
  ;; No fix is needed.
  (should
   (equal "1:02:03"
	  (org-timer-fix-incomplete "1:02:03")))
  ;; Hour is missing.
  (should
   (equal "0:02:03"
	  (org-timer-fix-incomplete "02:03")))
  ;; Minute is missing.
  (should
   (equal "0:00:03"
	  (org-timer-fix-incomplete "03"))))

(ert-deftest test-org-timer/change-times ()
  "Test changing HMS format by offset."
  ;; Add time.
  (should
   (equal "
1:31:15
4:00:55"
	  (org-test-with-temp-text "
0:00:25
2:30:05"
	    (org-timer-change-times-in-region (point-min) (point-max)
					      "1:30:50")
	    (buffer-string))))
  ;; Subtract time.
  (should
   (equal "
-1:30:25
0:59:15"
	  (org-test-with-temp-text "
0:00:25
2:30:05"
	    (org-timer-change-times-in-region (point-min) (point-max)
					      "-1:30:50")
	    (buffer-string)))))


;;; Timers

;; Dummy times for overriding `current-time'
(defvar test-org-timer/time0 '(21635 62793 797149 675000))
;; Add 3 minutes and 26 seconds.
(defvar test-org-timer/time1
  (time-add test-org-timer/time0 (seconds-to-time 206)))
;; Add 2 minutes and 41 seconds (6 minutes and 7 seconds total).
(defvar test-org-timer/time2
  (time-add test-org-timer/time1 (seconds-to-time 161)))
;; Add 4 minutes and 55 seconds (11 minutes and 2 seconds total).
(defvar test-org-timer/time3
  (time-add test-org-timer/time2 (seconds-to-time 295)))

(ert-deftest test-org-timer/start-relative ()
  "Test starting relative timer."
  ;; Insert plain timer string, starting with `org-timer-start'.
  (should
   (equal "0:03:26"
	  (test-org-timer/with-temp-text ""
	    (test-org-timer/with-current-time test-org-timer/time0
	      (org-timer-start))
	    (test-org-timer/with-current-time test-org-timer/time1
	      (org-timer))
	    (org-trim (buffer-string)))))
  ;; Insert item timer string.
  (should
   (equal "- 0:03:26 ::"
	  (test-org-timer/with-temp-text ""
	    (test-org-timer/with-current-time test-org-timer/time0
	      (org-timer-start))
	    (test-org-timer/with-current-time test-org-timer/time1
	      (org-timer-item))
	    (org-trim (buffer-string)))))
  ;; Start with `org-timer'.
  (should
   (equal "0:00:00 0:03:26"
	  (test-org-timer/with-temp-text ""
	    (test-org-timer/with-current-time test-org-timer/time0
	      (org-timer))
	    (test-org-timer/with-current-time test-org-timer/time1
	      (org-timer))
	    (org-trim (buffer-string)))))
  ;; Restart with `org-timer'.
  (should
   (equal "0:00:00"
	  (test-org-timer/with-temp-text ""
	    (test-org-timer/with-current-time test-org-timer/time0
	      (org-timer-start))
	    (test-org-timer/with-current-time test-org-timer/time1
	      (org-timer '(4)))
	    (org-trim (buffer-string))))))

(ert-deftest test-org-timer/set-timer ()
  "Test setting countdown timer."
  (should
   (equal "0:06:34"
	  (test-org-timer/with-temp-text ""
	    (test-org-timer/with-current-time test-org-timer/time0
	      (org-timer-set-timer 10))
	    (test-org-timer/with-current-time test-org-timer/time1
	      (org-timer))
	    (org-trim (buffer-string)))))
  (should
   (equal "0:00:04"
	  (test-org-timer/with-temp-text ""
	    (test-org-timer/with-current-time test-org-timer/time0
	      (org-timer-set-timer "3:30"))
	    (test-org-timer/with-current-time test-org-timer/time1
	      (org-timer))
	    (org-trim (buffer-string))))))

(ert-deftest test-org-timer/pause-timer ()
  "Test pausing relative and countdown timers."
  ;; Pause relative timer.
  (should
   (equal "0:03:26"
	  (test-org-timer/with-temp-text ""
	    (test-org-timer/with-current-time test-org-timer/time0
	      (org-timer-start))
	    (test-org-timer/with-current-time test-org-timer/time1
	      (org-timer-pause-or-continue))
	    (org-timer)
	    (org-trim (buffer-string)))))
  ;; Pause then continue relative timer.
  (should
   (equal "0:08:21"
	  (test-org-timer/with-temp-text ""
	    (test-org-timer/with-current-time test-org-timer/time0
	      (org-timer-start))
	    (test-org-timer/with-current-time test-org-timer/time1
	      (org-timer-pause-or-continue))
	    (test-org-timer/with-current-time test-org-timer/time2
	      (org-timer-pause-or-continue))
	    (test-org-timer/with-current-time test-org-timer/time3
	      (org-timer))
	    (org-trim (buffer-string)))))
  ;; Pause then continue countdown timer.
  (should
   (equal "0:01:39"
	  (test-org-timer/with-temp-text ""
	    (test-org-timer/with-current-time test-org-timer/time0
	      (org-timer-set-timer 10))
	    (test-org-timer/with-current-time test-org-timer/time1
	      (org-timer-pause-or-continue))
	    (test-org-timer/with-current-time test-org-timer/time2
	      (org-timer-pause-or-continue))
	    (test-org-timer/with-current-time test-org-timer/time3
	      (org-timer))
	    (org-trim (buffer-string))))))

(ert-deftest test-org-timer/stop ()
  "Test stopping relative and countdown timers."
  ;; Stop running relative timer.
  (test-org-timer/with-temp-text ""
    (test-org-timer/with-current-time test-org-timer/time0
      (org-timer-start))
    (test-org-timer/with-current-time test-org-timer/time1
      (org-timer-stop))
    (should-not org-timer-start-time))
  ;; Stop paused relative timer.
  (test-org-timer/with-temp-text ""
    (test-org-timer/with-current-time test-org-timer/time0
      (org-timer-start))
    (test-org-timer/with-current-time test-org-timer/time1
      (org-timer-pause-or-continue)
      (org-timer-stop))
    (should-not org-timer-start-time)
    (should-not org-timer-pause-time))
  ;; Stop running countdown timer.
  (test-org-timer/with-temp-text ""
    (test-org-timer/with-current-time test-org-timer/time0
      (org-timer-set-timer 10))
    (test-org-timer/with-current-time test-org-timer/time1
      (org-timer-stop))
    (should-not org-timer-start-time)
    (should-not org-timer-countdown-timer))
  ;; Stop paused countdown timer.
  (test-org-timer/with-temp-text ""
    (test-org-timer/with-current-time test-org-timer/time0
      (org-timer-set-timer 10))
    (test-org-timer/with-current-time test-org-timer/time1
      (org-timer-pause-or-continue)
      (org-timer-stop))
    (should-not org-timer-start-time)
    (should-not org-timer-pause-time)
    (should-not org-timer-countdown-timer)))

(ert-deftest test-org-timer/other-timer-error ()
  "Test for error when other timer running."
  ;; Relative timer is running.
  (should-error
   (test-org-timer/with-temp-text ""
     (org-timer-start)
     (org-timer-set-timer 10))
   :type (list 'error 'user-error))
  ;; Countdown timer is running.
  (should-error
   (test-org-timer/with-temp-text ""
     (org-timer-set-timer 10)
     (org-timer-start))
   :type (list 'error 'user-error)))

(ert-deftest test-org-timer/set-timer-from-effort-prop ()
  "Test timer setting from effort property."
  (should
   (< (* 60 9) 				; 9m
      (test-org-timer/with-temp-text
       "* foo
:PROPERTIES:
:Effort:   10
:END:"
       (org-mode)
       (org-timer-set-timer)
       (org-timer-hms-to-secs (org-timer nil t)))
      (1+ (* 60 10))			; 10m 1s
      )))


(provide 'test-org-timer)
;;; test-org-timer.el end here
