;;; test-org-agenda.el --- Tests for org-agenda.el -*- lexical-binding: t ; -*-

;; Copyright (C) 2017, 2019 Marco Wahl

;; Author: Marco Wahl <marcowahlsoft@gmail.com>

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

;;; Commentary:

;; Unit tests for Org Agenda.

;;; Code:

(require 'org-test)
(require 'org-agenda)
(eval-and-compile (require 'cl-lib))


;; General auxiliaries

(defun org-test-agenda--agenda-buffers ()
  "Return agenda buffers in a list."
  (cl-remove-if-not (lambda (x)
		      (with-current-buffer x
			(eq major-mode 'org-agenda-mode)))
		    (buffer-list)))

(defun org-test-agenda--kill-all-agendas ()
  "Kill all agenda buffers."
  (mapc #'kill-buffer
	(org-test-agenda--agenda-buffers)))


;; Test the Agenda

(ert-deftest test-org-agenda/empty ()
  "Empty agenda."
  (cl-assert (not org-agenda-sticky) nil "precondition violation")
  (cl-assert (not (org-test-agenda--agenda-buffers))
	     nil "precondition violation")
  (let ((org-agenda-span 'day)
        org-agenda-files)
    (org-agenda-list)
    (set-buffer org-agenda-buffer-name)
    (should (= 2 (count-lines (point-min) (point-max)))))
  (org-test-agenda--kill-all-agendas))

(ert-deftest test-org-agenda/one-line ()
  "One informative line in the agenda."
  (cl-assert (not org-agenda-sticky) nil "precondition violation")
  (cl-assert (not (org-test-agenda--agenda-buffers))
	     nil "precondition violation")
  (let ((org-agenda-span 'day)
	(org-agenda-files `(,(expand-file-name "examples/agenda-file.org"
					       org-test-dir))))
    (org-agenda-list nil  "<2017-03-10 Fri>")
    (set-buffer org-agenda-buffer-name)
    (should (= 3 (count-lines (point-min) (point-max)))))
  (org-test-agenda--kill-all-agendas))

(ert-deftest test-org-agenda/scheduled-non-todo ()
  "One informative line in the agenda from scheduled non-todo-keyword-item."
  (cl-assert (not org-agenda-sticky) nil "precondition violation")
  (cl-assert (not (org-test-agenda--agenda-buffers))
	     nil "precondition violation")
  (let ((org-agenda-span 'day)
	(org-agenda-files `(,(expand-file-name "examples/agenda-file.org"
					       org-test-dir))))
    (org-agenda-list nil "<2017-07-19 Wed>")
    (set-buffer org-agenda-buffer-name)
    (should
     (progn (goto-line 3)
	    (looking-at " *agenda-file:Scheduled: *test agenda"))))
  (org-test-agenda--kill-all-agendas))

(ert-deftest test-org-agenda/set-priority ()
  "One informative line in the agenda. Check that org-agenda-priority updates the agenda."
  (cl-assert (not org-agenda-sticky) nil "precondition violation")
  (cl-assert (not (org-test-agenda--agenda-buffers))
	     nil "precondition violation")
  (let ((org-agenda-span 'day)
	(org-agenda-files `(,(expand-file-name "examples/agenda-file.org"
					       org-test-dir))))
    (org-agenda-list nil "<2017-07-19 Wed>")
    (set-buffer org-agenda-buffer-name)

    (should
     (progn (goto-line 3)
	    (org-agenda-priority ?B)
	    (looking-at-p " *agenda-file:Scheduled: *\\[#B\\] test agenda"))))
  (org-test-agenda--kill-all-agendas))

(ert-deftest test-org-agenda/sticky-agenda-name ()
  "Agenda buffer name after having created one sticky agenda buffer."
  (cl-assert (not org-agenda-sticky) nil "precondition violation")
  (cl-assert (not (org-test-agenda--agenda-buffers))
	     nil "precondition violation")
  (let ((org-agenda-span 'day)
	(buf (get-buffer org-agenda-buffer-name))
        org-agenda-files)
    (when buf (kill-buffer buf))
    (dolist (fn '(org-agenda-list org-todo-list))
      (org-test-with-temp-text "<2017-03-17 Fri>"
			       (org-follow-timestamp-link)) ;creates a sticky agenda
      (org-test-agenda--kill-all-agendas)
      (funcall fn)
      (should (= 1 (length (org-test-agenda--agenda-buffers))))
      (should (string= "*Org Agenda*"
		       (buffer-name (car (org-test-agenda--agenda-buffers)))))))
  (org-test-agenda--kill-all-agendas))

(ert-deftest test-org-agenda/sticky-agenda-name-after-reload ()
  "Agenda buffer name of sticky agenda after reload."
  (cl-assert (not org-agenda-sticky) nil "precondition violation")
  (cl-assert (not (org-test-agenda--agenda-buffers))
	     nil "precondition violation")
  (org-toggle-sticky-agenda)
  (let (org-agenda-files)
    (org-agenda-list)
    (let* ((agenda-buffer-name
	    (progn
	      (cl-assert (= 1 (length (org-test-agenda--agenda-buffers))))
	      (buffer-name (car (org-test-agenda--agenda-buffers))))))
      (set-buffer agenda-buffer-name)
      (org-agenda-redo)
      (should (= 1 (length (org-test-agenda--agenda-buffers))))
      (should (string= agenda-buffer-name
                       (buffer-name (car (org-test-agenda--agenda-buffers)))))))
  (org-toggle-sticky-agenda)
  (org-test-agenda--kill-all-agendas))


;; agenda redo

(require 'face-remap)

(ert-deftest test-org-agenda/rescale ()
  "Text scale survives `org-agenda-redo'."
  (org-test-agenda--kill-all-agendas)
  (unwind-protect
      (let ((org-agenda-span 'day)
         org-agenda-files)
     (org-agenda-list)
     (set-buffer org-agenda-buffer-name)
     (text-scale-mode)
     (text-scale-set 11)
     (cl-assert (and (boundp text-scale-mode) text-scale-mode))
     (org-agenda-redo)
     (should text-scale-mode)
     (should (= 11 text-scale-mode-amount)))
   (org-test-agenda--kill-all-agendas)))


(ert-deftest test-org-agenda/diary-inclusion ()
  "Diary inclusion happens."
  (org-test-agenda--kill-all-agendas)
  (let ((diary-file (expand-file-name "examples/diary-file" org-test-dir))
	(org-agenda-files `(,(expand-file-name "examples/agenda-file.org"
					       org-test-dir)))
	(diary-date-forms '((month "[-/]" day "[^-/0-9]")
			    (year "[-/]" month "[-/]" day "[^0-9]")
			    (monthname " *" day "[^-0-9]")
			    (year " *" monthname " *" day "[^0-9]")
			    (dayname "\\W")))
	(org-agenda-span 'day)
	(org-agenda-include-diary t))
    (org-agenda-list nil "<2019-01-08>")
    (should (search-forward "f0bcf0cd8bad93c1451bb6e1b2aaedef5cce7cbb" nil t))
    (org-test-agenda--kill-all-agendas)))


(provide 'test-org-agenda)

;;; test-org-agenda.el ends here
