;;; test-org-tempo.el --- Tests for test-org-tempo.el     -*- lexical-binding: t; -*-

;; Copyright (C) 2017, 2019 Rasmus Pank Roulund

;; Author: Rasmus Pank Roulund <emacs at pank dot eu>

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

(require 'org-tempo)

(unless (featurep 'org-tempo)
  (signal 'missing-test-dependency "org-tempo"))

(ert-deftest test-org-tempo/completion ()
  "Test that blocks and keywords are expanded correctly by org-tempo."
  ;; Tempo completion should recognize snippet keywords and expand with tab
  (should
   (equal (org-test-with-temp-text "<L<point>"
	    (org-tempo-setup)
	    (tempo-complete-tag)
	    (buffer-string))
	  "#+latex: "))
  ;; Tempo completion should recognize snippet Blocks
  (should
   (equal (org-test-with-temp-text "<l<point>"
	    (org-tempo-setup)
	    (call-interactively 'org-cycle)
	    (buffer-string))
	  "#+begin_export latex\n\n#+end_export"))
  ;; Tab should work for expansion.
  (should
   (equal (org-test-with-temp-text "<L<point>"
	    (org-tempo-setup)
	    (tempo-complete-tag)
	    (buffer-string))
	  (org-test-with-temp-text "<L<point>"
	    (org-tempo-setup)
	    (org-cycle)
	    (buffer-string))))
  ;; Tempo should not expand unknown snippets
  (equal (org-test-with-temp-text "<k"
	    (org-tempo-setup)
	    (call-interactively 'org-cycle)
	    (buffer-string))
	 "<k"))

(ert-deftest test-org-tempo/space-first-line ()
  "Test space on first line after expansion."
  ;; Normal blocks should have no space at the end of the first line.
  (should (zerop
	   (org-test-with-temp-text "<l<point>"
	     (org-tempo-setup)
	     (tempo-complete-tag)
	     (goto-char (point-min))
	     (end-of-line)
	     (skip-chars-backward " "))))
  ;; src blocks, export blocks and keywords should have one space at
  ;; the end of the first line.
  (should (cl-every (apply-partially 'eq 1)
		    (mapcar (lambda (s)
			      (org-test-with-temp-text (format "<%s<point>" s)
				(org-tempo-setup)
				(tempo-complete-tag)
				(goto-char (point-min))
				(end-of-line)
				(abs (skip-chars-backward " "))))
			    '("s" "E" "L")))))

(ert-deftest test-org-tempo/cursor-placement ()
  "Test the placement of the cursor after tempo expand"
  ;; Normal blocks place point "inside" block.
  (should
   (eq (org-test-with-temp-text "<l<point>"
	  (org-tempo-setup)
	  (tempo-complete-tag)
	  (point))
       (length "#\\+begin_export latex\n")))
  ;; Special block stop at end of #+begin line.
  (should
   (eq (org-test-with-temp-text "<s<point>"
	  (org-tempo-setup)
	  (tempo-complete-tag)
	  (point))
       (length "#\\+begin_src "))))

(ert-deftest test-org-tempo/add-new-templates ()
  "Test that new structures and keywords are added correctly."
  ;; New blocks should be added.
  (should
   (let ((org-structure-template-alist '(("n" . "new_block"))))
     (org-tempo-add-templates)
     (assoc "<l" org-tempo-tags)))
  ;; New keys should be added.
  (should
   (let ((org-tempo-keywords-alist '(("N" . "new_keyword"))))
     (org-tempo-add-templates)
     (assoc "<N" org-tempo-tags))))

(provide 'test-org-tempo)
;;; test-org-tempo.el end here
