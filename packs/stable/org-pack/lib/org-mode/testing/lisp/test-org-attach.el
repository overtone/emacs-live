;;; test-org-attach.el --- tests for org-attach.el      -*- lexical-binding: t; -*-

;; Copyright (C) 2017, 2019

;; Author: Marco Wahl
;; Keywords: internal

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

;;; Commentary:

;;

;;; Code:

(require 'org-test "../testing/org-test")
(require 'org-attach)
(eval-and-compile (require 'cl-lib))

(ert-deftest test-org-attach/dir ()
  "Test `org-attach-get' specifications."
  (let ((org-file-apps '((t . emacs))))
    (should (equal "Text in fileA\n"
		   (org-test-in-example-file org-test-attachments-file
		     (goto-char 157) ;; First attachment link
		     (org-open-at-point)
		     (buffer-string))))
    (should-not (equal "Text in fileB\n"
		       (org-test-in-example-file org-test-attachments-file
			 (goto-char 219) ;; Second attachment link
			 (let ((org-attach-use-inheritance nil))
			   (org-open-at-point)
			   (buffer-string)))))
    (should (equal "Text in fileB\n"
		   (org-test-in-example-file org-test-attachments-file
		     (goto-char 219) ;; Second attachment link
		     (let ((org-attach-use-inheritance t))
		       (org-open-at-point)
		       (buffer-string)))))
    (should-not (equal "att1"
		       (org-test-in-example-file org-test-attachments-file
			 (goto-char 179) ;; H1.1
			 (let ((org-attach-use-inheritance nil))
			   (org-attach-dir)))))
    (should (equal "att1"
		   (org-test-in-example-file org-test-attachments-file
		     (goto-char 179) ;; H1.1
		     (let ((org-attach-use-inheritance t))
		       (org-attach-dir)))))
    (should (equal '("fileC" "fileD")
		   (org-test-in-example-file org-test-attachments-file
		     (goto-char 239) ;; H1.2
		     (org-attach-file-list (org-attach-dir)))))
    (should (equal '("fileC" "fileD")
		   (org-test-in-example-file org-test-attachments-file
		     (goto-char 239) ;; H1.2
		     (org-attach-file-list (org-attach-dir)))))
    (should (equal '("fileE")
		   (org-test-in-example-file org-test-attachments-file
		     (goto-char 289) ;; H2
		     (let ((org-attach-id-dir "data/"))
		       (org-attach-file-list (org-attach-dir))))))
    (should (equal "peek-a-boo\n"
		   (org-test-in-example-file org-test-attachments-file
		     (goto-char 289) ;; H2
		     (let ((org-attach-id-dir "data/"))
		       (org-attach-open-in-emacs)
		       (buffer-string)))))
    (should (equal  '("fileA" "fileB")
		    (org-test-in-example-file org-test-attachments-file
		      (goto-char 336) ;; H3
		      (org-attach-file-list (org-attach-dir)))))
    ;; Test for folder not initialized in the filesystem
    (should-not (org-test-in-example-file org-test-attachments-file
		  (goto-char 401) ;; H3.1
		  (let ((org-attach-use-inheritance nil)
			(org-attach-id-dir "data/"))
		    (org-attach-dir))))
    ;; Not yet initialized folder should be found if no-fs-check is
    ;; non-nil
    (should (equal "data/ab/cd12345"
		   (org-test-in-example-file org-test-attachments-file
		     (goto-char 401) ;; H3.1
		     (let ((org-attach-use-inheritance nil)
			   (org-attach-id-dir "data/"))
		       (file-relative-name (org-attach-dir nil t))))))
    (should (equal '("fileA" "fileB")
		   (org-test-in-example-file org-test-attachments-file
		     (goto-char 401) ;; H3.1
		     (let ((org-attach-use-inheritance t))
		       ;; This is where it gets a bit sketchy...! DIR always has
		       ;; priority over ID, even if ID is declared "higher up" in the
		       ;; tree.  This can potentially be revised.  But it is also
		       ;; pretty clean.  DIR is always higher in priority than ID right
		       ;; now, no matter the depth in the tree.
		       (org-attach-file-list (org-attach-dir))))))))

(ert-deftest test-org-attach/dired-attach-to-next-best-subtree/1 ()
  "Attach file at point in dired to subtree."
  (should
   (let ((a-filename (make-temp-file "a")) ; file is an attach candidate.
	 (org-attach-id-dir "data/"))
     (unwind-protect
	 (org-test-with-temp-text-in-file
	     "* foo   :foo:"
	   (split-window)
	   (let ((org-buffer (current-buffer))
		 (dired-buffer (dired temporary-file-directory)))
	     (cl-assert (eq 'dired-mode major-mode))
	     (revert-buffer)
	     (dired-goto-file a-filename)
					; action
	     (call-interactively #'org-attach-dired-to-subtree)
					; check
	     (delete-window)
	     (switch-to-buffer org-buffer)
	     (cl-assert (eq 'org-mode major-mode)))
	   (beginning-of-buffer)
	   (search-forward "* foo")
					; expectation.  tag ATTACH has been appended.
	   (cl-reduce (lambda (x y) (or x y))
		      (mapcar (lambda (x) (string-equal "ATTACH" x))
			      (plist-get
			       (plist-get
				(org-element-at-point) 'headline) :tags))))
       (delete-file a-filename)))))

(ert-deftest test-org-attach/dired-attach-to-next-best-subtree/2 ()
  "Attach 2 marked files."
  (should
   (let ((a-filename (make-temp-file "a"))
	 (b-filename (make-temp-file "b")) ; attach candidates.
	 (org-attach-id-dir "data/"))
     (unwind-protect
	 (org-test-with-temp-text-in-file
	  "* foo"
	  (split-window)
	  (let ((org-buffer (current-buffer))
		(dired-buffer (dired temporary-file-directory)))
	    (cl-assert (eq 'dired-mode major-mode))
	    (revert-buffer)
	    (dired-goto-file a-filename)
	    (dired-mark 1)
	    (dired-goto-file b-filename)
	    (dired-mark 1)
					; action
	    (call-interactively #'org-attach-dired-to-subtree)
					; check
	    (delete-window)
	    (switch-to-buffer org-buffer))
	  (cl-assert (eq 'org-mode major-mode))
	  (beginning-of-buffer)
	  (search-forward "* foo")
	  (and (file-exists-p (concat (org-attach-dir) "/"
				      (file-name-nondirectory a-filename)))
	       (file-exists-p (concat (org-attach-dir) "/"
				      (file-name-nondirectory b-filename)))))
       (delete-file a-filename)
       (delete-file b-filename)))))


(provide 'test-org-attach)
;;; test-org-attach.el ends here
