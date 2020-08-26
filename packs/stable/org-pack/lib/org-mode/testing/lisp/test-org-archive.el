;;; test-org-archive.el --- Test for Org Archive     -*- lexical-binding: t; -*-

;; Copyright (C) 2017, 2019  Jay Kamat

;; Author: Jay Kamat <jaygkamat@gmail.com>

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

(ert-deftest test-org-archive/update-status-cookie ()
  "Test archiving properly updating status cookies."
  ;; Test org-archive-subtree with two children.
  (should
   (equal
    "Top [0%]"
    (org-test-with-temp-text-in-file
	"* Top [%]\n** DONE One\n** TODO Two"
      (forward-line)
      (org-archive-subtree)
      (forward-line -1)
      (org-element-property :title (org-element-at-point)))))
  ;; Test org-archive-subtree with one child.
  (should
   (equal
    "Top [100%]"
    (org-test-with-temp-text-in-file "* Top [%]\n** TODO Two"
      (forward-line)
      (org-archive-subtree)
      (forward-line -1)
      (org-element-property :title (org-element-at-point)))))
  ;; Test org-archive-to-archive-sibling with two children.
  (should
   (equal
    "Top [100%]"
    (org-test-with-temp-text "* Top [%]\n<point>** TODO One\n** DONE Two"
      (org-archive-to-archive-sibling)
      (forward-line -1)
      (org-element-property :title (org-element-at-point)))))
  ;; Test org-archive-to-archive-sibling with two children.
  (should
   (equal
    "Top [0%]"
    (org-test-with-temp-text "* Top [%]\n<point>** DONE Two"
      (org-archive-to-archive-sibling)
      (forward-line -1)
      (org-element-property :title (org-element-at-point))))))

(ert-deftest test-org-archive/to-archive-sibling ()
  "Test `org-archive-to-archive-sibling' specifications."
  ;; Archive sibling before or after archive heading.
  (should
   (equal "* Archive :ARCHIVE:\n** H\n"
	  (org-test-with-temp-text "* H\n* Archive :ARCHIVE:\n"
	    (let ((org-archive-sibling-heading "Archive")
		  (org-archive-tag "ARCHIVE"))
	      (org-archive-to-archive-sibling)
	      (goto-char (point-min))
	      (buffer-substring-no-properties
	       (point) (line-beginning-position 3))))))
  (should
   (equal "* Archive :ARCHIVE:\n** H\n"
	  (org-test-with-temp-text "* Archive :ARCHIVE:\n<point>* H\n"
	    (let ((org-archive-sibling-heading "Archive")
		  (org-archive-tag "ARCHIVE"))
	      (org-archive-to-archive-sibling)
	      (goto-char (point-min))
	      (buffer-substring-no-properties
	       (point) (line-beginning-position 3))))))
  ;; When there is no sibling archive heading, create it.
  (should
   (equal "* Archive :ARCHIVE:\n** H\n"
	  (org-test-with-temp-text "* H\n"
	    (let ((org-archive-sibling-heading "Archive")
		  (org-archive-tag "ARCHIVE")
		  (org-tags-column 1))
	      (org-archive-to-archive-sibling)
	      (goto-char (point-min))
	      (buffer-substring-no-properties
	       (point) (line-beginning-position 3))))))
  ;; Ignore non-sibling archive headings.
  (should
   (equal "* Archive :ARCHIVE:\n* Top\n** Archive :ARCHIVE:\n*** H\n"
	  (org-test-with-temp-text "* Archive :ARCHIVE:\n* Top\n<point>** H\n"
	    (let ((org-archive-sibling-heading "Archive")
		  (org-archive-tag "ARCHIVE")
		  (org-tags-column 0))
	      (org-archive-to-archive-sibling)
	      (goto-char (point-min))
	      (buffer-substring-no-properties
	       (point) (line-beginning-position 5))))))
  ;; When archiving a heading, leave point on next heading.
  (should
   (equal "* H2"
	  (org-test-with-temp-text "* H1\n* H2\n* Archive :ARCHIVE:\n"
	    (let ((org-archive-sibling-heading "Archive")
		  (org-archive-tag "ARCHIVE"))
	      (org-archive-to-archive-sibling)
	      (buffer-substring-no-properties (point) (line-end-position))))))
  (should
   (equal "* H2"
	  (org-test-with-temp-text "* Archive :ARCHIVE:\n<point>* H1\n* H2\n"
	    (let ((org-archive-sibling-heading "Archive")
		  (org-archive-tag "ARCHIVE"))
	      (org-archive-to-archive-sibling)
	      (buffer-substring-no-properties (point) (line-end-position))))))
  ;; If `org-archive-reversed-order' is nil, archive as the last
  ;; child.  Otherwise, archive as the first one.
  (should
   (equal "* Archive :ARCHIVE:\n** A\n"
	  (org-test-with-temp-text "* H\n* Archive :ARCHIVE:\n** A\n"
	    (let ((org-archive-sibling-heading "Archive")
		  (org-archive-tag "ARCHIVE")
		  (org-archive-reversed-order nil))
	      (org-archive-to-archive-sibling)
	      (goto-char (point-min))
	      (buffer-substring-no-properties
	       (point) (line-beginning-position 3))))))
  (should
   (equal "* Archive :ARCHIVE:\n** H\n"
	  (org-test-with-temp-text "* H\n* Archive :ARCHIVE:\n** A\n"
	    (let ((org-archive-sibling-heading "Archive")
		  (org-archive-tag "ARCHIVE")
		  (org-archive-reversed-order t))
	      (org-archive-to-archive-sibling)
	      (goto-char (point-min))
	      (buffer-substring-no-properties
	       (point) (line-beginning-position 3)))))))

(provide 'test-org-archive)
;;; test-org-archive.el ends here
