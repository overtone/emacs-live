;;; test-org-info.el --- Tests for "org-info.el"     -*- lexical-binding: t; -*-

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

(ert-deftest test-org-info/export ()
  "Test `org-info-export' specifications."
  ;; Export to HTML.  Without node, refer to "Top".
  (should
   (equal (org-info-export "filename#node" nil 'html)
	  "<a href=\"filename.html#node\">filename#node</a>"))
  (should
   (equal (org-info-export "filename" nil 'html)
	  "<a href=\"filename.html#Top\">filename</a>"))
  ;; When exporting to HTML, ensure node names are expanded according
  ;; to (info "(texinfo) HTML Xref Node Name Expansion").
  (should
   (equal "_005f"
	  (let ((name (org-info-export "#_" nil 'html)))
	    (and (string-match "#\\(.*\\)\"" name)
		 (match-string 1 name)))))
  (should
   (equal "_002d"
	  (let ((name (org-info-export "#-" nil 'html)))
	    (and (string-match "#\\(.*\\)\"" name)
		 (match-string 1 name)))))
  (should
   (equal "A-node"
	  (let ((name (org-info-export "#A node" nil 'html)))
	    (and (string-match "#\\(.*\\)\"" name)
		 (match-string 1 name)))))
  (should
   (equal "A-node-_002d_002d_002d-with-_005f_0027_0025"
	  (let ((name (org-info-export "#A  node --- with _'%" nil 'html)))
	    (and (string-match "#\\(.*\\)\"" name)
		 (match-string 1 name)))))
  ;; Export to Texinfo.  Without a node name, refer to "Top".
  (should
   (equal (org-info-export "filename" nil 'texinfo)
	  "@ref{Top,,,filename,}"))
  (should
   (equal (org-info-export "filename#node" nil 'texinfo)
	  "@ref{node,,,filename,}")))



(provide 'test-org-info)
;;; test-org-info.el ends here
