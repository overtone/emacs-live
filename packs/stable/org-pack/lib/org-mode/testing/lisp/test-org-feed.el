;;; test-org-feed.el --- Tests for org-feed.el -*- lexical-binding: t; -*-

;; Copyright (C) 2016, 2019  Michael Brand

;; Author: Michael Brand <michael.ch.brand@gmail.com>

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

;; Unit tests for Org Feed library.

;;; Code:

(require 'org-feed)

(ert-deftest test-org-feed/fill-template ()
  "Test `org-feed-format-entry' template specifications."

  ;; When working on these tests consider to also change
  ;; `test-org-capture/fill-template'.

  ;; %(sexp) placeholder.
  (should
   (equal "success!"
	  (org-feed-format-entry nil "%(concat \"success\" \"!\")" nil)))
  ;; %a placeholder.
  (should
   (equal "[[https://orgmode.org]]\n"
	  (org-feed-format-entry '(:link "https://orgmode.org") "%a" nil)))
  ;; %t and %T placeholders.
  (should
   (equal (format-time-string (org-time-stamp-format nil nil))
	  (org-feed-format-entry nil "%t" nil)))
  (should
   (string-match-p
    "<2016-01-02 \\S-+>"
    (org-feed-format-entry
     '(:pubDate "Sat, 02 Jan 2016 12:00:00 +0000") "%t" nil)))
  (should
   (equal (format-time-string (org-time-stamp-format t nil))
	  (org-feed-format-entry nil "%T" nil)))
  (should
   (string-match-p
    "<2016-01-02 \\S-+ 12:00>"
    (org-feed-format-entry
     '(:pubDate "Sat, 02 Jan 2016 12:00:00 +0000") "%T" nil)))
  ;; %u and %U placeholders.
  (should
   (equal (format-time-string (org-time-stamp-format nil t))
	  (org-feed-format-entry nil "%u" nil)))
  (should
   (string-match-p
    "[2016-01-02 \\S-+]"
    (org-feed-format-entry
     '(:pubDate "Sat, 02 Jan 2016 12:00:00 +0000") "%u" nil)))
  (should
   (equal (format-time-string (org-time-stamp-format t t))
	  (org-feed-format-entry nil "%U" nil)))
  (should
   (string-match-p
    "[2016-01-02 \\S-+ 12:00]"
    (org-feed-format-entry
     '(:pubDate "Sat, 02 Jan 2016 12:00:00 +0000") "%U" nil)))
  ;; %h placeholder.  Make sure sexp placeholders are not expanded
  ;; when they are inserted through this one.
  (should
   (equal "success!"
	  (org-feed-format-entry '(:title "success!") "%h" nil)))
  (should
   (equal "%(concat \"no \" \"evaluation\")"
	  (org-feed-format-entry
	   '(:title "%(concat \"no \" \"evaluation\")") "%h" nil)))
  ;; Test %-escaping with \ character.
  (should
   (equal "%h"
	  (org-feed-format-entry '(:title "success!") "\\%h" nil)))
  (should
   (equal "\\success!"
	  (org-feed-format-entry '(:title "success!") "\\\\%h" nil)))
  (should
   (equal "\\%h"
	  (org-feed-format-entry '(:title "success!") "\\\\\\%h" nil)))
  ;; More than one placeholder in the same template.
  (should
   (equal "success! success! success! success!"
	  (org-feed-format-entry '(:title "success!") "%h %h %h %h" nil)))
  ;; %(sexp) placeholder with an input containing the traps %, ", )
  ;; and \n all at once which is complicated to parse.
  (should
   (equal
    "5 % Less (See\n Item \"3)\" Somewhere)"
    (org-feed-format-entry
     '(:title "5 % less (see\n item \"3)\" somewhere)")
     "%(capitalize \"%h\")" nil))))




(provide 'test-org-feed)
;;; test-org-feed.el ends here
