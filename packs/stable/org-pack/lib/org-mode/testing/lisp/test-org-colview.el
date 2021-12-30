;;; test-org-colview.el --- Tests for org-colview.el -*- lexical-binding: t; -*-

;; Copyright (C) 2016, 2019  Nicolas Goaziou

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

;;; Column view

(require 'cl-lib)

(ert-deftest test-org-colview/get-format ()
  "Test `org-columns-get-format' specifications."
  ;; Without any clue, use `org-columns-default-format'.
  (should
   (equal "%A"
	  (org-test-with-temp-text "* H"
	    (let ((org-columns-default-format "%A"))
	      (org-columns-get-format)))))
  ;; If COLUMNS keyword is set, use it.
  (should
   (equal "%B"
	  (org-test-with-temp-text "#+COLUMNS: %B\n* H"
	    (let ((org-columns-default-format "%A"))
	      (org-columns-get-format)))))
  (should
   (equal "%B"
	  (org-test-with-temp-text "#+columns: %B\n* H"
	    (let ((org-columns-default-format "%A"))
	      (org-columns-get-format)))))
  (should
   (equal "%B"
	  (org-test-with-temp-text "* H\n#+COLUMNS: %B"
	    (let ((org-columns-default-format "%A"))
	      (org-columns-get-format)))))
  ;; When :COLUMNS: property is set somewhere in the tree, use it over
  ;; the previous ways.
  (should
   (equal
    "%C"
    (org-test-with-temp-text
	"#+COLUMNS: %B\n* H\n:PROPERTIES:\n:COLUMNS: %C\n:END:\n** S\n<point>"
      (let ((org-columns-default-format "%A"))
	(org-columns-get-format)))))
  ;; When optional argument is provided, prefer it.
  (should
   (equal
    "%D"
    (org-test-with-temp-text
	"#+COLUMNS: %B\n* H\n:PROPERTIES:\n:COLUMNS: %C\n:END:\n** S\n<point>"
      (let ((org-columns-default-format "%A"))
	(org-columns-get-format "%D"))))))

(ert-deftest test-org-colview/columns-scope ()
  "Test `org-columns' scope."
  ;; Before the first headline, view all document.
  (should
   (equal
    '("H1" "H2" "H3")
    (org-test-with-temp-text "Top\n* H1\n** H2\n* H3"
      (let ((org-columns-default-format "%ITEM")) (org-columns))
      (org-map-entries
       (lambda () (get-char-property (point) 'org-columns-value))))))
  ;; When :COLUMNS: is set up in the hierarchy, view tree starting
  ;; there.
  (should
   (equal
    '(nil "H2" "H3" nil)
    (org-test-with-temp-text
	"* H1\n** H2\n:PROPERTIES:\n:COLUMNS: %ITEM\n:END:\n*** <point>H3\n* H4"
      (let ((org-columns-default-format "%ITEM")) (org-columns))
      (org-map-entries
       (lambda () (get-char-property (point) 'org-columns-value))))))
  ;; Otherwise, view tree starting at the current headline.
  (should
   (equal
    '(nil "H2" "H3" nil)
    (org-test-with-temp-text "Top\n* H1\n** <point>H2\n*** H3\n* H4"
      (let ((org-columns-default-format "%ITEM")) (org-columns))
      (org-map-entries
       (lambda () (get-char-property (point) 'org-columns-value))))))
  ;; With a non-nil prefix argument, always view all document.
  (should
   (equal
    '("H1" "H2" "H3" "H4")
    (org-test-with-temp-text
	"* H1\n** H2\n:PROPERTIES:\n:COLUMNS: %ITEM\n:END:\n*** <point>H3\n* H4"
      (let ((org-columns-default-format "%ITEM")) (org-columns t))
      (org-map-entries
       (lambda () (get-char-property (point) 'org-columns-value))))))
  (should
   (equal
    '("1" "1")
    (org-test-with-temp-text
	"Top\n* H1\n** <point>H2\n:PROPERTIES:\n:A: 1\n:END:"
      (let ((org-columns-default-format "%A{+}")) (org-columns t))
      (org-map-entries
       (lambda () (get-char-property (point) 'org-columns-value)))))))

(ert-deftest test-org-colview/columns-width ()
  "Test `org-columns' column widths."
  ;; When a width is specified in the format, use it.
  (should
   (= 9
      (org-test-with-temp-text "* H"
	(let ((org-columns-default-format "%9ITEM")) (org-columns))
	(aref org-columns-current-maxwidths 0))))
  ;; Otherwise, use the width of the largest value in the column.
  (should
   (= 2
      (org-test-with-temp-text
	  "* H\n:PROPERTIES:\n:P: X\n:END:\n** H2\n:PROPERTIES:\n:P: XX\n:END:"
	(let ((org-columns-default-format "%P")) (org-columns))
	(aref org-columns-current-maxwidths 0))))
  ;; If the title is wider than the widest value, use title width
  ;; instead.
  (should
   (= 4
      (org-test-with-temp-text "* H"
	(let ((org-columns-default-format "%ITEM")) (org-columns))
	(aref org-columns-current-maxwidths 0))))
  ;; Special case: stars do count for ITEM.
  (should
   (= 6
      (org-test-with-temp-text "* Head"
	(let ((org-columns-default-format "%ITEM")) (org-columns))
	(aref org-columns-current-maxwidths 0))))
  ;; Special case: width takes into account link narrowing in ITEM.
  (should
   (equal
    '("* 123" . 5)
    (org-test-with-temp-text "* [[https://orgmode.org][123]]"
      (let ((org-columns-default-format "%ITEM")) (org-columns))
      (cons (get-char-property (point) 'org-columns-value-modified)
	    (aref org-columns-current-maxwidths 0)))))
  ;; When a value is too wide for the current column, add ellipses.
  ;; Take into consideration length of `org-columns-ellipses'.
  (should
   (equal "123.. |"
	  (org-test-with-temp-text "* H\n:PROPERTIES:\n:P: 123456\n:END:"
	    (let ((org-columns-default-format "%5P")
		  (org-columns-ellipses ".."))
	      (org-columns))
	    (org-trim (get-char-property (point) 'display)))))
  (should
   (equal "1234… |"
	  (org-test-with-temp-text "* H\n:PROPERTIES:\n:P: 123456\n:END:"
	    (let ((org-columns-default-format "%5P")
		  (org-columns-ellipses "…"))
	      (org-columns))
	    (org-trim (get-char-property (point) 'display))))))

(ert-deftest test-org-colview/columns-summary ()
  "Test `org-columns' summary types."
  ;; {+} and {+;format} add numbers.
  (should
   (equal
    "3"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: 1
:END:
** S1
:PROPERTIES:
:A: 2
:END:"
      (let ((org-columns-default-format "%A{+}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  (should
   (equal
    "3.0"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: 1
:END:
** S1
:PROPERTIES:
:A: 2
:END:"
      (let ((org-columns-default-format "%A{+;%.1f}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  ;; {$} is a shortcut for {+;%.2f}.
  (should
   (equal
    "3.60"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: 1.50
:END:
** S1
:PROPERTIES:
:A: 2.10
:END:"
      (let ((org-columns-default-format "%A{$}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  ;; Obey to format string even in leaf values.
  (should
   (equal
    "1.0"
    (org-test-with-temp-text
	"* H
:PROPERTIES:
:A: 1
:END:"
      (let ((org-columns-default-format "%A{+;%.1f}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  ;; {:} sums times.  Plain numbers are minutes.
  (should
   (equal
    "4:10"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: 1:30
:END:
** S1
:PROPERTIES:
:A: 2:40
:END:"
      (let ((org-columns-default-format "%A{:}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  (should
   (equal
    "1:32"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: 1:30
:END:
** S1
:PROPERTIES:
:A: 2
:END:"
      (let ((org-columns-default-format "%A{:}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  ;; {X}, {X/} and {X%} indicate checkbox status.
  (should
   (equal
    "[ ]"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: [ ]
:END:
** S1
:PROPERTIES:
:A: [ ]
:END:"
      (let ((org-columns-default-format "%A{X}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  (should
   (equal
    "[-]"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: [ ]
:END:
** S1
:PROPERTIES:
:A: [X]
:END:"
      (let ((org-columns-default-format "%A{X}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  (should
   (equal
    "[X]"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: [X]
:END:
** S1
:PROPERTIES:
:A: [X]
:END:"
      (let ((org-columns-default-format "%A{X}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  (should
   (equal
    "[1/2]"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: [ ]
:END:
** S1
:PROPERTIES:
:A: [X]
:END:"
      (let ((org-columns-default-format "%A{X/}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  (should
   (equal
    "[50%]"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: [ ]
:END:
** S1
:PROPERTIES:
:A: [X]
:END:"
      (let ((org-columns-default-format "%A{X%}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  ;; {X/} handles recursive summaries.
  (should
   (equal
    "[1/2]"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: [ ]
:END:
** S2
*** S21
:PROPERTIES:
:A: [X]
:END:
*** S22
:PROPERTIES:
:A: [X]
:END:"
      (let ((org-columns-default-format "%A{X/}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  (should
   (equal
    "[1/2]"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: [X]
:END:
** S2
*** S21
:PROPERTIES:
:A: [ ]
:END:
*** S22
:PROPERTIES:
:A: [ ]
:END:"
      (let ((org-columns-default-format "%A{X/}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  ;; {X%} handles recursive summaries.
  (should
   (equal
    "[50%]"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: [ ]
:END:
** S2
*** S21
:PROPERTIES:
:A: [X]
:END:
*** S22
:PROPERTIES:
:A: [X]
:END:"
      (let ((org-columns-default-format "%A{X%}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  (should
   (equal
    "[50%]"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: [X]
:END:
** S2
*** S21
:PROPERTIES:
:A: [ ]
:END:
*** S22
:PROPERTIES:
:A: [ ]
:END:"
      (let ((org-columns-default-format "%A{X%}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  ;; {min} is the smallest number in column, {max} the largest one.
  ;; {mean} is the arithmetic mean of numbers in column.
  (should
   (equal
    "42"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: 99
:END:
** S1
:PROPERTIES:
:A: 42
:END:"
      (let ((org-columns-default-format "%A{min}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  (should
   (equal
    "99"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: 99
:END:
** S1
:PROPERTIES:
:A: 42
:END:"
      (let ((org-columns-default-format "%A{max}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  (should
   (equal
    "51.0"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: 60
:END:
** S1
:PROPERTIES:
:A: 42
:END:"
      (let ((org-columns-default-format "%A{mean}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  ;; {:min}, {:max} and {:mean} apply to time values.
  (should
   (equal
    "1:20"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: 4:40
:END:
** S1
:PROPERTIES:
:A: 1:20
:END:"
      (let ((org-columns-default-format "%A{:min}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  (should
   (equal
    "4:40"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: 4:40
:END:
** S1
:PROPERTIES:
:A: 1:20
:END:"
      (let ((org-columns-default-format "%A{:max}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  (should
   (equal
    "3:00"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: 4:40
:END:
** S1
:PROPERTIES:
:A: 1:20
:END:"
      (let ((org-columns-default-format "%A{:mean}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  ;; {@min}, {@max} and {@mean} apply to ages.
  (should
   (equal
    "0min"
    (org-test-at-time "<2014-03-04 Tue>"
      (org-test-with-temp-text
	  "* H
** S1
:PROPERTIES:
:A: <2012-03-29 Thu>
:END:
** S1
:PROPERTIES:
:A: <2014-03-04 Tue>
:END:"
	(let ((org-columns-default-format "%A{@min}")) (org-columns))
	(get-char-property (point) 'org-columns-value-modified)))))
  (should
   (equal
    "2d"
    (org-test-at-time "<2014-03-04 Tue>"
      (org-test-with-temp-text
	  "* H
** S1
:PROPERTIES:
:A: <2014-03-03 Mon>
:END:
** S1
:PROPERTIES:
:A: <2014-03-02 Sun>
:END:"
	(let ((org-columns-default-format "%A{@max}")) (org-columns))
	(get-char-property (point) 'org-columns-value-modified)))))
  (should
   (equal
    "1d 12h"
    (org-test-at-time "<2014-03-04 Tue>"
      (org-test-with-temp-text
	  "* H
** S1
:PROPERTIES:
:A: <2014-03-03 Mon>
:END:
** S1
:PROPERTIES:
:A: <2014-03-02 Sun>
:END:"
	(let ((org-columns-default-format "%A{@mean}")) (org-columns))
	(get-char-property (point) 'org-columns-value-modified)))))
  ;; If a time value is expressed as a duration, return a duration.
  ;; If any of them follows H:MM:SS pattern, use it too.  Also handle
  ;; combinations of duration and H:MM:SS patterns.
  (should
   (equal
    "3d 4:20"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: 3d 3h
:END:
** S1
:PROPERTIES:
:A: 1:20
:END:"
      (let ((org-columns-default-format "%A{:}")
	    (org-duration-units '(("d" . 1440) ("h" . 60)))
	    (org-duration-format '(("d" . nil) (special . h:mm))))
	(org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  (should
   (equal
    "6:00:10"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: 4:40:10
:END:
** S1
:PROPERTIES:
:A: 1:20
:END:"
      (let ((org-columns-default-format "%A{:}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  (should
   (equal
    "3d 4:20"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: 3d 3h
:END:
** S1
:PROPERTIES:
:A: 0d 1:20
:END:"
      (let ((org-columns-default-format "%A{:}")
	    (org-duration-units '(("d" . 1440) ("h" . 60)))
	    (org-duration-format '(("d" . nil) (special . h:mm))))
	(org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  ;; @min, @max and @mean also accept regular duration.
  (should
   (equal
    "1d 10h"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: 1d 10h 0min
:END:
** S1
:PROPERTIES:
:A: 5d 3h
:END:"
      (let ((org-columns-default-format "%A{@min}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  ;; {est+} gives a low-high estimate using mean and standard
  ;; deviation.
  (should
   (equal
    "3-17"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: 0-10
:END:
** S1
:PROPERTIES:
:A: 0-10
:END:"
      (let ((org-columns-default-format "%A{est+}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  ;; When using {est+} summary, a single number is understood as
  ;; a degenerate range.
  (should
   (equal
    "4-4"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: 4
:END:
"
      (let ((org-columns-default-format "%A{est+}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  ;; Allow custom summary types.
  (should
   (equal
    "1|2"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: 1
:END:
** S1
:PROPERTIES:
:A: 2
:END:"
      (let ((org-columns-summary-types
	     '(("custom" . (lambda (s _) (mapconcat #'identity s "|")))))
	    (org-columns-default-format "%A{custom}")) (org-columns))
      (get-char-property (point) 'org-columns-value-modified))))
  ;; Allow custom _collect_ for summary types.
  (should
   (equal
    "2"
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: 1
:END:
** S1
:PROPERTIES:
:A: 2
:A-OK: 1
:END:"
     (let ((org-columns-summary-types
	    '(("custom" org-columns--summary-sum
	       (lambda (p)
                 (if (equal "1" (org-entry-get nil (format "%s-OK" p)))
		     (org-entry-get nil p)
		   "")))))
	   (org-columns-default-format "%A{custom}")) (org-columns))
     (get-char-property (point) 'org-columns-value-modified))))
  ;; Allow custom collect function to be used for different columns
  (should
   (equal
    '("2" "1")
    (org-test-with-temp-text
     "* H
** S1
:PROPERTIES:
:A: 1
:B: 1
:B-OK: 1
:END:
** S1
:PROPERTIES:
:A: 2
:B: 2
:A-OK: 1
:END:"
     (let ((org-columns-summary-types
	    '(("custom" org-columns--summary-sum
	       (lambda (p)
                 (if (equal "1" (org-entry-get nil (format "%s-OK" p)))
		     (org-entry-get nil p)
		   "")))))
	   (org-columns-default-format "%A{custom} %B{custom}")) (org-columns))
     (list (get-char-property (point) 'org-columns-value-modified)
	   (get-char-property (1+ (point)) 'org-columns-value-modified)))))
  ;; Allow multiple summary types applied to the same property.
  (should
   (equal
    '("42" "99")
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: 99
:END:
** S1
:PROPERTIES:
:A: 42
:END:"
      (let ((org-columns-default-format "%A{min} %A{max}")) (org-columns))
      (list (get-char-property (point) 'org-columns-value-modified)
	    (get-char-property (1+ (point)) 'org-columns-value-modified)))))
  ;; Allow mixing both summarized and non-summarized columns for
  ;; a property.  However, the first column takes precedence and
  ;; updates the value.
  (should
   (equal
    '("1000" "42")
    (org-test-with-temp-text
	"* H
:PROPERTIES:
:A: 1000
:END:
** S1
:PROPERTIES:
:A: 99
:END:
** S1
:PROPERTIES:
:A: 42
:END:"
      (let ((org-columns-default-format "%A %A{min}")) (org-columns))
      (list (get-char-property (point) 'org-columns-value-modified)
	    (get-char-property (1+ (point)) 'org-columns-value-modified)))))
  (should
   (equal
    '("42" "42")
    (org-test-with-temp-text
	"* H
:PROPERTIES:
:A: 1000
:END:
** S1
:PROPERTIES:
:A: 99
:END:
** S1
:PROPERTIES:
:A: 42
:END:"
      (let ((org-columns-default-format "%A{min} %A")) (org-columns))
      (list (get-char-property (point) 'org-columns-value-modified)
	    (get-char-property (1+ (point)) 'org-columns-value-modified))))))

(ert-deftest test-org-colview/columns-new ()
  "Test `org-columns-new' specifications."
  ;; Insert new column at the left of the current one.
  (should
   (equal '("FOO" "ITEM")
	  (org-test-with-temp-text "* H"
	    (let ((org-columns-default-format "%ITEM")) (org-columns))
	    (org-columns-new nil "FOO" "FOO" nil nil nil)
	    (list (get-char-property (point) 'org-columns-key)
		  (get-char-property (1+ (point)) 'org-columns-key)))))
  (should
   (equal '("ITEM" "FOO" "BAR")
	  (org-test-with-temp-text "* H"
	    (let ((org-columns-default-format "%ITEM %BAR")) (org-columns))
	    (forward-char)
	    (org-columns-new nil "FOO" "FOO" nil nil nil)
	    (list (get-char-property (1- (point)) 'org-columns-key)
		  (get-char-property (point) 'org-columns-key)
		  (get-char-property (1+ (point)) 'org-columns-key)))))
  ;; Update #+COLUMNS keyword if needed.
  (should
   (equal "#+COLUMNS: %FOO %ITEM"
	  (org-test-with-temp-text "#+COLUMNS: %ITEM\n<point>* H"
	    (let ((org-columns-default-format "%ITEM")) (org-columns))
	    (org-columns-new nil "FOO" "FOO" nil nil nil)
	    (goto-char (point-min))
	    (buffer-substring-no-properties (point) (line-end-position)))))
  (should
   (equal "#+COLUMNS: %ITEM %FOO %BAR"
	  (org-test-with-temp-text "#+COLUMNS: %ITEM %BAR\n<point>* H"
	    (let ((org-columns-default-format "%ITEM %BAR")) (org-columns))
	    (forward-char)
	    (org-columns-new nil "FOO" "FOO" nil nil nil)
	    (goto-char (point-min))
	    (buffer-substring-no-properties (point) (line-end-position)))))
  ;; Mind case when updating #+COLUMNS.
  (should
   (equal "#+COLUMNS: %ITEM %Foo %BAR"
	  (org-test-with-temp-text "#+COLUMNS: %ITEM %BAR\n<point>* H"
	    (let ((org-columns-default-format "%ITEM %BAR")) (org-columns))
	    (forward-char)
	    (org-columns-new nil "Foo" "Foo" nil nil nil)
	    (goto-char (point-min))
	    (buffer-substring-no-properties (point) (line-end-position)))))
  (should
   (equal "#+columns: %ITEM %Foo %BAR"
	  (org-test-with-temp-text "#+columns: %ITEM %BAR\n<point>* H"
	    (let ((org-columns-default-format "%ITEM %BAR")) (org-columns))
	    (forward-char)
	    (org-columns-new nil "Foo" "Foo" nil nil nil)
	    (goto-char (point-min))
	    (buffer-substring-no-properties (point) (line-end-position)))))
  ;; Also update :COLUMNS: properties.
  (should
   (equal "%FOO %ITEM"
	  (org-test-with-temp-text "* H\n:PROPERTIES:\n:COLUMNS: %ITEM\n:END:"
	    (let ((org-columns-default-format "%ITEM")) (org-columns))
	    (org-columns-new nil "FOO" "FOO" nil nil nil)
	    (org-entry-get nil "COLUMNS"))))
  ;; If no keyword nor any property is available, insert one.
  (should
   (string-match-p (regexp-quote "#+COLUMNS: %FOO %ITEM")
		   (org-test-with-temp-text "* H"
		     (let ((org-columns-default-format "%ITEM")) (org-columns))
		     (org-columns-new nil "FOO" "FOO" nil nil nil)
		     (buffer-string)))))

(ert-deftest test-org-colview/columns-update ()
  "Test `org-columns-update' specifications."
  ;; Update display.
  (should
   (equal
    "12    |"
    (org-test-with-temp-text
	"* H
:PROPERTIES:
:A: 1
:END:
"
      (let ((org-columns-default-format "%5A")) (org-columns))
      (search-forward "1")
      (insert "2")
      (org-columns-update "A")
      (get-char-property (point-min) 'display))))
  ;; Update is case-insensitive.
  (should
   (equal
    "12    |"
    (org-test-with-temp-text
	"* H
:PROPERTIES:
:A: 1
:END:
"
      (let ((org-columns-default-format "%5A")) (org-columns))
      (search-forward "1")
      (insert "2")
      (org-columns-update "a")
      (get-char-property (point-min) 'display))))
  ;; Update stored values.
  (should
   (equal
    '("12" "12")
    (org-test-with-temp-text
	"* H
:PROPERTIES:
:A: 1
:END:
"
      (let ((org-columns-default-format "%5A")) (org-columns))
      (search-forward "1")
      (insert "2")
      (org-columns-update "A")
      (list (get-char-property (point-min) 'org-columns-value)
	    (get-char-property (point-min) 'org-columns-value-modified)))))
  ;; When multiple columns are using the same property, value is
  ;; updated according to the specifications of the first one.
  (should
   (equal
    "2"
    (org-test-with-temp-text
	"* H
:PROPERTIES:
:A: 1
:END:
** S
:PROPERTIES:
:A: 2
:END:"
      (let ((org-columns-default-format "%A{min} %A")) (org-columns))
      (org-columns-update "A")
      (org-entry-get nil "A"))))
  (should
   (equal
    "1"
    (org-test-with-temp-text
	"* H
:PROPERTIES:
:A: 1
:END:
** S
:PROPERTIES:
:A: 2
:END:"
      (let ((org-columns-default-format "%A %A{min}")) (org-columns))
      (org-columns-update "A")
      (org-entry-get nil "A"))))
  ;; Ensure modifications propagate in upper levels even when multiple
  ;; summary types apply to the same property.
  (should
   (equal
    '("1" "22")
    (org-test-with-temp-text
	"* H
** S1
:PROPERTIES:
:A: 1
:END:
** S2
:PROPERTIES:
:A: <point>2
:END:"
      (save-excursion
	(goto-char (point-min))
	(let ((org-columns-default-format "%A{min} %A{max}")) (org-columns)))
      (insert "2")
      (org-columns-update "A")
      (list (get-char-property 1 'org-columns-value)
	    (get-char-property 2 'org-columns-value-modified)))))
  ;; Ensure additional processing is done (e.g., ellipses, special
  ;; keywords fontification...).
  (should
   (equal
    "ve.. |"
    (org-test-with-temp-text
	"* H
:PROPERTIES:
:A: text
:END:
"
      (let ((org-columns-default-format "%4A")
	    (org-columns-ellipses ".."))
	(org-columns))
      (search-forward ":A: ")
      (insert "very long ")
      (org-columns-update "A")
      (get-char-property (point-min) 'display))))
  ;; Values obtained from inline tasks are at the same level as those
  ;; obtained from children of the current node.
  (when (featurep 'org-inlinetask)
    (should
     (equal
      "2"
      (org-test-with-temp-text
	  "* H
*************** Inline task
:PROPERTIES:
:A: 2
:END:
*************** END
** Children
:PROPERTIES:
:A: 3
:END:
"
	(let ((org-columns-default-format "%A{min}")
	      (org-columns-ellipses "..")
	      (org-inlinetask-min-level 15))
	  (org-columns))
	(get-char-property (point-min) 'org-columns-value)))))
  ;; Handle `org-columns-modify-value-for-display-function', even with
  ;; multiple titles for the same property.
  (should
   (equal '("foo" "bar")
	  (org-test-with-temp-text "* H"
	    (let ((org-columns-default-format "%ITEM %ITEM(Name)")
		  (org-columns-modify-value-for-display-function
		   (lambda (title value)
		     (pcase title ("ITEM" "foo") ("Name" "bar") (_ "baz")))))
	      (org-columns))
	    (list (get-char-property 1 'org-columns-value-modified)
		  (get-char-property 2 'org-columns-value-modified))))))

(ert-deftest test-org-colview/columns-move-left ()
  "Test `org-columns-move-left' specifications."
  ;; Error when trying to move the left-most column.
  (should-error
   (org-test-with-temp-text "* H"
     (let ((org-columns-default-format "%ITEM")) (org-columns))
     (org-columns-move-left)))
  ;; Otherwise, move column to left and update display.
  (should
   (equal '("2" "1")
	  (org-test-with-temp-text "* H\n:PROPERTIES:\n:A: 1\n:B: 2\n:END:"
	    (let ((org-columns-default-format "%A %B")) (org-columns))
	    (forward-char)
	    (org-columns-move-left)
	    (list (get-char-property (point) 'org-columns-value)
		  (get-char-property (1+ (point)) 'org-columns-value)))))
  ;; Handle multiple columns with the same property.
  (should
   (equal '("2" "1")
	  (org-test-with-temp-text
	      "* H
** S1
:PROPERTIES:
:A: 1
:END:
** S1
:PROPERTIES:
:A: 2
:END:"
	    (let ((org-columns-default-format "%ITEM %A{min} %A{max}"))
	      (org-columns))
	    (forward-char 2)
	    (org-columns-move-left)
	    (list (get-char-property (point) 'org-columns-value)
		  (get-char-property (1+ (point)) 'org-columns-value)))))
  ;; Special case: do not update values even if move entails changing
  ;; them.
  (should
   (equal "1"
	  (org-test-with-temp-text
	      "* H
:PROPERTIES:
:A: 1
:END:
** S1
:PROPERTIES:
:A: 99
:END:"
	    (let ((org-columns-default-format "%A %A{max}"))
	      (org-columns))
	    (forward-char)
	    (org-columns-move-left)
	    ;; Since the first column matching a given property
	    ;; determines how a value is computed, the following
	    ;; should return "99" instead.  However, this behavior is
	    ;; in practice surprising so we just ignore the value
	    ;; change.  It can be computed later.
	    (org-entry-get (point) "A")))))

(ert-deftest test-org-colview/columns-move-right ()
  "Test `org-columns-move-right' specifications."
  ;; Error when trying to move the right-most column.
  (should-error
   (org-test-with-temp-text "* H"
     (let ((org-columns-default-format "%ITEM")) (org-columns))
     (org-columns-move-right)))
  ;; Otherwise, move column to left and update display.
  (should
   (equal '("2" "1")
	  (org-test-with-temp-text "* H\n:PROPERTIES:\n:A: 1\n:B: 2\n:END:"
	    (let ((org-columns-default-format "%A %B")) (org-columns))
	    (org-columns-move-right)
	    (list (get-char-property (1- (point)) 'org-columns-value)
		  (get-char-property (point) 'org-columns-value)))))
  ;; Handle multiple columns with the same property.
  (should
   (equal '("2" "1")
	  (org-test-with-temp-text
	      "* H
** S1
:PROPERTIES:
:A: 1
:END:
** S1
:PROPERTIES:
:A: 2
:END:"
	    (let ((org-columns-default-format "%ITEM %A{min} %A{max}"))
	      (org-columns))
	    (forward-char)
	    (org-columns-move-right)
	    (list (get-char-property (1- (point)) 'org-columns-value)
		  (get-char-property (point) 'org-columns-value)))))
  ;; Special case: do not update values even if move entails changing
  ;; them.
  (should
   (equal "1"
	  (org-test-with-temp-text
	      "* H
:PROPERTIES:
:A: 1
:END:
** S1
:PROPERTIES:
:A: 99
:END:"
	    (let ((org-columns-default-format "%A %A{max}"))
	      (org-columns))
	    (org-columns-move-right)
	    ;; See `test-org-colview/columns-move-left' for an
	    ;; explanation.
	    (org-entry-get (point) "A")))))

(ert-deftest test-org-colview/columns-next-allowed-value ()
  "Test `org-columns-next-allowed-value' specifications."
  ;; Cannot shift "ITEM" property.
  (should-error
   (org-test-with-temp-text "* H"
     (let ((org-columns-default-format "%ITEM")) (org-columns))
     (org-columns-next-allowed-value)))
  ;; Throw an error when allowed values are not defined.
  (should-error
   (org-test-with-temp-text "* H\n:PROPERTIES:\n:A: 1\n:END:"
     (let ((org-columns-default-format "%A")) (org-columns))
     (org-columns-next-allowed-value)))
  ;; Throw an error when there's only one value to select.
  (should-error
   (org-test-with-temp-text "* H\n:PROPERTIES:\n:A: 1\n:A_ALL: 1\n:END:"
     (let ((org-columns-default-format "%A")) (org-columns))
     (org-columns-next-allowed-value)))
  ;; By default select the next allowed value.  Where there is no more
  ;; value, start again from first possible one.
  (should
   (equal "2"
	  (org-test-with-temp-text
	      "* H\n:PROPERTIES:\n:A: 1\n:A_ALL: 1 2 3\n:END:"
	    (let ((org-columns-default-format "%A")) (org-columns))
	    (org-columns-next-allowed-value)
	    (org-entry-get (point) "A"))))
  (should
   (equal "3"
	  (org-test-with-temp-text
	      "* H\n:PROPERTIES:\n:A: 2\n:A_ALL: 1 2 3\n:END:"
	    (let ((org-columns-default-format "%A")) (org-columns))
	    (org-columns-next-allowed-value)
	    (org-entry-get (point) "A"))))
  (should
   (equal "1"
	  (org-test-with-temp-text
	      "* H\n:PROPERTIES:\n:A: 3\n:A_ALL: 1 2 3\n:END:"
	    (let ((org-columns-default-format "%A")) (org-columns))
	    (org-columns-next-allowed-value)
	    (org-entry-get (point) "A"))))
  ;; PREVIOUS argument moves backward.
  (should
   (equal "1"
	  (org-test-with-temp-text
	      "* H\n:PROPERTIES:\n:A: 2\n:A_ALL: 1 2 3\n:END:"
	    (let ((org-columns-default-format "%A")) (org-columns))
	    (org-columns-next-allowed-value 'previous)
	    (org-entry-get (point) "A"))))
  (should
   (equal "2"
	  (org-test-with-temp-text
	      "* H\n:PROPERTIES:\n:A: 3\n:A_ALL: 1 2 3\n:END:"
	    (let ((org-columns-default-format "%A")) (org-columns))
	    (org-columns-next-allowed-value 'previous)
	    (org-entry-get (point) "A"))))
  (should
   (equal "3"
	  (org-test-with-temp-text
	      "* H\n:PROPERTIES:\n:A: 1\n:A_ALL: 1 2 3\n:END:"
	    (let ((org-columns-default-format "%A")) (org-columns))
	    (org-columns-next-allowed-value 'previous)
	    (org-entry-get (point) "A"))))
  ;; Select Nth element with optional argument NTH.
  (should
   (equal "1"
	  (org-test-with-temp-text
	      "* H\n:PROPERTIES:\n:A: 2\n:A_ALL: 1 2 3\n:END:"
	    (let ((org-columns-default-format "%A")) (org-columns))
	    (org-columns-next-allowed-value nil 1)
	    (org-entry-get (point) "A"))))
  ;; If NTH is negative, go backwards, 0 being the last one and -1 the
  ;; penultimate.
  (should
   (equal "3"
	  (org-test-with-temp-text
	      "* H\n:PROPERTIES:\n:A: 2\n:A_ALL: 1 2 3\n:END:"
	    (let ((org-columns-default-format "%A")) (org-columns))
	    (org-columns-next-allowed-value nil 0)
	    (org-entry-get (point) "A"))))
  (should
   (equal "2"
	  (org-test-with-temp-text
	      "* H\n:PROPERTIES:\n:A: 2\n:A_ALL: 1 2 3\n:END:"
	    (let ((org-columns-default-format "%A")) (org-columns))
	    (org-columns-next-allowed-value nil -1)
	    (org-entry-get (point) "A"))))
  ;; Throw an error if NTH is greater than the number of allowed
  ;; values.
  (should-error
   (org-test-with-temp-text
       "* H\n:PROPERTIES:\n:A: 2\n:A_ALL: 1 2 3\n:END:"
     (let ((org-columns-default-format "%A")) (org-columns))
     (org-columns-next-allowed-value nil 4)
     (org-entry-get (point) "A")))
  ;; Pathological case: when shifting the value alters the current
  ;; heading, make sure all columns are still at their correct
  ;; location.
  (should
   (equal '("H" "" "" "" "TODO")
	  (let ((org-todo-keywords '((sequence "TODO" "DONE"))))
	    (org-test-with-temp-text "* H"
	      (let ((org-columns-default-format "%ITEM %A %B %C %TODO"))
		(org-columns)
		(forward-char 4)
		(org-columns-next-allowed-value)
		(list (get-char-property (- (point) 4) 'org-columns-value)
		      (get-char-property (- (point) 3) 'org-columns-value)
		      (get-char-property (- (point) 2) 'org-columns-value)
		      (get-char-property (- (point) 1) 'org-columns-value)
		      (get-char-property (point) 'org-columns-value)))))))
  (should
   (equal '("H" "VERYLONGTODO")
	  (let ((org-todo-keywords '((sequence "TODO" "VERYLONGTODO"))))
	    (org-test-with-temp-text "* TODO H"
	      (let ((org-columns-default-format "%ITEM %TODO"))
		(org-columns)
		(forward-char)
		(org-columns-next-allowed-value)
		(list (get-char-property (- (point) 1) 'org-columns-value)
		      (get-char-property (point) 'org-columns-value))))))))



;;; Dynamic block

(ert-deftest test-org-colview/dblock ()
  "Test the column view table."
  (should
   (equal
    "#+BEGIN: columnview
| ITEM |
|------|
| H    |
#+END:"
    (org-test-with-temp-text
        "* H\n<point>#+BEGIN: columnview\n#+END:"
      (let ((org-columns-default-format "%ITEM")) (org-update-dblock))
      (buffer-substring-no-properties (point) (point-max)))))
  (should
   (equal
    "#+BEGIN: columnview
| ITEM | A |
|------+---|
| H    | 1 |
#+END:"
    (org-test-with-temp-text
        "* H\n:PROPERTIES:\n:A: 1\n:END:\n<point>#+BEGIN: columnview\n#+END:"
      (let ((org-columns-default-format "%ITEM %A")) (org-update-dblock))
      (buffer-substring-no-properties (point) (point-max)))))
  ;; Properties are case insensitive.
  (should
   (equal
    "#+BEGIN: columnview
| a |
|---|
| 1 |
#+END:"
    (org-test-with-temp-text
        "* H\n:PROPERTIES:\n:A: 1\n:END:\n<point>#+BEGIN: columnview\n#+END:"
      (let ((org-columns-default-format "%a")) (org-update-dblock))
      (buffer-substring-no-properties (point) (point-max)))))
  ;; Test titles given to columns.
  (should
   (equal
    "#+BEGIN: columnview
| Name | Prop |
|------+------|
| H    |    1 |
#+END:"
    (org-test-with-temp-text
        "* H\n:PROPERTIES:\n:A: 1\n:END:\n<point>#+BEGIN: columnview\n#+END:"
      (let ((org-columns-default-format "%ITEM(Name) %A(Prop)"))
        (org-update-dblock))
      (buffer-substring-no-properties (point) (point-max)))))
  ;; Test `:id' parameter
  (should
   (equal
    "#+BEGIN: columnview :id local
| ITEM |
|------|
| H1   |
| H1.1 |
#+END:
"
    (org-test-with-temp-text
        "* H1\n<point>#+BEGIN: columnview :id local\n#+END:\n** H1.1\n* H2"
      (let ((org-columns-default-format "%ITEM")) (org-update-dblock))
      (buffer-substring-no-properties (point) (outline-next-heading)))))
  (should
   (equal
    "#+BEGIN: columnview :id global
| ITEM |
|------|
| H1   |
| H1.1 |
| H2   |
#+END:
"
    (org-test-with-temp-text
        "\n* H1\n<point>#+BEGIN: columnview :id global\n#+END:\n** H1.1\n* H2"
      (let ((org-columns-default-format "%ITEM")) (org-update-dblock))
      (buffer-substring-no-properties (point) (outline-next-heading)))))
  ;; Test `:hlines' parameter.
  (should
   (equal
    "#+BEGIN: columnview :hlines t :id global
| ITEM |
|------|
| H    |
|------|
| H2   |
|------|
| H2.1 |
#+END:\n"
    (org-test-with-temp-text
        "
* H
<point>#+BEGIN: columnview :hlines t :id global
#+END:
* H2
** H2.1"
      (let ((org-columns-default-format "%ITEM")) (org-update-dblock))
      (buffer-substring-no-properties (point) (outline-next-heading)))))
  (should
   (equal
    "#+BEGIN: columnview :hlines 1 :id global
| ITEM |
|------|
| H    |
|------|
| H2   |
| H2.1 |
#+END:\n"
    (org-test-with-temp-text
        "
* H
<point>#+BEGIN: columnview :hlines 1 :id global
#+END:
* H2
** H2.1"
      (let ((org-columns-default-format "%ITEM")) (org-update-dblock))
      (buffer-substring-no-properties (point) (outline-next-heading)))))
  (should
   (equal
    "#+BEGIN: columnview :hlines 1 :id \"id\"
| ITEM |
|------|
| H2   |
| H2.1 |
#+END:
"
    (org-test-with-temp-text
	"
* H
<point>#+BEGIN: columnview :hlines 1 :id \"id\"
#+END:
* H2
:PROPERTIES:
:ID: id
:END:
** H2.1"
      (let ((org-columns-default-format "%ITEM")) (org-update-dblock))
      (buffer-substring-no-properties (point) (outline-next-heading)))))
  (should
   (equal
    "#+BEGIN: columnview :hlines 1 :id id
| ITEM |
|------|
| H2   |
| H2.1 |
#+END:
"
    (org-test-with-temp-text
	"
* H
<point>#+BEGIN: columnview :hlines 1 :id id
#+END:
* H2
:PROPERTIES:
:ID: id
:END:
** H2.1"
      (let ((org-columns-default-format "%ITEM")) (org-update-dblock))
      (buffer-substring-no-properties (point) (outline-next-heading)))))
  ;; Test `:indent' parameter.
  (should
   (equal
    "#+BEGIN: columnview :indent t
| ITEM     |
|----------|
| H1       |
| \\_  H1.1 |
#+END:
"
    (org-test-with-temp-text
        "* H1\n<point>#+BEGIN: columnview :indent t\n#+END:\n** H1.1"
      (let ((org-columns-default-format "%ITEM")) (org-update-dblock))
      (buffer-substring-no-properties (point) (outline-next-heading)))))
  (should
   (equal
    "#+BEGIN: columnview :indent t
| Prop | Name     |
|------+----------|
|      | H1       |
|      | \\_  H1.1 |
#+END:
"
    (org-test-with-temp-text
        "* H1\n<point>#+BEGIN: columnview :indent t\n#+END:\n** H1.1"
      (let ((org-columns-default-format "%A(Prop) %ITEM(Name)"))
        (org-update-dblock))
      (buffer-substring-no-properties (point) (outline-next-heading)))))
  ;; Test `:vlines' parameter.
  (should
   (equal
    "#+BEGIN: columnview :vlines t
|   | ITEM | A  |
|---+------+----|
|   | H    | 1  |
| / | <>   | <> |
#+END:"
    (org-test-with-temp-text
        "* H\n:PROPERTIES:\n:A: 1\n:END:\n<point>#+BEGIN: columnview :vlines t\n#+END:"
      (let ((org-columns-default-format "%ITEM %A")) (org-update-dblock))
      (buffer-substring-no-properties (point) (point-max)))))
  ;; Test `:skip-empty-rows' parameter.
  (should
   (equal
    "#+BEGIN: columnview :skip-empty-rows t
| ITEM | A |
|------+---|
| H1.1 | 1 |
#+END:
"
    (org-test-with-temp-text
        "
* H1
<point>#+BEGIN: columnview :skip-empty-rows t
#+END:
** H1.1
:PROPERTIES:
:A: 1
:END:"
      (let ((org-columns-default-format "%ITEM %A")) (org-update-dblock))
      (buffer-substring-no-properties (point) (outline-next-heading)))))
  ;; Test `:exclude-tags' parameter.
  (should
   (equal
    "#+BEGIN: columnview :exclude-tags (\"excludeme\")
| ITEM | A |
|------+---|
| H1   |   |
#+END:
"
    (org-test-with-temp-text
        "
* H1
<point>#+BEGIN: columnview :exclude-tags (\"excludeme\")
#+END:
** H1.1 :excludeme:
:PROPERTIES:
:A: 1
:END:"
      (let ((org-columns-default-format "%ITEM %A")) (org-update-dblock))
      (buffer-substring-no-properties (point) (outline-next-heading)))))
  ;; Test `:format' parameter.
  (should
   (equal
    "#+BEGIN: columnview :format \"%ITEM(Name)\"
| Name |
|------|
| H    |
#+END:"
    (org-test-with-temp-text
        "* H\n<point>#+BEGIN: columnview :format \"%ITEM(Name)\"\n#+END:"
      (let ((org-columns-default-format "%ITEM")) (org-update-dblock))
      (buffer-substring-no-properties (point) (point-max)))))
  ;; When inserting ITEM values, make sure to clean sensitive
  ;; contents, like unique targets or forbidden inline src-blocks.
  (should
   (equal
    "#+BEGIN: columnview
| ITEM |
|------|
| H 1  |
#+END:"
    (org-test-with-temp-text
        "* H <<target>> 1\n<point>#+BEGIN: columnview\n#+END:"
      (let ((org-columns-default-format "%ITEM")) (org-update-dblock))
      (buffer-substring-no-properties (point) (point-max)))))
  (should
   (equal
    "#+BEGIN: columnview
| ITEM |
|------|
| H 1  |
#+END:"
    (org-test-with-temp-text
        "* H src_emacs-lisp{(+ 1 1)} 1\n<point>#+BEGIN: columnview\n#+END:"
      (let ((org-columns-default-format "%ITEM")) (org-update-dblock))
      (buffer-substring-no-properties (point) (point-max)))))
  ;; Active time stamps are displayed as inactive.
  (should
   (equal
    "#+BEGIN: columnview
| ITEM | d                | s                | t                |
|------+------------------+------------------+------------------|
| H    | [2020-05-14 Thu] | [2020-05-11 Mon] | [2020-06-10 Wed] |
#+END:"
    (org-test-with-temp-text
     "* H
SCHEDULED: <2020-05-11 Mon> DEADLINE: <2020-05-14 Thu>
<2020-06-10 Wed>
<point>#+BEGIN: columnview\n#+END:"
     (let ((org-columns-default-format
	    "%ITEM %DEADLINE(d) %SCHEDULED(s) %TIMESTAMP(t)"))
       (org-update-dblock))
     (buffer-substring-no-properties (point) (point-max))))))

(provide 'test-org-colview)
;;; test-org-colview.el ends here
