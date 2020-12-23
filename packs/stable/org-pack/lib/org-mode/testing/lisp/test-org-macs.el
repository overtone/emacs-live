;;; test-org-macs.el --- Tests for Org Macs library  -*- lexical-binding: t; -*-

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


;;; String manipulation

(ert-deftest test-org/split-string ()
  "Test `org-split-string' specifications."
  ;; Regular test.
  (should (equal '("a" "b") (org-split-string "a b" " ")))
  ;; Empty parts are not removed.
  (should (equal '("a" "" "b") (org-split-string "a||b" "|")))
  ;; However, empty parts at beginning or end of string are removed.
  (should (equal '("a" "b") (org-split-string "|a|b|" "|")))
  ;; Pathological case: call on an empty string.  Since empty parts
  ;; are not removed, it shouldn't return nil.
  (should (equal '("") (org-split-string "")))
  ;; SEPARATORS, when non-nil, is a regexp.  In particular, do not
  ;; match more than specified.
  (should-not (equal '("a" "b") (org-split-string "a    b" " ")))
  ;; When nil, SEPARATORS matches any number of blank characters.
  (should (equal '("a" "b") (org-split-string "a \t\nb"))))

(ert-deftest test-org/string-width ()
  "Test `org-string-width' specifications."
  (should (= 1 (org-string-width "a")))
  (should (= 0 (org-string-width "")))
  ;; Ignore invisible characters.
  (should (= 0 (org-string-width #("a" 0 1 (invisible t)))))
  (should (= 1 (org-string-width #("ab" 0 1 (invisible t)))))
  (should (= 1 (org-string-width #("ab" 1 2 (invisible t)))))
  (should (= 3 (org-string-width
		#("abcde" 1 2 (invisible t) 3 4 (invisible t)))))
  ;; Check if `invisible' value really means invisibility.
  (should (= 0 (let ((buffer-invisibility-spec t))
                 (org-string-width #("a" 0 1 (invisible foo))))))
  (should (= 0 (let ((buffer-invisibility-spec '(foo)))
                 (org-string-width #("a" 0 1 (invisible foo))))))
  (should (= 0 (let ((buffer-invisibility-spec '((foo . t))))
                 (org-string-width #("a" 0 1 (invisible foo))))))
  (should (= 1 (let ((buffer-invisibility-spec '(bar)))
                 (org-string-width #("a" 0 1 (invisible foo))))))
  ;; Check `display' property.
  (should (= 3 (org-string-width #("a" 0 1 (display "abc")))))
  (should (= 5 (org-string-width #("1a3" 1 2 (display "abc")))))
  ;; `display' string can also contain invisible characters.
  (should (= 4 (org-string-width
		#("123" 1 2 (display #("abc" 1 2 (invisible t)))))))
  ;; Test `space' property in `display'.
  (should (= 2 (org-string-width #(" " 0 1 (display (space :width 2)))))))


;;; Regexp

(ert-deftest test-org/in-regexp ()
  "Test `org-in-regexp' specifications."
  ;; Standard tests.
  (should
   (org-test-with-temp-text "xx ab<point>c xx"
     (org-in-regexp "abc")))
  (should-not
   (org-test-with-temp-text "xx abc <point>xx"
     (org-in-regexp "abc")))
  ;; Return non-nil even with multiple matching regexps in the same
  ;; line.
  (should
   (org-test-with-temp-text "abc xx ab<point>c xx"
     (org-in-regexp "abc")))
  ;; With optional argument NLINES, check extra lines around point.
  (should-not
   (org-test-with-temp-text "A\nB<point>\nC"
     (org-in-regexp "A\nB\nC")))
  (should
   (org-test-with-temp-text "A\nB<point>\nC"
     (org-in-regexp "A\nB\nC" 1)))
  (should-not
   (org-test-with-temp-text "A\nB\nC<point>"
     (org-in-regexp "A\nB\nC" 1)))
  ;; When optional argument VISUALLY is non-nil, return nil if at
  ;; regexp boundaries.
  (should
   (org-test-with-temp-text "xx abc<point> xx"
     (org-in-regexp "abc")))
  (should-not
   (org-test-with-temp-text "xx abc<point> xx"
     (org-in-regexp "abc" nil t))))

(provide 'test-org-macs)
;;; test-org-macs.el ends here
