;;; test-org-bbdb.el --- tests for org-bbdb.el                        -*- lexical-binding: t; -*-

;; Copyright (C) 2018, 2019  Marco Wahl

;; Author:  <marcowahlsoft@gmail.com>
;; Keywords: calendar

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

;; Test some of org-bbdb.el.

;;; Code:

(require 'ol-bbdb)

(ert-deftest test-org-bbdb-anniv-extract-date ()
  (should (equal nil (org-bbdb-anniv-extract-date "foo")))
  (should (equal '(9 22 2018) (org-bbdb-anniv-extract-date "2018-09-22")))
  (should (equal '(9 22 nil) (org-bbdb-anniv-extract-date "09-22"))))

(provide 'test-ol-bbdb)

;;; test-org-bbdb.el ends here
