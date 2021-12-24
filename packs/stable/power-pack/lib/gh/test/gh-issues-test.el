;;; gh-issues-test.el --- test fir gh-issues.el

;; Copyright (C) 2012  Yann Hodique

;; Author: Yann Hodique <yann.hodique@gmail.com>
;; Keywords:

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;

;;; Code:

(require 'gh-test)
(require 'gh-issues)

(defun gh-issues-test:test-regular-issue (issue)
  (should (equal (oref issue :number) 1347))
  (should (equal (oref issue :state) "open")))

(ert-deftest gh-issues-test:regular-list ()
  (let* ((api (gh-test-mock-api 'gh-issues-api))
         (issues
          (gh-test-with-traces-buffers ((gists-buf "list_issues_sample.txt"))
            (gh-test-mock-url ((:record-cls mocker-stub-record
                                             :output gists-buf))
                               (oref
                                (gh-issues-issue-list api "octocat"
                                                      "Hello-World")
                                :data)))))
    (should (equal (length issues) 1))
    (let ((issue (car issues)))
      (should (object-of-class-p issue 'gh-issues-issue))
      (gh-issues-test:test-regular-issue issue))))

(provide 'gh-issues-test)
;;; gh-issues-test.el ends here
