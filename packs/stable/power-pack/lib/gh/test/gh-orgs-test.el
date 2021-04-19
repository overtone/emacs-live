;;; gh-orgs-test.el --- test for gh-orgs.el

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
(require 'gh-orgs)

(defun gh-orgs-test:test-regular-org-stub (org)
  (should (equal (oref org :id) 1))
  (should (equal (oref org :login) "github"))
  (should (equal "https://github.com/images/error/octocat_happy.gif" (oref org :avatar-url))))

(defun gh-orgs-test:test-regular-org (org)
  (gh-orgs-test:test-regular-org-stub org)
  (should (equal (oref org :public-gists) 1))
  (should (equal (oref org :public-repos) 2)))

(ert-deftest gh-orgs-test:regular-list ()
  (let* ((api (gh-test-mock-api 'gh-orgs-api))
         (orgs
          (gh-test-with-traces-buffers ((orgs-buf "list_orgs_sample.txt"))
            (gh-test-mock-url ((:record-cls mocker-stub-record
                                             :output orgs-buf))
                               (oref (gh-orgs-list api "dummy") :data)))))
    (should (equal (length orgs) 1))
    (let ((org (car orgs)))
      (should (object-of-class-p org 'gh-orgs-org-stub))
      (gh-orgs-test:test-regular-org-stub org))))

(ert-deftest gh-orgs-test:regular-get ()
  (let* ((api (gh-test-mock-api 'gh-orgs-api))
         (org
          (gh-test-with-traces-buffers ((orgs-buf "get_org_sample.txt"))
            (gh-test-mock-url ((:record-cls mocker-stub-record
                                             :output orgs-buf))
                               (oref (gh-orgs-get api "github") :data)))))
    (should (object-of-class-p org 'gh-orgs-org))
    (gh-orgs-test:test-regular-org org)))

(ert-deftest gh-orgs-test:regular-update ()
  (let* ((api (gh-test-mock-api 'gh-orgs-api))
         (org-stub
          (make-instance 'gh-orgs-org
                         :login "github"
                         :id 1
                         :url "https://api.github.com/orgs/1"
                         :avatar-url "https://github.com/images/error/octocat_happy.gif"))
         (org
          (gh-test-with-traces-buffers ((orgs-buf "get_org_sample.txt"))
            (gh-test-mock-url ((:record-cls mocker-stub-record
                                             :output orgs-buf))
                               (oref (gh-orgs-update api org-stub) :data)))))
    (should (object-of-class-p org 'gh-orgs-org))
    (gh-orgs-test:test-regular-org org)))

(provide 'gh-orgs-test)
;;; gh-orgs-test.el ends here
