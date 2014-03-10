;;; gh-gist-tests.el --- tests for gh-gist.el

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

(require 'gh-tests)
(require 'gh-gist)

(defun gh-gist-tests:test-regular-gist (gist)
  (should (equal (oref gist :id) "1"))
  (should (oref gist :public))
  (should (equal (length (oref gist :files)) 1)))

(ert-deftest gh-gist-tests:regular-list ()
  (let* ((api (gh-tests-mock-api 'gh-gist-api))
         (gists
          (gh-tests-with-traces-buffers ((gists-buf "list_gists_sample.txt"))
            (gh-tests-mock-url ((:record-cls mocker-stub-record
                                             :output gists-buf))
                               (oref (gh-gist-list api "octocat") :data)))))
    (should (equal (length gists) 1))
    (let ((gist (car gists)))
      (should (object-of-class-p gist 'gh-gist-gist-stub))
      (gh-gist-tests:test-regular-gist gist))))

(ert-deftest gh-gist-tests:regular-get ()
  (let* ((api (gh-tests-mock-api 'gh-gist-api))
         (gist
          (gh-tests-with-traces-buffers ((gist-buf "get_gist_sample.txt"))
            (gh-tests-mock-url ((:record-cls mocker-stub-record
                                             :output gist-buf))
                               (oref (gh-gist-get api "1") :data)))))
    (should (object-of-class-p gist 'gh-gist-gist))
    (gh-gist-tests:test-regular-gist gist)))

(ert-deftest gh-gist-tests:regular-new ()
  (let* ((api (gh-tests-mock-api 'gh-gist-api))
         (gist-stub
          (make-instance 'gh-gist-gist-stub
                         :description "description of gist"
                         :public t
                         :files (list
                                 (make-instance 'gh-gist-gist-file
                                                :filename "ring.erl"
                                                :content "contents of gist"))))
         (gist
          (gh-tests-with-traces-buffers ((gist-buf "get_gist_sample.txt"))
            (gh-tests-mock-url ((:record-cls mocker-stub-record
                                             :output gist-buf))
                               (oref (gh-gist-new api gist-stub) :data)))))
    (should (object-of-class-p gist 'gh-gist-gist))
    (gh-gist-tests:test-regular-gist gist)))

(provide 'gh-gist-tests)
;;; gh-gist-tests.el ends here
