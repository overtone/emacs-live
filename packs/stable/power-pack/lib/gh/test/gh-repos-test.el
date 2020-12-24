;;; gh-repos-test.el --- test for gh-repos.el

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
(require 'gh-repos)

(defun gh-repos-test:test-regular-repo (repo)
  (should (equal (oref repo :id) 1296269))
  (should (object-of-class-p (oref repo :owner) 'gh-user)))

(ert-deftest gh-repos-test:regular-get ()
  (let* ((api (gh-test-mock-api 'gh-repos-api))
         (repo
          (gh-test-with-traces-buffers ((repos-buf "get_repo_sample.txt"))
            (gh-test-mock-url ((:record-cls mocker-stub-record
                                             :output repos-buf))
                               (oref (gh-repos-repo-get api "foo") :data)))))
    (should (object-of-class-p repo 'gh-repos-repo))
    (gh-repos-test:test-regular-repo repo)))

(provide 'gh-repos-test)
;;; gh-repos-test.el ends here
