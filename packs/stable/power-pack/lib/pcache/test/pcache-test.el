;;; pcache-test.el --- tests for pcache.el

;; Copyright (C) 2011  Yann Hodique

;; Author: Yann Hodique <yann.hodique@gmail.com>

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

(require 'ert)
(require 'pcache)

(defmacro pcache-with-repository (var arglist &rest body)
  (declare (indent 2) (debug t))
  `(let ((,var (apply pcache-repository ',arglist)))
     (unwind-protect
         (progn
           ,@body)
       (pcache-destroy-repository ,(car arglist)))))

(ert-deftest pcache-create-repo ()
  (pcache-with-repository repo ("pcache-test/tmp")
    (should (object-of-class-p repo 'pcache-repository))))

(ert-deftest pcache-double-destroy ()
  (pcache-with-repository repo ("pcache-test/tmp")
    (pcache-destroy-repository "pcache-test/tmp")))

(ert-deftest pcache-put-get ()
  (pcache-with-repository repo ("pcache-test/tmp")
    (pcache-put repo 'foo 42)
    (should (eq 42 (pcache-get repo 'foo)))))

(ert-deftest pcache-validate-simple ()
  (pcache-with-repository repo ("pcache-test/tmp")
    (pcache-put repo 'foo 42)
    (should (pcache-validate-repo repo))))

(ert-deftest pcache-get-expired ()
  (pcache-with-repository repo ("pcache-test/tmp")
    (pcache-put repo 'foo 42 1)
    (should (eq 42 (pcache-get repo 'foo)))
    (sleep-for 1)
    (should (null (pcache-get repo 'foo)))))

(ert-deftest pcache-get-invalidated ()
  (pcache-with-repository repo ("pcache-test/tmp")
    (pcache-put repo 'foo 42)
    (should (eq 42 (pcache-get repo 'foo)))
    (pcache-invalidate repo 'foo)
    (should (null (pcache-get repo 'foo)))))

(ert-deftest pcache-put-reload-get ()
  (pcache-with-repository repo ("pcache-test/tmp1")
    (pcache-put repo 'foo 44)
    (pcache-save repo t)
    (with-current-buffer
        (find-file-noselect (concat pcache-directory "pcache-test/tmp1"))
      (goto-char (point-min))
      (while (search-forward "tmp1" nil t)
        (replace-match "tmp2"))
      (write-file (concat pcache-directory "pcache-test/tmp2"))))
  (pcache-with-repository repo ("pcache-test/tmp2")
    (should (eq 44 (pcache-get repo 'foo)))))

(provide 'pcache-test)
;;; pcache-test.el ends here
