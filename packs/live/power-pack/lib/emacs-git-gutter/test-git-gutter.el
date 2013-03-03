;;; test-git-gutter.el --- Test for git-gutter.el

;; Copyright (C) 2013 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>

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

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'ert)
(require 'git-gutter)

(ert-deftest git-gutter:root-directory ()
  "helper function `git-gutter:root-directory'"
  (let ((expected (expand-file-name default-directory))
        (got (git-gutter:root-directory)))
    (should (string= expected got)))

  ;; temporary directory maybe be version-controled
  (let ((default-directory temporary-file-directory))
    (should (null (git-gutter:root-directory))))

  ;; Files in .git/ directory are not version-controled
  (let ((default-directory (concat default-directory ".git/")))
    (should (null (git-gutter:root-directory)))))

(ert-deftest git-gutter:sign-width ()
  "helper function `git-gutter:sign-width'"
  (let ((got1 (git-gutter:sign-width "a"))
        (got2 (git-gutter:sign-width "0123456789")))
    (should (= got1 1))
    (should (= got2 10))))

(ert-deftest git-gutter:select-face ()
  "helper function `git-gutter:select-face'"
  (loop for (type . expected) in '((added . git-gutter:added)
                                   (modified . git-gutter:modified)
                                   (deleted . git-gutter:deleted))
        do
        (should (eq (git-gutter:select-face type) expected)))
  (should (not (git-gutter:select-face 'not-found))))

(ert-deftest git-gutter:select-sign ()
  "helper function `git-gutter:select-sign'"
  (loop for (type . expected) in '((added . "+") (modified . "=") (deleted . "-"))
        do
        (should (string= (git-gutter:select-sign type) expected)))
  (should (not (git-gutter:select-sign 'not-found))))

(ert-deftest git-gutter:propertized-sign ()
  "helper function `git-gutter:propertized-sign'"
  (should (string= (git-gutter:propertized-sign 'added) "+")))

(ert-deftest git-gutter:changes-to-number ()
  "helper function `git-gutter:changes-to-number'"
  (should (= (git-gutter:changes-to-number "") 1))
  (should (= (git-gutter:changes-to-number "123") 123)))

(ert-deftest git-gutter:make-diffinfo ()
  "helper function `git-gutter:make-diffinfo'"
  (let ((diffinfo1 (git-gutter:make-diffinfo 'added "diff1" 10 20))
        (diffinfo2 (git-gutter:make-diffinfo 'deleted "diff2" 5)))
    (loop for (prop . expected) in '((:type . added)
                                     (:start-line . 10) (:end-line . 20))
          do
          (should (eql (plist-get diffinfo1 prop) expected)))
    (should (string= (plist-get diffinfo1 :content) "diff1"))
    (loop for (prop . expected) in '((:type . deleted)
                                     (:start-line . 5) (:end-line . nil))
          do
          (should (eql (plist-get diffinfo2 prop) expected)))
    (should (string= (plist-get diffinfo2 :content) "diff2"))))

(ert-deftest git-gutter:in-git-repository-p ()
  "Should return nil if default-directory does not exist"

  ;; In git repository, but here is '.git'
  (let ((buf (find-file-noselect ".git/config")))
    (with-current-buffer buf
      (should (null (git-gutter:in-git-repository-p)))))

  (let ((default-directory (file-name-directory (locate-library "git-gutter"))))
    (should (git-gutter:in-git-repository-p))))

(ert-deftest git-gutter ()
  "Should return nil if buffer does not related with file or file is not existed"
  (with-current-buffer (get-buffer-create "*not-related-file*")
    (should (null (git-gutter))))
  (let ((buf (find-file-noselect "not-found")))
    (with-current-buffer buf
      (should (null (git-gutter))))))

;;; test-git-gutter.el end here
