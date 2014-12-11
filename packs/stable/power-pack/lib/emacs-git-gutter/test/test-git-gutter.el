;;; test-git-gutter.el --- Test for git-gutter.el

;; Copyright (C) 2014 by Syohei YOSHIDA

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

(require 'ert)
(require 'git-gutter)

;; suppress log message
(setq git-gutter:verbosity 0)

(ert-deftest git-gutter:sign-width ()
  "helper function `git-gutter:sign-width'"
  (let ((got1 (git-gutter:sign-width "a"))
        (got2 (git-gutter:sign-width "0123456789")))
    (should (= got1 1))
    (should (= got2 10))))

(ert-deftest git-gutter:select-face ()
  "helper function `git-gutter:select-face'"
  (cl-loop for (type . expected) in '((added . git-gutter:added)
                                      (modified . git-gutter:modified)
                                      (deleted . git-gutter:deleted))
           do
           (should (eq (git-gutter:select-face type) expected)))
  (should-not (git-gutter:select-face 'not-found)))

(ert-deftest git-gutter:select-sign ()
  "helper function `git-gutter:select-sign'"
  (cl-loop for (type . expected) in '((added . "+") (modified . "=") (deleted . "-"))
           do
           (should (string= (git-gutter:select-sign type) expected)))
  (should-not (git-gutter:select-sign 'not-found)))

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
        (diffinfo2 (git-gutter:make-diffinfo 'deleted "diff2" 5 nil)))
    (cl-loop for (prop . expected) in '((:type . added)
                                        (:start-line . 10) (:end-line . 20))
             do
             (should (eql (plist-get diffinfo1 prop) expected)))
    (should (string= (plist-get diffinfo1 :content) "diff1"))
    (cl-loop for (prop . expected) in '((:type . deleted)
                                        (:start-line . 5) (:end-line . nil))
             do
             (should (eql (plist-get diffinfo2 prop) expected)))
    (should (string= (plist-get diffinfo2 :content) "diff2"))))

(ert-deftest git-gutter:in-git-repository-p ()
  "Should return nil if default-directory does not exist"

  ;; In git repository, but here is '.git'
  (when (file-directory-p ".git") ;; #36
    (let ((buf (find-file-noselect ".git/config")))
      (with-current-buffer buf
        (should-not (git-gutter:in-git-repository-p)))))

  (let ((default-directory (file-name-directory (locate-library "git-gutter"))))
    (should (git-gutter:in-git-repository-p))))

(ert-deftest git-gutter ()
  "Should return nil if buffer does not related with file or file is not existed"
  (with-current-buffer (get-buffer-create "*not-related-file*")
    (should-not (git-gutter)))
  (let ((buf (find-file-noselect "not-found")))
    (with-current-buffer buf
      (should-not (git-gutter)))))

(ert-deftest git-gutter:collect-deleted-line ()
  "Should return lines which start with '-'"
  (let* ((input (mapconcat 'identity
                           (list "-apple" "-melon" "+orange")
                           "\n"))
         (got (git-gutter:collect-deleted-line input)))
    (should (equal got '("apple" "melon")))))

(ert-deftest git-gutter:insert-deleted-lines ()
  "Should insert deleted line"
  (let ((input (mapconcat 'identity
                          (list "-apple" "-melon" "+orange")
                          "\n")))
    (with-temp-buffer
      (git-gutter:insert-deleted-lines input)
      (should (string= (buffer-string)
                       "apple\nmelon\n")))))

(ert-deftest git-gutter:diff-content ()
  "Should return diff hunk"
  (let* ((input "@@-1,1+1,1@@
foo
bar
@@ -2,2 +2,2 @@")
         (got (with-temp-buffer
                (insert input)
                (goto-char (point-min))
                (goto-char (line-end-position))
                (git-gutter:diff-content))))
    (should (string= got "@@-1,1+1,1@@\nfoo\nbar"))))

(ert-deftest git-gutter:set-window-margin ()
  "Should change window margin"
  (git-gutter:set-window-margin 4)
  (let ((got (car (window-margins))))
    (should (= got 4))))

(ert-deftest git-gutter-mode-success ()
  "Case git-gutter-mode enabled"
  (with-current-buffer (find-file-noselect "test-git-gutter.el")
    (git-gutter-mode 1)
    (should git-gutter-mode))
  (kill-buffer "test-git-gutter.el"))

(ert-deftest git-gutter-mode-failed ()
  "Case git-gutter-mode disabled"
  (with-temp-buffer
    (git-gutter-mode 1)
    (should-not git-gutter-mode))

  (let ((default-directory nil))
    (git-gutter-mode 1)
    (should-not git-gutter-mode))

  (let ((default-directory "foo"))
    (git-gutter-mode 1)
    (should-not git-gutter-mode))

  (when (file-directory-p ".git") ;; #36
    (with-current-buffer (find-file-noselect ".git/config")
      (git-gutter-mode 1)
      (should-not git-gutter-mode))))

(ert-deftest global-git-gutter-mode-success ()
  "Case global-git-gutter-mode enabled"
  (with-current-buffer (find-file-noselect "test-git-gutter.el")
    (global-git-gutter-mode t)
    (should git-gutter-mode))

  (kill-buffer "test-git-gutter.el"))

(ert-deftest global-git-gutter-mode-failed ()
  "Case global-git-gutter-mode disabled"

  (with-temp-buffer
    (global-git-gutter-mode t)
    (should-not git-gutter-mode))

  (let ((git-gutter:disabled-modes '(emacs-lisp-mode)))
    (with-current-buffer (find-file-noselect "test-git-gutter.el")
      (global-git-gutter-mode t)
      (should-not git-gutter-mode)))

  (kill-buffer "test-git-gutter.el"))

(ert-deftest git-gutter-git-diff-arguments ()
  "Command line options of `git diff'"

  (let ((git-gutter:diff-option "-a -b -c")
        (file "git-gutter.el"))
    (let ((got (git-gutter:git-diff-arguments file)))
      (should (equal got '("-a" "-b" "-c" "git-gutter.el"))))

    (let* ((git-gutter:start-revision "HEAD")
           (got (git-gutter:git-diff-arguments file)))
      (should (equal got '("-a" "-b" "-c" "HEAD" "git-gutter.el"))))))

(ert-deftest git-gutter-hg-diff-arguments ()
  "Command line options of `hg diff'"

  (let ((git-gutter:mercurial-diff-option "-a -b -c")
        (file "git-gutter.el"))
    (let ((got (git-gutter:hg-diff-arguments file)))
      (should (equal got '("-a" "-b" "-c" "git-gutter.el"))))

    (let* ((git-gutter:start-revision "30000")
           (got (git-gutter:hg-diff-arguments file)))
      (should (equal got '("-a" "-b" "-c" "-r" "30000" "git-gutter.el"))))))

(ert-deftest git-gutter-bzr-diff-arguments ()
  "Command line options of `bzr diff'"

  (let ((git-gutter:bazaar-diff-option "-a -b -c")
        (file "git-gutter.el"))
    (let ((got (git-gutter:bzr-diff-arguments file)))
      (should (equal got '("-a" "-b" "-c" "git-gutter.el"))))

    (let* ((git-gutter:start-revision "30000")
           (got (git-gutter:bzr-diff-arguments file)))
      (should (equal got '("-a" "-b" "-c" "-r" "30000" "git-gutter.el"))))))

(ert-deftest git-gutter-vcs-check-functions ()
  "Check function of VCS"

  (should (eq (git-gutter:vcs-check-function 'git) 'git-gutter:in-git-repository-p))
  (should (eq (git-gutter:vcs-check-function 'hg) 'git-gutter:in-hg-repository-p))
  (should (eq (git-gutter:vcs-check-function 'bzr) 'git-gutter:in-bzr-repository-p)))

(ert-deftest git-gutter-read-header ()
  "Read header of diff hunk"

  (let ((got (git-gutter:read-hunk-header "@@ -658,31 +688,30 @@")))
    (should (= (nth 0 got) 658))
    (should (= (nth 1 got) 31))
    (should (= (nth 2 got) 688))
    (should (= (nth 3 got) 30)))

  (let ((got (git-gutter:read-hunk-header "@@ -100 +200 @@")))
    (should (= (nth 0 got) 100))
    (should (= (nth 1 got) 1))
    (should (= (nth 2 got) 200))
    (should (= (nth 3 got) 1))))

;;; test-git-gutter.el end here
