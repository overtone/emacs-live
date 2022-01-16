;; ffip-tests.el --- unit tests for find-file-in-project -*- coding: utf-8 -*-

;; Author: Chen Bin <chenbin DOT sh AT gmail DOT com>

;;; License:

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; Code:

(require 'ert)
(require 'find-file-in-project)

(defun get-full-path (filename)
  "Get full path of FILENAME in current directory."
  (concat
   (if load-file-name (file-name-directory load-file-name) default-directory)
   filename))

(ert-deftest ffip-test-find-by-selected ()
  (let (files)
    (setq ffip-project-root default-directory)
    (setq files (mapcar 'car (ffip-project-search "git-diff")))
    ;; (message "files=%s" files)
    (should (string-match-p "git-diff.diff" (car files)))))


(ert-deftest ffip-test-ffip ()
  (let (files)
    (setq ffip-project-root default-directory)
    (setq files (mapcar 'car (ffip-project-search nil)))
    (should (> (length files) 1))
    (should (not (active-minibuffer-window)))))

(ert-deftest ffip-test-ffip-open-another ()
  (let (files
        (prefix-args '(4 (4))))
    (dolist (open-another-arg prefix-args)
      (setq ffip-project-root default-directory)
      (setq files (mapcar 'car (ffip-project-search "git-diff")))
      (should (= (length files) 1))
      (should (not (active-minibuffer-window))))))

(ert-deftest ffip-test-ffip-show-diff ()
  (let* (files
         (ffip-diff-backends '((with-temp-buffer
                                 (insert-file-contents (get-full-path "git-diff.diff"))
                                 (buffer-string))))
         ;; see https://github.com/redguardtoo/find-file-in-project/issues/137
         ;; debian package creates some extra diff in parent directory "tests/"
         ;; So root directory should be set to "tests/"
         (ffip-diff-find-file-by-file-name-p t)
         (ffip-project-root (file-name-directory load-file-name)))
    (ffip-show-diff)
    (switch-to-buffer "*ffip-diff*")
    (goto-char (point-min))
    (diff-file-next)
    ;; find now
    (ffip-diff-find-file)
    (should (string= (file-name-nondirectory (buffer-file-name)) "ffip-tests.el"))

    ;; move to the second file hunk
    (switch-to-buffer "*ffip-diff*")
    (diff-file-next)
    ;; find file in the first diff hunk now
    (ffip-diff-find-file)
    (should (string= (file-name-nondirectory (buffer-file-name)) "git-diff.diff"))
    ;; cleanup
    (kill-buffer "*ffip-diff*")))

(ert-deftest ffip-test-ffip-parent-directory ()
  (let* ((dir "/home/cb/projs/find-file-in-project/"))
    (should (string= (ffip-parent-directory 0 dir) dir))
    (should (string= (ffip-parent-directory 1 dir) "/home/cb/projs/"))
    (should (string= (ffip-parent-directory 2 dir) "/home/cb/"))
    (should (string= (ffip-parent-directory 3 dir) "/home/"))
    (should (string= (ffip-parent-directory 4 dir) "/"))
    (should (string= (ffip-parent-directory 999 dir) "/"))))

(ert-deftest ffip-test-guess-physical-path ()
  (let* (fn
         (default-directory (file-name-directory (or load-file-name buffer-file-name))))
    (with-temp-buffer
      (insert "import './test1';\n")
      (insert "import './test2';\n")
      (js-mode) ; javascript

      ;; detect "test1.ts"
      (goto-char (point-min))
      (search-forward "test1")
      (setq fn (ffip-guess-file-name-at-point))
      (should (string= fn "./test1"))
      (should (string= (ffip--guess-physical-path fn) (file-truename "./test1.ts")))

      ;; detect "test2.js"
      (goto-char (point-min))
      (search-forward "test2")
      (setq fn (ffip-guess-file-name-at-point))
      (should (string= fn "./test2"))
      (should (string= (ffip--guess-physical-path fn) (file-truename "./test2.js"))))
    ))

(ert-deftest ffip-test-completing-read ()
  (should (eq (ffip-completing-read "hint:" '(a)) 'a))
  (should (eq (ffip-completing-read "hint:" '((a . b))) 'b))
  (ffip-completing-read "hint:"
                        '("a")
                        (lambda (selected) (should (string= selected "a")))))

(ert-deftest ffip-test-ido ()
  (should (boundp 'ffip-prefer-ido-mode))
  (should (not ffip-prefer-ido-mode)))

(ert-deftest ffip-test-windows ()
  (if (eq system-type 'windows-nt)
      (should (executable-find (ffip--guess-gnu-find)))
    (message "NOT windows native Emacs, nothing to test.")
    (should t)))

(ert-deftest ffip-test-relative-path-commands ()
  (with-temp-buffer
    (let* (orig-pos)
      (insert (get-full-path "git-diff.diff"))
      (goto-char 5)
      (should (file-exists-p (buffer-string)))
      ;; absolute path
      (should (not (string= "tests/git-diff.diff" (replace-regexp-in-string "/<<PKGBUILDDIR>>/" "" (buffer-string)))))
      (ffip-fix-file-path-at-point)
      ;; relative path
      (should (string= "tests/git-diff.diff" (replace-regexp-in-string "/<<PKGBUILDDIR>>/" "" (buffer-string)))))))

(ert-run-tests-batch-and-exit)
