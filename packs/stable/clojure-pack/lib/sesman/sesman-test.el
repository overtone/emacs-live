;;; sesman-test.el --- Tests for sesman -*- lexical-binding: t -*-
;;
;; Copyright (C) 2018, Vitalie Spinu
;; Author: Vitalie Spinu
;; URL: https://github.com/vspinu/sesman
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is *NOT* part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'ert)
(require 'sesman)
(require 'cl)


;;; UTILS

(defmacro with-empty-sesman-vars (&rest body)
  (declare (debug (body)))
  `(let ((sesman-links-alist)
         (sesman-sessions-hashmap (make-hash-table :test #'equal)))
     ,@body))


;;; SYSTEMS

;; A
(cl-defmethod sesman-start-session ((system (eql A)))
  (let ((name (gensym "A-")))
    (sesman-register 'A (list name "A-stuff-1" (gensym "A-stuff-")))))

(cl-defmethod sesman-quit-session ((system (eql A)) session)
  (setcdr session '("[A killed]")))

(cl-defmethod sesman-project ((system (eql A)))
  (file-name-directory (directory-file-name default-directory)))

;; B
(cl-defmethod sesman-start-session ((system (eql B)))
  (let ((name (gensym "B-")))
    (sesman-register 'B
                     (list name
                           (get-buffer-create (symbol-name (gensym "B-buf-")))
                           (get-buffer-create (symbol-name (gensym "B-buf-")))))))

(cl-defmethod sesman-quit-session ((system (eql B)) session)
  (mapc #'kill-buffer (cdr session)))

(cl-defmethod sesman-more-relevant-p ((_system (eql B)) session1 session2)
  (sesman-more-recent-p (cdr session1) (cdr session2)))

(cl-defmethod sesman-project ((system (eql B)))
  nil)


;;; LIFE CYCLE

(ert-deftest sesman-start-test ()
  (with-empty-sesman-vars
   (let ((sesman-system 'A))
     (sesman-start)
     (let ((sess (sesman-sessions 'A)))
       (should (= (length sess) 1))
       (should (string= (cadr (car sess)) "A-stuff-1"))
       (sesman-start)
       (let ((sess (sesman-sessions 'A)))
         (should (= (length sess) 2))
         (should (string= (cadr (cadr sess)) "A-stuff-1")))
       (let ((sesman-system 'B))
         (sesman-start)
         (let ((sess (sesman-sessions 'A)))
           (should (= (length sess) 2))
           (should (string= (cadr (cadr sess)) "A-stuff-1")))
         (let ((sess (sesman-sessions 'B)))
           (should (= (length sess) 1))
           (should (bufferp (cadr (car sess))))))))))

(ert-deftest sesman-quit-test ()
  (with-empty-sesman-vars

   ;; alphabetic relevance
   (let ((sesman-system 'A))
     (sesman-start)
     (let ((ses (car (sesman-sessions 'A))))
       (sesman-start)
       (sesman-quit)
       (should (= (length (sesman-sessions 'A)) 1))
       (should-not (string=
                    (car ses)
                    (car (sesman-current-session 'A))))))

   ;; recency relevance
   (let ((sesman-system 'B))
     (sesman-start)
     (let ((ses (car (sesman-sessions 'B))))
       (switch-to-buffer (cadr (sesman-start)))
       (sesman-quit)
       (should (= (length (sesman-sessions 'B)) 1))
       (should (eq
                (car ses)
                (car (sesman-current-session 'B))))))))

(ert-deftest sesman-restart-test ()
  (with-empty-sesman-vars
   (let ((sesman-system 'A))
     (sesman-start)
     (sesman-start)
     (let ((ses-name (car (sesman-current-session 'A))))
       (sesman-restart)
       (should (eq (car (sesman-current-session 'A))
                   ses-name))))))


;;; LINKING
(ert-deftest sesman-link-with-project-test ()
  (with-empty-sesman-vars
   (let ((sesman-system 'A))
     (let ((default-directory "/path/to/project/A")
           (other-dir "/path/to/other/project/B"))
       (sesman-start)

       (sesman-link-with-project nil (sesman-current-session 'A))
       (should (= (length (sesman-links 'A)) 1))
       (let ((lnk (car (sesman-links 'A))))
         (should (string= (sesman--lnk-value lnk) (file-name-directory default-directory)))
         (should (eq (sesman--lnk-context-type lnk) 'project))
         (should (eq (sesman--lnk-system-name lnk) 'A)))

       (sesman-link-with-project other-dir (sesman-current-session 'A))
       (should (= (length (sesman-links 'A)) 2))
       (let ((lnk (car (sesman-links 'A))))
         (should (string= (sesman--lnk-value lnk) other-dir))
         (should (eq (sesman--lnk-context-type lnk) 'project))
         (should (eq (sesman--lnk-system-name lnk) 'A)))))

   (let ((sesman-system 'B))
     (let ((default-directory "/path/to/project/A")
           (other-dir "/path/to/other/project/B"))
       (sesman-start)
       (should-error (sesman-link-with-project nil (sesman-current-session 'B)))))))

(ert-deftest sesman-link-with-directory-test ()
  (with-empty-sesman-vars
   (let ((sesman-system 'A))
     (let ((default-directory "/path/to/project/A")
           (other-dir "/path/to/other/project/B"))
       (sesman-start)

       (sesman-link-with-directory nil (sesman-current-session 'A))
       (should (= (length (sesman-links 'A)) 2))
       (should (= (length (sesman-links 'A nil 'directory)) 1))
       (let ((lnk (car (sesman-links 'A))))
         (should (string= (sesman--lnk-value lnk) default-directory))
         (should (eq (sesman--lnk-context-type lnk) 'directory))
         (should (eq (sesman--lnk-system-name lnk) 'A)))

       (sesman-link-with-directory other-dir (sesman-current-session 'A))
       (should (= (length (sesman-links 'A)) 3))
       (should (= (length (sesman-links 'A nil 'directory)) 2))
       (let ((lnk (car (sesman-links 'A))))
         (should (string= (sesman--lnk-value lnk) other-dir))
         (should (eq (sesman--lnk-context-type lnk) 'directory))
         (should (eq (sesman--lnk-system-name lnk) 'A)))))

   (let ((sesman-system 'B))
     (let ((default-directory "/path/to/project/B1")
           (other-dir "/path/to/other/project/B2"))
       (sesman-start)

       (sesman-link-with-directory nil (sesman-current-session 'B))
       (should (= (length (sesman-links 'B)) 1))
       (let ((lnk (car (sesman-links 'B))))
         (should (string= (sesman--lnk-value lnk) default-directory))
         (should (eq (sesman--lnk-context-type lnk) 'directory))
         (should (eq (sesman--lnk-system-name lnk) 'B)))))

   (should (= (length sesman-links-alist) 4))))

(ert-deftest sesman-link-with-buffer-test ()
  (with-empty-sesman-vars
   (let ((buf-1 (get-buffer-create "tmp-buf-1"))
         (buf-2 (get-buffer-create "tmp-buf-2"))
         (sesman-system 'A))
     (with-current-buffer buf-1
       (let ((default-directory "/path/to/project/A")
             (other-dir "/path/to/other/project/B"))
         (sesman-start)
         (sesman-link-with-buffer nil (sesman-current-session 'A))
         (should (= (length (sesman-links 'A)) 2))
         (should (= (length (sesman-links 'A nil 'project)) 1))
         (should (= (length (sesman-links 'A nil 'directory)) 0))
         (should (= (length (sesman-links 'A nil 'buffer)) 1))
         (let ((lnk (car (sesman-links 'A nil 'buffer))))
           (should (eq (sesman--lnk-value lnk) buf-1))
           (should (eq (sesman--lnk-context-type lnk) 'buffer))
           (should (eq (sesman--lnk-system-name lnk) 'A)))

         (sesman-link-with-buffer buf-2 (sesman-current-session 'A))
         (should (= (length (sesman-links 'A)) 3))
         (should (= (length (sesman-links 'A nil 'buffer)) 2))
         (let ((lnk (car (sesman-links 'A nil 'buffer))))
           (should (eq (sesman--lnk-value lnk) buf-2))
           (should (eq (sesman--lnk-context-type lnk) 'buffer))
           (should (eq (sesman--lnk-system-name lnk) 'A))))

       (let ((sesman-system 'B))
         (let ((default-directory "/path/to/project/B1")
               (other-dir "/path/to/other/project/B2"))
           (sesman-start)
           (should (= (length (sesman-links 'B nil 'buffer)) 0))
           (sesman-link-with-buffer nil (sesman-current-session 'B))
           (should (= (length (sesman-links 'B)) 2))
           (should (= (length (sesman-links 'B nil 'project)) 0))
           (should (= (length (sesman-links 'B nil 'directory)) 1))
           (should (= (length (sesman-links 'B nil 'buffer)) 1))
           (sesman-link-with-buffer buf-2 (sesman-current-session 'B))
           (should (= (length (sesman-links 'B nil 'buffer)) 2))
           (let ((lnk (car (sesman-links 'B nil 'buffer))))
             (should (eq (sesman--lnk-value lnk) buf-2))
             (should (eq (sesman--lnk-context-type lnk) 'buffer))
             (should (eq (sesman--lnk-system-name lnk) 'B)))))))

   (should (= (length sesman-links-alist) 6))))


;;; FILE PATHS

(cl-defmethod sesman-project ((system (eql C)))
  (directory-file-name default-directory))

(ert-deftest sesman-symlinked-projects-tests ()
  (let* ((dir1 (make-temp-file "1-" 'dir))
         (dir2 (make-temp-file "2-" 'dir))
         (dir1-link (format "%s/dir1" dir2)))

    ;; dir1 link in dir2
    (should (equal (shell-command (format "ln -s %s %s" dir1 dir1-link))
                   0))

    (let ((sesman-follow-symlinks nil)
          (vc-follow-symlinks t))
      (should (equal (sesman-expand-path dir1-link)
                     dir1-link)))
    (let ((sesman-follow-symlinks t)
          (vc-follow-symlinks nil))
      (should (equal (sesman-expand-path dir1-link)
                     dir1)))
    (let ((sesman-follow-symlinks 'vc)
          (vc-follow-symlinks t))
      (should (equal (sesman-expand-path dir1-link)
                     dir1)))
    (let ((sesman-follow-symlinks 'vc)
          (vc-follow-symlinks nil))
      (should (equal (sesman-expand-path dir1-link)
                     dir1-link)))

    (let ((sesman-follow-symlinks nil)
          (default-directory dir1-link))
      (should (equal (sesman-context 'project 'C)
                     dir1-link)))
    (let ((sesman-follow-symlinks t)
          (default-directory dir1-link))
      (should (equal (sesman-context 'project 'C)
                     dir1)))
    (let ((sesman-follow-symlinks 'vc)
          (vc-follow-symlinks t)
          (default-directory dir1-link))
      (should (equal (sesman-context 'project 'C)
                     dir1)))
    (let ((sesman-follow-symlinks 'vc)
          (vc-follow-symlinks nil)
          (default-directory dir1-link))
      (should (equal (sesman-context 'project 'C)
                     dir1-link)))

    (delete-directory dir1 t)
    (delete-directory dir2 t)))

(provide 'sesman-test)

;;; sesman-test.el ends here
