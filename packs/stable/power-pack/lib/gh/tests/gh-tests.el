;;; gh-tests.el --- tests for gh.el

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

(require 'cl)

(require 'mocker)
(require 'ert)
(require 'url-http)

(defun gh-tests-get-traces-root ()
  (let* ((this-file (car
                     (rassoc-if
                      (lambda (items)
                        (member (cons 'provide 'gh-tests) items))
                      load-history))))
    (concat (file-name-directory this-file)
            "traces/")))

(defmacro gh-tests-with-traces-buffers (bufs &rest body)
  (declare (indent 1) (debug t))
  (let* ((root (gh-tests-get-traces-root))
         (syms nil)
         (specs (mapcar
                 (lambda (s)
                   (let* ((sym (car s))
                          (filename (cadr s))
                          (file (concat root filename))
                          (buf-sym (make-symbol "buff")))
                     (push sym syms)
                     (list sym `(let ((,buf-sym (generate-new-buffer
                                                 ,(concat " " filename))))
                                  (with-current-buffer ,buf-sym
                                    (insert-file-contents ,file)
                                    (set (make-local-variable
                                          'url-http-end-of-headers)
                                         (search-forward-regexp "^$"))
                                    (make-local-variable 'url-http-response-version)
                                    (make-local-variable 'url-http-response-status)
                                    (url-http-parse-response))
                                  ,buf-sym))))
                 bufs)))
    `(let ,specs
       (unwind-protect
           (progn
             ,@body)
         (dolist (buff (list ,@syms))
           (and (buffer-name buff)
                (kill-buffer buff)))))))

(defun gh-tests-mock-api (cls)
  (make-instance cls :sync t
                 :auth (make-instance 'gh-authenticator :username "dummy")))

(defmacro gh-tests-mock-url (recs &rest body)
  `(mocker-let ((url-retrieve-synchronously
                 (url)
                 ,recs))
     ,@body))

(provide 'gh-tests)
;;; gh-tests.el ends here
