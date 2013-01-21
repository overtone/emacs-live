;;; gh-url.el --- url wrapper for gh.el

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

(eval-when-compile
  (require 'cl))

;;;###autoload
(require 'eieio)

(require 'url-http)

(defclass gh-url-request ()
  ((method :initarg :method :type string)
   (url :initarg :url :type string)
   (query :initarg :query :initform nil)
   (headers :initarg :headers :initform nil)
   (data :initarg :data :initform "" :type string)
   (async :initarg :async :initform nil)
   (num-retries :initarg :num-retries :initform 0)

   (default-response-cls :allocation :class :initform gh-url-response)))

(defclass gh-url-response ()
  ((data-received :initarg :data-received :initform nil)
   (data :initarg :data :initform nil)
   (headers :initarg :headers :initform nil)
   (http-status :initarg :http-status :initform nil)
   (callbacks :initarg :callbacks :initform nil)
   (transform :initarg :transform :initform nil)
   (-req :initarg :-req :initform nil)))

(defmethod gh-url-response-set-data ((resp gh-url-response) data)
  (let ((transform (oref resp :transform)))
    (oset resp :data
          (if transform
              (funcall transform data)
            data))
    (oset resp :data-received t)))

(defmethod gh-url-response-run-callbacks ((resp gh-url-response))
  (flet ((gh-url-copy-list (list)
                           (if (consp list)
                               (let ((res nil))
                                 (while (consp list) (push (pop list) res))
                                 (prog1 (nreverse res) (setcdr res list)))
                             (car list))))
    (let ((data (oref resp :data)))
      (dolist (cb (gh-url-copy-list (oref resp :callbacks)))
        (if (or (functionp cb) (symbolp cb))
            (funcall cb data)
          (apply (car cb) data (cdr cb)))
        (object-remove-from-list resp :callbacks cb))))
  resp)

(defmethod gh-url-add-response-callback ((resp gh-url-response) callback)
  (object-add-to-list resp :callbacks callback t)
  (if (oref resp :data-received)
    (gh-url-response-run-callbacks resp)
    resp))

;;; code borrowed from nicferrier's web.el
(defun gh-url-parse-headers (data)
  (let* ((headers nil)
         (header-lines (split-string data "\n"))
         (status-line (car header-lines)))
    (when (string-match
           "HTTP/\\([0-9.]+\\) \\([0-9]\\{3\\}\\)\\( \\(.*\\)\\)*"
           status-line)
      (push (cons 'status-version (match-string 1 status-line)) headers)
      (push (cons 'status-code (match-string 2 status-line)) headers)
      (push (cons 'status-string
                  (or (match-string 4 status-line) ""))
            headers))
    (loop for line in (cdr header-lines)
       if (string-match
           "^\\([A-Za-z0-9.-]+\\):[ ]*\\(.*\\)"
           line)
       do
       (let ((name (match-string 1 line))
             (value (match-string 2 line)))
         (push (cons name value) headers)))
    headers))

(defmethod gh-url-response-finalize ((resp gh-url-response))
  (when (oref resp :data-received)
    (gh-url-response-run-callbacks resp)))

(defmethod gh-url-response-init ((resp gh-url-response)
                                 buffer)
  (declare (special url-http-end-of-headers))
  (unwind-protect
      (with-current-buffer buffer
        (let ((headers (gh-url-parse-headers
                        (buffer-substring
                         (point-min) (1+ url-http-end-of-headers)))))
          (oset resp :headers headers)
          (oset resp :http-status (read (cdr (assoc 'status-code headers)))))
        (goto-char (1+ url-http-end-of-headers))
        (let ((raw (buffer-substring (point) (point-max))))
          (gh-url-response-set-data resp raw)))
    (kill-buffer buffer))
  (gh-url-response-finalize resp)
  resp)

(defun gh-url-set-response (status req-resp)
  (destructuring-bind (req resp) req-resp
    (condition-case err
        (progn
          (oset resp :-req req)
          (gh-url-response-init resp (current-buffer)))
      (error
       (let ((num (oref req :num-retries)))
         (if (or (null num) (zerop num))
             (signal (car err) (cdr err))
           (oset req :num-retries (1- num))
           (gh-url-run-request req resp)))))))

(defun gh-url-form-encode (form)
  (mapconcat (lambda (x) (format "%s=%s" (car x) (cdr x)))
             form "&"))

(defun gh-url-params-encode (form)
  (concat "?" (gh-url-form-encode form)))

(defmethod gh-url-run-request ((req gh-url-request) &optional resp)
  (let ((url-request-method (oref req :method))
        (url-request-data (oref req :data))
        (url-request-extra-headers (oref req :headers))
        (url (concat (oref req :url)
                     (let ((params (oref req :query)))
                       (if params
                           (gh-url-params-encode params)
                         "")))))
    (if (oref req :async)
        (let* ((resp (or resp (make-instance (oref req default-response-cls))))
               (req-resp (list req resp)))
          (url-retrieve url 'gh-url-set-response (list req-resp))
          resp)
      (let* ((resp (or resp (make-instance (oref req default-response-cls))))
             (req-resp (list req resp)))
        (with-current-buffer (url-retrieve-synchronously url)
          (gh-url-set-response nil req-resp))
        resp))))

(provide 'gh-url)
;;; gh-url.el ends here
