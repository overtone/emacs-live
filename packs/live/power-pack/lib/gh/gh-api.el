;;; gh-api.el --- api definition for gh.el

;; Copyright (C) 2011  Yann Hodique

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

(require 'url)
(require 'json)
(require 'gh-auth)
(require 'gh-cache)

(require 'logito)

(defgroup gh-api nil
  "Github API."
  :group 'gh)

;;;###autoload
(defclass gh-api ()
  ((sync :initarg :sync :initform t)
   (cache :initarg :cache :initform nil)
   (base :initarg :base :type string)
   (auth :initarg :auth :initform nil)
   (data-format :initarg :data-format)
   (num-retries :initarg :num-retries :initform 0)
   (log :initarg :log :initform nil)
   (cache-cls :initform gh-cache :allocation :class))
  "Github API")

(defmethod logito-log ((api gh-api) level tag string &rest objects)
  (apply 'logito-log (oref api :log) level tag string objects))

(defmethod constructor :static ((api gh-api) newname &rest args)
  (call-next-method))

(defmethod gh-api-set-default-auth ((api gh-api) auth)
  (let ((auth (or (oref api :auth) auth))
        (cache (oref api :cache)))
    (oset api :auth auth)
    (unless (or (null cache)
                (and (eieio-object-p cache)
                     (object-of-class-p cache 'gh-cache)))
      (oset api :cache (funcall (oref api cache-cls)
                                (format "gh/%s/%s"
                                        (symbol-name (object-class api))
                                        (gh-api-get-username api)))))))

(defmethod gh-api-expand-resource ((api gh-api)
                                   resource)
  resource)

(defmethod gh-api-get-username ((api gh-api))
  (oref (oref api :auth) :username))

;;;###autoload
(defclass gh-api-v2 (gh-api)
  ((base :initarg :base :initform "https://github.com/api/v2/json")
   (data-format :initarg :data-format :initform :json)))

(defcustom gh-api-v2-authenticator 'gh-oauth-authenticator
  "Authenticator for Github API v2"
  :type '(choice (const :tag "Password" gh-password-authenticator)
                 (const :tag "OAuth" gh-oauth-authenticator))
  :group 'gh-api)

(defmethod constructor :static ((api gh-api-v2) newname &rest args)
  (let ((obj (call-next-method)))
    (gh-api-set-default-auth obj
                             (or (oref obj :auth)
                                 (funcall gh-api-v2-authenticator "auth")))
    obj))

;;;###autoload
(defclass gh-api-v3 (gh-api)
  ((base :initarg :base :initform "https://api.github.com")
   (data-format :initarg :data-format :initform :json))
  "Github API v3")

(defcustom gh-api-v3-authenticator 'gh-oauth-authenticator
  "Authenticator for Github API v3"
  :type '(choice (const :tag "Password" gh-password-authenticator)
                 (const :tag "OAuth" gh-oauth-authenticator))
  :group 'gh-api)

(defmethod constructor :static ((api gh-api-v3) newname &rest args)
  (let ((obj (call-next-method)))
    (gh-api-set-default-auth obj
                             (or (oref obj :auth)
                                 (funcall gh-api-v3-authenticator "auth")))
    obj))

(defclass gh-api-request ()
  ((method :initarg :method :type string)
   (url :initarg :url :type string)
   (headers :initarg :headers)
   (data :initarg :data :initform "" :type string)))

(defclass gh-api-response ()
  ((data-received :initarg :data-received :initform nil)
   (data :initarg :data :initform nil)
   (callbacks :initarg :callbacks :initform nil)
   (-api :initarg :-api :initform nil))
  "Class for API responses")

(defun gh-api-json-decode (repr)
  (if (or (null repr) (string= repr ""))
      'empty
    (let ((json-array-type 'list))
      (json-read-from-string repr))))

(defun gh-api-json-encode (json)
  (json-encode-list json))

(defun gh-api-form-encode (form)
  (mapconcat (lambda (x) (format "%s=%s" (car x) (cdr x)))
             form "&"))

(defun gh-api-params-encode (form)
  (concat "?" (gh-api-form-encode form)))

(defmethod gh-api-response-init ((resp gh-api-response)
                                 buffer &optional transform)
  (declare (special url-http-end-of-headers))
  (unwind-protect
      (with-current-buffer buffer
        (logito:debug (oref resp :-api) "Response: \n%s" (buffer-string))
        (goto-char (1+ url-http-end-of-headers))
        (let ((raw (buffer-substring (point) (point-max))))
          (oset resp :data
                (if transform
                    (funcall transform (gh-api-json-decode raw))
                  raw)))
        (oset resp :data-received t))
    (kill-buffer buffer))
  (gh-api-response-run-callbacks resp)
  resp)

(defun gh-api-set-response (status retry-data)
  (destructuring-bind (api req transform resp num) retry-data
    (condition-case err
        (gh-api-response-init resp (current-buffer) transform)
      (error
       (if (or (null num) (zerop num))
           (signal (car err) (cdr err))
         (logito:info api "Retrying request %s %s"
                      (oref req :method) (oref req :url))
         (let ((num (1- num)))
           (gh-api-run-request api req transform resp num)))))))

(defmethod gh-api-response-run-callbacks ((resp gh-api-response))
  (flet ((gh-api-copy-list (list)
                           (if (consp list)
                               (let ((res nil))
                                 (while (consp list) (push (pop list) res))
                                 (prog1 (nreverse res) (setcdr res list)))
                             (car list))))
    (let ((data (oref resp :data)))
      (dolist (cb (gh-api-copy-list (oref resp :callbacks)))
        (if (or (functionp cb) (symbolp cb))
            (funcall cb data)
          (apply (car cb) data (cdr cb)))
        (object-remove-from-list resp :callbacks cb))))
  resp)

(defmethod gh-api-add-response-callback ((resp gh-api-response) callback)
  (object-add-to-list resp :callbacks callback t)
  (if (oref resp :data-received)
    (gh-api-response-run-callbacks resp)
    resp))

(defmethod gh-api-authenticated-request
  ((api gh-api) transformer method resource &optional data params)
  (let* ((fmt (oref api :data-format))
         (headers (when (eq fmt :form)
                    '(("Content-Type" . "application/x-www-form-urlencoded"))))
         (cache (oref api :cache))
         (key (and cache
                   (member method (oref cache safe-methods))
                   (list resource
                         method
                         (sha1 (format "%s" transformer)))))
         (has-value (and key (pcache-has cache key)))
         (value (and has-value (pcache-get cache key)))
         (req
          (and (not has-value) ;; we'll need the req only if value's not
                               ;; already in cache
               (gh-auth-modify-request
                (oref api :auth)
                (gh-api-request "request"
                                :method method
                                :url (concat (oref api :base)
                                             (gh-api-expand-resource
                                              api resource)
                                             (if params
                                                 (gh-api-params-encode params)
                                               ""))
                                :headers headers
                                :data (or (and (eq fmt :json)
                                               (gh-api-json-encode data))
                                          (and (eq fmt :form)
                                               (gh-api-form-encode data))
                                          ""))))))
    (cond (has-value ;; got value from cache
           (gh-api-response "cached" :data-received t :data value))
          (key ;; no value, but cache exists and method is safe
           (let ((resp (gh-api-run-request api req transformer)))
             (gh-api-add-response-callback
              resp (list #'(lambda (value cache key)
                             (pcache-put cache key value))
                         cache key))
             resp))
          (cache ;; unsafe method, cache exists
           (pcache-invalidate cache key)
           (gh-api-run-request api req transformer))
          (t ;; no cache involved
           (gh-api-run-request api req transformer)))))

(defmethod gh-api-run-request ((api gh-api) req transformer &optional resp num)
  (let ((url-request-method (oref req :method))
        (url-request-data (oref req :data))
        (url-request-extra-headers (oref req :headers))
        (url (oref req :url)))
    (logito:debug api "Request: %s %s %s"
                  url-request-method
                  url
                  url-request-extra-headers)
    (logito:debug api "Data: %s"
                  url-request-data)
    (if (oref api :sync)
        (let* ((resp (or resp (gh-api-response "sync" :-api api)))
               (retry-data (list api req transformer resp
                                 (or num (oref api :num-retries)))))
          (with-current-buffer (url-retrieve-synchronously url)
            (gh-api-set-response nil retry-data))
          resp)
      (let* ((resp (or resp (gh-api-response "async" :-api api)))
             (retry-data (list api req transformer resp
                               (or num (oref api :num-retries)))))
        (url-retrieve url 'gh-api-set-response (list retry-data))
        resp))))

(provide 'gh-api)
;;; gh-api.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
