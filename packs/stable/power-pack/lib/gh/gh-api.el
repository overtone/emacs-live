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

(require 'json)

(require 'gh-profile)
(require 'gh-url)
(require 'gh-auth)
(require 'gh-cache)

(require 'logito)

(defgroup gh-api nil
  "Github API."
  :group 'gh)

(defcustom gh-api-username-filter 'gh-api-enterprise-username-filter
  "Filter to apply to usernames to build URL components"
  :type 'function
  :group 'gh-api)

;;;###autoload
(defclass gh-api ()
  ((sync :initarg :sync :initform t)
   (cache :initarg :cache :initform nil)
   (base :initarg :base :type string)
   (profile :initarg :profile :type string)
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
        (cache (oref api :cache))
        (classname (symbol-name (funcall (if (fboundp 'eieio-object-class)
                                             'eieio-object-class
                                           'object-class)
                                         api))))
    (oset api :auth auth)
    (unless (or (null cache)
                (and (eieio-object-p cache)
                     (object-of-class-p cache 'gh-cache)))
      (oset api :cache (funcall (oref api cache-cls)
                                (format "gh/%s/%s"
                                        classname
                                        (gh-api-get-username api)))))))

(defmethod gh-api-expand-resource ((api gh-api)
                                   resource)
  resource)

(defun gh-api-enterprise-username-filter (username)
  (replace-regexp-in-string (regexp-quote ".") "-" username))

(defmethod gh-api-get-username ((api gh-api))
  (let ((username (oref (oref api :auth) :username)))
    (funcall gh-api-username-filter username)))

;;;###autoload
(defclass gh-api-v3 (gh-api)
  ((data-format :initarg :data-format :initform :json))
  "Github API v3")

(defcustom gh-api-v3-authenticator 'gh-oauth-authenticator
  "Authenticator for Github API v3"
  :type '(choice (const :tag "Password" gh-password-authenticator)
                 (const :tag "OAuth" gh-oauth-authenticator))
  :group 'gh-api)

(defmethod constructor :static ((api gh-api-v3) newname &rest args)
  (let ((obj (call-next-method))
        (gh-profile-current-profile (gh-profile-current-profile)))
    (oset obj :profile (gh-profile-current-profile))
    (oset obj :base (gh-profile-url))
    (gh-api-set-default-auth obj
                             (or (oref obj :auth)
                                 (funcall gh-api-v3-authenticator "auth")))
    obj))

(defclass gh-api-request (gh-url-request)
  ((default-response-cls :allocation :class :initform gh-api-response)))

(defclass gh-api-response (gh-url-response)
  ())

(defun gh-api-json-decode (repr)
  (if (or (null repr) (string= repr ""))
      'empty
    (let ((json-array-type 'list))
      (json-read-from-string repr))))

(defun gh-api-json-encode (json)
  (json-encode-list json))

(defmethod gh-url-response-set-data ((resp gh-api-response) data)
  (call-next-method resp (gh-api-json-decode data)))

(defclass gh-api-paged-request (gh-api-request)
  ((default-response-cls :allocation :class :initform gh-api-paged-response)))

(defclass gh-api-paged-response (gh-api-response)
  ())

(defmethod gh-api-paging-links ((resp gh-api-paged-response))
  (let ((links-header (cdr (assoc "Link" (oref resp :headers)))))
    (when links-header
      (loop for item in (split-string links-header ", ")
            when (string-match "^<\\(.*\\)>; rel=\"\\(.*\\)\"" item)
            collect (cons (match-string 2 item)
                          (match-string 1 item))))))

(defmethod gh-url-response-set-data ((resp gh-api-paged-response) data)
  (let ((previous-data (oref resp :data))
        (next (cdr (assoc "next" (gh-api-paging-links resp)))))
    (call-next-method)
    (oset resp :data (append previous-data (oref resp :data)))
    (when next
      (let ((req (oref resp :-req)))
        (oset resp :data-received nil)
        (oset req :url next)
        (gh-url-run-request req resp)))))

(defmethod gh-api-authenticated-request
  ((api gh-api) transformer method resource &optional data params)
  (let* ((fmt (oref api :data-format))
         (headers (cond ((eq fmt :form)
                         '(("Content-Type" .
                            "application/x-www-form-urlencoded")))
                        ((eq fmt :json)
                         '(("Content-Type" .
                            "application/json")))))
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
                ;; TODO: use gh-api-paged-request only when needed
                (make-instance 'gh-api-paged-request
                               :method method
                               :url (concat (oref api :base)
                                            (gh-api-expand-resource
                                             api resource))
                               :query params
                               :headers headers
                               :data (or (and (eq fmt :json)
                                              (gh-api-json-encode data))
                                         (and (eq fmt :form)
                                              (gh-url-form-encode data))
                                         ""))))))
    (cond (has-value ;; got value from cache
           (gh-api-response "cached" :data-received t :data value))
          (key ;; no value, but cache exists and method is safe
           (let ((resp (make-instance (oref req default-response-cls)
                                      :transform transformer)))
             (gh-url-run-request req resp)
             (gh-url-add-response-callback
              resp (list #'(lambda (value cache key)
                             (pcache-put cache key value))
                         cache key))
             resp))
          (cache ;; unsafe method, cache exists
           (pcache-invalidate cache key)
           (gh-url-run-request req (make-instance
                                    (oref req default-response-cls)
                                    :transform transformer)))
          (t ;; no cache involved
           (gh-url-run-request req (make-instance
                                    (oref req default-response-cls)
                                    :transform transformer))))))

(define-obsolete-function-alias 'gh-api-add-response-callback
  'gh-url-add-response-callback "0.6.0")

(provide 'gh-api)
;;; gh-api.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
