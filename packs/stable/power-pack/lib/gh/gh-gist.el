;;; gh-gist.el --- gist module for gh.el

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

(require 'gh-api)
(require 'gh-auth)
(require 'gh-common)

;;;###autoload
(defclass gh-gist-api (gh-api-v3)
  ((gist-cls :allocation :class :initform gh-gist-gist))
  "Gist API")

;;;###autoload
(defclass gh-gist-gist-stub (gh-object)
  ((files :initarg :files :type list :initform nil)
   (public :initarg :public)
   (description :initarg :description)

   (file-cls :allocation :class :initform gh-gist-gist-file))
  "Class for user-created gist objects")

(defmethod gh-object-read-into ((stub gh-gist-gist-stub) data)
  (call-next-method)
  (with-slots (files public description)
      stub
    (setq files (gh-object-list-read (oref stub file-cls)
                                     (gh-read data 'files))
          public (gh-read data 'public)
          description (gh-read data 'description))))

;;;###autoload
(defclass gh-gist-gist (gh-gist-gist-stub)
  ((date :initarg :date)
   (update :initarg :update)
   (push-url :initarg :push-url)
   (pull-url :initarg :pull-url)
   (html-url :initarg :html-url)
   (comments :initarg :comments)
   (user :initarg :user :initform nil)
   (id :initarg :id :type string)
   (url :initarg :url :type string)
   (history :initarg :history :initform nil)
   (forks :initarg :forks :initform nil)

   (user-cls :allocation :class :initform gh-user))
  "Gist object")

(defmethod gh-object-read-into ((gist gh-gist-gist) data)
  (call-next-method)
  (with-slots (date update push-url pull-url html-url comments user
                    id url history forks)
      gist
    (setq date (gh-read data 'created_at)
          update (gh-read data 'updated_at)
          push-url (gh-read data 'git_push_url)
          pull-url (gh-read data 'git_pull_url)
          html-url (gh-read data 'html_url)
          comments (gh-read data 'comments)
          user (gh-object-read (or (oref gist :user)
                                    (oref gist user-cls))
                                (gh-read data 'user))
          id (gh-read data 'id)
          url (gh-read data 'url)
          history (gh-read data 'history)
          forks (gh-read data 'forks))))

(defclass gh-gist-gist-file (gh-object)
  ((filename :initarg :filename)
   (size :initarg :size)
   (url :initarg :url)
   (content :initarg :content)))

(defmethod gh-object-read-into ((file gh-gist-gist-file) data)
  (call-next-method)
  (with-slots (filename size url content)
      file
    (setq
     filename (gh-read data 'filename)
     size (gh-read data 'size)
     url (gh-read data 'raw_url)
     content (gh-read data 'content))))

(defmethod gh-gist-gist-to-obj ((gist gh-gist-gist-stub))
  `(("description" . ,(oref gist :description))
    ("public" . ,(oref gist :public))
    ("files" . ,(mapcar 'gh-gist-gist-file-to-obj (oref gist :files)))))

(defmethod gh-gist-gist-has-files ((gist gh-gist-gist-stub))
  (not (memq nil (mapcar (lambda (f)
                           (oref f :content)) (oref gist :files)))))

(defmethod gh-gist-gist-file-to-obj ((file gh-gist-gist-file))
  `(,(oref file :filename) . (("filename" . ,(oref file :filename))
                              ("content" . ,(oref file :content)))))

(defmethod gh-gist-list ((api gh-gist-api) &optional username)
  (gh-api-authenticated-request
   api (gh-object-list-reader (oref api gist-cls)) "GET"
   (format "/users/%s/gists" (or username (gh-api-get-username api)))))

(defmethod gh-gist-list-public ((api gh-gist-api))
  (gh-api-authenticated-request
   api (gh-object-list-reader (oref api gist-cls)) "GET" "/gists/public"))

(defmethod gh-gist-list-starred ((api gh-gist-api))
  (gh-api-authenticated-request
   api (gh-object-list-reader (oref api gist-cls)) "GET" "/gists/starred"))

(defmethod gh-gist-get ((api gh-gist-api) gist-or-id)
  (let (id transformer)
    (if (stringp gist-or-id)
        (setq id gist-or-id
              transformer (gh-object-reader (oref api gist-cls)))
      (setq id (oref gist-or-id :id)
            transformer (gh-object-reader gist-or-id)))
    (gh-api-authenticated-request
     api transformer "GET" (format "/gists/%s" id))))

(defmethod gh-gist-new ((api gh-gist-api) gist-stub)
  (gh-api-authenticated-request
   api (gh-object-reader (oref api gist-cls)) "POST" "/gists"
   (gh-gist-gist-to-obj gist-stub)))

(defmethod gh-gist-edit ((api gh-gist-api) gist)
  (gh-api-authenticated-request
   api (gh-object-reader (oref api gist-cls)) "PATCH"
   (format "/gists/%s"
           (oref gist :id))
   (gh-gist-gist-to-obj gist)))

(defmethod gh-gist-set-star ((api gh-gist-api) gist-or-id star)
  (let ((id (if (stringp gist-or-id) gist-or-id
              (oref gist-or-id :id))))
    (gh-api-authenticated-request
     api 'ignore (if star "PUT" "DELETE")
     (format "/gists/%s/star" id))))

(defmethod gh-gist-get-star ((api gh-gist-api) gist-or-id)
  (let ((id (if (stringp gist-or-id) gist-or-id
              (oref gist-or-id :id))))
    (gh-api-authenticated-request
     api 'ignore "GET" (format "/gists/%s/star" id))))

(defmethod gh-gist-fork ((api gh-gist-api) gist-or-id)
  (let ((id (if (stringp gist-or-id) gist-or-id
              (oref gist-or-id :id))))
    (gh-api-authenticated-request
     api (gh-object-reader (oref api gist-cls)) "POST"
     (format "/gists/%s/forks" id))))

(defmethod gh-gist-delete ((api gh-gist-api) gist-or-id)
  (let ((id (if (stringp gist-or-id) gist-or-id
              (oref gist-or-id :id))))
    (gh-api-authenticated-request
     api 'ignore "DELETE" (format "/gists/%s" id))))

(provide 'gh-gist)
;;; gh-gist.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
