;;; gh-issue-comments.el --- issue comments api for github

;; Copyright (C) 2014 Travis Thieman

;; Author: Travis Thieman <travis.thieman@gmail.com>
;; Keywords:

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

;; TODOS:
;;   * Support listing all comments in a repository

;; Basic usage:

;; (setf api (gh-issue-comments-api "api" :sync nil :cache nil :num-retries 1))
;; (setf comments (gh-issue-comments-list api "user" "repo" "issue id"))
;; (setq my-comment (make-instance 'gh-issue-comments-comment :body "This is great!"))
;; (gh-issue-comments-new api "user" "repo" "issue id" my-comment)

;;; Code:

(eval-when-compile
  (require 'cl))

;;;###autoload
(require 'eieio)

(require 'gh-api)
(require 'gh-auth)
(require 'gh-common)

(require 'gh-issues)

(defclass gh-issue-comments-api (gh-api-v3)
  ((comment-cls :allocation :class :initform gh-issue-comments-comment))
  "GitHub Issue Comments api")

(defclass gh-issue-comments-comment (gh-object)
  ((url :initarg :url)
   (html-url :initarg :html-url)
   (body :initarg :body)
   (user :initarg :user :initform nil)
   (created-at :initarg :created_at)
   (updated-at :initarg :updated_at)

   (user-cls :allocation :class :initform gh-user))
  "issues comment")

(defmethod gh-object-read-into ((comment gh-issue-comments-comment) data)
  (call-next-method)
  (with-slots (url html-url body user created-at updated-at)
      comment
    (setq url (gh-read data 'url)
          html-url (gh-read data 'html-url)
          body (gh-read data 'body)
          user (gh-object-read  (or (oref comment :user)
                                    (oref comment user-cls))
                                (gh-read data 'user))
          created-at (gh-read data 'created_at)
          updated-at (gh-read data 'updated_at))))

(defmethod gh-issue-comments-list ((api gh-issue-comments-api) user repo issue-id)
  (gh-api-authenticated-request
   api (gh-object-list-reader (oref api comment-cls)) "GET"
   (format "/repos/%s/%s/issues/%s/comments" user repo issue-id)))

(defmethod gh-issue-comments-get ((api gh-issue-comments-api) user repo comment-id)
  (gh-api-authenticated-request
   api (gh-object-reader (oref api comment-cls)) "GET"
   (format "/repos/%s/%s/issues/comments/%s" user repo comment-id)))

(defmethod gh-issue-comments-req-to-update ((req gh-issue-comments-comment))
  `(("body" . ,(oref req body))))

(defmethod gh-issue-comments-update ((api gh-issue-comments-api) user repo comment-id comment)
  (gh-api-authenticated-request
   api (gh-object-reader (oref api comment-cls)) "PATCH"
   (format "/repos/%s/%s/issues/comments/%s" user repo comment-id)
   (gh-issue-comments-req-to-update comment)))

(defmethod gh-issue-comments-new ((api gh-issue-comments-api) user repo issue-id comment)
  (gh-api-authenticated-request
   api (gh-object-reader (oref api comment-cls)) "POST"
   (format "/repos/%s/%s/issues/%s/comments" user repo issue-id)
   (gh-issue-comments-req-to-update comment)))

(defmethod gh-issue-comments-delete ((api gh-issue-comments-api) user repo comment-id)
  (gh-api-authenticated-request
   api nil "DELETE"
   (format "/repos/%s/%s/issues/comments/%s" user repo comment-id)))

(provide 'gh-issue-comments)
;;; gh-issue-comments.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
