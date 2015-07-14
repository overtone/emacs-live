;;; gh-pull-comments.el --- pull request comments api for github

;; Copyright (C) 2014 Toni Reina

;; Author: Toni Reina <areina0@gmail.com>
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

;; (setf api (gh-pull-comments-api "api" :sync nil :cache nil :num-retries 1))
;; (setf comments (gh-pull-comments-list api "user" "repo" "pull request id"))
;; (setq my-comment (make-instance 'gh-pull-comments-comment
;; 				:body "This is great!"
;; 				:path "README.md"
;; 				:position 2
;; 				:commit-id "commit sha"))
;; (gh-pull-comments-new api "user" "repo" "pull request id" my-comment)

;;; Code:

(eval-when-compile
  (require 'cl))

;;;###autoload
(require 'eieio)

(require 'gh-api)
(require 'gh-auth)
(require 'gh-common)

(defclass gh-pull-comments-api (gh-api-v3)
  ((pull-comment-cls :allocation :class :initform gh-pull-comments-comment))
  "GitHub Pull Request Comments API")

(defclass gh-pull-comments-comment (gh-object)
  ((url :initarg :url)
   (html-url :initarg :html-url)
   (id :initarg :id)
   (body :initarg :body)
   (user :initarg :user :initform nil)
   (path :initarg :path)
   (diff-hunk :initarg :diff-hunk)
   (position :initarg :position)
   (original-position :initarg :original-position)
   (commit-id :initarg :commit-id)
   (original-commit-id :initarg :original-commit-id)
   (in-reply-to :initarg :in-reply-to :initform nil)
   (created-at :initarg :created_at)
   (updated-at :initarg :updated_at)
   (user-cls :allocation :class :initform gh-user))
  "Class for Pull Requests comments")

(defmethod gh-object-read-into ((comment gh-pull-comments-comment) data)
  (call-next-method)
  (with-slots (url html-url id body user path diff-hunk position
		   original-position commit-id original-commit-id in-reply-to
		   created-at updated-at)
      comment
    (setq url (gh-read data 'url)
          html-url (gh-read data 'html-url)
	  id (gh-read data 'id)
          body (gh-read data 'body)
          user (gh-object-read  (or (oref comment :user)
                                    (oref comment user-cls))
                                (gh-read data 'user))
	  path (gh-read data 'path)
	  diff-hunk (gh-read data 'diff_hunk)
	  position (gh-read data 'position)
	  original-position (gh-read data 'original_position)
	  commit-id (gh-read data 'commit_id)
	  original-commit-id (gh-read data 'original_commit_id)
	  in-reply-to (gh-read data 'in_reply_to)
          created-at (gh-read data 'created_at)
          updated-at (gh-read data 'updated_at))))

(defmethod gh-pull-comments-list ((api gh-pull-comments-api) user repo pull-id)
  (gh-api-authenticated-request
   api (gh-object-list-reader (oref api pull-comment-cls)) "GET"
   (format "/repos/%s/%s/pulls/%s/comments" user repo pull-id)))

(defmethod gh-pull-comments-get ((api gh-pull-comments-api) user repo pull-id)
  (gh-api-authenticated-request
   api (gh-object-reader (oref api pull-comment-cls)) "GET"
   (format "/repos/%s/%s/pulls/comments/%s" user repo pull-id)))

(defmethod gh-pull-comments-req-to-create ((req gh-pull-comments-comment))
  (let ((in-reply-to (oref req in-reply-to))
	(to-update `(("body" . ,(oref req body)))))
    (if in-reply-to
	(nconc to-update `(("in_reply_to" . ,in-reply-to)))
      (nconc to-update `(("commit_id" . ,(oref req commit-id))
			 ("path" . ,(oref req path))
			 ("position" . ,(oref req position)))))
    to-update))

(defmethod gh-pull-comments-req-to-update ((req gh-pull-comments-comment))
  `(("body" . ,(oref req body))))

(defmethod gh-pull-comments-update ((api gh-pull-comments-api) user repo comment-id comment)
  (gh-api-authenticated-request
   api (gh-object-reader (oref api pull-comment-cls)) "PATCH"
   (format "/repos/%s/%s/pulls/comments/%s" user repo comment-id)
   (gh-pull-comments-req-to-update comment)))

(defmethod gh-pull-comments-new ((api gh-pull-comments-api) user repo pull-id comment)
  (gh-api-authenticated-request
   api (gh-object-reader (oref api pull-comment-cls)) "POST"
   (format "/repos/%s/%s/pulls/%s/comments" user repo pull-id)
   (gh-pull-comments-req-to-create comment)))

(defmethod gh-pull-comments-delete ((api gh-pull-comments-api) user repo comment-id)
  (gh-api-authenticated-request
   api nil "DELETE"
   (format "/repos/%s/%s/pulls/comments/%s" user repo comment-id)))

(provide 'gh-pull-comments)
;;; gh-pull-comments.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
