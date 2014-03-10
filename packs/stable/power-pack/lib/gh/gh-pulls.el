;;; gh-pulls.el --- pull requests module for gh.el

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

(require 'gh-repos)

(defclass gh-pulls-cache (gh-cache)
  ((invalidation-chain :allocation :class
                       :initform '(("^/repos/.*/.*/pulls$" . "\0")
                                   ("^/repos/.*/.*/pulls/.*$" . "\0")))))

;;;###autoload
(defclass gh-pulls-api (gh-api-v3)
  ((cache-cls :allocation :class :initform gh-pulls-cache)

   (req-cls :allocation :class :initform gh-pulls-request))
  "Git pull requests API")

(defclass gh-pulls-request-stub (gh-object)
  ((url :initarg :url)
   (html-url :initarg :html-url)
   (diff-url :initarg :diff-url)
   (patch-url :initarg :patch-url)
   (issue-url :initarg :issue-url)
   (number :initarg :number)
   (state :initarg :state)
   (title :initarg :title)
   (body :initarg :body)
   (created-at :initarg :created-at)
   (updated-at :initarg :updated-at)
   (closed-at :initarg :closed-at)
   (merged-at :initarg :merged-at)))

(defmethod gh-object-read-into ((stub gh-pulls-request-stub) data)
  (call-next-method)
  (with-slots (url html-url diff-url patch-url issue-url number
                   state title body created-at updated-at
                   closed-at merged-at)
      stub
    (setq url (gh-read data 'url)
          html-url (gh-read data 'html_url)
          diff-url (gh-read data 'diff_url)
          patch-url (gh-read data 'patch_url)
          issue-url (gh-read data 'issue_url)
          number (gh-read data 'number)
          state (gh-read data 'state)
          title (gh-read data 'title)
          body (gh-read data 'body)
          created-at (gh-read data 'created_at)
          updated-at (gh-read data 'updated_at)
          closed-at (gh-read data 'closed_at)
          merged-at (gh-read data 'merged_at))))

;;;###autoload
(defclass gh-pulls-request (gh-pulls-request-stub)
  ((merged :initarg :merged)
   (mergeable :initarg :mergeable)
   (merged-by :initarg :merged-by)
   (comments :initarg :comments)
   (user :initarg :user :initform nil)
   (commits :initarg :commits)
   (additions :initarg :additions)
   (deletions :initarg :deletions)
   (changed-files :initarg :changed-files)
   (head :initarg :head :initform nil)
   (base :initarg :base :initform nil)

   (ref-cls :allocation :class :initform gh-repos-ref)
   (user-cls :allocation :class :initform gh-user))
  "Git pull requests API")

(defmethod gh-object-read-into ((req gh-pulls-request) data)
  (call-next-method)
  (with-slots (merged mergeable
                      merged-by comments user commits additions
                      deletions changed-files head base)
      req
    (setq merged (gh-read data 'merged)
          mergeable (gh-read data 'mergeable)
          merged-by (gh-read data 'merged_by)
          comments (gh-read data 'comments)
          user (gh-object-read (or (oref req :user)
                                   (oref req user-cls))
                               (gh-read data 'user))
          commits (gh-read data 'commits)
          additions (gh-read data 'additions)
          deletions (gh-read data 'deletions)
          changed-files (gh-read data 'changed_files)
          head (gh-object-read (or (oref req :head)
                                   (oref req ref-cls))
                                (gh-read data 'head))
          base (gh-object-read (or (oref req :base)
                                   (oref req ref-cls))
                                (gh-read data 'base)))))

(defmethod gh-pulls-req-to-new ((req gh-pulls-request))
  (let ((head (oref req :head))
        (base (oref req :base)))
    `(("title" . ,(oref req :title))
      ("body" . ,(oref req :body))
      ("head" . ,(or (oref head :ref) (oref head :sha)))
      ("base" . ,(or (oref base :ref) (oref base :sha))))))

(defmethod gh-pulls-req-to-update ((req gh-pulls-request-stub))
  `(("title" . ,(oref req :title))
    ("body" . ,(oref req :body))
    ("state" . ,(oref req :state))))

(defmethod gh-pulls-list ((api gh-pulls-api) user repo)
  (gh-api-authenticated-request
   api (gh-object-list-reader (oref api req-cls)) "GET"
   (format "/repos/%s/%s/pulls" user repo)))

(defmethod gh-pulls-get ((api gh-pulls-api) user repo id)
  (gh-api-authenticated-request
   api (gh-object-reader (oref api req-cls)) "GET"
   (format "/repos/%s/%s/pulls/%s" user repo id)))

(defmethod gh-pulls-new ((api gh-pulls-api) user repo req)
  (gh-api-authenticated-request
   api (gh-object-reader (oref api req-cls)) "POST"
   (format "/repos/%s/%s/pulls" user repo)
   (gh-pulls-req-to-new req)))

(defmethod gh-pulls-update ((api gh-pulls-api) user repo id req)
  (gh-api-authenticated-request
   api (gh-object-reader (oref api req-cls)) "PATCH"
   (format "/repos/%s/%s/pulls/%s" user repo id)
   (gh-pulls-req-to-update req)))

(provide 'gh-pulls)
;;; gh-pulls.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
