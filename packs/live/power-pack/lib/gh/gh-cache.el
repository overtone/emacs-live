;;; gh-cache.el --- caching for gh.el

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

(require 'pcache)

(defclass gh-cache (pcache-repository)
  ((entries :initarg :entries :initform (make-hash-table :test 'equal))
   (safe-methods :allocation :class :initform ("HEAD" "GET" "OPTIONS" "TRACE"))
   (invalidation-chain :allocation :class :initform nil)))

(defmethod pcache-invalidate :after ((cache gh-cache) key)
  (let ((resource (car key)))
    (pcache-map cache #'(lambda (k v)
                          (when (equal (car k) resource)
                            (pcache-invalidate cache k))))
    (dolist (next (oref cache invalidation-chain))
      (let ((nextresource
             (replace-regexp-in-string (car next) (cdr next) resource)))
        (when (not (equal nextresource resource))
          (pcache-map cache #'(lambda (k v)
                                (when (equal (car k) nextresource)
                                  (pcache-invalidate cache k)))))))))

(provide 'gh-cache)
;;; gh-cache.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
