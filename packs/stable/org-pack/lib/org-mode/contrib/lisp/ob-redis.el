;;; ob-redis.el --- Execute Redis queries within org-mode blocks.
;; Copyright 2016 stardiviner

;; Author: stardiviner <numbchild@gmail.com>
;; Maintainer: stardiviner <numbchild@gmail.com>
;; Keywords: org babel redis
;; URL: https://github.com/stardiviner/ob-redis
;; Created: 28th Feb 2016
;; Version: 0.0.1
;; Package-Requires: ((org "8"))

;;; Commentary:
;;
;; Execute Redis queries within org-mode blocks.

;;; Code:
(require 'org)
(require 'ob)

(defgroup ob-redis nil
  "org-mode blocks for Redis."
  :group 'org)

(defcustom ob-redis:default-db "127.0.0.1:6379"
  "Default Redis database."
  :group 'ob-redis
  :type 'string)

;;;###autoload
(defun org-babel-execute:redis (body params)
  "org-babel redis hook."
  (let* ((db (or (cdr (assoc :db params))
                 ob-redis:default-db))
         (cmd (mapconcat 'identity (list "redis-cli") " ")))
    (org-babel-eval cmd body)
    ))

;;;###autoload
(eval-after-load 'org
  '(add-to-list 'org-src-lang-modes '("redis" . redis)))

(provide 'ob-redis)

;;; ob-redis.el ends here
