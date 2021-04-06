;;; ob-php.el --- Execute PHP within org-mode blocks.
;; Copyright 2016 stardiviner

;; Author: stardiviner <numbchild@gmail.com>
;; Maintainer: stardiviner <numbchild@gmail.com>
;; Keywords: org babel php
;; URL: https://github.com/stardiviner/ob-php
;; Created: 04th May 2016
;; Version: 0.0.1
;; Package-Requires: ((org "8"))

;;; Commentary:
;;
;; Execute PHP within org-mode blocks.

;;; Code:
(require 'org)
(require 'ob)

(defgroup ob-php nil
  "org-mode blocks for PHP."
  :group 'org)

(defcustom ob-php:inf-php-buffer "*php*"
  "Default PHP inferior buffer."
  :group 'ob-php
  :type 'string)

;;;###autoload
(defun org-babel-execute:php (body params)
  "Orgmode Babel PHP evaluate function for `BODY' with `PARAMS'."
  (let* ((cmd "php")
         (body (concat "<?php\n" body "\n?>")))
    (org-babel-eval cmd body)
    ))

;;;###autoload
(eval-after-load "org"
  '(add-to-list 'org-src-lang-modes '("php" . php)))

(defvar org-babel-default-header-args:php '())

(add-to-list 'org-babel-default-header-args:php
             '(:results . "output"))

(provide 'ob-php)

;;; ob-php.el ends here
