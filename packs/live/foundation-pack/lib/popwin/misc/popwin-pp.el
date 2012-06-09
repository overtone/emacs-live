;;; popwin-pp.el --- Popwin Pp

;; Copyright (C) 2012  Tomohiro Matsuyama

;; Author: Tomohiro Matsuyama <tomo@cx4a.org>
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

;; 

;;; Code:

(require 'popwin)
(require 'pp)

(defadvice pp-display-expression (around popwin-pp:pp-display-expression (expression out-buffer-name) activate)
  (let (not-found)
    (popwin:display-buffer-1 out-buffer-name
                             :if-config-not-found (lambda (buffer) (setq not-found t) ad-do-it))
    (unless not-found
      (let ((buffer (get-buffer out-buffer-name)))
        (with-current-buffer buffer
          (delete-region (point-min) (point-max))
          (pp expression buffer)
          (emacs-lisp-mode))))))

(provide 'popwin-pp)
;;; popwin-pp.el ends here
