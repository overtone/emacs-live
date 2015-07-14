;;; org-ebib.el - Support for links to Ebib's entries in Org
;;
;; Author: Grégoire Jadi <daimrod@gmail.com>
;;
;; This file is not yet part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

(require 'org)

(org-add-link-type "ebib" 'org-ebib-open)

(add-hook 'org-store-link-functions 'org-ebib-store-link)

(defun org-ebib-open (key)
  "Open Ebib and jump to KEY."
  (ebib nil key))

(defun org-ebib-store-link ()
  "Store a key to an Ebib entry."
  (when (memq major-mode '(ebib-index-mode ebib-entry-mode))
    ;; This is an Ebib entry
    (let* ((key (ebib-cur-entry-key))
           (link (concat "ebib:" key))
           (description (ignore-errors (ebib-db-get-field-value 'title key ebib-cur-db))))
      (org-store-link-props
       :type "ebib"
       :link link
       :description description))))

(provide 'org-ebib)

;;; org-ebib.el ends here
