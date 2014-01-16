;;; popwin-w3m.el --- Popwin Configuration for w3m

;; Copyright (C) 2011, 2012  Tomohiro Matsuyama

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
(require 'browse-url)
(require 'w3m)

(defcustom popwin-w3m:w3m-special-display-config nil
  "Special display config for w3m buffers. Each element has a
form of (URL-REGEXP . KEYWORDS), where URL-REGEXP is a regexp to
match URL for displaying w3m buffers with popwin, and KEYWORDS is
same as `popwin:special-display-config'."
  :group 'popwin)
(defvaralias 'popwin:w3m-special-display-config 'popwin-w3m:w3m-special-display-config)

(defun popwin-w3m:w3m-browse-url (url &optional new-session)
  (interactive (browse-url-interactive-arg "Emacs-w3m URL: "))
  (loop for (pattern . keywords) in popwin-w3m:w3m-special-display-config
        when (and (stringp url) (string-match pattern url))
        return
        (let ((popwin:special-display-config `((w3m-mode ,@keywords))))
          (flet ((w3m-popup-buffer (buffer) (display-buffer buffer)))
            (w3m-browse-url url new-session)))
        finally return
        (w3m-browse-url url new-session)))
(defalias 'popwin:w3m-browse-url 'popwin-w3m:w3m-browse-url)

(provide 'popwin-w3m)
;;; popwin-w3m.el ends here
