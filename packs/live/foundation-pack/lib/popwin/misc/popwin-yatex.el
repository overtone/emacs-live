;;; popwin-yatex.el --- Popwin Configuration for YaTeX

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

;; This is a workaround for working with YaTeX. Write the following
;; configuration into .emacs:
;; 
;; (require 'popwin-yatex)
;; (push '("*YaTeX-typesetting*") popwin:special-display-config)

;;; Code:

(require 'popwin)
(require 'yatex)

(defadvice YaTeX-showup-buffer (around popwin-yatex:YaTeX-showup-buffer (buffer &optional func select) activate)
  (popwin:display-buffer-1 buffer
                           :default-config-keywords `(:noselect ,(not select))
                           :if-config-not-found (lambda (buffer) ad-do-it)))

(provide 'popwin-yatex)
;;; popwin-yatex.el ends here
