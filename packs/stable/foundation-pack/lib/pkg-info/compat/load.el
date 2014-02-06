;;; load.el --- EPL: Load compatibility libraries -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Sebastian Wiesner

;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;;     Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: http://github.com/cask/epl

;; This file is NOT part of GNU Emacs.

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

;; Load compatibility libraries if necessary.

;;; Code:

(require 'f)

(defconst epl-compat-directory (f-parent (f-this-file))
  "Directory of EPL compatibility libraries.")

(defun epl-compat-load-when-compat (library)
  (unless (locate-library library)
    (load (f-join epl-compat-directory library) nil 'no-message)))

(epl-compat-load-when-compat "package")
(epl-compat-load-when-compat "ert")

;;; load.el ends here
