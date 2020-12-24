;;; pkg-info-dummy-package.el --- pkg-info: Dummy package for unit tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Sebastian Wiesner

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;; URL: https://github.com/lunaryorn/pkg-info.el
;; Version: 3.4.2.1

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

;; A little dummy package for our tests.

;;; Code:

(defun pkg-info-dummy-package-dummy-function ()
  "Go ahead, nothing useful here."
  (message "Hello world"))

(provide 'pkg-info-dummy-package)

;;; pkg-info-dummy-package.el ends here
