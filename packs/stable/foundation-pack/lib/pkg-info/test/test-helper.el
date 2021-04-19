;;; test-helper.el --- pkg-info: Unit test helper    -*- lexical-binding: t; -*-

;; Copyright (C) 2013, 2014  Sebastian Wiesner

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;; URL: https://github.com/lunaryorn/pkg-info.el

;; This file is not part of GNU Emacs.

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

;; Unit test helper for pkg-info

;;; Code:

(unless noninteractive
  (error "The test suite cannot be used interactively"))

(message "Running tests on Emacs %s" emacs-version)

(let* ((current-file (if load-in-progress load-file-name (buffer-file-name)))
       (source-directory (locate-dominating-file current-file "Cask"))
       ;; Do not load outdated byte code for tests
       (load-prefer-newer t))

  (load (expand-file-name "pkg-info" source-directory))

  ;; Point package.el to our test packages
  (setq package-user-dir (expand-file-name "test/elpa" source-directory))
  (package-initialize))

;; Local Variables:
;; indent-tabs-mode: nil
;; coding: utf-8
;; End:

;;; test-helper.el ends here
