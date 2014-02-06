;;; test-helper.el --- pkg-info: Unit test helper    -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Sebastian Wiesner

;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://github.com/lunaryorn/pkg-info.el

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

(require 'cl-lib)
(require 'find-func)
(require 'f)

(unless noninteractive
  (error "The test suite cannot be used interactively."))

(message "Running tests on Emacs %s" emacs-version)


;;;; Directories

(eval-and-compile
  (defconst pkg-info-test-directory (f-parent (f-this-file))
    "Directory of unit tests.")

  (defconst pkg-info-source-directory (file-name-as-directory
                                       (f-parent pkg-info-test-directory))
    "Directory of unit tests."))


;;;; Load pkg-info

;; Load compatibility libraries for Emacs 23
(load (f-join pkg-info-source-directory "compat" "load.el") nil 'no-message)

(require 'pkg-info (f-join pkg-info-source-directory "pkg-info.elc"))

;; Check that we are testing the right pkg-info library
(cl-assert (f-same? (symbol-file #'pkg-info-package-version 'defun)
                    (f-join pkg-info-source-directory "pkg-info.elc"))
           nil "ERROR: pkg-info not loaded from compiled source.  Run make compile")


;;;; Provide dummy packages for unit tests

(defconst pkg-info-test-package-directory
  (f-join pkg-info-test-directory "elpa")
  "Directory for temporary package installations.")

(defun pkg-info-test-initialize-packages ()
  "Initialize packages for our unit tests."
  (epl-change-package-dir pkg-info-test-package-directory)
  (unless (epl-package-installed-p 'pkg-info-dummy-package)
    ;; Only install the dummy package if needed
    (epl-add-archive "localhost" "http://127.0.0.1:9191/packages/")
    (epl-refresh)
    (let ((package (car (epl-find-available-packages 'pkg-info-dummy-package))))
      (unless package
        (error (error "Test dummy package missing. \
Start servant with cask exec servant start")))
      (epl-package-install package))))

(message "Running tests on Emacs %s" emacs-version)
(pkg-info-test-initialize-packages)


;;;; Utilities

(defconst pkg-info-version
  (let ((default-directory pkg-info-source-directory))
    (version-to-list (car (process-lines "cask" "version"))))
  "The pkg-info version, to use in unit tests.")

(provide 'test-helper)

;; Local Variables:
;; indent-tabs-mode: nil
;; coding: utf-8
;; End:

;;; test-helper.el ends here
