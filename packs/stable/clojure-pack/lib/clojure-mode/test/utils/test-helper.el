;;; test-helper.el --- Clojure Mode: Non-interactive unit-test setup  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2020 Bozhidar Batsov <bozhidar@batsov.com>

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

;; Non-interactive test suite setup.

;;; Code:

(message "Running tests on Emacs %s" emacs-version)

(let* ((current-file (if load-in-progress load-file-name (buffer-file-name)))
       (source-directory (locate-dominating-file current-file "Cask"))
       ;; Do not load outdated byte code for tests
       (load-prefer-newer t))
  ;; Load the file under test
  (load (expand-file-name "clojure-mode" source-directory)))

(defmacro with-clojure-buffer (text &rest body)
  "Create a temporary buffer, insert TEXT, switch to clojure-mode and evaluate BODY."
  (declare (indent 1))
  `(with-temp-buffer
     (erase-buffer)
     (insert ,text)
     (clojure-mode)
     ,@body))

(defmacro when-refactoring-it (description before after &rest body)
  "Return a buttercup spec.

Insert BEFORE into a buffer, evaluate BODY and compare the resulting buffer to
AFTER.

BODY should contain the refactoring that transforms BEFORE into AFTER.

DESCRIPTION is the description of the spec."
  (declare (indent 1))
  `(it ,description
     (with-clojure-buffer ,before
       ,@body
       (expect (buffer-string) :to-equal ,after))))

;;; test-helper.el ends here
