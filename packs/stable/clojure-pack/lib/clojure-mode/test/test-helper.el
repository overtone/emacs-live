;;; test-helper.el --- Clojure Mode: Non-interactive unit-test setup  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2016 Bozhidar Batsov <bozhidar@batsov.com>

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

;; Non-interactive test suite setup for ERT Runner.

;;; Code:

(message "Running tests on Emacs %s" emacs-version)

(let* ((current-file (if load-in-progress load-file-name (buffer-file-name)))
       (source-directory (locate-dominating-file current-file "Cask"))
       ;; Do not load outdated byte code for tests
       (load-prefer-newer t))
  ;; Load the file under test
  (load (expand-file-name "clojure-mode" source-directory)))

(defmacro def-refactor-test (name before after &rest body)
  (declare (indent 3))
  `(progn
     (put ',name 'definition-name ',name)
     (ert-deftest ,name ()
       (let ((clojure-thread-all-but-last nil)
             (clojure-use-metadata-for-privacy nil))
         (with-temp-buffer
           (insert ,before)
           (clojure-mode)
           ,@body
           (should (equal ,(concat "\n" after)
                          (concat "\n" (buffer-substring-no-properties
                                        (point-min) (point-max))))))))))


;;; test-helper.el ends here
