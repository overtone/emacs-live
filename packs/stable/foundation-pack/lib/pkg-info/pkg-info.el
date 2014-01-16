;;; pkg-info.el --- Information about packages       -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Sebastian Wiesner

;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://github.com/lunaryorn/pkg-info.el
;; Keywords: convenience
;; Version: 0.2-cvs
;; Package-Requires: ((dash "1.6.0") (s "1.6.0"))

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

;; This library extracts information from installed packages.

;;;; Functions:

;; `pkg-info-library-version' extracts the version from the header of a library.
;;
;; `pkg-info-defining-library-version' extracts the version from the header of a
;;  library defining a function.
;;
;; `pkg-info-package-version' gets the version of an installed package.
;;
;; `pkg-info-format-version' formats a version list as human readable string.

;;; Code:

(require 'dash)
(require 's)
(require 'package)


;;;; Version information
(defun pkg-info-format-version (version)
  "Format VERSION as human-readable string.

Return a human-readable string representing VERSION."
  ;; XXX: Find a better, more flexible way of formatting?
  (package-version-join version))

(defsubst pkg-info--show-version-and-return (version show)
  "Show and return VERSION.

When SHOW is non-nil, show VERSION in minibuffer.

Return VERSION."
  (when show
    (message (pkg-info-format-version version)))
  version)

(defun pkg-info-locate-feature-source (feature)
  "Get the source file for FEATURE.

Return the source file as string, or nil if FEATURE was not
found."
  (-when-let* ((library (locate-library (symbol-name feature)))
               (s-chop-suffix "c" library))
    (when (file-exists-p library)
      library)))

;;;###autoload
(defun pkg-info-library-version (feature-or-file &optional show)
  "Get the version in the header of FEATURE-OR-FILE.

FEATURE-OR-FILE is either a symbol denoting a named feature, or a
string with the path to a library.

When SHOW is non-nil, show the version in the minibuffer.

Return the version from the library header as list or nil, if the
library was not found or had no proper library header.  See Info
node `(elisp)Library Headers' for more information about library
headers."
  (interactive
   (list (->> (completing-read "Load library: "
                               (apply-partially 'locate-file-completion-table
                                                load-path
                                                (get-load-suffixes)))
           locate-library
           (s-chop-suffix "c"))
         t))
  (-when-let (source-file (if (symbolp feature-or-file)
                              (pkg-info-locate-feature-source feature-or-file)
                            feature-or-file))
    (with-temp-buffer
      (insert-file-contents source-file)
      (let ((info (package-buffer-info)))
        (pkg-info--show-version-and-return
         (if (fboundp 'package-desc-version)
             (package-desc-version info)
           (version-to-list (aref (package-buffer-info) 3)))
         show)))))

;;;###autoload
(defun pkg-info-defining-library-version (function &optional show)
  "Get the version of the library defining FUNCTION.

When SHOW is non-nil, show the version in mini-buffer.

This function is mainly intended to find the version of a major
mode, i.e.

   (pkg-info-defining-library-version 'flycheck-mode)

Return the version of the library defining FUNCTION (as by
`pkg-info-locate-library-version'), or nil if the library was not
found or had no version."
  (interactive
   (let ((input (completing-read "Function: " obarray #'boundp :require-match)))
     (list (if (string= input "") nil (intern input)) t)))
  (-when-let* ((definition (symbol-function function))
               (source-file (find-lisp-object-file-name function definition)))
    (pkg-info-library-version source-file show)))

;;;###autoload
(defun pkg-info-package-version (package &optional show)
  "Get the version of an installed PACKAGE.

When SHOW is non-nil, show the version in the minibuffer.

Return the version as list, or nil if PACKAGE is not installed."
  (interactive
   (list (intern
          (completing-read "Installed package: "
                           (--map (symbol-name (car it)) package-alist)
                           nil :require-match
                           nil nil (symbol-name (caar package-alist))))
         t))
  (-when-let (info (assq package package-alist))
    (pkg-info--show-version-and-return
     (if (fboundp 'package-desc-version)
         (package-desc-version (cadr info))
       (aref (cdr info) 0))
     show)))

(provide 'pkg-info)

;;; pkg-info.el ends here
