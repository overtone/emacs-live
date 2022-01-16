;;; clojure-mode-bytecomp-warnings.el --- Check for byte-compilation problems  -*- lexical-binding: t; -*-

;; Copyright © 2012-2021 Bozhidar Batsov and contributors
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This is a script to be loaded while visiting a `clojure-mode' source file.
;; It will prepare all requirements and then byte-compile the file and signal an
;; error on any warning.  For example:
;;
;;    emacs -Q --batch -l test/clojure-mode-bytecomp-warnings.el clojure-mode.el

;; This assumes that all `clojure-mode' dependencies are already on the package
;; dir (probably from running `cask install').

(setq load-prefer-newer t)
(add-to-list 'load-path (expand-file-name "./"))
(require 'package)
(package-generate-autoloads 'clojure-mode default-directory)
(package-initialize)
(load-file "clojure-mode-autoloads.el")
(setq byte-compile-error-on-warn t)
(batch-byte-compile)

;;; clojure-mode-bytecomp-warnings.el ends here
