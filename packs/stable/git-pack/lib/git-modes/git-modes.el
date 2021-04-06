;;; git-modes.el --- Major modes for editing Git configuration files -*- lexical-binding: t; -*-

;; Copyright (c) 2012-2013  Sebastian Wiesner
;; Copyright (C) 2013-2015  Rüdiger Sonderfeld
;; Copyright (C) 2012-2016  The Magit Project Contributors

;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;;         Rüdiger Sonderfeld <ruediger@c-plusplus.net>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/magit/git-modes
;; Keywords: convenience vc git

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides several major modes for editing Git
;; configuration files.  The modes are:

;;    `gitattributes-mode'
;;    `gitconfig-mode', and
;;    `gitignore-mode'

;; Each mode is defined in its own library by the same name.  Loading
;; `git-modes' causes all three libraries to be loaded, but you could
;; also load the libraries individually.  On Melpa, the libraries are
;; distributed as separate packages.

;;; Code:

(require 'gitattributes-mode)
(require 'gitconfig-mode)
(require 'gitignore-mode)

(provide 'git-modes)

;;; gitignore-mode.el ends here
