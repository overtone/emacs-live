;;; git-modes.el --- Major modes for editing Git configuration files  -*- lexical-binding: t -*-

;; Copyright (c) 2012-2013  Sebastian Wiesner
;; Copyright (C) 2013-2015  Rüdiger Sonderfeld
;; Copyright (C) 2012-2021  The Magit Project Contributors

;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;;         Rüdiger Sonderfeld <ruediger@c-plusplus.net>
;;         Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/magit/git-modes
;; Keywords: convenience vc git
;; Package-Requires: ((emacs "24.3"))
;; Package-Version: 1.4.0
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3 of the License,
;; or (at your option) any later version.

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

;; Each mode is defined in its own library by the same name.
;; All additions to `auto-mode-alist' are autoloaded, so it is
;; not necessary load `git-modes' or the individual libraries.

;;; Code:

(require 'gitattributes-mode)
(require 'gitconfig-mode)
(require 'gitignore-mode)

(provide 'git-modes)

;;; git-modes.el ends here
