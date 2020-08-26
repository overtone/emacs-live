;;; haskell-test-utils.el --- Utilities for Haskell Mode tests.  -*- lexical-binding: t -*-

;; Copyright © 2017 Vasantha Ganesh Kanniappan <vasanthaganesh.k@tuta.io>

;; This file is part of haskell-mode package.
;; You can contact with authors using GitHub issue tracker:
;; https://github.com/haskell/haskell-mode/issues

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file provides tests for `haskell-test-utils.el'

(require 'haskell-test-utils)

(ert-deftest haskell-with-temp-dir-structure-test ()
  (setq cur-haskell-dir default-directory)
  (with-temp-dir-structure
   (("a.hs" . "-- Empty file")
    ("abc" . (("b.hs" . "-- Empty file"))))
   (cd "abc"))
  (should (eq default-directory cur-haskell-dir)))
