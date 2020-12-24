;;; interactive-haskell-mode-tests.el --- Tests for Haskell Interactive Mode  -*- lexical-binding: t -*-

;; Copyright © 2016 Athur Fayzrakhmanov. All rights reserved.

;; This file is part of haskell-mode package.
;; You can contact the authors using GitHub issue tracker:
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

;; This package provides regression tests for the package
;; haskell-interactive-mode.

;;; Code:


(require 'ert)
(require 'haskell-interactive-mode)

(defun should-match (str)
  (should (eq 0 (string-match-p haskell-interactive-mode-error-regexp str))))

(ert-deftest haskell-interactive-error-regexp-test ()
  "Tests the regexp `haskell-interactive-mode-error-regexp'"
  (should (eq 0 (string-match-p haskell-interactive-mode-error-regexp
                                "/home/user/Test.hs:24:30:")))
  (should (eq 0 (string-match-p haskell-interactive-mode-error-regexp
                                "Test.hs:5:18:")))
  (should (eq 0 (string-match-p haskell-interactive-mode-error-regexp
                                "Test.hs:7:6: Not in scope: type constructor or class ‘Ty’")))
  (should (eq 0 (string-match-p haskell-interactive-mode-error-regexp
                                "Test.hs:9:5: Not in scope: ‘c’")))
  (should (eq nil (string-match-p haskell-interactive-mode-error-regexp
                                  ;; leading space
                                  " Test.hs:8:9:")))
  )
