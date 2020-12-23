;;; haskell-doc-tests.el --- Tests for `haskell-docs' package -*- lexical-binding: t -*-

;; Copyright © 2016 Arthur Fayzrakhmanov. All rights reserved.

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

;; This package provides regression tests for haskell-docs package.

;;; Code:

(require 'ert)
(require 'haskell-mode)
(require 'haskell-doc)
(require 'haskell-test-utils)
(require 'haskell-utils)


(ert-deftest interactive-prompt-state ()
  (with-temp-buffer
    (haskell-mode)
    (haskell-doc-mode)
    (insert-lines "module A where"
                  "import B")
    (goto-char (point-min))
    (forward-line)
    (should (string=
             "import [qualified] modid [as modid] [impspec]"
             (haskell-doc-mode-print-current-symbol-info)))
    (haskell-mode-toggle-interactive-prompt-state)
    (should (eq nil
                   (haskell-doc-mode-print-current-symbol-info)))
    (haskell-mode-toggle-interactive-prompt-state t)
    (should (string=
             "import [qualified] modid [as modid] [impspec]"
             (haskell-doc-mode-print-current-symbol-info)))))

;;; haskell-doc-tests.el ends here
