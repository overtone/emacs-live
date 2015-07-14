;;; haskell-completions-tests.el --- Tests for Haskell Completion package

;; Copyright © 2015 Athur Fayzrakhmanov. All rights reserved.

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

;; This package provides regression tests for haskell-completions package.

;;; Code:

(require 'ert)
(require 'haskell-mode)
(require 'haskell-completions)


(ert-deftest haskell-completions-can-grab-prefix-test ()
  "Tests the function `haskell-completions-can-grab-prefix'."
  (with-temp-buffer
    (haskell-mode)
    (should (eql nil (haskell-completions-can-grab-prefix)))
    (insert " ")
    (should (eql nil (haskell-completions-can-grab-prefix)))
    (insert "a")
    (should (eql t (haskell-completions-can-grab-prefix)))
    (save-excursion
      (insert " ")
      (should (eql nil (haskell-completions-can-grab-prefix)))
      (insert "bc-")
      (should (eql t (haskell-completions-can-grab-prefix)))
      (insert "\n")
      (should (eql nil (haskell-completions-can-grab-prefix)))
      (insert "def:#!")
      (should (eql t (haskell-completions-can-grab-prefix))))
    (should (eql t (haskell-completions-can-grab-prefix)))
    ;; punctuation tests
    (save-excursion (insert ")"))
    (should (eql t (haskell-completions-can-grab-prefix)))
    (save-excursion (insert ","))
    (should (eql t (haskell-completions-can-grab-prefix)))
    (save-excursion (insert "'"))
    (should (eql t (haskell-completions-can-grab-prefix)))
    ;; should return nil in the middle of word
    (save-excursion (insert "bcd"))
    (should (eql nil (haskell-completions-can-grab-prefix)))
    ;; region case
    (let ((p (point)))
      (goto-char (point-min))
      (push-mark)
      (goto-char p)
      (activate-mark)
      (should (eql nil (haskell-completions-can-grab-prefix))))))


(provide 'haskell-completions-tests)
;;; haskell-completions-tests.el ends here
