;;; haskell-decl-scan-tests.el -*- lexical-binding: t -*-

;; Copyright © 2016 Chris Gregory. All rights reserved.

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

;; This package provides regression tests for haskell-decl-scan package.

;;; Code:

(require 'ert)
(require 'haskell-decl-scan)
(require 'haskell-test-utils)

(ert-deftest haskell-ds-line-commented-p-1 ()
  "All lines in this buffer should count as comments"
  (with-temp-buffer
    (haskell-mode)
    (insert-lines "" "--hi" "  -- hi\t " "" "{-hi-}" " \t{-hi-}  "
                  "-- | hi" "{-| hi -}")
    (font-lock-fontify-buffer)
    (goto-char (point-min))

    (while (not (eobp))
      (should (haskell-ds-line-commented-p))
      (forward-line))))

(ert-deftest haskell-ds-comment-p-1 ()
  "All characters in this buffer should count as comments"
  (with-temp-buffer
    (haskell-mode)
    (insert-lines "" "--hi" "  -- hi\t " "" "{-hi-}" " \t{-hi-}  ")
    (font-lock-fontify-buffer)
    (goto-char (point-min))

    (while (not (bobp))
      (should (haskell-ds-comment-p))
      (forward-char))))

(ert-deftest haskell-ds-backward-decl-1 ()
  "Test running haskell-ds-backward-decl"
  (with-temp-buffer
    (insert-lines "" "fun :: Int -> Int" "fun = id"
                  "" "f2 :: Int" "f2 = 3" "")
    (goto-char (point-max))

    (should (haskell-ds-backward-decl))
    (should (looking-at-p "f2 :: Int"))

    (should (haskell-ds-backward-decl))
    (should (looking-at-p "fun :: Int -> Int"))

    (should-not (haskell-ds-backward-decl))
    (should (bobp))))

(ert-deftest haskell-ds-backward-decl-2-commented ()
  "Test running haskell-ds-backward-decl"
  (with-temp-buffer
    (haskell-mode)
    (insert-lines "" "-- documentation" "fun :: Int -> Int"
                  "" "{- comment -}" "fun = id"
                  "" "  -- space comment" "f2 :: Int"
                  "" " {- trailing -} \t" "f2 = 3"
                  "" "" "")
    (font-lock-fontify-buffer)
    (goto-char (point-max))

    (should (haskell-ds-backward-decl))
    (should (looking-at-p "f2 :: Int"))

    (should (haskell-ds-backward-decl))
    (should (looking-at-p "fun :: Int -> Int"))

    (should-not (haskell-ds-backward-decl))
    (should (bobp))))

(ert-deftest haskell-ds-backward-decl-2 ()
  "Test running haskell-ds-backward-decl"
  (with-temp-buffer
    (insert-lines "" "" "fun :: Int -> Int"
                  "" "" "fun = id"
                  "" "" "f2 :: Int"
                  "" "" "f2 = 3"
                  "" "" "")
    (goto-char (point-max))

    (should (haskell-ds-backward-decl))
    (should (looking-at-p "f2 :: Int"))

    (should (haskell-ds-backward-decl))
    (should (looking-at-p "fun :: Int -> Int"))

    (should-not (haskell-ds-backward-decl))
    (should (= (point-min) (point)))))

(ert-deftest haskell-ds-forward-decl-1 ()
  "Test running haskell-ds-forward-decl"
  (with-temp-buffer
    (insert-lines "" "fun :: Int -> Int" "fun = id"
                  "" "f2 :: Int" "f2 = 3"
                  "")
    (goto-char (point-min))

    (should (haskell-ds-forward-decl))
    (should (looking-at-p "$"))
    (should (= (point) (save-excursion (goto-line 4) (point))))

    (should (haskell-ds-forward-decl))
    (should (looking-at-p "f2 :: Int"))

    (should (= (point-max) (haskell-ds-forward-decl)))
    (should (eobp))))

(ert-deftest haskell-ds-forward-decl-2 ()
  "Test running haskell-ds-forward-decl"
  (with-temp-buffer
    (insert-lines "" "" "fun :: Int -> Int"
                  "" "" "fun = id"
                  "" "" "f2 :: Int"
                  "" "" "f2 = 3"
                  "" "" "")
    (goto-char (point-min))

    (should (haskell-ds-forward-decl))
    (should (looking-at-p "$"))
    (should (= (point) (save-excursion (goto-line 7) (point))))

    (should (haskell-ds-forward-decl))
    (should (looking-at-p "f2 :: Int"))

    (should (haskell-ds-forward-decl))
    (should (= (point) (save-excursion (goto-line 13) (point))))

    (should (= (point-max) (progn (haskell-ds-forward-decl) (point))))
    (should (eobp))))

(ert-deftest haskell-ds-forward-decl-2-commented ()
  "Test running haskell-ds-backward-decl"
  (with-temp-buffer
    (haskell-mode)
    (insert-lines "" "-- documentation" "fun :: Int -> Int"
                  "" "{- comment -}" "fun = id"
                  "" "  -- space comment" "f2 :: Int"
                  "" " {- trailing -} \t" "f2 = 3"
                  "" "" "")
    (font-lock-fontify-buffer)
    (goto-char (point-min))

    (should (haskell-ds-forward-decl))
    (should (looking-at-p "$"))
    (should (= (point) (save-excursion (goto-line 7) (point))))

    (should (haskell-ds-forward-decl))
    (should (looking-at-p "f2 :: Int"))

    (should (haskell-ds-forward-decl))
    (should (= (point) (save-excursion (goto-line 13) (point))))

    (should (= (point-max) (progn (haskell-ds-forward-decl) (point))))
    (should (eobp))))

(provide 'haskell-decl-scan-tests)

;;; haskell-decl-scan-tests.el ends here
