;;; haskell-completions.el --- Haskell Completion package

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

;; This package provides completions related functionality for
;; Haskell Mode such grab completion prefix at point, and etc..

;;; Code:

(defun haskell-completions-can-grab-prefix ()
  "Check if the case is appropriate for grabbing completion prefix.
Returns t if point is either at whitespace character, or at
punctuation, or at line end and preceeding character is not a
whitespace or new line, otherwise returns nil.

  Returns nil in presense of active region."
  (when (not (region-active-p))
    (when (looking-at (rx (| space line-end punct)))
      (when (not (bobp))
        (save-excursion
          (backward-char)
          (not (looking-at (rx (| space line-end)))))))))

(provide 'haskell-completions)
;;; haskell-completions.el ends here
