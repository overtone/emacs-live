;;; gitconfig-mode.el --- Major mode for editing .gitconfig files -*- lexical-binding: t; -*-

;; Copyright (c) 2012-2013  Sebastian Wiesner

;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>
;; Version: 0.14.0
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

;; A major mode for editing .gitconfig files.

;;; Code:

(require 'rx)
(require 'conf-mode)

(defun gitconfig-line-indented-p ()
  "Determine whether the current line is indented correctly.

Return t if so, or nil otherwise."
  (save-excursion
    (beginning-of-line)
    (or (looking-at (rx line-start "["
                        symbol-start
                        (minimal-match (zero-or-more not-newline))
                        symbol-end "]"))
        (looking-at (rx line-start "\t"
                        symbol-start (or (syntax word)
                                         (syntax symbol)))))))

(defun gitconfig-point-in-indentation-p ()
  "Determine whether the point is in the indentation of the current line.

Return t if so, or nil otherwise."
  (save-excursion
    (let ((pos (point)))
      (back-to-indentation)
      (< pos (point)))))

(defun gitconfig-indent-line ()
  "Indent the current line."
  (interactive)
  (unless (gitconfig-line-indented-p)
    (let ((old-point (point-marker))
          (was-in-indent (gitconfig-point-in-indentation-p)))
      (beginning-of-line)
      (delete-horizontal-space)
      (unless (= (char-after) ?\[)
        (insert-char ?\t 1))
      (if was-in-indent
          (back-to-indentation)
        (goto-char (marker-position old-point))))))

(defvar gitconfig-mode-syntax-table
  (let ((table (make-syntax-table conf-unix-mode-syntax-table)))
    ;; ; is a comment in .gitconfig
    (modify-syntax-entry ?\; "<" table)
    table)
  "Syntax table to use in .gitconfig buffers.")

(defvar gitconfig-mode-font-lock-keywords
  `(
    ;; Highlight section and subsection gitconfig headers, and override
    ;; syntactic fontification in these.
    (,(rx line-start (zero-or-more (syntax whitespace))
          "[" symbol-start
          (group (one-or-more (or (syntax word) (syntax symbol))))
          symbol-end
          (optional (one-or-more (syntax whitespace))
                    (group (syntax string-quote)
                           (minimal-match (one-or-more not-newline))
                           (syntax string-quote)))
          "]" (zero-or-more (syntax whitespace)) line-end)
     (1 'font-lock-type-face t nil)
     (2 'font-lock-function-name-face t t))
    (,(rx line-start (zero-or-more (syntax whitespace)) symbol-start
          (group (one-or-more (or (syntax word) (syntax symbol))))
          symbol-end (zero-or-more (syntax whitespace))
          (optional "=" (zero-or-more not-newline)) line-end)
     (1 'font-lock-variable-name-face))
    ;; Highlight booleans and numbers
    (,(rx "="
          (zero-or-more (syntax whitespace)) word-start
          (group (or "yes" "no" "true" "false" "on" "off"))
          word-end (zero-or-more (syntax whitespace)) line-end)
     (1 'font-lock-keyword-face))
    (,(rx "="
          (zero-or-more (syntax whitespace)) word-start
          (group (one-or-more digit))
          word-end (zero-or-more (syntax whitespace)) line-end)
     (1 'font-lock-constant-face))))

;;;###autoload
(define-derived-mode gitconfig-mode conf-unix-mode "Gitconfig"
  "A major mode for editing .gitconfig files."
  ;; .gitconfig is indented with tabs only
  (conf-mode-initialize "#" gitconfig-mode-font-lock-keywords)
  (setq indent-tabs-mode t)
  (set (make-local-variable 'indent-line-function)
       'gitconfig-indent-line))

;;;###autoload
(dolist (pattern (list (rx "/.gitconfig" string-end)
                       (rx "/.git/config" string-end)
                       (rx "/git/config" string-end)))
  (add-to-list 'auto-mode-alist (cons pattern 'gitconfig-mode)))

(provide 'gitconfig-mode)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; gitconfig-mode.el ends here
