;;; gitignore-mode.el --- Major mode for editing .gitignore files -*- lexical-binding: t; -*-

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

;; A major mode for editing .gitignore files.

;;; Code:

(require 'rx)
(require 'conf-mode)

(defvar gitignore-mode-font-lock-keywords
  `((,(rx line-start (syntax comment-start) (zero-or-more not-newline) line-end)
     . 'font-lock-comment-face)
    (,(rx line-start (group (optional "!"))) ; Negated patterns
     (1 'font-lock-negation-char-face))
    ("/" . 'font-lock-constant-face)               ; Directory separators
    (,(rx (or "*" "?")) . 'font-lock-keyword-face) ; Glob patterns
    (,(rx "[" (minimal-match (one-or-more not-newline)) "]")
     . 'font-lock-keyword-face)         ; Ranged glob patterns
    ))

;;;###autoload
(define-derived-mode gitignore-mode conf-unix-mode "Gitignore"
  "A major mode for editing .gitignore files."
  (conf-mode-initialize "#")
  ;; Disable syntactic font locking, because comments are only valid at
  ;; beginning of line.
  (setq font-lock-defaults '(gitignore-mode-font-lock-keywords t t))
  (set (make-local-variable 'conf-assignment-sign) nil))

;;;###autoload
(dolist (pattern (list (rx "/.gitignore" string-end)
                       (rx "/.git/info/exclude" string-end)
                       (rx "/git/ignore" string-end)))
  (add-to-list 'auto-mode-alist (cons pattern 'gitignore-mode)))

(provide 'gitignore-mode)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; gitignore-mode.el ends here
