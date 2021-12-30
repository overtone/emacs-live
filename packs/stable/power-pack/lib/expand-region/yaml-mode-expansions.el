;;; yaml-mode-expansions.el --- expansions for yaml mode

;; Copyright (C) 2021 Aaron Gonzales

;; Author: Aaron Gonzales
;; Keywords: marking region yaml YAML expand

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;  - Additions implemented here:
;;    - er/mark-yaml-key-value
;;    - er/mark-yaml-list-item
;;    - er/mark-yaml-block
;;    - er/mark-yaml-outer-block
;;    - er/mark-yaml-inner-block


;;; Code:

(require 'expand-region-core)

(defconst yaml-indent 2)

(unless (fboundp 'yaml-indent-offset)
  (defalias 'yaml-indent-offset 'yaml-indent))

(defvar er--yaml-key-value-regex
  (rx (one-or-more
       (any "0-9A-Za-z"))
      ":"
      (zero-or-more " ")
      (one-or-more
       (any "0-9A-Za-z" " '_-"))))

(defvar er--yaml-list-item-regex
  (rx (seq "- "
           (one-or-more
            (any "0-9A-Za-z" "\"':=_-")))))

(defvar er--yaml-block-regex
  (rx (seq (zero-or-more
            (any " -"))
           (one-or-more
            (any "0-9A-Za-z" " '_-"))
           ":\n")))

(defun er--get-regex-indentation-level (regex)
  "Return the indentation level of the code with respect to the REGEX passed."
  (when (looking-at regex)
    ;; Block start means that the next level is deeper.
    (+ (current-indentation) yaml-indent-offset)
    ;; Assuming we're inside the block that we want to mark
    (current-indentation)))

(defun er/mark-yaml-line-base (regex)
  "Mark line of yaml file based on simple REGEX."
  (back-to-indentation)
  (when (looking-at regex)
    (set-mark (line-end-position))))

(defun er/mark-yaml-block-static-base (regex)
  "Mark yaml block based on REGEX passed.  NEXT-INDENT-LEVEL can be used to search outer blocks when necessary."
  ;; go bac to indentation so always can get regexp
  (back-to-indentation)
  ;; make sure the cursor is set inside the block
  ;; mark point at this higher code block
  (set-mark (point))
  ;; save level of this blocks indentation
  (let ((block-indentation (current-indentation)))
    (forward-line 1)
    (while (and
            ;; No need to go beyond the end of the buffer. Can't use
            ;; eobp as the loop places the point at the beginning of
            ;; line, but eob might be at the end of the line.
            (not (= (point-max) (point-at-eol)))
            ;; Proceed if: indentation is too deep
            (or (> (current-indentation) block-indentation)
                ;; Looking at an empty line
                (looking-at (rx line-start (* whitespace) line-end))
                ;; We're not looking at the start of a YAML block
                ;; and the indent is deeper than the block's indent
                (and (not (looking-at regex))
                     (> (current-indentation) block-indentation))))
      (forward-line 1)
      (back-to-indentation))
    ;; Find the end of the block by skipping comments backwards
    (python-util-forward-comment -1)
    (exchange-point-and-mark))
  (back-to-indentation))

(defun er/mark-yaml-block-base (regex &optional next-indent-level)
  "Mark yaml block based on REGEX passed.  NEXT-INDENT-LEVEL can be used to search outer blocks when necessary."
  ;; go bac to indentation so always can get regexp
  (back-to-indentation)
  ;; make sure the cursor is set inside the block
  (let ((next-indent-level
         (or
          ;; Use the given level
          next-indent-level
          ;; used to mark current block
          (er--get-regex-indentation-level regex))))
    ;; if true then at start of block and wanna mark itself
    ;; else were are inside the block already and will mark it)))
    ;; move up the code unti a parent code block is reached
    (while (and (>= (current-indentation) next-indent-level)
                (not (eq (current-indentation) 0)))
      (re-search-backward regex (point-min) t)
      (back-to-indentation))
    ;; mark point at this higher code block
    (set-mark (point))
    ;; save level of this blocks indentation
    (let ((block-indentation (current-indentation)))
      (forward-line 1)
      (while (and
              ;; No need to go beyond the end of the buffer. Can't use
              ;; eobp as the loop places the point at the beginning of
              ;; line, but eob might be at the end of the line.
              (not (= (point-max) (point-at-eol)))
              ;; Proceed if: indentation is too deep
              (or (> (current-indentation) block-indentation)
                  ;; Looking at an empty line
                  (looking-at (rx line-start (* whitespace) line-end))
                  ;; We're not looking at the start of a YAML block
                  ;; and the indent is deeper than the block's indent
                  (and (not (looking-at regex))
                       (> (current-indentation) block-indentation))))
        (forward-line 1)
        (back-to-indentation))
      ;; Find the end of the block by skipping comments backwards
      (python-util-forward-comment -1)
      (exchange-point-and-mark)))
  (back-to-indentation))

(defun er/mark-yaml-key-value ()
  "Mark a yaml key-value pair."
  (interactive)
  (er/mark-yaml-line-base er--yaml-key-value-regex))

(defun er/mark-yaml-list-item ()
  "Mark a yaml list item."
  (interactive)
  (er/mark-yaml-line-base er--yaml-list-item-regex))

(defun er/mark-yaml-inner-block ()
  "Mark the yaml contents of the block at point.  Command that wraps `er/mark-yaml-block-base'."
  (interactive)
  (er/mark-yaml-block-base er--yaml-block-regex (current-indentation))
  (forward-line)
  (back-to-indentation))

(defun er/mark-yaml-block ()
  "Mark the yaml block that point is currently at the top of.  Command that wraps `er/mark-yaml-block-base'."
  (interactive)
  (er/mark-yaml-block-static-base er--yaml-block-regex))

(defun er/mark-yaml-outer-block ()
  "Mark the outer yaml block that surrounds the block around point.  Command that wraps `er/mark-yaml-block-base'."
  (interactive)
  (er/mark-yaml-block-base er--yaml-block-regex (current-indentation)))

(defun er/add-yaml-mode-expansions ()
  "Add yaml-mode-specific expansions for buffers in yaml-mode."
  (let ((try-expand-list-additions '(er/mark-symbol
                                     er/mark-outside-quotes
                                     er/mark-yaml-list-item
                                     er/mark-yaml-key-value
                                     er/mark-yaml-block
                                     er/mark-yaml-outer-block
                                     er/mark-yaml-inner-block)))
    (set (make-local-variable 'expand-region-skip-whitespace) nil)
    (set (make-local-variable 'er/try-expand-list) try-expand-list-additions)))

(er/enable-mode-expansions 'yaml-mode 'er/add-yaml-mode-expansions)

(provide 'yaml-mode-expansions)

;;; yaml-mode-expansions.el ends here
