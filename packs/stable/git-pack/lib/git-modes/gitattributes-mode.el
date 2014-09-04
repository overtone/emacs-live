;;; gitattributes-mode.el --- Major mode for editing .gitattributes files -*- lexical-binding: t -*-

;; Copyright (C) 2013-2014  The Magit Project Developers

;; Author: Rüdiger Sonderfeld <ruediger@c-plusplus.de>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/magit/git-modes
;; Keywords: convenience vc git

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; A major mode for editing .gitattributes files.  See
;; the gitattributes(5) manpage for more information.
;; `eldoc-mode' is supported for known attributes.

;;; Code:

(require 'easymenu)
(require 'thingatpt)

(defgroup gitattributes-mode nil
  "Edit .gitattributes files."
  :link '(url-link "https://github.com/magit/git-modes")
  :prefix "gitattributes-mode-"
  :group 'tools)

(defcustom gitattributes-mode-enable-eldoc t
  "Enable `eldoc-mode' when loading `gitattributes-mode'.
This provides documentation for known variables in the echo area.
Alternatively add `turn-on-eldoc-mode' to the mode hook."
  :type 'boolean
  :group 'gitattributes-mode)

(defcustom gitattributes-mode-man-function #'man
  "Function to open the gitattributes(5) manpage."
  :type '(choice (const :tag "Man" #'man)
                 (const :tag "Woman" #'woman)
                 (function :tag "Function"))
  :group 'gitattributes-mode)

(defun gitattributes-mode-help ()
  "Open the gitattributes(5) manpage using `gitattributes-mode-man-function'."
  (interactive)
  (funcall gitattributes-mode-man-function "gitattributes"))

(defconst gitattributes-mode-attributes
  '(("text"
     "This attribute enables and controls end-of-line normalization."
     (t "auto"))
    ("eol"
     "This attribute sets a specific line-ending style to be used in the \
working directory."
     ("crlf" "lf"))
    ("ident" "Handle $Id$." t)
    ("filter"
     "A filter attribute can be set to a string value that names a filter \
driver specified in the configuration."
     string)
    ("diff"
     "The attribute diff affects how Git generates diffs for particular files."
     (t string "ada" "bibtex" "cpp" "csharp" "fortran" "html" "java"
        "matlab" "objc" "pascal" "perl" "php" "python" "ruby" "tex"))
    ("merge"
     "The attribute merge affects how three versions of a file are merged."
     (t string "text" "binary" "union"))
    ("conflict-marker-size"
     "This attribute controls the length of conflict markers left in the work \
tree file during a conflicted merge."
     (number))
    ("whitespace"
     "The core.whitespace configuration variable allows you to define what \
diff and apply should consider whitespace errors for all paths in the project."
     (t string))
    ("export-ignore"
     "Files and directories with the attribute export-ignore won’t be added to \
archive files."
     t)
    ("export-subst"
     "If the attribute export-subst is set for a file then Git will expand \
several placeholders when adding this file to an archive."
     t)
    ("delta"
     "Delta compression will not be attempted for blobs for paths with the \
attribute delta set to false."
     t)
    ("encoding"
     "The encoding used for the file in GUI Tools (e.g., gitk(1) and \
git-gui(1))."
     string))
  "List of known attributes.
Format (NAME DOC ALLOWED-STATES).
NAME should be the name as a string.
DOC should be a short doc-string.
ALLOWED-STATE should be a list or single symbol or string of allowed values.
t means the attribute can be Set or Unset.  `string' means the symbol value
can be any string and `number' means the value should be a number.")

(defun gitattributes-mode-eldoc (&optional no-state)
  "Support for `eldoc-mode'.
If NO-STATE is non-nil then do not print state."
  (let (entry)
    (when (and (thing-at-point-looking-at "\\s-+\\(-\\|!\\)?\\(\\(?:\\sw-?\\)+\\)\\(=\\)?")
               (setq entry (assoc-string (match-string 2)
                                         gitattributes-mode-attributes)))
      (concat (unless no-state
                (cond
                 ((string= (match-string 1) "-") "[Unset] ")
                 ((string= (match-string 1) "!") "[Unspecified] ")
                 ((string= (match-string 3) "=") "[Set to a value] ")
                 (t "[Set] ")))
              (cadr entry)))))

(defvar gitattributes-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\s " " table)
    (modify-syntax-entry ?\t " " table)
    (modify-syntax-entry ?- "_." table)
    (modify-syntax-entry ?! "." table)
    (modify-syntax-entry ?= "." table)
    table)
  "Syntax table for `gitattributes-mode'.")

(defun gitattributes-mode--highlight-1st-field (regexp)
  "Highlight REGEXP in the first field only."
  `(lambda (limit)
     (let ((old-limit limit))
       (save-excursion
         (beginning-of-line)
         (when (re-search-forward "[[:space:]]" limit 'noerror)
           (setq limit (point))))
       (unless (< limit (point))
         (if (re-search-forward ,regexp limit 'noerror)
             t
           (forward-line)
           (when (< (point) old-limit)
             (gitattributes-mode--highlight-1st-field old-limit)))))))

(defvar gitattributes-mode-font-lock-keywords
  `(("^\\s-*#.*" . 'font-lock-comment-face)
    ("^\\[attr]" . 'font-lock-function-name-face)
    ("\\s-+\\(-\\|!\\)[[:word:]]+" (1 'font-lock-negation-char-face))
    ("\\s-+\\(?:-\\|!\\)?\\(\\sw\\(?:\\sw\\|\\s_\\)*\\)=?"
     (1 'font-lock-variable-name-face))
    ;; Pattern highlight similar to `gitignore-mode-font-lock-keywords'
    (,(gitattributes-mode--highlight-1st-field "/") . 'font-lock-constant-face)
    (,(gitattributes-mode--highlight-1st-field "[*?]") . 'font-lock-keyword-face)
    (,(gitattributes-mode--highlight-1st-field "\\[.+?]") . 'font-lock-keyword-face))
  "Keywords for highlight in `gitattributes-mode'.")

(defun gitattributes-mode-forward-field (&optional arg)
  "Move point ARG fields forward.
If ARG is omitted or nil, move point forward one field."
  (interactive "p")
  (if (< arg 0)
      (gitattributes-mode-backward-field (- arg))
    (dotimes (_ (or arg 1))
      (re-search-forward "\\s-[!-]?\\<" nil 'move))))

(defun gitattributes-mode-backward-field (&optional arg)
  "Move point ARG fields backward.
If ARG is omitted or nil, move point backward one field."
  (interactive "p")
  (if (< arg 0)
      (gitattributes-mode-forward-field (- arg))
    (dotimes (_ (or arg 1))
      (re-search-backward "\\s-[!-]?\\<" nil 'move))))

(defvar gitattributes-mode-map (make-sparse-keymap)
  "Keymap for `gitattributes-mode'.")

(easy-menu-define gitattributes-mode-menu
  gitattributes-mode-map
  "Menu for `gitattributes-mode'."
  '("Git Attributes"
    ["Forward Field" forward-sexp :active t
     :help "Move forward across one field"]
    ["Backward Field" backward-sexp :active t
     :help "Move backward across one field"]
    ["Kill Field Forward" kill-sexp :active t
     :help "Kill field following cursor"]
    ["Kill Field Backward" backward-kill-sexp :active t
     :help "Kill field preceding cursor"]
    "--"
    ["Help" gitattributes-mode-help :active t
     :help "Open gitattributes(5) manpage"]))

;;;###autoload
(define-derived-mode gitattributes-mode text-mode "Gitattributes"
  "A major mode for editing .gitattributes files.
\\{gitattributes-mode-map}"
  :group 'gitattributes-mode
  :syntax-table gitattributes-mode-syntax-table
  (setq font-lock-defaults '(gitattributes-mode-font-lock-keywords))
  (setq-local eldoc-documentation-function #'gitattributes-mode-eldoc)
  (setq-local forward-sexp-function #'gitattributes-mode-forward-field)
  (when (and gitattributes-mode-enable-eldoc
             (require 'eldoc nil 'noerror))
    (eldoc-mode 1)))

;;;###autoload
(dolist (pattern '("/\\.gitattributes\\'"
                   "/\\.git/info/attributes\\'"
                   "/git/attributes\\'"))
  (add-to-list 'auto-mode-alist (cons pattern #'gitattributes-mode)))

(provide 'gitattributes-mode)

;;; gitattributes-mode.el ends here
