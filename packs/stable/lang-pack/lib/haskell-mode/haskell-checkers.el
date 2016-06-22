;;; haskell-checkers.el --- Emacs interface to haskell lint and style checkers -*- lexical-binding: t -*-

;; Copyright (C) 2009-2011  Alex Ott, Liam O'Reilly
;;
;; Author: Alex Ott <alexott@gmail.com>, Liam O'Reilly <csliam@swansea.ac.uk>
;; Keywords: haskell, lint, hlint, style scanner
;; Requirements: hlint, scan, haskell

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'compile)

;;;###autoload
(defgroup haskell-checkers nil
  "Run HLint as inferior of Emacs, parse error messages."
  :group 'haskell)

(defcustom haskell-lint-command "hlint"
  "The default lint command for \\[hlint]."
  :type 'string
  :group 'haskell-checkers)

(defcustom haskell-scan-command "scan"
  "The default scan command for \\[haskell-scan]."
  :type 'string
  :group 'haskell-checkers)

(defcustom haskell-scan-options ""
  "The default options for \\[haskell-scan]."
  :type 'string
  :group 'haskell-checkers)

(defcustom haskell-lint-options ""
  "The default options for \\[hlint]."
  :type 'string
  :group 'haskell-checkers)

(defcustom haskell-checkers-save-files t
  "Save modified files when run checker or not (ask user)"
  :type 'boolean
  :group 'haskell-checkers)

(defcustom haskell-checkers-replace-with-suggestions nil
  "Replace user's code with suggested replacements (hlint only)"
  :type 'boolean
  :group 'haskell-checkers)

(defcustom haskell-checkers-replace-without-ask nil
  "Replace user's code with suggested replacements automatically (hlint only)"
  :type 'boolean
  :group 'haskell-checkers)

;; regex for replace HLint's suggestions
;;
;; ^\(.*?\):\([0-9]+\):\([0-9]+\): .*
;; Found:
;; \s +\(.*\)
;; Why not:
;; \s +\(.*\)

(defvar haskell-lint-regex
  "^\\(.*?\\):\\([0-9]+\\):\\([0-9]+\\): .*[\n\C-m]Found:[\n\C-m]\\s +\\(.*\\)[\n\C-m]Why not:[\n\C-m]\\s +\\(.*\\)[\n\C-m]"
  "Regex for HLint messages")

(defun haskell-checkers-make-short-string (str maxlen)
  (if (< (length str) maxlen)
      str
    (concat (substring str 0 (- maxlen 3)) "...")))

;; TODO: check, is it possible to adopt it for haskell-scan?
(defun haskell-lint-replace-suggestions ()
  "Perform actual replacement of HLint's suggestions"
  (goto-char (point-min))
  (while (re-search-forward haskell-lint-regex nil t)
    (let* ((fname (match-string 1))
           (fline (string-to-number (match-string 2)))
           (old-code (match-string 4))
           (new-code (match-string 5))
           (msg (concat "Replace '" (haskell-checkers-make-short-string old-code 30)
                        "' with '" (haskell-checkers-make-short-string new-code 30) "'"))
           (bline 0)
           (eline 0)
           (spos 0)
           (new-old-code ""))
      (save-excursion
        (switch-to-buffer (get-file-buffer fname))
        (goto-char (point-min))
        (forward-line (1- fline))
        (beginning-of-line)
        (setq bline (point))
        (when (or haskell-checkers-replace-without-ask
                  (yes-or-no-p msg))
          (end-of-line)
          (setq eline (point))
          (beginning-of-line)
          (setq old-code (regexp-quote old-code))
          (while (string-match "\\\\ " old-code spos)
            (setq new-old-code (concat new-old-code
                                       (substring old-code spos (match-beginning 0))
                                       "\\ *"))
            (setq spos (match-end 0)))
          (setq new-old-code (concat new-old-code (substring old-code spos)))
          (remove-text-properties bline eline '(composition nil))
          (when (re-search-forward new-old-code eline t)
            (replace-match new-code nil t)))))))

(defun haskell-lint-finish-hook (_buf _msg)
  "Function, that is executed at the end of HLint or scan execution"
  (if haskell-checkers-replace-with-suggestions
      (haskell-lint-replace-suggestions)
    (next-error 1 t)))

(defun haskell-scan-finish-hook (_buf _msg)
  "Function, that is executed at the end of haskell-scan execution"
  (next-error 1 t))

(defun haskell-scan-make-command (file)
  "Generates command line for scan"
  (concat haskell-scan-command " " haskell-scan-options " \"" file "\""))

(defun haskell-lint-make-command (file)
  "Generates command line for lint"
  (concat haskell-lint-command  " \"" file "\"" " " haskell-lint-options))

(defmacro haskell-checkers-setup (type name)
  "Performs setup of corresponding checker. Receives two arguments:
type - checker's type (lint or scan) that is expanded into functions and hooks names
name - user visible name for this mode"
  (let ((nm (concat "haskell-" (symbol-name type))))
    `(progn
;;;###autoload
       (defvar ,(intern (concat nm "-setup-hook")) nil
         ,(concat "Hook, that will executed before running " name))
       (defun ,(intern (concat nm "-process-setup")) ()
         "Setup compilation variables and buffer for `hlint'."
         (run-hooks ',(intern (concat nm "-setup-hook"))))
;;;###autoload
       (define-compilation-mode ,(intern (concat nm "-mode")) ,name
         ,(concat "Mode to check Haskell source code using " name)
         (set (make-local-variable 'compilation-process-setup-function)
              ',(intern (concat nm "-process-setup")))
         (set (make-local-variable 'compilation-disable-input) t)
         (set (make-local-variable 'compilation-scroll-output) nil)
         (set (make-local-variable 'compilation-finish-functions)
              (list ',(intern (concat nm "-finish-hook")))))
;;;###autoload
       (defun ,(intern nm) ()
         ,(concat "Run " name " for current buffer with haskell source")
         (interactive)
         (save-some-buffers haskell-checkers-save-files)
         (compilation-start (,(intern (concat nm "-make-command")) buffer-file-name)
                            ',(intern (concat nm "-mode")))))
    ))

(haskell-checkers-setup lint "HLint")
(haskell-checkers-setup scan "HScan")

(provide 'haskell-checkers)

;;; haskell-checkers.el ends here
