;;; ioke-mode.el --- Major mode for the ioke language

;; Copyright (C) 2008-2009  Ola Bini

;; Author: Ola Bini <ola.bini@gmail.com>
;; Keywords: 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
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

;; 

;;; Code:

(defconst ioke-version "0"
  "ioke mode version number")

(defconst ioke-interpreter-executable "ioke"
  "ioke executable")

(defconst ioke-indent-offset 2
  "ioke mode indent offset")

(defconst ioke-electric-parens-p t
  "Should the ioke mode autoindent after parentheses are typed?")

(defconst ioke-clever-indent-p t
  "Should the ioke mode try to dedent and reindent depending on context?")

(defconst ioke-auto-mode-p t
  "Should the ioke mode add itself to the auto mode list?")

(defgroup ioke-font-lock-faces nil
  "Specific Ioke faces for highlighting Ioke sources."
  :prefix "ioke-font-lock-"
  :group (if (featurep 'xemacs)
             'font-lock-faces
           'font-lock-highlighting-faces))


(defface ioke-font-lock-object-mimic-face
  '((((class grayscale)) (:foreground "grey"))
    (((class color)) (:foreground "medium blue"))
    (t (:bold t)))
  "Font Lock Mode face used to highlight mimicking of something."
  :group 'ioke-font-lock-faces)

(defconst ioke-font-lock-object-mimic-face 'ioke-font-lock-object-mimic-face)

(defface ioke-font-lock-operator-name-face
  '((((type tty) (class color)) (:foreground "LightSteelBlue" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "Orchid"))
    (((class color) (background dark)) (:foreground "CornflowerBlue"))
    (t (:bold t)))
  "Font Lock mode face used to highlight operator names."
  :group 'ioke-font-lock-faces)

(defconst ioke-font-lock-operator-name-face 'ioke-font-lock-operator-name-face)

(defface ioke-font-lock-operator-symbol-face
  '((((type tty) (class color)) (:foreground "LightSteelBlue" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "Orchid"))
    (((class color) (background dark)) (:foreground "deep sky blue"))
    (t (:bold t)))
  "Font Lock mode face used to highlight operator symbols."
  :group 'ioke-font-lock-faces)

(defconst ioke-font-lock-operator-symbol-face 'ioke-font-lock-operator-symbol-face)

(defface ioke-font-lock-number-face
  '((((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
    (((class color) (background light)) (:foreground "RosyBrown"))
    (((class color) (background dark)) (:foreground "LightSalmon"))
    (t (:italic t)))
  "Font Lock mode face used to highlight numbers."
  :group 'ioke-font-lock-faces)

(defconst ioke-font-lock-number-face 'ioke-font-lock-number-face)

(defface ioke-font-lock-known-kind-face
  '((((type tty) (class color)) (:foreground "magenta"))
    (((class grayscale) (background light))
     (:foreground "LightGray" :bold t :underline t))
    (((class grayscale) (background dark))
     (:foreground "Gray50" :bold t :underline t))
    (((class color) (background light)) (:foreground "CadetBlue"))
    (((class color) (background dark)) (:foreground "Aquamarine"))
    (t (:bold t :underline t)))
  "Font Lock mode face used to highlight known kinds."
  :group 'ioke-font-lock-faces)

(defconst ioke-font-lock-known-kind-face 'ioke-font-lock-known-kind-face)

(defface ioke-font-lock-api-cell-face
  '((((class grayscale) (background light)) (:foreground "DimGray"))
    (((class grayscale) (background dark)) (:foreground "LightGray"))
    (((class color) (background light)) (:foreground "dark goldenrod"))
    (((class color) (background dark)) (:foreground "light goldenrod")))
  "Font Lock mode face used to highlight API cells."
  :group 'ioke-font-lock-faces)

(defconst ioke-font-lock-api-cell-face 'ioke-font-lock-api-cell-face)
(defconst ioke-font-lock-special-face font-lock-keyword-face)
(defconst ioke-font-lock-kind-face font-lock-type-face)

(defface ioke-font-lock-object-assign-face
  '((((type tty) (class color)) (:foreground "blue" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "Orchid"))
    (((class color) (background dark)) (:foreground "LightSteelBlue"))
    (t (:bold t)))
  "Font Lock Mode face used to highlight assignment."
  :group 'ioke-font-lock-faces)

(defconst ioke-font-lock-object-assign-face 'ioke-font-lock-object-assign-face)
(defconst ioke-font-lock-braces-face font-lock-preprocessor-face)
(defconst ioke-font-lock-symbol-face font-lock-reference-face)
(defconst ioke-font-lock-keyword-argument-face font-lock-reference-face)

(defface ioke-font-lock-regexp-face
  '((((type tty) (class color)) (:foreground "DeepPink" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "Orchid"))
    (((class color) (background dark)) (:foreground "DeepPink"))
    (t (:bold t)))
  "Font Lock mode face used to highlight regexps."
  :group 'ioke-font-lock-faces)

(defconst ioke-font-lock-regexp-face 'ioke-font-lock-regexp-face)
(defconst ioke-font-lock-string-face 'font-lock-string-face)

(defface ioke-font-lock-expression-face
  '((((class grayscale) (background light)) (:foreground "DimGray"))
    (((class grayscale) (background dark)) (:foreground "LightGray"))
    (((class color) (background light)) (:foreground "dark goldenrod"))
    (((class color) (background dark)) (:foreground "light goldenrod")))
  "Font Lock mode face used to highlight expressions inside of texts."
  :group 'ioke-font-lock-faces)

(defconst ioke-font-lock-expression-face 'ioke-font-lock-expression-face)

(defconst ioke-prototype-names '(
                                 "Base"
                                 "Call"
                                 "Condition"
                                 "DateTime"
                                 "DefaultBehavior"
                                 "DefaultMacro"
                                 "DefaultMethod"
                                 "DefaultSyntax"
                                 "Dict"
                                 "FileSystem"
                                 "Ground"
                                 "IokeGround"
                                 "JavaGround"
                                 "Handler"
                                 "IO"
                                 "JavaMethod"
                                 "LexicalBlock"
                                 "LexicalMacro"
                                 "List"
                                 "Message"
                                 "Method"
                                 "Mixins"
                                 "Number"
                                 "Number Decimal"
                                 "Number Integer"
                                 "Number Ratio"
                                 "Number Rational"
                                 "Number Real"
                                 "Origin"
                                 "Pair"
                                 "Range"
                                 "Regexp"
                                 "Rescue"
                                 "Restart"
                                 "Runtime"
                                 "Set"
                                 "Symbol"
                                 "System"
                                 "Text"
                                 )
  "ioke mode prototype names")

(defconst ioke-cell-names '(
                            "print"
                            "println"

                            "cell"
                            "cell?"
                            "documentation"

                            "if"
                            "unless"
                            "while"
                            "until"
                            "loop"
                            "for"
                            "for:set"
                            "for:dict"

                            "bind"
                            "rescue"
                            "handle"
                            "restart"

                            "asText"
                            "inspect"
                            "notice"

                            "do"

                            "call"

                            "list"
                            "dict"
                            "set"

                            "with"
                            "kind"
                            )
  "ioke mode cell names")

(defconst ioke-operator-symbols '(
                "..."
				".."
                "=>"
                "=>>"
                                  
				"++"
				"--"

				"**"
				"*"
				"/"
				"%"

				"+"
				"-"

				"<<"
				">>"

				">"
				"<"
				"<="
				">="
				"<=>"

				"==="
				"=="
				"!="

				"=~"
				"!~"

				"&"

				"^"

                "|"

;                "or"
;                "nor"
;                "xor"
;                "and"
;                "nand"
				"&&"
				"?&"

				"||"
				"?|"

				"<-"
				"<->"
				"->"

				"~"
				"$"
                "+>"
                "!>"
                "<>"
                "&>"
                "%>"
                "#>"
                "@>"
                "/>"
                "*>"
                "?>"
                "|>"
                "^>"
                "~>"
                "**>"
                "&&>"
                "||>"
                "$>"
                "->>"
                "+>>"
                "!>>"
                "<>>"
                "&>>"
                "%>>"
                "#>>"
                "@>>"
                "/>>"
                "*>>"
                "?>>"
                "|>>"
                "^>>"
                "~>>"
                "**>>"
                "&&>>"
                "||>>"
                "$>>"

				"="
				"+="
				"-="
				"*="
				"/="
				"%="
				"&="
				"^="
				"|="
				"<<="
				">>="
				)
  "ioke mode operator symbols")

(defconst ioke-operator-names '(
                                "return"
                                "break"
                                "continue"

                                "mimic"
                                "self"
                                "use"

                                "fn"
                                "fnx"
                                "method"
                                "macro"
                                "lecro"
                                "lecrox"
                                "syntax"
                                "dmacro"
                                "dlecro"
                                "dlecrox"
                                "dsyntax"

                                "true"
                                "false"
                                "nil"
                                )
  "ioke mode operator names")

(defconst ioke-special-names '(
                               "``"
                               "`"
                               "''"
                               "'"
                               "."
                               ","
                               "@"
                               "@@"
                               )
  "ioke mode special names")

(defconst ioke-standout-names '(
                               "it"
                               )
  "ioke mode names that should stand out")

(defconst ioke-custom-names '(
			    ; your custom identifiers here
			    )
  "ioke mode custom names")

(defconst ioke-region-comment-prefix ";"
  "ioke region comment prefix")

(defvar ioke-mode-hook nil
  "ioke mode hook")

(defvar ioke-keymap 
  (let ((ioke-keymap (make-sparse-keymap)))
    (if ioke-electric-parens-p
        (progn
          (define-key ioke-keymap "\C-c\C-t" 'ioke-eval-buffer)
          (define-key ioke-keymap "(" 'ioke-electric-open-paren)
          (define-key ioke-keymap ")" 'ioke-electric-close-paren)
          (define-key ioke-keymap "[" 'ioke-electric-open-s-paren)
          (define-key ioke-keymap "]" 'ioke-electric-close-s-paren)
          (define-key ioke-keymap "{" 'ioke-electric-open-c-paren)
          (define-key ioke-keymap "}" 'ioke-electric-close-c-paren)
          (define-key ioke-keymap (kbd "C-/") 'comment-or-uncomment-region)
          ))
    ioke-keymap)
  "ioke mode keymap")

(defconst ioke-font-lock-keywords
  (list
    '("\\([[:alnum:]!?_:-]+\\)[[:space:]]*=[^=][[:space:]]*[[:alnum:]_:-]+[[:space:]]+mimic" 1 ioke-font-lock-object-mimic-face)
    '("\\([[:alnum:]!?_:-]+\\)[[:space:]]*[+*/-]?=[^=]" 1 ioke-font-lock-object-assign-face)
    '("#/.*?/[oxpniums]*" 0 ioke-font-lock-regexp-face)
    '("#r\\[.*?\\][oxpniums]*" 0 ioke-font-lock-regexp-face)
    '("#\\[" 0 ioke-font-lock-string-face t)
    '("\\([^\\\\]\\|\\\\\\\\\\)\\(#{[^}]*}\\)" 2 ioke-font-lock-expression-face t)
    `(,(regexp-opt ioke-prototype-names 'words) . ioke-font-lock-known-kind-face)
    `(,(regexp-opt ioke-standout-names 'words) . font-lock-warning-face)
    '("\\<[A-Z][[:alnum:]!?_:-]*\\>" 0 ioke-font-lock-kind-face)
    '("\\<[[:alnum:]!?_:-]*?:\\>" 0 ioke-font-lock-keyword-argument-face)
    '("\\<:[[:alnum:]!?_:-]*\\>" 0 ioke-font-lock-symbol-face)
    '("\\<:\"[[:alnum:]!?_:-]*\"" 0 ioke-font-lock-symbol-face t)
    '("\\<[[:digit:]_\\.eE]+\\>" 0 ioke-font-lock-number-face)
    `(,(regexp-opt ioke-operator-names 'words) . ioke-font-lock-operator-name-face)
    `(,(regexp-opt ioke-operator-symbols t) . ioke-font-lock-operator-symbol-face)
    `(,(regexp-opt ioke-special-names t) . ioke-font-lock-special-face)
    `(,(regexp-opt ioke-cell-names 'words) . ioke-font-lock-api-cell-face)
    '("[](){}[]" 0 ioke-font-lock-braces-face)
   )
  "ioke mode font lock keywords")

(defvar ioke-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)
    (modify-syntax-entry ?\; "<" st)
    (modify-syntax-entry ?? "w" st)
    (modify-syntax-entry ?! "w" st)
    (modify-syntax-entry ?: "w" st)
    (modify-syntax-entry ?, "." st)
    (modify-syntax-entry ?. "." st)
    (modify-syntax-entry ?\' "'" st)
    (modify-syntax-entry ?\` "'" st)
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "ioke mode syntax table")

(defun ioke-eval-buffer () (interactive)
       "Evaluate the buffer with ioke."
       (shell-command-on-region (point-min) (point-max) "ioke -"))

(defun ioke-indent-line ()
  "ioke mode indent line"
  (interactive)
  (if (bobp)
      (indent-line-to 0)
      (let (current-depth current-close-flag current-close-open-flag
                          (last-indent 0) last-depth last-close-flag last-close-open-flag first-line)
        (save-excursion
          (let (start-point end-point)
                                        ; get the balance of parentheses on the current line
            (end-of-line)
            (setq end-point (point))
            (beginning-of-line)
            (setq first-line (bobp))
            (setq current-close-flag (looking-at "^[ \\t)]*[])}][ \\t)]*$"))
            (setq current-close-open-flag (looking-at "^\\s-*).*(\\s-*$"))
            (setq start-point (point))
            (setq current-depth (car (parse-partial-sexp start-point end-point)))
                                        ; and the previous non-blank line
            (if (not first-line)
                (progn
                  (while (progn 
                           (forward-line -1)
                           (beginning-of-line)
                           (and (not (bobp))
                                (looking-at "^\\s-*$"))))
                  (setq last-indent (current-indentation))
                  (end-of-line)
                  (setq end-point (point))
                  (beginning-of-line)
                  (setq last-close-flag (looking-at "^[ \\t)]*[])}][ \\t)]*$"))
                  (setq last-close-open-flag (looking-at "^\\s-*).*(\\s-*$"))
                  (setq start-point (point))
                  (setq last-depth (car (parse-partial-sexp start-point end-point))))
                (setq last-depth 0))))

        (let ((depth last-depth))
          (if ioke-clever-indent-p
              (setq depth (+ depth
                             (if current-close-flag current-depth 0)
                             (if last-close-flag (- last-depth) 0)
                             (if current-close-open-flag -1 0)
                             (if last-close-open-flag 1 0))))
          (indent-line-to (max 0
                          (+ last-indent
                             (* depth ioke-indent-offset))))))))







(defun ioke-electric-open-paren ()
  "ioke mode electric close parenthesis"
  (interactive)
  (insert ?\()
  (let ((marker (make-marker)))
    (set-marker marker (point-marker))
    (indent-according-to-mode)
    (goto-char (marker-position marker))
    (set-marker marker nil)))

(defun ioke-electric-close-paren ()
  "ioke mode electric close parenthesis"
  (interactive)
  (insert ?\))
  (let ((marker (make-marker)))
    (set-marker marker (point-marker))
    (indent-according-to-mode)
    (goto-char (marker-position marker))
    (set-marker marker nil))
  (blink-matching-open))

(defun ioke-electric-open-c-paren ()
  "ioke mode electric close parenthesis"
  (interactive)
  (insert ?\{)
  (let ((marker (make-marker)))
    (set-marker marker (point-marker))
    (indent-according-to-mode)
    (goto-char (marker-position marker))
    (set-marker marker nil)))

(defun ioke-electric-close-c-paren ()
  "ioke mode electric close parenthesis"
  (interactive)
  (insert ?\})
  (let ((marker (make-marker)))
    (set-marker marker (point-marker))
    (indent-according-to-mode)
    (goto-char (marker-position marker))
    (set-marker marker nil))
  (blink-matching-open))

(defun ioke-electric-open-s-paren ()
  "ioke mode electric close parenthesis"
  (interactive)
  (insert ?\[)
  (let ((marker (make-marker)))
    (set-marker marker (point-marker))
    (indent-according-to-mode)
    (goto-char (marker-position marker))
    (set-marker marker nil)))

(defun ioke-electric-close-s-paren ()
  "ioke mode electric close parenthesis"
  (interactive)
  (insert ?\])
  (let ((marker (make-marker)))
    (set-marker marker (point-marker))
    (indent-according-to-mode)
    (goto-char (marker-position marker))
    (set-marker marker nil))
  (blink-matching-open))

(defun ioke-comment-region (beg end &optional arg)
  "Comment region for Io."
  (interactive "r\nP")
  (let ((comment-start ioke-region-comment-prefix))
    (comment-region beg end arg)))

(defconst ioke-font-lock-syntactic-keywords
      '(
        ("#\\(\\[\\)\\([^]\\\\]\\|\\\\\\]\\|\\\\[]\\\\u01234567ntfreb#\"\n]\\)*\\(\\]\\)" (1 "|" t) (3 "|" t))
        ))

(defun ioke-mode ()
  "ioke mode"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table ioke-syntax-table)
  (use-local-map ioke-keymap)

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(ioke-font-lock-keywords nil nil))

  (make-local-variable 'font-lock-syntax-table)
  (setq font-lock-syntax-table ioke-syntax-table)

  (make-local-variable 'font-lock-syntactic-keywords)
  (setq font-lock-syntactic-keywords ioke-font-lock-syntactic-keywords)

  (set (make-local-variable 'indent-line-function) 'ioke-indent-line)
  (set (make-local-variable 'comment-start) "; ")
  (setq major-mode 'ioke-mode)
  (setq mode-name "ioke mode")
  (run-hooks 'ioke-mode-hook)
  (if ioke-auto-mode-p
      (add-to-list 'auto-mode-alist '("\\.ik$" . ioke-mode))))

(provide 'ioke-mode)
;;; ioke-mode.el ends here
