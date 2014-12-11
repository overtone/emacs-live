;;; clojure-mode.el --- Major mode for Clojure code -*- lexical-binding: t; -*-

;; Copyright © 2007-2014 Jeffrey Chu, Lennart Staflin, Phil Hagelberg
;; Copyright © 2013-2014 Bozhidar Batsov
;;
;; Authors: Jeffrey Chu <jochu0@gmail.com>
;;       Lennart Staflin <lenst@lysator.liu.se>
;;       Phil Hagelberg <technomancy@gmail.com>
;;       Bozhidar Batsov <bozhidar@batsov.com>
;; URL: http://github.com/clojure-emacs/clojure-mode
;; Keywords: languages clojure clojurescript lisp
;; Version: 3.0.1
;; Package-Requires: ((emacs "24.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides font-lock, indentation, and navigation for the Clojure
;; programming language (http://clojure.org).

;; Using clojure-mode with paredit or smartparens is highly recommended.

;; Here are some example configurations:

;;   ;; require or autoload paredit-mode
;;   (add-hook 'clojure-mode-hook 'paredit-mode)

;;   ;; require or autoload smartparens
;;   (add-hook 'clojure-mode-hook 'smartparens-strict-mode)

;; See CIDER (http://github.com/clojure-emacs/cider) for
;; better interaction with subprocesses via nREPL.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:


;;; Compatibility
(eval-and-compile
  ;; `setq-local' for Emacs 24.2 and below
  (unless (fboundp 'setq-local)
    (defmacro setq-local (var val)
      "Set variable VAR to value VAL in current buffer."
      `(set (make-local-variable ',var) ,val))))

(eval-when-compile
  (defvar calculate-lisp-indent-last-sexp)
  (defvar font-lock-beg)
  (defvar font-lock-end)
  (defvar paredit-space-for-delimiter-predicates)
  (defvar paredit-version)
  (defvar paredit-mode))

(require 'cl)
(require 'inf-lisp)
(require 'imenu)

(declare-function lisp-fill-paragraph  "lisp-mode" (&optional justify))

(defgroup clojure nil
  "Major mode for editing Clojure code."
  :prefix "clojure-"
  :group 'languages
  :link '(url-link :tag "Github" "https://github.com/clojure-emacs/clojure-mode")
  :link '(emacs-commentary-link :tag "Commentary" "clojure-mode"))

(defface clojure-keyword-face
  '((t (:inherit font-lock-constant-face)))
  "Face used to font-lock Clojure keywords (:something)."
  :group 'clojure
  :package-version '(clojure-mode . "3.0.0"))

(defface clojure-character-face
  '((t (:inherit font-lock-string-face)))
  "Face used to font-lock Clojure character literals."
  :group 'clojure
  :package-version '(clojure-mode . "3.0.0"))

(defface clojure-interop-method-face
  '((t (:inherit font-lock-preprocessor-face)))
  "Face used to font-lock interop method names (camelCase)."
  :group 'clojure
  :package-version '(clojure-mode . "3.0.0"))

(defcustom clojure-load-command  "(clojure.core/load-file \"%s\")\n"
  "Format-string for building a Clojure expression to load a file.
This format string should use `%s' to substitute a file name and
should result in a Clojure expression that will command the
inferior Clojure to load that file."
  :type 'string
  :group 'clojure
  :safe 'stringp)

(defcustom clojure-inf-lisp-command "lein repl"
  "The command used by `inferior-lisp-program'."
  :type 'string
  :group 'clojure
  :safe 'stringp)

(defcustom clojure-defun-style-default-indent nil
  "When non-nil, use default indenting for functions and macros.
Otherwise check `define-clojure-indent' and `put-clojure-indent'."
  :type 'boolean
  :group 'clojure
  :safe 'booleanp)

(defcustom clojure-use-backtracking-indent t
  "When non-nil, enable context sensitive indentation."
  :type 'boolean
  :group 'clojure
  :safe 'booleanp)

(defcustom clojure-max-backtracking 3
  "Maximum amount to backtrack up a list to check for context."
  :type 'integer
  :group 'clojure
  :safe 'integerp)

(defcustom clojure-docstring-fill-column fill-column
  "Value of `fill-column' to use when filling a docstring."
  :type 'integer
  :group 'clojure
  :safe 'integerp)

(defcustom clojure-docstring-fill-prefix-width 2
  "Width of `fill-prefix' when filling a docstring.
The default value conforms with the de facto convention for
Clojure docstrings, aligning the second line with the opening
double quotes on the third column."
  :type 'integer
  :group 'clojure
  :safe 'integerp)

(defcustom clojure-omit-space-between-tag-and-delimiters '(?\[ ?\{)
  "Allowed opening delimiter characters after a reader literal tag.
For example, \[ is allowed in :db/id[:db.part/user]."
  :type '(set (const :tag "[" ?\[)
              (const :tag "{" ?\{)
              (const :tag "(" ?\()
              (const :tag "\"" ?\"))
  :group 'clojure
  :safe (lambda (value)
          (and (listp value)
               (every 'characterp value))))

(defvar clojure-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map lisp-mode-shared-map)
    (define-key map (kbd "C-M-x")   'lisp-eval-defun)
    (define-key map (kbd "C-x C-e") 'lisp-eval-last-sexp)
    (define-key map (kbd "C-c C-e") 'lisp-eval-last-sexp)
    (define-key map (kbd "C-c C-l") 'clojure-load-file)
    (define-key map (kbd "C-c C-r") 'lisp-eval-region)
    (define-key map (kbd "C-c C-z") 'clojure-display-inferior-lisp-buffer)
    (define-key map (kbd "C-:") 'clojure-toggle-keyword-string)
    (easy-menu-define clojure-mode-menu map "Clojure Mode Menu"
      '("Clojure"
        ["Eval Top-Level Expression" lisp-eval-defun]
        ["Eval Last Expression" lisp-eval-last-sexp]
        ["Eval Region" lisp-eval-region]
        "--"
        ["Run Inferior Lisp" clojure-display-inferior-lisp-buffer]
        ["Display Inferior Lisp Buffer" clojure-display-inferior-lisp-buffer]
        ["Load File" clojure-load-file]
        "--"
        ["Toggle between string & keyword" clojure-toggle-keyword-string]
        ["Mark string" clojure-mark-string]
        ["Insert ns form at point" clojure-insert-ns-form-at-point]
        ["Insert ns form at beginning" clojure-insert-ns-form]
        ["Update ns form" clojure-update-ns]
        "--"
        ["Version" clojure-mode-display-version]))
    map)
  "Keymap for Clojure mode.  Inherits from `lisp-mode-shared-map'.")

(defvar clojure-mode-syntax-table
  (let ((table (copy-syntax-table emacs-lisp-mode-syntax-table)))
    (modify-syntax-entry ?~ "'   " table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?^ "'" table)
    (modify-syntax-entry ?@ "'" table)
    ;; Make hash a usual word character
    (modify-syntax-entry ?# "_ p" table)
    table))

(defvar clojure-prev-l/c-dir/file nil
  "Record last directory and file used in loading or compiling.
This holds a cons cell of the form `(DIRECTORY . FILE)'
describing the last `clojure-load-file' or `clojure-compile-file' command.")

(defconst clojure-mode-version "3.1.0-snapshot"
  "The current version of `clojure-mode'.")

(defconst clojure--prettify-symbols-alist
  '(("fn"  . ?λ)))

(defun clojure-mode-display-version ()
  "Display the current `clojure-mode-version' in the minibuffer."
  (interactive)
  (message "clojure-mode (version %s)" clojure-mode-version))

(defun clojure-space-for-delimiter-p (endp delim)
  "Prevent paredit from inserting useless spaces.
See `paredit-space-for-delimiter-predicates' for the meaning of
ENDP and DELIM."
  (if (or (derived-mode-p 'clojure-mode)
          (derived-mode-p 'cider-repl-mode))
      (save-excursion
        (backward-char)
        (if (and (or (char-equal delim ?\()
                     (char-equal delim ?\")
                     (char-equal delim ?{))
                 (not endp))
            (if (char-equal (char-after) ?#)
                (and (not (bobp))
                     (or (char-equal ?w (char-syntax (char-before)))
                         (char-equal ?_ (char-syntax (char-before)))))
              t)
          t))
    t))

(defun clojure-no-space-after-tag (endp delimiter)
  "Prevent inserting a space after a reader-literal tag?

When a reader-literal tag is followed be an opening delimiter
listed in `clojure-omit-space-between-tag-and-delimiters', this
function returns t.

This allows you to write things like #db/id[:db.part/user]
without inserting a space between the tag and the opening
bracket.

See `paredit-space-for-delimiter-predicates' for the meaning of
ENDP and DELIMITER."
  (if endp
      t
    (or (not (member delimiter clojure-omit-space-between-tag-and-delimiters))
        (save-excursion
          (let ((orig-point (point)))
            (not (and (re-search-backward
                       "#\\([a-zA-Z0-9._-]+/\\)?[a-zA-Z0-9._-]+"
                       (line-beginning-position)
                       t)
                      (= orig-point (match-end 0)))))))))

(defun clojure-paredit-setup ()
  "Make \"paredit-mode\" play nice with `clojure-mode'."
  (when (>= paredit-version 21)
    (define-key clojure-mode-map "{" 'paredit-open-curly)
    (define-key clojure-mode-map "}" 'paredit-close-curly)
    (add-to-list 'paredit-space-for-delimiter-predicates
                 'clojure-space-for-delimiter-p)
    (add-to-list 'paredit-space-for-delimiter-predicates
                 'clojure-no-space-after-tag)))

;;;###autoload
(define-derived-mode clojure-mode prog-mode "Clojure"
  "Major mode for editing Clojure code.

\\{clojure-mode-map}"
  (setq-local imenu-create-index-function
              (lambda ()
                (imenu--generic-function '((nil clojure-match-next-def 0)))))
  (setq-local indent-tabs-mode nil)
  (lisp-mode-variables nil)
  (setq fill-paragraph-function 'clojure-fill-paragraph)
  (setq adaptive-fill-function 'clojure-adaptive-fill-function)
  (setq-local normal-auto-fill-function 'clojure-auto-fill-function)
  (setq-local comment-start-skip
              "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)\\(;+\\|#|\\) *")
  (setq-local indent-line-function 'clojure-indent-line)
  (setq-local lisp-indent-function 'clojure-indent-function)
  (setq-local lisp-doc-string-elt-property 'clojure-doc-string-elt)
  (setq-local inferior-lisp-program clojure-inf-lisp-command)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local prettify-symbols-alist clojure--prettify-symbols-alist)
  (clojure-font-lock-setup)
  (setq-local open-paren-in-column-0-is-defun-start nil)
  (add-hook 'paredit-mode-hook 'clojure-paredit-setup))

(defsubst clojure-in-docstring-p ()
  "Check whether point is in a docstring."
  (eq (get-text-property (1- (point-at-eol)) 'face)
      'font-lock-doc-face))

(defsubst clojure-docstring-fill-prefix ()
  "The prefix string used by `clojure-fill-paragraph'.

It is simply `clojure-docstring-fill-prefix-width' number of spaces."
  (make-string clojure-docstring-fill-prefix-width ? ))

(defun clojure-adaptive-fill-function ()
  "Clojure adaptive fill function.
This only takes care of filling docstring correctly."
  (when (clojure-in-docstring-p)
    (clojure-docstring-fill-prefix)))

(defun clojure-fill-paragraph (&optional justify)
  "Like `fill-paragraph', but can handle Clojure docstrings.

If JUSTIFY is non-nil, justify as well as fill the paragraph."
  (if (clojure-in-docstring-p)
      (let ((paragraph-start
             (concat paragraph-start
                     "\\|\\s-*\\([(;:\"[]\\|~@\\|`(\\|#'(\\)"))
            (paragraph-separate
             (concat paragraph-separate "\\|\\s-*\".*[,\\.]$"))
            (fill-column (or clojure-docstring-fill-column fill-column))
            (fill-prefix (clojure-docstring-fill-prefix)))
        (fill-paragraph justify))
    (let ((paragraph-start (concat paragraph-start
                                   "\\|\\s-*\\([(;:\"[]\\|`(\\|#'(\\)"))
          (paragraph-separate
           (concat paragraph-separate "\\|\\s-*\".*[,\\.[]$")))
      (or (fill-comment-paragraph justify)
          (fill-paragraph justify))
      ;; Always return `t'
      t)))

(defun clojure-auto-fill-function ()
  "Clojure auto-fill function."
  ;; Check if auto-filling is meaningful.
  (let ((fc (current-fill-column)))
    (when (and fc (> (current-column) fc))
      (let ((fill-column (if (clojure-in-docstring-p)
                             clojure-docstring-fill-column
                           fill-column))
            (fill-prefix (clojure-adaptive-fill-function)))
        (do-auto-fill)))))

(defun clojure-display-inferior-lisp-buffer ()
  "Display a buffer bound to `inferior-lisp-buffer'."
  (interactive)
  (if (and inferior-lisp-buffer (get-buffer inferior-lisp-buffer))
      (pop-to-buffer inferior-lisp-buffer t)
    (run-lisp inferior-lisp-program)))

(defun clojure-load-file (file-name)
  "Load a Clojure file FILE-NAME into the inferior Clojure process."
  (interactive (comint-get-source "Load Clojure file: "
                                  clojure-prev-l/c-dir/file
                                  '(clojure-mode) t))
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq clojure-prev-l/c-dir/file (cons (file-name-directory file-name)
                                        (file-name-nondirectory file-name)))
  (comint-send-string (inferior-lisp-proc)
                      (format clojure-load-command file-name))
  (switch-to-lisp t))



(defun clojure-match-next-def ()
  "Scans the buffer backwards for the next \"top-level\" definition.
Called by `imenu--generic-function'."
  (when (re-search-backward "^(def\\sw*" nil t)
    (save-excursion
      (let (found?
            (start (point)))
        (down-list)
        (forward-sexp)
        (while (not found?)
          (forward-sexp)
          (or (if (char-equal ?[ (char-after (point)))
                              (backward-sexp))
                  (if (char-equal ?) (char-after (point)))
                (backward-sexp)))
          (destructuring-bind (def-beg . def-end) (bounds-of-thing-at-point 'sexp)
            (if (char-equal ?^ (char-after def-beg))
                (progn (forward-sexp) (backward-sexp))
              (setq found? t)
              (set-match-data (list def-beg def-end)))))
        (goto-char start)))))

(defconst clojure-font-lock-keywords
  (eval-when-compile
    `(;; Top-level variable definition
      (,(concat "(\\(?:clojure.core/\\)?\\("
                (regexp-opt '("def" "defonce"))
                ;; variable declarations
                "\\)\\>"
                ;; Any whitespace
                "[ \r\n\t]*"
                ;; Possibly type or metadata
                "\\(?:#?^\\(?:{[^}]*}\\|\\sw+\\)[ \r\n\t]*\\)*"
                "\\(\\sw+\\)?")
       (1 font-lock-keyword-face)
       (2 font-lock-variable-name-face nil t))
      ;; Type definition
      (,(concat "(\\(?:clojure.core/\\)?\\("
                (regexp-opt '("defstruct" "deftype" "defprotocol"
                              "defrecord"))
                ;; type declarations
                "\\)\\>"
                ;; Any whitespace
                "[ \r\n\t]*"
                ;; Possibly type or metadata
                "\\(?:#?^\\(?:{[^}]*}\\|\\sw+\\)[ \r\n\t]*\\)*"
                "\\(\\sw+\\)?")
       (1 font-lock-keyword-face)
       (2 font-lock-type-face nil t))
      ;; Function definition (anything that starts with def and is not
      ;; listed above)
      (,(concat "(\\(?:[a-z\.-]+/\\)?\\(def\[a-z\-\]*-?\\)"
                ;; Function declarations
                "\\>"
                ;; Any whitespace
                "[ \r\n\t]*"
                ;; Possibly type or metadata
                "\\(?:#?^\\(?:{[^}]*}\\|\\sw+\\)[ \r\n\t]*\\)*"
                "\\(\\sw+\\)?")
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face nil t))
      ;; (fn name? args ...)
      (,(concat "(\\(?:clojure.core/\\)?\\(fn\\)[ \t]+"
                ;; Possibly type
                "\\(?:#?^\\sw+[ \t]*\\)?"
                ;; Possibly name
                "\\(t\\sw+\\)?" )
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face nil t))
      ;; lambda arguments - %, %1, %2, etc
      ("\\<%[1-9]?" (0 font-lock-variable-name-face))
      ;; Special forms
      (,(concat
         "("
         (regexp-opt
          '("def" "do" "if" "let" "var" "fn" "loop"
            "recur" "throw" "try" "catch" "finally"
            "set!" "new" "."
            "monitor-enter" "monitor-exit" "quote") t)
         "\\>")
       1 font-lock-keyword-face)
      ;; Built-in binding and flow of control forms
      (,(concat
         "(\\(?:clojure.core/\\)?"
         (regexp-opt
          '("letfn" "case" "cond" "cond->" "cond->>" "condp"
            "for" "when" "when-not" "when-let" "when-first" "when-some"
            "if-let" "if-not" "if-some"
            ".." "->" "->>" "doto" "and" "or"
            "dosync" "doseq" "dotimes" "dorun" "doall"
            "load" "import" "unimport" "ns" "in-ns" "refer"
            "with-open" "with-local-vars" "binding"
            "gen-class" "gen-and-load-class" "gen-and-save-class"
            "handler-case" "handle" "declare") t)
         "\\>")
       1 font-lock-keyword-face)
      (,(concat
         "\\<"
         (regexp-opt
          '("*1" "*2" "*3" "*agent*"
            "*allow-unresolved-vars*" "*assert*" "*clojure-version*"
            "*command-line-args*" "*compile-files*"
            "*compile-path*" "*e" "*err*" "*file*" "*flush-on-newline*"
            "*in*" "*macro-meta*" "*math-context*" "*ns*" "*out*"
            "*print-dup*" "*print-length*" "*print-level*"
            "*print-meta*" "*print-readably*"
            "*read-eval*" "*source-path*"
            "*use-context-classloader*" "*warn-on-reflection*")
          t)
         "\\>")
       0 font-lock-builtin-face)
      ;; Dynamic variables - *something* or @*something*
      ("\\<@?\\(\\*[a-z-]*\\*\\)\\>" 1 font-lock-variable-name-face)
      ;; Global constants - nil, true, false
      (,(concat
         "\\<"
         (regexp-opt
          '("true" "false" "nil") t)
         "\\>")
       0 font-lock-constant-face)
      ;; Character literals - \1, \a, \newline, \u0000
      ;; FIXME: handle properly some punctuation characters (like commas and semicolumns)
      ("\\\\\\([[:punct:]]\\|[a-z0-9]+\\)\\>" 0 'clojure-character-face)
      ;; Constant values (keywords), including as metadata e.g. ^:static
      ("\\<^?\\(:\\(\\sw\\|\\s_\\)+\\(\\>\\|\\_>\\)\\)" 1 'clojure-keyword-face)
      ;; cljx annotations (#+clj and #+cljs)
      ("#\\+cljs?\\>" 0 font-lock-preprocessor-face)
      ;; Java interop highlighting
      ;; CONST SOME_CONST (optionally prefixed by /)
      ("\\(?:\\<\\|/\\)\\([A-Z]+\\|\\([A-Z]+_[A-Z1-9_]+\\)\\)\\>" 1 font-lock-constant-face)
      ;; .foo .barBaz .qux01 .-flibble .-flibbleWobble
      ("\\<\\.-?[a-z][a-zA-Z0-9]*\\>" 0 'clojure-interop-method-face)
      ;; Foo Bar$Baz Qux_ World_OpenUDP Foo. Babylon15.
      ("\\(?:\\<\\|\\.\\|/\\|#?^\\)\\([A-Z][a-zA-Z0-9_]*[a-zA-Z0-9$_]+\\.?\\>\\)" 1 font-lock-type-face)
      ;; foo.bar.baz
      ("\\<^?\\([a-z][a-z0-9_-]+\\.\\([a-z][a-z0-9_-]*\\.?\\)+\\)" 1 font-lock-type-face)
      ;; (ns namespace) - special handling for single segment namespaces
      (,(concat "(\\<ns\\>[ \r\n\t]*"
                ;; Possibly metadata
                "\\(?:\\^?{[^}]+}[ \r\n\t]*\\)*"
                ;; namespace
                "\\([a-z0-9-]+\\)")
       (1 font-lock-type-face nil t))
      ;; foo/ Foo/ @Foo/ /FooBar
      ("\\(?:\\<\\|\\.\\)@?\\([a-zA-Z][a-zA-Z0-9$_-]*\\)/" 1 font-lock-type-face)
      ;; fooBar
      ("\\(?:\\<\\|/\\)\\([a-z]+[A-Z]+[a-zA-Z0-9$]*\\>\\)" 1 'clojure-interop-method-face)
      ;; Highlight grouping constructs in regular expressions
      (clojure-font-lock-regexp-groups
       (1 'font-lock-regexp-grouping-construct prepend))))
  "Default expressions to highlight in Clojure mode.")

(defun clojure-font-lock-syntactic-face-function (state)
  "Find and highlight text with a Clojure-friendly syntax table.

This function is passed to `font-lock-syntactic-face-function',
which is called with a single parameter, STATE (which is, in
turn, returned by `parse-partial-sexp' at the beginning of the
highlighted region)."
  (if (nth 3 state)
      ;; This might be a (doc)string or a |...| symbol.
      (let ((startpos (nth 8 state)))
        (if (eq (char-after startpos) ?|)
            ;; This is not a string, but a |...| symbol.
            nil
          (let* ((listbeg (nth 1 state))
                 (firstsym (and listbeg
                                (save-excursion
                                  (goto-char listbeg)
                                  (and (looking-at "([ \t\n]*\\(\\(\\sw\\|\\s_\\)+\\)")
                                       (match-string 1)))))
                 (docelt (and firstsym
                              (function-get (intern-soft firstsym)
                                            lisp-doc-string-elt-property))))
            (if (and docelt
                     ;; It's a string in a form that can have a docstring.
                     ;; Check whether it's in docstring position.
                     (save-excursion
                       (when (functionp docelt)
                         (goto-char (match-end 1))
                         (setq docelt (funcall docelt)))
                       (goto-char listbeg)
                       (forward-char 1)
                       (condition-case nil
                           (while (and (> docelt 0) (< (point) startpos)
                                       (progn (forward-sexp 1) t))
                             ;; ignore metadata and type hints
                             (unless (looking-at "[ \n\t]*\\(\\^[A-Z:].+\\|\\^?{.+\\)")
                               (setq docelt (1- docelt))))
                         (error nil))
                       (and (zerop docelt) (<= (point) startpos)
                            (progn (forward-comment (point-max)) t)
                            (= (point) (nth 8 state)))))
                font-lock-doc-face
              font-lock-string-face))))
    font-lock-comment-face))

(defun clojure-font-lock-setup ()
  "Configures font-lock for editing Clojure code."
  (setq-local font-lock-multiline t)
  (add-to-list 'font-lock-extend-region-functions
               'clojure-font-lock-extend-region-def t)
  (setq font-lock-defaults
        '(clojure-font-lock-keywords    ; keywords
          nil nil
          (("+-*/.<>=!?$%_&~^:@" . "w")) ; syntax alist
          nil
          (font-lock-mark-block-function . mark-defun)
          (font-lock-syntactic-face-function
           . clojure-font-lock-syntactic-face-function))))

(defun clojure-font-lock-def-at-point (point)
  "Range between the top-most def* and the fourth element after POINT.
Note that this means that there is no guarantee of proper font
locking in def* forms that are not at top level."
  (goto-char point)
  (condition-case nil
      (beginning-of-defun)
    (error nil))

  (let ((beg-def (point)))
    (when (and (not (= point beg-def))
               (looking-at "(def"))
      (condition-case nil
          (progn
            ;; move forward as much as possible until failure (or success)
            (forward-char)
            (dotimes (_ 4)
              (forward-sexp)))
        (error nil))
      (cons beg-def (point)))))

(defun clojure-font-lock-extend-region-def ()
  "Set region boundaries to include the first four elements of def* forms."
  (let ((changed nil))
    (let ((def (clojure-font-lock-def-at-point font-lock-beg)))
      (when def
        (destructuring-bind (def-beg . def-end) def
          (when (and (< def-beg font-lock-beg)
                     (< font-lock-beg def-end))
            (setq font-lock-beg def-beg
                  changed t)))))
    (let ((def (clojure-font-lock-def-at-point font-lock-end)))
      (when def
        (destructuring-bind (def-beg . def-end) def
          (when (and (< def-beg font-lock-end)
                     (< font-lock-end def-end))
            (setq font-lock-end def-end
                  changed t)))))
    changed))

(defun clojure-font-lock-regexp-groups (bound)
  "Highlight grouping constructs in regular expression.

BOUND denotes the maximum number of characters (relative to the
point) to check."
  (catch 'found
    (while (re-search-forward (concat
                               ;; A group may start using several alternatives:
                               "\\(\\(?:"
                               ;; 1. (? special groups
                               "(\\?\\(?:"
                               ;; a) non-capturing group (?:X)
                               ;; b) independent non-capturing group (?>X)
                               ;; c) zero-width positive lookahead (?=X)
                               ;; d) zero-width negative lookahead (?!X)
                               "[:=!>]\\|"
                               ;; e) zero-width positive lookbehind (?<=X)
                               ;; f) zero-width negative lookbehind (?<!X)
                               "<[=!]\\|"
                               ;; g) named capturing group (?<name>X)
                               "<[[:alnum:]]+>"
                               "\\)\\|" ;; end of special groups
                               ;; 2. normal capturing groups (
                               ;; 3. we also highlight alternative
                               ;; separarators |, and closing parens )
                               "[|()]"
                               "\\)\\)")
                              bound t)
      (let ((face (get-text-property (1- (point)) 'face)))
        (when (and (or (and (listp face)
                            (memq 'font-lock-string-face face))
                       (eq 'font-lock-string-face face))
                   (clojure-string-start t))
          (throw 'found t))))))

;; Docstring positions
(put 'ns 'clojure-doc-string-elt 2)
(put 'def 'clojure-doc-string-elt 2)
(put 'defn 'clojure-doc-string-elt 2)
(put 'defn- 'clojure-doc-string-elt 2)
(put 'defmulti 'clojure-doc-string-elt 2)
(put 'defmacro 'clojure-doc-string-elt 2)
(put 'definline 'clojure-doc-string-elt 2)
(put 'defprotocol 'clojure-doc-string-elt 2)



(defun clojure-indent-line ()
  "Indent current line as Clojure code."
  (if (clojure-in-docstring-p)
      (save-excursion
        (beginning-of-line)
        (when (looking-at "^\\s-*")
          (replace-match (clojure-docstring-fill-prefix))))
    (lisp-indent-line)))

(defun clojure-indent-function (indent-point state)
  "When indenting a line within a function call, indent properly.

INDENT-POINT is the position where the user typed TAB, or equivalent.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Clojure function with a
non-nil property `clojure-indent-function', that specifies how to do
the indentation.

The property value can be

- `defun', meaning indent `defun'-style;
- an integer N, meaning indent the first N arguments specially
  like ordinary function arguments and then indent any further
  arguments like a body;
- a function to call just as this function was called.
  If that function returns nil, that means it doesn't specify
  the indentation.

This function also returns nil meaning don't specify the indentation."
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        ;; car of form doesn't seem to be a symbol
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
                                         calculate-lisp-indent-last-sexp 0 t)))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-lisp-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
      (let* ((function (buffer-substring (point)
                                         (progn (forward-sexp 1) (point))))
             (open-paren (elt state 1))
             (method nil)
             (function-tail (first
                             (last
                              (split-string (substring-no-properties function) "/")))))
        (setq method (get (intern-soft function-tail) 'clojure-indent-function))
        (cond ((member (char-after open-paren) '(?\[ ?\{))
               (goto-char open-paren)
               (1+ (current-column)))
              ((or (eq method 'defun)
                   (and clojure-defun-style-default-indent
                        ;; largely to preserve useful alignment of :require, etc in ns
                        (not (string-match "^:" function))
                        (not method))
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`\\(?:\\S +/\\)?\\(def\\|with-\\)"
                                      function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method indent-point state))
              (clojure-use-backtracking-indent
               (clojure-backtracking-indent
                indent-point state normal-indent)))))))

(defun clojure-backtracking-indent (indent-point state normal-indent)
  "Experimental backtracking support.

Given an INDENT-POINT, the STATE, and the NORMAL-INDENT, will
move upwards in an sexp to check for contextual indenting."
  (let (indent (path) (depth 0))
    (goto-char (elt state 1))
    (while (and (not indent)
                (< depth clojure-max-backtracking))
      (let ((containing-sexp (point)))
        (parse-partial-sexp (1+ containing-sexp) indent-point 1 t)
        (when (looking-at "\\sw\\|\\s_")
          (let* ((start (point))
                 (fn (buffer-substring start (progn (forward-sexp 1) (point))))
                 (meth (get (intern-soft fn) 'clojure-backtracking-indent)))
            (let ((n 0))
              (when (< (point) indent-point)
                (condition-case ()
                    (progn
                      (forward-sexp 1)
                      (while (< (point) indent-point)
                        (parse-partial-sexp (point) indent-point 1 t)
                        (incf n)
                        (forward-sexp 1)))
                  (error nil)))
              (push n path))
            (when meth
              (let ((def meth))
                (dolist (p path)
                  (if (and (listp def)
                           (< p (length def)))
                      (setq def (nth p def))
                    (if (listp def)
                        (setq def (car (last def)))
                      (setq def nil))))
                (goto-char (elt state 1))
                (when def
                  (setq indent (+ (current-column) def)))))))
        (goto-char containing-sexp)
        (condition-case ()
            (progn
              (backward-up-list 1)
              (incf depth))
          (error (setq depth clojure-max-backtracking)))))
    indent))

;; clojure backtracking indent is experimental and the format for these
;; entries are subject to change
(put 'implement 'clojure-backtracking-indent '(4 (2)))
(put 'letfn 'clojure-backtracking-indent '((2) 2))
(put 'proxy 'clojure-backtracking-indent '(4 4 (2)))
(put 'reify 'clojure-backtracking-indent '((2)))
(put 'deftype 'clojure-backtracking-indent '(4 4 (2)))
(put 'defrecord 'clojure-backtracking-indent '(4 4 (2)))
(put 'defprotocol 'clojure-backtracking-indent '(4 (2)))
(put 'extend-type 'clojure-backtracking-indent '(4 (2)))
(put 'extend-protocol 'clojure-backtracking-indent '(4 (2)))
(put 'specify 'clojure-backtracking-indent '(4 (2)))
(put 'specify! 'clojure-backtracking-indent '(4 (2)))

(defun put-clojure-indent (sym indent)
  "Instruct `clojure-indent-function' to indent the body of SYM by INDENT."
  (put sym 'clojure-indent-function indent))

(defmacro define-clojure-indent (&rest kvs)
  "Call `put-clojure-indent' on a series, KVS."
  `(progn
     ,@(mapcar (lambda (x) `(put-clojure-indent
                             (quote ,(first x)) ,(second x)))
               kvs)))

(defun add-custom-clojure-indents (name value)
  "Allow `clojure-defun-indents' to indent user-specified macros.

Requires the macro's NAME and a VALUE."
  (custom-set-default name value)
  (mapcar (lambda (x)
            (put-clojure-indent x 'defun))
          value))

(defcustom clojure-defun-indents nil
  "List of additional symbols with defun-style indentation in Clojure.

You can use this to let Emacs indent your own macros the same way
that it indents built-in macros like with-open.  To manually set
it from Lisp code, use (put-clojure-indent 'some-symbol 'defun)."
  :type '(repeat symbol)
  :group 'clojure
  :set 'add-custom-clojure-indents)

(define-clojure-indent
  ;; built-ins
  (ns 1)
  (fn 'defun)
  (def 'defun)
  (defn 'defun)
  (bound-fn 'defun)
  (if 1)
  (if-not 1)
  (case 1)
  (condp 2)
  (when 1)
  (while 1)
  (when-not 1)
  (when-first 1)
  (do 0)
  (future 0)
  (comment 0)
  (doto 1)
  (locking 1)
  (proxy 2)
  (with-open 1)
  (with-precision 1)
  (with-local-vars 1)

  (reify 'defun)
  (deftype 2)
  (defrecord 2)
  (defprotocol 1)
  (extend 1)
  (extend-protocol 1)
  (extend-type 1)

  (try 0)
  (catch 2)
  (finally 0)

  ;; binding forms
  (let 1)
  (letfn 1)
  (binding 1)
  (loop 1)
  (for 1)
  (doseq 1)
  (dotimes 1)
  (when-let 1)
  (if-let 1)
  (when-some 1)
  (if-some 1)

  ;; data structures
  (defstruct 1)
  (struct-map 1)
  (assoc 1)

  (defmethod 'defun)

  ;; clojure.test
  (testing 1)
  (deftest 'defun)
  (are 1)
  (use-fixtures 'defun)

  ;; core.logic
  (run 'defun)
  (run* 'defun)
  (fresh 'defun)

  ;; core.async
  (alt! 0)
  (alt!! 0)
  (go 0)
  (go-loop 1)
  (thread 0))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Better docstring filling for clojure-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clojure-string-start (&optional regex)
  "Return the position of the \" that begins the string at point.
If REGEX is non-nil, return the position of the # that begins the
regex at point.  If point is not inside a string or regex, return
nil."
  (when (nth 3 (syntax-ppss)) ;; Are we really in a string?
    (save-excursion
      (save-match-data
        ;; Find a quote that appears immediately after whitespace,
        ;; beginning of line, hash, or an open paren, brace, or bracket
        (re-search-backward "\\(\\s-\\|^\\|#\\|(\\|\\[\\|{\\)\\(\"\\)")
        (let ((beg (match-beginning 2)))
          (when beg
            (if regex
                (and (char-before beg) (char-equal ?# (char-before beg)) (1- beg))
              (when (not (char-equal ?# (char-before beg)))
                beg))))))))

(defun clojure-char-at-point ()
  "Return the char at point or nil if at buffer end."
  (when (not (= (point) (point-max)))
    (buffer-substring-no-properties (point) (1+ (point)))))

(defun clojure-char-before-point ()
  "Return the char before point or nil if at buffer beginning."
  (when (not (= (point) (point-min)))
    (buffer-substring-no-properties (point) (1- (point)))))

;; TODO: Deal with the fact that when point is exactly at the
;; beginning of a string, it thinks that is the end.
(defun clojure-string-end ()
  "Return the position of the \" that ends the string at point.

Note that point must be inside the string - if point is
positioned at the opening quote, incorrect results will be
returned."
  (save-excursion
    (save-match-data
      ;; If we're at the end of the string, just return point.
      (if (and (string= (clojure-char-at-point) "\"")
               (not (string= (clojure-char-before-point) "\\")))
          (point)
        ;; We don't want to get screwed by starting out at the
        ;; backslash in an escaped quote.
        (when (string= (clojure-char-at-point) "\\")
          (backward-char))
        ;; Look for a quote not preceeded by a backslash
        (re-search-forward "[^\\]\\\(\\\"\\)")
        (match-beginning 1)))))

(defun clojure-mark-string ()
  "Mark the string at point."
  (interactive)
  (goto-char (clojure-string-start))
  (forward-char)
  (set-mark (clojure-string-end)))

(defun clojure-toggle-keyword-string ()
  "Convert the string or keyword at point to keyword or string."
  (interactive)
  (let ((original-point (point)))
    (while (and (> (point) 1)
                (not (equal "\"" (buffer-substring-no-properties (point) (+ 1 (point)))))
                (not (equal ":" (buffer-substring-no-properties (point) (+ 1 (point))))))
      (backward-char))
    (cond
     ((equal 1 (point))
      (error "Beginning of file reached, this was probably a mistake"))
     ((equal "\"" (buffer-substring-no-properties (point) (+ 1 (point))))
      (insert ":" (substring (clojure-delete-and-extract-sexp) 1 -1)))
     ((equal ":" (buffer-substring-no-properties (point) (+ 1 (point))))
      (insert "\"" (substring (clojure-delete-and-extract-sexp) 1) "\"")))
    (goto-char original-point)))

(defun clojure-delete-and-extract-sexp ()
  "Delete the sexp and return it."
  (interactive)
  (let ((begin (point)))
    (forward-sexp)
    (let ((result (buffer-substring-no-properties begin (point))))
      (delete-region begin (point))
      result)))



(defconst clojure-namespace-name-regex
  (rx line-start
      (zero-or-more whitespace)
      "("
      (zero-or-one (group (regexp "clojure.core/")))
      (zero-or-one (submatch "in-"))
      "ns"
      (zero-or-one "+")
      (one-or-more (any whitespace "\n"))
      (zero-or-more (or (submatch (zero-or-one "#")
                                  "^{"
                                  (zero-or-more (not (any "}")))
                                  "}")
                        (zero-or-more "^:"
                                      (one-or-more (not (any whitespace)))))
                    (one-or-more (any whitespace "\n")))
      ;; why is this here? oh (in-ns 'foo) or (ns+ :user)
      (zero-or-one (any ":'"))
      (group (one-or-more (not (any "()\"" whitespace))) word-end)))

;; for testing clojure-namespace-name-regex, you can evaluate this code and make
;; sure foo (or whatever the namespace name is) shows up in results. some of
;; these currently fail.
;; (mapcar (lambda (s) (let ((n (string-match clojure-namespace-name-regex s)))
;;                       (if n (match-string 4 s))))
;;         '("(ns foo)"
;;           "(ns
;; foo)"
;;           "(ns foo.baz)"
;;           "(ns ^:bar foo)"
;;           "(ns ^:bar ^:baz foo)"
;;           "(ns ^{:bar true} foo)"
;;           "(ns #^{:bar true} foo)"
;;           "(ns #^{:fail {}} foo)"
;;           "(ns ^{:fail2 {}} foo.baz)"
;;           "(ns ^{} foo)"
;;           "(ns ^{:skip-wiki true}
;;   aleph.netty
;; "
;;           "(ns
;;  foo)"
;;     "foo"))



(defun clojure-expected-ns ()
  "Return the namespace name that the file should have."
  (let* ((project-dir (file-truename
                       (locate-dominating-file default-directory
                                               "project.clj")))
         (relative (substring (file-truename (buffer-file-name))
                              (length project-dir)
                              (- (length (file-name-extension (buffer-file-name) t))))))
    (replace-regexp-in-string
     "_" "-" (mapconcat 'identity (cdr (split-string relative "/")) "."))))

(defun clojure-insert-ns-form-at-point ()
  "Insert a namespace form at point."
  (interactive)
  (insert (format "(ns %s)" (clojure-expected-ns))))

(defun clojure-insert-ns-form ()
  "Insert a namespace form at the beginning of the buffer."
  (interactive)
  (widen)
  (goto-char (point-min))
  (clojure-insert-ns-form-at-point))

(defun clojure-update-ns ()
  "Update the namespace of the current buffer.
Useful if a file has been renamed."
  (interactive)
  (let ((nsname (clojure-expected-ns)))
    (when nsname
      (save-excursion
        (save-match-data
          (if (clojure-find-ns)
              (replace-match nsname nil nil nil 4)
            (error "Namespace not found")))))))

(defun clojure-find-ns ()
  "Find the namespace of the current Clojure buffer."
  (let ((regexp clojure-namespace-name-regex))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (when (re-search-forward regexp nil t)
          (match-string-no-properties 4))))))

(defun clojure-find-def ()
  "Find the var declaration macro and symbol name of the current form.
Returns a list pair, e.g. (\"defn\" \"abc\") or (\"deftest\" \"some-test\")."
  (let ((re (concat "(\\(?:\\(?:\\sw\\|\\s_\\)+/\\)?"
                    ;; Declaration
                    "\\(def\\sw*\\)\\>"
                    ;; Any whitespace
                    "[ \r\n\t]*"
                    ;; Possibly type or metadata
                    "\\(?:#?^\\(?:{[^}]*}\\|\\(?:\\sw\\|\\s_\\)+\\)[ \r\n\t]*\\)*"
                    ;; Symbol name
                    "\\(\\(?:\\sw\\|\\s_\\)+\\)")))
    (save-excursion
      (unless (looking-at re)
        (beginning-of-defun))
      (when (search-forward-regexp re nil t)
        (list (match-string 1)
              (match-string 2))))))

;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\.\\(clj[sx]?\\|dtm\\|edn\\)\\'" . clojure-mode))

(provide 'clojure-mode)

;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions)
;; indent-tabs-mode: nil
;; End:

;;; clojure-mode.el ends here
