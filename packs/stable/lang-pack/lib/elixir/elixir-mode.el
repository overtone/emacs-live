;;; elixir-mode.el --- Major mode for editing Elixir files -*- lexical-binding: t -*-

;; Copyright 2011-2015 secondplanet
;;           2013-2015 Samuel Tonini, Matt DeBoard, Andreas Fuchs
;; Authors: Humza Yaqoob,
;;          Andreas Fuchs <asf@boinkor.net>,
;;          Matt DeBoard
;;          Samuel Tonini <tonini.samuel@gmail.com>

;; URL: https://github.com/elixir-editors/emacs-elixir
;; Created: Mon Nov 7 2011
;; Keywords: languages elixir
;; Version: 2.4.0
;; Package-Requires: ((emacs "25"))

;; This file is not a part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;  Provides font-locking, indentation and navigation support
;;  for the Elixir programming language.

;;; Code:

(require 'easymenu)           ; Elixir Mode menu definition
(require 'elixir-smie)        ; Syntax and indentation support
(require 'elixir-format)      ; Elixir Format functions

(defgroup elixir nil
  "Major mode for editing Elixir code."
  :prefix "elixir-"
  :group 'languages
  :link '(url-link :tag "Github" "https://github.com/elixir-editors/emacs-elixir")
  :link '(emacs-commentary-link :tag "Commentary" "elixir-mode"))

(defcustom elixir-mode-website-url "http://elixir-lang.org"
  "Official url of Elixir programming website."
  :type 'string)

(defcustom elixir-mode-doc-url "https://hexdocs.pm/elixir"
  "Official documentation for the Elixir programming language."
  :type 'string)

(defcustom elixir-mode-hook nil
  "Hook that runs when switching to major mode"
  :type 'hook)

(defvar elixir-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap used in `elixir-mode'.")

(defvar elixir-imenu-generic-expression
  '(("Modules" "^\\s-*defmodule[ \n\t]+\\([A-Z][A-Za-z0-9._]+\\)\\s-+.*$" 1)
    ("Public Functions" "^\\s-*def[ \n\t]+\\([a-z0-9_!\\?]+\\)\\(([^)]*)\\)*.*" 1)
    ("Private Functions" "^\\s-*defp[ \n\t]+\\([a-z0-9_!\\?]+\\)\\(([^)]*)\\)*.*" 1)
    ("Public Macros" "^\\s-*defmacro[ \n\t]+\\([a-z0-9_!\\?]+\\)\\(([^)]*)\\)*.*" 1)
    ("Private Macros" "^\\s-*defmacrop[ \n\t]+\\([a-z0-9_!\\?]+\\)\\(([^)]*)\\)*.*" 1)
    ("Delegates" "^\\s-*defdelegate[ \n\t]+\\([a-z0-9_]+\\)\\(([^)]*)\\)*.*" 1)
    ("Overridables" "^\\s-*defoverridable[ \n\t]+\\([a-z0-9_]+\\)\\(([^)]*)\\)*.*" 1)
    ("Tests" "^\\s-*test[ \t\n]+\"?\\(:?[a-z0-9_@+() \t-]+\\)\"?[ \t\n]+.*" 1))
  "Imenu pattern for `elixir-mode'.")

(defcustom elixir-basic-offset 2
  "Basic offset."
  :type 'integer)
(defcustom elixir-key-label-offset 0
  "Offset used for key label."
  :type 'integer)
(defcustom elixir-match-label-offset 2
  "Offset for a match label."
  :type 'integer)

(defgroup elixir-faces nil
  "Font-lock faces for `elixir'."
  :group 'elixir
  :group 'faces)

(defvar elixir-attribute-face 'elixir-attribute-face)
(defface elixir-attribute-face
  '((t (:inherit font-lock-preprocessor-face)))
  "For use with module attribute tokens.")

(defvar elixir-atom-face 'elixir-atom-face)
(defface elixir-atom-face
  '((t (:inherit font-lock-builtin-face)))
  "For use with atoms & map keys.")

(defvar elixir-number-face 'elixir-number-face)
(defface elixir-number-face
  '((t (:inherit default)))
  "For use with numbers.")


(eval-when-compile
  (defconst elixir-rx-constituents
    `(
      (string-delimiter . ,(rx (and
                                ;; Match even number of backslashes.
                                (or (not (any ?\\ ?\' ?\")) point
                                    ;; Quotes might be preceded by escaped quote
                                    (and (or (not (any ?\\)) point) ?\\
                                         (* ?\\ ?\\) (any ?\' ?\")))
                                (* ?\\ ?\\)
                                ;; Match single or triple quotes of any kind.
                                (group (or "\"" "\"\"\"" "'" "'''")))))
      (atoms . ,(rx ":"
                    (or
                     (and
                      (any "a-z" "A-Z" "_" "\"" "'")
                      (zero-or-more (any "a-z" "A-Z" "0-9" "_" "\"" "'" "!" "@" "?")))
                     (and "\"" (one-or-more (not (any "\""))) "\"")
                     (and "'" (one-or-more (not (any "'"))) "'"))))
      (numbers . ,(rx (and symbol-start
                           (? "-")
                           (+ digit)
                           (0+ (and "_" (= 3 digit)))
                           symbol-end)))
      (builtin . ,(rx symbol-start
                      (or "case" "cond" "for" "if" "quote" "raise" "receive" "send"
                          "super" "throw" "try" "unless" "unquote" "unquote_splicing"
                          "with")
                      symbol-end))
      (builtin-declaration . ,(rx symbol-start
                                  (or "def" "defp" "defmodule" "defprotocol"
                                      "defmacro" "defmacrop" "defdelegate"
                                      "defexception" "defstruct" "defimpl"
                                      "defguard" "defguardp" "defcallback"
                                      "defoverridable")
                                  symbol-end))
      (builtin-namespace . ,(rx symbol-start
                                (or "import" "require" "use" "alias")
                                symbol-end))
      ;; Set aside code point syntax for negation face.
      (code-point . ,(rx symbol-start
                         "?"
                         anything
                         symbol-end))
      (function-declaration . ,(rx (or line-start (not (any ".")))
                                   symbol-start
                                   (or "def" "defp")
                                   symbol-end))
      ;; The first character of an identifier must be a letter or an underscore.
      ;; After that, they may contain any alphanumeric character + underscore.
      ;; Additionally, the final character may be either `?' or `!'.
      (identifiers . ,(rx (any "A-Z" "a-z" "_")
                          (zero-or-more (any "A-Z" "a-z" "0-9" "_"))
                          (optional (or "?" "!"))))
      (keyword . ,(rx symbol-start
                      (or "fn" "do" "end" "after" "else" "rescue" "catch")
                      symbol-end))
      (keyword-operator . ,(rx symbol-start
                               (or "not" "and" "or" "when" "in")
                               symbol-end))
      ;; Module and submodule names start with upper case letter. This
      ;; can then be followed by any combination of alphanumeric chars.
      ;; In turn, this can be followed by a `.' which begins the notation of
      ;; a submodule, which follows the same naming pattern of the module.
      ;; Finally, like other identifiers, it can be terminated with either `?'
      ;; or `!'.
      (module-names . ,(rx symbol-start
                           (optional (or "%" "&"))
                           (any "A-Z")
                           (zero-or-more (any "A-Z" "a-z" "_" "0-9"))
                           (zero-or-more
                            (and "."
                                 (any "A-Z" "_")
                                 (zero-or-more (any "A-Z" "a-z" "_" "0-9"))))
                           (optional (or "!" "?"))
                           symbol-end))
      (pseudo-var . ,(rx symbol-start
                         (optional (or "%" "&"))
                         (or "_" "__MODULE__" "__DIR__" "__ENV__" "__CALLER__"
                             "__block__" "__aliases__")
                         symbol-end))
      (sigils . ,(rx "~" (or "B" "C" "D" "E" "L" "N" "R" "S" "T" "U" "b" "c" "e" "r" "s" "w")))))

  (defmacro elixir-rx (&rest sexps)
    (let ((rx-constituents (append elixir-rx-constituents rx-constituents)))
      (cond ((null sexps)
             (error "No regexp"))
            ((cdr sexps)
             (rx-to-string `(and ,@sexps) t))
            (t
             (rx-to-string (car sexps) t))))))

(defsubst elixir-syntax-in-string-or-comment-p ()
  (elixir-ppss-comment-or-string-start (syntax-ppss)))

(defsubst elixir-syntax-count-quotes (quote-char &optional point limit)
  "Count number of quotes around point (max is 3).
QUOTE-CHAR is the quote char to count.  Optional argument POINT is
the point where scan starts (defaults to current point), and LIMIT
is used to limit the scan."
  (let ((i 0))
    (while (and (< i 3)
                (or (not limit) (< (+ point i) limit))
                (eq (char-after (+ point i)) quote-char))
      (setq i (1+ i)))
    i))

(defun elixir-syntax-stringify ()
  "Put `syntax-table' property correctly on single/triple quotes."
  (let* ((num-quotes (length (match-string-no-properties 1)))
         (quote-starting-pos (- (point) num-quotes))
         (quote-ending-pos (point))
         (ppss (save-excursion
                 (syntax-ppss quote-starting-pos)))
         (string-start (and (not (elixir-ppss-comment-depth ppss))
                            (elixir-ppss-comment-or-string-start ppss)))
         (num-closing-quotes
          (and string-start
               (elixir-syntax-count-quotes
                (char-before) string-start quote-starting-pos))))
    (cond ((and string-start (= num-closing-quotes 0))
           ;; This set of quotes doesn't match the string starting
           ;; kind. Do nothing.
           nil)
          ((not string-start)
           ;; This set of quotes delimit the start of a string.
           (put-text-property quote-starting-pos (1+ quote-starting-pos)
                              'syntax-table (string-to-syntax "|")))
          ((= num-quotes num-closing-quotes)
           ;; This set of quotes delimit the end of a string.
           (put-text-property (1- quote-ending-pos) quote-ending-pos
                              'syntax-table (string-to-syntax "|")))
          ((> num-quotes num-closing-quotes)
           ;; This may only happen whenever a triple quote is closing
           ;; a single quoted string. Add string delimiter syntax to
           ;; all three quotes.
           (put-text-property quote-starting-pos quote-ending-pos
                              'syntax-table (string-to-syntax "|"))))))


(defun elixir-syntax-propertize-interpolation ()
  (let* ((beg (match-beginning 0))
         (context (save-excursion (save-match-data (syntax-ppss beg)))))
    (put-text-property beg (1+ beg) 'syntax-table (string-to-syntax "w"))
    (put-text-property beg (1+ beg) 'elixir-interpolation
                       (cons (elixir-ppss-string-terminator context)
                             (match-data)))))

(defconst elixir-sigil-delimiter-pair
  '((?\( . ")")
    (?\{ . "}")
    (?\< . ">")
    (?\[ . "]")))

(defun elixir-syntax-replace-property-in-sigil ()
  (unless (elixir-syntax-in-string-or-comment-p)
    (let ((heredoc-p (save-excursion
                       (goto-char (match-beginning 0))
                       (looking-at-p "~[BCDELNRSTUbcersw]\\(?:'''\\|\"\"\"\\)"))))
      (unless heredoc-p
        (forward-char 1)
        (let* ((start-delim (char-after (1- (point))))
               (end-delim (or (assoc-default start-delim elixir-sigil-delimiter-pair)
                              (char-to-string start-delim)))
               (end (save-excursion
                      (let (finish)
                        (while (not finish)
                          (skip-chars-forward (concat "^" end-delim))
                          (if (or (not (eq (char-before) ?\\))
                                  (eq (char-before (1- (point))) ?\\)
                                  (eobp))
                              (setq finish t)
                            (forward-char 1)))
                        (point))))
               (word-syntax (string-to-syntax "w")))
          (when (memq start-delim '(?' ?\"))
            (setq end (1+ end))
            (forward-char -1))
          (while (re-search-forward "[\"'#]" end 'move)
            (put-text-property (1- (point)) (point) 'syntax-table word-syntax)))))))

(defun elixir-syntax-propertize-function (start end)
  (let ((case-fold-search nil))
    (goto-char start)
    (funcall
     (syntax-propertize-rules
      ("\\(\\?\\)[\"']"
       (1 (if (save-excursion
                (elixir-ppss-string-terminator
                 (syntax-ppss (match-beginning 0))))
              ;; Within a string, skip.
              (ignore
               (goto-char (match-end 1)))
            (put-text-property (match-end 1) (match-end 0)
                               'syntax-table (string-to-syntax "_"))
            (string-to-syntax "'"))))
      ((elixir-rx string-delimiter)
       (0 (ignore (elixir-syntax-stringify))))
      ((elixir-rx sigils)
       (0 (ignore (elixir-syntax-replace-property-in-sigil))))
      ((rx (group "#{" (0+ (not (any "}"))) "}"))
       (0 (ignore (elixir-syntax-propertize-interpolation)))))
     start end)))

(defun elixir-match-interpolation (limit)
  (let ((pos (next-single-char-property-change (point) 'elixir-interpolation
                                               nil limit)))
    (when (and pos (> pos (point)))
      (goto-char pos)
      (let ((value (get-text-property pos 'elixir-interpolation)))
        (if (car value)
            (progn
              (set-match-data (cdr value))
              t)
          (elixir-match-interpolation limit))))))


(defconst elixir-font-lock-keywords
  `(
    ;; String interpolation
    (elixir-match-interpolation 0 font-lock-variable-name-face t)

    ;; Module attributes
    (,(elixir-rx (and "@" identifiers))
     0 elixir-attribute-face)

    ;; Keywords
    (,(elixir-rx (and (or line-start (not (any ".")))
                      (group (or builtin builtin-declaration builtin-namespace
                                 keyword keyword-operator))))
     1 font-lock-keyword-face)

    ;; Function names, i.e. `def foo do'.
    (,(elixir-rx (group function-declaration)
                 space
                 (group identifiers))
     2 font-lock-function-name-face)

    ;; Sigil patterns. Elixir has support for eight different sigil delimiters.
    ;; This isn't a very DRY approach here but it gets the job done.
    (,(elixir-rx (group sigils)
                 (and "/"
                      (group (zero-or-more (or (and "\\" "/") (not (any "/" "\n" "\r")))))
                      "/"))
     (1 font-lock-builtin-face)
     (2 font-lock-string-face))
    (,(elixir-rx (group sigils)
                 (and "["
                      (group (zero-or-more (or (and "\\" "]") (not (any "]" "\n" "\r")))))
                      "]"))
     (1 font-lock-builtin-face)
     (2 font-lock-string-face))
    (,(elixir-rx (group sigils)
                 (and "{"
                      (group (zero-or-more (or (and "\\" "}") (not (any "}" "\n" "\r")))))
                      "}"))
     (1 font-lock-builtin-face)
     (2 font-lock-string-face))
    (,(elixir-rx (group sigils)
                 (and "("
                      (group (zero-or-more (or (and "\\" ")") (not (any ")" "\n" "\r")))))
                      ")"))
     (1 font-lock-builtin-face)
     (2 font-lock-string-face))
    (,(elixir-rx (group sigils)
                 (and "|"
                      (group (zero-or-more (or (and "\\" "|") (not (any "|" "\n" "\r")))))
                      "|"))
     (1 font-lock-builtin-face)
     (2 font-lock-string-face))
    (,(elixir-rx (group sigils)
                 (and "\""
                      (group (zero-or-more (or (and "\\" "\"") (not (any "\"" "\n" "\r")))))
                      "\""))
     (1 font-lock-builtin-face)
     (2 font-lock-string-face))
    (,(elixir-rx (group sigils)
                 (and "'"
                      (group (zero-or-more (or (and "\\" "'") (not (any "'" "\n" "\r")))))
                      "'"))
     (1 font-lock-builtin-face)
     (2 font-lock-string-face))
    (,(elixir-rx (group sigils)
                 (and "<"
                      (group (zero-or-more (or (and "\\" ">") (not (any ">" "\n" "\r")))))
                      ">"))
     (1 font-lock-builtin-face)
     (2 font-lock-string-face))

    ;; Modules
    (,(elixir-rx (group module-names))
     1 font-lock-type-face)

    ;; Atoms and singleton-like words like true/false/nil.
    (,(elixir-rx symbol-start
                 (group (or atoms "true" "false" "nil"))
                 symbol-end
                 (zero-or-more space)
                 (optional "="))
     1 elixir-atom-face)

    ;; Numbers
    (,(elixir-rx (group numbers))
     1 elixir-number-face)

    ;; Gray out variables starting with "_"
    (,(elixir-rx symbol-start
                 (group (and "_"
                             (any "A-Z" "a-z" "0-9"))
                        (zero-or-more (any "A-Z" "a-z" "0-9" "_"))
                        (optional (or "?" "!"))))
     1 font-lock-comment-face)

    ;; Variable definitions
    (,(elixir-rx (group identifiers)
                 (zero-or-more space)
                 (repeat 1 "=")
                 (or (or sigils identifiers space)
                     (one-or-more "\n")))
     1 font-lock-variable-name-face)

    ;; Map keys
    (,(elixir-rx (group (and identifiers ":")) (or space "\n"))
     1 elixir-atom-face)

    ;; Pseudovariables
    (,(elixir-rx (group pseudo-var))
     1 font-lock-constant-face)

    ;; Code points
    (,(elixir-rx (group code-point))
     1 font-lock-negation-char-face)))

;;;###autoload
(defun elixir-mode-open-github ()
  "Elixir mode open GitHub page."
  (interactive)
  (browse-url "https://github.com/elixir-editors/emacs-elixir"))

;;;###autoload
(defun elixir-mode-open-elixir-home ()
  "Elixir mode go to language home."
  (interactive)
  (browse-url elixir-mode-website-url))

;;;###autoload
(defun elixir-mode-open-docs-master ()
  "Elixir mode go to master documentation."
  (interactive)
  (browse-url (concat elixir-mode-doc-url "/master")))

;;;###autoload
(defun elixir-mode-open-docs-stable ()
  "Elixir mode go to stable documentation."
  (interactive)
  (browse-url elixir-mode-doc-url))

(defconst elixir--version
  (eval-when-compile
    (require 'lisp-mnt)
    (let ((file (or byte-compile-current-file
		    load-file-name
		    (buffer-file-name))))
      (if file
	  (with-temp-buffer
	    (insert-file-contents file)
	    (lm-version))
	"Unknown")))
  "The current version of `elixir-mode'.")

;;;###autoload
(defun elixir-mode-version (&optional show-version)
  "Get the Elixir-Mode version as string.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.

The returned string includes both, the version from package.el
and the library version, if both a present and different.

If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil."
  (interactive (list t))
  (when show-version
    (message "Elixir-Mode version: %s" elixir--version))
  elixir--version)

(defun elixir-mode-fill-doc-string ()
  (interactive)
  (save-excursion
    (re-search-backward (rx "@" (or "moduledoc" "typedoc" "doc") space "\"\"\"") nil t)
    (re-search-forward "\"\"\"" nil t)
    (set-mark (point))
    (re-search-forward "\"\"\"" nil t)
    (re-search-backward "^ *\"\"\"" nil t)
    (backward-char)
    (fill-region (point) (mark))))

(defun elixir-beginning-of-defun (&optional arg)
  (interactive "p")
  (let ((regexp (concat "^\\s-*" (elixir-rx builtin-declaration)))
        case-fold-search)
    (while (and (re-search-backward regexp nil t (or arg 1))
                (elixir-syntax-in-string-or-comment-p)))
    (goto-char (line-beginning-position))))

(defun elixir-end-of-defun ()
  (interactive)
  (goto-char (line-beginning-position))
  (if (re-search-forward "\\_<do:" (line-end-position) t)
      (goto-char (line-end-position))
    (goto-char (line-end-position))
    (let ((level (save-excursion
                   (elixir-beginning-of-defun)
                   (current-indentation)))
          finish)
      (while (and (not finish) (re-search-forward "^\\s-*\\_<end\\_>" nil t))
        (when (and (not (elixir-syntax-in-string-or-comment-p))
                   (= (current-indentation) level))
          (setq finish t)))
      (when (looking-back "^\\s-*\\_<end" (line-beginning-position))
        (forward-line 1)))))

(defun elixir--docstring-p (&optional pos)
  "Check to see if there is a docstring at POS."
  (let ((pos (or pos (elixir-ppss-comment-or-string-start
                      (parse-partial-sexp (point-min) (point))))))
    (when pos
      (save-excursion
        (goto-char pos)
        (and (looking-at "\"\"\"")
             (looking-back (rx "@" (or "moduledoc" "typedoc" "doc") (+ space))
                           (line-beginning-position)))))))

(defun elixir-font-lock-syntactic-face-function (state)
  (if (elixir-ppss-string-terminator state)
      (if (elixir--docstring-p (elixir-ppss-comment-or-string-start state))
          font-lock-doc-face
        font-lock-string-face)
    font-lock-comment-face))

(easy-menu-define elixir-mode-menu elixir-mode-map
  "Elixir mode menu."
  '("Elixir"
    ["Indent line" smie-indent-line]
    "---"
    ["elixir-mode on GitHub" elixir-mode-open-github]
    ["Elixir homepage" elixir-mode-open-elixir-home]
    ["About" elixir-mode-version]))

;;;###autoload
(define-derived-mode elixir-mode prog-mode "Elixir"
  "Major mode for editing Elixir code.

\\{elixir-mode-map}"
  (setq-local font-lock-defaults
              '(elixir-font-lock-keywords
                nil nil nil nil
                (font-lock-syntactic-face-function
                 . elixir-font-lock-syntactic-face-function)))
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#+ *")
  (setq-local comment-use-syntax t)
  (setq-local syntax-propertize-function #'elixir-syntax-propertize-function)
  (setq-local imenu-generic-expression elixir-imenu-generic-expression)

  (setq-local beginning-of-defun-function #'elixir-beginning-of-defun)
  (setq-local end-of-defun-function #'elixir-end-of-defun)

  (smie-setup elixir-smie-grammar 'verbose-elixir-smie-rules
              :forward-token 'elixir-smie-forward-token
              :backward-token 'elixir-smie-backward-token)
  ;; https://github.com/elixir-editors/emacs-elixir/issues/363
  ;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=35496
  (setq-local smie-blink-matching-inners nil))

;; Invoke elixir-mode when appropriate

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.elixir\\'" . elixir-mode))
  (add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-mode))
  (add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-mode))
  (add-to-list 'auto-mode-alist '("mix\\.lock" . elixir-mode)))

(provide 'elixir-mode)

;;; elixir-mode.el ends here
