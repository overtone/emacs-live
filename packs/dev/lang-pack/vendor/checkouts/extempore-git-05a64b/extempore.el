;;; extempore.el --- Emacs major mode for Extempore source files

;; Author: Ben Swift <benjamin.j.swift@gmail.com>
;; Keywords: Extempore

;; Adapted from: scheme.el by Bill Rozas and Dave Love
;; Also includes some work done by Hector Levesque and Andrew Sorensen

;; Copyright (c) 2011-2013, Andrew Sorensen

;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:

;; 1. Redistributions of source code must retain the above copyright notice,
;;    this list of conditions and the following disclaimer.

;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;    this list of conditions and the following disclaimer in the documentation
;;    and/or other materials provided with the distribution.

;; Neither the name of the authors nor other contributors may be used to endorse
;; or promote products derived from this software without specific prior written
;; permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;; Commentary:

;; A major mode for editing Extempore code. See the Extempore project
;; page at http://github.com/digego/extempore for more details.

;; Installation:

;; To set up Emacs to automatically load this major mode for any .xtm
;; files, add the following lines to your .emacs

;; (autoload 'extempore-mode "/path/to/extempore/extras/extempore.el" "" t)
;; (add-to-list 'auto-mode-alist '("\\.xtm$" . extempore-mode))

;; Currently, extempore.el requires Emacs 24, because it inherits from
;; prog-mode (via lisp-mode)

;;; Code:

(require 'lisp-mode)
(require 'eldoc)
;; to support both 24.3 and earlier
(unless (require 'cl-lib nil t)
  (require 'cl))

(defvar extempore-mode-syntax-table
  (let ((st (make-syntax-table))
	(i 0))
    ;; Symbol constituents
    (while (< i ?0)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?9))
    (while (< i ?A)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?Z))
    (while (< i ?a)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?z))
    (while (< i 128)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    ;; Whitespace
    (modify-syntax-entry ?\t "    " st)
    (modify-syntax-entry ?\n ">   " st)
    (modify-syntax-entry ?\f "    " st)
    (modify-syntax-entry ?\r "    " st)
    (modify-syntax-entry ?\s "    " st)
    ;; paren delimiters
    (modify-syntax-entry ?\( "()  " st)
    (modify-syntax-entry ?\) ")(  " st)
    ;; comment delimiters
    (modify-syntax-entry ?\; "<   " st)
    (modify-syntax-entry ?\" "\"   " st)
    (modify-syntax-entry ?' "'   " st)
    (modify-syntax-entry ?` "'   " st)
    ;; in xtlang, commas are used in type annotations
    (modify-syntax-entry ?, "_   " st)
    ;; Special characters
    (modify-syntax-entry ?@ "'   " st)
    (modify-syntax-entry ?# "'   " st)
    (modify-syntax-entry ?\\ "\\   " st)
    st))

(defvar extempore-mode-abbrev-table nil)
(define-abbrev-table 'extempore-mode-abbrev-table ())

(defvar extempore-imenu-generic-expression
  '(("scheme"
     "(\\(define\\(\\|-macro\\)\\)\\s-+(?\\(\\(\\sw\\|\\s_\\)+\\)\\_>" 3)
    ("instruments"
     "(\\(define\\(\\|-instrument\\|-sampler\\)\\)\\s-+\\(\\(\\sw\\|\\s_\\)+\\)\\_>" 3)
    ("xtlang"
     "(\\(bind-\\(func\\|val\\|type\\|alias\\|poly\\)\\)\\s-+\\(\\(\\sw\\|\\s_\\)+\\)\\_>" 3)
    ("xtlang"
     "(bind-lib\\s-+\\(\\sw\\|\\s_\\)+\\s-+\\(\\(\\sw\\|\\s_\\)+\\)\\_>" 2))
  "Imenu generic expression for Extempore mode.  See `imenu-generic-expression'.")


(defun extempore-mode-variables ()
  (set-syntax-table extempore-mode-syntax-table)
  (setq local-abbrev-table extempore-mode-abbrev-table)
  (set (make-local-variable 'paragraph-start) (concat "$\\|" page-delimiter))
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  (set (make-local-variable 'paragraph-ignore-fill-prefix) t)
  (set (make-local-variable 'fill-paragraph-function) 'lisp-fill-paragraph)
  ;; Adaptive fill mode gets in the way of auto-fill,
  ;; and should make no difference for explicit fill
  ;; because lisp-fill-paragraph should do the job.
  (set (make-local-variable 'adaptive-fill-mode) nil)
  (set (make-local-variable 'indent-line-function) 'lisp-indent-line)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'outline-regexp) ";;; \\|(....")
  (set (make-local-variable 'comment-start) ";")
  (set (make-local-variable 'comment-add) 1)
  ;; Look within the line for a ; following an even number of backslashes
  ;; after either a non-backslash or the line beginning.
  (set (make-local-variable 'comment-start-skip)
       "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\);+[ \t]*")
  (set (make-local-variable 'font-lock-comment-start-skip) ";+ *")
  (set (make-local-variable 'comment-column) 40)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'lisp-indent-function) 'extempore-indent-function)
  ;; (set (make-local-variable 'imenu-case-fold-search) t)
  (setq imenu-generic-expression extempore-imenu-generic-expression)
  (set (make-local-variable 'font-lock-defaults)
       '(extempore-font-lock-keywords
	 nil t (("+-*/,.<>=!?$%_&~^:" . "w") (?#. "w 14"))
	 beginning-of-defun
	 (font-lock-mark-block-function . mark-defun)
	 (font-lock-syntactic-face-function
	  . extempore-font-lock-syntactic-face-function)
	 (parse-sexp-lookup-properties . t)
	 (font-lock-extra-managed-props syntax-table)))
  (set (make-local-variable 'lisp-doc-string-elt-property)
       'extempore-doc-string-elt))

(defvar extempore-mode-map
  (let ((smap (make-sparse-keymap))
        (map (make-sparse-keymap "Extempore")))
    (set-keymap-parent smap lisp-mode-shared-map)
    (define-key smap [menu-bar extempore] (cons "Extempore" map))
    ;; (define-key map [extempore-run] '("Run Inferior Extempore" . extempore-run))
    (define-key map [uncomment-region]
      '("Uncomment Out Region" . (lambda (beg end)
                                   (interactive "r")
                                   (comment-region beg end '(4)))))
    (define-key map [comment-region] '("Comment Out Region" . comment-region))
    (define-key map [indent-region] '("Indent Region" . indent-region))
    (define-key map [indent-line] '("Indent Line" . lisp-indent-line))
    (put 'comment-region 'menu-enable 'mark-active)
    (put 'uncomment-region 'menu-enable 'mark-active)
    (put 'indent-region 'menu-enable 'mark-active)
    smap)
  "Keymap for Extempore mode.
All commands in `lisp-mode-shared-map' are inherited by this map.")

;;;###autoload
(define-derived-mode extempore-mode prog-mode "Extempore"
  "Major mode for editing Extempore code.
This mode has been adapted from `scheme-mode'.

In addition, if an Extempore process is running in a shell
buffer, some additional commands will be defined, for evaluating
expressions and controlling the extempore process.

Entry to this mode calls the value of `extempore-mode-hook'."
  (extempore-mode-variables))

(defgroup extempore nil
  "Editing Extempore code."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'lisp)

(defcustom extempore-mode-hook nil
  "Normal hook run when entering `extempore-mode'.
See `run-hooks'."
  :type 'hook
  :group 'extempore)

(defcustom extempore-default-device-number nil
  "Default device (passed as Extempore's --device option)."
  :type 'integer
  :group 'extempore)

(defcustom extempore-default-host "localhost"
  "Default host where the extempore process is running."
  :type 'string
  :group 'extempore)

(defcustom extempore-default-port 7099
  "Default port where the extempore process is running."
  :type 'integer
  :group 'extempore)

(defcustom extempore-default-connection-type "TCP"
  "Default connection type (either \"TCP\" or \"TCP-OSC\"."
  :type 'string
  :group 'extempore)

(defcustom extempore-use-pretty-lambdas t
  "Use pretty (greek symbol) lambdas in buffer?"
  :type 'boolean
  :group 'extempore)

(defcustom extempore-tab-completion t
  "Use <TAB> key for (contextual) symbol completion"
  :type 'boolean
  :group 'extempore)

(defcustom user-extempore-directory nil
  "Location of the extempore directory."
  :type 'string
  :group 'extempore)

(defcustom extempore-program-args nil
  "Arguments to pass to the extempore process started by `extempore-run'."
  :type 'string
  :group 'extempore)

(defface extempore-blink-face
  '((t (:foreground "#FF00FF" :weight bold :inherit nil)))
  "Face used for 'blinking' code when it is evaluated."
  :group 'extempore)

(defface extempore-sb-blink-face
  '((t (:foreground "#00FFFF" :weight bold :inherit nil)))
  "Face used for 'blinking' code in slave buffers."
  :group 'extempore)

;; from emacs-starter-kit

(defface extempore-paren-face
  '((((class color) (background dark))
     (:foreground "grey50"))
    (((class color) (background light))
     (:foreground "grey55")))
  "Face used to dim parentheses in extempore."
  :group 'extempore)

(defun extempore-keybindings (keymap)
  ;; emacs conventions
  (define-key keymap (kbd "C-M-x") 'extempore-send-definition)   ;gnu convention
  (define-key keymap (kbd "C-x C-e") 'extempore-send-last-sexp)  ;gnu convention
  (define-key keymap (kbd "C-c C-e") 'extempore-send-definition)
  (define-key keymap (kbd "C-c M-e") 'extempore-send-definition-and-go)
  (define-key keymap (kbd "C-c C-r") 'extempore-send-region)
  (define-key keymap (kbd "C-c M-r") 'extempore-send-region-and-go)
  (define-key keymap (kbd "C-c C-b") 'extempore-send-buffer)
  (define-key keymap (kbd "C-c M-b") 'extempore-send-buffer-and-go)
  (define-key keymap (kbd "C-c C-z") 'switch-to-extempore)
  ;; old Extempore defaults 
  (define-key keymap (kbd "C-x C-x") 'extempore-send-definition) ;extempore convention  
  (define-key keymap (kbd "C-x C-j") 'extempore-connect)
  (define-key keymap (kbd "C-u C-x C-j") 'extempore-disconnect-all)
  (define-key keymap (kbd "C-x C-j") 'extempore-connect)
  (define-key keymap (kbd "C-x C-r") 'extempore-send-region)
  (define-key keymap (kbd "C-x C-b") 'extempore-send-buffer)
  ;; (define-key keymap (kbd "C-x y")   'extempore-tr-animation-mode)
  (define-key keymap (kbd "C-c C-l") 'extempore-logger-mode)
  ;; slave buffer mode
  (define-key keymap (kbd "C-c c s") 'extempore-sb-mode)
  (define-key keymap (kbd "C-c c p") 'extempore-sb-toggle-current-buffer))

(extempore-keybindings extempore-mode-map)

(if extempore-tab-completion
    (define-key extempore-mode-map (kbd "TAB")
      '(lambda ()
         (interactive)
         (if (minibufferp)
             (unless (minibuffer-complete)
               (dabbrev-expand nil))
           (if mark-active
               (indent-region (region-beginning)
                              (region-end))
             (if (looking-at "\\_>")
                 (dabbrev-expand nil)
               (indent-for-tab-command)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generate function name lists from source files
;;
;; scheme ones from OPDefines.h
;; xtlang from llvm.ti
;; (these files need to be open in buffers for the below functions to
;; work properly)
;;
;; this stuff is currently a bit fragile, so I've hardcoded in the
;; names as they stand at 14/7/12

(defun extempore-find-scheme-names (names)
  (if (re-search-forward "\".*\"" nil t)
      (extempore-find-scheme-names
       (cons (buffer-substring-no-properties
              (+ (car (match-data)) 1)
              (- (cadr (match-data)) 1))
             names))
    (delete-dups names)))

;; (setq extempore-scheme-names
;;       (cl-set-difference
;;        (with-current-buffer "OPDefines.h"
;;          (goto-char (point-min))
;;          (extempore-find-scheme-names '()))
;;        extempore-builtin-names))

(defun extempore-find-xtlang-names (names)
  (if (re-search-forward "(\\(member\\|equal\\?\\|eq\\?\\) \\((car ast)\\|ast\\) \'" nil t)
      (let ((syms (read (thing-at-point 'sexp))))
        (extempore-find-xtlang-names
         (if (listp syms)
             (append syms names)
           (cons syms names))))
    (delete-dups (mapcar 'symbol-name names))))

;; (setq extempore-xtlang-names
;;       (cl-set-difference
;;        (with-current-buffer "llvmti.xtm"
;;          (goto-char (point-min))
;;          (extempore-find-xtlang-names '()))
;;        (append extempore-builtin-names
;;                extempore-scheme-names)
;;        :test 'string-equal))

(defconst extempore-font-lock-keywords-scheme
  ;; scheme language builtin & function names - used for font locking
  ;; (colouring).
  ;; This list is curated by hand - it's usually pretty up to date,
  ;; but shouldn't be relied on as an Extempore language reference.
  (eval-when-compile
    (let ((extempore-builtin-names '("or" "and" "let" "lambda" "if" "else" "dotimes" "cond" "begin" "syntax-rules" "syntax" "map" "do" "letrec-syntax" "letrec" "eval" "apply" "quote" "quasiquote" "let-syntax" "let*" "for-each" "case" "call-with-output-file" "call-with-input-file" "call/cc" "call-with-current-continuation" "memzone" "letz"))
          (extempore-scheme-names '("set!" "caaaar" "cdaaar" "cadaar" "cddaar" "caadar" "cdadar" "caddar" "cdddar" "caaadr" "cdaadr" "cadadr" "cddadr" "caaddr" "cdaddr" "cadddr" "cddddr" "caaar" "cdaar" "cadar" "cddar" "caadr" "cdadr" "caddr" "cdddr" "caar" "cdar" "cadr" "cddr" "car" "cdr" "print" "println" "load" "gensym" "tracing" "make-closure" "defined?" "inexact->exact" "exp" "log" "sin" "cos" "tan" "asin" "acos" "atan" "sqrt" "expt" "floor" "ceiling" "truncate" "round" "+" "-" "*" "/" "%" "bitwise-not" "bitwise-and" "bitwise-or" "bitwise-eor" "bitwise-shift-left" "bitwise-shift-right" "quotient" "remainder" "modulo" "car" "cdr" "cons" "set-car!" "set-cdr!" "char->integer" "integer->char" "char-upcase" "char-downcase" "symbol->string" "atom->string" "string->symbol" "string->atom" "sexpr->string" "string->sexpr" "real->integer" "make-string" "string-length" "string-ref" "string-set!" "string-append" "substring" "vector" "make-vector" "vector-length" "vector-ref" "vector-set!" "not" "boolean?" "eof-object?" "null?" "=" "<" ">" "<=" ">=" "member" "equal?" "eq?" "eqv?" "symbol?" "number?" "string?" "integer?" "real?" "rational?" "char?" "char-alphabetic?" "char-numeric?" "char-whitespace?" "char-upper-case?" "char-lower-case?" "port?" "input-port?" "output-port?" "procedure?" "pair?" "list?" "environment?" "vector?" "cptr?" "eq?" "eqv?" "force" "write" "write-char" "display" "newline" "error" "reverse" "list*" "append" "put" "get" "quit" "new-segment" "oblist" "sexp-bounds-port" "current-output-port" "open-input-file" "open-output-file" "open-input-output-file" "open-input-string" "open-output-string" "open-input-output-string" "close-input-port" "close-output-port" "interaction-environment" "current-environment" "read" "read-char" "peek-char" "char-ready?" "set-input-port" "set-output-port" "length" "assq" "get-closure-code" "closure?" "macro?" "macro-expand")))
      (list
       ;; other type annotations (has to be first in list)
       '(":[^ \t)]?+"
         (0 font-lock-type-face))
       ;; built-ins
       (list
        (concat
         "("
         (regexp-opt extempore-builtin-names t)
         "\\>")
        '(1 font-lock-keyword-face t))
       ;; float and int literals
       '("\\_<[-+]?[/.[:digit:]]+?\\_>"
         (0 font-lock-constant-face))
       ;; hack to make sure / gets highlighted as a function
       '("\\_</\\_>"
         (0 font-lock-function-name-face t))
       ;; boolean literals
       '("\\_<#[tf]\\_>"
         (0 font-lock-constant-face))
       ;; definitions
       (list (concat
              "(\\(define\\(\\|-macro\\|-syntax\\|-instrument\\|-sampler\\)\\)\\_>\\s-*(?\\(\\sw+\\)?")
             '(1 font-lock-keyword-face)
             '(3 font-lock-function-name-face))
       ;; scheme functions
       (list
        (regexp-opt extempore-scheme-names 'symbols)
        '(1 font-lock-function-name-face))
       ;; It wouldn't be Scheme w/o named-let.
       '("(let\\s-+\\(\\sw+\\)"
         (1 font-lock-function-name-face))))))

(defconst extempore-font-lock-keywords-xtlang
  ;; xtlang language builtin names - used for font locking (colouring).
  ;; This list is curated by hand - it's usually pretty up to date,
  ;; but shouldn't be relied on as an Extempore language reference.
  (eval-when-compile
    (let ((extempore-xtlang-names '("random" "afill!" "pfill!" "tfill!" "vfill!" "array-fill!" "pointer-fill!" "tuple-fill!" "vector-fill!" "free" "array" "tuple" "list" "~" "cset!" "cref" "cast" "convert" "&" "bor" "ang-names" "<<" ">>" "nil" "printf" "sprintf" "null" "now" "pset!" "pref-ptr" "vset!" "vref" "aset!" "aref" "aref-ptr" "tset!" "tref" "tref-ptr" "salloc" "halloc" "zalloc" "alloc" "schedule" "expf" "logf" "sinf" "cosf" "tanf" "asinf" "acosf" "atanf" "sqrtf" "exptf" "floorf" "ceilingf" "truncatef" "roundf" "llvm_printf" "push_zone" "pop_zone" "memzone" "callback" "llvm_sprintf" "make-array" "array-set!" "array-ref" "array-ref-ptr" "pointer-set!" "pointer-ref" "pointer-ref-ptr" "stack-alloc" "heap-alloc" "zone-alloc" "make-tuple" "tuple-set!" "tuple-ref" "tuple-ref-ptr" "closure-set!" "closure-ref" "pref" "pdref" "impc_null" "bitcast" "void" "ifret" "ret->" "clrun->" "make-env-zone" "make-env" "<>")))
      (list
       ;; definitions
       ;; closure type annotations (i.e. specified with a colon)
       '("(\\(bind-\\(func\\|poly\\)\\)\\s-+\\([^ \t:]+\\)\\(:[^ \t)]?+\\)?"
         (1 font-lock-keyword-face)
         (3 font-lock-function-name-face)
         (4 font-lock-type-face prepend t))
       ;; (list
       ;;  (concat
       ;;   "(\\(bind-\\(func\\|poly\\)\\)\\_>"
       ;;   ;; Any whitespace and declared object.
       ;;   "\s-*"
       ;;   "\\(\\sw+\\)?")
       ;;  '(1 font-lock-keyword-face)
       ;;  '(3 font-lock-function-name-face))
       ;; important xtlang functions
       (list
        (regexp-opt extempore-xtlang-names 'symbols)
        '(1 font-lock-function-name-face))
       ;; bind-type/alias
       '("(\\(bind-\\(type\\|alias\\)\\)\\s-+\\(\\S-+\\)\\s-+\\([^ \t)]+\\))"
         (1 font-lock-keyword-face)
         (3 font-lock-function-name-face)
         (4 font-lock-type-face t))
       ;; bind-lib
       '("(\\(bind-lib\\)\\s-+\\(\\S-+\\)\\s-+\\(\\S-+\\)\\s-+\\([^ \t)]+\\))"
         (1 font-lock-keyword-face)
         (2 font-lock-constant-face)
         (3 font-lock-function-name-face)
         (4 font-lock-type-face t))
       ;; bind-lib-func
       '("(\\(bind-lib-func\\)\\s-+\\(\\S-+\\)\\s-+\\(\\S-+\\)\\s-+\\([^ \t)]+\\))"
         (1 font-lock-keyword-face)
         (2 font-lock-constant-face)
         (3 font-lock-function-name-face)
         (4 font-lock-type-face t))
       ;; bind-val
       '("(\\(bind-val\\)\\s-+\\(\\S-+\\)\\s-+\\([^ \t)]?+\\)"
         (1 font-lock-keyword-face)
         (2 font-lock-function-name-face)
         (3 font-lock-type-face t))
       ;; bind-lib-val
       '("(\\(bind-lib-val\\)\\s-+\\(\\S-+\\)\\s-+\\(\\S-+\\)\\s-+\\([^ \t)]+\\))"
         (1 font-lock-keyword-face)
         (2 font-lock-constant-face)
         (3 font-lock-function-name-face)
         (4 font-lock-type-face t))
       ;; cast
       '("(\\(cast\\|convert\\)\\s-+\\S-+\\s-+\\([^ \t)]?+\\))"
	 (2 font-lock-type-face))
       ;; type coercion stuff
       (list
        (concat
         "(" (regexp-opt
              (let ((types '("i1" "i8" "i16" "i32" "i64" "f" "d")))
                (apply 'append (mapcar (lambda (a)
                                         (mapcar (lambda (b)
                                                   (concat a "to" b))
                                                 (remove a types)))
                                       types))) t) "\\>")
        '(1 font-lock-type-face))))))

(font-lock-add-keywords 'extempore-mode
                        '(("(\\|)" . 'extempore-paren-face)))

(defvar extempore-font-lock-keywords
  (append extempore-font-lock-keywords-scheme
          extempore-font-lock-keywords-xtlang)
  "Expressions to highlight in extempore-mode.")

(defconst extempore-sexp-comment-syntax-table
  (let ((st (make-syntax-table extempore-mode-syntax-table)))
    (modify-syntax-entry ?\; "." st)
    (modify-syntax-entry ?\n " " st)
    (modify-syntax-entry ?#  "'" st)
    st))

(put 'lambda 'extempore-doc-string-elt 2)
;; Docstring's pos in a `define' depends on whether it's a var or fun def.
(put 'define 'extempore-doc-string-elt
     (lambda ()
       ;; The function is called with point right after "define".
       (forward-comment (point-max))
       (if (eq (char-after) ?\() 2 0)))

(defun extempore-font-lock-syntactic-face-function (state)
  (when (and (null (nth 3 state))
	     (eq (char-after (nth 8 state)) ?#)
	     (eq (char-after (1+ (nth 8 state))) ?\;))
    ;; It's a sexp-comment.  Tell parse-partial-sexp where it ends.
    (save-excursion
      (let ((pos (point))
	    (end
	     (condition-case err
		 (let ((parse-sexp-lookup-properties nil))
		   (goto-char (+ 2 (nth 8 state)))
		   ;; FIXME: this doesn't handle the case where the sexp
		   ;; itself contains a #; comment.
		   (forward-sexp 1)
		   (point))
	       (scan-error (nth 2 err)))))
	(when (< pos (- end 2))
	  (put-text-property pos (- end 2)
			     'syntax-table extempore-sexp-comment-syntax-table))
	(put-text-property (- end 1) end 'syntax-table '(12)))))
  ;; Choose the face to use.
  (lisp-font-lock-syntactic-face-function state))

(defvar calculate-lisp-indent-last-sexp)

;; FIXME this duplicates almost all of lisp-indent-function.
;; Extract common code to a subroutine.
(defun extempore-indent-function (indent-point state)
  "Extempore mode function for the value of the variable `lisp-indent-function'.
This behaves like the function `lisp-indent-function', except that:

i) it checks for a non-nil value of the property `extempore-indent-function'
\(or the deprecated `extempore-indent-hook'), rather than `lisp-indent-function'.

ii) if that property specifies a function, it is called with three
arguments (not two), the third argument being the default (i.e., current)
indentation."
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
      (let ((function (buffer-substring (point)
					(progn (forward-sexp 1) (point))))
	    method)
	(setq method (or (get (intern-soft function) 'extempore-indent-function)
			 (get (intern-soft function) 'extempore-indent-hook)))
	(cond ((or (eq method 'defun)
		   (and (null method)
			(> (length function) 3)
			(string-match "\\`def" function)))
	       (lisp-indent-defform state indent-point))
	      ((integerp method)
	       (lisp-indent-specform method state
				     indent-point normal-indent))
	      (method
               (funcall method state indent-point normal-indent)))))))


;;; 'let' is different in Scheme/xtlang

(defun would-be-symbol (string)
  (not (string-equal (substring string 0 1) "(")))

(defun next-sexp-as-string ()
  ;; Assumes that it is protected by a save-excursion
  (forward-sexp 1)
  (let ((the-end (point)))
    (backward-sexp 1)
    (buffer-substring (point) the-end)))

;; This is correct but too slow.
;; The one below works almost always.
;;(defun extempore-let-indent (state indent-point)
;;  (if (would-be-symbol (next-sexp-as-string))
;;      (extempore-indent-specform 2 state indent-point)
;;      (extempore-indent-specform 1 state indent-point)))

(defun extempore-let-indent (state indent-point normal-indent)
  (skip-chars-forward " \t")
  (if (looking-at "[-a-zA-Z0-9+*/?!@$%^&_:~]")
      (lisp-indent-specform 2 state indent-point normal-indent)
    (lisp-indent-specform 1 state indent-point normal-indent)))

;; (put 'begin 'extempore-indent-function 0), say, causes begin to be indented
;; like defun if the first form is placed on the next line, otherwise
;; it is indented like any other form (i.e. forms line up under first).

(put 'begin 'extempore-indent-function 0)
(put 'case 'extempore-indent-function 1)
(put 'delay 'extempore-indent-function 0)
(put 'dotimes 'extempore-indent-function 1)
(put 'lambda 'extempore-indent-function 1)
(put 'memzone 'extempore-indent-function 1)
(put 'bind-func 'extempore-indent-function 'defun)
(put 'let 'extempore-indent-function 'extempore-let-indent)
(put 'letz 'extempore-indent-function 'extempore-let-indent)
(put 'let* 'extempore-indent-function 'extempore-let-indent)
(put 'letrec 'extempore-indent-function 'extempore-let-indent)
;; (put 'let-values 'extempore-indent-function 1) ; SRFI 11
;; (put 'let*-values 'extempore-indent-function 1) ; SRFI 11
;; (put 'sequence 'extempore-indent-function 0) ; SICP, not r4rs
(put 'let-syntax 'extempore-indent-function 1)
(put 'letrec-syntax 'extempore-indent-function 1)
(put 'syntax-rules 'extempore-indent-function 1)
(put 'syntax-case 'extempore-indent-function 2) ; not r5rs

(put 'call-with-input-file 'extempore-indent-function 1)
(put 'with-input-from-file 'extempore-indent-function 1)
(put 'with-input-from-port 'extempore-indent-function 1)
(put 'call-with-output-file 'extempore-indent-function 1)
(put 'with-output-to-file 'extempore-indent-function 1)
(put 'with-output-to-port 'extempore-indent-function 1)
(put 'call-with-values 'extempore-indent-function 1) ; r5rs?
(put 'dynamic-wind 'extempore-indent-function 3) ; r5rs?


;;; SLIP escape codes
;; END       ?\300    /* indicates end of packet */
;; ESC       ?\333    /* indicates byte stuffing */
;; ESC_END   ?\334    /* ESC ESC_END means END data byte */
;; ESC_ESC   ?\335    /* ESC ESC_ESC means ESC data byte */

(defun extempore-slip-process-filter (proc str)
  (message (extempore-slip-unescape-string str)))

;; connection management

(make-variable-buffer-local 'mode-line-process)
(setq mode-line-process nil)
(make-variable-buffer-local 'extempore-connection-list)
(setq extempore-connection-list nil)

(defun extempore-update-mode-line ()
  (let ((nprocs (length extempore-connection-list))
        (gethostportstr ))
    (setq mode-line-process
          (if (< nprocs 1)
              ""
            (mapconcat
             'identity
             (mapcar (lambda (proc)
                       (let ((host (process-contact proc :host)))
                         (concat " "
                                 (if (string= host "localhost") "" (concat host ":"))
                                 (number-to-string (process-contact proc :service)))))
                     extempore-connection-list)
             "")))))

(defun extempore-sync-connections ()
  (interactive)
  (dolist (proc extempore-connection-list)
    (let ((res (process-status proc)))
      (unless (member res '(run open))
        (setq extempore-connection-list
                     (delete proc extempore-connection-list))
        (delete-process proc))))
  (extempore-update-mode-line))

(defun extempore-get-connection (host port)
  (cl-find-if (lambda (proc)
		(and (string= host (process-contact proc :host))
		     (= port (process-contact proc :service))))
	      extempore-connection-list))

(defun extempore-new-connection (host port)
  (if (extempore-get-connection host port)
      (message "Already connected to %s on port %d" host port)
      (let ((proc (open-network-stream "extempore" nil host port)))
        (if proc
            (progn
              (set-process-coding-system proc 'iso-latin-1 'iso-latin-1)
              (set-process-filter proc #'extempore-minibuffer-echo-filter)
              (add-to-list 'extempore-connection-list proc t)
              (extempore-update-mode-line))))))

(defun extempore-minibuffer-echo-filter (proc str)
  (message (substring str 0 -1)))

(defun extempore-repl-preoutput-filter (string)
  (concat "=> " (substring string 0 -1) "\nextempore> "))

(defun extempore-disconnect (host port)
  "Terminate a specific connection to an Extempore process"
  (interactive
   (if extempore-connection-list
       (let ((read-host (ido-completing-read
                         "Hostname: " (cl-remove-duplicates
                                       (mapcar (lambda (proc)
                                                 (process-contact proc :host))
                                               extempore-connection-list)
                                       :test 'string=)
                         nil nil nil nil (process-contact (car extempore-connection-list) :host)))
             (read-port (string-to-number
                         (ido-completing-read
                          "Port: " (cl-remove-duplicates
                                    (mapcar (lambda (proc)
                                              (number-to-string
                                               (process-contact proc :service)))
                                            extempore-connection-list)
                                    :test 'string=)
                          nil nil nil nil (number-to-string (process-contact (car extempore-connection-list) :service))))))
         (list read-host read-port))
     (list nil nil)))
  (let ((proc (extempore-get-connection host port)))
    (if proc
        (progn (delete-process proc)
               (extempore-sync-connections))
      (message "No current connections to %s on port %d" host port))))

(defun extempore-disconnect-all ()
  "Terminate all connections (for this buffer)"
  (interactive)
  (dolist (proc extempore-connection-list)
    (delete-process proc))
  (setq extempore-connection-list nil)
  (extempore-update-mode-line))

(defun extempore-connect (host port)
  "Connect to an Extempore process running on HOST and PORT."
  (interactive
   ;; get args interactively
   (list (ido-completing-read
         "Hostname: " (list extempore-default-host) nil nil nil nil extempore-default-host)
         (string-to-number
          (ido-completing-read
           "Port: " '("7099" "7098") nil nil nil nil (number-to-string extempore-default-port)))))
  (extempore-sync-connections)
  (extempore-new-connection host port))

;;; SLIP escape codes
;; END       ?\300    /* indicates end of packet */
;; ESC       ?\333    /* indicates byte stuffing */
;; ESC_END   ?\334    /* ESC ESC_END means END data byte */
;; ESC_ESC   ?\335    /* ESC ESC_ESC means ESC data byte */

(defvar extempore-use-slip-tcp-connection nil)
(defvar extempore-slip-end-string (char-to-string ?\300))
(defvar extempore-slip-esc-string (char-to-string ?\333))
(defvar extempore-slip-esc-end-string (char-to-string ?\334))
(defvar extempore-slip-esc-esc-string (char-to-string ?\335))
(defvar extempore-slip-escaping-regexp
  (concat "[" extempore-slip-esc-string extempore-slip-end-string "]"))
(defvar extempore-slip-unescaping-regexp (concat extempore-slip-esc-string "."))

(defun extempore-slip-escape-string (str)
  (concat
   extempore-slip-end-string
   (replace-regexp-in-string extempore-slip-escaping-regexp
                             (lambda (s)
                               (if (string-equal s extempore-slip-end-string)
                                   (concat extempore-slip-esc-string
                                           extempore-slip-esc-end-string)
                                 (concat extempore-slip-esc-string
                                         extempore-slip-esc-esc-string)))
                             str)
   extempore-slip-end-string))

(defun extempore-slip-unescape-string (str)
  (if (and (string-equal (substring str 0 1)
                         extempore-slip-end-string)
           (string-equal (substring str -1)
                         extempore-slip-end-string))
      (replace-regexp-in-string extempore-slip-unescaping-regexp
                                (lambda (s)
                                  (if (string-equal (substring s 1)
                                                    extempore-slip-esc-end-string)
                                      extempore-slip-end-string
                                    extempore-slip-esc-string))
                                (substring str 1 -1))
    (progn (message "Dropping malformed SLIP packet.")
           nil)))

;; OSC (strings only at the moment)

(defun extempore-make-osc-string (str)
  (concat str (make-vector (- 4 (mod (length str) 4)) ?\0)))

(defun extempore-extract-osc-string (str &optional start)
  (and (string-match "[^\0]*" str start)
       (match-string 0 str)))

(defun extempore-extract-osc-address (str)
  (extempore-extract-osc-string str 0))

(defun extempore-extract-osc-type-tag (str)
  (and (string-match ",[^\0]*" str)
       (substring (match-string 0 str) 1)))

(defun extempore-osc-args-index (str)
  (and (string-match ",[^\0]*[\0]*" str)
       (match-end 0)))

;; correct escaping of eval strings

(defun extempore-make-crlf-evalstr (evalstr)
  (concat evalstr "\r\n"))

(defun extempore-make-osc-evalstr (evalstr)
  (concat (extempore-make-osc-string "/caas/eval")
          (extempore-make-osc-string ",s")
          (extempore-make-osc-string evalstr)))

(defun extempore-make-slip-evalstr (evalstr)
  (extempore-slip-escape-string evalstr))

(defun extempore-make-slip-osc-evalstr (evalstr)
  (extempore-slip-escape-string (extempore-make-osc-evalstr evalstr)))

;; sending code to the Extempore compiler

;; from http://emacswiki.org/emacs/ElispCookbook
(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'" str)
    (setq str (replace-match "" t t str)))
  str)

;; 'blinking' defuns as they are evaluated

(defvar extempore-blink-duration 0.3)

(defun extempore-make-blink-overlay (face-sym)
  (let ((overlay (make-overlay 0 0)))
    (overlay-put overlay 'face face-sym)
    overlay))

;; overlay for highlighting currently evaluated region or line
(setq extempore-blink-overlay
      (extempore-make-blink-overlay 'extempore-blink-face))
;; slave buffer version
(setq extempore-sb-blink-overlay
      (extempore-make-blink-overlay 'extempore-sb-blink-face))

;; for blinking evals in slave buffers (see `extempore-sb-mode')
(make-variable-buffer-local 'extempore-sb-eval-markers)
(setq extempore-sb-eval-markers nil)

(defun extempore-blink-region (overlay start end &optional buf)
  (move-overlay overlay start end buf)
  (redisplay)
  (run-with-timer extempore-blink-duration
                  nil
                  (lambda (overlay) (delete-overlay overlay))
                  overlay))

;; sending definitions (code) from the Emacs buffer

;;;;;;;;;;;;;;;;;;;;;;;;
;; inferior extempore ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; (heavily) based cmuscheme.el by Olin Shivers, Extempore conversion
;; work by Ben Swift

(require 'comint)

(defvar extempore-buffer)

(make-variable-buffer-local 'comint-preoutput-filter-functions)

(define-derived-mode inferior-extempore-mode comint-mode "Inferior Extempore"
  "Major mode for running an inferior Extempore process.

A Extempore process can be fired up with M-x extempore-run.

You can send text to the inferior Extempore process from other buffers containing
Extempore source.
    switch-to-extempore switches the current buffer to the Extempore process buffer.
    extempore-send-definition sends the current definition to the Extempore process.
    extempore-compile-definition compiles the current definition.
    extempore-send-region sends the current region to the Extempore process.
    extempore-compile-region compiles the current region.

    extempore-send-definition-and-go, extempore-compile-definition-and-go,
        extempore-send-region-and-go, and extempore-compile-region-and-go
        switch to the Extempore process buffer after sending their text.
For information on running multiple processes in multiple buffers, see
documentation for variable extempore-buffer.

Commands:
Return after the end of the process' output sends the text from the
    end of process to point.
Return before the end of the process' output copies the sexp ending at point
    to the end of the process' output, and sends it.
Delete converts tabs to spaces as it moves back.
Tab indents for Extempore; with argument, shifts rest
    of expression rigidly with the current line.
C-M-q does Tab on each line starting within following expression.
Paragraphs are separated only by blank lines.  Semicolons start comments.
If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it."
  ;; Customize in inferior-extempore-mode-hook
  (setq comint-preoutput-filter-functions nil)
  (setq mode-line-process '(":%s")))

(defvar extempore-repl-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "<return>") 'extempore-repl-return)
    m))

(define-derived-mode extempore-repl-mode comint-mode "Extempore REPL"
  "Major mode for running a REPL connected to an existing Extempore process."
  (setq comint-use-prompt-regexp t)
  (setq comint-prompt-regexp "extempore> ")
  (setq comint-input-sender (function extempore-repl-send))
  (setq comint-preoutput-filter-functions (list (function extempore-repl-preoutput-filter)))
  ;; (extempore-mode-variables)
  (setq mode-line-process '(":%s"))
  (setq comint-get-old-input (function extempore-get-old-input)))

(defun extempore-repl-send (proc string)
  (comint-simple-send proc (concat string "\r")))

(defun extempore-repl-return ()
  "Only send current input if it is a syntactically correct s-expression, otherwise newline-and-indent."
  (interactive)
  (let ((edit-pos (point)))
    (goto-char (process-mark (get-buffer-process (current-buffer))))
    (let ((sexp-bounds (bounds-of-thing-at-point 'sexp)))
      (if sexp-bounds
          (progn (set-mark (car sexp-bounds))
                 (goto-char (cdr sexp-bounds))
                 (comint-send-input))
        (progn (goto-char edit-pos)
               (newline-and-indent))))))

(defun extempore-get-old-input ()
  "Snarf the sexp ending at point."
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (buffer-substring (point) end))))

;;;###autoload
(defun extempore-run (program-args)
  "Run an inferior Extempore process, input and output via buffer `*extempore*'.
If there is a process already running in `*extempore*', switch to that buffer.

\(Type \\[describe-mode] in the process buffer for a list of commands.)"

  (interactive (list (read-string "Run Extempore: extempore " extempore-program-args)))
  (unless user-extempore-directory
    (error "Error: `user-extempore-directory' not set!\n\nNote that this var used to be called `extempore-path', so you may need to update your .emacs"))
  (if (not (comint-check-proc "*extempore*"))
      (let ((default-directory user-extempore-directory))
        (set-buffer (apply #'make-comint "extempore" (concat user-extempore-directory "extempore") nil
                           (split-string-and-unquote program-args)))
        (inferior-extempore-mode)))
  (setq extempore-buffer "*extempore*")
  (pop-to-buffer "*extempore*"))

(defun extempore-start-repl (host port)
  (interactive
   (list (ido-completing-read
          "Hostname: " (list extempore-default-host) nil nil nil nil extempore-default-host)
         (string-to-number
          (ido-completing-read
           "Port: " '("7099" "7098") nil nil nil nil (number-to-string extempore-default-port)))))
  "Start an Extempore REPL connected to HOST on PORT."
  (if (comint-check-proc "*extempore*")
      (progn (set-buffer (make-comint "extempore-repl" (cons host port)))
             (extempore-repl-mode)
             (pop-to-buffer "*extempore-repl*"))
    (message "No *extempore* buffer detected, you can set one up with M-x extempore-run")))

(defun extempore-send-region (start end)
  "Send the current region to the inferior Extempore process."
  (interactive "r")
  (if extempore-connection-list
      (let ((transient-mark-mode nil))
        (extempore-blink-region extempore-blink-overlay start end)
        (dolist (proc extempore-connection-list)
          (process-send-string
           proc
           (concat (buffer-substring-no-properties start end) "\r\n")))
        (sleep-for extempore-blink-duration))
    (message "%s is not connected to an Extempore process.  You can connect with `M-x extempore-connect' (C-x C-j)" (buffer-name))))

(defun extempore-send-definition ()
  "Send the current definition to the inferior Extempore process."
  (interactive)
  (save-excursion
   (end-of-defun)
   (let ((end (point)))
     (beginning-of-defun)
     (extempore-send-region (point) end))))

(defun extempore-send-buffer ()
  "Send the current buffer to the inferior Extempore process"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^(" nil t)
      (extempore-send-definition)
      (redisplay))))

(defun extempore-send-last-sexp ()
  "Send the previous sexp to the inferior Extempore process."
  (interactive)
  (extempore-send-region (save-excursion (backward-sexp) (point)) (point)))

(defun switch-to-extempore (eob-p)
  "Switch to the extempore process buffer.
With argument, position cursor at end of buffer."
  (interactive "P")
  (if (or (and extempore-buffer (comint-check-proc extempore-buffer))
          (extempore-interactively-start-process))
      (pop-to-buffer extempore-buffer)
    (error "No current process buffer.  See variable `extempore-buffer'"))
  (when eob-p
    (push-mark)
    (goto-char (point-max))))

(defun extempore-send-region-and-go (start end)
  "Send the current region to the inferior Extempore process.
Then switch to the process buffer."
  (interactive "r")
  (extempore-send-region start end)
  (switch-to-extempore t))

(defun extempore-send-definition-and-go ()
  "Send the current definition to the inferior Extempore.
Then switch to the process buffer."
  (interactive)
  (extempore-send-definition)
  (switch-to-extempore t))

(defun extempore-send-buffer-and-go ()
  "Send the current buffer to the inferior Extempore.
Then switch to the process buffer."
  (interactive)
  (extempore-send-buffer)
  (switch-to-extempore t))

(defvar extempore-prev-l/c-dir/file nil
  "Caches the last (directory . file) pair.
Caches the last pair used in the last `extempore-load-file' or
`extempore-compile-file' command.  Used for determining the default
in the next one.")

(defun extempore-load-file (file-name)
  "Load an Extempore (.xtm) file FILE-NAME into the inferior Extempore process."
  (interactive (comint-get-source "Load .xtm file: " extempore-prev-l/c-dir/file
				  '(extempore-mode) t))
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq extempore-prev-l/c-dir/file (cons (file-name-directory    file-name)
				       (file-name-nondirectory file-name)))
  (comint-send-string (extempore-proc) (concat "(sys:load \"" file-name "\"\)\n")))


(defvar extempore-buffer nil "*The current extempore process buffer.

MULTIPLE PROCESS SUPPORT
===========================================================================
extempore.el supports, in a fairly simple fashion, running multiple Extempore
processes.  To run multiple Extempore processes, you start the first up with
\\[extempore-run].  It will be in a buffer named *extempore*.  Rename this buffer
with \\[rename-buffer].  You may now start up a new process with another
\\[extempore-run].  It will be in a new buffer, named *extempore*.  You can
switch between the different process buffers with \\[switch-to-buffer].

Commands that send text from source buffers to Extempore processes --
like `extempore-send-definition' or `extempore-compile-region' -- have to choose a
process to send to, when you have more than one Extempore process around.  This
is determined by the global variable `extempore-buffer'.  Suppose you
have three inferior Extempores running:
    Buffer	Process
    foo		extempore
    bar		extempore<2>
    *extempore*    extempore<3>
If you do a \\[extempore-send-definition-and-go] command on some Extempore source
code, what process do you send it to?

- If you're in a process buffer (foo, bar, or *extempore*),
  you send it to that process.
- If you're in some other buffer (e.g., a source file), you
  send it to the process attached to buffer `extempore-buffer'.
This process selection is performed by function `extempore-proc'.

Whenever \\[extempore-run] fires up a new process, it resets `extempore-buffer'
to be the new process's buffer.  If you only run one process, this will
do the right thing.  If you run multiple processes, you can change
`extempore-buffer' to another process buffer with \\[set-variable].

More sophisticated approaches are, of course, possible.  If you find yourself
needing to switch back and forth between multiple processes frequently,
you may wish to consider ilisp.el, a larger, more sophisticated package
for running inferior Lisp and Extempore processes.  The approach taken here is
for a minimal, simple implementation.  Feel free to extend it.")

(defun extempore-proc ()
  "Return the current Extempore process, starting one if necessary.
See variable `extempore-buffer'."
  (unless (and extempore-buffer
               (get-buffer extempore-buffer)
               (comint-check-proc extempore-buffer))
    (extempore-interactively-start-process))
  (or (extempore-get-process)
      (error "No current process.  See variable `extempore-buffer'")))

(defun extempore-get-process ()
  "Return the current Extempore process or nil if none is running."
  (get-buffer-process (if (eq major-mode 'inferior-extempore-mode)
                          (current-buffer)
                        extempore-buffer)))

(defun extempore-interactively-start-process (&optional _cmd)
  "Start an inferior Extempore process.  Return the process started.
Since this command is run implicitly, always ask the user for the
command to run."
  (save-window-excursion
    (extempore-run (read-string "Run Extempore: extempore " extempore-program-args))))

;;;;;;;;;;;
;; eldoc ;;
;;;;;;;;;;;

(defcustom extempore-eldoc-active t
  "If non-nil, attempt to display live argument lists for the
  function under point."
  :type 'boolean
  :group 'extempore)

(defun extempore-fnsym-in-current-sexp ()
  (save-excursion
    (let ((argument-index (1- (eldoc-beginning-of-sexp))))
      ;; If we are at the beginning of function name, this will be -1.
      (when (< argument-index 0)
	(setq argument-index 0))
      ;; Don't do anything if current word is inside a string.
      (if (= (or (char-after (1- (point))) 0) ?\") ;" (to stop ST2's string highlighting stuffing up)
	  nil
	(current-word)))))

(make-variable-buffer-local 'eldoc-documentation-function)

;; currently doesn't actually return the symbol, but sends the request
;; which is echoed back through whichever process filter is active
(defun extempore-eldoc-documentation-function ()
  (if (and extempore-connection-list extempore-eldoc-active)
      (let ((fnsym (extempore-fnsym-in-current-sexp)))
        ;; send the documentation request
        (if extempore-connection-list
            (process-send-string (car extempore-connection-list)
                                 (format  "(get-eldoc-string %s)\r\n" fnsym)))
        ;; always return nil; docstring comes back through the process
        ;; filter
        nil)))

(add-hook 'extempore-mode-hook
          '(lambda ()
             (turn-on-eldoc-mode)
             (setq eldoc-documentation-function
                   'extempore-eldoc-documentation-function)))

;; misc bits and pieces

(defun xpb1 (name duration)
  (interactive "sName: \nsDuration: ")
  (insert (concat "(define " name
		  "\n  (lambda (beat dur)\n    "
		  "(callback (*metro* (+ beat (* .5 " duration "))) '"
		  name " (+ beat " duration ") " duration ")))\n\n"
		  "(" name " (*metro* 'get-beat 4) " duration ")")))

;; for greek symbol lambdas: from emacs-starter-kit

(if extempore-use-pretty-lambdas
    (font-lock-add-keywords
     nil `(("(?\\(lambda\\>\\)"
	    (0 (progn (compose-region (match-beginning 1) (match-end 1)
				      ,(make-char 'greek-iso8859-7 107))
		      nil))))))

;; useful for converting C header files to xtlang headers
(defun hex-to-decimal-at-point ()
  (interactive)
  (let ((hex-str (word-at-point)))
    (if hex-str
	(progn (kill-word 1)
	       (insert (number-to-string (string-to-number hex-str 16)))))))

(defun note-to-midi-at-point ()
  (interactive)
  (let ((note-str (looking-at "\\([a-gA-G]\\)\\(#\\|b\\)?\\([0-9]\\)")))
    (if note-str
        (let* ((data (match-data))
               (pc (case (mod (- (mod (string-to-char (buffer-substring
                                                       (nth 2 data)
                                                       (nth 3 data)))
                                      16) 3) 7)
                     ((0) 0) ((1) 2) ((2) 4) ((3) 5) ((4) 7) ((5) 9) ((6) 11)))
               (offset (+ 12 (* (string-to-number (buffer-substring (nth 6 data)
                                                                  (nth 7 data)))
                                12)))
               (sharp-flat (and (nth 4 data)
                                (buffer-substring (nth 4 data)
                                                  (nth 5 data)))))
          (replace-match (number-to-string
                          (+ offset pc
                             (if sharp-flat
                                 (if (string= sharp-flat "#") 1 -1)
                               0))))))))

;; interactive repeated evaluation of defun under point

(defvar extempore-repeated-eval-timer nil)

(defun extempore-start-repeated-eval (time-interval)
  "takes a time interval (in seconds)"
  (interactive "nTime interval (sec):")
  (setq extempore-repeated-eval-timer
	(run-with-timer 0 time-interval 'extempore-send-definition)))

(defun extempore-stop-repeated-eval ()
  (interactive)
  (cancel-timer extempore-repeated-eval-timer)
  (setq extempore-repeated-eval-timer nil))

;; processing compiler output for .xtmh files

(defun extempore-process-compiler-output (libname)
  (interactive "slibname: ")
  (unless (region-active-p)
    (error "You need to highlight the compiler output you want to process"))
  (let ((compiler-output (buffer-substring-no-properties (point) (mark)))
        (case-fold-search nil))
    (with-temp-buffer
      ;; bind-val
      (insert compiler-output)
      (goto-char (point-min))
      (while (search-forward-regexp "^Bound \\(.*\\) >>> \\(.*\\)$" nil t)
        (replace-match (concat "(bind-lib-val " libname " \\1 \\2)") t))
      ;; bind-func
      (goto-char (point-min))
      (while (search-forward-regexp "^Compiled \\(.*\\) >>> \\(.*\\)$" nil t)
        (replace-match (concat "(bind-lib-func " libname " \\1 \\2)") t))
      ;; bind-alias (and replace aliases in output)
      (goto-char (point-min))
      (while (search-forward-regexp "^Aliased \\(.*\\) >>> \\(.*\\)$" nil t)
        (let ((alias (match-string 1))
              (value (match-string 2)))
          (replace-match (concat "(bind-alias \\1 \\2)") t)
          (save-excursion
            (while (search-forward-regexp (format "\\<%s\\>" alias) nil t)
              (replace-match value t)))))
      (kill-region (point-min)
                   (point-max))
      (message "Processed output copied to kill ring."))))

;;;;;;;;;;;;;;;;
;; animations ;;
;;;;;;;;;;;;;;;;

(define-minor-mode extempore-tr-animation-mode
  "This minor mode automatically logs all keystrokes (and
  extempore code sent for evaluation) in all Extempore buffers."
  :global t
  :init-value nil
  :lighter " ExAnim"
  :keymap nil
  :group 'extempore

  (if extempore-tr-animation-mode
      (extempore-start-tr-animation)
    (extempore-stop-tr-animation)))

(defun extempore-beginning-of-defun-function (&optional arg)
  (beginning-of-defun arg))

(defun extempore-end-of-defun-function (&optional arg)
  (end-of-defun arg))

;; these could all be made more elegant using (sexp-at-point) to read
;; in the actual s-expressions, but this is probably a bit quicker

(defun extempore-scheme-defun-name ()
  (save-excursion
    (looking-at "(\\(define-\\(\\|macro\\|instrument\\|sampler\\)\\)\\s-+\\([^ \t\n:]+\\)")
    (match-string 3)))

(defun extempore-inside-scheme-defun-p ()
  (save-excursion
    (extempore-beginning-of-defun-function)
    (extempore-scheme-defun-name)))

(defun extempore-xtlang-defun-name ()
  (save-excursion
    (looking-at "(\\(bind-\\(func\\|val\\|type\\|alias\\|poly\\|lib\\)\\)\\s-+\\([^ \t\n:]+\\)")
    (match-string 3)))

(defun extempore-inside-xtlang-defun-p ()
  (save-excursion
    (extempore-beginning-of-defun-function)
    (extempore-xtlang-defun-name)))

(defun extempore-inside-tr-defun-p ()
  (save-excursion
    (extempore-end-of-defun-function)
    (search-backward ")" nil t 2)
    (looking-at "(callback")))

(defun extempore-find-defn-bounds (name)
  "Find the definition of the function `name'."
  (save-excursion
    (goto-char (point-max))
    (if (re-search-backward
         (concat "(\\(\\(bind-func\\)\\|\\(define\\)\\)\\s-+" name "[ \t\n:]")
         nil t)
        (cons (match-beginning 0) (1- (match-end 0)))
      nil)))

;; flash overlay

(defun extempore-make-tr-flash-overlay (name bounds)
  (if bounds
      (let ((overlay (make-overlay (car bounds)
                                   (cdr bounds)
                                   nil t nil)))
        ;; (overlay-put overlay 'face '(:inverse-video t))
        (overlay-put overlay 'evaporate t)
        (overlay-put overlay 'priority 2)
        overlay)))

(defun extempore-update-tr-flash-overlay (overlay flag)
  (if flag
      (overlay-put overlay 'face '(:inverse-video t))
    (overlay-put overlay 'face '(:inverse-video nil)))
  nil)

;; clock overlay

(defun extempore-make-tr-clock-overlay (name bounds)
  (if bounds
      (let* ((defun-start (car bounds))
             (overlay (make-overlay defun-start
                                    (1+ defun-start)
                                    nil t nil)))
        (overlay-put overlay 'face '(:underline t :overline t))
        (overlay-put overlay 'evaporate t)
        (overlay-put overlay 'priority 1)
        overlay)))

(defun extempore-update-tr-clock-overlay (overlay val beg end)
  (move-overlay overlay
                beg
                (min end (max (1+ beg) (round (+ beg (* val (- end beg))))))))

;; tetris-overlay

(defvar extempore-tetris-anim-str "*")

(defun extempore-make-tr-tetris-overlay (name bounds)
  (if bounds
      (let* ((tetris-lh-point (cdr bounds))
             (tetris-rh-point fill-column)
             (overlay (make-overlay defun-start
                                    (1+ defun-start)
                                    nil t nil)))
        (overlay-put overlay 'face '(:underline t :overline t))
        (overlay-put overlay 'evaporate t)
        overlay)))

(defun extempore-update-tr-tetris-overlay (overlay val beg end)
  (move-overlay overlay
                beg
		(min end (max (1+ beg) (round (+ beg (* val (- end beg))))))))


(defvar extempore-tr-anim-alist nil
  "List of TR animations.

Each element is a list, with a name as the first element, and then a list
of vectors as the cdr:

  (fn-name [flash-overlay clock-overlay delta-t time-to-live flash-frames-to-live] ...)

You shouldn't have to modify this list directly, use
`extempore-add-new-anim-to-name' and
`extempore-delete-tr-anim' instead.")

(defun extempore-delete-tr-anim (name)
  (cl-delete-if (lambda (x) (string= name (car x)))
                extempore-tr-anim-alist))

(defun extempore-create-anim-vector (delta-t)
  (vector (extempore-make-tr-clock-overlay name bounds)
          (extempore-make-tr-flash-overlay name bounds)
          delta-t    ; total time
          delta-t  ; time-to-live
          0))        ; flash-frames to live

(defun extempore-add-new-anim-to-name (name delta-t)
  (let ((bounds (extempore-find-defn-bounds name))
        (anim-list (extempore-get-tr-anims-for-name name)))
    (if bounds
        (if anim-list
            (setcdr anim-list
                    (cons (extempore-create-anim-vector delta-t)
                          (cdr anim-list)))
          (add-to-list 'extempore-tr-anim-alist
                       (list name (extempore-create-anim-vector delta-t)))))))

(defun extempore-get-tr-anims-for-name (name)
  (assoc name extempore-tr-anim-alist))

(defun extempore-get-active-tr-anims (anim-list)
  (cl-remove-if-not (lambda (x) (aref x 3)) anim-list))

(defun extempore-get-dormant-tr-anims (anim-list)
  (cl-remove-if (lambda (x) (aref x 3)) anim-list))

(defun extempore-reactivate-tr-anim (anim delta-t)
  (aset anim 2 delta-t)
  (aset anim 3 delta-t))

(defun extempore-trigger-tr-anim (name delta-t)
  (let ((anim-list (extempore-get-tr-anims-for-name name)))
    (if anim-list
        (let ((dormant-anims (extempore-get-dormant-tr-anims anim-list)))
          (if dormant-anims
              (extempore-reactivate-tr-anim (car dormant-anims) delta-t)
            (extempore-add-new-anim-to-name name delta-t)))
      (extempore-add-new-anim-to-name name delta-t))))

;; animate the overlays

(defvar extempore-tr-animation-update-period (/ 1.0 20))

(defun extempore-tr-update-anims ()
  (dolist (anim (apply #'append (mapcar #'cdr extempore-tr-anim-alist)))
    (let ((ttl (aref anim 3))
	  (ftl (aref anim 4))
	  (flash-overlay (aref anim 1)))
      ;; update ttl value
      (if (numberp ttl)
	  (setq ttl (aset anim 3 (- ttl extempore-tr-animation-update-period))))
      ;; finish 'flash' animation
      (if (> ftl 0)
	  (progn (if (= ftl 1)
		     (progn (extempore-update-tr-flash-overlay flash-overlay nil)
			    (extempore-update-tr-clock-overlay (aref anim 0)
						 0.0
						 (overlay-start flash-overlay)
						 (overlay-end flash-overlay))
			    (aset anim 3 nil)))
		 (aset anim 4 (- ftl 1)))
	(if (numberp ttl)
	    ;; trigger 'flash' animation
	    (if (<= ttl 0)
		(progn
		  ;; num of frames the flash overlay lives for
		  (aset anim 4 2)
		  (extempore-update-tr-flash-overlay flash-overlay t))
	      ;; update the 'clock' overlay
	      (extempore-update-tr-clock-overlay (aref anim 0)
						 (/ (- (aref anim 2)
						       (aref anim 3))
						    (aref anim 2))
						 (overlay-start flash-overlay)
						 (overlay-end flash-overlay))
	      ))))))

;; managing the animation timer

(defvar extempore-tr-animation-timer nil)

(defun extempore-stop-tr-animation-timer ()
  (message "Cancelling TR animiation timer.")
  (if extempore-tr-animation-timer
      (cancel-timer extempore-tr-animation-timer))
  (remove-overlays)
  (setq extempore-tr-animation-timer nil
	extempore-tr-anim-alist nil))

(defun extempore-start-tr-animation-timer ()
  (if extempore-tr-animation-timer
      (progn (extempore-stop-tr-animation-timer)
	     (message "Restarting TR animation timer."))
    (message "Starting TR animation timer."))
  (setq extempore-tr-animation-timer
	(run-with-timer 0
			extempore-tr-animation-update-period
			'extempore-tr-update-anims)))

;;  UDP server for recieving animation triggers

(defvar extempore-tr-anim-udp-server nil)

(defcustom extempore-tr-anim-udp-server-port 7097
  "Port for the the extempore TR-animation UDP server."
  :type 'integer
  :group 'extempore)

(defun extempore-tr-animation-filter (proc str)
  (if (string= (extempore-extract-osc-address str) "/anim")
      (eval (read (substring str (extempore-osc-args-index str))))))

(defun extempore-create-tr-anim-server (port)
  (make-network-process
   :name "tr-anim-server"
   ;; :buffer (current-buffer)
   :coding 'binary
   :service port
   :type 'datagram
   :family 'ipv4
   :server t
   :filter #'extempore-tr-animation-filter))

(defun extempore-stop-tr-anim-osc-server ()
  (if extempore-tr-anim-udp-server
      (progn (delete-process extempore-tr-anim-udp-server)
             (setq extempore-tr-anim-udp-server nil)
             (message "Deleting UDP listener."))))

(defun extempore-start-tr-anim-osc-server ()
  (extempore-stop-tr-anim-osc-server)
  (progn (setq extempore-tr-anim-udp-server
               (extempore-create-tr-anim-server
                extempore-tr-anim-udp-server-port))
         (message "Starting UDP listener for animation messages.")))

;; high-level control of TR animations: these are the functions that
;; the programmer should use to turn things on/off

(defun extempore-start-tr-animation ()
  (extempore-sync-connections)
  (if extempore-connection-list
      (progn (extempore-start-tr-animation-timer)
             (extempore-start-tr-anim-osc-server))
    (message "Can't start TR animations: this buffer has no active connections to an Extempore process.")))

(defun extempore-stop-tr-animation ()
  (extempore-stop-tr-animation-timer)
  (extempore-stop-tr-anim-osc-server))

;;;;;;;;;;;;;;;;;;;;;;
;; extempore logger ;;
;;;;;;;;;;;;;;;;;;;;;;

(define-minor-mode extempore-logger-mode
  "This minor mode automatically logs all keystrokes (and
  extempore code sent for evaluation) in all Extempore buffers."
  :global t
  :init-value nil
  :lighter " ExLog"
  :keymap nil
  :group 'extempore

  (if extempore-logger-mode
      (extempore-logger-start-logging)
    (extempore-logger-stop-logging)))

(defun extempore-logger-start-logging ()
  (add-hook 'pre-command-hook 'extempore-logger-pre-command-hook)
  (unless extempore-logger-logfile
    (setq extempore-logger-logfile
          (extempore-logger-new-logfile)))
  (call-interactively 'extempore-logger-add-comment)
  (extempore-logger-advise-functions extempore-logger-special-functions)
  ;; (extempore-logger-start-idle-write-timer)
  )

(defun extempore-logger-stop-logging ()
  (remove-hook 'pre-command-hook 'extempore-logger-pre-command-hook)
  (call-interactively 'extempore-logger-add-comment)
  (extempore-logger-finish-logfile)
  (setq extempore-logger-logfile nil)
  (extempore-logger-unadvise-functions extempore-logger-special-functions)
  ;; (extempore-logger-stop-idle-write-timer)
  )

(defun extempore-logger-new-session ()
  (interactive)
  (if extempore-logger-logfile
      (call-interactively 'extempore-logger-stop-logging))
  (extempore-logger-start-logging))

;; advise funcitions for logging

(defun extempore-logger-advise-functions (func-list)
  "Advise (via defadvice) the key extempore-mode functions"
  (mapc (lambda (function)
          (ad-add-advice
           function
	   '(extempore-logger-advice nil t (advice . (lambda () (extempore-logger-log-current-command real-this-command current-prefix-arg (ad-get-args 0)))))
           'after 'first)
          (ad-activate function))
        func-list))

(defun extempore-logger-unadvise-functions (func-list)
  "Remove advice from special extempore-mode functions"
  (mapc (lambda (function)
          (ad-remove-advice function 'after 'extempore-logger-advice))
        func-list))

(defvar extempore-logger-special-functions
  '(extempore-send-region
    extempore-connect
    extempore-disconnect)
  "A list of extempore-mode functions to specifically instrument for logging")

(defvar extempore-logger-cache nil
  "An in-memory cache of logged commands, which is flushed to
  disk when the system is idle through
  `extempore-logger-flush'.")

(defun extempore-logger-yasnippet-hook ()
  (extempore-logger-log-current-command 'yas-expand
					nil
					(list yas-snippet-beg yas-snippet-end)))

(add-hook 'yas-after-exit-snippet-hook
	  'extempore-logger-yasnippet-hook)

(defun extempore-logger-log-current-command (command event arg-list)
  (if (and (equal major-mode 'extempore-mode)
           (symbolp command))
      (setq extempore-logger-cache
            (cons (concat (format-time-string "%Y-%m-%d %T.%3N")
                          (format ",%s,%s,%s," 
                                  (buffer-name)
				  (symbol-name command)
				  event)
			  (prin1-to-string
			   (if (member command '(yas-expand
						 extempore-send-definition
						 extempore-send-region
						 extempore-send-buffer))
			       (prin1-to-string
				(read (concat "(" (remove ?\# (buffer-substring-no-properties (car arg-list) (cadr arg-list))) ")")))
			     (prin1-to-string arg-list))))
		  extempore-logger-cache))))

(defun extempore-logger-add-comment (comment)
  (interactive "sAny comments about this particular session? ")
  (if (equal major-mode 'extempore-mode)
      (setq extempore-logger-cache
            (cons (concat (format-time-string "%Y-%m-%d %T.%3N") ","
                          (buffer-name) ","
                          "comment,nil,"
                          (replace-regexp-in-string
                           "[\r\n]" " "
                           (prin1-to-string comment)))
                  extempore-logger-cache))))

(defun extempore-logger-pre-command-hook ()
  (extempore-logger-log-current-command real-this-command last-input-event nil))

;; writing command list to file

(defvar extempore-logger-logfile nil)

(defun extempore-logger-new-logfile ()
  (let* ((log-dir (concat (or user-extempore-directory user-emacs-directory) "keylogs/"))
         (dir-created (unless (file-exists-p log-dir) (make-directory log-dir)))
         (logfile-name (concat log-dir
                               (format-time-string "%Y%m%dT%H%M%S-")
                               user-login-name
                               ".keylog")))
    (if (file-exists-p logfile-name)
        (progn (message "Extempore logfile %s already exists" logfile-name)
               nil)
      logfile-name)))

(defun extempore-logger-flush ()
  (if extempore-logger-logfile
      (progn (append-to-file (concat (mapconcat 'identity (nreverse extempore-logger-cache) "\n") "\n")
                             nil
                             extempore-logger-logfile)
             (setq extempore-logger-cache nil))))

(defun extempore-logger-finish-logfile ()
  (extempore-logger-flush)
  (async-shell-command (format "gzip %s" extempore-logger-logfile)))

(defvar extempore-logger-write-timer nil)
(defvar extempore-logger-write-timer-interval 10.0)

(defun extempore-logger-start-idle-write-timer ()
  (setq extempore-logger-write-timer
        (run-with-idle-timer extempore-logger-write-timer-interval
                             t
                             'extempore-logger-flush)))

(defun extempore-logger-stop-idle-write-timer ()
  (cancel-timer extempore-logger-write-timer)
  (setq extempore-logger-write-timer nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; extempore slave buffer minor mode ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-minor-mode extempore-sb-mode
  "This minor allows emacs to create a 'slave' buffer on
another (potentially remote) emacs instance.

This read-only slave buffer will stay in sync with the master,
showing the text and current window position of the 'master'
buffer."

  :global t
  :init-value nil
  :lighter " esb"
  :keymap nil
  :group 'extempore

  (if extempore-sb-mode
      (call-interactively #'extempore-sb-start)
    (extempore-sb-stop)))

(defcustom extempore-sb-server-port 8420
  "Port for the the extempore slave buffer server."
  :type 'integer
  :group 'extempore)

(defcustom extempore-sb-host-name nil
  "Host name to use sending slave buffers around.

If you don't want to be prompted for this name each time, set the
`extempore-sb-host-name' variable, either through customize or in your
.emacs"
  :type 'string
  :group 'extempore)

(defvar extempore-sb-refresh-interval 0.2
  "The refresh interval (in seconds) for syncing the slave buffers")

(defvar extempore-sb-server nil)

(defun extempore-sb-stop ()
  (if extempore-sb-server
      (progn (delete-process extempore-sb-server)
             (setq extempore-sb-server nil)
             (cancel-function-timers #'extempore-sb-sync-slave-buffer)
             (extempore-sb-delete-all-connections)
             (message "Stopped esb server."))))

(defun extempore-sb-start (port)
  (interactive
   (list (string-to-number
          (ido-completing-read
           "Port: "
           (list (number-to-string extempore-sb-server-port))
           nil nil nil nil
           (number-to-string extempore-sb-server-port)))))
  (extempore-sb-stop)
  (extempore-sb-create-server port)
  (if (null extempore-sb-host-name)
      (setq extempore-sb-host-name
            (let ((default-host-name
                    (if (boundp 'user-login-name)
                        user-login-name
                      (if (functionp 'host-name)
                          (host-name)
                        "remote-host"))))
              (ido-completing-read
               "Your name: "
               (list default-host-name)
               nil nil nil nil
               default-host-name))))
  (message "Started esb server."))

(defun extempore-sb-create-server (port)
  (setq extempore-sb-server
        (make-network-process
         :name "extempore-sb-server"
         :buffer nil
         :coding 'iso-latin-1
         :service port
         :family 'ipv4
         :server t
         :sentinel #'extempore-sb-server-sentinel
         :filter #'extempore-sb-server-filter))
  (unless extempore-sb-server
    (message "esb error: couldn't start the server.")
    extempore-sb-server))

(defun extempore-sb-cleanup-dead-connections ()
  (interactive)
  (dolist (proc (process-list))
    (if (ignore-errors (string= (substring (process-name proc) 0 13)
                                "extempore-sb-"))
        (unless (member (process-status proc) '(run open))
          (delete-process proc)))))

(defun extempore-sb-delete-all-connections ()
  (interactive)
  (dolist (proc (process-list))
    (if (ignore-errors (string= (substring (process-name proc) 0 13)
                                "extempore-sb-"))
        (delete-process proc))))

(defun extempore-sb-server-sentinel (proc str)
  (message "esb: %s" str))

(defun extempore-sb-create-slave-buffer (proc buffer-name buffer-mode)
  (let ((buf (get-buffer-create buffer-name)))
    (set-process-buffer proc buf)
    (with-current-buffer buf
      (buffer-disable-undo)
      (read-only-mode 1)
      (if (fboundp buffer-mode) (funcall buffer-mode)))
    (message "esb: created slave buffer %s" buffer-name)
    buf))

(defun extempore-sb-update-slave-buffer (buf buffer-text pt eval-region)
  (with-current-buffer buf
    (let ((inhibit-read-only t)
          (curr-pt (point)))
      (delete-region (point-min) (point-max))
      (insert buffer-text)
      ;; if slave buffer is visible, and is not the current buffer,
      ;; have if follow the master (remote) cursor position
      (if (get-buffer-window buf)
          (progn (set-window-point (get-buffer-window buf)
                                   (if (eq (window-buffer) buf) curr-pt pt))
                 (if eval-region
                     (extempore-blink-region extempore-sb-blink-overlay
                                                  (car eval-region)
                                                  (cdr eval-region)
                                                  buf)))))))

;; `extempore-sb-partial-data' is for handling buffer text recieved by
;; the filter in multiple chunks
(make-variable-buffer-local 'extempore-sb-partial-data)
(setq extempore-sb-partial-data nil)

(defun extempore-sb-server-filter (proc str)
  (let ((proc-buf (process-buffer proc)))
    (if (null proc-buf)
        (let ((data (ignore-errors (read str))))
	  (if (and data (string= (car data) "esb-data"))
	      (extempore-sb-create-slave-buffer proc (cadr data) (caddr data))))
      (with-current-buffer proc-buf
        (setq extempore-sb-partial-data (concat extempore-sb-partial-data str))
	(if (not (ignore-errors (string= (substring extempore-sb-partial-data 0 11)
					 "(\"esb-data\"")))
	    (setq extempore-sb-partial-data nil)
	  (let ((data (ignore-errors (read extempore-sb-partial-data))))
	    (if data
		(progn (setq extempore-sb-partial-data nil)
		       (extempore-sb-dispatch-received-data proc-buf (cdr data))))))))))

;; data list (only the cdr of this list passed to the dispatch function)
;; ("esb-data" buffer-name major-mode position buffer-text eval-markers)

(defun extempore-sb-dispatch-received-data (buf data)
  (cond
   ((not (and (sequencep data) (= (length data) 5)))
    (setq extempore-sb-partial-data nil)
    (message "esb error: malformed buffer state recieved from remote host."))
   ((string= (buffer-name buf)
             (car data))
    (extempore-sb-update-slave-buffer buf
                                      (nth 3 data)
                                      (nth 2 data)
                                      (nth 4 data)))
   (t (message "esb error: received state from wrong buffer."))))

(defun extempore-sb-slave-buffer-name (buffer-name host-name)
  (concat buffer-name "@" host-name "<slave>"))

(defun extempore-sb-sync-slave-buffer (buf)
  (with-current-buffer buf
    (let ((proc (get-buffer-process buf)))
      (if proc
          (progn
            (process-send-string
             proc
             (prin1-to-string
	      (list "esb-data"
		    (extempore-sb-slave-buffer-name
                     (buffer-name)
                     extempore-sb-host-name)
                    major-mode
                    (point)
                    (buffer-substring-no-properties (point-min) (point-max))
                    extempore-sb-eval-markers)))
            (setq extempore-sb-eval-markers nil))))))

(defun extempore-sb-setup-buffer (buf host port)
  (let ((proc (open-network-stream
               (concat "extempore-sb-push-to-" host ":" (number-to-string port))
               buf host port)))
    (if proc
        (progn
          (set-process-sentinel proc #'extempore-sb-server-sentinel)
          (with-current-buffer buf
            (process-send-string
             proc
             (prin1-to-string
	      (list "esb-data"
		    (extempore-sb-slave-buffer-name
                     (buffer-name)
                     extempore-sb-host-name)
                    major-mode
                    0
                    "setup"))))
          (message "esb: created slave buffer on %s:%s" host port))
      (message "esb: couldn't connect to %s:%s" host port))))

(make-variable-buffer-local 'extempore-sb-push-timer)

(defun extempore-sb-start-timer (buf time-interval)
  (setq extempore-sb-push-timer
        (run-with-timer 0 time-interval #'extempore-sb-sync-slave-buffer buf)))

(defun extempore-sb-push-current-buffer (host port)
  (interactive
   (let ((read-host (ido-completing-read
                     "Hostname: "
                     (list "localhost")
                     nil nil nil nil
                     "localhost"))
         (read-port (string-to-number
                     (ido-completing-read
                      "Port: "
                      (list (number-to-string extempore-sb-server-port))
                      nil nil nil nil
                      (number-to-string extempore-sb-server-port)))))
     (list read-host read-port)))
  (extempore-sb-setup-buffer (current-buffer) host port)
  (extempore-sb-start-timer (current-buffer) extempore-sb-refresh-interval))

(defun extempore-sb-stop-pushing-current-buffer ()
  (interactive)
  (if (get-buffer-process (current-buffer))
      (progn (delete-process nil)
             (if extempore-sb-push-timer
                 (progn (cancel-timer extempore-sb-push-timer)
                        (setq extempore-sb-push-timer nil)))
             (message "esb: stopped syncing buffer: %s" (buffer-name)))
    (message "esb: not currently pushing this buffer")))

(defun extempore-sb-slave-buffer-p (buf)
  (let ((proc (get-buffer-process buf)))
    (if (and proc
             (ignore-errors (string= (substring (process-name proc) 0 13)
                                     "extempore-sb-")))
        t
      nil)))

(defun extempore-sb-toggle-current-buffer ()
  (interactive)
  (if (extempore-sb-slave-buffer-p (current-buffer))
      (extempore-sb-stop-pushing-current-buffer)
    (call-interactively #'extempore-sb-push-current-buffer)))

(provide 'extempore)

;;; extempore.el ends here
