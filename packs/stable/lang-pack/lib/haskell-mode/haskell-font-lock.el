;;; haskell-font-lock.el --- Font locking module for Haskell Mode

;; Copyright 2003, 2004, 2005, 2006, 2007, 2008  Free Software Foundation, Inc.
;; Copyright 1997-1998  Graeme E Moss, and Tommy Thorn

;; Author: 1997-1998 Graeme E Moss <gem@cs.york.ac.uk>
;;         1997-1998 Tommy Thorn <thorn@irisa.fr>
;;         2003      Dave Love <fx@gnu.org>
;; Keywords: faces files Haskell

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Code:

(require 'cl-lib)
(require 'haskell-mode)
(require 'font-lock)

(defcustom haskell-font-lock-symbols nil
  "Display \\ and -> and such using symbols in fonts.

This may sound like a neat trick, but be extra careful: it changes the
alignment and can thus lead to nasty surprises w.r.t layout."
  :group 'haskell
  :type 'boolean)

(defcustom haskell-font-lock-symbols-alist
  '(("\\" . "λ")
    ("not" . "¬")
    ("->" . "→")
    ("<-" . "←")
    ("=>" . "⇒")
    ("()" . "∅")
    ("==" . "≡")
    ("/=" . "≢")
    (">=" . "≥")
    ("<=" . "≤")
    ("!!" . "‼")
    ("&&" . "∧")
    ("||" . "∨")
    ("sqrt" . "√")
    ("undefined" . "⊥")
    ("pi" . "π")
    ("~>" . "⇝") ;; Omega language
    ;; ("~>" "↝") ;; less desirable
    ("-<" . "↢") ;; Paterson's arrow syntax
    ;; ("-<" "⤙") ;; nicer but uncommon
    ("::" . "∷")
    ("." "∘" ; "○"
     ;; Need a predicate here to distinguish the . used by
     ;; forall <foo> . <bar>.
     haskell-font-lock-dot-is-not-composition)
    ("forall" . "∀"))
  "Alist mapping Haskell symbols to chars.

Each element has the form (STRING . COMPONENTS) or (STRING
COMPONENTS PREDICATE).

STRING is the Haskell symbol.
COMPONENTS is a representation specification suitable as an argument to
`compose-region'.
PREDICATE if present is a function of one argument (the start position
of the symbol) which should return non-nil if this mapping should
be disabled at that position."
  :type '(alist string string)
  :group 'haskell)

(defun haskell-font-lock-dot-is-not-composition (start)
  "Return non-nil if the \".\" at START is not a composition operator.
This is the case if the \".\" is part of a \"forall <tvar> . <type>\"."
  (save-excursion
    (goto-char start)
    (or (re-search-backward "\\<forall\\>[^.\"]*\\="
                            (line-beginning-position) t)
        (not (or
              (string= " " (string (char-after start)))
              (string= " " (string (char-before start))))))))

(defface haskell-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face used to highlight Haskell keywords."
  :group 'haskell)

(defface haskell-constructor-face
  '((t :inherit font-lock-type-face))
  "Face used to highlight Haskell constructors."
  :group 'haskell)

;; This used to be `font-lock-variable-name-face' but it doesn't result in
;; a highlighting that's consistent with other modes (it's mostly used
;; for function defintions).
(defface haskell-definition-face
  '((t :inherit font-lock-function-name-face))
  "Face used to highlight Haskell definitions."
  :group 'haskell)

;; This is probably just wrong, but it used to use
;; `font-lock-function-name-face' with a result that was not consistent with
;; other major modes, so I just exchanged with `haskell-definition-face'.
(defface haskell-operator-face
  '((t :inherit font-lock-variable-name-face))
  "Face used to highlight Haskell operators."
  :group 'haskell)

(defface haskell-pragma-face
  '((t :inherit font-lock-preprocessor-face))
  "Face used to highlight Haskell pragmas."
  :group 'haskell)

(defface haskell-literate-comment-face
  '((t :inherit font-lock-doc-face))
  "Face with which to fontify literate comments.
Inherit from `default' to avoid fontification of them."
  :group 'haskell)

(defun haskell-font-lock-compose-symbol (alist)
  "Compose a sequence of ascii chars into a symbol.
Regexp match data 0 points to the chars."
  ;; Check that the chars should really be composed into a symbol.
  (let* ((start (match-beginning 0))
         (end (match-end 0))
         (syntaxes (cond
                    ((eq (char-syntax (char-after start)) ?w) '(?w))
                    ((eq (char-syntax (char-after start)) ?.) '(?.))
                    ;; Special case for the . used for qualified names.
                    ((and (eq (char-after start) ?\.) (= end (1+ start)))
                     '(?_ ?\\ ?w))
                    (t '(?_ ?\\))))
         sym-data)
    (if (or (memq (char-syntax (or (char-before start) ?\ )) syntaxes)
            (memq (char-syntax (or (char-after end) ?\ )) syntaxes)
            (or (elt (syntax-ppss) 3) (elt (syntax-ppss) 4))
            (and (consp (setq sym-data (cdr (assoc (match-string 0) alist))))
                 (let ((pred (cadr sym-data)))
                   (setq sym-data (car sym-data))
                   (funcall pred start))))
        ;; No composition for you.  Let's actually remove any composition
        ;; we may have added earlier and which is now incorrect.
        (remove-text-properties start end '(composition))
      ;; That's a symbol alright, so add the composition.
      (compose-region start end sym-data)))
  ;; Return nil because we're not adding any face property.
  nil)

(defun haskell-font-lock-symbols-keywords ()
  (when (and haskell-font-lock-symbols
	     haskell-font-lock-symbols-alist
	     (fboundp 'compose-region))
    `((,(regexp-opt (mapcar 'car haskell-font-lock-symbols-alist) t)
       (0 (haskell-font-lock-compose-symbol ',haskell-font-lock-symbols-alist)
	  ;; In Emacs-21, if the `override' field is nil, the face
	  ;; expressions is only evaluated if the text has currently
	  ;; no face.  So force evaluation by using `keep'.
	  keep)))))

;; The font lock regular expressions.
(defun haskell-font-lock-keywords-create (literate)
  "Create fontification definitions for Haskell scripts.
Returns keywords suitable for `font-lock-keywords'."
  (let* (;; Bird-style literate scripts start a line of code with
         ;; "^>", otherwise a line of code starts with "^".
         (line-prefix (if (eq literate 'bird) "^> ?" "^"))

         (varid "\\b[[:lower:]_][[:alnum:]'_]*\\b")
         ;; We allow ' preceding conids because of DataKinds/PolyKinds
         (conid "\\b'?[[:upper:]][[:alnum:]'_]*\\b")
         (modid (concat "\\b" conid "\\(\\." conid "\\)*\\b"))
         (qvarid (concat modid "\\." varid))
         (qconid (concat modid "\\." conid))
         (sym "\\s.+")

         ;; Reserved identifiers
         (reservedid
          (concat "\\<"
                  ;; `as', `hiding', and `qualified' are part of the import
                  ;; spec syntax, but they are not reserved.
                  ;; `_' can go in here since it has temporary word syntax.
                  ;; (regexp-opt
                  ;;  '("case" "class" "data" "default" "deriving" "do"
                  ;;    "else" "if" "import" "in" "infix" "infixl"
                  ;;    "infixr" "instance" "let" "module" "newtype" "of"
                  ;;    "then" "type" "where" "_") t)
                  "\\(_\\|c\\(ase\\|lass\\)\\|d\\(ata\\|e\\(fault\\|riving\\)\\|o\\)\\|else\\|i\\(mport\\|n\\(fix[lr]?\\|stance\\)\\|[fn]\\)\\|let\\|module\\|mdo\\|newtype\\|of\\|rec\\|proc\\|t\\(hen\\|ype\\)\\|where\\)"
                  "\\>"))

         ;; Top-level declarations
         (topdecl-var
          (concat line-prefix "\\(" varid "\\(?:\\s-*,\\s-*" varid "\\)*" "\\)\\s-*"
                  ;; optionally allow for a single newline after identifier
                  ;; NOTE: not supported for bird-style .lhs files
                  (if (eq literate 'bird) nil "\\([\n]\\s-+\\)?")
                  ;; A toplevel declaration can be followed by a definition
                  ;; (=), a type (::) or (∷), a guard, or a pattern which can
                  ;; either be a variable, a constructor, a parenthesized
                  ;; thingy, or an integer or a string.
                  "\\(" varid "\\|" conid "\\|::\\|∷\\|=\\||\\|\\s(\\|[0-9\"']\\)"))
         (topdecl-var2
          (concat line-prefix "\\(" varid "\\|" conid "\\)\\s-*`\\(" varid "\\)`"))
         (topdecl-bangpat
          (concat line-prefix "\\(" varid "\\)\\s-*!"))
         (topdecl-sym
          (concat line-prefix "\\(" varid "\\|" conid "\\)\\s-*\\(" sym "\\)"))
         (topdecl-sym2 (concat line-prefix "(\\(" sym "\\))"))

         keywords)

    (setq keywords
          `(;; NOTICE the ordering below is significant
            ;;
            ("^#.*$" 0 'font-lock-preprocessor-face t)

            ,@(haskell-font-lock-symbols-keywords)

            (,reservedid 1 'haskell-keyword-face)

            ;; Special case for `as', `hiding', `safe' and `qualified', which are
            ;; keywords in import statements but are not otherwise reserved.
            ("\\<import[ \t]+\\(?:\\(safe\\>\\)[ \t]*\\)?\\(?:\\(qualified\\>\\)[ \t]*\\)?\\(?:\"[^\"]*\"[\t ]*\\)?[^ \t\n()]+[ \t]*\\(?:\\(\\<as\\>\\)[ \t]*[^ \t\n()]+[ \t]*\\)?\\(\\<hiding\\>\\)?"
             (1 'haskell-keyword-face nil lax)
             (2 'haskell-keyword-face nil lax)
             (3 'haskell-keyword-face nil lax)
             (4 'haskell-keyword-face nil lax))

            ;; Special case for `foreign import'
            ;; keywords in foreign import statements but are not otherwise reserved.
            ("\\<\\(foreign\\)[ \t]+\\(import\\)[ \t]+\\(?:\\(ccall\\|stdcall\\|cplusplus\\|jvm\\|dotnet\\)[ \t]+\\)?\\(?:\\(safe\\|unsafe\\|interruptible\\)[ \t]+\\)?"
             (1 'haskell-keyword-face nil lax)
             (2 'haskell-keyword-face nil lax)
             (3 'haskell-keyword-face nil lax)
             (4 'haskell-keyword-face nil lax))

            ;; Special case for `foreign export'
            ;; keywords in foreign export statements but are not otherwise reserved.
            ("\\<\\(foreign\\)[ \t]+\\(export\\)[ \t]+\\(?:\\(ccall\\|stdcall\\|cplusplus\\|jvm\\|dotnet\\)[ \t]+\\)?"
             (1 'haskell-keyword-face nil lax)
             (2 'haskell-keyword-face nil lax)
             (3 'haskell-keyword-face nil lax))

            ;; Toplevel Declarations.
            ;; Place them *before* generic id-and-op highlighting.
            (,topdecl-var  (1 'haskell-definition-face))
            (,topdecl-var2 (2 'haskell-definition-face))
            (,topdecl-bangpat  (1 'haskell-definition-face))
            (,topdecl-sym  (2 (unless (member (match-string 2) '("\\" "=" "->" "→" "<-" "←" "::" "∷" "," ";" "`"))
                                'haskell-definition-face)))
            (,topdecl-sym2 (1 (unless (member (match-string 1) '("\\" "=" "->" "→" "<-" "←" "::" "∷" "," ";" "`"))
                                'haskell-definition-face)))

            ;; These four are debatable...
            ("(\\(,*\\|->\\))" 0 'haskell-constructor-face)
            ("\\[\\]" 0 'haskell-constructor-face)

            (,(concat "`" varid "`") 0 'haskell-operator-face)
            (,(concat "`" conid "`") 0 'haskell-operator-face)
            (,(concat "`" qvarid "`") 0 'haskell-operator-face)
            (,(concat "`" qconid "`") 0 'haskell-operator-face)

            (,qconid 0 'haskell-constructor-face)

            (,conid 0 'haskell-constructor-face)

            (,sym 0 (if (and (eq (char-after (match-beginning 0)) ?:)
                             (not (member (match-string 0) '("::" "∷"))))
                        'haskell-constructor-face
                      'haskell-operator-face))))
    keywords))

(defvar haskell-font-lock-latex-cache-pos nil
  "Position of cache point used by `haskell-font-lock-latex-cache-in-comment'.
Should be at the start of a line.")
(make-variable-buffer-local 'haskell-font-lock-latex-cache-pos)

(defvar haskell-font-lock-latex-cache-in-comment nil
  "If `haskell-font-lock-latex-cache-pos' is outside a
\\begin{code}..\\end{code} block (and therefore inside a comment),
this variable is set to t, otherwise nil.")
(make-variable-buffer-local 'haskell-font-lock-latex-cache-in-comment)

(defun haskell-font-lock-latex-comments (end)
  "Sets `match-data' according to the region of the buffer before end
that should be commented under LaTeX-style literate scripts."
  (let ((start (point)))
    (if (= start end)
        ;; We're at the end.  No more to fontify.
        nil
      (if (not (eq start haskell-font-lock-latex-cache-pos))
          ;; If the start position is not cached, calculate the state
          ;; of the start.
          (progn
            (setq haskell-font-lock-latex-cache-pos start)
            ;; If the previous \begin{code} or \end{code} is a
            ;; \begin{code}, then start is not in a comment, otherwise
            ;; it is in a comment.
            (setq haskell-font-lock-latex-cache-in-comment
                  (if (and
                       (re-search-backward
                        "^\\(\\(\\\\begin{code}\\)\\|\\(\\\\end{code}\\)\\)$"
                        (point-min) t)
                       (match-end 2))
                      nil t))
            ;; Restore position.
            (goto-char start)))
      (if haskell-font-lock-latex-cache-in-comment
          (progn
            ;; If start is inside a comment, search for next \begin{code}.
            (re-search-forward "^\\\\begin{code}$" end 'move)
            ;; Mark start to end of \begin{code} (if present, till end
            ;; otherwise), as a comment.
            (set-match-data (list start (point)))
            ;; Return point, as a normal regexp would.
            (point))
        ;; If start is inside a code block, search for next \end{code}.
        (if (re-search-forward "^\\\\end{code}$" end t)
            ;; If one found, mark it as a comment, otherwise finish.
            (point))))))

(defconst haskell-basic-syntactic-keywords
  '(;; Character constants (since apostrophe can't have string syntax).
    ;; Beware: do not match something like 's-}' or '\n"+' since the first '
    ;; might be inside a comment or a string.
    ;; This still gets fooled with "'"'"'"'"'"', but ... oh well.
    ("\\Sw\\('\\)\\([^\\'\n]\\|\\\\.[^\\'\n \"}]*\\)\\('\\)" (1 "\"") (3 "\""))
    ;; Deal with instances of `--' which don't form a comment
    ("[!#$%&*+./:<=>?@^|~\\]*--[!#$%&*+./:<=>?@^|~\\-]*" (0 (cond ((or (nth 3 (syntax-ppss)) (numberp (nth 4 (syntax-ppss))))
                              ;; There are no such instances inside
                              ;; nestable comments or strings
                              nil)
                             ((string-match "\\`-*\\'" (match-string 0))
                              ;; Sequence of hyphens. Do nothing in
                              ;; case of things like `{---'.
                              nil)
                             ((string-match "\\`[^-]+--.*" (match-string 0))
                              ;; Extra characters before comment starts
                              ".")
                             (t ".")))) ; other symbol sequence

    ;; Implement Haskell Report 'escape' and 'gap' rules. Backslash
    ;; inside of a string is escaping unless it is preceeded by
    ;; another escaping backslash. There can be whitespace between
    ;; those two.
    ;;
    ;; Backslashes outside of string never escape.
    ;;
    ;; Note that (> 0 (skip-syntax-backward ".")) this skips over *escaping*
    ;; backslashes only.
    ("\\\\" (0 (when (save-excursion (and (nth 3 (syntax-ppss))
                                          (goto-char (match-beginning 0))
                                          (skip-syntax-backward "->")
                                          (or (not (eq ?\\ (char-before)))
                                              (> 0 (skip-syntax-backward ".")))))
                  "\\")))

    ;; QuasiQuotes syntax: [quoter| string |], quoter is unqualified
    ;; name, no spaces, string is arbitrary (including newlines),
    ;; finishes at the first occurence of |], no escaping is provided.
    ;;
    ;; The quoter cannot be "e", "t", "d", or "p", since those overlap
    ;; with Template Haskell quotations.
    ;;
    ;; QuasiQuotes opens only when outside of a string or a comment
    ;; and closes only when inside a quasiquote.
    ;;
    ;; (syntax-ppss) returns list with two interesting elements:
    ;; nth 3. non-nil if inside a string. (it is the character that will
    ;;        terminate the string, or t if the string should be terminated
    ;;        by a generic string delimiter.)
    ;; nth 4. nil if outside a comment, t if inside a non-nestable comment,
    ;;        else an integer (the current comment nesting).
    ;;
    ;; Note also that we need to do in in a single pass, hence a regex
    ;; that covers both the opening and the ending of a quasiquote.

    ("\\(\\[[[:alnum:]]+\\)?\\(|\\)\\(]\\)?"
     (2 (save-excursion
          (goto-char (match-beginning 0))
          (if (eq ?\[ (char-after))
              ;; opening case
              (unless (or (nth 3 (syntax-ppss))
                          (nth 4 (syntax-ppss))
                          (member (match-string 1)
                                  '("[e" "[t" "[d" "[p")))
                "\"")
            ;; closing case
            (when (and (eq ?| (nth 3 (syntax-ppss)))
                       (equal "]" (match-string 3))
                       )
              "\"")))))
    ))

(defconst haskell-bird-syntactic-keywords
  (cons '("^[^\n>]"  (0 "<"))
        haskell-basic-syntactic-keywords))

(defconst haskell-latex-syntactic-keywords
  (append
   '(("^\\\\begin{code}\\(\n\\)" 1 "!")
     ;; Note: buffer is widened during font-locking.
     ("\\`\\(.\\|\n\\)" (1 "!"))               ; start comment at buffer start
     ("^\\(\\\\\\)end{code}$" 1 "!"))
   haskell-basic-syntactic-keywords))

(defun haskell-syntactic-face-function (state)
  "`font-lock-syntactic-face-function' for Haskell."
  (cond
   ((nth 3 state) 'font-lock-string-face) ; as normal
   ;; Else comment.  If it's from syntax table, use default face.
   ((or (eq 'syntax-table (nth 7 state))
        (and (eq haskell-literate 'bird)
             (memq (char-before (nth 8 state)) '(nil ?\n))))
    'haskell-literate-comment-face)
   ;; Detect pragmas. A pragma is enclosed in special comment
   ;; delimeters {-# .. #-}.
   ((save-excursion
      (goto-char (nth 8 state))
      (and (looking-at "{-#")
           (forward-comment 1)
           (goto-char (- (point) 3))
           (looking-at "#-}")))
    'haskell-pragma-face)
   ;; Haddock comment start with either "-- [|^*$]" or "{- ?[|^*$]"
   ;; (note space optional for nested comments and mandatory for
   ;; double dash comments).
   ;;
   ;; Haddock comment will also continue on next line, provided:
   ;; - current line is a double dash haddock comment
   ;; - next line is also double dash comment
   ;; - there is only whitespace between
   ;;
   ;; We recognize double dash haddock comments by property
   ;; 'font-lock-doc-face attached to newline. In case of bounded
   ;; comments newline is outside of comment.
   ((save-excursion
      (goto-char (nth 8 state))
      (or (looking-at "\\(?:{- ?\\|-- \\)[|^*$]")
	  (and (looking-at "--")              ; are we at double dash comment
	       (forward-line -1)              ; this is nil on first line
	       (eq (get-text-property (line-end-position) 'face)
		   'font-lock-doc-face)	      ; is a doc face
	       (forward-line)
	       (skip-syntax-forward "-")      ; see if there is only whitespace
	       (eq (point) (nth 8 state)))))  ; we are back in position
    'font-lock-doc-face)
   (t 'font-lock-comment-face)))

(defconst haskell-font-lock-keywords
  (haskell-font-lock-keywords-create nil)
  "Font lock definitions for non-literate Haskell.")

(defconst haskell-font-lock-bird-literate-keywords
  (haskell-font-lock-keywords-create 'bird)
  "Font lock definitions for Bird-style literate Haskell.")

(defconst haskell-font-lock-latex-literate-keywords
  (haskell-font-lock-keywords-create 'latex)
  "Font lock definitions for LaTeX-style literate Haskell.")

;;;###autoload
(defun haskell-font-lock-choose-keywords ()
  (let ((literate (if (boundp 'haskell-literate) haskell-literate)))
    (cl-case literate
      (bird haskell-font-lock-bird-literate-keywords)
      ((latex tex) haskell-font-lock-latex-literate-keywords)
      (t haskell-font-lock-keywords))))

(defun haskell-font-lock-choose-syntactic-keywords ()
  (let ((literate (if (boundp 'haskell-literate) haskell-literate)))
    (cl-case literate
      (bird haskell-bird-syntactic-keywords)
      ((latex tex) haskell-latex-syntactic-keywords)
      (t haskell-basic-syntactic-keywords))))

(defun haskell-font-lock-defaults-create ()
  "Locally set `font-lock-defaults' for Haskell."
  (set (make-local-variable 'font-lock-defaults)
       '(haskell-font-lock-choose-keywords
         nil nil ((?\' . "w") (?_  . "w")) nil
         (font-lock-syntactic-keywords
          . haskell-font-lock-choose-syntactic-keywords)
         (font-lock-syntactic-face-function
          . haskell-syntactic-face-function)
         ;; Get help from font-lock-syntactic-keywords.
         (parse-sexp-lookup-properties . t))))

;; The main functions.
(defun turn-on-haskell-font-lock ()
  "Turns on font locking in current buffer for Haskell 1.4 scripts.

Changes the current buffer's `font-lock-defaults', and adds the
following variables:

   `haskell-keyword-face'      for reserved keywords and syntax,
   `haskell-constructor-face'  for data- and type-constructors, class names,
                               and module names,
   `haskell-operator-face'     for symbolic and alphanumeric operators,
   `haskell-default-face'      for ordinary code.

The variables are initialised to the following font lock default faces:

   `haskell-keyword-face'      `font-lock-keyword-face'
   `haskell-constructor-face'  `font-lock-type-face'
   `haskell-operator-face'     `font-lock-function-name-face'
   `haskell-default-face'      <default face>

Two levels of fontification are defined: level one (the default)
and level two (more colour).  The former does not colour operators.
Use the variable `font-lock-maximum-decoration' to choose
non-default levels of fontification.  For example, adding this to
.emacs:

  (setq font-lock-maximum-decoration '((haskell-mode . 2) (t . 0)))

uses level two fontification for `haskell-mode' and default level for
all other modes.  See documentation on this variable for further
details.

To alter an attribute of a face, add a hook.  For example, to change
the foreground colour of comments to brown, add the following line to
.emacs:

  (add-hook 'haskell-font-lock-hook
      (lambda ()
          (set-face-foreground 'haskell-comment-face \"brown\")))

Note that the colours available vary from system to system.  To see
what colours are available on your system, call
`list-colors-display' from emacs.

To turn font locking on for all Haskell buffers, add this to .emacs:

  (add-hook 'haskell-mode-hook 'turn-on-haskell-font-lock)

To turn font locking on for the current buffer, call
`turn-on-haskell-font-lock'.  To turn font locking off in the current
buffer, call `turn-off-haskell-font-lock'.

Bird-style literate Haskell scripts are supported: If the value of
`haskell-literate-bird-style' (automatically set by the Haskell mode
of Moss&Thorn) is non-nil, a Bird-style literate script is assumed.

Invokes `haskell-font-lock-hook' if not nil."
  (haskell-font-lock-defaults-create)
  (run-hooks 'haskell-font-lock-hook)
  (turn-on-font-lock))

(defun turn-off-haskell-font-lock ()
  "Turns off font locking in current buffer."
  (font-lock-mode -1))

(defun haskell-fontify-as-mode (text mode)
  "Fontify TEXT as MODE, returning the fontified text."
  (with-temp-buffer
    (funcall mode)
    (insert text)
    (if (fboundp 'font-lock-ensure)
        (font-lock-ensure)
      (with-no-warnings (font-lock-fontify-buffer)))
    (buffer-substring (point-min) (point-max))))

;; Provide ourselves:

(provide 'haskell-font-lock)

;; Local Variables:
;; coding: utf-8-unix
;; tab-width: 8
;; End:

;;; haskell-font-lock.el ends here
