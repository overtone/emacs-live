;;; haskell-mode.el --- A Haskell editing mode    -*- coding: utf-8 -*-

;; Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008  Free Software Foundation, Inc
;; Copyright (C) 1992, 1997-1998  Simon Marlow, Graeme E Moss, and Tommy Thorn

;; Author:  1992      Simon Marlow
;;          1997-1998 Graeme E Moss <gem@cs.york.ac.uk> and
;;                    Tommy Thorn <thorn@irisa.fr>,
;;          2001-2002 Reuben Thomas (>=v1.4)
;;          2003      Dave Love <fx@gnu.org>
;; Keywords: faces files Haskell
;; URL: https://github.com/haskell/haskell-mode

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

;;; Commentary:

;; A major mode for editing Haskell (the functional programming
;; language, see URL `http://www.haskell.org') in Emacs.
;;
;; Some of its major features include:
;;
;;  - syntax highlighting (font lock),
;;
;;  - automatic indentation,
;;
;;  - on-the-fly documentation,
;;
;;  - interaction with inferior GHCi/Hugs instance,
;;
;;  - scans declarations and places them in a menu.
;;
;; See URL `https://github.com/haskell/haskell-mode' and/or
;; Info node `(haskell-mode)Introduction' for more information.
;;
;; Use `M-x haskell-mode-view-news` (after Haskell Mode is installed)
;; to show information on recent changes in Haskell Mode.

;;; Change Log:

;; This mode is based on an editing mode by Simon Marlow 11/1/92
;; and heavily modified by Graeme E Moss and Tommy Thorn 7/11/98.
;;
;; Version 1.5:
;;   Added autoload for haskell-indentation
;;
;; Version 1.43:
;;   Various tweaks to doc strings and customization support from
;;   Ville Skyttä <scop@xemacs.org>.
;;
;; Version 1.42:
;;   Added autoload for GHCi inferior mode (thanks to Scott
;;   Williams for the bug report and fix).
;;
;; Version 1.41:
;;   Improved packaging, and made a couple more variables
;;   interactively settable.
;;
;; Version 1.4:
;;   Added GHCi mode from Chris Webb, and tidied up a little.
;;
;; Version 1.3:
;;   The literate or non-literate style of a buffer is now indicated
;;   by just the variable haskell-literate: nil, `bird', or `tex'.
;;   For literate buffers with ambiguous style, the value of
;;   haskell-literate-default is used.
;;
;; Version 1.2:
;;   Separated off font locking, declaration scanning and simple
;;   indentation, and made them separate modules.  Modules can be
;;   added easily now.  Support for modules haskell-doc,
;;   haskell-indent, and haskell-hugs.  Literate and non-literate
;;   modes integrated into one mode, and literate buffer indicated by
;;   value of haskell-literate(-bird-style).
;;
;; Version 1.1:
;;   Added support for declaration scanning under XEmacs via
;;   func-menu.  Moved operators to level two fontification.
;;
;; Version 1.0:
;;   Added a nice indention support from Heribert Schuetz
;;   <Heribert.Schuetz@informatik.uni-muenchen.de>:
;;
;;     I have just hacked an Emacs Lisp function which you might prefer
;;     to `indent-relative' in haskell-mode.el.  See below.  It is not
;;     really Haskell-specific because it does not take into account
;;     keywords like `do', `of', and `let' (where the layout rule
;;     applies), but I already find it useful.
;;
;;   Cleaned up the imenu support.  Added support for literate scripts.
;;
;; Version 0.103 [HWL]:
;;   From Hans Wolfgang Loidl <hwloidl@dcs.gla.ac.uk>:
;;
;;   I (HWL) added imenu support by copying the appropriate functions
;;   from hugs-mode.  A menu-bar item "Declarations" is now added in
;;   haskell mode.  The new code, however, needs some clean-up.
;;
;; Version 0.102:
;;
;;   Moved C-c C-c key binding to comment-region.  Leave M-g M-g to do
;;   the work.  comment-start-skip is changed to comply with comment-start.
;;
;; Version 0.101:
;;
;;   Altered indent-line-function to indent-relative.
;;
;; Version 0.100:
;;
;;   First official release.

;;; Code:

(require 'dabbrev)
(require 'compile)
(require 'flymake)
(require 'outline)
(require 'haskell-align-imports)
(require 'haskell-sort-imports)
(require 'haskell-string)
(with-no-warnings (require 'cl))

;; FIXME: code-smell: too many forward decls for haskell-session are required here
(defvar haskell-session)
(declare-function haskell-process-do-try-info "haskell-process" (sym))
(declare-function haskell-process-generate-tags "haskell-process" (&optional and-then-find-this-tag))
(declare-function haskell-session "haskell-session" ())
(declare-function haskell-session-all-modules "haskell-session" (&optional DONTCREATE))
(declare-function haskell-session-cabal-dir "haskell-session" (session))
(declare-function haskell-session-maybe "haskell-session" ())
(declare-function haskell-session-tags-filename "haskell-session" (session))
(declare-function haskell-session-current-dir "haskell-session" (session))

;; All functions/variables start with `(literate-)haskell-'.

;; Version of mode.
(defconst haskell-version "@VERSION@"
  "The release version of `haskell-mode'.")

(defconst haskell-git-version "@GIT_VERSION@"
  "The Git version of `haskell-mode'.")

(defvar haskell-mode-pkg-base-dir (file-name-directory load-file-name)
  "Package base directory of installed `haskell-mode'.
Used for locating additional package data files.")

;;;###autoload
(defun haskell-version (&optional here)
  "Show the `haskell-mode` version in the echo area.
With prefix argument HERE, insert it at point.
When FULL is non-nil, use a verbose version string.
When MESSAGE is non-nil, display a message with the version."
  (interactive "P")
  (let* ((haskell-mode-dir (ignore-errors
                             (file-name-directory (or (locate-library "haskell-mode") ""))))
         (_version (format "haskell-mode version %s (%s @ %s)"
                           haskell-version
                           haskell-git-version
                           haskell-mode-dir)))
    (if here
        (insert _version)
      (message "%s" _version))))

;;;###autoload
(defun haskell-mode-view-news ()
  "Display information on recent changes to haskell-mode."
  (interactive)
  (with-current-buffer (find-file-read-only (expand-file-name "NEWS" haskell-mode-pkg-base-dir))
    (goto-char (point-min))
    (hide-sublevels 1)
    (outline-next-visible-heading 1)
    (show-subtree)))

(defgroup haskell nil
  "Major mode for editing Haskell programs."
  :link '(custom-manual "(haskell-mode)")
  :group 'languages
  :prefix "haskell-")

;;;###autoload
(defun haskell-customize ()
  "Browse the haskell customize sub-tree.
This calls 'customize-browse' with haskell as argument and makes
sure all haskell customize definitions have been loaded."
  (interactive)
  ;; make sure all modules with (defcustom ...)s are loaded
  (mapc 'require
        '(haskell-checkers haskell-compile haskell-doc haskell-font-lock haskell-indentation haskell-indent haskell-interactive-mode haskell-menu haskell-process haskell-yas inf-haskell))
  (customize-browse 'haskell))

;; Are we looking at a literate script?
(defvar haskell-literate nil
  "*If not nil, the current buffer contains a literate Haskell script.
Possible values are: `bird' and `tex', for Bird-style and LaTeX-style
literate scripts respectively.  Set by `haskell-mode' and
`literate-haskell-mode'.  For an ambiguous literate buffer -- i.e. does
not contain either \"\\begin{code}\" or \"\\end{code}\" on a line on
its own, nor does it contain \">\" at the start of a line -- the value
of `haskell-literate-default' is used.")
(make-variable-buffer-local 'haskell-literate)
(put 'haskell-literate 'safe-local-variable 'symbolp)
;; Default literate style for ambiguous literate buffers.
(defcustom haskell-literate-default 'bird
  "Default value for `haskell-literate'.
Used if the style of a literate buffer is ambiguous.  This variable should
be set to the preferred literate style."
  :group 'haskell
  :type '(choice (const bird) (const tex) (const nil)))

;;;###autoload
(defvar haskell-mode-map
  (let ((map (make-sparse-keymap)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; For inferior haskell mode, use the below bindings
    ;; (define-key map [?\M-C-x]     'inferior-haskell-send-defun)
    ;; (define-key map [?\C-x ?\C-e] 'inferior-haskell-send-last-sexp)
    ;; (define-key map [?\C-c ?\C-r] 'inferior-haskell-send-region)
    (define-key map [?\C-x ?\C-d] 'inferior-haskell-send-decl)
    (define-key map [?\C-c ?\C-z] 'switch-to-haskell)
    (define-key map [?\C-c ?\C-l] 'inferior-haskell-load-file)
    ;; I think it makes sense to bind inferior-haskell-load-and-run to C-c
    ;; C-r, but since it used to be bound to `reload' until June 2007, I'm
    ;; going to leave it out for now.
    ;; (define-key map [?\C-c ?\C-r] 'inferior-haskell-load-and-run)
    (define-key map [?\C-c ?\C-b] 'switch-to-haskell)
    ;; (define-key map [?\C-c ?\C-s] 'inferior-haskell-start-process)
    ;; That's what M-; is for.
    ;; (define-key map "\C-c\C-c" 'comment-region)
    (define-key map (kbd "C-c C-t") 'inferior-haskell-type)
    (define-key map (kbd "C-c C-i") 'inferior-haskell-info)
    (define-key map (kbd "C-c M-.") 'inferior-haskell-find-definition)
    (define-key map (kbd "C-c C-d") 'inferior-haskell-find-haddock)
    (define-key map [?\C-c ?\C-v] 'haskell-check)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Editing-specific commands
    (define-key map (kbd "C-c C-.") 'haskell-mode-format-imports)
    (define-key map [remap delete-indentation] 'haskell-delete-indentation)

    map)
  "Keymap used in Haskell mode.")

(easy-menu-define haskell-mode-menu haskell-mode-map
  "Menu for the Haskell major mode."
  ;; Suggestions from Pupeno <pupeno@pupeno.com>:
  ;; - choose the underlying interpreter
  ;; - look up docs
  `("Haskell"
    ["Indent line" indent-according-to-mode]
    ["Indent region" indent-region mark-active]
    ["(Un)Comment region" comment-region mark-active]
    "---"
    ["Start interpreter" switch-to-haskell]
    ["Load file" inferior-haskell-load-file]
    "---"
    ["Load tidy core" ghc-core-create-core]
    "---"
    ,(if (default-boundp 'eldoc-documentation-function)
         ["Doc mode" eldoc-mode
          :style toggle :selected (bound-and-true-p eldoc-mode)]
       ["Doc mode" haskell-doc-mode
        :style toggle :selected (and (boundp 'haskell-doc-mode) haskell-doc-mode)])
    ["Customize" (customize-group 'haskell)]
    ))

;; Syntax table.
(defvar haskell-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\  " " table)
    (modify-syntax-entry ?\t " " table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\' "\'" table)
    (modify-syntax-entry ?_  "w" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[  "(]" table)
    (modify-syntax-entry ?\]  ")[" table)

    (cond ((featurep 'xemacs)
           ;; I don't know whether this is equivalent to the below
           ;; (modulo nesting).  -- fx
           (modify-syntax-entry ?{  "(}5" table)
           (modify-syntax-entry ?}  "){8" table)
           (modify-syntax-entry ?-  "_ 1267" table))
          (t
           ;; In Emacs 21, the `n' indicates that they nest.
           ;; The `b' annotation is actually ignored because it's only
           ;; meaningful on the second char of a comment-starter, so
           ;; on Emacs 20 and before we get wrong results.  --Stef
           (modify-syntax-entry ?\{  "(}1nb" table)
           (modify-syntax-entry ?\}  "){4nb" table)
           (modify-syntax-entry ?-  "_ 123" table)))
    (modify-syntax-entry ?\n ">" table)

    (let (i lim)
      (map-char-table
       (lambda (k v)
         (when (equal v '(1))
           ;; The current Emacs 22 codebase can pass either a char
           ;; or a char range.
           (if (consp k)
               (setq i (car k)
                     lim (cdr k))
             (setq i k
                   lim k))
           (while (<= i lim)
             (when (> i 127)
               (modify-syntax-entry i "_" table))
             (setq i (1+ i)))))
       (standard-syntax-table)))

    (modify-syntax-entry ?\` "$`" table)
    (modify-syntax-entry ?\\ "\\" table)
    (mapc (lambda (x)
            (modify-syntax-entry x "_" table))
          ;; Some of these are actually OK by default.
          "!#$%&*+./:<=>?@^|~")
    (unless (featurep 'mule)
      ;; Non-ASCII syntax should be OK, at least in Emacs.
      (mapc (lambda (x)
              (modify-syntax-entry x "_" table))
            (concat "¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿"
                    "×÷"))
      (mapc (lambda (x)
              (modify-syntax-entry x "w" table))
            (concat "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ"
                    "ØÙÚÛÜÝÞß"
                    "àáâãäåæçèéêëìíîïðñòóôõö"
                    "øùúûüýþÿ")))
    table)
  "Syntax table used in Haskell mode.")

(defun haskell-ident-at-point ()
  "Return the identifier under point, or nil if none found.
May return a qualified name."
  (let ((reg (haskell-ident-pos-at-point)))
    (when reg
      (buffer-substring-no-properties (car reg) (cdr reg)))))

(defun haskell-ident-pos-at-point ()
  "Return the span of the identifier under point, or nil if none found.
May return a qualified name."
  (save-excursion
    ;; Skip whitespace if we're on it.  That way, if we're at "map ", we'll
    ;; see the word "map".
    (if (and (not (eobp))
             (eq ?  (char-syntax (char-after))))
        (skip-chars-backward " \t"))

    (let ((case-fold-search nil))
      (multiple-value-bind (start end)
          (if (looking-at "\\s_")
              (list (progn (skip-syntax-backward "_") (point))
                    (progn (skip-syntax-forward "_") (point)))
            (list
             (progn (skip-syntax-backward "w'")
                    (skip-syntax-forward "'") (point))
             (progn (skip-syntax-forward "w'") (point))))
        ;; If we're looking at a module ID that qualifies further IDs, add
        ;; those IDs.
        (goto-char start)
        (while (and (looking-at "[[:upper:]]") (eq (char-after end) ?.)
                    ;; It's a module ID that qualifies further IDs.
                    (goto-char (1+ end))
                    (save-excursion
                      (when (not (zerop (skip-syntax-forward
                                         (if (looking-at "\\s_") "_" "w'"))))
                        (setq end (point))))))
        ;; If we're looking at an ID that's itself qualified by previous
        ;; module IDs, add those too.
        (goto-char start)
        (if (eq (char-after) ?.) (forward-char 1)) ;Special case for "."
        (while (and (eq (char-before) ?.)
                    (progn (forward-char -1)
                           (not (zerop (skip-syntax-backward "w'"))))
                    (skip-syntax-forward "'")
                    (looking-at "[[:upper:]]"))
          (setq start (point)))
        ;; This is it.
        (cons start end)))))

(defun haskell-delete-indentation (&optional arg)
  "Like `delete-indentation' but ignoring Bird-style \">\"."
  (interactive "*P")
  (let ((fill-prefix (or fill-prefix (if (eq haskell-literate 'bird) ">"))))
    (delete-indentation arg)))

;; Various mode variables.

(defcustom haskell-mode-hook nil
  "Hook run after entering `haskell-mode'.

Some of the supported modules that can be activated via this hook:

   `haskell-decl-scan', Graeme E Moss
     Scans top-level declarations, and places them in a menu.

   `haskell-doc', Hans-Wolfgang Loidl
     Echoes types of functions or syntax of keywords when the cursor is idle.

   `haskell-indentation', Kristof Bastiaensen
     Intelligent semi-automatic indentation Mk2

   `haskell-indent', Guy Lapalme
     Intelligent semi-automatic indentation.

   `haskell-simple-indent', Graeme E Moss and Heribert Schuetz
     Simple indentation.

Module X is activated using the command `turn-on-X'.  For example,
`haskell-doc' is activated using `turn-on-haskell-doc'.
For more information on a specific module, see the help for its `X-mode'
function.  Some modules can be deactivated using `turn-off-X'.

See Info node `(haskell-mode)haskell-mode-hook' for more details.

Warning: do not enable more than one of the three indentation
modes. See Info node `(haskell-mode)indentation' for more
details."
  :type 'hook
  :group 'haskell
  :link '(info-link "(haskell-mode)haskell-mode-hook")
  :link '(function-link haskell-mode)
  :options `(capitalized-words-mode
             imenu-add-menubar-index
             turn-on-eldoc-mode
             turn-on-haskell-decl-scan
             turn-on-haskell-doc
             turn-on-haskell-indent
             turn-on-haskell-indentation
             turn-on-haskell-simple-indent
             turn-on-haskell-unicode-input-method))

(defvar eldoc-print-current-symbol-info-function)

;; For compatibility with Emacs < 24, derive conditionally
(defalias 'haskell-parent-mode
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

;; The main mode functions
;;;###autoload
(define-derived-mode haskell-mode haskell-parent-mode "Haskell"
  "Major mode for editing Haskell programs.

See also Info node `(haskell-mode)Getting Started' for more
information about this mode.

\\<haskell-mode-map>
Literate scripts are supported via `literate-haskell-mode'.
The variable `haskell-literate' indicates the style of the script in the
current buffer.  See the documentation on this variable for more details.

Use `haskell-version' to find out what version of Haskell mode you are
currently using.

Additional Haskell mode modules can be hooked in via `haskell-mode-hook';
see documentation for that variable for more details."
  :group 'haskell
  (set (make-local-variable 'paragraph-start) (concat "^$\\|" page-delimiter))
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  (set (make-local-variable 'fill-paragraph-function) 'haskell-fill-paragraph)
  ;; (set (make-local-variable 'adaptive-fill-function) 'haskell-adaptive-fill)
  (set (make-local-variable 'adaptive-fill-mode) nil)
  (set (make-local-variable 'comment-start) "-- ")
  (set (make-local-variable 'comment-padding) 0)
  (set (make-local-variable 'comment-start-skip) "[-{]-[ \t]*")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-end-skip) "[ \t]*\\(-}\\|\\s>\\)")
  (set (make-local-variable 'parse-sexp-ignore-comments) nil)
  (set (make-local-variable 'indent-line-function) 'haskell-mode-suggest-indent-choice)
  ;; Set things up for eldoc-mode.
  (set (make-local-variable 'eldoc-documentation-function)
       'haskell-doc-current-info)
  ;; Set things up for imenu.
  (set (make-local-variable 'imenu-create-index-function)
       'haskell-ds-create-imenu-index)
  ;; Set things up for font-lock.
  (set (make-local-variable 'font-lock-defaults)
       '(haskell-font-lock-choose-keywords
         nil nil ((?\' . "w") (?_  . "w")) nil
         (font-lock-syntactic-keywords
          . haskell-font-lock-choose-syntactic-keywords)
         (font-lock-syntactic-face-function
          . haskell-syntactic-face-function)
         ;; Get help from font-lock-syntactic-keywords.
         (parse-sexp-lookup-properties . t)))
  ;; Haskell's layout rules mean that TABs have to be handled with extra care.
  ;; The safer option is to avoid TABs.  The second best is to make sure
  ;; TABs stops are 8 chars apart, as mandated by the Haskell Report.  --Stef
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'tab-width) 8)
  ;; dynamic abbrev support: recognize Haskell identifiers
  ;; Haskell is case-sensitive language
  (set (make-local-variable 'dabbrev-case-fold-search) nil)
  (set (make-local-variable 'dabbrev-case-distinction) nil)
  (set (make-local-variable 'dabbrev-case-replace) nil)
  (set (make-local-variable 'dabbrev-abbrev-char-regexp) "\\sw\\|[.]")
  (setq haskell-literate nil)
  (add-hook 'before-save-hook 'haskell-mode-before-save-handler nil t)
  (add-hook 'after-save-hook 'haskell-mode-after-save-handler nil t)
  )

(defun haskell-fill-paragraph (justify)
  (save-excursion
    ;; Fill paragraph should only work in comments.
    ;; The -- comments are handled properly by default
    ;; The {- -} comments need some extra love.
    (let* ((syntax-values (syntax-ppss))
           (comment-num (nth 4 syntax-values)))
      (cond
       ((eq t comment-num)
        ;; standard fill works wonders inside a non-nested comment
        (fill-comment-paragraph justify))

       ((integerp comment-num)
        ;; we are in a nested comment. lets narrow to comment content
        ;; and use plain paragraph fill for that
        (let* ((comment-start-point (nth 8 syntax-values))
               (comment-end-point
                (save-excursion
                  (re-search-forward "-}" (point-max) t comment-num)
                  (point)))
               (fill-paragraph-handle-comment nil))
          (save-restriction
            (narrow-to-region (+ 2 comment-start-point) (- comment-end-point 2))
            (fill-paragraph justify))))
       ((eolp)
        ;; do nothing outside of a comment
        t)
       (t
        ;; go to end of line and try again
        (end-of-line)
        (haskell-fill-paragraph justify))))))


;; (defun haskell-adaptive-fill ()
;;   ;; We want to use "--  " as the prefix of "-- |", etc.
;;   (let* ((line-end (save-excursion (end-of-line) (point)))
;;          (line-start (point)))
;;     (save-excursion
;;       (unless (in-comment)
;;         ;; Try to find the start of a comment. We only fill comments.
;;         (search-forward-regexp comment-start-skip line-end t))
;;       (when (in-comment)
;;         (let ();(prefix-start (point)))
;;           (skip-syntax-forward "^w")
;;           (make-string (- (point) line-start) ?\s))))))



;;;###autoload
(define-derived-mode literate-haskell-mode haskell-mode "LitHaskell"
  "As `haskell-mode' but for literate scripts."
  (setq haskell-literate
        (save-excursion
          (goto-char (point-min))
          (cond
           ((re-search-forward "^\\\\\\(begin\\|end\\){code}$" nil t) 'tex)
           ((re-search-forward "^>" nil t) 'bird)
           (t haskell-literate-default))))
  (if (eq haskell-literate 'bird)
      ;; fill-comment-paragraph isn't much use there, and even gets confused
      ;; by the syntax-table text-properties we add to mark the first char
      ;; of each line as a comment-starter.
      (set (make-local-variable 'fill-paragraph-handle-comment) nil))
  (set (make-local-variable 'mode-line-process)
       '("/" (:eval (symbol-name haskell-literate)))))

;;;###autoload(add-to-list 'auto-mode-alist        '("\\.\\(?:[gh]s\\|hi\\)\\'" . haskell-mode))
;;;###autoload(add-to-list 'auto-mode-alist        '("\\.l[gh]s\\'" . literate-haskell-mode))
;;;###autoload(add-to-list 'interpreter-mode-alist '("runghc" . haskell-mode))
;;;###autoload(add-to-list 'interpreter-mode-alist '("runhaskell" . haskell-mode))

(defcustom haskell-hoogle-command
  (if (executable-find "hoogle") "hoogle")
  "Name of the command to use to query Hoogle.
If nil, use the Hoogle web-site."
  :group 'haskell
  :type '(choice (const :tag "Use Web-site" nil)
                 string))

;;;###autoload
(defun haskell-hoogle (query)
  "Do a Hoogle search for QUERY."
  (interactive
   (let ((def (haskell-ident-at-point)))
     (if (and def (symbolp def)) (setq def (symbol-name def)))
     (list (read-string (if def
                            (format "Hoogle query (default %s): " def)
                          "Hoogle query: ")
                        nil nil def))))
  (if (null haskell-hoogle-command)
      (browse-url (format "http://haskell.org/hoogle/?q=%s" query))
    (lexical-let ((temp-buffer (help-buffer)))
      (with-output-to-temp-buffer temp-buffer
        (with-current-buffer standard-output
          (let ((hoogle-process
                 (start-process "hoogle" (current-buffer) haskell-hoogle-command query))
                (scroll-to-top
                 (lambda (process event)
                   (set-window-start (get-buffer-window temp-buffer t) 1))))
            (set-process-sentinel hoogle-process scroll-to-top)))))))

;;;###autoload
(defalias 'hoogle 'haskell-hoogle)

;;;###autoload
(defun haskell-hayoo (query)
  "Do a Hayoo search for QUERY."
  (interactive
   (let ((def (haskell-ident-at-point)))
     (if (and def (symbolp def)) (setq def (symbol-name def)))
     (list (read-string (if def
                            (format "Hayoo query (default %s): " def)
                          "Hayoo query: ")
                        nil nil def))))
  (browse-url (format "http://holumbus.fh-wedel.de/hayoo/hayoo.html?query=%s" query)))

;;;###autoload
(defalias 'hayoo 'haskell-hayoo)

(defcustom haskell-check-command "hlint"
  "*Command used to check a Haskell file."
  :group 'haskell
  :type '(choice (const "hlint")
                 (const "ghc -fno-code")
                 (string :tag "Other command")))

(defcustom haskell-stylish-on-save nil
  "Whether to run stylish-haskell on the buffer before saving."
  :group 'haskell
  :type 'boolean)

(defcustom haskell-tags-on-save nil
  "Generate tags via hasktags after saving."
  :group 'haskell
  :type 'boolean)

(defvar haskell-saved-check-command nil
  "Internal use.")

(defcustom haskell-indent-spaces 2
  "Number of spaces to indent inwards.")

;; Like Python.  Should be abstracted, sigh.
(defun haskell-check (command)
  "Check a Haskell file (default current buffer's file).
Runs COMMAND, a shell command, as if by `compile'.
See `haskell-check-command' for the default."
  (interactive
   (list (read-string "Checker command: "
                      (or haskell-saved-check-command
                          (concat haskell-check-command " "
                                  (let ((name (buffer-file-name)))
                                    (if name
                                        (file-name-nondirectory name))))))))
  (setq haskell-saved-check-command command)
  (save-some-buffers (not compilation-ask-about-save) nil)
  (compilation-start command))

(defun haskell-flymake-init ()
  "Flymake init function for Haskell.
To be added to `flymake-init-create-temp-buffer-copy'."
  (let ((checker-elts (and haskell-saved-check-command
                           (split-string haskell-saved-check-command))))
    (list (car checker-elts)
          (append (cdr checker-elts)
                  (list (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))))))

(add-to-list 'flymake-allowed-file-name-masks '("\\.l?hs\\'" haskell-flymake-init))

(defun haskell-mode-suggest-indent-choice ()
  "Ran when the user tries to indent in the buffer but no indentation mode has been selected.
Brings up the documentation for haskell-mode-hook."
  (describe-variable 'haskell-mode-hook))

(defun haskell-mode-format-imports ()
  "Format the imports by aligning and sorting them."
  (interactive)
  (let ((col (current-column)))
    (haskell-sort-imports)
    (haskell-align-imports)
    (goto-char (+ (line-beginning-position)
                  col))))

(defun haskell-mode-message-line (str)
  "Message only one line, multiple lines just disturbs the programmer."
  (let ((lines (split-string str "\n" t)))
    (when (and (car lines) (stringp (car lines)))
      (message "%s"
               (concat (car lines)
                       (if (and (cdr lines) (stringp (cadr lines)))
                           (format " [ %s .. ]" (haskell-string-take (haskell-trim (cadr lines)) 10))
                         ""))))))

(defun haskell-mode-contextual-space ()
  "Contextually do clever stuff when hitting space."
  (interactive)
  (if (not (haskell-session-maybe))
      (self-insert-command 1)
    (cond ((save-excursion (forward-word -1)
                           (looking-at "^import$"))
           (insert " ")
           (let ((module (ido-completing-read "Module: " (haskell-session-all-modules))))
             (insert module)
             (haskell-mode-format-imports)))
          ((not (string= "" (save-excursion (forward-char -1) (haskell-ident-at-point))))
           (let ((ident (save-excursion (forward-char -1) (haskell-ident-at-point))))
             (insert " ")
             (haskell-process-do-try-info ident)))
          (t (insert " ")))))

(defun haskell-mode-before-save-handler ()
  "Function that will be called before buffer's saving."
  )

(defun haskell-mode-after-save-handler ()
  "Function that will be called after buffer's saving."
  (when haskell-tags-on-save
    (ignore-errors (when (and (boundp 'haskell-session) haskell-session)
                     (haskell-process-generate-tags))))
  (when haskell-stylish-on-save
    (ignore-errors (haskell-mode-stylish-buffer)))
  (let ((before-save-hook '())
        (after-save-hook '()))
    (basic-save-buffer))
  )

(defun haskell-mode-buffer-apply-command (cmd)
  "Execute shell command CMD with current buffer as input and
replace the whole buffer with the output. If CMD fails the buffer
remains unchanged."
  (set-buffer-modified-p t)
  (let* ((chomp (lambda (str)
                  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'" str)
                    (setq str (replace-match "" t t str)))
                  str))
         (errout (lambda (fmt &rest args)
                   (let* ((warning-fill-prefix "    "))
                     (display-warning cmd (apply 'format fmt args) :warning))))
         (filename (buffer-file-name (current-buffer)))
         (cmd-prefix (replace-regexp-in-string " .*" "" cmd))
         (tmp-file (make-temp-file cmd-prefix))
         (err-file (make-temp-file cmd-prefix))
         (default-directory (if (and (boundp 'haskell-session)
                                     haskell-session)
                                (haskell-session-cabal-dir haskell-session)
                              default-directory))
         (errcode (with-temp-file tmp-file
                    (call-process cmd filename
                                  (list (current-buffer) err-file) nil)))
         (stderr-output
          (with-temp-buffer
            (insert-file-contents err-file)
            (funcall chomp (buffer-substring-no-properties (point-min) (point-max)))))
         (stdout-output
          (with-temp-buffer
            (insert-file-contents tmp-file)
            (buffer-substring-no-properties (point-min) (point-max)))))
    (if (string= "" stderr-output)
        (if (string= "" stdout-output)
            (funcall errout
                     "Error: %s produced no output, leaving buffer alone" cmd)
          (save-restriction
            (widen)
            ;; command successful, insert file with replacement to preserve
            ;; markers.
            (insert-file-contents tmp-file nil nil nil t)))
      ;; non-null stderr, command must have failed
      (funcall errout "%s failed: %s" cmd stderr-output)
      )
    (delete-file tmp-file)
    (delete-file err-file)
    ))

(defun haskell-mode-stylish-buffer ()
  "Apply stylish-haskell to the current buffer."
  (interactive)
  (let ((column (current-column))
        (line (line-number-at-pos)))
    (haskell-mode-buffer-apply-command "stylish-haskell")
    (goto-char (point-min))
    (forward-line (1- line))
    (goto-char (+ column (point)))))

(defun haskell-mode-tag-find (&optional next-p)
  "The tag find function, specific for the particular session."
  (interactive "P")
  (cond
   ((eq 'font-lock-string-face
        (get-text-property (point) 'face))
    (let* ((string (save-excursion
                    (buffer-substring-no-properties
                     (1+ (search-backward-regexp "\"" (line-beginning-position) nil 1))
                     (1- (progn (forward-char 1)
                                (search-forward-regexp "\"" (line-end-position) nil 1))))))
           (fp (expand-file-name string
                                  (haskell-session-cabal-dir (haskell-session)))))
      (find-file
       (read-file-name
        ""
        fp
        fp))))
   (t (let ((tags-file-name (haskell-session-tags-filename (haskell-session)))
            (tags-revert-without-query t)
            (ident (haskell-ident-at-point)))
        (when (not (string= "" (haskell-trim ident)))
          (cond ((file-exists-p tags-file-name)
                 (find-tag ident next-p))
                (t (haskell-process-generate-tags ident))))))))

;; From Bryan O'Sullivan's blog:
;; http://www.serpentine.com/blog/2007/10/09/using-emacs-to-insert-scc-annotations-in-haskell-code/
(defun haskell-mode-insert-scc-at-point ()
  "Insert an SCC annotation at point."
  (interactive)
  (if (or (looking-at "\\b\\|[ \t]\\|$") (and (not (bolp))
                                              (save-excursion
                                                (forward-char -1)
                                                (looking-at "\\b\\|[ \t]"))))
      (let ((space-at-point (looking-at "[ \t]")))
        (unless (and (not (bolp)) (save-excursion
                                    (forward-char -1)
                                    (looking-at "[ \t]")))
          (insert " "))
        (insert "{-# SCC \"\" #-}")
        (unless space-at-point
          (insert " "))
        (forward-char (if space-at-point -5 -6)))
    (error "Not over an area of whitespace")))

;; Also Bryan O'Sullivan's.
(defun haskell-mode-kill-scc-at-point ()
  "Kill the SCC annotation at point."
  (interactive)
  (save-excursion
    (let ((old-point (point))
          (scc "\\({-#[ \t]*SCC \"[^\"]*\"[ \t]*#-}\\)[ \t]*"))
      (while (not (or (looking-at scc) (bolp)))
        (forward-char -1))
      (if (and (looking-at scc)
               (<= (match-beginning 1) old-point)
               (> (match-end 1) old-point))
          (kill-region (match-beginning 0) (match-end 0))
        (error "No SCC at point")))))

(defun haskell-rgrep (&optional prompt)
  "Grep the effective project for the symbol at point. Very
useful for codebase navigation. Prompts for an arbitrary regexp
given a prefix arg."
  (interactive "P")
  (let ((sym (if prompt
                 (read-from-minibuffer "Look for: ")
               (haskell-ident-at-point))))
    (rgrep sym
           "*.hs" ;; TODO: common Haskell extensions.
           (haskell-session-current-dir (haskell-session)))))

(defun haskell-fontify-as-mode (text mode)
  "Fontify TEXT as MODE, returning the fontified text."
  (with-temp-buffer
    (funcall mode)
    (insert text)
    (font-lock-fontify-buffer)
    (buffer-substring (point-min) (point-max))))

(defun haskell-guess-module-name ()
  "Guess the current module name of the buffer."
  (interactive)
  (let ((components (loop for part
                          in (reverse (split-string (buffer-file-name) "/"))
                          while (let ((case-fold-search nil))
                                  (string-match "^[A-Z]+" part))
                          collect (replace-regexp-in-string "\\.l?hs$" "" part))))
    (mapconcat 'identity (reverse components) ".")))


;; Provide ourselves:

(provide 'haskell-mode)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; haskell-mode.el ends here
