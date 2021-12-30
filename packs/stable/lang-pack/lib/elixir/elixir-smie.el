;;; elixir-smie.el --- Structural syntax support for elixir-mode

;; Copyright 2011-2015 secondplanet
;;           2013-2015 Samuel Tonini, Matt DeBoard, Andreas Fuchs

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

;;  Structural syntax support for elixir-mode

;;; Code:

(require 'smie)           ; Simple minded indentation engine
(require 'cl-lib)         ; `cl-flet'

;; HACK: Patch for Emacs 24.3 smie that fix
;; https://github.com/elixir-editors/emacs-elixir/issues/107.
;;
;; defadvice is used to change the behavior only for elixir-mode.
;; Definition of advice is a definition of corresponding function
;; in Emacs 24.4.
(when (and (= 24 emacs-major-version)
           (= 3  emacs-minor-version))
  (defadvice smie-rule-parent (around elixir-mode-patch activate)
    (if (not (eq major-mode 'elixir-mode))
        (progn ad-do-it)
      (setq ad-return-value
            (save-excursion
              (goto-char (cadr (smie-indent--parent)))
              (cons 'column
                    (+ (or offset 0)
                       (smie-indent-virtual)))))))

  (defadvice smie-indent-comment (around elixir-mode-patch activate)
    (if (not (eq major-mode 'elixir-mode))
        (progn ad-do-it)
      (setq ad-return-value
            (and (smie-indent--bolp)
                 (let ((pos (point)))
                   (save-excursion
                     (beginning-of-line)
                     (and (re-search-forward comment-start-skip (line-end-position) t)
                          (eq pos (or (match-end 1) (match-beginning 0))))))
                 (save-excursion
                   (forward-comment (point-max))
                   (skip-chars-forward " \t\r\n")
                   (unless
                       (save-excursion
                         (let ((next (funcall smie-forward-token-function)))
                           (or (if (zerop (length next))
                                   (or (eobp) (eq (car (syntax-after (point))) 5)))
                               (rassoc next smie-closer-alist))))
                     (smie-indent-calculate))))))))

;; In Emacs 27, ppss became a structure and has proper accessors.

(defalias 'elixir-ppss-depth
  (if (<= 27 emacs-major-version)
      'ppss-depth
    (lambda (parse-data) (nth 0 parse-data))))

(defalias 'elixir-ppss-innermost-start
  (if (<= 27 emacs-major-version)
      'ppss-innermost-start
    (lambda (parse-data) (nth 1 parse-data))))

(defalias 'elixir-ppss-last-complete-sexp-start
  (if (<= 27 emacs-major-version)
      'ppss-last-complete-sexp-start
    (lambda (parse-data) (nth 2 parse-data))))

(defalias 'elixir-ppss-string-terminator
  (if (<= 27 emacs-major-version)
      'ppss-string-terminator
    (lambda (parse-data) (nth 3 parse-data))))

(defalias 'elixir-ppss-comment-depth
  (if (<= 27 emacs-major-version)
      'ppss-comment-depth
    (lambda (parse-data) (nth 4 parse-data))))

(defalias 'elixir-ppss-comment-or-string-start
  (if (<= 27 emacs-major-version)
      'ppss-comment-or-string-start
    (lambda (parse-data) (nth 8 parse-data))))

(defun elixir-smie-looking-around (back at)
  "Check if looking backwards at BACK and forward at AT."
  (and (looking-at-p at) (looking-back back)))

;; Declare variable that we need from the smie package
(defvar smie--parent)

(defvar elixir-smie-verbose-p nil
  "Emit context information about the current syntax state.")

(defvar elixir-mode-syntax-table
  (let ((table (make-syntax-table)))

    ;; Note that ?_ might be better as class "_", but either seems to
    ;; work:
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?/ "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?? "w" table)
    (modify-syntax-entry ?~ "w" table)
    (modify-syntax-entry ?! "_" table)
    (modify-syntax-entry ?' "\"'" table)
    (modify-syntax-entry ?\" "\"\"" table)
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?: "_" table)
    (modify-syntax-entry ?@ "_" table)
    table)
  "Elixir mode syntax table.")

(defconst elixir-smie-grammar
  (smie-prec2->grammar
   (smie-merge-prec2s
    (smie-bnf->prec2
     '((id)
       (statements (statement)
                   (statement ";" statements))
       (statement (non-block-expr "fn" match-statements "end")
                  (non-block-expr "do" statements "end")
                  ("if" non-block-expr "do" statements "else" statements "end")
                  ("if" non-block-expr "do" statements "end")
                  ("if" non-block-expr "COMMA" "do:" non-block-expr)
                  ("if" non-block-expr "COMMA"
                   "do:" non-block-expr "COMMA"
                   "else:" non-block-expr)
                  ("try" "do" statements "after" statements "end")
                  ("try" "do" statements "catch" match-statements "end")
                  ("try" "do" statements "rescue" match-statements "end")
                  ("try" "do" statements "end")
                  ("case" non-block-expr "do" match-statements "end")
                  ("for" non-block-expr "COMMA" "do:" non-block-expr)
		  ("for" non-block-expr "COMMA" "into:" non-block-expr "COMMA" "do" statements "end")
		  ("for" non-block-expr "COMMA" "into:" non-block-expr "COMMA" "do:" non-block-expr)
		  ("with" non-block-expr "do" statements "else" statements "end")
		  ("with" non-block-expr "do:" non-block-expr "COMMA" "else:" non-block-expr))
       (non-block-expr (non-block-expr "OP" non-block-expr)
                       ("(" non-block-expr ")")
                       ("{" non-block-expr "}")
                       ("[" non-block-expr "]")
                       ("STRING"))
       (match-statements (match-statement "MATCH-STATEMENT-DELIMITER"
                                          match-statements)
                         (match-statement))
       (match-statement (non-block-expr "->" statements)))
     '((assoc "if" "do:" "else:")
       (assoc "COMMA")
       (left "OP")))

    (smie-precs->prec2
     '((left "||")
       (left "&&")
       (nonassoc "=~" "===" "!==" "==" "!=" "<=" ">=" "<" ">")
       (left "+" "-" "<<<" ">>>" "^^^" "~~~" "&&&" "|||")
       (left "*" "/"))))))

(defvar elixir-smie--operator-regexp
  (rx (or "<<<" ">>>" "^^^" "~~~" "&&&" "|||" "===" "!==" "==" "!=" "<="
          "=" ">=" "<" ">" "&&" "||" "<>" "++" "--" "//" "/>" "=~" "|>")))

(defvar elixir-smie--binary-sequence-regexp
  (rx (or "<<" ">>")))

(defvar elixir-smie--block-operator-regexp
  (rx "->" (0+ nonl)))

(defvar elixir-smie--oneline-def-operator-regexp
  (rx "do:" (0+ nonl)))

(defvar elixir-smie--spaces-til-eol-regexp
  (rx (and (1+ space) eol))
  "Regex representing one or more whitespace characters concluding with eol.")

(defvar elixir-smie--comment-regexp
  (rx (and (0+ space) "#" (0+ not-newline)))
  "Regex matching comments.")

(defvar elixir-smie-indent-basic 2)

(defmacro elixir-smie-debug (message &rest format-args)
  `(progn
     (when elixir-smie-verbose-p
       (message (format ,message ,@format-args)))
     nil))

(defun elixir-smie--implicit-semi-p ()
  (not (or (memq (char-before) '(?\{ ?\[))
           (looking-back elixir-smie--operator-regexp (- (point) 3) t))))

(defun elixir-smie-current-line-contains-built-in-keyword-p ()
  "Return non-nil if the current line contains built in keywords with a \".\"."
  (save-excursion
    (beginning-of-line)
    (looking-at ".+\\.\\(case\\|try\\|if\\|rescue\\)")))

(defun elixir-smie-last-line-end-with-block-operator-p ()
  "Return non-nil if the previous line ends with a `->' operator."
  (save-excursion
    (forward-line -1)
    (move-end-of-line 1)
    (looking-back elixir-smie--block-operator-regexp (- (point) 3) t)))

(defun elixir-smie-last-line-start-with-block-operator-p ()
  (save-excursion
    (forward-line -1)
    (beginning-of-line)
    (looking-at "^\s+->.+$")))

(defun elixir-smie-current-line-start-with-pipe-operator-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "^\s*|>.+$")))

(defun elixir-smie-last-line-is-assignment-p ()
  (save-excursion
    (forward-line -1)
    (beginning-of-line)
    (looking-at "^.+=.+$")))

(defun elixir-smie-last-line-start-with-pipe-operator-p ()
  (save-excursion
    (forward-line -1)
    (beginning-of-line)
    (looking-at "^\s*|>.+$")))

(defun elixir-smie-line-starts-with-do-colon-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "^\s+do:")))

(defun elixir-smie--semi-ends-match ()
  "Return non-nil if the current line concludes a match block."
  (when (not (eobp))
    (save-excursion
      ;; Warning: Recursion.
      ;; This is easy though.

      ;; 1. If we're at a blank line, move forward a character. This takes us to
      ;;    the next line.
      ;; 2. If we're not at the end of the buffer, call this function again.
      ;;    (Otherwise, return nil.)

      ;; The point here is that we want to treat blank lines as a single semi-
      ;; colon when it comes to detecting the end of match statements. This could
      ;; also be handled by a `while' expression or some other looping mechanism.
      (cl-flet ((self-call ()
                           (if (< (point) (point-max))
                               (elixir-smie--semi-ends-match)
                             nil)))
        (cond
         ((and (eolp) (bolp))
          (forward-char)
          (self-call))
         ((looking-at elixir-smie--spaces-til-eol-regexp)
          (forward-char)
          (self-call))
         ;; And if we're NOT on a blank line, move to the end of the line, and see
         ;; if we're looking back at a block operator.
         (t (move-end-of-line 1)
            (and (looking-back elixir-smie--block-operator-regexp)
                 (not (looking-back ".+fn.+")))))))))

(defun elixir-smie--same-line-as-parent (parent-pos child-pos)
  "Return non-nil if CHILD-POS is on same line as PARENT-POS."
  (= (line-number-at-pos parent-pos) (line-number-at-pos child-pos)))

(defun elixir-smie-forward-token ()
  (cond
   ;; If there is nothing but whitespace between the last token and eol, emit
   ;; a semicolon.
   ((looking-at elixir-smie--spaces-til-eol-regexp)
    (goto-char (match-end 0))
    ";")
   ((and (or (looking-at elixir-smie--comment-regexp)
             (looking-at "[\n#]"))
         (elixir-smie--implicit-semi-p))
    (if (eolp) (forward-char 1) (forward-comment 1))
    ;; Note: `elixir-smie--semi-ends-match' will be called when the point is at
    ;; the beginning of a new line. Keep that in mind.
    (if (elixir-smie--semi-ends-match)
        "MATCH-STATEMENT-DELIMITER"
      (if (and (looking-at ".+,$")
               (not (> (elixir-ppss-depth (syntax-ppss)) 0)))
          "COMMA"
        ";")))
   ((looking-at elixir-smie--block-operator-regexp)
    (goto-char (match-end 0))
    "->")
   ((looking-at elixir-smie--operator-regexp)
    (goto-char (match-end 0))
    "OP")
   (t
    (let ((token (smie-default-forward-token)))
      (unless (or (elixir-smie-empty-string-p token)
		  (elixir-smie--at-dot-call))
	token)))))

(defun elixir-smie--at-dot-call ()
  (and (eq ?w (char-syntax (following-char)))
       (eq (char-before) ?.)
       (not (eq (char-before (1- (point))) ?.))))

(defun elixir-smie-backward-token ()
  (let ((pos (point)))
    (forward-comment (- (point)))
    (cond
     ((and (> pos (line-end-position))
           (elixir-smie--implicit-semi-p))
      (if (elixir-smie--semi-ends-match)
          "MATCH-STATEMENT-DELIMITER"
        (if (and (looking-back ",$" (- (point) 3) t)
                 (not (> (elixir-ppss-depth (syntax-ppss)) 0)))
	    "COMMA"
	  ";")))
     ((looking-back elixir-smie--block-operator-regexp (- (point) 3) t)
      (goto-char (match-beginning 0))
      "->")
     ((looking-back elixir-smie--binary-sequence-regexp (- (point) 3) t)
      (goto-char (match-beginning 0))
      "OP")
     ((looking-back elixir-smie--operator-regexp (- (point) 3) t)
      (goto-char (match-beginning 0))
      "OP")
     (t (let ((token (smie-default-backward-token)))
          (unless (or (elixir-smie-empty-string-p token)
                      (elixir-smie--at-dot-call))
            token))))))

(defun verbose-elixir-smie-rules (kind token)
  (let ((value (elixir-smie-rules kind token)))
    (elixir-smie-debug "%s '%s'; sibling-p:%s parent:%s prev-is-OP:%s hanging:%s == %s" kind token
                       (ignore-errors (smie-rule-sibling-p))
                       (ignore-errors smie--parent)
                       (ignore-errors (smie-rule-prev-p "OP"))
                       (ignore-errors (smie-rule-hanging-p))
                       value)
    value))

(defun elixir-smie-rules (kind token)
  (pcase (cons kind token)
    (`(:list-intro . ";")
     -4)
    (`(:list-intro . nil)
     -4)
    (`(:elem . args)
     -4)
    (`(:before . "COMMA")
     (cond
      ((and (smie-rule-parent-p "with")
            (smie-rule-hanging-p))
       (smie-rule-parent 5))
      ((and (smie-rule-parent-p ";")
            (smie-rule-hanging-p))
       (smie-rule-parent))
      ((and (smie-rule-parent-p "COMMA")
            (smie-rule-hanging-p))
       (if (save-excursion
	     (forward-line 1)
	     (move-beginning-of-line 1)
	     (looking-at "^.+do:.+$"))
	   (smie-rule-parent -5)
	 (smie-rule-parent)))
      ((and (smie-rule-parent-p "COMMA")
            (not (smie-rule-hanging-p)))
       (smie-rule-parent elixir-smie-indent-basic))
      ((smie-rule-parent-p "(")
       (smie-rule-parent))
      ((smie-rule-parent-p "if")
       (smie-rule-parent))
      ((smie-rule-parent-p "->")
       (smie-rule-parent))))
    (`(:after . "COMMA")
     (cond
      ((and (smie-rule-parent-p ";")
            (smie-rule-hanging-p))
       (smie-rule-parent elixir-smie-indent-basic))
      ((and (smie-rule-parent-p "with")
            (smie-rule-hanging-p))
       (smie-rule-parent 5))
      ((and (smie-rule-parent-p "{")
            (smie-rule-hanging-p))
       (smie-rule-parent elixir-smie-indent-basic))
      ((and (smie-rule-parent-p "[")
            (smie-rule-hanging-p))
       0)
      ((smie-rule-parent-p "COMMA")
       (smie-rule-parent elixir-smie-indent-basic))
      ((smie-rule-parent-p "->")
       (smie-rule-parent elixir-smie-indent-basic))
      (t (smie-rule-parent elixir-smie-indent-basic))))
    (`(:before . "OP")
     (cond
      ((smie-rule-parent-p "for")
       (smie-rule-parent))
      ((and (not (smie-rule-hanging-p))
            (elixir-smie-current-line-start-with-pipe-operator-p)
            (elixir-smie-last-line-is-assignment-p))
       (smie-rule-parent))
      ((and (not (smie-rule-hanging-p))
            (elixir-smie-current-line-start-with-pipe-operator-p))
       (cons 'column  (elixir-smie--previous-line-indentation)))
      ((and (not (smie-rule-hanging-p))
            (smie-rule-prev-p "OP"))
       (- elixir-smie-indent-basic))
      ((smie-rule-parent-p "def" "defp" "defmacro" "defmacrop")
       (smie-rule-parent))
      (t (smie-rule-parent))))
    (`(:after . "OP")
     (cond
      ((smie-rule-sibling-p) nil)
      ((smie-rule-hanging-p)
       (smie-rule-parent elixir-smie-indent-basic))
      ((and (not (smie-rule-sibling-p))
            (not (smie-rule-hanging-p))
            (smie-rule-parent-p "do:"))
       (smie-rule-parent))
      ((smie-rule-parent-p ";")
       (smie-rule-parent))
      ((smie-rule-parent-p "{")
       (smie-rule-parent elixir-smie-indent-basic))
      (t (smie-rule-parent (- elixir-smie-indent-basic)))))
    (`(:before . "MATCH-STATEMENT-DELIMITER")
     (cond
      ((and (smie-rule-parent-p "do")
            (smie-rule-hanging-p))
       (smie-rule-parent))
      ((and (smie-rule-parent-p "do")
            (not (smie-rule-hanging-p)))
       (smie-rule-parent elixir-smie-indent-basic))
      ((and (smie-rule-parent-p "fn"))
       (smie-rule-parent elixir-smie-indent-basic))
      ;; There is a case when between two line inside a def block
      ;; when jumping to the next line and indent, where the cursor
      ;; jumps too much in front.
      ;;
      ;; Example:
      ;; def generate_pkg(path, opts) do
      ;;   name = Path.basename(Path.expand(path))
      ;;
      ;;   File.mkdir_p!(path)
      ;;                              <-
      ;;   File.cd! path, fn ->
      ;;     _generate_pkg(name, opts)
      ;;   end
      ;; end
      ((and (smie-rule-parent-p "do")
            (not (smie-rule-hanging-p)))
       0)
      ((and (not (smie-rule-sibling-p))
            (elixir-ppss-last-complete-sexp-start smie--parent)
            (smie-rule-hanging-p))
       (smie-rule-parent elixir-smie-indent-basic))
      ((and (not (smie-rule-sibling-p))
            (not (elixir-ppss-last-complete-sexp-start smie--parent))
            (smie-rule-hanging-p))
       (smie-rule-parent))))
    (`(:after . "MATCH-STATEMENT-DELIMITER")
     (cond
      ((and (smie-rule-parent-p "MATCH-STATEMENT-DELIMITER")
            (smie-rule-hanging-p)
            (smie-rule-sibling-p))
       (smie-rule-parent))
      ((and (smie-rule-parent-p "after")
            (smie-rule-hanging-p))
       (smie-rule-parent elixir-smie-indent-basic))
      ;; Correct indentation after a one-line fn definition
      ;; Example:
      ;;
      ;;  sum = Enum.reduce(dbms, fn(x, sum) -> x + sum end)
      ;;  average_dbm = sum / length(addresses)
      ((smie-rule-parent-p "fn")
       (smie-rule-parent elixir-smie-indent-basic))
      (t
       (smie-rule-parent))))
    (`(:before . "fn")
     (cond
      ((smie-rule-parent-p "(")
       (smie-rule-parent))
      (t (smie-rule-parent))))
    (`(:before . "for")
     (cond
      ((elixir-smie-last-line-end-with-block-operator-p)
       (smie-rule-parent elixir-smie-indent-basic))
      ((and (smie-rule-parent-p ";")
	    (smie-rule-prev-p "OP"))
       (smie-rule-parent))
      ((smie-rule-prev-p "OP" "def")
       (smie-rule-parent -2))))
    (`(:before . "into:")
     (cond
      ((smie-rule-parent-p "COMMA")
       (smie-rule-parent elixir-smie-indent-basic))))
    (`(:before . "do:")
     (cond
      ((smie-rule-parent-p "def" "defp" "defmacro" "defmacrop")
       (if (save-excursion
	     (move-beginning-of-line 1)
	     (looking-at "^\s*do:.+$"))
	   (smie-rule-parent)
	 (smie-rule-parent)))
      ;; Example
      ;;
      ;; hi = for i <- list, do: i
      ;; # weird spacing now <- Indent
      ;;
      ;; for i <- list, do: i
      ;; IO.puts 'WORKED' <- Indent
      ((and (smie-rule-parent-p "for")
            (not (smie-rule-hanging-p)))
       (smie-rule-parent))
      ((and (smie-rule-parent-p ";")
            (not (smie-rule-hanging-p))
	    (save-excursion
	      (move-beginning-of-line 1)
	      (looking-at "^\s*do:.+$")))
       (if (> (elixir-ppss-depth (syntax-ppss)) 0)
	   (smie-rule-parent (- 3))
	 (smie-rule-parent elixir-smie-indent-basic)))
      ((and (smie-rule-parent-p ";")
            (not (smie-rule-hanging-p)))
       (if (> (elixir-ppss-depth (syntax-ppss)) 0)
	   (smie-rule-parent (- elixir-smie-indent-basic))
	 (smie-rule-parent)))
      ((and (smie-rule-parent-p "OP")
            (not (smie-rule-hanging-p)))
       (smie-rule-parent elixir-smie-indent-basic))
      ((and (smie-rule-parent-p "COMMA")
            (not (smie-rule-hanging-p)))
       (smie-rule-parent elixir-smie-indent-basic))))
    (`(:before . "do")
     (cond
      ((and (smie-rule-parent-p "for")
            (smie-rule-hanging-p))
       (if (save-excursion
	     (move-beginning-of-line 1)
	     (looking-at "^.+\sfor\s.+\sdo\s*"))
	   (smie-rule-parent elixir-smie-indent-basic)
	 (smie-rule-parent (+ elixir-smie-indent-basic
			      elixir-smie-indent-basic))))
      ((and (smie-rule-parent-p "case")
            (smie-rule-hanging-p))
       (smie-rule-parent elixir-smie-indent-basic))
      ;; There is a case when between two line inside a def block
      ;; when jumping to the next line and indent, where the cursor
      ;; jumps too much in front.
      ;;
      ;; Example:
      ;; def generate_pkg(path, opts) do
      ;;   name = Path.basename(Path.expand(path))
      ;;
      ;;   File.mkdir_p!(path)
      ;;                              <-
      ;;   File.cd! path, fn ->
      ;;     _generate_pkg(name, opts)
      ;;   end
      ;; end
      ((and (smie-rule-parent-p "def")
            (smie-rule-hanging-p))
       (smie-rule-parent elixir-smie-indent-basic))
      (t elixir-smie-indent-basic)))
    (`(:before . "end")
     (cond
      ((smie-rule-parent-p "for")
       (smie-rule-parent))
      ((smie-rule-parent-p "(")
       (smie-rule-parent))
      (t (smie-rule-parent))))
    (`(:before . "else:")
     (cond
      ((smie-rule-parent-p ";")
       (if (> (elixir-ppss-depth (syntax-ppss)) 0)
	   (smie-rule-parent elixir-smie-indent-basic)
	 (smie-rule-parent)))
      ((smie-rule-parent-p "if")
       (smie-rule-parent elixir-smie-indent-basic))
      (t (smie-rule-parent))))
    ;; Closing paren on the other line
    (`(:before . "(")
     (cond
      ((smie-rule-parent-p "fn")
       (smie-rule-parent elixir-smie-indent-basic))
      ;; Indent parenthesis correctly inside a block
      ;;
      ;; Example:
      ;;
      ;; def bar do
      ;;   ()
      ;; .....
      ((smie-rule-parent-p "do")
       (smie-rule-parent))
      ((smie-rule-parent-p "OP")
       (smie-rule-parent))
      ((and (smie-rule-parent-p "with")
	    (smie-rule-hanging-p))
       (smie-rule-parent))
      ((and (smie-rule-parent-p "with")
	    (not (smie-rule-hanging-p)))
       (smie-rule-parent 3))
      ((smie-rule-parent-p ";")
       (smie-rule-parent))
      (t (smie-rule-parent))))
    (`(:before . "[")
     (cond
      ((smie-rule-hanging-p)
       (smie-rule-parent))))
    (`(:before . "{")
     (cond
      ((smie-rule-parent-p "COMMA")
       (smie-rule-parent))
      ;; Example
      ;;
      ;; case parse do
      ;;   { [ help: true ], _, _ }
      ;;     -> :help
      ;;   { _, [ user, project, count ], _ }
      ((and (not (smie-rule-hanging-p))
            (smie-rule-parent-p "do"))
       ;; If the last line ends with a block operator `->'
       ;; indent two spaces more
       ;;
       ;; Example
       ;;
       ;; case File.read("/usr/share/dict/words") do
       ;;   {:ok, contents} ->
       ;;     {:something, contents} <- Indent here two spaces
       ;;   ...
       (if (elixir-smie-last-line-end-with-block-operator-p)
           (smie-rule-parent elixir-smie-indent-basic)))
      ((and (smie-rule-parent-p "MATCH-STATEMENT-DELIMITER")
            (not (smie-rule-hanging-p)))
       (if (elixir-smie-last-line-end-with-block-operator-p)
           (smie-rule-parent elixir-smie-indent-basic)
         (if (elixir-smie-last-line-start-with-block-operator-p)
             (smie-rule-parent (- elixir-smie-indent-basic))
           (smie-rule-parent))))
      ((and (smie-rule-parent-p "OP")
            (smie-rule-hanging-p))
       (smie-rule-parent))
      ((smie-rule-parent-p ";")
       (if (save-excursion
             (move-end-of-line 1)
             (looking-back elixir-smie--block-operator-regexp (- (point) 3) t))
           (smie-rule-parent (- elixir-smie-indent-basic))
         (if (save-excursion
	       (move-beginning-of-line 1)
	       (looking-at "^.+->.+$"))
	     (smie-rule-parent (- elixir-smie-indent-basic))
	   (smie-rule-parent))))))
    (`(:after . "{")
     (cond
      ((smie-rule-hanging-p)
       (smie-rule-parent elixir-smie-indent-basic))
      (t elixir-smie-indent-basic)))
    (`(:after . "[")
     (cond
      ((smie-rule-hanging-p)
       (smie-rule-parent elixir-smie-indent-basic))
      (t elixir-smie-indent-basic)))
    (`(:before . "if")
     (cond
      ;; Indent when if is inside a `->' block
      ;;
      ;; Example:
      ;;
      ;; whatever ->
      ;;   if true do <-
      ;;     :foo
      ;;   end
      ;;   ....
      ((elixir-smie-last-line-end-with-block-operator-p)
       (smie-rule-parent elixir-smie-indent-basic))
      ;; Indent if inside else
      ;;
      ;; Example:
      ;;
      ;; else
      ;;   if condition, do: :bar <-
      ;; end
      ((smie-rule-parent-p "else")
       (smie-rule-parent elixir-smie-indent-basic))
      (t (smie-rule-parent))))
    (`(:before . "->")
     (cond
      ;; Example
      ;;
      ;; receive do
      ;; after
      ;;   2000 ->
      ;;     IO.puts 'hello'
      ;;     IO.puts 'status 2000 ends' <- Indent second line
      ;;   { :ok } ->
      ;;     ....
      ((and (smie-rule-parent-p "after")
            (not (smie-rule-sibling-p)))
       (smie-rule-parent (+ elixir-smie-indent-basic
                            elixir-smie-indent-basic)))
      ;; Example
      ;;
      ;; case parse do
      ;;   { [ help: true ], _, _ }
      ;;     -> :help
      ;;   ...
      ((and (not (smie-rule-hanging-p))
            (smie-rule-parent-p "do"))
       elixir-smie-indent-basic)
      ((and (not (smie-rule-hanging-p))
            (smie-rule-parent-p "MATCH-STATEMENT-DELIMITER"))
       (smie-rule-parent))
      (t (smie-rule-parent elixir-smie-indent-basic))))
    (`(:after . "->")
     (cond
      ;; This first condition is kind of complicated so I'll try to make this
      ;; comment as clear as possible.

      ;; "If `->' is the last thing on the line, and its parent token
      ;; is `fn' ..."
      ((and (smie-rule-hanging-p)
            (smie-rule-parent-p "fn"))
       ;; "... and if:

       ;; 1. `smie--parent' is non-nil
       ;; 2. the `->' token in question is on the same line as its parent (if
       ;;    the logic has gotten this far, its parent will be `fn')

       ;; ... then indent the line after the `->' aligned with the
       ;; parent, offset by `elixir-smie-indent-basic'."
       (if (and smie--parent (elixir-smie--same-line-as-parent
                              (elixir-ppss-innermost-start smie--parent)
                              (point)))
           (smie-rule-parent elixir-smie-indent-basic)
         elixir-smie-indent-basic))
      ;; Otherwise, if just indent by two.
      ((smie-rule-hanging-p)
       (cond
        ((smie-rule-parent-p "catch" "rescue" "else")
         (smie-rule-parent (+ elixir-smie-indent-basic
                              elixir-smie-indent-basic)))
        ((smie-rule-parent-p "do" "try")
         (smie-rule-parent elixir-smie-indent-basic))
        ;; Example
        ;;
        ;; receive do
        ;; after
        ;;   2000 ->
        ;;     IO.puts 'hello' <- Indent two spaces
        ((and (smie-rule-parent-p "after")
              (smie-rule-hanging-p)
              (not (smie-rule-sibling-p)))
         (smie-rule-parent (+ elixir-smie-indent-basic
                              elixir-smie-indent-basic)))
        (t (smie-rule-parent elixir-smie-indent-basic))))))
    (`(:before . ";")
     (cond
      ;; Handle cases where built in keywords are used
      ;; as function names.
      ;;
      ;; Example:
      ;;
      ;; def foo(test) do
      ;;   test_case = test.case
      ;;   run(test_case)
      ;; end
      ((and (smie-rule-parent-p "case" "try" "rescue")
            (smie-rule-hanging-p)
            (elixir-smie-current-line-contains-built-in-keyword-p))
       (+ (- (cdr (smie-rule-parent))) (+ elixir-smie-indent-basic
                                          elixir-smie-indent-basic)))
      ;; There is a case after an one line definition of functions/macros
      ;; when an `if' keyword token is involved, where the next block `end'
      ;; token will have a `if' as parent and it's hanging.
      ;;
      ;; Example:
      ;;
      ;; defmacro my_if(expr, do: if_block), do: if(expr, do: if_block, else: nil)
      ;; defmacro my_if(expr, do: if_block, else: else_block) do
      ;;   ...
      ;; end <- parent is `if`
      ((and (smie-rule-parent-p "if")
            (smie-rule-hanging-p))
       (smie-rule-parent))
      ((and (smie-rule-parent-p "else")
            (smie-rule-hanging-p))
       (smie-rule-parent elixir-smie-indent-basic))
      ((smie-rule-parent-p "catch" "def" "defmodule" "defp" "do" "else"
                           "fn" "if" "rescue" "try" "unless" "defmacro" "defmacrop")
       (smie-rule-parent))
      ((smie-rule-parent-p "after")
       (smie-rule-parent elixir-smie-indent-basic))
      ;; Example
      ;;
      ;; case parse do
      ;;   { [ help: true ], _, _ }
      ;;     -> :help
      ;;   { _, [ user, project, count ], _ }
      ;;     -> { user, project, count }
      ;;   ...
      ((and (smie-rule-parent-p "->")
            (smie-rule-hanging-p))
       (smie-rule-parent))
      ((and (smie-rule-parent-p ";")
      	    (smie-rule-hanging-p)
      	    (save-excursion
      	      (move-beginning-of-line 1)
      	      (looking-at "^\s+else:.+$"))
      	    (not (save-excursion
      	      (move-beginning-of-line 1)
      	      (looking-at "^\s+else:.+)$"))))
       (smie-rule-parent (- elixir-smie-indent-basic)))
      ((and (smie-rule-parent-p ";")
	    (save-excursion
	      (move-beginning-of-line 1)
	      (looking-at "^.+,$")))
       (smie-rule-parent))
      ((and (smie-rule-parent-p ";")
      	    (smie-rule-hanging-p)
      	    (save-excursion
      	      (move-beginning-of-line 1)
      	      (looking-at "^\s+do:.+$"))
      	    (not (save-excursion
		   (move-beginning-of-line 1)
		   (looking-at "^\s+do:.+)$"))))
       (smie-rule-parent))
      ((elixir-smie-current-line-start-with-pipe-operator-p)
       (smie-rule-parent))
      ((smie-rule-parent-p "(")
       (smie-rule-parent elixir-smie-indent-basic))))
    (`(:after . ";")
     (cond
      ((smie-rule-parent-p "def")
       (smie-rule-parent))
      ((and (smie-rule-parent-p "if")
            (elixir-smie-current-line-contains-built-in-keyword-p))
       (+ (- (cdr (smie-rule-parent))) (+ elixir-smie-indent-basic
                                          elixir-smie-indent-basic)))
      ((smie-rule-parent-p "if")
       (smie-rule-parent))
      ((smie-rule-parent-p "after")
       (smie-rule-parent elixir-smie-indent-basic))
      ((and (smie-rule-parent-p "(")
            (boundp 'smie--parent)
            (save-excursion
              (goto-char (cadr smie--parent))
              (smie-rule-hanging-p)))
       (smie-rule-parent elixir-smie-indent-basic))))))

(defun elixir-smie--heredoc-at-current-point-p ()
  "Return non-nil if cursor is at a string."
  (save-excursion
    (or (save-excursion
          (let ((parse-data (parse-partial-sexp 1 (point))))
            (and (elixir-ppss-string-terminator parse-data)
                 (elixir-ppss-comment-or-string-start parse-data))))
        (and (looking-at "\"\"\"")
             (match-beginning 0)))))

(defun elixir-smie--previous-line-empty-p ()
  "Return non-nil if the previous line is blank."
  (save-excursion
    (forward-line -1)
    (move-beginning-of-line 1)
    (looking-at "[[:space:]]*$")))

(defun elixir-smie--previous-line-indentation ()
  "Return the indentation of the previous line."
  (save-excursion
    (forward-line -1)
    (current-indentation)))

;; Add the custom function to handle indentation inside heredoc to the
;; smie-indent-functions list. The indentation function will only be
;; process inside an elixir-mode.
(defun elixir-smie--indent-inside-heredoc ()
  "Handle indentation inside Elixir heredocs.

Rules:
  1. If the previous line is empty, indent as the basic indentation
     at the beginning of the heredoc.
  2. If the previous line is not empty, indent as the previous line."
  (if (eq major-mode 'elixir-mode)
      (if (elixir-smie--heredoc-at-current-point-p)
          (let ((indent
                 (save-excursion
                   (when (re-search-backward "^\\(\s+\\)\\(@doc\\|@moduledoc\\|.*\\)\"\"\"" nil t)
                     (string-width (match-string 1))))))
	    (cond
	     ((elixir-smie--previous-line-empty-p)
	      (goto-char indent))
	     ((and (not (save-excursion (looking-at "\"\"\"")))
		   (not (elixir-smie--previous-line-empty-p)))
	      (goto-char (elixir-smie--previous-line-indentation)))
	     (indent
	      (goto-char indent)))))))

(defun elixir-smie-empty-string-p (string)
  "Return non-nil if STRING is null, blank or whitespace only."
  (or (null string)
      (string= string "")
      (if (string-match-p "^\s+$" string) t)))

(add-to-list 'smie-indent-functions 'elixir-smie--indent-inside-heredoc)

(provide 'elixir-smie)

;;; elixir-smie.el ends here
