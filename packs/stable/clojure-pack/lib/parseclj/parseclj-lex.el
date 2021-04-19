;;; parseclj-lex.el --- Clojure/EDN Lexer

;; Copyright (C) 2017-2018  Arne Brasseur

;; Author: Arne Brasseur <arne@arnebrasseur.net>

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; A reader for EDN data files and parser for Clojure source files.

;;; Code:

(defvar parseclj-lex--leaf-tokens '(:whitespace
                                    :comment
                                    :number
                                    :nil
                                    :true
                                    :false
                                    :symbol
                                    :keyword
                                    :string
                                    :regex
                                    :character)
  "Types of tokens that represent leaf nodes in the AST.")

(defvar parseclj-lex--closing-tokens '(:rparen
                                       :rbracket
                                       :rbrace)
  "Types of tokens that mark the end of a non-atomic form.")

(defvar parseclj-lex--prefix-tokens '(:quote
                                      :backquote
                                      :unquote
                                      :unquote-splice
                                      :discard
                                      :tag
                                      :reader-conditional
                                      :reader-conditional-splice
                                      :var
                                      :deref
                                      :map-prefix
                                      :eval)
  "Tokens that modify the form that follows.")

(defvar parseclj-lex--prefix-2-tokens '(:metadata)
  "Tokens that modify the two forms that follow.")

;; Token interface

(defun parseclj-lex-token (type form pos &rest attributes)
  "Create a lexer token with the specified attributes.

Tokens at a mimimum have these attributes
- TYPE: the type of token, like :whitespace or :lparen
- FORM: the source form, a string
- POS: the position in the input, starts from 1 (like point)

Other ATTRIBUTES can be given as a flat list of key-value pairs."
  (apply 'a-list :token-type type :form form :pos pos attributes))

(defun parseclj-lex-error-token (pos &optional error-type)
  "Create a lexer error token starting at POS.
ERROR-TYPE is an optional keyword to attach to the created token,
as the means for providing more information on the error."
  (apply #'parseclj-lex-token
         :lex-error
         (buffer-substring-no-properties pos (point))
         pos
         (when error-type
           (list :error-type error-type))))

(defun parseclj-lex-token-p (token)
  "Is the given TOKEN a parseclj-lex TOKEN.

A token is an association list with :token-type as its first key."
  (and (consp token)
       (consp (car token))
       (eq :token-type (caar token))))

(defun parseclj-lex-token-type (token)
  "Get the type of TOKEN."
  (and (consp token)
       (cdr (assq :token-type token))))

(defun parseclj-lex-token-form (token)
  "Get the form of TOKEN."
  (and (consp token)
       (cdr (assq :form token))))

(defun parseclj-lex-leaf-token-p (token)
  "Return t if the given AST TOKEN is a leaf node."
  (member (parseclj-lex-token-type token) parseclj-lex--leaf-tokens))

(defun parseclj-lex-closing-token-p (token)
  "Return t if the given ast TOKEN is a closing token."
  (member (parseclj-lex-token-type token) parseclj-lex--closing-tokens))

(defun parseclj-lex-error-p (token)
  "Return t if the TOKEN represents a lexing error token."
  (eq (parseclj-lex-token-type token) :lex-error))

;; Elisp values from tokens

(defun parseclj-lex--string-value (s)
  "Parse an EDN string S into a regular string.
S goes through three transformations:
- Escaped characters in S are transformed into Elisp escaped
  characters.
- Unicode escaped characters are decoded into its corresponding
  unicode character counterpart.
- Octal escaped characters are decoded into its corresponding
  character counterpart."
  (replace-regexp-in-string
   "\\\\o[0-8]\\{3\\}"
   (lambda (x)
     (make-string 1 (string-to-number (substring x 2) 8)))
   (replace-regexp-in-string
    "\\\\u[0-9a-fA-F]\\{4\\}"
    (lambda (x)
      (make-string 1 (string-to-number (substring x 2) 16)))
    (replace-regexp-in-string "\\\\[tbnrf'\"\\]"
                              (lambda (x)
                                (cl-case (elt x 1)
                                  (?t "\t")
                                  (?f "\f")
                                  (?\" "\"")
                                  (?r "\r")
                                  (?n "\n")
                                  (?\\ "\\\\")
                                  (t (substring x 1))))
                              (substring s 1 -1)))))

(defun parseclj-lex--character-value (c)
  "Parse an EDN character C into an Emacs Lisp character."
  (let ((first-char (elt c 1)))
    (cond
     ((equal c "\\newline") ?\n)
     ((equal c "\\return") ?\r)
     ((equal c "\\space") ?\ )
     ((equal c "\\tab") ?\t)
     ((eq first-char ?u) (string-to-number (substring c 2) 16))
     ((eq first-char ?o) (string-to-number (substring c 2) 8))
     (t first-char))))

(defun parseclj-lex--leaf-token-value (token)
  "Parse the given leaf TOKEN to an Emacs Lisp value."
  (cl-case (parseclj-lex-token-type token)
    (:number (string-to-number (alist-get :form token)))
    (:nil nil)
    (:true t)
    (:false nil)
    (:symbol (intern (alist-get :form token)))
    (:keyword (intern (alist-get :form token)))
    (:string (parseclj-lex--string-value (alist-get :form token)))
    (:character (parseclj-lex--character-value (alist-get :form token)))))


;; Stream tokenization

(defun parseclj-lex-at-whitespace-p ()
  "Return t if char at point is white space."
  (let ((char (char-after (point))))
    (or (equal char ?\ )
        (equal char ?\t)
        (equal char ?\n)
        (equal char ?\r)
        (equal char ?,))))

(defun parseclj-lex-at-eof-p ()
  "Return t if point is at the end of file."
  (eq (point) (point-max)))

(defun parseclj-lex-whitespace ()
  "Consume all consecutive white space as possible and return an :whitespace token."
  (let ((pos (point)))
    (while (parseclj-lex-at-whitespace-p)
      (right-char))
    (parseclj-lex-token :whitespace
                        (buffer-substring-no-properties pos (point))
                        pos)))

(defun parseclj-lex-skip-digits ()
  "Skip all consecutive digits after point."
  (while (and (char-after (point))
              (<= ?0 (char-after (point)))
              (<= (char-after (point)) ?9))
    (right-char)))

(defun parseclj-lex-skip-hex ()
  "Skip all consecutive hex digits after point."
  (while (and (char-after (point))
              (or (<= ?0 (char-after (point)) ?9)
                  (<= ?a (char-after (point)) ?f)
                  (<= ?A (char-after (point)) ?F)))
    (right-char)))

(defun parseclj-lex-skip-number ()
  "Skip a number at point."
  ;; [\+\-]?\d+\.\d+
  (if (and (eq ?0 (char-after (point)))
           (eq ?x (char-after (1+ (point)))))
      (progn
        (right-char 2)
        (parseclj-lex-skip-hex))
    (progn
      (when (member (char-after (point)) '(?+ ?-))
        (right-char))

      (parseclj-lex-skip-digits)

      (when (eq (char-after (point)) ?.)
        (right-char))

      (parseclj-lex-skip-digits))))

(defun parseclj-lex-number ()
  "Consume a number and return a `:number' token representing it."
  (let ((pos (point)))
    (parseclj-lex-skip-number)

    ;; 10110r2 or 4.3e+22
    (when (member (char-after (point)) '(?E ?e ?r))
      (right-char))

    (parseclj-lex-skip-number)

    ;; trailing M
    (when (eq (char-after (point)) ?M)
      (right-char))

    (let ((char (char-after (point))))
      (if (and char (or (and (<= ?a char) (<= char ?z))
                        (and (<= ?A char) (<= char ?Z))
                        (and (member char '(?. ?* ?+ ?! ?- ?_ ?? ?$ ?& ?= ?< ?> ?/)))))
          (progn
            (right-char)
            (parseclj-lex-error-token pos :invalid-number-format))
        (parseclj-lex-token :number
                            (buffer-substring-no-properties pos (point))
                            pos)))))


(defun parseclj-lex-digit-p (char)
  "Return t if CHAR is a digit."
  (and char (<= ?0 char) (<= char ?9)))

(defun parseclj-lex-at-number-p ()
  "Return t if point is at a number."
  (let ((char (char-after (point))))
    (or (parseclj-lex-digit-p char)
        (and (member char '(?- ?+ ?.))
             (parseclj-lex-digit-p (char-after (1+ (point))))))))

(defun parseclj-lex-symbol-start-p (char &optional alpha-only)
  "Return t if CHAR is a valid start for a symbol.

Symbols begin with a non-numeric character and can contain alphanumeric
characters and . * + ! - _ ? $ % & = < > '.  If - + or . are the first
character, the second character (if any) must be non-numeric.

In some cases, like in tagged elements, symbols are required to start with
alphabetic characters only.  ALPHA-ONLY ensures this behavior."
  (not (not (and char
                 (or (and (<= ?a char) (<= char ?z))
                     (and (<= ?A char) (<= char ?Z))
                     (and (not alpha-only) (member char '(?. ?* ?+ ?! ?- ?_ ?? ?$ ?% ?& ?= ?< ?> ?/ ?'))))))))

(defun parseclj-lex-symbol-rest-p (char)
  "Return t if CHAR is a valid character in a symbol.
For more information on what determines a valid symbol, see
`parseclj-lex-symbol-start-p'"
  (or (parseclj-lex-symbol-start-p char)
      (parseclj-lex-digit-p char)
      (eq ?: char)
      (eq ?# char)))

(defun parseclj-lex-get-symbol-at-point (pos)
  "Return the symbol at POS as a string."
  (while (parseclj-lex-symbol-rest-p (char-after (point)))
    (right-char))
  (buffer-substring-no-properties pos (point)))

(defun parseclj-lex-symbol ()
  "Return a lex token representing a symbol.
Because of their special meaning, symbols \"nil\", \"true\", and \"false\"
are returned as their own lex tokens."
  (let ((pos (point)))
    (right-char)
    (let ((sym (parseclj-lex-get-symbol-at-point pos)))
      (cond
       ((equal sym "nil") (parseclj-lex-token :nil "nil" pos))
       ((equal sym "true") (parseclj-lex-token :true "true" pos))
       ((equal sym "false") (parseclj-lex-token :false "false" pos))
       (t (parseclj-lex-token :symbol sym pos))))))

(defun parseclj-lex-string* ()
  "Helper for string/regex lexing.
Returns either the string, or an error token"
  (let ((pos (point)))
    (right-char)
    (while (not (or (equal (char-after (point)) ?\") (parseclj-lex-at-eof-p)))
      (if (equal (char-after (point)) ?\\)
          (right-char 2)
        (right-char)))
    (when (equal (char-after (point)) ?\")
      (right-char)
      (buffer-substring-no-properties pos (point)))))

(defun parseclj-lex-string ()
  "Return a lex token representing a string.
If EOF is reached without finding a closing double quote, a :lex-error
token is returned."
  (let ((pos (point))
        (str (parseclj-lex-string*)))
    (if str
        (parseclj-lex-token :string str pos)
      (parseclj-lex-error-token pos :invalid-string))))

(defun parseclj-lex-regex ()
  "Return a lex token representing a regular expression.
If EOF is reached without finding a closing double quote, a :lex-error
token is returned."
  (let ((pos (1- (point)))
        (str (parseclj-lex-string*)))
    (if str
        (parseclj-lex-token :regex (concat "#" str) pos)
      (parseclj-lex-error-token pos :invalid-regex))))

(defun parseclj-lex-lookahead (n)
  "Return a lookahead string of N characters after point."
  (buffer-substring-no-properties (point) (min (+ (point) n) (point-max))))

(defun parseclj-lex-character ()
  "Return a lex token representing a character."
  (let ((pos (point)))
    (right-char)
    (cond
     ((equal (parseclj-lex-lookahead 3) "tab")
      (right-char 3)
      (parseclj-lex-token :character (buffer-substring-no-properties pos (point)) pos))

     ((equal (parseclj-lex-lookahead 5) "space")
      (right-char 5)
      (parseclj-lex-token :character (buffer-substring-no-properties pos (point)) pos))

     ((equal (parseclj-lex-lookahead 6) "return")
      (right-char 6)
      (parseclj-lex-token :character (buffer-substring-no-properties pos (point)) pos))

     ((equal (parseclj-lex-lookahead 7) "newline")
      (right-char 7)
      (parseclj-lex-token :character (buffer-substring-no-properties pos (point)) pos))

     ((string-match-p "^u[0-9a-fA-F]\\{4\\}" (parseclj-lex-lookahead 5))
      (right-char 5)
      (parseclj-lex-token :character (buffer-substring-no-properties pos (point)) pos))

     ((string-match-p "^o[0-8]\\{3\\}" (parseclj-lex-lookahead 4))
      (right-char 4)
      (parseclj-lex-token :character (buffer-substring-no-properties pos (point)) pos))

     (t
      (right-char)
      (parseclj-lex-token :character (buffer-substring-no-properties pos (point)) pos)))))

(defun parseclj-lex-keyword ()
  "Return a lex token representing a keyword.
Keywords follow the same rules as symbols, except they start with one or
two colon characters.

See `parseclj-lex-symbol', `parseclj-lex-symbol-start-p'."
  (let ((pos (point)))
    (right-char)
    (when (equal (char-after (point)) ?:) ;; same-namespace keyword
      (right-char))
    (if (equal (char-after (point)) ?:) ;; three colons in a row => lex-error
        (progn
          (right-char)
          (parseclj-lex-error-token pos :invalid-keyword))
      (progn
        (while (or (parseclj-lex-symbol-rest-p (char-after (point)))
                   (equal (char-after (point)) ?#))
          (right-char))
        (parseclj-lex-token :keyword (buffer-substring-no-properties pos (point)) pos)))))

(defun parseclj-lex-comment ()
  "Return a lex token representing a comment."
  (let ((pos (point)))
    (goto-char (line-end-position))
    (when (equal (char-after (point)) ?\n)
      (right-char))
    (parseclj-lex-token :comment (buffer-substring-no-properties pos (point)) pos)))

(defun parseclj-lex-map-prefix ()
  "Return a lex token representing a map prefix."
  (let ((pos (1- (point))))
    (right-char)
    (when (equal (char-after (point)) ?:)
      (right-char))
    (while (parseclj-lex-symbol-rest-p (char-after (point)))
      (right-char))
    (parseclj-lex-token :map-prefix (buffer-substring-no-properties pos (point)) pos)))

(defun parseclj-lex-next ()
  "Consume characters at point and return the next lexical token.

See `parseclj-lex-token'."
  (if (parseclj-lex-at-eof-p)
      (parseclj-lex-token :eof nil (point))
    (let ((char (char-after (point)))
          (pos  (point)))
      (cond
       ((parseclj-lex-at-whitespace-p)
        (parseclj-lex-whitespace))

       ((equal char ?\()
        (right-char)
        (parseclj-lex-token :lparen "(" pos))

       ((equal char ?\))
        (right-char)
        (parseclj-lex-token :rparen ")" pos))

       ((equal char ?\[)
        (right-char)
        (parseclj-lex-token :lbracket "[" pos))

       ((equal char ?\])
        (right-char)
        (parseclj-lex-token :rbracket "]" pos))

       ((equal char ?{)
        (right-char)
        (parseclj-lex-token :lbrace "{" pos))

       ((equal char ?})
        (right-char)
        (parseclj-lex-token :rbrace "}" pos))

       ((equal char ?')
        (right-char)
        (parseclj-lex-token :quote "'" pos))

       ((equal char ?`)
        (right-char)
        (parseclj-lex-token :backquote "`" pos))

       ((equal char ?~)
        (right-char)
        (if (eq ?@ (char-after (point)))
            (progn
              (right-char)
              (parseclj-lex-token :unquote-splice "~@" pos))
          (parseclj-lex-token :unquote "~" pos)))

       ((parseclj-lex-at-number-p)
        (parseclj-lex-number))

       ((parseclj-lex-symbol-start-p char)
        (parseclj-lex-symbol))

       ((equal char ?\")
        (parseclj-lex-string))

       ((equal char ?\\)
        (parseclj-lex-character))

       ((equal char ?:)
        (parseclj-lex-keyword))

       ((equal char ?\;)
        (parseclj-lex-comment))

       ((equal char ?^)
        (right-char)
        (parseclj-lex-token :metadata "^" pos))

       ((equal char ?@)
        (right-char)
        (parseclj-lex-token :deref "@" pos))

       ((equal char ?#)
        (right-char)
        (let ((char (char-after (point))))
          (cond
           ((equal char ?{)
            (right-char)
            (parseclj-lex-token :set "#{" pos))
           ((equal char ?_)
            (right-char)
            (parseclj-lex-token :discard "#_" pos))
           ((equal char ?\()
            (right-char)
            (parseclj-lex-token :lambda "#(" pos))
           ((equal char ?')
            (right-char)
            (parseclj-lex-token :var "#'" pos))
           ((equal char ?=)
            (right-char)
            (parseclj-lex-token :eval "#=" pos))
           ((equal char ?\")
            (parseclj-lex-regex))
           ((equal char ?:)
            (parseclj-lex-map-prefix))
           ((equal char ?\?)
            (right-char)
            (if (eq ?@ (char-after (point)))
                (progn
                  (right-char)
                  (parseclj-lex-token :reader-conditional-splice "#?@" pos))
              (parseclj-lex-token :reader-conditional "#?" pos)))
           ((parseclj-lex-symbol-start-p char t)
            (right-char)
            (parseclj-lex-token :tag (concat "#" (parseclj-lex-get-symbol-at-point (1+ pos))) pos))
           (t
            (while (not (or (parseclj-lex-at-whitespace-p)
                            (parseclj-lex-at-eof-p)))
              (right-char))
            (parseclj-lex-error-token pos :invalid-hashtag-dispatcher)))))

       (t
        (progn
          (right-char)
          (parseclj-lex-error-token pos)))))))

(provide 'parseclj-lex)

;;; parseclj-lex.el ends here
