;;; haskell-lexeme.el --- haskell lexical tokens   -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2015 Gracjan Polak

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

;;; Code:

(require 'rx)

(unless (category-docstring ?P)
  (define-category ?P "Haskell symbol constituent characters")
  (map-char-table
   #'(lambda (key val)
       (if (or
            (and (consp key) (> (car key) 128))
            (and (numberp key) (> key 128)))
           (if (member val '(Pc Pd Po Sm Sc Sk So))
               (modify-category-entry key ?P))))
   unicode-category-table)

  (dolist (key (string-to-list "!#$%&*+./<=>?@^|~\\-"))
    (modify-category-entry key ?P)))

(defconst haskell-lexeme-modid
  "[[:upper:]][[:alnum:]'_]*"
  "Regexp matching a valid Haskell module identifier.

Note that GHC accepts Unicode category UppercaseLetter as a first
character. Following letters are from Unicode categories
UppercaseLetter, LowercaseLetter, OtherLetter, TitlecaseLetter,
ModifierLetter, DecimalNumber, OtherNumber, backslash or
underscore.

Note that this differs from constructor identifier as the latter
one can have any number of hash character at the end to
accommodate MagicHash extension.")

(defconst haskell-lexeme-id
  "[[:alpha:]_][[:alnum:]'_]*#*"
  "Regexp matching a valid Haskell identifier.

GHC accepts a string starting with any alphabetic character or
underscore followed by any alphanumeric character or underscore
or apostrophe.")

(defconst haskell-lexeme-sym
  "\\(:?\\cP\\|:\\)+"
  "Regexp matching a valid Haskell variable or constructor symbol.

GHC accepts a string of chars from the set
[:!#$%&*+./<=>?@^|~\\-] or Unicode category Symbol for chars with
codes larger than 128 only.")

(defconst haskell-lexeme-modid-opt-prefix
  (concat "\\(?:" haskell-lexeme-modid "\\.\\)*")
  "Regexp matching a valid Haskell module prefix, potentially empty.

Module path prefix is separated by dots and finishes with a
dot. For path component syntax see `haskell-lexeme-modid'.")

(defconst haskell-lexeme-qid-or-qsym
  (rx-to-string `(: (regexp ,haskell-lexeme-modid-opt-prefix)
                    (group (| (regexp ,haskell-lexeme-id) (regexp ,haskell-lexeme-sym)
                              ))))
  "Regexp matching a valid qualified identifier or symbol.

Note that (match-string 1) returns the unqualified part.")

(defconst haskell-lexeme-qid
  (rx-to-string `(: (regexp "'*")
                    (regexp ,haskell-lexeme-modid-opt-prefix)
                    (group (regexp ,haskell-lexeme-id))))
  "Regexp matching a valid qualified identifier.

Note that (match-string 1) returns the unqualified part.")

(defconst haskell-lexeme-qsym
  (rx-to-string `(: (regexp "'*")
                    (regexp ,haskell-lexeme-modid-opt-prefix)
                    (group (regexp ,haskell-lexeme-id))))
  "Regexp matching a valid qualified symbol.

Note that (match-string 1) returns the unqualified part.")

(defconst haskell-lexeme-number
  (rx (| (: (regexp "[0-9]+\\.[0-9]+") (opt (regexp "[eE][-+]?[0-9]+")))
         (regexp "[0-9]+[eE][-+]?[0-9]+")
         (regexp "0[xX][0-9a-fA-F]+")
         (regexp "0[oO][0-7]+")
         (regexp "[0-9]+")))
  "Regexp matching a floating point, decimal, octal or hexadecimal number.

Note that negative sign char is not part of a number.")

(defconst haskell-lexeme-char-literal-inside
  (rx (| (not (any "\n'\\"))
         (: "\\"
            (| "a" "b" "f" "n" "r" "t" "v" "\\" "\"" "'"
               "NUL" "SOH" "STX" "ETX" "EOT" "ENQ" "ACK"
               "BEL" "BS" "HT" "LF" "VT" "FF" "CR" "SO" "SI" "DLE"
               "DC1" "DC2" "DC3" "DC4" "NAK" "SYN" "ETB" "CAN"
               "EM" "SUB" "ESC" "FS" "GS" "RS" "US" "SP" "DEL"
               (regexp "[0-9]+")
               (: "x" (regexp "[0-9a-fA-F]+"))
               (: "o" (regexp "[0-7]+"))
               (: "^" (regexp "[]A-Z@^_\\[]"))))))
  "Regexp matching an inside of a character literal.")

(defconst haskell-lexeme-char-literal
  (rx-to-string `(: "'" (regexp ,haskell-lexeme-char-literal-inside) "'"))
  "Regexp matching a character literal.")

(defconst haskell-lexeme-string-literal-inside-item
  (rx (| (not (any "\n\"\\"))
         (: "\\"
            (| "a" "b" "f" "n" "r" "t" "v" "\\" "\"" "'" "&"
               "NUL" "SOH" "STX" "ETX" "EOT" "ENQ" "ACK"
               "BEL" "BS" "HT" "LF" "VT" "FF" "CR" "SO" "SI" "DLE"
               "DC1" "DC2" "DC3" "DC4" "NAK" "SYN" "ETB" "CAN"
               "EM" "SUB" "ESC" "FS" "GS" "RS" "US" "SP" "DEL"
               (regexp "[0-9]+")
               (: "x" (regexp "[0-9a-fA-F]+"))
               (: "o" (regexp "[0-7]+"))
               (: "^" (regexp "[]A-Z@^_\\[]"))
               (regexp "[ \t\n\r\v\f]*\\\\")))))
  "Regexp matching an item that is a single character or a single
escape sequence inside of a string literal.

Note that `haskell-lexeme-string-literal-inside-item' matches
strictly only escape sequences defined in Haskell Report.")

(defconst haskell-lexeme-string-literal
  (rx (: (group "\"")
         (group (* (| (regexp "\\\\[ \t\n\r\v\f]*\\\\")
                      (regexp "\\\\[ \t\n\r\v\f]+")
                      (regexp "\\\\[^ \t\n\r\v\f]")
                      (regexp "[^\"\n\\]"))))
         (group (| "\"" (regexp "$") (regexp "\\\\?\\'")
                   ))))
  "Regexp matching a string literal lookalike.

Note that `haskell-lexeme-string-literal' matches more than
Haskell Report specifies because we want to support also code
under edit.

String literals end with double quote or unescaped newline or end
of buffer.

Regexp has subgroup expressions:
 (match-text 1) matches the opening doublequote.
 (match-text 2) matches the inside of the string.
 (match-text 3) matches the closing quote, or a closing
                newline or empty string at the end of the buffer.")

(defconst haskell-lexeme-quasi-quote-literal
  (rx-to-string `(: "[" (optional "$")
                    (group (regexp ,haskell-lexeme-id))
                    (group "|")
                    (group (* (| (not (any "|"))
                                 (: "|" (not (any "]"))))
                              ))
                    (group (| "|" eos))
                    (| "]" eos)))
  "Regexp matching a quasi quote literal.

Quasi quotes start with '[xxx|' or '[$xxx|' sequence and end with
'|]'. The 'xxx' is a quoter name. There is no escaping mechanism
provided for the ending sequence.

Regexp has subgroup expressions:
 (match-text 1) matches the quoter name (without $ sign if present).
 (match-text 2) matches the opening vertical bar.
 (match-text 3) matches the inside of the quoted string.
 (match-text 4) matches the closing vertical bar
                or empty string if at the end of the buffer.

Note that this regexp admits 'e', 't', 'd', 'p' as quoter names
although template Haskell explicitly rejects those.")

(defun haskell-lexeme-classify-by-first-char (char)
  "Classify token by CHAR.

CHAR is a chararacter that is assumed to be the first character
of a token."
  (let ((category (get-char-code-property char 'general-category)))

    (cond
     ((or (member char '(?! ?# ?$ ?% ?& ?* ?+ ?. ?/ ?< ?= ?> ?? ?@ ?^ ?| ?~ ?\\ ?-))
          (and (> char 127)
               (member category '(Pc Pd Po Sm Sc Sk So))))
      'varsym)
     ((equal char ?:)
      'consym)
     ((equal char ?\')
      'char)
     ((equal char ?\")
      'string)
     ((member category '(Lu Lt))
      'conid)
     ((or (equal char ?_)
          (member category '(Ll Lo)))
      'varid)
     ((and (>= char ?0) (<= char ?9))
      'number)
     ((member char '(?\] ?\[ ?\( ?\) ?\{ ?\} ?\` ?\, ?\;))
      'special))))

(defun haskell-lexeme-looking-at-token (&rest flags)
  "Like `looking-at' but understands Haskell lexemes.

Moves point forward over whitespace.  Returns a symbol describing
type of Haskell token recognized.  Use `match-string',
`match-beginning' and `match-end' with argument 0 to query match
result.

Possible results are:
- 'special: for chars [](){}`,;
- 'comment: for single line comments
- 'nested-comment: for multiline comments
- 'qsymid: for qualified identifiers or symbols
- 'string: for strings literals
- 'char: for char literals
- 'number: for decimal, float, hexadecimal and octal number literals
- 'template-haskell-quote: for a string of apostrophes for template haskell
- 'template-haskell-quasi-quote: for a string of apostrophes for template haskell

Note that for qualified symbols (match-string 1) returns the
unqualified identifier or symbol.  Further qualification for
symbol or identifier can be done with:

   (haskell-lexeme-classify-by-first-char (char-after (match-beginning 1)))

See `haskell-lexeme-classify-by-first-char' for details."
  (while
      ;; Due to how unterminated strings terminate at newline, some
      ;; newlines have syntax set to generic string delimeter. We want
      ;; those to be treated as whitespace anyway
      (or
       (> (skip-syntax-forward "-") 0)
       (and (not (member 'newline flags))
            (> (skip-chars-forward "\n") 0))))
  (let
      ((case-fold-search nil)
       (point (point-marker)))
    (or
     (and
      (equal (string-to-syntax "<") (syntax-after (point)))
      (progn
        (set-match-data (list point (set-marker (make-marker) (line-end-position))))
        'literate-comment))
     (and (looking-at "\n")
          'newline)
     (and (looking-at "{-")
          (save-excursion
            (forward-comment 1)
            (set-match-data (list point (point-marker)))
            'nested-comment))
     (and (looking-at haskell-lexeme-char-literal)
          'char)
     (and (looking-at haskell-lexeme-string-literal)
          'string)
     (and (looking-at "[][(){}`,;]")
          (let ((match-data (match-data)))
            (if (and (equal "[" (match-string-no-properties 0))
                     (looking-at haskell-lexeme-quasi-quote-literal))
                (if (or (member (match-string-no-properties 1) '("e" "d" "p" "t"))
                        (not (equal (haskell-lexeme-classify-by-first-char (char-after (match-beginning 1)))
                                    'varid)))
                    (progn
                      (set-match-data match-data)
                      'special)
                    'template-haskell-quasi-quote)
              'special)))
     (and (looking-at haskell-lexeme-qid-or-qsym)
          (if (save-match-data
                (string-match "\\`---*\\'" (match-string-no-properties 0)))
              (progn
                (set-match-data (list point (set-marker (make-marker) (line-end-position))))
                'comment)
            'qsymid))
     (and (looking-at haskell-lexeme-number)
          'number)
     (and (looking-at "'+")
          'template-haskell-quote)
     (and (looking-at ".")
          'illegal))))

(provide 'haskell-lexeme)

;;; haskell-lexeme.el ends here
