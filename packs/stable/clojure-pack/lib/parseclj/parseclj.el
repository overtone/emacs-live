;;; parseclj.el --- Clojure/EDN parser              -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018  Arne Brasseur

;; Author: Arne Brasseur <arne@arnebrasseur.net>
;; Keywords: lisp clojure edn parser
;; Package-Requires: ((emacs "25") (a "0.1.0alpha4"))
;; Version: 0.1.0

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

;; Top level API for the Clojure parser.

;;; Code:

(require 'parseclj-parser)
(require 'parseclj-ast)

(defun parseclj-parse-clojure (&rest string-and-options)
  "Parse Clojure source to AST.

Reads either from the current buffer, starting from point, until
`point-max', or reads from the optional string argument.

STRING-AND-OPTIONS can be an optional string, followed by
key-value pairs to specify parsing options.

- `:lexical-preservation' Retain whitespace, comments, and
  discards.  Defaults to nil.
- `:fail-fast' Raise an error when encountering invalid syntax.
  Defaults to t.
- `:read-one'
  Read a single form.  Defaults to false: parse the complete input."
  (if (stringp (car string-and-options))
      (with-temp-buffer
        (insert (car string-and-options))
        (goto-char 1)
        (apply 'parseclj-parse-clojure (cdr string-and-options)))
    (let* ((value-p (lambda (e)
                      (and (parseclj-ast-node-p e)
                           (not (member (parseclj-ast-node-type e) '(:whitespace :comment :discard))))))
           (options (apply 'a-list :value-p value-p string-and-options))
           (lexical? (a-get options :lexical-preservation)))
      (parseclj-parser (if lexical?
                           #'parseclj-ast--reduce-leaf-with-lexical-preservation
                         #'parseclj-ast--reduce-leaf)
                       (if lexical?
                           #'parseclj-ast--reduce-branch-with-lexical-preservation
                         #'parseclj-ast--reduce-branch)
                       options))))

(defun parseclj-unparse-clojure (ast)
  "Parse Clojure AST to source code.

Given an abstract syntax tree AST (as returned by
`parseclj-parse-clojure'), turn it back into source code, and
insert it into the current buffer."
  (if (parseclj-ast-leaf-node-p ast)
      (insert (a-get ast :form))
    (if (eql (parseclj-ast-node-type ast) :tag)
        (parseclj-ast--unparse-tag ast)
      (parseclj-ast--unparse-collection ast))))

(defun parseclj-unparse-clojure-to-string (ast)
  "Parse Clojure AST to a source code string.

Given an abstract syntax tree AST (as returned by
`parseclj-parse-clojure'), turn it back into source code, and
return it as a string"
  (with-temp-buffer
    (parseclj-unparse-clojure ast)
    (buffer-substring-no-properties (point-min) (point-max))))

(provide 'parseclj)

;;; parseclj.el ends here
