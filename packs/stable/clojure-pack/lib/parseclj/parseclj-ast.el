;;; parseclj-ast.el --- Clojure parser/unparser              -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2021  Arne Brasseur

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

;; Parse Clojure code to an AST, and unparse back to code.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'parseclj-lex)
(require 'parseclj-alist)

;; AST helper functions

(defun parseclj-ast-node (type position &rest attributes)
  "Create an AST node with given TYPE and POSITION.
Other ATTRIBUTES can be given as a flat list of key-value pairs."
  (apply 'parseclj-alist :node-type type :position position attributes))

(defun parseclj-ast-node-p (node)
  "Return t if the given NODE is a Clojure AST node."
  (and (consp node)
       (consp (car node))
       (eq :node-type (caar node))))

(defun parseclj-ast-node-attr (node attr)
  "Return NODE's ATTR, or nil."
  (map-elt node attr))

(defun parseclj-ast-node-type (node)
  "Return the type of the AST node NODE."
  (map-elt node :node-type))

(defun parseclj-ast-children (node)
  "Return children for the AST NODE."
  (map-elt node :children))

(defun parseclj-ast-value (node)
  "Return the value of NODE as another AST node."
  (map-elt node :value))

(defun parseclj-ast-leaf-node-p (node)
  "Return t if the given ast NODE is a leaf node."
  (member (parseclj-ast-node-type node) parseclj-lex--leaf-tokens))

(defun parseclj-ast-branch-node-p (node)
  "Return t if the given AST NODE is a branch node."
  (not (parseclj-ast-leaf-node-p node)))


;; Parse/reduce strategy functions

(defun parseclj-ast--reduce-leaf (stack token &optional _options)
  "Put into the STACK an AST leaf node based on TOKEN.
Ignores white spaces and comments.

OPTIONS is an association list.  See `parseclj-parse' for more information
on available options."
  (if (member (parseclj-lex-token-type token) '(:whitespace :comment))
      stack
    (cons
     (parseclj-ast-node (parseclj-lex-token-type token)
                        (map-elt token :pos)
                        :form (map-elt token :form)
                        :value (parseclj-lex--leaf-token-value token))
     stack)))

(defun parseclj-ast--reduce-leaf-with-lexical-preservation (stack token options)
  "Put into STACK an AST leaf node based on TOKEN.
This function is very similar to `parseclj-ast--reduce-leaf', but unlike
it, takes into account tokens representing white space or comments and
saves them into the STACK.  Nodes produced by this function have a
`:lexical-preservation' key set to t.

OPTIONS is an association list.  See `parseclj-parse' for more information
on available options."
  (let ((token-type (parseclj-lex-token-type token))
        (top (car stack)))
    (if (member token-type '(:whitespace :comment))
        ;; merge consecutive whitespace or comment tokens
        (if (eq token-type (map-elt top :node-type))
            (cons (parseclj-alist-update top :form #'concat (map-elt token :form))
                  (cdr stack))
          (cons (parseclj-ast-node (parseclj-lex-token-type token)
                                   (map-elt token :pos)
                                   :form (map-elt token :form))
                stack))
      (parseclj-ast--reduce-leaf stack token options))))

(defun parseclj-ast--reduce-branch (stack opening-token children _options)
  "Reduce STACK with an AST branch node representing a collection of elements.
Ignores discard tokens.

OPENING-TOKEN is a lex token representing an opening paren, bracket or
brace.
CHILDREN is the collection of nodes to be reduced into the AST branch node.
OPTIONS is an association list.  See `parseclj-parse' for more information
on available options."
  (let* ((pos (map-elt opening-token :pos))
         (type (parseclj-lex-token-type opening-token))
         (type (cond
                ((eq :lparen type) :list)
                ((eq :lbracket type) :vector)
                ((eq :lbrace type) :map)
                (t type))))
    (cond
     ((eq :root type) (cons (parseclj-ast-node :root pos :children children) stack))
     ((eq :discard type) stack)
     ((eq :tag type) (cons (parseclj-ast-node :tag
                                              pos
                                              :tag (intern (substring (map-elt opening-token :form) 1))
                                              :children children)
                           stack))
     ((eq :metadata type) (cons (parseclj-ast-node :with-meta
                                                   pos
                                                   :children children)
                                stack))
     ((eq :map-prefix type) (cons (parseclj-alist-assoc (car children)
                                                        :map-prefix opening-token)
                                  stack))
     (t (cons
         (parseclj-ast-node type pos :children children)
         stack)))))

(defun parseclj-ast--reduce-branch-with-lexical-preservation (stack opening-token children options)
  "Reduce STACK with an AST branch node representing a collection of elements.
Similar to `parseclj-ast--reduce-branch', but reduces discard tokens as
well.  Nodes produced by this function have a `:lexical-preservation'
key set to t.

OPENING-TOKEN is a lex token representing an opening paren, bracket or
brace.
CHILDREN is the collection of tokens to be reduced into the AST branch
node.
OPTIONS is an association list.  See `parseclj-parse' for more information
on available options."
  (if (eq :discard (parseclj-lex-token-type opening-token))
      (cons (parseclj-ast-node :discard (map-elt opening-token :pos) :children children) stack)
    (let* ((stack (funcall #'parseclj-ast--reduce-branch stack opening-token children options))
           (top (car stack)))
      (if (parseclj-ast-node-p top)
          (cons (cl-list* (car top) ;; make sure :node-type remains the first element in the list
                          '(:lexical-preservation . t)
                          (cdr top))
                (cdr stack))
        stack))))



;; Unparse functions

(declare-function parseclj-unparse-clojure "parseclj")

(defun parseclj-ast--unparse-collection (node)
  "Insert a string representation of the given AST branch NODE into buffer."
  (let* ((token-type (parseclj-ast-node-type node))
         (delimiters (cond
                      ((eq :root token-type) (cons "" ""))
                      ((eq :list token-type) (cons "(" ")"))
                      ((eq :vector token-type) (cons "[" "]"))
                      ((eq :set token-type) (cons "#{" "}"))
                      ((eq :map token-type) (cons "{" "}")))))
    (insert (car delimiters))
    (let ((nodes (alist-get ':children node)))
      (when-let (node (car nodes))
        (parseclj-unparse-clojure node))
      (seq-doseq (child (cdr nodes))
        (when (not (map-elt node :lexical-preservation))
          (insert " "))
        (parseclj-unparse-clojure child)))
    (insert (cdr delimiters))))

(defun parseclj-ast--unparse-tag (node)
  "Insert a string representation of the given AST tag NODE into buffer."
  (progn
    (insert "#")
    (insert (symbol-name (map-elt node :tag)))
    (insert " ")
    (parseclj-unparse-clojure (car (map-elt node :children)))))

(provide 'parseclj-ast)

;;; parseclj-ast.el ends here
