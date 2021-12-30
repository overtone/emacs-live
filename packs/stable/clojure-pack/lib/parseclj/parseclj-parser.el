;;; parseclj-parser.el --- Clojure/EDN parser              -*- lexical-binding: t; -*-

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

;; A shift/reduce parser for Clojure source.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'parseclj-lex)
(require 'parseclj-alist)

(define-error 'parseclj-parser-error "parseclj: Syntax error")

(defun parseclj--error (format &rest args)
  "Signal a parse error.
Takes a FORMAT string and optional ARGS to be passed to
`format-message'.  Signals a 'parseclj-parser-error signal, which
can be handled with `condition-case'."
  (signal 'parseclj-parser-error (list (apply #'format-message format args))))

(defun parseclj--find-opening-token (stack closing-token)
  "Scan STACK for an opening-token matching CLOSING-TOKEN."
  (let ((token-type (parseclj-lex-token-type closing-token)))
    (cond
     ((eq :rparen token-type) (parseclj-lex-token-type
                               (seq-find (lambda (token)
                                           (member (parseclj-lex-token-type token)
                                                   '(:lparen :lambda)))
                                         stack)))
     ((eq :rbracket token-type) :lbracket)
     ((eq :rbrace token-type) (parseclj-lex-token-type
                               (seq-find (lambda (token)
                                           (member (parseclj-lex-token-type token)
                                                   '(:lbrace :set)))
                                         stack))))))

(defun parseclj--reduce-coll (stack closing-token reduce-branch options)
  "Reduce collection based on the top of the STACK and a CLOSING-TOKEN.

REDUCE-BRANCH is a function to be applied to the collection of tokens found
from the top of the stack until an opening token that matches
CLOSING-TOKEN.  This function should return an AST token representing such
collection.

OPTIONS is an association list.  This list is also passed down to the
REDUCE-BRANCH function.  See `parseclj-parser' for more information on
available options."
  (let ((opening-token-type (parseclj--find-opening-token stack closing-token))
        (fail-fast (map-elt options :fail-fast t))
        (collection nil))
    (if (not opening-token-type)
        (if fail-fast
            (parseclj--error "At position %s, unmatched %S"
                             (map-elt closing-token :pos)
                             (parseclj-lex-token-type closing-token))

          stack)

      (progn
        ;; unwind the stack until opening-token-type is found, adding to collection
        (while (and stack (not (eq (parseclj-lex-token-type (car stack)) opening-token-type)))
          (push (pop stack) collection))

        ;; did we find the right token?
        (if (eq (parseclj-lex-token-type (car stack)) opening-token-type)
            (progn
              (when fail-fast
                ;; any unreduced tokens left: bail early
                (when-let ((token (seq-find #'parseclj-lex-token-p collection)))
                  (parseclj--error "At position %s, unmatched %S"
                                   (map-elt token :pos)
                                   (parseclj-lex-token-type token))))

              ;; all good, call the reducer so it can return an updated stack with a
              ;; new node at the top.
              (let ((opening-token (pop stack)))
                (funcall reduce-branch stack opening-token collection options)))

          ;; Unwound the stack without finding a matching paren: either bail early
          ;; or return the original stack and continue parsing
          (if fail-fast
              (parseclj--error "At position %s, unmatched %S"
                               (map-elt closing-token :pos)
                               (parseclj-lex-token-type closing-token))

            (reverse collection)))))))

(defun parseclj--take-value (stack value-p)
  "Scan STACK until a value is found.
Return everything up to the value in reversed order (meaning the value
comes first in the result).

STACK is the current parse stack to scan.

VALUE-P a predicate to distinguish reduced values from non-values (tokens
and whitespace)."
  (let ((result nil))
    (cl-block nil
      (while stack
        (cond
         ((parseclj-lex-token-p (car stack))
          (cl-return nil))

         ((funcall value-p (car stack))
          (cl-return (cons (car stack) result)))

         (t
          (push (pop stack) result)))))))

(defun parseclj--take-token (stack value-p token-types)
  "Scan STACK until a token of a certain type is found.
Returns nil if a value is encountered before a matching token is found.
Return everything up to the token in reversed order (meaning the token
comes first in the result).

STACK is the current parse stack to scan.

VALUE-P a predicate to distinguish reduced values from non-values (tokens
and whitespace).

TOKEN-TYPES are the token types to look for."
  (let ((result nil))
    (cl-block nil
      (while stack
        (cond
         ((member (parseclj-lex-token-type (car stack)) token-types)
          (cl-return (cons (car stack) result)))
         ((funcall value-p (car stack))
          (cl-return nil))
         ((parseclj-lex-token-p (car stack))
          (cl-return nil))
         (t
          (push (pop stack) result)))))))

(defun parseclj-single-value-p (stack value-p)
  "Return t if STACK only has a single node for which VALUE-P is true.

This checks if the stack contains a single, fully reduced value, and no
dangling unmatched tokens.  When parsing with `:read-one' this indicates a
form can be returned."
  (and (not (cl-reduce (lambda (bool node)
                         (or bool (parseclj-lex-token-p node)))
                       stack
                       :initial-value nil))
       (parseclj--take-value stack value-p)))

(defun parseclj-parser (reduce-leaf reduce-branch &optional options)
  "Clojure/EDN stack-based shift-reduce parser.

REDUCE-LEAF does reductions for leaf nodes.  It is a function that takes
the current value of the stack and a token, and either returns an updated
stack, with a new leaf node at the top (front), or returns the stack
unmodified.

REDUCE-BRANCH does reductions for branch nodes.  It is a function that
takes the current value of the stack, the type of branch node to create,
and a list of child nodes, and returns an updated stack, with the new node
at the top (front).

What \"node\" means in this case is up to the reducing functions, it could
be AST nodes (as in the case of `parseclj-parser-clojure'), or plain
values/sexps (as in the case of `parseedn-read'), or something else. The
only requirement is that they should not put raw tokens back on the stack,
as the parser relies on the presence or absence of these to detect parse
errors.

OPTIONS is an association list which is passed on to the reducing
functions. Additionally the following options are recognized

- `:fail-fast'
  Raise an error when a parse error is encountered, rather than continuing
  with a partial result.
- `:value-p'
  A predicate function to differentiate values from tokens and
  whitespace. This is needed when scanning the stack to see if any
  reductions can be performed. By default anything that isn't a token is
  considered a value. This can be problematic when parsing with
  `:lexical-preservation', and which case you should provide an
  implementation that also returns falsy for :whitespace, :comment, and
  :discard AST nodes.
- `:tag-readers'
  An association list that describes tag handler functions for any possible
  tag.  This options in only available in `parseedn-read', for more
  information, please refer to its documentation.
- `:read-one'
  Return as soon as a single complete value has been read."
  (let ((fail-fast (map-elt options :fail-fast t))
        (read-one (map-elt options :read-one))
        (value-p (map-elt options :value-p (lambda (e) (not (parseclj-lex-token-p e)))))
        (stack nil)
        (token (parseclj-lex-next)))

    (while (not (or (and read-one (parseclj-single-value-p stack value-p))
                    (eq (parseclj-lex-token-type token) :eof)))
      ;; (message "STACK: %S" stack)
      ;; (message "TOKEN: %S\n" token)

      (when (and fail-fast (parseclj-lex-error-p token))
        (parseclj--error "Invalid token at %s: %S"
                         (map-elt token :pos)
                         (parseclj-lex-token-form token)))

      ;; Reduce based on the top item on the stack (collections)
      (cond
       ((parseclj-lex-leaf-token-p token)
        (setf stack (funcall reduce-leaf stack token options)))

       ((parseclj-lex-closing-token-p token)
        (setf stack (parseclj--reduce-coll stack token reduce-branch options)))

       (t (push token stack)))

      ;; Reduce based on top two items on the stack (special prefixed elements)
      (let* ((top-value (parseclj--take-value stack value-p))
             (opening-token (parseclj--take-token (nthcdr (length top-value) stack) value-p parseclj-lex--prefix-tokens))
             new-stack)
        (while (and top-value opening-token)
          ;; (message "Reducing...")
          ;; (message "  - STACK %S" stack)
          ;; (message "  - OPENING-TOKEN %S" opening-token)
          ;; (message "  - TOP-VALUE %S" top-value)
          (setq new-stack (nthcdr (+ (length top-value) (length opening-token)) stack))
          (setq stack (funcall reduce-branch new-stack (car opening-token) (append (cdr opening-token) top-value) options))

          ;; recur
          (setq top-value (parseclj--take-value stack value-p))
          (setq opening-token (parseclj--take-token (nthcdr (length top-value) stack) value-p parseclj-lex--prefix-tokens))))

      ;; Reduce based on top three items on the stack (metadata, namespaced maps)
      (let* ((top-value-1 (parseclj--take-value stack value-p))
             (top-value-2 (parseclj--take-value (nthcdr (length top-value-1) stack) value-p))
             (opening-token (parseclj--take-token (nthcdr (+ (length top-value-1)
                                                             (length top-value-2)) stack) value-p parseclj-lex--prefix-2-tokens))
             new-stack)
        (while (and top-value-1 top-value-2 opening-token)
          (setq new-stack (nthcdr (apply #'+ (mapcar #'length (list top-value-1 top-value-2 opening-token))) stack))
          (setq stack (funcall reduce-branch new-stack (car opening-token) (append (cdr opening-token) top-value-2 top-value-1) options))

          ;; recur
          (setq top-value-1 (parseclj--take-value stack value-p))
          (setq top-value-2 (parseclj--take-value (nthcdr (length top-value-1) stack) value-p))
          (setq opening-token (parseclj--take-token (nthcdr (+ (length top-value-1)
                                                               (length top-value-2)) stack) value-p parseclj-lex--prefix-2-tokens))))

      (setq token (parseclj-lex-next)))

    ;; reduce root
    (when fail-fast
      (when-let ((token (seq-find #'parseclj-lex-token-p stack)))
        (parseclj--error "At position %s, unmatched %S"
                         (map-elt token :pos)
                         (parseclj-lex-token-type token))))

    (if read-one
        (car (parseclj--take-value stack value-p))
      (car (funcall reduce-branch nil (parseclj-lex-token :root "" 1)
                    (reverse stack)
                    options)))))

(provide 'parseclj-parser)
;;; parseclj-parser.el ends here
