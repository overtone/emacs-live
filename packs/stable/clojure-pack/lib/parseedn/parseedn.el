;;; parseedn.el --- Clojure/EDN parser              -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2021  Arne Brasseur

;; Author: Arne Brasseur <arne@arnebrasseur.net>
;; Keywords: lisp clojure edn parser
;; Package-Requires: ((emacs "26") (parseclj "1.0.6") (map "2"))
;; Version: 1.0.6

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

;; parseedn is an Emacs Lisp library for parsing EDN (Clojure) data.
;; It uses parseclj's shift-reduce parser internally.

;; EDN and Emacs Lisp have some important differences that make
;; translation from one to the other not transparent (think
;; representing an EDN map into Elisp, or being able to differentiate
;; between false and nil in Elisp). Because of this, parseedn takes
;; certain decisions when parsing and transforming EDN data into Elisp
;; data types.  For more information please refer to parseclj's design
;; documentation.

;;; Code:

;; The EDN spec is not clear about whether \u0123 and \o012 are supported in
;; strings. They are described as character literals, but not as string escape
;; codes. In practice all implementations support them (mostly with broken
;; surrogate pair support), so we do the same. Sorry, emoji 🙁.
;;
;; Note that this is kind of broken, we don't correctly detect if \u or \o forms
;; don't have the right forms.

(require 'cl-lib)
(require 'map)
(require 'parseclj-parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reader

(defvar parseedn-default-tag-readers
  (list (cons 'inst (lambda (s)
                      (cl-list* 'edn-inst (date-to-time s))))
        (cons 'uuid (lambda (s)
                      (list 'edn-uuid s))))
  "Default reader functions for handling tagged literals in EDN.
These are the ones defined in the EDN spec, #inst and #uuid.  It
is not recommended you change this variable, as this globally
changes the behavior of the EDN reader.  Instead pass your own
handlers as an optional argument to the reader functions.")

(defun parseedn-reduce-leaf (stack token _options)
  "Put in the STACK an elisp value representing TOKEN.

OPTIONS is an association list.  See `parseclj-parse' for more information
on available options."
  (if (member (parseclj-lex-token-type token) (list :whitespace :comment))
      stack
    (cons (parseclj-lex--leaf-token-value token) stack)))

(defun parseedn-reduce-branch (stack opening-token children options)
  "Reduce STACK with an sequence containing a collection of other elisp values.
Ignores discard tokens.

OPENING-TOKEN is a lex token representing an opening paren, bracket or
brace.
CHILDREN is a collection elisp values to be reduced into an elisp
sequence.
OPTIONS is an association list.  See `parseclj-parse' for more information
on available options."
  (let ((tag-readers (parseclj-alist-merge parseedn-default-tag-readers (alist-get :tag-readers options)))
        (token-type (parseclj-lex-token-type opening-token)))
    (if (eq token-type :discard)
        stack
      (cons
       (cond
        ((eq :root token-type) children)
        ((eq :lparen token-type) children)
        ((eq :lbracket token-type) (apply #'vector children))
        ((eq :set token-type) (list 'edn-set children))
        ((eq :lbrace token-type) (let* ((kvs (seq-partition children 2))
                                        (hash-map (make-hash-table :test 'equal :size (length kvs))))
                                   (seq-do (lambda (pair)
                                             (puthash (car pair) (cadr pair) hash-map))
                                           kvs)
                                   hash-map))
        ((eq :tag token-type) (let* ((tag (intern (substring (alist-get :form opening-token) 1)))
                                     (reader (alist-get tag tag-readers :missing)))
                                (when (eq :missing reader)
                                  (user-error "No reader for tag #%S in %S" tag (map-keys tag-readers)))
                                (funcall reader (car children)))))
       stack))))

(defun parseedn-read (&optional tag-readers)
  "Read content from current buffer and parse it as EDN source.
Returns an Emacs Lisp value.

TAG-READERS is an optional association list where keys are symbols
identifying *tags*, and values are tag handler functions that receive one
argument: *the tagged element*, and specify how to interpret it."
  (parseclj-parser #'parseedn-reduce-leaf
                   #'parseedn-reduce-branch
                   (list (cons :tag-readers tag-readers))))

(defun parseedn-read-str (s &optional tag-readers)
  "Parse string S as EDN.
Returns an Emacs Lisp value.

TAG-READERS is an optional association list.  For more information, see
`parseedn-read'."
  (with-temp-buffer
    (insert s)
    (goto-char 1)
    (car (parseedn-read tag-readers))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Printer

(defun parseedn-print-seq (coll)
  "Insert sequence COLL as EDN into the current buffer."
  (parseedn-print (elt coll 0))
  (let ((next (seq-drop coll 1)))
    (when (not (seq-empty-p next))
      (insert " ")
      (parseedn-print-seq next))))

(defun parseedn-print-hash-or-alist (map &optional ks)
  "Insert hash table MAP or elisp alist as an EDN map into the current buffer."
  (let ((alist? (listp map))
        (keys (or ks (map-keys map))))
    (parseedn-print (car keys))
    (insert " ")
    (parseedn-print (map-elt map (car keys)))
    (let ((next (cdr keys)))
      (when (not (seq-empty-p next))
        (insert ", ")
        (parseedn-print-hash-or-alist map next)))))

(defun parseedn-print-plist (plist)
  "Insert an elisp property list as an EDN map into the current buffer."
  (parseedn-print (car plist))
  (insert " ")
  (parseedn-print (cadr plist))
  (let ((next (cddr plist)))
    (when (not (seq-empty-p next))
      (insert ", ")
      (parseedn-print-plist next))))

(defun parseedn-print-inst (time)
  "Insert an inst value into the current buffer.

Take an encode-time style value and print it as a timestamp
deliniated by double quotes."
  (insert (format-time-string "\"%Y-%m-%dT%T\"" time)))

(defun parseedn-alist-p (list)
  "Non-null if and only if LIST is an alist with simple keys."
  (while (consp list)
    (setq list (if (and (consp (car list))
                        (atom (caar list)))
                   (cdr list)
                 'not-alist)))
  (null list))

(defun parseedn-plist-p (list)
  "Non-null if and only if LIST is a plist with keyword keys."
  (while (consp list)
    (setq list (if (and (keywordp (car list))
                        (consp (cdr list)))
                   (cddr list)
                 'not-plist)))
  (null list))

(defun parseedn-print (datum)
  "Insert DATUM as EDN into the current buffer.
DATUM can be any Emacs Lisp value."
  (cond
   ((or (null datum) (numberp datum))
    (prin1 datum (current-buffer)))

   ((stringp datum)
    (insert "\"")
    (seq-doseq (char datum)
      (insert (cond
               ((eq ?\t char) "\\t")
               ((eq ?\f char) "\\f")
               ((eq ?\" char) "\\\"")
               ((eq ?\r char) "\\r")
               ((eq ?\n char) "\\n")
               ((eq ?\\ char) "\\\\")
               (t (char-to-string char)))))
    (insert "\""))

   ((eq t datum)
    (insert "true"))

   ((or (keywordp datum) (symbolp datum))
    (insert (symbol-name datum)))

   ((vectorp datum) (insert "[") (parseedn-print-seq datum) (insert "]"))

   ((or (hash-table-p datum) (parseedn-alist-p datum))
    (insert "{")
    (parseedn-print-hash-or-alist datum)
    (insert "}"))

   ((parseedn-plist-p datum)
    (insert "{")
    (parseedn-print-plist datum)
    (insert "}"))

   ((consp datum)
    (cond
     ((not (listp (cdr datum))) ; dotted pair
      (error "Don't know how to print: %s" datum))
     ((eq 'edn-set (car datum))
      (insert "#{") (parseedn-print-seq (cadr datum)) (insert "}"))
     ((eq 'edn-uuid (car datum))
      (insert "#uuid ") (parseedn-print-seq (cdr datum)))
     ((eq 'edn-inst (car datum))
      (insert "#inst ") (parseedn-print-inst (cdr datum)))
     (t (insert "(") (parseedn-print-seq datum) (insert ")"))))

   (t (error "Don't know how to print: %s" datum))))

(defun parseedn-print-str (datum)
  "Return a string containing DATUM as EDN.
DATUM can be any Emacs Lisp value."
  (with-temp-buffer
    (parseedn-print datum)
    (buffer-substring-no-properties (point-min) (point-max))))

(provide 'parseedn)

;;; parseedn.el ends here
