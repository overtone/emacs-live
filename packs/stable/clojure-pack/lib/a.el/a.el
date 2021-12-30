;;; a.el --- Associative data structure functions   -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2021  Arne Brasseur

;; Author: Arne Brasseur <arne@arnebrasseur.net>
;; URL: https://github.com/plexus/a.el
;; Keywords: lisp
;; Version: 1.0.0
;; Package-Requires: ((emacs "25"))

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

;; Library for dealing with associative data structures: alists, hash-maps, and
;; vectors (for vectors, the indices are treated as keys).
;;
;; This library is largely inspired by Clojure, it has many of the functions
;; found in clojure.core, prefixed with `a-'. All functions treat their
;; arguments as immutable, so e.g. `a-assoc' will clone the hash-table or alist
;; it is given. Keep this in mind when writing performance sensitive code.

;;; Code:

(eval-when-compile (require 'subr-x)) ;; for things like hash-table-keys

(require 'cl-lib)
(require 'seq)

(defun a-associative-p (obj)
  (or (not obj)
      (hash-table-p obj)
      (and (consp obj)
           (consp (car obj)))))

(defalias 'a-associative? 'a-associative-p)

(defun a-get (map key &optional not-found)
  "Return the value MAP mapped to KEY, NOT-FOUND or nil if key not present."
  (cond
   ;; own implementation instead of alist-get so keys are checked with equal
   ;; instead of eq
   ((listp map)
    (a--alist-get map key not-found))

   ((vectorp map)
    (if (a-has-key? map key)
        (aref map key)
      not-found))

   ((hash-table-p map)
    (gethash key map not-found))
   (t (user-error "Not associative: %S" map))))

(defun a--alist-get (map key &optional not-found)
  "Like alist-get, but uses equal instead of eq to look up in map MAP key KEY.
Returns NOT-FOUND if the key is not present, or `nil' if
NOT-FOUND is not specified."
  (cl-block nil
    (seq-doseq (pair map)
      (when (equal (car pair) key)
        (cl-return (cdr pair))))
    not-found))

(defun a-get-in (m ks &optional not-found)
  "Look up a value in a nested associative structure.

Given a data structure M, and a sequence of keys KS, find the
value found by using each key in turn to do a lookup in the next
\"layer\". Return `nil' if the key is not present, or the NOT-FOUND
value if supplied."
  (let ((result m))
    (cl-block nil
      (seq-doseq (k ks)
        (if (a-has-key? result k)
            (setq result (a-get result k))
          (cl-return not-found)))
      result)))

(defmacro a-get* (&rest keys)
  "Look up a value in a nested associative structure.

Like a-get-in, but takes the key sequence KEYS directly as vararg
arguments, rather than as a single sequence."
  (cl-labels ((rec (keys)
                   `(a-get ,(if (and (consp (cdr keys))
                                     (cddr keys))
                                (rec (cdr keys))
                              (cadr keys))
                           ,(car keys))))
    (rec (nreverse keys))))

(defun a-has-key (coll k)
  "Check if the given associative collection COLL has a certain key K."
  (cond
   ((listp coll)         (not (eq (a--alist-get coll k :not-found) :not-found)))
   ((vectorp coll)       (and (integerp k) (< -1 k (length coll))))
   ((hash-table-p coll)  (not (eq (gethash k coll :not-found) :not-found)))
   (t (user-error "Not associative: %S" coll))))

(defalias 'a-has-key? 'a-has-key)

(defun a-assoc-1 (coll k v)
  "Like `a-assoc', (in COLL assoc K with V) but only takes a single k-v pair.
Internal helper function."
  (cond
   ((listp coll)
    (if (a-has-key? coll k)
        (mapcar (lambda (entry)
                  (if (equal (car entry) k)
                      (cons k v)
                    entry))
                coll)
      (cons (cons k v) coll)))

   ((vectorp coll)
    (if (and (integerp k) (>= k 0))
        (if (< k (length coll))
            (let ((copy (copy-sequence coll)))
              (aset copy k v)
              copy)
          (vconcat coll (make-list (- k (length coll)) nil) (list v)))))

   ((hash-table-p coll)
    (let ((copy (copy-hash-table coll)))
      (puthash k v copy)
      copy))))

(defun a-assoc (coll &rest kvs)
  "Return an updated collection COLL, associating values with keys KVS."
  (when (not (cl-evenp (a-count kvs)))
    (user-error "a-assoc requires an even number of arguments!"))
  (seq-reduce (lambda (coll kv)
                  (seq-let [k v] kv
                    (a-assoc-1 coll k v)))
              (seq-partition kvs 2)
              coll))

(defun a-keys (coll)
  "Return the keys in the collection COLL."
  (cond
   ((listp coll)
    (mapcar #'car coll))

   ((hash-table-p coll)
    (hash-table-keys coll))))

(defun a-vals (coll)
  "Return the values in the collection COLL."
  (cond
   ((listp coll)
    (mapcar #'cdr coll))

   ((hash-table-p coll)
    (hash-table-values coll))))

(defun a-reduce-kv (fn from coll)
  "Reduce with FN starting from FROM the collection COLL.
Reduce an associative collection COLL, starting with an initial
value of FROM. The reducing function FN receives the intermediate
value, key, and value."
  (seq-reduce (lambda (acc key)
                  (funcall fn acc key (a-get coll key)))
              (a-keys coll)
              from))

(defun a-count (coll)
  "Count the number of key-value pairs in COLL.
Like length, but can also return the length of hash tables."
  (cond
   ((seqp coll)
    (length coll))

   ((hash-table-p coll)
    (hash-table-count coll))))

(defun a-equal (a b)
  "Compare collections A, B for value equality.

Associative collections (hash tables and a-lists) are considered
equal if they contain equal key-value pairs, regardless of order.

Sequences (lists or vectors) are considered equal if they contain
the same elements in the same order.

Collection elements are compared using `a-equal'. In other words,
the equality check is recursive, resulting in a \"deep\" equality
check.

Anything that isn't associative or a sequence is compared with
`equal'."
  (cond
   ((and (a-associative? a) (a-associative? b))
    (or (equal a b)
        (when (eq (a-count a) (a-count b))
          (cl-block nil
            (seq-doseq (k (a-keys a))
              (when (not (and
                          (a-has-key b k)
                          (a-equal (a-get a k) (a-get b k))))
                (cl-return nil)))
            (seq-doseq (k (a-keys b))
              (when (not (and
                          (a-has-key a k)
                          (a-equal (a-get a k) (a-get b k))))
                (cl-return nil)))
            t))))
   ((and (sequencep a) (sequencep b))
    (and (eq (length a) (length b))
         (or (and (seq-empty-p a) (seq-empty-p b))
             (and (a-equal (elt a 0) (elt b 0))
                  (a-equal (seq-drop a 1) (seq-drop b 1))))))
   (t
    (equal a b))))

(defalias 'a-equal? 'a-equal)

(defun a-merge (&rest colls)
  "Merge multiple associative collections.
Return the type of the first collection COLLS."
  (seq-reduce (lambda (this that)
                (a-reduce-kv (lambda (coll k v)
                               (a-assoc coll k v))
                             this
                             that))
              (cdr colls)
              (car colls)))

(defun a-merge-with (f &rest colls)
  "Merge multiple associative collections.
Return the type of the first collection COLLS. If a key exists in
both, then combine the associated values by calling f on them."
  (seq-reduce (lambda (this that)
                (a-reduce-kv (lambda (coll k v)
                               (a-assoc coll k (if (a-has-key coll k)
                                                   (funcall f v (a-get coll k))
                                                 v)))
                             this
                             that))
              (cdr colls)
              (car colls)))

(defun a-alist (&rest kvs)
  "Create an association list from the given keys and values KVS.
Arguments are simply provided in sequence, rather than as lists or cons cells.
For example: (a-alist :foo 123 :bar 456)"
  (mapcar (lambda (kv) (cons (car kv) (cadr kv))) (seq-partition kvs 2)))

(defalias 'a-list 'a-alist)

(defun a-hash-table (&rest kvs)
  "Create a hash table from the given keys and values KVS.
Arguments are simply provided in sequence, rather than as lists
or cons cells. As \"test\" for the hash table, equal is used. The
hash table is created without extra storage space, so with a size
equal to amount of key-value pairs, since it is assumed to be
treated as immutable.
For example: (a-hash-table :foo 123 :bar 456)"
  (let* ((kv-pairs (seq-partition kvs 2))
         (hash-map (make-hash-table :test 'equal :size (length kv-pairs))))
    (seq-do (lambda (pair)
              (puthash (car pair) (cadr pair) hash-map))
            kv-pairs)
    hash-map))

(defun a-assoc-in (coll keys value)
  "In collection COLL, at location KEYS, associate value VALUE.
Associates a value in a nested associative collection COLL, where
KEYS is a sequence of keys and VALUE is the new value and returns
a new nested structure. If any levels do not exist, association
lists will be created."
  (cl-case (length keys)
    (0 coll)
    (1 (a-assoc-1 coll (elt keys 0) value))
    (t (a-assoc-1 coll
                  (elt keys 0)
                  (a-assoc-in (a-get coll (elt keys 0))
                              (seq-drop keys 1)
                              value)))))

(defun a-dissoc--list (list keys)
  "Return updated LIST with KEYS removed.
Internal helper. Use `a-dissoc' instead."
  (a-reduce-kv (lambda (res k v)
                 (if (member k keys)
                     res
                   (cons (cons k v) res)))
               nil
               list))

(defun a-dissoc--hash-table (table keys)
  "Return updated TABLE with KEYS removed.
Internal helper. Use `a-dissoc' instead."
  (let ((new-table (make-hash-table :size (hash-table-count table)
                                    :test (hash-table-test table)))
        (rest-keys (seq-remove (lambda (k)
                                 (member k keys))
                               (a-keys table))))
    (seq-doseq (k rest-keys)
      (puthash k (gethash k table) new-table))
    new-table))

(defun a-dissoc (coll &rest keys)
  "Return an updated version of collection COLL with the KEY removed."
  (cond
   ((listp coll) (a-dissoc--list coll keys))
   ((hash-table-p coll) (a-dissoc--hash-table coll keys))))

(defun a-update (coll key fn &rest args)
  "In collection COLL, at location KEY, apply FN with extra args ARGS.
'Updates' a value in an associative collection COLL, where KEY is
a key and FN is a function that will take the old value and any
supplied args and return the new value, and returns a new
structure. If the key does not exist, nil is passed as the old
value."
  (a-assoc-1 coll
             key
             (apply #'funcall fn (a-get coll key) args)))

(defun a-update-in (coll keys fn &rest args)
  "In collection COLL, at location KEYS, apply FN with extra args ARGS.
'Updates' a value in a nested associative collection COLL, where
KEYS is a sequence of keys and FN is a function that will take
the old value and any supplied ARGS and return the new value, and
returns a new nested structure. If any levels do not exist,
association lists will be created."
  (cl-case (length keys)
    (0 coll)
    (1 (apply #'a-update coll (elt keys 0) fn args))
    (t (a-assoc-1 coll
                  (elt keys 0)
                  (apply #'a-update-in
                         (a-get coll (elt keys 0))
                         (seq-drop keys 1)
                         fn
                         args)))))

(provide 'a)

;;; a.el ends here
