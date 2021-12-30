;;; parseclj-alist.el --- Clojure/EDN parser              -*- lexical-binding: t; -*-

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

(defun parseclj-alist (&rest kvs)
  "Create an association list from the given keys and values KVS.
Arguments are simply provided in sequence, rather than as lists or cons cells.
For example: (parseclj-alist :foo 123 :bar 456)"
  ;; Emacs 27:
  ;; (map-into kvs 'alist)
  (mapcar (lambda (kv) (cons (car kv) (cadr kv))) (seq-partition kvs 2)))

(defun parseclj-alist-assoc (coll k v)
  "Associate a key K with a value V in the association list COLL

Returns a new alist (does not mutate its argument). If an entry
with the same key is present it will be replaced, otherwise the
new kv-pair is added to the head of the list."
  (if (map-contains-key coll k)
      (mapcar (lambda (entry)
                (if (equal (car entry) k)
                    (cons k v)
                  entry))
              coll)
    (cons (cons k v) coll)))

(defun parseclj-alist-update (coll key fn &rest args)
  "In collection COLL, at location KEY, apply FN with extra args ARGS.
'Updates' a value in an associative collection COLL, where KEY is
a key and FN is a function that will take the old value and any
supplied args and return the new value, and returns a new
structure. If the key does not exist, nil is passed as the old
value."
  (parseclj-alist-assoc coll
                        key
                        (apply #'funcall fn (map-elt coll key) args)))

(defun parseclj-hash-table (&rest kvs)
  "Create a hash table from the given keys and values KVS.
Arguments are simply provided in sequence, rather than as lists
or cons cells. As \"test\" for the hash table, equal is used. The
hash table is created without extra storage space, so with a size
equal to amount of key-value pairs, since it is assumed to be
treated as immutable.
For example: (parseclj-hash-table :foo 123 :bar 456)"
  ;; Emacs 27:
  ;; (map-into kvs 'hash-table)
  (let* ((kv-pairs (seq-partition kvs 2))
         (hash-map (make-hash-table :test 'equal :size (length kv-pairs))))
    (seq-do (lambda (pair)
              (puthash (car pair) (cadr pair) hash-map))
            kv-pairs)
    hash-map))

(defun parseclj-alist-merge (l1 l2)
  "Merge two association lists."
  ;; Emacs 27: (map-merge 'alist l1 l2)
  (let ((keys (delete-dups (append (mapcar #'car l1) (mapcar #'car l2))))
        (res '()))
    (mapcar
     (lambda (key)
       (push (or (assoc key l2)
                 (assoc key l1))
             res))
     keys)
    (nreverse res)))

(provide 'parseclj-alist)

;;; parseclj-alist.el ends here
