;;; a-test.el --- Associative data structure functions: tests              -*- lexical-binding: t; -*-

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

;; Test suite for a.el

;;; Code:

(require 'a)
(require 'ert)

(ert-deftest a-associative?-test ()
  (should (a-associative? nil))
  (should (a-associative? (a-list)))
  (should (a-associative? (a-list :foo 1)))
  (should (a-associative? (a-hash-table :foo 1)))
  (should (not (a-associative? '(1 2 3)))))

(ert-deftest a-get-test ()
  (should (equal (a-get '((:foo . 5)) :foo) 5))
  (should (equal (a-get '((:foo . 5)) :bar) nil))
  (should (equal (a-get '((:foo . 5)) :bar :fallback) :fallback))
  (should (equal (a-get [1 2 3] 1) 2))
  (should (equal (a-get [1 2 3] 5) nil))
  (should (equal (a-get [1 2 3] 5 :fallback) :fallback))
  (should-error (a-get 5 :nope))

  (let ((hash (make-hash-table :test #'equal)))
    (puthash :foo 123 hash)
    (puthash :bar 456 hash)
    (should (equal (a-get hash :bar) 456))
    (should (equal (a-get hash :baz) nil))
    (should (equal (a-get hash :baz :baq) :baq))))

(ert-deftest a-get-in-test ()
  (should (equal (a-get-in [1 2 [3 4 [5] 6]] [2 2 0]) 5))
  (should (equal (a-get-in [1 ((:a . ((:b . [3 4 5]))))] [1 :a :b 2]) 5))
  (should (equal (a-get-in [] []) []))
  (should (equal (a-get-in [] [2] :foo) :foo)))

(ert-deftest a-has-key-test ()
  (should (a-has-key [1 2 3] 2))
  (should (not (a-has-key [1 2 3] 3)))
  (should (not (a-has-key [1 2 3] -1)))
  (should (not (a-has-key [1 2 3] :foo)))
  (should-error (a-has-key? 1 :nope))

  (should (a-has-key '((:a . 5)) :a))
  (should (not (a-has-key '((:a . 5)) :b)))

  (let ((hash (make-hash-table :test #'equal)))
    (puthash :foo 123 hash)
    (should (equal (a-has-key hash :foo) t))
    (should (equal (a-has-key hash :bar) nil))))

(ert-deftest a-assoc-test ()
  (should (equal (a-assoc '() :foo :bar) '((:foo . :bar))))
  (should (equal (a-assoc '((:foo . :baz)) :foo :bar) '((:foo . :bar))))
  (should (equal (a-assoc '((:foo . :baz))
                          :foo :bar
                          :baz :baq) '((:baz . :baq) (:foo . :bar))))

  (should (equal (a-assoc [1 2 3] 0 :foo) [:foo 2 3]))
  (should (equal (a-assoc [1 2 3] 1 :foo) [1 :foo 3]))
  (should (equal (a-assoc [1 2 3] 5 :foo) [1 2 3 nil nil :foo]))
  (should-error (a-assoc '() :foo))

  (let ((hash (make-hash-table :test #'equal)))
    (puthash :foo 123 hash)
    (should (a-equal (a-assoc hash :bar :val) '((:bar . :val) (:foo . 123))))))

(ert-deftest a-keys-test ()
  (should (equal (a-keys '((:a . 1) (:b . 2))) '(:a :b)))

  (let ((hash (make-hash-table :test #'equal)))
    (puthash :foo 123 hash)
    (puthash :bar 123 hash)

    (should (equal (sort (a-keys hash) #'string<) '(:bar :foo)))))

(ert-deftest a-vals-test ()
  (should (equal (a-vals '((:a . 1) (:b . 2))) '(1 2)))
  (let ((hash (make-hash-table :test #'equal)))
    (puthash :foo 123 hash)
    (puthash :bar 456 hash)
    (should (equal (sort (a-vals hash) #'<) '(123 456)))))

(ert-deftest a-reduce-kv ()
  (should (equal
           (a-reduce-kv
            (lambda (acc k v)
              (cons
               (concat (symbol-name k) "--" (number-to-string v))
               acc))
            nil '((:a . 1) (:b . 2)))
           '(":b--2" ":a--1"))))

(ert-deftest a-reduce-kv-test ()
  (should (equal
           (a-reduce-kv
            (lambda (acc k v)
              (cons
               (concat (symbol-name k) "--" (number-to-string v))
               acc))
            nil '((:a . 1) (:b . 2)))
           '(":b--2" ":a--1"))))

(ert-deftest a-equal-test ()
  (should (a-equal '((:a . 1) (:b . 2)) '((:b . 2) (:a . 1))))
  (should (a-equal '((:a . 1) (:b . 2)) '((:a . 1) (:b . 2))))
  (should (not (a-equal '((:a . 1) (:c . 2)) '((:a . 1) (:b . 2)))))
  (should (not (a-equal '((:a . 1) (:b . 2)) '((:a . 1) (:b . 3)))))
  (should (not (a-equal '((:a . 1) (:b . 2)) '((:a . 1)))))
  (should (not (a-equal '((:a . 1) (:b . 2)) '((:a . 1) (:b . 2) (:c . 3)))))

  (should (not (a-equal '(((:position . 5))) '(((:position . 15))))))

  (let ((hash (make-hash-table :test #'equal)))
    (puthash :foo 123 hash)
    (puthash :bar 456 hash)
    (should (a-equal hash '((:foo . 123) (:bar . 456)))))

  (should (a-equal '((:a . 1) (:b . 2)) '((:b . 2) (:a . 1))))

  (should (a-equal 1 1))
  (should (a-equal '(1 2 3) [1 2 3]))
  (should (not (a-equal '(1 2 3 4) [1 2 3])))
  (should (not (a-equal '(1 2 3) [1 2 3 4])))
  (should (a-equal (a-list :foo (a-hash-table :bar [1 2] :baz nil))
                   (a-hash-table :foo (a-list :bar '(1 2) :baz nil)))))

(ert-deftest a-merge-test ()
  (should
   (a-equal
    (a-merge
     '((:a . 1) (:b . 2)))
    '((:b . 2) (:a . 1))))

  (should
   (a-equal
    (a-merge
     nil
     '((:a . 1) (:b . 2)))
    '((:b . 2) (:a . 1))))

  (should
   (a-equal
    (a-merge
     nil
     '((:a . 1) (:b . 2))
     '((:c . 3) (:b . 5)))
    '((:a . 1) (:b . 5) (:c . 3)))))

(ert-deftest a-merge-with-test ()
  (should (a-equal (a-merge-with '+ (a-list :x 5 :y 3) (a-list :x 7 :z 6))
                   (a-list :z 6 :x 12 :y 3))))

(ert-deftest a-alist ()
  (should
   (a-equal
    (a-alist :a 1 :b 2)
    '((:b . 2) (:a . 1)))))

(ert-deftest a-assoc-in-test ()
  (should
   (equal
    (a-assoc-in (a-alist :foo (a-alist :bar [1 2 3])) [:foo :bar 2] 5)
    '((:foo
       (:bar . [1 2 5])))))

  (should
   (equal
    (a-assoc-in (a-alist :foo nil) [:foo :bar 2] 5)
    '((:foo . ((:bar . ((2 . 5)))))))))

(ert-deftest a-dissoc-test ()
  (should (a-equal
           (a-dissoc (a-list :foo 1 :baz 2 :baq 3) :baz)
           (a-list :foo 1 :baq 3)))
  (should (a-equal
           (a-dissoc (a-list :foo 1 :baz 2 :baq 3) :baq :foo)
           (a-list :baz 2)))
  (should (a-equal
           (a-dissoc (a-hash-table :foo 1 :baz 2 :baq 3) :baq)
           (a-hash-table  :foo 1 :baz 2))))

(ert-deftest a-update-in-test ()
  (should
   (equal
    (a-update-in (a-alist :foo (a-alist :bar [1 2 "x"]))
                 [:foo :bar 2]
                 #'concat "y")
    '((:foo (:bar . [1 2 "xy"]))))))

(ert-deftest a-get*-test ()
  (let ((alphabets (a-list "Greek" (a-list 1 (a-list 'letter "α"
                                                     'name "alpha")
                                           2 (a-list 'letter "β"
                                                     'name "beta"))
                           "English" (a-list 1 (a-list 'letter "a"
                                                       'name "A")
                                             2 (a-list 'letter "b"
                                                       'name "B")))))
    (should (equal (a-get* alphabets "Greek" 1 'letter)
                   "α"))))

;;; a-test.el ends here
