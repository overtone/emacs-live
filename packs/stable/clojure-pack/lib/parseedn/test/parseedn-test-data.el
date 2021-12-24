;;; parseedn-test-data.el --- Clojure/EDN parser - test data

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

;; Test data for reader / parser / printer / unparser

;;; Code:

(setq parseedn-test-data
      (a-list

       "simple-list"
       (a-list
        :tags '(:edn-roundtrip)
        :source "(1 2 3)"
        :edn '((1 2 3))
        :ast '((:node-type . :root)
               (:position . 1)
               (:children . (((:node-type . :list)
                              (:position . 1)
                              (:children . (((:node-type . :number)
                                             (:position . 2)
                                             (:form . "1")
                                             (:value . 1))
                                            ((:node-type . :number)
                                             (:position . 4)
                                             (:form . "2")
                                             (:value . 2))
                                            ((:node-type . :number)
                                             (:position . 6)
                                             (:form . "3")
                                             (:value . 3)))))))))


       "empty-list"
       (a-list
        :source "()"
        :edn '(())
        :ast '((:node-type . :root)
               (:position . 1)
               (:children . (((:node-type . :list)
                              (:position . 1)
                              (:children . nil))))))

       "size-1"
       (a-list
        :tags '(:edn-roundtrip)
        :source "(1)"
        :edn '((1))
        :ast '((:node-type . :root)
               (:position . 1)
               (:children . (((:node-type . :list)
                              (:position . 1)
                              (:children . (((:node-type . :number)
                                             (:position . 2)
                                             (:form . "1")
                                             (:value . 1)))))))))

       "leafs"
       (a-list
        :source "(nil true false hello-world)"
        :edn '((nil t nil hello-world))
        :ast '((:node-type . :root)
               (:position . 1)
               (:children . (((:node-type . :list)
                              (:position . 1)
                              (:children . (((:node-type . :nil)
                                             (:position . 2)
                                             (:form . "nil")
                                             (:value . nil))
                                            ((:node-type . :true)
                                             (:position . 6)
                                             (:form . "true")
                                             (:value . t))
                                            ((:node-type . :false)
                                             (:position . 11)
                                             (:form . "false")
                                             (:value . nil))
                                            ((:node-type . :symbol)
                                             (:position . 17)
                                             (:form . "hello-world")
                                             (:value . hello-world)))))))))

       "qualified-symbol"
       (a-list
        :tags '(:edn-roundtrip)
        :source "clojure.string/join"
        :edn '(clojure.string/join)
        :ast '((:node-type . :root)
               (:position . 1)
               (:children . (((:node-type . :symbol)
                              (:position . 1)
                              (:form . "clojure.string/join")
                              (:value . clojure.string/join))))))

       "nested-lists"
       (a-list
        :source "((.9 abc (true) (hello)))"
        :edn '(((0.9 abc (t) (hello))))
        :ast '((:node-type . :root)
               (:position . 1)
               (:children . (((:node-type . :list)
                              (:position . 1)
                              (:children . (((:node-type . :list)
                                             (:position . 2)
                                             (:children ((:node-type . :number)
                                                         (:position . 3)
                                                         (:form . ".9")
                                                         (:value . 0.9))
                                                        ((:node-type . :symbol)
                                                         (:position . 6)
                                                         (:form . "abc")
                                                         (:value . abc))
                                                        ((:node-type . :list)
                                                         (:position . 10)
                                                         (:children ((:node-type . :true)
                                                                     (:position . 11)
                                                                     (:form . "true")
                                                                     (:value . t))))
                                                        ((:node-type . :list)
                                                         (:position . 17)
                                                         (:children ((:node-type . :symbol)
                                                                     (:position . 18)
                                                                     (:form . "hello")
                                                                     (:value . hello)))))))))))))

       "strings-1"
       (a-list
        :tags '(:edn-roundtrip)
        :source "\"abc hello \\t\\\"x\""
        :edn '("abc hello \t\"x")
        :ast '((:node-type . :root)
               (:position . 1)
               (:children . (((:node-type . :string)
                              (:position . 1)
                              (:form . "\"abc hello \\t\\\"x\"")
                              (:value . "abc hello \t\"x"))))))

       "strings-2"
       (a-list
        :source "(\"---\\f---\\\"-'\\'-\\\\-\\r\\n\")"
        :edn '(("---\f---\"-''-\\-\r\n"))
        :ast '((:node-type . :root)
               (:position . 1)
               (:children . (((:node-type . :list)
                              (:position . 1)
                              (:children . (((:node-type . :string)
                                             (:position . 2)
                                             (:form . "\"---\\f---\\\"-'\\'-\\\\-\\r\\n\"")
                                             (:value . "---\f---\"-''-\\-\r\n")))))))))

       "chars-1"
       (a-list
        :source "(\\newline \\return \\space \\tab \\a \\b \\c \\u0078 \\o171)"
        :edn '((?\n ?\r ?\ ?\t ?a ?b ?c ?x ?y))
        :ast '((:node-type . :root)
               (:position . 1)
               (:children . (((:node-type . :list)
                              (:position . 1)
                              (:children . (((:node-type . :character) (:position . 2) (:form . "\\newline") (:value . ?\n))
                                            ((:node-type . :character) (:position . 11) (:form . "\\return") (:value . ?\r))
                                            ((:node-type . :character) (:position . 19) (:form . "\\space") (:value . 32))
                                            ((:node-type . :character) (:position . 26) (:form . "\\tab") (:value . ?\t))
                                            ((:node-type . :character) (:position . 31) (:form . "\\a") (:value . ?a))
                                            ((:node-type . :character) (:position . 34) (:form . "\\b") (:value . ?b))
                                            ((:node-type . :character) (:position . 37) (:form . "\\c") (:value . ?c))
                                            ((:node-type . :character) (:position . 40) (:form . "\\u0078") (:value . ?x))
                                            ((:node-type . :character) (:position . 47) (:form . "\\o171") (:value . ?y)))))))))

       "chars-2"
       (a-list
        :source "\"\\u0078 \\o171\""
        :edn '("x y")
        :ast '((:node-type . :root)
               (:position . 1)
               (:children . (((:node-type . :string)
                              (:position . 1)
                              (:form . "\"\\u0078 \\o171\"")
                              (:value . "x y"))))))

       "keywords"
       (a-list
        :tags '(:edn-roundtrip)
        :source ":foo-bar"
        :edn '(:foo-bar)
        :ast '((:node-type . :root)
               (:position . 1)
               (:children . (((:node-type . :keyword)
                              (:position . 1)
                              (:form . ":foo-bar")
                              (:value . :foo-bar))))))

       "vector"
       (a-list
        :tags '(:edn-roundtrip)
        :source "[123]"
        :edn '([123])
        :ast '((:node-type . :root)
               (:position . 1)
               (:children . (((:node-type . :vector)
                              (:position . 1)
                              (:children . (((:node-type . :number)
                                             (:position . 2)
                                             (:form . "123")
                                             (:value . 123)))))))))

       "map"
       (a-list
        :tags '(:edn-roundtrip)
        :source "{:count 123}"
        :edn (list (a-hash-table :count 123))
        :ast '((:node-type . :root)
               (:position . 1)
               (:children . (((:node-type . :map)
                              (:position . 1)
                              (:children . (((:node-type . :keyword)
                                             (:position . 2)
                                             (:form . ":count")
                                             (:value . :count))
                                            ((:node-type . :number)
                                             (:position . 9)
                                             (:form . "123")
                                             (:value . 123)))))))))

       "set"
       (a-list
        :tags '(:edn-roundtrip)
        :source "#{:x}"
        :edn '((edn-set (:x)))
        :ast '((:node-type . :root)
               (:position . 1)
               (:children . (((:node-type . :set)
                              (:position . 1)
                              (:children . (((:node-type . :keyword)
                                             (:position . 3)
                                             (:form . ":x")
                                             (:value . :x)))))))))

       "discard"
       (a-list
        :source "(10 #_11 12 #_#_ 13 14)"
        :edn '((10 12))
        :ast '((:node-type . :root)
               (:position . 1)
               (:children . (((:node-type . :list)
                              (:position . 1)
                              (:children . (((:node-type . :number)
                                             (:position . 2)
                                             (:form . "10")
                                             (:value . 10))
                                            ((:node-type . :number)
                                             (:position . 10)
                                             (:form . "12")
                                             (:value . 12)))))))))


       "tag-1"
       (a-list
        :source "#foo/bar [1]"
        :ast '((:node-type . :root)
               (:position . 1)
               (:children . (((:node-type . :tag)
                              (:position . 1)
                              (:tag . foo/bar)
                              (:children . (((:node-type . :vector)
                                             (:position . 10)
                                             (:children . (((:node-type . :number)
                                                            (:position . 11)
                                                            (:form . "1")
                                                            (:value . 1))))))))))))

       "tag-2"
       (a-list
        :source "(fn #param :param-name 1)"
        :ast '((:node-type . :root)
               (:position . 1)
               (:children . (((:node-type . :list)
                              (:position . 1)
                              (:children . (((:node-type . :symbol)
                                             (:position . 2)
                                             (:form . "fn")
                                             (:value . fn))
                                            ((:node-type . :tag)
                                             (:position . 5)
                                             (:tag . param)
                                             (:children . (((:node-type . :keyword)
                                                            (:position . 12)
                                                            (:form . ":param-name")
                                                            (:value . :param-name)))))
                                            ((:node-type . :number)
                                             (:position . 24)
                                             (:form . "1")
                                             (:value . 1)))))))))

       "nested-tags"
       (a-list
        :source "[#lazy-error #error {:cause \"Divide by zero\"}]"
        :ast '((:node-type . :root)
               (:position . 1)
               (:children ((:node-type . :vector)
                           (:position . 1)
                           (:children ((:node-type . :tag)
                                       (:position . 2)
                                       (:tag . lazy-error)
                                       (:children ((:node-type . :tag)
                                                   (:position . 14)
                                                   (:tag . error)
                                                   (:children ((:node-type . :map)
                                                               (:position . 21)
                                                               (:children ((:node-type . :keyword)
                                                                           (:position . 22)
                                                                           (:form . ":cause")
                                                                           (:value . :cause))
                                                                          ((:node-type . :string)
                                                                           (:position . 29)
                                                                           (:form . "\"Divide by zero\"")
                                                                           (:value . "Divide by zero")))))))))))))

       "booleans"
       (a-list
        :source "[nil true false]"
        :edn '([nil t nil]))))

;;; parseedn-test-data.el ends here
