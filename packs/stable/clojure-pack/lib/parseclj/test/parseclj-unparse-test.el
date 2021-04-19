;;; parseclj-unparse-test.el --- Print Clojure AST back to code - tests

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

;; Print Clojure AST back to code - tests

;;; Code:

(require 'ert)
(require 'parseclj-ast)

;;; Printer modes
;; ----------------------------------------------------------------------------

(ert-deftest parseclj-unparse-clojure-list ()
  (should (equal "(0 1 2)"
                 (parseclj-unparse-clojure-to-string
                  '((:node-type . :root)
                    (:position . 1)
                    (:children . (((:node-type . :list)
                                   (:position . 1)
                                   (:children . (((:node-type . :number)
                                                  (:position . 2)
                                                  (:form . "0")
                                                  (:value . 0))
                                                 ((:node-type . :number)
                                                  (:position . 4)
                                                  (:form . "1")
                                                  (:value . 1))
                                                 ((:node-type . :number)
                                                  (:position . 6)
                                                  (:form . "2")
                                                  (:value . 2))))))))))))

(ert-deftest parseclj-unparse-clojure-empty-list ()
  (should (equal "()"
                 (parseclj-unparse-clojure-to-string
                  '((:node-type . :root)
                    (:position . 1)
                    (:children . (((:node-type . :list)
                                   (:position . 1)
                                   (:children . nil)))))))))

(ert-deftest parseclj-unparse-clojure-nested-list ()
  (should (equal "((.9 abc (true) (hello)))"
                 (parseclj-unparse-clojure-to-string
                  '((:node-type . :root)
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
                                                                          (:value . hello))))))))))))))))

(ert-deftest parseclj-unparse-clojure-to-stringing ()
  (should (equal "\"abc hello \\t\\\"x\""
                 (parseclj-unparse-clojure-to-string
                  '((:node-type . :root)
                    (:position . 1)
                    (:children . (((:node-type . :string)
                                   (:position . 1)
                                   (:form . "\"abc hello \\t\\\"x\"")
                                   (:value . "abc hello \t\"x")))))))))

(ert-deftest parseclj-unparse-clojure-chars ()
  (should (equal "(\\newline \\return \\space \\tab \\a \\b \\c \\u0078 \\o171)"
                 (parseclj-unparse-clojure-to-string
                  '((:node-type . :root)
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
                 )))

(ert-deftest parseclj-unparse-clojure-keyword ()
  (should (equal ":foo-bar"
                 (parseclj-unparse-clojure-to-string
                  '((:node-type . :root)
                    (:position . 1)
                    (:children . (((:node-type . :keyword)
                                   (:position . 1)
                                   (:form . ":foo-bar")
                                   (:value . :foo-bar)))))))))

(ert-deftest parseclj-unparse-clojure-vector ()
  (should (equal "[123]"
                 (parseclj-unparse-clojure-to-string
                  '((:node-type . :root)
                    (:position . 1)
                    (:children . (((:node-type . :vector)
                                   (:position . 1)
                                   (:children . (((:node-type . :number)
                                                  (:position . 2)
                                                  (:form . "123")
                                                  (:value . 123))))))))))))

(ert-deftest parseclj-unparse-clojure-map ()
  (should (equal "{:count 123}"
                 (parseclj-unparse-clojure-to-string
                  '((:node-type . :root)
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
                                                  (:value . 123))))))))))))

(ert-deftest parseclj-unparse-clojure-set ()
  (should (equal "#{:x}"
                 (parseclj-unparse-clojure-to-string
                  '((:node-type . :root)
                    (:position . 1)
                    (:children . (((:node-type . :set)
                                   (:position . 1)
                                   (:children . (((:node-type . :keyword)
                                                  (:position . 3)
                                                  (:form . ":x")
                                                  (:value . :x))))))))))))

(ert-deftest parseclj-unparse-clojure-lexical ()
  (let ((src ";; hello world
              (+ 1  2
                      ;;good stuff
          3)"))
    (should (equal src (thread-first src
                         (parseclj-parse-clojure :lexical-preservation t)
                         (parseclj-unparse-clojure-to-string))))))

(provide 'parseclj-unparse-test)

;;; parseclj-unparse-test.el ends here
