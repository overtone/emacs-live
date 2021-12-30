;;; edn-el-parity.el --- Tests from edn.el

;; Author: Lars Andersen <expez@expez.com>, Arne Brasseur <arne@arnebrasseur.net>

;; Copyright (C) 2015  Lars Andersen

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

;; These tests are copied verbatim from the edn.el source, and adapted to use
;; our API. This way we assure that parseclj can act as a drop-in replacement
;; for edn.el.

;;; Code:

(require 'ert)
(require 'parseedn)
(eval-when-compile (require 'subr-x)) ;; for things like hash-table-keys

(ert-deftest whitespace ()
  (should (null (parseedn-read-str "")))
  (should (null (parseedn-read-str " ")))
  (should (null (parseedn-read-str "   ")))
  (should (null (parseedn-read-str "	")))
  (should (null (parseedn-read-str "		")))
  (should (null (parseedn-read-str ",")))
  (should (null (parseedn-read-str ",,,,")))
  (should (null (parseedn-read-str "	  , ,\n")))
  (should (null (parseedn-read-str "\n ,, 	")))
  (should (equal [a b c d] (parseedn-read-str "[a ,,,,,, b,,,,,c ,d]"))))

(ert-deftest symbols ()
  :tags '(edn symbol)
  (should (equal 'foo (parseedn-read-str "foo")))
  (should (equal 'foo\. (parseedn-read-str "foo.")))
  (should (equal '%foo\. (parseedn-read-str "%foo.")))
  (should (equal 'foo/bar (parseedn-read-str "foo/bar")))
  (equal 'some\#sort\#of\#symbol (parseedn-read-str "some#sort#of#symbol"))
  (equal 'truefalse (parseedn-read-str "truefalse"))
  (equal 'true. (parseedn-read-str "true."))
  (equal '/ (parseedn-read-str "/"))
  (should (equal '.true (parseedn-read-str ".true")))
  (should (equal 'some:sort:of:symbol (parseedn-read-str "some:sort:of:symbol")))
  (equal 'foo-bar (parseedn-read-str "foo-bar"))
  (should (equal '+some-symbol (parseedn-read-str "+some-symbol")))
  (should (equal '-symbol (parseedn-read-str "-symbol"))))

(ert-deftest booleans ()
  :tags '(edn boolean)
  (should (equal t (parseedn-read-str "true")))
  (should (equal nil (parseedn-read-str "false "))))

(ert-deftest characters ()
  :tags '(edn characters)
  (should (equal 97 (parseedn-read-str "\\a")))
  (should (equal 960 (parseedn-read-str "\\u03C0")))
  ;;(should (equal 'newline (parseedn-read-str "\\newline")))
  )

(ert-deftest elision ()
  :tags '(edn elision)
  (should-not (parseedn-read-str "#_foo"))
  (should-not (parseedn-read-str "#_ 123"))
  (should-not (parseedn-read-str "#_:foo"))
  (should-not (parseedn-read-str "#_ \\a"))
  (should-not (parseedn-read-str "#_
\"foo\""))
  (should-not (parseedn-read-str "#_ (1 2 3)"))
  (should (equal '(1 3) (parseedn-read-str "(1 #_ 2 3)")))
  (should (equal '[1 2 3 4] (parseedn-read-str "[1 2 #_[4 5 6] 3 4]")))
  (should (map-equal (make-seeded-hash-table :foo :bar)
                     (parseedn-read-str "{:foo #_elided :bar}")))
  (should (equal '(edn-set (1 2 3 4))
                 (parseedn-read-str "#{1 2 #_[1 2 3] 3 #_ (1 2) 4}")))
  (should (equal [a d] (parseedn-read-str "[a #_ ;we are discarding what comes next
 c d]"))))

(ert-deftest string ()
  :tags '(edn string)
  (should (equal "this is a string" (parseedn-read-str "\"this is a string\"")))
  (should (equal "this has an escaped \"quote in it"
                 (parseedn-read-str "\"this has an escaped \\\"quote in it\"")))
  (should (equal "foo\tbar" (parseedn-read-str "\"foo\\tbar\"")))
  (should (equal "foo\nbar" (parseedn-read-str "\"foo\\nbar\"")))
  (should (equal "this is a string \\ that has an escaped backslash"
                 (parseedn-read-str "\"this is a string \\\\ that has an escaped backslash\"")))
  (should (equal "[" (parseedn-read-str "\"[\""))))

(ert-deftest keywords ()
  :tags '(edn keywords)
  (should (equal :namespace\.of\.some\.length/keyword-name
                 (parseedn-read-str ":namespace.of.some.length/keyword-name")))
  (should (equal :\#/\# (parseedn-read-str ":#/#")))
  (should (equal :\#/:a (parseedn-read-str ":#/:a")))
  (should (equal :\#foo (parseedn-read-str ":#foo"))))

(ert-deftest integers ()
  :tags '(edn integers)
  (should (= 0 (parseedn-read-str "0")))
  (should (= 0 (parseedn-read-str "+0")))
  (should (= 0 (parseedn-read-str "-0")))
  (should (= 100 (parseedn-read-str "100")))
  (should (= -100 (parseedn-read-str "-100"))))

(ert-deftest floats ()
  :tags '(edn floats)
  (should (= 12.32 (parseedn-read-str "12.32")))
  (should (= -12.32 (parseedn-read-str "-12.32")))
  (should (= 9923.23 (parseedn-read-str "+9923.23")))
  (should (= 4.5e+044 (parseedn-read-str "45e+43")))
  (should (= -4.5e-042 (parseedn-read-str "-45e-43")))
  (should (= 4.5e+044 (parseedn-read-str "45E+43"))))

(ert-deftest lists ()
  :tags '(edn lists)
  (should-not (parseedn-read-str "()"))
  (should (equal '(1 2 3) (parseedn-read-str "( 1 2 3)")))
  (should (equal '(12.1 ?a foo :bar) (parseedn-read-str "(12.1 \\a foo :bar)")))
  (should (equal '((:foo bar :bar 12)) (parseedn-read-str "( (:foo bar :bar 12))")))
  (should (equal
           '(defproject com\.thortech/data\.edn "0.1.0-SNAPSHOT")
           (parseedn-read-str "(defproject com.thortech/data.edn \"0.1.0-SNAPSHOT\")"))))

(ert-deftest vectors ()
  :tags '(edn vectors)
  (should (equal [] (parseedn-read-str "[]")))
  (should (equal [] (parseedn-read-str "[ ]")))
  (should (equal '[1 2 3] (parseedn-read-str "[ 1 2 3 ]")))
  (should (equal '[12.1 ?a foo :bar] (parseedn-read-str "[ 12.1 \\a foo :bar]")))
  (should (equal '[[:foo bar :bar 12]] (parseedn-read-str "[[:foo bar :bar 12]]")))
  (should (equal '[( :foo bar :bar 12 ) "foo"]
                 (parseedn-read-str "[(:foo bar :bar 12) \"foo\"]")))
  (should (equal '[/ \. * ! _ \? $ % & = - +]
                 (parseedn-read-str "[/ . * ! _ ? $ % & = - +]")))
  (should (equal
           ;;[99 newline return space tab]
           [99 10 13 32 9]
           (parseedn-read-str "[\\c \\newline \\return \\space \\tab]"))))

(defun map-equal (m1 m2)
  (and (and (hash-table-p m1) (hash-table-p m2))
       (eq (hash-table-test m1) (hash-table-test m2))
       (= (hash-table-count m1) (hash-table-count m2))
       (equal (hash-table-keys m1) (hash-table-keys m2))
       (equal (hash-table-values m1) (hash-table-values m2))))

(defun make-seeded-hash-table (&rest keys-and-values)
  (let ((m (make-hash-table :test #'equal)))
    (while keys-and-values
      (puthash (pop keys-and-values) (pop keys-and-values) m))
    m))

(ert-deftest maps ()
  :tags '(edn maps)
  (should (hash-table-p (parseedn-read-str "{ }")))
  (should (hash-table-p (parseedn-read-str "{}")))
  (should (map-equal (make-seeded-hash-table :foo :bar :baz :qux)
                     (parseedn-read-str "{ :foo :bar :baz :qux}")))
  (should (map-equal (make-seeded-hash-table 1 "123" 'vector [1 2 3])
                     (parseedn-read-str "{ 1 \"123\" vector [1 2 3]}")))
  (should (map-equal (make-seeded-hash-table [1 2 3] "some numbers")
                     (parseedn-read-str "{[1 2 3] \"some numbers\"}"))))

(ert-deftest sets ()
  :tags '(edn sets)
  (should (eq 'edn-set (car (parseedn-read-str "#{}"))))
  (should (eq 'edn-set (car (parseedn-read-str "#{ }"))))
  (should (equal '(edn-set (1 2 3)) (parseedn-read-str "#{1 2 3}")))
  (should (equal '(edn-set (1 [1 2 3] 3)) (parseedn-read-str "#{1 [1 2 3] 3}"))))

(ert-deftest comment ()
  :tags '(edn comments)
  (should-not (parseedn-read-str ";nada"))
  (should (equal 1 (parseedn-read-str ";; comment
1")))
  (should (equal [1 2 3] (parseedn-read-str "[1 2 ;comment to eol
3]")))
  (should (equal '[valid more items] (parseedn-read-str "[valid;touching trailing comment
 more items]")))
  (should (equal [valid vector more vector items] (parseedn-read-str "[valid vector
 ;;comment in vector
 more vector items]"))))

(defun test-val-passed-to-handler (val)
  (should (listp val))
  (should (= (length val) 2))
  (should (= 1 (car val)))
  1)

(setq parseedn-test-extra-handlers
      (list (cons 'my/type #'test-val-passed-to-handler)
            (cons 'my/other-type (lambda (val) 2))))

(ert-deftest tags ()
  :tags '(edn tags)
  (should-error (parseedn-read-str "#my/type value" parseedn-test-extra-handlers))
  (should (= 1 (parseedn-read-str "#my/type (1 2)" parseedn-test-extra-handlers)))
  (should (= 2 (parseedn-read-str "#my/other-type {:foo :bar}" parseedn-test-extra-handlers)))
  (should-error (parseedn-read-str "#myapp/Person {:first \"Fred\" :last \"Mertz\"}")))

(ert-deftest roundtrip ()
  :tags '(edn roundtrip)
  (let ((data [1 2 3 :foo (4 5) qux "quux"]))
    (should (equal data (parseedn-read-str (parseedn-print-str data))))
    (should (map-equal (make-seeded-hash-table :foo :bar)
                       (parseedn-read-str (parseedn-print-str (make-seeded-hash-table :foo :bar)))))
    (should (equal '(edn-set (1 2 3 [3 1.11]))
                   (parseedn-read-str (parseedn-print-str '(edn-set (1 2 3 [3 1.11]))))))))

(ert-deftest inst ()
  :tags '(edn inst)
  (let* ((inst-str "#inst \"1985-04-12T23:20:50.52Z\"")
         (inst (parseedn-read-str inst-str))
         (time (date-to-time "1985-04-12T23:20:50.52Z")))
    (should (eq 'edn-inst (car inst)))
    (should (equal time (cdr inst)))))

(ert-deftest uuid ()
  :tags '(edn uuid)
  (let* ((str "f81d4fae-7dec-11d0-a765-00a0c91e6bf6")
         (uuid (parseedn-read-str (concat "#uuid \"" str "\""))))
    (should (eq 'edn-uuid (car uuid)))))

;; (ert-deftest invalid-edn ()
;;   (should-error (parseedn-read-str "///"))
;;   (should-error (parseedn-read-str "~cat"))
;;   (should-error (parseedn-read-str "foo/bar/baz/qux/quux"))
;;   (should-error (parseedn-read-str "#foo/"))
;;   (should-error (parseedn-read-str "foo/"))
;;   (should-error (parseedn-read-str ":foo/"))
;;   (should-error (parseedn-read-str "#/foo"))
;;   (should-error (parseedn-read-str "/symbol"))
;;   (should-error (parseedn-read-str ":/foo"))
;;   (should-error (parseedn-read-str "+5symbol"))
;;   (should-error (parseedn-read-str ".\\newline"))
;;   (should-error (parseedn-read-str "0cat"))
;;   (should-error (parseedn-read-str "-4cats"))
;;   (should-error (parseedn-read-str ".9"))
;;   (should-error (parseedn-read-str ":keyword/with/too/many/slashes"))
;;   (should-error (parseedn-read-str ":a.b.c/"))
;;   (should-error (parseedn-read-str "\\itstoolong"))
;;   (should-error (parseedn-read-str ":#/:"))
;;   (should-error (parseedn-read-str "/foo//"))
;;   (should-error (parseedn-read-str "///foo"))
;;   (should-error (parseedn-read-str ":{}"))
;;   (should-error (parseedn-read-str "//"))
;;   (should-error (parseedn-read-str "##"))
;;   (should-error (parseedn-read-str "::"))
;;   (should-error (parseedn-read-str "::a"))
;;   (should-error (parseedn-read-str ".5symbol"))
;;   (should-error (parseedn-read-str "{ \"foo\""))
;;   (should-error (parseedn-read-str "{ \"foo\" :bar"))
;;   (should-error (parseedn-read-str "{"))
;;   (should-error (parseedn-read-str ":{"))
;;   (should-error (parseedn-read-str "{{"))
;;   (should-error (parseedn-read-str "}"))
;;   (should-error (parseedn-read-str ":}"))
;;   (should-error (parseedn-read-str "}}"))
;;   (should-error (parseedn-read-str "#:foo"))
;;   (should-error (parseedn-read-str "\\newline."))
;;   (should-error (parseedn-read-str "\\newline0.1"))
;;   (should-error (parseedn-read-str "^"))
;;   (should-error (parseedn-read-str ":^"))
;;   (should-error (parseedn-read-str "_:^"))
;;   (should-error (parseedn-read-str "#{{[}}"))
;;   (should-error (parseedn-read-str "[}"))
;;   (should-error (parseedn-read-str "@cat")))

;;; edn-el-parity-test.el ends here
