;;; parseclj-ast-test.el --- Unit tests for AST parsing/unparsing

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

;;; Commentary

;; Unit tests for AST parsing/unparsing

;;; Code

(require 'ert)
(require 'parseclj-ast)

(load "test/parseclj-test-data.el")

(defmacro define-parseclj-parse-clojure-tests ()
  `(progn
     ,@(mapcar
        (lambda (pair)
          (let ((name (car pair))
                (data (cdr pair)))
            (if (and (map-elt data :source) (map-elt data :ast))
                (let ((test-name (intern (concat "parseclj-parse-clojure:" name))))
                  `(ert-deftest ,test-name ()
                     :tags '(parseclj-ast)
                     (with-temp-buffer
                       (insert ,(map-elt data :source))
                       (goto-char 1)
                       (should (equal (parseclj-parse-clojure) ',(map-elt data :ast)))))))))
        parseclj-test-data)))

(defmacro define-parseclj-ast-roundtrip-tests ()
  `(progn
     ,@(mapcar
        (lambda (pair)
          (let ((name (car pair))
                (data (cdr pair)))
            (if (and (map-elt data :ast) (map-elt data :source))
                (let ((test-name (intern (concat "parseclj-ast-rountrip:" name))))
                  `(ert-deftest ,test-name ()
                     :tags '(parseclj-ast-rountrip)
                     (should (equal (parseclj-parse-clojure (parseclj-unparse-clojure-to-string
                                                             ',(map-elt data :ast)))
                                    ',(or (map-elt data :roundtrip-ast)
                                          (map-elt data :ast)))))))))
        parseclj-test-data)))

(define-parseclj-ast-roundtrip-tests)
(define-parseclj-parse-clojure-tests)

;;; parseclj-ast-test.el ends here
