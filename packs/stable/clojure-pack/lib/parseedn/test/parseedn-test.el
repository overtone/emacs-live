;;; parseedn-test.el --- Unit tests for EDN reading/printing

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

;; Unit tests for EDN reading/printing

;;; Code

(require 'ert)
(require 'parseedn)

(load "test/parseedn-test-data.el")

(ert-deftest parseedn-print-test ()
  (should (equal (parseedn-print-str nil) "nil"))
  (should (equal (parseedn-print-str 100) "100"))
  (should (equal (parseedn-print-str 1.2) "1.2"))
  (should (equal (parseedn-print-str [1 2 3]) "[1 2 3]"))
  (should (equal (parseedn-print-str t) "true"))
  (should (equal (parseedn-print-str '((a . 1) (b . 2))) "{a 1, b 2}"))
  (should (equal (parseedn-print-str '((a . 1) (b . ((c . 3))))) "{a 1, b {c 3}}"))
  (should (equal (parseedn-print-str '(:a 1 :b 2)) "{:a 1, :b 2}"))
  (should (equal (parseedn-print-str '(:a 1 :b (:c 3))) "{:a 1, :b {:c 3}}"))
  (should (listp (member (parseedn-print-str
                          (let ((ht (make-hash-table)))
                            (puthash :a 1 ht)
                            (puthash :b 2 ht)
                            (puthash :c 3 ht)
                            ht))
                         '("{:a 1, :b 2, :c 3}"
                           "{:a 1, :c 3, :b 2}"
                           "{:b 2, :a 1, :c 3}"
                           "{:b 2, :c 3, :a 1}"
                           "{:c 3, :a 1, :b 2}"
                           "{:c 3, :b 2, :a 1}")))))

(ert-deftest parseedn-read-test ()
  (should (equal (parseedn-read-str "true") t)))

(defmacro define-parseedn-read-tests ()
  `(progn
     ,@(mapcar
        (lambda (pair)
          (let ((name (car pair))
                (data (cdr pair)))
            (if (and (a-get data :edn) (a-get data :source))
                (let ((test-name (intern (concat "parseedn-read:" name))))
                  `(ert-deftest ,test-name ()
                     :tags '(parseedn)
                     (with-temp-buffer
                       (insert ,(a-get data :source))
                       (goto-char 1)
                       (should (a-equal (parseedn-read) ',(a-get data :edn)))))))))
        parseedn-test-data)))

(defmacro define-parseedn-roundtrip-tests ()
  `(progn
     ,@(mapcar
        (lambda (pair)
          (let ((name (car pair))
                (data (cdr pair)))
            (if (and (a-get data :edn) (a-get data :source) (member :edn-roundtrip (a-get data :tags)))
                (let ((test-name (intern (concat "parseedn-rountrip:" name))))
                  `(ert-deftest ,test-name ()
                     :tags '(parseedn-rountrip)
                     (should (equal (parseedn-print-str (car ',(a-get data :edn))) ,(a-get data :source))))))))
        parseedn-test-data)))

(define-parseedn-read-tests)
(define-parseedn-roundtrip-tests)

;;; parseedn-test.el
