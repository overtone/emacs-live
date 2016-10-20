;;; clojure-mode-convert-collection-test.el --- Clojure Mode: convert collection type  -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Benedek Fazekas <benedek.fazekas@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The convert collection code originally was implemented
;; as cycling collection type in clj-refactor.el and is the work
;; of the clj-reafctor.el team.

;;; Code:

(require 'clojure-mode)
(require 'ert)

(def-refactor-test test-convert-collection-list-map
  "(:a 1 :b 2)"
  "{:a 1 :b 2}"
  (backward-sexp)
  (down-list)
  (clojure-convert-collection-to-map))

(def-refactor-test test-convert-collection-map-vector
  "{:a 1 :b 2}"
  "[:a 1 :b 2]"
  (backward-sexp)
  (down-list)
  (clojure-convert-collection-to-vector))

(def-refactor-test test-convert-collection-vector-set
  "[1 2 3]"
  "#{1 2 3}"
  (backward-sexp)
  (down-list)
  (clojure-convert-collection-to-set))

(def-refactor-test test-convert-collection-set-list
  "#{1 2 3}"
  "(1 2 3)"
  (backward-sexp)
  (down-list)
  (clojure-convert-collection-to-list))

(def-refactor-test test-convert-collection-set-quoted-list
  "#{1 2 3}"
  "'(1 2 3)"
  (backward-sexp)
  (down-list)
  (clojure-convert-collection-to-quoted-list))

(def-refactor-test test-convert-collection-quoted-list-set
  "'(1 2 3)"
  "#{1 2 3}"
  (backward-sexp)
  (down-list)
  (clojure-convert-collection-to-set))

(provide 'clojure-mode-convert-collection-test)

;;; clojure-mode-convert-collection-test.el ends here
