;;; clojure-mode-cycling-test.el --- Clojure Mode: cycling things tests  -*- lexical-binding: t; -*-

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

;; The cycling privacy and if/if-not code is ported from
;; clj-refactor.el and the work of the clj-reafctor.el team.

;;; Code:

(require 'clojure-mode)
(require 'ert)

(def-refactor-test test-cycle-privacy-public-defn-private-defn
  "(defn add [a b]
  (+ a b))"
  "(defn- add [a b]
  (+ a b))"
  (clojure-cycle-privacy))

(def-refactor-test test-cycle-privacy-from-sexp-beg
  "(defn- add [a b]
  (+ a b))"
  "(defn add [a b]
  (+ a b))"
  (backward-sexp)
  (clojure-cycle-privacy))

(def-refactor-test test-cycle-privacy-public-defn-private-defn-metadata
  "(defn add [a b]
  (+ a b))"
  "(defn ^:private add [a b]
  (+ a b))"
  (let ((clojure-use-metadata-for-privacy t))
      (clojure-cycle-privacy)))

(def-refactor-test test-cycle-privacy-private-defn-public-defn
  "(defn- add [a b]
  (+ a b))"
  "(defn add [a b]
  (+ a b))"
  (clojure-cycle-privacy))

(def-refactor-test test-cycle-privacy-private-defn-public-defn-metadata
  "(defn ^:private add [a b]
  (+ a b))"
  "(defn add [a b]
  (+ a b))"
  (let ((clojure-use-metadata-for-privacy t))
    (clojure-cycle-privacy)))

(def-refactor-test test-cycle-privacy-public-def-private-def
  "(def ^:dynamic config
  \"docs\"
  {:env \"staging\"})"
  "(def ^:private ^:dynamic config
  \"docs\"
  {:env \"staging\"})"
  (clojure-cycle-privacy))

(def-refactor-test test-cycle-privacy-private-def-public-def
  "(def ^:private config
  \"docs\"
  {:env \"staging\"})"
  "(def config
  \"docs\"
  {:env \"staging\"})"
  (clojure-cycle-privacy))

(def-refactor-test test-cycle-if-inner-if
  "(if this
  (if that
    (then AAA)
    (else BBB))
  (otherwise CCC))"
  "(if this
  (if-not that
    (else BBB)
    (then AAA))
  (otherwise CCC))"
  (beginning-of-buffer)
  (search-forward "BBB)")
  (clojure-cycle-if))

(def-refactor-test test-cycle-if-outer-if
  "(if-not this
  (if that
    (then AAA)
    (else BBB))
  (otherwise CCC))"
  "(if this
  (otherwise CCC)
  (if that
    (then AAA)
    (else BBB)))"
  (beginning-of-buffer)
  (search-forward "BBB))")
  (clojure-cycle-if))

(provide 'clojure-mode-cycling-test)

;;; clojure-mode-cycling-test.el ends here
