;;; clojure-mode-cycling-test.el --- Clojure Mode: cycling things tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2021 Benedek Fazekas <benedek.fazekas@gmail.com>

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
(require 'buttercup)

(describe "clojure-cycle-privacy"

  (when-refactoring-it "should turn a public defn into a private defn"
    "(defn add [a b]
  (+ a b))"

    "(defn- add [a b]
  (+ a b))"

    (clojure-cycle-privacy))

  (when-refactoring-it "should also work from the beginning of a sexp"
     "(defn- add [a b]
  (+ a b))"

     "(defn add [a b]
  (+ a b))"

     (backward-sexp)
     (clojure-cycle-privacy))

  (when-refactoring-it "should use metadata when clojure-use-metadata-for-privacy is set to true"
    "(defn add [a b]
  (+ a b))"

    "(defn ^:private add [a b]
  (+ a b))"

    (let ((clojure-use-metadata-for-privacy t))
      (clojure-cycle-privacy)))

  (when-refactoring-it "should turn a private defn into a public defn"
    "(defn- add [a b]
  (+ a b))"

    "(defn add [a b]
  (+ a b))"

    (clojure-cycle-privacy))

  (when-refactoring-it "should turn a private defn with metadata into a public defn"
    "(defn ^:private add [a b]
  (+ a b))"

    "(defn add [a b]
  (+ a b))"

    (let ((clojure-use-metadata-for-privacy t))
      (clojure-cycle-privacy)))

  (when-refactoring-it "should also work with pre-existing metadata"
    "(def ^:dynamic config
  \"docs\"
  {:env \"staging\"})"

    "(def ^:private ^:dynamic config
  \"docs\"
  {:env \"staging\"})"

    (clojure-cycle-privacy))

  (when-refactoring-it "should turn a private def with metadata into a public def"
    "(def ^:private config
  \"docs\"
  {:env \"staging\"})"

    "(def config
  \"docs\"
  {:env \"staging\"})"

    (clojure-cycle-privacy)))

(describe "clojure-cycle-if"

  (when-refactoring-it "should cycle inner if"
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

  (when-refactoring-it "should cycle outer if"
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
    (clojure-cycle-if)))

(describe "clojure-cycle-when"

  (when-refactoring-it "should cycle inner when"
    "(when this
  (when that
    (aaa)
    (bbb))
  (ccc))"

    "(when this
  (when-not that
    (aaa)
    (bbb))
  (ccc))"

    (beginning-of-buffer)
    (search-forward "bbb)")
    (clojure-cycle-when))

  (when-refactoring-it "should cycle outer when"
    "(when-not this
  (when that
    (aaa)
    (bbb))
  (ccc))"

    "(when this
  (when that
    (aaa)
    (bbb))
  (ccc))"

    (beginning-of-buffer)
    (search-forward "bbb))")
    (clojure-cycle-when)))

(describe "clojure-cycle-not"

  (when-refactoring-it "should add a not when missing"
    "(ala bala portokala)"
    "(not (ala bala portokala))"

    (beginning-of-buffer)
    (search-forward "bala")
    (clojure-cycle-not))

  (when-refactoring-it "should remove a not when present"
    "(not (ala bala portokala))"
    "(ala bala portokala)"

    (beginning-of-buffer)
    (search-forward "bala")
    (clojure-cycle-not)))

(provide 'clojure-mode-cycling-test)

;;; clojure-mode-cycling-test.el ends here
