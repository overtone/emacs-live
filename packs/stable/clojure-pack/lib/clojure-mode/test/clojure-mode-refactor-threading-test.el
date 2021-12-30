;;; clojure-mode-refactor-threading-test.el --- Clojure Mode: refactor threading tests  -*- lexical-binding: t; -*-

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

;; The threading refactoring code is ported from clj-refactor.el
;; and mainly the work of Magnar Sveen, Alex Baranosky and
;; the rest of the clj-reafctor.el team.

;;; Code:

(require 'clojure-mode)
(require 'buttercup)

(describe "clojure-thread"

  (when-refactoring-it "should work with -> when performed once"
    "(-> (dissoc (assoc {} :key \"value\") :lock))"

    "(-> (assoc {} :key \"value\")
    (dissoc :lock))"

    (clojure-thread))

  (when-refactoring-it "should work with -> when performed twice"
    "(-> (dissoc (assoc {} :key \"value\") :lock))"

    "(-> {}
    (assoc :key \"value\")
    (dissoc :lock))"

    (clojure-thread)
    (clojure-thread))

  (when-refactoring-it "should not thread maps"
    "(-> (dissoc (assoc {} :key \"value\") :lock))"

    "(-> {}
    (assoc :key \"value\")
    (dissoc :lock))"

    (clojure-thread)
    (clojure-thread)
    (clojure-thread))

  (when-refactoring-it "should not thread last sexp"
    "(-> (dissoc (assoc (get-a-map) :key \"value\") :lock))"

    "(-> (get-a-map)
    (assoc :key \"value\")
    (dissoc :lock))"

    (clojure-thread)
    (clojure-thread)
    (clojure-thread))

  (when-refactoring-it "should thread-first-easy-on-whitespace"
    "(->
 (dissoc (assoc {} :key \"value\") :lock))"

    "(->
 (assoc {} :key \"value\")
 (dissoc :lock))"

    (clojure-thread))

  (when-refactoring-it "should remove superfluous parens"
    "(-> (square (sum [1 2 3 4 5])))"

    "(-> [1 2 3 4 5]
    sum
    square)"

    (clojure-thread)
    (clojure-thread))

  (when-refactoring-it "should work with cursor before ->"
    "(-> (not (s-acc/mobile? session)))"

    "(-> (s-acc/mobile? session)
    not)"

    (beginning-of-buffer)
    (clojure-thread))

  (when-refactoring-it "should work with one step with ->>"
    "(->> (map square (filter even? [1 2 3 4 5])))"

    "(->> (filter even? [1 2 3 4 5])
     (map square))"

    (clojure-thread))

  (when-refactoring-it "should work with two steps with ->>"
    "(->> (map square (filter even? [1 2 3 4 5])))"

    "(->> [1 2 3 4 5]
     (filter even?)
     (map square))"

    (clojure-thread)
    (clojure-thread))

  (when-refactoring-it "should not thread vectors with ->>"
    "(->> (map square (filter even? [1 2 3 4 5])))"

    "(->> [1 2 3 4 5]
     (filter even?)
     (map square))"

    (clojure-thread)
    (clojure-thread)
    (clojure-thread))

  (when-refactoring-it "should not thread last sexp with ->>"
    "(->> (map square (filter even? (get-a-list))))"

    "(->> (get-a-list)
     (filter even?)
     (map square))"

    (clojure-thread)
    (clojure-thread)
    (clojure-thread))

  (when-refactoring-it "should work with some->"
    "(some-> (+ (val (find {:a 1} :b)) 5))"

    "(some-> {:a 1}
        (find :b)
        val
        (+ 5))"

    (clojure-thread)
    (clojure-thread)
    (clojure-thread))

  (when-refactoring-it "should work with some->>"
    "(some->> (+ 5 (val (find {:a 1} :b))))"

    "(some->> :b
         (find {:a 1})
         val
         (+ 5))"

    (clojure-thread)
    (clojure-thread)
    (clojure-thread)))

(describe "clojure-unwind"

  (when-refactoring-it "should unwind -> one step"
    "(-> {}
    (assoc :key \"value\")
    (dissoc :lock))"

    "(-> (assoc {} :key \"value\")
    (dissoc :lock))"

    (clojure-unwind))

  (when-refactoring-it "should unwind -> two steps"
    "(-> {}
    (assoc :key \"value\")
    (dissoc :lock))"

    "(-> (dissoc (assoc {} :key \"value\") :lock))"

    (clojure-unwind)
    (clojure-unwind))

  (when-refactoring-it "should unwind -> completely"
    "(-> {}
    (assoc :key \"value\")
    (dissoc :lock))"

    "(dissoc (assoc {} :key \"value\") :lock)"

    (clojure-unwind)
    (clojure-unwind)
    (clojure-unwind))

  (when-refactoring-it "should unwind ->> one step"
    "(->> [1 2 3 4 5]
     (filter even?)
     (map square))"

    "(->> (filter even? [1 2 3 4 5])
     (map square))"

    (clojure-unwind))

  (when-refactoring-it "should unwind ->> two steps"
    "(->> [1 2 3 4 5]
     (filter even?)
     (map square))"

    "(->> (map square (filter even? [1 2 3 4 5])))"

    (clojure-unwind)
    (clojure-unwind))

  (when-refactoring-it "should unwind ->> completely"
    "(->> [1 2 3 4 5]
     (filter even?)
     (map square))"

    "(map square (filter even? [1 2 3 4 5]))"

    (clojure-unwind)
    (clojure-unwind)
    (clojure-unwind))

  (when-refactoring-it "should unwind N steps with numeric prefix arg"
    "(->> [1 2 3 4 5]
     (filter even?)
     (map square)
     sum)"

    "(->> (sum (map square (filter even? [1 2 3 4 5]))))"

    (clojure-unwind 3))

  (when-refactoring-it "should unwind completely with universal prefix arg"
    "(->> [1 2 3 4 5]
     (filter even?)
     (map square)
     sum)"

    "(sum (map square (filter even? [1 2 3 4 5])))"

    (clojure-unwind '(4)))

  (when-refactoring-it "should unwind with function name"
    "(->> [1 2 3 4 5]
     sum
     square)"

    "(->> (sum [1 2 3 4 5])
     square)"

    (clojure-unwind))

  (when-refactoring-it "should unwind with function name twice"
    "(-> [1 2 3 4 5]
     sum
     square)"

    "(-> (square (sum [1 2 3 4 5])))"

    (clojure-unwind)
    (clojure-unwind))

  (when-refactoring-it "should thread-issue-6-1"
    "(defn plus [a b]
  (-> a (+ b)))"

    "(defn plus [a b]
  (-> (+ a b)))"

    (clojure-unwind))

  (when-refactoring-it "should thread-issue-6-2"
    "(defn plus [a b]
  (->> a (+ b)))"

    "(defn plus [a b]
  (->> (+ b a)))"

    (clojure-unwind))

  (when-refactoring-it "should unwind some->"
    "(some-> {:a 1}
        (find :b)
        val
        (+ 5))"

    "(some-> (+ (val (find {:a 1} :b)) 5))"

    (clojure-unwind)
    (clojure-unwind)
    (clojure-unwind))

  (when-refactoring-it "should unwind some->>"
    "(some->> :b
         (find {:a 1})
         val
         (+ 5))"

    "(some->> (+ 5 (val (find {:a 1} :b))))"

    (clojure-unwind)
    (clojure-unwind)
    (clojure-unwind)))

(describe "clojure-thread-first-all"

  (when-refactoring-it "should thread first all sexps"
    "(->map (assoc {} :key \"value\") :lock)"

    "(-> {}
    (assoc :key \"value\")
    (->map :lock))"

    (beginning-of-buffer)
    (clojure-thread-first-all nil))

  (when-refactoring-it "should thread a form except the last expression"
    "(->map (assoc {} :key \"value\") :lock)"

    "(-> (assoc {} :key \"value\")
    (->map :lock))"

    (beginning-of-buffer)
    (clojure-thread-first-all t)))

(describe "clojure-thread-last-all"

  (when-refactoring-it "should fully thread a form"
    "(map square (filter even? (make-things)))"

    "(->> (make-things)
     (filter even?)
     (map square))"

    (beginning-of-buffer)
    (clojure-thread-last-all nil))

  (when-refactoring-it "should thread a form except the last expression"
    "(map square (filter even? (make-things)))"

    "(->> (filter even? (make-things))
     (map square))"

    (beginning-of-buffer)
    (clojure-thread-last-all t))

  (when-refactoring-it "should handle dangling parens 1"
    "(map inc
        (range))"

    "(->> (range)
     (map inc))"

    (beginning-of-buffer)
    (clojure-thread-last-all nil))

  (when-refactoring-it "should handle dangling parens 2"
    "(deftask dev []
  (comp (serve)
   (cljs)))"

    "(->> (cljs)
     (comp (serve))
     (deftask dev []))"

    (beginning-of-buffer)
    (clojure-thread-last-all nil)))

(describe "clojure-unwind-all"

  (when-refactoring-it "should unwind all in ->"
    "(-> {}
     (assoc :key \"value\")
           (dissoc :lock))"

    "(dissoc (assoc {} :key \"value\") :lock)"

    (beginning-of-buffer)
    (clojure-unwind-all))

  (when-refactoring-it "should unwind all in ->>"
    "(->> (make-things)
           (filter even?)
           (map square))"

    "(map square (filter even? (make-things)))"

    (beginning-of-buffer)
    (clojure-unwind-all))

  ;; fix for clojure-emacs/clj-refactor.el#259
  (when-refactoring-it "should leave multiline sexp alone"
    "(->> [a b]
     (some (fn [x]
             (when x
               10))))"

    "(some (fn [x]
        (when x
          10))
      [a b])"

    (clojure-unwind-all))

  (when-refactoring-it "should thread-last-maybe-unjoin-lines"
    "(deftask dev []
  (comp (serve)
        (cljs (lala)
              10)))"

    "(deftask dev []
  (comp (serve)
        (cljs (lala)
              10)))"

    (goto-char (point-min))
    (clojure-thread-last-all nil)
    (clojure-unwind-all)))

(describe "clojure-thread-first-all"

  (when-refactoring-it "should thread with an empty first line"
    "(map
   inc
   [1 2])"

    "(-> inc
    (map
     [1 2]))"

    (goto-char (point-min))
    (clojure-thread-first-all nil))

  (when-refactoring-it "should thread-first-maybe-unjoin-lines"
    "(map
 inc
 [1 2])"

    "(map
 inc
 [1 2])"

    (goto-char (point-min))
    (clojure-thread-first-all nil)
    (clojure-unwind-all)))

(provide 'clojure-mode-refactor-threading-test)

;;; clojure-mode-refactor-threading-test.el ends here
