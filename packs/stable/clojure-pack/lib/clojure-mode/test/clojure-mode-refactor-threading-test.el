;;; clojure-mode-refactor-threading-test.el --- Clojure Mode: refactor threading tests  -*- lexical-binding: t; -*-

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

;; The threading refactoring code is ported from clj-refactor.el
;; and mainly the work of Magnar Sveen, Alex Baranosky and
;; the rest of the clj-reafctor.el team.

;;; Code:

(require 'clojure-mode)
(require 'ert)

(defmacro def-threading-test (name before after &rest body)
  (declare (indent 3))
  (let ((sym (intern (format "test-thread-%s" name))))
    `(progn
       (put ',sym 'definition-name ',name)
       (ert-deftest ,sym ()
         (let ((clojure-thread-all-but-last nil))
           (with-temp-buffer
             (insert ,before)
             (clojure-mode)
             ,@body
             (should (equal ,(concat "\n" after)
                            (concat "\n" (buffer-substring-no-properties
                                          (point-min) (point-max)))))))))))

;; thread first

(def-threading-test first-one-step
    "(-> (dissoc (assoc {} :key \"value\") :lock))"
    "(-> (assoc {} :key \"value\")
    (dissoc :lock))"
  (clojure-thread))

(def-threading-test first-two-steps
    "(-> (dissoc (assoc {} :key \"value\") :lock))"
    "(-> {}
    (assoc :key \"value\")
    (dissoc :lock))"
  (clojure-thread)
  (clojure-thread))

(def-threading-test first-dont-thread-maps
    "(-> (dissoc (assoc {} :key \"value\") :lock))"
    "(-> {}
    (assoc :key \"value\")
    (dissoc :lock))"
  (clojure-thread)
  (clojure-thread)
  (clojure-thread))

(def-threading-test first-dont-thread-last-one
    "(-> (dissoc (assoc (get-a-map) :key \"value\") :lock))"
    "(-> (get-a-map)
    (assoc :key \"value\")
    (dissoc :lock))"
  (clojure-thread)
  (clojure-thread)
  (clojure-thread))

(def-threading-test first-easy-on-whitespace
    "(->
 (dissoc (assoc {} :key \"value\") :lock))"
    "(->
 (assoc {} :key \"value\")
 (dissoc :lock))"
  (clojure-thread))

(def-threading-test first-remove-superfluous-parens
    "(-> (square (sum [1 2 3 4 5])))"
    "(-> [1 2 3 4 5]
    sum
    square)"
  (clojure-thread)
  (clojure-thread))

(def-threading-test first-cursor-before-threading
    "(-> (not (s-acc/mobile? session)))"
    "(-> (s-acc/mobile? session)
    not)"
  (beginning-of-buffer)
  (clojure-thread))

;; unwind thread first
(def-threading-test first-one-step
    "(-> {}
    (assoc :key \"value\")
    (dissoc :lock))"
    "(-> (assoc {} :key \"value\")
    (dissoc :lock))"
  (clojure-unwind))

(def-threading-test first-two-steps
    "(-> {}
    (assoc :key \"value\")
    (dissoc :lock))"
    "(-> (dissoc (assoc {} :key \"value\") :lock))"
  (clojure-unwind)
  (clojure-unwind))

(def-threading-test first-jump-out-of-threading
    "(-> {}
    (assoc :key \"value\")
    (dissoc :lock))"
    "(dissoc (assoc {} :key \"value\") :lock)"
  (clojure-unwind)
  (clojure-unwind)
  (clojure-unwind))

;; thread last
(def-threading-test last-one-step
    "(->> (map square (filter even? [1 2 3 4 5])))"
    "(->> (filter even? [1 2 3 4 5])
     (map square))"
  (clojure-thread))

(def-threading-test last-two-steps
    "(->> (map square (filter even? [1 2 3 4 5])))"
    "(->> [1 2 3 4 5]
     (filter even?)
     (map square))"
  (clojure-thread)
  (clojure-thread))

(def-threading-test last-dont-thread-vectors
    "(->> (map square (filter even? [1 2 3 4 5])))"
    "(->> [1 2 3 4 5]
     (filter even?)
     (map square))"
  (clojure-thread)
  (clojure-thread)
  (clojure-thread))

(def-threading-test last-dont-thread-last-one
    "(->> (map square (filter even? (get-a-list))))"
    "(->> (get-a-list)
     (filter even?)
     (map square))"
  (clojure-thread)
  (clojure-thread)
  (clojure-thread))

;; unwind thread last
(def-threading-test last-one-step
    "(->> [1 2 3 4 5]
     (filter even?)
     (map square))"
    "(->> (filter even? [1 2 3 4 5])
     (map square))"
  (clojure-unwind))

(def-threading-test last-two-steps
    "(->> [1 2 3 4 5]
     (filter even?)
     (map square))"
    "(->> (map square (filter even? [1 2 3 4 5])))"
  (clojure-unwind)
  (clojure-unwind))

(def-threading-test last-jump-out-of-threading
    "(->> [1 2 3 4 5]
     (filter even?)
     (map square))"
    "(map square (filter even? [1 2 3 4 5]))"
  (clojure-unwind)
  (clojure-unwind)
  (clojure-unwind))

(def-threading-test function-name
    "(->> [1 2 3 4 5]
     sum
     square)"
    "(->> (sum [1 2 3 4 5])
     square)"
  (clojure-unwind))

(def-threading-test function-name-twice
    "(-> [1 2 3 4 5]
     sum
     square)"
    "(-> (square (sum [1 2 3 4 5])))"
  (clojure-unwind)
  (clojure-unwind))

(def-threading-test issue-6-1
    "(defn plus [a b]
  (-> a (+ b)))"
    "(defn plus [a b]
  (-> (+ a b)))"
  (clojure-unwind))

(def-threading-test issue-6-2
    "(defn plus [a b]
  (->> a (+ b)))"
    "(defn plus [a b]
  (->> (+ b a)))"
  (clojure-unwind))

(def-threading-test first-some
    "(some-> (+ (val (find {:a 1} :b)) 5))"
    "(some-> {:a 1}
        (find :b)
        val
        (+ 5))"
  (clojure-thread)
  (clojure-thread)
  (clojure-thread))

(def-threading-test last-some
    "(some->> (+ 5 (val (find {:a 1} :b))))"
    "(some->> :b
         (find {:a 1})
         val
         (+ 5))"
  (clojure-thread)
  (clojure-thread)
  (clojure-thread))

(def-threading-test last-first-some
    "(some-> {:a 1}
        (find :b)
        val
        (+ 5))"
    "(some-> (+ (val (find {:a 1} :b)) 5))"
  (clojure-unwind)
  (clojure-unwind)
  (clojure-unwind))

(def-threading-test thread-last-some
    "(some->> :b
         (find {:a 1})
         val
         (+ 5))"
    "(some->> (+ 5 (val (find {:a 1} :b))))"
  (clojure-unwind)
  (clojure-unwind)
  (clojure-unwind))

(def-threading-test first-all
    "(->map (assoc {} :key \"value\") :lock)"
    "(-> {}
    (assoc :key \"value\")
    (->map :lock))"
  (beginning-of-buffer)
  (clojure-thread-first-all nil))

(def-threading-test first-all-but-last
    "(->map (assoc {} :key \"value\") :lock)"
    "(-> (assoc {} :key \"value\")
    (->map :lock))"
  (beginning-of-buffer)
  (clojure-thread-first-all t))

(def-threading-test last-all
    "(map square (filter even? (make-things)))"
    "(->> (make-things)
     (filter even?)
     (map square))"
  (beginning-of-buffer)
  (clojure-thread-last-all nil))

(def-threading-test last-all-but-last
    "(map square (filter even? (make-things)))"
    "(->> (filter even? (make-things))
     (map square))"
  (beginning-of-buffer)
  (clojure-thread-last-all t))

(def-threading-test all-thread-first
    "(-> {}
    (assoc :key \"value\")
    (dissoc :lock))"
    "(dissoc (assoc {} :key \"value\") :lock)"
  (beginning-of-buffer)
  (clojure-unwind-all))

(def-threading-test all-thread-last
    "(->> (make-things)
     (filter even?)
     (map square))"
    "(map square (filter even? (make-things)))"
  (beginning-of-buffer)
  (clojure-unwind-all))

(def-threading-test last-dangling-parens
    "(map inc
     (range))"
    "(->> (range)
     (map inc))"
  (beginning-of-buffer)
  (clojure-thread-last-all nil))

(def-threading-test last-dangling-parens-2
    "(deftask dev []
  (comp (serve)
        (cljs)))"
    "(->> (cljs)
     (comp (serve))
     (deftask dev []))"
  (beginning-of-buffer)
  (clojure-thread-last-all nil))

;; fix for clojure-emacs/clj-refactor.el#259
(def-threading-test last-leaves-multiline-sexp-alone
    "(->> [a b]
     (some (fn [x]
             (when x
               10))))"
    "(some (fn [x]
        (when x
          10))
      [a b])"
  (clojure-unwind-all))

(def-threading-test last-maybe-unjoin-lines
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
  (clojure-unwind-all))

(def-threading-test empty-first-line
    "(map
 inc
 [1 2])"
    "(-> inc
    (map
     [1 2]))"
  (goto-char (point-min))
  (clojure-thread-first-all nil))

(def-threading-test first-maybe-unjoin-lines
    "(map
 inc
 [1 2])"
    "(map
 inc
 [1 2])"
  (goto-char (point-min))
  (clojure-thread-first-all nil)
  (clojure-unwind-all))

(provide 'clojure-mode-refactor-threading-test)

;;; clojure-mode-refactor-threading-test.el ends here
