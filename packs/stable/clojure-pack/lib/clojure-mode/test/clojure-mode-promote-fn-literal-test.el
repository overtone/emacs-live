;;; clojure-mode-promote-fn-literal-test.el --- Clojure Mode: convert fn syntax -*- lexical-binding: t; -*-

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

;; Tests for clojure-promote-fn-literal

;;; Code:

(require 'clojure-mode)
(require 'buttercup)
(require 'test-helper "test/utils/test-helper")

(describe "clojure-promote-fn-literal"
  :var (names)

  (before-each
    (spy-on 'read-string
            :and-call-fake (lambda (_) (or (pop names) (error "")))))

  (when-refactoring-it "should convert 0-arg fns"
    "#(rand)"
    "(fn [] (rand))"
    (clojure-promote-fn-literal))

  (when-refactoring-it "should convert 1-arg fns"
    "#(= % 1)"
    "(fn [x] (= x 1))"
    (setq names '("x"))
    (clojure-promote-fn-literal))

  (when-refactoring-it "should convert 2-arg fns"
    "#(conj (pop %) (assoc (peek %1) %2 (* %2 %2)))"
    "(fn [acc x] (conj (pop acc) (assoc (peek acc) x (* x x))))"
    (setq names '("acc" "x"))
    (clojure-promote-fn-literal))

  (when-refactoring-it "should convert variadic fns"
    ;; from https://hypirion.com/musings/swearjure
    "#(* (`[~@%&] (+))
   ((% (+)) % (- (`[~@%&] (+)) (*))))"
    "(fn [v & vs] (* (`[~@vs] (+))
   ((v (+)) v (- (`[~@vs] (+)) (*)))))"
    (setq names '("v" "vs"))
    (clojure-promote-fn-literal))

  (when-refactoring-it "should ignore strings and comments"
    "#(format \"%2\" ;; FIXME: %2 is an illegal specifier
   %7) "
    "(fn [_ _ _ _ _ _ id] (format \"%2\" ;; FIXME: %2 is an illegal specifier
   id)) "
    (setq names '("_" "_" "_" "_" "_" "_" "id"))
    (clojure-promote-fn-literal)))


(provide 'clojure-mode-convert-fn-test)


;;; clojure-mode-promote-fn-literal-test.el ends here
