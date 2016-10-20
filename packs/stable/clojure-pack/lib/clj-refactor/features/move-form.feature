Feature: Move forms

  Background:
    Given I have a project "cljr" in "tmp"
    And I have a clojure-file "tmp/src/cljr/src.clj"
    And I have a clojure-file "tmp/src/cljr/dest.clj"
    And I have a clojure-file "tmp/src/cljr/target.clj"
    And I open file "tmp/src/cljr/dest.clj"
    And I clear the buffer
    And I insert:
    """
    (ns cljr.dest)

    (defn frobinator [a b]
      (+ a b))
    """
    And I open file "tmp/src/cljr/target.clj"
    And I clear the buffer
    And I insert:
    """
    (ns cljr.target)

    (defn really-do-it [x] (println x))
    """
    And I open file "tmp/src/cljr/src.clj"
    And I clear the buffer

  Scenario: Move the fn surrounding cursor
    When I insert:
    """
    (ns cljr.src)

    (defn add [a b]
      (+ a b))

    (add 1 2)
    """
    And the cursor is inside the first defn form
    And I disable cljr-clean-ns
    And I start an action chain
    And I press "C-! mf"
    And I type "dest.clj"
    And I press "RET"
    And I execute the action chain
    Then I should see:
    """
    (ns cljr.src
      (:require [cljr.dest :refer [add]]))

    (add 1 2)
    """
    And I open file "tmp/src/cljr/dest.clj"
    Then I should see:
    """
    (ns cljr.dest)

    (defn frobinator [a b]
      (+ a b))

    (defn add [a b]
      (+ a b))
    """

  Scenario: Include requires when moving
    When I insert:
    """
    (ns cljr.src
      (:require [clojure.string :as str]))

    (defn foo [y x]
      (str/join x y))

    (foo 1 2)
    """
    And the cursor is inside the first defn form
    And I start an action chain
    And I press "C-! mf"
    And I type "dest.clj"
    And I press "RET"
    And I execute the action chain
    Then I should see:
    """
    (ns cljr.src
      (:require [clojure.string :as str]
                [cljr.dest :refer [foo]]))

    (foo 1 2)
    """
    And I open file "tmp/src/cljr/dest.clj"
    Then I should see:
    """
    (ns cljr.dest
      (:require [clojure.string :as str]))

    (defn frobinator [a b]
      (+ a b))

    (defn foo [y x]
      (str/join x y))
    """

  Scenario: Use visual selection to select multiple defn forms
    When I insert:
    """
    (ns cljr.src)

    (defn add [a b]
      (+ a b))

    (defn sub [a b]
      (- a b))

    (defn mult [a b]
      (* a b))

    (defn div [a b]
      (/ a b))

    (div (add 1 (sub 1 2)) (mult 1 3))
    """
    And I press "M-<"
    And I press "C-2 C-n"
    And I press "C-SPC"
    And I press "C-9 C-n"
    And I start an action chain
    And I press "C-! mf"
    And I type "dest.clj"
    And I press "RET"
    And I execute the action chain
    Then I should see:
    """
    (ns cljr.src
      (:require [cljr.dest :refer [add sub mult]]))

    (defn div [a b]
      (/ a b))

    (div (add 1 (sub 1 2)) (mult 1 3))
    """
    And I open file "tmp/src/cljr/dest.clj"
    Then I should see:
    """
    (ns cljr.dest)

    (defn frobinator [a b]
      (+ a b))

    (defn add [a b]
      (+ a b))

    (defn sub [a b]
      (- a b))

    (defn mult [a b]
      (* a b))
    """

  Scenario: Move the fn surrounding cursor and add to existing :refer
    When I insert:
    """
    (ns cljr.src
      (:require [clojure.string :refer [join split]]))

    (defn add [a b]
      (+ a b))

    (add (this 1) (that 2))
    """
    And the cursor is inside the first defn form
    And I start an action chain
    And I press "C-! mf"
    And I type "dest.clj"
    And I press "RET"
    And I execute the action chain
    Then I should see:
    """
    (ns cljr.src
      (:require [clojure.string :refer [join split]]
                [cljr.dest :refer [add]]))

    (add (this 1) (that 2))
    """
    And I open file "tmp/src/cljr/dest.clj"
    Then I should see:
    """
    (ns cljr.dest
      (:require [clojure.string :refer [join split]]))

    (defn frobinator [a b]
      (+ a b))

    (defn add [a b]
      (+ a b))
    """

  Scenario: Move the selected functions and add to :refer, excluding private fn
    When I insert:
    """
    (ns ^{:meta "..."}
      cljr.foo
      "doc..."
      (:use [cljr.core]
            [cljr.page]
            [cljr.element]
            [cljr.form]
            [some.lib ns1 ns2 ns3])
      (:require [cljr.foobar :as foo]
                [clojure.string :refer [join split]]
                [cljr.dest :refer [this that]])
      (:refer-clojure :exclude [this that])
      (:import [java.util.Date]))

    (defn select
      "Returns a set of the elements for which pred is true"
      {:added "1.0"}
      [pred xset]
        (reduce (fn [s k] (if (pred k) s (disj s k)))
                xset xset))

    (defn- add
      "docstring"
      {:map "with" :random "facts"}
      [a b]
      (+ a b))

    (defroute create "/*/create")
    (defroute update "/*/update/:id")

    (defn find-doc
      "Prints documentation for any var whose documentation or name
     contains a match for re-string-or-pattern"
      {:added "1.0"}
      [re-string-or-pattern]
        (let [re  (re-pattern re-string-or-pattern)]
          (doseq [ns (all-ns)
                  v (sort-by (comp :name meta) (vals (ns-interns ns)))
                  :when (and (:doc (meta v))
                             (or (re-find (re-matcher re (:doc (meta v))))
                                 (re-find (re-matcher re (str (:name (meta v)))))))]
                   (print-doc v))))

    (defn rename
      "Returns a rel of the maps in xrel with the keys in kmap renamed to the vals in kmap"
      {:added "1.0"}
      [xrel kmap]
        (set (map #(rename-keys kmap) xrel)))

    (select (find-doc (foo/those)))
    """
    And I press "M-<"
    And I press "C-u 13 C-n"
    And I press "C-SPC"
    And I press "C-u 30 C-n"
    And I start an action chain
    And I press "C-! mf"
    And I type "dest.clj"
    And I press "RET"
    And I execute the action chain
    Then I should see:
    """
    (ns ^{:meta "..."}
      cljr.foo
      "doc..."
      (:use [cljr.core]
            [cljr.page]
            [cljr.element]
            [cljr.form]
            [some.lib ns1 ns2 ns3])
      (:require [cljr.foobar :as foo]
                [clojure.string :refer [join split]]
                [cljr.dest :refer [this that select find-doc]])
      (:refer-clojure :exclude [this that])
      (:import [java.util.Date]))

    (defn rename
      "Returns a rel of the maps in xrel with the keys in kmap renamed to the vals in kmap"
      {:added "1.0"}
      [xrel kmap]
        (set (map #(rename-keys kmap) xrel)))

    (select (find-doc (foo/those)))
    """
    And I open file "tmp/src/cljr/dest.clj"
    Then I should see:
    """
    (ns cljr.dest
      (:require [cljr.foobar :as foo]
                [clojure.string :refer [join split]]))

    (defn frobinator [a b]
      (+ a b))

    (defn select
      "Returns a set of the elements for which pred is true"
      {:added "1.0"}
      [pred xset]
        (reduce (fn [s k] (if (pred k) s (disj s k)))
                xset xset))

    (defn- add
      "docstring"
      {:map "with" :random "facts"}
      [a b]
      (+ a b))

    (defroute create "/*/create")
    (defroute update "/*/update/:id")

    (defn find-doc
      "Prints documentation for any var whose documentation or name
     contains a match for re-string-or-pattern"
      {:added "1.0"}
      [re-string-or-pattern]
        (let [re  (re-pattern re-string-or-pattern)]
          (doseq [ns (all-ns)
                  v (sort-by (comp :name meta) (vals (ns-interns ns)))
                  :when (and (:doc (meta v))
                             (or (re-find (re-matcher re (:doc (meta v))))
                                 (re-find (re-matcher re (str (:name (meta v)))))))]
                   (print-doc v))))
    """

  Scenario: Move forms does not create circular dependencies, #341
    When I insert:
    """
    (ns cljr.src
      (:require [cljr.target :as t]
                [clojure.set :as st]
                [clojure.string :as str]))

    (defn doit [x]
      (str/join (st/union '("a" "b")))
      (t/really-do-it x))
    """
    And the cursor is inside the first defn form
    And I start an action chain
    And I press "C-! mf"
    And I type "target.clj"
    And I press "RET"
    And I execute the action chain
    Then I should see:
    """
    (ns cljr.src
      (:require [cljr.target :as t :refer [doit]]
                [clojure.set :as st]
                [clojure.string :as str]))
    """
    And I open file "tmp/src/cljr/target.clj"
    Then I should see:
    """
    (ns cljr.target
      (:require [clojure.set :as st]
                [clojure.string :as str]))

    (defn really-do-it [x] (println x))

    (defn doit [x]
      (str/join (st/union '("a" "b")))
      (really-do-it x))
    """