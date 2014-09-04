Feature: Move forms

  Background:
    Given I have a project "cljr" in "tmp"
    And I have a clojure-file "tmp/src/cljr/src.clj"
    And I have a clojure-file "tmp/src/cljr/dest.clj"
    And I open file "tmp/src/cljr/dest.clj"
    And I clear the buffer
    And I insert:
    """
    (ns cljr.dest)

    (defn frobinator [a b]
      (+ a b))
    """
    And I open file "tmp/src/cljr/src.clj"
    And I clear the buffer

  Scenario: Move the fn surrounding cursor
    When I insert:
    """
    (ns cljr.src)

    (defn add [a b]
      (+ a b))
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
      (:require [cljr.dest :refer [add]]))
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
      (:require [cljr.dest :refer [this that]]))

    (defn add [a b]
      (+ a b))
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
      (:require [cljr.dest :refer [this that add]]))
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
    """
    And I press "M-<"
    And I press "C-u 13 C-n"
    And I press "C-SPC"
    And I press "C-u 29 C-n"
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
            [cljr.element]
            [cljr.form]
            [cljr.page]
            [some.lib ns1 ns2 ns3])
      (:require [cljr.dest :refer [this that select find-doc]]
                [cljr.foobar :as foo])
      (:refer-clojure :exclude [this that])
      (:import [java.util.Date]))

    (defn rename
      "Returns a rel of the maps in xrel with the keys in kmap renamed to the vals in kmap"
      {:added "1.0"}
      [xrel kmap]
        (set (map #(rename-keys kmap) xrel)))
    """
    And I open file "tmp/src/cljr/dest.clj"
    Then I should see:
    """
    (ns cljr.dest)

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