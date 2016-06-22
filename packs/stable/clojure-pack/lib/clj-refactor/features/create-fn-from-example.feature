Feature: Create Function from Example

  Background:
    Given I have a project "cljr" in "tmp"
    And I have a clojure-file "tmp/src/cljr/core.clj"
    And I open file "tmp/src/cljr/core.clj"
    And I clear the buffer

  Scenario: Create function from example, all words
    When I insert:
    """
    (defn some-fn []
      (foo bar baz))
    """
    And I place the cursor before "bar"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- foo [bar baz]
      )

    (defn some-fn []
      (foo bar baz))
    """

   Scenario: Create function from example, line breaks
    When I insert:
    """
    (defn some-fn []
      (foo
        bar
        baz))
    """
    And I place the cursor before "bar"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- foo [bar baz]
      )

    (defn some-fn []
      (foo
        bar
        baz))
    """

  Scenario: Create function from example, all sorts of parameters
    When I insert:
    """
    (defn some-fn []
      (foo
        "some string"
        bar
        {:keyword :value}
        (clojure.string/trim (:a some-map))
        baz))
    """
    And I place the cursor before "baz"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- foo [arg0 bar arg2 arg3 baz]
      )

    (defn some-fn []
      (foo
        "some string"
        bar
        {:keyword :value}
        (clojure.string/trim (:a some-map))
        baz))
    """

  Scenario: Create function from example no params
    When I insert:
    """
    (defn some-fn []
      (foo))
    """
    And I place the cursor after "foo"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- foo []
      )

    (defn some-fn []
      (foo))
    """

  Scenario: Create function from example using literals
    When I insert:
    """
    (defn some-fn []
      (foo 1 bar :key #{1 2}))
    """
    And I place the cursor after "foo"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- foo [arg0 bar arg2 arg3]
      )

    (defn some-fn []
      (foo 1 bar :key #{1 2}))
    """

  Scenario: Create function from example, don't create keyword functions
    When I insert "(foo (:bar baz))"
    And I place the cursor before "baz"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- foo [bar]
      )

    (foo (:bar baz))
    """

  Scenario: Create function from example with thread-first
    When I insert "(-> game (reveal-tile index))"
    And I place the cursor after "reveal"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- reveal-tile [game index]
      )

    (-> game (reveal-tile index))
    """

  Scenario: Create function from example with thread-last
    When I insert "(->> game (reveal-tile index))"
    And I place the cursor after "reveal"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- reveal-tile [index game]
      )

    (->> game (reveal-tile index))
    """

  Scenario: Create function from example, update
    When I insert "(update foo :bar do-stuff)"
    And I place the cursor after "do"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- do-stuff [bar]
      )

    (update foo :bar do-stuff)
    """

  Scenario: Create function from example, update-in
    When I insert "(update-in foo [:bar :baz] do-stuff)"
    And I place the cursor after "do"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- do-stuff [baz]
      )

    (update-in foo [:bar :baz] do-stuff)
    """

  Scenario: Create function from example, update-in + threading
    When I insert "(-> foo (update-in [:bar :baz] do-stuff))"
    And I place the cursor after "do"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- do-stuff [baz]
      )

    (-> foo (update-in [:bar :baz] do-stuff))
    """

  Scenario: Create function from example, map
    When I insert "(map do-stuff items (:oxen bar))"
    And I place the cursor after "do"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- do-stuff [item ox]
      )

    (map do-stuff items (:oxen bar))
    """

  Scenario: Create function from example, invalid fn name
    When I insert "(map :key items)"
    And I place the cursor after "key"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- map [arg0 items]
      )

    (map :key items)
    """

  Scenario: Create function from example, sort-by keyfn
    When I insert "(sort-by my-keyfn comp items)"
    And I place the cursor after "my"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- my-keyfn [item]
      )

    (sort-by my-keyfn comp items)
    """

  Scenario: Create function from example, sort-by comp
    When I insert "(sort-by :name my-comp items)"
    And I place the cursor after "my"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- my-comp [name-a name-b]
      )

    (sort-by :name my-comp items)
    """

  Scenario: Create function from example, sort comp
    When I insert "(sort my-comp [1 2 3])"
    And I place the cursor after "my"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- my-comp [a b]
      )

    (sort my-comp [1 2 3])
    """

  Scenario: Create function from example, keep-indexed
    When I insert "(keep-indexed do-stuff foos (:bars baz))"
    And I place the cursor after "do"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- do-stuff [index foo bar]
      )

    (keep-indexed do-stuff foos (:bars baz))
    """

  Scenario: Create function from example, reduce 2 args
    When I insert "(reduce my-fold items)"
    And I place the cursor after "my"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- my-fold [acc item]
      )

    (reduce my-fold items)
    """

  Scenario: Create function from example, reduce 3 args
    When I insert "(reduce my-fold game items)"
    And I place the cursor after "my"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- my-fold [game item]
      )

    (reduce my-fold game items)
    """

  Scenario: Create function from example, reduce 3 args, anonymous val
    When I insert "(reduce my-fold {} items)"
    And I place the cursor after "my"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- my-fold [acc item]
      )

    (reduce my-fold {} items)
    """

  Scenario: Create function from example, naked fn in threading
    When I insert "(-> game do-stuff)"
    And I place the cursor after "do"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- do-stuff [game]
      )

    (-> game do-stuff)
    """

  Scenario: Guess at param name, keyword
    When I insert "(do-stuff (:foo x) (:bar x))"
    And I place the cursor after "do"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- do-stuff [foo bar]
      )

    (do-stuff (:foo x) (:bar x))
    """

  Scenario: Guess at param name, threading
    When I insert "(do-stuff (-> x :foo) (->> x :bar :baz))"
    And I place the cursor after "do"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- do-stuff [foo baz]
      )

    (do-stuff (-> x :foo) (->> x :bar :baz))
    """

  Scenario: Guess at param name, get-in
    When I insert "(do-stuff (get-in x [:foo :bar]))"
    And I place the cursor after "do"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- do-stuff [bar]
      )

    (do-stuff (get-in x [:foo :bar]))
    """

  Scenario: Guess at param name, get
    When I insert "(do-stuff (get x :foo) (get x (:bar y)))"
    And I place the cursor after "do"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- do-stuff [foo arg1]
      )

    (do-stuff (get x :foo) (get x (:bar y)))
    """

  Scenario: Guess at param name, create
    When I insert "(do-stuff (create-thing))"
    And I place the cursor after "do"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- do-stuff [thing]
      )

    (do-stuff (create-thing))
    """

  Scenario: Guess at param name, constructor
    When I insert "(do-stuff (FileDescriptor. path))"
    And I place the cursor after "do"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- do-stuff [file-descriptor]
      )

    (do-stuff (FileDescriptor. path))
    """

  Scenario: Guess at param name, namespaced constructor
    When I insert "(do-stuff (java.io.FileDescriptor. path))"
    And I place the cursor after "do"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- do-stuff [file-descriptor]
      )

    (do-stuff (java.io.FileDescriptor. path))
    """

  Scenario: Guess at param name, java getter
    When I insert "(do-stuff (.getName f))"
    And I place the cursor after "do"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- do-stuff [name]
      )

    (do-stuff (.getName f))
    """

  Scenario: Guess at param name, ignore assoc, update and dissoc
    When I insert "(do-stuff (-> game (assoc :foo true) (update :bar inc) (dissoc :baz)))"
    And I place the cursor after "do"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- do-stuff [game]
      )

    (do-stuff (-> game (assoc :foo true) (update :bar inc) (dissoc :baz)))
    """

  Scenario: Guess at param name, ignore assoc-in and update-in
    When I insert "(do-stuff (-> game (assoc-in [:foo] true) (update-in [:bar] inc)))"
    And I place the cursor after "do"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- do-stuff [game]
      )

    (do-stuff (-> game (assoc-in [:foo] true) (update-in [:bar] inc)))
    """

  Scenario: Guess at param name, singular when picking things out of colls
    When I insert "(do-stuff (first things) (second items) (last foos))"
    And I place the cursor after "do"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- do-stuff [thing item foo]
      )

    (do-stuff (first things) (second items) (last foos))
    """

  Scenario: Guess at param name, plural when repeating
    When I insert "(do-stuff (repeat 100 (:item stuff)))"
    And I place the cursor after "do"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- do-stuff [items]
      )

    (do-stuff (repeat 100 (:item stuff)))
    """

  Scenario: Guess at param name, don't get confused by multiline stuff
    When I insert:
    """
    (do-stuff (first (keep-indexed (fn [index tile]
                          (when (and (= face (:face tile))
                                     (not (:revealed? tile)))
                            index))
                        (:tiles game))))
    """
    And I place the cursor after "do"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- do-stuff [arg0]
      )

    (do-stuff (first (keep-indexed (fn [index tile]
                          (when (and (= face (:face tile))
                                     (not (:revealed? tile)))
                            index))
                        (:tiles game))))
    """

  Scenario: Placeholders for args
    When I insert:
    """
    (defn some-fn []
      (foo 1 2))
    """
    And I place the cursor after "foo"
    And I press "C-! fe"
    And I type "one"
    And I press "<tab>"
    And I type "two"
    And I press "<tab>"
    And I type "body"
    Then I should see:
    """
    (defn- foo [one two]
      body)

    (defn some-fn []
      (foo 1 2))
    """

    Scenario: Create fn in another namespace
    When I insert:
    """
    (ns core
      (:require [refactor-nrepl.util :as util]))

    (defn some-fn []
      (util/foo bar baz))
    """
    And I have a clojure-file "tmp/src/cljr/util.clj"
    And cljr--ns-path returns "tmp/src/cljr/util.clj"
    And I place the cursor before "foo"
    And I press "C-! fe"
    And I open file "tmp/src/cljr/util.clj"
    Then I should see:
    """
    (ns cljr.util)

    (defn foo [bar baz]
      )
    """
    And I kill the "util.clj" buffer
    And I kill the "core.clj" buffer
