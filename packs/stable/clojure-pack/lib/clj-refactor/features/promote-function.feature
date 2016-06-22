Feature: Tests for some minor features

  Background:
    Given I have a project "cljr" in "tmp"
    And I have a clojure-file "tmp/src/cljr/core.clj"
    And I open file "tmp/src/cljr/core.clj"
    And I clear the buffer
    And I don't use multiple-cursors
    And I switch warn-on-analyzer-needs-eval off
    And I mock out the call to the middleware to find locals

  Scenario: Promote fn to defn
    When I insert:
    """
    (map (fn [sym] (-> sym (str "!") symbol)) '[aww yeah])
    """
    And I place the cursor before "symbol"
    And I start an action chain
    And I press "C-! pf"
    And I type "shout-it!"
    And I press "RET"
    And I execute the action chain
    Then I should see:
    """
    (defn- shout-it!
      [sym]
      (-> sym (str "!") symbol))

    (map shout-it! '[aww yeah])
    """

  Scenario: Promote function literal to a fn
    When I insert:
    """
    (map #(str %) (range 10))
    """
    And I place the cursor before "str"
    And I start an action chain
    And I press "C-! pf"
    And I type "x"
    And I press "RET"
    And I execute the action chain
    Then I should see:
    """
    (map (fn [x] (str x)) (range 10))
    """

  Scenario: Promote named fn to defn
    When I insert:
    """
    ((fn foobar [x] (str x)) 123)
    """
    And I place the cursor before "str"
    And I start an action chain
    And I press "C-! pf"
    And I execute the action chain
    Then I should see:
    """
    (defn- foobar [x]
      (str x))

    (foobar 123)
    """

  Scenario: Promote named fn to defn, with point before args
    When I insert:
    """
    ((fn foobar [x] (str x)) 123)
    """
    And I place the cursor before "foobar"
    And I start an action chain
    And I press "C-! pf"
    And I execute the action chain
    Then I should see:
    """
    (defn- foobar [x]
      (str x))

    (foobar 123)
    """

  Scenario: Promote nested fn inside another fn, with a fn-literal, to a defn
    When I insert:
    """
    (fn [cells]
      (reduce (fn [{:keys [key-1 key-2] :as acc} [idx cell]]
                (assoc acc (if (even? idx) key-1 key-2)
                       (map #(do-stuff %) (range idx))))
              {}
              (map-indexed vector cells)))
    """
    And I place the cursor before ":keys"
    And I start an action chain
    And I press "C-! pf"
    And I type "step"
    And I press "RET"
    And I execute the action chain
    Then I should see:
    """
    (defn- step
      [{:keys [key-1 key-2] :as acc} [idx cell]]
      (assoc acc (if (even? idx) key-1 key-2)
             (map #(do-stuff %) (range idx))))

    (fn [cells]
      (reduce step
              {}
              (map-indexed vector cells)))
    """

  Scenario: Promote fn-literal inside a nested fn
    When I insert:
    """
    (fn [cells]
      (reduce (fn [{:keys [key-1 key-2] :as acc} [idx cell]]
                (assoc acc (if (even? idx) key-1 key-2)
                       (map #(do-stuff %) (range idx))))
              {}
              (map-indexed vector cells)))
    """
    And I place the cursor before "%"
    And I start an action chain
    And I press "C-! pf"
    And I type "x"
    And I press "RET"
    And I execute the action chain
    Then I should see:
    """
    (fn [cells]
      (reduce (fn [{:keys [key-1 key-2] :as acc} [idx cell]]
                (assoc acc (if (even? idx) key-1 key-2)
                       (map (fn [x] (do-stuff x)) (range idx))))
              {}
              (map-indexed vector cells)))
    """

  Scenario: Promote fn capturing locals
    When I insert:
    """
    (let [foo 1
          bar 2]
      (map (fn [n] (+ foo bar n)) [1 2 3]))
    """
    And I place the cursor after "fn"
    And The middleware is mocked to return foo bar as locals
    And I start an action chain
    And I press "C-! pf"
    And I type "foobar-adder"
    And I press "RET"
    And I execute the action chain
    Then I should see:
    """
    (defn- foobar-adder
      [foo bar n]
      (+ foo bar n))

    (let [foo 1
          bar 2]
      (map (partial foobar-adder foo bar) [1 2 3]))
    """
