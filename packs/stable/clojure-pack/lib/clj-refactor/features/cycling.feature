Feature: Code Cycling

  Background:
    Given I have a project "cljr" in "tmp"
    And I have a clojure-file "tmp/src/cljr/core.clj"
    And I open file "tmp/src/cljr/core.clj"
    And I clear the buffer

  Scenario: Cycling Privacy (defn -> defn-)
    When I insert:
    """
    (defn add [a b]
      (+ a b))
    """
    And I press "C-! cp"
    Then I should see:
    """
    (defn- add [a b]
      (+ a b))
    """

  Scenario: Cycling Privacy (defn -> defn ^:private)
    When I insert:
    """
    (defn add [a b]
      (+ a b))
    """
    And I set cljr-use-metadata-for-privacy to t
    And I press "C-! cp"
    And I set cljr-use-metadata-for-privacy to nil
    Then I should see:
    """
    (defn ^:private add [a b]
      (+ a b))
    """    

  Scenario: Cycling Privacy (defn- -> defn)

    When I insert:
    """
    (defn- add [a b]
      (+ a b))
    """
    And I press "C-! cp"
    Then I should see:
    """
    (defn add [a b]
      (+ a b))
    """

  Scenario: Cycling Privacy (defn ^:private -> defn)

    When I insert:
    """
    (defn ^:private add [a b]
      (+ a b))
    """
    And I set cljr-use-metadata-for-privacy to t
    And I press "C-! cp"
    And I set cljr-use-metadata-for-privacy to nil
    Then I should see:
    """
    (defn add [a b]
      (+ a b))
    """

  Scenario: Cycling Privacy (def -> def ^:private)
    When I insert:
    """
    (def ^:dynamic config
      "docs"
      {:env "staging"})
    """
    And I press "C-! cp"
    Then I should see:
    """
    (def ^:private ^:dynamic config
      "docs"
      {:env "staging"})
    """

  Scenario: Cycling Privacy (def ^:private- -> def)

    When I insert:
    """
    (def ^:private config
      "docs"
      {:env "staging"})
    """
    And I press "C-! cp"
    Then I should see:
    """
    (def config
      "docs"
      {:env "staging"})
    """

  Scenario: Cycling Collection Types (1 2) -> {1 2} 
    When I insert:
    """
    (:a 1 :b 2)
    """
    And I place the cursor before "(:a 1 :b 2)"
    And I press "C-! cc"
    Then I should see:
    """
    {:a 1 :b 2}
    """

  Scenario: Cycling Collection Types {1 2} -> [1 2]
    When I insert:
    """
    {:a 1 :b 2}     
    """
    And I place the cursor before ":b 2}"
    And I press "C-! cc"
    Then I should see:
    """
    [:a 1 :b 2]
    """

  Scenario: Cycling Collection Types [1 2] -> #{1 2} 
    When I insert:
    """
    [1 2 3]   
    """
    And I place the cursor before "[1 2 3]"
    And I press "C-! cc"
    Then I should see:
    """
    #{1 2 3}
    """

  Scenario: Cycling Collection Types #{1 2} -> (1 2)
    When I insert:
    """
    #{1 2 3}
    """
    And I place the cursor before "3}"
    And I press "C-! cc"
    Then I should see:
    """
    (1 2 3)
    """

Scenario: Cycling if to if-not, in inner if
   When I insert:
   """
   (if this
     (if that
       (then AAA)
       (else BBB))
     (otherwise CCC))
   """
   And I place the cursor after "BBB)"
   And I press "C-! ci"
   Then I should see:
   """
   (if this
     (if-not that
       (else BBB)
       (then AAA))
     (otherwise CCC))
   """

Scenario: Cycling if-not to if, in outer if-not
   When I insert:
   """
   (if-not this
     (if that
       (then AAA)
       (else BBB))
     (otherwise CCC))
   """
   And I place the cursor after "BBB))"
   And I press "C-! ci"
   Then I should see:
   """
   (if this
     (otherwise CCC)
     (if that
       (then AAA)
       (else BBB)))
   """

Scenario: Cycling thread-first to thread-last
   When I insert:
   """
   (defn foo [coll]
     (-> coll
         (map inc)))
   """
   And I place the cursor after "(map"
   And I press "C-! ct"
   Then I should see:
   """
   (defn foo [coll]
     (->> coll
          (map inc)))
   """

Scenario: Cycling thread-last to thread-first
   When I insert:
   """
   (defn foo []
     (-> {}
         (->> (assoc :bar 1))))
   """
   And I place the cursor after "(assoc"
   And I press "C-! ct"
   Then I should see:
   """
   (defn foo []
     (-> {}
         (-> (assoc :bar 1))))
   """
