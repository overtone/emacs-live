Feature: Extract def

  Background:
    Given I have a project "cljr" in "tmp"
    And I have a clojure-file "tmp/src/cljr/core.clj"
    And I open file "tmp/src/cljr/core.clj"
    And I clear the buffer

  Scenario: Extract def, basic
    When I insert:
    """
    (ns cljr.core)

    (defn add-five [a]
      (+ a 5))
    """
    And I place the cursor before "5"
    And I start an action chain
    And I press "C-! ed"
    And I type "five"
    And I press "RET"
    And I execute the action chain
    Then I should see:
    """
    (ns cljr.core)

    (def five 5)

    (defn add-five [a]
      (+ a five))
    """

  Scenario: Extract def, query for others
    When I insert:
    """
    (ns cljr.core)

    (defn multiply-by-five [a]
      (* a 5))

    (defn add-five [a]
      (+ a 5))
    """
    And I place the cursor before "5"
    And I start an action chain
    And I press "C-! ed"
    And I type "five"
    And I press "RET"
    And I press "y"
    And I execute the action chain
    Then I should see:
    """
    (ns cljr.core)

    (def five 5)

    (defn multiply-by-five [a]
      (* a five))

    (defn add-five [a]
      (+ a five))
    """

  Scenario: Extract def, append to group of defs
    When I insert:
    """
    (ns cljr.core)

    (def one 1)
    (def two 2)

    (def three 3)

    (defn add-five [a]
      (+ a 5))
    """
    And I place the cursor before "5"
    And I start an action chain
    And I press "C-! ed"
    And I type "five"
    And I press "RET"
    And I execute the action chain
    Then I should see:
    """
    (ns cljr.core)

    (def one 1)
    (def two 2)

    (def three 3)
    (def five 5)

    (defn add-five [a]
      (+ a five))
    """
