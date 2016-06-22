Feature: Extract constant

  Background:
    Given I have a project "cljr" in "tmp"
    And I have a clojure-file "tmp/src/cljr/core.clj"
    And I open file "tmp/src/cljr/core.clj"
    And I clear the buffer

  Scenario: Extract constant, basic
    When I insert:
    """
    (ns cljr.core)

    (defn add-five [a]
      (+ a 5))
    """
    And I place the cursor before "5"
    And I start an action chain
    And I press "C-! ec"
    And I type "five"
    And I press "RET"
    And I execute the action chain
    Then I should see:
    """
    (ns cljr.core)

    (def ^:const five 5)

    (defn add-five [a]
      (+ a five))
    """

  Scenario: Extract constant, query for others
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
    And I press "C-! ec"
    And I type "five"
    And I press "RET"
    And I press "y"
    And I execute the action chain
    Then I should see:
    """
    (ns cljr.core)

    (def ^:const five 5)

    (defn multiply-by-five [a]
      (* a five))

    (defn add-five [a]
      (+ a five))
    """
