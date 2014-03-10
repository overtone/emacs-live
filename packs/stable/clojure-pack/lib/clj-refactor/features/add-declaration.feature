Feature: Declare current top-level form

  Background:
    Given I have a project "cljr" in "tmp"
    And I have a clojure-file "tmp/src/cljr/core.clj"
    And I open file "tmp/src/cljr/core.clj"
    And I clear the buffer

  Scenario: Declare the current toplevel form without previous declarations
    When I insert:
    """
    (ns cljr.core)

    (defn- ^:meta add [a b]
      (+ a b))
    """
    And the cursor is inside the first defn form
    And I press "C-! ad"
    Then I should see:
    """
    (ns cljr.core)

    (declare add)

    (defn- ^:meta add [a b]
      (+ a b))
    """

  Scenario: Declare the current toplevel form with previous declarations
    When I insert:
    """
    (ns cljr.core)

    (declare foo)

    (defn- ^{:meta :data} add
      [a b]
      (+ a b))
    """
    And the cursor is inside the first defn form
    And I press "C-! ad"
    Then I should see:
    """
    (ns cljr.core)

    (declare foo add)

    (defn- ^{:meta :data} add
      [a b]
      (+ a b))
    """