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

  Scenario: Declare a schema def
    When I insert:
    """
    (ns cljr.core)

    (s/defn foo
      [a b]
      (+ a b))
    """
    And the cursor is inside the first defn form
    And I press "C-! ad"
    Then I should see:
    """
    (ns cljr.core)

    (declare foo)

    (s/defn foo
      [a b]
      (+ a b))
    """

  Scenario: Declare the thing at point if outside a def
    When I insert:
    """
    (ns cljr.core)

    (foo :bar)
    """
    And I place the cursor before " :bar"
    And I press "C-! ad"
    Then I should see:
    """
    (ns cljr.core)

    (declare foo)

    (foo :bar)
    """

  Scenario: Declare the thing at point if inside a def
    When I insert:
    """
    (ns cljr.core)

    (declare foo)

    (defn- ^{:meta :data} add
      [a b]
      (bar a b))
    """
    And I place the cursor before " a b"
    And I press "C-u C-! ad"
    Then I should see:
    """
    (ns cljr.core)

    (declare foo bar)

    (defn- ^{:meta :data} add
      [a b]
      (bar a b))
    """
