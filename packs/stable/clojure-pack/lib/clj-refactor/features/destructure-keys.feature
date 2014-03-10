Feature: Destructure keys

  Background:
    Given I have a project "cljr" in "tmp"
    And I have a clojure-file "tmp/src/cljr/core.clj"
    And I open file "tmp/src/cljr/core.clj"
    And I clear the buffer

  Scenario: Just keys
    When I insert:
    """
    (defn f [o]
      (+ (:a1 o) (:a2 o)))
    """
    And I place the cursor before "o]"
    And I press "C-! dk"
    Then I should see:
    """
    (defn f [{:keys [a1 a2]}]
      (+ a1 a2))
    """

  Scenario: More than just keys
    When I insert:
    """
    (defn f [o]
      (+ (:a1 o) (some-fn o)))
    """
    And I place the cursor before "o]"
    And I press "C-! dk"
    Then I should see:
    """
    (defn f [{:keys [a1] :as o}]
      (+ a1 (some-fn o)))
    """

  Scenario: Let statement with ref
    When I insert:
    """
    (let [a (get-obj)
          b (:p1 a)
          c (:p2 a)]
      (+ b c (:p3 a)))
    """
    And I place the cursor before "a ("
    And I press "C-! dk"
    Then I should see:
    """
    (let [{:keys [p1 p2 p3]} (get-obj)
          b p1
          c p2]
      (+ b c p3))
    """

  Scenario: Duplicates
    When I insert:
    """
    (defn f [o]
      (+ (:a1 o) (:a1 o)))
    """
    And I place the cursor before "o]"
    And I press "C-! dk"
    Then I should see:
    """
    (defn f [{:keys [a1]}]
      (+ a1 a1))
    """
