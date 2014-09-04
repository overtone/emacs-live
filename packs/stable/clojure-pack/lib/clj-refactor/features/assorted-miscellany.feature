Feature: Tests for some minor features

  Background:
    Given I have a project "cljr" in "tmp"
    And I have a clojure-file "tmp/src/cljr/core.clj"
    And I open file "tmp/src/cljr/core.clj"
    And I clear the buffer

  Scenario: Remove # when raising sexp for function literals
    Given I turn on paredit-mode
    When I insert "(map #(partial f) l)"
    And I place the cursor before "f"
    And I press "M-r"
    Then I should see "(map f l)"

  Scenario: Remove # when splicing sexp killing backward
    Given I turn on paredit-mode
    When I insert "(map #(partial f) l)"
    And I place the cursor before "f"
    And I press "M-<up>"
    Then I should see "(map f l)"

  Scenario: Remove # when splicing sexp killing forward
    Given I turn on paredit-mode
    When I insert "(map #(partial f d) l)"
    And I place the cursor after "f"
    And I press "M-<down>"
    Then I should see "(map partial f l)"
