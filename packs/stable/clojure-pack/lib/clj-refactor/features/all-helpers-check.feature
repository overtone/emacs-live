Feature: test if cljr--all-helpers is valid

  Background:
    Given I have a project "cljr" in "tmp"
    And I have a clojure-file "tmp/src/cljr/core.clj"
    And I open file "tmp/src/cljr/core.clj"
    And I clear the buffer

  Scenario: check if one character shortcut is unique for a type
    When I run cljr--all-helpers check