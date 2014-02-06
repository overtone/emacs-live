Feature: Rename a file, update namespaces

  Background:
    Given I have a project "cljr" in "tmp"

  Scenario: Rename and update namespace
    Given I have a clojure-file "tmp/src/cljr/rename_me.clj"
    When I open file "tmp/src/cljr/rename_me.clj"
    And I start an action chain
    And I press "C-! rf"
    And I press "C-2 M-b"
    And I press "M-d"
    And I type "done"
    And I press "RET"
    And I execute the action chain
    Then the file should be named "tmp/src/cljr/rename_done.clj"
    And I should see "(ns cljr.rename-done)"

  Scenario: Rename and update dependencies
    Given I have a clojure-file "tmp/src/cljr/dependency.clj"
    And I have a clojure-file "tmp/src/cljr/dependent_file.clj"

    When I open file "tmp/src/cljr/dependent_file.clj"
    And I press "M->"
    And I insert:
    """

    (require '[cljr.dependency])
    (cljr.dependency/abc 123)
    """
    And I press "C-x C-s"

    When I open file "tmp/src/cljr/dependency.clj"
    And I start an action chain
    And I press "C-! rf"
    And I press "C-2 M-b"
    And I press "M-d"
    And I type "renamed"
    And I press "RET"
    And I press "y"
    And I press "y"
    And I press "y"
    And I execute the action chain

    When I open file "tmp/src/cljr/dependent_file.clj"
    Then I should see "(cljr.renamed/abc 123)"
    And I should see "(require '[cljr.renamed])"

  Scenario: Don't update related but unchanged namespaces
    Given I have a clojure-file "tmp/src/cljr/rename_me.clj"
    And I have a clojure-file "tmp/test/cljr/rename_me_test.clj"

    When I open file "tmp/src/cljr/rename_me.clj"
    And I start an action chain
    And I press "C-! rf"
    And I press "C-2 M-b"
    And I press "M-d"
    And I type "done"
    And I press "RET"
    And I press "y"
    And I press "y"
    And I press "y"
    And I execute the action chain

    When I open file "tmp/test/cljr/rename_me_test.clj"
    Then I should not see "(ns cljr.rename-done-test"
