Feature: find usages of a given symbol; nrepl middleware response(s) mocked (big time)

  Background:
    Given I switch project-clean-prompt off
    And I have a project "example" in "tmp"
    And I have a clojure-file "tmp/src/example/one.clj"
    And I have a clojure-file "tmp/src/example/two.clj"
    When I open file "tmp/src/example/one.clj"
    And I clear the buffer
    And I press "M->"
    And I insert:
    """
    (ns example.one
      (:require [example.two :as two]))

    (defn bar []
      (str "bar" (two/foo) "goo"))

    (defn sky []
      (str "sky: " (two/star*)))
    """
    And I press "C-x C-s"
    And I press "C-x k"
    And I open file "tmp/src/example/two.clj"
    And I clear the buffer
    And I press "M->"
    And I insert:
    """
    (ns example.two)

    (defn foo []
      "goo")

    (defn star* []
      "star")
    """
    And I press "C-x C-s"
    And I press "C-x k"

  Scenario: find usages of two/foo
    Given I call find usages for "foo"
    And I pop to find usages buffer
    Then I should see:
    """
    'foo' occurs in the following places:

    tmp/src/example/two.clj:3:6: (defn foo []
    tmp/src/example/one.clj:5:14: (str "bar" (two/foo) "goo"))

    Find symbol finished: 2 occurrences found
    """

  Scenario: find usages eliminates duplicates
    Given I call find usages for "foo" and the middleware gives me three matches
    And I pop to find usages buffer
    Then I should see:
    """
    'foo' occurs in the following places:

    tmp/src/example/two.clj:3:6: (defn foo []
    tmp/src/example/one.clj:5:14: (str "bar" (two/foo) "goo"))

    Find symbol finished: 3 occurrences found
    """
