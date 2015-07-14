Feature: renames symbol (def, defn); nrepl middleware response(s) mocked

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

  Scenario: Rename symbol two/foo -> two/baz
    Given I call the rename callback directly with mock data for foo->baz
    When I open file "tmp/src/example/two.clj"
    Then I should see:
    """
    (ns example.two)

    (defn baz []
      "goo")

    (defn star* []
      "star")
    """
    When I open file "tmp/src/example/one.clj"
    Then I should see:
    """
    (ns example.one
      (:require [example.two :as two]))

    (defn bar []
      (str "bar" (two/baz) "goo"))

    (defn sky []
      (str "sky: " (two/star*)))
    """

  Scenario: Rename symbol two/star -> two/asterisk
    Given I call the rename callback directly with mock data for star->asterisk
    When I open file "tmp/src/example/two.clj"
    Then I should see:
    """
    (ns example.two)

    (defn foo []
      "goo")

    (defn asterisk* []
      "star")
    """
    When I open file "tmp/src/example/one.clj"
    Then I should see:
    """
    (ns example.one
      (:require [example.two :as two]))

    (defn bar []
      (str "bar" (two/foo) "goo"))

    (defn sky []
      (str "sky: " (two/asterisk*)))
    """
