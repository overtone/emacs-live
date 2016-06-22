Feature: replace refer all with aliases

  Background:
    Given I have a project "example" in "tmp"
    And I have a clojure-file "tmp/src/example/one.clj"
    When I open file "tmp/src/example/one.clj"
    And I clear the buffer
    And I press "M->"
    And I insert:
    """
    (ns example.one
      (:require [example.two :refer :all]))

    (defn bar []
      (str "bar" (foo) "goo"))

    (defn sky []
      (str "sky: " (star*)))
    """
    And I press "C-x C-s"
    And I press "C-x k"

  Scenario: replace refer all with aliased refer
    Given I open file "tmp/src/example/one.clj"
    And I call replace refer all with alias with mock data for "example.two"
    Then I should see:
    """
    (ns example.one
      (:require [example.two :as two]))

    (defn bar []
      (str "bar" (two/foo) "goo"))

    (defn sky []
      (str "sky: " (two/star*)))
    """
