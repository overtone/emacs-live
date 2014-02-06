Feature: Add namespace to blank .clj files

  Background:
    Given I have a project "cljr" in "tmp"

  Scenario: Source file
    When I open file "tmp/src/cljr/core.clj"
    Then I should see "(ns cljr.core)"

  Scenario: Test file
    When I open file "tmp/test/cljr/core_test.clj"
    Then I should see:
    """
    (ns cljr.core-test
      (:require [cljr.core :refer :all]
                [clojure.test :refer :all]))
    """

  Scenario: Midje
    When I open file "tmp/project.clj"
    And I press "C-M-f"
    And I press "C-b"
    And I press "C-j"
    And I type ":profiles {:dev {:dependencies [[midje "1.4"]]}}"
    And I press "C-x C-s"
    When I open file "tmp/test/cljr/core_test.clj"
    Then I should see:
    """
    (ns cljr.core-test
      (:require [cljr.core :refer :all]
                [midje.sweet :refer :all]))
    """
