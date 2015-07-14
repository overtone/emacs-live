Feature: Add namespace to blank .clj files

  Background:
    Given I have a project "cljr" in "tmp"
    When I have a clojure-file "tmp/src/cljr/core.clj"

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

  Scenario: Test file with foo_ prefix
    When I open file "tmp/test/cljr/foo_core.clj"
    Then I should see:
    """
    (ns cljr.foo-core
      (:require [cljr.core :refer :all]
                [clojure.test :refer :all]))
    """

 Scenario: Test file with _bar suffix
    When I open file "tmp/test/cljr/core_bar.clj"
    Then I should see:
    """
    (ns cljr.core-bar
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

  Scenario: Midje with t_ prefix
    When I open file "tmp/project.clj"
    And I press "C-M-f"
    And I press "C-b"
    And I press "C-j"
    And I type ":profiles {:dev {:dependencies [[midje "1.4"]]}}"
    And I press "C-x C-s"
    When I open file "tmp/test/cljr/t_core.clj"
    Then I should see:
    """
    (ns cljr.t-core
      (:require [cljr.core :refer :all]
                [midje.sweet :refer :all]))
    """
