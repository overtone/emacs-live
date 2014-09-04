Feature: Magic requires

  Background:
    Given I have a project "cljr" in "tmp"
    And I have a clojure-file "tmp/src/cljr/core.clj"
    And I open file "tmp/src/cljr/core.clj"
    And I clear the buffer

  Scenario: Require is inserted automagically
    When I insert:
    """
    (ns cljr.core)

    (set)
    """
    And I place the cursor after "set"
    And I type "/union"
    Then I should see:
    """
    (ns cljr.core
      (:require [clojure.set :as set]))

    (set/union)
    """
