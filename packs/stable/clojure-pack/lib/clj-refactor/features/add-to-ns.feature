Feature: Add to namespace

  Background:
    Given I have a project "cljr" in "tmp"
    And I have a clojure-file "tmp/src/cljr/core.clj"
    And I open file "tmp/src/cljr/core.clj"
    And I press "M-<"

  Scenario: Add require to namespace
    And I press "C-! ar"
    And I press "TAB"
    And I press "TAB"
    And I type "clojure.strings"
    And I press "TAB"
    And I type "s"
    And I press "TAB"
    And I press "C-! ar"
    And I type "[clj-time.core :refer :all]"
    And I press "TAB"
    Then I should see:
    """
    (ns cljr.core
      (:require [clojure.strings :as s]
                [clj-time.core :refer :all]))
    """

  Scenario: Add use to namespace
    When I press "C-! au"
    And I type "clj-time.core"
    And I press "TAB"
    And I press ":all"
    And I press "C-! au"
    And I type "clojure.strings"
    And I press "TAB"
    And I press "TAB"
    And I type "join"
    Then I should see:
    """
    (ns cljr.core
      (:require [clj-time.core :refer :all]
                [clojure.strings :refer [join]]))
    """

  Scenario: Add import to namespace
    When I press "C-! ai"
    And I type "java.io.File"
    And I press "TAB"
    And I press "C-! ai"
    And I type "[java.util Date]"
    Then I should see:
    """
    (ns cljr.core
      (:import java.io.File
               [java.util Date]))
    """
