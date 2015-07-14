Feature: resolve-missing; nrepl middleware response mocked

  Background:
    And I have a project "example" in "tmp"
    And I have a clojure-file "tmp/src/example/core.clj"
    When I open file "tmp/src/example/core.clj"
    And I clear the buffer
    And I press "M->"
    And I insert:
    """
    (ns example.core)
    """

  Scenario: Import java.util.Date
    Given I call the add-missing-libspec callback directly with mock data to import java.util.Date
    Then I should see:
    """
    (ns example.core
      (:import java.util.Date))
    """

  Scenario: Import clojure.string no prefix
    Given I call the add-missing-libspec callback directly with mock data to refer split
    Then I should see:
    """
    (ns example.core
      (:require [clojure.string :refer [split]]))
    """

  Scenario: Import clojure.string as str
    Given I call the add-missing-libspec callback directly with mock data to alias clojure.string
    Then I should see:
    """
    (ns example.core
      (:require [clojure.string :as str]))
    """

  Scenario: Require defrecord
    Given I call the add-missing-libspec callback directly with mock data to require WebrequestHandler
    Then I should see:
    """
    (ns example.core
      (:require modular.ring)
      (:import modular.ring.WebrequestHandler)
    """
