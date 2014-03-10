Feature: Sort ns form

  Background:
    Given I have a project "cljr" in "tmp"
    And I have a clojure-file "tmp/src/cljr/core.clj"
    And I open file "tmp/src/cljr/core.clj"
    And I press "M-<"

  Scenario: Sort ns
    When I insert:
    """
    (ns ^{:doc "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do
            eiusmod tempor incididunt ut labore (et dolore magna aliqua)."}
      furtive.runtime.session.bucket
      (:use clojure.test 
            clojure.test
            clojure.string)
      (:require [foo.bar :refer :all]
                [clj-time.core :as clj-time])
      (:import (java.security MessageDigest)
               java.util.Calendar
               [org.joda.time DateTime]
               (java.nio.charset Charset)))
    """
    And I place the cursor before "Calendar"
    And I press "C-! sn"
    Then I should see:
    """
    (ns ^{:doc "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do
            eiusmod tempor incididunt ut labore (et dolore magna aliqua)."}
      furtive.runtime.session.bucket
      (:use clojure.string
            clojure.test)
      (:require [clj-time.core :as clj-time]
                [foo.bar :refer :all])
      (:import (java.nio.charset Charset)
               (java.security MessageDigest)
               java.util.Calendar
               [org.joda.time DateTime]))
    """

  Scenario: no ns form
    When I insert:
    """
    {:config :file}
    """
    And I place the cursor before "file"
    And I press "C-! sn"
    Then I should see:
    """
    {:config :file}
    """
  Scenario: regression 1 (:use in file doesn't confuse)
    When I insert:
    """
    (ns furtive.runtime.session.bucket
      (:require [foo.bar :refer :all]
                [clj-time.core :as clj-time]))

      (defn foo [x]
        (:user-agent x))
    """
    And I place the cursor before "foo"
    And I press "C-! sn"
    Then I should see:
    """
    (ns furtive.runtime.session.bucket
      (:require [clj-time.core :as clj-time]
                [foo.bar :refer :all]))

      (defn foo [x]
        (:user-agent x))
    """
