Feature: project clean, run multiple features to clean up whole project

  Background:
    Given I switch project-clean-prompt off
    And I have a project "cljr" in "tmp"
    And I have a clojure-file "tmp/src/cljr/foo.clj"
    And I have a clojure-file "tmp/src/cljr/bar.clj"
    When I open file "tmp/src/cljr/foo.clj"
    And I clear the buffer
    And I press "M->"
    And I insert:
    """
    (ns cljr.foo
      (:require [clojure.string :as s]
                [clojure.set :as st]
                [clj-time.core :as t]))

    (defn use-time []
      (t/now)
      (st/difference #{:a :b} #{:a :c}))
    """
    And I press "C-x C-s"
    And I press "C-x k"
    And I open file "tmp/src/cljr/bar.clj"
    And I clear the buffer
    And I press "M->"
    And I insert:
    """
    (ns cljr.bar
      (:require [clojure.string :as s]
                [clojure.set :as st]
                [clj-time.core :as t]))

    (defn use-time []
      (s/split "sort and remove requires")
      (st/difference #{:a :b} #{:a :c}))
    """
    And I press "C-x C-s"
    And I press "C-x k"
    And I open file "tmp/project.clj"

  Scenario: Remove unusued requires and sort ns from in multiple files
    Given I press "C-! pc"
    When I open file "tmp/src/cljr/foo.clj"
    Then I should see:
    """
    (ns cljr.foo
      (:require [clj-time.core :as t]
                [clojure.set :as st]))

    (defn use-time []
      (t/now)
      (st/difference #{:a :b} #{:a :c}))
    """
    When I open file "tmp/src/cljr/bar.clj"
    Then I should see:
    """
    (ns cljr.bar
      (:require [clojure.set :as st]
                [clojure.string :as s]))

    (defn use-time []
      (s/split "sort and remove requires")
      (st/difference #{:a :b} #{:a :c}))
    """