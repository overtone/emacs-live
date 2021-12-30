Feature: Add or update dependency in leiningen project

  Background:
    Given I have a leiningen project with dependencies "cljr" in "tmp"
    And I open file "tmp/project.clj"

  Scenario: New dependency is added
    When I add dependency artifact "org.clojure/tools.namespace" with version "0.3.0-alpha4"
    Then I should see:
    """
    (defproject cljr "0.1.0-SNAPSHOT"
      :dependencies [[org.clojure/clojure "1.9.0"]
                     [org.clojure/tools.namespace "0.3.0-alpha4"]])
    """

  Scenario: Existing dependency is updated
    When I locate dependency artifact "org.clojure/clojure"
    And I update artifact version to "1.9.1"
    Then I should see:
    """
    (defproject cljr "0.1.0-SNAPSHOT"
      :dependencies [[org.clojure/clojure "1.9.1"]])
    """
