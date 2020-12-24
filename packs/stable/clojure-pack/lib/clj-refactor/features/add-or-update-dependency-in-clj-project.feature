Feature: Add or update dependency in clj project

  Background:
    Given I have a clj project with dependencies "cljr" in "tmp"
    And I open file "tmp/deps.edn"

  Scenario: New dependency is added
    When I add dependency artifact "org.clojure/tools.namespace" with version "0.3.0-alpha4"
    Then I should see:
    """
    {:paths ["src" "resources"]
     :deps {org.clojure/clojure {:mvn/version "1.9.0"}
            org.clojure/tools.namespace {:mvn/version "0.3.0-alpha4"}}}
    """

  Scenario: New dependency without namespace is added
    When I add dependency artifact "hiccup" with version "2.0.0-alpha2"
    Then I should see:
    """
    {:paths ["src" "resources"]
     :deps {org.clojure/clojure {:mvn/version "1.9.0"}
            hiccup/hiccup {:mvn/version "2.0.0-alpha2"}}}
    """

  Scenario: Existing dependency is updated
    When I locate dependency artifact "org.clojure/clojure"
    And I update artifact version to "1.9.1"
    Then I should see:
    """
    {:paths ["src" "resources"]
     :deps {org.clojure/clojure {:mvn/version "1.9.1"}}}
    """
