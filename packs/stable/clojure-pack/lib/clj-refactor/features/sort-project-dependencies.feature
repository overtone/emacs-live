Feature: Sort project dependencies

  Background:
    Given I have a project "cljr" in "tmp"
    And I open file "tmp/project.clj"
    And I clear the buffer

  Scenario: All dependency vectors are sorted
    When I insert:
    """
    (defproject refactor-nrepl "0.1.0-SNAPSHOT"
      :description "nREPL middleware to support editor agnostic refactoring"
      :url "http://github.com/clojure-emacs/refactor-nrepl"
      :dependencies [[org.clojure/clojure "1.5.1"]
                     [org.clojure/tools.nrepl "0.2.3"]
                     [org.clojure/tools.analyzer "0.5.0"]
                     [org.clojure/tools.analyzer.jvm "0.5.0"]
                     [org.clojure/tools.namespace "0.2.5"]
                     [org.clojure/tools.reader "0.8.5"]
                     [clj-http "0.9.2"]]
      :profiles {:test {:dependencies [[print-foo "0.5.3"]]}
                 :1.5 {:dependencies [[org.clojure/tools.analyzer.jvm "0.5.0"]
                                      [org.clojure/clojure "1.5.1"]
                                      [org.clojure/tools.namespace "0.2.5"]
                                      [org.clojure/tools.analyzer "0.5.0"]]}
                 :1.6 {:dependencies [[org.clojure/clojure "1.6.0"]]}
                 :1.7 {:dependencies [[org.clojure/clojure "1.7.0-master-SNAPSHOT"]]}
                 :dev {:plugins [[jonase/eastwood "0.1.4"]]
                       :repositories [["snapshots" "http://oss.sonatype.org/content/repositories/snapshots"]]}})
    """
    And I press "C-! sp"
    Then I should see:
    """
    (defproject refactor-nrepl "0.1.0-SNAPSHOT"
      :description "nREPL middleware to support editor agnostic refactoring"
      :url "http://github.com/clojure-emacs/refactor-nrepl"
      :dependencies [[clj-http "0.9.2"]
                     [org.clojure/clojure "1.5.1"]
                     [org.clojure/tools.analyzer "0.5.0"]
                     [org.clojure/tools.analyzer.jvm "0.5.0"]
                     [org.clojure/tools.namespace "0.2.5"]
                     [org.clojure/tools.nrepl "0.2.3"]
                     [org.clojure/tools.reader "0.8.5"]]
      :profiles {:test {:dependencies [[print-foo "0.5.3"]]}
                 :1.5 {:dependencies [[org.clojure/clojure "1.5.1"]
                                      [org.clojure/tools.analyzer "0.5.0"]
                                      [org.clojure/tools.analyzer.jvm "0.5.0"]
                                      [org.clojure/tools.namespace "0.2.5"]]}
                 :1.6 {:dependencies [[org.clojure/clojure "1.6.0"]]}
                 :1.7 {:dependencies [[org.clojure/clojure "1.7.0-master-SNAPSHOT"]]}
                 :dev {:plugins [[jonase/eastwood "0.1.4"]]
                       :repositories [["snapshots" "http://oss.sonatype.org/content/repositories/snapshots"]]}})
    """
