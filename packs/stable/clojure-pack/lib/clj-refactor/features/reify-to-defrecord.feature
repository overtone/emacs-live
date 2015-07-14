Feature: Turn a reify call into a call to the constructor of a newly created defrecord

  Background:
    Given I have a project "cljr" in "tmp"
    And I have a clojure-file "tmp/src/cljr/core.clj"
    And I open file "tmp/src/cljr/core.clj"
    And I clear the buffer

  Scenario: Replace a call to reify with a call to defrecord constructor
    When I insert:
    """
    (defn create-runner []
      (reify Runnable
        (run [this] :running)))
    """
    And I place the cursor before "this"
    And I start an action chain
    And I press "M-x"
    And I type "cljr-reify-to-defrecord"
    And I press "RET"
    And I type "Runner"
    And I execute the action chain
    Then I should see:
    """
    (defrecord Runner []
      Runnable
      (run [this] :running))

    (defn create-runner []
      (Runner.))
    """

  Background:
    Given I have a project "cljr" in "tmp"
    And I have a clojure-file "tmp/src/cljr/core.clj"
    And I open file "tmp/src/cljr/core.clj"
    And I clear the buffer

  Scenario: Replace a call to reify with a call to defrecord constructor in nested context
    When I insert:
    """
    (.listFiles (java.io.File. ".")
                (reify
                  java.io.FileFilter
                  (accept [this f]
                    (.isDirectory f))))
    """
    And I place the cursor before "this"
    And I start an action chain
    And I press "M-x"
    And I type "cljr-reify-to-defrecord"
    And I press "RET"
    And I type "FileFilteringFoo"
    And I execute the action chain
    Then I should see:
    """
    (defrecord FileFilteringFoo []
      java.io.FileFilter
      (accept [this f]
        (.isDirectory f)))

    (.listFiles (java.io.File. ".")
                (FileFilteringFoo.))
    """

  Background:
    Given I have a project "cljr" in "tmp"
    And I have a clojure-file "tmp/src/cljr/core.clj"
    And I open file "tmp/src/cljr/core.clj"
    And I clear the buffer

  Scenario: Replace a call to reify with a call to defrecord constructor with multiple interfaces
    When I insert:
    """
    (.listFiles (java.io.File. ".")
                (reify
                  java.io.FileFilter
                  (accept [this f]
                    (.isDirectory f))
                  java.lang.Object
                  (toString [this] (println "'sup?"))))))
    """
    And I place the cursor before "this"
    And I start an action chain
    And I press "M-x"
    And I type "cljr-reify-to-defrecord"
    And I press "RET"
    And I type "FileFilteringFoo"
    And I execute the action chain
    Then I should see:
    """
    (defrecord FileFilteringFoo []
      java.io.FileFilter
      (accept [this f]
        (.isDirectory f))
      java.lang.Object
      (toString [this] (println "'sup?")))

    (.listFiles (java.io.File. ".")
                (FileFilteringFoo.))
    """
