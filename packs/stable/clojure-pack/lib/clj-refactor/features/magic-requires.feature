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

  Scenario: Require is not inserted automagically when in-ns is used
    When I insert:
    """
    (in-ns 'cljr.core)

    (set)
    """
    And I place the cursor after "set"
    And I type "/union"
    Then I should see:
    """
    (in-ns 'cljr.core)

    (set/union)
    """

  Scenario: Require is inserted automagically after getting suggestions from middleware
    When I insert:
    """
    (ns cljr.core)

    (util)
    """
    And the cache of namespace aliases is populated
    And I place the cursor after "util"
    And I start an action chain
    And I type "/"
    And I type "refactor-nrepl.util"
    And I press "RET"
    And I type "get-last-sexp"
    And I execute the action chain
    Then I should see:
    """
    (ns cljr.core
      (:require [refactor-nrepl.util :as util]))

    (util/get-last-sexp)
    """

  Scenario: Require is inserted automagically for namespaced keyword
    When I insert:
    """
    (ns cljr.core)

    (::util)
    """
    And the cache of namespace aliases is populated
    And I place the cursor after "util"
    And I start an action chain
    And I type "/"
    And I type "refactor-nrepl.util"
    And I press "RET"
    And I type "some-keyword"
    And I execute the action chain
    Then I should see:
    """
    (ns cljr.core
      (:require [refactor-nrepl.util :as util]))

    (::util/some-keyword)
    """

  Scenario: If alias exists nothing happens
    When I insert:
    """
    (ns cljr.core
      (:require [refactor-nrepl.util :as util]))

    (util)
    """
    And the cache of namespace aliases is populated
    And I place the cursor after "(util"
    And I start an action chain
    And I type "/get-last-sexp"
    And I execute the action chain
    Then I should see:
    """
    (ns cljr.core
      (:require [refactor-nrepl.util :as util]))

    (util/get-last-sexp)
    """

Scenario: Nothing happens when destructuring a map
    When I insert:
    """
    (ns cljr.core)

    (defn f [{:keys [set]}])
    """
    And the cache of namespace aliases is populated
    And I place the cursor after "set"
    And I start an action chain
    And I type "/union"
    And I execute the action chain
    Then I should see:
    """
    (ns cljr.core)

    (defn f [{:keys [set/union]}])
    """
