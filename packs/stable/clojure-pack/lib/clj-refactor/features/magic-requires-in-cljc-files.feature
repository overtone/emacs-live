  Background:
    Given I have a project "cljr" in "tmp"
    And I have a clojure-file "tmp/src/cljr/core.cljc"
    And I open file "tmp/src/cljr/core.cljc"
    And I clear the buffer

  Scenario: Does the right thing when facing clj reader conditionals
    When I insert:
    """
    (ns cljr.core)

    #?(:clj (pprint))
    """
    And the cache of namespace aliases is populated
    And I place the cursor after "pprint"
    And I start an action chain
    And I type "/"
    And I type "pprint"
    And I execute the action chain
    Then I should see:
    """
    (ns cljr.core
      (:require [clojure.pprint :as pprint]))

    #?(:clj (pprint/pprint))
    """

  Scenario: Does the right thing when facing cljs reader conditionals
    When I insert:
    """
    (ns cljr.core)

    #?(:cljs (pprint))
    """
    And the cache of namespace aliases is populated
    And I place the cursor after "pprint"
    And I start an action chain
    And I type "/"
    And I type "pprint"
    And I execute the action chain
    Then I should see:
    """
    (ns cljr.core
      (:require [cljs.pprint :as pprint]))

    #?(:cljs (pprint/pprint))
    """

  Scenario: Prompts to resolve ambiguity
    When I insert:
    """
    (ns cljr.core)

    (pprint)
    """
    And the cache of namespace aliases is populated
    And I place the cursor after "pprint"
    And I start an action chain
    And I type "/"
    And I type "cljs"
    And I press "RET"
    And I type "pprint"
    And I execute the action chain
    Then I should see:
    """
    (ns cljr.core
      (:require [cljs.pprint :as pprint]))

    (pprint/pprint)
    """
