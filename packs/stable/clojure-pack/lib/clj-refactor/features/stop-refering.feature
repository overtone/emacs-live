Feature: Stop referring

  Background:
    Given I have a project "cljr" in "tmp"
    And I have a clojure-file "tmp/src/cljr/core.clj"
    And I open file "tmp/src/cljr/core.clj"
    And I clear the buffer

  Scenario: Without :as
    When I insert:
    """
    (ns cljr.core
      (:require [my.lib :refer [a b]]))

    (+ (a 1) (b 2))
    (+ (A 1) (B 2))
    """
    And I place the cursor before "my.lib"
    And I press "C-! sr"
    Then I should see:
    """
    (ns cljr.core
      (:require [my.lib]))

    (+ (my.lib/a 1) (my.lib/b 2))
    (+ (A 1) (B 2))
    """

  Scenario: With :as
    When I insert:
    """
    (ns cljr.core
      (:require [my.lib :as lib :refer [a b]]))

    (+ (a 1) (b 2))
    (+ (A 1) (B 2))
    """
    And I place the cursor before "my.lib"
    And I press "C-! sr"
    Then I should see:
    """
    (ns cljr.core
      (:require [my.lib :as lib]))

    (+ (lib/a 1) (lib/b 2))
    (+ (A 1) (B 2))
    """

  Scenario: With newline after :require, #208
    When I insert:
    """
    (ns cljr.core
      (:require
        [my.lib :as lib :refer [a b]]))

    (+ (a 1) (b 2))
    (+ (A 1) (B 2))
    """
    And I place the cursor before "my.lib"
    And I press "C-! sr"
    Then I should see:
    """
    (ns cljr.core
      (:require
        [my.lib :as lib]))

    (+ (lib/a 1) (lib/b 2))
    (+ (A 1) (B 2))
    """

  Scenario: With newline after and before :refer, #339
    When I insert:
    """
    (ns cljr.core
      (:require [my.lib :as lib
                 :refer
                 [a b]]))

    (+ (a 1) (b 2))
    (+ (A 1) (B 2))
    """
    And I place the cursor before "my.lib"
    And I press "C-! sr"
    Then I should see:
    """
    (ns cljr.core
      (:require [my.lib :as lib]))

    (+ (lib/a 1) (lib/b 2))
    (+ (A 1) (B 2))
    """
