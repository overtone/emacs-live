Feature: Add to namespace

  Background:
    Given I have a project "cljr" in "tmp"
    And I have a clojure-file "tmp/src/cljr/core.clj"
    And I open file "tmp/src/cljr/core.clj"
    And I press "M-<"

  Scenario: Add require to namespace
    And I press "C-! ar"
    And I press "TAB"
    And I press "TAB"
    And I type "clojure.string"
    And I press "TAB"
    And I type "s"
    And I press "TAB"
    And I press "C-! ar"
    And I type "[clj-time.core :refer :all]"
    And I press "TAB"
    And cljr--clean-ns sorts stuff
    Then I should see:
    """
    (ns cljr.core
      (:require [clj-time.core :refer :all]
                [clojure.string :as s]))
    """

  Scenario: Add require to cljs part of cljc namespace
    When I clear the buffer
    And I insert:
    """
    (ns cljr.core
      #?@(:clj
          [(:require
            [clj-time.core :refer :all]
            [clojure.string :as s])]
          :cljs
          [(:require
            [cljs-time.core :refer :all])]))
    """
    And I press "C-u C-! ar"
    And I press "TAB"
    And I press "TAB"
    And I type "cljs.string"
    And I press "TAB"
    And I type "s"
    Then I should see:
    """
    (ns cljr.core
      #?@(:clj
          [(:require
            [clj-time.core :refer :all]
            [clojure.string :as s])]
          :cljs
          [(:require
            [cljs-time.core :refer :all]
            [cljs.string :as s])]))
    """

  Scenario: Add use to namespace
    When I press "C-! au"
    And I type "clj-time.core"
    And I press "TAB"
    And I press ":all"
    And I press "C-! au"
    And I type "clojure.string"
    And I press "TAB"
    And I press "TAB"
    And I type "join"
    Then I should see:
    """
    (ns cljr.core
      (:require [clj-time.core :refer :all]
                [clojure.string :refer [join]]))
    """

  Scenario: Add use to cljs part of cljc namespace
    When I clear the buffer
    And I insert:
    """
    (ns cljr.core
      #?@(:clj
          [(:require
            [clj-time.core :refer :all]
            [clojure.string :as s])]
          :cljs
          [(:require
            [cljs.string :refer :all])]))
    """
    When I press "C-u C-! au"
    And I type "cljs-time.core"
    And I press "TAB"
    And I press ":all"
    Then I should see:
    """
    (ns cljr.core
      #?@(:clj
          [(:require
            [clj-time.core :refer :all]
            [clojure.string :as s])]
          :cljs
          [(:require
            [cljs.string :refer :all]
            [cljs-time.core :refer :all])]))
    """

  Scenario: Add import to namespace
    When I press "C-! ai"
    And I type "java.io.File"
    And I press "TAB"
    And I press "C-! ai"
    And I type "[java.util Date]"
    Then I should see:
    """
    (ns cljr.core
      (:import java.io.File
               [java.util Date]))
    """

  Scenario: Add import to cljs part of cljc namespace
    When I clear the buffer
    And I insert:
    """
    (ns cljr.core
      #?@(:clj
          [(:require
            [clj-time.core :refer :all]
            [clojure.string :as s])
           (:import java.util.Date)]
          :cljs
          [(:require
            [cljs.string :refer :all])]))
    """
    When I press "C-u C-! ai"
    And I type "goog.string"
    And I press "TAB"
    Then I should see:
    """
    (ns cljr.core
      #?@(:clj
          [(:require
            [clj-time.core :refer :all]
            [clojure.string :as s])
           (:import java.util.Date)]
          :cljs
          [(:require
            [cljs.string :refer :all])
           (:import goog.string)]))
    """

  Scenario: Add require macros to namespace
    When I press "C-! rm"
    And I press "TAB"
    And I press "TAB"
    And I type "clojure.test"
    And I press "TAB"
    And I type "t"
    And I press "TAB"
    Then I should see:
    """
    (ns cljr.core
      (:require-macros [clojure.test :as t]))
    """
