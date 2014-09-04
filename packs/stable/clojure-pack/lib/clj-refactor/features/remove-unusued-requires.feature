Feature: remove unused require

  Background:
    Given I have a project "cljr" in "tmp"
    And I have a clojure-file "tmp/src/cljr/core.clj"
    And I open file "tmp/src/cljr/core.clj"
    And I press "M-<"
    And I switch auto-sort off

  Scenario: Removes not used with :as
    When I insert:
    """
    (ns cljr.core
      (:require [clojure.string :as s]
                [clojure.set :as st]
                [clj-time.core :as t]))

    (defn use-time []
      (t/now)
      (st/difference #{:a :b} #{:a :c}))
    """
    And I place the cursor before "now"
    And I press "C-! rr"
    Then I should see:
    """
    (ns cljr.core
      (:require [clojure.set :as st]
                [clj-time.core :as t]))

    (defn use-time []
      (t/now)
      (st/difference #{:a :b} #{:a :c}))
    """

  Scenario: Removes not used with :as with ( brackets
    When I insert:
    """
    (ns cljr.core
      (:require (clojure.string :as s)
                (clojure.set :as st)
                [clj-time.core :as t]))

    (defn use-time []
      (t/now)
      (st/difference #{:a :b} #{:a :c}))
    """
    And I place the cursor before "now"
    And I press "C-! rr"
    Then I should see:
    """
    (ns cljr.core
      (:require (clojure.set :as st)
                [clj-time.core :as t]))

    (defn use-time []
      (t/now)
      (st/difference #{:a :b} #{:a :c}))
    """

  Scenario: Removes not used without :as
    When I insert:
    """
    (ns cljr.core
      (:require [clojure.string :as s]
                [clojure.set :as st]
                [clojure.tools.cli]
                [clj-time.core]))

    (defn use-time []
      (clj-time.core/now)
      (st/difference #{:a :b} #{:a :c}))
    """
    And I place the cursor before "now"
    And I press "C-! rr"
    Then I should see:
    """
    (ns cljr.core
      (:require [clojure.set :as st]
                [clj-time.core]))

    (defn use-time []
      (clj-time.core/now)
      (st/difference #{:a :b} #{:a :c}))
    """

  Scenario: Removes if line commented out
    When I insert:
    """
    (ns cljr.core
      (:require [clojure.string :as s]
                [clojure.set :as st]
                [clojure.tools.cli]
                [clj-time.core]))

    (defn use-time []
      (clj-time.core/now)
      ; (s/blank? "foobar")
      ;;;; (clojure.tools.cli/flag? "f")
      (st/difference #{:a :b} #{:a :c}))
    """
    And I place the cursor before "now"
    And I press "C-! rr"
    Then I should see:
    """
    (ns cljr.core
      (:require [clojure.set :as st]
                [clj-time.core]))

    (defn use-time []
      (clj-time.core/now)
      ; (s/blank? "foobar")
      ;;;; (clojure.tools.cli/flag? "f")
      (st/difference #{:a :b} #{:a :c}))
    """

  Scenario: removes require if all elements removed
    When I insert:
    """
    (ns cljr.core
      (:require [clojure.set :as st]
                [clj-time.core]))

    (defn use-time []
      (count [:a :b :c]))
    """
    And I place the cursor before "count"
    And I press "C-! rr"
    Then I should see:
    """
    (ns cljr.core)

    (defn use-time []
      (count [:a :b :c]))
    """

  Scenario: keeps it if referenced
    When I insert:
    """
    (ns cljr.core
      (:require [clojure.string :refer [trim]]
                [clojure.set :refer [difference]]
                [clj-time.core]))

    (defn use-time []
      (clj-time.core/now)
      ;;(trim "  foobar ")
      (difference #{:a :b} #{:a :c}))
    """
    And I place the cursor before "now"
    And I press "C-! rr"
    Then I should see:
    """
    (ns cljr.core
      (:require [clojure.set :refer [difference]]
                [clj-time.core]))

    (defn use-time []
      (clj-time.core/now)
      ;;(trim "  foobar ")
      (difference #{:a :b} #{:a :c}))
    """

  Scenario: keeps it if referenced with ( brackets
    When I insert:
    """
    (ns cljr.core
      (:require [clojure.string :refer [trim]]
                [clojure.set :refer (difference)]
                [clj-time.core]))

    (defn use-time []
      (clj-time.core/now)
      ;;(trim "  foobar ")
      (difference #{:a :b} #{:a :c}))
    """
    And I place the cursor before "now"
    And I press "C-! rr"
    Then I should see:
    """
    (ns cljr.core
      (:require [clojure.set :refer [difference]]
                [clj-time.core]))

    (defn use-time []
      (clj-time.core/now)
      ;;(trim "  foobar ")
      (difference #{:a :b} #{:a :c}))
    """

  Scenario: keeps alias too if used even if there is :refer
    When I insert:
    """
    (ns cljr.core
      (:require [clojure.string :as st :refer [trim]]
                [clojure.set :as s :refer [difference]]
                [clj-time.core :as t :refer [ago]]))

    (defn use-time []
      (t/now)
      (trim "  foobar ")
      (s/union #{:d} (difference #{:a :b} #{:a :c})))
    """
    And I place the cursor before "now"
    And I press "C-! rr"
    Then I should see:
    """
    (ns cljr.core
      (:require [clojure.string :refer [trim]]
                [clojure.set :as s :refer [difference]]
                [clj-time.core :as t]))

    (defn use-time []
      (t/now)
      (trim "  foobar ")
      (s/union #{:d} (difference #{:a :b} #{:a :c})))
    """

  Scenario: keeps it if referenced multiple
    When I insert:
    """
    (ns cljr.core
      (:require [clojure.string :refer [trim blank? reverse]]
                [clojure.set :refer [difference]]
                [clj-time.core]))

    (defn use-time []
      (clj-time.core/now)
      (reverse "baz")
      (trim "  foobar ")
      (difference #{:a :b} #{:a :c}))
    """
    And I place the cursor before "now"
    And I press "C-! rr"
    Then I should see:
    """
    (ns cljr.core
      (:require [clojure.string :refer [trim reverse]]
                [clojure.set :refer [difference]]
                [clj-time.core]))

    (defn use-time []
      (clj-time.core/now)
      (reverse "baz")
      (trim "  foobar ")
      (difference #{:a :b} #{:a :c}))
    """

  Scenario: :refer combined with :as
    When I insert:
    """
    (ns cljr.core
      (:require [clojure.string :as st :refer [trim split reverse]]
                [clojure.set :refer [difference]]
                [clj-time.core]))

    (defn use-time []
      (clj-time.core/now)
      (st/split "foo bar" #" ")
      (trim "  foobar ")
      (difference #{:a :b} #{:a :c}))
    """
    And I place the cursor before "now"
    And I press "C-! rr"
    Then I should see:
    """
    (ns cljr.core
      (:require [clojure.string :as st :refer [trim split]]
                [clojure.set :refer [difference]]
                [clj-time.core]))

    (defn use-time []
      (clj-time.core/now)
      (st/split "foo bar" #" ")
      (trim "  foobar ")
      (difference #{:a :b} #{:a :c}))
    """

  Scenario: prefix list -- simple case
    When I insert:
    """
    (ns cljr.core
      (:require [clj-time.core]
                [clojure string walk set]))

    (defn use-time []
      (clj-time.core/now)
      (clojure.string/split "foo bar" #" ")
      (clojure.set/difference #{:a :b} #{:a :c}))
    """
    And I place the cursor before "now"
    And I press "C-! rr"
    Then I should see:
    """
    (ns cljr.core
      (:require [clj-time.core]
                [clojure string set]))

    (defn use-time []
      (clj-time.core/now)
      (clojure.string/split "foo bar" #" ")
      (clojure.set/difference #{:a :b} #{:a :c}))
    """

  Scenario: prefix list with as
    When I insert:
    """
    (ns cljr.core
      (:require [clj-time.core]
                [clojure string
                 [set :as st]
                 walk]))

    (defn use-time []
      (clj-time.core/now)
      (clojure.string/split "foo bar" #" ")
      (st/difference #{:a :b} #{:a :c}))
    """
    And I place the cursor before "now"
    And I press "C-! rr"
    Then I should see:
    """
    (ns cljr.core
      (:require [clj-time.core]
                [clojure string 
                 [set :as st]]))

    (defn use-time []
      (clj-time.core/now)
      (clojure.string/split "foo bar" #" ")
      (st/difference #{:a :b} #{:a :c}))
    """

  Scenario: prefix list with refer
    When I insert:
    """
    (ns cljr.core
      (:require [clj-time.core]
                [clojure string walk
                 [set :refer [difference union]]]))

    (defn use-time []
      (clj-time.core/now)
      (clojure.string/split "foo bar" #" ")
      (difference #{:a :b} #{:a :c}))
    """
    And I place the cursor before "now"
    And I press "C-! rr"
    Then I should see:
    """
    (ns cljr.core
      (:require [clj-time.core]
                [clojure string 
                 [set :refer [difference]]]))

    (defn use-time []
      (clj-time.core/now)
      (clojure.string/split "foo bar" #" ")
      (difference #{:a :b} #{:a :c}))
    """

  Scenario: prefix list with refer with ( brackets
    When I insert:
    """
    (ns cljr.core
      (:require [clj-time.core]
                (clojure string walk
                 [set :refer [difference union]])))

    (defn use-time []
      (clj-time.core/now)
      (clojure.string/split "foo bar" #" ")
      (difference #{:a :b} #{:a :c}))
    """
    And I place the cursor before "now"
    And I press "C-! rr"
    Then I should see:
    """
    (ns cljr.core
      (:require [clj-time.core]
                [clojure string 
                 [set :refer [difference]]]))

    (defn use-time []
      (clj-time.core/now)
      (clojure.string/split "foo bar" #" ")
      (difference #{:a :b} #{:a :c}))
    """

  Scenario: keeps :refer :all
    When I insert:
    """
    (ns cljr.core
      (:require [clj-time.core :refer :all]
                [clojure string walk
                 [set :refer :all]]))

    (defn use-time []
      (now)
      (clojure.string/split "foo bar" #" ")
      (difference #{:a :b} #{:a :c}))
    """
    And I place the cursor before "now"
    And I press "C-! rr"
    Then I should see:
    """
    (ns cljr.core
      (:require [clj-time.core :refer :all]
                [clojure string 
                 [set :refer :all]]))

    (defn use-time []
      (now)
      (clojure.string/split "foo bar" #" ")
      (difference #{:a :b} #{:a :c}))
    """

  Scenario: keeps alias even if there is :refer :all
    When I insert:
    """
    (ns cljr.core
      (:require [clj-time.core :as tc :refer :all]
                [clojure string walk
                 [set :as s :refer :all]]))

    (defn use-time []
      (now)
      (clojure.string/split "foo bar" #" ")
      (difference #{:a :b} #{:a :c}))
    """
    And I place the cursor before "now"
    And I press "C-! rr"
    Then I should see:
    """
    (ns cljr.core
      (:require [clj-time.core :as tc :refer :all]
                [clojure string 
                 [set :as s :refer :all]]))

    (defn use-time []
      (now)
      (clojure.string/split "foo bar" #" ")
      (difference #{:a :b} #{:a :c}))
    """

  Scenario: simple require after prefix list
    When I insert:
    """
    (ns cljr.core
      (:require [clojure string walk
                 [set :as st]]
                [clj-time.core]))

    (defn use-time []
      (clj-time.core/now)
      (clojure.string/split "foo bar" #" ")
      (st/difference #{:a :b} #{:a :c}))
    """
    And I place the cursor before "now"
    And I press "C-! rr"
    Then I should see:
    """
    (ns cljr.core
      (:require [clojure string 
                 [set :as st]]
                [clj-time.core]))

    (defn use-time []
      (clj-time.core/now)
      (clojure.string/split "foo bar" #" ")
      (st/difference #{:a :b} #{:a :c}))
    """

  Scenario: refer with as, refer first
    When I insert:
    """
    (ns bug
      (:require [clojure.string :refer [join trim] :as str]))

    (join "+" (map str/upper-case ["a" "b" "c"]))
    """
    And I place the cursor before "join"
    And I press "C-! rr"
    Then I should see:
    """
    (ns bug
      (:require [clojure.string :refer [join] :as str]))

    (join "+" (map str/upper-case ["a" "b" "c"]))
    """

  Scenario: refer with as, refer first, as not used
    When I insert:
    """
    (ns bug
      (:require [clojure.string :refer [join trim] :as str]))

    (join "+" ["a" "b" "c"])
    """
    And I place the cursor before "join"
    And I press "C-! rr"
    Then I should see:
    """
    (ns bug
      (:require [clojure.string :refer [join]]))

    (join "+" ["a" "b" "c"])
    """

  Scenario: refer with as, refer first, nothing referred
    When I insert:
    """
    (ns bug
      (:require [clojure.string :refer [replace trim] :as str]))

    (str/join "+" ["a" "b" "c"])
    """
    And I place the cursor before "join"
    And I press "C-! rr"
    Then I should see:
    """
    (ns bug
      (:require [clojure.string :as str]))

    (str/join "+" ["a" "b" "c"])
    """

  Scenario: Also sorts ns if auto-sort is on
    When I insert:
    """
    (ns cljr.core
      (:require [clojure.string :as s]
                [clojure.set :as st]
                [clj-time.core :as t]))

    (defn use-time []
      (t/now)
      (st/difference #{:a :b} #{:a :c}))
    """
    And I place the cursor before "now"
    And I switch auto-sort on
    And I press "C-! rr"
    Then I should see:
    """
    (ns cljr.core
      (:require [clj-time.core :as t]
                [clojure.set :as st]))

    (defn use-time []
      (t/now)
      (st/difference #{:a :b} #{:a :c}))
    """
