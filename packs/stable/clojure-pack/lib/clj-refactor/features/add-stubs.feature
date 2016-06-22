Feature: Add stub implementations for java.util.List

  Background:
    Given I have a project "cljr" in "tmp"
    And I have a clojure-file "tmp/src/cljr/core.clj"
    And I open file "tmp/src/cljr/core.clj"
    And I clear the buffer

  Scenario: Stub implementations for java.util.List in defrecord
    When I insert:
    """
    (ns cljr.core)

    (defrecord MyRecord []
      java.util.List)
    """
    And I place the cursor before "List"
    And I call the add-stubs function directly with mock data for java.util.List from the middleware
    Then I should see:
    """
    (defrecord MyRecord []
      java.util.List
      (remove [^int arg])
      (add [^int arg0 ^Object arg1])
      (replaceAll [^java.util.function.UnaryOperator arg])
      (containsAll [^java.util.Collection arg])
      (removeAll [^java.util.Collection arg])
      (listIterator [])
      (subList [^int arg0 ^int arg1])
      (iterator [])
      (lastIndexOf [^Object arg])
      (listIterator [^int arg])
      (addAll [^int arg0 ^java.util.Collection arg1])
      (add [^Object arg])
      (get [^int arg])
      (toArray [])
      (clear [])
      (set [^int arg0 ^Object arg1])
      (retainAll [^java.util.Collection arg])
      (isEmpty [])
      (addAll [^java.util.Collection arg])
      (spliterator [])
      (indexOf [^Object arg])
      (toArray [^Object... arg])
      (sort [^java.util.Comparator arg])
      (size [])
      (equals [^Object arg])
      (remove [^Object arg])
      (hashCode [])
      (contains [^Object arg]))
    """

  Background:
    Given I have a project "cljr" in "tmp"
    And I have a clojure-file "tmp/src/cljr/core.clj"
    And I open file "tmp/src/cljr/core.clj"
    And I clear the buffer

  Scenario: Stub implementations for a protocol in an aliased namespace in deftype
    When I insert:
    """
    (ns cljr.core
      (:require [clojure.repl :as repl]
                [clojure.reflect :as r])

    (defrecord MyRecord []
      r/TypeReference)
    """
    And I place the cursor before "TypeReference"
    And I call the add-stubs function directly with mock data for clojure.reflect from the middleware
    Then I should see:
    """
    (defrecord MyRecord []
      r/TypeReference
      (typename [this]))
    """
