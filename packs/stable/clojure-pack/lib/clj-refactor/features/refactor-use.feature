Feature: Remove Use from ns form

  Background:
    Given I have a project "cljr" in "tmp"
    And I have a clojure-file "tmp/src/cljr/core.clj"
    And I open file "tmp/src/cljr/core.clj"
    And I clear the buffer

  Scenario: Replacing :use with :require :refer :all
    When I insert:
    """
    (ns ^{:meta "..."}
      cljr.foo
      "doc..."
      (:use [cljr.core]
            [cljr.page]
            [cljr.element]
            [cljr.form]
            [some.lib ns1 ns2 ns3])
      (:require [cljr.foobar :as foo])
      (:refer-clojure :exclude [this that])
      (:import [java.util.Date]))
    """
    And I press "C-! ru"
    Then I should see:
    """
    (ns ^{:meta "..."}
      cljr.foo
      "doc..."
      (:require [cljr.core :refer :all]
                [cljr.element :refer :all]
                [cljr.foobar :as foo]
                [cljr.form :refer :all]
                [cljr.page :refer :all]
                [some.lib.ns1 :refer :all]
                [some.lib.ns2 :refer :all]
                [some.lib.ns3 :refer :all])
      (:refer-clojure :exclude [this that])
      (:import [java.util.Date]))
    """

  Scenario: Replacing :use with :require :refer :all
    When I insert:
    """
    (ns payments.http.transaction
      (:require [payments.billing.accounts :as accounts]
                [payments.billing.orders :as orders])
      (:use [metrics.meters :only [defmeter mark!]]
            [payments.exceptions.http :only [unauthorised bad-request]]
            [payments.gateways :only [execute-transaction]]
            [payments.utils :only [uuid is-numeric? seq-not-nil?]]))
   """
    And I press "C-! ru"
    Then I should see:
    """
    (ns payments.http.transaction
      (:require [metrics.meters :refer [defmeter mark!]]
                [payments.billing.accounts :as accounts]
                [payments.billing.orders :as orders]
                [payments.exceptions.http :refer [unauthorised bad-request]]
                [payments.gateways :refer [execute-transaction]]
                [payments.utils :refer [uuid is-numeric? seq-not-nil?]])
    """