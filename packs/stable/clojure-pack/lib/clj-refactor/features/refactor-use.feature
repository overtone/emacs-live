Feature: Remove Use from ns form

  Background:
    Given I have a project "cljr" in "tmp"
    Given I switch auto-sort on
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

  Scenario: Replacing :use with :require :refer :all
    When I insert:
    """
    (ns payments.billing.accounts
      (:require [clojure.data.json :as json]
                [clojure.tools.logging :refer [fatal]]
                [org.httpkit.client :as http]
                [payments.billing.endpoints :as endpoints]
                [payments.configuration.properties :refer [config]]
                [payments.configuration.schematables :as tables]
                [payments.gateways :as gateways])
      (:use [clojurewerkz.cassaforte.cql]
            [clojurewerkz.cassaforte.query]
            [payments.exceptions.http :only [unauthorised not-found
                                             forbidden internal-server-error]]
            [payments.utils :only [force-uuid]]))
   """
    And I press "C-! ru"
    Then I should see:
    """
    (ns payments.billing.accounts
      (:require [clojure.data.json :as json]
                [clojure.tools.logging :refer [fatal]]
                [clojurewerkz.cassaforte.cql :refer :all]
                [clojurewerkz.cassaforte.query :refer :all]
                [org.httpkit.client :as http]
                [payments.billing.endpoints :as endpoints]
                [payments.configuration.properties :refer [config]]
                [payments.configuration.schematables :as tables]
                [payments.exceptions.http :refer [unauthorised not-found
                                                  forbidden internal-server-error]]
                [payments.gateways :as gateways]
                [payments.utils :refer [force-uuid]]))
    """

  Scenario: Replacing :use with :require :refer :all
    When I insert:
    """
    (ns payments.core
      (:require [clojure.tools.cli :refer [parse-opts]]
                [clojure.tools.nrepl.server :as nrepl]
                [compojure.route :as route]
                [payments.configuration.properties
                 :refer [config merge-config-from-file! log-configuration]]
                [payments.db :as db]
                [payments.version :as version]
                [ring.adapter.jetty :as jetty])
      (:use [clojure.java.io]
            [clojure.tools.logging]
            [compojure.core :only [defroutes routes GET POST]]
            [metrics.core :only [report-to-console]]
            [metrics.ring.instrument :only [instrument]]
            [payments.configuration.schematables :only [set-keyspace!]]
            [payments.exceptions.http :only [all-http-exceptions-middleware]]
            [payments.http.account :only [create-account
                                          retrieve-account]]
            [payments.http.transaction :only [payment-request]]
            [payments.sso.middleware :only [ensure-auth-cookie-middleware
                                            refresh-cookie-middleware]]
            [ring.middleware.cookies :only [wrap-cookies]]
            [ring.middleware.json]
            [ring.middleware.stacktrace]
            [ring.util.response])
      (:import com.yammer.metrics.reporting.GraphiteReporter)
      (:import [org.apache.commons.daemon DaemonContext Daemon])
      (:gen-class
       :implements [org.apache.commons.daemon.Daemon]))
    """
    And I press "C-! ru"
    Then I should see:
    """
    (ns payments.core
      (:require [clojure.java.io :refer :all]
                [clojure.tools.cli :refer [parse-opts]]
                [clojure.tools.logging :refer :all]
                [clojure.tools.nrepl.server :as nrepl]
                [compojure.core :refer [defroutes routes GET POST]]
                [compojure.route :as route]
                [metrics.core :refer [report-to-console]]
                [metrics.ring.instrument :refer [instrument]]
                [payments.configuration.properties
                 :refer [config merge-config-from-file! log-configuration]]
                [payments.configuration.schematables :refer [set-keyspace!]]
                [payments.db :as db]
                [payments.exceptions.http :refer [all-http-exceptions-middleware]]
                [payments.http.account :refer [create-account
                                               retrieve-account]]
                [payments.http.transaction :refer [payment-request]]
                [payments.sso.middleware :refer [ensure-auth-cookie-middleware
                                                 refresh-cookie-middleware]]
                [payments.version :as version]
                [ring.adapter.jetty :as jetty]
                [ring.middleware.cookies :refer [wrap-cookies]]
                [ring.middleware.json :refer :all]
                [ring.middleware.stacktrace :refer :all]
                [ring.util.response :refer :all])
      (:import com.yammer.metrics.reporting.GraphiteReporter)
      (:import [org.apache.commons.daemon DaemonContext Daemon])
      (:gen-class
       :implements [org.apache.commons.daemon.Daemon]))
    """

  Scenario: Replacing :use with :require :refer :all, issue #52
    When I insert:
    """
    (ns furtive.test.stress.riemann.client-tests
      (:use clojure.test)
      (:require [furtive.config :as config]
                [furtive.dev :refer :all]
                [furtive.riemann.client :as client]
                [furtive.riemann.driver :as driver]))
    """
    And I press "C-! ru"
    Then I should see:
    """
    (ns furtive.test.stress.riemann.client-tests
      (:require [clojure.test :refer :all]
                [furtive.config :as config]
                [furtive.dev :refer :all]
                [furtive.riemann.client :as client]
                [furtive.riemann.driver :as driver]))
    """

  Scenario: Replacing :use with :require :refer :all, issue #50
    When I insert:
    """
    (ns payments.authnet.transactions
      (:require [payments.authnet.client :refer [create-client]]
                [payments.utils :refer [uuid]])
      (:use [clojurewerkz.cassaforte.cql :as cql]
            [clojurewerkz.cassaforte.query]))
    """
    And I press "C-! ru"
    Then I should see:
    """
    (ns payments.authnet.transactions
      (:require [clojurewerkz.cassaforte.cql :refer :all]
                [clojurewerkz.cassaforte.query :refer :all]
                [payments.authnet.client :refer [create-client]]
                [payments.utils :refer [uuid]]))
    """