Feature: Let bindings

  Background:
    Given I have a project "cljr" in "tmp"
    And I have a clojure-file "tmp/src/cljr/core.clj"
    And I open file "tmp/src/cljr/core.clj"
    And I clear the buffer

  Scenario: Introduce let
    When I insert:
    """
    {:status 200
     :body (find-body abc)}
    """
    And I place the cursor before "(find-body abc)"
    And I press "C-! il"
    And I type "body"
    Then I should see:
    """
    {:status 200
     :body (let [body (find-body abc)]
             body)}
    """

  Scenario: Expand let, part 1
    When I insert:
    """
    (defn handle-request
      {:status 200
       :body (let [body (find-body abc)]
               body)})
    """
    And I place the cursor before "(find-body abc)"
    And I press "C-! el"
    Then I should see:
    """
    (defn handle-request
      (let [body (find-body abc)]
        {:status 200
         :body body}))
    """

  Scenario: Expand let, part 2
    When I insert:
    """
    (defn handle-request
      (let [body (find-body abc)]
        {:status 200
         :body body}))
    """
    And I place the cursor before "(find-body abc)"
    And I press "C-! el"
    Then I should see:
    """
    (let [body (find-body abc)]
      (defn handle-request
        {:status 200
         :body body}))
    """

  Scenario: Expand let, multiple occurance
    When I insert:
    """
    (defn handle-request
      (when (find-body abc)
        (println "body: " (find-body	abc) ", params: " ["param1"    "param2"] ", status: " 200)
        {:status 200
         :foobar 200000
         :baz 049403200
         :body (let [body (find-body abc)
                     params ["param1" "param2"]
                     status 200]
                 body)
         :params ["param1"
                  "param2"]}))

    (defn handle-request-other
       {:status 200})
    """
    And I place the cursor before "[body (find-body abc)"
    And I press "C-! el"
    And I press "C-! el"
    Then I should see:
    """
    (defn handle-request
      (let [body (find-body abc)
            params ["param1" "param2"]
            status 200]
        (when body
          (println "body: " body ", params: " params ", status: " status)
          {:status status
           :foobar 200000
           :baz 049403200
           :body body
           :params params})))

    (defn handle-request-other
       {:status 200})
    """

  Scenario: Move s-expression to let
    When I insert:
    """
    (defn handle-request
      (let [body (find-body abc)]
        {:status (or status 500)
         :body body}))
    """
    And I place the cursor before "(or status 500)"
    And I press "C-! ml"
    And I type "status"
    Then I should see:
    """
    (defn handle-request
      (let [body (find-body abc)
            status (or status 500)]
        {:status status
         :body body}))
    """

  Scenario: Move constant to when-let
    When I insert:
    """
    (defn handle-request
      (when-let[body (find-body abc)]
        {:status 42
         :body body}))
    """
    And I place the cursor before "42"
    And I press "C-! ml"
    And I type "status"
    Then I should see:
    """
    (defn handle-request
      (when-let[body (find-body abc)
                status 42]
        {:status status
         :body body}))
    """

  Scenario: Move to empty if-let
    When I insert:
    """
    (defn handle-request
      (if-let []
        {:status (or status 500)
         :body body}))
    """
    And I place the cursor before "(or status 500)"
    And I press "C-! ml"
    And I type "status"
    Then I should see:
    """
    (defn handle-request
      (if-let [status (or status 500)]
        {:status status
         :body body}))
    """

  Scenario: Introduce let if let does not exist
    When I insert:
    """
    (defn handle-request
      {:status (or status 500)
       :body body})
    """
    And I place the cursor before "(or status 500)"
    And I press "C-! ml"
    And I type "status"
    Then I should see:
    """
    (defn handle-request
      {:status (let [status (or status 500)]
                 status)
       :body body})
    """
