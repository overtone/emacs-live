Feature: Change function signature

  Background:
    And I have a project "example" in "tmp"
    And I have a clojure-file "tmp/src/example/core.clj"
    When I open file "tmp/src/example/core.clj"
    And I clear the buffer
    And I press "M->"

  Scenario: Change parameter in function definition
    When I insert:
    """
    (ns core)

    (defn tt [foo bar baz]
      (println foo bar baz))
    """
    And I call the cljr--change-function-signature function directly with mockdata to swap foo and bar in function definition
    Then I should see:
    """
    (ns core)

    (defn tt [bar foo baz]
      (println foo bar baz))
    """

  Scenario: Change parameter in function definition using schemas
    When I insert:
    """
    (ns core)

    (defn tt [foo :- s/str bar :- s/int baz :- User]
      (println foo bar baz))
    """
    And I call the cljr--change-function-signature function directly with mockdata to swap foo and bar in function definition
    Then I should see:
    """
    (ns core)

    (defn tt [bar :- s/int foo :- s/str baz :- User]
      (println foo bar baz))
    """

  Scenario: Puts the lambdalist on its own line when it's too far to the right
    When I insert:
    """
    (ns core)

    (defn this-function-has-a-super-long-name-which-warrants-putting-the-lambda-list-on-its-own-line [foo bar baz]
      (println foo bar baz))
    """
    And I call the cljr--change-function-signature function directly with mockdata to swap foo and bar in function definition
    Then I should see:
    """
    (ns core)

    (defn this-function-has-a-super-long-name-which-warrants-putting-the-lambda-list-on-its-own-line
      [bar foo baz]
      (println foo bar baz))
    """

  Scenario: Change parameter in regular call-site
    When I insert:
    """
    (ns core)

    (defn regular-call-site []
      (tt 1 2 3))
    """
    And I call the cljr--change-function-signature function directly with mockdata to swap foo and bar in a regular call-site
    Then I should see:
    """
    (ns core)

    (defn regular-call-site []
      (tt 2 1 3))
    """

  Scenario: Change parameter in higher order call-site requires manual intervention
    When I insert:
    """
    (ns core)

    (defn higher-order-call-site []
      (map tt [1] [2] [3]))
    """
    And I call the cljr--change-function-signature function directly with mockdata to swap foo and bar in a higher-order call-site
    And I switch to buffer "*cljr-manual-intervention*"
    And I go to line "3"
    Then I should see pattern ".*core.clj:4"

  Scenario: Change parameter in call-site with partial application requires manual intervention
    When I insert:
    """
    (ns core)

    (defn partial-call-site []
      (map (partial tt 1) 2 3))
    """
    And I call the cljr--change-function-signature function directly with mockdata to swap foo and bar in a partial call-site
    And I switch to buffer "*cljr-manual-intervention*"
    And I go to line "3"
    Then I should see pattern ".*core.clj:4"

  Scenario: Change parameter in call-site wirth partial application does not require manual intervention
    When I insert:
    """
    (ns core)

    (defn partial-call-site []
      (map (partial tt 1 :two) 'three))
    """
    And I call the cljr--change-function-signature function directly with mockdata to swap foo and bar in in partial application
    Then I should see:
    """
    (ns core)

    (defn partial-call-site []
      (map (partial tt :two 1) 'three))
    """

  Scenario: Change parameter in call-site with apply
    When I insert:
    """
    (ns core)

    (defn apply-call-site []
      (let [args [1]]
        (apply tt [1] [2] args)))
    """
    And I call the cljr--change-function-signature function directly with mockdata to swap foo and bar in a call-site with apply
    And I switch to buffer "core.clj"
    Then I should see:
    """
    (ns core)

    (defn apply-call-site []
      (let [args [1]]
        (apply tt [2] [1] args)))
    """

  Scenario: Change parameter in call-site with apply with changes in rest args requires manual intervention
    When I insert:
    """
    (ns core)

    (defn apply-call-site []
      (let [args [1]]
        (apply tt [1] [2] args)))
    """
    And I call the cljr--change-function-signature function directly with mockdata to swap bar and baz in a call-site with apply
    And I switch to buffer "*cljr-manual-intervention*"
    And I go to line "3"
    Then I should see pattern ".*core.clj:5"

  Scenario: I rename a function parameter
    When I insert:
    """
    (ns core)

    (defn tt [foo bar baz]
      (println foo bar baz))
    """
    And I call the cljr--change-function-signature function directly with mockdata to rename baz to qux
    Then I should see:
    """
    (ns core)

    (defn tt [foo bar qux]
      (println foo bar qux))
    """
