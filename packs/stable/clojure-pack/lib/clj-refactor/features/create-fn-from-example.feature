Feature: Create Function from Example

  Background:
    Given I have a project "cljr" in "tmp"
    And I have a clojure-file "tmp/src/cljr/core.clj"
    And I open file "tmp/src/cljr/core.clj"
    And I clear the buffer

  Scenario: Create function from example, all words
    When I insert:
    """
    (defn some-fn []
      (foo bar baz))
    """
    And I place the cursor before "bar"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- foo [bar baz]
      )

    (defn some-fn []
      (foo bar baz))
    """

   Scenario: Create function from example, line breaks
    When I insert:
    """
    (defn some-fn []
      (foo
        bar
        baz))
    """
    And I place the cursor before "bar"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- foo [bar baz]
      )

    (defn some-fn []
      (foo
        bar
        baz))
    """

  Scenario: Create function from example, all sorts of parameters
    When I insert:
    """
    (defn some-fn []
      (foo
        "some string"
        bar
        {:keyword :value}
        (clojure.string/trim (:a some-map))
        baz))
    """
    And I place the cursor before "baz"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- foo [arg0 bar arg2 arg3 baz]
      )

    (defn some-fn []
      (foo
        "some string"
        bar
        {:keyword :value}
        (clojure.string/trim (:a some-map))
        baz))
    """

  Scenario: Create function from example no params
    When I insert:
    """
    (defn some-fn []
      (foo))
    """
    And I place the cursor after "foo"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- foo []
      )

    (defn some-fn []
      (foo))
    """

  Scenario: Create function from example using literals
    When I insert:
    """
    (defn some-fn []
      (foo 1 bar :key #{1 2}))
    """
    And I place the cursor after "foo"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- foo [arg0 bar arg2 arg3]
      )

    (defn some-fn []
      (foo 1 bar :key #{1 2}))
    """

  Scenario: Placeholders for args
    When I insert:
    """
    (defn some-fn []
      (foo 1 2))
    """
    And I place the cursor after "foo"
    And I press "C-! fe"
    And I type "one"
    And I press "<tab>"
    And I type "two"
    And I press "<tab>"
    And I type "body"
    Then I should see:
    """
    (defn- foo [one two]
      body)

    (defn some-fn []
      (foo 1 2))
    """
