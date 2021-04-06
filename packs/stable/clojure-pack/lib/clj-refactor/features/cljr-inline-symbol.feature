Feature: Inlining of symbols

  Background:
    And I have a project "example" in "tmp"
    And I have a clojure-file "tmp/src/example/core.clj"
    When I open file "tmp/src/example/core.clj"
    And I clear the buffer
    And I press "M->"

  Scenario: Inline def
    When I insert:
    """
    (def my-constant 123)

    (defn my-function []
      (let [another-val 321]
        (println my-constant my-constant another-val)))
    """
    And I call the cljr--inline-symbol function directly with mockdata to inline my-constant
    Then I should see:
    """
    (defn my-function []
      (let [another-val 321]
        (println 123 123 another-val)))
    """

  Scenario: Inline let bound
    When I insert:
    """
    (def my-constant 123)

    (defn my-function []
      (let [another-val 321]
        (println my-constant my-constant another-val)))
    """
    And I call the cljr--inline-symbol function directly with mockdata to inline another-val
    Then I should see:
    """
    (def my-constant 123)

    (defn my-function []
      (println my-constant my-constant 321))
    """

  Scenario: Inline let bound at end of vector
    When I insert:
    """
    (def my-constant 123)

    (defn my-function []
      (let [another-val 321
            some-val 110]
        (println my-constant my-constant another-val)))
    """
    And I call the cljr--inline-symbol function directly with mockdata to inline some-val
    Then I should see:
    """
    (def my-constant 123)

    (defn my-function []
      (let [another-val 321]
        (println my-constant my-constant another-val)))
    """


  Scenario: Inline anonymous fn and replace call-site
    When I insert:
    """
    (defn my-inc [n]
      (+ 1 n))

    (+ (my-inc (- 17 4)) 55)

    (map my-inc (range 10))
    """
    And I call the cljr--inline-symbol function directly with mockdata to inline my-inc
    Then I should see:
    """
    (+ (+ 1 (- 17 4)) 55)

    (map (fn [n]
      (+ 1 n)) (range 10))
  """
