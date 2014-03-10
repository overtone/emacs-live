Feature: Threading and unwinding of macros

  Background:
    Given I have a project "cljr" in "tmp"
    And I have a clojure-file "tmp/src/cljr/core.clj"
    And I open file "tmp/src/cljr/core.clj"
    And I clear the buffer

  Scenario: Thread first (->), part 1
    When I insert "(-> (dissoc (assoc {} :key "value") :lock))"
    And I press "C-! th"
    Then I should see:
    """
    (-> (assoc {} :key "value")
        (dissoc :lock))
    """

  Scenario: Thread first (->), part 2
    When I insert "(-> (dissoc (assoc {} :key "value") :lock))"
    And I press "C-! th"
    And I press "C-! th"
    Then I should see:
    """
    (-> {}
        (assoc :key "value")
        (dissoc :lock))
    """

  Scenario: Thread first (->), part 3 - don't thread maps and stuff
    When I insert "(-> (dissoc (assoc {} :key "value") :lock))"
    And I press "C-! th"
    And I press "C-! th"
    And I press "C-! th"
    Then I should see:
    """
    (-> {}
        (assoc :key "value")
        (dissoc :lock))
    """

  Scenario: Thread first (->), part 4 - don't thread last one
    When I insert "(-> (dissoc (assoc (get-a-map) :key "value") :lock))"
    And I press "C-! th"
    And I press "C-! th"
    And I press "C-! th"
    Then I should see:
    """
    (-> (get-a-map)
        (assoc :key "value")
        (dissoc :lock))
    """

  Scenario: Thread first (->), part 5 - don't be a stickler for white space
    When I insert:
    """
    (->
     (dissoc (assoc {} :key "value") :lock))
    """
    And I press "C-! th"
    Then I should see:
    """
    (->
     (assoc {} :key "value")
     (dissoc :lock))
    """

  Scenario: Thread first, part 6 - remove superfluous parens
    When I insert "(-> (square (sum [1 2 3 4 5])))"
    And I press "C-! th"
    And I press "C-! th"
    Then I should see:
    """
    (-> [1 2 3 4 5]
        sum
        square)
    """

  Scenario: Thread first, part 7 - allow cursor placement before threading
    When I insert "(-> (not (s-acc/mobile? session)))"
    And I place the cursor before "(->"
    And I press "C-! th"
    Then I should see:
    """
    (-> (s-acc/mobile? session)
        not)
    """

  Scenario: Unwind first (->), part 1
    When I insert:
    """
    (-> {}
        (assoc :key "value")
        (dissoc :lock))
    """
    And I press "C-! uw"
    Then I should see:
    """
    (-> (assoc {} :key "value")
        (dissoc :lock))
    """

  Scenario: Unwind first (->), part 2
    When I insert:
    """
    (-> {}
        (assoc :key "value")
        (dissoc :lock))
    """
    And I press "C-! uw"
    And I press "C-! uw"
    Then I should see "(-> (dissoc (assoc {} :key "value") :lock))"

  Scenario: Unwind first (->), part 3 - jump out of threading
    When I insert:
    """
    (-> {}
        (assoc :key "value")
        (dissoc :lock))
    """
    And I press "C-! uw"
    And I press "C-! uw"
    And I press "C-! uw"
    Then I should see "(dissoc (assoc {} :key "value") :lock)"
    And I should not see "->"

  Scenario: Thread last (->>), part 1
    When I insert "(->> (map square (filter even? [1 2 3 4 5])))"
    And I press "C-! th"
    Then I should see:
    """
    (->> (filter even? [1 2 3 4 5])
         (map square))
    """

  Scenario: Thread last (->>), part 2
    When I insert "(->> (map square (filter even? [1 2 3 4 5])))"
    And I press "C-! th"
    And I press "C-! th"
    Then I should see:
    """
    (->> [1 2 3 4 5]
         (filter even?)
         (map square))
    """

  Scenario: Thread last (->>), part 3 - don't thread vectors and stuff
    When I insert "(->> (map square (filter even? [1 2 3 4 5])))"
    And I press "C-! th"
    And I press "C-! th"
    And I press "C-! th"
    Then I should see:
    """
    (->> [1 2 3 4 5]
         (filter even?)
         (map square))
    """

  Scenario: Thread last (->>), part 4 - don't thread last one
    When I insert "(->> (map square (filter even? (get-a-list))))"
    And I press "C-! th"
    And I press "C-! th"
    And I press "C-! th"
    Then I should see:
    """
    (->> (get-a-list)
         (filter even?)
         (map square))
    """

  Scenario: Unwind last (->>), part 1
    When I insert:
    """
    (->> [1 2 3 4 5]
         (filter even?)
         (map square))
    """
    And I press "C-! uw"
    Then I should see:
    """
    (->> (filter even? [1 2 3 4 5])
         (map square))
    """

  Scenario: Unwind last (->>), part 2
    When I insert:
    """
    (->> [1 2 3 4 5]
         (filter even?)
         (map square))
    """
    And I press "C-! uw"
    And I press "C-! uw"
    Then I should see "(->> (map square (filter even? [1 2 3 4 5])))"

  Scenario: Unwind last (->>), part 3 - jump out of threading
    When I insert:
    """
    (->> [1 2 3 4 5]
         (filter even?)
         (map square))
    """
    And I press "C-! uw"
    And I press "C-! uw"
    And I press "C-! uw"
    Then I should see "(map square (filter even? [1 2 3 4 5]))"
    And I should not see "->>"

  Scenario: Unwind function name, part 1
    When I insert:
    """
    (->> [1 2 3 4 5]
         sum
         square)
    """
    And I press "C-! uw"
    Then I should see:
    """
    (->> (sum [1 2 3 4 5])
         square)
    """

  Scenario: Unwind function name, part 2
    When I insert:
    """
    (-> [1 2 3 4 5]
        sum
        square)
    """
    And I press "C-! uw"
    And I press "C-! uw"
    Then I should see "(-> (square (sum [1 2 3 4 5])))"

  Scenario: Unwind, issue #6, part 1 - formatting
    When I insert:
    """
    (defn plus [a b]
      (-> a (+ b)))
    """
    And I press "C-! uw"
    Then I should see:
    """
    (defn plus [a b]
      (-> (+ a b)))
    """

  Scenario: Unwind, issue #6, part 2 - formatting
    When I insert:
    """
    (defn plus [a b]
      (->> a (+ b)))
    """
    And I press "C-! uw"
    Then I should see:
    """
    (defn plus [a b]
      (->> (+ b a)))
    """

  Scenario: Thread first (some->)
    When I insert "(some-> (+ (val (find {:a 1} :b)) 5))"
    And I press "C-! th"
    And I press "C-! th"
    And I press "C-! th"
    Then I should see:
    """
    (some-> {:a 1}
            (find :b)
            val
            (+ 5))
    """

  Scenario: Thread last (some->>)
    When I insert "(some->> (+ 5 (val (find {:a 1} :b))))"
    And I press "C-! th"
    And I press "C-! th"
    And I press "C-! th"
    Then I should see:
    """
    (some->> :b
             (find {:a 1})
             val
             (+ 5))
    """

  Scenario: Unwind last (some->)
    When I insert:
    """
    (some-> {:a 1}
            (find :b)
            val
            (+ 5))
    """
    And I press "C-! uw"
    And I press "C-! uw"
    And I press "C-! uw"
    Then I should see:
    """
    (some-> (+ (val (find {:a 1} :b)) 5))
    """

  Scenario: Unwind last (some->>)
    When I insert:
    """
    (some->> :b
             (find {:a 1})
             val
             (+ 5))
    """
    And I press "C-! uw"
    And I press "C-! uw"
    And I press "C-! uw"
    Then I should see:
    """
    (some->> (+ 5 (val (find {:a 1} :b))))
    """

  Scenario: Thread first all (->)
    When I insert "(->map (assoc {} :key "value") :lock)"
    And I place the cursor before "(->map (assoc"
    And I press "C-! tf"
    Then I should see:
    """
    (-> {}
        (assoc :key "value")
        (->map :lock))
    """

  Scenario: Thread last all (->>)
    When I insert "(map square (filter even? (make-things)))"
    And I place the cursor before "(map square"
    And I press "C-! tl"
    Then I should see:
    """
    (->> (make-things)
         (filter even?)
         (map square))
    """

  Scenario: Unwind all (->)
    When I insert:
    """
    (-> {}
        (assoc :key "value")
        (dissoc :lock))
    """
    And I place the cursor before "(-> "
    And I press "C-! ua"
    Then I should see:
    """
    (dissoc (assoc {} :key "value") :lock)
    """

  Scenario: Unwind all (->>)
    When I insert: 
    """
    (->> (make-things)
         (filter even?)
         (map square))
    """
    And I place the cursor before "(->>"
    And I press "C-! ua"
    Then I should see:
    """
    (map square (filter even? (make-things)))
    """
