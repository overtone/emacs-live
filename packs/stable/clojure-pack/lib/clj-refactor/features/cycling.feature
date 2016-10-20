Feature: Code Cycling

  Background:
    Given I have a project "cljr" in "tmp"
    And I have a clojure-file "tmp/src/cljr/core.clj"
    And I open file "tmp/src/cljr/core.clj"
    And I clear the buffer

Scenario: Cycling thread-first to thread-last
   When I insert:
   """
   (defn foo [coll]
     (-> coll
         (map inc)))
   """
   And I place the cursor after "(map"
   And I press "C-! ct"
   Then I should see:
   """
   (defn foo [coll]
     (->> coll
          (map inc)))
   """

Scenario: Cycling thread-last to thread-first
   When I insert:
   """
   (defn foo []
     (-> {}
         (->> (assoc :bar 1))))
   """
   And I place the cursor after "(assoc"
   And I press "C-! ct"
   Then I should see:
   """
   (defn foo []
     (-> {}
         (-> (assoc :bar 1))))
   """
