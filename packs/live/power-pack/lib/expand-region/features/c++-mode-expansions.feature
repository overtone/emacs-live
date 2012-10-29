Feature: C++-mode expansions
  Background:
    Given there is no region selected
    And I turn on c++-mode
    And I insert:
    """
    #include <iostream>

    namespace Foo {
      struct Bar {
        static float val (int x, double y) { return 42.; }
      };
    }

    int main (int argc, char **argv) {
      int x = 0;
      double y = 1.;
      float z = Foo::Bar::val (x, y);
      char t = argv [x + 3];
    
      int i = 0;
      for ( ; i<N ; ++i ) {
        doSomething (i);
      }
    }
    """

  Scenario: Mark fully-qualified symbol
    When I place the cursor after "Foo::Ba"
    And I press "C-@"
    Then the region should be "Bar"
    And I press "C-@"
    Then the region should be "Foo::Bar::val"



  Scenario: Mark fully-qualified function call (1)
    When I place the cursor after "Foo::Bar::va"
    And I press "C-u 2 C-@"
    Then the region should be "Foo::Bar::val"
    And I press "C-@"
    Then the region should be "Foo::Bar::val (x, y)"

  Scenario: Mark fully-qualified function call (2)
    When I place the cursor after "Foo::Bar::val ("
    And I press "C-u 3 C-@"
    Then the region should be "(x, y)"
    And I press "C-@"
    Then the region should be "Foo::Bar::val (x, y)"
