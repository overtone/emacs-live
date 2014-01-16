Feature: C-mode expansions
  Background:
    Given there is no region selected
    And I turn on c-mode
    And I insert:
    """
    int main (int argc, char **argv) {
      int x = 0;
      double y = 1.;
      float z = my_function (x, y);
      char t = argv [x + 3];

      fun ( (char*)bob, joe );
    
      int i = 0;
      for ( ; i<N ; ++i ) {
        doSomething (i);
      }
    }
    """



  Scenario: Mark function call (inside function name)
    When I place the cursor after "my_fun"
    And I press "C-@"
    Then the region should be "function"
    And I press "C-@"
    Then the region should be "my_function"
    And I press "C-@"
    Then the region should be "my_function (x, y)"

  Scenario: Mark function call (inside arguments)
    When I place the cursor after "my_function ("
    And I press "C-@"
    Then the region should be "x"
    And I press "C-u 3 C-@"
    Then the region should be "my_function (x, y)"



  Scenario: Mark vector access (inside vector name)
    When I place the cursor after "char t = ar"
    And I press "C-@"
    Then the region should be "argv"
    And I press "C-@"
    Then the region should be "argv [x + 3]"

  Scenario: Mark vector access (inside argument)
    When I place the cursor after "argv ["
    And I press "C-@"
    Then the region should be "x"
    And I press "C-u 3 C-@"
    Then the region should be "argv [x + 3]"



  Scenario: Mark simple statement (before)
    When I place the cursor after "double"
    And I press "C-@"
    Then the region should be "double"
    When I press "C-@"
    Then the region should be "double y = 1.;"
    
  Scenario: Mark simple statement (inside)
    When I place the cursor before "double"
    And I press "C-@"
    Then the region should be "double"
    When I press "C-@"
    Then the region should be "double y = 1.;"

  Scenario: Mark simple statement (at end)
    When I place the cursor after "y = 1."
    And I press "C-@"
    Then the region should be "1."
    When I press "C-@"
    Then the region should be "double y = 1.;"



  Scenario: Mark complex statement (before)
    When I place the cursor after "fo"
    And I press "C-@"
    Then the region should be "for"
    And I press "C-@"
    Then the region should be "for ( ; i<N ; ++i )"

  Scenario: Mark complex statement (inside)
    When I place the cursor after "i<"
    And I press "C-@"
    Then the region should be "N"
    And I press "C-@"
    Then the region should be "i<N ;"
    And I press "C-u 3 C-@"
    Then the region should be "for ( ; i<N ; ++i )"
    
  Scenario: Mark complex statement (at end)
    When I place the cursor after "++"
    And I press "C-@"
    Then the region should be "i"
    And I press "C-u 3 C-@"
    Then the region should be "for ( ; i<N ; ++i )"



  Scenario: Mark statement-block (inside statement)
    When I place the cursor after "fo"
    And I press "C-u 2 C-@"
    Then the region should be "for ( ; i<N ; ++i )"
    And I press "C-@"
    Then the region should be:
    """
    for ( ; i<N ; ++i ) {
        doSomething (i);
      }
    """

  Scenario: Mark statement-block (inside block)
    When I place the cursor after "some"
    And I press "C-u 5 C-@"
    Then the region should be:
    """
    for ( ; i<N ; ++i ) {
        doSomething (i);
      }
    """



  Scenario: Handle consecutive open parens (issue #69)
    When I place the cursor after "(char*)"
    And I press "C-u 3 C-@"
    Then the region should be "( (char*)bob, joe )"
