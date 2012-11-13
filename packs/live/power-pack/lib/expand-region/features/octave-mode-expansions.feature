Feature: octave-mod expansions
  In order to quickly and precisely mark octave units
  As an Emacs user
  I want to expand to them

  Scenario: Mark block from inside
    Given I turn on octave-mode
    And there is no region selected
    When I insert:
    """
    exprBefore;
    for i=1:n,
      something;
    end;
    exprAfter;
    """
    And I go to point "26"
    And I press "C-@"
    And I press "C-@"
    Then the region should be:
    """
    for i=1:n,
      something;
    end
    """


  Scenario: Mark block when looking at it
    Given I turn on octave-mode
    And there is no region selected
    When I insert:
    """
    exprBefore;
    for i=1:n,
      something;
    end;
    exprAfter;
    """
    And I go to point "13"
    And I press "C-@"
    And I press "C-@"
    Then the region should be:
    """
    for i=1:n,
      something;
    end
    """


  Scenario: Mark block when looking at it inside another block
    Given I turn on octave-mode
    And there is no region selected
    When I insert:
    """
    exprBefore;
    for i=1:n,
      for j=i:k,
        something;
      end;
    end;
    exprAfter;
    """
    And I go to point "26"
    And I press "C-@"
    And I press "C-@"
    Then the region should be:
    """
    for j=i:k,
        something;
      end
    """


  Scenario: Mark block from inside while looking at another
    Given I turn on octave-mode
    And there is no region selected
    When I insert:
    """
    exprBefore;
    for i=1:n,
      for j=i:k,
        something;
      end;
    end;
    exprAfter;
    """
    And I go to point "26"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    Then the region should be:
    """
    for i=1:n,
      for j=i:k,
        something;
      end;
    end
    """

