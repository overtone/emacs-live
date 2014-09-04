Feature: org-mode expansions
  In order to quickly and precisely mark org mode sections
  As an Emacs user
  I want to expand to them

  Scenario: Org level 3
    Given I turn on org-mode
    When I insert:
    """
    * lvl 1
    ** lvl 2
    *** lvl 3
    """
    And I place the cursor before "*** lvl 3"
    And I press "C-@"
    And I press "C-@"
    Then the region should be "*** lvl 3"

  Scenario: Org level 2
    Given I turn on org-mode
    When I insert:
    """
    * lvl 1
    ** lvl 2
    *** lvl 3
    """
    And I place the cursor before "*** lvl 3"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    Then the region should be:
    """
    ** lvl 2
    *** lvl 3
    """

  Scenario: Org level 1
    Given I turn on org-mode
    When I insert:
    """
    * lvl 1
    ** lvl 2
    *** lvl 3
    """
    And I place the cursor before "*** lvl 3"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    Then the region should be:
    """
    * lvl 1
    ** lvl 2
    *** lvl 3
    """
