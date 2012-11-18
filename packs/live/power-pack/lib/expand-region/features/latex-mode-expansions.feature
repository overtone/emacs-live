Feature: latex-mode expansions
  Background:
    Given there is no region selected
    And I turn on latex-mode

    Scenario: Mark simple math
    When I insert "$E=mc^2$"
    And I place the cursor before "="
    And I press "C-@"
    Then the region should be "E"
    And I press "C-@"
    Then the region should be "E=mc"
    And I press "C-@"
    Then the region should be "$E=mc^2$"
