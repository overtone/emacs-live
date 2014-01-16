Feature: Basic expansions

  Scenario: Mark URL
    Given there is no region selected
    And I insert "Here is the link: http://emacsrocks.com :-)"
    When I place the cursor after "http"
    And I press "C-@"
    And I press "C-@"
    Then the region should be "http://emacsrocks.com"

  Scenario: Mark email
    Given there is no region selected
    And I insert "Here is the email: sample@example.com :-)"
    When I place the cursor after "sample"
    And I press "C-@"
    And I press "C-@"
    Then the region should be "sample@example.com"
