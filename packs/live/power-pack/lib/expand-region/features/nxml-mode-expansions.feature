Feature: nxml-mode expansions
  In order to quickly and precisely mark xml units
  As an Emacs user
  I want to expand to them

  Scenario: Mark xml attribute from start
    Given I turn on nxml-mode
    And there is no region selected
    When I insert "<div id="5">"
    And I place the cursor between " " and "id"
    And I press "C-@"
    And I press "C-@"
    Then the region should be "id="5""

  Scenario: Mark xml tags, part 1
    Given I turn on nxml-mode
    And there is no region selected
    When I insert "... <div class='hi'><div>before <span></span></div> after</div> ..."
    And I place the cursor between "before " and "<span>"
    And I press "C-@"
    Then the region should be "<span>"

  Scenario: Mark xml tags, part 2
    Given I turn on nxml-mode
    And there is no region selected
    When I insert "... <div class='hi'><div>before <span></span></div> after</div> ..."
    And I place the cursor between "before " and "<span>"
    And I press "C-@"
    And I press "C-@"
    Then the region should be "<span></span>"

  Scenario: Mark xml tags, part 3
    Given I turn on nxml-mode
    And there is no region selected
    When I insert "... <div class='hi'><div>before <span></span></div> after</div> ..."
    And I place the cursor between "before " and "<span>"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    Then the region should be "before <span></span>"

  Scenario: Mark xml tags, part 4
    Given I turn on nxml-mode
    And there is no region selected
    When I insert "... <div class='hi'><div>before <span></span></div> after</div> ..."
    And I place the cursor between "before " and "<span>"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    Then the region should be "<div>before <span></span></div>"

  Scenario: Mark xml tags, part 5
    Given I turn on nxml-mode
    And there is no region selected
    When I insert "... <div class='hi'><div>before <span></span></div> after</div> ..."
    And I place the cursor between "before " and "<span>"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    Then the region should be "<div>before <span></span></div> after"

  Scenario: Mark xml tags, part 6
    Given I turn on nxml-mode
    And there is no region selected
    When I insert "... <div class='hi'><div>before <span></span></div> after</div> ..."
    And I place the cursor between "before " and "<span>"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    Then the region should be "<div class='hi'><div>before <span></span></div> after</div>"
