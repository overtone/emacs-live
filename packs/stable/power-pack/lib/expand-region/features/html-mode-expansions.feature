Feature: html-mode expansions
  In order to quickly and precisely mark html units
  As an Emacs user
  I want to expand to them

  Scenario: Mark html attribute from start
    Given I turn on html-mode
    And there is no region selected
    When I insert "<div id="5">"
    And I place the cursor between " " and "id"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    Then the region should be "id="5""

  Scenario: Mark html attribute from end
    Given I turn on html-mode
    And there is no region selected
    When I insert "<div id="5">"
    And I go to point "12"
    And I press "C-@"
    And I press "C-@"
    Then the region should be "id="5""

  Scenario: Mark html tags, part 1
    Given I turn on html-mode
    And there is no region selected
    When I insert "... <div class='hi'><div>before <span></span></div> after</div> ..."
    And I place the cursor between "before " and "<span>"
    And I press "C-@"
    Then the region should be "<span>"

  Scenario: Mark html tags, part 2
    Given I turn on html-mode
    And there is no region selected
    When I insert "... <div class='hi'><div>before <span></span></div> after</div> ..."
    And I place the cursor between "before " and "<span>"
    And I press "C-@"
    And I press "C-@"
    Then the region should be "<span></span>"

  Scenario: Mark html tags, part 3
    Given I turn on html-mode
    And there is no region selected
    When I insert "... <div class='hi'><div>before <span></span></div> after</div> ..."
    And I place the cursor between "before " and "<span>"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    Then the region should be "before <span></span>"

  Scenario: Mark html tags, part 4
    Given I turn on html-mode
    And there is no region selected
    When I insert "... <div class='hi'><div>before <span></span></div> after</div> ..."
    And I place the cursor between "before " and "<span>"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    Then the region should be "<div>before <span></span></div>"

  Scenario: Mark html tags, part 5
    Given I turn on html-mode
    And there is no region selected
    When I insert "... <div class='hi'><div>before <span></span></div> after</div> ..."
    And I place the cursor between "before " and "<span>"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    Then the region should be "<div>before <span></span></div> after"

  Scenario: Mark html tags, part 6
    Given I turn on html-mode
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

  Scenario: Text mode expansions shouldn't be here
    Given I turn on html-mode
    And there is no region selected
    When I insert "Sentence the first.  Sentence the second"
    And I place the cursor between "first.  " and "Sentence"
    And I press "C-@"
    And I press "C-@"
    Then the region should be "Sentence the first.  Sentence the second"
