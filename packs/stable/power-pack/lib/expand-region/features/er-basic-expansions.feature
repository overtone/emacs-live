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

  Scenario: Mark symbol with prefix
    Given I turn on emacs-lisp-mode
    And I insert "(set 'abc 123)"
    When I place the cursor after "abc"
    And I press "C-@"
    And I press "C-@"
    Then the region should be "'abc"

  Scenario: Mark string
    Given I turn on emacs-lisp-mode
    And I insert "(set 'abc "123")"
    When I place the cursor after "2"
    And I press "C-@"
    And I press "C-@"
    Then the region should be ""123""

  Scenario: Mark word
    Given I turn on emacs-lisp-mode
    And I insert "(set-default 'abc 123)"
    When I place the cursor after "f"
    And I press "C-@"
    Then the region should be "default"

  Scenario: Mark symbol
    Given I turn on emacs-lisp-mode
    And I insert "(set-default 'abc 123)"
    When I place the cursor after "f"
    And I press "C-@"
    And I press "C-@"
    Then the region should be "set-default"

  Scenario: Mark method call
    Given I turn on js-mode
    And I insert "document.write('abc');"
    When I place the cursor after "write"
    And I press "C-@"
    And I press "C-@"
    Then the region should be "document.write('abc')"

  Scenario: Mark current pair
    Given I turn on emacs-lisp-mode
    And I insert "((foo)(bar))"
    When I place the cursor after "oo)"
    And I press "C-@"
    Then the region should be "(bar)"

