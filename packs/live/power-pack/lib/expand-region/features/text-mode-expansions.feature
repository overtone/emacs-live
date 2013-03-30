Feature: Text-mode expansions
  Background:
    Given there is no region selected
    And I turn on text-mode
    And I insert:
    """
    Lorem ipsum dolor sit amet, consectetur adipiscing elit.
    Here is a sentence.  Here is another.  And one with Dr. Baker.

    Another paragraph.  With 2 sentences.
    
    "We're on a different page," said the man.
    """

  Scenario: Mark sentence ending on a line
    When I place the cursor after "consectetur"
    And I press "C-@"
    Then the region should be "consectetur"
    And I press "C-@"
    Then the region should be "Lorem ipsum dolor sit amet, consectetur adipiscing elit."
    And I press "C-@"
    Then the region should be:
    """
    Lorem ipsum dolor sit amet, consectetur adipiscing elit.
    Here is a sentence.  Here is another.  And one with Dr. Baker.

    """

  Scenario: Mark sentence ending on a line 2
    When I place the cursor before "Lorem"
    And I press "C-@"
    Then the region should be "Lorem"
    And I press "C-@"
    Then the region should be "Lorem ipsum dolor sit amet, consectetur adipiscing elit."
    And I press "C-@"
    Then the region should be:
    """
    Lorem ipsum dolor sit amet, consectetur adipiscing elit.
    Here is a sentence.  Here is another.  And one with Dr. Baker.

    """

  Scenario: Mark sentence beginning a line
    When I place the cursor after "sentence."
    And I press "C-@"
    Then the region should be "sentence."
    And I press "C-@"
    Then the region should be "Here is a sentence."
    And I press "C-@"
    Then the region should be:
    """
    Lorem ipsum dolor sit amet, consectetur adipiscing elit.
    Here is a sentence.  Here is another.  And one with Dr. Baker.

    """

  Scenario: Mark sentence in the middle of a line
    When I place the cursor before "is another"
    And I press "C-@"
    Then the region should be "is"
    And I press "C-@"
    Then the region should be "Here is another."
    And I press "C-@"
    Then the region should be:
    """
    Lorem ipsum dolor sit amet, consectetur adipiscing elit.
    Here is a sentence.  Here is another.  And one with Dr. Baker.

    """

  Scenario: Mark sentence in the middle of a line
    When I place the cursor after "Baker."
    And I press "C-@"
    Then the region should be "Baker."
    And I press "C-@"
    Then the region should be "And one with Dr. Baker."
    And I press "C-@"
    Then the region should be:
    """
    Lorem ipsum dolor sit amet, consectetur adipiscing elit.
    Here is a sentence.  Here is another.  And one with Dr. Baker.

    """

  Scenario: Mark a page
    When I place the cursor after "Baker."
    And I press "C-u 4 C-@ C-x C-x C-u 2 C-b"
    Then the region should be:
    """
    Lorem ipsum dolor sit amet, consectetur adipiscing elit.
    Here is a sentence.  Here is another.  And one with Dr. Baker.

    Another paragraph.  With 2 sentences.
    """
    # trailing blank lines aren't captured for some reason.  That's
    # why all the C-x ... C-b stuff

  Scenario: Sentence endings
    When I place the cursor before "Dr."
    And I set sentence-end-double-space to nil
    And I press "C-u 3 C-@"
    Then the region should be "And one with Dr."

  Scenario: Sentence endings 2
    When I place the cursor before "Dr."
    And I set sentence-end-double-space to t
    And I press "C-u 3 C-@"
    Then the region should be "And one with Dr. Baker."
    # I turned sentence-end-double-space back to the default here in
    # case it comes into play in other tests.
