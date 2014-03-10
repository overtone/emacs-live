@requires-e24-3
Feature: fgallinas python.el expansions
  In order to quickly and precisely mark Python code blocks
  As an Emacs user
  I want to expand to them

  Scenario: Baseline feature test.
    Given I turn on python-mode
    And there is no region selected
    When I insert "run(23)"
    And I place the cursor between "n" and "("
    And I press "C-@"
    And I press "C-@"
    Then the region should be "run(23)"

  Scenario: Mark region inside a string.
    Given I turn on python-mode
    And there is no region selected
    When I insert "'X-Men: Wolverine'"
    And I place the cursor between "r" and "i"
    And I press "C-@"
    And I press "C-@"
    Then the region should be "X-Men: Wolverine"

  Scenario: Mark region inside a string with escape delimiter.
    Given I turn on python-mode
    And there is no region selected
    When I insert "'pre' + 'X-Men: Wol\'verine' + 'post'"
    And I place the cursor between "r" and "i"
    And I press "C-@"
    And I press "C-@"
    Then the region should be "X-Men: Wol\'verine"

  Scenario: Mark region outside a string.
    Given I turn on python-mode
    And there is no region selected
    When I insert "run('X-Men: ' + 'Wolverine')"
    And I place the cursor between "M" and "e"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    Then the region should be "'X-Men: '"

  Scenario: Mark region inside a multi-line string.
    Given I turn on python-mode
    And there is no region selected
    When I insert:
      """
      print('lalelu')

      '''This is a multi-line Python string
      with lots of useless content.
      '''

      print('lalelu')
      """
    And I place the cursor between "-" and "l"
    And I press "C-@"
    And I press "C-@"
    Then the region should be:
      """
      This is a multi-line Python string
      with lots of useless content.

      """

#  Scenario: Mark region outside a multi-line string.
#    Given I turn on python-mode
#    And there is no region selected
#    When I insert:
#      """
#      '''This is a multi-line Python string
#      with lots of useless content.
#      '''
#      """
#    And I place the cursor between "-" and "l"
#    And I press "C-@"
#    And I press "C-@"
#    And I press "C-@"
#    Then the region should be:
#      """
#      '''This is a multi-line Python string
#      with lots of useless content.
#      '''
#      """

  Scenario: Mark a basic Python block
    Given I turn on python-mode
    And there is no region selected
    When I insert:
      """
      if True:
          print('To be, or not to be...')
      else:
          print('Booyah.')
      """
    And I go to point "1"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    Then the region should be:
      """
      if True:
          print('To be, or not to be...')
      """

  Scenario: Mark a Python block with a nested block
    Given I turn on python-mode
    And there is no region selected
    When I insert:
      """
      if True:
          if True:
              print(23)
          print('To be, or not to be...')
      else:
          print('Booyah.')
      """
    And I go to point "1"
    And I press "C-@"
    Then the region should be:
      """
      if
      """
    And I press "C-@"
    Then the region should be:
      """
      if True:
      """
    And I press "C-@"
    Then the region should be:
      """
      if True:
          if True:
              print(23)
          print('To be, or not to be...')
      """

  Scenario: Mark another Python block with a nested block
    Given I turn on python-mode
    And there is no region selected
    When I insert:
      """
      def moo(data):
          for foo in data.items():
              print(foo)

      """
    And I go to point "1"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    Then the region should be:
      """
      def moo(data):
          for foo in data.items():
              print(foo)
      """

  Scenario: Mark an outer Python block
    Given I turn on python-mode
    And there is no region selected
    When I insert:
      """
      print('More stuff')

      def the_truth():
          if True:
              print('To be, or not to be...')
          else:
              print('Booyah.')

      print('Even more stuff.')
      """
    And I go to the front of the word "if"
    And I press "C-@"
    Then the region should be:
      """
      if
      """
    And I press "C-@"
    Then the region should be:
      """
      if True:
      """
    And I press "C-@"
    Then the region should be:
      """
      if True:
              print('To be, or not to be...')
      """
    And I press "C-@"
    Then the region should be:
      """
      def the_truth():
          if True:
              print('To be, or not to be...')
          else:
              print('Booyah.')
      """

  Scenario: Mark nested Python block with subsequent statements in outer block
    Given I turn on python-mode
    And there is no region selected
    When I insert:
      """
      def outer_foo():

          def inner_foo():
              return 23

          return inner_foo()

      """
    And I go to point "23"
    And I press "C-@"
    Then the region should be:
      """
      def
      """
    And I press "C-@"
    Then the region should be:
      """
      def inner_foo():
      """
    And I press "C-@"
    Then the region should be:
      """
      def inner_foo():
              return 23
      """
