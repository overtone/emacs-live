Feature: cperl-mode expansions
  In order to quickly and precisely mark perl variable names
  As an Emacs user
  I want to expand to them

  Scenario: Mark perl variable name
    Given I turn on cperl-mode
    And there is no region selected
    When I insert:
    """
    my $foo = "bar";
    """
    And I place the cursor after "$f"
    And I press "C-@"
    And I press "C-@"
    Then the region should be:
    """
    $foo
    """

  Scenario: Mark interpolated perl variable name
    Given I turn on cperl-mode
    And there is no region selected
    When I insert:
    """
    my $foo = "something $bar here";
    """
    And I place the cursor after "something "
    And I press "C-@"
    Then the region should be:
    """
    $bar
    """

  Scenario: Mark perl package name
    Given I turn on cperl-mode
    And there is no region selected
    When I insert:
    """
    Namespace::Foo::Bar::method_call($baz);
    """
    And I place the cursor before "::Foo"
    And I press "C-@"
    And I press "C-@"
    Then the region should be:
    """
    Namespace::Foo::Bar
    """

  Scenario: Mark one perl subroutine
    Given I turn on cperl-mode
    And there is no region selected
    When I insert:
    """
    sub foo {
      foo_do_something;
    }

    sub bar {
       bar_do_something;
    }

    sub baz {
       baz_do_something;
    }
    """
    And I place the cursor before "foo_do_something"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    Then the region should be:
    """
    sub foo {
      foo_do_something;
    }
    """
