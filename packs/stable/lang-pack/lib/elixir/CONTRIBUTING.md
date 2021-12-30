## Guidelines For Reporting An Issue/Feature

So you've found a bug or have a great idea for a feature. Here's the steps you
should take to help get it added/fixed in emacs-elixir

* First, check to see if there's an existing issue/pull request for the
  bug/feature. All issues are at https://github.com/elixir-editors/emacs-elixir/issues
  and pull reqs are at https://github.com/elixir-editors/emacs-elixir/pulls.
* If there isn't one there, please file an issue. The ideal report includes:

  * A description of the problem/suggestion.
  * How to recreate the bug.
  * Versions of your:

    * operating system
    * elixir-mode
    * emacs

  * Ideally, creating a pull request with a test case demonstrating what's wrong.
    This makes it easy for us to review, reproduce & fix the problem.

You might also hop into the IRC channel (``#elixir-lang`` on ``irc.freenode.net``)
& raise your question there, as there may be someone who can help you with a
work-around.


## Guidelines For Contributing Code

If you're ready to take the plunge & contribute back some code, the
process should look like:

* Fork the project on GitHub into your own account.
* Clone your copy of emacs-elixir.
* Make a new branch in git & commit your changes there.
* Push your new branch up to GitHub.
* Again, ensure there isn't already an issue or pull request out there on it.
  If there is & you feel you have a better fix, please take note of the issue
  number & mention it in your pull request.
* Create a new pull request (based on your branch), including what the
  problem/feature is, versions of your software & referencing any related
  issues/pull requests.

In order to be merged into emacs-elixir, contributions must have the following:

* A solid patch that:

  * is clear.
  * works across all supported versions of Emacs (25+).
  * follows the existing style of the code base.
  * comments included as needed.

* A test case that demonstrates the previous flaw that now passes
  with the included patch.

If your contribution lacks any of these things, they will have to be added
by a core contributor before being merged into emacs-elixir proper, which may take
substantial time for the all-volunteer team to get to.

## How to run tests

We use [Eldev](https://github.com/doublep/eldev) as the project management tool and its built-in ERT integration. Our build matrix can be seen in the workflow file in `.github/workflows/ci.yml`. This ensure we run tests in all of our build matrix.

Read more about Eldev in its home page.

#### Examples of usage

* Run all tests:

```bash
$ eldev test
```

* Check dependencies

```bash
$ eldev deps
```

```bash
$ eldev deps test
```

* Run linters

```bash
$ eldev lint
```

* Compile project (byte compilation)

```bash
$ eldev compile
```

* Clean-up, for example, after compilation

```bash
$ eldev clean
```
