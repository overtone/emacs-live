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
  * works across all supported versions of Emacs (24+).
  * follows the existing style of the code base.
  * comments included as needed.

* A test case that demonstrates the previous flaw that now passes
  with the included patch.

If your contribution lacks any of these things, they will have to be added
by a core contributor before being merged into emacs-elixir proper, which may take
substantial time for the all-volunteer team to get to.

## How to run tests

There are two tools that helps us to test emacs-elixir:

* [Cask](https://github.com/cask/cask) - a project management tool for Emacs that helps automate the package development cycle.
* [Ert-runner](https://github.com/rejeep/ert-runner.el) - a tool for Emacs projects tested using Ert.

### Emacs Version Manager

Emacs has many versions and currently we support from version 24+. If you want to reproduce a bug/issue on a specific version of Emacs, there are some alternatives like EVM. Here is a setup for EVM. 

To install [EVM](https://github.com/rejeep/evm), run:

```bash
$ sudo mkdir /usr/local/evm
$ sudo chown $USER: /usr/local/evm
$ curl -fsSkL https://raw.github.com/rejeep/evm/master/go | bash
$ export PATH="~/.evm/bin:$PATH" # Add it to your .bashrc or analogue
```

To list all available Emacs versions you can install, run:

```bash
$ evm list
```

To install a version (for example `emacs-24.3-bin`), run:

```bash
$ evm install emacs-24.3-bin
```

Read more about [EVM](https://github.com/rejeep/evm).

### Cask and ert-runner

To install Cask, run:

```bash
$ curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python
$ export PATH="~/.cask/bin:$PATH" # Add it to your .bashrc or analogue
```

To install [Ert-runner](https://github.com/rejeep/ert-runner.el), run:

```bash
$ cd path/to/emacs-elixir
$ cask install # install ert-runner
$ EMACS=`evm bin emacs-24.3-bin` cask install # install ert-runner for Emacs 24.3
```

#### Examples of usage

* Run all tests:

```bash
$ cask exec ert-runner
```

* Run all tests for Emacs 24.3:

```bash
$ EMACS=`evm bin emacs-24.3-bin` cask exec ert-runner
```

Run all tests which are tagged `fontification`:

```bash
$ cask exec ert-runner -t fontification
```

Run all tests with `elixir-smie-verbose-p` equal to `t`:

```bash
$ cask exec ert-runner --verbose
```

Run all tests interactively:

```bash
$ cask exec ert-runner --win
```

Run all tests which are tagged `fontification` for Emacs 24.3 interactively:

```bash
$ EMACS=`evm bin emacs-24.3-bin` cask exec ert-runner -t fontification --win
```

Read more about [Cask](https://github.com/cask/cask) and [Ert-runner](https://github.com/rejeep/ert-runner.el).
