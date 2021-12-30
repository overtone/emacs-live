# Contributing

Do you have an issue to report or an idea to submit? That's great!
We're eager to make clj-refactor better. Please report issues to
the [issue tracker][1] of the repository or submit a pull request.

To help us, please, follow these guidelines:

## Issue reporting

* Check that the issue has not already been reported.
* Check that the issue has not already been fixed in the latest code
  (a.k.a. `master`).
* Be clear, concise and precise in your description of the problem.
* Mention the version you're running. You can call `cljr-version` to obtain this information.
* Mention the CIDER version you're running.
* Mention the leiningen or boot version you're using.
* Mention your Emacs version and operating system.
* Include any relevant code to the issue summary.

## Pull requests

* Read [how to properly contribute to open source projects on Github][2].
* Use a topic branch to easily amend a pull request later, if necessary.
* Write [good commit messages][3].
* Mention related tickets in the commit messages (e.g. `[Fix #N] Add command ...`)
* Update the [changelog][6].
* Use the same coding conventions as the rest of the project.
* [Squash related commits together][5].
* Open a [pull request][4] that relates to *only* one subject with a clear title and description in grammatically correct, complete sentences.

## Development Setup

1. Fork and clone the repository.
1. Install [Cask][7].
1. Run `cask install` in the repository folder.
1. Run tests with `make test`.

[1]: https://github.com/clojure-emacs/clj-refactor.el/issues
[2]: http://gun.io/blog/how-to-github-fork-branch-and-pull-request
[3]: http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html
[4]: https://help.github.com/articles/using-pull-requests
[5]: http://gitready.com/advanced/2009/02/10/squashing-commits-with-rebase.html
[6]: https://github.com/clojure-emacs/clj-refactor.el/blob/master/CHANGELOG.md
