# Contributing

If you discover issues, have ideas for improvements or new features,
please report them to the [issue tracker][1] of the repository or
submit a pull request. Please, try to follow these guidelines when you
do so.

## Issue reporting

* Check that the issue has not already been reported.
* Check that the issue has not already been fixed in the latest code
  (a.k.a. `master`).
* Be clear, concise and precise in your description of the problem.
* Open an issue with a descriptive title and a summary in grammatically correct,
  complete sentences.
* Mention your Emacs version and operating system.
* Mention `clojure-mode`'s version info (`M-x clojure-mode-version-info`), e.g.:

```el
clojure-mode (version 2.1.1)
```

* Include any relevant code to the issue summary.

## Pull requests

* Read [how to properly contribute to open source projects on Github][2].
* Use a topic branch to easily amend a pull request later, if necessary.
* Write [good commit messages][3].
* Mention related tickets in the commit messages (e.g. `[Fix #N] Font-lock properly ...`)
* Update the [changelog][6].
* Use the same coding conventions as the rest of the project.
* Verify your Emacs Lisp code with `checkdoc` (<kbd>C-c ? d</kbd>).
* [Squash related commits together][5].
* Open a [pull request][4] that relates to *only* one subject with a clear title
and description in grammatically correct, complete sentences.
* When applicable, attach ERT unit tests. See below for instructions on running the tests.

## Development setup

1. Fork and clone the repository.
1. Install [Cask][7].
1. Run `cask install` in the repository folder.
1. Run tests with `make test`.

**Note:** macOS users should make sure that the `emacs` command resolves the version of Emacs they've installed
manually (e.g. via `homebrew`), instead of the ancient Emacs 22 that comes bundled with macOS.
See [this article][8] for more details.

[1]: https://github.com/clojure-emacs/clojure-mode/issues
[2]: http://gun.io/blog/how-to-github-fork-branch-and-pull-request
[3]: http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html
[4]: https://help.github.com/articles/using-pull-requests
[5]: http://gitready.com/advanced/2009/02/10/squashing-commits-with-rebase.html
[6]: https://github.com/clojure-emacs/clojure-mode/blob/master/CHANGELOG.md
[7]: https://github.com/cask/cask
[8]: https://emacsredux.com/blog/2015/05/09/emacs-on-os-x/
