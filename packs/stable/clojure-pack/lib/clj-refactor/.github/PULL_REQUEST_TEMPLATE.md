Before submitting a PR make sure the following things have been done (and denote this
by checking the relevant checkboxes):

- [ ] The commits are consistent with our [contribution guidelines](CONTRIBUTING.md)
- [ ] You've added tests (if possible) to cover your change(s)
- [ ] The new code is not generating byte compile warnings (run `cask exec emacs -batch -Q -L . -eval "(progn (setq byte-compile-error-on-warn t) (batch-byte-compile))" clj-refactor.el`)
- [ ] All tests are passing (run `./run-tests.sh`)
- [ ] You've updated the changelog (if adding/changing user-visible functionality)
- [ ] You've updated the readme (if adding/changing user-visible functionality)

Thanks!
