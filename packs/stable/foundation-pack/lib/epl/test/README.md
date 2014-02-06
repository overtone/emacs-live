# EPL unit tests

Unit tests for EPL, for use with [`ert-runner`][1].

- `provision-vm.sh` provisions a testing environment with all supported Emacs
  versions and [Cask][2].  It is used to provision Travis CI and the local
  Vagrant testing VM.
- `test-helper.el` provides helper functions and utilities for the tests.  It is
  loaded automatically by ert-runner.
- `resources/` contains resources for use in the test cases.  Use
  `epl-test-resource-file-name` to load resources from this directory.

The test cases are contained in the `-test.el` files.


[1]: https://github.com/rejeep/ert-runner.el
[2]: https://github.com/cask/cask
