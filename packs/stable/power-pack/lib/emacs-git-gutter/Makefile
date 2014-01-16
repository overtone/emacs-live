.PHONY : test

EMACS ?= emacs

test:
	$(EMACS) -Q -batch -L . -l test-git-gutter.el -f ert-run-tests-batch-and-exit
