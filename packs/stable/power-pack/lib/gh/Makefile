EMACS ?= emacs

.PHONY: build test clean

build:
	keg build

test:
	keg exec $(EMACS) --batch \
	-l test/gh-test.el \
	-l test/gh-gist-test.el \
	-l test/gh-issues-test.el \
	-l test/gh-orgs-test.el \
	-l test/gh-repos-test.el \
	-f ert-run-tests-batch-and-exit

clean:
	keg clean
