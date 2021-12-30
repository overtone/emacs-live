EMACS ?= emacs

.PHONY: build test clean

build:
	keg build

test:
	keg exec $(EMACS) --batch -l test/logito-test.el -f ert-run-tests-batch-and-exit

lint:
	keg lint

clean:
	keg clean
