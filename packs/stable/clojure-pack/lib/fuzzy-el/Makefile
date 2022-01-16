SHELL := /usr/bin/env bash

EMACS ?= emacs
CASK ?= cask

PKG-FILES := fuzzy.el

TEST-FILES := $(shell ls test/fuzzy-*.el)

.PHONY: clean checkdoc lint unix-build unix-compile	unix-test

unix-ci: clean unix-build unix-compile

unix-build:
	$(CASK) install

unix-compile:
	@echo "Compiling..."
	@$(CASK) $(EMACS) -Q --batch \
		-L . \
		--eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile $(PKG-FILES)

unix-test:
	@echo "Testing..."
	$(CASK) exec ert-runner -L . $(LOAD-TEST-FILES) -t '!no-win' -t '!org'

clean:
	rm -rf .cask *.elc
