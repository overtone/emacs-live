EMACS ?= emacs
CASK ?= cask
EMACS23=emacs23

ELPA_DIR = $(shell EMACS=$(EMACS) $(CASK) package-directory)

.PHONY: test test-nw test-emacs23 test-emacs23-nw travis-ci

test:
	$(CASK) exec $(EMACS) -Q -L . -l tests/run-test.el

test-nw:
	$(CASK) exec $(EMACS) -Q -nw -L . -l tests/run-test.el

test-emacs23: tests/ert.el
	${EMACS23} -Q -L . -l test/ert.el -l tests/run-test.el

test-emacs23-nw: tests/ert.el
	$(EMACS23) -Q -nw -L . -l test/ert.el -l tests/run-test.el

travis-ci: elpa
	$(CASK) exec $(EMACS) -batch -Q -l tests/run-test.el

elpa: $(ELPA_DIR)
$(ELPA_DIR): Cask
	$(CASK) install
	touch $@
