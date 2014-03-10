EMACS  ?= emacs
EFLAGS ?=
BATCH   = $(EMACS) $(EFLAGS) -batch -Q -L .
BATCHE  = $(BATCH) -eval
BATCHC  = $(BATCH) -f batch-byte-compile

ELS  = git-commit-mode.el
ELS += git-rebase-mode.el
ELS += gitattributes-mode.el
ELS += gitconfig-mode.el
ELS += gitignore-mode.el
ELCS = $(ELS:.el=.elc)

.PHONY: lisp
lisp: $(ELCS)

.PHONY: clean
clean:
	@echo "Cleaning..."
	@rm -f $(ELCS)

%.elc: %.el
	@$(EMACS) $(EFLAGS) -Q -batch -f batch-byte-compile $<

.PHONY: test
test:
	@$(BATCHE) "(progn\
	(require 'cl) \
	(put 'flet 'byte-obsolete-info nil))" \
	-l tests/git-commit-tests.el -f ert-run-tests-batch-and-exit

.PHONY: test-interactive
test-interactive:
	@$(EMACS) $(EFLAGS) -Q -L "." --eval "(progn\
	(require 'cl)\
	(put 'flet 'byte-obsolete-info nil)\
	(load-file \"tests/git-commit-tests.el\")\
	(ert t))"
