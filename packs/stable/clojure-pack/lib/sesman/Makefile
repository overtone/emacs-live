export EMACS ?= emacs
EMACSFLAGS = -L .
VERSION = $(git describe --tags --abbrev=0 | sed 's/^v//')

ELS = $(wildcard *.el)
OBJECTS = $(ELS:.el=.elc)

.PHONY: test version compile

all: compile checkdoc test

compile: version clean
	$(EMACS) --batch --load targets/compile.el

checkdoc: version
	$(EMACS) --batch --load targets/checkdoc.el

lint: checkdoc

test: version
	$(EMACS) --batch --directory . --load sesman-test.el --funcall ert-run-tests-batch-and-exit

version:
	@echo SESMAN: $(VERSION)
	@$(EMACS) --version

clean:
	rm -f $(OBJECTS)
