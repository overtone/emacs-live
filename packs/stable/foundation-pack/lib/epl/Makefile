EMACS ?= emacs
CASK ?= cask
TESTARGS =

PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

# Export the used EMACS to recipe environments
export EMACS

SRCS = epl.el
OBJECTS = $(SRCS:.el=.elc)

.PHONY: compile
compile: $(OBJECTS)

.PHONY: clean
clean:
	rm -rf $(OBJECTS)

.PHONY: test
test: compile
	$(CASK) exec ert-runner $(TESTARGS)

$(PKGDIR):
	$(CASK) install
	touch $(PKGDIR)

%.elc: %.el $(PKGDIR)
	$(CASK) exec $(EMACS) -Q --batch -f batch-byte-compile $<
