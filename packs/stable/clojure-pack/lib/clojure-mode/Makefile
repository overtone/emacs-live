CASK = cask
export EMACS ?= emacs
EMACSFLAGS =

PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

SRCS = $(wildcard *.el)
OBJS = $(SRCS:.el=.elc)

.PHONY: compile test clean elpa

all: compile

elpa-$(EMACS):
	$(CASK) install
	$(CASK) update
	touch $@

elpa: elpa-$(EMACS)

elpaclean:
	rm -f elpa*
	rm -rf .cask # Clean packages installed for development

compile: elpa
	$(CASK) build

clean:
	rm -f $(OBJS) clojure-mode-autoloads.el

test: $(PKGDIR)
	$(CASK) exec buttercup

test-checks:
	$(CASK) exec $(EMACS) --no-site-file --no-site-lisp --batch \
		-l test/test-checks.el ./

test-bytecomp: $(SRCS:.el=.elc-test)

%.elc-test: %.el elpa
	$(CASK) exec $(EMACS) --no-site-file --no-site-lisp --batch \
		-l test/clojure-mode-bytecomp-warnings.el $<
