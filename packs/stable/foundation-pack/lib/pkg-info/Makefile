EMACS ?= emacs
EMACSFLAGS =
CASK = cask
TESTARGS =
PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

export EMACS

SRCS = pkg-info.el
OBJECTS = $(SRCS:.el=.elc)

.PHONY: compile
compile : $(OBJECTS)

.PHONY: clean
clean :
	rm -rf $(OBJECTS)

.PHONY: test
test : compile
	$(CASK) exec ert-runner $(TESTARGS)

.PHONY: start-server
start-server :
	rm -f servant/tmp/servant.log
	$(CASK) exec servant start > servant/tmp/servant.log 2>&1 &

.PHONY: stop-server
stop-server :
	$(CASK) exec servant stop

%.elc : %.el $(PKGDIR)
	$(CASK) exec $(EMACS) -Q -l compat/load.el --batch $(EMACSFLAGS) -f batch-byte-compile $<

$(PKGDIR) : Cask
	$(CASK) install
	touch $(PKGDIR)
