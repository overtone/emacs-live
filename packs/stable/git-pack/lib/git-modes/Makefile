PREFIX  ?= /usr/local
LISPDIR ?= $(PREFIX)/share/emacs/site-lisp/git-modes

ELS  = gitattributes-mode.el
ELS += gitconfig-mode.el
ELS += gitignore-mode.el
ELCS = $(ELS:.el=.elc)
ELMS = $(ELS:%.el=marmalade/%-$(VERSION).el)

EMACS_BIN ?= emacs

CP    ?= install -p -m 644
MKDIR ?= install -p -m 755 -d
RMDIR ?= rm -rf
SED   ?= sed

VERSION ?= $(shell test -e .git && git describe --tags --dirty 2> /dev/null)
ifeq "$(VERSION)" ""
  VERSION = 1.2.0
endif

.PHONY: install clean marmalade-upload

lisp: $(ELCS)

install: lisp
	@printf "Installing...\n"
	@$(MKDIR) $(DESTDIR)$(LISPDIR)
	@$(CP) $(ELS) $(ELCS) $(DESTDIR)$(LISPDIR)

clean:
	@printf "Cleaning...\n"
	@$(RM) $(ELCS)
	@$(RMDIR) marmalade

%.elc: %.el
	@$(EMACS_BIN) -batch -Q -f batch-byte-compile $<

marmalade-upload: marmalade
	@marmalade-upload $(ELMS)
	@$(RMDIR) marmalade
marmalade: $(ELMS)
$(ELMS): marmalade/%-$(VERSION).el: %.el
	@$(MKDIR) -p marmalade
	@$(CP) $< $@
	@$(SED) -e "/^;; Keywords:/a;; Package-Version: $(VERSION)" -i $@
