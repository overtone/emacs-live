VERSION = $(shell git describe --tags --match 'v[0-9]*' --abbrev=0 | sed 's/^v//;s/\.0*/./g')
GIT_VERSION = $(shell git describe --tags --match 'v[0-9]*' --long --dirty | sed 's/^v//')

INSTALL_INFO = install-info
EMACS = emacs
EFLAGS =
BATCH = $(EMACS) $(EFLAGS) --batch -Q -L .
SUBST_ATAT = sed -e 's/@@GIT_VERSION@@/$(GIT_VERSION)/g;s/@GIT_VERSION@/$(GIT_VERSION)/g;s/@@VERSION@@/$(VERSION)/g;s/@VERSION@/$(VERSION)/g'

ELFILES = \
	ghc-core.el \
	haskell-align-imports.el \
	haskell-c.el \
	haskell-cabal.el \
	haskell-checkers.el \
	haskell-compat.el \
	haskell-compile.el \
	haskell-decl-scan.el \
	haskell-doc.el \
	haskell-font-lock.el \
	haskell-indent.el \
	haskell-indentation.el \
	haskell-interactive-mode.el \
	haskell-menu.el \
	haskell-mode.el \
	haskell-move-nested.el \
	haskell-navigate-imports.el \
	haskell-package.el \
	haskell-process.el \
	haskell-session.el \
	haskell-show.el \
	haskell-simple-indent.el \
	haskell-sort-imports.el \
	haskell-string.el \
	haskell-str.el \
	haskell-unicode-input-method.el \
	haskell-utils.el \
	haskell-yas.el \
	haskell-presentation-mode.el \
	inf-haskell.el

ELCFILES = $(ELFILES:.el=.elc)
AUTOLOADS = haskell-mode-autoloads.el

PKG_DIST_FILES = $(ELFILES) logo.svg NEWS haskell-mode.info dir
PKG_TAR = haskell-mode-$(VERSION).tar
ELCHECKS=$(addprefix check-, $(ELFILES:.el=))

%.elc: %.el
	@$(BATCH) \
	   --eval "(byte-compile-disable-warning 'cl-functions)" \
       -f batch-byte-compile $<

.PHONY: all compile info clean check $(ELCHECKS) elpa package

all: compile $(AUTOLOADS) info

compile: $(ELCFILES)

$(ELCHECKS): check-%: %.el
	@$(BATCH) --eval '(when (check-declare-file "$*.el") (error "check-declare failed"))'
	@$(BATCH) \
	     --eval "(setq byte-compile-error-on-warn t)" \
	 	 --eval "(byte-compile-disable-warning 'cl-functions)" \
		 -f batch-byte-compile $*.el
	@$(RM) $*.elc
	@if [ -f "$(<:%.el=tests/%-tests.el)" ]; then \
	if $(BATCH) --eval "(require 'ert)" 2> /dev/null; then \
		echo; \
		$(BATCH) -l "$(<:%.el=tests/%-tests.el)" -f ert-run-tests-batch-and-exit; \
	else \
		echo "ERT not available, skipping unit tests"; \
	fi; \
	fi
	@echo "--"

check: clean $(ELCHECKS)
	@echo "checks passed!"

clean:
	$(RM) $(ELCFILES) $(AUTOLOADS) $(AUTOLOADS:.el=.elc) $(PKG_TAR) haskell-mode.tmp.texi haskell-mode.info dir

info: haskell-mode.info dir

dir: haskell-mode.info
	$(INSTALL_INFO) --dir=$@ $<

haskell-mode.tmp.texi: haskell-mode.texi
	$(SUBST_ATAT) < haskell-mode.texi > haskell-mode.tmp.texi

haskell-mode.info: haskell-mode.tmp.texi
	$(MAKEINFO) $(MAKEINFO_FLAGS) -o $@ $<

haskell-mode.html: haskell-mode.tmp.texi
	$(MAKEINFO) $(MAKEINFO_FLAGS) --html --no-split -o $@ $<

# Generate ELPA-compatible package
package: $(PKG_TAR)
elpa: $(PKG_TAR)

$(PKG_TAR): $(PKG_DIST_FILES) haskell-mode-pkg.el.in
	rm -rf haskell-mode-$(VERSION)
	mkdir haskell-mode-$(VERSION)
	cp $(PKG_DIST_FILES) haskell-mode-$(VERSION)/
	$(SUBST_ATAT) < haskell-mode-pkg.el.in > haskell-mode-$(VERSION)/haskell-mode-pkg.el
	$(SUBST_ATAT) < haskell-mode.el > haskell-mode-$(VERSION)/haskell-mode.el
	(sed -n -e '/^;;; Commentary/,/^;;;/p' | egrep '^;;( |$$)' | cut -c4-) < haskell-mode.el > haskell-mode-$(VERSION)/README
	tar cvf $@ haskell-mode-$(VERSION)
	rm -rf haskell-mode-$(VERSION)
	@echo
	@echo "Created ELPA compatible distribution package '$@' from $(GIT_VERSION)"

$(AUTOLOADS): $(ELFILES) haskell-mode.elc
	$(BATCH) \
		--eval '(setq make-backup-files nil)' \
		--eval '(setq generated-autoload-file "$(CURDIR)/$@")' \
		-f batch-update-autoloads "."

# HACK: embed version number into .elc file
haskell-mode.elc: haskell-mode.el
	$(SUBST_ATAT) < haskell-mode.el > haskell-mode.tmp.el
	@$(BATCH) --eval "(byte-compile-disable-warning 'cl-functions)" -f batch-byte-compile haskell-mode.tmp.el
	mv haskell-mode.tmp.elc haskell-mode.elc
	$(RM) haskell-mode.tmp.el
