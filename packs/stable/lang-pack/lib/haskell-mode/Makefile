#
# Note: Due to MELPA distributing directly from github source version
# needs to be embedded in files as is without proprocessing.
#
# Version string is present in:
# - Makefile
# - haskell-mode.el
# - haskell-mode.texi
#
# We should have a script that changes it everywhere it is needed and
# syncs it with current git tag.
#
VERSION = 13.15-git

INSTALL_INFO = install-info

# Use $EMACS environment variable if present, so that all of these are
# equivalent:
#
# 1.  export EMACS=/path/to/emacs && make
# 2.  EMACS=/path/to/emacs make
# 3.  make EMACS=/path/to/emacs
#
# This is particularly useful when EMACS is set in ~/.bash_profile
#
EMACS := $(shell echo "$${EMACS:-emacs}")

EFLAGS = --eval "(add-to-list 'load-path (expand-file-name \"tests/compat\") 'append)" \
	 --eval "(when (< emacs-major-version 24) \
		    (setq byte-compile-warnings '(not cl-functions)))" \
	 --eval '(setq byte-compile-error-on-warn t)'

BATCH = $(EMACS) $(EFLAGS) --batch -Q -L .

ELFILES = \
	ghc-core.el \
	ghci-script-mode.el \
	highlight-uses-mode.el \
	haskell-align-imports.el \
	haskell-bot.el \
	haskell-cabal.el \
	haskell-checkers.el \
	haskell-collapse.el \
	haskell-modules.el \
	haskell-sandbox.el \
	haskell-commands.el \
	haskell-compat.el \
	haskell-compile.el \
	haskell-complete-module.el \
	haskell-completions.el \
	haskell-customize.el \
	haskell-debug.el \
	haskell-decl-scan.el \
	haskell-doc.el \
	haskell.el \
	haskell-font-lock.el \
	haskell-indentation.el \
	haskell-indent.el \
	haskell-interactive-mode.el \
	haskell-load.el \
	haskell-menu.el \
	haskell-mode.el \
	haskell-move-nested.el \
	haskell-navigate-imports.el \
	haskell-package.el \
	haskell-presentation-mode.el \
	haskell-process.el \
	haskell-repl.el \
	haskell-session.el \
	haskell-simple-indent.el \
	haskell-sort-imports.el \
	haskell-string.el \
	haskell-unicode-input-method.el \
	haskell-utils.el \
	inf-haskell.el

ELCFILES = $(ELFILES:.el=.elc)
AUTOLOADS = haskell-mode-autoloads.el

PKG_DIST_FILES = $(ELFILES) logo.svg NEWS haskell-mode.info dir
ELCHECKS=$(addprefix check-, $(ELFILES:.el=))

%.elc: %.el
	@$(BATCH) \
		 -f batch-byte-compile $*.el

.PHONY: all compile info clean check $(ELCHECKS) elpa package check-emacs-version

all: check-emacs-version compile $(AUTOLOADS) info

check-emacs-version :
	@$(BATCH) --eval "(when (< emacs-major-version 23)					\
                            (message \"Error: haskell-mode requires Emacs 23 or later\")	\
                            (message \"Your version of Emacs is %s\" emacs-version)		\
                            (message \"Found as '$(EMACS)'\")					\
                            (message \"Use one of:\")						\
                            (message \"   1.  export EMACS=/path/to/emacs && make\")		\
                            (message \"   2.  EMACS=/path/to/emacs make\")			\
                            (message \"   3.  make EMACS=/path/to/emacs\")			\
                            (kill-emacs 2))"

compile: $(ELCFILES)

$(ELCHECKS): check-%: %.el %.elc
	@$(BATCH) --eval '(when (check-declare-file "$*.el") (error "check-declare failed"))'
	@if [ -f "$(<:%.el=tests/%-tests.el)" ]; then \
		$(BATCH) -l "$(<:%.el=tests/%-tests.el)" -f ert-run-tests-batch-and-exit; \
	fi
	@echo "--"

check: $(ELCHECKS)
	@echo "checks passed!"

clean:
	$(RM) $(ELCFILES) $(AUTOLOADS) $(AUTOLOADS:.el=.elc) haskell-mode.info dir

info: haskell-mode.info dir

dir: haskell-mode.info
	$(INSTALL_INFO) --dir=$@ $<

haskell-mode.info: doc/haskell-mode.texi
	LANG=en_US.UTF-8 $(MAKEINFO) $(MAKEINFO_FLAGS) -o $@ $<

doc/haskell-mode.html: doc/haskell-mode.texi doc/haskell-mode.css
	LANG=en_US.UTF-8 $(MAKEINFO) $(MAKEINFO_FLAGS) --html --css-include=doc/haskell-mode.css --no-split -o $@ $<

doc/html/index.html : doc/haskell-mode.texi
	if [ -e doc/html ]; then rm -r doc/html; fi
	LANG=en_US.UTF-8 $(MAKEINFO) $(MAKEINFO_FLAGS) --html				\
	    --css-ref=haskell-mode.css							\
	    -c AFTER_BODY_OPEN='<div class="background"> </div>'			\
	    -c EXTRA_HEAD='<link rel="shortcut icon" href="haskell-mode-32x32.png">'	\
	    -c SHOW_TITLE=0								\
	    -o doc/html $<

doc/html/haskell-mode.css : doc/haskell-mode.css doc/html/index.html
	cp $< $@

doc/html/haskell-mode.svg : images/haskell-mode.svg doc/html/index.html
	cp $< $@

doc/html/haskell-mode-32x32.png : images/haskell-mode-32x32.png doc/html/index.html
	cp $< $@

doc/html : doc/html/index.html doc/html/haskell-mode.css doc/html/haskell-mode.svg doc/html/haskell-mode-32x32.png

deploy-manual : doc/html
	cd doc && ./deploy-manual.sh

$(AUTOLOADS): $(ELFILES) haskell-mode.elc
	$(BATCH) \
		--eval '(setq make-backup-files nil)' \
		--eval '(setq generated-autoload-file "$(CURDIR)/$@")' \
		-f batch-update-autoloads "."
