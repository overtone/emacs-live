#
# Note: Due to MELPA distributing directly from github source version
# needs to be embedded in files as is without preprocessing.
#
# Version string is present in:
# - Makefile
# - haskell-mode.el
# - haskell-mode.texi
#
# We should have a script that changes it everywhere it is needed and
# syncs it with current git tag.
#
VERSION = 17.2-git

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
EMACS ?= emacs
EMACS_VERSION := $(shell "$(EMACS)" -Q --batch --eval '(princ emacs-version)')

EFLAGS = --eval "(when (boundp 'load-prefer-newer) (setq load-prefer-newer t))"

BATCH = @echo EMACS $@; "$(EMACS)" $(EFLAGS) --batch -Q -L .

ELFILES := $(filter-out haskell-mode-autoloads.el haskell-mode-pkg.el,$(wildcard *.el))

ELCHECKS := $(wildcard tests/*-tests.el)

AUTOLOADS = haskell-mode-autoloads.el

PKG_DIST_FILES = $(ELFILES) logo.svg NEWS haskell-mode.info dir

INIT_PACKAGES="(progn \
  (require 'package) \
  (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
  (package-initialize) \
  (dolist (pkg '(package-lint)) \
    (unless (package-installed-p pkg) \
      (unless (assoc pkg package-archive-contents) \
        (package-refresh-contents)) \
      (package-install pkg))) \
  )"


.PHONY: all compile info clean check check-emacs-version check-ert check-package-lint check-relint

all: check-emacs-version compile $(AUTOLOADS) info

check-emacs-version :
	$(BATCH) --eval "(when (version< emacs-version \"25.1\")				\
                            (message \"Error: haskell-mode requires Emacs 25.1 or later\")	\
                            (message \"Your version of Emacs is %s\" emacs-version)		\
                            (message \"Found as '$(EMACS)'\")					\
                            (message \"Use one of:\")						\
                            (message \"   1.  export EMACS=/path/to/emacs && make\")		\
                            (message \"   2.  EMACS=/path/to/emacs make\")			\
                            (message \"   3.  make EMACS=/path/to/emacs\")			\
                            (kill-emacs 2))"
	@echo Using EMACS = $(EMACS), version = $(EMACS_VERSION)

compile: build-$(EMACS_VERSION)/build-flag

build-$(EMACS_VERSION) :
	mkdir $@

# Emacs byte compilation state leaks from file to file if multiple
# files are requested to be build at the same time. We have to
# workaround this issue on Makefile level. Note also that we consider
# an .el file to be dependent on all other files because we do not do
# proper dependency tracking (yet).
build-$(EMACS_VERSION)/%.elc : %.el $(ELFILES)
	$(BATCH) --eval '(setq byte-compile-error-on-warn t)'						\
	         --eval "(setq byte-compile-dest-file-function (lambda (filename)					\
	               	       (concat (file-name-directory filename) \"build-\" emacs-version \"/\"	\
	                      	    (file-name-nondirectory filename) \"c\")))"				\
	         --eval "(when (check-declare-file \"$<\") (kill-emacs 2))" \
	         -f batch-byte-compile $<								\

build-$(EMACS_VERSION)/build-flag : build-$(EMACS_VERSION) $(patsubst %.el,build-$(EMACS_VERSION)/%.elc,$(ELFILES))
	touch $@

check-%: tests/%-tests.el
	$(BATCH) -l "$<" -f ert-run-tests-batch-and-exit;

check: compile $(AUTOLOADS) check-package-lint check-relint check-ert

check-ert: $(ELCHECKS)
	$(BATCH) -L tests									\
                 $(patsubst %,-l %,$(ELCHECKS))							\
                 -f ert-run-tests-batch-and-exit
	@echo "checks passed!"

# TODO: fix issues, then enforce build failure if this fails
check-package-lint:
	$(BATCH) --eval $(INIT_PACKAGES) --eval '(setq package-lint-main-file "haskell-mode-pkg.el")' -f package-lint-batch-and-exit $(ELFILES) || true

clean:
	$(RM) -r build-$(EMACS_VERSION) $(AUTOLOADS) $(AUTOLOADS:.el=.elc) haskell-mode.info dir

info: haskell-mode.info dir

dir: haskell-mode.info
	$(INSTALL_INFO) --dir=$@ $<

haskell-mode.info: doc/haskell-mode.texi
	LANG=en_US.UTF-8 $(MAKEINFO) $(MAKEINFO_FLAGS) -o $@ $<

doc/haskell-mode.html: doc/haskell-mode.texi doc/haskell-mode.css
	LANG=en_US.UTF-8 $(MAKEINFO) $(MAKEINFO_FLAGS) --html --css-include=doc/haskell-mode.css --no-split -o $@ $<
	$(BATCH) -l doc/haskell-manual-fixups.el -f haskell-manual-fixups-batch-and-exit $@

doc/html/index.html : doc/haskell-mode.texi
	if [ -e doc/html ]; then rm -r doc/html; fi
	mkdir doc/html
	cp -r doc/anim doc/html/anim
	LANG=en_US.UTF-8 $(MAKEINFO) $(MAKEINFO_FLAGS) --html				\
	    --css-ref=haskell-mode.css							\
	    -c AFTER_BODY_OPEN='<div class="background"> </div>'			\
	    -c EXTRA_HEAD='<link rel="shortcut icon" href="haskell-mode-32x32.png">'	\
	    -c SHOW_TITLE=0								\
	    -o doc/html $<
	$(BATCH) -l doc/haskell-manual-fixups.el -f haskell-manual-fixups-batch-and-exit doc/html/*.html

doc/html/haskell-mode.css : doc/haskell-mode.css doc/html/index.html
	cp $< $@

doc/html/haskell-mode.svg : images/haskell-mode.svg doc/html/index.html
	cp $< $@

doc/html/haskell-mode-32x32.png : images/haskell-mode-32x32.png doc/html/index.html
	cp $< $@

doc/html : doc/html/index.html			\
           doc/html/haskell-mode.css		\
           doc/html/haskell-mode.svg		\
           doc/html/haskell-mode-32x32.png

deploy-manual : doc/html
	cd doc && ./deploy-manual.sh

$(AUTOLOADS): $(ELFILES)
	$(BATCH) \
		--eval '(setq make-backup-files nil)' \
		--eval "(setq generated-autoload-file (concat command-line-default-directory \"/\" \"$@\"))" \
		-f batch-update-autoloads "."
# check if autoloads will really load
	$(BATCH) -l "$@"
