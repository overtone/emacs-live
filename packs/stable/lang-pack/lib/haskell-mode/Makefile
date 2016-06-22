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
VERSION = 13.20

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
EMACS := $(shell which "$${EMACS}" || which "emacs")
EMACS_VERSION := $(shell "$(EMACS)" -Q --batch --eval '(princ emacs-version)')

EFLAGS = --eval "(add-to-list 'load-path (expand-file-name \"tests/compat\") 'append)" \
	 --eval "(when (< emacs-major-version 24) \
		    (setq byte-compile-warnings '(not cl-functions)))" \
	 --eval '(setq byte-compile-error-on-warn t)' \
	 --eval '(when (not (version< emacs-version "24.4")) (setq load-prefer-newer t))' \
	 --eval '(defun byte-compile-dest-file (filename) \
                    (concat (file-name-directory filename) "build-" emacs-version "/" \
                            (file-name-nondirectory filename) "c"))'

BATCH = $(EMACS) $(EFLAGS) --batch -Q -L .

ELFILES := $(filter-out haskell-mode-autoloads.el haskell-mode-pkg.el,$(wildcard *.el))

ELCHECKS := $(wildcard tests/*-tests.el)

AUTOLOADS = haskell-mode-autoloads.el

PKG_DIST_FILES = $(ELFILES) logo.svg NEWS haskell-mode.info dir

.PHONY: all compile info clean check check-emacs-version

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

compile: build-$(EMACS_VERSION)

build-$(EMACS_VERSION) : $(ELFILES)
	if [ ! -d $@ ]; then mkdir $@; fi
	$(BATCH) -f batch-byte-compile-if-not-done $^

check-%: tests/%-tests.el
	$(BATCH) -l "$<" -f ert-run-tests-batch-and-exit;

check: $(ELCHECKS) build-$(EMACS_VERSION)
	$(BATCH) --eval "(when (= emacs-major-version 24)					\
                           (require 'undercover)						\
                           (undercover \"*.el\"							\
                              (:exclude \"haskell-mode-pkg.el\" \"haskell-compat.el\")))"	\
                 -L tests									\
                 $(patsubst %,-l %,$(ELCHECKS))							\
                 -f ert-run-tests-batch-and-exit
	@TAB=$$(echo "\t"); \
	if grep -Hn "[ $${TAB}]\+\$$" *.el tests/*.el; then \
	    echo "Error: Files contain whitespace at the end of lines" >&2; \
	    exit 3; \
	fi; \
	if grep -Hn "[$${TAB}]" *.el tests/*.el; then \
	    echo "Error: Tab character is not allowed" >&2; \
	    exit 3; \
	fi
	@echo "checks passed!"

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
		--eval '(setq generated-autoload-file "$(CURDIR)/$@")' \
		-f batch-update-autoloads "."
	# check if autoloads will really load
	$(BATCH) -l "$@"
