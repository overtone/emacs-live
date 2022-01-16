TOP := $(dir $(lastword $(MAKEFILE_LIST)))

PKG = with-editor

ELS   = $(PKG).el
ELCS  = $(ELS:.el=.elc)

DEPS  = dash

DOMAIN      ?= magit.vc
CFRONT_DIST ?= E2LUHBKU1FBV02

VERSION ?= $(shell test -e $(TOP).git && git describe --tags --abbrev=0 | cut -c2-)

EMACS      ?= emacs
EMACS_ARGS ?=
EMACS_ARGS += --eval '(setq with-editor-emacsclient-executable nil)'

LOAD_PATH  ?= $(addprefix -L ../../,$(DEPS))
LOAD_PATH  += -L .

ifndef ORG_LOAD_PATH
ORG_LOAD_PATH  = -L ../../org/lisp
endif

INSTALL_INFO     ?= $(shell command -v ginstall-info || printf install-info)
MAKEINFO         ?= makeinfo
MANUAL_HTML_ARGS ?= --css-ref /assets/page.css

GITSTATS_DIR  ?= $(TOP)docs/stats
GITSTATS_ARGS ?= -c style=https://magit.vc/assets/stats.css -c max_authors=999
