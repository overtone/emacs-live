-include .config.mk

PKG = git-modes

ELS   = $(PKG).el
ELS  += gitattributes-mode.el
ELS  += gitconfig-mode.el
ELS  += gitignore-mode.el
ELCS  = $(ELS:.el=.elc)

DEPS  =

EMACS      ?= emacs
EMACS_ARGS ?=

LOAD_PATH  ?= $(addprefix -L ../,$(DEPS))
LOAD_PATH  += -L .

PREFIX ?= /usr/local

all: lisp

help:
	$(info make all          - generate byte-code and autoloads)
	$(info make lisp         - generate byte-code and autoloads)
	$(info make clean        - remove generated files)
	$(info make install      - install in $(PREFIX))
	@printf "\n"

lisp: $(ELCS) loaddefs

loaddefs: $(PKG)-autoloads.el

%.elc: %.el
	@printf "Compiling $<\n"
	@$(EMACS) -Q --batch $(EMACS_ARGS) $(LOAD_PATH) -f batch-byte-compile $<

CLEAN  = $(ELCS) $(PKG)-autoloads.el

clean:
	@printf "Cleaning...\n"
	@rm -rf $(CLEAN)

define LOADDEFS_TMPL
;;; $(PKG)-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name \
(or (file-name-directory #$$) (car load-path))))

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; $(PKG)-autoloads.el ends here
endef
export LOADDEFS_TMPL
#'

$(PKG)-autoloads.el: $(ELS)
	@printf "Generating $@\n"
	@printf "%s" "$$LOADDEFS_TMPL" > $@
	@$(EMACS) -Q --batch --eval "(progn\
	(setq make-backup-files nil)\
	(setq vc-handled-backends nil)\
	(setq default-directory (file-truename default-directory))\
	(setq generated-autoload-file (expand-file-name \"$@\"))\
	(setq find-file-visit-truename t)\
	(update-directory-autoloads default-directory))"

CP      ?= install -p -m 644
MKDIR   ?= install -p -m 755 -d
LISPDIR ?= $(PREFIX)/share/emacs/site-lisp/git-modes

.PHONY: install
install: lisp
	@printf "Installing...\n"
	@$(MKDIR) $(DESTDIR)$(LISPDIR)
	@$(CP) $(ELS) $(ELCS) $(DESTDIR)$(LISPDIR)
