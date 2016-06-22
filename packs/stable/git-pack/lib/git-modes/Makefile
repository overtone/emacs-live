PREFIX  ?= /usr/local
LISPDIR ?= $(PREFIX)/share/emacs/site-lisp/git-modes

ELS  = git-modes.el
ELS += gitattributes-mode.el
ELS += gitconfig-mode.el
ELS += gitignore-mode.el
ELCS = $(ELS:.el=.elc)

LOADDEFS = git-modes-autoloads.el

EMACS_BIN ?= emacs
BATCH = @$(EMACS_BIN) --batch -Q

CP    ?= install -p -m 644
MKDIR ?= install -p -m 755 -d
RMDIR ?= rm -rf
SED   ?= sed

VERSION ?= $(shell test -e .git && git describe --tags --dirty 2> /dev/null)
ifeq "$(VERSION)" ""
  VERSION = 1.2.1
endif

.PHONY: install clean

lisp: $(ELCS) $(LOADDEFS)

install: lisp
	@printf "Installing...\n"
	@$(MKDIR) $(DESTDIR)$(LISPDIR)
	@$(CP) $(ELS) $(ELCS) $(DESTDIR)$(LISPDIR)

clean:
	@printf "Cleaning...\n"
	@$(RM) $(ELCS) $(LOADDEFS)

%.elc: %.el
	@printf "Compiling $<...\n"
	@$(BATCH) -L . -f batch-byte-compile $<

define LOADDEFS_TMPL
;;; $(LOADDEFS) --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name \
(or (file-name-directory #$$) (car load-path))))

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; $(LOADDEFS) ends here
endef
export LOADDEFS_TMPL
#'

$(LOADDEFS): $(ELS)
	@printf "Generating $@\n"
	@printf "%s" "$$LOADDEFS_TMPL" > $@
	@$(BATCH) --eval "(progn\
	(setq make-backup-files nil)\
	(setq vc-handled-backends nil)\
	(setq default-directory (file-truename default-directory))\
	(setq generated-autoload-file (expand-file-name \"$@\"))\
	(setq find-file-visit-truename t)\
	(update-directory-autoloads default-directory))"
