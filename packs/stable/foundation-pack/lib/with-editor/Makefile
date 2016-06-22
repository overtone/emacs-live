ELS = with-editor.el

DEPS = dash

INFOPAGES = with-editor.info

ELCS    = $(ELS:.el=.elc)
DFLAGS  = $(addprefix -L ../,$(DEPS))
EFLAGS ?= $(DFLAGS)
EMACS  ?= emacs
BATCH   = $(EMACS) -batch -Q -L . $(EFLAGS)

MAKEINFO     ?= makeinfo
INSTALL_INFO ?= $(shell command -v ginstall-info || printf install-info)

.PHONY: help clean AUTHORS.md

help:
	$(info make all       - compile elisp and manual)
	$(info make lisp      - compile elisp)
	$(info make info      - generate info manual)
	$(info make authors   - generate AUTHORS.md)
	$(info make clean     - remove generated files)
	@printf "\n"

all: lisp info

lisp: $(ELCS)
%.elc: %.el
	@printf "Compiling %s\n" $<
	@$(BATCH)\
	  --eval '(setq with-editor-emacsclient-executable nil)'\
	  -f batch-byte-compile $<

info: $(INFOPAGES) dir
%.info: %.texi
	@printf "Generating $@\n"
	@$(MAKEINFO) --no-split $< -o $@

dir: $(INFOPAGES)
	@printf "Generating dir\n"
	@echo $^ | xargs -n 1 $(INSTALL_INFO) --dir=$@

authors: AUTHORS.md
AUTHORS.md:
	@ printf "Authors\n=======\n\n" > $@
	@ ( printf "%s\n" "- Barak A. Pearlmutter <barak+git@pearlmutter.net>" && \
	    printf "%s\n" "- Lele Gaifax <lele@metapensiero.it>" && \
	    printf "%s\n" "- Rémi Vanicat <vanicat@debian.org>" && \
	    git log --pretty=format:'- %aN <%aE>' \
	  ) | sort -u >> $@

clean:
	@printf "Cleaning...\n"
	@rm -f $(ELCS)
