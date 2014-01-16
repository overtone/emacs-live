EMACS = emacs
EMACSFLAGS =
BYTECOMPILEFORM = (progn \
	(setq byte-compile-dest-file-function (lambda (fn) "$@")) \
	(byte-compile-file "$<"))

OBJECTS = git-commit-mode.elc gitconfig-mode.elc gitignore-mode.elc

.PHONY: build
build : $(OBJECTS)

.PHONY: clean
clean :
	rm -f $(OBJECTS)

%.elc : %.el
	$(EMACS) -Q --batch $(EMACSFLAGS) -f batch-byte-compile $<
