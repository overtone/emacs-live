# -*- Makefile -*-

EMACS = emacs

# Compile with noninteractive and relatively clean environment.
BATCHFLAGS = -batch -q --no-site-file

SRCS = js2-mode.el js2-imenu-extras.el

OBJS = $(SRCS:.el=.elc)

%.elc: %.el
	${EMACS} $(BATCHFLAGS) -f batch-byte-compile $^

all: $(OBJS)

clean:
	-rm -f $(OBJS)

# custom build (require loads)
js2-imenu-extras.elc: js2-mode.elc
	${EMACS} $(BATCHFLAGS) -l ./js2-mode.elc -f batch-byte-compile $*.el

test:
	${EMACS} $(BATCHFLAGS) -l js2-mode.el -l tests/parser.el\
	  -l tests/indent.el -l tests/externs.el -f ert-run-tests-batch-and-exit
