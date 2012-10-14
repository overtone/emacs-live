# -*- Makefile -*-

# Compile with noninteractive and relatively clean environment.
BATCHFLAGS = -batch -q --no-site-file

SRCS = js2-mode.el js2-imenu-extras.el

OBJS = $(SRCS:.el=.elc)

%.elc: %.el
	emacs $(BATCHFLAGS) -f batch-byte-compile $^

all: $(OBJS)

clean:
	-rm -f $(OBJS)

# custom build (require loads)
js2-imenu-extras.elc: js2-mode.elc
	emacs $(BATCHFLAGS) -l ./js2-mode.elc -f batch-byte-compile $*.el

test:
	emacs $(BATCHFLAGS) -l js2-mode.el -l tests/ast.el -f ert-run-tests-batch
