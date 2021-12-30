# -*- Makefile -*-

EMACS = emacs

# Compile with noninteractive and relatively clean environment.
BATCHFLAGS = -batch -Q

SRCS = js2-mode.el js2-imenu-extras.el

TESTS = $(wildcard tests/*.el)

OBJS = $(SRCS:.el=.elc) $(TESTS:.el=.elc)

%.elc: %.el
	${EMACS} $(BATCHFLAGS) -L . -f batch-byte-compile $^

all: $(OBJS)

clean:
	-rm -f $(OBJS)

test:	all
	${EMACS} $(BATCHFLAGS) -L . \
	  $(addprefix -l ,$(OBJS)) \
	  -f ert-run-tests-batch-and-exit
