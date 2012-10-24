EMACS=emacs
EMACS23=emacs23

.PHONY: test test-nw test-emacs23 test-emacs23-nw travis-ci

test:
	${EMACS} -Q -L . -l tests/run-test.el

test-nw:
	${EMACS} -Q -nw -L . -l tests/run-test.el

test-emacs23: tests/ert.el
	${EMACS23} -Q -L . -l test/ert.el -l tests/run-test.el

test-emacs23-nw: tests/ert.el
	${EMACS23} -Q -nw -L . -l test/ert.el -l tests/run-test.el

travis-ci:
	${EMACS} -batch -Q -l tests/run-test.el

tests/ert.el:
	wget "http://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/emacs-lisp/ert.el" -O $@
