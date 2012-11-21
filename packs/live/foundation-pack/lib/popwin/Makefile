EMACS=emacs
EMACS23=emacs23

.PHONY: test test-nw test-emacs23 test-emacs23-nw

compile: popwin.el
	$(EMACS) --batch -f batch-byte-compile popwin.el

test:
	$(EMACS) -Q -L . -l test/popwin-test.el

test-nw:
	$(EMACS) -Q -nw -L . -l test/popwin-test.el

test-emacs23: test/ert.el
	$(EMACS23) -Q -L . -l test/ert.el -l test/popwin-test.el

test-emacs23-nw: test/ert.el
	$(EMACS23) -Q -nw -L . -l test/ert.el -l test/popwin-test.el

test/ert.el:
	wget http://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/emacs-lisp/ert.el -O $@
