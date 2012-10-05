.PHONY: test test-nw test-emacs23 test-emacs23-nw

test:
	emacs -Q -L . -l test/popwin-test.el

test-nw:
	emacs -Q -nw -L . -l test/popwin-test.el

test-emacs23: test/ert.el
	emacs23 -Q -L . -l test/ert.el -l test/popwin-test.el

test-emacs23-nw: test/ert.el
	emacs23 -Q -nw -L . -l test/ert.el -l test/popwin-test.el

test/ert.el:
	wget http://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/emacs-lisp/ert.el -O $@
