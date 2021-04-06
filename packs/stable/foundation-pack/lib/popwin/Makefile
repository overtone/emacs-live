EMACS=emacs

.PHONY: byte-compile clean test test-nw

byte-compile: popwin.el
	$(EMACS) -Q --batch -f batch-byte-compile popwin.el

clean:
	rm -f popwin.elc test/ert.el

test: byte-compile test/ert.el
	$(EMACS) -Q -l popwin.el -l test/ert.el -l test/popwin-test.el

test-nw: byte-compile
	$(EMACS) -Q -nw -l popwin.el -l test/ert.el -l test/popwin-test.el

test/ert.el:
	wget https://raw.github.com/ohler/ert/c619b56c5bc6a866e33787489545b87d79973205/lisp/emacs-lisp/ert.el -O $@
