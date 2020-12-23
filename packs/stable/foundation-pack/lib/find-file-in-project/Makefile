SHELL = /bin/sh
EMACS ?= emacs
PROFILER =

.PHONY: test

# Delete byte-compiled files etc.
clean:
	rm -f *~
	rm -f \#*\#
	rm -f *.elc

# Run tests.
test: clean
	! $(EMACS) -Q -batch --eval "(defun ivy-read (a1 a2 &optional a3 a4))" -l find-file-in-project.el --eval "(byte-compile-file \"find-file-in-project.el\")" 2>&1 | grep "Warning: the function"
	$(EMACS) -Q -batch -l ert -l find-file-in-project.el -l tests/ffip-tests.el
