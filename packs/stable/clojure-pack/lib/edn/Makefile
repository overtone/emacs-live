EMACS ?= emacs
CASK ?= cask

test: elpa
	${CASK} exec ${EMACS} -Q -batch -L . -L tests \
	-l tests/edn-tests.el \
	-f ert-run-tests-batch-and-exit

elpa:
	mkdir -p elpa
	${CASK} install 2> elpa/install.log

clean-elpa:
	rm -rf elpa

clean-elc:
	rm -f *.elc tests/*.elc

clean: clean-elpa clean-elc

print-deps:
	${EMACS} --version
	@echo CASK=${CASK}

travis-ci: print-deps test
