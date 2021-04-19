#!/bin/sh
set -o errexit
set -o nounset

if [ "$EMACS_VERSION" = '23.4' ]; then
    curl -O 'https://raw.githubusercontent.com/ohler/ert/c619b56c5bc6a866e33787489545b87d79973205/lisp/emacs-lisp/ert.el'
fi

EMACS="${EMACS:=emacs}"

"$EMACS" -Q -batch --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile rainbow-delimiters.el
"$EMACS" -Q -batch -l rainbow-delimiters-test.el -f ert-run-tests-batch-and-exit
