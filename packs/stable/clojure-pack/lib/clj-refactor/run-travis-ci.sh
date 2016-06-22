#!/bin/sh -e

cd "$(dirname "$0")"

ECUKES_EMACS=${EMACS:-$(which emacs)}
export ECUKES_EMACS

echo "*** Emacs version ***"
echo "ECUKES_EMACS = $ECUKES_EMACS"
"$ECUKES_EMACS" --version
echo

cask exec emacs -batch -Q -L . -eval "(progn (setq byte-compile-error-on-warn t) (batch-byte-compile))" clj-refactor.el
exec ./run-tests.sh $TAGS
