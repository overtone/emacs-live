[![Build Status](https://travis-ci.org/clojure-emacs/parseedn.svg?branch=master)](https://travis-ci.org/clojure-emacs/parseedn)

# EDN parser for Emacs Lisp

`parseedn` is an Emacs Lisp library for parsing [EDN
data](https://github.com/edn-format/edn). It uses
[`parseclj`](https://github.com/clojure-emacs/parseclj)'s shift-reduce parser
internally.

EDN and Emacs Lisp have some important differences that make translation from
one to the other not transparent (think representing an EDN map into Elisp, or
being able to differentiate between `false` and `nil` in Elisp).  Because of
this, `parseedn` takes certain decisions when parsing and transforming EDN data
into Elisp data types.  For more information please refer to [`parseclj`
DESIGN.md](https://github.com/clojure-emacs/parseclj/blob/master/DESIGN.md)
document.

Lastly, `parseedn` is in **alpha** stage, so its API is subject to change.

## Installation

Available on the major `package.el` community maintained repos -
[MELPA Stable][] and [MELPA][] repos.

MELPA Stable is the recommended repo as it has the latest stable
version.  MELPA has a development snapshot for users who don't mind
(infrequent) breakage but don't want to run from a git checkout.

You can install `parseedn` using the following command:

<kbd>M-x package-install [RET] parseedn [RET]</kbd>

or if you'd rather keep it in your dotfiles:

```el
(unless (package-installed-p 'parseedn)
  (package-install 'parseedn))
```

If the installation doesn't work try refreshing the package list:

<kbd>M-x package-refresh-contents</kbd>

[melpa]: http://melpa.org
[melpa stable]: http://stable.melpa.org

## Usage

- `parseedn-read`

    Read content from the current buffer as EDN and transforms it into an Emacs
    Lisp value.

- `parseedn-read-str` str

    Read STR as EDN and transfroms it into an Emacs Lisp value.

- `parseedn-print` datum

    Inserts DATUM as EDN Into the current buffer.  DATUM can be any Emacs Lisp
    value.

- `parseedn-print-str` datum

    Returns a string containing DATUM as EDN.  DATUM can be any Emacs Lisp
    value.

## Prior art

[edn.el](https://github.com/expez/edn.el) is an EDN-to-elisp parser based on the
PEG parser generator library.

## License

&copy; 2017-2021 Arne Brasseur

Distributed under the terms of the GNU General Public License 3.0 or later. See
[LICENSE](LICENSE).
