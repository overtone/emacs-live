# s.el [![Build Status](https://secure.travis-ci.org/magnars/s.el.png)](http://travis-ci.org/magnars/s.el) [![Coverage Status](https://coveralls.io/repos/magnars/s.el/badge.svg?branch=master)](https://coveralls.io/r/magnars/s.el?branch=master)

The long lost Emacs string manipulation library.

## Installation

It's available on [marmalade](http://marmalade-repo.org/) and [Melpa](https://melpa.org/):

    M-x package-install s

Or you can just dump `s.el` in your load path somewhere.

## Functions

[[ function-list ]]

## Documentation and examples

[[ function-docs ]]

## What's with the built-in wrappers?

Imagine looking through the function list and seeing `s-ends-with?`, but
`s-starts-with?` is nowhere to be found. Why? Well, because Emacs already has
`string-prefix-p`. Now you're starting out slightly confused, then have to go
somewhere else to dig for the command you were looking for.

The wrapping functions serve as both documentation for existing functions and
makes for a consistent API.

## Other string related libraries

* [inflections](https://github.com/eschulte/jump.el/blob/master/inflections.el) package
provides functions for strings pluralization and singularization.

* [levenshtein](http://emacswiki.org/emacs/levenshtein.el) package provides a function to
calculate the Levenshtein distance between two strings.

* [string-utils](https://github.com/rolandwalker/string-utils) is another general string manipulation library.

## Changelist

### From 1.11.0 to 1.12.0

- Alias all functions ending in `?` (Tianxiang Xiong)
- Add `s-blank-str?` (Aborn Jiang)
- Several bugfixes

### From 1.10.0 to 1.11.0

- Add `s-matched-positions-all` (ono hiroko)

### From 1.9.0 to 1.10.0

- Add `s-wrap` (Johan Andersson)
- Add `s-split-up-to` (Matus Goljer)
- Fix `s-reverse` for Unicode combining characters. (Christopher Wellons)

### From 1.8.0 to 1.9.0

- Add `s-count-matches` (Lars Andersen)

### From 1.7.0 to 1.8.0

- Add `s-present?` and `s-present?` (Johan Andersson)
- Better handling of international characters

### From 1.6.0 to 1.7.0

- Add `s-word-initials` (Sylvain Rousseau)
- Better handling of camel cased strings (@Bruce-Connor)

### From 1.5.0 to 1.6.0

- Add `s-pad-left` and `s-pad-right`
- Bugfixes for `s-format` (Nic Ferrier)

### From 1.4.0 to 1.5.0

- Add `s-all-match-strings` (Geoff Gole)
- Add `s-lex-format` (Nic Ferrier)

### From 1.3.1 to 1.4.0

- Add `s-capitalized?`
- Add `s-replace-all`
- Add `s-slice-at`
- Add `s-split` alias for `split-string` (Rüdiger Sonderfeld)
- Add `s-less?` predicate (Rüdiger Sonderfeld)
- Add START parameter to `s-matches?` (Rüdiger Sonderfeld)
- Bugfixes

### From 1.3.0 to 1.3.1

- Add `s-numeric?`
- Add `s-match` (Arthur Andersen)
- Add `s-format` (Nic Ferrier)
- Move .el files out of root to avoid problems with require.

### From 1.2.1 to 1.3.0

- **Breaking change:** `s-capitalize` now converts the first word's first
  character to upper case and the rest to lower case. `s-titleize`
  works like the old `s-capitalize` and capitalizes each word.
  (Johan Andersson)

- `s-capitalized-words` and `s-titleized-words` mirror this change.

## Contributors

* [Arthur Andersen](https://github.com/leoc) contributed `s-match`
* [Rolando](https://github.com/rolando2424) contributed `s-shared-start` and `s-shared-end`
* [Johan Andersson](https://github.com/rejeep) contributed `s-presence`, `s-present?` and fixed `s-titleize` vs `s-capitalize`
* [Nic Ferrier](https://github.com/nicferrier) added `s-format` and `s-lex-format`
* [Rüdiger Sonderfeld](https://github.com/ruediger) contributed `s-less?`, `s-split` and several bugfixes.
* [Geoff Gole](https://github.com/gsg) contributed `s-all-match-strings`
* [Sylvain Rousseau](https://github.com/thisirs) contributed `s-word-initials`
* [Lars Andersen](https://github.com/expez) contributed `s-count-matches`
* [ono hiroko](https://github.com/kuanyui) contributed `s-matched-positions-all`

Thanks!

## Contribute

Yes, please do. Pure functions in the string manipulation realm only,
please. There's a suite of tests in `dev/examples.el`, so remember to add
tests for your function, or I might break it later.

You'll find the repo at:

    https://github.com/magnars/s.el

Run the tests with

    ./run-tests.sh

Create the docs with

    ./create-docs.sh

I highly recommend that you install these as a pre-commit hook, so that
the tests are always running and the docs are always in sync:

    cp pre-commit.sh .git/hooks/pre-commit

Oh, and don't edit `README.md` directly, it is auto-generated.
Change `readme-template.md` or `examples-to-docs.el` instead.

## License

Copyright (C) 2012-2015 Magnar Sveen

Authors: Magnar Sveen <magnars@gmail.com>
Keywords: strings

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
