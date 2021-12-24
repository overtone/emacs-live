![GitHub Workflow Status](https://img.shields.io/github/workflow/status/magnars/dash.el/CI)
[![MELPA](https://melpa.org/packages/dash-badge.svg)](https://melpa.org/#/dash)
[![MELPA Stable](https://stable.melpa.org/packages/dash-badge.svg)](https://stable.melpa.org/#/dash)

# <img align="right" src="https://raw.github.com/magnars/dash.el/master/rainbow-dash.png"> dash.el

A modern list api for Emacs. No 'cl required.

## Installation

It's available on [GNU ELPA](https://elpa.gnu.org/) and
[MELPA](https://melpa.org/):

    M-x package-install dash

Or you can just dump `dash.el` in your load
path somewhere.

If you want the function combinators, then also:

    M-x package-install dash-functional

## Using in a package

Add this to the big comment block at the top:

    ;; Package-Requires: ((dash "[[ version ]]"))

To get function combinators:

    ;; Package-Requires: ((dash "[[ version ]]") (dash-functional "1.2.0") (emacs "24"))

## Upcoming breaking change!

- For backward compatibility reasons `-zip` return a cons-cell instead of a list
  with two elements when called on two lists. This is a clunky API, and in an
  upcoming 3.0 release of Dash it will always return a list. If you rely on the
  cons-cell return value, use `-zip-pair` instead.  During the 2.x
  release cycle the new API is available as `-zip-lists`.

## Fontification of special variables

Font lock of special Dash variables (`it`, `acc`, etc.) in Emacs Lisp
buffers can optionally be enabled with the autoloaded minor mode
`dash-fontify-mode`.  In older Emacs versions which do not dynamically
detect macros, the minor mode also fontifies Dash macro calls.

To automatically enable the minor mode in all Emacs Lisp buffers, just
call its autoloaded global counterpart `global-dash-fontify-mode`,
either interactively or from your `user-init-file`:

```el
(global-dash-fontify-mode)
```

## Functions

All functions and constructs in the library are prefixed with a dash (-).

There are also anaphoric versions of functions where that makes sense,
prefixed with two dashes instead of one.

While `-map` takes a function to map over the list, you can also use
the anaphoric form with double dashes - which will then be executed
with `it` exposed as the list item. Here's an example:

```el
(-map (lambda (n) (* n n)) '(1 2 3 4)) ;; normal version

(--map (* it it) '(1 2 3 4)) ;; anaphoric version
```

of course the original can also be written like

```el
(defun square (n) (* n n))

(-map 'square '(1 2 3 4))
```

which demonstrates the usefulness of both versions.

[[ function-list ]]

[[ function-docs ]]

## Contribute

Yes, please do. Pure functions in the list manipulation realm only,
please. There's a suite of tests in `dev/examples.el`, so remember to add
tests for your function, or I might break it later.

You'll find the repo at:

    https://github.com/magnars/dash.el

Run the tests with

    ./run-tests.sh

Create the docs with

    ./create-docs.sh

I highly recommend that you install these as a pre-commit hook, so that
the tests are always running and the docs are always in sync:

    cp pre-commit.sh .git/hooks/pre-commit

Oh, and don't edit `README.md` directly, it is auto-generated.
Change `readme-template.md` or `examples-to-docs.el` instead.

## Changelist

### From 2.16 to 2.17

- Speed up `-uniq` by using hash-tables when possible (@cireu, #305)
- Fix `-inits` to be non-destructive (@SwiftLawnGnome, #313)
- Fix indent rules for `-some->` and family (@wbolster, #321)
- Add `-zip-lists` which always returns list of lists, even for two
  input lists (see issue #135).

### From 2.15 to 2.16

- Added `--doto`, anaphoric version of `-doto` (#282)
- Aliased `-cons-pair-p` to `-cons-pair?`(#288)
- Generalized `-rotate` for |n| greater than the length of the list (@leungbk, #290)
- Added a mechanism to extend destructuring with custom matchers (@yyoncho, #277)

### From 2.14 to 2.15

This release brings new destructuring features, some new control flow
functions and performance optimizations.

- Added `-setq` with destructuring binding support similar to `-let` family ([#116](https://github.com/magnars/dash.el/issues/116))
- Added smarter key destructuring in `-let` and friends where variables are auto-derived from keys ([#111](https://github.com/magnars/dash.el/issues/111))
- Allow `-let` bindings with place only ([#256](https://github.com/magnars/dash.el/issues/256))
- Added `-each-r` and `-each-r-while` (@doublep, [#159](https://github.com/magnars/dash.el/issues/159))
- Added `-common-suffix` (@basil-conto, [#263](https://github.com/magnars/dash.el/issues/263))
- Improved performance of folds (`-reduce` and friends) (@basil-conto, [#264](https://github.com/magnars/dash.el/issues/264))

### From 2.13 to 2.14

This release retires Emacs 23 support.  We will still try to keep
things compatible but no future guarantees are made.

- Added edebug support for threading macros (@Wilfred)
- Added `-unzip`
- Added gv setters for `-first-item` and `-last-item`
- Added `-powerset` and `-permutations` (@holomorph)
- Added `-as->` for threading a named variable (@zck)
- Added `-partition-after-pred`, `-partition-before-pred`, `-partition-after-item`, `-partition-before-item` (@zck)
- Fixed a bug in `-any-p` and friends testing for `null` on lists containing `nil` (#239)
- Fixed infinite loop bug in `-zip` and `-interleave` when called with empty input.
- Added `-second-item` through to `-fifth-item` as an alternative to `nth` (@Wilfred)
- Added `-tails` and `-inits`
- Added `-running-sum` and `-running-product`
- Added `-reductions[-r][-from]` family of functions (like `-reduce` but collecting intermediate results)
- Added `-common-prefix` (@basil-conto)

### From 2.12 to 2.13

- `-let` now supports `&alist` in destructuring.
- Various performance improvements.
- `-zip` will change in future so it always returns lists. Added
  `-zip-pair` for users who explicitly want the old behavior.
- Added lexical binding pragma to dash.el, fixes
  [#130](https://github.com/magnars/dash.el/issues/130) in Emacs 24+.
- Added `-select-column` and `-select-columns`.
- Fixed an issue with `-map-last` and `--remove-last` where they
  modified their inputs
  ([#158](https://github.com/magnars/dash.el/issues/158)).
- Added `-each-indexed` and `--each-indexed`.
- Added `-take-last` and `-drop-last`.
- Added `-doto` macro.
- `-cut <>` is now treated as a function, consistent with SRFI 26
  ([#185](https://github.com/magnars/dash.el/issues/185))

### From 2.11 to 2.12

- Add GNU ELPA support. (Phillip Lord)
- Add `-some->`, `-some->>`, and `-some-->` macros. (Cam Saul)
- `-is-suffix?` no longer destroys input list.
- Faster hashtable implementation for `-union`.
- Improvements to docstrings and examples

### From 2.10 to 2.11

- Lots of clean up wrt byte compilation, debug macros and tests

### From 2.9 to 2.10

- Add `-let` destructuring to `-if-let` and `-when-let` (Fredrik Bergroth)

### From 2.8 to 2.9

- Add `-let`, `-let*` and `-lambda` with destructuring
- Add `-tree-seq` and `-tree-map-nodes`
- Add `-non-nil`
- Add `-fix`
- Add `-fixfn` (dash-functional 1.2)
- Add `-copy` (Wilfred Hughes)

### From 2.7 to 2.8

- Add `-butlast`

### From 2.6 to 2.7

- `-zip` now supports more than two lists (Steve Lamb)
- Add  `-cycle` ,  `-pad` ,  `-annotate` ,  `-zip-fill` (Steve Lamb)
- Add `-table`, `-table-flat` (finite cartesian product)
- Add `-flatten-n`
- `-slice` now supports "step" argument
- Add functional combinators `-iteratefn`, `-prodfn`
- Add `-replace`, `-splice`, `-splice-list` which generalize `-replace-at` and `-insert-at`
- Add `-compose`, `-iteratefn` and `-prodfn` (dash-functional 1.1)

### From 2.5 to 2.6

- Add `-is-prefix-p`, `-is-suffix-p`, `-is-infix-p` (Matus Goljer)
- Add `-iterate`, `-unfold` (Matus Goljer)
- Add `-split-on`, `-split-when` (Matus Goljer)
- Add `-find-last-index` (Matus Goljer)
- Add `-list` (Johan Andersson)

### From 2.4 to 2.5

- Add `-same-items?` (Johan Andersson)
- A few bugfixes

### From 2.3 to 2.4

- Add `-snoc` (Matus Goljer)
- Add `-replace-at`, `-update-at`, `-remove-at`, and `-remove-at-indices` (Matus Goljer)

### From 2.2 to 2.3

- Add tree operations (Matus Goljer)
- Make font-lock optional

### From 2.1 to 2.2

- Add `-compose` (Christina Whyte)

### From 2.0 to 2.1

- Add indexing operations (Matus Goljer)

### From 1.8 to 2.0

- Split out `dash-functional.el` (Matus Goljer)
- Add `-andfn`, `-orfn`, `-not`, `-cut`, `-const`, `-flip` and `-on`. (Matus Goljer)
- Fix `-min`, `-max`, `-min-by` and `-max-by` (Matus Goljer)

### From 1.7 to 1.8

- Add `-first-item` and `-last-item` (Wilfred Hughes)

### From 1.6 to 1.7

- Add `-rotate` (Matus Goljer)

### From 1.5 to 1.6

- Add `-min`, `-max`, `-min-by` and `-max-by` (Johan Andersson)

### From 1.4 to 1.5

- Add `-sum` and `-product` (Johan Andersson)

### From 1.3 to 1.4

- Add `-sort`
- Add `-reduce-r` (Matus Goljer)
- Add `-reduce-r-from` (Matus Goljer)

### From 1.2 to 1.3

- Add `-partition-in-steps`
- Add `-partition-all-in-steps`

### From 1.1 to 1.2

- Add `-last` (Matus Goljer)
- Add `-insert-at` (Emanuel Evans)
- Add `-when-let` and `-if-let` (Emanuel Evans)
- Add `-when-let*` and `-if-let*` (Emanuel Evans)
- Some bugfixes

## Contributors

 - [Matus Goljer](https://github.com/Fuco1) contributed lots of features and functions.
 - [Takafumi Arakaki](https://github.com/tkf) contributed `-group-by`.
 - [tali713](https://github.com/tali713) is the author of `-applify`.
 - [Víctor M. Valenzuela](https://github.com/vemv) contributed `-repeat`.
 - [Nic Ferrier](https://github.com/nicferrier) contributed `-cons*`.
 - [Wilfred Hughes](https://github.com/Wilfred) contributed `-slice`, `-first-item` and `-last-item`.
 - [Emanuel Evans](https://github.com/shosti) contributed `-if-let`, `-when-let` and `-insert-at`.
 - [Johan Andersson](https://github.com/rejeep) contributed `-sum`, `-product` and `-same-items?`
 - [Christina Whyte](https://github.com/kurisuwhyte) contributed `-compose`
 - [Steve Lamb](https://github.com/steventlamb) contributed `-cycle`, `-pad`, `-annotate`, `-zip-fill` and an n-ary version of `-zip`.
 - [Fredrik Bergroth](https://github.com/fbergroth) made the `-if-let` family use `-let` destructuring and improved script for generating documentation.
 - [Mark Oteiza](https://github.com/holomorph) contributed the script to create an info manual.
 - [Vasilij Schneidermann](https://github.com/wasamasa) contributed `-some`.
 - [William West](https://github.com/occidens) made `-fixfn` more robust at handling floats.
 - [Cam Saül](https://github.com/camsaul) contributed `-some->`, `-some->>`, and `-some-->`.
 - [Basil L. Contovounesios](https://github.com/basil-conto) contributed `-common-prefix`.
 - [Paul Pogonyshev](https://github.com/doublep) contributed `-each-r` and `-each-r-while`.

Thanks!

New contributors are welcome.  To ensure that dash.el can be distributed with
GNU ELPA or Emacs, we would request that all contributors assign copyright to
the Free Software Foundation.  For more on this, see [`(info "(emacs) Copyright
Assignment")`](https://www.gnu.org/software/emacs/manual/html_node/emacs/Copyright-Assignment.html).

## License

Copyright (C) 2012-2016 Free Software Foundation, Inc.

Authors: Magnar Sveen <magnars@gmail.com>

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
