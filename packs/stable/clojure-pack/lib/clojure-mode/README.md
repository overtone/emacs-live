[![License GPL 3][badge-license]][copying]
[![MELPA][melpa-badge]][melpa-package]
[![MELPA Stable][melpa-stable-badge]][melpa-stable-package]
[![travis][badge-travis]][travis]

# Clojure Mode

Provides Emacs font-lock, indentation, navigation and refactoring for the
[Clojure(Script) programming language](http://clojure.org).

This document assumes you're familiar with Emacs.  More thorough walkthroughs,
targeting Emacs beginners, are available at
[clojure-doc.org](http://clojure-doc.org/articles/tutorials/emacs.html) and
[Clojure for the Brave and the True](http://www.braveclojure.com/basic-emacs/).
Keep in mind, however, that they might be out-of-date.

**This documentation tracks the `master` branch of `clojure-mode`. Some of
the features and settings discussed here might not be available in
older releases (including the current stable release). Please, consult
the relevant git tag (e.g. 5.1.0) if you need documentation for a
specific `clojure-mode` release.**

***

- [Installation](#installation)
- [Bundled major modes](#bundled-major-modes)
- [Configuration](#configuration)
  - [Indentation options](#indentation-options)
    - [Indentation of function forms](#indentation-of-function-forms)
    - [Indentation of macro forms](#indentation-of-macro-forms)
  - [Vertical alignment](#vertical-alignment)
- [Refactoring support](#refactoring-support)
  - [Threading macros](#threading-macros-related-features)
- [Related packages](#related-packages)
- [REPL Interaction](#repl-interaction)
  - [Basic REPL](#basic-repl)
  - [CIDER](#cider)
- [Changelog](#changelog)
- [License](#license)

## Installation

Available on the major `package.el` community maintained repos -
[MELPA Stable][] and [MELPA][] repos.

MELPA Stable is the recommended repo as it has the latest stable
version.  MELPA has a development snapshot for users who don't mind
(infrequent) breakage but don't want to run from a git checkout.

You can install `clojure-mode` using the following command:

<kbd>M-x package-install [RET] clojure-mode [RET]</kbd>

or if you'd rather keep it in your dotfiles:

```el
(unless (package-installed-p 'clojure-mode)
  (package-install 'clojure-mode))
```

If the installation doesn't work try refreshing the package list:

<kbd>M-x package-refresh-contents</kbd>

## Bundled major modes

The `clojure-mode` package actually bundles together several major modes:

* `clojure-mode` is a major mode for editing Clojure code
* `clojurescript-mode` is a major mode for editing ClojureScript code
* `clojurec-mode` is a major mode for editing `.cljc` source files
* `clojurex-mode` is a major mode for editing `.cljx` source files

All the major modes derive from `clojure-mode` and provide more or less the same
functionality.  Differences can be found mostly in the font-locking -
e.g. ClojureScript has some built-in constructs that are not present in Clojure.

The proper major mode is selected automatically based on the extension of the
file you're editing.

Having separate major modes gives you the flexibility to attach different hooks
to them and to alter their behavior individually (e.g. add extra font-locking
just to `clojurescript-mode`) .

Note that all modes derive from `clojure-mode`, so things you add to
`clojure-mode-hook` and `clojure-mode-map` will affect all the derived modes as
well.

## Configuration

To see a list of available configuration options do `M-x customize-group RET clojure`.

### Indentation options

The default indentation rules in `clojure-mode` are derived from the
[community Clojure Style Guide](https://github.com/bbatsov/clojure-style-guide).
Please, refer to the guide for the general Clojure indentation rules.

#### Indentation of function forms

The indentation of function forms is configured by the variable
`clojure-indent-style`. It takes three possible values:

- `:always-align` (the default)

```clj
(some-function
 10
 1
 2)
(some-function 10
               1
               2)
```

- `:always-indent`

```clj
(some-function
  10
  1
  2)
(some-function 10
  1
  2)
```

- `:align-arguments`

```clj
(some-function
  10
  1
  2)
(some-function 10
               1
               2)
```

#### Indentation of macro forms

The indentation of special forms and macros with bodies is controlled via
`put-clojure-indent`, `define-clojure-indent` and `clojure-backtracking-indent`.
Nearly all special forms and built-in macros with bodies have special indentation
settings in `clojure-mode`. You can add/alter the indentation settings in your
personal config. Let's assume you want to indent `->>` and `->` like this:

```clojure
(->> something
  ala
  bala
  portokala)
```

You can do so by putting the following in your config:

```el
(put-clojure-indent '-> 1)
(put-clojure-indent '->> 1)
```

This means that the body of the `->/->>` is after the first argument.

A more compact way to do the same thing is:

```el
(define-clojure-indent
  (-> 1)
  (->> 1))
```

You can also specify different indentation settings for symbols
prefixed with some ns (or ns alias):

```el
(put-clojure-indent 'do 0)
(put-clojure-indent 'my-ns/do 1)
```

The bodies of certain more complicated macros and special forms
(e.g. `letfn`, `deftype`, `extend-protocol`, etc) are indented using
a contextual backtracking indentation method, require more sophisticated
indent specifications. Here are a few examples:

```el
(define-clojure-indent
  (implement '(1 (1)))
  (letfn     '(1 ((:defn)) nil))
  (proxy     '(2 nil nil (1)))
  (reify     '(:defn (1)))
  (deftype   '(2 nil nil (1)))
  (defrecord '(2 nil nil (1)))
  (specify   '(1 (1)))
  (specify   '(1 (1))))
```

These follow the same rules as the `:style/indent` metadata specified by [cider-nrepl][].
For instructions on how to write these specifications, see
[this document](http://cider.readthedocs.org/en/latest/indent_spec/).
The only difference is that you're allowed to use lists instead of vectors.

### Vertical alignment

You can vertically align sexps with `C-c SPC`. For instance, typing
this combo on the following form:

```clj
(def my-map
  {:a-key 1
   :other-key 2})
```

Leads to the following:

```clj
(def my-map
  {:a-key     1
   :other-key 2})
```

This can also be done automatically (as part of indentation) by
turning on `clojure-align-forms-automatically`. This way it will
happen whenever you select some code and hit `TAB`.

## Refactoring support

The available refactorings were originally created and maintained by the
`clj-refactor.el` team. The ones implemented in Elisp only are gradually migrated
to `clojure-mode`.

### Threading macros related features

* Thread another expression.

Thread another form into the surrounding thread. Both `->>` and `->` variants
are supported. See demonstration on the
[clj-refactor.el wiki](https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-thread).

* Unwind a threaded expression.

Supports both `->>` and `->`. See demonstration on the
[clj-refactor.el wiki](https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-unwind-thread).

* Wrap in thread first (`->`) and fully thread.

Introduce the thread first macro and rewrite the entire form. With a prefix
argument do not thread the last form. See demonstration on the
[clj-refactor.el wiki](https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-thread-first-all).

* Wrap in thread last (`->>`) and fully thread.

Introduce the thread last macro and rewrite the entire form. With a prefix
argument do not thread the last form. See demonstration on the
[clj-refactor.el wiki](https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-thread-last-all).

* Fully unwind a threaded expression.

Unwind and remove the threading macro. See demonstration on the
[clj-refactor.el wiki](https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-unwind-all).

## Related packages

* [clojure-mode-extra-font-locking][] provides additional font-locking
for built-in methods and macros.  The font-locking is pretty
imprecise, because it doesn't take namespaces into account and it
won't font-lock a function at all possible positions in a sexp, but
if you don't mind its imperfections you can easily enable it:

```el
(require 'clojure-mode-extra-font-locking)
```

The code in `clojure-mode-font-locking` used to be bundled with
`clojure-mode` before version 3.0.

You can also use the code in this package as a basis for extending the
font-locking further (e.g. functions/macros from more
namespaces). Generally you should avoid adding special font-locking
for things that don't have fairly unique names, as this will result in
plenty of incorrect font-locking. CIDER users should avoid this package,
as CIDER does its own dynamic font-locking, which is namespace-aware
and doesn't produce almost any false positives.

* [clj-refactor][] provides refactoring support.

* Enabling `CamelCase` support for editing commands(like
`forward-word`, `backward-word`, etc) in `clojure-mode` is quite
useful since we often have to deal with Java class and method
names. The built-in Emacs minor mode `subword-mode` provides such
functionality:

```el
(add-hook 'clojure-mode-hook #'subword-mode)
```

* The use of [paredit][] when editing Clojure (or any other Lisp) code
is highly recommended. It helps ensure the structure of your forms is
not compromised and offers a number of operations that work on code
structure at a higher level than just characters and words. To enable
it for Clojure buffers:

```el
(add-hook 'clojure-mode-hook #'paredit-mode)
```

* [smartparens][] is an excellent
  (newer) alternative to paredit. Many Clojure hackers have adopted it
  recently and you might want to give it a try as well. To enable
  `smartparens` use the following code:

```el
(add-hook 'clojure-mode-hook #'smartparens-strict-mode)
```

* [RainbowDelimiters][] is a
  minor mode which highlights parentheses, brackets, and braces
  according to their depth. Each successive level is highlighted in a
  different color. This makes it easy to spot matching delimiters,
  orient yourself in the code, and tell which statements are at a
  given depth. Assuming you've already installed `RainbowDelimiters` you can
  enable it like this:

```el
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
```

* [aggressive-indent-mode][] automatically adjust the indentation of your code,
while you're writing it. Using it together with `clojure-mode` is highly
recommended. Provided you've already installed `aggressive-indent-mode` you can
enable it like this:

```el
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)
```

## REPL Interaction

One of the fundamental aspects of Lisps in general and Clojure in
particular is the notion of interactive programming - building your
programs by continuously changing the state of the running Lisp
program (as opposed to doing something more traditional like making a
change and re-running the program afterwards to see the changes in
action). To get the most of clojure-mode you'll have to combine it
with some tool which will allow you to interact with your Clojure program
(a.k.a. process/REPL).

A number of options exist for connecting to a
running Clojure process and evaluating code interactively.

### Basic REPL

Install [inf-clojure][] for basic interaction with a REPL process.

### CIDER

[CIDER][] is a powerful Clojure interactive development environment,
similar to SLIME for Common Lisp.

If you're into Clojure and Emacs you should definitely check it out.

## Changelog

An extensive changelog is available [here](CHANGELOG.md).

## License

Copyright © 2007-2016 Jeffrey Chu, Lennart Staflin, Phil Hagelberg, Bozhidar
Batsov, Artur Malabarba and [contributors][].

Distributed under the GNU General Public License; type <kbd>C-h C-c</kbd> to view it.

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
[melpa-badge]: http://melpa.org/packages/clojure-mode-badge.svg
[melpa-stable-badge]: http://stable.melpa.org/packages/clojure-mode-badge.svg
[melpa-package]: http://melpa.org/#/clojure-mode
[melpa-stable-package]: http://stable.melpa.org/#/clojure-mode
[COPYING]: http://www.gnu.org/copyleft/gpl.html
[badge-travis]: https://travis-ci.org/clojure-emacs/clojure-mode.svg?branch=master
[travis]: https://travis-ci.org/clojure-emacs/clojure-mode
[CIDER]: https://github.com/clojure-emacs/cider
[cider-nrepl]: https://github.com/clojure-emacs/cider-nrepl
[inf-clojure]: https://github.com/clojure-emacs/inf-clojure
[contributors]: https://github.com/clojure-emacs/clojure-mode/contributors
[melpa]: http://melpa.org
[melpa stable]: http://stable.melpa.org
[clojure-mode-extra-font-locking]: https://github.com/clojure-emacs/clojure-mode/blob/master/clojure-mode-extra-font-locking.el
[clj-refactor]: https://github.com/clojure-emacs/clj-refactor.el
[paredit]: http://mumble.net/~campbell/emacs/paredit.html
[smartparens]: https://github.com/Fuco1/smartparens
[RainbowDelimiters]: https://github.com/Fanael/rainbow-delimiters
[aggressive-indent-mode]: https://github.com/Malabarba/aggressive-indent-mode
