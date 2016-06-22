# Changelog

## master (unreleased)

## 5.4.0 (2016-05-21)

### New features

* When aligning forms with `clojure-align` (or with the automatic align feature), blank lines will divide alignment regions.
* [#378](https://github.com/clojure-emacs/clojure-mode/issues/378): Font-lock escape characters in strings.
* Port threading macros related features from clj-refactor.el. Available refactorings: thread, unwind, thread first all, thread last all, unwind all.
* New command: `clojure-sort-ns`.
* All ns manipulation commands have keybindings under `C-c C-r n`.

## 5.3.0 (2016-04-04)

### Bugs fixed

* [#371](https://github.com/clojure-emacs/clojure-mode/issues/371): Don't font-lock `:foo/def` like a `def` form.
* [#367](https://github.com/clojure-emacs/clojure-mode/issues/367): `clojure-align` no longer gets confused with commas. In fact, now it even removes extra commas.

### New features

* [#370](https://github.com/clojure-emacs/clojure-mode/issues/370): Warn the user if they seem to have activated the wrong major-mode.
* Make the expected ns function configurable via `clojure-expected-ns-function`.

## 5.2.0 (2016-02-04)

### Bugs fixed

* [#361](https://github.com/clojure-emacs/clojure-mode/issues/361): Fixed a typo preventing the highlighting of fn names that don't start with `t`.
* [#360](https://github.com/clojure-emacs/clojure-mode/issues/360): `clojure-align` now reindents after aligning, which also fixes an issue with nested alignings.

### New features

* [#362](https://github.com/clojure-emacs/clojure-mode/issues/362): New custom option `clojure-indent-style` offers 3 different ways to indent code.

## 5.1.0 (2016-01-04)

### New features

* Vertically align sexps with `C-c SPC`. This can also be done automatically (as part of indentation) by turning on `clojure-align-forms-automatically`.
* Indent and font-lock forms that start with `let-`, `while-` or `when-` like their counterparts.
* Apply the `font-lock-comment-face` to code commented out with `#_`.
* Add indentation config for ClojureScript's `this-as`.

### Bugs fixed

* Namespaces can now use the full palette of legal symbol characters.
* Namespace font-locking according to `clojure.lang.LispReader`.
* Fixed the indentation for `specify` and `specify!`.
* Fixed the docstring indentation for `defprotocol`.

## 5.0.1 (2015-11-15)

### Bugs fixed

* Don't treat the symbol `default-(something)` as def* macro.
* `cider-find-ns` now returns the closest `ns` instead of the first one.
* [#344](https://github.com/clojure-emacs/clojure-mode/issues/344): Fixed the indentation of `extend-type`.

## 5.0.0 (2015-10-30)

### New features

* [#302](https://github.com/clojure-emacs/clojure-mode/pull/302): Add new sexp navigation commands. `clojure-forward-logical-sexp` and `clojure-backward-logical-sexp` consider `^hints` and `#reader.macros` to be part of the sexp that follows them.
* [#303](https://github.com/clojure-emacs/clojure-mode/issues/303): Handle `boot` projects in `clojure-expected-ns`.
* Added dedicated modes for ClojureScript, ClojureC and ClojureX. All of them are derived from `clojure-mode`.
* Added support for Gradle projects.
* Vastly improved indentation engine.
* Added support for reader conditionals.
* Improved font-locking of namespaced symbols.

### Bugs fixed

* [#310](https://github.com/clojure-emacs/clojure-mode/issues/310) and [#311](https://github.com/clojure-emacs/clojure-mode/issues/311) Fix `clojure-expected-ns` in multi-source projects.
* [#307](https://github.com/clojure-emacs/clojure-mode/issues/307): Don't highlight `handle` and `handler-case` as keywords.
* Fix font-locking for def with special chars such as: `defn*`, `defspecial!`.
* Numerous indentation issues.

## 4.1.0 (2015-06-20)

### Changes

* Add `.cljc` to `auto-mode-alist`.
* [#281](https://github.com/clojure-emacs/clojure-mode/pull/281): Add support for namespace-prefixed definition forms.
* Remove `clojure-mark-string`.
* [#283](https://github.com/clojure-emacs/clojure-mode/pull/283): You can now specify different indentation settings for ns-prefixed symbols.
* [#285](https://github.com/clojure-emacs/clojure-mode/issues/285): Require Emacs 24.3+.

### Bugs fixed

* Prevent error when calling `indent-for-tab-command` at the start of
the buffer at end of line.
* [#274](https://github.com/clojure-emacs/clojure-mode/issues/274): Correct font-locking of certain punctuation character literals.
* Fix font-locking of namespace-prefixed dynamic vars (e.g. `some.ns/*var*`).
* [#284](https://github.com/clojure-emacs/clojure-mode/issues/284): Fix the indentation of the `are` macro.

## 4.0.1 (2014-12-19)

### Bugs fixed

* Indent properly `as->`.
* Revert the indentation settings for `->`, `->>`, `some->` and `some->>`.

## 4.0.0 (2014-12-12)

### Changes

* Removed `inferior-lisp` integration in favor of `inf-clojure`.
* Indented the body of `cond` with 2 spaces.
* Removed special indentation settings for `defstruct`, `struct-map` and `assoc`.
* Added special indentation settings for `->`, `->>`, `cond->`, `cond->>`, `some->` and `some->>`.

## 3.0.1 (2014-11-24)

### Bugs fixed

* Numerous font-lock bug fixes.
* [#260](https://github.com/clojure-emacs/clojure-mode/pull/260): Don't treat `@` as a word character.
* [#239](https://github.com/clojure-emacs/clojure-mode/issues/239): Indent properly multi-arity definitions.

## 3.0.0 (2014-09-02)

### New features

* Added font-locking for namespaces and namespace aliases.
* Added font-locking for character literals.
* Added font-locking for constants.
* Added font-locking for dynamic vars.
* Added font-locking for `cljx`.
* Various docstring filling improvements.
* Introduced additional faces for keyword literals, character literals and
interop method invocations.
* Added support for `prettify-symbols-mode`.

### Changes

* Emacs 24.1 is required.
* Removed deprecated `clojure-font-lock-comment-sexp`.
* Renamed `clojure-mode-font-lock-setup` to `clojure-font-lock-setup`.
* Some font-locking was extracted to a separate package. ([clojure-mode-extra-font-locking](https://github.com/clojure-emacs/clojure-mode/blob/master/clojure-mode-extra-font-locking.el)).

### Bugs fixed

* Properly font-lock docstrings regardless of the presence of metadata or type hints.
