# Changelog

## master (unreleased)

## 3.0.1 (24/11/2014)

### Bugs fixed

* Numerous font-lock bug fixes.
* [#260](https://github.com/clojure-emacs/clojure-mode/pull/260): Don't treat `@` as a word character.
* [#239](https://github.com/clojure-emacs/clojure-mode/issues/239): Indent properly multi-arity definitions.

## 3.0.0 (2/9/2014)

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
