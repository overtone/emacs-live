# Changelog

## master (unreleased)

### Changes

* [#571](https://github.com/clojure-emacs/clojure-mode/issues/571): Remove `project.el` integration.
* [#574](https://github.com/clojure-emacs/clojure-mode/issues/574): Remove `clojure-view-grimoire` command.
* Stop `clojure-sort-ns` from calling `redisplay`

## 5.12.0 (2020-08-13)

### New features

* [#556](https://github.com/clojure-emacs/clojure-mode/issues/556): `clojure-rename-ns-alias` picks up existing aliases for minibuffer completion.

### Bugs fixed

* [#565](https://github.com/clojure-emacs/clojure-mode/issues/565): Fix extra spaces being inserted after quote in paredit-mode.
* [#544](https://github.com/clojure-emacs/clojure-mode/issues/544): Fix docstring detection when string contains backslash.
* [#547](https://github.com/clojure-emacs/clojure-mode/issues/547): Fix font-lock regex for character literals for uppercase chars and other symbols.
* [#556](https://github.com/clojure-emacs/clojure-mode/issues/556): Fix renaming of ns aliases containing regex characters.
* [#555](https://github.com/clojure-emacs/clojure-mode/issues/555): Fix ns detection for ns forms with complex metadata.
* [#550](https://github.com/clojure-emacs/clojure-mode/issues/550): Fix `outline-regexp` so `outline-insert-heading` behaves correctly.
* [#551](https://github.com/clojure-emacs/clojure-mode/issues/551): Indent `clojure-align` region before aligning.
* [#520](https://github.com/clojure-emacs/clojure-mode/issues/508): Fix allow `clojure-align-cond-forms` to recognize qualified forms.
* [#404](https://github.com/clojure-emacs/clojure-mode/issues/404)/[#528]((https://github.com/clojure-emacs/clojure-mode/issues/528)): Fix syntax highlighting for multiple consecutive comment reader macros (`#_#_`).

### Changes

* Inline definition of `clojure-mode-syntax-table` and support `'` quotes in symbols.
* Enhance add arity refactoring to support a `defn` inside a reader conditional.
* Enhance add arity refactoring to support new forms: `letfn`, `fn`, `defmacro`, `defmethod`, `defprotocol`, `reify` and `proxy`.

## 5.11.0 (2019-07-16)

### New features

* [#496](https://github.com/clojure-emacs/clojure-mode/issues/496): Highlight `[[wikilinks]]` in comments.
* [#366](https://github.com/clojure-emacs/clj-refactor.el/issues/366): Add support for renaming ns aliases (`clojure-rename-ns-alias`, default binding `C-c C-r n r`).
* [#410](https://github.com/clojure-emacs/clojure-mode/issues/410): Add support for adding an arity to a function (`clojure-add-arity`, default binding `C-c C-r a`).

### Bugs fixed

* Dynamic vars whose names contain non-alphanumeric characters are now font-locked correctly.
* [#445 (comment)](https://github.com/clojure-emacs/clojure-mode/issues/445#issuecomment-340460753): Proper font-locking for namespaced keywords like for example `(s/def ::keyword)`.
* [#508](https://github.com/clojure-emacs/clojure-mode/issues/508): Fix font-locking for namespaces with metadata.
* [#506](https://github.com/clojure-emacs/clojure-mode/issues/506): `clojure-mode-display-version` correctly displays the package's version.
* [#445](https://github.com/clojure-emacs/clojure-mode/issues/445), [#405](https://github.com/clojure-emacs/clojure-mode/issues/405), [#469](https://github.com/clojure-emacs/clojure-mode/issues/469): Correct font-locking on string definitions with docstrings, e.g: `(def foo "doc" "value")`. Correct indentation as well.
* [#518](https://github.com/clojure-emacs/clojure-mode/issues/518): Fix `clojure-find-ns` when there's an `ns` form inside a string.
* [#530](https://github.com/clojure-emacs/clojure-mode/pull/530): Prevent electric indentation within inlined docstrings.

### Changes

* [#524](https://github.com/clojure-emacs/clojure-mode/issues/524): Add proper indentation rule for `delay` (same as for `future`).
* [#538](https://github.com/clojure-emacs/clojure-mode/pull/538): Refactor `clojure-unwind` to take numeric prefix argument for unwinding N steps, and universal argument for unwinding completely. The dedicated `C-c C-r a` binding for `clojure-unwind-all`is now removed and replaced with the universal arg convention `C-u C-c C-r u`.

## 5.10.0 (2019-01-05)

### New features

* Recognize Gradle projects using the new Kotlin DSL (`build.gradle.kts`).
* [#481](https://github.com/clojure-emacs/clojure-mode/issues/481): Support vertical alignment even in the presence of blank lines, with the new `clojure-align-separator` user option.
* [#483](https://github.com/clojure-emacs/clojure-mode/issues/483): Support alignment for reader conditionals, with the new `clojure-align-reader-conditionals` user option.
* [#497](https://github.com/clojure-emacs/clojure-mode/pull/497): Indent "let", "when" and "while" as function form if not at start of a symbol.

### Bugs fixed

* [#489](https://github.com/clojure-emacs/clojure-mode/issues/489): Inserting parens before comment form doesn't move point.
* [#500](https://github.com/clojure-emacs/clojure-mode/pull/500): Fix project.el integration.
* [#513](https://github.com/clojure-emacs/clojure-mode/pull/513): Fix incorrect indentation of namespaced map.

### Changes

* Change the accepted values of `clojure-indent-style` from keywords to symbols.
* [#503](https://github.com/clojure-emacs/clojure-mode/pull/503): Fix Makefile so that we can compile again.

## 5.9.1 (2018-08-27)

* [#485](https://github.com/clojure-emacs/clojure-mode/issues/485): Fix a regression in `end-f-defun`.

## 5.9.0 (2018-08-18)

### Changes

* Add `clojure-toplevel-inside-comment-form` to make forms inside of `(comment ...)` forms appear as top level forms for evaluation and navigation.
* Require Emacs 25.1+.

## 5.8.2 (2018-08-09)

### Changes

* Disable ns caching by default.

## 5.8.1 (2018-07-03)

### Bugs fixed

* Fix the project.el integration.

## 5.8.0 (2018-06-26)

### New features

* New interactive commands `clojure-show-cache` and `clojure-clear-cache`.
* Add basic integration with `project.el`.
* The results of `clojure-project-dir` are cached by default to optimize performance.
* [#478](https://github.com/clojure-emacs/clojure-mode/issues/478): Cache the result of `clojure-find-ns` to optimize performance.

### Changes

* Indent `fdef` (clojure.spec) like a `def`.
* Add `shadow-cljs.edn` to the default list of build tool files.

## 5.7.0 (2018-04-29)

### New features

* Add imenu support for multimethods.
* Make imenu recognize indented def-forms.
* New interactive command `clojure-cycle-when`.
* New interactive command `clojure-cycle-not`.
* New defcustom `clojure-comment-regexp` for font-locking `#_` or `#_` AND `(comment)` sexps.
* [#459](https://github.com/clojure-emacs/clojure-mode/issues/459): Add font-locking for new built-ins added in Clojure 1.9.
* [#471](https://github.com/clojure-emacs/clojure-mode/issues/471): Support tagged maps (new in Clojure 1.9) in paredit integration.
* Consider `deps.edn` a project root.
* [#467](https://github.com/clojure-emacs/clojure-mode/issues/467): Make `prog-mode-map` the parent keymap for `clojure-mode-map`.

### Changes

* Drop support for CLJX.
* Remove special font-locking of Java interop methods & constants: There is no semantic distinction between interop methods, constants and global vars in Clojure.

### Bugs fixed

* [#458](https://github.com/clojure-emacs/clojure-mode/pull/458): Get correct ns when in middle of ns form with `clojure-find-ns`
* [#447](https://github.com/clojure-emacs/clojure-mode/issues/241): When `electric-indent-mode` is on, force indentation from within docstrings.
* [#438](https://github.com/clojure-emacs/clojure-mode/issues/438): Filling within a doc-string doesn't affect surrounding code.
* Fix fill-paragraph in multi-line comments.
* [#443](https://github.com/clojure-emacs/clojure-mode/issues/443): Fix behavior of `clojure-forward-logical-sexp` and `clojure-backward-logical-sexp` with conditional macros.
* [#429](https://github.com/clojure-emacs/clojure-mode/issues/429): Fix a bug causing last occurrence of expression sometimes is not replaced when using `move-to-let`.
* [#423](https://github.com/clojure-emacs/clojure-mode/issues/423): Make `clojure-match-next-def` more robust against zero-arity def-like forms.
* [#451](https://github.com/clojure-emacs/clojure-mode/issues/451): Make project root directory calculation customized by `clojure-project-root-function`.
* Fix namespace font-locking: namespaces may also contain non alphanumeric chars.


## 5.6.1 (2016-12-21)

### Bugs fixed

* Make `clojure--read-let-bindings` more robust so `let` related refactorings do not bail on an incorrectly formatted binding form.

## 5.6.0 (2016-11-18)

### New features

* New interactive command `clojure-mode-report-bug`.
* New interactive command `clojure-view-guide`.
* New interactive command `clojure-view-reference-section`.
* New interactive command `clojure-view-cheatsheet`.
* New interactive command `clojure-view-grimoire`.
* New interactive command `clojure-view-style-guide`.
* Make the refactoring keymap prefix customizable via `clojure-refactor-map-prefix`.
* Port and rework `let`-related features from `clj-refactor`. Available features: introduce `let`, move to `let`, forward slurp form into `let`, backward slurp form into `let`.

### Changes

* `clojure-mode` now requires Emacs 24.4.

## 5.5.2 (2016-08-03)

### Bugs fixed

* [#399](https://github.com/clojure-emacs/clojure-mode/issues/399): Fix fontification of prefix characters inside keywords.

## 5.5.1 (2016-07-25)

### Bugs fixed

* [#394](https://github.com/clojure-emacs/clojure-mode/issues/394): `?` character is now treated as prefix when outside symbols.
* [#394](https://github.com/clojure-emacs/clojure-mode/issues/394): `#` character now has prefix syntax class.
* Fixed indentation of `definterface` to match that of `defprotocol`.
* [#389](https://github.com/clojure-emacs/clojure-mode/issues/389): Fixed the indentation of `defrecord` and `deftype` multiple airity protocol forms.
* [#393](https://github.com/clojure-emacs/clojure-mode/issues/393): `imenu-generic-expression` is no longer hard-coded and its global value is respected.

## 5.5.0 (2016-06-25)

### New features

* Port cycle privacy, cycle collection type and cycle if/if-not from clj-refactor.el.
* Rework cycle collection type into convert collection to list, quoted list, map, vector, set.

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
