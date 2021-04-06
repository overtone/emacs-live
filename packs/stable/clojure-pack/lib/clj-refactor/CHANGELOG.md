# Changelog

## Up next

- New config setting `cljr-libspec-whitelist` to prevent libspecs which appear unused but are side-effecting at load from being pruned.
- [#301] (https://github.com/clojure-emacs/clj-refactor.el/issues/301) `ad`  has gained a prefix to declare the symbol under the cursor.
- [#312](https://github.com/clojure-emacs/clj-refactor.el/issues/312) Allow `sut` alias to be customized.
- [#305](https://github.com/clojure-emacs/clj-refactor.el/issues/305) Don't call lookup-alias for non namespaced keywords at all when slash is typed. However trigger lookup alias with the leading :: stripped off the prefix if the keyword is namespaced.

### Changes

- [#302] (https://github.com/clojure-emacs/clj-refactor.el/issues/302) `ad` now understands def-like things, e.g. defs created by Schema.
- When inserting ns form to blank clojure-ish file, check if cider is available and connected for better detecting the expected namespace.
- Remove the warning about missing nREPl ops.
- Remove threading macro related features because they are moved to Clojure mode. However, the usual mnemonics for these features still work only they reference the Clojure mode implementations.
- Remove cycle privacy, cycle if and cycle collection type features. They are moved to Clojure mode. The usual mnemonics for cycle privacy and cycle if features still work only they refer the Clojure mode implementations. Cycle collection type got reworked into convert collection to list, quoted list, map, vector and set. *Cycle* collection type is no longer supported.

### Bugs fixed

- [#299](https://github.com/clojure-emacs/clj-refactor.el/issues/299) `ml` moves cursor
- [#309](https://github.com/clojure-emacs/clj-refactor.el/issues/309)  `am` creates alias for fully-qualified symbols.
- [#313](https://github.com/clojure-emacs/clj-refactor.el/issues/313)  teach `pf` about function literals using `%&`.
- [#320](https://github.com/clojure-emacs/clj-refactor.el/issues/320) `*data-readers*` ignored when searching for macros.
- [#339](https://github.com/clojure-emacs/clj-refactor.el/issues/339) Teach stop refer to understand multiline refer clauses.
- [#341](https://github.com/clojure-emacs/clj-refactor.el/issues/341) Avoid creating circular dependencies with move form when source namespace refers to the target namespace with a require.

## 2.2.0

- Smarten up `cljr-stop-referring` to replace `:refer :all` style require with alias and apply the alias to all occurrences of symbols from the referred namespace.
- [#292](https://github.com/clojure-emacs/clj-refactor.el/issues/292) The buffer wasn't saved after adding a missing libspec causing clean-ns
to act on stale data.
- Don't try to resolve `js/` in cljs-mode
- `cljr-create-fn-from-example` improvements: strip ns off keywords when making param name; always include a blank line over new function
- [#306](https://github.com/clojure-emacs/clj-refactor.el/issues/306) Add-require doesn't jump back if there is no REPL connection with refactor-nrepl configured

### Changes

- Compatible with CIDER 0.11
- Follow up CIDER 0.11 injecting its own dependencies at `cider-jack-in` by adding clj-refactor's own dependencies to the approriate vars in CIDER. Both leiningen and boot are supported. Set `cljr-inject-dependencies-at-jack-in` to nil to opt out.

## 2.0.0

- [#267](https://github.com/clojure-emacs/clj-refactor.el/issues/267)
 Add `cljr-require-macro` which requires a macro into the current
 namespace.
- Add prefix variant to `cljr-add-import-to-ns` for insertion of imports in the cljs part of the ns declaration.
- Add prefix variant to `cljr-add-use-to-ns` for insertion of 'use' in the cljs part of the ns declaration.
- Add prefix variant to `cljr-add-require` for insertion of requires in the cljs part of the ns declaration.
- Boot support for `cljr-clean-ns`.
- Boot support for `cljr-sort-project-dependencies`.
- Boot support for `cljr-update-project-dependencies`.
- Boot support for `cljr-update-project-dependency`.
- [#228](https://github.com/clojure-emacs/clj-refactor.el/issues/238) Boot support for `cljr-add-project-dependency`.
- Get rid of `cljr-reload-config`.  We're now sending the configuration options down to the middleware on each request instead of storing it down there.
- Make magic requires cljc aware.
- [#215](https://github.com/clojure-emacs/clj-refactor.el/issues/215) Improve the magic requires feature (when you hit `/`) by asking the middleware for all available namespace aliases.
- Add `cljr-extract-def` which extracts the form at, or around, point as a def.
- Add `cljr-change-function-signature` to re-order or re-name function parameters.
- Keep pressing `l` after `cljr-expand-let` to expand further.
- [refactor-nrepl#99](https://github.com/clojure-emacs/refactor-nrepl/issues/99) if cljr-thread-first-all or cljr-thread-last-all is called with a prefix the last expression is not threaded. cljr-thread-all-but-last defcustom has the same effect without the prefix
- [hydra](https://github.com/abo-abo/hydra) menus for discoverability: they help to (re)learn clj-refactor key bindings. See: [parent hydra](https://github.com/clojure-emacs/clj-refactor.el/wiki/Hydra).

### Bugs fixed

- [#285](https://github.com/clojure-emacs/clj-refactor.el/issues/285) clean-ns did the wrong thing unless the code was loaded.

### Changes

- [#265](https://github.com/clojure-emacs/clj-refactor.el/issues/265) Feedback to the user is lost among other general messages from emacs.
- Make `cljr-clean-ns` the only default function used by `cljr-project-clean`.
- Remove `cljr-remove-unused-requires` which is replaced by `cljr-clean-ns`.
- Remove `cljr-replace-use` which is replaced by `cljr-clean-ns`.
- Remove `cljr-sort-ns` which is replaced by `cljr-clean-ns`.
- `cljr-remove-debug-fns` has been removed.
- `cljr-magic-require-namespaces` is now only consulted in the event the namespace alias isn't already used in the project.
- [#217](https://github.com/clojure-emacs/clj-refactor.el/issues/217) When requiring the test framework in test files stop favoring `:refer :all`.
- [#217](https://github.com/clojure-emacs/clj-refactor.el/issues/217) Add a bunch of defcustoms to parameterise what gets inserted into the test namespaces for the various test frameworks.
- [#216](https://github.com/clojure-emacs/clj-refactor.el/issues/216) Teach our automatic ns generator about cljc files.
- Teach `cljr-extract-constant` about the `^:const` hint to the compiler.
- Use yasnippet for placeholder parameters in `cljr-create-fn-from-example`
- Highlight the function be promoted with overlays in `cljr-promote-function`.
- Highlight the form to be extracted with overlays in `cljr-extract-function`.
- `cljr-create-fn-from-example` is now significantly smarter about guessing parameter numbers and names.
- `cljr-sort-ns` no longer marks the buffer as changed if it did no work.
- `cljr-rename-symbol` now fails earlier, before prompting the user for a new name if an AST can't be built due to errors.
- support for emacs 24.3 and older is dropped
- [refactor-nrepl#85](https://github.com/clojure-emacs/refactor-nrepl/issues/85) Eliminate some find usages duplicates
- Some AST based features (find usages, rename symbol, inline symbol) ignore namespaces that cannot be analyzed if `cljr-ignore-analyzer-errors` set to true instead of failing entirely.
- By default warning is given when AST based feature is used and clj-refactor only proceeds with it if the user allowed evalling the project as the analyzer also evals first level forms. To disable the warning set `cljr-warn-on-eval` to `nil`. This also reenables warming AST cache at startup of the REPL.

## 1.1.0

- Add `cljr-describe-refactoring` which shows the wiki page describing one of the available refactorings inline in emacs.
- Add `cljr-rename-file-or-dir` to replace `cljr-rename-file`.
- Add `cljr-inline-symbol` which replaces the symbol at point with its definition.
- Add `cljr-add-stubs` which adds a skeleton implementation of the protocol or interface at point.
- Add `cljr-reify-to-defrecord`
- Add `cljr-show-changelog` so users don't have to visit github to find out what's changed after a package update.
- Add `cljr-create-fn-from-example` to create function stub based on example usage
- Add `cljr-update-project-dependencies` (for leiningen only) prompts to update dependencies in project.clj, listing available versions
- Now `cljr--add-test-use-declarations` actually checks the file system in order to find its require for the source ns.
- Improvements to error handling and reporting around analyzing namespaces
- Configuration option `cljr-find-usages-ignore-analyzer-errors`: when true, find-usages will run even if there are compilation/analyzer problems in some namespaces; defaults to nil.
- Configuration option `cljr-favor-private-functions`: set to nil to create public functions where applicable.
- Better support for `cljr-use-metadata-for-privacy` when creating functions.
- Highlight the relevant form when extracting and promoting functions.
- Multiple cursors is used when extracting and promoting functions (disable by setting `cljr-use-multiple-cursors` to nil)
- Not using multiple-cursors when in evil-mode.

## 1.0.5

- Add `cljr-reload-config` to resubmit config settings to the middleware
- Add config setting for `clean-ns` to not do rewriting to favor prefix form.
- Add `cljr-extract-function`
- Add `cljr-hotload-dependency`
- Hotloading of artifacts added with `cljr-add-project-dependency`
- Add `cljr-remove-let`
- Add `cljr-clean-ns`
- Add `cljr-add-missing-libspec`
- Add `cljr-promote-function`
- Add `cljr-find-usages`
- Add `cljr-rename-symbol`

## 0.13

- Removed `cljr-cycle-stringlike`.  This function was duplicating the functionality of `clojure-mode`s `clojure-toggle-keyword-string`
- Add `cljr-cycle-if`
- Common namespace shorthands are (optionally) automatically required when you type it.
- Comparator for sort require, use and import is configurable, add optional lenght based comparator to sort longer first
- Add semantic comparator to sort items closer to the current namespace first
- Add `cljr-project-clean` with configurable clean functions
- Add `cljr-sort-project-dependencies`
- Add `cljr-add-project-dependency`
- Add `cljr-remove-debug-fns`
- performance tweak for `cljr-remove-unused-requires` if `refactor-nrepl` is used

## 0.12

- When expanding let, or moving expressions to let, it now replaces
  duplicates in the let body with the bound name.

## 0.11

- Add `cljr-raise-sexp`
- Add `cljr-remove-unused-requires`
- Add `cljr-move-form`

## 0.10

- Add `cljr-stop-referring`
- Add `cljr-destructure-keys`
- Add `cljr-sort-ns`

## 0.9

- Add `cljr-replace-use`
- Add `cljr-add-declaration`

## 0.8

- Add `cljr-cycle-stringlike`
- Add `cljr-cycle-coll`
- Add `cljr-cycle-privacy`

## 0.7

- Add `cljr-thread-first-all`, `cljr-thread-last-all` and `cljr-unwind-all`

## 0.6

- Add `cljr-move-to-let`
