# Changelog

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
