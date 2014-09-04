# Emacs Live Changelog

## 1.0-BETA-25 (Thurs 4th Sept 2014)
* Update many, many dependencies (As usual)
* Move to CIDER 0.7.0
* Enable stacktrace popup for CIDER
* Improve Gandalf theme a little

## 1.0-BETA-24 (Thurs 6th Feb 2014)
* Update many, many dependencies (As usual)
* Move from nrepl.el to new cider library
* Add Magnar Sveen's clj-refactor bound to C-c C-m See: https://github.com/magnars/clj-refactor.el
* Rename live pack directory to stable to clarify the distinction with the dev packs
* Specify hard dependency on Emacs 24.3
* Add support for Haskell
* Add support for Extempore
* Add support for floobits
* Add word count mode (M-x wc-mode)
* Improve format of boot messages
* Remove cl-lib.el as it's included in Emacs 24.3 by default.


## 1.0-BETA-23 (Tues 3rd Sept 2013)
* Update many, many dependencies
* Update Cyberpunk colour theme to handle more modes
* Add function for setting `custom-file`
* Fix sudo-edit fn
* Add new default bindings
* Add a shortcut for cycling through different coll types
* Ensure js-mode snippets are loaded
* Make M-T transpose-sexps
* Pull in clojure-test-mode
* Move to using flx rather than flex
* Move back to vanilla nrepl.el now issues are resolved
* Ensure nrepl-eval-sexp-fu works with latest Emacs
* Remove live-reload-snippets - favour yas-reload-all
* Add new dependencies: dash, pkg-info and s
* Improve git highlighting with git-modes
* Add live-paredit-copy-sexp-at-point bound to C-M-k


## 1.0-BETA-22 (Sat 30th March 2013)
* Update many dependencies
* Make grep highlights more permanent (rather than just flashing away)
* Move default location of customisation to separate location
* Update Gandalf theme
* add live-recentf-ido-find-file
* Reduce impact of whitespace fixes on file save - only remove trailing
  whitespace and empty lines at the start and end of file
* Update git gutter and tone down colours
* Add binding for git-gutter:revert-hunk C-c g r
* Add glsl major mode to lang pack.
* Allow default font to be specified on Darwin [Roth Michaels]
* Add license - GPL v3
* Add live-persistent-scratch-buffer fn for creating unique buffers
  backed by files


## 1.0-BETA-21 (Sun 3rd March 2013)

* Update many dependencies.
* Allow user to define Emacs Live root directory to a location other
  than `~/.emcs.d`. Either by setting a var called`live-root-dir` or an
  env variable called `EMACS_LIVE_DIR`.
* Clojure print enhancement - specify `*print-length*` to 100 to stop
  infinite sequences killing things.
* Monkey-patch basic-save-buffer to ask you nicely about buffers that
  don't have associated files when quitting Emacs.
* Switch to ruby-mode in .rake, .gemspec and Gemfile buffers
* Initialise mouse mode for new terminals
* Add git-gutter mode - marks the lines which are different between the
  last saved version of the buffer (not fully live) and the last
  commited version of that file in git. + denotes a new line, - for
  removed lines and ~ for modified lines. The following fns are useful:
  - C-c g g git-gutter:toggle
  - C-c g p git-gutter:previous-diff
  - C-c g n git-gutter:next-diff
  - C-c g d git-gutter:popup-diff

  `git-gutter:popup-diff` is awesome: try it on a line marked by Git
  Gutter.


## 1.0-BETA-20 (Tues 22nd Jan 2013)
* relax pack checks to not throw exceptions with invalid or absent pack metadata

## 1.0-BETA-19 (Mon 21st Jan 2013)

* update clojure-mode, nrepl, gist and yasnippet dependencies
* make `*magit-commit*` popwindows sticky
* fix SuperCollider support - now uses latest scel classes
* add new fn live-server-kill-terminal which allows you to make a speedy
  exit without having to save stuff.
* bindings modifications:
  - don't clobber M-\ unless in paredit-mode, when we give it more useful behaviour.
  - C-\ backward-kill-word
  - C-] kill-region
  - M-] kill-ring-save
* start working on support for pack metadata via info.el files
