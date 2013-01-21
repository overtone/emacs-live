# Emacs Live Changelog

## 1.0-BETA-19 (Mon 21st Jan 2013)

* update clojure-monde, nrepl, gist and yasnippet dependencies
* make `*magit-commit*` popwindows sticky
* fix SuperCollider support - now uses latest scel classes
* add new fn live-server-kill-terminal which allows you to make a speedy exit without having to save stuff.
* bindings modifications:
  - don't clobber M-\ unless in paredit-mode, when we give it more useful behaviour.
  - C-\ backward-kill-word
  - C-] kill-region
  - M-] kill-ring-save
* start working on support for pack metadata via info.el files
