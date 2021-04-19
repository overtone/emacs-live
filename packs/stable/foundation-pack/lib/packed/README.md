Package manager agnostic Emacs Lisp package utilities
=====================================================

Packed provides some package manager agnostic utilities to work
with Emacs Lisp packages.  As far as Packed is concerned packages
are collections of Emacs Lisp libraries that are stored in a
dedicated directory such as a Git repository.  And libraries are
Emacs Lisp files that provide the correct feature (matching the
filename).

Where a package manager might depend on metadata, Packed instead
uses some heuristics to get the same information — that is slower
and might also fail at times but makes it unnecessary to maintain
package recipes.
