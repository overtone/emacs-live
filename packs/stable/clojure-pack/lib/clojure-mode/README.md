# Clojure Mode

Provides Emacs font-lock, indentation, and navigation for the
[Clojure programming language](http://clojure.org).

A more thorough walkthrough is available at [clojure-doc.org](http://clojure-doc.org/articles/tutorials/emacs.html)

## Installation

Available on both [Marmalade](http://marmalade-repo.org/packages/clojure-mode) and
[MELPA](http://melpa.milkbox.net) repos.

Marmalade is recommended as it has the latest stable version, but
MELPA has a development snapshot for users who don't mind breakage but
don't want to run from a git checkout.

If you're not already using Marmalade, add this to your
`~/.emacs.d/init.el` and load it with <kbd>M-x eval-buffer</kbd>.

```lisp
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
```

If you're feeling adventurous and you'd like to use MELPA add this bit
of code instead:

```lisp
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
```

And then you can install:

<kbd>M-x package-refresh-contents</kbd>

<kbd>M-x package-install [RET] clojure-mode [RET]</kbd>

or if you'd rather keep it in your dotfiles:

```lisp
(unless (package-installed-p 'clojure-mode)
  (package-refresh-contents)
  (package-install 'clojure-mode))
```

On Emacs 23 you will need to get [package.el](http://bit.ly/pkg-el23)
yourself or install manually by placing `clojure-mode.el` on your `load-path`
and `require`ing it.

## Clojure Test Mode

This source repository also includes `clojure-test-mode.el`, which
provides support for running Clojure tests (using the `clojure.test`
framework) via CIDER and seeing feedback in the test buffer about
which tests failed or errored. The installation instructions above
should work for clojure-test-mode as well.

Once you have a repl session active, you can run the tests in the
current buffer with <kbd>C-c C-,</kbd>. Failing tests and errors will be
highlighted using overlays. To clear the overlays, use <kbd>C-c k</kbd>.

## Paredit

Using clojure-mode with
[Paredit](http://mumble.net/~campbell/emacs/paredit.el) is highly
recommended. It helps ensure the structure of your forms is not
compromised and offers a number of operations that work on code
structure at a higher level than just characters and words.

It is also available using package.el from the above archives.

Use Paredit as you normally would any other minor mode; for instance:

```lisp
;; (require 'paredit) if you didn't install it via package.el
(add-hook 'clojure-mode-hook 'paredit-mode)
```

See [the cheat sheet](http://www.emacswiki.org/emacs/PareditCheatsheet)
for Paredit usage hints.

## REPL Interaction

A number of options exist for connecting to a running Clojure process
and evaluating code interactively.

### Basic REPL

Use <kbd>M-x run-lisp</kbd> to open a simple REPL subprocess using
[Leiningen](http://github.com/technomancy/leiningen). Once that has
opened, you can use <kbd>C-c C-r</kbd> to evaluate the region or
<kbd>C-c C-l</kbd> to load the whole file.

If you don't use Leiningen, you can set `inferior-lisp-program` to
a different REPL command.

### CIDER

You can also use [Leiningen](http://leiningen.org) to start an
enhanced REPL via [CIDER](https://github.com/clojure-emacs/cider).

### Ritz

Another option is [Ritz](https://github.com/pallet/ritz), which is a
bit more complicated but offers advanced debugging functionality using
SLIME.

### Swank Clojure

SLIME is available via
[swank-clojure](http://github.com/technomancy/swank-clojure) in `clojure-mode` 1.x.
SLIME support was removed in version 2.x in favor of `CIDER`.

## Indentation options

Characterizing the default indentation rules of clojure-mode is difficult to do
in summary; this is one attempt:

1. Bodies of parenthesized forms are indented such that arguments are aligned to
  the start column of the first argument, _except_ for a class of forms
  identified by the symbol in function position, the bodies of which are
  indented two spaces, regardless of the position of their first argument (this
  is called "defun" indentation, for historical reasons):
  1. Known special forms (e.g. `case`, `try`, etc)
  2. Nearly all "core" macros that ship as part of Clojure itself
  3. Userland macros (and any other form?) that are locally registered via
  `put-clojure-indent`, `define-clojure-indent` (helpers for adding mappings to
  `clojure-indent-function`).
2. The bodies of certain more complicated macros and special forms
  (e.g. `letfn`, `deftype`, `extend-protocol`, etc) are indented using a
  contextual backtracking indentation method, controlled by
  `clojure-backtracking-indent`.
3. The bodies of other forms (e.g. vector, map, and set literals) are indented
  such that each new line within the form is set just inside of the opening
  delimiter of the form.

Please see the docstrings of the elisp functions/vars noted above for
information about customizing this indentation behaviour.

## License

Copyright © 2007-2013 Jeffrey Chu, Lennart Staflin, Phil Hagelberg,
and [contributors](https://github.com/clojure-emacs/clojure-mode/contributors).

Distributed under the GNU General Public License; type <kbd>C-h C-c</kbd> to view it.
