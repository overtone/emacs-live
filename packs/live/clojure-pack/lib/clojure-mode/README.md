# Clojure Mode

Provides Emacs font-lock, indentation, and navigation for the
[Clojure language](http://clojure.org).

## Installation

Available on the [Marmalade repo](http://marmalade-repo.org/packages/clojure-mode).

If you're not already using Marmalade, add this to your
`~/.emacs.d/init.el` and load it with `M-x eval-buffer`.

```lisp
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
```

And then you can install:

`M-x package-install [RET] clojure-mode [RET]`

or

```lisp
(when (not (package-installed-p 'clojure-mode))
  (package-install 'clojure-mode))
```

On Emacs 23 you will need to get [package.el](http://bit.ly/pkg-el23)
yourself or install manually by placing `clojure-mode.el` on your `load-path`
and `require`ing it.

## Clojure Test Mode

This source repository also includes `clojure-test-mode.el`, which
provides support for running Clojure tests (using the `clojure.test`
framework) via nrepl.el or SLIME and seeing feedback in the test
buffer about which tests failed or errored. The installation
instructions above should work for clojure-test-mode as well.
(nrepl.el support is still in progress.)

Once you have a repl session active, you can run the tests in the
current buffer with `C-c C-,`. Failing tests and errors will be
highlighted using overlays. To clear the overlays, use `C-c k`.

You can jump between implementation and test files with `C-c t` if
your project is laid out in a way that clojure-test-mode expects. Your
project root should have a `src/` directory containing files that
correspond to their namespace. It should also have a `test/` directory
containing files that correspond to their namespace, and the test
namespaces should mirror the implementation namespaces with the
addition of "-test" as the suffix to the last segment of the namespace.

So `my.project.frob` would be found in `src/my/project/frob.clj` and
its tests would be in `test/my/project/frob_test.clj` in the
`my.project.frob-test` namespace.

## Paredit

Using clojure-mode with paredit is highly recommended. It is also
available using package.el from the above archive.

Use paredit as you normally would with any other mode; for instance:

```lisp
;; (require 'paredit) if you didn't install via package.el
(defun turn-on-paredit () (paredit-mode 1))
(add-hook 'clojure-mode-hook 'turn-on-paredit)
```

See [the cheat sheet](http://www.emacswiki.org/emacs/PareditCheatsheet)
for paredit usage hints.

## REPL Interaction

A number of options exist for connecting to a running Clojure process
and evaluating code interactively.

## Basic REPL

Use <kbd>M-x run-lisp</kbd> to open a simple REPL subprocess using
[Leiningen](http://github.com/technomancy/leiningen). Once that has
opened, you can use <kbd>C-c C-r</kbd> to evaluate the region or
<kbd>C-c C-l</kbd> to load the whole file.

If you don't use Leiningen, you can set `inferior-lisp-program` to
a different REPL command.

## nrepl.el

You can also use [Leiningen](http://leiningen.org) to start an
enhanced REPL via [nrepl.el](https://github.com/kingtim/nrepl.el).

## Ritz

Another option is [Ritz](https://github.com/pallet/ritz), which is a
bit more complicated but offers advanced debugging functionality using
SLIME.

## Swank Clojure

SLIME is also available via
[swank-clojure](http://github.com/technomancy/swank-clojure), though
it is no longer actively maintained.

## License

Copyright © 2007-2012 Jeffrey Chu, Lennart Staflin, Phil Hagelberg,
and [contributors](https://github.com/technomancy/clojure-mode/contributors).

Distributed under the GNU General Public License; see C-h t to view.
