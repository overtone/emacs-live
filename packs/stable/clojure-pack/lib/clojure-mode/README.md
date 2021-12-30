[![circleci][badge-circleci]][circleci]
[![MELPA][melpa-badge]][melpa-package]
[![MELPA Stable][melpa-stable-badge]][melpa-stable-package]
[![NonGNU ELPA](https://elpa.nongnu.org/nongnu/clojure-mode.svg)](https://elpa.nongnu.org/nongnu/clojure-mode.html)
[![Discord](https://img.shields.io/badge/chat-on%20discord-7289da.svg?sanitize=true)](https://discord.com/invite/nFPpynQPME)
[![License GPL 3][badge-license]][copying]

# Clojure Mode

`clojure-mode` is an Emacs major mode that provides font-lock (syntax
highlighting), indentation, navigation and refactoring support for the
[Clojure(Script) programming language](http://clojure.org).

-----------

**This documentation tracks the `master` branch of `clojure-mode`. Some of
the features and settings discussed here might not be available in
older releases (including the current stable release). Please, consult
the relevant git tag (e.g. 5.1.0) if you need documentation for a
specific `clojure-mode` release.**

## Installation

Available on the major `package.el` community maintained repos -
[MELPA Stable][] and [MELPA][] repos.

MELPA Stable is the recommended repo as it has the latest stable
version.  MELPA has a development snapshot for users who don't mind
(infrequent) breakage but don't want to run from a git checkout.

You can install `clojure-mode` using the following command:

<kbd>M-x package-install [RET] clojure-mode [RET]</kbd>

or if you'd rather keep it in your dotfiles:

```el
(unless (package-installed-p 'clojure-mode)
  (package-install 'clojure-mode))
```

If the installation doesn't work try refreshing the package list:

<kbd>M-x package-refresh-contents</kbd>

## Bundled major modes

The `clojure-mode` package actually bundles together several major modes:

* `clojure-mode` is a major mode for editing Clojure code
* `clojurescript-mode` is a major mode for editing ClojureScript code
* `clojurec-mode` is a major mode for editing `.cljc` source files

All the major modes derive from `clojure-mode` and provide more or less the same
functionality.  Differences can be found mostly in the font-locking -
e.g. ClojureScript has some built-in constructs that are not present in Clojure.

The proper major mode is selected automatically based on the extension of the
file you're editing.

Having separate major modes gives you the flexibility to attach different hooks
to them and to alter their behavior individually (e.g. add extra font-locking
just to `clojurescript-mode`) .

Note that all modes derive from `clojure-mode`, so things you add to
`clojure-mode-hook` and `clojure-mode-map` will affect all the derived modes as
well.

## Configuration

In the spirit of Emacs, pretty much everything you can think of in `clojure-mode` is configurable.

To see a list of available configuration options do `M-x customize-group RET clojure`.

### Indentation options

The default indentation rules in `clojure-mode` are derived from the
[community Clojure Style Guide](https://guide.clojure.style).
Please, refer to the guide for the general Clojure indentation rules.

#### Indentation of docstrings

By default multi-line docstrings are indented with 2 spaces, as this is a
somewhat common standard in the Clojure community. You can however adjust this
by modifying `clojure-docstring-fill-prefix-width`. Set it to 0 if you don't
want multi-line docstrings to be indented at all (which is pretty common in most lisps).

#### Indentation of function forms

The indentation of function forms is configured by the variable
`clojure-indent-style`. It takes three possible values:

- `always-align` (the default)

```clj
(some-function
 10
 1
 2)
(some-function 10
               1
               2)
```

- `always-indent`

```clj
(some-function
  10
  1
  2)
(some-function 10
  1
  2)
```

- `align-arguments`

```clj
(some-function
  10
  1
  2)
(some-function 10
               1
               2)
```

**Note:** Prior to clojure-mode 5.10 the configuration options for `clojure-indent-style` used to be
keywords, but now they are symbols. Keywords will still be supported at least until clojure-mode 6.

#### Indentation of macro forms

The indentation of special forms and macros with bodies is controlled via
`put-clojure-indent`, `define-clojure-indent` and `clojure-backtracking-indent`.
Nearly all special forms and built-in macros with bodies have special indentation
settings in `clojure-mode`. You can add/alter the indentation settings in your
personal config. Let's assume you want to indent `->>` and `->` like this:

```clojure
(->> something
  ala
  bala
  portokala)
```

You can do so by putting the following in your config:

```el
(put-clojure-indent '-> 1)
(put-clojure-indent '->> 1)
```

This means that the body of the `->/->>` is after the first argument.

A more compact way to do the same thing is:

```el
(define-clojure-indent
  (-> 1)
  (->> 1))
```

To indent something like a definition (`defn`) you can do something like:

``` el
(put-clojure-indent '>defn :defn)
```

You can also specify different indentation settings for symbols
prefixed with some ns (or ns alias):

```el
(put-clojure-indent 'do 0)
(put-clojure-indent 'my-ns/do 1)
```

The bodies of certain more complicated macros and special forms
(e.g. `letfn`, `deftype`, `extend-protocol`, etc) are indented using
a contextual backtracking indentation method, require more sophisticated
indent specifications. Here are a few examples:

```el
(define-clojure-indent
  (implement '(1 (1)))
  (letfn     '(1 ((:defn)) nil))
  (proxy     '(2 nil nil (1)))
  (reify     '(:defn (1)))
  (deftype   '(2 nil nil (1)))
  (defrecord '(2 nil nil (1)))
  (specify   '(1 (1)))
  (specify   '(1 (1))))
```

These follow the same rules as the `:style/indent` metadata specified by [cider-nrepl][].
For instructions on how to write these specifications, see
[this document](https://docs.cider.mx/cider/indent_spec.html).
The only difference is that you're allowed to use lists instead of vectors.

The indentation of [special arguments](https://docs.cider.mx/cider/indent_spec.html#special-arguments) is controlled by
`clojure-special-arg-indent-factor`, which by default indents special arguments
a further `lisp-body-indent` when compared to ordinary arguments.

An example of the default formatting is:

```clojure
(defrecord MyRecord
    [my-field])
```

Note that `defrecord` has two special arguments, followed by the form's body -
namely the record's name and its fields vector.

Setting `clojure-special-arg-indent-factor` to 1, results in:

```clojure
(defrecord MyRecord
  [my-field])
```

### Indentation of Comments

`clojure-mode` differentiates between comments like `;`, `;;`, etc.
By default `clojure-mode` treats `;` as inline comments and *always* indents those.
You can change this behaviour like this:

```emacs-lisp
(add-hook 'clojure-mode-hook (lambda () (setq-local comment-column 0)))
```

You might also want to change `comment-add` to 0 in that way, so that Emacs comment
functions (e.g. `comment-region`) would use `;` by default instead of `;;`.

**Note:** Check out [this section](https://guide.clojure.style/#comments) of the Clojure style guide to understand better the semantics of the different comment levels and why `clojure-mode` treats them differently by default.

### Vertical alignment

You can vertically align sexps with `C-c SPC`. For instance, typing
this combo on the following form:

```clj
(def my-map
  {:a-key 1
   :other-key 2})
```

Leads to the following:

```clj
(def my-map
  {:a-key     1
   :other-key 2})
```

This can also be done automatically (as part of indentation) by
turning on `clojure-align-forms-automatically`. This way it will
happen whenever you select some code and hit `TAB`.

### Font-locking

`clojure-mode` features static font-locking (syntax highlighting) that you can extend yourself
if needed. As typical for Emacs, it's based on regular expressions. You can find
the default font-locking rules in `clojure-font-lock-keywords`. Here's how you can add font-locking for built-in Clojure functions and vars:

``` el
(defvar clojure-built-in-vars
  '(;; clojure.core
    "accessor" "aclone"
    "agent" "agent-errors" "aget" "alength" "alias"
    "all-ns" "alter" "alter-meta!" "alter-var-root" "amap"
    ;; omitted for brevity
    ))

(defvar clojure-built-in-dynamic-vars
  '(;; clojure.test
    "*initial-report-counters*" "*load-tests*" "*report-counters*"
    "*stack-trace-depth*" "*test-out*" "*testing-contexts*" "*testing-vars*"
    ;; clojure.xml
    "*current*" "*sb*" "*stack*" "*state*"
    ))

(font-lock-add-keywords 'clojure-mode
                        `((,(concat "(\\(?:\.*/\\)?"
                                    (regexp-opt clojure-built-in-vars t)
                                    "\\>")
                           1 font-lock-builtin-face)))

(font-lock-add-keywords 'clojure-mode
                        `((,(concat "\\<"
                                    (regexp-opt clojure-built-in-dynamic-vars t)
                                    "\\>")
                           0 font-lock-builtin-face)))

```

**Note:** The package `clojure-mode-extra-font-locking` provides such additional
font-locking for Clojure built-ins.

As you might imagine one problem with this font-locking approach is that because
it's based on regular expressions you'll get some false positives here and there
(there's no namespace information, and no way for `clojure-mode` to know what
var a symbol resolves to). That's why `clojure-mode`'s font-locking defaults are
conservative and minimalistic.

Precise font-locking requires additional data that can obtained from a running
REPL (that's how CIDER's [dynamic font-locking](https://docs.cider.mx/cider/config/syntax_highlighting.html) works) or from static code analysis.

When it comes to definitions, `clojure-mode` employs a simple heuristic and will treat every symbol named `def`something as a built-in keyword. Still, you'll need to
teach `clojure-mode` manually how to handle the docstrings of non built-in definition forms. Here's an example:

``` emacs-lisp
(put '>defn 'clojure-doc-string-elt 2)
```

**Note:** The `clojure-doc-string-elt` attribute is processed by the function `clojure-font-lock-syntactic-face-function`.

## Refactoring support

The available refactorings were originally created and maintained by the
`clj-refactor.el` team. The ones implemented in Elisp only are gradually migrated
to `clojure-mode`.

### Threading macros related features

`clojure-thread`: Thread another form into the surrounding thread. Both `->>`
and `->` variants are supported.

<img width="512" src="/doc/clojure-thread.gif">

`clojure-unwind`: Unwind a threaded expression. Supports both `->>` and `->`.

<img width="512" src="/doc/clojure-unwind.gif">

`clojure-thread-first-all`: Introduce the thread first macro (`->`) and rewrite
the entire form. With a prefix argument do not thread the last form.

<img width="512" src="/doc/clojure-thread-first-all.gif">

`clojure-thread-last-all`: Introduce the thread last macro and rewrite the
entire form. With a prefix argument do not thread the last form.

<img width="512" src="/doc/clojure-thread-last-all.gif">

`clojure-unwind-all`: Fully unwind a threaded expression removing the threading
macro.

<img width="512" src="/doc/clojure-unwind-all.gif">

### Cycling things

`clojure-cycle-privacy`: Cycle privacy of `def`s or `defn`s. Use metadata
explicitly with setting `clojure-use-metadata-for-privacy` to `t` for `defn`s
too.

<img width="512" src="/doc/clojure-cycle-privacy.gif">

`clojure-cycle-not`: Add or remove a `not` form around the current form.

<img width="512" src="/doc/clojure-cycle-not.gif">

`clojure-cycle-when`: Find the closest `when` or `when-not` up the syntax tree
and toggle it.

<img width="512" src="/doc/clojure-cycle-when.gif">

`clojure-cycle-if`: Find the closest `if` or `if-not` up the syntax tree and
toggle it. Also transpose the `else` and `then` branches, keeping the semantics
the same as before.

<img width="512" src="/doc/clojure-cycle-if.gif">

### Convert collection

Convert any given collection at point to list, quoted list, map, vector or set.

### Let expression

`clojure-introduce-let`: Introduce a new `let` form. Put the current form into
its binding form with a name provided by the user as a bound name. If called
with a numeric prefix put the let form Nth level up in the form hierarchy.

<img width="512" src="/doc/clojure-introduce-let.gif">

`clojure-move-to-let`: Move the current form to the closest `let`'s binding
form. Replace all occurrences of the form in the body of the let.

<img width="512" src="/doc/clojure-move-to-let.gif">

`clojure-let-forward-slurp-sexp`: Slurp the next form after the `let` into the
`let`. Replace all occurrences of the bound forms in the form added to the `let`
form. If called with a prefix argument slurp the next n forms.

<img width="512" src="/doc/clojure-let-forward-slurp-sexp.gif">

`clojure-let-backward-slurp-sexp`: Slurp the form before the `let` into the
`let`. Replace all occurrences of the bound forms in the form added to the `let`
form. If called with a prefix argument slurp the previous n forms.

<img width="512" src="/doc/clojure-let-backward-slurp-sexp.gif">

`paredit-convolute-sexp` is advised to replace occurrences of bound forms with their bound names when convolute is used on a let form.

### Rename ns alias

`clojure-rename-ns-alias`: Rename an alias inside a namespace declaration,
and all of its usages in the buffer

<img width="512" src="/doc/clojure-rename-ns-alias.gif">

If there is an active selected region, only rename usages of aliases within the region,
without affecting the namespace declaration.

<img width="512" src="/doc/clojure-rename-ns-alias-region.gif">

### Add arity to a function

`clojure-add-arity`: Add a new arity to an existing single-arity or multi-arity function.

<img width="512" src="/doc/clojure-add-arity.gif">

## Related packages

* [clojure-mode-extra-font-locking][] provides additional font-locking
for built-in methods and macros.  The font-locking is pretty
imprecise, because it doesn't take namespaces into account and it
won't font-lock a function at all possible positions in a sexp, but
if you don't mind its imperfections you can easily enable it:

```el
(require 'clojure-mode-extra-font-locking)
```

The code in `clojure-mode-font-locking` used to be bundled with
`clojure-mode` before version 3.0.

You can also use the code in this package as a basis for extending the
font-locking further (e.g. functions/macros from more
namespaces). Generally you should avoid adding special font-locking
for things that don't have fairly unique names, as this will result in
plenty of incorrect font-locking. CIDER users should avoid this package,
as CIDER does its own dynamic font-locking, which is namespace-aware
and doesn't produce almost any false positives.

* [clj-refactor][] provides refactoring support.

* Enabling `CamelCase` support for editing commands(like
`forward-word`, `backward-word`, etc) in `clojure-mode` is quite
useful since we often have to deal with Java class and method
names. The built-in Emacs minor mode `subword-mode` provides such
functionality:

```el
(add-hook 'clojure-mode-hook #'subword-mode)
```

* The use of [paredit][] when editing Clojure (or any other Lisp) code
is highly recommended. It helps ensure the structure of your forms is
not compromised and offers a number of operations that work on code
structure at a higher level than just characters and words. To enable
it for Clojure buffers:

```el
(add-hook 'clojure-mode-hook #'paredit-mode)
```

* [smartparens][] is an excellent
  (newer) alternative to paredit. Many Clojure hackers have adopted it
  recently and you might want to give it a try as well. To enable
  `smartparens` use the following code:

```el
(add-hook 'clojure-mode-hook #'smartparens-strict-mode)
```

* [RainbowDelimiters][] is a
  minor mode which highlights parentheses, brackets, and braces
  according to their depth. Each successive level is highlighted in a
  different color. This makes it easy to spot matching delimiters,
  orient yourself in the code, and tell which statements are at a
  given depth. Assuming you've already installed `RainbowDelimiters` you can
  enable it like this:

```el
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
```

* [aggressive-indent-mode][] automatically adjust the indentation of your code,
while you're writing it. Using it together with `clojure-mode` is highly
recommended. Provided you've already installed `aggressive-indent-mode` you can
enable it like this:

```el
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)
```

## REPL Interaction

One of the fundamental aspects of Lisps in general, and Clojure in
particular, is the notion of interactive programming - building your
programs by continuously changing the state of the running Lisp
program (as opposed to doing something more traditional like making a
change and re-running the program afterwards to see the changes in
action). To get the most of clojure-mode you'll have to combine it
with some tool which will allow you to interact with your Clojure program
(a.k.a. process/REPL).

A number of options exist for connecting to a
running Clojure process and evaluating code interactively.

### Basic REPL

[inf-clojure][] provides basic interaction with a Clojure REPL process.
It's very similar in nature and supported functionality to `inferior-lisp-mode`
for Common Lisp.

### CIDER

[CIDER][] is a powerful Clojure interactive development environment,
similar to SLIME for Common Lisp.

If you're into Clojure and Emacs you should definitely check it out.

## Tutorials

Tutorials,
targeting Emacs beginners, are available at
[clojure-doc.org](http://clojure-doc.org/articles/tutorials/emacs.html) and
[Clojure for the Brave and the True](http://www.braveclojure.com/basic-emacs/).
Keep in mind, however, that they might be out-of-date.

## Caveats

`clojure-mode` is a capable tool, but it's certainly not perfect. This section
lists a couple of general design problems/limitations that might affect your
experience negatively.

### General Issues

`clojure-mode` derives a lot of functionality directly from `lisp-mode` (an Emacs major mode for Common Lisp), which
simplified the initial implementation, but also made it harder to implement
certain functionality. Down the road it'd be nice to fully decouple `clojure-mode`
from `lisp-mode`.

See [this ticket](https://github.com/clojure-emacs/clojure-mode/issues/270) for a bit more details.

### Indentation Performance

`clojure-mode`'s indentation engine is a bit slow. You can speed things up significantly by disabling `clojure-use-backtracking-indent`, but this will break the indentation of complex forms like `deftype`, `defprotocol`, `reify`, `letfn`, etc.

We should look into ways to optimize the performance of the backtracking indentation logic. See [this ticket](https://github.com/clojure-emacs/clojure-mode/issues/606) for more details.

### Font-locking Implementation

As mentioned [above](https://github.com/clojure-emacs/clojure-mode#font-locking), the font-locking is implemented in terms of regular expressions which makes it both slow and inaccurate.

## Changelog

An extensive changelog is available [here](CHANGELOG.md).

## License

Copyright © 2007-2021 Jeffrey Chu, Lennart Staflin, Phil Hagelberg, Bozhidar
Batsov, Artur Malabarba, Magnar Sveen and [contributors][].

Distributed under the GNU General Public License; type <kbd>C-h C-c</kbd> to view it.

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
[melpa-badge]: http://melpa.org/packages/clojure-mode-badge.svg
[melpa-stable-badge]: http://stable.melpa.org/packages/clojure-mode-badge.svg
[melpa-package]: http://melpa.org/#/clojure-mode
[melpa-stable-package]: http://stable.melpa.org/#/clojure-mode
[COPYING]: http://www.gnu.org/copyleft/gpl.html
[badge-circleci]: https://circleci.com/gh/clojure-emacs/clojure-mode.svg?style=svg
[circleci]: https://circleci.com/gh/clojure-emacs/clojure-mode
[CIDER]: https://github.com/clojure-emacs/cider
[cider-nrepl]: https://github.com/clojure-emacs/cider-nrepl
[inf-clojure]: https://github.com/clojure-emacs/inf-clojure
[contributors]: https://github.com/clojure-emacs/clojure-mode/contributors
[melpa]: http://melpa.org
[melpa stable]: http://stable.melpa.org
[clojure-mode-extra-font-locking]: https://github.com/clojure-emacs/clojure-mode/blob/master/clojure-mode-extra-font-locking.el
[clj-refactor]: https://github.com/clojure-emacs/clj-refactor.el
[paredit]: http://mumble.net/~campbell/emacs/paredit.html
[smartparens]: https://github.com/Fuco1/smartparens
[RainbowDelimiters]: https://github.com/Fanael/rainbow-delimiters
[aggressive-indent-mode]: https://github.com/Malabarba/aggressive-indent-mode
