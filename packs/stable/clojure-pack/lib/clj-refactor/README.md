# clj-refactor.el [![Build Status](https://secure.travis-ci.org/magnars/clj-refactor.el.png)](http://travis-ci.org/magnars/clj-refactor.el)

A collection of simple clojure refactoring functions. Please send help.

## Installation

I highly recommend installing clj-refactor through elpa.

It's available on [marmalade](http://marmalade-repo.org/) and
[melpa](http://melpa.milkbox.net/):

    M-x package-install clj-refactor

You can also install the dependencies on your own, and just dump
clj-refactor in your path somewhere:

 - <a href="https://github.com/magnars/s.el">s.el</a>
 - <a href="https://github.com/magnars/dash.el">dash.el</a>
 - <a href="https://github.com/capitaomorte/yasnippet">yasnippet</a>
 - <a href="http://mumble.net/~campbell/emacs/paredit.el">paredit</a>
 - <a href="https://github.com/magnars/multiple-cursors.el">multiple-cursors</a>

## Setup

```cl
(require 'clj-refactor)
(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               ;; insert keybinding setup here
                               ))
```

You'll also have to set up the keybindings in the lambda. Read on.

## Setup keybindings

All functions in clj-refactor have a two-letter mnemonic shortcut. For
instance, rename-file is `rf`. You get to choose how those are bound.
Here's how:

```cl
(cljr-add-keybindings-with-prefix "C-c C-m")
;; eg. rename files with `C-c C-m rf`.
```

If you would rather have a modifier key, instead of a prefix, do:

```cl
(cljr-add-keybindings-with-modifier "C-s-")
;; eg. rename files with `C-s-r C-s-f`.
```

If neither of these appeal to your sense of keyboard layout aesthetics, feel free
to pick and choose your own keybindings with a smattering of:

```cl
(define-key clj-refactor-map (kbd "C-x C-r") 'cljr-rename-file)
```

## Usage

This is it so far:

 - `th`: thread another expression
 - `uw`: unwind a threaded expression
 - `ua`: fully unwind a threaded expression
 - `tf`: wrap in thread-first (->) and fully thread
 - `tl`: wrap in thread-last (->>) and fully thread
 - `il`: introduce let
 - `el`: expand let
 - `ml`: move to let
 - `rf`: rename file, update ns-declaration, and then query-replace new ns in project.
 - `ar`: add require to namespace declaration, then jump back (see optional setup)
 - `au`: add "use" (ie require refer all) to namespace declaration, then jump back
 - `ai`: add import to namespace declaration, then jump back
 - `ru`: replace all `:use` in namespace with `:refer :all`
 - `sn`: sort :use, :require and :import in the ns form
 - `sr`: stop referring (removes `:refer []` from current require, fixing references)
 - `cc`: cycle surrounding collection type
 - `cp`: cycle privacy of `defn`s and `def`s
 - `cs`: cycle between "string" -> :string -> "string"
 - `ad`: add declaration for current top-level form
 - `dk`: destructure keys

Combine with your keybinding prefix/modifier.

## Thread / unwind example

Given this:

```clj
(map square (filter even? [1 2 3 4 5]))
```

Start by wrapping it in a threading macro:

```clj
(->> (map square (filter even? [1 2 3 4 5])))
```

And start threading away, using `cljr-thread`:

```clj
(->> (filter even? [1 2 3 4 5])
     (map square))
```

And again:

```clj
(->> [1 2 3 4 5]
     (filter even?)
     (map square))
```

You can also do all of these steps in one go.

Start again with:

```clj
(map square (filter even? [1 2 3 4 5]))
```

Put your cursor in front of the s-exp, and call `cljr-thread-last-all`:

```clj
(->> [1 2 3 4 5]
     (filter even?)
     (map square))
```

There is a corresponding `cljr-thread-first-all` as well.

To revert this, there's `cljr-unwind` to unwind one step at a time. Or
there's `cljr-unwind-all` to unwind the entire expression at once.

To see how that works, just read the examples in the other direction.

## Introduce / expand / move to let example

Given this:

```clj
(defn handle-request
  {:status 200
   :body (find-body abc)})
```

With the cursor in front of `(find-body abc)`, I do `cljr-introduce-let`:

```clj
(defn handle-request
  {:status 200
   :body (let [X (find-body abc)]
           X)})
```

Now I have two cursors where the `X`es are. Just type out the name,
and press enter. Of course, that's not where I wanted the let
statement. So I do `cljr-expand-let`:

```clj
(defn handle-request
  (let [body (find-body abc)]
    {:status 200
     :body body}))
```

Yay.

Next with the cursor in front of `200`, I do `cljr-move-to-let`:

```clj
(defn handle-request
  (let [body (find-body abc)
        X 200]
    {:status X
     :body body}))
```

Again I have two cursors where the `X`es are, so I type out the name,
and press enter:

```clj
(defn handle-request
  (let [body (find-body abc)
        status 200]
    {:status status
     :body body}))
```

Pretty handy. And it works with `if-let` and `when-let` too.

## Cycling Privacy

Given this function:

```clj
(defn add [a b]
  (+ a b))
```

I do `cljr-cycle-privacy`:

```clj
(defn- add [a b]
  (+ a b))
```

I do `cljr-cycle-privacy` again to return to the original:

```clj
(defn add [a b]
  (+ a b))
```

Given this def:

```clj
(def config
  "docs"
  {:env "staging"})
```

I do `cljr-cycle-privacy`:

```clj
(def ^:private config
  "docs"
  {:env "staging"})
```

I do `cljr-cycle-privacy` again to return to the original:

```clj
(def config
  "docs"
  {:env "staging"})
```

## Cycling Collection Type

Given this collection:

```clj
(:a 1 :b 2)
```

I do `cljr-cycle-coll` to return:

```clj
{:a 1 :b 2}
```

... and then 3 more times:

```clj
[:a 1 :b 2]
#{:a 1 :b 2}
(:a 1 :b 2)
```

## Cycling Between Strings and Keywords

Given this string:

```clj
"refactor"
```

I do `cljr-cycle-stringlike` to return:

```clj
:refactor
```

... and then 3 more times:

```clj
"refactor"
:refactor
"refactor"
```

Thanks to [Jay Fields](https://github.com/jaycfields) and
[emacs-live](https://github.com/overtone/emacs-live) for these cycling features. Good idea!

## Destructuring keys

Given this:

```clj
(defn- render-recommendation [rec]
  (list [:h3 (:title rec)]
        [:p (:by rec)]
        [:p (:blurb rec) " "
         (render-link (:link rec))]))
```

I place the cursor on `rec` inside `[rec]` and do `cljr-destructure-keys`:

```clj
(defn- render-recommendation [{:keys [title by blurb link]}]
  (list [:h3 title]
        [:p by]
        [:p blurb " "
         (render-link link)]))
```

If `rec` had still been in use, it would have added an `:as` clause.

For now this feature is limited to top-level symbols in a let form. PR welcome.

## Stop referring

Given this:

```clj
(ns cljr.core
  (:require [my.lib :as lib :refer [a b]]))

(+ (a 1) (b 2))
```

I place cursor on `my.lib` and do `cljr-stop-referring`:

```clj
(ns cljr.core
  (:require [my.lib :as lib]))

(+ (lib/a 1) (lib/b 2))
```

## Optional setup

If you're not using yasnippet, then the "jumping back"-part of adding to
namespace won't work. To remedy that, enable the mode with either:

```cl
(yas/global-mode 1)
```

or

```cl
(add-hook 'clojure-mode-hook (lambda () (yas/minor-mode 1)))
```

It is an excellent package, so I recommend looking into it. Here are
some snippet packages for Clojure:

 - David Nolen has created some [clojure-snippets](https://github.com/swannodette/clojure-snippets)
 - I've made some [datomic-snippets](https://github.com/magnars/datomic-snippets)

## Automatic insertion of namespace declaration

When you open a blank `.clj`-file, clj-refactor inserts the namespace
declaration for you.

It will also add the relevant `:use` clauses in test files, normally
using `clojure.test`, but if you're depending on midje in your
`project.clj` it uses that instead.

Like clojure-mode, clj-refactor presumes that you are postfixing your
test files with `_test`.

Prefer to insert your own ns-declarations? Then:

```cl
(setq clj-add-ns-to-blank-clj-files nil)
```

## More stuff to check out

You might also like

- [align-cljlet](https://github.com/gstamp/align-cljlet) - which is an Emacs package for aligning let-like forms.

## Changelog

#### From 0.9 to 0.10

- Add `cljr-stop-referring`
- Add `cljr-destructure-keys`
- Add `cljr-sort-ns` [AlexBaranosky](https://github.com/AlexBaranosky)

#### From 0.8 to 0.9

- Add `cljr-replace-use` [Lars Andersen](https://github.com/expez)
- Add `cljr-add-declaration` [Lars Andersen](https://github.com/expez)

#### From 0.7 to 0.8

- Add `cljr-cycle-stringlike` [AlexBaranosky](https://github.com/AlexBaranosky)
- Add `cljr-cycle-coll` [AlexBaranosky](https://github.com/AlexBaranosky)
- Add `cljr-cycle-privacy` [AlexBaranosky](https://github.com/AlexBaranosky)

#### From 0.6 to 0.7

- Add `cljr-thread-first-all`, `cljr-thread-last-all` and `cljr-unwind-all` [AlexBaranosky](https://github.com/AlexBaranosky)

#### From 0.5 to 0.6

- Add `cljr-move-to-let` [AlexBaranosky](https://github.com/AlexBaranosky)

## Contribute

Yes, please do. There's a suite of tests, so remember to add tests for your
specific feature, or I might break it later.

You'll find the repo at:

    https://github.com/magnars/clj-refactor.el

To fetch the test dependencies, install
[cask](https://github.com/rejeep/cask.el) if you haven't already,
then:

    $ cd /path/to/clj-refactor
    $ cask

Run the tests with:

    $ ./run-tests.sh

## Contributors

- [AlexBaranosky](https://github.com/AlexBaranosky) added a bunch of features. See the [Changelog](#changelog) for details.
- [Lars Andersen](https://github.com/expez) added `cljr-replace-use` and `cljr-add-declaration`.

Thanks!

## License

Copyright © 2012-2013 Magnar Sveen

Authors: Magnar Sveen <magnars@gmail.com>
Keywords: clojure convenience

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
