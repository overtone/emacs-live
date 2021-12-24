[![MELPA](https://melpa.org/packages/a-badge.svg)](https://melpa.org/#/a)
[![MELPA Stable](https://stable.melpa.org/packages/a-badge.svg)](https://stable.melpa.org/#/a)

# a.el

Emacs Lisp functions for dealing with associative structures in a uniform and functional way.

Inspired by Clojure, dash, and seq.el.

These functions can take association lists, hash tables, and in some cases vectors (where the index is considered the key).

This library copies the names and semantics of the Clojure standard library. If you know Clojure then just add `a-` to the function name. The only exceptions are:

- `a-alist` is an association list contructor, it has no Clojure counterpart.
- `a-has-key?` is the equivalent of Clojure's `contains?`. This historical naming mistake won't be fixed in Clojure, but we can fix it here.
- predicates have both a `?` and a `-p` version, e.g. `a-has-key-p`. Use the latter if you want greater consistency with existing Elisp code.

All functions in this library are pure, they do not mutate their arguments.

## Alternatives

The one unique selling point for `a.el` is that it lets you reuse your Clojure experience in Emacs, so Clojure programmers can be productive quickly. If you are not experienced in Clojure, or you want to do things "the Emacs way", then consider these alternatives.

- [map.el](https://github.com/emacs-mirror/emacs/blob/master/lisp/emacs-lisp/map.el) comes bundled with recent Emacs versions
- [asoc.el](https://github.com/troyp/asoc.el) Association list library
- [ht.el](https://github.com/Wilfred/ht.el) Hash table library
- [kv.el](https://github.com/nicferrier/emacs-kv) A collection of tools for dealing with key/value data structures such as plists, alists and hash-tables.

## Requirements

a.el relies on features that are part of Emacs 25, so you need Emacs 25 or later. There are no other dependencies.

## Installation

a.el is available from Lambda Island ELPA. Add this to your `~/.emacs` or `~/.emacs.d/init.el`

``` emacs-lisp
(require 'package)
(add-to-list 'package-archives
             '("lambdaisland" . "http://lambdaisland.github.io/elpa/") t)
```

## Functions

``` emacs-lisp
(a-list :foo 5 :bar 6)
;;=> ((:foo . 5) (:bar . 6))

(setq m (a-list :foo 5 :bar 6))
(setq h (a-hash-table :abc 123 :def 456))

(a-associative? m)
;;=> t
(a-associative? h)
;;=> t

(a-get m :foo)
;;=> 5
(a-get h :abc)
;;=> 123

(a-assoc m :foo 7 :baq 20)
;;=> ((:baq . 20) (:foo . 7) (:bar . 6))
(a-assoc h :foo 7)
;;=> #s(hash-table ... (:abc 123 :def 456 :foo 7 ...))

(a-keys m)
;;=> (:foo :bar)
(a-keys h)
;;=> (:def :abc)

(a-vals m)
;;=> (5 6)
(a-vals h)
;;=> (456 123)

(a-equal m (a-list :bar 6 :foo 5))
;;=> t

(a-has-key? m :bar)
;;=> t

(a-count h)
;;=> 2

(a-dissoc m :foo)
;;=> ((:bar . 6))

(a-assoc-in (a-list :name "Arne")
            [:stats :score] 100)
;;=> ((:name . "Arne") (:stats . ((:score . 100))))

(a-merge m h (a-list :and :more))
;;=> ((:and . :more) (:abc . 123) (:def . 456) (:foo . 5) (:bar . 6))

(a-merge-with '+ m (a-list :foo 10))
;;=> ((:foo . 15) (:bar . 6))

(a-update (a-list :name "Arne") :name 'concat " Brasseur")
;;=> ((:name . "Arne Brasseur"))

(setq player (a-list :name "Arne" :stats (a-list :score 99)))
(a-update-in player  [:stats :score] '+ 1)
;;=> ((:name . "Arne") (:stats (:score . 100)))
```

## LICENSE

&copy; Arne Brasseur 2017

Distributed under the terms of the GNU General Public License, version 3.0 or later. See LICENSE.
