[![Build Status](https://travis-ci.org/clojure-emacs/parseclj.svg?branch=master)](https://travis-ci.org/clojure-emacs/parseclj)

# Clojure parser for Emacs Lisp

`parseclj` is an Emacs Lisp library for parsing Clojure code and [EDN
data](https://github.com/edn-format/edn). It supports several input and output
formats, all powered by the same shift-reduce parser function.

Take a look at the [design document](DESIGN.md) for more details.

`parseclj` is in **alpha** state right now, its API might be subject to change.

## Installation

Available on the major `package.el` community maintained repos -
[MELPA Stable][] and [MELPA][] repos.

MELPA Stable is the recommended repo as it has the latest stable
version.  MELPA has a development snapshot for users who don't mind
(infrequent) breakage but don't want to run from a git checkout.

You can install `parseclj` using the following command:

<kbd>M-x package-install [RET] parseclj [RET]</kbd>

or if you'd rather keep it in your dotfiles:

```el
(unless (package-installed-p 'parseclj)
  (package-install 'parseclj))
```

If the installation doesn't work try refreshing the package list:

<kbd>M-x package-refresh-contents</kbd>

[melpa]: http://melpa.org
[melpa stable]: http://stable.melpa.org

## Usage

`parseclj` contains function that return an
[AST](https://en.wikipedia.org/wiki/Abstract_syntax_tree) that, for example,
given as input `(1 2 [:a :b :c])`, it looks like this:

``` emacs-lisp
((:node-type . :root)
 (:position . 1)
 (:children ((:node-type . :list)
             (:position . 1)
             (:children ((:node-type . :number)
                         (:position . 2)
                         (:form . "1")
                         (:value . 1))
                        ((:node-type . :number)
                         (:position . 4)
                         (:form . "2")
                         (:value . 2))
                        ((:node-type . :vector)
                         (:position . 6)
                         (:children ((:node-type . :keyword)
                                     (:position . 7)
                                     (:form . ":a")
                                     (:value . :a))
                                    ((:node-type . :keyword)
                                     (:position . 10)
                                     (:form . ":b")
                                     (:value . :b))
                                    ((:node-type . :keyword)
                                     (:position . 13)
                                     (:form . ":c")
                                     (:value . :c))))))))
```

In order to use any of these functions, you first need to require it:

```emacs-lisp
(require 'parseclj)
```

And then you will have the following functions at your disposal:

- `parseclj-parse-clojure` &rest string-and-options

    When no arguments, parses Clojure source code into an AST and returns it.
    When given a string as a first argument, parses it and returns the
    corresponding AST.

    A list of options can be passed down to the parsing process, particularly:
    * `:lexical-preservation`: a boolean value to retain whitespace, comments,
      and discards.  Defaults to nil.
    * `:fail-fast`: a boolean value to raise an error when encountering invalid
      syntax.  Defaults to t.

    Examples:

   ```emacs-lisp
   (parseclj-parse-clojure) ;; will parse clojure code in the current buffer and return an AST
   (parseclj-parse-clojure "(1 2 3)")  ;; => ((:node-type . :root) ... )
   (parseclj-parse-clojure :lexical-preservation t) ;; will parse clojure code in current buffer preserving whitespaces, comments and discards
   ```

    > Note: there's an open issue to extend this API to [parse clojure code within
    > some boundaries of a
    > buffer](https://github.com/clojure-emacs/parseclj/issues/13).  Pull requests
    > are welcome.

- `parseclj-unparse-clojure` ast

    Transform the given AST into Clojure source code and inserts it into the
    current buffer.

- `parseclj-unparse-clojure-to-string` ast

    Transfrom the given AST into Clojure source code and returns it as a string.


## License

&copy; 2017-2021 Arne Brasseur and contributors.

Distributed under the terms of the GNU General Public License 3.0 or later. See
[LICENSE](LICENSE).
