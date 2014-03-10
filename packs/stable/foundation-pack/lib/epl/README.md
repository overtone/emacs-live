EPL [![Build Status](https://travis-ci.org/cask/epl.png?branch=master)](https://travis-ci.org/cask/epl)
===

**E**macs **P**ackage **L**ibrary

EPL provides a convenient high-level API for various package.el versions, and
aims to overcome its most striking idiocies.

Installation
------------

Install from [MELPA](http://melpa.milkbox.net/#/epl) or
[Marmalade](http://marmalade-repo/packages/epl).  The former is strongly
recommended.

In your [`Cask` file](https://github.com/cask/cask):

```lisp
(source gnu)
(source melpa)

(depends-on "epl")
```

EPL supports GNU Emacs 23 with the
[3rd party `package.el`](https://github.com/technomancy/package.el), and GNU
Emacs 24 with the built-in `package.el` library.  Other versions of Emacs, and
other flavors of Emacs (e.g. XEmacs, Aquamacs, etc.) are *not* supported.

At the time of writing, Emacs trunk (upcoming Emacs 24.4) is the preferred
version, because its `package.el` is the most advanced and best implemented
version.  Stable GNU Emacs 24 is supported, but limited in features.  GNU Emacs
23 is only supported as long as possible without greater pain.  It may be
dropped any time if obstacles appear.

If you have any choice, use Emacs trunk.

Usage
-----

Refer to the commentary of `epl.el` for a list of functions, and to the
docstrings of these functions for usage instructions.

We may add a list of all functions to this README, but this is not exactly
easy.  Pull requests welcome.

License
-------

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program.  If not, see http://www.gnu.org/licenses/.

See [COPYING](https://github.com/cask/epl/blob/master/COPYING) for
details.
