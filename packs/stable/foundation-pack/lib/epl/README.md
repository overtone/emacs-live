EPL — Emacs Package Library
===========================

[![License GPL 3][badge-license]][copying]
[![Build Status][badge-travis]][travis]

EPL provides a convenient high-level API for various package.el versions, and
aims to overcome its most striking idiocies.

Installation
------------

Install from [MELPA][] or [Marmalade][].  The former is strongly recommended.

In your [`Cask`][cask]:

```cl
(source gnu)
(source melpa)

(depends-on "epl")
```

EPL supports GNU Emacs 24 with the built-in `package.el` library.  EPL should
also work on GNU Emacs 23 with the [3rd party `package.el`][legacy-package], but
this combination is not regularly tested anymore.  Other versions of Emacs, and
other flavors of Emacs (e.g. XEmacs, Aquamacs, etc.) are *not* supported.

Usage
-----

Refer to the commentary of `epl.el` for a list of functions, and to the
docstrings of these functions for usage instructions.

We may add a list of all functions to this README, but this is not exactly
easy.  Pull requests welcome.

License
-------

EPL is free software: you can redistribute it and/or modify it under the terms
of the GNU General Public License as published by the Free Software Foundation,
either version 3 of the License, or (at your option) any later version.

EPL is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the GNU General Public License for more details.

See [`COPYING`][copying] for the complete text of the license.

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg?dummy
[badge-travis]: https://travis-ci.org/cask/epl.svg?branch=master
[travis]: https://travis-ci.org/cask/epl
[MELPA]: http://melpa.milkbox.net/#/epl
[Marmalade]: http://marmalade-repo/packages/epl
[Cask]: http://cask.github.io/
[legacy-package]: https://github.com/technomancy/package.el
[COPYING]: https://github.com/cask/epl/blob/master/COPYING
