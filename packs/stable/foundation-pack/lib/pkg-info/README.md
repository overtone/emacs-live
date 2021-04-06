pkg-info.el [![Build Status](https://travis-ci.org/lunaryorn/pkg-info.el.png?branch=master)](https://travis-ci.org/lunaryorn/pkg-info.el)
===========

Provide information about Emacs packages.

Installation
------------

As usual, from [MELPA](http://melpa.milkbox.net) and
[Marmalade](http://marmalade-repo.org/).

In your `Cask` file:

```lisp
(source melpa)

(depends-on "pkg-info")
```

This library is compatible with GNU Emacs 24.1 and newer.  It will work with
Emacs 23 as well, if package.el is installed.

Functions
---------

- `pkg-info-library-version` extracts the *version* from the header of a library.
- `pkg-info-defining-library-version` extracts the *version* from the header of
  a library defining a function.
- `pkg-info-package-version` gets the *version* of an installed package.
- `pkg-info-format-version` formats a *version* as human readable string.

A *version* is simply a list of integers.

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

See [COPYING](https://github.com/lunaryorn/pkg-info.el/blob/master/COPYING) for
details.
