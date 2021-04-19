pkg-info.el
===========

[![License GPL 3][badge-license]][copying]
[![Build Status][badge-travis]][travis]

Provide information about Emacs packages.

Installation
------------

From [MELPA][] with <kbd>M-x package-install RET pkg-info</kbd>.

In your [`Cask`][cask] file:

```cl
(source melpa)

(depends-on "pkg-info")
```

This library is compatible with GNU Emacs 24.1 and newer.  It will probably work
with Emacs 23 as well, if package.el is installed, but it is not tested on Emacs
23 anymore.

Functions
---------

This library defines the following functions for use in your Emacs extensions:

- `pkg-info-library-original-version` extracts the *original version* from the
  header of a library.
- `pkg-info-library-version` extracts the *version* from the header of a library.
- `pkg-info-defining-library-version` extracts the *version* from the header of
  a library defining a function.
- `pkg-info-defining-library-original-version` extracts the *original version*
  from the header of a library defining a function.
- `pkg-info-package-version` gets the *version* of an installed package.
- `pkg-info-format-version` formats a *version* as human readable string.
- `pkg-info-get-melpa-recipe` gets the MELPA recipe for a package.
- `pkg-info-wiki-package-p` determines whether a package was build from
  EmacsWiki on MELPA.

All of these functions are interactive commands as well.

A *version* is the version from the Library Headers, i.e. either from the
`Version` or from the `Package-Version` headers.  An *original version* is the
version from the `X-Original-Version` header added by MELPA_ to preserve
upstream versions.

Versions are represented as version lists, as returned by `version-to-list`.

License
-------

pkg-info.el is free software: you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

pkg-info.el is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

See [`COPYING`][copying] for the complete text of the license.

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg?dummy
[COPYING]: https://github.com/lunaryorn/pkg-info.el/blob/master/COPYING
[badge-travis]: https://travis-ci.org/lunaryorn/pkg-info.el.svg?branch=master
[travis]: https://travis-ci.org/lunaryorn/pkg-info.el
[cask]: http://cask.github.io/
[MELPA]: http://melpa.org
