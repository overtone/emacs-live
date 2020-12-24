Changelog
=========

master (in development)
-----------------------

0.6 (May 17, 2015)
------------------

- Drop dash.el dependency
- Add `pkg-info-get-melpa-recipe`
- Add `pkg-info-get-melpa-fetcher`
- Add `pkg-info-wiki-package-p`

0.5 (Mar 13, 2014)
------------------

- Add support for ``X-Original-Version`` from [MELPA][] (GH-5):

  - Add `pkg-info-library-original-version` to extract `X-Original-Version`
    from a library
  - Add `pkg-info-defining-library-original-version` to extract the
    `X-Original-Version` from a library defining a symbol
  - Prefer `X-Original-Version` over the library version in
    `pkg-info-version-info`

0.4 (Nov 1, 2013)
-----------------

- Add `pkg-info-version-info` to provide comprehensive version information about
  libraries

0.3 (Oct 20, 2013)
------------------

- Use [EPL][] for Emacs 23 compatibility

0.2 (Oct 15, 2013)
------------------

- Fix source lookup for libraries (GH-1)

0.1 (Aug 17, 2013)
------------------

Initial release

[MELPA]: http://melpa.milkbox.net/
[EPL]: https://github.com/cask/epl
