Git modes
=========

GNU Emacs modes for Git-related files.


Features
--------

- `git-commit-mode` – A major mode for editing Git commit messages
  according to the [guidelines by Tim Pope][1].
- `git-rebase-mode` – A major mode for `git-rebase-todo` files created
  by `git rebase -i`
- `gitconfig-mode` – A `conf-mode`-derived major mode for editing
  `.gitconfig` files.
- `gitignore-mode` – A `conf-mode`-derived major mode for editing `.gitignore`
  files.
- `gitattributes-mode` – A major mode for editing `.gitattributes` files.

The first two modes integrate into [Magit][2].


Installation
------------

Install the ELPA packages from [MELPA][3] (bleeding edge snapshots) or
[Marmalade][4] (stable releases):

- `git-commit-mode`: `M-x package-install git-commit-mode`
- `git-rebase-mode`: `M-x package-install git-rebase-mode`
- `gitconfig-mode`: `M-x package-install gitconfig-mode`
- `gitignore-mode`: `M-x package-install gitignore-mode`

Or [download][5] the latest release and install the desired modes with `M-x
package-install-file`, e.g. `M-x package-install-file git-commit-mode`.

The modes are written and tested against GNU Emacs 24 and may or may not work in
earlier versions of GNU Emacs.


Usage
-----

### `git-commit-mode`

Just configure `emacsclient` or `emacs` as editor for Git. `git-commit-mode`
will automatically be enabled for Git message buffers.

### `git-rebase-mode`

`git rebase-mode` is automatically enabled for `git-rebase-todo` files, such as
created by `git rebase -i`.

### `gitconfig-mode`

`gitconfig-mode` is automatically enabled for `.gitconfig`, `.git/config`
and `git/config` files.  The mode is derived from `conf-unix-mode`, so all
commands provided by `conf-mode` (e.g. `conf-align-assignments`) will work
as expected.

### `gitignore-mode`

`gitignore-mode` is automatically enabled for `.gitignore`,
`.git/info/exclude` and `git/ignore` files.

### `gitattributes-mode`
`gitattributes-mode` is automatically enabled for `.gitattributes`,
`.git/info/attributes`, and `git/attributes`.


Customization
-------------

- `git-commit-mode`: `M-x customize-group git-commit`
- `git-rebase-mode`: `M-x customize-group git-rebase`
- `gitconfig-mode`: No customization provided.
- `gitignore-mode`: No customization provided.
- `gitattributes-mode`: `M-x customize-group gitattributes-mode`.


Further help
------------

- `C-h f git-commit-mode`
- `C-h f git-rebase-mode`
- `C-h f gitconfig-mode`
- `C-h f gitignore-mode`
- `C-h f gitattributes-mode`


Credits
-------

`git-commit-mode` is forked of the [original work][6] done by
[Florian Ragwitz][7] and improved by [John Wiegley][8].  And then
[Sebastian Wiesner][9] took it to the next level.

`git-rebase-mode` was previously part of [Magit][2] as
`rebase-mode`. It was created by [Phil Jackson][10] and improved by
[Peter J. Weisberg][11].

`gitconfig-mode` and `gitignore-mode` were created by
[Sebastian Wiesner][9].

`gitattributes-mode` was created by [Rüdiger Sonderfeld][16].

The following people contributed to these modes:

- [Alan Falloon](https://github.com/alanfalloon)
- [Bradley Wright](https://github.com/bradleywright)
- [Jonas Bernoulli](https://github.com/tarsius)
- [Marco Craveiro](https://github.com/mcraveiro)
- [Mitchel Humpherys](https://github.com/mgalgs)
- [Pekka Pessi](https://github.com/pessi)
- [Peter Eisentraut](https://github.com/petere)
- [Pieter Praet](https://github.com/praet)
- [Ramkumar Ramachandra](https://github.com/artagnon)
- [Ryan Thompson](https://github.com/DarwinAwardWinner)
- [Tim Wraight](https://github.com/timwraight)
- [Yann Hodique](https://github.com/sigma)

An up-to-date list of contributors can also be found [here][12].

Great thanks also goes to [Bozhidar Batsov][13] for adding these modes
to his awesome [Prelude][14] project, thus making them available to a
larger user base.


License
-------

Git-Modes is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

Git-Modes is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Git-Modes.  If not, see <http://www.gnu.org/licenses/>.

See [`COPYING`][15] for details.


[1]: http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html
[2]: http://magit.github.com/magit
[3]: http://melpa.milkbox.net
[4]: http://marmalade-repo.org
[5]: https://github.com/magit/git-modes/tags
[6]: https://github.com/rafl/git-commit-mode
[7]: https://github.com/rafl
[8]: https://github.com/jwiegley
[9]: https://github.com/lunaryorn
[10]: https://github.com/philjackson
[11]: https://github.com/pjweisberg
[12]: https://github.com/magit/git-modes/graphs/contributors
[13]: https://github.com/bbatsov
[14]: https://github.com/bbatsov/prelude
[15]: https://github.com/magit/git-modes/blob/master/COPYING
[16]: https://github.com/ruediger
