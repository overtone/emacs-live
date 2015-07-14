![Haskell Mode Logo](images/haskell-mode-128x128.png)

Haskell Mode for Emacs
======================

This is an Emacs mode for editing, debugging and developing Haskell programs.

[![Build Status](https://travis-ci.org/haskell/haskell-mode.svg?branch=master)](https://travis-ci.org/haskell/haskell-mode)
[![Melpa Status](http://melpa.org/packages/haskell-mode-badge.svg)](http://melpa.org/#/haskell-mode)
[![Melpa Stable Status](http://stable.melpa.org/packages/haskell-mode-badge.svg)](http://stable.melpa.org/#/haskell-mode)

## Installation

`haskell-mode` is best installed as a package.

To install `haskell-mode` you need to add a package archive repository that distributes
`haskell-mode`. Execute

```
M-x customize-option RET package-archives
```

and add

    Archive name: melpa-stable
    URL or directory name: http://stable.melpa.org/packages/

Fetch list of packages with

    M-x package-refresh-contents

and then follow by

    M-x package-install RET haskell-mode

Voila! `haskell-mode` is installed! You should be able to edit Haskell
source code in color now.

You need to enable indentation as this does not happen automatically
currently. Add this line to your ~/.emacs file:

```el
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)

```

`Haskell-mode` has much much much more to offer but the above should get you
going!


## Advanced configuration

For setup instructions, please consult the integrated `haskell-mode`
[Info](https://www.gnu.org/software/texinfo/manual/info/info.html)
manual which can be accessed after installation via `M-x
info-display-manual [RET] haskell-mode`.  Alternatively, you can also
direct your browser to the
[the online haskell-mode manual](https://github.com/haskell/haskell-mode/wiki)
for setup and user guide.


## Installation - more information

`haskell-mode` supports GNU Emacs versions 23, 24 and upcoming 25
(snapshot).

`haskell-mode` is available from [melpa-stable](http://stable.melpa.org) (releases) and [melpa](http://melpa.org)
(git snapshots).

Other means of obtaining `haskell-mode` include
[el-get](https://github.com/dimitri/el-get),
[Emacs Prelude](https://github.com/bbatsov/prelude) and [Debian package](https://packages.debian.org/search?keywords=haskell-mode).

Emacs23 requires an the use of "cl-lib".  cl-lib.el can be found in
the tests/compat directory.  Copy cl-lib.el to your emacs
directory, e.g. ~/.emacs.d directory and put

```el
(add-to-list 'load-path "~/.emacs.d/")
(require 'cl-lib)
```

in your .emacs file.


## Installation from git repository

Running `haskell-mode` directly from sources is easy but
requires a little preparation:

- `git clone https://github.com/haskell/haskell-mode.git` into a
  suitable directory, e.g. `~/lib/emacs/haskell-mode/` where `~`
  stands for your home directory.

- Assuming you have unpacked the various haskell-mode modules
  (`haskell-mode.el` and the rest) in the directory
  `~/lib/emacs/haskell-mode/`, you need to generate various files, the
  autoloads file (`haskell-mode-autoloads.el`) is one among
  them. Invoke:

```bash
make EMACS=/path/to/your/emacs
```

  and then adding the following command to your `.emacs`:

```el
(add-to-list 'load-path "~/lib/emacs/haskell-mode/")
(require 'haskell-mode-autoloads)
(add-to-list 'Info-default-directory-list "~/lib/emacs/haskell-mode/")
```

## Contributing

If you followed the above you are just a couple of steps away from
contributing to `haskell-mode`.

`haskell-mode` is activelly seeking contributions from users of
`haskell-mode`. For more information have a look at
[the wiki page on contributing](https://github.com/haskell/haskell-mode/wiki/Contributing).


## Getting in contact

- [Mailing list](http://projects.haskell.org/cgi-bin/mailman/listinfo/haskellmode-emacs)
- [Github homepage](https://github.com/haskell/haskell-mode)
- IRC: #haskell-emacs on irc.freenode.net

Have fun!
