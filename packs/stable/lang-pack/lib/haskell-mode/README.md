<img src="https://rawgithub.com/haskell/haskell-mode/master/images/haskell-mode.svg" width="35" height="35" /> Haskell Mode for Emacs
======================

This is an Emacs mode for editing, debugging and developing Haskell programs.

[![Coverage Status](https://coveralls.io/repos/haskell/haskell-mode/badge.svg?branch=master&service=github)](https://coveralls.io/github/haskell/haskell-mode?branch=master)
[![Build Status](https://travis-ci.org/haskell/haskell-mode.svg?branch=master)](https://travis-ci.org/haskell/haskell-mode)
[![Melpa Status](http://melpa.org/packages/haskell-mode-badge.svg)](http://melpa.org/#/haskell-mode)
[![Melpa Stable Status](http://stable.melpa.org/packages/haskell-mode-badge.svg)](http://stable.melpa.org/#/haskell-mode)
[![Latest Docs](http://img.shields.io/badge/docs-latest-fc1e5e.svg)](http://haskell.github.io/haskell-mode/manual/latest/)
[![License GPL3](https://img.shields.io/badge/license-GPL3-blue.svg)](https://github.com/haskell/haskell-mode/blob/master/COPYING)
[![Twitter Follow](https://img.shields.io/twitter/follow/HaskellMode.svg?style=social)](https://twitter.com/HaskellMode)

> I just want to thank everybody involved in one way or another with the Haskell Emacs tooling.
> It is one of the best language experiences I had in Emacs.
> -- [cocreature, 2015-03-01](https://www.reddit.com/r/haskell/comments/2xjum3/haskellmode_february_2015_developments/cp0qa9a)

> I've been using it for a long time and love it. Great work on haskell-mode! Keep up the good work!
> -- [LukeHoersten, 2015-04-02](https://www.reddit.com/r/haskell/comments/316hcm/month_in_haskell_mode_march_2015/cpyutph)

> This sounds wonderful. Does anything similar exist for Vim?
> -- [earldouglas, 2015-07-02](https://www.reddit.com/r/haskell/comments/3bsa0f/month_in_haskell_mode_june_2015/cspdbb6)


## Quick Installation

Make sure you have this in your [init file](http://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html) (usually `~/.emacs`). If you already have `custom-set-variables`, merge its contents:

```elisp
(require 'package)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa-stable" . "http://stable.melpa.org/packages/")))))
```

Then run emacs, and evaluate:

    M-x package-refresh-contents

and then follow by

    M-x package-install RET haskell-mode

Voila! `haskell-mode` is installed! You should be able to edit Haskell
source code in color now.

`Haskell-mode` has much much much more to offer but the above should get you
going!


## Advanced configuration

For setup instructions, please consult the integrated `haskell-mode`
[Info](https://www.gnu.org/software/texinfo/manual/info/info.html)
manual which can be accessed after installation via `M-x
info-display-manual [RET] haskell-mode`.  Alternatively, you can also
direct your browser to the
[the online haskell-mode manual](http://haskell.github.io/haskell-mode/manual/latest/)
for setup and user guide.


## Installation - more information

`haskell-mode` supports GNU Emacs version 24 or later.

`haskell-mode` is available from [melpa-stable](http://stable.melpa.org) (releases) and [melpa](http://melpa.org)
(git snapshots).

Other means of obtaining `haskell-mode` include
[el-get](https://github.com/dimitri/el-get),
[Emacs Prelude](https://github.com/bbatsov/prelude) and [Debian package](https://packages.debian.org/search?keywords=haskell-mode).

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

`haskell-mode` is actively seeking contributions from users of
`haskell-mode`. For more information have a look at
[the wiki page on contributing](https://github.com/haskell/haskell-mode/wiki/Contributing).


## Getting in contact

- [Mailing list](http://projects.haskell.org/cgi-bin/mailman/listinfo/haskellmode-emacs)
- [Github homepage](https://github.com/haskell/haskell-mode)
- IRC: #haskell-emacs on irc.freenode.net

Have fun!
