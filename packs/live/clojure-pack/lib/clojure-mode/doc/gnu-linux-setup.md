# Emacs Setup on GNU/Linux

Emacs should be available via your OS's package manager.  For example,
on Debian-based distros you would typically install Emacs like so:

    sudo apt-get install emacs

We recommend you install the current stable release of Emacs, version
24. Since v24 was released only very recently, the above command may
get you v23 instead. Use `apt-cache show emacs` to see which version
it will provide.

## Nix

You can use [Nix](http://nixos.org/nix) to install Emacs 24 on nearly any
distro:

    $ nix-env -i emacs-24

## Debian

Debian builds are available at http://emacs.naquadah.org/

## Ubuntu

To get Emacs version 24 on an Ubuntu-based distribution, you currently
need to do the following:

    sudo add-apt-repository ppa:cassou/emacs
    sudo apt-get update
    sudo apt-get install emacs-snapshot

## Fedora

To get Emacs version 24 on Fedora, do the following:

    sudo yum install emacs

## Arch, Gentoo, etc

TODO
