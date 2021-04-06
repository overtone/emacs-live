WARNING: Things have changed considerably since version 0.5.1. Keep reading if you're unsure how this affects you.

gist.el -- Emacs integration for gist.github.com
================================================

Uses your local GitHub config if it can find it.
See <http://github.com/blog/180-local-github-config>

News
====

What's new in 1.0 ?

* gist.el now maintains a local cache so as to not go to the gist server every now and then.
* multi-files gist support (indicated by a '+' in the gist list)
* improved gist-list buffer, based on tabulated-list.el (same codebase as package.el)
    New keybindings:
    * `g` : reload the gist list from server
    * `e` : edit current gist description
    * `k` : delete current gist
    * `+` : add a file to the current gist
    * `-` : remove a file from the current gist
    * `y` : print current gist url
    * `b` : browse current gist
    * `*` : star gist
    * `^` : unstar gist
    * `f` : fork gist
* in-place edition. While viewing a gist file buffer, you can:
    * `C-x C-s` : save a new version of the gist
    * `C-x C-w` : rename some file
* dired integration. From a dired buffer, you can:
    * `@` : make a gist out of marked files (with a prefix, make it private)

Install
=======

Dependencies
------------

gist.el depends on a number of other modules, that you'll need to install, either manually or by way of Emacs package manager

* tabulated-list.el
  built-in for emacs 24. If you're using emacs 23, you can find a backport here: https://github.com/sigma/tabulated-list.el
* gh.el
  GitHub client library. Install from there: https://github.com/sigma/gh.el
* pcache.el
  Really a gh.el dependency. Install from there: https://github.com/sigma/pcache
* logito.el
  Really a gh.el dependency. Install from there: https://github.com/sigma/logito

Install gist.el from marmalade (recommended)
--------------------------------------------

In that scenario, you don't have to deal with the above dependencies yourself.

For emacs 24, first make sure http://marmalade-repo.org/ is properly configured. Then

    M-x package-install RET gist RET

For emacs 23, you'll need to install a version of package.el first. Some bootstrap code is available there: https://gist.github.com/1884169
Then proceed as for emacs 24. You might get some compilation errors, but the package should be operational in the end.

Install gist.el from git
------------------------

After installing the required dependencies, proceed with:

    $ cd ~/.emacs.d/vendor
    $ git clone git://github.com/defunkt/gist.el.git

In your emacs config:

    (add-to-list 'load-path "~/.emacs.d/vendor/gist.el")
    (require 'gist)

Getting started
===============

When you first run a gist.el operation, you might be asked for your GitHub username and password. The username will be stored for future use, and a OAuth token will be stored in place of your password.

To make gist.el forget about those information, just remove them from your ~/.gitconfig file

Functions
=========

    gist-list - Lists your gists in a new buffer. Use arrow keys
    to browse, RET to open one in the other buffer.

    gist-region - Copies Gist URL into the kill ring.
    With a prefix argument, makes a private gist.

    gist-region-private - Explicitly create a private gist.

    gist-buffer - Copies Gist URL into the kill ring.
    With a prefix argument, makes a private gist.

    gist-buffer-private - Explicitly create a private gist.

    gist-region-or-buffer - Post either the current region, or if mark
    is not set, the current buffer as a new paste at gist.github.com .
    Copies the URL into the kill ring.
    With a prefix argument, makes a private paste.

    gist-region-or-buffer-private - Explicitly create a gist from the
    region or buffer.

Config
======

Set `gist-view-gist` to non-nil if you want to view your Gist using
`browse-url` after it is created.

Meta
====

* Code: `git clone git://github.com/defunkt/gist.el.git`
* Home: <http://github.com/defunkt/gist.el>
* Bugs: <http://github.com/defunkt/gist.el/issues>
