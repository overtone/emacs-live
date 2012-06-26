Slime completion source for Emacs auto-complete package
=======================================================

This plugin provides a completion source for the popular Emacs
interactive auto-completion framework
[auto-complete](http://cx4a.org/software/auto-complete/).

Where slime provides it, pop-up documentation for completed symbols
will be displayed.


Installation
=============

First, ensure `auto-complete` and `slime` are installed: I recommend
using packages from [Marmalade][marmalade] or [Melpa][melpa].

You'll need both `auto-complete` and `slime` to be enabled and
working, so please consult the corresponding documentation is you have
any trouble with this.

Next, install `ac-slime`. If you choose not to use the convenient
package in [Melpa][melpa] and [Marmalade][marmalade], you'll need to
add the directory containing `ac-slime.el` to your `load-path`, and
then `(require 'ac-slime)`.

`ac-slime` provides a couple of `slime`-specific completion sources,
so `auto-complete` needs to be told to use them when `slime-mode` is
active. To do this, put the following code in your emacs init file to 

     (add-hook 'slime-mode-hook 'set-up-slime-ac)
     (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
     (eval-after-load "auto-complete"
       '(add-to-list 'ac-modes 'slime-repl-mode))

Usage
=====

`ac-slime` should now automatically be enabled when you visit a buffer
in which `slime-mode` is active and `auto-complete` is enabled. (The
symbols "Slime" and "AC" should appear in the modeline.)

Simply trigger auto-completion, and completion candidates supplied by
slime should be displayed, with the symbol `l` on the right hand side
of the completion pop-up. After a short delay, popup documentation
for the completed symbol should also be displayed.

More
====

`ac-slime` supports fuzzy completion -- see the comments in
`ac-slime.el` for details. Note that to use fuzzy completion, you must
install and require `slime-fuzzy.el`, which comes with the full Slime
distro, but not the packaged version in [Marmalade][marmalade].




[marmalade]: http://marmalade-repo.org
[melpa]: http://melpa.milkbox.net

