nrepl completion source for Emacs auto-complete package
=======================================================

This plugin provides a completion source for the popular Emacs
interactive auto-completion framework
[auto-complete](http://cx4a.org/software/auto-complete/).

Where nrepl provides it, pop-up documentation for completed symbols
will be displayed.


*This is an early work in progress, and liable to breakage as nrepl evolves.*

Installation
=============

First, ensure `auto-complete` and `nrepl` are installed: I recommend
using packages from [Marmalade][marmalade] or [Melpa][melpa].

You'll need both `auto-complete` and `nrepl` to be enabled and
working, so please consult the corresponding documentation is you have
any trouble with this.

Next, install `ac-nrepl`. If you choose not to use the convenient
package in [Melpa][melpa] and [Marmalade][marmalade], you'll need to
add the directory containing `ac-nrepl.el` to your `load-path`, and
then `(require 'ac-nrepl)`.

`ac-nrepl` provides an `nrepl`-specific completion source,
so `auto-complete` needs to be told to use them when `nrepl-mode` is
active. To do this, put the following code in your emacs init file to 

     (require 'ac-nrepl)
     (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
     (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
     (eval-after-load "auto-complete"
       '(add-to-list 'ac-modes 'nrepl-mode))

If you want to trigger `auto-complete` using TAB in nrepl buffers, you may
want to put `auto-complete` into your `completion-at-point-functions`:

    (defun set-auto-complete-as-completion-at-point-function ()
      (setq completion-at-point-functions '(auto-complete)))
    (add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

    (add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
    (add-hook 'nrepl-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)

Usage
=====

`ac-nrepl` should now automatically be enabled when you visit a buffer
in which `nrepl-mode` is active and `auto-complete` is enabled. (The
symbols "nrepl and "AC" should appear in the modeline.)

Simply trigger auto-completion, and completion candidates supplied by
nrepl should be displayed, with the symbol `n` on the right hand side
of the completion pop-up. After a short delay, popup documentation
for the completed symbol should also be displayed.



[marmalade]: http://marmalade-repo.org
[melpa]: http://melpa.milkbox.net



<hr>

[![](http://api.coderwall.com/purcell/endorsecount.png)](http://coderwall.com/purcell)

[![](http://www.linkedin.com/img/webpromo/btn_liprofile_blue_80x15.png)](http://uk.linkedin.com/in/stevepurcell)
