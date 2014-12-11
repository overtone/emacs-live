nrepl completion source for Emacs auto-complete package
=======================================================

__Deprecation notice: These days you probably want to use
 [ac-cider](https://github.com/clojure-emacs/ac-cider)
 or Cider's built-in `company` completion support instead of this.__

This plugin provides a completion source for the popular Emacs
interactive auto-completion framework
[auto-complete](http://cx4a.org/software/auto-complete/).

Where nrepl provides it, pop-up documentation for completed symbols
will be displayed.

**Latest stable version**: see the [latest numbered tag](https://github.com/clojure-emacs/ac-nrepl/tags), which
will also be the latest version available via Marmalade.

Installation
=============

First, ensure `auto-complete` and `cider` are installed: I recommend
using packages from [Marmalade][marmalade] or [Melpa][melpa].

You'll need both `auto-complete` and `cider` to be enabled and
working, so please consult the corresponding documentation if you have
any trouble with this.

Next, install `ac-nrepl`. If you choose not to use the convenient
package in [Melpa][melpa] and [Marmalade][marmalade], you'll need to
add the directory containing `ac-nrepl.el` to your `load-path`, and
then `(require 'ac-nrepl)`.

`ac-nrepl` provides an `nrepl`-specific completion source,
so `auto-complete` needs to be told to use them when `cider-repl-mode` is
active. To do this, put the following code in your emacs init file to

```el
(require 'ac-nrepl)
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))
```

If you want to trigger `auto-complete` using <kbd>TAB</kbd> in nrepl buffers, you may
want to put `auto-complete` into your `completion-at-point-functions`:

```el
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

(add-hook 'cider-repl-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)
```

You might consider using `ac-nrepl`'s popup documentation in place of `nrepl-doc`:

```el
(eval-after-load "cider"
  '(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))
```

Usage
=====

`ac-nrepl` should now automatically be enabled when you visit a buffer
in which `cider-repl-mode` is active and `auto-complete` is enabled. (The
symbols "nrepl and "AC" should appear in the modeline.)

Simply trigger auto-completion, and completion candidates supplied by
nrepl should be displayed, with symbols on the right hand side of the
completion pop-up to indicate the "flavour" of the completion
candidate, e.g. "v" for variables. After a short delay, popup
documentation for the completed symbol should also be displayed.



[marmalade]: http://marmalade-repo.org
[melpa]: http://melpa.milkbox.net

Acknowledgements
================

`ac-nrepl` was written by [Steve Purcell](https://github.com/purcell), with important contributions from [Sam Aaron](https://github.com/samaaron).

<hr>

Author links:

[![](http://api.coderwall.com/purcell/endorsecount.png)](http://coderwall.com/purcell)

[![](http://www.linkedin.com/img/webpromo/btn_liprofile_blue_80x15.png)](http://uk.linkedin.com/in/stevepurcell)

[Steve Purcell's blog](http://www.sanityinc.com/) // [@sanityinc on Twitter](https://twitter.com/sanityinc)
