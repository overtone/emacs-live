[![Melpa Status](http://melpa.org/packages/elisp-slime-nav-badge.svg)](http://melpa.org/#/elisp-slime-nav)
[![Melpa Stable Status](http://stable.melpa.org/packages/elisp-slime-nav-badge.svg)](http://stable.melpa.org/#/elisp-slime-nav)

# Slime-style navigation for Emacs Lisp

Slime allows very convenient navigation to the symbol at point (using
<kbd>M-.</kbd>), and the ability to pop back to previous marks (using <kbd>M-,</kbd>).

This plugin provides similar navigation for Emacs Lisp, supporting
navigation to the definitions of variables, functions, libraries and
faces.

Additionally, `elisp-slime-nav` provides a way to describe the symbol
at point, whatever its type. As with `slime-describe-symbol`, this
functionality is bound both to <kbd>C-c C-d d</kbd> and <kbd>C-c C-d
C-d</kbd> by default.

## Installation

### Manual

Ensure `elisp-slime-nav.el` is in a directory on your load-path, and
add the following to your `~/.emacs` or `~/.emacs.d/init.el`:

``` lisp
(require 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))
```

### MELPA

If you're an Emacs 24 user or you have a recent version of
`package.el` you can install `elisp-slime-nav` from the
[MELPA](http://melpa.org) repository. The version of
`elisp-slime-nav` there will always be up-to-date. There are also
packages in Marmalade.

Enable `elisp-slime-nav` in `emacs-lisp-mode` and `ielm` by adding
code such as the following to your emacs startup file:

``` lisp
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))
```


## About

Author: Steve Purcell <steve at sanityinc dot com>

Homepage: https://github.com/purcell/elisp-slime-nav

This little library was extracted from the author's
[full Emacs configuration](https://github.com/purcell/emacs.d), which
readers might find of interest.

<hr>

[![](http://api.coderwall.com/purcell/endorsecount.png)](http://coderwall.com/purcell)

[![](http://www.linkedin.com/img/webpromo/btn_liprofile_blue_80x15.png)](http://uk.linkedin.com/in/stevepurcell)

[Steve Purcell's blog](http://www.sanityinc.com/) // [@sanityinc on Twitter](https://twitter.com/sanityinc)

