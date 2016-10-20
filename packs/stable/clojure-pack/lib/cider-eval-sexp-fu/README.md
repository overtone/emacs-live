# cider-eval-sexp-fu

[eval-sexp-fu][] extensions for [CIDER][].

`eval-sexp-fu` provides tiny improvements to expression evaluation - e.g.
the expression you've just evaluated would briefly flash and so on.

You can see the package in action [here][eval-sexp-fu-demo].

## Install

The package is available in [MELPA][].

If you have MELPA in your `package-archives` variable, just use

    M-x package-install RET cider-eval-sexp-fu RET

If you don't, open `cider-eval-sexp-fu.el` in Emacs and invoke
`M-x package-install-from-buffer`.

## Usage

Just require it:

```elisp
(require 'cider-eval-sexp-fu)
```

## Customization

Customization is done via [eval-sexp-fu][].

[MELPA]: http://melpa.org/
[eval-sexp-fu]: https://github.com/hchbaw/eval-sexp-fu.el
[eval-sexp-fu-demo]: https://github.com/hchbaw/eval-sexp-fu.el/wiki/LongVersionOfTheDemo
[CIDER]: https://github.com/clojure-emacs/cider
