[![MELPA](http://melpa.org/packages/rainbow-delimiters-badge.svg)](http://melpa.org/#/rainbow-delimiters)
[![MELPA Stable](http://stable.melpa.org/packages/rainbow-delimiters-badge.svg)](http://stable.melpa.org/#/rainbow-delimiters)
[![Build Status](https://github.com/Fanael/rainbow-delimiters/workflows/CI/badge.svg)](https://github.com/Fanael/rainbow-delimiters/actions)

# rainbow-delimiters

`rainbow-delimiters` is a "rainbow parentheses"-like mode which highlights
delimiters such as parentheses, brackets or braces according to their depth.
Each successive level is highlighted in a different color. This makes it easy to
spot matching delimiters, orient yourself in the code, and tell which statements
are at a given depth.

Great care has been taken to make this mode fast. You shouldn't see any change
in scrolling or editing speed when it's on even when working in delimiter-rich
languages like Clojure or Emacs Lisp. It can be used with any language.

You can customize the colors `rainbow-delimiters` uses. The default colors are
intentionally subtle; they are unobtrusive enough to make the mode worth looking
at even if you usually don't like rainbow parentheses modes. A number of major
color themes such as Zenburn and Solarized have added their own faces for the
mode.

This is the official github repository for `rainbow-delimiters`.

The latest **release** of `rainbow-delimiters` is always found at
https://github.com/Fanael/rainbow-delimiters/tree/master

## Installation

The recommended way is to use [MELPA](http://melpa.org/) or
[MELPA Stable](http://stable.melpa.org/). If either is in your
`package-archives`, do

    M-x package-install RET rainbow-delimiters RET

Otherwise, open `rainbow-delimiters.el` in Emacs and use

    M-x package-install-from-buffer

Any other methods of installation are unsupported.

## Usage

To toggle the mode in the current buffer:

    M-x rainbow-delimiters-mode

To start the mode automatically in `foo-mode`, add the following to your init
file:

    (add-hook 'foo-mode-hook #'rainbow-delimiters-mode)

To start the mode automatically in most programming modes (Emacs 24 and above):

    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

### Global mode

There's no `global-rainbow-delimiters-mode` anymore. It used to exist, but it
was impossible to keep it from breaking some major modes. It's *strongly
recommended* to use major mode hooks instead, as shown above. There's nothing
stopping you from defining `global-rainbow-delimiters-mode` yourself, but if it
breaks something, you're on your own.

## Customization

To customize various options, including the color theme:

    M-x customize-group rainbow-delimiters

You can specify custom colors by customizing following faces:
 * Faces take the form `rainbow-delimiters-depth-N-face`, with N being the
   depth. Depth begins at 1, the outermost color. Faces exist for depths 1-9.
 * The unmatched delimiter face: `rainbow-delimiters-unmatched-face`.
 * The mismatched delimiter face: `rainbow-delimiters-mismatched-face`.
