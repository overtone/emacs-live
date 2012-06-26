popwin.el
=========

Overview
--------

popwin is a popup window manager for Emacs which makes you free from
the hell of annoying buffers such like `*Help*`, `*Completions*`,
`*compilation*`, and etc.

Take an example. When you complete file names during `find-file`, the
(annoying) `*Completions*` buffer will appear in a newly splitted
window. You might understand the necessity of the window, but you may
wonder why the window still remains after completion...

popwin resolves there problems. Windows of such temporary buffers will
be shown as a popup window, and you can close them smoothly by typing
`C-g` in anytime.

Screenshots
-----------

**Before Popup Window**

![](http://cx4a.org/software/popwin/popwin1.png)

**After Popup Window**

![](http://cx4a.org/software/popwin/popwin2.png)

Installation
------------

Install `popwin.el` into your `load-path` directory. If you have
`install-elisp` or `auto-install`, you also be able to install
`popwin.el` like:

    ;; install-elisp
    (install-elisp "https://raw.github.com/m2ym/popwin-el/master/popwin.el")
    ;; auto-install
    (auto-install-from-url "https://raw.github.com/m2ym/popwin-el/master/popwin.el")

And then add the following code into your `.emacs`:

    (require 'popwin)
    (setq display-buffer-function 'popwin:display-buffer)

popwin is tested under GNU Emacs 22 or later.

Basic Usage
-----------

Special buffers, for example `*Help*`, specified in
`popwin:special-display-config` will be shown in a popup window. You
can close the popup window by typing `C-g` or selecting other windows.

By default, `*Help*`, `*Completions*`, `*compilation*`, and `*Occur*`
buffers will be shown in a popup window. Try `M-x find-file` and type
`TAB TAB`. You may see a popup window at the bottom of the frame.

**File Name Completion**

![](http://cx4a.org/software/popwin/popwin-find-file.png)

Let me show other examples.

**`M-x occur`**

![](http://cx4a.org/software/popwin/popwin-occur.png)

**`M-x compile`**

![](http://cx4a.org/software/popwin/popwin-compile.png)

Customization
-------------

Please do `M-x customize-group RET popwin RET`. See the header of
`popwin.el`, source code, and docstrings for more information.

### Default Keymap

popwin provides a default keymap named `popwin:keymap`. You can use it
like:

    (global-set-key (kbd "C-x p") popwin:keymap)

Keymap:

    | Key    | Command                    |
    |--------+----------------------------|
    | b, C-b | popwin:popup-buffer        |
    | M-b    | popwin:popup-buffer-tail   |
    | o, C-o | popwin:display-buffer      |
    | p, C-p | popwin:display-last-buffer |
    | f, C-f | popwin:find-file           |
    | M-f    | popwin:find-file-tail      |
    | s, C-s | popwin:select-popup-window |
    | M-s    | popwin:stick-popup-window  |
    | 0      | popwin:close-popup-window  |
    | m, C-m | popwin:messages            |

Special Display Config
----------------------

When you want to show buffers in a popup window as you like, you need
to write a configuration about `popwin:special-display-config`.

This variable is a list of a form like `(pattern :regexp REGEXP :width
WIDTH :height HEIGHT :position POS :noselect NOSEL :stick
STICK)`. Only `pattern` is necessary and other keywords are
optional. `PATTERN` is string or symbol. If string, it indicates which
buffers should be shown in a popup window. If symbol, it indicates
which buffers of the major mode of the symbol should be shown in a
popup window.

Take an example. If you want to show `*scratch*` buffer, write the
following code:

    (setq popwin:special-display-config '(("*scratch*")))

And then display `*scratch*` like:

    (display-buffer "*scratch*")

You may see the buffer at the bottom of the frame.

If you specify `t` to `REGEXP`, you can specify a regexp to `PATTERN`
for matching a buffer name.

If you specify a number to `WIDTH`, the value will be used instead of
`popwin:popup-window-width`. `HEIGHT` and `POS` are same.

If you specify `t` to `NOSEL`, a popup window will not be selected
when it is shown. If you specify `t` to `STICK`, a popup window will be
stuck by default.

Remember that popwin can handle `display-buffer` only. So popwin can't
handle the behaviors like switching buffer. This is NOT a bug but a
feature.

### Examples

#### Anything

Show `*anything*` in a popup window.

    (setq anything-samewindow nil)
    (push '("*anything*" :height 20) popwin:special-display-config)

![](http://cx4a.org/software/popwin/popwin-anything.png)

#### Dired

Show dired buffers in a popup window by `M-x dired-jump-other-window`.

    (push '(dired-mode :position top) popwin:special-display-config)

![](http://cx4a.org/software/popwin/popwin-dired.png)

Working with Other Extensions
-----------------------------

Some extensions can't work with popwin or needs workarounds. Here is
the known issues and solutions.

#### YaTeX

`misc/popwin-yatex.el` helps you to show YaTeX related buffers in
popup windows. Add the following code into `.emacs`.

    (require 'popwin-yatex)

You may write the configuration like:

    (push '("*YaTeX-typesetting*") popwin:special-display-config)

#### w3m

`misc/popwin-w3m.el` helps you to show specific pages with w3m in
popup windows. Add the following code into `.emacs`.

    (require 'popwin-w3m)

It is recommended to change `browse-url-browser-function` to
`popwin:w3m-browse-url`.

    (setq browse-url-browser-function 'popwin:w3m-browse-url)

`popwin:w3m-browse-url` is a function (and command) displaying w3m
buffers in popup windows if the given URL is matched with the rules.

The rules are described by `popwin:w3m-special-display-config`
variable, which has a almost same structure of
`popwin:special-display-config`.

The difference is `popwin:w3m-special-display-config` takes an URL
regular expression instead of buffer name pattern.

For example, if you want to show google search pages in popup windows,
the configuration could be:

    (push '("^http://www\\.google\\.com/.*$") popwin:w3m-special-display-config)

#### `windows.el`

There is an problem when loading `windows.el` after loading
`popwin.el`. So load `windows.el` first.

API
---

Introduce basic API of popwin. See source code for more information.

### Function: `popwin:create-popup-window`

    popwin:create-popup-window &optional size position adjust => (master-window popup-window)

`popwin:create-popup-window` creates a popup window and return it with
a master window. Master window is a window which is splitted when
creating the popup window. A resposibility of closing the popup window
is on developers.

### Function: `popwin:popup-buffer`

    popwin:popup-buffer buffer &key width height position noselect stick => popup-window

`popwin:popup-buffer` displays the buffer in a popup window. The popup
window will be closed automatically. Keywords arguments are same
meanings to an element of `popwin:special-display-config`.

### Function: `popwin:display-buffer`

Same as `popwin:popup-buffer` except `popwin:display-buffer` refers to
`popwin:special-display-config` and uses its configuration. If no
entry is found in `popwin:special-display-config`, the buffer will be
displayed as usual way.

----

Copyright (C) 2011  Tomohiro Matsuyama <<tomo@cx4a.org>>
