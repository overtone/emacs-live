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
`install-elisp` or `auto-install`, you may install `popwin.el` like:

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

Please do `M-x customize-group RET popwin RET` and `M-x
customize-variable RET popwin:special-display-config RET`. See the
header of `popwin.el`, source code, and docstrings for more
information.

### Default Keymap

popwin provides a default keymap named `popwin:keymap`. You can use it
like:

    (global-set-key (kbd "C-z") popwin:keymap)

Keymap:

    | Key    | Command                               |
    |--------+---------------------------------------|
    | b      | popwin:popup-buffer                   |
    | l      | popwin:popup-last-buffer              |
    | o      | popwin:display-buffer                 |
    | C-b    | popwin:switch-to-last-buffer          |
    | C-p    | popwin:original-pop-to-last-buffer    |
    | C-o    | popwin:original-display-last-buffer   |
    | SPC    | popwin:select-popup-window            |
    | s      | popwin:stick-popup-window             |
    | 0      | popwin:close-popup-window             |
    | f, C-f | popwin:find-file                      |
    | e      | popwin:messages                       |
    | C-u    | popwin:universal-display              |
    | 1      | popwin:one-window                     |

Special Display Config
----------------------

`popwin:special-display-config` is a list of `CONFIG`. `CONFIG` may be
a form of `(PATTERN . KEYWORDS)`, where `PATTERN` is a pattern of
specifying a buffer, and `KEYWORDS` is a list of a pair of key and
value. `PATTERN` is a buffer name, a symbol specifying major-mode, or
a predicate function which takes the buffer. If `CONFIG` is a string
or a symbol, `PATTERN` will be `CONFIG` and `KEYWORDS` will be
empty. Available keywords are following:

`:regexp`

:   If the value is non-nil, `PATTERN` will be used as regexp to matching buffer.

`:width`, `:height`

:   Specify width or height of the popup window. If no size specified,
    `popwin:popup-window-width` or `popwin:popup-window-height` will be
    used. See also position keyword.

`:position`

:   The value must be one of `(left top right bottom)`. The popup window
    will shown at the position of the frame.  If no position specified,
    `popwin:popup-window-position` will be used.

`:noselect`

:   If the value is non-nil, the popup window will not be selected when
    it is shown.

`:dedicated`

:   If the value is non-nil, the popup window will be dedicated to the
    original popup buffer. In this case, when another buffer is selected
    in the popup window, the popup window will be closed immedicately
    and the selected buffer will be shown on the previously selected
    window.

`:stick`

:   If the value is non-nil, the popup window will be stuck when it is
    shown.

`:last`

:   If the value is non-nil, the popup window will show the last
    contents.

### Examples

    ;; M-x anything
    (setq anything-samewindow nil)
    (push '("*anything*" :height 20) popwin:special-display-config)
    
    ;; M-x dired-jump-other-window
    (push '(dired-mode :position top) popwin:special-display-config)
    
    ;; M-!
    (push "*Shell Command Output*" popwin:special-display-config)
    
    ;; M-x compile
    (push '(compilation-mode :noselect t) popwin:special-display-config)
    
    ;; slime
    (push "*slime-apropos*" popwin:special-display-config)
    (push "*slime-macroexpansion*" popwin:special-display-config)
    (push "*slime-description*" popwin:special-display-config)
    (push '("*slime-compilation*" :noselect t) popwin:special-display-config)
    (push "*slime-xref*" popwin:special-display-config)
    (push '(sldb-mode :stick t) popwin:special-display-config)
    (push 'slime-repl-mode popwin:special-display-config)
    (push 'slime-connection-list-mode popwin:special-display-config)
    
    ;; vc
    (push "*vc-diff*" popwin:special-display-config)
    (push "*vc-change-log*" popwin:special-display-config)
    
    ;; undo-tree
    (push '(" *undo-tree*" :width 0.3 :position right) popwin:special-display-config)

Universal Display Config
------------------------

`popwin:universal-display-config` is a special alternative value of
`popwin:special-display-config`, which will be used when executing a
command with `M-x popwin:universal-display` prefix. If you want to
show a specific buffer in a popup window at the time, for example, you
can do it with `M-x popwin:universal-display RET C-x 4 C-o BUFNAME
RET`.

The default value is `(t)`, meaning all of buffers with `M-x
popwin:universal-display` prefix will be shown in a popup window.

Working with Other Extensions
-----------------------------

Some extensions needs workaround for working with popwin.

#### YaTeX

`misc/popwin-yatex.el` helps you to show YaTeX related buffers in a
popup window. Add the following code into `.emacs`.

    (require 'popwin-yatex)

You may write a configuration like:

    (push '("*YaTeX-typesetting*") popwin:special-display-config)

#### w3m

`misc/popwin-w3m.el` helps you to show specific pages with w3m in a
popup window. Add the following code into `.emacs`.

    (require 'popwin-w3m)

It is recommended to change `browse-url-browser-function` to
`popwin:w3m-browse-url`.

    (setq browse-url-browser-function 'popwin:w3m-browse-url)

`popwin:w3m-browse-url` is a function (and a command) displaying w3m
buffers in a popup window if the given URL is matched with the rules.

The rules are described by `popwin:w3m-special-display-config`
variable, which has almost same structure of
`popwin:special-display-config`.

The difference is `popwin:w3m-special-display-config` takes an URL
regular expression instead of a buffer pattern.

For example, if you want to show google search pages in a popup
window, a configuration could be:

    (push '("^http://www\\.google\\.com/.*$") popwin:w3m-special-display-config)

#### `term.el`

`misc/popwin-term.el` helps you to show term buffers in a popup
window. Add the following code into `.emacs`.

    (require 'popwin-term)

Then write a configuration like:

    (push '(term-mode :position :top :height 16 :stick t) popwin:special-display-config)

Now, you can show a term buffer in a popup window with `M-x
popwin-term:term`.

#### `browse-kill-ring.el`

`misc/browse-kill-ring.el` helps you to show `*Kill Ring*` buffer in a
popup window. Add the following code into `.emacs`.

    (require 'popwin-browse-kill-ring)

Then write a configuration like:

    (push "*Kill Ring*" popwin:special-display-config)

`M-x browse-kill-ring` now shows `*Kill Ring*` buffer in a popup
window.

#### `windows.el`

Do not load `windows.el` after loading `popwin.el`. Load `windows.el`
first.

Basic Commands
--------------

### Command: `popwin:popup-buffer`

Focely show the specified buffer in a popup
window. `popwin:special-display-config` will be ignored.

### Command: `popwin:display-buffer`

Show the specified buffer in a popup window if possible, meaning there
is at least one matched configuration in
`popwin:special-display-config`. Otherwise, fallback to
`display-buffer`.

### Command: `popwin:display-last-buffer`

Show the lastly shown buffer in a popup window.

### Command: `popwin:pop-to-buffer`

Same as `popwin:display-buffer`, but behaves like `pop-to-buffer`.

### Command: `popwin:one-window`

Same like `C-x 1` except that `C-g` restore the original window
configuration. This is useful when you see the contents of the popup
window in full window temporarily.

### Command: `popup:find-file`

`find-file` in a popup window.

### Command: `popwin:messages`

Show `*Messages*` buffer in a popup window.

Basic API
---------

### Function: `popwin:create-popup-window`

    popwin:create-popup-window &optional size position adjust => (master-window popup-window)

`popwin:create-popup-window` creates a popup window and return it with
a master window. Master window is a window which is splitted when
creating the popup window. A resposibility of closing the popup window
is on developers.

----

Copyright (C) 2011, 2012  Tomohiro Matsuyama <<tomo@cx4a.org>>
