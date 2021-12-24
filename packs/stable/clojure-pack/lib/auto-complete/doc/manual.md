# Auto-Complete - User Manual

# Introduction

Auto-Complete (a.k.a `auto-complete.el`, `auto-complete-mode`) is an
extension that automates and advances the completion-system of GNU
Emacs. It is superior to the old system. Features include:

* Visual interface
* Reduce overhead of completion by using a statistical method
* Extensibility

This user manual covers from how to install and use to how to
extend. Please contact me if you have any questions.

Auto Complete Mode is licensed under the terms of GPLv3. And this
document is licensed under the term of GFDL.

# Installation

## Requirements

* 800MHz or higher CPU
* 256MB or higher RAM
* GNU Emacs 24 or later

## Installation

You can install `auto-complete.el` from [MELPA](https://melpa.org/#/) or [MELPA Stable](https://stable.melpa.org/#/) with package.el.
Add following configurations for initializing package.el.

```emacs-lisp
(require 'package)

;; If you want to use latest version
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; If you want to use last tagged version
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)
```

Then You can install auto-complete by

- `M-x package-list-packages` and select auto-complete
- `M-x package-refresh-contents` and `M-x package-install auto-complete`

## After-Installation Check

Type some characters in \*scratch\* buffer in a restarted or
newly-launched Emacs. Installation has been successful if you see a
completion menu. If you have an error or no completion is started, it
could be a failure. Please contact me in such case with confirmation
following:

  * Characters `AC` in mode-line?

    If you don't see characters `AC` in mode-line (a gray line of
    bottom of buffer), `auto-complete-mode` is not enabled. Type `M-x
    auto-complete-mode` to enable and try again.

  * Error occurred

    If you have \*Backtrace* with errors or errors in minibuffer
    (bottom of frame), please contact me with the errors.

# Basic Usage

First, in a meaning, `auto-complete-mode` has no "usage". Because
`auto-complete-mode` is designed to fade into the Emacs editing
system. Users will receive a highly-developed completion system
automatically without any difficulty. Ultimately, a goal of
`auto-complete-mode` is to provide a system that does what users want
without any command, but it is impossible to accomplish 100% accuracy
actually. So there is "usage" to cover those points.

## Input Characters

Inputting characters is basic. Completions will never be shown without
any character. So when completion will be started, what character
causes completion to be started? It is a good question but it is
difficult to answer here. In simple words, completion will be started
when a character is inserted. See [`ac-trigger-commands`](#ac-trigger-commands) for more
details.

![Inputting Characters](ac.png)

## Completion by TAB

After completion is started, completion by TAB will be enabled
temporarily. Completion by TAB is the most important and most frequent
used command. TAB has several meanings.

  * Case that only one candidate remains

    If only one candidate remains, the candidate will be used to
    complete.

  * Case that there is a common part among candidates

    For example, if all candidates start with "set", it means they
    have a common part "set". So TAB completes "set" at first.

  * Otherwise

    Otherwise, select candidates in cycle by typing TAB.

It may be a little different according to settings, but basically
completion by TAB works as we wrote above. A reason why TAB has
several meanings is that we want users to do anything with TAB.

## Completion by RET

Like completion by TAB but some points are different:

* Complete a selected candidate immediately
* Execute an action if a selected candidate has the action

It is necessary to type TAB a few times for completion by
TAB. Completion by RET instead complete a selected candidate
immediately, so when you see a candidate you want, just type RET. If
the candidate has an action, the action will be executed. Take a
example of builtin abbrev completion. In completion by TAB, an abbrev
which expands "www" to "World Wide Web" will be completed to "www",
but in completion by RET, the abbrev will be expanded to "World Wide
Web" as completion.

## Candidate Selection

Following the `auto-complete-mode` philosophy, it is not recommended
to manually select candidates. That means it has been failed to guess
the completion, and also it requires users to do candidate selection
which is a high cost operation. We think there are so many cases that
requires to do candidate selection, because completion by TAB will
help candidate selection somehow and in recent versions, a statistical
method contributes to make a candidate suggestion more
accurate. However, actually, this is such cases. So we also think it
is not bad idea to remember how to select candidates.

Selecting candidates is not a complex operation. You can select
candidates forward or backward by cursor key or `M-p` and
`M-n`. According to settings, a behavior of completion by TAB will be
changed as a behavior of completion by RET. See [`ac-dwim`](#ac-dwim) for more
details.

There are other ways to select candidates. `M-1` to select candidate
1, `M-2` to select candidate 2, and so on.

## Help

`auto-complete-mode` has two types of help functionalities called
*Quick Help* and *Buffer Help*. They are different in a point of
displaying. Quick help will appear at the side of completion menu, so
you can easily see that, but there is a problem if there is no space
to display the help. Quick help will be shown automatically. To use
quick help, you need to set [`ac-use-quick-help`](#ac-use-quick-help) to `t`. Delay time
to show quick help is given by [`ac-quick-help-delay`](#ac-quick-help-delay).

On the other side, buffer help will not be shown without any
instructions from users. Buffer help literally displays help in a
buffer in another window. It costs more to see than quick help, but it
has more readability. To show buffer help, press `C-?` or `f1`. By
pressing `C-M-v` or `C-M-S-v` after showing buffer help, you can
scroll forward or backward through the help buffer. Other commands
will be fallbacked and buffer help will be closed.

## Summary

Completion will be started by inserting characters. After completion
is started, operations in the following table will be enabled
temporarily. After completion is finished, these operations will be
disabled.

| Key           | Command       | Description               |
|---------------|---------------|---------------------------|
| `TAB`, `C-i`  | `ac-expand`   | Completion by TAB         |
| `RET`, `C-m`  | `ac-complete` | Completion by RET         |
| `down`, `M-n` | `ac-next`     | Select next candidate     |
| `up`, `M-p`   | `ac-previous` | Select previous candidate |
| `C-?`, `f1`   | `ac-help`     | Show buffer help          |

To stop completion, simply use `C-g`.

# Advanced Usage

## `auto-complete` command

Basically there is an assumption that `auto-complete-mode` will be
started automatically, but there is also exception. For example, that
is a case that an user wants to complete without inserting any
character or a case not to start `auto-complete-mode` automatically by
settings. A command called `auto-complete` is useful in such cases,
which is used with key binding in general. The following code changes
a default completion command to more advanced feature that
`auto-complete-mode` provides.

```emacs-lisp
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
```

So, as of `auto-complete` command, it is a little different from an
original automatic completion.

  * Case that only one candidate remains

    Complete immediately without showing completion menu.

  * Case that no candidates remains

    Attempt to complete with fuzzy matching. See
    [Completion by Fuzzy Matching](#completion-by-fuzzy-matching) for more details.

  * Otherwise

    Otherwise start completion with/without expanding a whole common
    part and showing completion menu. See also
    [`ac-show-menu-immediately-on-auto-complete`](#ac-show-menu-immediately-on-auto-complete) and
    [`ac-expand-on-auto-complete`](#ac-expand-on-auto-complete).

## Completion by Fuzzy Matching

In a case that there are no candidates by `auto-complete` command or
the case where `ac-fuzzy-complete` command is executed,
`auto-complete-mode` attempts to complete with fuzzy matching instead
of the usual exact matching. Parameters of fuzzy matching have already
been optimized for use, so users don't need to change them. However if
you want to know the internals, see `fuzzy.el`. Using completion by
fuzzy matching, typos will be fixed as a series of completion. For
instance, input "messaeg" in a buffer, and then do `M-x auto-complete`
or `M-x ac-fuzzy-complete`. The cursor color will be changed to red if
completion has been successful, and then you can continue to complete,
regarding "messaeg" as "message". It is not a bad idea to bind
`auto-complete` command to some key to handle such cases.

![Fuzzy matching](ac-fuzzy.png)

## Filtering Completion Candidates

You can start filtering by `C-s`(You need to set `ac-use-menu-map` to `t`).
The cursor color will change to blue. Then input characters to filter.
It is possible to do completion by TAB or select candidates, which changes
the cursor color to original so that telling filtering completion candidates
has done. The filtering string will be restored when `C-s` again. To delete the
filter string, press `DEL` or `C-h`. Other general operations is not allowed there.

![Filtering](ac-isearch.png)

## Trigger Key

It is difficult what key `auto-complete` command is bound to. It
should be bound to a key which is easy to press as much as possible
because completion operation is often happened. However, it is a major
problem that there is no empty key to press
easily. `auto-complete-mode` provides a feature called *Trigger Key*
that handles such the problem. Using trigger key, you can use an
arbitrary key temporarily if necessary. The following code uses `TAB`
as trigger key.

```emacs-lisp
(ac-set-trigger-key "TAB")
```

Trigger key will be enabled after inserting characters. Otherwise it
is dealt as an usual command (TAB will be indent). Generally, trigger
key is used with `ac-auto-start` being `nil`.

```emacs-lisp
(setq ac-auto-start nil)
```

As of `ac-auto-start`, see [Not to complete automatically](#not-to-complete-automatically) or
[`ac-auto-start`](#ac-auto-start) for more details.

## Candidate Suggestion

`auto-complete-mode` analyzes completion operations one by one and
reduces overheads of completion as much as possible. For example,
having a candidate "foobar" been completed few times,
`auto-complete-mode` arranges it to top of the candidates next time
and make a situation that allows users to complete the word with one
time TAB or few times TAB. It is called `comphist` internally, and you
can use it by setting `ac-use-comphist` to `t`. It is enabled by
default. Collection operations data will be stored in
`user-emacs-directory` or `~/.emacs.d/` with a name `ac-comphist.dat`.

`auto-complete-mode` collects two types of data to accomplish accurate
candidate suggestion.

* Count of completion
* Position of completion

Simply saying, it collects not only a completion count but also a
position of completion. A completion candidate will be scored with the
count and the point. If you complete `find-file` with a word f few
times, in next time `find-file` will be arranged to top of
candidates. However it is too simple. Actually `find-file` with
`find-` will not have the same score, because a distance between `f`
and `find-` will reduce a weight of scoring. It means that if you
often complete `find-library` after `find-`, `find-library` will get
high score than `find-file` at that position. So `auto-complete-mode`
can guess `find-file` will be top after `f` and `find-library` will be
top after `find-` as it seems to learn from users' operations.

## Completion by Dictionary

Dictionary is a simple list of string. There is three types of
dictionary: user defined dictionary, major mode dictionary, and
extension dictionary. You need to add `ac-source-dictionary` to
`ac-sources` (default). See [Source](#source) for more details.

### User Defined Dictionary#

User defined dictionary is composed of a list of string specified
`ac-user-dictionary` and dictionary files specified by
`ac-user-dictionary-files`. Dictionary file is a word list separated
with newline. User defined dictionary is shared with all buffers. Here
is example adding your mail address to dictionary.

```emacs-lisp
(add-to-list 'ac-user-dictionary "foobar@example.com")
```

Setting will be applied immediately. Try to input "foo" in a
buffer. You may see `foobar@example.com` as a completion
candidate. This setting will be cleared if Emacs will quit. You need
to write the following code to keep setting in next Emacs launching.

```emacs-lisp
(setq ac-user-dictionary '("foobar@example.com" "hogehoge@example.com"))
```

There is more easy way to add word to dictionary. Files specified by
`ac-user-dictionary-files` will be treated as dictionary files. By
default, `~/.dict` will be a dictionary file, so edit `~/.dict` like:

```
foobar@example.com
hogehoge@example.com
```

As we said, words are separated with newline. They are not applied
immediately, because `auto-complete-mode` uses cache not to load every
time from a dictionary file. It may be high cost. To clear cache, do
`M-x ac-clear-dictionary-cache`. After that, dictionary files will be
load absolutely.

No need to say perhaps, you can use other files as dictionary file by
adding to `ac-user-dictionary-files`.

### Major Mode Dictionary and Extension Dictionary

You can use other dictionaries for every major-modes and extensions. A
dictionary will loaded from a directory specified with
`ac-dictionary-directories`. `ac-dictionary-directories` may be the
following setting if you followed [Installation](#installation) instructions.

```emacs-lisp
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
```

A dictionary named `c++-mode` for specific major-mode and a dictionary
named `txt` for specific extension will be stored in the
directory. For instance, you complete in a buffer named `a.cpp` with
dictionary completion, following the setting above,
`~/.emacs.d/ac-dict/c++-mode` and `~/.emacs.d/ac-dict/cpp` will be
loaded as dictionary file. You can edit the dictionary files and make
a new one. In addition, you can add a new dictionary file to a
directory that has same configuration.

As same as user defined dictionary, after editing and adding
dictionary, you should do `M-x ac-clear-dictionary-cache` to apply
changes.

# Source

*Source* is a concept that ensures the extensibility of
`auto-complete-mode`. Simply saying, source is a description of:

* How to generate completion candidates
* How to complete
* How to show

Anybody who knows a little Emacs Lisp can define a source easily. See
[Extend](#extend) for how to define a source. Here we will explain how to use
built-in sources.

Usually a source name starts with `ac-source-`. So you can list
sources with `apropos` (`M-x apropos RET ^ac-source-`). You may see
`ac-source-filename` and `ac-source-dictionary` which are entities of
sources.

## Using Source

If you wrote `(ac-config-default)` in your `.emacs.d/init.el`, it is rare to
change a source setting because it is already optimized to use. Here
is a short explanation about source however. Sources will be used by
setting `ac-sources` to a list of sources. You can see the setting by
evaluating `ac-sources` in \*scratch\* buffer:

```emacs-lisp
;; Formatted
(ac-source-filename
 ac-source-functions
 ac-source-yasnippet
 ac-source-variables
 ac-source-symbols
 ac-source-features
 ac-source-abbrev
 ac-source-words-in-same-mode-buffers
 ac-source-dictionary)
```

As you see, `ac-sources` in \*scratch\* buffer has six sources. We
explain each source for detail, you can guess meanings of sources. It
is worth to remember that `ac-sources` is a buffer local variable,
which means each `ac-sources` for buffers will be different.

Here is an example. Imagine you are at the \*scratch\* buffer. As we
said, this buffer has many sources. Some people think there are too
many. So try to change `ac-sources` to reduce functionality. It is
easy to change. Just evaluate the following code in the \*scratch\*
buffer or with `M-:`:

```emacs-lisp
(setq ac-sources '(ac-source-symbols ac-source-words-in-same-mode-buffers))
```

This example changes `ac-source` setting and enable only symbol
completion and word completion among same major modes. Then, how can
we enable this setting in next Emacs launching? We can change settings
by adding a hook which is called when \*scratch\* buffer is created.

```emacs-lisp
(defun my-ac-emacs-lisp-mode ()
  (setq ac-sources '(ac-source-symbols ac-source-words-in-same-mode-buffers)))

(add-hook 'emacs-lisp-mode-hook 'my-ac-emacs-lisp-mode)
```

If the code `(ac-config-default)` is written in `.emacs.d/init.el`, the code
above may not work correctly. This is because `(ac-config-default)`
will overwrite the setting. In such case, you can redefine a function
which is used in `(ac-config-default)`. The function name is
`ac-emacs-lisp-mode-setup` in `emacs-lisp-mode`. See
`auto-complete-config.el` for more details.

```emacs-lisp
(defun ac-emacs-lisp-mode-setup ()
  (setq ac-sources '(ac-source-symbols ac-source-words-in-same-mode-buffers)))
```

So, now you know how to change sources in a specific major
mode. Summary is:

1. Define a function changing `ac-sources`
2. Register the function to proper mode hooks (`c++-mode-hook`, `ruby-mode-hook`, and `python-mode-hook`, etc)

By the way, how can we change a setting for all buffers? We use
`setq-default` to change `ac-sources` instead of `setq` in such
case. Then the default value of `ac-sources` will be changed to the
value you specified.

```emacs-lisp
(setq-default ac-sources '(ac-source-words-in-all-buffer))
```

There are other ways to do that. `(ac-config-default)` changes the
default value of `ac-sources` by registering a hook for
`auto-complete-mode`. The registered function is `ac-common-setup`
that adds `ac-source-filename` to the first of `ac-sources` by
default. So all `auto-complete-mode` enabled buffer will have
`ac-source-filename` at the first of `ac-sources`. A reason why adding
to the first is relating to [Omni Completion](#omni-completion). Anyway you don't care
about it here. So if you want to change `ac-sources` of all buffer,
you can redefine `ac-common-setup` function to do that.

```emacs-lisp
;; Add ac-source-dictionary to ac-sources of all buffer
(defun ac-common-setup ()
  (setq ac-sources (append ac-sources '(ac-source-dictionary))))
```

## Builtin Sources

Here are defined sources in `auto-complete.el` and
`auto-complete-config.el`.

### `ac-source-abbrev`

A source for Emacs abbreviation function. See `info emacs Abbrevs`
about abbreviation function.

### `ac-source-css-property`#

A source for CSS property.

### `ac-source-dictionary`

A source for dictionary. See [Completion by Dictionary](#completion-by-dictionary) about
dictionary.

### `ac-source-eclim`

A source for [Emacs-eclim](http://github.com/senny/emacs-eclim).

### `ac-source-features`

A source for completing features which are available with `(require
'`.

### `ac-source-filename`

A source for completing file name. Completion will be started after
inserting `/`.

### `ac-source-files-in-current-dir`

A source for completing files in a current directory. It may be useful
with `eshell`.

### `ac-source-functions`

A source for completing Emacs Lisp functions. It is available only
after `(`.

### `ac-source-ghc-mod`

A source for [ghc-mod](http://www.mew.org/~kazu/proj/ghc-mod/en/).

### `ac-source-gtags`

A source for completing tags of
[Global](http://www.tamacom.com/global.html).

### `ac-source-imenu`

A source for completing `imenu` nodes. See `info emacs imenu` for
details.

### `ac-source-semantic`

A source for
[Semantic](http://cedet.sourceforge.net/semantic.shtml). It can be
used for completing member name for C/C++.

### `ac-source-slime`

A source for [SLIME](http://common-lisp.net/project/slime/).

### `ac-source-semantic-raw`

Unlike `ac-source-semantic`, this source is for completing symbols in
a raw namespace.

### `ac-source-symbols`

A source for completing Emacs Lisp symbols.

### `ac-source-variables`

A source for completing Emacs Lisp symbols.

### `ac-source-words-in-all-buffer`

A source for completing words in all buffer. Unlikely
[`ac-source-words-in-same-mode-buffers`](#ac-source-words-in-same-mode-buffers), it doesn't regard
major-mode.

### `ac-source-words-in-buffer`

A source for completing words in a current buffer.

### `ac-source-words-in-same-mode-buffers`

A source for completing words which are collected over buffers whom
major-mode is same to of a current buffer. For example, words will
shared among `a.cpp` and `b.cpp`, but not shared among `a.pl` and
`b.cpp` because they are different major-mode buffers. Usually this
source is more useful than [`ac-source-words-in-all-buffer`](#).

### `ac-source-yasnippet`

A source for [Yasnippet](http://code.google.com/p/yasnippet/) to
complete and expand snippets.

# Tips

## Not to complete automatically

If you are being annoyed with displaying completion menu, you can
disable automatic starting completion by setting `ac-auto-start` to
`nil`.

```emacs-lisp
(setq ac-auto-start nil)
```

You need to bind some key to `auto-complete` command (because you need
to complete anyway). For example, bind to `ac-mode-map`, which is a
key map for `auto-complete-mode` enabled buffer:

```emacs-lisp
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
```

Or bind to global key map.

```emacs-lisp
(global-set-key "\M-/" 'auto-complete)
```

In addition, if you allow to start completion automatically but also
want to be silent as much as possible, you can do it by setting
`ac-auto-start` to an prefix length integer. For example, if you want
to start completion automatically when you has inserted 4 or more
characters, just set `ac-auto-start` to 4:

```emacs-lisp
(setq ac-auto-start 4)
```

Setting `ac-auto-start` to large number will result in good for
performance. Lesser `ac-auto-start`, more high cost to produce
completion candidates, because there will be so many candidates
necessarily. If you feel `auto-complete-mode` is stalling, change
`ac-auto-start` to a larger number or `nil`.

See [`ac-auto-start`](#ac-auto-start) for more details.

And consider to use [Trigger Key](#trigger-key).

## Not to show completion menu automatically

There is another approach to solve the annoying problem is that not to
show completion menu automatically. Not to show completion menu
automatically, set [`ac-auto-show-menu`](#ac-auto-show-menu) to `nil`.

```emacs-lisp
(setq ac-auto-show-menu nil)
```

When you select or filter candidates, completion menu will be shown.

In other way, you can delay showing completion menu by setting
`ac-auto-show-menu` to seconds in real number.

```emacs-lisp
;; Show 0.8 second later
(setq ac-auto-show-menu 0.8)
```

This interface has both good points of completely automatic completion
and completely non-automatic completion. This may be default in the
future.

## Stop completion

You can stop completion by pressing `C-g`. However you won't press
`C-g` while defining a macro. In such case, it is a good idea to bind
some key to `ac-completing-map`.

```emacs-lisp
(define-key ac-completing-map "\M-/" 'ac-stop)
```

Now you can stop completion by pressing `M-/`.

## Finish completion by TAB

As we described above, there is many behaviors in TAB. You need to use
TAB and RET properly, but there is a simple interface that bind RET to
original and TAB to finish completion:

```emacs-lisp
(define-key ac-completing-map "\t" 'ac-complete)
(define-key ac-completing-map "\r" nil)
```

## Select candidates with C-n/C-p only when completion menu is displayed

By evaluating the following code, you can select candidates with
C-n/C-p, but it might be annoying sometimes.

```emacs-lisp
;; Bad config
(define-key ac-completing-map "\C-n" 'ac-next)
(define-key ac-completing-map "\C-p" 'ac-previous)
```

In this case, it is better that selecting candidates is enabled only
when completion menu is displayed so that the key input will not be
taken as much as possible. `ac-menu-map` is a keymap for completion on
completion menu which is enabled when `ac-use-menu-map` is `t`.

```emacs-lisp
(setq ac-use-menu-map t)
;; Default settings
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)
```

See [`ac-use-menu-map`](#ac-use-menu-map) and [`ac-menu-map`](#ac-menu-map) for more details.

## Not to use quick help

A tooltip help that is shown when completing is called quick help. You
can disable it if you don't want to use it:

```emacs-lisp
(setq ac-use-quick-help nil)
```

## Change a height of completion menu

Set `ac-menu-height` to number of lines.

```emacs-lisp
;; 20 lines
(setq ac-menu-height 20)
```

## Enable `auto-complete-mode` automatically for specific modes

`auto-complete-mode` won't be enabled automatically for modes that are
not in `ac-modes`. So you need to set if necessary:

```emacs-lisp
(add-to-list 'ac-modes 'brandnew-mode)
```

## Ignore case

There is three ways to distinguish upper case and lower case.

```emacs-lisp
;; Just ignore case
(setq ac-ignore-case t)
;; Ignore case if completion target string doesn't include upper characters
(setq ac-ignore-case 'smart)
;; Distinguish case
(setq ac-ignore-case nil)
```

Default is `smart`.

## Stop completion automatically after inserting specific words

Set `ac-stop-words` to words that stops completion automatically. In
ruby, some people want to stop completion automatically after
inserting "end":

```
(add-hook 'ruby-mode-hook
          (lambda ()
            (make-local-variable 'ac-stop-words)
            (add-to-list 'ac-stop-words "end")))
```

Note that `ac-stop-words` is not a buffer local variable, so you need
to make it buffer local with `make-local-variable` if it is buffer
specific setting.

## Change colors

Colors settings are following:

| Face                 | Description                           |
|----------------------|---------------------------------------|
| `ac-completion-face` | Foreground color of inline completion |
| `ac-candidate-face`  | Color of completion menu              |
| `ac-selection-face`  | Selection color of completion menu    |

To change face background color, use `set-face-background`. To change
face foreground color, use `set-face-foreground`. To set underline,
use `set-face-underline`.

```emacs-lisp
;; Examples
(set-face-background 'ac-candidate-face "lightgray")
(set-face-underline 'ac-candidate-face "darkgray")
(set-face-background 'ac-selection-face "steelblue")
```

## Change default sources

Read [Source](#source) first if you don't familiar with sources. To change
default of sources, use `setq-default`:

```emacs-lisp
(setq-default ac-sources '(ac-source-words-in-all-buffer))
```

## Change sources for specific major modes

For example, you may want to use specific sources for C++ buffers. To
do that, register a hook by `add-hook` and change `ac-sources`
properly:

```emacs-lisp
(add-hook 'c++-mode (lambda () (add-to-list 'ac-sources 'ac-source-semantic)))
```

## Completion with specific source

You can start completion with specific source. For example, if you
want to complete file name, do `M-x ac-complete-filename` at point. Or
if you want to complete C/C++ member name, do `M-x
ac-complete-semantic` at point. Usually, you may bind them to some key
like:

```emacs-lisp
;; Complete member name by C-c . for C++ mode.
(add-hook 'c++-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c .") 'ac-complete-semantic)))
;; Complete file name by C-c /
(global-set-key (kbd "C-c /") 'ac-complete-filename)
```

Generally, such commands will be automatically available when sources
are defined. Assume that a source named `ac-source-foobar` is being
defined for example, a command called `ac-complete-foobar` will be
also defined automatically. See also [Builtin Sources](#builtin-sources) for available
commands.

If you want to use multiple sources for a command, you need to define
a command for it like:

```emacs-lisp
(defun semantic-and-gtags-complete ()
  (interactive)
  (auto-complete '(ac-source-semantic ac-source-gtags)))
```

`auto-complete` function can take an alternative of `ac-sources`.

## Show help persistently

Use `ac-persist-help` instead of `ac-help`, which is bound to `M-<f1>`
and `C-M-?`.

## Show a lastly completed candidate help

`ac-last-help` command shows a lastly completed candidate help in a
`ac-help` (buffer help) form. If you give an argument by `C-u` or just
call `ac-last-persist-help`, its help buffer will not disappear
automatically.

`ac-last-quick-help` command show a lastly completed candidate help in
a `ac-quick-help` (quick help) form. It is useful if you want to see a
function documentation, for example.

You may bind keys to these command like:

```emacs-lisp
(define-key ac-mode-map (kbd "C-c h") 'ac-last-quick-help)
(define-key ac-mode-map (kbd "C-c H") 'ac-last-help)
```

## Show help beautifully

If [pos-tip.el](http://www.emacswiki.org/emacs-en/PosTip) is
installed, `auto-complete-mode` uses its native rendering engine for
displaying quick help instead of legacy one.

# Third-party Extensions

## SLIME

See [ac-slime](https://github.com/purcell/ac-slime) page.

## Scala

See [ENSIME](https://github.com/aemoncannon/ensime) page.

# Configuration

Any configuration item will be set in `.emacs.d/init.el` or with `M-x
customize-group RET auto-complete RET`.

## `ac-delay`

Delay time to start completion in real number seconds. It is a
trade-off of responsiveness and performance.

## `ac-auto-show-menu`

Show completion menu automatically if `t` specified. `t` means always
automatically showing completion menu. `nil` means never showing
completion menu. Real number means delay time in seconds.

## `ac-show-menu-immediately-on-auto-complete`

Whether or not to show completion menu immediately on `auto-complete`
command. If inline completion has already been showed, this
configuration will be ignored.

## `ac-expand-on-auto-complete`

Whether or not to expand a common part of whole candidates.

## `ac-disable-faces`

Specify a list of face symbols for disabling auto completion. Auto
completion will not be started if a face text property at a point is
included in the list.

## `ac-stop-flymake-on-completing`

Whether or not to stop Flymake on completion.

## `ac-use-fuzzy`

Whether or not to use [fuzzy matching](#completion-by-fuzzy-matching).

## `ac-fuzzy-cursor-color`

Change cursor color to specified color when
[Completion by Fuzzy Matching](#completion-by-fuzzy-matching) is started. `nil` means never
changed. Available colors can be seen with `M-x list-colors-display`.

## `ac-use-comphist`

Whether or not to use [Candidate Suggestion](#candidate-suggestion). `nil` means never
using it and get performance better maybe.

## `ac-comphist-threshold`

Specify a percentage of limiting lower scored candidates. 100% for
whole scores.

## `ac-comphist-file`

Specify a file stores data of [Candidate Suggestion](#candidate-suggestion).

## `ac-use-quick-help`

Whether or not to use quick help.

## `ac-quick-help-delay`

Delay time to show quick help in real number seconds.

## `ac-menu-height`

Specify an integer of lines of completion menu.

## `ac-quick-help-height`

Specify an integer of lines of quick help.

## `ac-quick-help-prefer-pos-tip`

Whether or not auto-complete prefers native tooltip with pos-tip than
overlap popup for displaying quick help. If non-nil, you also need to
install [pos-tip.el](http://www.emacswiki.org/emacs/PosTip) so that
displaying tooltip can work well.

## `ac-candidate-limit`

Limit a number of candidates. Specifying an integer, the value will be
a limit of candidates. `nil` means no limit.

## `ac-modes`

Specify major modes as a list of symbols that will be enabled
automatically if `global-auto-complete-mode` is enabled.

## `ac-compatible-packages-regexp`

Specify a regexp that identifies starting completion or not for that
package.

## `ac-non-trigger-commands`

Specify commands as a list of symbols that does NOT starts completion
automatically.

## `ac-trigger-commands`

Specify commands as a list of symbols that starts completion
automatically. `self-insert-command` is one of default.

## `ac-trigger-commands-on-completing`

Same as [`ac-trigger-commands`](#ac-trigger-commands) expect this will be used on
completing.

## `ac-trigger-key`

Specify a [Trigger Key](#trigger-key).

## `ac-auto-start`

Specify how completion will be started. `t` means always starting
completion automatically. `nil` means never started automatically. An
integer means completion will not be started until the value is more
than a length of the completion target string.

## `ac-stop-words`

Specify a list of strings that stops completion.

## `ac-use-dictionary-as-stop-words`

Specify whether auto-complete uses dictionaries as stop words.

## `ac-ignore-case`

Specify how distinguish case. `t` means always ignoring case. `nil`
means never ignoring case. `smart` in symbol means ignoring case only
when the completion target string doesn't include upper characters.

## `ac-dwim`

"Do What I Mean" function. `t` means:

* After selecting candidates, TAB will behave as RET
* TAB will behave as RET only on candidate remains

## `ac-use-menu-map`

Specify a special keymap (`ac-menu-map`) should be enabled when
completion menu is displayed. `ac-menu-map` will be enabled when it is
`t` and satisfy one of the following conditions:

* `ac-auto-start` and `ac-auto-show-menu` are not `nil`, and
  completion menu is displayed after starting completion
* Completion menu is displayed by `auto-complete` command
* Completion menu is displayed by `ac-isearch` command

## `ac-use-overriding-local-map`

Use only when operations is not affected. Internally it uses
`overriding-local-map`, which is too powerful to use with keeping
orthogonality. So don't use as much as possible.

## `ac-completion-face`

Face of inline completion.

## `ac-candidate-face`

Face of completion menu background.

## `ac-selection-face`

Face of completion menu selection.

## `global-auto-complete-mode`

Whether or not to use `auto-complete-mode` globally. It is `t` in
general.

## `ac-user-dictionary`

Specify a dictionary as a list of string for
[Completion by Dictionary](#completion-by-dictionary).

## `ac-user-dictionary-files`

Specify a dictionary files as a list of string for
[Completion by Dictionary](#completion-by-dictionary).

## `ac-dictionary-directories`

Specify a dictionary directories as a list of string for
[Completion by Dictionary](#completion-by-dictionary).

## `ac-sources`

Specify sources as a list of [Source](#source). This is a buffer local
variable.

## `ac-completing-map`

Keymap for completion.

## `ac-menu-map`

Keymap for completion on completion menu. See also
[`ac-use-menu-map`](#ac-use-menu-map).

## `ac-mode-map`

Keymap for `auto-complete-mode` enabled buffers.

# Extend

A meaning to extend `auto-complete-mode` is just defining a
[Source](#source). This section describe how to define a source.

## Prototype

Source basically takes a form of the following:

```emacs-lisp
(defvar ac-source-mysource1
  '((prop . value)
     ...))
```

As you see, source is just an associate list. You can define a source
by combining pairs of defined property and its value.

## Example

The most important property for source is [`candidates`](#candidates)
property. This property describes how to generate completion
candidates by giving a function, an expression, or a variable. A
result of evaluation should be a list of strings. Here is an example
to generate candidates "Foo", "Bar", and "Baz":

```emacs-lisp
(defvar ac-source-mysource1
  '((candidates . (list "Foo" "Bar" "Baz"))))
```

Then add this source to `ac-sources` and use:

```emacs-lisp
(setq ac-sources '(ac-source-mysource1))
```

It is successful if you have "Bar" and "Baz" by inserting "B". The
example above has an expression `(list ...)` in `candidates`
property. The expression specified there will not be byte-compiled, so
you should not use an expression unless it is too simple, because it
has a bad affection on performance. You should use a function instead
maybe:

```emacs-lisp
(defun mysource1-candidates ()
  '("Foo" "Bar" "Baz"))

(defvar ac-source-mysource1
  '((candidates . mysource1-candidates)))
```

The function specified in `candidates` property will be called without
any arguments on every time candidates updated. There is another way:
a variable.

## Initialization

You may want to initialize a source at first time to complete. Use
`init` property in these cases. As same as `candidates` property,
specify a function without any parameters or an expression. Here is an
example:

```emacs-lisp
(defvar mysource2-cache nil)

(defun mysource2-init ()
  (setq mysource2-cache '("Huge" "Processing" "Is" "Done" "Here")))

(defvar ac-source-mysource2
  '((init . mysource2-init)
    (candidates . mysource2-cache)))
```

In this example, `mysource2-init` function does huge processing, and
stores the result into `mysource2-cache` variable. Then specifying the
variable in `candidates` property, this source prevents huge
processing on every time update completions. There are possible usage:

* Do `require`
* Open buffers first of all

## Cache

Caching strategy is important for `auto-complete-mode`. There are two
major ways: `init` property and `cache` property that is described in
this section. Specifying `cache` property in source definition, a
result of evaluation of `candidates` property will be cached and
reused the result as the result of evaluation of `candidates` property
next time.

Rewrite the example in previous section by using `cache` property.

```emacs-lisp
(defun mysource2-candidates ()
  '("Huge" "Processing" "Is" "Done" "Here"))

(defvar ac-source-mysource2
  '((candidates . mysource2-candidates)
    (cache)))
```

There is no performance problem because this source has `cache`
property even if `candidates` property will do huge processing.

### Cache Expiration

It is possible to keep among more wider scope than `init` property and
`cache` property. It may be useful for remembering all function names
which is rarely changed. In these cases, how can we clear cache
property not at the expense of performance? This is true time use that
functionality.

Use `ac-clear-variable-after-save` to clear cache every time a buffer
saved. Here is an example:

```emacs-lisp
(defvar mysource3-cache nil)

(ac-clear-variable-after-save 'mysource3-cache)

(defun mysource3-candidates ()
  (or mysource3-cache
      (setq mysource3-cache (list (format "Time %s" (current-time-string))))))

(defvar ac-source-mysource3
  '((candidates . mysource3-candidates)))
```

Add this source to `ac-sources` and complete with "Time". You may see
a time when completion has been started. After that, you also see the
same time, because `mysource3-candidates` returns the cache as much as
possible. Then, save the buffer once and complete with "Time"
again. In this time, you may find a new time. An essence of this
source is to use `ac-clear-variable-after-save` to manage a variable
for cache.

It is also possible to clear cache periodically. Use
`ac-clear-variable-every-minute` to do that. A way to use is same to
`ac-clear-variable-after-save` except its cache will be cleared every
minutes. A builtin source `ac-source-functions` uses this
functionality.

## Action

[Completion by RET](#completion-by-ret) will evaluate a function or an expression
specified in `action` property. A builtin sources `ac-source-abbrev`
and `ac-source-yasnippet` use this property.

## Omni Completion

*Omni Completion* is a type of completion which regards of a context
of editing. A file name completion which completes with slashed
detected and a member name completion in C/C++ with dots detected are
omni completions. To make a source support for omni completion, use
`prefix` property. A result of evaluation of `prefix` property must be
a beginning point of completion target string. Retuning `nil` means
the source is disabled within the context.

Consider a source that completes mail addresses only after "To:
". First of all, define a mail address completion source as same as
above.

```emacs-lisp
(defvar ac-source-to-mailaddr
  '((candidates . (list "foo1@example.com"
                        "foo2@example.com"
                        "foo3@example.com"))))

(setq ac-sources '(ac-source-to-mailaddr))
```

Then enable completions only after "To: " by using `prefix`
property. `prefix` property must be one of:

* Regexp
* Function
* Expression

Specifying a regexp, `auto-complete-mode` thinks of a point of start
of group 1 or group 0 as a beginning point of completion target string
by doing `re-search-backward`[^1] with the regexp. If you want to do
more complicated, use a function or an expression instead. The
beginning point that is evaluated here will be stored into
[`ac-point`](#ac-point). In above example, regexp is enough.

```
^To: \(.*\)
```

A reason why capturing group 1 is skipping "To: ". By adding this into
the source definition, the source looks like:

```emacs-lisp
(defvar ac-source-to-mailaddr
  '((candidates . (list "foo1@example.com"
                        "foo2@example.com"
                        "foo3@example.com"))
    (prefix . "^To: \\(.*\\)")))
```

Add this source to `ac-sources` and then type "To: ". You will be able
to complete mail addresses.

[^1]: Strictly `re-search-backward` with the added adding `\=` at the end

## `ac-define-source`

You may use an utility macro called `ac-define-source` which defines a
source and a command.

```emacs-lisp
(ac-define-source mysource3
  '((candidates . (list "Foo" "Bar" "Baz"))))
```

This expression will be expanded like:

```emacs-lisp
(defvar ac-source-mysource3
  '((candidates . (list "Foo" "Bar" "Baz"))))

(defun ac-complete-mysource3 ()
  (interactive)
  (auto-complete '(ac-source-mysource3)))
```

A source will be defined as usual and in addition a command that
completes with the source will be defined. Calling `auto-complete`
without arguments will use `ac-sources` as default sources and with
arguments will use the arguments as default sources. Considering
compatibility, it is difficult to answer which you should use `defvar`
and `ac-define-source`. Builtin sources are defined with
`ac-define-sources`, so you can use them alone by binding some key to
these commands such like `ac-complete-filename`. See also
[Completion with specific source](#completion-with-specific-sourc).

### Source Properties

#### `init`

Specify a function or an expression that is evaluated only once when
completion is started.

#### `candidates`

Specify a function, an expression, or a variable to calculate
candidates. Candidates should be a list of string. If [`cache`](#cache-1)
property is enabled, this property will be ignored twice or later.

### `prefix`

Specify a regexp, a function, or an expression to find a point of
completion target string for [Omni Completion](#omni-completion). This
source will be ignored when `nil` returned. If a regexp is specified,
a start point of group 1 or group 2 will be used as a value.

### `requires`

Specify a required number of characters of completion target
string. If nothing is specified, `auto-complete-mode` uses
[`ac-auto-start`](#ac-auto-start) instead.

### `action`

Specify a function or an expression that is executed on
[Completion by RET](#completion-by-ret).

### `limit`

Specify a limit of candidates. It overrides `ac-candidate-limit`
partially.

### `symbol`

Specify a symbol of candidate meaning in one character string. The
symbol will be any character, but you should follow the rule:

| Symbol | Meaning          |
|--------+------------------|
| `s`    | Symbol           |
| `f`    | Function, Method |
| `v`    | Variable         |
| `c`    | Constant         |
| `a`    | Abbreviation     |
| `d`    | Dictionary       |

### `summary`

Specify a summary of candidate in string. It should be used for
summarizing the candidate in short string.

### `cache`

Use [Cache](#cache).

### `require`

Specify an integer or `nil`. This source will be ignored when the
integer value is lager than a length of completion target
string. `nil` means nothing ignored.

### `candidate-face`

Specify a face of candidate. It overrides [`ac-candidate-face`](#ac-candidate-face)
partially.

### `selection-face`

Specify a face of selection. It overrides [`ac-selection-face`](#ac-selection-face)
partially.

### `depends`

Specify a list of features (which are `require`d) that the source is
depending.

### `available`

Specify a function or an expression that describe the source is
available or not.

### `document`

Specify a function or an expression that returns documentation of the
candidate.

## Variables

Here is a list of often used variables.

### `ac-buffer`

A buffer where completion started.

### `ac-point`

A start point of completion target string.

### `ac-prefix`

A string of completion target.

### `ac-limit`

A limit of candidates. Its value may be one of
[`ac-candidate-limit`](#ac-candidate-limit) and [`limit`](#limit) property.

### `ac-candidates`

A list of candidates.

# Trouble Shooting

## Response Latency

To keep much responsibility is very important for
`auto-complete-mode`. However it is well known fact that a performance
is a trade off of functionalities. List up options related to the
performance.

### `ac-auto-start` {#trouble-ac-auto-start}

For a larger number, it reduces a cost of generating completion
candidates. Or you can remove the cost by setting `nil` and you can
use when you truly need. See [Not to complete automatically](#not-to-complete-automatically) for
more details.

### `ac-delay` {#trouble-ac-delay}

For a larger number, it reduces a cost of starting completion.

### `ac-auto-show-menu` {#trouble-ac-auto-show-menu}

For a larger number, it reduces a displaying cost of completion menu.

### `ac-use-comphist` {#trouble-ac-use-comphist}

Setting [`ac-use-comphist`](#ac-use-comphist) to `nil` to disable
[Candidate Suggestion](#candidate-suggestion), it reduces a cost of suggestion.

### `ac-candidate-limit` {#trouble-ac-candidate-limit}

For a property number, it reduces much computation of generating
candidates.

## Completion menu is disrupted

There is two major cases.

### Column Computation Case

`auto-complete-mode` tries to reduce a cost of computation of columns
to show completion menu correctly by using a optimized function at the
expense of accuracy. However, it probably causes a menu to be
disrupted. Not to use the optimized function, evaluate the following
code:

```emacs-lisp
(setq popup-use-optimized-column-computation nil)
```

### Font Case

There is a problem when render
[IPA font](http://ossipedia.ipa.go.jp/ipafont/) with Xft in Ubuntu
9.10. Use [VL gothic](http://dicey.org/vlgothic/), which renders more
suitably. Or disable Xft, then it can render correctly.

We don't good answers now, but you may shot the troubles by changing
font size with `set-face-font`. For instance, completion menu may be
disrupted when displaying the menu including Japanese in NTEmacs. In
such case, it is worth to try to evaluate the following code to fix
it:

```emacs-lisp
(set-face-font 'ac-candidate-face "MS Gothic 11")
(set-face-font 'ac-selection-face "MS Gothic 11")
```

# Known Bugs

## Auto completion will not be started in a buffer `flyspell-mode` enabled {#flyspell-mode-bug}

A way of delaying processes of `flyspell-mode` disables auto
completion. You can avoid this problem by `M-x
ac-flyspell-workaround`. You can write the following code into your
`~/.emacs.d/init.el`.

```emacs-lisp
(ac-flyspell-workaround)
```

## `linum-mode` tries to display the line numbers even for the comletion menu {#linum-mode-bug}

linum-mode tries to add the line numbers even for the comletion
menu. To stop that annoying behavior, do `M-x ac-linum-workaround` or
add the following code into your `~/.emacs.d/init.el`.

```emacs-lisp
(ac-linum-workaround)
```

# Reporting Bugs

Visit
[Auto-Complete Issue Tracker](https://github.com/auto-complete/auto-complete/issues)
and create a new issue.
