# git-gutter.el [![travis badge][travis-badge]][travis-link] [![melpa badge][melpa-badge]][melpa-link] [![melpa stable badge][melpa-stable-badge]][melpa-stable-link]

## Introduction
`git-gutter.el` is port of [GitGutter](https://github.com/jisaacks/GitGutter)
which is a plugin of Sublime Text. `git-gutter.el` supports `git`, `mercurial`
and `bazaar`.


`git-gutter.el` also supports TRAMP so you can use `git-gutter.el` for remote files.


## Screenshot

![git-gutter.el](image/git-gutter1.png)


## Requirements

* Emacs 24 or higher
* [Git](http://git-scm.com/) 1.7.0 or higher
* [Mercurial](http://mercurial.selenic.com/)
* [Bazaar](http://bazaar.canonical.com/)


## git-gutter.el vs [git-gutter-fringe.el](https://github.com/syohex/emacs-git-gutter-fringe)

|                      | git-gutter.el   | git-gutter-fringe.el |
|:---------------------|:---------------:|:--------------------:|
| Work in tty frame    | OK              | NG                   |
| Work with linum-mode | OK(experimental)| OK                   |
| Show on right side   | NG              | OK                   |
| More configurable    | OK              | NG                   |


## Installation

You can install `git-gutter.el` from [MELPA](http://melpa.org) with package.el
(`M-x package-install git-gutter`).

And you can also install it with [el-get](https://github.com/dimitri/el-get).


## Global Minor Mode and Minor Mode

`git-gutter.el` provides global minor-mode(`global-git-gutter-mode`) and minor-mode(`git-gutter-mode`).

If you want to use `git-gutter` for files in git repository.
You add following s-exp in your configuration file(`~/.emacs.d/init.el` or `~/.emacs`).

```lisp
(global-git-gutter-mode +1)
```

Other case, you want to use `git-gutter` for some files, you can use `git-gutter-mode`.
Following example of enabling `git-gutter` for some mode.

```lisp
(add-hook 'ruby-mode-hook 'git-gutter-mode)
(add-hook 'python-mode-hook 'git-gutter-mode)
```

## Commands

`git-gutter.el` provides following commands.

#### `git-gutter:next-hunk`

Jump to next hunk(alias `git-gutter:next-diff`)

#### `git-gutter:previous-hunk`

Jump to previous hunk(alias `git-gutter:previous-diff`)

#### `git-gutter:set-start-revision`

Set start revision where got diff(`git diff`, `hg diff` or `bzr diff`) from.

#### `git-gutter:popup-hunk`

Popup current diff hunk(alias `git-gutter:popup-diff`)

`git-gutter:next-hunk` and `git-gutter:previous-hunk` update content
of buffer popuped by `git-gutter:popup-diff` to current hunk.

#### `git-gutter:stage-hunk`

Stage current hunk. You can use this command like `git add -p`.
This command is not supported for `Mercurial`.

#### `git-gutter:revert-hunk`

Revert current hunk

#### `git-gutter`

Show changes from last commit or Update change information.
Please execute this command if diff information is not be updated.

#### `git-gutter:clear`

Clear changes

#### `git-gutter:toggle`

Toggle git-gutter

#### `git-gutter:linum-setup`

Setup for working with `linum-mode`.

#### `git-gutter:update-all-windows`

Update git-gutter information of buffers in all visible window.


## Sample Configuration

```lisp
(require 'git-gutter)

;; If you enable global minor mode
(global-git-gutter-mode t)

;; If you would like to use git-gutter.el and linum-mode
(git-gutter:linum-setup)

;; If you enable git-gutter-mode for some modes
(add-hook 'ruby-mode-hook 'git-gutter-mode)

(global-set-key (kbd "C-x C-g") 'git-gutter:toggle)
(global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)

;; Jump to next/previous hunk
(global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-x n") 'git-gutter:next-hunk)

;; Stage current hunk
(global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)

;; Revert current hunk
(global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)
```


## Customize

### Look and feel

![git-gutter-multichar](image/git-gutter-multichar.png)

You can change the signs and those faces.

```lisp
(custom-set-variables
 '(git-gutter:modified-sign "  ") ;; two space
 '(git-gutter:added-sign "++")    ;; multiple character is OK
 '(git-gutter:deleted-sign "--"))

(set-face-background 'git-gutter:modified "purple") ;; background color
(set-face-foreground 'git-gutter:added "green")
(set-face-foreground 'git-gutter:deleted "red")
```

You can change minor-mode name in mode-line to set `git-gutter:lighter`.
Default is " GitGutter"

```lisp
;; first character should be a space
(custom-set-variables
 '(git-gutter:lighter " GG"))
```


### Using full width characters

![git-gutter-fullwidth](image/git-gutter-fullwidth.png)

Emacs has `char-width` function which returns character width.
`git-gutter.el` uses it for calculating character length of the signs.
But `char-width` does not work for some full-width characters.
So you should explicitly specify window width, if you use full-width
character.

```lisp
(custom-set-variables
 '(git-gutter:window-width 2)
 '(git-gutter:modified-sign "☁")
 '(git-gutter:added-sign "☀")
 '(git-gutter:deleted-sign "☂"))
```

### Backends

`git-gutter.el` supports `git`, `mercurial`, and `bazaar` backends.
You can set backends which `git-gutter.el` will be used.
Default value of `git-gutter:handled-backends` is `'(git)`. If you want to use
`git-gutter.el` for mercurial or bazaar projects, please change value of
`git-gutter:handled-backends` as below.

```lisp
;; Use git-gutter for 'git', 'mercurial' and 'bazaar' project.
(custom-set-variables
 '(git-gutter:handled-backends '(git hg bzr)))
```

### Updates hooks

diff information is updated at hooks in `git-gutter:update-hooks`.

```lisp
(add-to-list 'git-gutter:update-hooks 'focus-in-hook)
```

### Updates commands

diff information is updated after command in `git-gutter:update-commands` executed.

```lisp
(add-to-list 'git-gutter:update-commands 'other-window)
```

### Disabled modes

If you use `global-git-gutter-mode`, you may want some modes to disable
`git-gutter-mode`. You can make it by setting `git-gutter:disabled-modes`
to `non-nil`.

```lisp
;; inactivate git-gutter-mode in asm-mode and image-mode
(custom-set-variables
 '(git-gutter:disabled-modes '(asm-mode image-mode)))
```

Default is `nil`.

### Show Unchanged Information

![git-gutter-unchanged](image/git-gutter-unchanged.png)

`git-gutter.el` can view unchanged information by setting `git-gutter:unchanged-sign`.
Like following.

```lisp
(custom-set-variables
 '(git-gutter:unchanged-sign " "))
(set-face-background 'git-gutter:unchanged "yellow")
```

Default value of `git-gutter:unchanged-sign` is `nil`.

### Show a separator column

![git-gutter-separator](image/git-gutter-separator.png)

`git-gutter.el` can display an additional separator character at the right of the changed
signs. This is mostly useful when running emacs in a console.

```lisp
(custom-set-variables
 '(git-gutter:separator-sign "|"))
(set-face-foreground 'git-gutter:separator "yellow")
```

Default value of `git-gutter:separator-sign` is `nil`.

### Hide gutter if there are no changes

Hide gutter when there are no changes if `git-gutter:hide-gutter` is non-nil.
(Default is nil)

```elisp
(custom-set-variables
 '(git-gutter:hide-gutter t))
```

### Pass option to 'git diff' command

You can pass `git diff` option to set `git-gutter:diff-option`.

```lisp
;; ignore all spaces
(custom-set-variables
 '(git-gutter:diff-option "-w"))
```

### Log/Message Level

```lisp
;; Don't need log/message.
(custom-set-variables
 '(git-gutter:verbosity 0))
```

Default value is 4(`0` is lowest, `4` is highest).

### Run hook

Run hook `git-gutter-mode-on-hook` when `git-gutter-mode` is turn on, and
run hook `git-gutter-mode-off-hook` when `git-gutter-mode` is turn off.


## See Also

### [GitGutter](https://github.com/jisaacks/GitGutter)

GitGutter is Sublime Text plugin.

### [vim-gitgutter](https://github.com/airblade/vim-gitgutter)

Vim version of GitGutter

### [diff-hl](https://github.com/dgutov/diff-hl)

`diff-hl` has more features than `git-gutter.el`.

### [git-gutter-plus](https://github.com/nonsequitur/git-gutter-plus)

Fork of `git-gutter.el`.


### Another implementation of git-gutter.el

[How to write another implementation](wiki/Write-another-git-gutter.el-implementation)

[travis-badge]: https://travis-ci.org/syohex/emacs-git-gutter.png
[travis-link]: https://travis-ci.org/syohex/emacs-git-gutter
[melpa-link]: http://melpa.org/#/git-gutter
[melpa-stable-link]: http://stable.melpa.org/#/git-gutter
[melpa-badge]: http://melpa.org/packages/git-gutter-badge.svg
[melpa-stable-badge]: http://stable.melpa.org/packages/git-gutter-badge.svg
