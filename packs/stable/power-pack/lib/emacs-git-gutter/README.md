# git-gutter.el

## Introduction
`git-gutter.el` is port of [GitGutter](https://github.com/jisaacks/GitGutter)
which is a plugin of Sublime Text.


`git-gutter.el` also supports TRAMP so you can use `git-gutter.el` for remote files.


## Screenshot

![git-gutter.el](image/git-gutter1.png)


## Requirements

* Emacs 23 or higher
* [Git](http://git-scm.com/) 1.7.0 or higher


## git-gutter.el vs [git-gutter-fringe.el](https://github.com/syohex/emacs-git-gutter-fringe)

|                      | git-gutter.el | git-gutter-fringe.el |
|:---------------------|:-------------:|:--------------------:|
| Work in tty frame    | OK            | NG                   |
| Work with linum-mode | NG            | OK                   |
| Show on right side   | NG            | OK                   |
| More configurable    | OK            | NG                   |


## Installation

You can install `git-gutter.el` from [MELPA](https://github.com/milkypostman/melpa.git) with package.el
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

#### `git-gutter:popup-hunk`

Popup current diff hunk(alias `git-gutter:popup-diff`)

`git-gutter:next-hunk` and `git-gutter:previous-hunk` update content
of buffer popuped by `git-gutter:popup-diff` to current hunk.

#### `git-gutter:stage-hunk`

Stage current hunk. You can use this command like `git add -p`.

#### `git-gutter:revert-hunk`

Revert current hunk

#### `git-gutter`

Show changes from last commit or Update change information.

#### `git-gutter:clear`

Clear changes

#### `git-gutter:toggle`

Toggle git-gutter


## Sample Configuration

```lisp
(require 'git-gutter)

;; If you enable global minor mode
(global-git-gutter-mode t)

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
(setq git-gutter:modified-sign "  ") ;; two space
(setq git-gutter:added-sign "++")    ;; multiple character is OK
(setq git-gutter:deleted-sign "--")

(set-face-background 'git-gutter:modified "purple") ;; background color
(set-face-foreground 'git-gutter:added "green")
(set-face-foreground 'git-gutter:deleted "red")
```

You can change minor-mode name in mode-line to set `git-gutter:lighter`.
Default is " GitGutter"

```lisp
;; first character should be a space
(setq git-gutter:lighter " GG")
```


### Using full width characters

![git-gutter-fullwidth](image/git-gutter-fullwidth.png)

Emacs has `char-width` function which returns character width.
`git-gutter.el` uses it for calculating character length of the signs.
But `char-width` does not work for some full-width characters.
So you should explicitly specify window width, if you use full-width
character.

```lisp
(setq git-gutter:window-width 2)
(setq git-gutter:modified-sign "☁")
(setq git-gutter:added-sign "☀")
(setq git-gutter:deleted-sign "☂")
```


### For MacOSX Users

`git-gutter.el` with GUI Emacs on MacOSX makes display strange.
This issues is only GUI mode.(If you have use CUI mode(`emacs -nw`),
it may be no problems)

You can avoid this issue by adding additional space to each diff signs like below.

```lisp
(setq git-gutter:added-sign "+ ")
(setq git-gutter:deleted-sign "- ")
(setq git-gutter:modified-sign "= ")
```


### If You Feel git-gutter is Slow

`git-gutter.el` updates gutter at hooks below

* `after-save-hook`
* `after-revert-hook`
* `window-configuration-change-hook`

`window-configuration-change-hook` is run at buffer updated, window construction
changed etc. But it's hook is called a lot of times and it makes Emacs slow.
Additionally it is difficult to control(Update function is called (2 * N)
times if you show N windows in frame).

So `git-gutter.el` does not call update function within `git-gutter:update-threshold`
second(Default is 1 second) at `window-configuration-change-hook`. Please set longer
time if you feel slow yet. Update function is always called if `git-gutter:update-threshold`
is nil.

If you even feel Emacs slow, please remove `window-configuration-change-hook` to
update points as below and instead you update `M-x git-gutter` manually when needed.

```lisp
(setq git-gutter:update-threshold 2)

(setq git-gutter:update-hooks '(after-save-hook after-revert-hook))
```

You can also add other hook points by setting `git-gutter:update-hooks`.

### Disabled modes

If you use `global-git-gutter-mode`, you may want some modes to disable
`git-gutter-mode`. You can make it by setting `git-gutter:disabled-modes`
to `non-nil`.

```lisp
;; inactivate git-gutter-mode in asm-mode and image-mode
(setq git-gutter:disabled-modes '(asm-mode image-mode))
```

Default is `nil`.

### Show Unchanged Information

![git-gutter-unchanged](image/git-gutter-unchanged.png)

`git-gutter.el` can view unchanged information by setting `git-gutter:unchanged-sign`.
Like following.

```lisp
(setq git-gutter:unchanged-sign " ")
(set-face-background 'git-gutter:unchanged "yellow")
```

Default value of `git-gutter:unchanged-sign` is `nil`.

### Show a separator column

![git-gutter-separator](image/git-gutter-separator.png)

`git-gutter.el` can display an additional separator character at the right of the changed
signs. This is mostly useful when running emacs in a console.

```lisp
(setq git-gutter:separator-sign "|")
(set-face-foreground 'git-gutter:separator "yellow")
```

Default value of `git-gutter:separator-sign` is `nil`.

### Hide gutter if there are no changes

Hide gutter when there are no changes if `git-gutter:hide-gutter` is non-nil.
(Default is nil)

```elisp
(setq git-gutter:hide-gutter t)
```

### Pass option to 'git diff' command

You can pass `git diff` option to set `git-gutter:diff-option`.

```lisp
;; ignore all spaces
(setq git-gutter:diff-option "-w")
```

### Log/Message Level

```lisp
;; Don't need log/message.
(setq git-gutter:verbosity 0)
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
