# git-gutter.el

## Introduction
`git-gutter.el` is port of [GitGutter](https://github.com/jisaacks/GitGutter)
which is a plugin of Sublime Text2.


`git-gutter.el` does not work well with `linum-mode`.
Please see [git-gutter-fringe](https://github.com/syohex/emacs-git-gutter-fringe)
which can work with `linum-mode`, if you use `linum-mode`.


## Screenshot

![git-gutter.el](image/git-gutter1.png)


## Requirements

* Emacs 23 or higher

## Installation

You can install `git-gutter.el` from [MELPA](https://github.com/milkypostman/melpa.git) with package.el
(`M-x package-install git-gutter`).

And you can also install it with [el-get](https://github.com/dimitri/el-get).


## Global Minor Mode and Minor Mode

`git-gutter.el` provides global minor-mode(`global-git-gutter-mode`) and minor-mode(`git-gutter-mode`).

If you want to use `git-gutter` for files in git repository.
You add following s-exp in your configuration file(`~/.emacs.d/init.el` or `~/.emacs`).

````elisp
(global-git-gutter-mode t)
````

Other case, you want to use `git-gutter` for some files, you can use `git-gutter-mode`.
Following example of enabling `git-gutter` for some mode.

````elisp
(add-hook 'ruby-mode-hook 'git-gutter-mode)
(add-hook 'python-mode-hook 'git-gutter-mode)
````


## Basic Usage

`git-gutter.el` provides following commands.

Show changes from last commit or Update change information.

    M-x git-gutter

Clear changes

    M-x git-gutter:clear

Toggle git-gutter

    M-x git-gutter:toggle

 Jump to next diff

    M-x git-gutter:next-diff

 Jump to previous diff

    M-x git-gutter:previous-diff

 Popup diff of current position

    M-x git-gutter:popup-diff


## Sample Configuration

````elisp
(require 'git-gutter)

;; If you enable global minor mode
(global-git-gutter-mode t)

;; If you enable git-gutter-mode for some modes
(add-hook 'ruby-mode-hook 'git-gutter-mode)

(global-set-key (kbd "C-x C-g") 'git-gutter:toggle)
(global-set-key (kbd "C-x v =") 'git-gutter:popup-diff)

;; Jump to next/previous diff
(global-set-key (kbd "C-x p") 'git-gutter:previous-diff)
(global-set-key (kbd "C-x n") 'git-gutter:next-diff)
````


## Customize

### Look and feel

![git-gutter-multichar](image/git-gutter-multichar.png)

You can change the signs and those faces.

````elisp
(setq git-gutter:modified-sign "  ") ;; two space
(setq git-gutter:added-sign "++")    ;; multiple character is OK
(setq git-gutter:deleted-sign "--")

(set-face-background 'git-gutter:modified "purple") ;; background color
(set-face-foreground 'git-gutter:added "green")
(set-face-foreground 'git-gutter:deleted "red")
````

You can change minor-mode name in mode-line to set `git-gutter:lighter`.
Default is " GitGutter"

````elisp
;; first character should be a space
(setq git-gutter:lighter " GG")
````


### Using full width characters

![git-gutter-fullwidth](image/git-gutter-fullwidth.png)

Emacs has `char-width` function which returns character width.
`git-gutter.el` uses it for calculating character length of the signs.
But `char-width` does not work for some full-width characters.
So you should explicitly specify window width, if you use full-width
character.

```` elisp
(setq git-gutter:window-width 2)
(setq git-gutter:modified-sign "☁")
(setq git-gutter:added-sign "☀")
(setq git-gutter:deleted-sign "☂")
````

### Show Unchanged Information

![git-gutter-unchanged](image/git-gutter-unchanged.png)

`git-gutter.el` can view unchanged information by setting `git-gutter:unchanged-sign`.
Like following.

````elisp
(setq git-gutter:unchanged-sign " ")
(set-face-background 'git-gutter:unchanged "yellow")
````

Default value of `git-gutter:unchanged-sign` is `nil`.

### Always Show Gutter

Always show gutter if `git-gutter:always-show-gutter` is non-nil. (Default is `nil`)

````elisp
(setq git-gutter:always-show-gutter t)
````


### Pass option to 'git diff' command

You can pass `git diff` option to set `git-gutter:diff-option`.

````elisp
;; ignore all spaces
(setq git-gutter:diff-option "-w")
````


## See Also

### [GitGutter](https://github.com/jisaacks/GitGutter)

GitGutter is Sublime text2 plugin.

### [diff-hl](https://github.com/dgutov/diff-hl)

`diff-hl` has more features than `git-gutter.el`.

### [vim-gitgutter](https://github.com/airblade/vim-gitgutter)

Vim version of GitGutter

### Another implementation of git-gutter.el

[How to write another implementation](wiki/Write-another-git-gutter.el-implementation)
