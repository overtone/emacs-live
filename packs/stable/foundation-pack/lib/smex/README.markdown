![Smex](https://sites.google.com/site/cumulatm/home/smex-logo.png)

## Smex

Smex is a M-x enhancement for Emacs. Built on top of Ido, it provides
a convenient interface to your recently and most frequently used
commands. And to all the other commands, too.

![Smex](https://sites.google.com/site/cumulatm/home/SmexScreenshotImage.png)

## [Changelog](#changelog-1)
Jump to [Changelog](#changelog-1).

## Get started

* Get Smex
   * Via [Marmalade](http://marmalade-repo.org/packages/smex) or
     [MELPA](http://melpa.milkbox.net/).
   * Or manually download Smex and set-up your load path.
     [(Find out more.)](http://www.emacswiki.org/emacs/InstallingPackages)

* To auto-start Smex every time you open Emacs add these lines to your .emacs file:

        (require 'smex) ; Not needed if you use package.el
        (smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                          ; when Smex is auto-initialized on its first run.

* Bind some keys:

        (global-set-key (kbd "M-x") 'smex)
        (global-set-key (kbd "M-X") 'smex-major-mode-commands)
        ;; This is your old M-x.
        (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

* Run Smex. (Type M-x, if that's your key binding).

The commands are displayed in an Ido completion buffer, ordered by
relevance.  The 7 most recently executed commands come first, the rest
are sorted by frequency of use, command length and in alphabetical
order.

Ido completion in 10 seconds: Typing selects matching commands:
e.g. 'lnmd' matches 'line-number-mode'. `C-s`/`C-r` switches to the
next/previous match. `Enter` executes the selected command.

## Learn more

### Show only major mode commands
`smex-major-mode-commands` runs Smex, limited to commands that
are relevant to the active major mode. Try it with Dired or Magit.

### Command help
`C-h f`, while Smex is active, runs `describe-function` on the
currently selected command.

`M-.` jumps to the definition of the selected command.

`C-h w` shows the key bindings for the selected command. (Via `where-is`.)

### Accessing new commands
Before accepting user input, Smex checks for new command definitions
and updates its caches accordingly.
You can disable auto-updating and gain some slight speed improvements
on older systems by setting `smex-auto-update` to nil.

To manually update Smex, call it a second time while it is already
running.

Additionally, you can teach Smex to auto-update after Emacs has
been idle for 60 seconds: Call `(smex-auto-update)`; provide an integer
argument for a custom time period in seconds.

### Show unbound commands
`smex-show-unbound-commands` shows frequently used commands that have
no key bindings.

### Persistence
Smex keeps a file to save its state betweens Emacs sessions. The
default path is "~/.smex-items"; you can change it by setting the
variable `smex-save-file`.

### History
Set `smex-history-length` to change the number of recent commands that
Smex keeps track of.

### Prompt
Set `smex-prompt-string` for a custom prompt.

## Changelog

### 3.0
  * Use `execute-extended-command` for running commands.
    This breaks support for Emacs versions older than 24 and
    obviates the user option `smex-key-advice-ignore-menu-bar`.
    (The default Emacs behaviour is to not show menu bar bindings.)

    As a result of this change, the variable `suggest-key-bindings`
    is now honored when executing commands.
  * Save command history in `extended-command-history`, like M-x.

### 2.1
  * Improved tab completion in the Smex minibuffer
  * Add compatibility with command-frequency

### 2.0
  * Remove `smex-detect-legacy-save-file`.
    Only relevant (but a breaking change) when you rely on a long deprecated default
    value of smex-save-file.
  * Ignore smex-save-file when it's empty instead of raising an error.
    Warn on invalid data in smex-save-file. Fixes [issue #23]
    (https://github.com/nonsequitur/smex/issues/23/).

### 1.1.4
  * Allow running `where-is` on the selected command.
  * Fix compatibility with ido-ubiquitous.

### 1.1.3
  * Add auto-initialization.
  * Minor fixes.

## Appendix

Smex is my first venture into Elisp. I'd be glad to receive patches,
comments and your considered criticism.

*Have fun with Smex!*
