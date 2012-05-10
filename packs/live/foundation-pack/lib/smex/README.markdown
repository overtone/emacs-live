![Smex](http://sites.google.com/site/cumulatm/home/smex-logo.png)

## Smex

Smex is a M-x enhancement for Emacs. Built on top of Ido, it provides
a convenient interface to your recently and most frequently used
commands. And to all the other commands, too.

![Smex](http://sites.google.com/site/cumulatm/home/SmexScreenshotImage.png)

## Get started

* Get Smex
   * Via [package.el](http://marmalade-repo.org/packages/smex).
   * Or manually download Smex and set-up your load path. [(Find out more.)](http://www.emacswiki.org/emacs/InstallingPackages)

* To auto-start Smex every time you open Emacs add these lines to your .emacs file:

        (require 'smex)
        (smex-initialize)
        
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

### Ignore menu bar bindings
Enable `smex-key-advice-ignore-menu-bar` to ignore clues to menu bar bindings.

## Appendix

Smex is my first venture into Elisp. I'd be glad to receive patches,
comments and your considered criticism.

*Have fun with Smex!*
