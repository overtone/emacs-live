Transient commands
==================

Taking inspiration from prefix keys and prefix arguments, Transient
implements a similar abstraction involving a prefix command, infix
arguments and suffix commands.  We could call this abstraction a
"transient command", but because it always involves at least two
commands (a prefix and a suffix) we prefer to call it just a
"transient".

> Transient keymaps are a feature provided by Emacs.  Transients as
> implemented by this package involve the use of transient keymaps.
> 
> Emacs provides a feature that it calls "prefix commands".  When we
> talk about "prefix commands" in Transient's documentation, then we
> mean our own kind of "prefix commands", unless specified otherwise.
> To avoid ambiguity we sometimes use the terms "transient prefix
> command" for our kind and "regular prefix command" for Emacs' kind.

When the user calls a transient prefix command, then a transient
(temporary) keymap is activated, which binds the transient's infix and
suffix commands, and functions that control the transient state are
added to `pre-command-hook` and `post-command-hook`.  The available
suffix and infix commands and their state are shown in a popup buffer
until the transient is exited by invoking a suffix command.

Calling an infix command causes its value to be changed.  How that is
done depends on the type of the infix command.  The simplest case is
an infix command that represents a command-line argument that does not
take a value.  Invoking such an infix command causes the switch to be
toggled on or off.  More complex infix commands may read a value from
the user, using the minibuffer.

Calling a suffix command usually causes the transient to be exited;
the transient keymaps and hook functions are removed, the popup buffer
no longer shows information about the (no longer bound) suffix
commands, the values of some public global variables are set, while
some internal global variables are unset, and finally the command is
actually called.  Suffix commands can also be configured to not exit
the transient.

A suffix command can, but does not have to, use the infix arguments in
much the same way it can choose to use or ignore the prefix arguments.
For a suffix command that was invoked from a transient the variable
`transient-current-suffixes` and the function `transient-args` serve about
the same purpose as the variables `prefix-arg` and `current-prefix-arg` do
for any command that was called after the prefix arguments have been
set using a command such as `universal-argument`.

![screenshot](http://readme.emacsair.me/transient.png)
