---

title: First Steps
filename: first-steps
layout: doc

---

# --> First Steps

If you're new to Emacs, it's worth taking the time to read the tutorial. You can open it from inside Emacs Live using `M-h t` (should be `Alt+h` followed by `t`). This will explain most of the basic operations and navigation.

Here's a quick list of useful and basic combinations, most are covered by the tutorial but if you're familiar with another editor this may provide you with an early productivity boost:

`C-x C-f` - Open a file

`C-x b` - Switch to an already open file

`C-x C-s` - Save

`C-_` - Undo

`C-x u` - Visual Undo!

`C-x 2` - Horizontal split

`C-x 3` - Vertical split

`C-x o` - Switch to the next split pane

`C-x 0` - Close current split pane

`C-x 1` - Close all other split panes and leave the current

`C-g` - Cancel a command or dialog

`C-x C-c` - Quit Emacs

The tutorial covers all these and more, so give it a read through and get hacking!

# --> Working with paredit

Emacs Live comes bundled with paredit which keeps your parens and square brackets balanced at all times. It takes some getting used to but makes manipulating Lisp expressions (sets of parens) much quicker.

One stumbling block for me is that you won't be allowed to remove closing parens. The closing paren will instead be removed when you remove it's opening match. But you can't remove that before the expression is empty. For example if you have a buffer like this (pipe represents the cursor):

    (|saw)

You can't backspace to delete the whole form. You have to first remove the saw. Thankfully paredit gives you a number of commands to move inner expressions out of the way.

The main manipulations key commands are:

`C-)` - "Slurp" the next expression into this expression

`C-(` - "Slurp" the previous expression into this expression

`C-}` - "Barf" the current expression out to the right of it's parent expression

`C-{` - "Barf" the current expression out to the left of it's parent expression

As a beginner you may find stumble upon some ways to unbalance the parens. If this happens use C-q ) to force insert a close paren. Once the parens are balanced you can go back to editing as usual.

To see all the paredit commands and get more info about how it works check out the source and look for `setq paredit-commands`
