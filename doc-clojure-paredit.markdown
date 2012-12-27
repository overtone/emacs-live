---

title: Mastering Paredit
filename: clojure-paredit
layout: doc

---

# --> Working with paredit

Emacs Live comes bundled with paredit which keeps your parens and square brackets balanced at all times. It takes some getting used to but makes manipulating Lisp expressions (sets of parens) much quicker.

One stumbling block for me is that you won't be allowed to remove closing parens. The closing paren will instead be removed when you remove its opening match. But you can't remove that before the expression is empty. For example if you have a buffer like this (pipe represents the cursor):

    (|saw)

You can't backspace to delete the whole form. You have to first remove the saw. Thankfully paredit gives you a number of commands to move inner expressions out of the way.

The main manipulations key commands are:

`C-)` - "Slurp" the next expression into this expression

`C-(` - "Slurp" the previous expression into this expression

`C-}` - "Barf" the current expression out to the right of its parent expression

`C-{` - "Barf" the current expression out to the left of its parent expression

As a beginner you may stumble upon some ways to unbalance the parens. If this happens use C-q ) to forcibly insert a close paren. Once the parens are balanced you can go back to editing as usual.

To see all the paredit commands and get more info about how it works check out the source and look for `setq paredit-commands`
