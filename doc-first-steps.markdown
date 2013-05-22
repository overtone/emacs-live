---

title: First Steps
filename: first-steps
layout: doc

---

# --> First Steps

If you're new to Emacs, it's worth taking the time to read the tutorial. You can open it from inside Emacs Live using `M-h t` (should be `Alt+h` followed by `t`). This will explain most of the basic operations and navigation.

Here's a quick list of useful and basic combinations, most are covered by the tutorial but if you're familiar with another editor this may provide you with an early productivity boost:

`M-x` **smex** - Run a command by name (e.g., `M-x emacs-uptime`)

`C-g` **keyboard-quit** - Cancel a command or dialog

`M-h ?` **help-for-help** - Help menu

`C-x C-f` **ido-find-file** - Open a file

`C-x b`**ido-switch-buffer**  - Switch to an already open file

`C-b` **ibuffer** - Display open buffers

`C-x C-s` **save-buffer* - Save

`C-_` **undo-tree-undo* - Undo

`C-x u` **undo-tree-visualize** - Visual Undo!

`C-x 2` **split-window-below** - Horizontal split

`C-x 3` **split-window-right** - Vertical split

`C-x o` **win-switch-dispatch** - Switch to the next split pane

`C-x 0` **delete-window** - Close current split pane

`C-x 1` **delete-other-windows** - Close all other split panes and leave the current

`C-x C-c` **save-buffers-kill-terminal** - Quit Emacs

The tutorial covers all these and more, so give it a read through and get hacking!

# --> Go spelunking

There's a lot to love in Emacs Live. One interesting place to start exploring is in the [default key bindings](https://github.com/overtone/emacs-live/blob/master/packs/live/bindings-pack/config/default-bindings.el) in `packs/live/bindings-pack/config/default-bindings.el`. Have a look around and you may find something interesting.

# --> And on to Clojure

Once you've got a handle on Emacs basics, head over to the [Clojure Hacking](doc-clojure.html) documentation to learn more about using Emacs Live to work on Clojure code!
