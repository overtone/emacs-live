---

title: First Steps
filename: first-steps
layout: doc

---

# --> First Steps

If you're new to Emacs, it's worth taking the time to read the
tutorial. You can open it from inside Emacs Live using `M-h t` (should
be `Alt+h` followed by `t`). This will explain most of the basic
operations and navigation.

Here's a quick list of useful and basic combinations, most are covered
by the tutorial but if you're familiar with another editor this may
provide you with an early productivity boost:

## Keys

* `C` **Control** - control key
* `M` **Meta** - cmd or alt key depending on your setup
* `S` **Shift** - shift key
* `ESC` **Escape** - escape key
* `SPC` **Space** - space bar

## Understanding Shortcuts

`key-shortcut` **name-of-function** - Description of function

`C-M-p` **a-key-chord** - Hold down Control and Meta and p

`C-M-f g` **a-key-sequence** - Hold down Control and Meta and f, release all keys, then press g

## General

`M-x` **smex** - Run a command by name (e.g., `M-x emacs-uptime`)

`C-g` **keyboard-quit** - Cancel a command or dialog

## Help

`M-h ?` **help-for-help** - Help menu

`M-h t` **help-with-tutorial** - Emacs tutorial

`M-h k` **describe-key** - Describe which function a specific shortcut will evoke

`M-h f` **describe-function** - Show docstring for a given function

## Files

`C-x C-f` **ido-find-file** - Open a file

## Buffers

`C-x b`**ido-switch-buffer**  - Switch to an already open file

`C-x C-b` **ibuffer** - Switch buffers using buffer listing. Navigate to line representing buffer and press `RET`

`C-x C-s` **save-buffer** - Save

## Organise windows

`C-x 2` **split-window-below** - Horizontal split

`C-x 3` **split-window-right** - Vertical split

`C-x o` **win-switch-dispatch** - Switch to the next split pane. Pressing `o` again in quick succession will allow you to keep cycling through panes.

`C-x 0` **delete-window** - Close current split pane

`C-x 1` **delete-other-windows** - Close all other split panes and leave the current

`C-c b` **winner-undo** - Undo last window modification (i.e. splitting planes or closing windows)

`C-c f` **winner-redo** - Redo last window modification (i.e. splitting planes or closing windows)

`C-l` **recenter-top-bottom** - Center current window around point (cursor)

## Navigation

`C-n` **next-line** - Move down one line.

`C-p` **previous-line** - Move up one line

`C-f` **forward-char** - Move forward one character

`C-b` **backward-char** - Move backward one character

`C-a` **move-beginning-of-line** - Move to start of line

`C-e` **move-end-of-line** - Move to end of line

`M-f` **forward-word** - Move forward one word

`M-b` **backward-word** - Move backward one word

`ESC->` **end-of-buffer** - Move to the end of the buffer

`ESC-<` **beginning-of-buffer** - Move to the beginning of the buffer

## Search Navigation

`C-o` **ace-jump-mode** - Quickly jump to any visible word. Type the first character of the word you want to visit, the first char of the word you wish to jump to will now change to another, type that and you'll jump directly there.

`C-s` **isearch-forward** - Search forward for matching word. Subsequent `s` presses (whilst continuing to  hold down `C`) will skip through all matches. Whilst in search mode, `C-w` will extend the search term based on the rest of the currently matched word.

`C-r` **isearch-backward** - Search backward for matching word. Subsequent `r` presses (whilst continuing to  hold down `C`) will skip through all matches. Whilst in search mode, `C-w` will extend the search term based on the rest of the currently matched word.


## Editing

`C-h` **delete-backward-char** - Delete the previous character

`C-_` **undo-tree-undo** - Undo

`C-x u` **undo-tree-visualize** - Visual Undo! Use `p` and `n` to navigate up and down history. `f` and `b` let you select which branch to navigate and `q` chooses a particular point in history to work from.

`C-k` **kill-line** - Cut to end of line - placing line in the kill ring (paste buffer)

`C-y` **cua-paste**  - paste/yank most recent thing in the kill ring

`M-y` **browse-kill-ring** - Visual paste/yank

`C-SPC` **cua-set-mark** - Start highlighting region. Navigate the point (cursor) around to select a region.

`M-]` **kill-ring-save** - Copy region

`C-]` **kill-region** - Cut region

`M-/` **comment-or-uncomment-region** - Comment or uncomment region

`M-SPC` **live-delete-whitespace-except-one** - Delete all whitespace between last and next words except for one space

`M-\` **delete-horizontal-space** - Delete all whitespace between last and next words

`C-t` **transpose-chars** - Swap adjacent characters

`M-u` **upcase-word** - Make following word ALL-CAPS

`M-c` **capitalize-word** - Make the followign word Capitalised

## Quitting
`C-x C-c` **save-buffers-kill-terminal** - Quit Emacs




The tutorial covers all these and more, so give it a read through and get hacking!

# --> Go spelunking

There's a lot to love in Emacs Live. One interesting place to start exploring is in the [default key bindings](https://github.com/overtone/emacs-live/blob/master/packs/live/bindings-pack/config/default-bindings.el) in `packs/live/bindings-pack/config/default-bindings.el`. Have a look around and you may find something interesting.

# --> And on to Clojure

Once you've got a handle on Emacs basics, head over to the [Clojure Hacking](doc-clojure.html) documentation to learn more about using Emacs Live to work on Clojure code!
