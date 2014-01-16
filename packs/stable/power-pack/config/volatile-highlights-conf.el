;; momentarily highlight changes made by commands such as undo, yank-pop, etc.
(live-add-pack-lib "volatile-highlights")
(require 'volatile-highlights)
(volatile-highlights-mode t)
