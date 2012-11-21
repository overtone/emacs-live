v0.6
====

New Features
------------

- GNU Emacs trunk (24.3) support
- Restore `window-start`

New Commands
------------

- `popwin:switch-to-last-buffer`
- `popwin:popup-last-buffer`
- `popwin:original-display-last-buffer`
- `popwin:original-pop-to-last-buffer`

New Variables
-------------

- `popwin:reuse-window`

Contributors
------------

- ARISAWA Yuuki added a lot of test cases (test/popwin-test.el)

v0.5.1
======

Fixed Bugs
----------

- Restore `window-point` more correctly

v0.5
====

New Commands
------------

- `popwin:onw-window`: Temporarily enlarge a popup window

New Keywords
------------

- `tail`: Display a popup window with showing the buffer tail contents

Fixed Bugs
----------

- Restore `window-point` more correctly

Contributors
------------

- Yuuki Arisawa: Amazingly improve automatic regression tests

v0.4
====

New Features
------------

- GNU Emacs 24 support
- Dedicated popup windows
- Stacked popup windows
- Universal displaying

New Commands
------------

- `popwin:pop-to-buffer`

New Variables
-------------

- `popwin:before-popup-hook`
- `popwin:after-popup-book`

Fixed Bugs
----------

- Keep minibuffer window selected when closing popup windows
- Restore `window-point` when closing popup windows
- Fixed corrupted popup windows in some situations

Compatibilities
---------------

- Added `misc/popwin-pp.el`
- Added `misc/popwin-term.el`
- Added `misc/popwin-browse-kill-ring.el`
- Deprecated `popwin:special-display-buffer`

Contributors
------------

- IRIE Shinsuke
- rubikitch
