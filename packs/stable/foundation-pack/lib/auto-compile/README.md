Automatically compile Emacs Lisp libraries
------------------------------------------

This package provides two minor modes which automatically recompile
Emacs Lisp source files.  Together these modes guarantee that Emacs
never loads outdated byte code files.

`auto-compile-on-save-mode` re-compiles source files when they are
being saved and `auto-compile-on-load-mode` does so before they are
being loaded (by advising `load` and `require`).  Both modes only
ever _re-compile_ a source file when the respective byte code file
already exists but is outdated.  Otherwise they do _not_ compile
the source file.

Even when using `auto-compile-on-save-mode` it can happen that some
source file is newer than the respective byte code file, which is a
problem because by default Emacs load the byte code file even when
the respective source file has been modified more recently.

Starting with Emacs version 24.4, setting `load-prefer-newer` to t
prevents outdated byte code files from being loaded.  However this
does not cause re-compilation of the source file, to actually do
that `auto-compile-on-load-mode` is still required.

Setup
-----

To reduce the risk of loading outdated byte code files, you should set
`load-prefer-newer` and enable `auto-compile-on-load-mode` as early as
possible.  Then also enable `auto-compile-on-save-mode`.  You should
also consider not byte-compiling your personal init file, or setting
`load-prefer-newer` in a system-wide init file.

If you use `package.el` then use something like this:

    ;;; init.el --- user init file      -*- no-byte-compile: t -*-
    (setq load-prefer-newer t)
    (package-initialize)
    (require 'auto-compile)
    (auto-compile-on-load-mode)
    (auto-compile-on-save-mode)

otherwise:

    ;;; init.el --- user init file      -*- no-byte-compile: t -*-
    (setq load-prefer-newer t)
    (add-to-list 'load-path "/path/to/dash")
    (add-to-list 'load-path "/path/to/packed")
    (add-to-list 'load-path "/path/to/auto-compile")
    (require 'auto-compile)
    (auto-compile-on-load-mode)
    (auto-compile-on-save-mode)

Usage
-----

Take note of the compile warnings and fix them.

To permanently or temporarily toggle automatic compilation of some
source file use the command `toggle-auto-compile`.  Since the modes
only ever _update_ byte code files, toggling automatic compilation
is done simply by either creating the byte code file or by removing
it.  `toggle-auto-compile` can also toggle automatic compilation of
multiple files at once; see its doc-string for more information.

Customization
-------------

Constantly having the *Compile-Log* buffer pop up when a file is
being saved can quickly become annoying.  Obviously the first thing
you should do to about that is to actually fix outstanding issues.

Once you have done that you might also want to keep that buffer
from being automatically displayed and instead only show the number
of compile warnings for the current file in the mode-line.

    (setq auto-compile-display-buffer nil)
    (setq auto-compile-mode-line-counter t)

To display the buffer use `M-x auto-compile-display-log` or click
on the counter in the mode-line.

Using `auto-compile-inhibit-compile-hook` it is possible to inhibit
automatic compilation under certain circumstances; e.g. when HEAD
is detached inside a Git repository (useful during rebase sessions).

