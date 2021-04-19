With-Editor
===========

This library makes it possible to reliably use the Emacsclient as
the `$EDITOR` of child processes.  It makes sure that they know how
to call home.  For remote processes a substitute is provided, which
communicates with Emacs on standard output/input instead of using a
socket as the Emacsclient does.

It provides the commands `with-editor-async-shell-command` and
`with-editor-shell-command`, which are intended as replacements
for `async-shell-command` and `shell-command`.  They automatically
export `$EDITOR` making sure the executed command uses the current
Emacs instance as "the editor".  With a prefix argument these
commands prompt for an alternative environment variable such as
`$GIT_EDITOR`.  To always use these variants add this to your init
file:

    (define-key (current-global-map)
      [remap async-shell-command] 'with-editor-async-shell-command)
    (define-key (current-global-map)
      [remap shell-command] 'with-editor-shell-command)

Alternatively use the global `shell-command-with-editor-mode`,
which always sets `$EDITOR` for all Emacs commands which ultimately
use `shell-command` to asynchronously run some shell command.

The command `with-editor-export-editor` exports `$EDITOR` or
another such environment variable in `shell-mode`, `term-mode` and
`eshell-mode` buffers.  Use this Emacs command before executing a
shell command which needs the editor set, or always arrange for the
current Emacs instance to be used as editor by adding it to the
appropriate mode hooks:

    (add-hook 'shell-mode-hook  'with-editor-export-editor)
    (add-hook 'term-exec-hook   'with-editor-export-editor)
    (add-hook 'eshell-mode-hook 'with-editor-export-editor)

Some variants of this function exist, these two forms are
equivalent:

    (add-hook 'shell-mode-hook
              (apply-partially 'with-editor-export-editor "GIT_EDITOR"))
    (add-hook 'shell-mode-hook 'with-editor-export-git-editor)

This library can also be used by other packages which need to use
the current Emacs instance as editor.  In fact this library was
written for Magit and its `git-commit-mode` and `git-rebase-mode`.
Consult `git-rebase.el` and the related code in `magit-sequence.el`
for a simple example.
