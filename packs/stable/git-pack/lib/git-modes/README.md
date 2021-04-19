Git Config Modes
================

Emacs major modes for various Git configuration files.

The list of contributors can be found
[here](https://github.com/magit/git-modes/graphs/contributors).

`gitattributes-mode`
--------------------

Auto-/loading the library `gitattributes-mode` enabled the mode for
`.gitattributes`, `.git/info/attributes`, and `git/attributes` files.

`gitconfig-mode`
----------------

Auto-/loading the library `gitconfig-mode` enables the mode for
`.gitconfig`, `.git/config`, `git/config`, and `.gitmodules` files.

`gitconfig-mode` derives from `conf-unix-mode`.

`gitignore-mode`
----------------

Auto-/loading the library `gitignore-mode` enables the mode for
`.gitignore`, `.git/info/exclude`, and `git/ignore` files.

`gitignore-mode` derives from `conf-unix-mode`.

This mode may be of use in other files that don't have anything to do
with Git, for example:

```lisp
(add-to-list 'auto-mode-alist
             (cons "/.dockerignore\\'" 'gitignore-mode))
```
