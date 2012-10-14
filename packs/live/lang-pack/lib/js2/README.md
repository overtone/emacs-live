Description
======

An improved JavaScript mode for GNU Emacs. Forked from <http://code.google.com/p/js2-mode/>.

For some of the user-visible changes, see
[Changes from the original mode](https://github.com/mooz/js2-mode/wiki/Changes-from-the-original-mode).

Installation
======

    $ git clone git://github.com/mooz/js2-mode.git
    $ cd js2-mode
    $ emacs --batch -f batch-byte-compile js2-mode.el

Then put js2-mode.elc into your site-lisp directory.

In your emacs config:

    (autoload 'js2-mode "js2-mode" nil t)
    (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

See <http://code.google.com/p/js2-mode/wiki/InstallationInstructions> for
additional details.

Emacs 24
========

The version from the [branch `emacs24`](https://github.com/mooz/js2-mode/tree/emacs24) is recommended.

Bugs
====

If you find problems, please report them at <http://github.com/mooz/js2-mode/issues>.

See Also
======

Some third-party modes that use the generated syntax tree:

* [js2-highlight-vars-mode](http://mihai.bazon.net/projects/editing-javascript-with-emacs-js2-mode/js2-highlight-vars-mode)
* [js2-refactor](https://github.com/magnars/js2-refactor.el)
