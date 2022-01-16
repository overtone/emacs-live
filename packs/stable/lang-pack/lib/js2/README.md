About [![Build Status](https://github.com/mooz/js2-mode/actions/workflows/test.yml/badge.svg)](https://github.com/mooz/js2-mode/actions/workflows/test.yml) [![GNU ELPA](https://elpa.gnu.org/packages/js2-mode.svg)](https://elpa.gnu.org/packages/js2-mode.html) [![MELPA](https://melpa.org/packages/js2-mode-badge.svg)](https://melpa.org/#/js2-mode)
======

Improved JavaScript editing mode for GNU Emacs ([description here](http://elpa.gnu.org/packages/js2-mode.html)).

For some of the latest changes, see [latest user-visible changes](https://github.com/mooz/js2-mode/blob/master/NEWS.md).

Installation
======

The stable versions are hosted at [GNU ELPA](http://elpa.gnu.org/)
(<kbd>M-x list-packages</kbd>).

You can also install the latest development version from
[MELPA](https://melpa.org/#/getting-started).

Requirements
======

Emacs 24.1+ and `cl-lib` (either built-in or installed from GNU ELPA).

React and JSX
======

The currently recommended solution is to install Emacs 27 (you can [build from
source](http://git.savannah.gnu.org/cgit/emacs.git/tree/INSTALL.REPO)
or e.g. install a snapshot from a
[PPA](https://launchpad.net/~ubuntu-elisp/+archive/ubuntu/ppa)) and
use `js-mode` as the major mode. To make use of the JS2 AST and the
packages that integrate with it, we recommend `js2-minor-mode`. See
the corresponding [instructions in the
Commentary](https://github.com/mooz/js2-mode/blob/bb73461c2c7048d811b38e6b533a30fb5fdcea93/js2-mode.el#L57).

`js-mode` in Emacs 27 includes full support for syntax highlighting
and indenting of JSX syntax.

[rjsx-mode](https://github.com/felipeochoa/rjsx-mode/) is an
alternative option which comes with certain tradeoffs.

Bugs
====

* See broken syntax highlighting and timer errors? Recently upgraded
Emacs from version 24.2 or earlier? Try
[reinstalling or byte-recompiling](https://github.com/mooz/js2-mode/issues/72)
the package.

* Any indentation problems should be reported with `M-x report-emacs-bug`
(please try reproducing them with `js-mode` first, for clarity).
Starting with Emacs 25, `js2-mode` delegates indentation to
the indentation engine of `js-mode`.

Please report other problems at <http://github.com/mooz/js2-mode/issues>.

Contributing
======

`js2-mode` is subject to the same
[copyright assignment](http://www.gnu.org/prep/maintain/html_node/Copyright-Papers.html)
policy as Emacs itself, `org-mode`, `CEDET` and other packages in
[GNU ELPA](http://elpa.gnu.org/packages/).

Any
[legally significant](http://www.gnu.org/prep/maintain/html_node/Legally-Significant.html#Legally-Significant)
contributions can only be accepted after the author has completed their
paperwork. Please ask for the request form, and we'll send it to you.

See Also
======

Some third-party modes that use the generated syntax tree:

* [js2-refactor](https://github.com/magnars/js2-refactor.el)
* [skewer-mode](https://github.com/skeeto/skewer-mode)
