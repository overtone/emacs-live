---

title: Installation
layout: doc

---

# --> Installation

Emacs Live is a bespoke configuration for Emacs - as such it consists of
two independent parts. Firstly, there is the vanilla Emacs application
and secondly there is the config itself.

# --> Vanilla Emacs

To use Emacs Live you need to have a copy of vanilla Emacs version 24 or
higher. Depending on your operating system, this can be obtained from
different sources.

* **OS X GUI:** [http://emacsformacosx.com/](http://emacsformacosx.com/)
* **OS X Console:** via [homebrew](http://mxcl.github.com/homebrew/) - `brew install emacs`
* **Windows:** [http://alpha.gnu.org/gnu/emacs/windows/](http://alpha.gnu.org/gnu/emacs/windows/)
* **Linux:** consult your _package_ manager or compile from source

# --> Basic Installation

As Emacs Live is a completely stand-alone configuration `~/.emacs.d`
folder with no external dependencies, installation is therefore simply a
matter of downloading the
[latest release](https://github.com/overtone/emacs-live/tags),
uncompressing it and then moving the uncompressed folder to
`~/.emacs.d`.

# --> Advanced Installation

It is recommended that you use [git](http://git-scm.com) to download and install Emacs Live. This makes it trivial for you to keep up-to-date with new versions:

    git clone git://github.com/overtone/emacs-live.git ~/.emacs.d

# --> Brogrammer Style

If you're using either Linux or OS X and are the kind of
shoot-from-the-hip brogrammer that doesn't mind executing random scripts
from the interwebs, then the easiest way to install Emacs Live is to run
the following which will safely preserve any Emacs configs you already
have:

    bash <(curl -fksSL https://raw.github.com/overtone/emacs-live/master/installer/install-emacs-live.sh)

This will also set you up for your own personal customisations which we
cover in [another section](doc-customisation.html).
