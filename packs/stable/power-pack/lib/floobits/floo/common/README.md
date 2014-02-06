plugin-common-python
====================

This repository contains python code shared between [Floobits](https://floobits.com/) plugins for [Sublime Text](https://github.com/Floobits/floobits-sublime), [Emacs](https://github.com/Floobits/floobits-emacs), and [Vim](https://github.com/Floobits/floobits-vim).

(For Vim, use `--prefix plugin/floo/common`)

To get all the common changes:

    git subtree pull --prefix floo/common git@github.com:Floobits/plugin-common-python master --squash

To push to common:

    git subtree push --prefix floo/common git@github.com:Floobits/plugin-common-python master --squash
