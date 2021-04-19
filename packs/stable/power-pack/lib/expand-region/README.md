# expand-region.el [![Build Status](https://secure.travis-ci.org/magnars/expand-region.el.png)](http://travis-ci.org/magnars/expand-region.el) [![Coverage Status](https://coveralls.io/repos/magnars/expand-region.el/badge.svg?branch=master&service=github)](https://coveralls.io/github/magnars/expand-region.el)

Expand region increases the selected region by semantic units. Just keep
pressing the key until it selects what you want.

An example:

    (setq alphabet-start "abc def")

With the cursor at the `c`, it starts by marking the entire word `abc`, then
expand to the contents of the quotes `abc def`, then to the entire quote
`"abc def"`, then to the contents of the sexp `setq alphabet-start "abc def"`
and finally to the entire sexp.

You can set it up like this:

    (require 'expand-region)
    (global-set-key (kbd "C-=") 'er/expand-region)

If you expand too far, you can contract the region by pressing `-` (minus key),
or by prefixing the shortcut you defined with a negative argument: `C-- C-=`.

## Maintenance warning

I use this package every day, and have been doing so for years. It just works.
At least, it works for all my use cases. And if it breaks somehow, I fix it.

However, it has become painfully clear to me that I don't have time to fix
problems I don't have. It's been years since I could keep pace with the issues
and pull requests. Whenever I try, I keep getting feedback that my fix isn't
good enough by some standard I don't particularly care about.

So, I have closed the issue tracker and the pull requests. I hope you can
happily use this package, just like I do. If it doesn't work for you, then I'm
sorry. Thankfully Emacs is infinitely malleable, you can probably fix it
yourself.

TLDR: *I am still maintaining this package*, but I am no longer crowdsourcing a list of issues.

## Video

You can [watch an intro to expand-region at Emacs Rocks](http://emacsrocks.com/e09.html).

## Installation

I highly recommend installing expand-region through elpa.

It's available on [MELPA](https://melpa.org/):

    M-x package-install expand-region

## Language support

Expand region works fairly well with most languages, due to the general
nature of the basic expansions:

    er/mark-word
    er/mark-symbol
    er/mark-symbol-with-prefix
    er/mark-next-accessor
    er/mark-method-call
    er/mark-inside-quotes
    er/mark-outside-quotes
    er/mark-inside-pairs
    er/mark-outside-pairs
    er/mark-comment
    er/mark-url
    er/mark-email
    er/mark-defun

However, most languages also will benefit from some specially crafted
expansions. For instance, expand-region comes with these extra expansions for
html-mode:

    er/mark-html-attribute
    er/mark-inner-tag
    er/mark-outer-tag

You can add your own expansions to the languages of your choice simply by
creating a function that looks around point to see if it's inside or looking
at the construct you want to mark, and if so - mark it.

There's plenty of examples to look at in these files.

After you make your function, add it to a buffer-local version of
the `er/try-expand-list`.

**Example:**

Let's say you want expand-region to also mark paragraphs and pages in
text-mode. Incidentally Emacs already comes with `mark-paragraph` and
`mark-page`. To add it to the try-list, do this:

    (defun er/add-text-mode-expansions ()
      (make-variable-buffer-local 'er/try-expand-list)
      (setq er/try-expand-list (append
                                er/try-expand-list
                                '(mark-paragraph
                                  mark-page))))

    (add-hook 'text-mode-hook 'er/add-text-mode-expansions)

Add that to its own file, and add it to the `expand-region.el`-file,
where it says "Mode-specific expansions"

**Warning:** Badly written expansions might slow down expand-region
dramatically. Remember to exit quickly before you start traversing
the entire document looking for constructs to mark.

## Contribute

If you make some nice expansions for your favorite mode, it would be
great if you opened a pull-request. The repo is at:

    https://github.com/magnars/expand-region.el

All changes must be accompanied by feature tests.
They are written in [Ecukes](http://ecukes.info), a Cucumber for Emacs.

To fetch the test dependencies, install
[cask](https://github.com/rejeep/cask.el) if you haven't already,
then:

    $ cd /path/to/expand-region
    $ cask

Run the tests with:

    $ ./run-tests.sh

If feature tests are missing for the mode you are changing, please make
sure to add a set of basic tests around the functionality you're changing.

## Contributors

* [Josh Johnston](https://github.com/joshwnj) contributed `er/contract-region`
* [Le Wang](https://github.com/lewang) contributed consistent handling of the mark ring, expanding into pairs/quotes just left of the cursor, and general code clean-up.
* [Raimon Grau](https://github.com/kidd) added support for when transient-mark-mode is off.
* [Roland Walker](https://github.com/rolandwalker) added option to copy the contents of the most recent action to a register, and some fixes.
* [Damien Cassou](https://github.com/DamienCassou) added option to continue expanding/contracting with fast keys after initial expand.
* [Sylvain Rousseau](https://github.com/thisirs) fixed loads of little annoyances.
* [Ryan Mulligan](https://github.com/ryantm) cleaned up a lot of byte compilation warnings.
* [Lefteris Karapetsas](https://github.com/LefterisJP) added subword-mode expansions.

### Language specific contributions

* [Matt Briggs](https://github.com/mbriggs), [Jorge Dias](https://github.com/diasjorge) and [Le Wang](https://github.com/lewang) contributed Ruby expansions.
* [Ivan Andrus](https://github.com/gvol), [fgeller](https://github.com/fgeller), [edmccard](https://github.com/edmccard) and [Rotem Yaari](https://github.com/vmalloc) contributed Python expansions.
* [François Févotte](https://github.com/ffevotte) contributed C and C++ expansions.
* [Ivan Andrus](https://github.com/gvol) contributed text-mode, LaTeX-mode and nxml-mode expansions.
* [Gleb Peregud](https://github.com/gleber) contributed Erlang expansions.
* [Mark Hepburn](https://github.com/markhepburn) contributed Octave expansions.
* [Rotem Yaari](https://github.com/vmalloc) also contributed an adapter for the region expansion in web-mode.
* [Kang-min Liu](https://github.com/gugod) contributed Perl expansions.
* [Alexis Gallagher](https://github.com/algal) contributs Standard ML expansions.
* [Matt Price](https://github.com/titaniumbones) improved on org-mode expansions.
* [Maksim Grinman](https://github.com/maksle) added inner-quotes expansion for nxml-mode.
* [Andrea Orru](https://github.com/AndreaOrru) added `expand-region-smart-cursor`.

Thanks!

## Changelog

### From 0.11 to 0.12 (WIP)

* Option `expand-region-subword-enabled` to enable subword expansions
* Improve web-mode expansions (Renato F)
* Fixes for cc-mode expansions (Wilfred Hughes)
* Fixes for org-mode expansions (Wilfred Hughes)
* Fix unnecessary unfolding in org-mode
* Fix bug with transient-mark-mode (Russell Black)
* Fix problems with auto-loading (Philippe Vaucher, Wilfred Hughes)

### From 0.10 to 0.11

* Option `expand-region-smart-cursor` to keep cursor at beginning of region if it is there (Andrea Orru)
* Add subword-mode expansions (Lefteris Karapetsas)
* Improve enh-ruby-mode expansions (Ryan Davis)
* Improve nxml-mode expansions (Maksim Grinman)
* Improve org-mode expansions (Matt Price)
* Improve js-mode expansions
* Better performance
* Lots of bugfixes

### From 0.9 to 0.10

* Smarter expansion of ruby heredoc contents (Steve Purcell)
* Add enh-ruby-mode expansions (Bradley Wright)
* Add basic expansion er/mark-defun
* Big cleanup of byte compilation warnings (Ryan Mulligan)
* Better performance
* Lots of bugfixes

### From 0.8 to 0.9

* Improve org-, clojure-, python-, latex-, cc- and ruby-modes
* Add basic expansions: email and url
* Add sml-mode expansions (Alexis Gallagher)
* Add cperl-mode expansions (Kang-min Liu)
* Add octave-mode expansions (Mark Hepburn)
* Add web-mode expansions (Rotem Yaari)
* Use Carton for dev-dependencies
* Fix bad behavior in minibuffer (Sylvain Rousseau)
* More robust comment expansions
* Improve loading of expansions for all major modes

### From 0.7 to 0.8

* Improve js-, ruby-, python- and latex-modes
* Support built-in javascript-mode
* Handle narrowed buffers correctly
* Include mode-specific expansions when autoloading
* Provide option to copy the contents of the most recent action to a register
* Add cc-mode specific expansions
* Add customization to turn off skipping whitespace when expanding
* Continue expanding/contracting with one key press (optional)

## License

Copyright (C) 2011-2019 Magnar Sveen

Author: Magnar Sveen <magnars@gmail.com>
Keywords: marking region

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
