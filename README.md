<img
src="http://github.com/downloads/overtone/emacs-live/emacs-live.png"
alt="Emacs Live Ascii Art" title = "Emacs Live" />

    Energy starts surging through your fingertips. Sparks fly, making
    the shadows grow and flicker ominously around you. You arch your
    back and open your eyes as if for the first time. The text appears
    more vibrant and colourful than you remember. Your fingers dance on
    the keyboard commanding the cursor with a joyful precision and
    control.

    You lean back and marvel as a shimmering swirl of syntactic and
    symantic structures project out of the screen and intertwine with a
    fractal beauty. You watch as meaning recursively unfolds into deeper
    meaning live in front of your eyes. Your Emacs is alive.

    M-x start-hacking.

# Emacs Live

An opinionated set of defaults for getting started with Emacs with a
specific focus on live coding with Overtone. However, it not only also
happens to be a jolly good generic Clojure hacking config but a nice
pack-oriented structure for building your own personalised config.

         "Power of the horse, full force!"
                      The Space Stallions.

### Prerequisites

Emacs live has only been tested with a terminal hosted Emacs `
24.1.50.2` (pre-release). Issues and pull-requests for this and later
versions will be happily accepted.

### Getting started

1. Download the zip bundle and move and rename the folder to `~/.emacs.d`
2. Launch Emacs
3. Live code your hat off!

### Screenshots

<img src="https://github.com/downloads/overtone/live-coding-emacs/live-coding-config-in-use.png" alt="Screenshot 1" title="Live Coding Config Screenshot 1" />

<img src="https://github.com/downloads/overtone/live-coding-emacs/live-coding-config-in-use-2.png" alt="Screenshot 2" title="Live Coding Config Screenshot 2" />

### Video

Here's a video showing the config in use: [Quick Intro to Live Programming with Overtone](http://vimeo.com/22798433)


## Live Packs

Emacs Live is powered by a number of special packs. Packs are
directories which are used to store isolated sets of functionality and
configuration. These may be symlinks or git submodules depending on
how you choose to store and manage your dot emacs.

### Pack Structure

Each pack consists of three components: the `init.el`, `config` dir
and `lib` dir. The `init.el` file is loaded first and it is here that
you can run arbitrary elisp. However, it is recommended that you
organise your pack by placing library code in the lib dir and
individual config files in the config dir. Emacs Live provides helper
fns to make it easy for you to load config files and for you to add
lib dirs to your load path. See the section on helper fns below.

### Shipped Packs


Emacs live ships with a few packs:

#### Foundation

A set of defaults to create a clutter free, friendly and more dynamic Emacs foundation. Also adds fuzzy matching autocomplete functionality for most of the Emacs minibuffer action - such as finding files, calling functions and switching buffers.

#### Colour

Colour highlighting in two flavours - cyberpunk and gandalf. User `color-theme-cyberpunk` and `color-theme-gandalf` to switch between the two themes. Currently cyberpunk has seen more love - patch requests accepted for appropriate improvements to Gandalf.

#### Clojure

A set of goodies to get you hacking Clojure like a pro.

* Clojure Mode (with fancy (λ [a] (+ a 5)) and ƒ(+ % 5) prettifications)
* Slime (for communicating with swank servers)
* Auto completion (configured to work with slime for inline auto completion of documentation)
* Tailor-made colour theme
* Fancy highlighting of sexps on eval
* Rainbow parens and delimiters (to allow you to easily navigate a sea of parens)

#### Lang

 A number of extra language modes for your joy. Languages include:

* Markdown
* Yaml
* Ruby
* SuperCollider

#### Power

A boost of fantastic functionality for your live-coding fingertips. Highlights include:

* The amazing undo-tree (live-code with confidence!)
* Textmate-like snippets
* Refheap mode for pasting snippets to refheap.com
* Quick jump mode for accessing previous locations
* Ace jump mode for jumping directly to any symbol on the screen with 2 or three keystrokes.

### Loading Packs

By default, Emacs live will load the packs in the following order:

* live
  - `foundation-pack`, `colour-pack`,`clojure-pack`, `lang-pack` and `power-pack`
* user
  - `user-pack`

However, you may create a `~/.emacs-live.el` file to override this
behaviour. Simply set the var live-packs to a list of symbols
representing the packs you'd like to load up (the order will be
honoured). For example to only load the foundation and colour packs:

    (setq live-packs '(live/foundation-pack live/colour-pack))

The user-pack is initially empty, and you're encouraged to fill it out with your own configuration.

### Creating your own Packs

Emacs Live provides a couple of useful helper fns which you can use
within your own live packs:

* `live-pack-lib-dir` this returns the path of the lib dir for the current pack
* `live-load-config-file` loads a config file located in the config dir of the current pack

It is recommended that you place your own personal packs within `packs/user`. See the user-pack's README for more information.

## Feedback

I'm very happy to hear any feedback regarding this config. The idea is
for you to use it to get started and give you a platform to start
editing it and turning it into something personal.
