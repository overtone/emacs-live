<img src="http://github.com/downloads/overtone/emacs-live/emacs-live.png" />

<!--
           MM""""""""`M
           MM  mmmmmmmM
           M`      MMMM 88d8b.d8b. .d8888b. .d8888b. .d8888b.
           MM  MMMMMMMM 88''88'`88 88'  `88 88'  `"" Y8ooooo.
           MM  MMMMMMMM 88  88  88 88.  .88 88.  ...       88
           MM        .M dP  dP  dP `88888P8 '88888P' '88888P'
           MMMMMMMMMMMM

               M""MMMMMMMM M""M M""MMMMM""M MM""""""""`M
               M  MMMMMMMM M  M M  MMMMM  M MM  mmmmmmmM
               M  MMMMMMMM M  M M  MMMMP  M M`      MMMM
               M  MMMMMMMM M  M M  MMMM' .M MM  MMMMMMMM
               M  MMMMMMMM M  M M  MMP' .MM MM  MMMMMMMM
               M         M M  M M     .dMMM MM        .M
               MMMMMMMMMMM MMMM MMMMMMMMMMM MMMMMMMMMMMM
-->

    Energy starts surging through your fingertips. Sparks fly, making
    the shadows grow and flicker ominously around you. You arch your
    back and open your eyes as if for the first time. The text appears
    more vibrant and colourful than you remember. Your fingers dance on
    the keyboard commanding the cursor with a joyful precision and
    control.

    You lean back and marvel as a shimmering swirl of syntactic and
    symantic structures project out of the screen and intertwine with a
    fractal beauty. You watch as meaning recursively unfolds into deeper
    meaning live in front of your eyes. You feel a deep and lasting
    synchronicity form as the boundaries between you and your Emacs
    wash away. You and your Emacs Live.

    M-x start-hacking.

# Emacs Live

An opinionated set of defaults for getting started with a specific focus
on live coding with [Overtone](http://github.com/overtone/overtone) and
[Quil](http://github.com/quil/quil). However, it's not just a one trick
pony. It also happens to be:

* a jolly good generic Clojure hacking config
* a nice structured approach to organising your Emacs config
* modular in that functionality is organised by discrete _packs_
* a decent starting point for live coding in general
* a goldmine of config snippets to plunder and add to your own config

So, wherever you are in the multiverse, Emacs Live is ready to join you
in battle against the evil friction of poor text editor workflows.

    "Power of the horse, full force!"
                 The Space Stallions.

### Requires Emacs 24

Emacs Live is only compatible with Emacs 24 and above.

### Easy Install

If you're the kind of shoot-from-the-hip brogrammer that doesn't mind
executing random scripts from the interwebs, then the easiest way to
install Emacs Live is to run the following which will safely preserve
any Emacs configs you already have:

    bash <(curl -fsSL https://raw.github.com/overtone/emacs-live/master/installer/install-emacs-live.sh)

Note: you should always read through random scripts before executing
them!

### Getting Started

The (only ever-so-slightly) more involved way to install is to follow
these steps:

1. Move aside `~/.emacs`, `~/.emacs.el` or `~/.emacs.d` if they
   currently exist.
2. Download the zip bundle (or clone the repository with git) and move
   and rename to `~/.emacs.d`
3. Launch Emacs version 24+
4. Live code your hat off!

### Clojure Hacking

If you wish to hack with Clojure projects such as
[Overtone](http://github.com/overtone/overtone) and
[Quil](http://github.com/quil/quil) you'll need to install lein and
Swank:

1. Install [Leiningen 2](https://github.com/technomancy/leiningen/wiki/Upgrading)
2. Create a file called `~/.lein/profiles.clj` with the following contents:

    {:user {:plugins [[lein-swank "1.4.4"]]}}

And you're ready to roll. Simply start swank in a Clojure project with
`lein2 swank` and connect to it from Emacs with `M-x slime-connect` for
full Emacs REPL/autocompletion joy.

### Screenshots

<img src="https://github.com/downloads/overtone/live-coding-emacs/live-coding-config-in-use.png" alt="Screenshot 1" title="Live Coding Config Screenshot 1" />

<img src="https://github.com/downloads/overtone/live-coding-emacs/live-coding-config-in-use-2.png" alt="Screenshot 2" title="Live Coding Config Screenshot 2" />

### Video

Here's a video showing the config in use:
[Quick Intro to Live Programming with Overtone](http://vimeo.com/22798433)


## Live Packs

Emacs Live is powered by a number of special packs. Packs are
directories which are used to store isolated sets of functionality and
configuration. These may be symlinks or git submodules depending on how
you choose to store and manage your dot emacs.

### Pack Structure

Each pack consists of three components: the `init.el`, `config` dir and
`lib` dir. The `init.el` file is loaded first and it is here that you
can run arbitrary elisp. However, it is recommended that you organise
your pack by placing library code in the lib dir and individual config
files in the config dir. Emacs Live provides helper fns to make it easy
for you to load config files and for you to add lib dirs to your load
path. See the section on helper fns below.

### Shipped Packs

Emacs live ships with a few packs:

#### Foundation

A set of defaults to create a clutter free, friendly and more dynamic
Emacs foundation. Also adds fuzzy matching autocomplete functionality
for most of the Emacs minibuffer action - such as finding files, calling
functions and switching buffers.

#### Colour

Colour highlighting in two flavours - cyberpunk and gandalf. User
`color-theme-cyberpunk` and `color-theme-gandalf` to switch between the
two themes. Currently cyberpunk has seen more love - patch requests
accepted for appropriate improvements to Gandalf.

#### Clojure

A set of goodies to get you hacking Clojure like a pro.

* Clojure Mode (with fancy (λ [a] (+ a 5)) and ƒ(+ % 5) prettifications)
* Slime (for communicating with swank servers)
* Auto completion (configured to work with slime for inline auto
  completion of documentation)
* Tailor-made colour theme
* Fancy highlighting of sexps on eval
* Rainbow parens and delimiters (to allow you to easily navigate a sea
  of parens)

#### Lang

A number of extra language modes for your joy. Languages include:

* Markdown
* Yaml
* Ruby
* SuperCollider

#### Power

A boost of fantastic functionality for your live-coding
fingertips. Highlights include:

* The amazing undo-tree (live-code with confidence!)
* Textmate-like snippets
* Refheap mode for pasting snippets to refheap.com
* Quick jump mode for accessing previous locations
* Ace jump mode for jumping directly to any symbol on the screen with 2
  or three keystrokes.

### Loading Packs

By default, Emacs live will load the packs in the following order:

1. `live/foundation-pack`
2. `live/colour-pack`
3. `live/clojure-pack`
4. `live/lang-pack`
5.  `live/power-pack`

However, you may create a `~/.emacs-live.el` file to override this
behaviour. Simply set the var live-packs to a list of symbols
representing the packs you'd like to load up (the order will be
honoured). For example to only load the foundation and colour packs:

    (live-use-packs '(live/foundation-pack live/colour-pack))

If just you wish to load your own packs after the default packs then
simply use `live-add-packs`:

    (live-add-packs '(~/.live-packs/yourname-pack))

Packs are expected to reside in `~/.emacs.d/packs/` unless you specify
them with absolute paths in which case the absolute path with be
honoured.

### Creating your own Packs

Emacs Live provides a couple of useful helper fns which you can use
within your own live packs:

* `live-pack-lib-dir` this returns the path of the lib dir for the
  current pack
* `live-load-config-file` loads a config file located in the config dir
  of the current pack

It is recommended that you place your own personal packs in an external
directory. See the `user/template-pack`'s README for more information.

## Feedback

I'm very happy to hear any feedback regarding this config. The idea is
for you to use it to get started and give you a platform to start
editing it and turning it into something personal.
