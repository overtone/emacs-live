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

    The energy rushes through your fingertips. Sparks fly and you arch
    your back as your spine takes on the excess surge.

    This is the power of Emacs Live.

# Emacs Live

<img src="https://github.com/downloads/overtone/live-coding-emacs/improcess-logo-2.png" alt="Fuzzy Improcess Logo" title="Improcess" align="right" />

An opinionated set of defaults for getting started with Emacs with a specific focus on live coding with Overtone. However, it also happens to be a jolly good generic Clojure hacking config.

### Prerequisites

This dot-emacs config has only been tested with a terminal hosted Emacs ` 24.1.50.2` (pre-release). Issues and pull-requests for this and later versions will be happily accepted.

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
* **Foundation** A set of defaults to create a clutter free and friendly Emacs.
* **Colour** Colour highlighting in two flavours - cyberpunk and gandalf.
* **Clojure** A set of goodies to get you hacking Clojure like a pro.
* **Lang** A number of extra language modes for your joy.
* **Power** A boost of fantastic functionality for your live-coding fingertips.

### Loading Packs

By default, Emacs live will load the `foundation`, `colour`,
`clojure`, `lang` and `power` packs in that order. However, you may
create a `~/.emacs-live.el` file to override this behaviour. Simply set
the var live-packs to a list of symbols representing the packs you'd
like to load up (the order will be honoured). For example to only load
the foundation and colour packs:

    (setq live-packs '(live/foundation-pack live/colour-pack))

### Creating your own Packs

Emacs Live provides a couple of useful helper fns which you can use
within your own live packs:

* `live-pack-lib-dir` this returns the path of the lib dir for the current pack
* `live-load-config-file` loads a config file located in the config dir of the current pack

It is recommended that you place your own personal packs within `packs/user`.


## Feedback

I'm very happy to hear any feedback regarding this config. The idea is
for you to use it to get started and give you a platform to start
editing it and turning it into something personal.
