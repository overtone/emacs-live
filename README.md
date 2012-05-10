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

Emacs Live
==========

The energy rushes through your fingertips, sparks fly and you
stretch your back as your spine takes on the excess surge. This is
the power of Emacs Live - a modular pack-oriented config structure
for Emacs.

Packs
-----

Packs are directories which are used to store isolated sets of
functionality and configuration. These may be symlinks or git
submodules depending on how you choose to store and manage your dot
emacs.

Pack Structure
--------------

Each pack consists of three components: the init.el, config dir and
lib dir. The init.el file is loaded first and it is here that you
can run arbitrary elisp. However, it is recommended that you
organise your pack by placing library code in the lib dir and
individual config files in the config dir. Emacs Live provides
helper fns to make it easy for you to load config files and for you
to add lib dirs to your load path. See the section on helper fns
below.

Shipped Packs
-------------

Emacs live ships with a few packs:
* **Foundation** A set of defaults to create a clutter free and friendly Emacs.
* **Colour** Colour highlighting in two flavours - blackbeard and whitebeard.
* **Clojure** A set of goodies to get you hacking Clojure like a pro.
* **Lang** A number of extra language modes for your joy.
* **Power** A boost of fantastic functionality for your live-coding fingertips.
* **User** An empty place for your own libs and config.

Loading Packs
-------------

By default, Emacs live will load the foundation, colour, clojure, lang and power packs in
that order. However, you may create a ~/.emacs-live.el file to
override this behaviour. Simply set the var live-packs to a list of
symbols representing the packs you'd like to load up (the order
will be honoured). For example to only load the foundation and colour packs:

    (setq live-packs '(live/foundation-pack live/colour-pack))

Helper fns
----------

Emacs Live provides a couple of useful helper fns which you can use
within your live packs:

* `live-pack-lib-dir` this returns the path of the lib dir for the current pack
* `live-load-config-file` loads a config file located in the config dir of the current pack
