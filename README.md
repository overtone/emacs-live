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
* **Live** A live-programming power up with a specific focus on Clojure and Overtone hacking
* **Colour** Colour highlighting in two flavours - blackbeard and whitebeard
* **User** An empty pack for your own libs and config.

Loading Packs
-------------

By default, Emacs live will load the live, colour and user packs in
that order. However, you may create a ~/.emacs-live.el file to
override this behaviour. Simply set the var live-packs to a list of
symbols representing the packs you'd like to load up (the order
will be honoured). For example to keep the default:
(setq live-packs '(live colour user))

Helper fns
----------

Emacs Live provides a couple of useful helper fns which you can use
within your live packs:

* `live-pack-lib-dir` this returns the path of the lib dir for the current pack
* `live-load-config-file` loads a config file located in the config dir of the current pack
dfdf
