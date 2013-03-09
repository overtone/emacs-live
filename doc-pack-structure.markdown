---

title: Pack Structure
filename: pack-structure
layout: doc

---

# --> Pack Structure

Emacs Live packs have a simple yet specific structure:

    pack/
    ├── README.md
    ├── config
    │   └── foo-conf.el
    ├── info.el
    ├── init.el
    └── lib
        └── foo-mode
            └── foo.el

Let's explore each part individually.

## --> README.md

In this file you should describe the pack, what it does, list any
dependencies etc. This is the location of the main documentation for
your pack.

## --> info.el

This file contains metadata about your pack that Emacs Live can read and
use. Currently there are the following entries to this file:


    (live-pack-name "your-pack-name-goes-here")
    (live-pack-version "1.0beta21"))
    (live-pack-description "A description of your pack")

## --> init.el

This is the file that first gets loaded by Emacs Live when it loads this
pack. You can place arbitrary elisp code here and Emacs will execute
it. However, it's not advised to put large amounts of elisp in this
file. Instead, it's recommended that you break up the config into
conceptual chunks and create separate config files which you can place
in the config directory. Emacs Live makes it easy to pull in these
config files with the fn `live-load-config-file`. For example, you can
load the file `foo-conf.el` stored in the config directory with the
following line:

    (live-load-config-file foo-conf.el")

## --> config

This is the directory into which you should place your config
files. Config files typically are elisp files for configuring a certain
aspect of your pack. Should your pack wish to pull in external projects,
such as new major modes which are stored in the lib directory (explained
below) then these projects can be added to the load path (and therefore
made visible to Emacs to `require` things with the fn
`live-add-pack-lib`. For example, if you have added the project
`foo-mode` to your lib directory which contains a `foo.el` file you need
to require to enable its functionality, you can add the following to a
`foo-conf.el` config file:

    (live-add-pack-lib "foo-mode")
    (require 'foo)

You can then use the rest of the config file to configure `foo-mode` in
any way you fancy.

## --> lib

This directory is where you can place any Emacs functionality that you
wish to include in your pack. For example, you may place any number new
major or minor modes in here. These files won't be loaded
automatically. This directory can be empty if you don't wish to pull in
any external projects.
