---

title: Customisation
filename: customisation
layout: doc

---

# --> Customisation

Emacs Live allows you to supply your own personal configuration which
will be evaluated after Emacs Live has booted allowing you to add new
functionality or to override any setting Emacs Live may have defined.

Although it is possible to edit the `~/.emacs.d/init.el` file that is
included with Emacs Live to do exactly this, it's not recommended. This
is because editing any file in the Emacs Live `~/.emacs.d` folder will
make it harder to upgrade to new versions of Emacs Live. Instead it's
recommended to create your own user pack in a directory external to
`~/.emacs.d` and to use that. This makes upgrading Emacs Live as simple
as replacing `~/.emacs.d` or doing a `git pull` if you cloned via git.

User packs are directories with a specific structure that can live
anywhere on your file system.

## --> Creating a User Pack

If you install Emacs Live with the fancybrogrammer script, then it gives
you the option of creating an Emacs Live user pack. If you choose this
option, your user pack will be found in `~/.live-packs`

However, the brogrammer script is written with Bash - so it's not going
to work on Windows (perhaps cygwin may work, but it's
untested). Therefore, Windows users need to set up user packs
themselves. Luckily this is pretty simple.

Inside `~/.emacs.d/packs` thereis a directory called `template`. You can
copy this whole directory and rename it to something like `~/.live-packs` -
this folder can exist anywhere on your file system.

Inside your new `~/.live-packs` dir you'll find a subdir named
`user-template-pack`. You should rename this to something more
meaningful such as `yourname-pack`. This is where you'll place your own
config.

## --> Telling Emacs Live about your User Pack

Finally, you need to teach Emacs Live to load your packs. This is
possible via a config file with the path `~/.emacs-live.el`. If this
file doesn't exist on your system, you should create it. Inside this file you should add the line:

    (live-append-packs '(~/.live-packs/your-name-pack))

Next time you boot Emacs, your live pack will be loaded.
