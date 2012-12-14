---

title: Starting
filename: starting
layout: doc

---

# --> Starting Emacs Live

Typically, starting Emacs Live is achieved by starting Emacs. It's that simple :-)

# --> Daemon Mode

Due to the amount of functionality, Emacs Live  can take some time to boot. It's possible to avoid paying this boot-time cost every time you wish to edit stuff in Emacs by running Emacs in daemon mode and then using Emacs in client mode to connect to the server process. This way, the server process pays the boot-time cost, and the clients fire up instantly.

# --> Daemon Mode in the Terminal

If you're running Emacs in the terminal on Linux or OS X, starting it up into daemon mode is as simple as: `emacs --daemon`. Then, in order to connect to the server, you need to use the command `emacsclient`. I typically create the following aliases in my shell to make this easier to work with:

    alias emacs="/usr/local/bin/emacsclient -ct"
    alias es="/usr/local/bin/emacs --daemon"

I then run `es` when I start my terminal and then forget about it. Running the command `emacs` starts up a new Emacs terminal session instantly. The `-ct` flags create a new Emacs frame to the daemon process in the current terminal.

It's also useful to set `emacsclient` as your default editor in the `EDITOR` shell variable:

    export EDITOR='emacsclient -ct'

# --> Daemon mode with OS X GUI Emacs

If you're running a GUI version of Emacs on OS X, then you can use the following configuration to start emacs in daemon mode and create new GUI frames from the terminal:

    alias es='/Applications/Emacs.app/Contents/MacOS/Emacs --daemon'
    alias emacs='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -c -n'
    export EDITOR='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -c'

It's also possible to [automatically start the daemon process on boot with launchd](http://cubiclemuses.com/cm/articles/2009/08/06/emacs-and-os-x-launchd/).

# --> General Daemon Mode Information

More information about using Emacs with Daemon mode [can be found on the Emacs wiki](http://www.emacswiki.org/emacs/EmacsAsDaemon)
