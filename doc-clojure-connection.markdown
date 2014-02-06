---

title: Clojure Connection
filename: clojure-connection
layout: doc

---

## Obtaining Leiningen

The only external dependency required to connect to Clojure is Leiningen version 2 or higher. This can be obtained here: [https://github.com/technomancy/leiningen](https://github.com/technomancy/leiningen). The brief instructions are as follows:

1. Download the [script](https://raw.github.com/technomancy/leiningen/preview/bin/lein).
2. Place it on your `$PATH`. (I like to use `~/bin`)
3. Set it to be executable. (`chmod 755 ~/bin/lein`)

You should then verify that you have at least version 2.0 by running the command:

    lein -version

# nREPL

Emacs Live has support for nREPL built in via the cider library, and is the preferred way of connecting to and communicating with a Clojure process. nREPL is a server implementation and simple protocol for supporting external REPLs in addition to supporting the implementation of editors that offer incremental evaluation of buffers (i.e. you can specify specific Clojure forms to be evaluated in addition to evaluating all the forms in a given buffer). This will be explored in greater detail in a subsequent section, but for now we'll focus on how to connect Emacs Live to an nREPL server.

## Jacking in

The simplest way to get Emacs Live connected with a Clojure process is to use the function `cider-jack-in`. This allows you to connect to a specific lein project which therefore ensures that all the project dependencies defined in the project's `project.clj` are available on the JVM classpath.

In order to communicate with the correct project, you should summon `M-x cider-jack-in` whilst the point is in a buffer which is part of the Leiningen project you wish to connect to. This will then spawn `lein repl :headless` as an external shell process and automatically connect to it.

Project discovery is made possible because each buffer has a buffer-local variable named `default-directory` which stores the directory the file associated with the buffer is located within. This is then used as the working directory of the shell process, and even when started from a project subfolder, Leiningen automagically knows how to find the root of the project directory and start the JVM process from there. You can always verify what directory the JVM thinks is the working directory by issuing the following in the REPL: `(System/getProperty "user.dir")`.

You should now see a Clojure REPL which you can use to hack Clojure.

## Manual connection

Another option for connecting to an nREPL process is to manually start a Leiningen REPL by `cd`ing to the project directory and issuing the command `lein repl`. This will start a REPL on the terminal, but also tell you which port nREPL is listening on i.e.:

    nREPL server started on port 57371

You can then run the `cider` function within emacs via `M-x cider` specifying the host `localhost` and then entering the port that nREPL is currently listening on.

Clojure Hacking can now commence.

## Manual connection with default port

If you wish to connect manually but get annoyed finding the port number and manually entering every time you connect, you can create a profile within Leiningen that will force  nREPL to listen on a specific port. By default, Emacs Live attempts to connect to port `4555` so we should therefore ask nREPL to listen to this port number. This is achieved by modifying the file `~/.lein/profiles.clj`. If it doesn't exist, then you should create it and add the following:


    {:live-repl {:repl-options {:port 4555}}
     :user {:aliases {"live" ["with-profile" "default,live-repl" "repl"]}}}

If `~/.lein/profiles.clj` already exists, then you will need to merge the above map appropriately.

You can now start Leiningen with `lein live` which will start a normal REPL session, but this time it will always be listening on port `4555` for connections.

Connection is now a matter of `M-x cider RET RET`.
