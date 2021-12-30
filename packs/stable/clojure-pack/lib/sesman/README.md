[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](http://melpa.org/packages/sesman-badge.svg)](http://melpa.org/#/sesman)
[![MELPA Stable](http://stable.melpa.org/packages/sesman-badge.svg)](http://stable.melpa.org/#/sesman)
[![Build Status](https://travis-ci.org/vspinu/sesman.svg?branch=master)](https://travis-ci.org/vspinu/sesman)

## Generic Session Manager for Emacs

Sesman provides facilities for session management and interactive session association with the current contexts (e.g. project, directory, buffers). While sesman can be used to manage arbitrary "sessions", it primary targets the Emacs based IDEs ([CIDER][], [ESS][], [Geiser][], [Robe][], [SLIME][] etc.)

For Emacs based IDEs, session are commonly composed of one or more physical processes (sub-processes, sockets, websockets etc). For example in the current implementation of [CIDER][] a session would be composed of one or more sesman connections (Clojure or ClojureScript). Each [CIDER][] connection consists of user REPL buffer and two sub-processes, one for user eval communication and another for tooling (completion, inspector etc).

### Concepts:

  - "session" is a list of the form `(session-name ..other-stuff..)` where `..other-stuff..` is system dependent.
  - "system" is generic name used for a tool which uses sesman (e.g. `CIDER`, `ESS` etc)
  - "contexts" are Emacs objects which describe current context. For example `current-buffer`, `default-directory` and `project-current` are such contexts. Context objects are used to create associations (links) between the current context and sessions. At any given time the user can link (unlink) sessions to (from) contexts. By default there are three types of contexts - buffer, directory and project, but systems can define their own specialized context types..
  
Sesman is composed of two parts, user interface, available as a sesman map and menu, and system interface consisting of a few generics and API functions.

### User Interface

Consists of 

 - lifecycle management commands (`sesman-start`, `sesman-quit` and `sesman-restart`), and
 - association management commands (`sesman-link-with-buffer`, `sesman-link-with-directory`, `sesman-link-with-project` and `sesman-unlink`). 

From the user's prospective the work-flow is as follow. Start a session, either with `sesman-start` (`C-c C-s C-s`) or some of the system specific commands (`run-xyz`, `xyz-jack-in` etc). On startup each session is automatically associated with the least specific context (commonly a project). In the most common case the user has only one session open per project. In such case, no ambiguity arises when a system retrieves the current session. If multiple sessions are associated with the current context the ambiguity is automatically resolved through the system specific relevance mechanism. Usually it is the most recently used session.

By default links with projects and directories are many-to-many in the sense that any session can be linked to multiple context and each context can be associated with multiple sessions. Buffers instead are 1-to-many. One buffer can be associated with only one session and a session can be associated with multiple buffers. This behavior is controlled by a custom `sesman-single-link-context-types`.

### System Interface

Consists of several generics, of which only first three are strictly required:

  - `sesman-start-session`
  - `sesman-quit-session`
  - `sesman-restart-session`
  - `sesman-session-info`
  - `sesman-context-types`
  - `sesman-more-relevant-p`
  
Sesman also provides [a range of utility functions][system api] functions to manipulate sessions, links and session components. Systems can register entire sessions with `sesman-register` or add/remove objects one by one with `sesman-add-object`/`sesman-remove-object`.

Systems should link sesman map into their key-maps (ideally on `C-c C-s`) and install sesman menu with `sesman-install-menu`. 

### Implementations

  - [CIDER implementation](https://docs.cider.mx/cider/usage/managing_connections.html)

<!-- , which is a good mnemonic and is already used in CIDER and ESS. -->


[cider]: https://github.com/clojure-emacs/cider
[ess]: https://ess.r-project.org/
[geiser]: https://github.com/jaor/geiser
[robe]: https://github.com/dgutov/robe
[slime]: https://common-lisp.net/project/slime/

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
