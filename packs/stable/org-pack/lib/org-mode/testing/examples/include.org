Small Org file with an include keyword.

#+BEGIN_SRC emacs-lisp :exports results
(+ 2 1)
#+END_SRC

#+INCLUDE: "include2.org"

* Heading
body

* Another heading
:PROPERTIES:
:CUSTOM_ID: ah
:END:
1
2
3

* A headline with a table
:PROPERTIES:
:CUSTOM_ID: ht
:END:
#+CAPTION: a table
#+NAME: tbl
| 1 |

* drawer-headline
:PROPERTIES:
:CUSTOM_ID: dh
:END:
:LOGBOOK:
drawer
:END:
content
