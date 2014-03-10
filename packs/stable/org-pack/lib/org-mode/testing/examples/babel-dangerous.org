#+Title: dangerous code block examples which should be isolated
#+OPTIONS: ^:nil

* no default value for vars
  :PROPERTIES:
  :ID:       f2df5ba6-75fa-4e6b-8441-65ed84963627
  :END:

There is no default value assigned to =x= variable. This is not permitted
anymore.

#+name: carre
#+begin_src python :var x
  return x*x
#+end_src
