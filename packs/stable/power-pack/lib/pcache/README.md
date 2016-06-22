[![Build Status](https://travis-ci.org/sigma/pcache.png?branch=master)](https://travis-ci.org/sigma/pcache)

## Pcache

pcache provides a persistent way of caching data, in a hashtable-like
structure. It relies on `eieio-persistent' in the backend, so that any
object that can be serialized by EIEIO can be stored with pcache.

pcache handles objects called "repositories" (`pcache-repository`) and
"entries" (`pcache-entry`). Each repository is identified by a unique name,
that defines an entry in `pcache-directory`. Subdirectories are allowed, by
the use of a directory separator in the repository name.

Example:

```lisp
(let ((repo (pcache-repository "plop")))
  (pcache-put repo 'foo 42) ; store value 42 with key 'foo
  (pcache-get repo 'foo) ; => 42
)
```

Keys can be pretty much any Lisp object, and are compared for equality using
`eql`

Optionally, cache entries can expire:

```lisp
(let ((repo (pcache-repository "plop")))
  (pcache-put repo 'foo 42 1) ; store value 42 with key 'foo for 1 second
  (sleep-for 1)
  (pcache-get repo 'foo) ; => nil
)
```
