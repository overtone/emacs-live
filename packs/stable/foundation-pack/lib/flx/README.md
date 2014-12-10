[![Build Status](https://travis-ci.org/lewang/flx.png)](http://travis-ci.org/lewang/flx)

## Status

This project is more than a year old now.  Lots of bugs have been worked out.

It appears some people use it on a regular basis.

## Screencast

[Screencast showing rationale and ido workflow][]

## Installation

### Manual

Just drop all `.el` files somewhere on your `load-path`. Here's an
example using the folder `~/.emacs.d/vendor`:

```lisp
(add-to-list 'load-path "~/emacs.d/vendor")
```

### Package Repositories

Available packages:

- `flx` - matching engine
- `flx-ido` - ido interface for flx

Install `flx-ido` will pull in `flx` as a dependency.


#### [MELPA](http://melpa.milkbox.net)

If you're an Emacs 24 user or you have a recent version of `package.el` you
can install `flx-ido` from MELPA.

This version will always be up-to-date.

#### [Marmalade](http://marmalade-repo.org/)

`flx-ido` is also available on the Marmalade `package.el` repository.

### Emacs Prelude

`flx-ido` is part of the
[Emacs Prelude](https://github.com/bbatsov/prelude). If you're a Prelude
user - `flx-ido` is already properly configured and ready for
action.

## Usage

The sorting algorithm is a balance between word beginnings (abbreviation) and
contiguous matches (substring).

The longer the substring match, the higher it scores.  This maps well to how
we think about matching.

In general, it's better form queries with only lowercase characters so
the sorting algorithm can do something smart.

For example, if you have these files:

        projects/clojure-mode/clojure-mode.el
        projects/prelude/core/prelude-mode.el

If the search term was *pre-mode*, you might expect "prelude-mode.el" to rank
higher.  However because the substring match "re-mode" is so long,
"clojure-mode.el" actually scores higher.

**Here, using *premode* would give the expected order.** Notice that the
"-" actually prevents the algorithm from helping you.

### uppercase letters

Flx always folds lowercase letters to match uppercase.  However, you can use uppercase letters for force flx to only match uppercase.

This is similar to Emacs' case-folding.  The difference is mixing in uppercase letters **does not disable** folding.

### completing file names

Matches within the basepath score higher.

## ido support

Add this to your init file and *flx* match will be enabled for ido.

```lisp
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
```

If you don't want to use the `flx`'s highlights you can turn them off like this:

```lisp
(setq flx-ido-use-faces nil)
```

### Flx uses a complex matching heuristics which can be slow for large collections

Customize `flx-ido-threshold` to change the collection size above which flx
will revert to flex matching.

As soon as the collection is narrowed below `flx-ido-threshold`, flx will
kick in again.

As a point of reference for a 2.3 GHz quad-core i7 processor, a value of
`10000` still provides a reasonable completion experience.

- see `flx-ido-big-demo` for example with 36k string (Emacs 24.3).



## Helm support

[Helm][] is not supported yet.  There is a demo showing how it could work, but I'm
still working through how to integrate it into helm.

The Helm demo shows the score of the top 20 matches.

## Memory Usage

The `flx` algorithm willingly sacrifices memory usage for speed.

For 10k file names, about 10 MB of memory will be used to speed up future
matching.  This memory is never released to keep the match speed fast.

So far with modern computers, this feels like a reasonable design decision.

It may change in future.

## GC Optimization

Emacs's garbage collector is fairly primitive stop the world type.  GC time can
contribute significantly to the run-time of computation that allocates and
frees a lot of memory.

Consider the following example:

```lisp
(defun uuid ()
  (format "%08x-%08x-%08x-%08x"
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))))

(benchmark-run 1
  (let ((cache (flx-make-filename-cache)))
    (dolist (i (number-sequence 0 10000))
      (flx-process-cache (uuid) cache))))
        ;;; ⇒ (0.899678 9 0.33650300000000044)
```

This means that roughly 30% of time is spent just doing garbage-collection.

`flx` can benefit significantly from garbage collection tuning.

By default Emacs will initiate GC every 0.76 MB allocated (`gc-cons-threshold`
== 800000).  If we increase this to 20 MB (`gc-cons-threshold` == 20000000)
we get:

````lisp
(benchmark-run 1
  (setq gc-cons-threshold 20000000)
  (let ((cache (flx-make-filename-cache)))
    (dolist (i (number-sequence 0 10000))
      (flx-process-cache (uuid) cache))))
    ;;; ⇒ (0.62035 1 0.05461100000000041)
```

So if you have a modern machine, I encourage you to add the following:

```lisp
(setq gc-cons-threshold 20000000)
```

to your init file.


[Screencast showing rationale and ido workflow]: http://www.youtube.com/watch?v=_swuJ1RuMgk
[Helm]: https://github.com/emacs-helm/helm
