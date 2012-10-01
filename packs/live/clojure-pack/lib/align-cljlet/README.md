# Description:

'align-cljlet' is an emacs addin for aligning let-like forms. This
program exists because I was tired of manually aligning let statements
in Clojure.  This program is designed to quickly and easily allow
let-like forms to be aligned.  This is my first emacs lisp program and
as a result if probably less than optimal.  Feel free to suggest
improvements or send in patches.

This program was inspired by align-let.el although only borrows one
function from that code.  I had considered altering align-let.el to
work correctly with Clojure however it was easiler to simply start
from scratch.

Forms currently handled:

 * let
 * when-let
 * if-let
 * binding
 * loop
 * with-open
 * literal hashes {}
 * defroute

If there are let pairs together on the same line the code will refuse
to align them. For example, the following will not align:

    (let [apple 2 pair 3
          peach 23] ...)

will not be aligned.

# Custom variables:

`defroute-columns` The number of columns to align in a defroute form.

# Known limitations:

* This program requires clojure mode to be running in order to
  function correctly.

# Installation:

To use align-cljlet.el, add it to your load-path and include the
following to your .emacs configuration.

(require 'align-cljlet)

Alternatively you may install this package from Marmalade.

# Usage:

To invoke simply position anywhere inside an alignable form and
invoke:

 M-x align-cljlet

You may wish to bind this to a specific key.
