nrepl-eval-sexp-fu
==================

eval-sexp-fu.el modified for use with nrepl. This is essentially the
same as the original source written by Takeshi Banse:

http://www.emacswiki.org/emacs/eval-sexp-fu.el

However, this version has been modified to work with nrepl rather than
slime. This version can happily co-exist with the original
`eval-sexp-fu` if desired.

Just drop the following into your `~/.emacs.d`

    ;; highlight expression on eval
    (require 'highlight)
    (require 'nrepl-eval-sexp-fu)
    (setq nrepl-eval-sexp-fu-flash-duration 0.5)

Enjoy!

(See nrepl-eval-sexp-fu.el for license)
