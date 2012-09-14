;; highlight expression on eval
(require 'highlight)
(live-add-pack-lib "nrepl-eval-sexp-fu")
(require 'nrepl-eval-sexp-fu)
(setq nrepl-eval-sexp-fu-flash-duration 0.5)
