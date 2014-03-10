#!/bin/sh
:;# Exit code 0 means success
:;# -*- emacs-lisp -*-
:;exec emacs -q --load "$0"

(load (expand-file-name "./popwin.el"))
(setq display-buffer-function 'popwin:display-buffer)
(setq popwin:special-display-config '(("*Backtrace*")))
(setq debug-on-error t)
(run-with-idle-timer
 0 nil
 (lambda ()
   (with-current-buffer "*Backtrace*"
     (kill-emacs
      (if (eq (point-max) 1)
          1 0)))))
(error "Can you see this?")
