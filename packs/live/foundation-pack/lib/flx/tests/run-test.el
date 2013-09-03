;; Usage:
;;
;;   emacs -Q -l tests/run-test.el           # interactive mode
;;   emacs -batch -Q -l tests/run-test.el    # batch mode


;; Utils
(defun flx-test-join-path (path &rest rest)
  "Join a list of PATHS with appropriate separator (such as /).

\(fn &rest paths)"
  (if rest
      (concat (file-name-as-directory path) (apply 'flx-test-join-path rest))
    path))

(defvar flx-test-dir (file-name-directory load-file-name))
(defvar flx-root-dir (file-name-as-directory (expand-file-name ".." flx-test-dir)))


;; Setup `load-path'
(mapc (lambda (p) (add-to-list 'load-path p))
      (list flx-test-dir
            flx-root-dir))


;; Use ERT from github when this Emacs does not have it
(unless (locate-library "ert")
  (add-to-list
   'load-path
   (flx-test-join-path flx-root-dir "lib" "ert" "lisp" "emacs-lisp"))
  (require 'ert-batch)
  (require 'ert-ui))


;; Load tests
(load "flx-test")


;; Run tests
(if noninteractive
    (ert-run-tests-batch-and-exit)
  (ert t))
