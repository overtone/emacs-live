;; Usage:
;;
;;   cask exec emacs -Q -l tests/run-test.el           # interactive mode
;;   cask exec emacs -batch -Q -l tests/run-test.el    # batch mode


;; Utils
(defun popup-test-join-path (path &rest rest)
  "Join a list of PATHS with appropriate separator (such as /).

\(fn &rest paths)"
  (if rest
      (concat (file-name-as-directory path) (apply 'popup-test-join-path rest))
    path))

(defvar popup-test-dir (file-name-directory load-file-name))
(defvar popup-root-dir (concat popup-test-dir ".."))


;; Setup `load-path'
(mapc (lambda (p) (add-to-list 'load-path p))
      (list popup-test-dir
            popup-root-dir))

;; Load tests
(load "popup-test")


;; Run tests
(if noninteractive
    (ert-run-tests-batch-and-exit)
  (ert t))
