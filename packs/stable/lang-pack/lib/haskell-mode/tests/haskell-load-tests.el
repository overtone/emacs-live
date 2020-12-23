;;; haskell-load-tests.el  -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'haskell-test-utils)

(require 'haskell-load)

(defun insert-errors ()
  (insert "import Control.Applicativ\nimport Data.Mayb\nimport Data.String")
  (goto-char 1)
  (let ((applicativ (progn
                      (search-forward "Control.Applicativ")
                      (make-overlay (match-beginning 0) (match-end 0)))))
    (overlay-put applicativ 'haskell-check t)
    (overlay-put applicativ 'haskell-msg-type 'error)
    (overlay-put applicativ 'haskell-msg "Could not find module ‘Control.Applicativ’\n    Perhaps you meant Control.Applicative (from base-4.8.1.0)\n    Use -v to see a list of the files searched for."))
  (let ((mayb (progn
                (search-forward "Data.Mayb")
                (make-overlay (match-beginning 0) (match-end 0)))))
    (overlay-put mayb 'haskell-check t)
    (overlay-put mayb 'haskell-msg-type 'error)
    (overlay-put mayb 'haskell-msg "Could not find module ‘Data.Mayb’\n    Perhaps you meant\n      Data.Maybe (from base-4.8.1.0)\n      Data.Map (from containers-0.5.6.2@conta_LKCPrTJwOTOLk4OU37YmeN)\n    Use -v to see a list of the files searched for."))
  (goto-char 1))

(ert-deftest goto-first-error-before ()
  (with-temp-switch-to-buffer
   (insert-errors)
   (haskell-goto-first-error)
   (should (looking-at-p "Control.Applicativ"))))

(ert-deftest goto-first-error-after ()
  (with-temp-switch-to-buffer
   (insert-errors)
   (search-forward "Data.String")
   (haskell-goto-first-error)
   (should (looking-at-p "Control.Applicativ"))))

(ert-deftest goto-first-error-between ()
  (with-temp-switch-to-buffer
   (insert-errors)
   (search-forward "import Data.Mayb")
   (haskell-goto-first-error)
   (should (looking-at-p "Control.Applicativ"))))

(ert-deftest goto-next-error-before ()
  (with-temp-switch-to-buffer
   (insert-errors)
   (haskell-goto-next-error)
   (should (looking-at-p "Control.Applicativ"))))

(ert-deftest goto-next-error-between ()
  (with-temp-switch-to-buffer
   (insert-errors)
   (search-forward "import" nil nil 2)
   (haskell-goto-next-error)
   (should (looking-at-p "Data.Mayb"))))

(ert-deftest goto-next-error-after ()
  (with-temp-switch-to-buffer
   (insert-errors)
   (search-forward "import" nil nil 3)
   (haskell-goto-next-error)
   (should (looking-at-p " Data.String"))))

(ert-deftest goto-prev-error-before ()
  (with-temp-switch-to-buffer
   (insert-errors)
   (haskell-goto-prev-error)
   (should (looking-at-p "import Control.Applicativ"))))

(ert-deftest goto-prev-error-between ()
  (with-temp-switch-to-buffer
   (insert-errors)
   (search-forward "import" nil nil 2)
   (haskell-goto-prev-error)
   (should (looking-at-p "Control.Applicativ"))))

(ert-deftest goto-prev-error-after ()
  (with-temp-switch-to-buffer
   (insert-errors)
   (search-forward "import Data.String")
   (haskell-goto-prev-error)
   (should (looking-at-p "Data.Mayb"))))

(ert-deftest do-cabal-no-process ()
  "Ensure that haskell-process-do-cabal can call cabal directly.

Redefine `shell-command' to just capture the command it's asked
to execute, and make sure it matches what we expected."
  (let (shell-call)
    (cl-letf (((symbol-function 'shell-command) (lambda (command &optional input-buffer output-buffer)
                                                  (setq shell-call command))))
      (haskell-process-do-cabal "help")
      (should (equal shell-call "cabal help")))))
