;;; haskell-completions-tests.el --- Tests for Haskell Completion package -*- lexical-binding: t -*-

;; Copyright © 2015 Athur Fayzrakhmanov. All rights reserved.

;; This file is part of haskell-mode package.
;; You can contact with authors using GitHub issue tracker:
;; https://github.com/haskell/haskell-mode/issues

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package provides regression tests for haskell-completions package.

;;; Code:

(require 'ert)
(require 'haskell-mode)
(require 'haskell-completions)


(ert-deftest haskell-completions-can-grab-prefix-test ()
  "Tests the function `haskell-completions-can-grab-prefix'."
  (with-temp-buffer
    (haskell-mode)
    (should (eql nil (haskell-completions-can-grab-prefix)))
    (insert " ")
    (should (eql nil (haskell-completions-can-grab-prefix)))
    (insert "a")
    (should (eql t (haskell-completions-can-grab-prefix)))
    (save-excursion
      (insert " ")
      (should (eql nil (haskell-completions-can-grab-prefix)))
      (insert "bc-")
      (should (eql t (haskell-completions-can-grab-prefix)))
      (insert "\n")
      (should (eql nil (haskell-completions-can-grab-prefix)))
      (insert "def:#!")
      (should (eql t (haskell-completions-can-grab-prefix))))
    (should (eql t (haskell-completions-can-grab-prefix)))
    ;; punctuation tests
    (save-excursion (insert ")"))
    (should (eql t (haskell-completions-can-grab-prefix)))
    (save-excursion (insert ","))
    (should (eql t (haskell-completions-can-grab-prefix)))
    (save-excursion (insert "'"))
    (should (eql t (haskell-completions-can-grab-prefix)))
    ;; should return nil in the middle of word
    (save-excursion (insert "bcd"))
    (should (eql nil (haskell-completions-can-grab-prefix)))
    ;; region case
    (push-mark (point-min) t)
    (activate-mark)
    (should (eql nil (haskell-completions-can-grab-prefix)))))


(ert-deftest haskell-completions-grab-pragma-prefix-nil-cases-test ()
  "Tests the function `haskell-completions-grab-pragma-prefix'
within empty pragma comment {-# #-} and outside of it."
  (with-temp-buffer
    (haskell-mode)
    (goto-char (point-min))
    (insert "{")
    (should (eql nil (haskell-completions-grab-pragma-prefix)))
    (insert "-")
    (should (eql nil (haskell-completions-grab-pragma-prefix)))
    (insert "#")
    (should (eql nil (haskell-completions-grab-pragma-prefix)))
    (insert "  ")
    (should (eql nil (haskell-completions-grab-pragma-prefix)))
    (insert "#")
    (should (eql nil (haskell-completions-grab-pragma-prefix)))
    (insert "-")
    (should (eql nil (haskell-completions-grab-pragma-prefix)))
    (insert "}")
    (should (eql nil (haskell-completions-grab-pragma-prefix)))
    (insert "\n")
    (should (eql nil (haskell-completions-grab-pragma-prefix)))
    (insert "main")
    (should (eql nil (haskell-completions-grab-pragma-prefix)))
    (insert ":: IO ()")
    (should (eql nil (haskell-completions-grab-pragma-prefix)))))

(ert-deftest haskell-completions-grab-pragma-name-prefix-test ()
  "Tests both `haskell-completions-grab-pragma-prefix' and
`haskell-completions-grab-prefix' functions for pragma names
completions such as WARNING, LANGUAGE, DEPRECATED and etc."
  (let ((expected (list 5 8 "LAN" 'haskell-completions-pragma-name-prefix)))
    (with-temp-buffer
      (haskell-mode)
      (goto-char (point-min))
      (insert "{-# LAN")
      (should (equal expected (haskell-completions-grab-pragma-prefix)))
      ;; minimal prefix length
      (should (equal expected (haskell-completions-grab-prefix 3)))
      (should (eql nil (haskell-completions-grab-prefix 4)))
      (save-excursion (insert " #-}"))
      ;; should work in case of closed comment, e.g. {-# LAN| #-}
      (should (equal expected (haskell-completions-grab-pragma-prefix)))
      (should (equal expected (haskell-completions-grab-prefix)))
      ;; pragma function should work in the middle of word
      (backward-char)
      (should (not (equal expected (haskell-completions-grab-pragma-prefix))))
      ;; but general function should not
      (should (eql nil (haskell-completions-grab-prefix))))
    (with-temp-buffer
      (haskell-mode)
      (goto-char (point-min))
      (insert "{-#\nLAN")
      ;; should work for multiline case
      (should (equal expected (haskell-completions-grab-pragma-prefix)))
      (should (equal expected (haskell-completions-grab-prefix))))))

(ert-deftest haskell-completions-grab-ghc-options-prefix-test-01 ()
  "Tests both `haskell-completions-grab-pragma-prefix' and
`haskell-completions-grab-prefix' functions for GHC options
prefixes."
  (let (expected)
    (with-temp-buffer
      (haskell-mode)
      (goto-char (point-min))
      (setq expected
            (list 5 16 "OPTIONS_GHC" 'haskell-completions-pragma-name-prefix))
      (insert "{-# OPTIONS_GHC")
      (should (equal expected (haskell-completions-grab-pragma-prefix)))
      (should (equal expected (haskell-completions-grab-prefix)))
      (insert " --opt1")
      (setq expected
            (list 17 23 "--opt1" 'haskell-completions-ghc-option-prefix))
      (should (equal expected (haskell-completions-grab-pragma-prefix)))
      (should (equal expected (haskell-completions-grab-prefix)))
      (insert "    -XOpt-2")
      (setq expected
            (list 27 34 "-XOpt-2" 'haskell-completions-ghc-option-prefix))
      (should (equal expected (haskell-completions-grab-pragma-prefix)))
      (should (equal expected (haskell-completions-grab-prefix)))
      (save-excursion
        (insert "\n")
        (insert "\"new-line\"")
        ;; should handle multiline case
        (setq
         expected
         (list 35 45 "\"new-line\"" 'haskell-completions-ghc-option-prefix))
        (should (equal expected (haskell-completions-grab-pragma-prefix)))
        (should (equal expected (haskell-completions-grab-prefix)))
        (insert " test")
        (setq expected
              (list 46 50 "test" 'haskell-completions-ghc-option-prefix))
        ;; minimal prefix length
        (should (equal expected (haskell-completions-grab-prefix 4)))
        (should (eql nil (haskell-completions-grab-prefix 5)))
        (insert "    ")
        (should (eql nil (haskell-completions-grab-pragma-prefix)))
        (should (eql nil (haskell-completions-grab-prefix)))
        (insert "#-}"))
      ;; should work in case of closed comment, e.g. {-# OPTIONS_GHC xyz| #-}
      (setq expected
            (list 27 34 "-XOpt-2" 'haskell-completions-ghc-option-prefix))
      (should (equal expected (haskell-completions-grab-pragma-prefix)))
      (should (equal expected (haskell-completions-grab-prefix)))
      (backward-char)
      ;; pragma function should work in the middle of word
      (should (not (eql nil (haskell-completions-grab-pragma-prefix))))
      ;; but general function should not
      (should (eql nil (haskell-completions-grab-prefix))))))

(ert-deftest haskell-completions-grab-ghc-options-prefix-test-02 ()
  "Tests both `haskell-completions-grab-pragma-prefix' and
`haskell-completions-grab-prefix' functions  for GHC options prefixes.  Same
tests as above for obsolete OPTIONS pragma."
  (let (expected)
    (with-temp-buffer
      (haskell-mode)
      (goto-char (point-min))
      (insert "{-# OPTIONS")
      (setq expected
            (list 5 12 "OPTIONS" 'haskell-completions-pragma-name-prefix))
      (should (equal expected (haskell-completions-grab-pragma-prefix)))
      (should (equal expected (haskell-completions-grab-prefix)))
      (insert " --opt1")
      (setq expected
            (list 13 19 "--opt1" 'haskell-completions-ghc-option-prefix))
      (should (equal expected (haskell-completions-grab-pragma-prefix)))
      (should (equal expected (haskell-completions-grab-prefix)))
      (insert "    -XOpt-2")
      (setq expected
            (list 23 30 "-XOpt-2" 'haskell-completions-ghc-option-prefix))
      (should (equal expected (haskell-completions-grab-pragma-prefix)))
      (should (equal expected (haskell-completions-grab-prefix)))
      (save-excursion
        (insert "\n")
        (insert "\"new-line\"")
        ;; should handle multiline case
        (setq
         expected
         (list 31 41 "\"new-line\"" 'haskell-completions-ghc-option-prefix))
        (should (equal expected (haskell-completions-grab-pragma-prefix)))
        (should (equal expected (haskell-completions-grab-prefix)))
        (insert " test")
        ;; minimal prefix length
        (setq expected
              (list 42 46 "test" 'haskell-completions-ghc-option-prefix))
        (should (equal expected (haskell-completions-grab-prefix 4)))
        (should (eql nil (haskell-completions-grab-prefix 5)))
        (insert "    ")
        (should (eql nil (haskell-completions-grab-pragma-prefix)))
        (should (eql nil (haskell-completions-grab-prefix)))
        (insert "#-}"))
      ;; should work in case of closed comment
      (setq expected
            (list 23 30 "-XOpt-2" 'haskell-completions-ghc-option-prefix))
      (should (equal expected (haskell-completions-grab-pragma-prefix)))
      (should (equal expected (haskell-completions-grab-prefix)))
      (backward-char)
      ;; pragma function should work in the middle of word
      (should (not (eql nil (haskell-completions-grab-pragma-prefix))))
      ;; but general function should not
      (should (eql nil (haskell-completions-grab-prefix))))))

(ert-deftest haskell-completions-grab-language-extenstion-prefix-test ()
  "Tests both `haskell-completions-grab-pragma-prefix' and
`haskell-completions-grab-prefix' functions for language
extension prefixes."
  (let (expected)
    (with-temp-buffer
      (haskell-mode)
      (goto-char (point-min))
      (insert "{-# LANGUAGE")
      (setq expected
            (list 5 13 "LANGUAGE" 'haskell-completions-pragma-name-prefix))
      (should (equal expected (haskell-completions-grab-pragma-prefix)))
      (should (equal expected (haskell-completions-grab-prefix)))
      (insert " Rec")
      (setq expected
            (list 14 17 "Rec" 'haskell-completions-language-extension-prefix))
      (should (equal expected (haskell-completions-grab-pragma-prefix)))
      (should (equal expected (haskell-completions-grab-prefix)))
      (insert ",    -XOpt-2")
      (setq
       expected
       (list 22 29 "-XOpt-2" 'haskell-completions-language-extension-prefix))
      (should (equal expected (haskell-completions-grab-pragma-prefix)))
      (should (equal expected (haskell-completions-grab-prefix)))
      (insert ",\n")
      (insert "\"new-line\"")
      ;; should handle multiline case
      (setq expected
            (list 31
                  41
                  "\"new-line\""
                  'haskell-completions-language-extension-prefix))
      (should (equal expected (haskell-completions-grab-pragma-prefix)))
      (should (equal expected (haskell-completions-grab-prefix)))
      (insert " -test")
      ;; minimal prefix length
      (setq expected
            (list 42 47 "-test" 'haskell-completions-language-extension-prefix))
      (should (equal expected (haskell-completions-grab-prefix 5)))
      (should (eql nil (haskell-completions-grab-prefix 6)))
      (save-excursion (insert "     #-}"))
      ;; should work in case of closed comment
      (setq expected
            (list 42 47 "-test" 'haskell-completions-language-extension-prefix))
      (should (equal expected (haskell-completions-grab-pragma-prefix)))
      (should (equal expected (haskell-completions-grab-prefix)))
      (backward-char)
      ;; pragma function should work in the middle of the word
      (should (not (eql nil (haskell-completions-grab-pragma-prefix))))
      ;; but general function does not
      (should (eql nil (haskell-completions-grab-prefix))))))


(ert-deftest haskell-completions-grab-identifier-prefix-test ()
  "Tests both `haskell-completions-grab-identifier-prefix' and
`haskell-completions-grab-prefix' functions for arbitrary haskell
identifiers and module identifiers."
  (let (expected)
    (with-temp-buffer
      (haskell-mode)
      (should (eql nil (haskell-completions-grab-identifier-prefix)))
      (should (eql nil (haskell-completions-grab-prefix)))
      (insert "import")
      (setq expected (list 1 7 "import" 'haskell-completions-identifier-prefix))
      (should (equal expected (haskell-completions-grab-identifier-prefix)))
      (should (equal expected (haskell-completions-grab-prefix)))
      (insert " Da")
      (setq expected (list 8 10 "Da" 'haskell-completions-module-name-prefix))
      (should (equal expected (haskell-completions-grab-identifier-prefix)))
      (should (equal expected (haskell-completions-grab-prefix)))
      ;; minimal length test
      (should (equal expected (haskell-completions-grab-prefix 2)))
      (should (eql nil (haskell-completions-grab-prefix 3)))
      (insert "ta.")
      (setq expected
            (list 8 13 "Data." 'haskell-completions-module-name-prefix))
      (should (equal expected (haskell-completions-grab-identifier-prefix)))
      (should (equal expected (haskell-completions-grab-prefix)))
      (insert "Text (pack")
      (setq expected (list 19 23 "pack" 'haskell-completions-identifier-prefix))
      (should (equal expected (haskell-completions-grab-identifier-prefix)))
      (should (equal expected (haskell-completions-grab-prefix)))
      (insert ")")
      ;; should work when point is at punctuation
      (backward-char)
      (should (equal expected (haskell-completions-grab-identifier-prefix)))
      (should (equal expected (haskell-completions-grab-prefix)))
      (forward-char)
      ;; but should not work after punctuation
      (should (eql nil (haskell-completions-grab-identifier-prefix)))
      (should (eql nil (haskell-completions-grab-prefix)))
      (insert "\n")
      (insert "import qualified Data.Text as T")
      (insert "\n\n")
      ;; should not work at the beginning of a line
      (should (eql nil (haskell-completions-grab-identifier-prefix)))
      (should (eql nil (haskell-completions-grab-prefix)))
      (insert "main")
      (setq expected (list 58 62 "main" 'haskell-completions-identifier-prefix))
      (should (equal expected (haskell-completions-grab-identifier-prefix)))
      (should (equal expected (haskell-completions-grab-prefix)))
      (insert " :: IO")
      (setq expected (list 66 68 "IO" 'haskell-completions-identifier-prefix))
      (should (equal expected (haskell-completions-grab-identifier-prefix)))
      (should (equal expected (haskell-completions-grab-prefix)))
      (insert " ()\n")
      (insert "main")
      (save-excursion
        (insert " = putStrLn")
        (setq expected
              (list 79 87 "putStrLn" 'haskell-completions-identifier-prefix))
        (should (equal expected (haskell-completions-grab-identifier-prefix)))
        (should (equal expected (haskell-completions-grab-prefix)))
        (insert " (T.")
        (setq expected (list 89 91 "T." 'haskell-completions-identifier-prefix))
        (should (equal expected (haskell-completions-grab-identifier-prefix)))
        (should (equal expected (haskell-completions-grab-prefix)))
        (insert "unpack")
        (setq expected
              (list 89 97 "T.unpack" 'haskell-completions-identifier-prefix))
        (should (equal expected (haskell-completions-grab-identifier-prefix)))
        (should (equal expected (haskell-completions-grab-prefix)))
        (insert " (T.pack (\"Hello")
        (setq expected nil)
        (should (equal expected (haskell-completions-grab-identifier-prefix)))
        (should (equal expected (haskell-completions-grab-prefix)))
        (insert " World!\" :: String")
        (setq expected
              (list 125 131 "String" 'haskell-completions-identifier-prefix))
        (should (equal expected (haskell-completions-grab-identifier-prefix)))
        (should (equal expected (haskell-completions-grab-prefix)))
        (insert " -- Comment")
        (setq expected nil)
        (should (equal expected (haskell-completions-grab-identifier-prefix)))
        (should (equal expected (haskell-completions-grab-prefix))))
      ;; test in the middle of line
      (setq expected (list 72 76 "main" 'haskell-completions-identifier-prefix))
      (should (equal expected (haskell-completions-grab-identifier-prefix)))
      (should (equal expected (haskell-completions-grab-prefix)))
      (backward-char)
      (setq expected (list 72 75 "mai" 'haskell-completions-identifier-prefix))
      (should (equal expected (haskell-completions-grab-identifier-prefix)))
      (should (equal nil (haskell-completions-grab-prefix))))
    ;; qualified imports and "as indentifier" tests
    (with-temp-buffer
      (haskell-mode)
      (insert "import qualified Data.Text")
      (setq expected
            (list 18 27 "Data.Text" 'haskell-completions-module-name-prefix))
      (should (equal expected (haskell-completions-grab-identifier-prefix)))
      (should (equal expected (haskell-completions-grab-prefix)))
      (insert " as T")
      (setq expected (list 31 32 "T" 'haskell-completions-identifier-prefix))
      (should (equal expected (haskell-completions-grab-identifier-prefix)))
      (should (equal expected (haskell-completions-grab-prefix))))))


(ert-deftest haskell-completions-complete-operators-test ()
  "Tests `haskell-completions-complete-operators' and its effects
on `haskell-completions-sync-repl-completion-at-point'."

  ;; Mock `haskell-completions-sync-complete-repl'
  (defadvice haskell-completions-sync-complete-repl-mock (around haskell-completions-sync-complete-repl
                                                                 (prefix &optional import))
    nil)
  (ad-activate 'haskell-completions-sync-complete-repl-mock)

  ;; Mock `haskell-session-maybe'
  (defadvice haskell-session-maybe-mock (around haskell-session-maybe)
    t)
  (ad-activate 'haskell-session-maybe-mock)

  ;; Mock `haskell-process-cmd'
  (defadvice haskell-process-cmd-mock (around haskell-process-cmd
                                          (P))
    nil)
  (ad-activate 'haskell-process-cmd-mock)

  ;; Mock `haskell-interactive-process'
  (defadvice haskell-interactive-process-mock (around haskell-interactive-process)
    nil)
  (ad-activate 'haskell-interactive-process-mock)

  ;;; Tests
  (unwind-protect
      (let (expected)
        (with-temp-buffer
          (haskell-mode)

          (insert "import qualified Data.List as L\n\n")
          (insert "test = L")
          (let* ((haskell-completions-complete-operators nil))
            (setq expected (nth 2 (haskell-completions-sync-repl-completion-at-point)))
            (should expected))
          (let* ((haskell-completions-complete-operators t))
            (setq expected (nth 2 (haskell-completions-sync-repl-completion-at-point)))
            (should expected))

          (insert ".")
          (let* ((haskell-completions-complete-operators nil))
            (setq expected (nth 2 (haskell-completions-sync-repl-completion-at-point)))
            (should-not expected))
          (let* ((haskell-completions-complete-operators t))
            (setq expected (nth 2 (haskell-completions-sync-repl-completion-at-point)))
            (should expected))))
    (progn
      ;; Remove mocks
      (ad-deactivate 'haskell-completions-sync-complete-repl-mock)
      (ad-deactivate 'haskell-session-maybe-mock)
      (ad-deactivate 'haskell-process-cmd-mock)
      (ad-deactivate 'haskell-interactive-process-mock))))


(provide 'haskell-completions-tests)
;;; haskell-completions-tests.el ends here
