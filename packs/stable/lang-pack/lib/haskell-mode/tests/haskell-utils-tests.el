;;; haskell-utils-tests.el --- Tests for Haskell utilities package  -*- lexical-binding: t -*-

;; Copyright © 2016 Arthur Fayzrakhmanov. All rights reserved.

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

;; This package provides regression tests for haskell-utils package.

;;; Code:

(require 'ert)
(require 'haskell-test-utils)
(require 'haskell-utils)

(ert-deftest simple-import-parse ()
  (should (equal "A.B.C"
                 (with-temp-buffer
                   (insert-lines "import A.B.C")
                   (goto-char (point-min))
                   (forward-line 0)
                   (haskell-utils-parse-import-statement-at-point)))))

(ert-deftest qualified-import-parse ()
  (should (equal "A.B.C"
                 (with-temp-buffer
                   (insert-lines "import qualified A.B.C")
                   (goto-char (point-min))
                   (forward-line 0)
                   (haskell-utils-parse-import-statement-at-point)))))

(ert-deftest qualified-as-import-parse ()
  (should (equal "AAA.Bc.Cx"
                 (with-temp-buffer
                   (insert-lines "import qualified AAA.Bc.Cx as Something")
                   (goto-char (point-min))
                   (forward-line 0)
                   (haskell-utils-parse-import-statement-at-point)))))

(ert-deftest international-characters-import-parse ()
  (should (equal "Żółć"
                 (with-temp-buffer
                   (insert-lines "import Żółć")
                   (goto-char (point-min))
                   (forward-line 0)
                   (haskell-utils-parse-import-statement-at-point)))))

(ert-deftest commented-out-import-parse ()
  (should (equal nil
                 (with-temp-buffer
                   (insert-lines "-- import Nothing")
                   (goto-char (point-min))
                   (forward-line 0)
                   (haskell-utils-parse-import-statement-at-point)))))

(ert-deftest non-import-import-parse ()
  (should (equal nil
                 (with-temp-buffer
                   (insert-lines "something import Nothing")
                   (goto-char (point-min))
                   (forward-line 0)
                   (haskell-utils-parse-import-statement-at-point)))))

(ert-deftest many-spaces-import-parse ()
  (should (equal "M"
                 (with-temp-buffer
                   (insert-lines "\t import\t qualified \t\tM\tas G")
                   (goto-char (point-min))
                   (forward-line 0)
                   (haskell-utils-parse-import-statement-at-point)))))

(ert-deftest using-underscores-import-parse ()
  (should (equal "Module_1.S_3_3_"
                 (with-temp-buffer
                   (insert-lines "import Module_1.S_3_3_")
                   (goto-char (point-min))
                   (forward-line 0)
                   (haskell-utils-parse-import-statement-at-point)))))

(ert-deftest slightly-malformed-import-parse ()
  (should (equal "q.Module...qwerqwe..."
                 (with-temp-buffer
                   (insert-lines "import q.Module...qwerqwe...")
                   (goto-char (point-min))
                   (forward-line 0)
                   (haskell-utils-parse-import-statement-at-point)))))

(ert-deftest package-import-parse ()
  (should (equal "B"
                 (with-temp-buffer
                   (insert-lines "import \"package-1.2.3\" B")
                   (goto-char (point-min))
                   (forward-line 0)
                   (haskell-utils-parse-import-statement-at-point)))))

(ert-deftest safe-haskell-import-parse ()
  (should (equal "B"
                 (with-temp-buffer
                   (insert-lines "import safe B")
                   (goto-char (point-min))
                   (forward-line 0)
                   (haskell-utils-parse-import-statement-at-point)))))

(ert-deftest full-import-parse ()
  (should (equal "Data.Char.Unicode_v_7"
                 (with-temp-buffer
                   (insert-lines "import safe qualified \"unicode-7.0\" Data.Char.Unicode_v_7 as U (func)")
                   (goto-char (point-min))
                   (forward-line 0)
                   (haskell-utils-parse-import-statement-at-point)))))

(ert-deftest type-at-command-composition ()
  "Test `haskell-utils-compose-type-at-command'.
Test only position conversion to line and column numbers, do not
test last string compontent, it is used in `:type-at` command to
provide user friendly output only and could be any string, even
empty one. Very likely the way how its composed for multilne
strings will change in future."
  (with-temp-buffer
    (insert-lines "module A where"
                  ""
                  "int :: Int"
                  "int = 369"
                  ""
                  "act ="
                  "  do print int"
                  "     return int")
    (goto-char (point-min))
    (let (test-a-points
          test-b-points
          test-a-result
          test-b-result)
      ;; go to third line, e.g. `int` definition
      (forward-line 3)
      (setq test-a-points (point))
      ;; go to at the end of `int` definition, i.e. point stands at whitespace
      (forward-char 3)
      (setq test-a-points `(,test-a-points . ,(point)))
      (goto-char (line-beginning-position))
      ;; go to do-block line
      (forward-line 3)
      ;; go to `do` keyword beginning
      (forward-char 2)
      (setq test-b-points (point))
      ;; go to the end of do-block
      (goto-char (point-max))
      (setq test-b-points `(,test-b-points . ,(point)))
      (setq test-a-result
            (haskell-utils-compose-type-at-command test-a-points))
      (setq test-b-result
            (haskell-utils-compose-type-at-command test-b-points))
      (should (string-prefix-p ":type-at nil 4 1 4 4" test-a-result))
      (should (string-prefix-p ":type-at nil 7 3 8 16" test-b-result)))))

(ert-deftest parse-repl-response ()
  "Test `haskell-utils-repl-response-error-status' function."
  (let* ((t1-str "unknown command ':type-at'\nuse :? for help.")
         (t2-str "\n<interactive>:3:5: Not in scope: ‘x’")
         (t3-str "Couldn't guess that module name. Does it exist?")
         (t4-str "Hello World!")
         (t5-str " ")
         (t6-str "")
         (t7-str "\n\n\n\n")
         (r1 (haskell-utils-repl-response-error-status t1-str))
         (r2 (haskell-utils-repl-response-error-status t2-str))
         (r3 (haskell-utils-repl-response-error-status t3-str))
         (r4 (haskell-utils-repl-response-error-status t4-str))
         (r5 (haskell-utils-repl-response-error-status t5-str))
         (r6 (haskell-utils-repl-response-error-status t6-str))
         (r7 (haskell-utils-repl-response-error-status t7-str)))
    (should (equal r1 'unknown-command))
    (should (equal r2 'interactive-error))
    (should (equal r3 'option-missing))
    (should (equal r4 'no-error))
    (should (equal r5 'no-error))
    (should (equal r6 'no-error))
    (should (equal r7 'no-error))))

(ert-deftest reduce-strign ()
  "Test `haskell-utils-reduce-strign' command.
Whitespace sequences at beginning of lines should be replaced
with single whitespace, all newline characters should be
removed."
  (should (string-equal "" (haskell-utils-reduce-string "\n")))
  (should (string-equal "" (haskell-utils-reduce-string "\r\n")))
  (should (string-equal " " (haskell-utils-reduce-string "    \n")))
  (should (string-equal
           "TestTest"
           (haskell-utils-reduce-string "Test\nTest")))
  (should (string-equal
           "Test Test"
           (haskell-utils-reduce-string "Test\n     Test")))
  (should (string-equal
           " Test Test"
           (haskell-utils-reduce-string "   Test\r\n     Test")))
  (should (string-equal
           " TestTest"
           (haskell-utils-reduce-string "  Test\r\nTest")))
  (should (string-equal
           " TestTest Test test"
           (haskell-utils-reduce-string " Test\r\nTest\n    Test test"))))

(ert-deftest post-command-hooks ()
  "Test commands related `haskell-utils-async-post-command-flag'.
Tests flag updates and `post-command-hook' cleanup."
  (with-temp-switch-to-buffer
    ;; set some random value to flag to test that it will be reseted
    (setq haskell-utils-async-post-command-flag "non nil")
    (haskell-utils-async-watch-changes)
    ;; now flag should be empty
    (should (null haskell-utils-async-post-command-flag))
    ;; execute some commands
    (save-excursion (insert "Hello World!"))
    (execute-kbd-macro (kbd "SPC"))
    ;; now flag should not be empty
    (should (not (null haskell-utils-async-post-command-flag)))
    ;; check that hook was installed
    (should (cl-member #'haskell-utils-async-update-post-command-flag
                       post-command-hook
                       :test #'equal))
    ;; check that flag was cleaned up
    (haskell-utils-async-stop-watching-changes (current-buffer))
    (should (null haskell-utils-async-post-command-flag))
    ;; check that hook was removed
    (should (not (cl-member #'haskell-utils-async-update-post-command-flag
                            post-command-hook
                            :test #'equal)))))

;;; haskell-utils-tests.el ends here
