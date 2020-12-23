;;; haskell-mode-tests.el ---  -*- lexical-binding: t -*-

;; Copyright (c) 2014 Chris Done. All rights reserved.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'haskell-mode)
(require 'haskell-test-utils)

(ert-deftest haskell-mode-ident-at-point-empty ()
  (should (with-temp-buffer
            (haskell-mode)
            (eq nil (haskell-ident-at-point)))))

(ert-deftest haskell-mode-ident-pos-at-point-empty ()
  (should (with-temp-buffer
            (haskell-mode)
            (eq nil (haskell-ident-pos-at-point)))))

(ert-deftest haskell-mode-spanable-pos-at-point-empty-spanable ()
  (should (with-temp-buffer
            (haskell-mode)
            (eq nil (haskell-spanable-pos-at-point)))))

(ert-deftest haskell-mode-ident-at-point-aftercolons ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "foo ::")
            (string= "::" (haskell-ident-at-point)))))

(ert-deftest haskell-mode-ident-pos-at-point-aftercolons ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "foo ::")
            (not (eq nil (haskell-ident-pos-at-point))))))

(ert-deftest haskell-mode-ident-at-point-beforetype ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "foo ::")
            (save-excursion (insert " bar -> baz"))
            (string= "::" (haskell-ident-at-point)))))

(ert-deftest haskell-mode-ident-pos-at-point-beforetype ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "foo ::")
            (save-excursion (insert " bar -> baz"))
            (not (eq nil (haskell-ident-pos-at-point))))))

(ert-deftest haskell-mode-spanable-pos-at-point-beforetype ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "foo ::")
            (save-excursion (insert " bar -> baz"))
            (not (eq nil (haskell-spanable-pos-at-point))))))

(ert-deftest haskell-mode-ident-at-point-single ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "a")
            (string= "a" (haskell-ident-at-point)))))

(ert-deftest haskell-mode-ident-at-point-constructor ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "Hello123")
            (string= "Hello123" (haskell-ident-at-point)))))

(ert-deftest haskell-mode-ident-at-point-in-string ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "\"Hello\"")
            (goto-char (1- (point-max)))
            (eq nil (haskell-ident-at-point)))))

(ert-deftest haskell-mode-ident-at-point-in-commas ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert ",Hello,")
            (goto-char (1- (point-max)))
            (string= "Hello" (haskell-ident-at-point)))))

(ert-deftest haskell-mode-ident-at-point-var ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "hello")
            (string= "hello" (haskell-ident-at-point)))))

(ert-deftest haskell-mode-ident-at-point-prime ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "f'")
            (string= "f'" (haskell-ident-at-point)))))

(ert-deftest haskell-mode-ident-at-point-prime2 ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "f5oo'")
            (string= "f5oo'" (haskell-ident-at-point)))))

(ert-deftest haskell-mode-ident-at-point-prime3 ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "f'oo'")
            (string= "f'oo'" (haskell-ident-at-point)))))

(ert-deftest haskell-mode-ident-at-point-prime4 ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "f'oo'")
            (goto-char (point-min))
            (string= "f'oo'" (haskell-ident-at-point)))))

(ert-deftest haskell-mode-ident-at-point-prime5 ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "f'o6o'")
            (goto-char (+ 1 (point-min)))
            (string= "f'o6o'" (haskell-ident-at-point)))))

(ert-deftest haskell-mode-ident-at-point-prime6 ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "f'oo'")
            (goto-char (+ 2 (point-min)))
            (string= "f'oo'" (haskell-ident-at-point)))))

(ert-deftest haskell-mode-ident-at-point-underscore ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "f_oo_")
            (goto-char (+ 2 (point-min)))
            (string= "f_oo_" (haskell-ident-at-point)))))

(ert-deftest haskell-mode-ident-at-point-underscore2 ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "_oo_")
            (goto-char (point-min))
            (string= "_oo_" (haskell-ident-at-point)))))

(ert-deftest haskell-mode-ident-at-point-underscore3 ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "o3o_")
            (string= "o3o_" (haskell-ident-at-point)))))

(ert-deftest haskell-mode-ident-at-point-unicode ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "åöèą5ċōïá")
            (string= "åöèą5ċōïá" (haskell-ident-at-point)))))

(ert-deftest haskell-mode-ident-at-point-unicode2 ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "Äöèąċōïá")
            (string= "Äöèąċōïá" (haskell-ident-at-point)))))

(ert-deftest haskell-mode-ident-pos-at-point-unicode ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "åöèą5ċōïá")
            (equal (cons (point-min) (point-max))
                   (haskell-ident-pos-at-point)))))

(ert-deftest haskell-mode-ident-pos-at-point-unicode2 ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "Äöèąċōïá")
            (equal (cons (point-min) (point-max))
                   (haskell-ident-pos-at-point)))))

(ert-deftest haskell-mode-spanable-pos-at-point-unicode ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "åöèą5ċōïá")
            (equal (cons (point-min) (point-max))
                   (haskell-spanable-pos-at-point)))))

(ert-deftest haskell-mode-spanable-pos-at-point-unicode2 ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "Äöèąċōïá")
            (equal (cons (point-min) (point-max))
                   (haskell-spanable-pos-at-point)))))

(ert-deftest haskell-mode-ident-at-point-in-backticks ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "`foo`")
            (backward-char 2)
            (string= "foo" (haskell-ident-at-point)))))

(ert-deftest haskell-mode-ident-pos-at-point-in-backticks ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "`foo`")
            (backward-char 2)
            (equal (cons (1+ (point-min)) (1- (point-max)))
                   (haskell-ident-pos-at-point)))))

(ert-deftest haskell-mode-spanable-pos-at-point-in-backticks ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "`foo`")
            (backward-char 2)
            (equal (cons (point-min) (point-max))
                   (haskell-spanable-pos-at-point)))))

(ert-deftest haskell-mode-ident-at-point-operators ()
  "Test `haskell-ident-at-point' for all operator cases."
  (with-temp-buffer
    (haskell-mode)
    ;; point at the end of unqualified operator
    (insert ">>")
    (should (string= ">>" (haskell-ident-at-point)))
    ;; point in the middle of unqualified operator
    (save-excursion
      (insert "=")
      (insert "\n"))
    (should (string= ">>=" (haskell-ident-at-point)))
    (forward-line)
    ;; point at the end of qualified operator
    (insert "Control.Monad.>>=")
    (should (string= "Control.Monad.>>=" (haskell-ident-at-point)))
    ;; point at the beginning of qualified operator
    (goto-char (line-beginning-position))
    (should (string= "Control.Monad.>>=" (haskell-ident-at-point)))
    ;; point in the middle of qualified part of operator
    (forward-char)
    (should (string= "Control.Monad.>>=" (haskell-ident-at-point)))
    ;; point atfer `.` dot in qualified part of operator
    (search-forward ".")
    (should (string= "Control.Monad.>>=" (haskell-ident-at-point)))
    ;; point at `.` dot in qualified part
    (backward-char)
    (should (string= "Control.Monad.>>=" (haskell-ident-at-point)))
    (goto-char (line-end-position))
    (insert "\n")
    ;; Overloaded `.` dot tests.
    ;; point at operator's `.` dot preceded by delimiting `.` dot
    (insert "Data.Aeson.")
    (save-excursion
      (insert ".:"))
    (should (string= "Data.Aeson..:" (haskell-ident-at-point)))
    (forward-char)
    (should (string= "Data.Aeson..:" (haskell-ident-at-point)))
    ;; surrounding parentheses
    (goto-char (line-beginning-position))
    (save-excursion (insert "("))
    (should (eq nil (haskell-ident-at-point)))
    (goto-char (line-end-position))
    (insert ")")
    (should (eq nil (haskell-ident-at-point)))
    (backward-char 2)
    (should (string= "Data.Aeson..:" (haskell-ident-at-point)))))

(defun check-fill (expected initial)
  "Check using ERT if `fill-paragraph' over `initial' gives
`expected' result. Cursor fill be positioned at '@' character or at
the beginning of the buffer.

`fill-column' will be set to 10 so that it is easy to spot issues."
  (should (equal expected
                 (with-temp-buffer
                   (haskell-mode)
                   (setq fill-column 10)
                   (dolist (line initial)
                     (insert line)
                     (insert "\n"))
                   (goto-char (point-min))
                   (skip-chars-forward "^@")
                   (if (eobp)
                       (goto-char (point-min))
                     (delete-char 1))
                   (fill-paragraph nil)
                   (split-string (buffer-substring-no-properties (point-min) (1- (point-max))) "\n")))))

(ert-deftest fill-comment-1 ()
  (check-fill '("{- a -}")
              '("{- @a -}")))

(ert-deftest fill-comment-2 ()
  (check-fill '("{- a b c d e"
                " f g h i j"
                " k -}")
              '("{- @a b c d e f g h i j k -}")))

(ert-deftest fill-comment-3 ()
  (check-fill '("{-"
                "a"
                "-}")
              '("{-"
                "@a"
                "-}")))

(ert-deftest fill-comment-4 ()
  (check-fill '("{-"
                "a b c d e"
                "f g h i-}")
              '("{-"
                "@a"
                "b"
                "c"
                "d e f g h i-}")))

(ert-deftest fill-comment-5 ()
  (check-fill '("    {-"
                " a b c d e"
                " f g h i"
                "    -}")
              '("    {-" " @a b c d e f g h i" "    -}")))

(ert-deftest fill-comment-6 ()
  (check-fill '("  -- a b c"
                "  -- d e f"
                "  -- g h i"
                "  -- j k l"
                "  -- m n o"
                "  -- p q r"
                "  -- s t u"
                "  -- v")
              '("  -- @a b c d e f g h i j k l m n o p q r s t u v")))

(ert-deftest fill-comment-7 ()
  (check-fill '("  --  a b"
                "  --  c d"
                "  --  e f"
                "  --  g h"
                "  --  i j")
              '("  --  @a b c d e f g h i j ")))

(ert-deftest fill-comment-8 ()
  "Note: first letter of second line should be in the same column
as first letter in the first line.

Also should respect 10 column fill."
  :expected-result :failed
  (check-fill '("  {-  a b"
                "      c d"
                "      e f"
                "      g h"
                "      i j"
                "   -}")
              '("  {-  @a b c d e f g h i j"
                "  -}")))

(ert-deftest fill-comment-9 ()
  "Note: first letter in the second line position should be kept
as defined, just the content should move properly.

Also should respect 10 column fill."
  :expected-result :failed
  (check-fill '("  {-  a b"
                "     c d e"
                "     f g h"
                "     i j"
                "   -}")
              '("  {-  @a"
                "     b c d e f g h i j"
                "  -}")))

(ert-deftest fill-comment-10 ()
  "Note: first letter in the second line position should be kept
as defined, just the content should move properly. Following
lines should take position from second line.

Also should respect 10 column fill."
  :expected-result :failed
  (check-fill '("  {-  a b"
                "     c d e"
                "     f g h"
                "     i j"
                "   -}")
              '("  {-  @a"
                "     b c d e"
                "  f g h"
                "                    i j"
                "  -}")))

(ert-deftest fill-comment-11 ()
  "Note: first letter in the second line position should be kept
as defined, just the content should move properly.

Also should respect 10 column fill."
  (check-fill '("  --  a b"
                "  -- c d e"
                "  -- f g h"
                "  -- i j")
              '("  --  @a"
                "  -- b c d e f g h i j")))

(ert-deftest fill-comment-12 ()
  "Note: first letter in the second line position should be kept
as defined, just the content should move properly. Following
lines should take position from second line.

Also should respect 10 column fill."
  (check-fill '("  --  a b"
                "  -- c d e"
                "  -- f g h"
                "  -- i j")
              '("  --  @a"
                "  -- b c d e"
                "--f g h"
                "        --            i j")))

(ert-deftest fill-comment-haddock-1 ()
  (check-fill '("-- | a b c"
                "-- d")
              '("-- @| a b c d")))

(ert-deftest fill-comment-haddock-2 ()
  (check-fill '("-- | a b c"
                "-- d e")
              '("-- @| a b c d"
                "-- e")))

(ert-deftest insert-scc-feasible ()
  "insert an SCC where it's possible to do so"
  (should (with-temp-buffer
            (insert "hello world")
            (goto-char 6)
            (haskell-mode-toggle-scc-at-point)
            (string= "hello {-# SCC \"\" #-} world"
                     (buffer-substring 1 (point-max))))))

(ert-deftest insert-scc-infeasible ()
  "insert an SCC where it's not possible to do so"
  (should-error (with-temp-buffer
            (insert "hello world")
            (goto-char 2)
            (haskell-mode-toggle-scc-at-point)
            (string= "hello world"
                     (buffer-substring 1 (point-max))))))

(ert-deftest remove-scc ()
  "insert an SCC where it's possible to do so"
  (should (with-temp-buffer
            (insert "hello {-# SCC \"\" #-} world")
            (goto-char 10)
            (haskell-mode-toggle-scc-at-point)
            (string= "hello world"
                     (buffer-substring 1 (point-max))))))

(ert-deftest forward-sexp-function-1 ()
  "Check if `forward-sexp-function' behaves properly on end of
sexp."
  (should (with-temp-buffer
            (haskell-mode)
            (insert "(foo) bar")
            (goto-char 5)
            (condition-case err
                (progn (forward-sexp)
                       nil)
              (scan-error (equal (cddr err) (list 5 6)))))))

(ert-deftest forward-sexp-function-2 ()
  "Check if `forward-sexp-function' behaves properly on beginning
of sexp."
  (should (with-temp-buffer
            (haskell-mode)
            (insert "(foo) bar")
            (goto-char 1)
            (forward-sexp)
            (eq (point) 6))))

(ert-deftest haskell-forward-sexp-1 ()
  "Check if `haskell-forward-sexp' properly moves over sexps."
  (should (with-temp-buffer
            (insert "foo = bar . baz")
            (goto-char 1)
            (haskell-forward-sexp 4)
            (eq (point) 12))))

(ert-deftest haskell-forward-sexp-2 ()
  "Check if `haskell-forward-sexp' properly moves over sexps."
  (should (with-temp-buffer
            (insert "foo = bar . baz")
            (goto-char 1)
            (haskell-forward-sexp 1)
            (eq (point) 4))))

(ert-deftest haskell-forward-sexp-3 ()
  "Check if `haskell-forward-sexp' properly moves over sexps."
  (should (with-temp-buffer
            (insert "(a b) c = d . e")
            (goto-char 1)
            (haskell-forward-sexp 5)
            (eq (point) 14))))

(ert-deftest haskell-forward-sexp-4 ()
  "Check if `haskell-forward-sexp' properly moves over sexps."
  (should (with-temp-buffer
            (insert "(a b) c = d . e")
            (goto-char 1)
            (haskell-forward-sexp 1)
            (eq (point) 6))))

(ert-deftest backward-sexp ()
  "Check if `forward-sexp-function' behaves properly on
beginning of sexp."
  (should (with-temp-buffer
            (haskell-mode)
            (insert "(foo) bar")
            (goto-char 2)
            (condition-case err
                (progn (backward-sexp)
                       nil)
              (scan-error (equal (cddr err) (list 1 1)))))))

(ert-deftest haskell-backward-sexp ()
  "Check if `haskell-forward-sexp' with negatives arg properly
moves over sexps."
  (should (with-temp-buffer
            (insert "a (b c) = d . e")
            (goto-char 15)
            (haskell-forward-sexp -4)
            (eq (point) 3))))

(ert-deftest haskell-guess-module-name ()
  "Check if `haskell-guess-module-name'."
  (should (equal nil (haskell-guess-module-name-from-file-name "nonmodule.hs")))
  (should (equal "Mod" (haskell-guess-module-name-from-file-name "Mod.hs")))
  (should (equal "Żółw" (haskell-guess-module-name-from-file-name "Żółw.hs")))
  (should (equal "Mod" (haskell-guess-module-name-from-file-name "żółw/Mod.hs")))
  (should (equal "Module" (haskell-guess-module-name-from-file-name "Juicy-pixels/Module.lhs")))
  (should (equal "Mod.Mod.Mod" (haskell-guess-module-name-from-file-name "src/Mod/Mod/Mod.hs")))
  (should (equal "Żółw1.Żółw2.Żółw3" (haskell-guess-module-name-from-file-name "Żółw1/Żółw2/Żółw3.lhs")))
  (should (equal "Mod'X.Mod" (haskell-guess-module-name-from-file-name "c:/Mod'X/Mod.lhs")))
  (should (equal "Mod" (haskell-guess-module-name-from-file-name "Mod.xx/Mod.hs"))))

(defun haskell-generate-tags-test-helper ()
  (with-current-buffer (find-file-noselect "TAGS-test-format")
    (erase-buffer)
    (dolist (arg (sort argv #'string<))
      (insert arg "\n"))
    (save-buffer)
    (kill-buffer)))

(ert-deftest haskell-generate-tags ()
  ;; this test is special for Unix because under Windows the
  ;; invocation is different
  :expected-result (if (equal system-type 'windows-nt)
                       :failed
                     :passed)
  (with-temp-dir-structure
   (("xxx.cabal" . "")
    ("T1.hs" . "i1 :: Int")
    ("src" . (("T2.hs" . "i2 :: Int")))
    (".git" . (("Tx.hs" . "should_not_see_me :: Int"))))
    (with-script-path
     haskell-hasktags-path
     haskell-generate-tags-test-helper
     (haskell-mode-generate-tags)
     (with-current-buffer (find-file-noselect "TAGS-test-format")
       (should (equal "-e\n-x\n./T1.hs\n./src/T2.hs\n"
                      (buffer-substring (point-min) (point-max))))))))

(defun haskell-stylish-haskell-add-first-line ()
  (message-stdout "-- HEADER")
  (let (line)
    (while (setq line (read-stdin))
      (message-stdout line))))

(defun haskell-stylish-haskell-no-change ()
  (let (line)
    (while (setq line (read-stdin))
      (message-stdout line))))

(defun haskell-stylish-haskell-bad-exit-code ()
  (when noninteractive
    (kill-emacs 34)))

(defun haskell-stylish-haskell-error-message ()
  (message-stderr "Something wrong"))

(ert-deftest haskell-stylish-on-save-add-first-line ()
  (with-temp-dir-structure
   (("T.hs" . "main :: IO ()\n"))
    (with-script-path
     haskell-mode-stylish-haskell-path
     haskell-stylish-haskell-add-first-line
     (let ((haskell-stylish-on-save t))
       (with-current-buffer (find-file-noselect "T.hs")
         (goto-char (point-max))
         (save-excursion
           (insert "main = return ()\n"))
         (save-buffer)
         ;; should have header added
         (goto-char (point-min))
         (should (looking-at-p "-- HEADER")))))))

(ert-deftest haskell-stylish-on-save-keep-point ()
  ;; Looks like insert-file-contents under Windows does not keep the
  ;; point as it should.
  :expected-result (if (equal system-type 'windows-nt)
                       :failed
                     :passed)
  (with-temp-dir-structure
   (("T.hs" . "main :: IO ()\n"))
    (with-script-path
     haskell-mode-stylish-haskell-path
     haskell-stylish-haskell-add-first-line
     (let ((haskell-stylish-on-save t))
       (with-current-buffer (find-file-noselect "T.hs")
         (goto-char (point-max))
         (save-excursion
           (insert "main = return ()\n"))
         (save-buffer)
         ;; should keep pointer in place
         (should (looking-at-p "main = return")))))))

(ert-deftest haskell-stylish-on-save-no-change ()
  (with-temp-dir-structure
   (("T.hs" . "main :: IO ()"))
    (with-script-path
     haskell-mode-stylish-haskell-path
     haskell-stylish-haskell-no-change
     (let ((haskell-stylish-on-save t))
       (with-current-buffer (find-file-noselect "T.hs")
         (insert "main = return ()\n")
         (save-buffer)
         (goto-char (point-min))
         (should (looking-at-p "main = return")))))))

(ert-deftest haskell-stylish-on-save-bad-exit-code ()
  (with-temp-dir-structure
   (("T.hs" . "main :: IO ()"))
    (with-script-path
     haskell-mode-stylish-haskell-path
     haskell-stylish-haskell-bad-exit-code
     (let ((haskell-stylish-on-save t))
       (with-current-buffer (find-file-noselect "T.hs")
         (insert "main = return ()\n")
         (save-buffer)
         (goto-char (point-min))
         (should (looking-at-p "main = return ()")))))))

(ert-deftest haskell-stylish-on-save-error-message ()
  (with-temp-dir-structure
   (("T.hs" . "main :: IO ()"))
    (with-script-path
     haskell-mode-stylish-haskell-path
     haskell-stylish-haskell-error-message
     (let ((haskell-stylish-on-save t))
       (with-current-buffer (find-file-noselect "T.hs")
         (insert "main = return ()\n")
         (save-buffer)
         (goto-char (point-min))
         (should (looking-at-p "main = return ()")))))))

(provide 'haskell-mode-tests)
