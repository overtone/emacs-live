;;; haskell-mode-tests.el ---

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

(ert-deftest empty ()
  (should (with-temp-buffer
            (haskell-mode)
            (eq nil (haskell-ident-at-point)))))

(ert-deftest empty-pos ()
  (should (with-temp-buffer
            (haskell-mode)
            (eq nil (haskell-ident-pos-at-point)))))

(ert-deftest empty-spanable ()
  (should (with-temp-buffer
            (haskell-mode)
            (eq nil (haskell-spanable-pos-at-point)))))

(ert-deftest aftercolons ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "foo ::")
            (eq nil (haskell-ident-at-point)))))

(ert-deftest aftercolons-pos ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "foo ::")
            (eq nil (haskell-ident-pos-at-point)))))

(ert-deftest beforetype ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "foo ::")
            (save-excursion (insert " bar -> baz"))
            (eq nil (haskell-ident-at-point)))))

(ert-deftest beforetype-pos ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "foo ::")
            (save-excursion (insert " bar -> baz"))
            (eq nil (haskell-ident-pos-at-point)))))

(ert-deftest beforetype-spanable ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "foo ::")
            (save-excursion (insert " bar -> baz"))
            (eq nil (haskell-spanable-pos-at-point)))))

(ert-deftest single ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "a")
            (string= "a" (haskell-ident-at-point)))))

(ert-deftest constructor ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "Hello123")
            (string= "Hello123" (haskell-ident-at-point)))))

(ert-deftest in-string ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "\"Hello\"")
            (goto-char (1- (point-max)))
            (string= "Hello" (haskell-ident-at-point)))))

(ert-deftest in-commas ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert ",Hello,")
            (goto-char (1- (point-max)))
            (string= "Hello" (haskell-ident-at-point)))))

(ert-deftest var ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "hello")
            (string= "hello" (haskell-ident-at-point)))))

(ert-deftest prime ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "f'")
            (string= "f'" (haskell-ident-at-point)))))

(ert-deftest prime2 ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "f5oo'")
            (string= "f5oo'" (haskell-ident-at-point)))))

(ert-deftest prime3 ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "f'oo'")
            (string= "f'oo'" (haskell-ident-at-point)))))

(ert-deftest prime4 ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "f'oo'")
            (goto-char (point-min))
            (string= "f'oo'" (haskell-ident-at-point)))))

(ert-deftest prime5 ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "f'o6o'")
            (goto-char (+ 1 (point-min)))
            (string= "f'o6o'" (haskell-ident-at-point)))))

(ert-deftest prime6 ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "f'oo'")
            (goto-char (+ 2 (point-min)))
            (string= "f'oo'" (haskell-ident-at-point)))))

(ert-deftest underscore ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "f_oo_")
            (goto-char (+ 2 (point-min)))
            (string= "f_oo_" (haskell-ident-at-point)))))


(ert-deftest underscore2 ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "_oo_")
            (goto-char (point-min))
            (string= "_oo_" (haskell-ident-at-point)))))

(ert-deftest underscore3 ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "o3o_")
            (string= "o3o_" (haskell-ident-at-point)))))

(ert-deftest unicode ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "åöèą5ċōïá")
            (string= "åöèą5ċōïá" (haskell-ident-at-point)))))

(ert-deftest unicode2 ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "Äöèąċōïá")
            (string= "Äöèąċōïá" (haskell-ident-at-point)))))            

(ert-deftest unicode-pos ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "åöèą5ċōïá")
            (equal (cons (point-min) (point-max)) (haskell-ident-pos-at-point)))))

(ert-deftest unicode2-pos ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "Äöèąċōïá")
            (equal (cons (point-min) (point-max)) (haskell-ident-pos-at-point)))))

(ert-deftest unicode-spanable ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "åöèą5ċōïá")
            (equal (cons (point-min) (point-max)) (haskell-spanable-pos-at-point)))))

(ert-deftest unicode2-spanable ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "Äöèąċōïá")
            (equal (cons (point-min) (point-max)) (haskell-spanable-pos-at-point)))))

(ert-deftest ident-in-backticks ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "`foo`")
            (backward-char 2)
            (string= "foo" (haskell-ident-at-point)))))

(ert-deftest ident-pos-in-backticks ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "`foo`")
            (backward-char 2)
            (equal (cons (1+ (point-min)) (1- (point-max)))
                   (haskell-ident-pos-at-point)))))

(ert-deftest spanable-pos-in-backticks ()
  (should (with-temp-buffer
            (haskell-mode)
            (insert "`foo`")
            (backward-char 2)
            (equal (cons (point-min) (point-max))
                   (haskell-spanable-pos-at-point)))))


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
		"f g h i j"
		"k -}")
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
		"f g h i"
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
  :expected-result :failed
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
  :expected-result :failed
  (check-fill '("  --  a b"
		"  -- c d e"
		"  -- f g h"
		"  -- i j")
	      '("  --  @a"
		"  -- b c d e"
		"--f g h"
		"        --            i j")))

(provide 'haskell-mode-tests)
