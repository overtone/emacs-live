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

(provide 'haskell-mode-tests)
