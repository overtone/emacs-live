;;; clojure-mode-sexp-test.el --- Clojure Mode: sexp tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2016 Artur Malabarba <artur@endlessparentheses.com>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'clojure-mode)
(require 'ert)

(ert-deftest test-sexp-with-commas ()
  (with-temp-buffer
    (insert "[], {}, :a, 2")
    (clojure-mode)
    (goto-char (point-min))
    (clojure-forward-logical-sexp 1)
    (should (looking-at-p " {}, :a, 2"))
    (clojure-forward-logical-sexp 1)
    (should (looking-at-p " :a, 2"))))

(ert-deftest test-sexp ()
  (with-temp-buffer
    (insert "^String #macro ^dynamic reverse")
    (clojure-mode)
    (clojure-backward-logical-sexp 1)
    (should (looking-at-p "\\^String \\#macro \\^dynamic reverse"))
    (clojure-forward-logical-sexp 1)
    (should (looking-back "\\^String \\#macro \\^dynamic reverse"))
    (insert " ^String biverse inverse")
    (clojure-backward-logical-sexp 1)
    (should (looking-at-p "inverse"))
    (clojure-backward-logical-sexp 2)
    (should (looking-at-p "\\^String \\#macro \\^dynamic reverse"))
    (clojure-forward-logical-sexp 2)
    (should (looking-back "\\^String biverse"))
    (clojure-backward-logical-sexp 1)
    (should (looking-at-p "\\^String biverse"))))

(ert-deftest test-buffer-corners ()
  (with-temp-buffer
    (insert "^String reverse")
    (clojure-mode)
    ;; Return nil and don't error
    (should-not (clojure-backward-logical-sexp 100))
    (should (looking-at-p "\\^String reverse"))
    (should-not (clojure-forward-logical-sexp 100))
    (should (looking-at-p "$")))
  (with-temp-buffer
    (clojure-mode)
    (insert "(+ 10")
    (should-error (clojure-backward-logical-sexp 100))
    (goto-char (point-min))
    (should-error (clojure-forward-logical-sexp 100))
    ;; Just don't hang.
    (goto-char (point-max))
    (should-not (clojure-forward-logical-sexp 1))
    (erase-buffer)
    (insert "(+ 10")
    (newline)
    (erase-buffer)
    (insert "(+ 10")
    (newline-and-indent)))

(provide 'clojure-mode-sexp-test)

;;; clojure-mode-sexp-test.el ends here
