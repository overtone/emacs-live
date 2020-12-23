;;; cider-eval-sexp-fu.el --- Briefly highlights an evaluated sexp.

;; Adapted from Sam Aaron's code found in emacs-live in order to
;; be distributable as a package by syl20bnr.

;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; Keywords: languages, clojure, cider
;; Created: 20 Mar 2015
;; Version: 1.2
;; Package-Requires: ((emacs "24") (eval-sexp-fu "0.5.0"))

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

;;; Commentary:

;; Tiny feature adding support for cider eval functions.
;; See `eval-sexp-fu' help for more info on how to configure the
;; flash behavior.

;;; Code:

(require 'eval-sexp-fu)

(defun cider-esf--bounds-of-last-sexp ()
  "Return the bounds of the defun around point.

Copies semantics directly from `cider-last-sexp' to ensure highlighted
area is identical to that which is evaluated."
  (cons (save-excursion
          (backward-sexp)
          (point))
        (point)))

(defun cider-esf--initialize-cider ()
  "Initialize the CIDER integration for eval-sexp-fu."
  (define-eval-sexp-fu-flash-command cider-eval-last-sexp
    (eval-sexp-fu-flash (cider-esf--bounds-of-last-sexp)))
  (define-eval-sexp-fu-flash-command cider-pprint-eval-last-sexp
    (eval-sexp-fu-flash (cider-esf--bounds-of-last-sexp)))
  (define-eval-sexp-fu-flash-command cider-eval-defun-at-point
    (eval-sexp-fu-flash (let ((bounds (cider-defun-at-point 'bounds)))
                          (cons (car bounds) (cadr bounds)))))

  ;; Defines:
  ;; `eval-sexp-fu-cider-sexp-inner-list',
  ;; `eval-sexp-fu-cider-sexp-inner-sexp'
  ;; and the pprint variants respectively.
  (define-eval-sexp-fu-eval-sexp eval-sexp-fu-cider-eval-sexp
    cider-eval-last-sexp)
  (define-eval-sexp-fu-eval-sexp eval-sexp-fu-cider-pprint-eval-sexp
    cider-pprint-eval-last-sexp))

(eval-after-load 'cider
  '(cider-esf--initialize-cider))

(provide 'cider-eval-sexp-fu)

;;; cider-eval-sexp-fu.el ends here
