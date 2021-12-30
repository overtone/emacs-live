;;; clojure-mode-safe-eval-test.el --- Clojure Mode: safe eval test suite  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2021 Bozhidar Batsov <bozhidar@batsov.dev>
;; Copyright (C) 2021 Rob Browning <rlb@defaultvalue.org>

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

;; The safe eval test suite of Clojure Mode

;;; Code:
(require 'clojure-mode)
(require 'buttercup)

(describe "put-clojure-indent safe-local-eval-function property"
  (it "should be set to clojure--valid-put-clojure-indent-call-p"
      (expect (get 'put-clojure-indent 'safe-local-eval-function)
              :to-be 'clojure--valid-put-clojure-indent-call-p)))

(describe "clojure--valid-put-clojure-indent-call-p"
  (it "should approve valid forms"
      (expect (clojure--valid-put-clojure-indent-call-p
               '(put-clojure-indent 'foo 1)))
      (expect (clojure--valid-put-clojure-indent-call-p
               '(put-clojure-indent 'foo :defn)))
      (expect (clojure--valid-put-clojure-indent-call-p
               '(put-clojure-indent 'foo :form)))
      (expect (clojure--valid-put-clojure-indent-call-p
               '(put-clojure-indent 'foo '(1))))
      (expect (clojure--valid-put-clojure-indent-call-p
               '(put-clojure-indent 'foo '(:defn))))
      (expect (clojure--valid-put-clojure-indent-call-p
               '(put-clojure-indent 'foo '(:form))))
      (expect (clojure--valid-put-clojure-indent-call-p
               '(put-clojure-indent 'foo '(1 1))))
      (expect (clojure--valid-put-clojure-indent-call-p
               '(put-clojure-indent 'foo '(2 :form :form (1))))))
  (it "should reject invalid forms"
      (expect (clojure--valid-put-clojure-indent-call-p
               '(put-clojure-indent 1 1))
              :to-throw 'error)
      (expect (clojure--valid-put-clojure-indent-call-p
               '(put-clojure-indent 'foo :foo))
              :to-throw 'error)
      (expect (clojure--valid-put-clojure-indent-call-p
               '(put-clojure-indent 'foo (:defn)))
              :to-throw 'error)
      (expect (clojure--valid-put-clojure-indent-call-p
               '(put-clojure-indent 'foo '(:foo)))
              :to-throw 'error)
      (expect (clojure--valid-put-clojure-indent-call-p
               '(put-clojure-indent 'foo '(1 :foo)))
              :to-throw 'error)
      (expect (clojure--valid-put-clojure-indent-call-p
               '(put-clojure-indent 'foo '(1 "foo")))
              :to-throw 'error)))

(provide 'clojure-mode-safe-eval-test)

;;; clojure-mode-safe-eval-test.el ends here
