;;; tests/navigation.el --- Some tests for js2-mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2009, 2011-2015  Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'js2-mode)

(cl-defun js2-navigation-helper (buffer-content &optional expected-point (point-offset 1) expected-error-msg)
  (with-temp-buffer
    (insert buffer-content)
    (let ((start-point (or (- (point) point-offset)))
          actual-error-msg)
      (js2-mode)
      (goto-char start-point)
      (if expected-error-msg
          (setq actual-error-msg
                (cadr (should-error (js2-jump-to-definition) :type 'error)))
        (js2-jump-to-definition))
      (print (format "%d %d" (point) start-point))
      (should (= (point) (or expected-point start-point)))
      (should (string= actual-error-msg expected-error-msg)))))

(ert-deftest js2-jump-to-var ()
  (js2-navigation-helper "var soup = 2; soup" 5))

(ert-deftest js2-jump-to-function ()
  (js2-navigation-helper "function aFunction() {}; aFunction" 1))

(ert-deftest js2-jump-to-function-parameters ()
  (js2-navigation-helper "var p1 = 4; function aFunction(p1, p2) {p1};" 32 4))

(ert-deftest js2-jump-to-object-property ()
  (js2-navigation-helper "var aObject = {prop1: 3, prop2: \"hello\"}; aObject.prop1" 16))

(ert-deftest js2-no-jump-to-object-property ()
  (js2-navigation-helper "var aObject = {prop1: 3, prop2: \"hello\"}; anotherObject.prop1"
                         61 1 "No jump location found"))

(ert-deftest js2-jump-to-nested-property ()
  (js2-navigation-helper "var aObject = {prop1: {prop2: { prop3: 4}}}; aObject.prop1.prop2.prop3" 33))

(ert-deftest js2-jump-to-object ()
  (js2-navigation-helper "var aObject = {prop1: 3, prop2: \"hello\"}; aObject.prop1" 5 13))

(ert-deftest js2-jump-to-property ()
  (js2-navigation-helper "aObject.func = functon(){};aObject.func" 9))

(ert-deftest js2-jump-to-property-object-property ()
  (js2-navigation-helper "aObject.value = {prop:1};aObject.value.prop" 18))

(ert-deftest js2-jump-to-function-definition-inside-object-value ()
  (js2-navigation-helper
   "function aFunction(p1, p2) {return p1+p2}; module.exports = {aFunction:aFunction};" 1 6))

(ert-deftest js2-no-jump-to-function-definition-object-property ()
  (js2-navigation-helper
   "function aFunction(p1, p2) {return p1+p2}; module.exports = {aFunction:aFunction};"
   67 16 "Node is not a supported jump node"))

(ert-deftest js2-jump-to-function-inside-property-value-syntax ()
  (js2-navigation-helper "function aFunction(p1, p2) {return p1+p2}; module.exports = {aFunction};" 1 6))


;; forward-sexp

(defun js2-test-forward-sexp (pre-point skipped after-sexp)
  "Test `js2-mode-forward-sexp'.
The test buffer's contents are set to the concatenation of
PRE-POINT, SKIPPED, and AFTER-SEXP.  Point is placed after
PRE-POINT, and `forward-sexp' is called.  Then point should be
after SKIPPED."
  (with-temp-buffer
    (insert pre-point skipped after-sexp)
    (js2-mode)
    (goto-char (1+ (length pre-point)))
    (forward-sexp)
    (should (= (point) (+ 1 (length pre-point) (length skipped))))))

(ert-deftest js2-forward-sexp-skip-semi ()
  "Ensure expr-stmt-nodes' semicolons are skipped over."
  (js2-test-forward-sexp "" "const s = 123;" ""))

(ert-deftest js2-forward-sexp-inside-string ()
  "Test forward sexp inside a string."
  (js2-test-forward-sexp "const s = 'some " "(string contents)" " xyz';"))

(ert-deftest js2-backward-sexp-inside-string ()
  "Test backward sexp inside a string."
  (with-temp-buffer
    (insert "const s = 'some (string contents) ")
    (save-excursion (insert "xyz';"))
    (backward-sexp)
    (should (= (point) 17))))
