;;; tests/navigation.el --- Some tests for js2-mode.

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

(cl-defun js2-navigation-helper (buffer-content &optional expected-point (point-offset 1))
  (with-temp-buffer
    (insert buffer-content)
    (let ((start-point (or (- (point) point-offset))))
      (js2-mode)
      (goto-char start-point)
      (ignore-errors (js2-jump-to-definition))
      (print (format "%d %d" (point) start-point))
      (should (= (point) (or expected-point start-point))))))

(ert-deftest js2-jump-to-var ()
  (js2-navigation-helper "var soup = 2; soup" 5))

(ert-deftest js2-jump-to-function ()
  (js2-navigation-helper "function aFunction() {}; aFunction" 1))

(ert-deftest js2-jump-to-function-parameters ()
  (js2-navigation-helper "var p1 = 4; function aFunction(p1, p2) {p1};" 32 4))

(ert-deftest js2-jump-to-object-property ()
  (js2-navigation-helper "var aObject = {prop1: 3, prop2: \"hello\"}; aObject.prop1" 16))

(ert-deftest js2-no-jump-to-object-property ()
  (js2-navigation-helper "var aObject = {prop1: 3, prop2: \"hello\"}; anotherObject.prop1"))

(ert-deftest js2-jump-to-nested-property ()
  (js2-navigation-helper "var aObject = {prop1: {prop2: { prop3: 4}}}; aObject.prop1.prop2.prop3" 33))

(ert-deftest js2-jump-to-object ()
  (js2-navigation-helper "var aObject = {prop1: 3, prop2: \"hello\"}; aObject.prop1" 5 13))

(ert-deftest js2-jump-to-property ()
  (js2-navigation-helper "aObject.func = functon(){};aObject.func" 9))

(ert-deftest js2-jump-to-property-object-property ()
  (js2-navigation-helper "aObject.value = {prop:1};aObject.value.prop" 18))
