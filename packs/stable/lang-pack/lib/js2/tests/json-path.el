;;; tests/json-path.el --- Test of using js2-mode AST to print JSON path.

;; Copyright (C) 2015  Free Software Foundation, Inc.

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

(ert-deftest js2-json-path-with-actual-array-index ()
  (with-temp-buffer
    (insert "var a = { hello: [1, 2, [1,3,3,4, { world: { hell: { yes: [1,2, 'test'] } } }]] };")
    (js2-mode)
    (goto-char 0)
    (search-forward "test")
    (should (string= (js2-print-json-path) "hello[2][4].world.hell.yes[2]"))))

(ert-deftest js2-json-path-pure-arrays ()
  (with-temp-buffer
    (insert "var a = [5, 1, 4, [ 4, [1, 2, [1, 3.9, 4, [1, 2, 'test',3]]]], 9, 9];")
    (js2-mode)
    (goto-char 0)
    (search-forward "test")
    (should (string= (js2-print-json-path) "[3][1][2][3][2]"))))

(ert-deftest js2-json-path-key-is-numeric ()
  (with-temp-buffer
    (insert "var b = {hello: {3 : {world: {2: 'test'}}}};")
    (js2-mode)
    (goto-char 0)
    (search-forward "test")
    (should (string= (js2-print-json-path) "hello.3.world.2"))))

(ert-deftest js2-json-path-not-found ()
  (with-temp-buffer
    (insert "console.log('test');")
    (js2-mode)
    (goto-char 0)
    (search-forward "test")
    (should (eq (js2-print-json-path) nil))))

(ert-deftest js2-json-path-with-array-index-hardcoded ()
  (with-temp-buffer
    (insert "var a = { hello: [1, 2, [1,3,3,4, { world: { hell: { yes: [1,2, 'test'] } } }]] };")
    (js2-mode)
    (goto-char 0)
    (search-forward "test")
    (should (string= (js2-print-json-path 1) "hello[1][1].world.hell.yes[1]"))
    (should (string= (js2-print-json-path 0) "hello[0][0].world.hell.yes[0]"))))
