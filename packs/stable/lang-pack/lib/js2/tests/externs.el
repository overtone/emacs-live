;;; tests/externs.el --- Some tests for js2-mode.

;; Copyright (C) 2009, 2011-2013  Free Software Foundation, Inc.

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

(ert-deftest js2-finds-jslint-globals ()
  (with-temp-buffer
    (insert "/*global foo, bar:false, baz: true */")
    (js2-mode)
    (should (equal (js2-get-jslint-globals)
                   '("foo" "bar" "baz")))))

(ert-deftest js2-no-jslint-globals-without-keyword ()
  (with-temp-buffer
    (insert "/* foo, bar:false, baz: true */")
    (js2-mode)
    (should (null (js2-get-jslint-globals)))))

(ert-deftest js2-finds-jslint-globals-in-other-comments ()
  (with-temp-buffer
    (insert "/* foo, bar */\n\n\n/*global quux, tee: true, $*/")
    (js2-mode)
    (should (equal (js2-get-jslint-globals)
                   '("quux" "tee" "$")))))

(ert-deftest js2-finds-jslint-globals-with-space ()
  (with-temp-buffer
    (insert "/* global foo, bar:false, baz:true")
    (js2-mode)
    (should (equal (js2-get-jslint-globals)
                   '("foo" "bar" "baz")))))

;;;TODO
;; ensure that any symbols bound with the import syntax are added to the extern list
;; ensure that any symbols bound with the export syntax exist in the file scope
