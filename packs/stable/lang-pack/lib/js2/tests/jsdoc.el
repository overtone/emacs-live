;;; tests/jsdoc.el --- Tests for js2-mode highlighting of jsdoc comments.  -*- lexical-binding: t; -*-

;; Copyright (C) 2009, 2011-2017  Free Software Foundation, Inc.

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
(require 'ert-x)
(require 'js2-mode)

(defun js2-get-font-lock-face-props (beg end)
  "Return a list of three-tuples (START END FONT-LOCK-FACE).
BEG and END are the current buffer boundaries to use."
  (unless (get-char-property beg 'font-lock-face)
    (setq beg (next-single-char-property-change beg 'font-lock-face nil end)))
  (let (fontification change)
    (while (< beg end)
      (setq change (next-single-char-property-change beg 'font-lock-face nil end))
      (push (list beg change (get-char-property beg 'font-lock-face)) fontification)
      (setq beg (if (get-char-property change 'font-lock-face)
                    change
                  (next-single-char-property-change change 'font-lock-face nil end))))
    (nreverse fontification)))

(defmacro js2-jsdoc-deftest (name comment-text fontification)
  (declare (indent defun))
  (let ((test-name (intern (format "js2-jsdoc-%s" name))))
    `(ert-deftest ,test-name ()
       (ert-with-test-buffer (:name ',test-name)
         (insert ,comment-text)
         (js2-mode)
         (js2-reparse)
         (should (equal (js2-get-font-lock-face-props (point-min) (point-max))
                        ,fontification))))))

(js2-jsdoc-deftest param
  "/**\n * @prop {string} p - The property\n */\n"
  '((1 8 font-lock-doc-face)
    (8 13 js2-jsdoc-tag)
    (13 15 font-lock-doc-face)
    (15 21 js2-jsdoc-type)
    (21 23 font-lock-doc-face)
    (23 24 js2-jsdoc-value)
    (24 43 font-lock-doc-face)))

(js2-jsdoc-deftest typed
  "/**\n * @implements {Interface}\n */\n"
  '((1 8 font-lock-doc-face)
    (8 19 js2-jsdoc-tag)
    (19 21 font-lock-doc-face)
    (21 30 js2-jsdoc-type)
    (30 35 font-lock-doc-face)))

(js2-jsdoc-deftest arg
  "/**\n * @name TheName \n */\n"
  '((1 8 font-lock-doc-face)
    (8 13 js2-jsdoc-tag)
    (13 14 font-lock-doc-face)
    (14 21 js2-jsdoc-value)
    (21 26 font-lock-doc-face)))

(js2-jsdoc-deftest empty
  "/**\n * @class \n */\n"
  '((1 8 font-lock-doc-face)
    (8 14 js2-jsdoc-tag)
    (14 19 font-lock-doc-face)))

(js2-jsdoc-deftest param-same-line
  "/** @prop {string} p - The property */\n"
  '((1 5 font-lock-doc-face)
    (5 10 js2-jsdoc-tag)
    (10 12 font-lock-doc-face)
    (12 18 js2-jsdoc-type)
    (18 20 font-lock-doc-face)
    (20 21 js2-jsdoc-value)
    (21 39 font-lock-doc-face)))

(js2-jsdoc-deftest typed-same-line
  "/** @implements {Interface} */\n"
  '((1 5 font-lock-doc-face)
    (5 16 js2-jsdoc-tag)
    (16 18 font-lock-doc-face)
    (18 27 js2-jsdoc-type)
    (27 31 font-lock-doc-face)))

(js2-jsdoc-deftest arg-same-line
  "/** @name TheName */\n"
  '((1 5 font-lock-doc-face)
    (5 10 js2-jsdoc-tag)
    (10 11 font-lock-doc-face)
    (11 18 js2-jsdoc-value)
    (18 21 font-lock-doc-face)))

(js2-jsdoc-deftest empty-same-line
  "/** @class */\n"
  '((1 5 font-lock-doc-face)
    (5 11 js2-jsdoc-tag)
    (11 14 font-lock-doc-face)))
