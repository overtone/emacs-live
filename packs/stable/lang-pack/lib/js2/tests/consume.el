;;; tests/consume.el --- Some tests for js2-mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Free Software Foundation, Inc.

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

(defun js2-mode--and-parse ()
  (js2-mode)
  (js2-reparse))

;;; Comments

(ert-deftest js2-comments-between ()
  (with-temp-buffer
    (insert "0\n//\n[0,/* */1]")
    (js2-mode--and-parse)
    (let ((comments-list (js2-ast-root-comments js2-mode-ast))
           comments)
      (setq comments (js2-comments-between 1 2 comments-list))
      (should (null comments))
      ;; comment head between region
      (setq comments (js2-comments-between 1 3 comments-list))
      (should (= (length comments) 1))
      ;; comment body between region
      (setq comments (js2-comments-between 4 5 comments-list))
      (should (= (length comments) 1))
      ;; comment tail between region
      (setq comments (js2-comments-between 5 6 comments-list))
      (should (= (length comments) 1))
      (setq comments (js2-comments-between 6 6 comments-list))
      (should (null comments))
      (setq comments (js2-comments-between 10 12 comments-list))
      (should (= (length comments) 1))
      ;; multiple comments between
      (setq comments (js2-comments-between 5 15 comments-list))
      (should (= (length comments) 2))
      ;; pass comments-list when no AST available
      (setq js2-mode-ast nil)
      (setq comments (js2-comments-between 8 9 comments))
      (should (= (length comments) 1))
      )))

;;; Visitors

(ert-deftest js2-visit-import-clause-in-order ()
  (with-temp-buffer
    (insert "import defaultImport, { a, b, c} from 'xyz';")
    (js2-mode--and-parse)
    (let (visit-log)
     (js2-visit-ast js2-mode-ast (lambda (node end-p)
                                   (when (and (not end-p) (js2-name-node-p node))
                                     (let* ((start (js2-node-abs-pos node))
                                            (end (+ start (js2-node-len node))))
                                       (push (buffer-substring-no-properties start end) visit-log)))
                                   t))
     (setq visit-log (nreverse visit-log))
     (should (equal visit-log (list "defaultImport" "a" "b" "c"))))))

(ert-deftest js2-node-parent-stmt/arrow-function ()
  (ert-with-test-buffer (:name 'js2-node-parent-stmt/arrow-function)
    (insert "expect(() => ")
    (save-excursion (insert "func(undefined)).toThrow(/undefined/);"))
    (js2-mode--and-parse)
    (let ((parent-stmt (js2-node-parent-stmt (js2-node-at-point))))
      (should (= (js2-node-abs-pos parent-stmt) 1)))))
