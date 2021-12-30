;;; tests/imenu-mocha.el --- Tests for imenu support in mocha buffers.  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Damien Cassou

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

(require 'ert)
(require 'js2-mode)
(require 'js2-imenu-extras)

(defmacro js2-imenu-mocha-create-buffer (lines &rest body)
  "Execute BODY in a `js2-mode' buffer containing LINES."
  `(with-temp-buffer
     ,@(mapcar (lambda (line) `(insert ,line "\n")) lines)
     (js2-mode)
     (js2-parse)
     (js2-imenu-extras-setup)
     (setq-local js2-imenu-enabled-frameworks '(mocha))
     ,@body))

(ert-deftest js2-imenu-mocha-top-level-describe ()
  (js2-imenu-mocha-create-buffer
   ("describe(\"top-level\", () => {});")
   (let ((result (js2-mode-create-imenu-index)))
     (should (equal result '(("top-level" . 1)))))))

(ert-deftest js2-imenu-mocha-top-level-indented-describe ()
  (js2-imenu-mocha-create-buffer
   ("  describe(\"top-level\", () => {});")
   (let ((result (js2-mode-create-imenu-index)))
     (should (equal result '(("top-level" . 3)))))))

(ert-deftest js2-imenu-mocha-top-level-fdescribe ()
  (js2-imenu-mocha-create-buffer
   ("fdescribe(\"top-level\", () => {});")
   (let ((result (js2-mode-create-imenu-index)))
     (should (equal result '(("top-level" . 1)))))))

(ert-deftest js2-imenu-mocha-top-level-describe-only ()
  (js2-imenu-mocha-create-buffer
   ("describe.only(\"top-level\", () => {});")
   (let ((result (js2-mode-create-imenu-index)))
     (should (equal result '(("top-level" . 1)))))))

(ert-deftest js2-imenu-mocha-top-level-describes ()
  (js2-imenu-mocha-create-buffer
   ("describe(\"top1\", () => {});"
    "describe(\"top2\", () => {});")
   (let ((result (js2-mode-create-imenu-index)))
     (should (equal result
                    '(("top1" . 1)
                      ("top2" . 29)))))))

(ert-deftest js2-imenu-mocha-describe-in-describe ()
  (js2-imenu-mocha-create-buffer
   ("describe(\"top-level\", () => {"
    "  describe(\"sub\", () => {})"
    "});")
   (let ((result (js2-mode-create-imenu-index)))
     (should (equal result
                    '(("top-level"
                       ("<definition-1>" . 1)
                       ("sub" . 33))))))))

(ert-deftest js2-imenu-mocha-fdescribe-in-describe ()
  (js2-imenu-mocha-create-buffer
   ("describe(\"top-level\", () => {"
    "  fdescribe(\"sub\", () => {})"
    "});")
   (let ((result (js2-mode-create-imenu-index)))
     (should (equal result
                    '(("top-level"
                       ("<definition-1>" . 1)
                       ("sub" . 33))))))))

(ert-deftest js2-imenu-mocha-two-describes-in-describe ()
  (js2-imenu-mocha-create-buffer
   ("describe(\"top-level\", () => {"
    "  describe(\"sub1\", () => {})"
    "  describe(\"sub2\", () => {})"
    "});")
   (let ((result (js2-mode-create-imenu-index)))
     (should (equal result
                    '(("top-level"
                       ("<definition-1>" . 1)
                       ("sub1" . 33)
                       ("sub2" . 62))))))))

(ert-deftest js2-imenu-mocha-describe-in-describe-in-describe ()
  (js2-imenu-mocha-create-buffer
   ("describe(\"top-level\", () => {"
    "  describe(\"sub\", () => {"
    "    describe(\"subsub\", () => {});"
    "  });"
    "});")
   (let ((result (js2-mode-create-imenu-index)))
     (should (equal result
                    '(("top-level"
                       ("<definition-1>" . 1)
                       ("sub"
                        ("<definition-1>" . 33)
                        ("subsub" . 61)))))))))

(ert-deftest js2-imenu-mocha-beforeEach-in-describe ()
  (js2-imenu-mocha-create-buffer
   ("describe(\"top-level\", () => {"
    "  beforeEach(() => {})"
    "});")
   (let ((result (js2-mode-create-imenu-index)))
     (should (equal result
                    '(("top-level"
                       ("<definition-1>" . 1)
                       ("beforeEach" . 33))))))))

(ert-deftest js2-imenu-mocha-it-in-describe ()
  (js2-imenu-mocha-create-buffer
   ("describe(\"top-level\", () => {"
    "  it(\"sub\", () => {})"
    "});")
   (let ((result (js2-mode-create-imenu-index)))
     (should (equal result
                    '(("top-level"
                       ("<definition-1>" . 1)
                       ("sub" . 33))))))))

(ert-deftest js2-imenu-mocha-top-level-function ()
  (js2-imenu-mocha-create-buffer
   ("function foo () {}")
   (let ((result (js2-mode-create-imenu-index)))
     (should (equal result '(("foo" . 1)))))))

(ert-deftest js2-imenu-mocha-function-in-describe ()
  (js2-imenu-mocha-create-buffer
   ("describe(\"top-level\", () => {"
    "  function foo () {}"
    "});")
   (let ((result (js2-mode-create-imenu-index)))
     (should (equal result
                    '(("top-level"
                       ("<definition-1>" . 1)
                       ("foo" . 33))))))))

(ert-deftest js2-imenu-mocha-customize-describe-node-name ()
  (js2-imenu-mocha-create-buffer
   ("describe(\"top-level\", () => {"
    "  fooBar(\"sub\", () => {})"
    "});")
   (let* ((js2-imenu-mocha-describe-node-names '("describe" "fooBar"))
          (result (js2-mode-create-imenu-index)))
     (should (equal result
                    '(("top-level"
                       ("<definition-1>" . 1)
                       ("sub" . 33))))))))

(ert-deftest js2-imenu-mocha-customize-it-node-name ()
  (js2-imenu-mocha-create-buffer
   ("describe(\"top-level\", () => {"
    "  fooBar(\"sub\", () => {})"
    "});")
   (let* ((js2-imenu-mocha-it-node-names '("fooBar"))
          (result (js2-mode-create-imenu-index)))
     (should (equal result
                    '(("top-level"
                       ("<definition-1>" . 1)
                       ("sub" . 33))))))))

(ert-deftest js2-imenu-mocha-customize-hook-node-name ()
  (js2-imenu-mocha-create-buffer
   ("describe(\"top-level\", () => {"
    "  fooBar(() => {})"
    "});")
   (let* ((js2-imenu-mocha-hook-node-names '("fooBar"))
          (result (js2-mode-create-imenu-index)))
     (should (equal result
                    '(("top-level"
                       ("<definition-1>" . 1)
                       ("fooBar" . 33))))))))
