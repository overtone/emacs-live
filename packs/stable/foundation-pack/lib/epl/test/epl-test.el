;;; epl-test.el --- EPL: Test suite -*- lexical-binding: t; -*-

;; Copyright (C) 2013, 2014  Sebastian Wiesner

;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;;     Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: http://github.com/cask/epl

;; This file is NOT part of GNU Emacs.

;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; Keywords: convenience

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

;; Test EPL

;;; Code:

(require 'epl)
(require 'f)
(require 'ert)


;;;; Directories
(defconst epl-test-directory (f-parent (f-this-file))
  "The directory of the test suite.")


;;;; Resource handling
(defconst epl-test-resource-directory (f-join epl-test-directory "resources")
  "The directory of test resource files.")

(defun epl-test-resource-file-name (resource)
  "Get the file name of a RESOURCE."
  (f-join epl-test-resource-directory resource))


;;;; Package structures

(ert-deftest epl-package-as-description/variable-must-be-a-symbol ()
  ;; We explicitly `eval' the expression here, to avoid eager macro expansion
  ;; kicking in, and triggering the error before the test gets executed
  (let* ((expr '(epl-package-as-description "foo" (message "bar")))
         (err-and-data (should-error (eval expr)))
         (data (cdr err-and-data)))
    (should (eq (car err-and-data) 'wrong-type-argument))
    (should (eq (car data) #'symbolp))
    (should (equal (cadr data) "foo"))))

(ert-deftest epl-package-as-description/variable-must-be-bound-to-epl-package ()
  (let* ((foo "bar")
         (err-and-data (should-error (epl-package-as-description foo
                                       (message "bar"))))
         (data (cdr err-and-data)))
    (should (eq (car err-and-data) 'wrong-type-argument))
    (should (eq (car data) #'epl-package-p))
    (should (equal (cadr data) "bar"))))

(ert-deftest epl-package-from-file/valid-lisp-package ()
  (let* ((file (epl-test-resource-file-name "dummy-package.el"))
         (package (epl-package-from-file file))
         (summary "EPL: Dummy package for unit tests"))
    (when (< emacs-major-version 24)
      ;; Emacs 23 package.el does not strip local variable lines from summary
      (setq summary (concat summary "  -*- lexical-binding: t; -*-")))
    (should (epl-package-p package))
    (should (string= (epl-package-name package) 'dummy-package))
    (should (string= (epl-package-summary package) summary))
    (should (equal (epl-package-version package) '(4 3 1 2 -3)))
    (should (equal (epl-package-requirements package)
                   (list (epl-requirement-create :name 'foo :version '(1 2))
                         (epl-requirement-create :name 'bar :version '(2 2)))))))

(ert-deftest epl-package-from-file-tar/tar-file-without-package-descriptor ()
  "Test a TAR package without package descriptor."
  (should-error
   (epl-package-from-file
    (epl-test-resource-file-name "dummy-package-4.3.1.2alpha.tar"))))

(ert-deftest epl-package-from-file/valid-tar-package ()
  (let* ((file (epl-test-resource-file-name "dummy-package-4.3.2.tar"))
         (package (epl-package-from-file file)))
    (should (epl-package-p package))
    (should (string= (epl-package-name package) 'dummy-package))
    (should (string= (epl-package-summary package) "EPL dummy package"))
    (should (equal (epl-package-version package) '(4 3 2)))
    (should (equal (epl-package-requirements package)
                   (list (epl-requirement-create :name 'foo :version '(0 3))
                         (epl-requirement-create :name 'spam :version '(0 4)))))))

(ert-deftest epl-package-from-file/tar-file-does-not-exist ()
  (should-error
   (epl-package-from-file
    (epl-test-resource-file-name "no-such-package.tar"))))

(ert-deftest epl-package-from-file/lisp-file-does-not-exist ()
  (should-error
   (epl-package-from-file
    (epl-test-resource-file-name "no-such-package.el"))))

(ert-deftest epl-package-from-descriptor-file/should-not-define-the-package ()
  "Loading a package descriptor should not affect the database.

package.el tends to have such unfortunate side effects."
  (let ((file (epl-test-resource-file-name "dummy-package-pkg.el")))
    (epl-package-from-descriptor-file file)
    (should-not (assq 'dummy-package package-alist))))

(ert-deftest epl-package-from-descriptor-file/valid-descriptor ()
  (let* ((file (epl-test-resource-file-name "dummy-package-pkg.el"))
         (package (epl-package-from-descriptor-file file)))
    (should (epl-package-p package))
    (should (string= (epl-package-name package) 'dummy-package))
    (should (string= (epl-package-summary package) "EPL dummy package"))
    (should (equal (epl-package-version package) '(4 3 6)))
    (should (equal (epl-package-requirements package)
                   (list (epl-requirement-create :name 'bar :version '(8 1 -3))
                         (epl-requirement-create :name 'spam :version '(0 4)))))))

(ert-deftest epl-package-from-descriptor/descriptor-file-does-not-exist ()
  (should-error
   (epl-package-from-file
    (epl-test-resource-file-name "no-such-descriptor-pkg.el"))))

(ert-deftest epl-package-from-descriptor/package-file-is-invalid ()
  (should-error
   (epl-package-from-file
    (epl-test-resource-file-name "invalid-package-pkg.el"))))

(provide 'epl-test)

;;; epl-test.el ends here
