;;; pkg-info-tests.el --- Unit tests for pkg-info    -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2016  Sebastian Wiesner

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;; Keywords:

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

;; Unit tests for pkg-info.

;;; Code:

(require 'pkg-info)

(require 'ert)
(require 'lisp-mnt)

(require 'pkg-info-dummy-package)
(require 'pkg-info-dummy-original-version)

(defconst pkg-info-ruby-mode-version
  (with-temp-buffer
    (insert-file-contents (find-library-name "ruby-mode"))
    (version-to-list (lm-header "Version"))))

(ert-deftest pkg-info-library-source/existing-library ()
  (should (string-match-p "test/elpa/pkg-info-dummy-package-3.4.2.1/pkg-info-dummy-package.el\\'"
                          (pkg-info-library-source "pkg-info-dummy-package"))))

(ert-deftest pkg-info-library-source/non-existing-library ()
  (should-error (pkg-info-library-source "foobar-does-not-exist")))

(ert-deftest pkg-info-defining-library-version/existing-function ()
  (should (string-match-p
           "test/elpa/pkg-info-dummy-package-3.4.2.1/pkg-info-dummy-package.el\\'"
           (pkg-info-defining-library 'pkg-info-dummy-package-dummy-function))))

(ert-deftest pkg-info-defining-library-version/non-existing-function ()
  (should-error (pkg-info-defining-library 'foo-bar-is-no-function)))

(ert-deftest pkg-info-library-original-version/feature-exists ()
  (should (equal (pkg-info-library-original-version 'pkg-info-dummy-original-version)
                 '(1 3))))

(ert-deftest pkg-info-library-original-version/library-exists ()
  (should (equal (pkg-info-library-original-version "pkg-info-dummy-original-version")
                 '(1 3))))

(ert-deftest pkg-info-library-original-version/feature-does-not-exist ()
  (should-error (pkg-info-library-original-version 'no-such-feature)))

(ert-deftest pkg-info-library-original-version/library-does-not-exist ()
  (should-error (pkg-info-library-original-version "no-such-library")))

(ert-deftest pkg-info-library-version/feature-exists ()
  (should (equal (pkg-info-library-version 'pkg-info-dummy-package) '(3 4 2 1))))

(ert-deftest pkg-info-library-version/builtin-feature ()
  (should (equal (pkg-info-library-version 'ruby-mode)
                 pkg-info-ruby-mode-version)))

(ert-deftest pkg-info-library-version/feature-does-not-exist ()
  (should-error (pkg-info-library-version 'no-such-feature)))

(ert-deftest pkg-info-library-version/loaded-feature-outside-load-path ()
  :expected-result :failed
  ;; TODO: Fails?  We must find a way to get the defining file of a `require'
  (should (equal (pkg-info-library-version 'pkg-info) (pkg-info-version))))

(ert-deftest pkg-info-library-version/library-exists ()
  (should (equal (pkg-info-library-version "pkg-info-dummy-package.el")
                 '(3 4 2 1))))

(ert-deftest pkg-info-library-version/library-does-not-exist ()
  (should-error (pkg-info-library-version "no-such-library")))

(ert-deftest pkg-info-defining-library-original-version/defined-function ()
  (should (equal (pkg-info-defining-library-original-version
                  #'pkg-info-dummy-original-version-dummy-function)
                 '(1 3))))

(ert-deftest pkg-info-defining-library-original-version/no-x-original-version-header ()
  (should-error (pkg-info-defining-library-original-version
                 #'pkg-info-dummy-package-dummy-function)))

(ert-deftest pkg-info-defining-library-original-version/undefined-function ()
  (should-error (pkg-info-defining-library-original-version #'this-is-no-function)))

(ert-deftest pkg-info-defining-library-original-version/anonymous-function-without-source ()
  (should-error (pkg-info-defining-library-original-version (lambda () "foo"))))

(ert-deftest pkg-info-defining-library-version/defined-function ()
  (should (equal (pkg-info-defining-library-version
                  #'pkg-info-dummy-package-dummy-function)
                 '(3 4 2 1))))

(ert-deftest pkg-info-defining-library-version/undefined-function ()
  (should-error (pkg-info-defining-library-version #'this-is-no-function)))

(ert-deftest pkg-info-defining-library-version/anonymous-function-without-source ()
  (should-error (pkg-info-defining-library-version (lambda () "foo"))))

(ert-deftest pkg-info-package-version/installed-package ()
  (should (equal (pkg-info-package-version 'pkg-info-dummy-package) '(3 4 2 1))))

(ert-deftest pkg-info-package-version/package-does-not-exist ()
  (should-error (pkg-info-package-version 'no-such-package)))

(ert-deftest pkg-info-version-info/feature-and-package-are-the-same ()
  (should (equal (pkg-info-version-info 'pkg-info-dummy-package) "3.4.2.1")))

(ert-deftest pkg-info-version-info/library-and-package-are-the-same ()
  (should (equal (pkg-info-version-info "pkg-info-dummy-package") "3.4.2.1")))

(ert-deftest pkg-info-version-info/feature-and-package-are-different ()
  (should (equal (pkg-info-version-info 'ruby-mode 'pkg-info-dummy-package)
                 (format "%s (package: 3.4.2.1)"
                         (pkg-info-format-version pkg-info-ruby-mode-version)))))

(ert-deftest pkg-info-version-info/library-and-package-are-different ()
  (should (equal (pkg-info-version-info "ruby-mode" 'pkg-info-dummy-package)
                 (format "%s (package: 3.4.2.1)"
                         (pkg-info-format-version pkg-info-ruby-mode-version)))))

(ert-deftest pkg-info-version-info/library-with-original-version ()
  (should (equal
           (pkg-info-version-info "pkg-info-dummy-original-version") "1.3")))

(ert-deftest pkg-info-version-info/library-and-package-with-original-version ()
  (should (equal (pkg-info-version-info "pkg-info-dummy-original-version"
                                        'pkg-info-dummy-package)
                 "1.3 (package: 3.4.2.1)")))

(ert-deftest pkg-info-version-info/package-does-not-exist ()
  (should (equal (pkg-info-version-info 'pkg-info-dummy-package 'no-such-package)
                 "3.4.2.1")))

(ert-deftest pkg-info-version-info/feature-does-not-exist ()
  (should-error (pkg-info-version-info 'no-such-library)))

(ert-deftest pkg-info-version-info/library-does-not-exist ()
  (should-error (pkg-info-version-info "no-such-library")))

(ert-deftest pkg-info-format-version/release-version ()
  (should (equal (pkg-info-format-version '(3 4 2 1)) "3.4.2.1")))

(ert-deftest pkg-info-format-version/prerelease-version ()
  (should (equal (pkg-info-format-version '(2 1 -3)) "2.1alpha")))

(ert-deftest pkg-info-get-melpa-recipe/has-a-proper-recipe ()
  (let ((recipe (pkg-info-get-melpa-recipe 'pkg-info)))
    (should (equal (cdr (assq 'fetcher recipe)) "github"))
    (should (equal (cdr (assq 'repo recipe)) "lunaryorn/pkg-info.el"))))

(ert-deftest pkg-info-get-melpa-recipe/package-does-not-exist ()
  (should-not (pkg-info-get-melpa-recipe 'foobarblubb)))

(ert-deftest pkg-info-get-melpa-fetcher/has-a-fetcher ()
  (should (equal (pkg-info-get-melpa-fetcher 'pkg-info) "github")))

(ert-deftest pkg-info-get-melpa-fetcher/package-does-not-exist ()
  (should-not (pkg-info-get-melpa-fetcher 'foobarblubb)))

(ert-deftest pkg-info-wiki-package-p/a-wiki-package ()
  (should (pkg-info-wiki-package-p 'dired+)))

(ert-deftest pkg-info-wiki-package-p/not-a-wiki-package ()
  (should-not (pkg-info-wiki-package-p 'pkg-info)))

(ert-deftest pkg-info-wiki-package-p/package-does-not-exist ()
  (should-not (pkg-info-wiki-package-p 'foobarblubb)))

;; Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; pkg-info-test.el ends here
