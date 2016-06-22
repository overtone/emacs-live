;;; clojure-mode-util-test.el --- Clojure Mode: util test suite  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2016 Bozhidar Batsov <bozhidar@batsov.com>

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

;; The unit test suite of Clojure Mode

;;; Code:
(require 'clojure-mode)
(require 'cl-lib)
(require 'ert)

(let ((project-dir "/home/user/projects/my-project/")
      (clj-file-path "/home/user/projects/my-project/src/clj/my_project/my_ns/my_file.clj")
      (project-relative-clj-file-path "src/clj/my_project/my_ns/my_file.clj")
      (clj-file-ns "my-project.my-ns.my-file"))

  (ert-deftest project-relative-path ()
    :tags '(utils)
    (cl-letf (((symbol-function 'clojure-project-dir) (lambda () project-dir)))
      (should (string= (clojure-project-relative-path clj-file-path)
                       project-relative-clj-file-path))))

  (ert-deftest expected-ns ()
    :tags '(utils)
    (cl-letf (((symbol-function 'clojure-project-relative-path)
               (lambda (&optional current-buffer-file-name)
                 project-relative-clj-file-path)))
      (should (string= (clojure-expected-ns clj-file-path) clj-file-ns))))

  (ert-deftest expected-ns-without-argument ()
    :tags '(utils)
    (cl-letf (((symbol-function 'clojure-project-relative-path)
               (lambda (&optional current-buffer-file-name)
                 project-relative-clj-file-path)))
      (should (string= (let ((buffer-file-name clj-file-path))
                         (clojure-expected-ns))
                       clj-file-ns)))))

(ert-deftest clojure-namespace-name-regex-test ()
  :tags '(regexp)
  (let ((ns "(ns foo)"))
    (should (string-match clojure-namespace-name-regex ns))
    (match-string 4 ns))
  (let ((ns "(ns
foo)"))
    (should (string-match clojure-namespace-name-regex ns))
    (should (equal "foo" (match-string 4 ns))))
  (let ((ns "(ns foo.baz)"))
    (should (string-match clojure-namespace-name-regex ns))
    (should (equal "foo.baz" (match-string 4 ns))))
  (let ((ns "(ns ^:bar foo)"))
    (should (string-match clojure-namespace-name-regex ns))
    (should (equal "foo" (match-string 4 ns))))
  (let ((ns "(ns ^:bar ^:baz foo)"))
    (should (string-match clojure-namespace-name-regex ns))
    (should (equal "foo" (match-string 4 ns))))
  (let ((ns "(ns ^{:bar true} foo)"))
    (should (string-match clojure-namespace-name-regex ns))
    (should (equal "foo" (match-string 4 ns))))
  (let ((ns "(ns #^{:bar true} foo)"))
    (should (string-match clojure-namespace-name-regex ns))
    (should (equal "foo" (match-string 4 ns))))
  ;; TODO
  ;; (let ((ns "(ns #^{:fail {}} foo)"))
  ;;   (should (string-match clojure-namespace-name-regex ns))
  ;;   (match-string 4 ns))
  ;; (let ((ns "(ns ^{:fail2 {}} foo.baz)"))
  ;;   (should (string-match clojure-namespace-name-regex ns))
  ;;   (should (equal "foo.baz" (match-string 4 ns))))
  (let ((ns "(ns ^{} foo)"))
    (should (string-match clojure-namespace-name-regex ns))
    (should (equal "foo" (match-string 4 ns))))
  (let ((ns "(ns ^{:skip-wiki true}
  aleph.netty"))
    (should (string-match clojure-namespace-name-regex ns))
    (should (equal "aleph.netty" (match-string 4 ns))))
  (let ((ns "(ns foo+)"))
    (should (string-match clojure-namespace-name-regex ns))
    (should (equal "foo+" (match-string 4 ns)))))

(ert-deftest test-sort-ns ()
  (with-temp-buffer
    (insert "\n(ns my-app.core
  (:require [my-app.views [front-page :as front-page]]
            [my-app.state :refer [state]] ; Comments too.
            ;; Some comments.
            [rum.core :as rum]
            [my-app.views [user-page :as user-page]]
            my-app.util.api)
  (:import java.io.Writer
           [clojure.lang AFunction Atom MultiFn Namespace]))")
    (clojure-mode)
    (clojure-sort-ns)
    (should (equal (buffer-string)
                   "\n(ns my-app.core
  (:require [my-app.state :refer [state]] ; Comments too.
            my-app.util.api
            [my-app.views [front-page :as front-page]]
            [my-app.views [user-page :as user-page]]
            ;; Some comments.
            [rum.core :as rum])
  (:import [clojure.lang AFunction Atom MultiFn Namespace]
           java.io.Writer))")))
  (with-temp-buffer
    (insert "(ns my-app.core
  (:require [rum.core :as rum] ;comment
            [my-app.views [user-page :as user-page]]))")
    (clojure-mode)
    (clojure-sort-ns)
    (should (equal (buffer-string)
                   "(ns my-app.core
  (:require [my-app.views [user-page :as user-page]]
            [rum.core :as rum] ;comment\n))"))))

(provide 'clojure-mode-util-test)

;;; clojure-mode-util-test.el ends here
