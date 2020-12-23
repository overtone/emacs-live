;;; clojure-mode-util-test.el --- Clojure Mode: util test suite  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2020 Bozhidar Batsov <bozhidar@batsov.com>

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
(require 'buttercup)


(describe "clojure-mode-version"
  (it "should not be nil"
    (expect clojure-mode-version)))

(let ((project-dir "/home/user/projects/my-project/")
      (clj-file-path "/home/user/projects/my-project/src/clj/my_project/my_ns/my_file.clj")
      (project-relative-clj-file-path "src/clj/my_project/my_ns/my_file.clj")
      (clj-file-ns "my-project.my-ns.my-file")
      (clojure-cache-project nil))

  (describe "clojure-project-relative-path"
    (cl-letf (((symbol-function 'clojure-project-dir) (lambda () project-dir)))
      (expect (string= (clojure-project-relative-path clj-file-path)
                       project-relative-clj-file-path))))

  (describe "clojure-expected-ns"
    (it "should return the namespace matching a path"
      (cl-letf (((symbol-function 'clojure-project-relative-path)
                 (lambda (&optional current-buffer-file-name)
                   project-relative-clj-file-path)))
        (expect (string= (clojure-expected-ns clj-file-path) clj-file-ns))))

    (it "should return the namespace even without a path"
      (cl-letf (((symbol-function 'clojure-project-relative-path)
                 (lambda (&optional current-buffer-file-name)
                   project-relative-clj-file-path)))
        (expect (string= (let ((buffer-file-name clj-file-path))
                           (clojure-expected-ns))
                         clj-file-ns))))))

(describe "clojure-find-ns"
  (it "should find common namespace declarations"
    (with-clojure-buffer "(ns foo)"
      (expect (clojure-find-ns) :to-equal "foo"))
    (with-clojure-buffer "(ns
    foo)"
      (expect (clojure-find-ns) :to-equal "foo"))
    (with-clojure-buffer "(ns foo.baz)"
      (expect (clojure-find-ns) :to-equal "foo.baz"))
    (with-clojure-buffer "(ns ^:bar foo)"
      (expect (clojure-find-ns) :to-equal "foo"))
    (with-clojure-buffer "(ns ^:bar ^:baz foo)"
      (expect (clojure-find-ns) :to-equal "foo")))
  (it "should find namespace declarations with nested metadata and docstrings"
    (with-clojure-buffer "(ns ^{:bar true} foo)"
      (expect (clojure-find-ns) :to-equal "foo"))
    (with-clojure-buffer "(ns #^{:bar true} foo)"
      (expect (clojure-find-ns) :to-equal "foo"))
    (with-clojure-buffer "(ns #^{:fail {}} foo)"
      (expect (clojure-find-ns) :to-equal "foo"))
    (with-clojure-buffer "(ns ^{:fail2 {}} foo.baz)"
      (expect (clojure-find-ns) :to-equal "foo.baz"))
    (with-clojure-buffer "(ns ^{} foo)"
      (expect (clojure-find-ns) :to-equal "foo"))
    (with-clojure-buffer "(ns ^{:skip-wiki true}
      aleph.netty"
      (expect (clojure-find-ns) :to-equal "aleph.netty"))
    (with-clojure-buffer "(ns ^{:foo {:bar :baz} :fake (ns in.meta)} foo
  \"docstring
(ns misleading)\")"
      (expect (clojure-find-ns) :to-equal "foo")))
  (it "should support non-alphanumeric characters"
    (with-clojure-buffer "(ns foo+)"
      (expect (clojure-find-ns) :to-equal "foo+"))
    (with-clojure-buffer "(ns bar**baz$-_quux)"
      (expect (clojure-find-ns) :to-equal "bar**baz$-_quux"))
    (with-clojure-buffer "(ns aoc-2019.puzzles.day14)"
      (expect (clojure-find-ns) :to-equal "aoc-2019.puzzles.day14")))
  (it "should support in-ns forms"
    (with-clojure-buffer "(in-ns 'bar.baz)"
      (expect (clojure-find-ns) :to-equal "bar.baz")))
  (it "should take the closest ns before point"
    (with-clojure-buffer " (ns foo1)

(ns foo2)"
      (expect (clojure-find-ns) :to-equal "foo2"))
    (with-clojure-buffer " (in-ns foo1)
(ns 'foo2)
(in-ns 'foo3)
|
(ns foo4)"
      (re-search-backward "|")
      (expect (clojure-find-ns) :to-equal "foo3"))
    (with-clojure-buffer "(ns foo)
(ns-unmap *ns* 'map)
(ns.misleading 1 2 3)"
      (expect (clojure-find-ns) :to-equal "foo"))))

(describe "clojure-sort-ns"
  (it "should sort requires in a basic ns"
    (with-clojure-buffer "(ns my-app.core
    (:require [rum.core :as rum] ;comment
              [my-app.views [user-page :as user-page]]))"
      (clojure-sort-ns)
      (expect (buffer-string) :to-equal
              "(ns my-app.core
    (:require [my-app.views [user-page :as user-page]]
              [rum.core :as rum] ;comment\n))")))

  (it "should also sort imports in a ns"
    (with-clojure-buffer "\n(ns my-app.core
    (:require [my-app.views [front-page :as front-page]]
              [my-app.state :refer [state]] ; Comments too.
              ;; Some comments.
              [rum.core :as rum]
              [my-app.views [user-page :as user-page]]
              my-app.util.api)
    (:import java.io.Writer
             [clojure.lang AFunction Atom MultiFn Namespace]))"
      (clojure-mode)
      (clojure-sort-ns)
      (expect (buffer-string) :to-equal
                     "\n(ns my-app.core
    (:require [my-app.state :refer [state]] ; Comments too.
              my-app.util.api
              [my-app.views [front-page :as front-page]]
              [my-app.views [user-page :as user-page]]
              ;; Some comments.
              [rum.core :as rum])
    (:import [clojure.lang AFunction Atom MultiFn Namespace]
             java.io.Writer))"))))

(provide 'clojure-mode-util-test)

;;; clojure-mode-util-test.el ends here
