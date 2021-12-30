;;; clojure-mode-refactor-rename-ns-alias-test.el --- Clojure Mode: refactor rename ns alias  -*- lexical-binding: t; -*-

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

;; Tests for clojure-rename-ns-alias

;;; Code:

(require 'clojure-mode)
(require 'ert)

(describe "clojure--rename-ns-alias-internal"

  (when-refactoring-it "should rename an alias"
    "(ns cljr.core
       (:require [my.lib :as lib]))

     (def m #::lib{:kw 1, :n/kw 2, :_/bare 3, 0 4})

     (+ (lib/a 1) (b 2))"

    "(ns cljr.core
       (:require [my.lib :as foo]))

     (def m #::foo{:kw 1, :n/kw 2, :_/bare 3, 0 4})

     (+ (foo/a 1) (b 2))"

    (clojure--rename-ns-alias-internal "lib" "foo"))
  (when-refactoring-it "should handle multiple aliases with common prefixes"

    "(ns foo
  (:require [clojure.string :as string]
            [clojure.spec.alpha :as s]
            [clojure.java.shell :as shell]))

(s/def ::abc string/blank?)
"
    "(ns foo
  (:require [clojure.string :as string]
            [clojure.spec.alpha :as spec]
            [clojure.java.shell :as shell]))

(spec/def ::abc string/blank?)
"
    (clojure--rename-ns-alias-internal "s" "spec"))

  (when-refactoring-it "should handle ns declarations with missing as"
    "(ns cljr.core
       (:require [my.lib :as lib]))

     (def m #::lib{:kw 1, :n/kw 2, :_/bare 3, 0 4})

     (+ (lib/a 1) (b 2))"

    "(ns cljr.core
       (:require [my.lib :as lib]))

     (def m #::lib{:kw 1, :n/kw 2, :_/bare 3, 0 4})

     (+ (lib/a 1) (b 2))"

    (clojure--rename-ns-alias-internal "foo" "bar"))

  (when-refactoring-it "should skip strings"
    "(ns cljr.core
       (:require [my.lib :as lib]))

     (def dirname \"/usr/local/lib/python3.6/site-packages\")

     (+ (lib/a 1) (b 2))"

    "(ns cljr.core
       (:require [my.lib :as foo]))

     (def dirname \"/usr/local/lib/python3.6/site-packages\")

     (+ (foo/a 1) (b 2))"

    (clojure--rename-ns-alias-internal "lib" "foo"))

  (when-refactoring-it "should not skip comments"
    "(ns cljr.core
       (:require [my.lib :as lib]))

     (def dirname \"/usr/local/lib/python3.6/site-packages\")

     ;; TODO refactor using lib/foo
     (+ (lib/a 1) (b 2))"

    "(ns cljr.core
       (:require [my.lib :as new-lib]))

     (def dirname \"/usr/local/lib/python3.6/site-packages\")

     ;; TODO refactor using new-lib/foo
     (+ (new-lib/a 1) (b 2))"

    (clojure--rename-ns-alias-internal "lib" "new-lib"))

  (when-refactoring-it "should escape regex characters"
    "(ns test.ns
  (:require [my.math.subtraction :as math.-]
            [my.math.multiplication :as math.*]))

(math.*/operator 1 (math.-/subtract 2 3))"
    "(ns test.ns
  (:require [my.math.subtraction :as math.-]
            [my.math.multiplication :as m*]))

(m*/operator 1 (math.-/subtract 2 3))"
    (clojure--rename-ns-alias-internal "math.*" "m*"))

  (when-refactoring-it "should replace aliases in region"
    "(str/join [])

(s/with-gen #(string/includes? % \"gen/nope\")
  #(gen/fmap (fn [[s1 s2]] (str s1 \"hello\" s2))
     (gen/tuple (gen/string-alphanumeric) (gen/string-alphanumeric))))

(gen/different-library)"
    "(string/join [])

(s/with-gen #(string/includes? % \"gen/nope\")
  #(s.gen/fmap (fn [[s1 s2]] (str s1 \"hello\" s2))
     (s.gen/tuple (s.gen/string-alphanumeric) (s.gen/string-alphanumeric))))

(gen/different-library)"

    (clojure--rename-ns-alias-usages "str" "string" (point-min) 13)
    (clojure--rename-ns-alias-usages "gen" "s.gen" (point-min) (- (point-max) 23)))

  (it "should offer completions for ns forms"
    (expect
     (with-clojure-buffer
         "(ns test.ns
  (:require [my.math.subtraction :as math.-]
            [my.math.multiplication :as math.*]
            [clojure.spec.alpha :as s]
            ;; [clojure.spec.alpha2 :as s2]
            [symbols :as abc123.-$#.%*+!@]))

(math.*/operator 1 (math.-/subtract 2 3))"
       (clojure--collect-ns-aliases (point-min) (point-max) 'ns-form))
     :to-equal '("math.-" "math.*" "s" "abc123.-$#.%*+!@")))

  (it "should offer completions for usages in region"
    (expect
     (with-clojure-buffer
         "(s/with-gen #(string/includes? % \"hello\")
  #(gen/fmap (fn [[s1 s2]] (str s1 \"hello\" s2))
     (gen/tuple (gen/string-alphanumeric) (gen/string-alphanumeric))))"
       (clojure--collect-ns-aliases (point-min) (point-max) nil))
     :to-equal '("s" "string" "gen"))))


(provide 'clojure-mode-refactor-rename-ns-alias-test)

;;; clojure-mode-refactor-rename-ns-alias-test.el ends here
