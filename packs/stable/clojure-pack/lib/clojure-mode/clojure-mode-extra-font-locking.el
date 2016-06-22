;;; clojure-mode-extra-font-locking.el --- Extra font-locking for Clojure mode

;; Copyright © 2014-2016 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: http://github.com/clojure-emacs/clojure-mode
;; Version: 3.0.0
;; Keywords: languages, lisp
;; Package-Requires: ((clojure-mode "3.0"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides additional font-locking for clojure-mode.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'clojure-mode)

(defvar clojure-built-in-vars
  '(;; clojure.core
    "accessor" "aclone"
    "agent" "agent-errors" "aget" "alength" "alias"
    "all-ns" "alter" "alter-meta!" "alter-var-root" "amap"
    "ancestors" "apply" "areduce" "array-map" "as->"
    "aset" "aset-boolean" "aset-byte" "aset-char" "aset-double"
    "aset-float" "aset-int" "aset-long" "aset-short" "assert"
    "assoc" "assoc!" "assoc-in" "associative?" "atom"
    "await" "await-for" "await1" "bases" "bean"
    "bigdec" "bigint" "bit-and" "bit-and-not"
    "bit-clear" "bit-flip" "bit-not" "bit-or" "bit-set"
    "bit-shift-left" "bit-shift-right" "bit-test" "bit-xor" "boolean"
    "boolean-array" "booleans" "bound-fn" "bound-fn*" "bound?" "butlast"
    "byte" "byte-array" "bytes" "cast" "char"
    "char-array" "char-escape-string" "char-name-string" "char?" "chars"
    "chunk" "chunk-append" "chunk-buffer" "chunk-cons" "chunk-first"
    "chunk-next" "chunk-rest" "chunked-seq?" "class" "class?"
    "clear-agent-errors" "clojure-version" "coll?" "comment" "commute"
    "comp" "comparator" "compare" "compare-and-set!" "compile"
    "complement" "concat" "conj"
    "conj!" "cons" "constantly" "construct-proxy" "contains?"
    "count" "counted?" "create-ns" "create-struct" "cycle"
    "dec" "decimal?" "delay" "delay?" "deliver" "denominator" "deref"
    "derive" "descendants" "destructure" "disj" "disj!"
    "dissoc" "dissoc!" "distinct" "distinct?"
    "doc"
    "double" "double-array" "doubles" "drop"
    "drop-last" "drop-while" "empty" "empty?" "ensure"
    "enumeration-seq" "error-handler" "error-mode" "eval" "even?" "every?"
    "every-pred" "extend" "extend-protocol" "extend-type" "extends?"
    "extenders" "ex-info" "ex-data"
    "false?" "ffirst" "file-seq" "filter" "filterv" "find" "find-doc"
    "find-ns" "find-keyword" "find-var" "first" "flatten" "float" "float-array"
    "float?" "floats" "flush" "fn?"
    "fnext" "force" "format" "frequencies" "future"
    "future-call" "future-cancel" "future-cancelled?" "future-done?" "future?"
    "gen-interface" "gensym" "get" "get-in"
    "get-method" "get-proxy-class" "get-thread-bindings" "get-validator"
    "group-by" "hash" "hash-map" "hash-ordered-coll" "hash-set"
    "hash-unordered-coll" "identical?" "identity" "ifn?" "inc"
    "init-proxy" "instance?" "int" "int-array" "integer?"
    "interleave" "intern" "interpose" "into" "into-array"
    "ints" "io!" "isa?" "iterate" "iterator-seq"
    "juxt" "keep" "keep-indexed" "key" "keys" "keyword" "keyword?"
    "last" "lazy-cat" "lazy-seq"
    "line-seq" "list" "list*" "list?"
    "load-file" "load-reader" "load-string" "loaded-libs" "locking"
    "long" "long-array" "longs" "macroexpand"
    "macroexpand-1" "make-array" "make-hierarchy" "map" "mapv" "map?"
    "map-indexed" "mapcat" "max" "max-key" "memfn" "memoize"
    "merge" "merge-with" "meta" "method-sig" "methods"
    "min" "min-key" "mix-collection-hash" "mod" "name" "namespace"
    "neg?" "newline" "next" "nfirst" "nil?"
    "nnext" "not" "not-any?" "not-empty" "not-every?"
    "not=" "ns-aliases" "ns-imports" "ns-interns"
    "ns-map" "ns-name" "ns-publics" "ns-refers" "ns-resolve"
    "ns-unalias" "ns-unmap" "nth" "nthnext" "nthrest" "num"
    "number?" "numerator" "object-array" "odd?" "parents" "partial"
    "partition" "partition-all" "partition-by" "pcalls" "peek" "persistent!"
    "pmap" "pop" "pop!" "pop-thread-bindings" "pos?" "pr"
    "pr-str" "prefer-method" "prefers" "primitives-classnames" "print"
    "print-ctor" "print-doc" "print-dup" "print-method" "print-namespace-doc"
    "print-simple" "print-special-doc" "print-str" "printf" "println"
    "println-str" "prn" "prn-str" "promise" "proxy"
    "proxy-call-with-super" "proxy-mappings" "proxy-name" "proxy-super"
    "push-thread-bindings" "pvalues" "quot" "rand" "rand-int" "rand-nth" "range"
    "ratio?" "rational?" "rationalize" "re-find" "re-groups"
    "re-matcher" "re-matches" "re-pattern" "re-seq" "read"
    "read-line" "read-string" "realized?" "record?" "reduce" "reduce-kv"
    "reduced" "reduced?" "reductions" "reify" "ref" "ref-history-count"
    "ref-max-history" "ref-min-history" "ref-set" "refer-clojure"
    "release-pending-sends" "rem" "remove" "remove-all-methods"
    "remove-method" "remove-ns" "remove-watch"
    "repeat" "repeatedly" "replace" "replicate"
    "require" "restart-agent" "reset!" "reset-meta!" "resolve" "rest"
    "resultset-seq" "reverse" "reversible?" "rseq" "rsubseq"
    "satisfies?" "second" "select-keys" "send" "send-off" "send-via" "seq"
    "seq?" "seque" "sequence" "sequential?" "set"
    "set-agent-send-executor!" "set-agent-send-off-executor!"
    "set-error-handler!" "set-error-mode!" "set-validator!" "set?" "short"
    "short-array" "shorts" "shuffle"
    "shutdown-agents" "slurp" "some" "some->" "some->>" "some-fn" "some?"
    "sort" "sort-by" "sorted-map" "sorted-map-by" "sorted-set" "sorted-set-by"
    "sorted?" "special-form-anchor" "special-symbol?" "specify" "specify!"
    "spit" "split-at" "split-with" "str"
    "stream?" "string?" "struct" "struct-map" "subs"
    "subseq" "subvec" "supers" "swap!" "symbol"
    "symbol?" "sync" "syntax-symbol-anchor" "take" "take-last"
    "take-nth" "take-while" "test" "the-ns" "thread-bound?" "time"
    "to-array" "to-array-2d" "trampoline" "transient" "tree-seq"
    "true?" "type" "unchecked-add" "unchecked-add-int" "unchecked-byte"
    "unchecked-char" "unchecked-dec" "unchecked-dec-int" "unchecked-divide"
    "unchecked-divide-int" "unchecked-double" "unchecked-float"
    "unchecked-inc" "unchecked-inc-int" "unchecked-long" "unchecked-multiply"
    "unchecked-multiply-int" "unchecked-negate" "unchecked-negate-int"
    "unchecked-remainder" "unchecked-remainder-int" "unchecked-short"
    "unchecked-subtract-int" "unchecked-subtract"
    "underive" "unsigned-bit-shift-right" "unquote" "unquote-splicing"
    "update" "update-in" "update-proxy" "use" "val" "vals" "var-get" "var-set"
    "var?" "vary-meta" "vec" "vector" "vector?" "vector-of" "while"
    "with-bindings" "with-bindings*" "with-in-str" "with-loading-context"
    "with-meta" "with-out-str" "with-precision"
    "xml-seq" "zero?" "zipmap"
    ;; clojure.inspector
    "atom?" "collection-tag" "get-child" "get-child-count" "inspect"
    "inspect-table" "inspect-tree" "is-leaf" "list-model" "list-provider"
    ;; clojure.main
    "load-script" "main" "repl" "repl-caught" "repl-exception"
    "repl-prompt" "repl-read" "skip-if-eol" "skip-whitespace" "with-bindings"
    ;; clojure.set
    "difference" "index" "intersection" "join" "map-invert"
    "project" "rename" "rename-keys" "select" "union"
    ;; clojure.stacktrace
    "e" "print-cause-trace" "print-stack-trace" "print-throwable"
    "print-trace-element"
    ;; clojure.template
    "do-template" "apply-template"
    ;; clojure.test
    "are" "assert-any" "assert-expr"
    "assert-predicate" "compose-fixtures" "deftest" "deftest-" "file-position"
    "function?" "get-possibly-unbound-var" "inc-report-counter" "is"
    "join-fixtures" "report" "run-all-tests" "run-tests" "set-test"
    "successful?" "test-all-vars" "test-ns" "test-var" "test-vars" "testing"
    "testing-contexts-str" "testing-vars-str" "try-expr" "use-fixtures"
    "with-test" "with-test-out"
    ;; clojure.walk
    "keywordize-keys" "macroexpand-all" "postwalk" "postwalk-demo"
    "postwalk-replace" "prewalk" "prewalk-demo" "prewalk-replace"
    "stringify-keys" "walk"
    ;; clojure.xml
    "attrs"
    "content" "content-handler" "element" "emit" "emit-element"
    ;; clojure.zip
    "append-child" "branch?" "children" "down" "edit"
    "end?" "insert-child" "insert-left" "insert-right" "left"
    "leftmost" "lefts" "make-node" "next" "node"
    "path" "prev" "remove" "replace" "right"
    "rightmost" "rights" "root" "seq-zip" "up"
    ))

(defvar clojure-built-in-dynamic-vars
  '(;; clojure.test
    "*initial-report-counters*" "*load-tests*" "*report-counters*"
    "*stack-trace-depth*" "*test-out*" "*testing-contexts*" "*testing-vars*"
    ;; clojure.xml
    "*current*" "*sb*" "*stack*" "*state*"
    ))

(font-lock-add-keywords 'clojure-mode
                        `((,(concat "(\\(?:\.*/\\)?"
                                    (regexp-opt clojure-built-in-vars t)
                                    "\\>")
                           1 font-lock-builtin-face)))

(font-lock-add-keywords 'clojure-mode
                        `((,(concat "\\<"
                                    (regexp-opt clojure-built-in-dynamic-vars t)
                                    "\\>")
                           0 font-lock-builtin-face)))

(provide 'clojure-mode-extra-font-locking)

;;; clojure-mode-extra-font-locking.el ends here
