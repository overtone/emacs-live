;;; clojure-mode.el --- Major mode for Clojure code

;; Copyright © 2007-2013 Jeffrey Chu, Lennart Staflin, Phil Hagelberg
;;
;; Authors: Jeffrey Chu <jochu0@gmail.com>
;;          Lennart Staflin <lenst@lysator.liu.se>
;;          Phil Hagelberg <technomancy@gmail.com>
;; URL: http://github.com/technomancy/clojure-mode
;; Version: 2.0.0
;; Keywords: languages, lisp

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides font-lock, indentation, and navigation for the Clojure
;; language. (http://clojure.org)

;; Users of older Emacs (pre-22) should get version 1.4:
;; http://github.com/technomancy/clojure-mode/tree/1.4

;; Slime integration has been removed; see the 1.x releases if you need it.

;; Using clojure-mode with paredit is highly recommended. Use paredit
;; as you would with any other minor mode; for instance:
;;
;;   ;; require or autoload paredit-mode
;;   (add-hook 'clojure-mode-hook 'paredit-mode)

;; See nREPL.el (http://github.com/kingtim/nrepl.el) for
;; better interaction with subprocesses via nREPL.

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

(eval-when-compile
  (defvar calculate-lisp-indent-last-sexp)
  (defvar font-lock-beg)
  (defvar font-lock-end)
  (defvar paredit-version)
  (defvar paredit-mode))

(require 'cl)
(require 'tramp)
(require 'inf-lisp)

(require 'imenu)

(declare-function clojure-test-jump-to-implementation  "clojure-test-mode.el")

(defconst clojure-font-lock-keywords
  (eval-when-compile
    `( ;; Definitions.
      (,(concat "(\\(?:clojure.core/\\)?\\("
                (regexp-opt '("defn" "defn-" "def" "defonce"
                              "defmulti" "defmethod" "defmacro"
                              "defstruct" "deftype" "defprotocol"
                              "definst" "defsynth" "defcgen"
                              "defrecord" "deftest" "def\\[a-z\\]"))

                ;; Function declarations.
                "\\)\\>"
                ;; Any whitespace
                "[ \r\n\t]*"
                ;; Possibly type or metadata
                "\\(?:#?^\\(?:{[^}]*}\\|\\sw+\\)[ \r\n\t]*\\)*"
                "\\(\\sw+\\)?")
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face nil t))
      ;; (fn name? args ...)
      (,(concat "(\\(?:clojure.core/\\)?\\(fn\\)[ \t]+"
                ;; Possibly type
                "\\(?:#?^\\sw+[ \t]*\\)?"
                ;; Possibly name
                "\\(t\\sw+\\)?" )
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face nil t))

      (,(concat "(\\(\\(?:[a-z\.-]+/\\)?def\[a-z\]*-?\\)"
                ;; Function declarations.
                "\\>"
                ;; Any whitespace
                "[ \r\n\t]*"
                ;; Possibly type or metadata
                "\\(?:#?^\\(?:{[^}]*}\\|\\sw+\\)[ \r\n\t]*\\)*"
                "\\(\\sw+\\)?")
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face nil t))
      ;; Deprecated functions
      (,(concat
         "(\\(?:clojure.core/\\)?"
         (regexp-opt
          '("add-watcher" "remove-watcher" "add-classpath") t)
         "\\>")
       1 font-lock-warning-face)
      ;; Control structures
      (,(concat
         "(\\(?:clojure.core/\\)?"
         (regexp-opt
          '("let" "letfn" "do"
            "case" "cond" "condp"
            "for" "loop" "recur"
            "when" "when-not" "when-let" "when-first"
            "if" "if-let" "if-not"
            "." ".." "->" "->>" "doto"
            "and" "or"
            "dosync" "doseq" "dotimes" "dorun" "doall"
            "load" "import" "unimport" "ns" "in-ns" "refer"
            "try" "catch" "finally" "throw"
            "with-open" "with-local-vars" "binding"
            "gen-class" "gen-and-load-class" "gen-and-save-class"
            "handler-case" "handle") t)
         "\\>")
       1 font-lock-keyword-face)
      ;; Built-ins
      (,(concat
         "(\\(?:clojure.core/\\)?"
         (regexp-opt
          '("*" "*1" "*2" "*3" "*agent*"
        "*allow-unresolved-vars*" "*assert*" "*clojure-version*" "*command-line-args*" "*compile-files*"
        "*compile-path*" "*e" "*err*" "*file*" "*flush-on-newline*"
        "*in*" "*macro-meta*" "*math-context*" "*ns*" "*out*"
        "*print-dup*" "*print-length*" "*print-level*" "*print-meta*" "*print-readably*"
        "*read-eval*" "*source-path*" "*use-context-classloader*" "*warn-on-reflection*" "+"
        "-" "/"
        "<" "<=" "=" "==" ">"
        ">=" "accessor" "aclone"
        "agent" "agent-errors" "aget" "alength" "alias"
        "all-ns" "alter" "alter-meta!" "alter-var-root" "amap"
        "ancestors" "and" "apply" "areduce" "array-map"
        "aset" "aset-boolean" "aset-byte" "aset-char" "aset-double"
        "aset-float" "aset-int" "aset-long" "aset-short" "assert"
        "assoc" "assoc!" "assoc-in" "associative?" "atom"
        "await" "await-for" "await1" "bases" "bean"
        "bigdec" "bigint" "binding" "bit-and" "bit-and-not"
        "bit-clear" "bit-flip" "bit-not" "bit-or" "bit-set"
        "bit-shift-left" "bit-shift-right" "bit-test" "bit-xor" "boolean"
        "boolean-array" "booleans" "bound-fn" "bound-fn*" "butlast"
        "byte" "byte-array" "bytes" "case" "cast" "char"
        "char-array" "char-escape-string" "char-name-string" "char?" "chars"
        "chunk" "chunk-append" "chunk-buffer" "chunk-cons" "chunk-first"
        "chunk-next" "chunk-rest" "chunked-seq?" "class" "class?"
        "clear-agent-errors" "clojure-version" "coll?" "comment" "commute"
        "comp" "comparator" "compare" "compare-and-set!" "compile"
        "complement" "concat" "cond" "condp" "conj"
        "conj!" "cons" "constantly" "construct-proxy" "contains?"
        "count" "counted?" "create-ns" "create-struct" "cycle"
        "dec" "decimal?" "declare" "definline" "defmacro"
        "defmethod" "defmulti" "defn" "defn-" "defonce"
        "defstruct" "delay" "delay?" "deliver" "deref"
        "derive" "descendants" "destructure" "disj" "disj!"
        "dissoc" "dissoc!" "distinct" "distinct?" "doall"
        "doc" "dorun" "doseq" "dosync" "dotimes"
        "doto" "double" "double-array" "doubles" "drop"
        "drop-last" "drop-while" "empty" "empty?" "ensure"
        "enumeration-seq" "eval" "even?" "every?"
        "extend" "extend-protocol" "extend-type" "extends?" "extenders"
        "false?" "ffirst" "file-seq" "filter" "find" "find-doc"
        "find-ns" "find-var" "first" "flatten" "float" "float-array"
        "float?" "floats" "flush" "fn" "fn?"
        "fnext" "for" "force" "format" "future"
        "future-call" "future-cancel" "future-cancelled?" "future-done?" "future?"
        "gen-class" "gen-interface" "gensym" "get" "get-in"
        "get-method" "get-proxy-class" "get-thread-bindings" "get-validator" "group-by"
        "hash" "hash-map" "hash-set" "identical?" "identity" "if-let"
        "if-not" "ifn?" "import" "in-ns" "inc"
        "init-proxy" "instance?" "int" "int-array" "integer?"
        "interleave" "intern" "interpose" "into" "into-array"
        "ints" "io!" "isa?" "iterate" "iterator-seq"
        "juxt" "key" "keys" "keyword" "keyword?"
        "last" "lazy-cat" "lazy-seq" "let" "letfn"
        "line-seq" "list" "list*" "list?" "load"
        "load-file" "load-reader" "load-string" "loaded-libs" "locking"
        "long" "long-array" "longs" "loop" "macroexpand"
        "macroexpand-1" "make-array" "make-hierarchy" "map" "map?"
        "mapcat" "max" "max-key" "memfn" "memoize"
        "merge" "merge-with" "meta" "method-sig" "methods"
        "min" "min-key" "mod" "name" "namespace"
        "neg?" "newline" "next" "nfirst" "nil?"
        "nnext" "not" "not-any?" "not-empty" "not-every?"
        "not=" "ns" "ns-aliases" "ns-imports" "ns-interns"
        "ns-map" "ns-name" "ns-publics" "ns-refers" "ns-resolve"
        "ns-unalias" "ns-unmap" "nth" "nthnext" "num"
        "number?" "odd?" "or" "parents" "partial"
        "partition" "partition-all" "partition-by" "pcalls" "peek" "persistent!" "pmap"
        "pop" "pop!" "pop-thread-bindings" "pos?" "pr"
        "pr-str" "prefer-method" "prefers" "primitives-classnames" "print"
        "print-ctor" "print-doc" "print-dup" "print-method" "print-namespace-doc"
        "print-simple" "print-special-doc" "print-str" "printf" "println"
        "println-str" "prn" "prn-str" "promise" "proxy"
        "proxy-call-with-super" "proxy-mappings" "proxy-name" "proxy-super" "push-thread-bindings"
        "pvalues" "quot" "rand" "rand-int" "range"
        "ratio?" "rational?" "rationalize" "re-find" "re-groups"
        "re-matcher" "re-matches" "re-pattern" "re-seq" "read"
        "read-line" "read-string" "reify" "reduce" "ref" "ref-history-count"
        "ref-max-history" "ref-min-history" "ref-set" "refer" "refer-clojure"
        "release-pending-sends" "rem" "remove" "remove-method" "remove-ns"
        "repeat" "repeatedly" "replace" "replicate"
        "require" "reset!" "reset-meta!" "resolve" "rest"
        "resultset-seq" "reverse" "reversible?" "rseq" "rsubseq"
        "satisfies?" "second" "select-keys" "send" "send-off" "seq"
        "seq?" "seque" "sequence" "sequential?" "set"
        "set-validator!" "set?" "short" "short-array" "shorts"
        "shutdown-agents" "slurp" "some" "sort" "sort-by"
        "sorted-map" "sorted-map-by" "sorted-set" "sorted-set-by" "sorted?"
        "special-form-anchor" "special-symbol?" "spit" "split-at" "split-with" "str"
        "stream?" "string?" "struct" "struct-map" "subs"
        "subseq" "subvec" "supers" "swap!" "symbol"
        "symbol?" "sync" "syntax-symbol-anchor" "take" "take-last"
        "take-nth" "take-while" "test" "the-ns" "time"
        "to-array" "to-array-2d" "trampoline" "transient" "tree-seq"
        "true?" "type" "unchecked-add" "unchecked-dec" "unchecked-divide"
        "unchecked-inc" "unchecked-multiply" "unchecked-negate" "unchecked-remainder" "unchecked-subtract"
        "underive" "unquote" "unquote-splicing" "update-in" "update-proxy"
        "use" "val" "vals" "var-get" "var-set"
        "var?" "vary-meta" "vec" "vector" "vector?"
        "when" "when-first" "when-let" "when-not" "while"
        "with-bindings" "with-bindings*" "with-in-str" "with-loading-context" "with-local-vars"
        "with-meta" "with-open" "with-out-str" "with-precision" "xml-seq" "zipmap"
        ) t)
         "\\>")
       1 font-lock-builtin-face)
      ;;Other namespaces in clojure.jar
      (,(concat
         "(\\(?:\.*/\\)?"
         (regexp-opt
          '(;; clojure.inspector
        "atom?" "collection-tag" "get-child" "get-child-count" "inspect"
        "inspect-table" "inspect-tree" "is-leaf" "list-model" "list-provider"
        ;; clojure.main
        "load-script" "main" "repl" "repl-caught" "repl-exception"
        "repl-prompt" "repl-read" "skip-if-eol" "skip-whitespace" "with-bindings"
        ;; clojure.set
        "difference" "index" "intersection" "join" "map-invert"
        "project" "rename" "rename-keys" "select" "union"
        ;; clojure.stacktrace
        "e" "print-cause-trace" "print-stack-trace" "print-throwable" "print-trace-element"
        ;; clojure.template
        "do-template" "apply-template"
        ;; clojure.test
        "*initial-report-counters*" "*load-tests*" "*report-counters*" "*stack-trace-depth*" "*test-out*"
        "*testing-contexts*" "*testing-vars*" "are" "assert-any" "assert-expr"
        "assert-predicate" "compose-fixtures" "deftest" "deftest-" "file-position"
        "function?" "get-possibly-unbound-var" "inc-report-counter" "is" "join-fixtures"
        "report" "run-all-tests" "run-tests" "set-test" "successful?"
        "test-all-vars" "test-ns" "test-var" "testing" "testing-contexts-str"
        "testing-vars-str" "try-expr" "use-fixtures" "with-test" "with-test-out"
        ;; clojure.walk
        "keywordize-keys" "macroexpand-all" "postwalk" "postwalk-demo" "postwalk-replace"
        "prewalk" "prewalk-demo" "prewalk-replace" "stringify-keys" "walk"
        ;; clojure.xml
        "*current*" "*sb*" "*stack*" "*state*" "attrs"
        "content" "content-handler" "element" "emit" "emit-element"
        ;; clojure.zip
        "append-child" "branch?" "children" "down" "edit"
        "end?" "insert-child" "insert-left" "insert-right" "left"
        "leftmost" "lefts" "make-node" "next" "node"
        "path" "prev" "remove" "replace" "right"
        "rightmost" "rights" "root" "seq-zip" "up"
        ) t)
         "\\>")
       1 font-lock-type-face)
      ;; Constant values (keywords), including as metadata e.g. ^:static
      ("\\<^?:\\(\\sw\\|#\\)+\\>" 0 font-lock-constant-face)
      ;; Meta type annotation #^Type or ^Type
      ("#?^\\sw+" 0 font-lock-preprocessor-face)
      ("\\<io\\!\\>" 0 font-lock-warning-face)

      ;;Interop Matching

      ;; Should match:
      ;; .foo .barBaz .qux01 .-flibble .-flibbleWobble Foo Bar$Baz Qux_ World_OpenUDP Foo/Bar foo.bar.Baz foo.Bar/baz Foo. BarBaz. Qux$Quux. Corge9. Foo/barBaz. fooBar

      ;; Shouldn't match:
      ;; FOO bar bar-baz clojure.repl/source

      ;; Foo. BarBaz. Qux$Quux. Corge9. Foo/barBaz.
      ("\\<[A-Z][a-zA-Z0-9$./]*\\.\\>" 0 font-lock-preprocessor-face)

      ;; .foo .barBaz .qux01 .-flibble .-flibbleWobble
      ("\\<\\.-?[a-z][a-zA-Z0-9]*\\>" 0 font-lock-preprocessor-face)

      ;; Foo Bar$Baz Qux_ World_OpenUDP Foo/Bar
      ("\\<[A-Z][a-zA-Z0-9/$_]*[a-z]+[a-zA-Z0-9/$_]*\\>" 0 font-lock-preprocessor-face)

      ;; foo.bar.Baz foo.Bar/baz
      ("\\<[a-z]+\\.[a-zA-Z0-9._]*[A-Z]+[a-zA-Z0-9/.$]*\\>" 0 font-lock-preprocessor-face)

      ;; fooBar
      ("[a-z]*[A-Z]+[a-z][a-zA-Z0-9$]*\\>" 0 font-lock-preprocessor-face)))

  "Default expressions to highlight in Clojure mode.")

(defgroup clojure-mode nil
  "A mode for Clojure"
  :prefix "clojure-mode-"
  :group 'applications)

(defcustom clojure-mode-font-lock-comment-sexp nil
  "Set to non-nil in order to enable font-lock of (comment...)
forms. This option is experimental. Changing this will require a
restart (ie. M-x clojure-mode) of existing clojure mode buffers."
  :type 'boolean
  :group 'clojure-mode)

(defcustom clojure-mode-load-command  "(clojure.core/load-file \"%s\")\n"
  "*Format-string for building a Clojure expression to load a file.
This format string should use `%s' to substitute a file name
and should result in a Clojure expression that will command the inferior
Clojure to load that file."
  :type 'string
  :group 'clojure-mode)

(defcustom clojure-mode-use-backtracking-indent t
  "Set to non-nil to enable backtracking/context sensitive indentation."
  :type 'boolean
  :group 'clojure-mode)

(defcustom clojure-max-backtracking 3
  "Maximum amount to backtrack up a list to check for context."
  :type 'integer
  :group 'clojure-mode)

(defvar clojure-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map lisp-mode-shared-map)
    (define-key map "\e\C-x" 'lisp-eval-defun)
    (define-key map "\C-x\C-e" 'lisp-eval-last-sexp)
    (define-key map "\C-c\C-e" 'lisp-eval-last-sexp)
    (define-key map "\C-c\C-l" 'clojure-load-file)
    (define-key map "\C-c\C-r" 'lisp-eval-region)
    (define-key map (kbd "C-c C-t") 'clojure-jump-between-tests-and-code)
    (define-key map "\C-c\C-z" 'clojure-display-inferior-lisp-buffer)
    (define-key map (kbd "C-c t") 'clojure-jump-to-test)
    (define-key map (kbd "C-c M-q") 'clojure-fill-docstring)
    map)
  "Keymap for Clojure mode. Inherits from `lisp-mode-shared-map'.")

(defvar clojure-mode-syntax-table
  (let ((table (copy-syntax-table emacs-lisp-mode-syntax-table)))
    (modify-syntax-entry ?~ "'   " table)
    ;; can't safely make commas whitespace since it will apply even
    ;; inside string literals--ick!
    ;; (modify-syntax-entry ?, "    " table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?^ "'" table)
    table))

(defvar clojure-mode-abbrev-table nil
  "Abbrev table used in clojure-mode buffers.")

(define-abbrev-table 'clojure-mode-abbrev-table ())

(defvar clojure-prev-l/c-dir/file nil
  "Record last directory and file used in loading or compiling.
This holds a cons cell of the form `(DIRECTORY . FILE)'
describing the last `clojure-load-file' or `clojure-compile-file' command.")

(defvar clojure-test-ns-segment-position -1
  "Which segment of the ns is \"test\" inserted in your test name convention.

Customize this depending on your project's conventions. Negative
numbers count from the end:

  leiningen.compile -> leiningen.test.compile (uses 1)
  clojure.http.client -> clojure.http.test.client (uses -1)")

(defun clojure-mode-version ()
  "Currently package.el doesn't support prerelease version numbers."
  "2.0.0")

;; For compatibility with Emacs < 24, derive conditionally
(defalias 'clojure-parent-mode
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

;;;###autoload
(define-derived-mode clojure-mode clojure-parent-mode "Clojure"
  "Major mode for editing Clojure code - similar to Lisp mode.
Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{clojure-mode-map}
Note that `run-lisp' may be used either to start an inferior Lisp job
or to switch back to an existing one.

Entry to this mode calls the value of `clojure-mode-hook'
if that value is non-nil."
  (interactive)
  (use-local-map clojure-mode-map)
  (set (make-local-variable 'imenu-create-index-function)
       (lambda ()
         (imenu--generic-function '((nil clojure-match-next-def 0)))))
  (set (make-local-variable 'local-abbrev-table) clojure-mode-abbrev-table)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (lisp-mode-variables nil)
  (set-syntax-table clojure-mode-syntax-table)
  (set (make-local-variable 'comment-start-skip)
       "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)\\(;+\\|#|\\) *")
  (set (make-local-variable 'lisp-indent-function)
       'clojure-indent-function)
  (when (< emacs-major-version 24)
    (set (make-local-variable 'forward-sexp-function)
         'clojure-forward-sexp))
  (set (make-local-variable 'lisp-doc-string-elt-property)
       'clojure-doc-string-elt)
  (set (make-local-variable 'inferior-lisp-program) "lein repl")
  (set (make-local-variable 'parse-sexp-ignore-comments) t)

  (clojure-mode-font-lock-setup)

  (add-hook 'paredit-mode-hook
            (lambda ()
              (when (>= paredit-version 21)
                (define-key clojure-mode-map "{" 'paredit-open-curly)
                (define-key clojure-mode-map "}" 'paredit-close-curly)))))

(defun clojure-display-inferior-lisp-buffer ()
  "Display a buffer bound to `inferior-lisp-buffer'."
  (interactive)
  (if (and inferior-lisp-buffer (get-buffer inferior-lisp-buffer))
      (pop-to-buffer inferior-lisp-buffer t)
      (run-lisp inferior-lisp-program)))

(defun clojure-load-file (file-name)
  "Load a Lisp file into the inferior Lisp process."
  (interactive (comint-get-source "Load Clojure file: "
                                  clojure-prev-l/c-dir/file
                                  '(clojure-mode) t))
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq clojure-prev-l/c-dir/file (cons (file-name-directory file-name)
                                        (file-name-nondirectory file-name)))
  (comint-send-string (inferior-lisp-proc)
                      (format clojure-mode-load-command file-name))
  (switch-to-lisp t))



(defun clojure-match-next-def ()
  "Scans the buffer backwards for the next top-level definition.
Called by `imenu--generic-function'."
  (when (re-search-backward "^\\s *(def\\S *[ \n\t]+" nil t)
    (save-excursion
      (goto-char (match-end 0))
      (when (looking-at "#?\\^")
        (let (forward-sexp-function) ; using the built-in one
          (forward-sexp)))           ; skip the metadata
      (re-search-forward "[^ \n\t)]+"))))

(defun clojure-mode-font-lock-setup ()
  "Configures font-lock for editing Clojure code."
  (interactive)
  (set (make-local-variable 'font-lock-multiline) t)
  (add-to-list 'font-lock-extend-region-functions
               'clojure-font-lock-extend-region-def t)

  (when clojure-mode-font-lock-comment-sexp
    (add-to-list 'font-lock-extend-region-functions
                 'clojure-font-lock-extend-region-comment t)
    (make-local-variable 'clojure-font-lock-keywords)
    (add-to-list 'clojure-font-lock-keywords
                 'clojure-font-lock-mark-comment t)
    (set (make-local-variable 'open-paren-in-column-0-is-defun-start) nil))

  (setq font-lock-defaults
        '(clojure-font-lock-keywords    ; keywords
          nil nil
          (("+-*/.<>=!?$%_&~^:@" . "w")) ; syntax alist
          nil
          (font-lock-mark-block-function . mark-defun)
          (font-lock-syntactic-face-function
           . lisp-font-lock-syntactic-face-function))))

(defun clojure-font-lock-def-at-point (point)
  "Find the position range between the top-most def* and the
fourth element afterwards. Note that this means there's no
gaurantee of proper font locking in def* forms that are not at
top-level."
  (goto-char point)
  (condition-case nil
      (beginning-of-defun)
    (error nil))

  (let ((beg-def (point)))
    (when (and (not (= point beg-def))
               (looking-at "(def"))
      (condition-case nil
          (progn
            ;; move forward as much as possible until failure (or success)
            (forward-char)
            (dotimes (i 4)
              (forward-sexp)))
        (error nil))
      (cons beg-def (point)))))

(defun clojure-font-lock-extend-region-def ()
  "Move fontification boundaries to always include the first four
elements of a def* forms."
  (let ((changed nil))
    (let ((def (clojure-font-lock-def-at-point font-lock-beg)))
      (when def
        (destructuring-bind (def-beg . def-end) def
          (when (and (< def-beg font-lock-beg)
                     (< font-lock-beg def-end))
            (setq font-lock-beg def-beg
                  changed t)))))

    (let ((def (clojure-font-lock-def-at-point font-lock-end)))
      (when def
        (destructuring-bind (def-beg . def-end) def
          (when (and (< def-beg font-lock-end)
                     (< font-lock-end def-end))
            (setq font-lock-end def-end
                  changed t)))))
    changed))

(defun clojure-find-block-comment-start (limit)
  "Search for (comment...) or #_ style block comments and put
  point at the beginning of the expression."
  (let ((pos (re-search-forward "\\((comment\\>\\|#_\\)" limit t)))
    (when pos
      (forward-char (- (length (match-string 1))))
      pos)))

(defun clojure-font-lock-extend-region-comment ()
  "Move fontification boundaries to always contain
  entire (comment ..) and #_ sexp. Does not work if you have a
  white-space between ( and comment, but that is omitted to make
  this run faster."
  (let ((changed nil))
    (goto-char font-lock-beg)
    (condition-case nil (beginning-of-defun) (error nil))
    (let ((pos (clojure-find-block-comment-start font-lock-end)))
      (when pos
        (when (< (point) font-lock-beg)
          (setq font-lock-beg (point)
                changed t))
        (condition-case nil (forward-sexp) (error nil))
        (when (> (point) font-lock-end)
          (setq font-lock-end (point)
                changed t))))
    changed))

(defun clojure-font-lock-mark-comment (limit)
  "Marks all (comment ..) and #_ forms with font-lock-comment-face."
  (let (pos)
    (while (and (< (point) limit)
                (setq pos (clojure-find-block-comment-start limit)))
      (when pos
        (condition-case nil
            (add-text-properties (point)
                                 (progn
                                   (forward-sexp)
                                   (point))
                                 '(face font-lock-comment-face multiline t))
          (error (forward-char 8))))))
  nil)

(defconst clojure-font-lock-keywords
  (eval-when-compile
    `( ;; Definitions.
      (,(concat "(\\(?:clojure.core/\\)?\\("
                (regexp-opt '("defn" "defn-" "def" "def-" "defonce"
                              "defmulti" "defmethod" "defmacro"
                              "defstruct" "deftype" "defprotocol"
                              "defrecord" "deftest"
                              "slice" "def\\[a-z\\]"
                              "defalias" "defhinted" "defmacro-"
                              "defn-memo" "defnk" "defonce-"
                              "defstruct-" "defunbound" "defunbound-"
                              "defvar" "defvar-"
                              "definst" "defsynth" "defcgen"))
                ;; Function declarations.
                "\\)\\>"
                ;; Any whitespace
                "[ \r\n\t]*"
                ;; Possibly type or metadata
                "\\(?:#?^\\(?:{[^}]*}\\|\\sw+\\)[ \r\n\t]*\\)*"
                "\\(\\sw+\\)?")
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face nil t))
      ;; (fn name? args ...)
      (,(concat "(\\(?:clojure.core/\\)?\\(fn\\)[ \t]+"
                ;; Possibly type
                "\\(?:#?^\\sw+[ \t]*\\)?"
                ;; Possibly name
                "\\(t\\sw+\\)?" )
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face nil t))

      (,(concat "(\\(\\(?:[a-z\.-]+/\\)?def\[a-z\]*-?\\)"
                ;; Function declarations.
                "\\>"
                ;; Any whitespace
                "[ \r\n\t]*"
                ;; Possibly type or metadata
                "\\(?:#?^\\(?:{[^}]*}\\|\\sw+\\)[ \r\n\t]*\\)*"
                "\\(\\sw+\\)?")
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face nil t))
      ;; Deprecated functions
      (,(concat
         "(\\(?:clojure.core/\\)?"
         (regexp-opt
          '("add-watcher" "remove-watcher" "add-classpath") t)
         "\\>")
       1 font-lock-warning-face)
      ;; Control structures
      (,(concat
         "(\\(?:clojure.core/\\)?"
         (regexp-opt
          '("let" "letfn" "do"
            "case" "cond" "condp"
            "for" "loop" "recur"
            "when" "when-not" "when-let" "when-first"
            "if" "if-let" "if-not"
            "." ".." "->" "->>" "doto"
            "and" "or"
            "dosync" "doseq" "dotimes" "dorun" "doall"
            "load" "import" "unimport" "ns" "in-ns" "refer"
            "try" "catch" "finally" "throw"
            "with-open" "with-local-vars" "binding"
            "gen-class" "gen-and-load-class" "gen-and-save-class"
            "handler-case" "handle") t)
         "\\>")
       1 font-lock-builtin-face)
      ;; Built-ins
      (,(concat
         "(\\(?:clojure.core/\\)?"
         (regexp-opt
          '("*" "*1" "*2" "*3" "*agent*"
        "*allow-unresolved-vars*" "*assert*" "*clojure-version*" "*command-line-args*" "*compile-files*"
        "*compile-path*" "*e" "*err*" "*file*" "*flush-on-newline*"
        "*in*" "*macro-meta*" "*math-context*" "*ns*" "*out*"
        "*print-dup*" "*print-length*" "*print-level*" "*print-meta*" "*print-readably*"
        "*read-eval*" "*source-path*" "*use-context-classloader*" "*warn-on-reflection*" "+"
        "-" "/"
        "<" "<=" "=" "==" ">"
        ">=" "accessor" "aclone"
        "agent" "agent-errors" "aget" "alength" "alias"
        "all-ns" "alter" "alter-meta!" "alter-var-root" "amap"
        "ancestors" "and" "apply" "areduce" "array-map"
        "aset" "aset-boolean" "aset-byte" "aset-char" "aset-double"
        "aset-float" "aset-int" "aset-long" "aset-short" "assert"
        "assoc" "assoc!" "assoc-in" "associative?" "atom"
        "await" "await-for" "await1" "bases" "bean"
        "bigdec" "bigint" "binding" "bit-and" "bit-and-not"
        "bit-clear" "bit-flip" "bit-not" "bit-or" "bit-set"
        "bit-shift-left" "bit-shift-right" "bit-test" "bit-xor" "boolean"
        "boolean-array" "booleans" "bound-fn" "bound-fn*" "butlast"
        "byte" "byte-array" "bytes" "case" "cast" "char"
        "char-array" "char-escape-string" "char-name-string" "char?" "chars"
        "chunk" "chunk-append" "chunk-buffer" "chunk-cons" "chunk-first"
        "chunk-next" "chunk-rest" "chunked-seq?" "class" "class?"
        "clear-agent-errors" "clojure-version" "coll?" "comment" "commute"
        "comp" "comparator" "compare" "compare-and-set!" "compile"
        "complement" "concat" "cond" "condp" "conj"
        "conj!" "cons" "constantly" "construct-proxy" "contains?"
        "count" "counted?" "create-ns" "create-struct" "cycle"
        "dec" "decimal?" "declare" "definline" "defmacro"
        "defmethod" "defmulti" "defn" "defn-" "defonce"
        "defstruct" "delay" "delay?" "deliver" "deref"
        "derive" "descendants" "destructure" "disj" "disj!"
        "dissoc" "dissoc!" "distinct" "distinct?" "doall"
        "doc" "dorun" "doseq" "dosync" "dotimes"
        "doto" "double" "double-array" "doubles" "drop"
        "drop-last" "drop-while" "empty" "empty?" "ensure"
        "enumeration-seq" "eval" "even?" "every?"
        "extend" "extend-protocol" "extend-type" "extends?" "extenders"
        "false?" "ffirst" "file-seq" "filter" "find" "find-doc"
        "find-ns" "find-var" "first" "flatten" "float" "float-array"
        "float?" "floats" "flush" "fn" "fn?"
        "fnext" "for" "force" "format" "future"
        "future-call" "future-cancel" "future-cancelled?" "future-done?" "future?"
        "gen-class" "gen-interface" "gensym" "get" "get-in"
        "get-method" "get-proxy-class" "get-thread-bindings" "get-validator" "group-by"
        "hash" "hash-map" "hash-set" "identical?" "identity" "if-let"
        "if-not" "ifn?" "import" "in-ns" "inc"
        "init-proxy" "instance?" "int" "int-array" "integer?"
        "interleave" "intern" "interpose" "into" "into-array"
        "ints" "io!" "isa?" "iterate" "iterator-seq"
        "juxt" "key" "keys" "keyword" "keyword?"
        "last" "lazy-cat" "lazy-seq" "let" "letfn"
        "line-seq" "list" "list*" "list?" "load"
        "load-file" "load-reader" "load-string" "loaded-libs" "locking"
        "long" "long-array" "longs" "loop" "macroexpand"
        "macroexpand-1" "make-array" "make-hierarchy" "map" "map?"
        "mapcat" "max" "max-key" "memfn" "memoize"
        "merge" "merge-with" "meta" "method-sig" "methods"
        "min" "min-key" "mod" "name" "namespace"
        "neg?" "newline" "next" "nfirst" "nil?"
        "nnext" "not" "not-any?" "not-empty" "not-every?"
        "not=" "ns" "ns-aliases" "ns-imports" "ns-interns"
        "ns-map" "ns-name" "ns-publics" "ns-refers" "ns-resolve"
        "ns-unalias" "ns-unmap" "nth" "nthnext" "num"
        "number?" "odd?" "or" "parents" "partial"
        "partition" "partition-all" "partition-by" "pcalls" "peek" "persistent!" "pmap"
        "pop" "pop!" "pop-thread-bindings" "pos?" "pr"
        "pr-str" "prefer-method" "prefers" "primitives-classnames" "print"
        "print-ctor" "print-doc" "print-dup" "print-method" "print-namespace-doc"
        "print-simple" "print-special-doc" "print-str" "printf" "println"
        "println-str" "prn" "prn-str" "promise" "proxy"
        "proxy-call-with-super" "proxy-mappings" "proxy-name" "proxy-super" "push-thread-bindings"
        "pvalues" "quot" "rand" "rand-int" "range"
        "ratio?" "rational?" "rationalize" "re-find" "re-groups"
        "re-matcher" "re-matches" "re-pattern" "re-seq" "read"
        "read-line" "read-string" "reify" "reduce" "ref" "ref-history-count"
        "ref-max-history" "ref-min-history" "ref-set" "refer" "refer-clojure"
        "release-pending-sends" "rem" "remove" "remove-method" "remove-ns"
        "repeat" "repeatedly" "replace" "replicate"
        "require" "reset!" "reset-meta!" "resolve" "rest"
        "resultset-seq" "reverse" "reversible?" "rseq" "rsubseq"
        "satisfies?" "second" "select-keys" "send" "send-off" "seq"
        "seq?" "seque" "sequence" "sequential?" "set"
        "set-validator!" "set?" "short" "short-array" "shorts"
        "shutdown-agents" "slurp" "some" "sort" "sort-by"
        "sorted-map" "sorted-map-by" "sorted-set" "sorted-set-by" "sorted?"
        "special-form-anchor" "special-symbol?" "spit" "split-at" "split-with" "str"
        "stream?" "string?" "struct" "struct-map" "subs"
        "subseq" "subvec" "supers" "swap!" "symbol"
        "symbol?" "sync" "syntax-symbol-anchor" "take" "take-last"
        "take-nth" "take-while" "test" "the-ns" "time"
        "to-array" "to-array-2d" "trampoline" "transient" "tree-seq"
        "true?" "type" "unchecked-add" "unchecked-dec" "unchecked-divide"
        "unchecked-inc" "unchecked-multiply" "unchecked-negate" "unchecked-remainder" "unchecked-subtract"
        "underive" "unquote" "unquote-splicing" "update-in" "update-proxy"
        "use" "val" "vals" "var-get" "var-set"
        "var?" "vary-meta" "vec" "vector" "vector?"
        "when" "when-first" "when-let" "when-not" "while"
        "with-bindings" "with-bindings*" "with-in-str" "with-loading-context" "with-local-vars"
        "with-meta" "with-open" "with-out-str" "with-precision" "xml-seq" "zipmap"
        ) t)
         "\\>")
       1 font-lock-variable-name-face)
      ;;Other namespaces in clojure.jar
      (,(concat
         "(\\(?:\.*/\\)?"
         (regexp-opt
          '(;; clojure.inspector
        "atom?" "collection-tag" "get-child" "get-child-count" "inspect"
        "inspect-table" "inspect-tree" "is-leaf" "list-model" "list-provider"
        ;; clojure.main
        "load-script" "main" "repl" "repl-caught" "repl-exception"
        "repl-prompt" "repl-read" "skip-if-eol" "skip-whitespace" "with-bindings"
        ;; clojure.set
        "difference" "index" "intersection" "join" "map-invert"
        "project" "rename" "rename-keys" "select" "union"
        ;; clojure.stacktrace
        "e" "print-cause-trace" "print-stack-trace" "print-throwable" "print-trace-element"
        ;; clojure.template
        "do-template" "apply-template"
        ;; clojure.test
        "*initial-report-counters*" "*load-tests*" "*report-counters*" "*stack-trace-depth*" "*test-out*"
        "*testing-contexts*" "*testing-vars*" "are" "assert-any" "assert-expr"
        "assert-predicate" "compose-fixtures" "deftest" "deftest-" "file-position"
        "function?" "get-possibly-unbound-var" "inc-report-counter" "is" "join-fixtures"
        "report" "run-all-tests" "run-tests" "set-test" "successful?"
        "test-all-vars" "test-ns" "test-var" "testing" "testing-contexts-str"
        "testing-vars-str" "try-expr" "use-fixtures" "with-test" "with-test-out"
        ;; clojure.walk
        "keywordize-keys" "macroexpand-all" "postwalk" "postwalk-demo" "postwalk-replace"
        "prewalk" "prewalk-demo" "prewalk-replace" "stringify-keys" "walk"
        ;; clojure.xml
        "*current*" "*sb*" "*stack*" "*state*" "attrs"
        "content" "content-handler" "element" "emit" "emit-element"
        ;; clojure.zip
        "append-child" "branch?" "children" "down" "edit"
        "end?" "insert-child" "insert-left" "insert-right" "left"
        "leftmost" "lefts" "make-node" "next" "node"
        "path" "prev" "remove" "replace" "right"
        "rightmost" "rights" "root" "seq-zip" "up"
        ) t)
         "\\>")
       1 font-lock-type-face)
      ;; Constant values (keywords), including as metadata e.g. ^:static
      ("\\<^?:\\(\\sw\\|#\\)+\\>" 0 font-lock-constant-face)
      ;; Meta type annotation #^Type or ^Type
      ("#?^\\sw+" 0 font-lock-preprocessor-face)
      ("\\<io\\!\\>" 0 font-lock-warning-face)

      ;;Interop Matching

      ;; Should match:
      ;; .foo .barBaz .qux01 .-flibble .-flibbleWobble Foo Bar$Baz Qux_ World_OpenUDP Foo/Bar foo.bar.Baz foo.Bar/baz Foo. BarBaz. Qux$Quux. Corge9. Foo/barBaz. fooBar

      ;; Shouldn't match:
      ;; FOO bar bar-baz clojure.repl/source

      ;; Foo. BarBaz. Qux$Quux. Corge9. Foo/barBaz.
      ("\\<[A-Z][a-zA-Z0-9$./]*\\.\\>" 0 font-lock-preprocessor-face)

      ;; .foo .barBaz .qux01 .-flibble .-flibbleWobble
      ("\\<\\.-?[a-z][a-zA-Z0-9]*\\>" 0 font-lock-preprocessor-face)

      ;; Foo Bar$Baz Qux_ World_OpenUDP Foo/Bar
      ("\\<[A-Z][a-zA-Z0-9/$_]*[a-z]+[a-zA-Z0-9/$_]*\\>" 0 font-lock-preprocessor-face)

      ;; foo.bar.Baz foo.Bar/baz
      ("\\<[a-z]+\\.[a-zA-Z0-9._]*[A-Z]+[a-zA-Z0-9/.$]*\\>" 0 font-lock-preprocessor-face)

      ;; fooBar
      ("[a-z]*[A-Z]+[a-z][a-zA-Z0-9$]*\\>" 0 font-lock-preprocessor-face)
      ))

  "Default expressions to highlight in Clojure mode.")


;; Docstring positions
(put 'ns 'clojure-doc-string-elt 2)
(put 'defn 'clojure-doc-string-elt 2)
(put 'defn- 'clojure-doc-string-elt 2)
(put 'defmulti 'clojure-doc-string-elt 2)
(put 'defmacro 'clojure-doc-string-elt 2)
(put 'definline 'clojure-doc-string-elt 2)
(put 'defprotocol 'clojure-doc-string-elt 2)



(defun clojure-forward-sexp (n)
  "Treat record literals like #user.Foo[1] and #user.Foo{:size 1}
as a single sexp so that slime will send them properly. Arguably
this behavior is unintuitive for the user pressing (eg) C-M-f
himself, but since these are single objects I think it's right."
  (let ((dir (if (> n 0) 1 -1))
        (forward-sexp-function nil)) ; force the built-in version
    (while (not (zerop n))
      (forward-sexp dir)
      (when (save-excursion ; move back to see if we're in a record literal
              (and
               (condition-case nil
                   (progn (backward-sexp) 't)
                 ('scan-error nil))
               (looking-at "#\\w")))
        (forward-sexp dir)) ; if so, jump over it
      (setq n (- n dir)))))

(defun clojure-indent-function (indent-point state)
  "This function is the normal value of the variable `lisp-indent-function'.
It is used when indenting a line within a function call, to see if the
called function says anything special about how to indent the line.

INDENT-POINT is the position where the user typed TAB, or equivalent.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function
which has a non-nil property `lisp-indent-function',
that specifies how to do the indentation.  The property value can be
* `defun', meaning indent `defun'-style;
* an integer N, meaning indent the first N arguments specially
  like ordinary function arguments and then indent any further
  arguments like a body;
* a function to call just as this function was called.
  If that function returns nil, that means it doesn't specify
  the indentation.

This function also returns nil meaning don't specify the indentation."
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        ;; car of form doesn't seem to be a symbol
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
                                         calculate-lisp-indent-last-sexp 0 t)))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-lisp-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (if (and (eq (char-after (point)) ?\[)
                   (eq (char-after (elt state 1)) ?\())
              (+ (current-column) 2) ;; this is probably inside a defn
            (current-column)))
      (let* ((function (buffer-substring (point)
                                         (progn (forward-sexp 1) (point))))
             (open-paren (elt state 1))
             (method nil)
             (function-tail (first
                             (last
                              (split-string (substring-no-properties function) "/")))))
        (setq method (get (intern-soft function-tail) 'clojure-indent-function))

        (cond ((member (char-after open-paren) '(?\[ ?\{))
               (goto-char open-paren)
               (1+ (current-column)))
              ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`\\(?:\\S +/\\)?def\\|with-"
                                      function)))
               (lisp-indent-defform state indent-point))

              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method indent-point state))
              (clojure-mode-use-backtracking-indent
               (clojure-backtracking-indent
                indent-point state normal-indent)))))))

(defun clojure-backtracking-indent (indent-point state normal-indent)
  "Experimental backtracking support. Will upwards in an sexp to
check for contextual indenting."
  (let (indent (path) (depth 0))
    (goto-char (elt state 1))
    (while (and (not indent)
                (< depth clojure-max-backtracking))
      (let ((containing-sexp (point)))
        (parse-partial-sexp (1+ containing-sexp) indent-point 1 t)
        (when (looking-at "\\sw\\|\\s_")
          (let* ((start (point))
                 (fn (buffer-substring start (progn (forward-sexp 1) (point))))
                 (meth (get (intern-soft fn) 'clojure-backtracking-indent)))
            (let ((n 0))
              (when (< (point) indent-point)
                (condition-case ()
                    (progn
                      (forward-sexp 1)
                      (while (< (point) indent-point)
                        (parse-partial-sexp (point) indent-point 1 t)
                        (incf n)
                        (forward-sexp 1)))
                  (error nil)))
              (push n path))
            (when meth
              (let ((def meth))
                (dolist (p path)
                  (if (and (listp def)
                           (< p (length def)))
                      (setq def (nth p def))
                    (if (listp def)
                        (setq def (car (last def)))
                      (setq def nil))))
                (goto-char (elt state 1))
                (when def
                  (setq indent (+ (current-column) def)))))))
        (goto-char containing-sexp)
        (condition-case ()
            (progn
              (backward-up-list 1)
              (incf depth))
          (error (setq depth clojure-max-backtracking)))))
    indent))

;; clojure backtracking indent is experimental and the format for these
;; entries are subject to change
(put 'implement 'clojure-backtracking-indent '(4 (2)))
(put 'letfn 'clojure-backtracking-indent '((2) 2))
(put 'proxy 'clojure-backtracking-indent '(4 4 (2)))
(put 'reify 'clojure-backtracking-indent '((2)))
(put 'deftype 'clojure-backtracking-indent '(4 4 (2)))
(put 'defrecord 'clojure-backtracking-indent '(4 4 (2)))
(put 'defprotocol 'clojure-backtracking-indent '(4 (2)))
(put 'extend-type 'clojure-backtracking-indent '(4 (2)))
(put 'extend-protocol 'clojure-backtracking-indent '(4 (2)))

(defun put-clojure-indent (sym indent)
  (put sym 'clojure-indent-function indent))

(defmacro define-clojure-indent (&rest kvs)
  `(progn
     ,@(mapcar (lambda (x) `(put-clojure-indent
                        (quote ,(first x)) ,(second x))) kvs)))

(defun add-custom-clojure-indents (name value)
  (custom-set-default name value)
  (mapcar (lambda (x)
            (put-clojure-indent x 'defun))
          value))

(defcustom clojure-defun-indents nil
  "List of symbols to give defun-style indentation to in Clojure
code, in addition to those that are built-in. You can use this to
get emacs to indent your own macros the same as it does the
built-ins like with-open. To set manually from lisp code,
use (put-clojure-indent 'some-symbol 'defun)."
  :type '(repeat symbol)
  :group 'clojure-mode
  :set 'add-custom-clojure-indents)

(define-clojure-indent
  ;; built-ins
  (ns 1)
  (fn 'defun)
  (def 'defun)
  (defn 'defun)
  (bound-fn 'defun)
  (if 1)
  (if-not 1)
  (case 1)
  (condp 2)
  (when 1)
  (while 1)
  (when-not 1)
  (when-first 1)
  (do 0)
  (future 0)
  (comment 0)
  (doto 1)
  (locking 1)
  (proxy 2)
  (with-open 1)
  (with-precision 1)
  (with-local-vars 1)

  (reify 'defun)
  (deftype 2)
  (defrecord 2)
  (defprotocol 1)
  (extend 1)
  (extend-protocol 1)
  (extend-type 1)

  (try 0)
  (catch 2)
  (finally 0)

  ;; binding forms
  (let 1)
  (letfn 1)
  (binding 1)
  (loop 1)
  (for 1)
  (doseq 1)
  (dotimes 1)
  (when-let 1)
  (if-let 1)

  ;; data structures
  (defstruct 1)
  (struct-map 1)
  (assoc 1)

  (defmethod 'defun)

  ;; clojure.test
  (testing 1)
  (deftest 'defun)
  (use-fixtures 'defun))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Better docstring filling for clojure-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clojure-string-start ()
  "Return the position of the \" that begins the string at point."
  (save-excursion
    (save-match-data
      ;; Find a quote that appears immediately after whitespace,
      ;; beginning of line, or an open paren, brace, or bracket
      (re-search-backward "\\(\\s-\\|^\\|(\\|\\[\\|{\\)\\(\"\\)")
      (match-beginning 2))))

(defun clojure-char-at-point ()
  "Return the char at point or nil if at buffer end."
  (when (not (= (point) (point-max)))
   (buffer-substring-no-properties (point) (1+ (point)))))

(defun clojure-char-before-point ()
  "Return the char before point or nil if at buffer beginning."
  (when (not (= (point) (point-min)))
    (buffer-substring-no-properties (point) (1- (point)))))

;; TODO: Deal with the fact that when point is exactly at the
;; beginning of a string, it thinks that is the end.
(defun clojure-string-end ()
  "Return the position of the \" that ends the string at point.

Note that point must be inside the string - if point is
positioned at the opening quote, incorrect results will be
returned."
  (save-excursion
    (save-match-data
      ;; If we're at the end of the string, just return point.
      (if (and (string= (clojure-char-at-point) "\"")
               (not (string= (clojure-char-before-point) "\\")))
          (point)
        ;; We don't want to get screwed by starting out at the
        ;; backslash in an escaped quote.
        (when (string= (clojure-char-at-point) "\\")
          (backward-char))
        ;; Look for a quote not preceeded by a backslash
        (re-search-forward "[^\\]\\\(\\\"\\)")
        (match-beginning 1)))))

(defun clojure-docstring-start+end-points ()
  "Return the start and end points of the string at point as a cons."
  (if (and (fboundp 'paredit-string-start+end-points) paredit-mode)
      (paredit-string-start+end-points)
    (cons (clojure-string-start) (clojure-string-end))))

(defun clojure-mark-string ()
  "Mark the string at point."
  (interactive)
  (goto-char (clojure-string-start))
  (forward-char)
  (set-mark (clojure-string-end)))

(defun clojure-fill-docstring (&optional argument)
  "Fill the definition that the point is on appropriate for Clojure.

  Fills so that every paragraph has a minimum of two initial spaces,
  with the exception of the first line. Fill margins are taken from
  paragraph start, so a paragraph that begins with four spaces will
  remain indented by four spaces after refilling."
  (interactive "P")
  (if (and (fboundp 'paredit-in-string-p) paredit-mode)
      (unless (paredit-in-string-p)
        (error "Must be inside a string")))
  ;; Oddly, save-excursion doesn't do a good job of preserving point.
  ;; It's probably because we delete the string and then re-insert it.
  (let ((old-point (point)))
    (save-restriction
      (save-excursion
        (let* ((string-region (clojure-docstring-start+end-points))
               (string-start (1+ (car string-region)))
               (string-end (cdr string-region))
               (string (buffer-substring-no-properties (1+ (car string-region))
                                                       (cdr string-region))))
          (delete-region string-start string-end)
          (insert
           (with-temp-buffer
             (insert string)
             (let ((left-margin 2))
               (delete-trailing-whitespace)
               (fill-region (point-min) (point-max))
               (buffer-substring-no-properties (+ 2 (point-min)) (point-max))))))))
    (goto-char old-point)))



(defconst clojure-namespace-name-regex
  (rx line-start
      "("
      (zero-or-one (group (regexp "clojure.core/")))
      (zero-or-one (submatch "in-"))
      "ns"
      (zero-or-one "+")
      (one-or-more (any whitespace "\n"))
      (zero-or-more (or (submatch (zero-or-one "#")
                                  "^{"
                                  (zero-or-more (not (any "}")))
                                  "}")
                        (zero-or-more "^:"
                                      (one-or-more (not (any whitespace)))))
                    (one-or-more (any whitespace "\n")))
      ;; why is this here? oh (in-ns 'foo) or (ns+ :user)
      (zero-or-one (any ":'"))
      (group (one-or-more (not (any "()\"" whitespace))) word-end)))

;; for testing clojure-namespace-name-regex, you can evaluate this code and make
;; sure foo (or whatever the namespace name is) shows up in results. some of
;; these currently fail.
;; (mapcar (lambda (s) (let ((n (string-match clojure-namespace-name-regex s)))
;;                       (if n (match-string 4 s))))
;;         '("(ns foo)"
;;           "(ns
;; foo)"
;;           "(ns foo.baz)"
;;           "(ns ^:bar foo)"
;;           "(ns ^:bar ^:baz foo)"
;;           "(ns ^{:bar true} foo)"
;;           "(ns #^{:bar true} foo)"
;;           "(ns #^{:fail {}} foo)"
;;           "(ns ^{:fail2 {}} foo.baz)"
;;           "(ns ^{} foo)"
;;           "(ns ^{:skip-wiki true}
;;   aleph.netty
;; "
;;           "(ns
;;  foo)"
;;     "foo"))



(defun clojure-expected-ns ()
  "Returns the namespace name that the file should have."
  (let* ((project-dir (file-truename
                       (locate-dominating-file default-directory
                                               "project.clj")))
         (relative (substring (buffer-file-name) (length project-dir) -4)))
    (replace-regexp-in-string
     "_" "-" (mapconcat 'identity (cdr (split-string relative "/")) "."))))

(defun clojure-insert-ns-form ()
  (interactive)
  (goto-char (point-min))
  (insert (format "(ns %s)" (clojure-expected-ns))))

(defun clojure-update-ns ()
  "Updates the namespace of the current buffer. Useful if a file has been renamed."
  (interactive)
  (let ((nsname (clojure-expected-ns)))
    (when nsname
      (save-restriction
        (save-excursion
          (save-match-data
            (if (clojure-find-ns)
                (replace-match nsname nil nil nil 4)
              (error "Namespace not found"))))))))

(defun clojure-find-ns ()
  (let ((regexp clojure-namespace-name-regex))
    (save-restriction
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward regexp nil t)
          (match-string-no-properties 4))))))

(defalias 'clojure-find-package 'clojure-find-ns)

;; Test navigation:
(defun clojure-in-tests-p ()
  (or (string-match-p "test\." (clojure-find-ns))
      (string-match-p "/test" (buffer-file-name))))

(defun clojure-underscores-for-hyphens (namespace)
  (replace-regexp-in-string "-" "_" namespace))

(defun clojure-test-for (namespace)
  (let* ((namespace (clojure-underscores-for-hyphens namespace))
         (segments (split-string namespace "\\.")))
    (mapconcat 'identity segments "/")))

(defun clojure-jump-to-test ()
  "Jump from implementation file to test."
  (interactive)
  (find-file (format "%stest/%s_test.clj"
                     (file-name-as-directory
                      (locate-dominating-file buffer-file-name "src/"))
                     (clojure-test-for (clojure-find-ns)))))

(defun clojure-jump-between-tests-and-code ()
  (interactive)
  (if (clojure-in-tests-p)
      (clojure-test-jump-to-implementation)
    (clojure-jump-to-test)))

;;;###autoload
(progn
  (put 'clojure-test-ns-segment-position 'safe-local-variable 'integerp)
  (put 'clojure-mode-load-command 'safe-local-variable 'stringp)

  (add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))
  (add-to-list 'interpreter-mode-alist '("jark" . clojure-mode))
  (add-to-list 'interpreter-mode-alist '("cake" . clojure-mode)))

(provide 'clojure-mode)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; clojure-mode.el ends here
