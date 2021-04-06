(require 'cl-lib)

(Given "^I open file \"\\(.+\\)\"$"
       (lambda (filename)
         (setq default-directory clj-refactor-root-path)
         (find-file filename)))

(Given "^I have a project \"\\([^\"]+\\)\" in \"\\([^\"]+\\)\"$"
       (lambda (project-name dir-name)
         (setq default-directory clj-refactor-root-path)

         ;; delete old directory
         (when (file-exists-p dir-name)
           (delete-directory dir-name t))

         ;; create directory structure
         (mkdir (expand-file-name project-name (expand-file-name "src" dir-name)) t)
         (mkdir (expand-file-name project-name (expand-file-name "test" dir-name)) t)

         ;; add project.clj
         (with-temp-file (expand-file-name "project.clj" dir-name)
           (insert "(defproject " project-name " \"0.1.0-SNAPSHOT\")"))))

(Given "^I have a clojure-file \"\\([^\"]+\\)\"$"
       (lambda (file-name)
         (setq default-directory clj-refactor-root-path)
         (find-file file-name)
         (save-buffer)
         (kill-buffer)))

(Given "^I switch project-clean-prompt off$"
       (lambda ()
         (setq cljr-project-clean-prompt nil)))

(Given "^I switch warn-on-analyzer-needs-eval off$"
       (lambda ()
         (setq cljr-warn-on-eval nil)))

(Given "^I exit multiple-cursors-mode"
       (lambda ()
         (multiple-cursors-mode 0)))

(Given "^I don't use multiple-cursors"
       (lambda ()
         (setq cljr-use-multiple-cursors nil)))

(defun cljr--plist-to-hash (plist)
  (let ((h (make-hash-table)))
    (dolist (k (-filter #'keywordp plist))
      (puthash k (plist-get plist k) h))
    h))

(Given "^I call the rename callback directly with mock data for foo->baz"
       (lambda ()
         (cljr--rename-occurrences "example.two"
                                   (list (cljr--plist-to-hash
                                          '(:line-beg 3 :line-end 4 :col-beg 7 :col-end 9
                                                      :name "foo"
                                                      :file "tmp/src/example/two.clj"
                                                      :match ""))
                                         (cljr--plist-to-hash
                                          '(:line-beg 5 :line-end 5 :col-beg 15
                                                      :col-end 23 :name "foo"
                                                      :file "tmp/src/example/one.clj"
                                                      :match "")))
                                   "baz")))

(Given "^I call the rename callback directly with mock data for star->asterisk"
       (lambda ()
         (cljr--rename-occurrences "example.two"
                                   (list (cljr--plist-to-hash
                                          '(:line-beg 6 :line-end 7 :col-beg 7
                                                      :col-end 10 :name "star*"
                                                      :file "tmp/src/example/two.clj"
                                                      :match ""))
                                         (cljr--plist-to-hash
                                          '(:line-beg 8 :line-end 8 :col-beg 17
                                                      :col-end 27 :name "star*"
                                                      :file "tmp/src/example/one.clj"
                                                      :match "")))
                                   "asterisk*")))

(defun cljr--make-seeded-hash-table (&rest keys-and-values)
  (let ((m (make-hash-table :test #'equal))
        (kv-pairs (-partition 2 keys-and-values)))
    (dolist (pair kv-pairs)
      (puthash (car pair) (cadr pair) m))
    m))

(Given "^I call the add-missing-libspec callback directly with mock data to import"
       (lambda ()
         (cljr--add-missing-libspec
          "Date" (list (cljr--make-seeded-hash-table
                        :name 'java.util.Date :type :class)))))

(Given "^I call the add-missing-libspec callback directly with mock data to refer split"
       (lambda ()
         (cljr--add-missing-libspec
          "split" (list (cljr--make-seeded-hash-table
                         :name 'clojure.string :type :ns)))))

(Given "^I call the add-missing-libspec callback directly with mock data to alias clojure.string"
       (lambda ()
         (cljr--add-missing-libspec "str/split"
                                    (list (cljr--make-seeded-hash-table
                                           :name 'clojure.string :type :ns)))))

(Given "^I call the add-missing-libspec callback directly with mock data to require WebrequestHandler"
       (lambda ()
         (cljr--add-missing-libspec
          "WebrequestHandler"
          (list (cljr--make-seeded-hash-table
                 :name 'modular.ring.WebrequestHandler :type :type)))))

(Given "^I call the add-missing-libspec callback directly with mock data to require schema.test"
       (lambda ()
         (cljr--add-missing-libspec "schema.test/validate-schemas"
                                    (list (cljr--make-seeded-hash-table
                                           :name 'schema.test :type :ns)))))

(Given "^I call the add-missing-libspec callback directly with mock data to require a schema"
       (lambda ()
         (cljr--add-missing-libspec "CustomerId"
                                    (list (cljr--make-seeded-hash-table
                                           :name 'my.schemas :type :ns)))))

(Then "^the file should be named \"\\([^\"]+\\)\"$"
      (lambda (file-name-postfix)
        (assert (s-ends-with? file-name-postfix (buffer-file-name)) nil "Expected %S to end with %S" (buffer-file-name) file-name-postfix)))

(And "^the cursor is inside the first defn form$"
     (lambda ()
       (goto-char (point-min))
       (re-search-forward "defn")))

(Given "^I call the add-stubs function directly with mock data for java.util.List from the middleware"
       (lambda ()
         (cljr--insert-function-stubs (edn-read "(
{:parameter-list \"[^int arg]\", :name \"remove\"}
{:parameter-list \"[^int arg0 ^Object arg1]\", :name \"add\"}
{:parameter-list \"[^java.util.function.UnaryOperator arg]\", :name \"replaceAll\"}
{:parameter-list \"[^java.util.Collection arg]\", :name \"containsAll\"}
{:parameter-list \"[^java.util.Collection arg]\", :name \"removeAll\"}
{:parameter-list \"[]\", :name \"listIterator\"}
{:parameter-list \"[^int arg0 ^int arg1]\", :name \"subList\"}
{:parameter-list \"[]\", :name \"iterator\"}
{:parameter-list \"[^Object arg]\", :name \"lastIndexOf\"}
{:parameter-list \"[^int arg]\", :name \"listIterator\"}
{:parameter-list \"[^int arg0 ^java.util.Collection arg1]\", :name \"addAll\"}
{:parameter-list \"[^Object arg]\", :name \"add\"}
{:parameter-list \"[^int arg]\", :name \"get\"}
{:parameter-list \"[]\", :name \"toArray\"}
{:parameter-list \"[]\", :name \"clear\"}
{:parameter-list \"[^int arg0 ^Object arg1]\", :name \"set\"}
{:parameter-list \"[^java.util.Collection arg]\", :name \"retainAll\"}
{:parameter-list \"[]\", :name \"isEmpty\"}
{:parameter-list \"[^java.util.Collection arg]\", :name \"addAll\"}
{:parameter-list \"[]\", :name \"spliterator\"}
{:parameter-list \"[^Object arg]\", :name \"indexOf\"}
{:parameter-list \"[^Object... arg]\", :name \"toArray\"}
{:parameter-list \"[^java.util.Comparator arg]\", :name \"sort\"}
{:parameter-list \"[]\", :name \"size\"}
{:parameter-list \"[^Object arg]\", :name \"equals\"}
{:parameter-list \"[^Object arg]\", :name \"remove\"}
{:parameter-list \"[]\", :name \"hashCode\"}
{:parameter-list \"[^Object arg]\", :name \"contains\"})"))))

(Given "^I call the add-stubs function directly with mock data for clojure.reflect from the middleware"
       (lambda ()
         (cljr--insert-function-stubs (edn-read "({:parameter-list \"[this]\", :name \"typename\"})"))))

(Given "I call the cljr--inline-symbol function directly with mockdata to inline my-constant"
       (lambda ()
         (let ((response (edn-read "{:occurrences ({:match \"(println my-constant my-constant another-val)))\"
:file \"core.clj\"
:name \"refactor-nrepl.test/my-constant\"
:col-end 26
:col-beg 14
:line-end 5
:line-beg 5} {:match \"(println my-constant my-constant another-val)))\"
:file \"core.clj\"
:name \"refactor-nrepl.test/my-constant\"
:col-end 38
:col-beg 26
:line-end 5
:line-beg 5})
:definition {:line-beg 1
:line-end 1
:col-beg 1
:col-end 22
:name \"refactor-nrepl.test/my-constant\"
:file \"core.clj\"
:match \"(def my-constant 123)\"
:definition \"123\"}}")))
           (cljr--inline-symbol "fake-ns" (gethash :definition response)
                                (gethash :occurrences response)))))

(Given "I call the cljr--inline-symbol function directly with mockdata to inline another-val"
       (lambda ()
         (let ((response (edn-read "{:definition {:line-beg 4
:line-end 4
:col-beg 9
:col-end 21
:name \"another-val\"
:file \"core.clj\"
:match \"(let [another-val 321]\"
:definition \"321\"}
:occurrences ({:match \"(println my-constant my-constant another-val)))\"
:file \"core.clj\"
:name \"another-val\"
:col-end 50
:col-beg 38
:line-end 5
:line-beg 5})}")))
           (cljr--inline-symbol "fake-ns" (gethash :definition response)
                                (gethash :occurrences response)))))

(Given "I call the cljr--inline-symbol function directly with mockdata to inline some-val"
       (lambda ()
         (let ((response (edn-read "{:definition {:line-beg 5
:line-end 5
:col-beg 9
:col-end 17
:name \"some-val\"
:file \"core.clj\"
:match \"some-val 110]\"
:definition \"110\"}
:occurrences ()}")))
           (cljr--inline-symbol "fake-ns" (gethash :definition response)
                                (gethash :occurrences response)))))

(Given "I call the cljr--inline-symbol function directly with mockdata to inline my-inc"
       (lambda ()
         (let ((response (edn-read "{:definition {:definition \"(fn [n]\\n  (+ 1 n))\"
:line-beg 1
:line-end 2
:col-beg 1
:col-end 11
:name \"refactor-nrepl.foo/my-inc\"
:file \"core.clj\"
:match \"(defn my-inc [n]\\n  (+ 1 n))\"}
:occurrences ({:line-beg 4
:line-end 4
:col-beg 5
:col-end 12
:name \"refactor-nrepl.foo/my-inc\"
:file \"core.clj\"
:match \"(+ (my-inc (- 17 4) 55))\"} {:line-beg 6
:line-end 6
:col-beg 6
:col-end 13
:name \"refactor-nrepl.foo/my-inc\"
:file \"core.clj\"
:match \"(map my-inc (range 10))\"})}")))
           (cljr--inline-symbol "fake-ns" (gethash :definition response)
                                (gethash :occurrences response)))))

(And "I mock out the call to the middleware to find locals$"
     (lambda ()
       (defun cljr--call-middleware-to-find-used-locals (file line column)
         "")))

(And "The middleware is mocked to return foo bar as locals$"
     (lambda ()
       (defun cljr--call-middleware-to-find-used-locals (file line column)
         "foo bar")))

(defun cljr--make-signature-change
    (old-index new-index old-name &optional new-name)
  (let ((h (make-hash-table)))
    (puthash :old-index old-index h)
    (puthash :new-index new-index h)
    (puthash :old-name old-name h)
    (if new-name
        (puthash :new-name new-name h)
      (puthash :new-name old-name h))
    h))

(setq cljr--test-occurrences
      '((:line-beg 4
                   :line-end 4
                   :col-beg 4
                   :col-end 7
                   :name "core/tt"
                   :file "core.clj"
                   :match "(tt 1 2 3))")
        (:line-beg 3
                   :line-end 3
                   :col-beg 1
                   :col-end 25
                   :name "core/tt"
                   :file "core.clj"
                   :match "(defn tt [foo bar baz]\n  (println foo bar baz))")
        (:line-beg 4
                   :line-end 4
                   :col-beg 8
                   :col-end 11
                   :name "core/tt"
                   :file "core.clj"
                   :match "(map tt [1] [2] [3]))")
        (:line-beg 4
                   :line-end 4
                   :col-beg 17
                   :col-end 20
                   :name "core/tt"
                   :file "core.clj"
                   :match "(map (partial tt [1]) [2] [3]))")
        (:line-beg 5
                   :line-end 5
                   :col-beg 12
                   :col-end 15
                   :name "core/tt"
                   :file "core.clj"
                   :match "(apply tt [1] [2] args)))"))
      cljr--foo-bar-swapped (list (cljr--make-signature-change 0 1 "foo")
                                  (cljr--make-signature-change 1 0 "bar")
                                  (cljr--make-signature-change 2 2 "baz"))
      cljr--bar-baz-swapped (list (cljr--make-signature-change 0 0 "foo")
                                  (cljr--make-signature-change 1 2 "bar")
                                  (cljr--make-signature-change 2 1 "baz"))
      cljr--baz-renamed-to-qux (list (cljr--make-signature-change 0 0 "foo")
                                     (cljr--make-signature-change 1 1 "bar")
                                     (cljr--make-signature-change 2 2 "baz" "qux")))

(Given "I call the cljr--change-function-signature function directly with mockdata to swap foo and bar in a regular call-site"
       (lambda ()
         (cljr--change-function-signature (list (cljr--plist-to-hash (cl-first cljr--test-occurrences)))
                                          cljr--foo-bar-swapped)))

(Given "I call the cljr--change-function-signature function directly with mockdata to swap foo and bar in function definition"
       (lambda ()
         (cljr--change-function-signature (list (cljr--plist-to-hash (cl-second cljr--test-occurrences)))
                                          cljr--foo-bar-swapped)))

(Given "I call the cljr--change-function-signature function directly with mockdata to swap foo and bar in a higher-order call-site"
       (lambda ()
         (cljr--change-function-signature (list (cljr--plist-to-hash (cl-third cljr--test-occurrences)))
                                          cljr--foo-bar-swapped)))

(Given "I call the cljr--change-function-signature function directly with mockdata to swap foo and bar in a partial call-site"
       (lambda ()
         (cljr--change-function-signature (list (cljr--plist-to-hash (cl-fourth cljr--test-occurrences)))
                                          cljr--foo-bar-swapped)))

(Given "I call the cljr--change-function-signature function directly with mockdata to swap foo and bar in in partial application"
       (lambda ()
         (cljr--change-function-signature (list (cljr--plist-to-hash (cl-fourth cljr--test-occurrences)))
                                          cljr--foo-bar-swapped)))

(Given "I call the cljr--change-function-signature function directly with mockdata to swap bar and baz in a call-site with apply"
       (lambda ()
         (cljr--change-function-signature (list (cljr--plist-to-hash (cl-fifth cljr--test-occurrences)))
                                          cljr--bar-baz-swapped)))

(Given "I call the cljr--change-function-signature function directly with mockdata to swap foo and bar in a call-site with apply"
       (lambda ()
         (cljr--change-function-signature (list (cljr--plist-to-hash (cl-fifth cljr--test-occurrences)))
                                          cljr--foo-bar-swapped)))

(Given "I call the cljr--change-function-signature function directly with mockdata to rename baz to qux"
       (lambda ()
         (cl-letf (((symbol-function 'cljr-rename-symbol)
                    (lambda (new-name)
                      (backward-char)
                      (replace-regexp "baz" new-name nil (point)
                                      (cljr--point-after '(paredit-forward-up 2))))))
           (cljr--change-function-signature (list (cljr--plist-to-hash (cl-second cljr--test-occurrences)))
                                            cljr--baz-renamed-to-qux))))

(Given "The cache of namespace aliases is populated"
       (lambda ()
         (defun cljr--call-middleware-for-namespace-aliases ()
           (edn-read "{:clj {t (clojure.test)
set (clojure.set)
pprint (clojure.pprint)
util (refactor-nrepl.util clojure.tools.analyzer.jvm.utils)
readers (clojure.tools.reader.reader-types) }
:cljs {set (clojure.set)
pprint (cljs.pprint)}}"))))

(When "I kill the \"\\(.+\\)\" buffer"
      (lambda (buffer)
        (kill-matching-buffers-dont-ask buffer)))

(And "^I save the file$"
     (lambda ()
       (save-buffer)))

(Then "^The buffer should be modified$"
      (lambda ()
        (assert (buffer-modified-p))))

(Then "^The buffer should not be modified$"
      (lambda ()
        (assert (not (buffer-modified-p)))))

(defun validate-helper-type (type)
  (let ((all-descriptions (-map 'cdr cljr--all-helpers))
        result)
    (--each (-filter (lambda (desc) (-contains? (-last-item desc) type))
                     all-descriptions)
      (if (-contains? result (nth 2 it))
          (error "'%s' is a duplicate in type '%s'!" (byte-to-string (nth 2 it)) type)
        (!cons (nth 2 it) result)))))

(defun validate-all-helpers ()
  (-each
      (->> cljr--all-helpers
           (-map (lambda (fn-description) (-last-item (cdr fn-description))))
           (-flatten)
           (-distinct))
    'validate-helper-type))

(When "I run cljr--all-helpers check"
      (lambda ()
        (validate-all-helpers)))

(Given "^I call replace refer all with alias with mock data for \"\\(.*\\)\"$"
       (lambda (ns-name)
         (cljr--replace-refer-all-with-alias
          ns-name
          (edn-read
           "({:line-beg 5 :line-end 5 :col-beg 13 :col-end 30 :file\"one.clj\" :name \"foo\"} {:line-beg 8 :line-end 8 :col-beg 15 :col-end 28 :file \"one.clj\" :name \"star*\"})")
          "two")))

(Given "^I call find usages for \"\\(.*\\)\"$"
       (lambda (symbol-name)
         (cljr--setup-find-symbol-buffer symbol-name)
         (-map 'cljr--format-and-insert-symbol-occurrence
               (list '(dict "occurrence" "{:line-beg 3 :line-end 4 :col-beg 7 :col-end 9 :name \"foo\" :file \"tmp/src/example/two.clj\" :match \"(defn foo []\"}")
                     '(dict "occurrence" "{:line-beg 5 :line-end 5 :col-beg 15 :col-end 23 :name \"foo\" :file \"tmp/src/example/one.clj\" :match \"(str \\\"bar\\\" (two/foo) \\\"goo\\\"))\"}")
                     '(dict "count" 2)))))

(Given "^I call find usages for \"\\(.*\\)\" and the middleware gives me three matches"
       (lambda (symbol-name)
         (cljr--setup-find-symbol-buffer symbol-name)
         (-map 'cljr--format-and-insert-symbol-occurrence
               (list '(dict "occurrence" "{:line-beg 3 :line-end 4 :col-beg 7 :col-end 9 :name \"foo\" :file \"tmp/src/example/two.clj\" :match \"(defn foo []\"}")
                     '(dict "occurrence" "{:line-beg 3 :line-end 4 :col-beg 7 :col-end 9 :name \"foo\" :file \"tmp/src/example/two.clj\" :match \"(defn foo []\"}")
                     '(dict "occurrence" "{:line-beg 5 :line-end 5 :col-beg 15 :col-end 23 :name \"foo\" :file \"tmp/src/example/one.clj\" :match \"(str \\\"bar\\\" (two/foo) \\\"goo\\\"))\"}")
                     '(dict "count" 3)))))

(And "^I pop to find usages buffer$"
     (lambda ()
       (pop-to-buffer cljr--find-symbol-buffer)))

(defvar cljr--ns-path-return-value nil)
(defun cljr--ns-path (&rest _)
  cljr--ns-path-return-value)

(And "^cljr--ns-path returns \"\\([^\"]+\\)\"$"
     (lambda (path)
       (setq cljr--ns-path-return-value path)))

(And "^cljr--clean-ns sorts stuff$"
     (lambda ()
       ;; This might look slightly convoluted, but if we just return
       ;; the correct ns form we're not testing the first half of the
       ;; test
       (cljr--goto-ns)
       (replace-regexp "\\[clojure.string :as s\\]\n\\(\\s-+\\)\\[clj-time.core :refer :all\\]"
                       "[clj-time.core :refer :all]
\\1[clojure.string :as s]"
                       nil (point) (cljr--point-after 'paredit-forward))))

(And "^I disable cljr-clean-ns$"
     (lambda ()
       (defun cljr-clean-ns ()(interactive))))
