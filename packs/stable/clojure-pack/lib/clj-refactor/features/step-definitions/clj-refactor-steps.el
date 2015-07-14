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

(Given "^I switch auto-sort off$"
       (lambda ()
         (setq cljr-auto-sort-ns nil)))

(Given "^I switch auto-sort on$"
       (lambda ()
         (setq cljr-auto-sort-ns t)))

(Given "^I set sort comparator to string length$"
       (lambda ()
         (setq cljr-sort-comparator 'cljr--string-length-comparator)))

(Given "^I set sort comparator to semantic$"
       (lambda ()
         (setq cljr-sort-comparator 'cljr--semantic-comparator)))

(Given "^I set sort comparator to string natural$"
       (lambda ()
         (setq cljr-sort-comparator 'cljr--string-natural-comparator)))

(Given "^I exit multiple-cursors-mode"
       (lambda ()
         (multiple-cursors-mode 0)))

(Given "^I don't use multiple-cursors"
       (lambda ()
         (setq cljr-use-multiple-cursors nil)))

(Given "^I call the rename callback directly with mock data for foo->baz"
       (lambda ()
         (cljr--rename-occurrences "example.two"
                                   '((:line-beg 3 :line-end 4 :col-beg 7 :col-end 9
                                                :name "foo"
                                                :file "tmp/src/example/two.clj"
                                                :match "")
                                     (:line-beg 5 :line-end 5 :col-beg 15
                                                :col-end 23 :name "foo"
                                                :file "tmp/src/example/one.clj"
                                                :match ""))
                                   "baz")))

(Given "^I call the rename callback directly with mock data for star->asterisk"
       (lambda ()
         (cljr--rename-occurrences "example.two"
                                   '((:line-beg 6 :line-end 7 :col-beg 7
                                                :col-end 10 :name "star*"
                                                :file "tmp/src/example/two.clj"
                                                :match "")
                                     (:line-beg 8 :line-end 8 :col-beg 17
                                                :col-end 27 :name "star*"
                                                :file "tmp/src/example/one.clj"
                                                :match ""))
                                   "asterisk*")))

(Given "^I call the add-missing-libspec callback directly with mock data to import"
       (lambda ()
         (cljr--add-missing-libspec "Date" '((java.util.Date :class)))))

(Given "^I call the add-missing-libspec callback directly with mock data to refer split"
       (lambda ()
         (cljr--add-missing-libspec "split" '((clojure.string  :ns)))))

(Given "^I call the add-missing-libspec callback directly with mock data to alias clojure.string"
       (lambda ()
         (cljr--add-missing-libspec "str/split" '((clojure.string :ns)))))

(Given "^I call the add-missing-libspec callback directly with mock data to require WebrequestHandler"
       (lambda ()
         (cljr--add-missing-libspec "WebrequestHandler" '((modular.ring.WebrequestHandler :type)))))

(Then "^the file should be named \"\\([^\"]+\\)\"$"
      (lambda (file-name-postfix)
        (assert (s-ends-with? file-name-postfix (buffer-file-name)) nil "Expected %S to end with %S" (buffer-file-name) file-name-postfix)))

(And "^the cursor is inside the first defn form$"
     (lambda ()
       (goto-char (point-min))
       (re-search-forward "defn")))

(Given "^I call the add-stubs function directly with mock data from the middleware"
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
