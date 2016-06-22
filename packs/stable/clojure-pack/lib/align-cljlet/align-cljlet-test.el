(require 'align-cljlet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unit Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun acl-forward-sexp-should-move-to (initial-buffer expected-buffer)
  (with-temp-buffer
    (clojure-mode)
    (insert initial-buffer)
    (goto-char (point-min))
    (search-forward "|" nil nil 1)
    (delete-backward-char 1)
    (acl-forward-sexp)
    (insert "|")
    (should (equal (buffer-substring-no-properties 1 (point-max))
                   expected-buffer))))

(ert-deftest acl-foward-sexp-test ()
             (acl-forward-sexp-should-move-to "|a b"
                                              "a| b")
             (acl-forward-sexp-should-move-to "a| b"
                                              "a b|")
             (acl-forward-sexp-should-move-to "a b|"
                                              "a b|")
             (acl-forward-sexp-should-move-to "|#foo/bar [1 2 3] [1 2 3]"
                                              "#foo/bar [1 2 3]| [1 2 3]")
             (acl-forward-sexp-should-move-to "|^int 123 999"
                                              "^int 123| 999")
             (acl-forward-sexp-should-move-to "|#_(ignore me) 123 999"
                                              "#_(ignore me) 123| 999")
             (acl-forward-sexp-should-move-to "|123 #_(ignore me) 999"
                                              "123| #_(ignore me) 999")
             (acl-forward-sexp-should-move-to "123| #_(ignore me) 999"
                                              "123 #_(ignore me) 999|")
             )

(defun acl-goto-next-pair-should-move-to (initial-buffer expected-buffer)
  (with-temp-buffer
    (clojure-mode)
    (insert initial-buffer)
    (goto-char (point-min))
    (search-forward "|" nil nil 1)
    (delete-backward-char 1)
    (acl-goto-next-pair)
    (let ((expected-position (1+ (string-match "|" expected-buffer))))
      (should (equal (point)
                     expected-position)))))

(ert-deftest acl-goto-next-pair-test ()
  (acl-goto-next-pair-should-move-to "|a b c d" "a b |c d")
  (acl-goto-next-pair-should-move-to "a b |c d e f" "a b c d |e f")
  (acl-goto-next-pair-should-move-to "a b c d |e f" "a b c d e |f") ;; slightly odd behaviour here
  (acl-goto-next-pair-should-move-to "|(1 2 3) b c d" "(1 2 3) b |c d")
  (acl-goto-next-pair-should-move-to "|a #_(1 2 3) b c d" "a #_(1 2 3) b |c d")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Alignment tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun acl-should-error (buffer)
  (with-temp-buffer
    (clojure-mode)
    (insert buffer)
    (goto-char (point-min))
    (down-list)

    (should-error (align-cljlet))))

(defun acl-should-align (buffer expected)
  (with-temp-buffer
    (clojure-mode)
    (insert buffer)
    (goto-char (point-min))
    (down-list)
    (align-cljlet)
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   expected))))

(ert-deftest align-let ()
  (acl-should-align "
      (let [apple 1
            orange 2])"
                    "
\(let [apple  1
      orange 2])")
  )

(ert-deftest align-let-with-tagged-type ()
  (acl-should-align "
      (let [^Long apple 1
            orange 2])"
                    "
(let [^Long apple 1
      orange      2])"))

(ert-deftest align-hash ()
  (acl-should-align
   "
      {:foo 234
       :foobar (list 1 2 3)
}"
   "
{:foo    234
 :foobar (list 1 2 3)
}")

  (acl-should-error
   " {:a 1 :b 2}"))

(ert-deftest align-hash-with-reader-macro ()
  (acl-should-align
   "
      {#foo/bar [1 2 3] 234
       :foobar (list 1 2 3)
}"
   "
{#foo/bar [1 2 3] 234
 :foobar          (list 1 2 3)
}"))

(ert-deftest align-hash-with-commented-form ()
  (acl-should-align
   "
(let [variable-a     #_(slow-database-call) 1
      some-other-variable 2] body)
"
   "
(let [variable-a          #_(slow-database-call) 1
      some-other-variable 2] body)
"))

(ert-deftest align-hash-with-commented-form-at-front ()
  (acl-should-align
   "
(let [#_(slow-database-call) variable-a 1
      some-other-variable  2] body)
"
   "
(let [#_(slow-database-call) variable-a 1
      some-other-variable               2] body)
"))

(ert-deftest align-hash-with-all-sorts-of-commented-forms ()
  (acl-should-align
   "
(let [#_(slow-database-call) variable-a #_(1 2 3) 1
        some-other-variable 2] body)"
   "
(let [#_(slow-database-call) variable-a #_(1 2 3) 1
      some-other-variable               2] body)"))

(ert-deftest align-cond ()
             (acl-should-align
              "(cond
   (> grade 90) \"A\"
   (> grade 80)    \"B\"
   (> grade 70)    \"C\"
   (> grade 60) \"D\"
   :else        \"F\")"
              "(cond
  (> grade 90) \"A\"
  (> grade 80) \"B\"
  (> grade 70) \"C\"
  (> grade 60) \"D\"
  :else        \"F\")")

             ;; TODO: Currently does not handle :>> forms
             )

(ert-deftest align-for ()
  (acl-should-align
   "
(for [apple [1 2]
            orange [3 4]]
     [apple orange])"
   "
(for [apple  [1 2]
      orange [3 4]]
     [apple orange])"))

(ert-deftest align-condp ()
  (acl-should-align
   "(condp = value
  1     \"one\"
  2 \"two\"
  3   \"three\"
  :else (str \"unexpected value\"))"
   "(condp = value
  1     \"one\"
  2     \"two\"
  3     \"three\"
  :else (str \"unexpected value\"))"))

(ert-deftest align-cond ()
  (acl-should-align
   "(cond
   (> grade 90) \"A\"
   (> grade 80)    \"B\"
   (> grade 70)    \"C\"
   (> grade 60) \"D\"
   :else        \"F\")"
   "(cond
  (> grade 90) \"A\"
  (> grade 80) \"B\"
  (> grade 70) \"C\"
  (> grade 60) \"D\"
  :else        \"F\")"))

(ert-deftest calc-route-widths ()
  (with-temp-buffer
    (clojure-mode)
    (insert "(defroutes test (GET \"/\" [] (render-home-page))\n(DELETE \"/delete/:id\" [id] (handle-delete-library id)))")
    (goto-char (point-min))
    (down-list)
    (should (equal (list 6 13 4)
                   (acl-calc-route-widths)))))

(ert-deftest respace-defroute-subform ()
  (with-temp-buffer
    (clojure-mode)
    (insert "(GET \"/\" [] (render-home-page))")
    (goto-char (point-min))
    (down-list)
    (acl-respace-subform (list 8 4 6))
    (should (equal "(GET      \"/\"  []     (render-home-page))"
                   (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest respace-defroute-form ()
  (with-temp-buffer
    (clojure-mode)
    (insert "(defroutes test\n(GET \"/\" [] (render-home-page))\n(DELETE \"/delete/:id\" [id] (handle-delete-library id)))")
    (goto-char (point-min))
    (down-list)
    (acl-respace-defroute-form (list 8 12 6))
    (should (equal "(defroutes test\n  (GET      \"/\"          []     (render-home-page))\n  (DELETE   \"/delete/:id\"[id]   (handle-delete-library id)))"
                   (buffer-substring-no-properties (point-min) (point-max)))))
  )
