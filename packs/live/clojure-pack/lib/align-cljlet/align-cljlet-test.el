(require 'align-cljlet)

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
    (should (equal expected
                   (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest align-let ()
  (acl-should-align "
      (let [apple 1
            orange 2])"
                    "
\(let [apple  1
      orange 2])")
  )

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
