(require 'ert)
(require 'js2-mode)

(ert-deftest js2-finds-jslint-globals ()
  (with-temp-buffer
    (insert "/*global foo, bar:false, baz: true */")
    (js2-mode)
    (should (equal (js2-get-jslint-globals)
                   '("foo" "bar" "baz")))))

(ert-deftest js2-no-jslint-globals-without-keyword ()
  (with-temp-buffer
    (insert "/* foo, bar:false, baz: true */")
    (js2-mode)
    (should (null (js2-get-jslint-globals)))))

(ert-deftest js2-finds-jslint-globals-in-other-comments ()
  (with-temp-buffer
    (insert "/* foo, bar */\n\n\n/*global quux, tee: true, $*/")
    (js2-mode)
    (should (equal (js2-get-jslint-globals)
                   '("quux" "tee" "$")))))
