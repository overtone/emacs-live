(ns clojure-mode.test
  (:use [clojure.test]))

(deftest test-str
  (is (= "o hai" (str "o" "hai"))))

(deftest test-errs
  (is (({} :hi)))
  (is (str "This one doesn't actually error."))
  (is (= 0 (/ 9 0))))

(deftest test-bad-math
  (is (= 0 (* 8 2)))
  (is (= 5 (+ 2 2))))

(deftest test-something-that-actually-works
  (is (= 1 1)))

;; For debugging
;; (map #(cons (str (:name (meta %))) (:status (meta %))) (vals (ns-interns *ns*)))
;; (insert (pp the-result))

(comment ;; for indentation
  (with-hi heya
    somebuddy)

  (deftoggle cap
    gabba)

  (couch/with-db hi
    your-db)

  (clo/defguppy gurgle
    minnow))

(defn say-hello
  "This is a long doc string to test clojure-fill-docstring. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Phasellus sed nunc luctus leo ultricies semper. Nullam id tempor mi. Cras adipiscing scelerisque purus, at semper magna tincidunt ut. Sed eget dolor vitae enim feugiat porttitor. Etiam vulputate pulvinar lacinia. Nam vitae nisl sit amet libero pulvinar pretium nec a dui. Ut luctus elit eu nulla posuere nec feugiat ipsum vehicula. Quisque eu pulvinar neque. Fusce fermentum adipiscing mauris, sit amet accumsan ante dignissim ac. Pellentesque molestie mollis condimentum.

Etiam commodo nulla id risus convallis pharetra. Integer dapibus, eros vitae vehicula rhoncus, nisl lorem ornare magna, eu vehicula justo nunc ac nunc. In dolor sem, vulputate eget vulputate id, euismod eu ligula. Nullam elit augue, ultrices ut pretium vel, bibendum sit amet est. Curabitur vulputate arcu vitae neque adipiscing vel commodo ante faucibus. Cras tempor placerat erat. Sed ultrices faucibus sodales. Vestibulum sollicitudin consectetur mauris, nec mollis quam accumsan ultrices. Vestibulum tincidunt libero a lectus condimentum et fermentum diam eleifend. Nam accumsan interdum neque nec aliquet. Praesent feugiat dui at est rhoncus lacinia."
  []
  (println "Hello, World!"))
