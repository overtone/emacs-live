;;; font locking
(ns clojure-mode.demo
  (:require [clojure.something]
            [something.s]))

(comment ;; for indentation
  (with-hi heya
    somebuddy)

  (deftoggle cap
    gabba)

  (couch/with-db hi
    your-db)

  (clo/defguppy gurgle
    minnow))

;; character literals
[\a \newline \u0032 \/ \+ \,, \;]

;; namespaced/static calls/references
(core.foo-baz/bar)
@foo-bar/bar
(FooBar/bar)
(some.package.FooBar/baz)

;; cljx
(defn x-to-string
  [x]
  (let [buf #+clj (StringBuilder.) #+cljs (gstring/StringBuffer.)]
    (.append buf "x is: ")
    (.append buf (str x))))

;; metadata doesn't break docstrings
(defn max
  "Returns the greatest of the nums."
  {:added "1.0"
   :inline-arities >1?
   :inline (nary-inline 'max)}
  ([x] x)
  ([x y] (. clojure.lang.Numbers (max x y)))
  ([x y & more]
     (reduce1 max (max x y) more)))

(defn ^String reverse
  "Returns s with its characters reversed."
  {:added "1.2"}
  [^CharSequence s]
  (.toString (.reverse (StringBuilder. s))))

;; useful for testing docstring filling
(defn say-hello
  "This is a long doc string to test clojure-fill-docstring. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Phasellus sed nunc luctus leo ultricies semper. Nullam id tempor mi. Cras adipiscing scelerisque purus, at semper magna tincidunt ut. Sed eget dolor vitae enim feugiat porttitor. Etiam vulputate pulvinar lacinia. Nam vitae nisl sit amet libero pulvinar pretium nec a dui. Ut luctus elit eu nulla posuere nec feugiat ipsum vehicula. Quisque eu pulvinar neque. Fusce fermentum adipiscing mauris, sit amet accumsan ante dignissim ac. Pellentesque molestie mollis condimentum.

Etiam commodo nulla id risus convallis pharetra. Integer dapibus, eros vitae vehicula rhoncus, nisl lorem ornare magna, eu vehicula justo nunc ac nunc. In dolor sem, vulputate eget vulputate id, euismod eu ligula. Nullam elit augue, ultrices ut pretium vel, bibendum sit amet est. Curabitur vulputate arcu vitae neque adipiscing vel commodo ante faucibus. Cras tempor placerat erat. Sed ultrices faucibus sodales. Vestibulum sollicitudin consectetur mauris, nec mollis quam accumsan ultrices. Vestibulum tincidunt libero a lectus condimentum et fermentum diam eleifend. Nam accumsan interdum neque nec aliquet. Praesent feugiat dui at est rhoncus lacinia."
  []
  (println "Hello, World!"))
