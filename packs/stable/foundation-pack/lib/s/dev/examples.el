;; -*- lexical-binding: t; eval: (font-lock-add-keywords nil '(("defexamples\\|def-example-group\\| => " (0 'font-lock-keyword-face)))); -*-

;; Only the first three examples per function are shown in the docs,
;; so make those good.

(require 's)
(require 'assoc)

(def-example-group "Tweak whitespace"
  (defexamples s-trim
    (s-trim "trim ") => "trim"
    (s-trim " this") => "this"
    (s-trim " only  trims beg and end  ") => "only  trims beg and end")

  (defexamples s-trim-left
    (s-trim-left "trim ") => "trim "
    (s-trim-left " this") => "this")

  (defexamples s-trim-right
    (s-trim-right "trim ") => "trim"
    (s-trim-right " this") => " this")

  (defexamples s-chomp
    (s-chomp "no newlines\n") => "no newlines"
    (s-chomp "no newlines\r\n") => "no newlines"
    (s-chomp "some newlines\n\n") => "some newlines\n")

  (defexamples s-collapse-whitespace
    (s-collapse-whitespace "only   one space   please") => "only one space please"
    (s-collapse-whitespace "collapse \n all \t sorts of \r whitespace") => "collapse all sorts of whitespace")

  (defexamples s-word-wrap
    (s-word-wrap 10 "This is too long") => "This is\ntoo long"
    (s-word-wrap 10 "This is way way too long") => "This is\nway way\ntoo long"
    (s-word-wrap 10 "It-wraps-words-but-does-not-break-them") => "It-wraps-words-but-does-not-break-them")

  (defexamples s-center
    (s-center 5 "a") => "  a  "
    (s-center 5 "ab") => "  ab "
    (s-center 1 "abc") => "abc"
    (s-center 6 "ab") => "  ab  ")

  (defexamples s-pad-left
    (s-pad-left 3 "0" "3") => "003"
    (s-pad-left 3 "0" "23") => "023"
    (s-pad-left 3 "0" "1234") => "1234")

  (defexamples s-pad-right
    (s-pad-right 3 "." "3") => "3.."
    (s-pad-right 3 "." "23") => "23."
    (s-pad-right 3 "." "1234") => "1234"))

(def-example-group "To shorter string"
  (defexamples s-truncate
    (s-truncate 6 "This is too long") => "Thi..."
    (s-truncate 16 "This is also too long") => "This is also ..."
    (s-truncate 16 "But this is not!") => "But this is not!")

  (defexamples s-left
    (s-left 3 "lib/file.js") => "lib"
    (s-left 3 "li") => "li")

  (defexamples s-right
    (s-right 3 "lib/file.js") => ".js"
    (s-right 3 "li") => "li")

  (defexamples s-chop-suffix
    (s-chop-suffix "-test.js" "penguin-test.js") => "penguin"
    (s-chop-suffix "\n" "no newlines\n") => "no newlines"
    (s-chop-suffix "\n" "some newlines\n\n") => "some newlines\n")

  (defexamples s-chop-suffixes
    (s-chop-suffixes '("_test.js" "-test.js" "Test.js") "penguin-test.js") => "penguin"
    (s-chop-suffixes '("\r" "\n") "penguin\r\n") => "penguin\r"
    (s-chop-suffixes '("\n" "\r") "penguin\r\n") => "penguin")

  (defexamples s-chop-prefix
    (s-chop-prefix "/tmp" "/tmp/file.js") => "/file.js"
    (s-chop-prefix "/tmp" "/tmp/tmp/file.js") => "/tmp/file.js")

  (defexamples s-chop-prefixes
    (s-chop-prefixes '("/tmp" "/my") "/tmp/my/file.js") => "/file.js"
    (s-chop-prefixes '("/my" "/tmp") "/tmp/my/file.js") => "/my/file.js")

  (defexamples s-shared-start
    (s-shared-start "bar" "baz") => "ba"
    (s-shared-start "foobar" "foo") => "foo"
    (s-shared-start "bar" "foo") => ""
    (s-shared-start "" "foo") => ""
    (s-shared-start "foo" "foo") => "foo"
    (s-shared-start "" "") => "")

  (defexamples s-shared-end
    (s-shared-end "bar" "var") => "ar"
    (s-shared-end "foo" "foo") => "foo"
    (s-shared-end "bar" "foo") => ""
    (s-shared-end "" "foo") => ""
    (s-shared-end "" "") => ""))

(def-example-group "To longer string"
  (defexamples s-repeat
    (s-repeat 10 " ") => "          "
    (s-concat (s-repeat 8 "Na") " Batman!") => "NaNaNaNaNaNaNaNa Batman!")

  (defexamples s-concat
    (s-concat "abc" "def" "ghi") => "abcdefghi")

  (defexamples s-prepend
    (s-prepend "abc" "def") => "abcdef")

  (defexamples s-append
    (s-append "abc" "def") => "defabc"))

(def-example-group "To and from lists"
  (defexamples s-lines
    (s-lines "abc\ndef\nghi") => '("abc" "def" "ghi")
    (s-lines "abc\rdef\rghi") => '("abc" "def" "ghi")
    (s-lines "abc\r\ndef\r\nghi") => '("abc" "def" "ghi"))

  (defexamples s-match
    (s-match "^def" "abcdefg") => nil
    (s-match "^abc" "abcdefg") => '("abc")
    (s-match "^/.*/\\([a-z]+\\)\\.\\([a-z]+\\)" "/some/weird/file.html") => '("/some/weird/file.html" "file" "html")
    (s-match "^/.*/\\([a-z]+\\)\\.\\([a-z]+\\)" "/some/weird/file.org") => '("/some/weird/file.org" "file" "org")
    (s-match "^\\(abc\\)\\(def\\)?" "abcdef") => '("abcdef" "abc" "def")
    (s-match "^\\(abc\\)\\(def\\)?" "abc") => '("abc" "abc")
    (s-match "^\\(abc\\)\\(def\\)?\\(ghi\\)" "abcghi") => '("abcghi" "abc" nil "ghi")
    (s-match "abc" "abcdef" 1) => nil
    (s-match "abc" "abcdefabc" 2) => '("abc"))

  (defexamples s-match-strings-all
    (s-match-strings-all
     "{\\([^}]+\\)}" "x is {x} and y is {y}") => '(("{x}" "x") ("{y}" "y"))
    (s-match-strings-all "ab." "abXabY") => '(("abX") ("abY"))
    (s-match-strings-all "\\<" "foo bar baz") => '(("") ("") ("")))

  (defexamples s-slice-at
    (s-slice-at "-" "abc") => '("abc")
    (s-slice-at "-" "abc-def") => '("abc" "-def")
    (s-slice-at "[\.#]" "abc.def.ghi#id") => '("abc" ".def" ".ghi" "#id")
    (s-slice-at "-" "abc-def-") => '("abc" "-def" "-"))

  (defexamples s-split
    (s-split "|" "a|bc|12|3") => '("a" "bc" "12" "3")
    (s-split ":" "a,c,d") => '("a,c,d")
    (s-split "\n" "z\nefg\n") => '("z" "efg" "")
    (s-split "\n" "z\nefg\n" t) => '("z" "efg")
    (s-split "ö" "xyöözeföklmö") => '("xy" "" "zef" "klm" "")
    (s-split "ö" "xyöözeföklmö" t) => '("xy" "zef" "klm"))

  (defexamples s-join
    (s-join "+" '("abc" "def" "ghi")) => "abc+def+ghi"
    (s-join "\n" '("abc" "def" "ghi")) => "abc\ndef\nghi"))

(def-example-group "Predicates"
  (defexamples s-equals?
    (s-equals? "abc" "ABC") => nil
    (s-equals? "abc" "abc") => t)

  (defexamples s-less?
    (s-less? "abc" "abd") => t
    (s-less? "abd" "abc") => nil
    (s-less? "abc" "abc") => nil)

  (defexamples s-matches?
    (s-matches? "^[0-9]+$" "123") => t
    (s-matches? "^[0-9]+$" "a123") => nil
    (s-matches? "1" "1a" 1) => nil
    (s-matches? "1" "1a1" 1) => t)

  (defexamples s-blank?
    (s-blank? "") => t
    (s-blank? nil) => t
    (s-blank? " ") => nil)

  (defexamples s-present?
    (s-present? "") => nil
    (s-present? nil) => nil
    (s-present? " ") => t)

  (defexamples s-ends-with?
    (s-ends-with? ".md" "readme.md") => t
    (s-ends-with? ".MD" "readme.md") => nil
    (s-ends-with? ".MD" "readme.md" t) => t
    (s-ends-with? ".md" "md") => nil
    (s-suffix? ".md" "readme.md") => t)

  (defexamples s-starts-with?
    (s-starts-with? "lib/" "lib/file.js") => t
    (s-starts-with? "LIB/" "lib/file.js") => nil
    (s-starts-with? "LIB/" "lib/file.js" t) => t
    (s-starts-with? "lib/" "lib") => nil
    (s-prefix? "lib/" "lib/file.js") => t)

  (defexamples s-contains?
    (s-contains? "file" "lib/file.js") => t
    (s-contains? "nope" "lib/file.js") => nil
    (s-contains? "^a" "it's not ^a regexp") => t
    (s-contains? "FILE" "lib/file.js") => nil
    (s-contains? "FILE" "lib/file.js" t) => t)

  (defexamples s-lowercase?
    (s-lowercase? "file") => t
    (s-lowercase? "File") => nil
    (s-lowercase? "filä") => t
    (s-lowercase? "filÄ") => nil
    (s-lowercase? "123?") => t)

  (defexamples s-uppercase?
    (s-uppercase? "HULK SMASH") => t
    (s-uppercase? "Bruce no smash") => nil
    (s-uppercase? "FöB") => nil
    (s-uppercase? "FÖB") => t
    (s-uppercase? "123?") => t)

  (defexamples s-mixedcase?
    (s-mixedcase? "HULK SMASH") => nil
    (s-mixedcase? "Bruce no smash") => t
    (s-mixedcase? "BRÜCE") => nil
    (s-mixedcase? "BRüCE") => t
    (s-mixedcase? "123?") => nil)

  (defexamples s-capitalized?
    (s-capitalized? "Capitalized") => t
    (s-capitalized? "I am capitalized") => t
    (s-capitalized? "I Am Titleized") => nil
    (s-capitalized? "lower") => nil
    (s-capitalized? "UPPER") => nil
    (s-capitalized? "Привет") => t)

  (defexamples s-numeric?
    (s-numeric? "123") => t
    (s-numeric? "onetwothree") => nil
    (s-numeric? "7a") => nil
    (s-numeric? "a89") => nil))

(def-example-group "The misc bucket"
  (defexamples s-replace
    (s-replace "file" "nope" "lib/file.js") => "lib/nope.js"
    (s-replace "^a" "\\1" "it's not ^a regexp") => "it's not \\1 regexp")

  (defexamples s-replace-all
    (s-replace-all '(("lib" . "test") ("file" . "file_test")) "lib/file.js") => "test/file_test.js"
    (s-replace-all '(("lib" . "test") ("test" . "lib")) "lib/test.js") => "test/lib.js")

  (defexamples s-downcase
    (s-downcase "ABC") => "abc")

  (defexamples s-upcase
    (s-upcase "abc") => "ABC")

  (defexamples s-capitalize
    (s-capitalize "abc DEF") => "Abc def"
    (s-capitalize "abc.DEF") => "Abc.def")

  (defexamples s-titleize
    (s-titleize "abc DEF") => "Abc Def"
    (s-titleize "abc.DEF") => "Abc.Def")

  (defexamples s-with
    (s-with "   hulk smash   " s-trim s-upcase) => "HULK SMASH"
    (s-with "My car is a Toyota" (s-replace "car" "name") (s-replace "a Toyota" "Bond") (s-append ", James Bond")) => "My name is Bond, James Bond"
    (s-with "abc \ndef  \nghi" s-lines (mapcar 's-trim) (s-join "-") s-reverse) => "ihg-fed-cba")

  (defexamples s-index-of
    (s-index-of "abc" "abcdef") => 0
    (s-index-of "CDE" "abcdef" t) => 2
    (s-index-of "n.t" "not a regexp") => nil)

  (defexamples s-reverse
    (s-reverse "abc") => "cba"
    (s-reverse "ab xyz") => "zyx ba"
    (s-reverse "") => "")

  (defexamples s-presence
    (s-presence nil) => nil
    (s-presence "") => nil
    (s-presence "foo") => "foo")

  (defexamples s-format
     ;; One with an alist works
    (s-format
     "help ${name}! I'm ${malady}"
     'aget
     '(("name" . "nic") ("malady" . "on fire")))
    => "help nic! I'm on fire"

    ;; One with a function works
    (s-format "hello ${name}, nice day" (lambda (var-name) "nic"))
    => "hello nic, nice day"

    ;; One with a list works
    (s-format "hello $0, nice $1" 'elt '("nic" "day"))
    => "hello nic, nice day"

    ;; Two with a hash-table works
    (s-format
     "help ${name}! I'm ${malady}"
     'gethash
     #s(hash-table test equal data ("name" "nic" "malady" "on fire")))
    => "help nic! I'm on fire"

    ;; Replacing case has no effect on s-format
    (let ((case-replace t))
      (s-format "help ${NAME}!" 'aget '(("NAME" . "Nick"))))
    => "help Nick!"

    (let ((case-replace nil))
      (s-format "help ${NAME}!" 'aget '(("NAME" . "Nick"))))
    => "help Nick!"

    (let ((case-replace nil))
      (s-format "help ${name}!" 'aget '(("name" . "Nick"))))
    => "help Nick!"

    ;; What happens when we have literal slashes?
    (s-format "$0" 'elt '("Hello\\nWorld"))
    => "Hello\\nWorld"

    ;; What happens when we don't have the elements? with hash...
    (condition-case err
        (s-format
         "help ${name}! I'm ${malady}"
         'gethash
         #s(hash-table test equal data ("name" "nic" )))
      (s-format-resolve (car err)))
    => 's-format-resolve)

  (defexamples s-lex-format
      ;; lexical stuff
      (let ((x 1))
        (s-lex-format "x is ${x}"))
    => "x is 1"

    (let ((str1 "this")
          (str2 "that"))
      (s-lex-format "${str1} and ${str2}"))
    => "this and that"

    ;; Have a litteral \ in the replacement
    (let ((foo "Hello\\nWorld"))
      (s-lex-format "${foo}"))
    => "Hello\\nWorld"
    )

  (defexamples s-count-matches
    (s-count-matches "a" "aba") => 2
    (s-count-matches "a" "aba" 0 2) => 1
    (s-count-matches "\\w\\{2\\}[0-9]+" "ab1bab2frobinator") => 2))

(def-example-group "Pertaining to words"
  (defexamples s-split-words
    (s-split-words "under_score") => '("under" "score")
    (s-split-words "some-dashed-words") => '("some" "dashed" "words")
    (s-split-words "evenCamelCase") => '("even" "Camel" "Case")
    (s-split-words "!map (fn list)") => '("map" "fn" "list")
    (s-split-words "Привет, мир") => '("Привет" "мир")
    (s-split-words "e é è e") => '("e" "é" "è" "e")
    (s-split-words "MANYUpperCases") => '("MANY" "Upper" "Cases")
    (s-split-words "Приве́т") => '("Приве́т")
    (s-split-words "漢語") => '("漢語"))

  (defexamples s-lower-camel-case
    (s-lower-camel-case "some words") => "someWords"
    (s-lower-camel-case "dashed-words") => "dashedWords"
    (s-lower-camel-case "under_scored_words") => "underScoredWords")

  (defexamples s-upper-camel-case
    (s-upper-camel-case "some words") => "SomeWords"
    (s-upper-camel-case "dashed-words") => "DashedWords"
    (s-upper-camel-case "under_scored_words") => "UnderScoredWords")

  (defexamples s-snake-case
    (s-snake-case "some words") => "some_words"
    (s-snake-case "dashed-words") => "dashed_words"
    (s-snake-case "camelCasedWords") => "camel_cased_words")

  (defexamples s-dashed-words
    (s-dashed-words "some words") => "some-words"
    (s-dashed-words "under_scored_words") => "under-scored-words"
    (s-dashed-words "camelCasedWords") => "camel-cased-words")

  (defexamples s-capitalized-words
    (s-capitalized-words "some words") => "Some words"
    (s-capitalized-words "under_scored_words") => "Under scored words"
    (s-capitalized-words "camelCasedWords") => "Camel cased words")

  (defexamples s-titleized-words
    (s-titleized-words "some words") => "Some Words"
    (s-titleized-words "under_scored_words") => "Under Scored Words"
    (s-titleized-words "camelCasedWords") => "Camel Cased Words")

  (defexamples s-word-initials
    (s-word-initials "some words") => "sw"
    (s-word-initials "under_scored_words") => "usw"
    (s-word-initials "camelCasedWords") => "cCW"
    (s-word-initials "dashed-words") => "dw"))

