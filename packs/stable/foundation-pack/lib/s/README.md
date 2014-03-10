# s.el [![Build Status](https://secure.travis-ci.org/magnars/s.el.png)](http://travis-ci.org/magnars/s.el)

The long lost Emacs string manipulation library.

## Installation

It's available on [marmalade](http://marmalade-repo.org/) and [Melpa](http://melpa.milkbox.net/):

    M-x package-install s

Or you can just dump `s.el` in your load path somewhere.

## Functions


### Tweak whitespace

* [s-trim](#s-trim-s) `(s)`
* [s-trim-left](#s-trim-left-s) `(s)`
* [s-trim-right](#s-trim-right-s) `(s)`
* [s-chomp](#s-chomp-s) `(s)`
* [s-collapse-whitespace](#s-collapse-whitespace-s) `(s)`
* [s-word-wrap](#s-word-wrap-len-s) `(len s)`
* [s-center](#s-center-len-s) `(len s)`
* [s-pad-left](#s-pad-left-len-padding-s) `(len padding s)`
* [s-pad-right](#s-pad-right-len-padding-s) `(len padding s)`

### To shorter string

* [s-truncate](#s-truncate-len-s) `(len s)`
* [s-left](#s-left-len-s) `(len s)`
* [s-right](#s-right-len-s) `(len s)`
* [s-chop-suffix](#s-chop-suffix-suffix-s) `(suffix s)`
* [s-chop-suffixes](#s-chop-suffixes-suffixes-s) `(suffixes s)`
* [s-chop-prefix](#s-chop-prefix-prefix-s) `(prefix s)`
* [s-chop-prefixes](#s-chop-prefixes-prefixes-s) `(prefixes s)`
* [s-shared-start](#s-shared-start-s1-s2) `(s1 s2)`
* [s-shared-end](#s-shared-end-s1-s2) `(s1 s2)`

### To longer string

* [s-repeat](#s-repeat-num-s) `(num s)`
* [s-concat](#s-concat-rest-strings) `(&rest strings)`
* [s-prepend](#s-prepend-prefix-s) `(prefix s)`
* [s-append](#s-append-suffix-s) `(suffix s)`

### To and from lists

* [s-lines](#s-lines-s) `(s)`
* [s-match](#s-match-regexp-s-optional-start) `(regexp s &optional start)`
* [s-match-strings-all](#s-match-strings-all-regex-string) `(regex string)`
* [s-slice-at](#s-slice-at-regexp-s) `(regexp s)`
* [s-split](#s-split-separator-s-optional-omit-nulls) `(separator s &optional omit-nulls)`
* [s-join](#s-join-separator-strings) `(separator strings)`

### Predicates

* [s-equals?](#s-equals-s1-s2) `(s1 s2)`
* [s-less?](#s-less-s1-s2) `(s1 s2)`
* [s-matches?](#s-matches-regexp-s-optional-start) `(regexp s &optional start)`
* [s-blank?](#s-blank-s) `(s)`
* [s-present?](#s-present-s) `(s)`
* [s-ends-with?](#s-ends-with-suffix-s-optional-ignore-case) `(suffix s &optional ignore-case)`
* [s-starts-with?](#s-starts-with-prefix-s-optional-ignore-case) `(prefix s &optional ignore-case)`
* [s-contains?](#s-contains-needle-s-optional-ignore-case) `(needle s &optional ignore-case)`
* [s-lowercase?](#s-lowercase-s) `(s)`
* [s-uppercase?](#s-uppercase-s) `(s)`
* [s-mixedcase?](#s-mixedcase-s) `(s)`
* [s-capitalized?](#s-capitalized-s) `(s)`
* [s-numeric?](#s-numeric-s) `(s)`

### The misc bucket

* [s-replace](#s-replace-old-new-s) `(old new s)`
* [s-replace-all](#s-replace-all-replacements-s) `(replacements s)`
* [s-downcase](#s-downcase-s) `(s)`
* [s-upcase](#s-upcase-s) `(s)`
* [s-capitalize](#s-capitalize-s) `(s)`
* [s-titleize](#s-titleize-s) `(s)`
* [s-with](#s-with-s-form-rest-more) `(s form &rest more)`
* [s-index-of](#s-index-of-needle-s-optional-ignore-case) `(needle s &optional ignore-case)`
* [s-reverse](#s-reverse-s) `(s)`
* [s-presence](#s-presence-s) `(s)`
* [s-format](#s-format-template-replacer-optional-extra) `(template replacer &optional extra)`
* [s-lex-format](#s-lex-format-format-str) `(format-str)`
* [s-count-matches](#s-count-matches-regexp-s-optional-start-end) `(regexp s &optional start end)`

### Pertaining to words

* [s-split-words](#s-split-words-s) `(s)`
* [s-lower-camel-case](#s-lower-camel-case-s) `(s)`
* [s-upper-camel-case](#s-upper-camel-case-s) `(s)`
* [s-snake-case](#s-snake-case-s) `(s)`
* [s-dashed-words](#s-dashed-words-s) `(s)`
* [s-capitalized-words](#s-capitalized-words-s) `(s)`
* [s-titleized-words](#s-titleized-words-s) `(s)`
* [s-word-initials](#s-word-initials-s) `(s)`

## Documentation and examples


### s-trim `(s)`

Remove whitespace at the beginning and end of `s`.

```cl
(s-trim "trim ") ;; => "trim"
(s-trim " this") ;; => "this"
(s-trim " only  trims beg and end  ") ;; => "only  trims beg and end"
```

### s-trim-left `(s)`

Remove whitespace at the beginning of `s`.

```cl
(s-trim-left "trim ") ;; => "trim "
(s-trim-left " this") ;; => "this"
```

### s-trim-right `(s)`

Remove whitespace at the end of `s`.

```cl
(s-trim-right "trim ") ;; => "trim"
(s-trim-right " this") ;; => " this"
```

### s-chomp `(s)`

Remove one trailing `\n`, `\r` or `\r\n` from `s`.

```cl
(s-chomp "no newlines\n") ;; => "no newlines"
(s-chomp "no newlines\r\n") ;; => "no newlines"
(s-chomp "some newlines\n\n") ;; => "some newlines\n"
```

### s-collapse-whitespace `(s)`

Convert all adjacent whitespace characters to a single space.

```cl
(s-collapse-whitespace "only   one space   please") ;; => "only one space please"
(s-collapse-whitespace "collapse \n all \t sorts of \r whitespace") ;; => "collapse all sorts of whitespace"
```

### s-word-wrap `(len s)`

If `s` is longer than `len`, wrap the words with newlines.

```cl
(s-word-wrap 10 "This is too long") ;; => "This is\ntoo long"
(s-word-wrap 10 "This is way way too long") ;; => "This is\nway way\ntoo long"
(s-word-wrap 10 "It-wraps-words-but-does-not-break-them") ;; => "It-wraps-words-but-does-not-break-them"
```

### s-center `(len s)`

If `s` is shorter than `len`, pad it with spaces so it is centered.

```cl
(s-center 5 "a") ;; => "  a  "
(s-center 5 "ab") ;; => "  ab "
(s-center 1 "abc") ;; => "abc"
```

### s-pad-left `(len padding s)`

If `s` is shorter than `len`, pad it with `padding` on the left.

```cl
(s-pad-left 3 "0" "3") ;; => "003"
(s-pad-left 3 "0" "23") ;; => "023"
(s-pad-left 3 "0" "1234") ;; => "1234"
```

### s-pad-right `(len padding s)`

If `s` is shorter than `len`, pad it with `padding` on the right.

```cl
(s-pad-right 3 "." "3") ;; => "3.."
(s-pad-right 3 "." "23") ;; => "23."
(s-pad-right 3 "." "1234") ;; => "1234"
```


### s-truncate `(len s)`

If `s` is longer than `len`, cut it down to `len` - 3 and add ... at the end.

```cl
(s-truncate 6 "This is too long") ;; => "Thi..."
(s-truncate 16 "This is also too long") ;; => "This is also ..."
(s-truncate 16 "But this is not!") ;; => "But this is not!"
```

### s-left `(len s)`

Returns up to the `len` first chars of `s`.

```cl
(s-left 3 "lib/file.js") ;; => "lib"
(s-left 3 "li") ;; => "li"
```

### s-right `(len s)`

Returns up to the `len` last chars of `s`.

```cl
(s-right 3 "lib/file.js") ;; => ".js"
(s-right 3 "li") ;; => "li"
```

### s-chop-suffix `(suffix s)`

Remove `suffix` if it is at end of `s`.

```cl
(s-chop-suffix "-test.js" "penguin-test.js") ;; => "penguin"
(s-chop-suffix "\n" "no newlines\n") ;; => "no newlines"
(s-chop-suffix "\n" "some newlines\n\n") ;; => "some newlines\n"
```

### s-chop-suffixes `(suffixes s)`

Remove `suffixes` one by one in order, if they are at the end of `s`.

```cl
(s-chop-suffixes '("_test.js" "-test.js" "Test.js") "penguin-test.js") ;; => "penguin"
(s-chop-suffixes '("\r" "\n") "penguin\r\n") ;; => "penguin\r"
(s-chop-suffixes '("\n" "\r") "penguin\r\n") ;; => "penguin"
```

### s-chop-prefix `(prefix s)`

Remove `prefix` if it is at the start of `s`.

```cl
(s-chop-prefix "/tmp" "/tmp/file.js") ;; => "/file.js"
(s-chop-prefix "/tmp" "/tmp/tmp/file.js") ;; => "/tmp/file.js"
```

### s-chop-prefixes `(prefixes s)`

Remove `prefixes` one by one in order, if they are at the start of `s`.

```cl
(s-chop-prefixes '("/tmp" "/my") "/tmp/my/file.js") ;; => "/file.js"
(s-chop-prefixes '("/my" "/tmp") "/tmp/my/file.js") ;; => "/my/file.js"
```

### s-shared-start `(s1 s2)`

Returns the longest prefix `s1` and `s2` have in common.

```cl
(s-shared-start "bar" "baz") ;; => "ba"
(s-shared-start "foobar" "foo") ;; => "foo"
(s-shared-start "bar" "foo") ;; => ""
```

### s-shared-end `(s1 s2)`

Returns the longest suffix `s1` and `s2` have in common.

```cl
(s-shared-end "bar" "var") ;; => "ar"
(s-shared-end "foo" "foo") ;; => "foo"
(s-shared-end "bar" "foo") ;; => ""
```


### s-repeat `(num s)`

Make a string of `s` repeated `num` times.

```cl
(s-repeat 10 " ") ;; => "          "
(s-concat (s-repeat 8 "Na") " Batman!") ;; => "NaNaNaNaNaNaNaNa Batman!"
```

### s-concat `(&rest strings)`

Join all the string arguments into one string.

```cl
(s-concat "abc" "def" "ghi") ;; => "abcdefghi"
```

### s-prepend `(prefix s)`

Concatenate `prefix` and `s`.

```cl
(s-prepend "abc" "def") ;; => "abcdef"
```

### s-append `(suffix s)`

Concatenate `s` and `suffix`.

```cl
(s-append "abc" "def") ;; => "defabc"
```


### s-lines `(s)`

Splits `s` into a list of strings on newline characters.

```cl
(s-lines "abc\ndef\nghi") ;; => '("abc" "def" "ghi")
(s-lines "abc\rdef\rghi") ;; => '("abc" "def" "ghi")
(s-lines "abc\r\ndef\r\nghi") ;; => '("abc" "def" "ghi")
```

### s-match `(regexp s &optional start)`

When the given expression matches the string, this function returns a list
of the whole matching string and a string for each matched subexpressions.
If it did not match the returned value is an empty list (nil).

When `start` is non-nil the search will start at that index.

```cl
(s-match "^def" "abcdefg") ;; => nil
(s-match "^abc" "abcdefg") ;; => '("abc")
(s-match "^/.*/\\([a-z]+\\)\\.\\([a-z]+\\)" "/some/weird/file.html") ;; => '("/some/weird/file.html" "file" "html")
```

### s-match-strings-all `(regex string)`

Return a list of matches for `regex` in `string`.

Each element itself is a list of matches, as per
`match-string`. Multiple matches at the same position will be
ignored after the first.

```cl
(s-match-strings-all "{\\([^}]+\\)}" "x is {x} and y is {y}") ;; => '(("{x}" "x") ("{y}" "y"))
(s-match-strings-all "ab." "abXabY") ;; => '(("abX") ("abY"))
(s-match-strings-all "\\<" "foo bar baz") ;; => '(("") ("") (""))
```

### s-slice-at `(regexp s)`

Slices `s` up at every index matching `regexp`.

```cl
(s-slice-at "-" "abc") ;; => '("abc")
(s-slice-at "-" "abc-def") ;; => '("abc" "-def")
(s-slice-at "[.#]" "abc.def.ghi#id") ;; => '("abc" ".def" ".ghi" "#id")
```

### s-split `(separator s &optional omit-nulls)`

Split `s` into substrings bounded by matches for regexp `separator`.
If `omit-nulls` is t, zero-length substrings are omitted.

This is a simple wrapper around the built-in `split-string`.

```cl
(s-split "|" "a|bc|12|3") ;; => '("a" "bc" "12" "3")
(s-split ":" "a,c,d") ;; => '("a,c,d")
(s-split "\n" "z\nefg\n") ;; => '("z" "efg" "")
```

### s-join `(separator strings)`

Join all the strings in `strings` with `separator` in between.

```cl
(s-join "+" '("abc" "def" "ghi")) ;; => "abc+def+ghi"
(s-join "\n" '("abc" "def" "ghi")) ;; => "abc\ndef\nghi"
```


### s-equals? `(s1 s2)`

Is `s1` equal to `s2`?

This is a simple wrapper around the built-in `string-equal`.

```cl
(s-equals? "abc" "ABC") ;; => nil
(s-equals? "abc" "abc") ;; => t
```

### s-less? `(s1 s2)`

Is `s1` less than `s2`?

This is a simple wrapper around the built-in `string-lessp`.

```cl
(s-less? "abc" "abd") ;; => t
(s-less? "abd" "abc") ;; => nil
(s-less? "abc" "abc") ;; => nil
```

### s-matches? `(regexp s &optional start)`

Does `regexp` match `s`?
If `start` is non-nil the search starts at that index.

This is a simple wrapper around the built-in `string-match-p`.

```cl
(s-matches? "^[0-9]+$" "123") ;; => t
(s-matches? "^[0-9]+$" "a123") ;; => nil
(s-matches? "1" "1a" 1) ;; => nil
```

### s-blank? `(s)`

Is `s` nil or the empty string?

```cl
(s-blank? "") ;; => t
(s-blank? nil) ;; => t
(s-blank? " ") ;; => nil
```

### s-present? `(s)`

Is `s` anything but nil or the empty string?

```cl
(s-present? "") ;; => nil
(s-present? nil) ;; => nil
(s-present? " ") ;; => t
```

### s-ends-with? `(suffix s &optional ignore-case)`

Does `s` end with `suffix`?

If `ignore-case` is non-nil, the comparison is done without paying
attention to case differences.

Alias: `s-suffix?`

```cl
(s-ends-with? ".md" "readme.md") ;; => t
(s-ends-with? ".MD" "readme.md") ;; => nil
(s-ends-with? ".MD" "readme.md" t) ;; => t
```

### s-starts-with? `(prefix s &optional ignore-case)`

Does `s` start with `prefix`?

If `ignore-case` is non-nil, the comparison is done without paying
attention to case differences.

Alias: `s-prefix?`. This is a simple wrapper around the built-in
`string-prefix-p`.

```cl
(s-starts-with? "lib/" "lib/file.js") ;; => t
(s-starts-with? "LIB/" "lib/file.js") ;; => nil
(s-starts-with? "LIB/" "lib/file.js" t) ;; => t
```

### s-contains? `(needle s &optional ignore-case)`

Does `s` contain `needle`?

If `ignore-case` is non-nil, the comparison is done without paying
attention to case differences.

```cl
(s-contains? "file" "lib/file.js") ;; => t
(s-contains? "nope" "lib/file.js") ;; => nil
(s-contains? "^a" "it's not ^a regexp") ;; => t
```

### s-lowercase? `(s)`

Are all the letters in `s` in lower case?

```cl
(s-lowercase? "file") ;; => t
(s-lowercase? "File") ;; => nil
(s-lowercase? "filä") ;; => t
```

### s-uppercase? `(s)`

Are all the letters in `s` in upper case?

```cl
(s-uppercase? "HULK SMASH") ;; => t
(s-uppercase? "Bruce no smash") ;; => nil
(s-uppercase? "FöB") ;; => nil
```

### s-mixedcase? `(s)`

Are there both lower case and upper case letters in `s`?

```cl
(s-mixedcase? "HULK SMASH") ;; => nil
(s-mixedcase? "Bruce no smash") ;; => t
(s-mixedcase? "BRÜCE") ;; => nil
```

### s-capitalized? `(s)`

In `s`, is the first letter upper case, and all other letters lower case?

```cl
(s-capitalized? "Capitalized") ;; => t
(s-capitalized? "I am capitalized") ;; => t
(s-capitalized? "I Am Titleized") ;; => nil
```

### s-numeric? `(s)`

Is `s` a number?

```cl
(s-numeric? "123") ;; => t
(s-numeric? "onetwothree") ;; => nil
(s-numeric? "7a") ;; => nil
```


### s-replace `(old new s)`

Replaces `old` with `new` in `s`.

```cl
(s-replace "file" "nope" "lib/file.js") ;; => "lib/nope.js"
(s-replace "^a" "\\1" "it's not ^a regexp") ;; => "it's not \\1 regexp"
```

### s-replace-all `(replacements s)`

`replacements` is a list of cons-cells. Each `car` is replaced with `cdr` in `s`.

```cl
(s-replace-all '(("lib" . "test") ("file" . "file_test")) "lib/file.js") ;; => "test/file_test.js"
(s-replace-all '(("lib" . "test") ("test" . "lib")) "lib/test.js") ;; => "test/lib.js"
```

### s-downcase `(s)`

Convert `s` to lower case.

This is a simple wrapper around the built-in `downcase`.

```cl
(s-downcase "ABC") ;; => "abc"
```

### s-upcase `(s)`

Convert `s` to upper case.

This is a simple wrapper around the built-in `upcase`.

```cl
(s-upcase "abc") ;; => "ABC"
```

### s-capitalize `(s)`

Convert the first word's first character to upper case and the rest to lower case in `s`.

```cl
(s-capitalize "abc DEF") ;; => "Abc def"
(s-capitalize "abc.DEF") ;; => "Abc.def"
```

### s-titleize `(s)`

Convert each word's first character to upper case and the rest to lower case in `s`.

This is a simple wrapper around the built-in `capitalize`.

```cl
(s-titleize "abc DEF") ;; => "Abc Def"
(s-titleize "abc.DEF") ;; => "Abc.Def"
```

### s-with `(s form &rest more)`

Threads `s` through the forms. Inserts `s` as the last item
in the first form, making a list of it if it is not a list
already. If there are more forms, inserts the first form as the
last item in second form, etc.

```cl
(s-with "   hulk smash   " s-trim s-upcase) ;; => "HULK SMASH"
(s-with "My car is a Toyota" (s-replace "car" "name") (s-replace "a Toyota" "Bond") (s-append ", James Bond")) ;; => "My name is Bond, James Bond"
(s-with "abc \ndef  \nghi" s-lines (mapcar 's-trim) (s-join "-") s-reverse) ;; => "ihg-fed-cba"
```

### s-index-of `(needle s &optional ignore-case)`

Returns first index of `needle` in `s`, or nil.

If `ignore-case` is non-nil, the comparison is done without paying
attention to case differences.

```cl
(s-index-of "abc" "abcdef") ;; => 0
(s-index-of "CDE" "abcdef" t) ;; => 2
(s-index-of "n.t" "not a regexp") ;; => nil
```

### s-reverse `(s)`

Return the reverse of `s`.

```cl
(s-reverse "abc") ;; => "cba"
(s-reverse "ab xyz") ;; => "zyx ba"
(s-reverse "") ;; => ""
```

### s-presence `(s)`

Return `s` if it's `s-present?`, otherwise return nil.

```cl
(s-presence nil) ;; => nil
(s-presence "") ;; => nil
(s-presence "foo") ;; => "foo"
```

### s-format `(template replacer &optional extra)`

Format `template` with the function `replacer`.

`replacer` takes an argument of the format variable and optionally
an extra argument which is the `extra` value from the call to
`s-format`.

Several standard `s-format` helper functions are recognized and
adapted for this:

    (s-format "${name}" 'gethash hash-table)
    (s-format "${name}" 'aget alist)
    (s-format "$0" 'elt sequence)

The `replacer` function may be used to do any other kind of
transformation.

```cl
(s-format "help ${name}! I'm ${malady}" 'aget '(("name" . "nic") ("malady" . "on fire"))) ;; => "help nic! I'm on fire"
(s-format "hello ${name}, nice day" (lambda (var-name) "nic")) ;; => "hello nic, nice day"
(s-format "hello $0, nice $1" 'elt '("nic" "day")) ;; => "hello nic, nice day"
```

### s-lex-format `(format-str)`

`s-format` with the current environment.

`format-str` may use the `s-format` variable reference to refer to
any variable:

 (let ((x 1))
   (s-lex-format "x is: ${x}"))

The values of the variables are interpolated with "%s" unless
the variable `s-lex-value-as-lisp` is `t` and then they are
interpolated with "%S".

```cl
(let ((x 1)) (s-lex-format "x is ${x}")) ;; => "x is 1"
(let ((str1 "this") (str2 "that")) (s-lex-format "${str1} and ${str2}")) ;; => "this and that"
(let ((foo "Hello\\nWorld")) (s-lex-format "${foo}")) ;; => "Hello\\nWorld"
```

### s-count-matches `(regexp s &optional start end)`

Count occurrences of `regexp` in `s'.

`start`, inclusive, and `end`, exclusive, delimit the part of `s`
to match. 

```cl
(s-count-matches "a" "aba") ;; => 2
(s-count-matches "a" "aba" 0 2) ;; => 1
(s-count-matches "\\w\\{2\\}[0-9]+" "ab1bab2frobinator") ;; => 2
```


### s-split-words `(s)`

Split `s` into list of words.

```cl
(s-split-words "under_score") ;; => '("under" "score")
(s-split-words "some-dashed-words") ;; => '("some" "dashed" "words")
(s-split-words "evenCamelCase") ;; => '("even" "Camel" "Case")
```

### s-lower-camel-case `(s)`

Convert `s` to lowerCamelCase.

```cl
(s-lower-camel-case "some words") ;; => "someWords"
(s-lower-camel-case "dashed-words") ;; => "dashedWords"
(s-lower-camel-case "under_scored_words") ;; => "underScoredWords"
```

### s-upper-camel-case `(s)`

Convert `s` to UpperCamelCase.

```cl
(s-upper-camel-case "some words") ;; => "SomeWords"
(s-upper-camel-case "dashed-words") ;; => "DashedWords"
(s-upper-camel-case "under_scored_words") ;; => "UnderScoredWords"
```

### s-snake-case `(s)`

Convert `s` to snake_case.

```cl
(s-snake-case "some words") ;; => "some_words"
(s-snake-case "dashed-words") ;; => "dashed_words"
(s-snake-case "camelCasedWords") ;; => "camel_cased_words"
```

### s-dashed-words `(s)`

Convert `s` to dashed-words.

```cl
(s-dashed-words "some words") ;; => "some-words"
(s-dashed-words "under_scored_words") ;; => "under-scored-words"
(s-dashed-words "camelCasedWords") ;; => "camel-cased-words"
```

### s-capitalized-words `(s)`

Convert `s` to Capitalized words.

```cl
(s-capitalized-words "some words") ;; => "Some words"
(s-capitalized-words "under_scored_words") ;; => "Under scored words"
(s-capitalized-words "camelCasedWords") ;; => "Camel cased words"
```

### s-titleized-words `(s)`

Convert `s` to Titleized Words.

```cl
(s-titleized-words "some words") ;; => "Some Words"
(s-titleized-words "under_scored_words") ;; => "Under Scored Words"
(s-titleized-words "camelCasedWords") ;; => "Camel Cased Words"
```

### s-word-initials `(s)`

Convert `s` to its initials.

```cl
(s-word-initials "some words") ;; => "sw"
(s-word-initials "under_scored_words") ;; => "usw"
(s-word-initials "camelCasedWords") ;; => "cCW"
```


## What's with the built-in wrappers?

Imagine looking through the function list and seeing `s-ends-with?`, but
`s-starts-with?` is nowhere to be found. Why? Well, because Emacs already has
`string-prefix-p`. Now you're starting out slightly confused, then have to go
somewhere else to dig for the command you were looking for.

The wrapping functions serve as both documentation for existing functions and
makes for a consistent API.

## Other string related libraries

* [inflections](https://github.com/eschulte/jump.el/blob/master/inflections.el) package
provides functions for strings pluralization and singularization.

* [levenshtein](http://emacswiki.org/emacs/levenshtein.el) package provides a function to
calculate the Levenshtein distance between two strings.

* [string-utils](https://github.com/rolandwalker/string-utils) is another general string manipulation library.

## Changelist

### From 1.7.0 to 1.8.0

- Add `s-present?` and `s-present?` (Johan Andersson)
- Better handling of international characters

### From 1.6.0 to 1.7.0

- Add `s-word-initials` (Sylvain Rousseau)
- Better handling of camel cased strings (@Bruce-Connor)

### From 1.5.0 to 1.6.0

- Add `s-pad-left` and `s-pad-right`
- Bugfixes for `s-format` (Nic Ferrier)

### From 1.4.0 to 1.5.0

- Add `s-all-match-strings` (Geoff Gole)
- Add `s-lex-format` (Nic Ferrier)

### From 1.3.1 to 1.4.0

- Add `s-capitalized?`
- Add `s-replace-all`
- Add `s-slice-at`
- Add `s-split` alias for `split-string` (Rüdiger Sonderfeld)
- Add `s-less?` predicate (Rüdiger Sonderfeld)
- Add START parameter to `s-matches?` (Rüdiger Sonderfeld)
- Bugfixes

### From 1.3.0 to 1.3.1

- Add `s-numeric?`
- Add `s-match` (Arthur Andersen)
- Add `s-format` (Nic Ferrier)
- Move .el files out of root to avoid problems with require.

### From 1.2.1 to 1.3.0

- **Breaking change:** `s-capitalize` now converts the first word's first
  character to upper case and the rest to lower case. `s-titleize`
  works like the old `s-capitalize` and capitalizes each word.
  (Johan Andersson)

- `s-capitalized-words` and `s-titleized-words` mirror this change.

## Contributors

* [Arthur Andersen](https://github.com/leoc) contributed `s-match`
* [Rolando](https://github.com/rolando2424) contributed `s-shared-start` and `s-shared-end`
* [Johan Andersson](https://github.com/rejeep) contributed `s-presence`, `s-present?` and fixed `s-titleize` vs `s-capitalize`
* [Nic Ferrier](https://github.com/nicferrier) added `s-format` and `s-lex-format`
* [Rüdiger Sonderfeld](https://github.com/ruediger) contributed `s-less?`, `s-split` and several bugfixes.
* [Geoff Gole](https://github.com/gsg) contributed `s-all-match-strings`
* [Sylvain Rousseau](https://github.com/thisirs) contributed `s-word-initials`

Thanks!

## Contribute

Yes, please do. Pure functions in the string manipulation realm only,
please. There's a suite of tests in `dev/examples.el`, so remember to add
tests for your function, or I might break it later.

You'll find the repo at:

    https://github.com/magnars/s.el

Run the tests with

    ./run-tests.sh

Create the docs with

    ./create-docs.sh

I highly recommend that you install these as a pre-commit hook, so that
the tests are always running and the docs are always in sync:

    cp pre-commit.sh .git/hooks/pre-commit

Oh, and don't edit `README.md` directly, it is auto-generated.
Change `readme-template.md` or `examples-to-docs.el` instead.

## License

Copyright (C) 2012 Magnar Sveen

Authors: Magnar Sveen <magnars@gmail.com>
Keywords: strings

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
