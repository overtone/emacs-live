# parseclj Design Goals / Roadmap

parseclj is an Emacs Lisp library for parsing Clojure code and [EDN
data](https://github.com/edn-format/edn). It supports several input and output
formats, all powered by the same shift-reduce parser function.

This documents describes the design goals for parseclj, and as such may describe features which are not implemented yet.

## Motivation

People's parsing needs can differ for several reasons

- parsing code (Clojure) vs parsing data (EDN, a subset of Clojure)
- asserting valid input (fail fast) vs dealing gracefully with syntax errors (editor integration)
- analyzing code (whitespace can be ignored) vs doing transformations on code (whitespace aware round-tripping)
- parsing the contents of a buffer, or a string, or a file
- strict parsing (all tagged literals must be understood) vs pass-through (parsing/unparsing unknown tagged literals while ignoring semantics)

This library aims to support all of these use cases, either directly, or by providing the building blocks to do it yourself.

## Prior art

[edn.el](https://github.com/expez/edn.el) is an EDN-to-elisp parser based on the PEG parser generator library.

## Challenges

The data structures available in Emacs are less rich than those used by Clojure.

- Clojure has `nil` and `false`, Emacs only has `nil`.
- Emacs has no notion of sets
- Emacs has no date/timestamp type
  - there is a `time-date.el` which has functions for working with day/time represented as cons cells.
- Emacs has no "character" type (characters are represented as numbers)
- Emacs does not support custom records/types (there is a Common Lisp inspired object system, but it implements types on top of regular lists and vectors).
- Emacs does not support adding metadata to values
- Emacs does not support bignums

On the other hand Emacs supports strings/buffers with arbitrary encoding, on the JVM and on JavaScript strings are always UTF-16/UCS-2.

## Architecture

The implementation is implemented in three parts: a lexer, a parser, and [multiple reducers](#multiple-reducers).

### Lexer

The *lexer* turns the input text, a buffer, into tokens, data structures representing a single syntactical unit, such as a symbol, a number, or a delimiter like "(", ")", "#{", or "#_".

In parseclj the lexer is a single function `parseclj-lex-next` which can be called repeatedly to get a sequence of tokens. `parseclj-lex-next` returns the token at "point" (i.e. the Emacs cursor position), and moves point to after the token.

A *token* is a association list (list of cons cells), with keys `:token-type`, `:form`, `:position`, and optionally `:error-type`.

Note: we don't add line/column numbers to the token, the consumer can add these if needed based on the position of point before calling `parseclj-lex-next`.

Example:

This Clojure/EDN input:

``` clojure
(42 "hello" #_ignored #{:a})
```

Results in these tokens

``` emacs-lisp
((:token-type . :lparen)
 (:form . "(")
 (:pos . 1))

((:token-type . :number)
 (:form . "42")
 (:pos . 2))

((:token-type . :whitespace)
 (:form . " ")
 (:pos . 4))

((:token-type . :string)
 (:form . "\"hello\"")
 (:pos . 5))

((:token-type . :whitespace)
 (:form . " ")
 (:pos . 12))

((:token-type . :discard)
 (:form . "#_")
 (:pos . 13))

((:token-type . :symbol)
 (:form . "ignored")
 (:pos . 15))

((:token-type . :whitespace)
 (:form . " ")
 (:pos . 22))

((:token-type . :set)
 (:form . "#{")
 (:pos . 23))

((:token-type . :keyword)
 (:form . ":a")
 (:pos . 25))

((:token-type . :rbrace)
 (:form . "}")
 (:pos . 27))

((:token-type . :rparen)
 (:form . ")")
 (:pos . 28))

((:token-type . :eof)
 (:form . nil)
 (:pos . 29))
```

Note that the lexer makes no attempt to "interpret" the tokens, it merely finds their boundaries. Concatentating the `:form` values yields a string identical to the original input.

Tokens can be recognized by the `:token-type` key, which must always come first in the association list.

## Shift-reduce parser

The parser is again a single function `parseclj-parse`. It is a higher order function, with much of the final result determined by the `reduce-leaf` and `reduce-branch` functions passed in as arguments.

`parseclj-parse` internally operates by using a stack. This stack contains tokens (as returned by `parseclj-lex-next`), and reduced values.

`reduce-leaf` is a two-argument function. It takes the current value of the stack, and a token, and returns an updated stack, typically by parsing the token to a value and pushing that value onto the stack.

`reduce-branch` is a three-argument function. It takes the current value of the stack, a node type, and a list of children, and returns an updated stack.

The parser reads consecutive input tokens. If the token represents a leaf node (like a number, symbol, string), then it calls `reduce-leaf`, giving it a chance to add a value to the stack. If the token is a non-leaf node (a delimiter) it gets put on the stack as-is. This part is known as the "shift" phase.

After "shifting" a value on to the stack, the parser tries to "reduce", by inspecting the top one, two, or three items on the stack.

If the top item is a closing delimiter token, then the parser scans down the stack until it finds a matching opening delimiter. It pops both delimiters and everything in between them off the stack, and passes them to `reduce-branch`, which can "reduce" this sequence to a single value (say, a list), and push that item onto the stack.

The type of values pushed onto the stack depends on the reducing functions used. The parser only distinguishes between tokens and non-tokens. It follows that a reducing functions should not push raw tokens back onto the stack.

When parsing finishes the stack will contain all parsed forms in reverse order. It will call `reduce-branch` once more with a node type of `:root`, to give it a chance to finalize.

### Example

An example, when parsing the following EDN, with parsing done up to the position of `|`, the stack looks as follows.

``` clojure
;; input
(1 2 (3|))
```

``` emacs-lisp
;; stack
(3
 ((:token-type . :lparen) (:form . "(") (:pos . 6)))
 2
 1
 ((:token-type . :lparen) (:form . "(") (:pos . 1)))
```

Now the parser encounters the first closing parenthesis. It pops `3` and `:lparen` off the stack, and passes `(((:token-type . :lparen) 3 ((:token-type . :rparen)))` to `reduce-branch`, which reduces this a single list, and pushes it onto the stack.

``` clojure
;; input
(1 2 (3)|)
```

``` emacs-lisp
;; stack
((3)
 2
 1
 ((:token-type . :lparen) (:form . "(") (:pos . 1)))
```

Now the parser encounters the second closing parenthesis. It pops everything until `:lparen` off the stack, and passes it to `reduce-branch`, which turns the result into a list and pushes it onto the stack.

``` clojure
;; input
(1 2 (3))|
```

``` emacs-lisp
;; stack
((1 2 (3)))
```

### Dealing with parse errors

`parseclj-parse` needs to be able to parse invalid input. Imagine analyzing a user's buffer while they are editing, to provide contextual help or do linting. Even when delimiters are unbalanced it should still be possible to get a "best effort" parse result. It turns out the shift-reduce approach provides that out of the box. The result of parsing invalid input is a stack which still has unreduced tokens in it.

Unmatched opening delimiter:

``` clojure
;; input
(1 2 { 3)
```

``` emacs-lisp
;; result
((1 2 ((:token-type . :lbrace)) 3))
```

Unmatched closing delimiter:

``` clojure
;; input
(1 2 3))
```

``` emacs-lisp
;; result
((1 2 3) ((:token-type . :lparen)))
```

In many cases it will be desirable to "fail fast", and raise an error as soon as a syntax error is encountered. A `reduce-branch` function can do so if it wishes by checking its input sequence for raw tokens, and raising an error if any are present.

## Multiple Reducers

While the shift-reduce parser parses input, it reduces parsed values (leafs tokens and collections) using "reducing functions".  These functions are in charge of giving actual meaning to the parsed data.

Currently, there are two sets of reducing functions implemented: one set that reduces parsed values to AST data structures, and another one that reduces to Emacs Lisp values.

The AST reducing functions are part of [`parseclj`](https://github.com/clojure-emacs/parseclj), while the Emacs Lisp reducing functions are separated into a differente package called [`parseedn`](https://github.com/clojure-emacs/parseedn).

To understand why these two set of reducing functions are separated, it's important to look at the difference between EDN and Clojure, and also between Emacs Lisp elements and EDN elements.

### EDN vs Clojure

EDN syntax is a subset of Clojure syntax used for representing data (as opposed to code).

Several constructs that are common in Clojure code are not valid in EDN. This includes

- quoting, syntax quoting, syntax quote splicing, etc.
- read time evaluation with `#=(,,,)`

### Dealing with tagged literals

EDN/Clojure syntax is extensible. Values can be prefixed with user-provided tags, which indicates they need to be transformed after reading by a registered handler function.

In general a consumer of EDN needs to be aware of the possible tags that can occur in the input, as well as their semantics, in order to preserve the data's semantics. When a parser encounters an unknown tag then this is considered an error.

The EDN parser functions will take an optional tag handler function. This function receives two arguments, the tag symbol, and the parsed form that follows the tag. It should either return a correctly coerced value, or raise an error. A default function that knows how to deal with the tags that are part of the EDN spec will be provided.

When parsing code to an AST the situation is different, here tags simply become part of the AST, without caring about their semantics.

### Representing EDN as Emacs Lisp values

See also the section at the top regarding differences between Clojure's data types vs those available in Emacs Lisp.

These are the choices that the edn.el library has made:

- represent sets, inst and uuid values as `cl-defstruct` objects
  - `'(edn-set (... set values ...))`
  - `'(edn-inst high low)` see `time-date.el`
- parse maps as hash tables
- represent characters as integers, *but* parse `\newline` `\return` `\space` and `\tab` as the symbols `'newline` `'return` etc.
- parse `true` as `t`, `nil` and `false` as `nil`.

#### Differences with EDN.el

At the moment the `parseedn-*` copy the parsing behavior of edn.el, *except* that the character literals `\newline`, `\return`, `\space`, and `\tab` are parsed to their character code (10, 13, 32, and 9 respectively), instead of to symbols.

### AST

An AST (abstract syntax tree) is a tree structure made up of nodes. A node looks like this

``` emacs-lisp
((:node-type . :root)
 (:position . 0)
 (:children . (((:node-type . :comment)
                (:position . 0)
                (:form . ";; cool stuff\n;; in here"))
               ((:node-type . :list)
                (:position . 26)
                (:children . (((:node-type . :number)
                               (:position . 27)
                               (:form "123")
                               (:value 123))))))))
```

Nodes are an alist with `:node-type` as the first key and a `:position`. Leaf nodes have `:form` and `:value` keys, for the unparsed and parsed form respectively. `:whitespace` and `:comment` nodes only have a `:form`, not a `:value`.

Non-leaf nodes contain a list of `:children`.

## Public API

> Disclaimer: outdated information -- this API has been deprecated in favor of [Alternative Package Layout](#alternative-package-layout), and it's only kept here for historical purposes.

parseclj provides three "parse modes"

- `edn` meant for parsing data, it parses EDN to emacs lisp data
- `ast` meant for analyzing source code, parses to a "semantic" AST, does not preserve whitespace or comments
- `source` meant for transforming/round-tripping source codes. Like `ast`, but preserves whitespace and comments

For each of these there can be the following functions

- `parseclj-{mode}` parse the current buffer starting at `point`, raise an error when syntax/lexing errors are encountered
- `parseclj-{mode}-full` same as above but ignore syntax errors, returning a partially parsed result
- `clj-print-{mode}` turn the result of the corresponding parse function back into Clojure/EDN, and insert it into the current buffer

Each of these have `-str` variant which instead works on strings. This yields a total potential API of:

```
(defun parseedn (&OPTIONAL tag-handler))
(defun parseedn-full (&OPTIONAL tag-handler))
(defun clj-print-edn (edn))
(defun parseedn-str (string &OPTIONAL tag-handler))
(defun parseedn-full-str (string &OPTIONAL tag-handler))
(defun clj-print-edn-str (edn))
(defun parseclj-ast ())
(defun parseclj-ast-full ())
(defun clj-print-ast (node))
(defun parseclj-ast-str ())
(defun parseclj-ast-full-str ())
(defun clj-print-ast-str (node))
(defun parseclj-source ())
(defun parseclj-source-full ())
(defun clj-print-source (node))
(defun parseclj-source-str ())
(defun parseclj-source-full-str ())
(defun clj-print-source-str (node))
```

This may seem like a massive API surface, but these will all be only one or two lines, combining the generic parsing function with the right reducers.

Beyond that we provide two sets of functions for working with AST nodes.

- zipper-like functions for navigating and transforming ASTs
- functions for reading values out of ASTs, effectively treating them as the data structure they represent.

This second set of functions is important for applications that need to deal with EDN data, but for which the lossy nature of EDN->Elisp transformation is not an option. For instance, unrepl sends EDN messages, but these messages contain code forms that we need to be able to reproduce. In this case converting `false` to `nil` or a set to a list is not acceptable. Instead we can parse the EDN to an AST, and deal with the AST directly.

## Alternative package layout

**Package**: parseclj

Contains the core parser backend, and the Clojure-to-AST parser

- file: parseclj.el

``` emacs-lisp
(defun parseclj-parse-clojure (&rest string-and-options)
  "Parse Clojure source to AST.

Reads either from the current buffer, starting from point, until
point-max, or reads from the optional string argument.

STRING-AND-OPTIONS can be an optional string, followed by
key-value pairs to specify parsing options.

- `:lexical-preservation' Retain whitespace, comments, and
  discards. Defaults to false (`nil').
- `:fail-fast' Raise an error
  when encountering invalid syntax. Defaults to true (`t'). ")

(defun parseclj-unparse-clojure (ast &rest options)
  "Parse Clojure AST to source code.

Given an abstract syntax tree AST (as returned by
parseclj-parse-clojure), turn it back into source code, and
insert it into the current buffer.

OPTIONS is a list of key value pairs containing options.

- `:lexical-preservation' If `t', assume the AST contains
  whitespace. If `nil', insert whitespace between forms. When
  parsing with `:lexical-preservation', you should unparse the
  same way. ")

(defun parseclj-unparse-clojure-to-string (ast &rest options)
  "Parse Clojure AST to a source code string.

Given an abstract syntax tree AST (as returned by
parseclj-parse-clojure), turn it back into source code, and
return it as a string

OPTIONS is a list of key value pairs containing options.

- `:lexical-preservation' If `t', assume the AST contains
  whitespace. If `nil', insert whitespace between forms. When
  parsing with `:lexical-preservation', you should unparse the
  same way.")
```

- file: parseclj-lex.el

``` emacs-lisp
(defun parseclj-lex-next ()
  "Move past the token at point, and return the token")
```

- file: parseclj-ast.el

``` emacs-lisp
(defun parseclj-ast--reduce-leaf (stack token)
  "Create a leaf AST node and put it onto the stack.

Given the current parser STACK and a TOKEN coming from the lexer,
create an AST leaf node and return an updated stack.

Whitespace and comment tokens are ignored (i.e. the stack is
returned unchanged).

This function is only called for tokens that correspond with AST
leaf nodes.")

(defun parseclj-ast--reduce-leaf-with-lexical-preservation (stack token)
  "Create a leaf AST node and put it onto the stack.

Given the current parser STACK and a TOKEN coming from the lexer,
create an AST leaf node and return an updated stack.

This functions creates nodes for whitespace and comment tokens,
for other tokens it delegates to `parseclj-ast--reduce-leaf'.")

(defun parseclj-ast--reduce-branch (stack type children)
  "Create a branch AST node and put it onto the stack.

This function is passed the current parser STACK the node TYPE to
be created, and a list of AST nodes that will become the CHILDREN
of the newly created node.

This implementation ignores `:discard' nodes (#_), when called
with a TYPE of `:discard' it returns the stack unchanged.")

(defun parseclj-ast--reduce-branch-with-lexical-preservation (stack type children)
  "Create a branch AST node and put it onto the stack.

This function is passed the current parser STACK the node TYPE to
be created, and a list of AST nodes that will become the CHILDREN
of the newly created node.

This implementation handles `:discard' nodes (#_), for other node
types it delegates to `parseclj-ast--reduce-branch'.")

(defun parseclj-ast-value (node)
  "Given an AST NODE, returns its value.

Recursively convert the AST node into an Emacs Lisp value. E.g.
turn a `:list' node into a sexp, a `:number' node into a number.

This operation is lossy because not all Clojure types have a
corresponding type in Emacs. `nil' and `false' form a
particularly poignant case, both are converted to `nil'.")
```

**Package**: [parseedn](https://github.com/clojure-emacs/parseedn)

- file: parseedn.el

``` emacs-lisp
(defun parseedn-read (&rest string-and-options)
  "Reads an EDN form and converts it an Emacs Lisp value.

Reads either from the current buffer, starting from point, or
reads from the optional string argument. Only reads the first
complete form. When used on a buffer, this moves `(point)' to
after the form.

By default uses an output format that uses tagged lists to
preserve type information. This makes the conversion lossless,
but still easy to process.

  \"#{1 2 3}\" => (set (1 2 3))
  \"false\"    => (false)
  \"t\"        => t
  \"true\"     => (true)
  \"(1 2 3)\"  => (list (1 2 3))
  \"#uuid \\\"255efd69-dec9-4428-9142-cebd5357fb2a\\\"\"
    => (uuid \"255efd69-dec9-4428-9142-cebd5357fb2a\")

Alternatively a compatibility mode is available which mimics
exactly the behavior of `edn-read' as implemented in `edn.el'.

STRING-AND-OPTIONS can be an optional string, followed by
key-value pairs to specify parsing options.

- `:compat' Mimic edn.el. Defaults to false (`nil').
- `:tag-readers' An association list mapping symbols to
  functions, used to parse tagged literals. The function is given
  the parsed value and given an opportunity to transform it.
  Defaults for `uuid' and `inst' are provided but can be
  overridden.
- `:fail-fast' Raise an error when encountering invalid syntax.
  Defaults to true (`t'). ")

(defun parseclj-print (value &rest options)
  "Convert an Emacs Lisp value to EDN.

OPTIONS is a list of key value pairs containing options.

By default assumes that any list is of the form `(type value)'.
Extra `:tag-writers' can be specified to handle unknown types.
Alternatively a compatibility mode is available which emulates
the behavior of `edn.el'

- `:compat' If `t', mimic `edn.el'. Defaults to `false' (`nil').
  When this is set to `t' then `:tag-writers' is ignored.
- `:tag-writers' An association list from symbol to function.
  Each function is given a list including `type' tag, and should
  return a value that can be handled by `parseclj-print'.")
```
