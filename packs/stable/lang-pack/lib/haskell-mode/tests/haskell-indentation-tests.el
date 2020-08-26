;;; haskell-indentation-tests.el --- tests for indentation module  -*- lexical-binding: t -*-

;; Copyright © 2015 Haskell Mode contributors. All rights reserved.
;;             2016 Arthur Fayzrakhmanov.

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; These are tests for `haskell-indentation-mode'.  It's easy to add new
;; tests, just...

(require 'cl-lib)
(require 'ert)
(require 'haskell-test-utils)
(require 'haskell-mode)
(require 'haskell-font-lock)
(require 'haskell-indentation)
(require 'haskell-indent)

;;; Code:

(defsubst string-trim-left (string)
  "Remove leading whitespace from STRING."
  (if (string-match "\\`[ \t\n\r]+" string)
      (replace-match "" t t string)
    string))

(defun haskell-indentation-check (source &rest test-cases)
  "Check if `haskell-indentation-find-indentations' returns expected results.

SOURCE should be a string representing Haskell source code.  It
will be inserted in a brand-new buffer where `haskell-mode' and
`haskell-indent-mode' are enabled.  Position of point and
expected results of `haskell-indentation-find-indentations' are
described in TEST-CASES.

Every element in TEST-CASES list should have the following
structure:

    (line pos0 pos1 pos2 …)

point will be placed in the first column of LINE before testing.
POS0, POS1, POS2, … are expressions representing indentation
positions.

For example:

    (2 0 7)

means that when point is placed at line 2 and column 0,
`haskell-indentation-find-indentations' should return value that's
equal to (0 7).

It's recommended to specify several test-cases per one snippet
because it helps increase coverage."
  (dolist (current test-cases)
    (cl-destructuring-bind (line . result) current
      (with-temp-buffer
        (haskell-mode)
        (haskell-indentation-mode 1)
        (insert source)
        (newline)
        (font-lock-fontify-buffer)
        (goto-char (point-min))
        (forward-line (1- line))
        (should
         (equal current
                (cons (line-number-at-pos)
                      (condition-case condition
                          (haskell-indentation-find-indentations)
                        (error
                         ;; for unknown reason Emacs 24.4 ERT does not
                         ;; catch overrun recursion, so we have to
                         ;; catch it here, and throw it again
                         (signal (car condition) (cdr condition)))))))))))

(defmacro hindent-test (name source &rest test-cases)
  "Define ert test using `haskell-indentation-check'.

This little macro helps eliminate boilerplate.  It automatically
expracts prefix from NAME and uses it to name result test.  If
the prefix (everything before first space) contains asterisk *,
this test is allowed to fail.  It trims empty lines from the
beginning of SOURCE.  TEST-CASES don't need to be quoted, the
macro quotes them for you."
  (declare (indent defun))
  (let ((split-pos (cl-position ?  name)))
    (cl-destructuring-bind (test-name allow-failure doc-string)
        (if split-pos
            (let ((raw-prefix (substring name 0 split-pos)))
              (list (intern
                     (concat "haskell-indentation-check-"
                             (remove ?* raw-prefix)))
                    (cl-find ?* raw-prefix)
                    (substring name (1+ split-pos))))
          (list 'haskell-indentation-check-fixme
                nil
                name))
      `(ert-deftest ,test-name ()
         ,doc-string
         :expected-result
         ,(if allow-failure :failed :passed)
         (haskell-indentation-check
          ,(string-trim-left source)
          ,@(mapcar (lambda (x)
                      (list 'quote x))
                    test-cases))))))

(ert-deftest haskell-indentation-turns-off-haskell-indent ()
  (with-temp-buffer
    (haskell-mode)
    (haskell-indent-mode)
    (should haskell-indent-mode)
    (haskell-indentation-mode)
    (should haskell-indentation-mode)
    (should-not haskell-indent-mode)

    (haskell-indent-mode)
    (should-not haskell-indentation-mode)
    (should haskell-indent-mode)))

(hindent-test "1 Check if '{' on its own line gets properly indented""
function = Record
       { field = 123 }"
              (1 0)
              (2 2 11))

(hindent-test "2 Handle underscore in identifiers""
function = do
  (_x) <- return ()
 z"
              (1 0)
              (2 2)
              (3 0 2 4 10))

(hindent-test "2u Handle underscore in identifiers""
function = do
  (_x) ← return ()
 z"
              (1 0)
              (2 2)
              (3 0 2 4 9))

(hindent-test "2a Handle apostrophe in identifiers""
function = do
  (_'x') <- return ()
 z"
              (1 0)
              (2 2)
              (3 0 2 4 12))

(hindent-test "2au Handle apostrophe in identifiers""
function = do
  (_'x') ← return ()
 z"
              (1 0)
              (2 2)
              (3 0 2 4 11))

(hindent-test "3 Import statememnt symbol list 1""
import Control.Concurrent
       ( forkIO,
         killThread )"
              (1 0)
              (2 0 2 7)
              (3 9)
              (4 0 2 7))

(hindent-test "4 Import statememnt symbol list 2""
import Control.Concurrent
       ( forkIO
       , killThread )"
              (1 0)
              (2 0 2 7)
              (3 7)
              (4 0 2 7))

(hindent-test "5 List comprehension""
fun = [ x | y
          , z ]"
              (1 0)
              (2 8 10)
              (3 0 2 6))

(hindent-test "5a* List comprehension""
fun = [ x | y,
            z ]"
              (1 0)
              (2 12)
              (3 0 6))

(hindent-test "6a* \"let\" in list comprehension""
fun = [ f | x <- xs
          , y <- ys
          , let c = 123
          , f <- fx x y c ]"
              (1 0)
              (2 10)
              (3 10)
              (4 10)
              (5 0 6))

(hindent-test "6b* \"let\" in list comprehension""
fun = [ f | x <- [1]
          , y <- [2]
          , let c, d :: Int
                c = 123
                d = 45
          , f <- [x * y * c * d] ]"
              ;; this one is interesting because the comma betweed c
              ;; and d does not count as a comma for list
              ;; comprehension elements
              (1 0)
              (2 10)
              (3 10)
              (4 16)
              (5 16)
              (6 10)
              (7 0 6))


(hindent-test "6u* \"let\" in list comprehension""
fun = [ f | x ← xs
          , y ← ys
          , let c = 123
          , f ← fx x y c ]"
              (1 0)
              (2 10)
              (3 10)
              (4 10)
              (5 0))

(hindent-test "6bx \"let\" in do""
fact n = do
  let g = 7
  z <- let x = 5
       in return (x + 4)"
              (1 0)
              (2 2)
              (3 2 6 8 10)
              (4 4 7)
              (5 0 2 4 10))


(hindent-test "7a \"data\" after \"data\"""
data ABC = ABC
data DEF = DEF"
              (1 0)
              (2 0))

(hindent-test "7 \"import\" after \"import\"""
import ABC
import DEF"
              (1 0)
              (2 0)
              (3 0 2 7))

(hindent-test "7b declaration after declaration" "
fun1 = undefined
fun2 = undefined"
              (1 0)
              (2 0 2 7))

(hindent-test "8 Guards in function definition""
resolve (amount, max) number
  | number > max = (1, number)
  | number == max = (amount + 1, number)"
              (1 0)
              (2 2)
              (3 2)
              (4 0 2 4 20))

(hindent-test "9 Operator last on line""
fun = x ++"
              (1 0)
              (2 2 6))

(hindent-test "10 Operator first on line""
fun = x
      ++ z"
              (1 0)
              (2 2 6))

(hindent-test "11 Guards with commas""
clunky env var1 var2
  | Just val1 <- lookup env var1
  , Just val2 <- lookup env var2"
              (1 0)
              (2 2)
              (3 2)
              (4 0 2 4 17))

(hindent-test "11u Guards with commas""
clunky env var1 var2
  | Just val1 ← lookup env var1
  , Just val2 ← lookup env var2"
              (1 0)
              (2 2)
              (3 2)
              (4 0 2 4 16))

(hindent-test "12 \"do\" as expression""
fun = do { putStrLn \"X\";
         }"
              (1 0)
              (2 9 11)
              (3 0 2))

(hindent-test "13a Deriving on new line""
data X = X | Y
deriving (Eq, Ord, Show)"
              (1 0)
              (2 0 2 7))

(hindent-test "13b Don't indent after deriving""
data X = X
  deriving (Eq, Ord, Show)"
              (1 0)
              (2 0 2 7)
              (3 0))

(hindent-test "13bb Don't indent after deriving""
data X = X
  deriving"
              (1 0)
              (2 0 2 7)
              (3 4))

(hindent-test "13c honour = on a separate line in data declaration" "
data X a b
  = X"
              (1 0)
              (2 2))

(hindent-test "14* Line starting with operator inside \"do\" needs to be indented""
fun = do
  pure X
  something
    <*> marg"
              (1 0)
              (2 2)
              (3 0 2)
              (4 4))

(hindent-test "15* An \"if..then\" inside a \"do\" block""
fun = do
  if x
  then do
    putStrLn \"True\""
              (1 0)
              (2 2)
              (3 2)
              (4 4)
              (5 2 4))

(hindent-test "16 Lambda and a \"do\" block""
fun = \\x -> do"
              (1 0)
              (2 2))

(hindent-test "16a A lambda""
fun = \\x ->"
              (1 0)
              (2 2 8))

(hindent-test "16u Lambda and a do block""
fun = \\x → do"
              (1 0)
              (2 2))

(hindent-test "16au A lambda""
fun = \\x →"
              (1 0)
              (2 2 8))

(hindent-test "17a A type for a function""
fun :: Int
    -> Int"
              (1 0)
              (2 2 4)
              (3 0 2 4))

(hindent-test "17au A type for a function""
fun :: Int
    → Int"
              (1 0)
              (2 2 4)
              (3 0 2 4))

(hindent-test "17b* A type for a function with context""
fun :: Monad m
    => Int"
              (1 0)
              (2 4)
              (3 0 4))

(hindent-test "17bu* A type for a function with context""
fun ∷ Monad m
    ⇒ Int"
              (1 0)
              (2 4)
              (3 0 4))

(hindent-test "17c* A type for a function with complicated context""
fun :: (Monad m, MonadBaseControl IO m, MyMonad (A v) m)
    => MyMonad (A v) m"
              (1 0)
              (2 4)
              (3 0 4))

(hindent-test "17cu* A type for a function with complicated context""
fun ∷ (Monad m, MonadBaseControl IO m, MyMonad (A v) m)
    ⇒ MyMonad (A v) m"
              (1 0)
              (2 4)
              (3 0 4))

(hindent-test "17d* A type for a function with param and a complicated context""
fun :: (Monad m, MonadBaseControl IO m, MyMonad (A v) m)
    => MyMonad (A v) m
    -> m (Maybe a)"
              (1 0)
              (2 4)
              (3 4)
              (4 0 4))

(hindent-test "17du* A type for a function with param and a complicated context""
fun ∷ (Monad m, MonadBaseControl IO m, MyMonad (A v) m)
    ⇒ MyMonad (A v) m
    → m (Maybe a)"
              (1 0)
              (2 4)
              (3 4)
              (4 0 4))

(hindent-test "18a* \"if-then-else\" indentation: \"then\"""
x = if flag
    then 1"
              (1 0)
              (2 4)
              (3 4 9))

(hindent-test "18b \"if-then-else\" indentation: \"else\"""
x = if flag
    then 1
    else 0"
              (1 0)
              (2 2 4)
              (3 2 4)
              (4 0 2 9))

(hindent-test "18c* \"do\" and \"if-then-else\" indentation: \"then\"""
x = do
  if flag
  then 1"
              (1 0)
              (2 2)
              (3 2)
              (4 2))

(hindent-test "18d* \"do\" and \"if-then-else\" indentation: \"else\"""
x = do
  if flag
  then 1
  else 0"
              (1 0)
              (2 2)
              (3 2)
              (4 2))

(hindent-test "18e* \"do\" and \"if-then-else\" indentation: \"else\"""
x = do
  if flag
  then do
    return ()"
              (1 0)
              (2 2)
              (3 2)
              (4 4)
              (5 2 4))

(hindent-test "18f* \"do\" and \"if-then-else\" indentation: \"else\"""
x = do
  if flag
  then do
    return ()
  else do
    return ()"
              (1 0)
              (2 2)
              (3 2)
              (4 4)
              (5 2 4)
              (6 4)
              (7 0 2 4))

(hindent-test "19a \"let\" and \"in\"""
x = let
  y"
              (1 0)
              (2 2)
              (3 0 2 4))

(hindent-test "19b \"let\" and \"in\"" "
x = let y
    in
      z"
              (1 0)
              (2 2 4)
              (3 2 6))

(hindent-test "19c \"let\" in a \"do\"""
x = do
  thing
  let
    z = 5"
              (1 0)
              (2 2)
              (3 0 2 4)
              (4 4))

(hindent-test "20a \"instance\" declaration""
instance C a where
  c = undefined"
              (1 0)
              (2 2)
              (3 0 2 4 6))

(hindent-test "20b \"instance\" declaration""
instance (Monad m) => C m a where
  c = undefined"
              (1 0)
              (2 2)
              (3 0 2 4 6))

(hindent-test "20bu \"instance\" declaration""
instance (Monad m) ⇒ C m a where
  c = undefined"
              (1 0)
              (2 2)
              (3 0 2 4 6))

(hindent-test "21a fix \"let\" statement in \"do\" block""
main :: IO ()
main = do
let foo = Foo {
      bar = 0
      , baz = 0"
              (1 0)
              (2 0 2 8)
              (3 2)
              (4 6)
              (5 6)
              (6 6 8 14))

(hindent-test "21b fix named fields in \"data\" declaration""
data Foo = Foo {
  bar :: Int
  , baz :: Int"
              (1 0)
              (2 2)
              (3 2)
              (4 2 4 8 11))

(hindent-test "21c \"data\" declaration open on next line" "
data Foo = Foo
  { bar :: Int
  , baz :: Int"
              (1 0)
              (2 2 11)
              (3 2)
              (4 2 4 8 11))

(hindent-test "22 should obey layout only outside parentheses" "
func = 1234
  where
    foo :: Ivory eff ()
    foo = do
      return ()"
              (1 0)
              (2 2)
              (3 4)
              (4 0 4 6 11)
              (5 6))

(hindent-test "23 should not fail when seeing comments" "
-- important non-empty line
{-
-}"
              (3 0))

(hindent-test "24 should parse inline type signatures properly" "
foo = do
  _ :: String <- undefined
  _ :: String <- undefined
  return ()"
              (1 0)
              (2 2)
              (3 0 2 4 17)
              (4 0 2 4 17))

(hindent-test "25a* support scoped type declarations" "
foo = do
  bar :: String
      -> String
    <- undefined"
              (1 0)
              (2 2)
              (3 4 6 9)
              ;; here it brakes, it would like to put '<-' on same line with 'bar'
              ;; the culprit is the 'do' keyword
              (4 4))

(hindent-test "25b support scoped type declarations" "
foo = let
  bar :: String
      -> String
    = undefined"
              (1 0)
              (2 2)
              (3 4 6)
              (4 4))

(hindent-test "26 should parse unindented where-clause properly" "
foo = do
    return ()
  where
    bar = undefined"
              (4 4))

(hindent-test "27* expecting then (GH-884)" "
foo = do
    if True
    then return ()
"
              (4 4))

(hindent-test "28a names starting with quotes" "
f = a (a 'A)
    (a 'A)
"
              (2 0 2 4))

(hindent-test "28g continue expression after value" "
f = a
       a"
              (3 0 2 7))

(hindent-test "28h continue expression after parentheses" "
f = a
       (a)"
              (3 0 2 7))

(hindent-test "28b character literal (escape sequence)" "
f = '\\\\'

"
              (2 0 2 4))


(hindent-test "28c name starting with a quote" "
function (Operation 'Init) = do
  print 'Init
"
              (2 2))

(hindent-test "29a quasiquote single line" "
test = [randomQQ| This is a quasiquote with the word in |]"
              (2 2))

(hindent-test "29b quasiquote multiple lines" "
test = [randomQQ| This is
          a quasiquote
          with the word in |]"
              (4 2))

(hindent-test "29c quasiquote with quotes in it and a string outside" "
foo = do
  let bar = [text|\"some text\"|]
  button \"Cancel\" $ do
"
              (4 4))

(hindent-test "29d unfinished quasiquote" "
foo = [text|some
"
              (2 0 11))

(hindent-test "29e an expression quotation" "
foo = [|forever $ do
"
              (2 10))

(hindent-test "30 parse '[] identifier correctly" "
instance Callable '[] where
"
              (2 2))

(hindent-test "31* allow type class declaration without methods" "
class Foo a where
instance Bar Int
"
              (2 0))

(hindent-test "32 allow type operators" "
data (:.) a b = a :. b"
              (2 0 2 14 16))

(hindent-test "32b next line after data" "
data X = X | Y"
              (2 0 2 7 13))

(hindent-test "32c* next line after unfinished data" "
data X = X | Y |"
              (2 2 9))

(hindent-test "33* parse #else in CPP" "
#ifdef FLAG
foo = ()
#else
"
              (4 0))


(hindent-test "34 beginning of line inside parentheses" "
data T = T {
  foo :: String
, bar :: String
}

"
              (5 0 2 7 9))

(hindent-test "35 baroque construct which causes parse error" "
az = Projection
  { unproject = do
        case x of
          _ -> return
  , maxR = pi
  }
"
              (6 2))

(hindent-test "35a parse a backslash properly" "
az = Projection
  { unproject = \\x -> do
        case x of
          _ -> return
  , maxR = pi
  }
"
              (6 2))

(hindent-test "36 yet another parser failure" "
tokOpenTag =
  asum [ do void
       , return
       ]
"
              (4 7))
(hindent-test "37 Indent continuation lines in multiline string literal" "
a = \"multiline\\
"
              (2 0 4))

(hindent-test "38 Indent in do block after multiline string literal" "
s = do
  a <- \"multiline\\
       \\ line 2\"
"
              (4 0 2 4 7))

(hindent-test "39 do not crash after two multiline literals in do block" "
servePost = do
  a <- fun \"line 1\\
           \\line 2\"
  b <- queryT \"comma is important: , \\
             \\ line 2 \"
"
              (6 0 2 4 7))

(hindent-test "40 parse error in multiline tuple" "
a = ( 1
, "
              (2 4)
              (3 6))

(hindent-test "41 open do inside a list" "
x = asum [ withX $ do
             return ()
         ]
"
              (2 13))

(hindent-test "42 open do inside a list second element" "
x = asum [ mzero
         , withX $ do
             return ()
         ]
"
              (3 13))

(hindent-test "43 open do inside a list second element, reset alignment" "
x = asum [ mzero
             , withX $ do
                 return ()
         ]
"
              (3 17))

(hindent-test "44 expression continues, reset alignment" "
function = abc
       def
       xyz"
              (3 0 2 7))

(hindent-test "46 case expression with paths on their own lines" "
fact n =
  case n of
    0 -> 1
    _ -> n * fact (n - 1)
"
              (1 0)
              (2 2)
              (3 4)
              (4 0 2 4 6 9)
              (5 0 2 4 6 9))

(hindent-test "46b case expression with guards" "
fact n = case n of
  n | n == 0 -> 1
  _ | n > 0
    , True == True -> n * fact (n - 1)"
              (1 0)
              (2 2 11)
              (3 0 2 6 9 16)
              (4 4)
              (5 0 2 4 6 9 22))

(hindent-test "47a multiline strings" "
fact n = \"\\
         \\a\""
              (1 0)
              ;; we want to offer both a continuation style and the
              ;; align to left column style (like in lisp)
              (2 0 9)
              (3 0 2 9))

(hindent-test "47b multiline strings" "
fact n = \"\\
      \\a\\
      \\x\""
              ;; here we want to keep third line like the second one,
              ;; although the second one wasn't best indented
              (1 0)
              (2 0 9)
              (3 6))

(hindent-test "48 functional dependencies" "
class X a b | a -> b
            , b -> a where
  fun :: a -> b"
              (1 0)
              (2 2 12)
              (3 2)
              (4 0 2 4 6 9))

(hindent-test "49 data with GADT syntax" "
data Term a where
  Lit :: Int -> Term Int
  Pair :: Term a -> Term b -> Term (a,b)"
              (1 0)
              (2 2)
              (3 0 2 4 9)
              (4 0 2 4 7 10))

(hindent-test "49b data with GADT syntax and a deriving clause" "
data G [a] b where
  G1 :: c -> G [Int] b
  deriving (Eq)"
              (1 0)
              (2 2)
              (3 0 2))

(hindent-test "50 standalone deriving" "
data Name = Name String
deriving instance Eq Name"
              (1 0)
              ;; We accept position 2 here because we have just one
              ;; look-ahead token so we do not see 'instance'
              ;; following 'deriving'.
              (2 0 2 10))

(hindent-test "51 standalone deriving" "
data family T a
data    instance T Int  = T1 Int | T2 Bool
newtype instance T Char = TC Bool"
              ;; We check that indentation does not bail on 'instance'
              ;; here, we do not really check if it is working
              ;; correctly. Needs better test.
              (1 0)
              (2 0)
              (3 0)
              (4 0 2 24 26))

(hindent-test "52a module simplest case two lines" "
module A.B
where"
              (1 0)
              (2 0)
              (3 0))

(hindent-test "52b module simplest case one line" "
module A.B where"
              (1 0)
              (2 0))

(hindent-test "52c module with exports" "
module A.B
  ( x
  , y
  )
where"
              (1 0)
              (2 2)
              (3 2)
              (4 2)
              (5 0)
              (6 0))

(hindent-test "53 multiway if" "
fun = if | guard1 -> expr1
         | guardN -> exprN"
              (1 0)
              (2 9)
              (3 0 2 9 11 21))

(hindent-test "54 equal after guards on separate line" "
foo x
  | True
  = X"
              (1 0)
              (2 2)
              (3 2))

(hindent-test "55 data constructor on separate line" "
data Foo = Bar
         | Baz"
              (1 0)
              (2 2 9))

(hindent-test "55a deriving below aligned data constructors" "
data Foo = Bar
         | Baz
         deriving (Show)"
              (1 0)
              (2 2 9)
              (3 0 2 9))

(hindent-test "56 module name on next line" "
module
  X"
              (1 0)
              (2 2))

(hindent-test "57 module continued" "
module X"
              (1 0)
              (2 2))

(hindent-test "58 module where same line" "
module X where"
              (1 0)
              (2 0))

(hindent-test "59 module where same line" "
module X
   where"
              (1 0)
              (2 0)
              (3 0))

(hindent-test "60* must continue indentation after a vertical bar" "
data X = X |
         Y"
              (1 0)
              (2 2 9)
              (3 0 7 9))

(hindent-test "61 unterminated/multiline strings whose line doesn't end in backslash" "
func = \"unterminated
           where"
              (1 0)
              (2 2))

(hindent-test "62 foreign import""
import javascript unsafe
  \"$2[$1]\" js_getProp :: S.JSString -> O.Object -> T.JSVal"
              (1 0)
              (2 0 2 7))

(ert-deftest haskell-indentation-ret-indents ()
  (with-temp-switch-to-buffer
   (haskell-mode)
   (insert "main = do")

   (execute-kbd-macro (kbd "<RET>"))
   (should (equal 2 (- (point) (line-beginning-position))))))

(ert-deftest haskell-indentation-tab-and-backtab ()
  (with-temp-switch-to-buffer
    (haskell-mode)
    (insert "main = do\n  lala\n  ")

    (execute-kbd-macro (kbd "TAB"))
    (should (equal 4 (- (point) (line-beginning-position))))

    (execute-kbd-macro (kbd "<backtab>"))
    (should (equal 2 (- (point) (line-beginning-position))))))

(ert-deftest haskell-indentation-altj-comment ()
  :expected-result :failed
  ;; Emacs 25 (snapshot) somehow passes this test, there is something
  ;; fishy going on
  (skip-unless (< emacs-major-version 25))
  (with-temp-switch-to-buffer
    (haskell-mode)
    (insert "main = do\n    return ()\n\n-- comment")
    (execute-kbd-macro (kbd "M-j"))
    (should (equal 3 (- (point) (line-beginning-position))))))

;;; haskell-indentation-tests.el ends here
