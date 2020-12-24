;;  -*- lexical-binding: t -*-
(require 'ert)
(require 'haskell-indent)
(require 'haskell-mode)


(ert-deftest haskell-indent-in-comment-1 ()
  "Document bad behavior. Should not assert."
  :expected-result :failed
  ;; Emacs 25 (snapshot) starts debugger on cl-assert
  ;; even in batch mode. So we do not run this test.
  (skip-unless (< emacs-major-version 25))
  (should (with-temp-buffer
            (haskell-mode)
            (haskell-indent-mode)
            (insert (concat "module Test where\n"
                            "-- {{{ A header\n"
                            "--\n"))
            (indent-for-tab-command)
            t)))


;; haskell-indent-put-region-in-literate happens to be in haskell-indent
;; when the function is moved, move the tests also
(ert-deftest haskell-indent-put-region-in-literate-1 ()
  (should (equal "> literate"
                 (with-temp-buffer
                   (insert "literate")
                   (haskell-literate-mode)
                   (haskell-indent-put-region-in-literate (point-min) (point-max))
                   (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest haskell-indent-put-region-in-literate-2 ()
  (should (equal "literate"
                 (with-temp-buffer
                   (insert "> literate")
                   (haskell-literate-mode)
                   (haskell-indent-put-region-in-literate (point-min) (point-max) -1)
                   (buffer-substring-no-properties (point-min) (point-max))))))

(defsubst string-trim-left (string)
  "Remove leading whitespace from STRING."
  (if (string-match "\\`[ \t\n\r]+" string)
      (replace-match "" t t string)
    string))

(defun haskell-indent-format-info (info)
  (if (cdr info)
      (list (car info) (substring-no-properties (cdr info)))
    (car info)))

(defun haskell-indent-check (source &rest test-cases)
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
        (let ((haskell-indent-offset 2))
          (haskell-mode)
          (haskell-indent-mode 1)
          (insert source)
          (newline)
          (font-lock-fontify-buffer)
          (goto-char (point-min))
          (forward-line (1- line))
          (should
           (equal current
                  (cons (line-number-at-pos)
                        (condition-case condition
                            (reverse (mapcar #'haskell-indent-format-info (haskell-indent-indentation-info)))
                          (error
                           ;; for unknown reason Emacs 24.4 ERT does not
                           ;; catch overrun recursion, so we have to
                           ;; catch it here, and throw it again
                           (signal (car condition) (cdr condition))))))))))))

(defmacro hindent1-test (name source &rest test-cases)
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
                     (concat "haskell-indent-check-"
                             (remove ?* raw-prefix)))
                    (cl-find ?* raw-prefix)
                    (substring name (1+ split-pos))))
          (list 'haskell-indent-check-fixme
                nil
                name))
      `(ert-deftest ,test-name ()
         ,doc-string
         :expected-result
         ,(if allow-failure :failed :passed)
         (haskell-indent-check
          ,(string-trim-left source)
          ,@(mapcar (lambda (x)
                      (list 'quote x))
                    test-cases))))))

(hindent1-test "1 Check if '{' on its own line gets properly indented""
function = Record
       { field = 123 }"
              (1 0)
              (2 0 11))

(hindent1-test "2* Handle underscore in identifiers""
function = do
  (_x) <- return ()
 z"
              (1 0)
              (2 2)
              (3 0 2 4))

(hindent1-test "2u* Handle underscore in identifiers""
function = do
  (_x) ← return ()
 z"
              (1 0)
              (2 2)
              (3 0 2 4))

(hindent1-test "2a* Handle apostrophe in identifiers""
function = do
  (_'x') <- return ()
 z"
              (1 0)
              (2 2)
              (3 0 2 4))

(hindent1-test "2au* Handle apostrophe in identifiers""
function = do
  (_'x') ← return ()
 z"
              (1 0)
              (2 2)
              (3 0 2 4))

(hindent1-test "3* Import statememnt symbol list 1""
import Control.Concurrent
       ( forkIO,
         killThread )"
              (1 0)
              (2 0 7)
              (3 9)
              (4 0 7))

(hindent1-test "4* Import statememnt symbol list 2""
import Control.Concurrent
       ( forkIO
       , killThread )"
              (1 0)
              (2 0 7)
              (3 7)
              (4 0 7))

(hindent1-test "5* List comprehension""
fun = [ x | y
          , z ]"
              (1 0)
              (2 10)
              (3 0 2))

(hindent1-test "5a* List comprehension""
fun = [ x | y,
            z ]"
              (1 0)
              (2 12)
              (3 0 6))

(hindent1-test "6* \"let\" in list comprehension""
fun = [ f | x <- xs
          , y <- ys
          , let c = 123
          , f <- fx x y c ]"
              (1 0)
              (2 10)
              (3 10)
              (4 10)
              (5 0 6))

(hindent1-test "6u* \"let\" in list comprehension""
fun = [ f | x ← xs
          , y ← ys
          , let c = 123
          , f ← fx x y c ]"
              (1 0)
              (2 10)
              (3 10)
              (4 10)
              (5 0))

(hindent1-test "6b* \"let\" in do""
fact n = do
  let g = 7
  z <- let x = 5
       in return (x + 4)"
              (1 0)
              (2 2)
              (3 2 6 8)
              (4 7)
              (5 2 10))


(hindent1-test "7a* \"data\" after \"data\"""
data ABC = ABC
data DEF = DEF"
              (1 0)
              (2 0))

(hindent1-test "7* \"import\" after \"import\"""
import ABC
import DEF"
              (1 0)
              (2 0)
              (3 0 7))

(hindent1-test "7b* declaration after declaration" "
fun1 = undefined
fun2 = undefined"
              (1 0)
              (2 0))

(hindent1-test "8* Guards in function definition""
resolve (amount, max) number
  | number > max = (1, number)
  | number == max = (amount + 1, number)"
              (1 0)
              (2 2)
              (3 0 2)
              (4 0 2))

(hindent1-test "9* Operator last on line""
fun = x ++"
              (1 0)
              (2 6))

(hindent1-test "10 Operator first on line""
fun = x
      ++ z"
              (1 0)
              (2 0 6))

(hindent1-test "11* Guards with commas""
clunky env var1 var2
  | Just val1 <- lookup env var1
  , Just val2 <- lookup env var2"
              (1 0)
              (2 2)
              (3 2)
              (4 0 4))

(hindent1-test "11u* Guards with commas""
clunky env var1 var2
  | Just val1 ← lookup env var1
  , Just val2 ← lookup env var2"
              (1 0)
              (2 2)
              (3 2)
              (4 0 4))

(hindent1-test "12* \"do\" as expression""
fun = do { putStrLn \"X\";
         }"
              (1 0)
              (2 9 11)
              (3 0))

(hindent1-test "13* Don't indent after deriving""
data X = X
  deriving (Eq, Ord, Show)"
              (1 0)
              (2 0 2)
              (3 0))

(hindent1-test "13b honour = on a separate line in data declaration" "
data X a b
  = X"
              (1 0)
              (2 2))

(hindent1-test "14* Line starting with operator inside \"do\" needs to be indented""
fun = do
  pure X
  something
    <*> marg"
              (1 0)
              (2 2)
              (3 0 2)
              (4 4))

(hindent1-test "15* An \"if..then\" inside a \"do\" block""
fun = do
  if x
  then do
    putStrLn \"True\""
              (1 0)
              (2 2)
              (3 2)
              (4 4)
              (5 2 4))

(hindent1-test "16 Lambda and a \"do\" block""
fun = \\x -> do"
              (1 0)
              (2 8))

(hindent1-test "16a* A lambda""
fun = \\x ->"
              (1 0)
              (2 2))

(hindent1-test "16u Lambda and a do block""
fun = \\x → do"
              (1 0)
              (2 8))

(hindent1-test "16au* A lambda""
fun = \\x →"
              (1 0)
              (2 8))

(hindent1-test "17a* A type for a function""
fun :: Int
    -> Int"
              (1 0)
              (2 4)
              (3 0 4))

(hindent1-test "17au* A type for a function""
fun :: Int
    → Int"
              (1 0)
              (2 4)
              (3 0 4))

(hindent1-test "17b* A type for a function with context""
fun :: Monad m
    => Int"
              (1 0)
              (2 4)
              (3 0 4))

(hindent1-test "17bu* A type for a function with context""
fun ∷ Monad m
    ⇒ Int"
              (1 0)
              (2 4)
              (3 0 4))

(hindent1-test "17c* A type for a function with complicated context""
fun :: (Monad m, MonadBaseControl IO m, MyMonad (A v) m)
    => MyMonad (A v) m"
              (1 0)
              (2 4)
              (3 0 4))

(hindent1-test "17cu* A type for a function with complicated context""
fun ∷ (Monad m, MonadBaseControl IO m, MyMonad (A v) m)
    ⇒ MyMonad (A v) m"
              (1 0)
              (2 4)
              (3 0 4))

(hindent1-test "17d* A type for a function with param and a complicated context""
fun :: (Monad m, MonadBaseControl IO m, MyMonad (A v) m)
    => MyMonad (A v) m
    -> m (Maybe a)"
              (1 0)
              (2 4)
              (3 4)
              (4 0 4))

(hindent1-test "17du* A type for a function with param and a complicated context""
fun ∷ (Monad m, MonadBaseControl IO m, MyMonad (A v) m)
    ⇒ MyMonad (A v) m
    → m (Maybe a)"
              (1 0)
              (2 4)
              (3 4)
              (4 0 4))

(hindent1-test "18a* \"if-then-else\" indentation: \"then\"""
x = if flag
    then 1"
              (1 0)
              (2 4)
              (3 4 9))

(hindent1-test "18b* \"if-then-else\" indentation: \"else\"""
x = if flag
    then 1
    else 0"
              (1 0)
              (2 4)
              (3 4)
              (4 0 9))

(hindent1-test "18c* \"do\" and \"if-then-else\" indentation: \"then\"""
x = do
  if flag
  then 1"
              (1 0)
              (2 2)
              (3 2)
              (4 2))

(hindent1-test "18d* \"do\" and \"if-then-else\" indentation: \"else\"""
x = do
  if flag
  then 1
  else 0"
              (1 0)
              (2 2)
              (3 2)
              (4 2))

(hindent1-test "18e* \"do\" and \"if-then-else\" indentation: \"else\"""
x = do
  if flag
  then do
    return ()"
              (1 0)
              (2 2)
              (3 2)
              (4 4)
              (5 2 4))

(hindent1-test "18f* \"do\" and \"if-then-else\" indentation: \"else\"""
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

(hindent1-test "19a* \"let\" and \"in\"""
x = let
  y"
              (1 0)
              (2 2)
              (3 2))

(hindent1-test "19b* \"let\" and \"in\"" "
x = let y
    in
      z"
              (1 0)
              (2 4)
              (3 6))

(hindent1-test "19c* \"let\" in a \"do\"""
x = do
  thing
  let
    z = 5"
              (1 0)
              (2 2)
              (3 2)
              (4 4))

(hindent1-test "20a* \"instance\" declaration""
instance C a where
  c = undefined"
              (1 0)
              (2 2)
              (3 0 2))

(hindent1-test "20b* \"instance\" declaration""
instance (Monad m) => C m a where
  c = undefined"
              (1 0)
              (2 2)
              (3 0 2))

(hindent1-test "20bu* \"instance\" declaration""
instance (Monad m) ⇒ C m a where
  c = undefined"
              (1 0)
              (2 2)
              (3 0 2))

(hindent1-test "21a* fix \"let\" statement in \"do\" block""
main :: IO ()
main = do
let foo = Foo {
      bar = 0
      , baz = 0"
              (1 0)
              (2 0)
              (3 2)
              (4 6)
              (5 6)
              (6 8))

(hindent1-test "21b* fix named fields in \"data\" declaration""
data Foo = Foo {
  bar :: Int
  , baz :: Int"
              (1 0)
              (2 4)
              (3 2)
              (4 11))

(hindent1-test "21c* \"data\" declaration open on next line" "
data Foo = Foo
  { bar :: Int
  , baz :: Int"
              (1 0)
              (2 2)
              (3 2)
              (4 4 11))

(hindent1-test "22* should obey layout only outside parentheses" "
func = 1234
  where
    foo :: Ivory eff ()
    foo = do
      return ()"
              (1 0)
              (2 2)
              (3 4)
              (4 0 4 11)
              (5 6))

(hindent1-test "23* should not fail when seeing comments" "
-- important non-empty line
{-
-}"
              ((3 2) 0))

(hindent1-test "24* should parse inline type signatures properly" "
foo = do
  _ :: String <- undefined
  _ :: String <- undefined
  return ()"
              (1 0)
              (2 2)
              (3 0 2 4)
              (4 0 2 4))

(hindent1-test "25a* support scoped type declarations" "
foo = do
  bar :: String
      -> String
    <- undefined"
              (1 0)
              (2 2)
              (3 6 9)
              ;; here it brakes, it would like to put '<-' on same line with 'bar'
              ;; the culprit is the 'do' keyword
              (4 4))

(hindent1-test "25b* support scoped type declarations" "
foo = let
  bar :: String
      -> String
    = undefined"
              (1 0)
              (2 2)
              (3 6 9)
              (4 4))

(hindent1-test "26 should parse unindented where-clause properly" "
foo = do
    return ()
  where
    bar = undefined"
              (4 4))

(hindent1-test "27* expecting then (GH-884)" "
foo = do
    if True
    then return ()
"
              (4 4))

(hindent1-test "28a names starting with quotes" "
f = a (a 'A)
    (a 'A)
"
              (2 0 4))

(hindent1-test "28b character literal (escape sequence)" "
f = '\\\\'

"
              (2 0 4))


(hindent1-test "28c name starting with a quote" "
function (Operation 'Init) = do
  print 'Init
"
              (2 2))

(hindent1-test "29a quasiquote single line" "
test = [randomQQ| This is a quasiquote with the word in |]

"
              (2 0 7))

(hindent1-test "29b* quasiquote multiple lines" "
test = [randomQQ| This is
          a quasiquote
          with the word in |]

"
              (4 0 2))

(hindent1-test "30* parse '[] identifier correctly" "
instance Callable '[]
"
              (1 2))

(hindent1-test "31* allow type class declaration without methods" "
class Foo a where
instance Bar Int
"
              (2 0))

(hindent1-test "32* allow type operators" "
data (:.) a b = a :. b
"
              (2 0 2))

(hindent1-test "33* parse #else in CPP" "
#ifdef FLAG
foo = ()
#else
"
              (4 0))


(hindent1-test "34 beginning of line inside parentheses" "
data T = T {
  foo :: String
, bar :: String
}

"
              (5 0))

(hindent1-test "35* baroque construct which causes parse error" "
az = Projection
  { unproject = do
        case x of
          _ -> return
  , maxR = pi
  }
"
              (6 2))

(hindent1-test "35a* parse a backslash properly" "
az = Projection
  { unproject = \\x -> do
        case x of
          _ -> return
  , maxR = pi
  }
"
              (6 2))

(hindent1-test "36* yet another parser failure" "
tokOpenTag =
  asum [ do void
       , return
       ]
"
              (4 7))

(hindent1-test "37* Indent continuation lines in multiline string literal" "
a = \"multiline\\
"
              (2 4))

(hindent1-test "38* Indent in do block after multiline string literal" "
s = do
  a <- \"multiline\\
       \\ line 2\"
"
              (4 0 2 4))

(hindent1-test "39* do not crash after two multiline literals in do block" "
servePost = do
  a <- fun \"line 1\\
           \\line 2\"
  b <- queryT \"comma is important: , \\
             \\ line 2 \"
"
              (6 0 2 4))

(hindent1-test "40* parse error in multiline tuple" "
a = ( 1
, "
              (2 4)
              (3 2))

(hindent1-test "41 open do inside a list" "
x = asum [ withX $ do
             return ()
         ]
"
              (2 13))

(hindent1-test "42 open do inside a list second element" "
x = asum [ mzero
         , withX $ do
             return ()
         ]
"
              (3 13))

(hindent1-test "43 open do inside a list second element, reset alignment" "
x = asum [ mzero
             , withX $ do
                 return ()
         ]
"
              (3 15))

(hindent1-test "44 expression continues, reset alignment" "
function = abc
       def
       xyz"
              (3 0 7))

(hindent1-test "46* case expression with paths on their own lines" "
fact n =
  case n of
    0 -> 1
    _ -> n * fact (n - 1)
"
              (1 0)
              (2 2)
              (3 4)
              (4 0 2 4 6)
              (5 0 2 4 6))

(hindent1-test "46b* case expression with guards" "
fact n = case n of
  n | n == 0 -> 1
  _ | n > 0
    , True == True -> n * fact (n - 1)"
              (1 0)
              (2 2)
              ;; returns (0 2 2 6), to investigate
              (3 0 2 6)
              (4 4)
              (5 0 2 6))

(hindent1-test "47a* multiline strings" "
fact n = \"\\
         \\a\""
              (1 0)
              ;; we want to offer both a continuation style and the
              ;; align to left column style (like in lisp)
              (2 0 9)
              (3 0 2))

(hindent1-test "47b* multiline strings" "
fact n = \"\\
      \\a\\
      \\x\""
              ;; here we want to keep third line like the second one,
              ;; although the second one wasn't best indented
              (1 0)
              (2 0 9)
              (3 6))

(hindent1-test "48* functional dependencies" "
class X a b | a -> b
            , b -> a where
  fun :: a -> b"
              (1 0)
              (2 12)
              (3 2)
              (4 0 2 9))

(hindent1-test "49* data with GADT syntax" "
data Term a where
  Lit :: Int -> Term Int
  Pair :: Term a -> Term b -> Term (a,b)"
              (1 0)
              (2 2)
              (3 0 2 9)
              (4 0 2 10))

(hindent1-test "49b* data with GADT syntax and a deriving clause" "
data G [a] b where
  G1 :: c -> G [Int] b
  deriving (Eq)"
              (1 0)
              (2 2)
              (3 0 2))

(hindent1-test "50* standalone deriving" "
data Name = Name String
deriving instance Eq Name"
              (1 0)
              ;; We accept position 2 here because we have just one
              ;; look-ahead token so we do not see 'instance'
              ;; following 'deriving'.
              (2 0 2))

(hindent1-test "51* standalone deriving" "
data family T a
data    instance T Int  = T1 Int | T2 Bool
newtype instance T Char = TC Bool"
              ;; We check that indentation does not bail on 'instance'
              ;; here, we do not really check if it is working
              ;; correctly. Needs better test.
              (1 0)
              (2 0)
              (3 0)
              (4 0 2))

(hindent1-test "52a* module simplest case two lines" "
module A.B
where"
              (1 0)
              (2 0)
              (3 0))

(hindent1-test "52b* module simplest case one line" "
module A.B where"
              (1 0)
              (2 0))

(hindent1-test "52c* module with exports" "
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

(hindent1-test "53* multiway if" "
fun = if | guard1 -> expr1
         | guardN -> exprN"
              (1 0)
              (2 9)
              (3 0 11))

(hindent1-test "54* equal after guards on separate line" "
foo x
  | True
  = X"
              (1 0)
              (2 2)
              (3 2))
