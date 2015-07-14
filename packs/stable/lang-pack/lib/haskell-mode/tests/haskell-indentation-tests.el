(require 'ert)
(require 'haskell-indentation)
(require 'haskell-mode)
(require 'cl-lib)


(defun haskell-indentation-check (&rest lines)
  "Check if `haskell-indentation-find-indentations` returns expected list of positions.

LINES is the lines to insert to a temp buffer. Last line is not
inserted, it is treated as a pattern of indentation points marked
by '^' characters in positions. Point will be set on the *last*
line inserted in the buffer so if you need to test indentation
*past* source code on empty line then an empty line must be
specified.

Example of lines:

\"func = do\"
\"      x\"
\"  ^\"
"
  (with-temp-buffer
    (haskell-mode)
    (haskell-indentation-mode)

    (let (expected
          indentations
          (result ""))
      (dolist (line lines)
        (when expected
          (insert expected)
          (insert "\n"))
        (setq expected line))

      (forward-line -1)
      (setq indentations (haskell-indentation-find-indentations))

      (dotimes (i (1+ (apply #'max indentations)))
        (setq result (concat result (if (memq i indentations)
                                        "^" " "))))

      (should (equal expected result)))))


(ert-deftest haskell-indentation-check-1 ()
  "Check if '{' on its own line gets properly indented"
  (haskell-indentation-check
   "function = Record"
   "       { field = 123 }"
   "^          ^"))

(ert-deftest haskell-indentation-check-2 ()
  "Handle underscore in identifiers"
  (haskell-indentation-check
   "function = do"
   "  (_x) <- return ()"
   " z"
   "^ ^       ^"))

(ert-deftest haskell-indentation-check-2-unicode ()
  :expected-result :failed
  "Handle underscore in identifiers (unicode)"
  (haskell-indentation-check
   "function = do"
   "  (_x) ← return ()"
   " z"
   "^ ^       ^"))

(ert-deftest haskell-indentation-check-2a ()
  "Handle apostrophe in identifiers"
  (haskell-indentation-check
   "function = do"
   "  (_'x') <- return ()"
   " z"
   "^ ^         ^"))

(ert-deftest haskell-indentation-check-2a-unicode ()
  "Handle apostrophe in identifiers (unicode)"
  :expected-result :failed
  (haskell-indentation-check
   "function = do"
   "  (_'x') ← return ()"
   " z"
   "^ ^         ^"))

(ert-deftest haskell-indentation-check-3 ()
  "Import statememnt symbol list 1"
  (haskell-indentation-check
   "import Control.Concurrent"
   "  ( forkIO,"
   "    killThread)"
   "    ^"))

(ert-deftest haskell-indentation-check-4 ()
  "Import statememnt symbol list 2"
  (haskell-indentation-check
   "import Control.Concurrent"
   "  ( forkIO"
   "  , killThread)"
   "  ^"))

(ert-deftest haskell-indentation-check-5 ()
  "List comprehension"
  (haskell-indentation-check
   "fun = [ x | y"
   "          , z ]"
   "          ^"))

(ert-deftest haskell-indentation-check-5a ()
  "List comprehension"
  :expected-result :failed
  (haskell-indentation-check
   "fun = [ x | y,"
   "            z ]"
   "            ^"))

(ert-deftest haskell-indentation-check-6 ()
  "let in list comprehension"
  :expected-result :failed
  (haskell-indentation-check
   "fun = [ f | x <- xs"
   "          , y <- ys"
   "          , let c = 123"
   "          , f <- fx x y c ]"
   "          ^"))

(ert-deftest haskell-indentation-check-6-unicode ()
  "let in list comprehension (unicode)"
  :expected-result :failed
  (haskell-indentation-check
   "fun = [ f | x ← xs"
   "          , y ← ys"
   "          , let c = 123"
   "          , f ← fx x y c ]"
   "          ^"))

(ert-deftest haskell-indentation-check-7 ()
  "import after import"
  :expected-result :failed
  (haskell-indentation-check
   "import ABC"
   "import DEF"
   "^"))

(ert-deftest haskell-indentation-check-8 ()
  "Guards in function definition"
  (haskell-indentation-check
   "resolve (amount, max) number"
   "  | number > max = (1, number)"
   "  | number == max = (amount + 1, number)"
   "  ^"))

(ert-deftest haskell-indentation-check-9 ()
  "Operator last on line"
  :expected-result :failed
  (haskell-indentation-check
   "fun = x ++"
   "      ^"))

(ert-deftest haskell-indentation-check-10 ()
  "Operator first on line"
  :expected-result :failed
  (haskell-indentation-check
   "fun = x"
   "      ++ z"
   "      ^"))

(ert-deftest haskell-indentation-check-11 ()
  "Guards with commas"
  (haskell-indentation-check
   "clunky env var1 var2"
   "  | Just val1 <- lookup env var1"
   "  , Just val2 <- lookup env var2"
   "  ^"))

(ert-deftest haskell-indentation-check-11-unicode ()
  :expected-result :failed
  "Guards with commas (unicode)"
  (haskell-indentation-check
   "clunky env var1 var2"
   "  | Just val1 ← lookup env var1"
   "  , Just val2 ← lookup env var2"
   "  ^"))

(ert-deftest haskell-indentation-check-12 ()
  "Guards with commas"
  :expected-result :failed
  (haskell-indentation-check
   "fun = do { putStrLn \"X\";"
   "         }"
   "         ^"))

(ert-deftest haskell-indentation-check-13 ()
  "Don't indent after deriving"
  :expected-result :failed
  (haskell-indentation-check
   "data X = X"
   "  deriving (Eq, Ord, Show)"
   "^"))

(ert-deftest haskell-indentation-check-14 ()
  "Line starting with operator inside a 'do' needs to be indented"
  :expected-result :failed
  (haskell-indentation-check
   "fun = do"
   "  pure X"
   "    <*> marg"
   "    ^"))

(ert-deftest haskell-indentation-check-15 ()
  "An if..then inside a do block"
  :expected-result :failed
  (haskell-indentation-check
   "fun = do"
   "  if x"
   "    then do"
   "      putStrLn \"True\""
   "      ^"))

(ert-deftest haskell-indentation-check-16 ()
  "Lambda and a do block"
  :expected-result :failed
  (haskell-indentation-check
   "fun = \x -> do"
   "  ^"))

(ert-deftest haskell-indentation-check-16-unicode ()
  "Lambda and a do block (unicode)"
  :expected-result :failed
  (haskell-indentation-check
   "fun = \x → do"
   "  ^"))

(ert-deftest haskell-indentation-check-16a ()
  "A lambda"
  :expected-result :failed
  (haskell-indentation-check
   "fun = \x ->"
   "  ^"))

(ert-deftest haskell-indentation-check-16a-unicode ()
  "A lambda (unicode)"
  :expected-result :failed
  (haskell-indentation-check
   "fun = \x →"
   "  ^"))

(ert-deftest haskell-indentation-check-17a ()
  "A type for a function"
  :expected-result :failed
  (haskell-indentation-check
   "fun :: Int"
   "    -> Int"
   "    ^"))

(ert-deftest haskell-indentation-check-17b ()
  "A type for a function with context"
  :expected-result :failed
  (haskell-indentation-check
   "fun :: Monad m"
   "    => Int"
   "    ^"))

(ert-deftest haskell-indentation-check-17b-unicode ()
  "A type for a function with context (unicode)"
  :expected-result :failed
  (haskell-indentation-check
   "fun ∷ Monad m"
   "    ⇒ Int"
   "    ^"))

(ert-deftest haskell-indentation-check-17c ()
  "A type for a function with complicated context"
  :expected-result :failed
  (haskell-indentation-check
   "fun :: (Monad m, MonadBaseControl IO m, MyMonad (A v) m)"
   "    => MyMonad (A v) m"
   "    ^"))

(ert-deftest haskell-indentation-check-17c-unicode ()
  "A type for a function with complicated context (unicode)"
  :expected-result :failed
  (haskell-indentation-check
   "fun ∷ (Monad m, MonadBaseControl IO m, MyMonad (A v) m)"
   "    ⇒ MyMonad (A v) m"
   "    ^"))

(ert-deftest haskell-indentation-check-17d ()
  "A type for a function with param and a complicated context"
  :expected-result :failed
  (haskell-indentation-check
   "fun :: (Monad m, MonadBaseControl IO m, MyMonad (A v) m)"
   "    => MyMonad (A v) m"
   "    -> m (Maybe a)"
   "    ^"))

(ert-deftest haskell-indentation-check-17d-unicode ()
  "A type for a function with param and a complicated context (unicode)"
  :expected-result :failed
  (haskell-indentation-check
   "fun ∷ (Monad m, MonadBaseControl IO m, MyMonad (A v) m)"
   "    ⇒ MyMonad (A v) m"
   "    → m (Maybe a)"
   "    ^"))

(ert-deftest haskell-indentation-check-18a ()
  "if then else indentation: then"
  (haskell-indentation-check
   "x = if flag"
   "    then 1"
   "    ^"))

(ert-deftest haskell-indentation-check-18b ()
  "if then else indentation: else"
  (haskell-indentation-check
   "x = if flag"
   "    then 1"
   "    else 0"
   "    ^"))

(ert-deftest haskell-indentation-check-18c ()
  "do and if then else indentation: then"
  (haskell-indentation-check
   "x = do"
   "  if flag"
   "    then 1"
   "    ^"))

(ert-deftest haskell-indentation-check-18d ()
  "do and if then else indentation: else"
  (haskell-indentation-check
   "x = do"
   "  if flag"
   "    then 1"
   "    else 0"
   "    ^"))

(ert-deftest haskell-indentation-check-18e ()
  "do and if then else indentation: else"
  :expected-result :failed
  (haskell-indentation-check
   "x = do"
   "  if flag"
   "    then do"
   "      return ()"
   "      ^"))

(ert-deftest haskell-indentation-check-18f ()
  "do and if then else indentation: else"
  :expected-result :failed
  (haskell-indentation-check
   "x = do"
   "  if flag"
   "    then do"
   "      return ()"
   "    else do"
   "      return ()"
   "      ^"))

(ert-deftest haskell-indentation-check-19a ()
  "let and in"
  (haskell-indentation-check
   "x = let"
   "  y"
   "  ^"))

(ert-deftest haskell-indentation-check-19b ()
  "let and in"
  (haskell-indentation-check
   "x = let y"
   "    in "
   "      z "
   "      ^"))

(ert-deftest haskell-indentation-check-19c ()
  "let in a do"
  (haskell-indentation-check
   "x = do"
   "  thing"
   "  let "
   "    z = 5"
   "    ^"))

(ert-deftest haskell-indentation-check-instance-20a ()
  "instance declaration"
  (haskell-indentation-check
   "instance C a where"
   "  c = undefined"
   "  ^"))

(ert-deftest haskell-indentation-check-instance-20b ()
  "instance declaration"
  (haskell-indentation-check
   "instance (Monad m) => C m a where"
   "  c = undefined"
   "  ^"))

(ert-deftest haskell-indentation-check-instance-20b-unicode ()
  "instance declaration (unicode)"
  (haskell-indentation-check
   "instance (Monad m) ⇒ C m a where"
   "  c = undefined"
   "  ^"))

(ert-deftest haskell-indentation-check-instance-21a ()
  "layout versus comma in braces"
  (haskell-indentation-check
   "main :: IO ()"
   "main = do"
   "let foo = Foo {"
   "      bar = 0"
   "      , baz = 0"
   "      ^"))

(ert-deftest haskell-indentation-check-instance-21a-unicode ()
  "layout versus comma in braces (unicode)"
  (haskell-indentation-check
   "main ∷ IO ()"
   "main = do"
   "let foo = Foo {"
   "      bar = 0"
   "      , baz = 0"
   "      ^"))
