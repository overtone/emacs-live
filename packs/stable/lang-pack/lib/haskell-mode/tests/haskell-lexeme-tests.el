;; unit tests for haskell-string.el

(require 'ert)
(require 'haskell-lexeme) ;; implementation under test
(require 'haskell-mode)
(require 'haskell-font-lock)

(defun check-lexemes (lines-or-contents lexemes &optional literate)
  "Checks if tokenization works as expected.

LINES is a list of strings that will be inserted to a new
buffer. Then LEXEMES is a list of lexemes that should be found in
order."
  (when (get-buffer "*haskell-mode-buffer*")
    (kill-buffer "*haskell-mode-buffer*"))
  (save-current-buffer
    (set-buffer (get-buffer-create "*haskell-mode-buffer*"))

    ;; Note that all of this should work both in haskell-mode and
    ;; outside of it. Currently we test only haskell-mode setup.
    (if literate
        (literate-haskell-mode)
      (haskell-mode))

    (if (consp lines-or-contents)
        (dolist (line lines-or-contents)
          (insert line)
          (insert "\n"))
      (insert lines-or-contents))

    (font-lock-fontify-buffer)

    (goto-char (point-min))
    (let (current-token
          (left-lexemes lexemes))
      (while (haskell-lexeme-looking-at-token)
        (setq current-token (match-string-no-properties 0))
        ;; it should be in the list of lexemes at all
        (should (member current-token lexemes))
        ;; it should be next in the list
        (should (equal (car left-lexemes) current-token))
        (setq left-lexemes (cdr left-lexemes))
        (goto-char (match-end 0)))
      (should (equal nil left-lexemes)))))

(ert-deftest haskell-lexeme-classify-chars-1 ()
  (should (equal 'varsym (haskell-lexeme-classify-by-first-char ?=)))
  (should (equal 'conid (haskell-lexeme-classify-by-first-char ?L)))
  (should (equal 'consym (haskell-lexeme-classify-by-first-char ?:)))
  (should (equal 'varid (haskell-lexeme-classify-by-first-char ?_)))
  (should (equal 'varid (haskell-lexeme-classify-by-first-char ?x)))
  (should (equal 'char (haskell-lexeme-classify-by-first-char ?')))
  (should (equal 'string (haskell-lexeme-classify-by-first-char ?\")))
  (should (equal 'special (haskell-lexeme-classify-by-first-char ?\;)))
  (should (equal 'number (haskell-lexeme-classify-by-first-char ?4))))

(ert-deftest haskell-lexeme-basic-tokens-1 ()
  "Get some basic self delimiting tokens right"
  (check-lexemes
   '(")(}{][,;;")
   '(")" "(" "}" "{" "]" "[" "," ";" ";")))

(ert-deftest haskell-lexeme-qid-1 ()
  "Indentifiers"
  (check-lexemes
   '("head,at_first,safeHead;Data")
   '("head" "," "at_first" "," "safeHead" ";" "Data")))

(ert-deftest haskell-lexeme-qid-2 ()
  "Operators (symbols)"
  (check-lexemes
   '(">>=,---->,<-;::::")
   '(">>=" "," "---->" "," "<-" ";" "::::")))

(ert-deftest haskell-lexeme-qid-3 ()
  "Qualified Indentifiers"
  (check-lexemes
   '("Data.List.head,Modu.at_first,Zonk.safeHead;Data.Data")
   '("Data.List.head" "," "Modu.at_first" "," "Zonk.safeHead" ";" "Data.Data")))

(ert-deftest haskell-lexeme-qid-4 ()
  "Qualified Operators (symbols)"
  (check-lexemes
   '("Monad.>>=,Comment.---->,Func.<-;Cons.::::;Category..")
   '("Monad.>>=" "," "Comment.---->" "," "Func.<-" ";" "Cons.::::" ";" "Category..")))

(ert-deftest haskell-lexeme-unicode-ids-1 ()
  "Unicode ids"
  (check-lexemes
   '("Żółw.head,Data.żółw,Артур.Артур ")
   '("Żółw.head" "," "Data.żółw" "," "Артур.Артур")))

(ert-deftest haskell-lexeme-unicode-ids-2 ()
  "Unicode ids, unicode as last character in line"
  ;;:expected-result :failed
  (check-lexemes
   '("Żółw.head,Data.żółw,Артур.Артур")
   '("Żółw.head" "," "Data.żółw" "," "Артур.Артур")))

(ert-deftest haskell-lexeme-unicode-syms-1 ()
  "Unicode symbols"
  (check-lexemes
   '("∷∷,.→,Control.Monad.★★")
   '("∷∷" "," ".→" "," "Control.Monad.★★")))


(ert-deftest haskell-lexeme-spaces ()
  (check-lexemes
   '("fun::C.Monad a -> ()"
     "fun=do { xyz <- abc <*> def; xyz }")
   '("fun" "::" "C.Monad" "a" "->" "(" ")"
     "fun" "=" "do" "{" "xyz" "<-" "abc" "<*>" "def" ";" "xyz" "}")))

(ert-deftest haskell-lexeme-japanese-is-treated-as-lowercase ()
  (check-lexemes
   '("てすと3 ∷ IO ()"
     "てすと3 = do"
     "    putStrLn $ show 人間虫 where"
     "        人間虫 = x123")
   '("てすと3" "∷" "IO" "(" ")"
     "てすと3" "=" "do"
     "putStrLn" "$" "show" "人間虫" "where"
     "人間虫" "=" "x123")))

(ert-deftest haskell-lexeme-modifier-letters ()
  (check-lexemes
   '("xᵦ xᵦxᵦ xxx###")
   '("xᵦ" "xᵦxᵦ" "xxx###")))

(ert-deftest haskell-lexeme-char-literal-1 ()
  (check-lexemes
   '("'\\ENQ'")
   '("'\\ENQ'")))

(ert-deftest haskell-lexeme-char-literal-2 ()
  (check-lexemes
   '("'\\''")
   '("'\\''")))

(ert-deftest haskell-lexeme-char-literal-3 ()
  (check-lexemes
   '("'\"'")
   '("'\"'")))

(ert-deftest haskell-lexeme-char-literal-4 ()
  (check-lexemes
   '("'D'")
   '("'D'")))

(ert-deftest haskell-lexeme-string-literal-1 ()
  (check-lexemes
   '("\"\\   \\\"")
   '("\"\\   \\\"")))

(ert-deftest haskell-lexeme-string-literal-1a ()
  (check-lexemes
   '("\"\\ \n  \\\"")
   '("\"\\ \n  \\\"")))

(ert-deftest haskell-lexeme-string-literal-2 ()
  (check-lexemes
   '("\"\"")
   '("\"\"")))

(ert-deftest haskell-lexeme-string-literal-3 ()
  (check-lexemes
   '("\"foobar\"")
   '("\"foobar\"")))

(ert-deftest haskell-lexeme-string-literal-4 ()
  (check-lexemes
   '("\"\\^Z\"")
   '("\"\\^Z\"")))

(ert-deftest haskell-lexeme-string-literal-5 ()
  (check-lexemes
   '("\"\\\\\"")
   '("\"\\\\\"")))

(ert-deftest haskell-lexeme-string-literal-6 ()
  (check-lexemes
   '("\"\\ENQ\"")
   '("\"\\ENQ\"")))

(ert-deftest haskell-lexeme-string-literal-7 ()
  (check-lexemes
   '("\"\\\\\"")
   '("\"\\\\\"")))

(ert-deftest haskell-lexeme-string-literal-8 ()
  (check-lexemes
   '("foo = \"zonk"
     "       Cons")
   '("foo" "=" "\"zonk"
     "Cons")))

(ert-deftest haskell-lexeme-line-comment-1 ()
  (check-lexemes
   '("   -- x  "
     " --%% cons"
     " -- cons"
     )
   '("-- x  "
     "--%%" "cons"
     "-- cons"
     )))

(ert-deftest haskell-lexeme-template-haskell-1 ()
  (check-lexemes
   '("  'C  ''C 'x ''x 0x12'x'xx")
   '("'" "C" "''" "C" "'" "x" "''" "x" "0x12" "'x'" "xx")))

(ert-deftest haskell-lexeme-decimal-numbers-1 ()
  (check-lexemes
   '("123+345-123412")
   '("123" "+" "345" "-" "123412")))

(ert-deftest haskell-lexeme-octal-hexadecimal-numbers-1 ()
  (check-lexemes
   '("0o123+0xaf345-0O121 0X234523fff")
   '("0o123" "+" "0xaf345" "-" "0O121" "0X234523fff")))

(ert-deftest haskell-lexeme-float-numbers-1 ()
  (check-lexemes
   '("0.12 34.22.33 1e+23 1e23 1e+33 455.33E1456.4")
   '("0.12" "34.22" "." "33" "1e+23" "1e23" "1e+33" "455.33E1456" "." "4")))

(ert-deftest haskell-lexeme-quasi-quote-1 ()
  (check-lexemes
   '("[xml| <xml /> |]")
   '("[xml| <xml /> |]")))

(ert-deftest haskell-lexeme-quasi-quote-2 ()
  (check-lexemes
   '("[xml| <xml /> |] |]")
   '("[xml| <xml /> |]" "|" "]")))

(ert-deftest haskell-lexeme-quasi-quote-3 ()
  (check-lexemes
   "[xml| <xml /> |"
   '("[xml| <xml /> |")))

(ert-deftest haskell-lexeme-quasi-quote-4 ()
  (check-lexemes
   "[xml| <xml />"
   '("[xml| <xml />")))

(ert-deftest haskell-lexeme-literate-1 ()
  (check-lexemes
   '("no code"
     "\\begin{code}"
     "code code"
     "\\end{code}"
     "no code no code")
   '("no code"
     "\\begin{code}"
     "code"
     "code"
     "\\end{code}"
     "no code no code")
   'literate))

(ert-deftest haskell-lexeme-literate-2 ()
  (check-lexemes
   '("no code"
     "> code code"
     "no code")
   '("no code"
     "code"
     "code"
     "no code")
   'literate))
