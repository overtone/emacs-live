;;  -*- lexical-binding: t -*-
(require 'ert)
(require 'haskell-test-utils)
(require 'haskell-font-lock)
(require 'haskell-mode)

(ert-deftest haskell-syntactic-test-1 ()
  "Simple keywords fontified"
  (check-properties
   '("module Test where")
   '(("module" "w" haskell-keyword-face)
     ("Test" "w" haskell-constructor-face)
     ("where" "w" haskell-keyword-face))))

(ert-deftest haskell-syntactic-test-4 ()
  "Apostrophe as part of a contructor token."
  :expected-result :failed
  (check-properties
   '("T_'ttt_'t_' T Tx T'x T_x T_'_")
   '(("T_'ttt_'t_'" "w" haskell-constructor-face)
     ("T" "w" haskell-constructor-face)
     ("T'x" "w" haskell-constructor-face)
     ("T_x" "w" haskell-constructor-face)
     ("T_'_" "w" haskell-constructor-face))))

(ert-deftest haskell-syntactic-test-5 ()
  "Apostrophe inside a token."
  :expected-result :failed
  (check-properties
   '("_T_'tt't_'t_' xxx'ff _f _'''")
   '(("_T_'tt't_'t_'" "w" haskell-symbol-face)
     ("xxx'ff" "w" haskell-symbol-face)
     ("_f" "w" haskell-symbol-face)
     ("_'''" "w" haskell-symbol-face))))

(ert-deftest haskell-syntactic-test-character-literal-escapes ()
  (check-properties
   '("'\\000' '\\x01'")
   '(("'\\000'" t font-lock-string-face)
     ("'\\x01'" t font-lock-string-face))))

(ert-deftest haskell-syntactic-test-7 ()
  "Take quotes and double quotes under control."
  (check-properties
   '("\"\'\" Cons1"
     "\'\"\' Cons2")
   '(("\"\'\"" t font-lock-string-face)
     ("Cons1" "w" haskell-constructor-face)
     ("\'\"\'" t font-lock-string-face)
     ("Cons2" "w" haskell-constructor-face))))

(ert-deftest haskell-syntactic-test-7b ()
  "Take quotes and double quotes under control."
  (check-properties
    ;; do not get fooled
   '("\"\'\"\'\"\'\"\'\"\'\"\'\"\'\"\'\"\' Cons")
   '(("Cons" "w" haskell-constructor-face))))

(ert-deftest haskell-syntactic-test-7c ()
  "Tripple backslash in a string that also has a quote."
  (check-properties
   ;; the below is: "\"\\\"" and \\\ get marked as punctuation because
   ;; of detecting -- that is a part of larger non-comment lexeme
   '("  \"\\\"\\\\\\\"\" Cons")
   '(("Cons" "w" haskell-constructor-face))))

(ert-deftest haskell-syntactic-test-8 ()
  "Check if gap-rule works."
  (check-properties
   '("\"\\  \\\\\\ \\  "
     "   \\\" Cons")
   '(("\\" "." t)               ; 1st is escape
     ("\\"  "." t)               ; 2nd is punctuation
     ("\\" "." t)               ; 3rd is escape
     ("\\"  "." t)               ; 4th is punctuation
     ("\\" "." t)               ; 5th is escape
     ("\\"  "." t)               ; 6th is punctuation
     ("Cons" "w" haskell-constructor-face))))

(ert-deftest haskell-syntactic-test-9 ()
  "Syntax for hierarchical modules."
  ; note that quite many things here are not consistent but for now
  ; this test describes what it is
  (check-properties
   '(" A.B.C"
     " D.E.f"
     " G.H.>>="
     " <=<"
     )
   '(("A" "w" haskell-constructor-face)
     ("." "." haskell-constructor-face)
     ("B" "w" haskell-constructor-face)
     ("." "." haskell-constructor-face)
     ("C" "w" haskell-constructor-face)

     ("D" "w" nil)
     ("." "." nil)
     ("E" "w" nil)
     ("." "." nil)
     ("f" "w" nil)

     ("G" "w" haskell-operator-face)
     ("." "." haskell-operator-face)
     ("H" "w" haskell-operator-face)
     ("." "." haskell-operator-face)
     (">>=" "." haskell-operator-face)

     ("<=<" "." haskell-operator-face))))

(ert-deftest haskell-syntactic-test-18 ()
  "Backtick operators"
  (check-properties
   '(" `fmap1`"
     " ` fmap2 `"
     " ` {- C1 -} M.fmap3 {- C2 -} `")
   '(("`" t haskell-operator-face)
     ("fmap1" t haskell-operator-face)
     ("`" t haskell-operator-face)

     ("`" t haskell-operator-face)
     ("fmap2" t haskell-operator-face)
     ("`" t haskell-operator-face)

     ("`" t haskell-operator-face)
     ("C1" t font-lock-comment-face)
     ("fmap3" t haskell-operator-face)
     ("C2" t font-lock-comment-face)
     ("`" t haskell-operator-face))))

(ert-deftest haskell-syntactic-test-18a-multiline ()
  "Backtick operators multiline"
  ;; strangely thins works in interactive session
  :expected-result :failed
  (check-properties
   '(" `"
     " fmap "
     "   `")
   '(("`" t haskell-operator-face)
     ("fmap" t haskell-operator-face)
     ("`" t haskell-operator-face))))

(ert-deftest haskell-syntactic-test-9a ()
  "Syntax for hierarchical modules when on the first line."
  ;; note that quite many things here are not consistent but for now
  ;; this test describes what it is. When on the first column
  ;; font-lock thins we are defining (.) operator. Not good.
  :expected-result :failed
  (check-properties
   '("A1.B.C"
     "A2.B.c"
     "A3.B.>>="
     "<=<"
     )
   '(("A1" "w" haskell-constructor-face)
     ("."  "." haskell-constructor-face)
     ("B"  "w" haskell-constructor-face)
     ("."  "." haskell-constructor-face)
     ("C"  "w" haskell-constructor-face)

     ("A2" "w" nil)
     ("."  "." nil)
     ("B"  "w" nil)
     ("."  "." nil)
     ("C"  "w" nil)

     ("A3" "w" haskell-constructor-face)
     ("."  "." haskell-constructor-face)
     ("B"  "w" haskell-constructor-face)
     ("."  "." haskell-operator-face)        ; this is wrong
     (">>="  "." haskell-operator-face)

     ("<=<"  "." haskell-operator-face))))


(ert-deftest haskell-syntactic-test-10 ()
  "Syntax for comments"
  (check-properties
   '(" Cons0 -- Comm1"
     " --\ Cons2"
     " ----- Comment3"
     " {- Comm4 -} -- Comm5"
     " -- \" Comm6"
     " Cons7"
     "{-# pragma1 #-}"
     "{-# non_pragma2 -}"
     "{- non_pragma3 #-}"
     "{-@ liquid_haskell @-}"
     "{-@ non_liquid_haskell_2 -}"
     "{- non_liquid_haskell_3 @-}"
     )
   '(("Cons0" "w" haskell-constructor-face)
     ("Comm1" "w" font-lock-comment-face)
     ;("Cons2" "w" haskell-constructor-face) -- works in real life, does not work in tests...
     ("Comment3" "w" font-lock-comment-face)
     ("Comm4"  "w" font-lock-comment-face)

     ("Comm5" "w" font-lock-comment-face)
     ("Comm6" "w" font-lock-comment-face)
     ("Cons7"  "w" haskell-constructor-face)
     ("pragma1"  "w" haskell-pragma-face)
     ("non_pragma2"  "w_" font-lock-comment-face)

     ("non_pragma3" "w_" font-lock-comment-face)
     ("liquid_haskell"  "w_" haskell-liquid-haskell-annotation-face)
     ("non_liquid_haskell_2"  "w_" font-lock-comment-face)

     ("non_liquid_haskell_3" "w_" font-lock-comment-face))))


(ert-deftest haskell-syntactic-string-vs-comment-escape ()
  "Check string escape vs comment escape"
  (check-properties
   ;; "\"" \--  Cons
   '("\"\\\"\" \\--  Cons")
   '(("\\--" "." haskell-operator-face)
     ("Cons" "w" haskell-constructor-face))))

(ert-deftest haskell-syntactic-quasiquote-no-escape ()
  "Check string escape vs comment escape"
  (check-properties
   '("[qq| \\|]  Cons")
   '(("qq" "w" nil)
     ("\\" "." haskell-quasi-quote-face)
     ("Cons" "w" haskell-constructor-face))))

(ert-deftest haskell-syntactic-quasiquote-three-punctuation ()
  "Check string escape vs comment escape"
  (check-properties
   '("[qq| %\\|]  Cons")
   '(("qq" "w" nil)
     ("%\\" "." haskell-quasi-quote-face)
     ("Cons" "w" haskell-constructor-face))))

(ert-deftest haskell-syntactic-quasiquote-constructor ()
  "Check string escape vs comment escape"
  (check-properties
   '("[Cons1| Cons2 |]")
   '(("Cons1" "w" haskell-constructor-face)
     ("Cons2" "w" haskell-constructor-face))))

(ert-deftest haskell-syntactic-quasiquote-number ()
  "Check string escape vs comment escape"
  (check-properties
   '("[11| Cons2 |]")
   '(("1" "w" nil)
     ("Cons2" "w" haskell-constructor-face))))

(ert-deftest haskell-haddock-01 ()
  "Syntax for haddock comments"
  (check-properties
   '(" -- | Dcom1"                      ; haddocks
     " -- $ Dcom2"
     " -- ^ Dcom3"
     " -- * Dcom4"
     " --| Cons5"                       ; non-haddocks, operators
     " --$ Cons6"
     " --^ Cons7"
     " --* Cons8"
     " --  | Com5"                      ; non-haddocks, comments
     " --  $ Com6"
     " --  ^ Com7"
     " --  * Com8"
     " {-| Dcom10 -}"                   ; haddocks
     " {-$ Dcom11 -}"
     " {-^ Dcom12 -}"
     " {-* Dcom13 -}"
     " {- | Dcom14 -}"                  ; also haddocks
     " {- $ Dcom15 -}"
     " {- ^ Dcom16 -}"
     " {- * Dcom17 -}")
   '(("Dcom1" "w" font-lock-doc-face)
     ("Dcom2" "w" font-lock-doc-face)
     ("Dcom3" "w" font-lock-doc-face)
     ("Dcom4" "w" font-lock-doc-face)
     ("Cons5" "w" haskell-constructor-face)
     ("Cons6" "w" haskell-constructor-face)
     ("Cons7" "w" haskell-constructor-face)
     ("Cons8" "w" haskell-constructor-face)
     ("Com5" "w" font-lock-comment-face)
     ("Com6" "w" font-lock-comment-face)
     ("Com7" "w" font-lock-comment-face)
     ("Com8" "w" font-lock-comment-face)
     ("Dcom10" "w" font-lock-doc-face)
     ("Dcom11" "w" font-lock-doc-face)
     ("Dcom12" "w" font-lock-doc-face)
     ("Dcom13" "w" font-lock-doc-face)
     ("Dcom14" "w" font-lock-doc-face)
     ("Dcom15" "w" font-lock-doc-face)
     ("Dcom16" "w" font-lock-doc-face)
     ("Dcom17" "w" font-lock-doc-face)
     )))

(ert-deftest haskell-haddock-02 ()
  "Syntax for italic and bold in haddock comments"

  ;; Emacs 23 does not have `add-face-text-property'
  :expected-result (if (fboundp 'add-face-text-property)
                       :passed
                     :failed)
  (check-properties
   '("-- | haddock"
     "-- /it1/ nonit2 /it3/"
     "-- /it\\/4/ nonit5"
     "-- __/boldit1/__"
     "-- /__boldit2__/"
     "-- __bold_bold_bold__"
     "-- __/unfinished")
   '(("/it1/" t ((:slant italic) font-lock-doc-face))
     ("nonit2" t font-lock-doc-face)
     ("/it3/" t ((:slant italic) font-lock-doc-face))
     ("/it\\/4/" t ((:slant italic) font-lock-doc-face))
     ("nonit5" t font-lock-doc-face)
     ("boldit1" t ((:weight bold) (:slant italic) font-lock-doc-face))
     ("boldit2" t ((:slant italic) (:weight bold) font-lock-doc-face))
     ("__bold_bold_bold__" t ((:weight bold) font-lock-doc-face))
     ("unfinished" t font-lock-doc-face))))

(ert-deftest haskell-syntactic-test-11b ()
  "Syntax for haddock comments"
  ;; Note: all of these are prefixed with space so that
  ;; top-level definition detection does not kick in.
  (check-properties
   '(" 'a''b'"                          ; ('a','b')
     " 12'c'"                           ; (12,'c')
     " 0x32'd'"                         ; (0x34,'d')
     " e56'f'"                          ; Not in scope: ‘e56'f'’
     " 'g''h''" ; lexical error in string/character literal at end of input
     " 'i'j "                           ; ('i',45)
     " 'k''l"                           ; ('k',l_1627393257)
     " \"m\"'n'"                        ; ("m",'n')
     " 'o'\"p\""                        ; ('o',"p")
     " '\"'\"'\""                       ; ('"',"'")
     " 7e-8'q'"                         ; (7e-8,'q')
     " 9.9'r'"                          ; (9.9,'r')
     " 's'12e-34"                       ; ('s',1.2e-33)
     " 't'56.78"                        ; ('t',56.78)
     " ."
     )
   '(("'a'" t font-lock-string-face)
     ("'b'" t font-lock-string-face)
     ("12" t nil)
     ("'c'" t font-lock-string-face)
     ("0x32" t nil)
     ("'d'" t font-lock-string-face)
     ("e56'f'" t nil)
     ("'g'" t font-lock-string-face)
     ("'h'" t font-lock-string-face)
     ("'" t nil)  ; ?? stray apostrophe is what??
     ("'i'" t font-lock-string-face)
     ("j" t nil)
     ("'k'" t font-lock-string-face)
     ("'l" t nil) ;; apostrophe here should be prefix operator
     ("\"m\"'n'" t font-lock-string-face)
     ("'o'\"p\"" t font-lock-string-face)
     ("'\"'\"'\"" t font-lock-string-face)
     ("7e-8" t nil)
     ("'q'" t font-lock-string-face)
     ("9.9" t nil)
     ("'r'" t font-lock-string-face)
     ("'s'" t font-lock-string-face)
     ("12e-34" t nil)
     ("'t'" t font-lock-string-face)
     ("56.78" t nil)
     ("." t haskell-operator-face)
     )))

(ert-deftest haskell-syntactic-preprocessor-01 ()
  (check-properties
   '("  # NoPreproc"
     "#preproc"
     "#prep1    \\"
     "    Prep2"
     "Cons")
   '(("NoPreproc" t haskell-constructor-face)
     ("preproc" t font-lock-preprocessor-face)
     ("prep1" t font-lock-preprocessor-face)
     ("Prep2" t font-lock-preprocessor-face)
     ("Cons" t haskell-constructor-face))))

(ert-deftest haskell-syntactic-preprocessor-02 ()
  (check-properties
   '"#preproc\\\ncont\\" ;; backslash is last char in buffer
   '(("preproc" t font-lock-preprocessor-face)
     ("cont" t font-lock-preprocessor-face)
     ("\\" t font-lock-preprocessor-face))))

(ert-deftest haskell-syntactic-test-quasiquoter-1 ()
  "Basic syntax for QuasiQuote"
  (check-properties
   '("v = [quoter| string |] Cons")
   '(("[" t nil)
     ("|" t haskell-quasi-quote-face)
     ("string" t haskell-quasi-quote-face)
     ("|" t haskell-quasi-quote-face)
     ("]" t nil)
     ("Cons" "w" haskell-constructor-face))))

(ert-deftest haskell-syntactic-test-quasiquoter-2 ()
  "Basic syntax for QuasiQuote multiline"
  (check-properties
   '("v = [quoter| string"
     " one more  | ]"
     " finishing line"
     "|] Cons")
   '(("[" t nil)
     ("|" t haskell-quasi-quote-face)
     ("string" t haskell-quasi-quote-face)
     ("line" t haskell-quasi-quote-face)
     ("|" t haskell-quasi-quote-face)
     ("]" t nil)
     ("Cons" "w" haskell-constructor-face))))

(ert-deftest haskell-syntactic-test-quasiquoter-3 ()
  "QuasiQuote inside quasi quote"
  (check-properties
   '("v = [quoter| [inner| string {- -- |] Outside |]")
   '(("[" t nil)
     ("|" t haskell-quasi-quote-face)
     ("inner" t haskell-quasi-quote-face)
     ("string" t haskell-quasi-quote-face)
     ("|" t haskell-quasi-quote-face)
     ("]" t nil)
     ("Outside" "w" haskell-constructor-face)
     )))

(ert-deftest haskell-syntactic-test-quasiquoter-4 ()
  "QuasiQuote inside comment"
  (check-properties
   '("v = -- [quoter| "
     "    [inner| string {- -- |] Outside1 |] Outside2")
   '(("quoter" t font-lock-comment-face)
     ("inner" t nil)
     ("string" t haskell-quasi-quote-face)
     ("|" t haskell-quasi-quote-face)
     ("]" t nil)
     ("Outside1" "w" haskell-constructor-face)
     ("Outside2" "w" haskell-constructor-face)
     )))

(ert-deftest haskell-syntactic-test-quasiquoter-5 ()
  "QuasiQuote should not conflict with TemplateHaskell"
  (check-properties
   '("nope = [| Cons |]"
     "nope = [e| Cons_e |]"
     "nope = [t| Cons_t |]"
     "nope = [d| Cons_d |]"
     "nope = [p| Cons_p |]"
     "yes = [x| Cons_x |]")
   '(("Cons" t haskell-constructor-face)
     ("Cons_e" t haskell-constructor-face)
     ("Cons_t" t haskell-constructor-face)
     ("Cons_d" t haskell-constructor-face)
     ("Cons_p" t haskell-constructor-face)
     ("Cons_x" t haskell-quasi-quote-face))))

(ert-deftest haskell-syntactic-test-quasiquoter-sql-1 ()
  "Embedded SQL statements"
  (check-properties
   '("sql = [sql| SELECT title FROM books; |]")
   '(("SELECT" t font-lock-keyword-face)
     ("title" t default)
     ("FROM" t font-lock-keyword-face)
     ("books" t default))))

(ert-deftest haskell-syntactic-test-quasiquoter-sql-2 ()
  "Embedded SQL statements"
  ;; for now we have this problem that connstructor faces are used,
  ;; org-mode knows how to get around this problem
  (check-properties
   '("sql = [sql| SELECT Title FROM Books; |]")
   '(("Title" t default)
     ("Books" t default))))

(ert-deftest haskell-syntactic-test-quasiquoter-sql-3 ()
  "Embedded SQL statements"
  (check-properties
   '("sql = [Mod.sql| SELECT title FROM books; |]")
   '(("SELECT" t font-lock-keyword-face)
     ("title" t default)
     ("FROM" t font-lock-keyword-face)
     ("books" t default))))


(ert-deftest haskell-syntactic-test-special-not-redefined ()
  "QuasiQuote should not conflict with TemplateHaskell"
  (check-properties
   '("nope1, nope2 "
     "nope3 = nope4 "
     "nope -> nope"
     "nope :: nope"
     "nope <- nope"
     "nope ` nope")
   '(("," t nil)
     ("=" t haskell-operator-face)
     ("->" t haskell-operator-face)
     ("::" t haskell-operator-face)
     ("<-" t haskell-operator-face)
     ("`" t nil))))

(ert-deftest haskell-syntactic-definition-face-1 ()
  (check-properties
   '("F +++ G")
   '(("+++" t haskell-definition-face))))

(ert-deftest haskell-syntactic-definition-face-1a ()
  (check-properties
   '("F `abc` G")
   '(("abc" t haskell-definition-face))))

(ert-deftest haskell-syntactic-definition-face-2 ()
  :expected-result :failed
  (check-properties
   '("M.F +++ N.G")
   '(("+++" t haskell-definition-face))))

(ert-deftest haskell-syntactic-definition-face-2a ()
  :expected-result :failed
  (check-properties
   '("M.F `abc` N.G")
   '(("abc" t haskell-definition-face))))

(ert-deftest haskell-syntactic-definition-face-3 ()
  (check-properties
   '("Q +++ 12.12")
   '(("+++" t haskell-definition-face))))

(ert-deftest haskell-syntactic-definition-face-4 ()
  (check-properties
   '("_test'")
   '(("_test'" t nil))))

(ert-deftest haskell-syntactic-definition-face-5 ()
  (check-properties
   '("_test' _")
   '(("_test'" t haskell-definition-face))))


(ert-deftest haskell-literate-bird-1 ()
  (check-properties
   '("Comment1"
     ""
     "> code1 = 1"
     "> code2 = 1"
     ""
     "Comment2"
     ""
     "> code3 = 1"
     ""
     "Comment3")
   '(("Comment1" t haskell-literate-comment-face)
     ("code1" t haskell-definition-face)
     ("code2" t haskell-definition-face)
     ("Comment2" t haskell-literate-comment-face)
     ("code3" t haskell-definition-face)
     ("Comment3" t haskell-literate-comment-face))
   'haskell-literate-mode))

(ert-deftest haskell-literate-bird-2 ()
  ;; Haskell Report requires empty line before bird code block. So it
  ;; is a code block, just in error.
  :expected-result :failed
  (check-properties
   '("Comment1"
     "> code1 = 1"
     "> code2 = 1"
     "Comment2"
     ""
     "> code3 = 1"
     ""
     "Comment3")
   '(("Comment1" t haskell-literate-comment-face)
     (">" t font-lock-warning-face)
     ("code1" t haskell-definition-face)
     ("code2" t haskell-definition-face)
     ("Comment2" t haskell-literate-comment-face)
     ("code3" t haskell-definition-face)
     ("Comment3" t haskell-literate-comment-face))
   'haskell-literate-mode))

(ert-deftest haskell-literate-latex-1 ()
  (check-properties
   '("Comment1"
     ""
     "\\begin{code}"
     "code1 = 1"
     "code2 = 1"
     "\\end{code}"
     ""
     "Comment2"
     "\\begin{code}"
     "code3 = 1"
     "\\end{code}"
     "Comment3")
   '(("Comment1" t haskell-literate-comment-face)
     ("code1" t haskell-definition-face)
     ("code2" t haskell-definition-face)
     ("Comment2" t haskell-literate-comment-face)
     ("code3" t haskell-definition-face)
     ("Comment3" t haskell-literate-comment-face))
   'haskell-literate-mode))

(ert-deftest haskell-literate-mixed-1 ()
  ;; Although Haskell Report does not advice mixing modes, it is a
  ;; perfectly valid construct that we should support in syntax
  ;; highlighting.
  (check-properties
   '("Comment1"
     ""
     "> code1 = 1"
     "> code2 = 1"
     ""
     "Comment2"
     "\\begin{code}"
     "code3 = 1"
     "\\end{code}"
     "Comment3")
   '(("Comment1" t haskell-literate-comment-face)
     ("code1" t haskell-definition-face)
     ("code2" t haskell-definition-face)
     ("Comment2" t haskell-literate-comment-face)
     ("code3" t haskell-definition-face)
     ("Comment3" t haskell-literate-comment-face))
   'haskell-literate-mode))

(ert-deftest haskell-type-instance ()
  "Fontify \"instance\" after \"type\""
  ;; Note that instance is always fontified, because it is a keyword even
  ;; without 'type' before it.
  (check-properties
   '("type instance Foo Int = Char")
    '(("type" "w" haskell-keyword-face)
      ("instance" "w" haskell-keyword-face))))

(ert-deftest haskell-type-family ()
  "Fontify \"family\" after \"type\""
  (check-properties
   '("type family Foo a :: *")
    '(("type" "w" haskell-keyword-face)
      ("family" "w" haskell-keyword-face))))

(ert-deftest haskell-data-family ()
  "Fontify \"family\" after \"data\""
  (check-properties
   '("data family Foo a :: *")
    '(("data" "w" haskell-keyword-face)
      ("family" "w" haskell-keyword-face))))

(ert-deftest haskell-no-family ()
  "Don't fontify \"family\" when not after \"type\" or \"data\""
  (check-properties
   '("foo family = 10")
    '(("foo" "w" haskell-definition-face)
      ("family" "w" nil))))

(ert-deftest haskell-type-role ()
  "Fontify \"role\" after \"type\""
  (check-properties
    '("type role Ptr representational")
    '(("type" "w" haskell-keyword-face)
      ("role" "w" haskell-keyword-face)
      ("Ptr" "w" haskell-type-face))))

(ert-deftest haskell-no-type-role ()
  "Don't fontify \"role\" when not after \"type\""
  (check-properties
    '("foo role = 3")
    '(("foo" "w" haskell-definition-face)
      ("role" "w" nil))))

(ert-deftest haskell-forall-in-type ()
  (check-properties
   '("forall = 23"
     "zonk :: forall x . x -> x")
   '(("forall" "w" haskell-definition-face)
     ("forall" "w" haskell-keyword-face))))

(ert-deftest haskell-unterminated-string-1 ()
  (check-properties
   '("foo = \"zonk"
     "       Cons")
    '(("\"" "|" font-lock-warning-face)
      ("zonk" t font-lock-string-face)
      ("Cons" "w" haskell-constructor-face))))

(ert-deftest haskell-unterminated-string-2 ()
  (check-properties
   '"foo = \"zonk"
    '(("\"" "\"" font-lock-warning-face)
      ("zonk" t font-lock-string-face))))

(ert-deftest haskell-unterminated-string-3 ()
  (check-properties
   '"foo = \"zonk\\"
    '(("\"" "\"" font-lock-warning-face)
      ("zonk" t font-lock-string-face)
      ("\\" t font-lock-warning-face))))

(ert-deftest haskell-type-colors-01 ()
  (check-properties
   "x :: Int -> String"
   '(("Int" t haskell-type-face)
     ("String" t haskell-type-face))))

(ert-deftest haskell-type-colors-02 ()
  (check-properties
   '("x :: (Monad m,"
     "      Applicative m)"
     "  => m Int")
   '(("Monad" t haskell-type-face)
     ("Applicative" t haskell-type-face)
     ("Int" t haskell-type-face))))

(ert-deftest haskell-type-colors-03 ()
  (check-properties
   '("x :: Lens' S A"
     "y Nothing1 Nothing2 = Nothing3")
   '(("Lens" t haskell-type-face)
     ("S" t haskell-type-face)
     ("A" t haskell-type-face)
     ("Nothing1" t haskell-constructor-face)
     ("Nothing2" t haskell-constructor-face)
     ("Nothing3" t haskell-constructor-face))))

(ert-deftest haskell-type-colors-04 ()
  (check-properties
   '("x :: Lens' S A"
     "(++++) Nothing1 Nothing2 = Nothing3")
   '(("Lens" t haskell-type-face)
     ("S" t haskell-type-face)
     ("A" t haskell-type-face)
     ("Nothing1" t haskell-constructor-face)
     ("Nothing2" t haskell-constructor-face)
     ("Nothing3" t haskell-constructor-face))))


(ert-deftest haskell-type-colors-05 ()
  (check-properties
   '"class (Monad a, Applicative b) => m a Int | a -> b String where"
   '(("Monad" t haskell-type-face)
     ("Applicative" t haskell-type-face)
     ("Int" t haskell-type-face)
     ("String" t haskell-type-face))))

(ert-deftest haskell-type-colors-06 ()
  (check-properties
   '"instance (Monad a, Applicative b) => m a Int | a -> b String where"
   '(("Monad" t haskell-type-face)
     ("Applicative" t haskell-type-face)
     ("Int" t haskell-type-face)
     ("String" t haskell-type-face))))

(ert-deftest haskell-type-colors-07 ()
  :expected-result :failed
  (check-properties
   '"data X = X1 String | X2 Int"
   '(("X" t haskell-type-face)
     ("X1" t haskell-constructor-face)
     ("String" t haskell-type-face)
     ("X2" t haskell-constructor-face)
     ("Int" t haskell-type-face))))

(ert-deftest haskell-type-colors-08 ()
  ;; simplified version of 07
  (check-properties
   '"data X = X1 String | X2 Int"
   '(("X" t haskell-type-face))))

(ert-deftest haskell-type-colors-09 ()
  (check-properties
   '"type X a b = Monad a (Lens b)"
   '(("X" t haskell-type-face)
     ("Monad" t haskell-type-face)
     ("Lens" t haskell-type-face))))

(ert-deftest haskell-type-colors-10 ()
  (check-properties
   '"type family X a b = Monad a (Lens b)"
   '(("X" t haskell-type-face)
     ("Monad" t haskell-type-face)
     ("Lens" t haskell-type-face))))

(ert-deftest haskell-type-colors-11 ()
  (check-properties
   '("data X a where"
     "    X1 :: Int1 -> X Int2"
     "    X2 :: String1 -> X String2")
   '(("X" t haskell-type-face)
     ("X1" t haskell-constructor-face)
     ("Int1" t haskell-type-face)
     ("X" t haskell-type-face)
     ("Int2" t haskell-type-face)
     ("X2" t haskell-constructor-face)
     ("String1" t haskell-type-face)
     ("X" t haskell-type-face)
     ("String2" t haskell-type-face))))


(ert-deftest haskell-type-colors-12 ()
  :expected-result :failed
  (check-properties
   '"data X = Int1 :+: String2 | String3 :-: Int4"
   '(("X" t haskell-type-face)
     ("Int1" t haskell-type-face)
     (":+:" t haskell-constructor-face)
     ("String2" t haskell-type-face)
     ("String3" t haskell-type-face)
     (":-:" t haskell-constructor-face)
     ("Int4" t haskell-type-face))))

(ert-deftest haskell-type-colors-13 ()
  (check-properties
   '"newtype Xa = Xb Int"
   '(("Xa" t haskell-type-face)
     ("Xb" t haskell-constructor-face))))

(ert-deftest haskell-type-colors-14 ()
  :expected-result :failed
  (check-properties
   '"newtype Xa = Xb Int"
   '(("Xa" t haskell-type-face)
     ("Xb" t haskell-constructor-face)
     ("Int" t haskell-type-face))))

(ert-deftest haskell-type-colors-15 ()
  (check-properties
   '"newtype Xa = Xb { xbField :: Int }"
   '(("Xa" t haskell-type-face)
     ("Xb" t haskell-constructor-face)
     ("Int" t haskell-type-face))))

(ert-deftest haskell-type-colors-16 ()
  :expected-result :failed
  (check-properties
   '"module M ( a, X(..), Y, Z(A,B)) where"
   '(("M" t haskell-constructor-face)
     ("X" t haskell-type-face)
     ("Y" t haskell-type-face)
     ("Z" t haskell-type-face)
     ("A" t haskell-constructor-face)
     ("B" t haskell-constructor-face))))

(ert-deftest haskell-type-colors-17 ()
  (check-properties
   '"[Just 5 :: Maybe Int | X <- xs]"
   '(("Just" t haskell-constructor-face)
     ("Maybe" t haskell-type-face)
     ("Int" t haskell-type-face)
     ("X" t haskell-constructor-face))))

(ert-deftest haskell-type-colors-18 ()
  (check-properties
   '"[Just 5 :: Maybe Int]"
   '(("Just" t haskell-constructor-face)
     ("Maybe" t haskell-type-face)
     ("Int" t haskell-type-face))))

(ert-deftest haskell-type-colors-19 ()
  (check-properties
   '"(5 :: Int, Just 5 :: Maybe Int) X"
   '(("Int" t haskell-type-face)
     ("Just" t haskell-constructor-face)
     ("Maybe" t haskell-type-face)
     ("Int" t haskell-type-face)
     ("X" t haskell-constructor-face))))

(ert-deftest haskell-type-colors-20 ()
  (check-properties
   '"x { x = 5 :: Int} Nothing"
   '(("Int" t haskell-type-face)
     ("Nothing" t haskell-constructor-face))))

(ert-deftest haskell-type-colors-21 ()
  (check-properties
   '("x = do"
     "  y :: Maybe Int <- return Nothing")
   '(("Maybe" t haskell-type-face)
     ("Int" t haskell-type-face)
     ("Nothing" t haskell-constructor-face))))

(ert-deftest haskell-type-colors-22 ()
  (check-properties
   '("x = case y :: Int of"
     "  42 -> Nothing")
   '(("Int" t haskell-type-face)
     ("Nothing" t haskell-constructor-face))))

(ert-deftest haskell-type-colors-23 ()
  (check-properties
   '("x :: Int ->"
     "     String")
   '(("Int" t haskell-type-face)
     ("String" t haskell-type-face))))

(ert-deftest haskell-type-colors-24 ()
  (check-properties
   '("x :: Int ->"
     ""
     " -- comment"
     "  {-"
     " multiline"
     " -}"
     ""
     "     String")
   '(("Int" t haskell-type-face)
     ("String" t haskell-type-face))))

(ert-deftest haskell-type-colors-25 ()
  (check-properties
   '("x :: Int"
     ""
     " -- comment"
     "  {-"
     " multiline"
     " -}"
     ""
     "     -> String")
   '(("Int" t haskell-type-face)
     ("String" t haskell-type-face))))

(ert-deftest haskell-type-colors-26 ()
  (check-properties
   '("x :: Int"
     ""
     " -- comment"
     "  {-"
     " multiline"
     " -}"
     ""
     "X `abc` Z")
   '(("Int" t haskell-type-face)
     ("X" t haskell-constructor-face)
     ("Z" t haskell-constructor-face))))

(ert-deftest haskell-type-colors-27 ()
  (check-properties
   '("x"
     "    ::"
     "   Int"
     "  ->"
     "  String")
   '(("Int" t haskell-type-face)
     ("String" t haskell-type-face))))

(ert-deftest haskell-type-colors-28 ()
  (check-properties
   "type instance Typ Int  b = Show b"
   '(("Typ" t haskell-type-face)
     ("Int" t haskell-type-face)
     ("Show" t haskell-type-face))))

(ert-deftest haskell-type-colors-29 ()
  :expected-result :failed
  (check-properties
   "import qualified X as Y(a,Z(C))"
   '(("X" t haskell-constructor-face)
     ("Y" t haskell-constructor-face)
     ("Z" t haskell-type-face)
     ("C" t haskell-constructor-face))))

(ert-deftest haskell-type-colors-30 ()
  :expected-result :failed
  (check-properties
   "import qualified X as Y hiding(a,Z(C))"
   '(("X" t haskell-constructor-face)
     ("Y" t haskell-constructor-face)
     ("Z" t haskell-type-face)
     ("C" t haskell-constructor-face))))

(ert-deftest haskell-type-colors-31 ()
  (check-properties
   ;; open parentheses do not keep type decl open because there might
   ;; be an unclosed parenthesis stretching to the end of file and
   ;; that is very costly to check
   '("x :: (OpenParen"
     "   NotType)")
   '(("OpenParen" t haskell-type-face)
     ("NotType" t haskell-constructor-face))))

(ert-deftest haskell-type-colors-32 ()
  (check-properties
   ;; keywords in comments or strings should not create problems
   '("flagSpec \"partial-type-signatures\"     Opt_WarnPartialTypeSignatures,"
     "{- class -} Cons2"
     "-- type"
     " Cons3")
   '(("Opt_WarnPartialTypeSignatures" t haskell-constructor-face)
     ("Cons2" t haskell-constructor-face)
     ("Cons3" t haskell-constructor-face))))

(ert-deftest haskell-pattern-1 ()
  "Fontify the \"pattern\" keyword in contexts related to pattern synonyms."
  (check-properties
   '("pattern A = B"
     "pattern A <- B"
     "pattern A ← B"
     "pattern A n <- (subtract 1 -> n) where A n = n + 1"
     "module Main (pattern A) where"
     "pattern A :: a -> B"
     "pattern A :: (C a) => a -> B"
     "pattern A :: (C a) => a -> B"
     "pattern A :: (C a) => () => a -> B"
     "pattern A :: (C a) => () => a -> B"
     "pattern A :: (C a) => (C a) => a -> B"
     "pattern A :: (C a) => (C a) => a -> B")
   '(("pattern" t haskell-keyword-face)
     ("pattern" t haskell-keyword-face)
     ("pattern" t haskell-keyword-face)
     ("pattern" t haskell-keyword-face)
     ("module"  t haskell-keyword-face)
     ("pattern" t haskell-keyword-face)
     ("where"   t haskell-keyword-face)
     ("pattern" t haskell-keyword-face)
     ("pattern" t haskell-keyword-face)
     ("pattern" t haskell-keyword-face)
     ("pattern" t haskell-keyword-face)
     ("pattern" t haskell-keyword-face)
     ("pattern" t haskell-keyword-face)
     ("pattern" t haskell-keyword-face))))

(ert-deftest haskell-pattern-2 ()
  (check-properties
   '("pattern :: Int"
     "pattern = 3")
   '(("pattern" t haskell-keyword-face)
     ("pattern" t haskell-keyword-face))))

(ert-deftest haskell-pattern-3 ()
  (check-properties
   '("foo :: (a -> pattern) -> a -> pattern"
     "foo pattern x = pattern x"
     "bar = pattern where pattern = 5")
   '(("pattern" t haskell-keyword-face)
     ("pattern" t haskell-keyword-face)
     ("pattern" t haskell-keyword-face)
     ("pattern" t haskell-keyword-face)
     ("pattern" t haskell-keyword-face)
     ("pattern" t haskell-keyword-face))))
