;; unit tests for haskell-string.el  -*- lexical-binding: t -*-

(require 'ert)

(require 'haskell-string) ;; implementation under test

(ert-deftest haskell-string-take ()
  (should (string= (haskell-string-take "" 0) ""))
  (should (string= (haskell-string-take "" 1) ""))
  (should (string= (haskell-string-take "" 2) ""))
  (should (string= (haskell-string-take "x" 0) ""))
  (should (string= (haskell-string-take "x" 1) "x"))
  (should (string= (haskell-string-take "x" 2) "x"))
  (should (string= (haskell-string-take "x" 3) "x"))
  (should (string= (haskell-string-take "xy" 0) ""))
  (should (string= (haskell-string-take "xy" 1) "x"))
  (should (string= (haskell-string-take "xy" 2) "xy"))
  (should (string= (haskell-string-take "xy" 3) "xy"))
  (should (string= (haskell-string-take "xyz" 0) ""))
  (should (string= (haskell-string-take "xyz" 1) "x"))
  (should (string= (haskell-string-take "xyz" 2) "xy"))
  (should (string= (haskell-string-take "xyz" 3) "xyz"))
  (should (string= (haskell-string-take "xyz" 4) "xyz")))

(ert-deftest haskell-string-ellipsize ()
  (should (string= (haskell-string-ellipsize "" 0) ""))
  (should (string= (haskell-string-ellipsize "" 1) ""))
  (should (string= (haskell-string-ellipsize "" 2) ""))
  (should (string= (haskell-string-ellipsize "x" 0) ""))
  (should (string= (haskell-string-ellipsize "x" 1) "x"))
  (should (string= (haskell-string-ellipsize "x" 2) "x"))
  (should (string= (haskell-string-ellipsize "x" 3) "x"))
  (should (string= (haskell-string-ellipsize "xy" 0) ""))
  (should (string= (haskell-string-ellipsize "xy" 1) "…"))
  (should (string= (haskell-string-ellipsize "xy" 2) "xy"))
  (should (string= (haskell-string-ellipsize "xy" 3) "xy"))
  (should (string= (haskell-string-ellipsize "xyz" 0) ""))
  (should (string= (haskell-string-ellipsize "xyz" 1) "…"))
  (should (string= (haskell-string-ellipsize "xyz" 2) "x…"))
  (should (string= (haskell-string-ellipsize "xyz" 3) "xyz"))
  (should (string= (haskell-string-ellipsize "xyz" 4) "xyz")))

(ert-deftest haskell-string-literal-encode-empty ()
  (should (string= (haskell-string-literal-encode "") "\"\""))
  (should (string= (haskell-string-literal-encode "" t) "")))

(ert-deftest haskell-string-literal-decode-empty ()
  (dolist (s0 (list "\"\""
                  "\"\\&\""
                  "\"\\&\\&\\&\""
                  "\"\\          \\\""
                  "\"\\         \\\\         \\\""
                  "\"\\&\\     \\\""
                  "\"\\ \\\\&\\         \\\""))
    (should (string= "" (haskell-string-literal-decode s0)))
    (should (string= "" (haskell-string-literal-decode (substring s0 1 -1) t)))))

(ert-deftest haskell-string-literal-decode-backslash ()
  "Test some edge cases involving backslashes."
  (dolist (cs (list (cons "\\\\" "\\")
                    (cons "\\x10" "\x10")
                    (cons "\\\\x10" "\\x10")
                    (cons "\\ \\x10" "x10")
                    (cons "\\ \\  \\x30" "  0")
                    (cons "\\SO\\&H" "\x0eH")
                    (cons "\\SOH\\&" "\x01")
                    (cons "\\n" "\n")
                    (cons "\\'" "'")
                    (cons "\\\"" "\"")
                    (cons "\\SOH" "\x01")
                    (cons "\\1114111" "\x10ffff")
                    (cons "\\o4177777" "\x10ffff")
                    (cons "\\x10ffff" "\x10ffff")
                    (cons "\\^@" "\x00")
                    (cons "\\^A" "\x01")
                    (cons "\\^Z" "\x1A")
                    (cons "\\^[" "\x1B")
                    (cons "\\^\\" "\x1C")
                    (cons "\\^]" "\x1D")
                    (cons "\\^^" "\x1E")
                    (cons "\\^_" "\x1F")))
    (should (string= (cdr cs)
                     (haskell-string-literal-decode (concat "\"" (car cs) "\""))))
    (should (string= (cdr cs)
                     (haskell-string-literal-decode (car cs) t)))))

(defun haskell-string-random (n)
  "Generate random N characters long string."
  (let ((a ()))
    (apply #'string (dotimes (_ n a)
                      (setq a (cons (random 1024) a))))))

(ert-deftest haskell-string-literal-decode-encode ()
  "Test whether decode+encode is the identity function."
  (random "c7430a4")
  ;; some edge cases
  (dolist (s0 (list "\x0e\x48" ;; '\SO' 'H'
                  "\x01"     ;; '\SOH'
                  "\x00df\x30" ;; '\223' '0'
                  "'"
                    "\'"
                  "\""
                  "\x0e&H"
                  "\\"
                  " \\   \\"
                  "\\\\\""
                    (string 40 945 8322 946 8323 8743 947 178 949 178 41)
                    "x"
                    "xy"
                    "\\x123"
                    "\\ \\x123"
                    " "
                    "  "
                  ""))
    (should (string= s0 (haskell-string-literal-decode (haskell-string-literal-encode s0))))
    (should (string= s0 (haskell-string-literal-decode (haskell-string-literal-encode s0 t) t))))

  ;; randomized testing
  (dotimes (_ 50)
    (dotimes (n 15)
      (let* ((s0 (haskell-string-random (+ 1 n)))
             (s1 (haskell-string-literal-decode (haskell-string-literal-encode s0)))
             (s2 (haskell-string-literal-decode (haskell-string-literal-encode s0 t) t)))
        (should (string= s0 s1))
        (should (string= s0 s2))))))

(ert-deftest haskell-string-test-trim ()
  (should (equal "saf \t  sdsaf"
                 (haskell-string-trim "\r\n saf \t  sdsaf \t\v\n   \f")))
  (should (haskell-string-only-spaces-p "\r\n \t  \t\v\n   \f"))
  (should-not (haskell-string-only-spaces-p "\r\n \t  x  \t\v\n   \f")))
