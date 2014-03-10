;; unit tests for haskell-str.el

(require 'ert)

(require 'haskell-str ) ;; implementation under test

(ert-deftest haskell-str-take ()
  (should (string= (haskell-str-take "" 0) ""))
  (should (string= (haskell-str-take "" 1) ""))
  (should (string= (haskell-str-take "" 2) ""))
  (should (string= (haskell-str-take "x" 0) ""))
  (should (string= (haskell-str-take "x" 1) "x"))
  (should (string= (haskell-str-take "x" 2) "x"))
  (should (string= (haskell-str-take "x" 3) "x"))
  (should (string= (haskell-str-take "xy" 0) ""))
  (should (string= (haskell-str-take "xy" 1) "x"))
  (should (string= (haskell-str-take "xy" 2) "xy"))
  (should (string= (haskell-str-take "xy" 3) "xy"))
  (should (string= (haskell-str-take "xyz" 0) ""))
  (should (string= (haskell-str-take "xyz" 1) "x"))
  (should (string= (haskell-str-take "xyz" 2) "xy"))
  (should (string= (haskell-str-take "xyz" 3) "xyz"))
  (should (string= (haskell-str-take "xyz" 4) "xyz")))

(ert-deftest haskell-str-ellipsize ()
  (should (string= (haskell-str-ellipsize "" 0) ""))
  (should (string= (haskell-str-ellipsize "" 1) ""))
  (should (string= (haskell-str-ellipsize "" 2) ""))
  (should (string= (haskell-str-ellipsize "x" 0) ""))
  (should (string= (haskell-str-ellipsize "x" 1) "x"))
  (should (string= (haskell-str-ellipsize "x" 2) "x"))
  (should (string= (haskell-str-ellipsize "x" 3) "x"))
  (should (string= (haskell-str-ellipsize "xy" 0) ""))
  (should (string= (haskell-str-ellipsize "xy" 1) "…"))
  (should (string= (haskell-str-ellipsize "xy" 2) "xy"))
  (should (string= (haskell-str-ellipsize "xy" 3) "xy"))
  (should (string= (haskell-str-ellipsize "xyz" 0) ""))
  (should (string= (haskell-str-ellipsize "xyz" 1) "…"))
  (should (string= (haskell-str-ellipsize "xyz" 2) "x…"))
  (should (string= (haskell-str-ellipsize "xyz" 3) "xyz"))
  (should (string= (haskell-str-ellipsize "xyz" 4) "xyz")))

(ert-deftest haskell-str-literal-encode-empty ()
  (should (string= (haskell-str-literal-encode "") "\"\""))
  (should (string= (haskell-str-literal-encode "" t) "")))

(ert-deftest haskell-str-literal-decode-empty ()
  (dolist (s0 (list "\"\""
		    "\"\\&\""
		    "\"\\&\\&\\&\""
		    "\"\\	   \\\""
		    "\"\\	  \\\\	  \\\""
		    "\"\\&\\     \\\""
		    "\"\\ \\\\&\\	  \\\""))
    (should (string= "" (haskell-str-literal-decode s0)))
    (should (string= "" (haskell-str-literal-decode (substring s0 1 -1) t)))))

(ert-deftest haskell-str-literal-decode-backslash ()
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
                    (cons "\\SOH" "\x01")))
    (should (string= (cdr cs)
                     (haskell-str-literal-decode (concat "\"" (car cs) "\""))))
    (should (string= (cdr cs)
                     (haskell-str-literal-decode (car cs) t)))))

(defun haskell-str-random (n)
  "Generate random N characters long string."
  (let ((a ()))
    (apply #'string (dotimes (_ n a)
                      (setq a (cons (random 1024) a))))))

(ert-deftest haskell-str-literal-decode-encode ()
  "Test whether decode+encode is the identity function."
  (random t)
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
    (should (string= s0 (haskell-str-literal-decode (haskell-str-literal-encode s0))))
    (should (string= s0 (haskell-str-literal-decode (haskell-str-literal-encode s0 t) t))))

  ;; randomized testing
  (dotimes (_ 500)
    (dotimes (n 15)
      (let* ((s0 (haskell-str-random (+ 1 n)))
             (s1 (haskell-str-literal-decode (haskell-str-literal-encode s0)))
             (s2 (haskell-str-literal-decode (haskell-str-literal-encode s0 t) t)))
        (should (string= s0 s1))
        (should (string= s0 s2))))))
