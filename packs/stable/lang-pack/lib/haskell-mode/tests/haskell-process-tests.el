;;; haskell-process-tests.el

;;; Code:

(require 'ert)
(require 'el-mock)

(require 'haskell-process)

(ert-deftest haskell-process-wrapper-command-function-identity ()
  "No wrapper, return directly the command."
  (should (equal '("ghci")
                 (progn
                   (custom-set-variables '(haskell-process-wrapper-function #'identity))
                   (apply haskell-process-wrapper-function (list '("ghci")))))))

(ert-deftest haskell-process-wrapper-function-non-identity ()
  "Wrapper as a string, return the wrapping command as a string."
  (should (equal '("nix-shell" "default.nix" "--command" "cabal\\ run")
                 (progn
                   (custom-set-variables '(haskell-process-wrapper-function (lambda (argv)
                                                                              (append '("nix-shell" "default.nix" "--command")
                                                                                      (list (shell-quote-argument argv))))))
                   (apply haskell-process-wrapper-function (list "cabal run"))))))

(ert-deftest test-haskell-process--compute-process-log-and-command-ghci ()
  (should (equal '("Starting inferior GHCi process ghci ..." "dumses1" nil "ghci" "-ferror-spans")
                 (let ((haskell-process-path-ghci "ghci")
                       (haskell-process-args-ghci '("-ferror-spans")))
                   (custom-set-variables '(haskell-process-wrapper-function #'identity))
                   (mocklet (((haskell-session-name "dummy-session") => "dumses1"))
                     (haskell-process-compute-process-log-and-command "dummy-session" 'ghci))))))

(ert-deftest test-haskell-process--with-wrapper-compute-process-log-and-command-ghci ()
  (should (equal '("Starting inferior GHCi process ghci ..." "dumses1" nil "nix-shell" "default.nix" "--command" "ghci\\ -ferror-spans")
                 (let ((haskell-process-path-ghci "ghci")
                       (haskell-process-args-ghci '("-ferror-spans")))
                   (custom-set-variables '(haskell-process-wrapper-function
                                           (lambda (argv) (append (list "nix-shell" "default.nix" "--command" )
                                                             (list (shell-quote-argument (mapconcat 'identity argv " ")))))))
                   (mocklet (((haskell-session-name "dummy-session") => "dumses1"))
                     (haskell-process-compute-process-log-and-command "dummy-session" 'ghci))))))

(ert-deftest test-haskell-process--compute-process-log-and-command-cabal-repl ()
  (should (equal '("Starting inferior `cabal repl' process using cabal ..." "dumses2" nil "cabal" "repl" "--ghc-option=-ferror-spans" "dumdum-session")
                 (let ((haskell-process-path-cabal      "cabal")
                       (haskell-process-args-cabal-repl '("--ghc-option=-ferror-spans")))
                   (custom-set-variables '(haskell-process-wrapper-function #'identity))
                   (mocklet (((haskell-session-name "dummy-session2") => "dumses2")
                             ((haskell-session-target "dummy-session2") => "dumdum-session"))
                     (haskell-process-compute-process-log-and-command "dummy-session2" 'cabal-repl))))))

(ert-deftest test-haskell-process--with-wrapper-compute-process-log-and-command-cabal-repl ()
  (should (equal '("Starting inferior `cabal repl' process using cabal ..." "dumses2" nil "nix-shell" "default.nix" "--command" "cabal\\ repl\\ --ghc-option\\=-ferror-spans\\ dumdum-session")
                 (let ((haskell-process-path-cabal      "cabal")
                       (haskell-process-args-cabal-repl '("--ghc-option=-ferror-spans")))
                   (custom-set-variables '(haskell-process-wrapper-function
                                           (lambda (argv) (append (list "nix-shell" "default.nix" "--command" )
                                                             (list (shell-quote-argument (mapconcat 'identity argv " ")))))))
                   (mocklet (((haskell-session-name "dummy-session2") => "dumses2")
                             ((haskell-session-target "dummy-session2") => "dumdum-session"))
                     (haskell-process-compute-process-log-and-command "dummy-session2" 'cabal-repl))))))


;;; haskell-process-tests.el ends here
