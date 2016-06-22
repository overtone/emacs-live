;;; haskell-cabal-tests.el
;;; Code:

(require 'ert)
(require 'haskell-cabal)

(ert-deftest haskell-cabal-enum-targets-1 ()
  "Test enumerating .cabal targets."
  (with-temp-buffer
    (haskell-cabal-mode)
    (let ((scriptDir
           (file-name-directory
             (or (symbol-file 'haskell-cabal-enum-targets-1)
                 (buffer-file-name)))))
      (setq default-directory (expand-file-name "test-data" scriptDir)))
    (should (equal '("Test" "test-1" "bench-1" "bin-1")
                   (haskell-cabal-enum-targets)))))

(ert-deftest haskell-cabal-get-field-1 ()
  (with-temp-buffer
    (let ((scriptDir
           (file-name-directory
             (or (symbol-file 'haskell-cabal-get-field-1)
                 (buffer-file-name)))))
      (set-visited-file-name (expand-file-name "test-data/Source.hs" scriptDir) t t))
    (should (equal "Simple"
                   (haskell-cabal-get-field "build-type")))))

(ert-deftest haskell-cabal-compute-checksum-1 ()
  (let ((scriptDir
         (file-name-directory
          (or (symbol-file 'haskell-cabal-get-field-1)
              (buffer-file-name)))))

    (should (equal "263e67082326a27585639420f4d42c8b"
                   (haskell-cabal-compute-checksum (expand-file-name "test-data" scriptDir))))))

(ert-deftest haskell-cabal-compute-next-prev-section-1 ()
  (let ((scriptDir
         (file-name-directory
          (or (symbol-file 'haskell-cabal-get-field-1)
              (buffer-file-name)))))

    (with-temp-buffer
      (insert-file-contents (expand-file-name "test-data/Test.cabal" scriptDir))
      (haskell-cabal-mode)
      (goto-char (point-min))
      (haskell-cabal-next-section)
      (haskell-cabal-next-subsection)
      (haskell-cabal-previous-subsection)
      (haskell-cabal-previous-section))))

(ert-deftest haskell-cabal-period-is-a-word-break ()
  (with-temp-buffer
    (insert "Executable bin
    Main-Is:           Main
    Exposed-Modules:   Some.Internal.Type
")
    (haskell-cabal-mode)
    (goto-char (point-min))
    (search-forward "Modules:")
    (skip-chars-forward " ")
    (should (looking-at-p "Some"))
    (forward-word)
    (should (looking-at-p ".Internal"))
    (forward-word)
    (should (looking-at-p ".Type"))
    (backward-word)
    (should (looking-at-p "Internal"))))

(ert-deftest haskell-cabal-subsection-arrange-lines-keep-trailing-commas ()
  (should (with-temp-buffer
            (insert "Executable bin-1
    Main-Is:          TestParsing.hs
    Build-Depends:    base,
                      bytestring,
                      filepath,
                      directory,
                      text
    Ghc-Options: -O -Wall
")
            (haskell-cabal-mode)
            (goto-char (point-min))
            (search-forward "Build-Depends:")
            (haskell-cabal-subsection-arrange-lines)
            (string= (buffer-string)
                     "Executable bin-1
    Main-Is:          TestParsing.hs
    Build-Depends:    base,
                      bytestring,
                      directory,
                      filepath,
                      text
    Ghc-Options: -O -Wall
"))))

(ert-deftest haskell-cabal-subsection-arrange-lines-keep-commas-before ()
  (should (with-temp-buffer
            (insert "Executable bin-1
    Main-Is:          TestParsing.hs
    Build-Depends:    base
                    , bytestring
                    , filepath
                    , directory
                    , text
    Ghc-Options: -O -Wall
")
            (haskell-cabal-mode)
            (goto-char (point-min))
            (search-forward "Build-Depends:")
            (haskell-cabal-subsection-arrange-lines)
            (string= (buffer-string)
                     "Executable bin-1
    Main-Is:          TestParsing.hs
    Build-Depends:    base
                    , bytestring
                    , directory
                    , filepath
                    , text
    Ghc-Options: -O -Wall
"))))

(ert-deftest haskell-cabal-subsection-arrange-lines-no-commas ()
  (should (with-temp-buffer
            (insert "Executable bin-1
    Main-Is:          TestParsing.hs
    Other-Modules:    Some.Module
                      Some.Other.Other.Module
                      Some.Other.Module
")
            (haskell-cabal-mode)
            (goto-char (point-min))
            (search-forward "Other-Modules:")
            (haskell-cabal-subsection-arrange-lines)
            (string= (buffer-string)
                     "Executable bin-1
    Main-Is:          TestParsing.hs
    Other-Modules:    Some.Module
                      Some.Other.Module
                      Some.Other.Other.Module
"))))

(ert-deftest haskell-cabal-subsection-arrange-lines-mixed-styles ()
  (should (with-temp-buffer
            (insert "Executable bin-1
    Main-Is:          TestParsing.hs
    Build-Depends:    base
                    , bytestring,
                      filepath, directory, text
    Ghc-Options: -O -Wall
")
            (haskell-cabal-mode)
            (goto-char (point-min))
            (search-forward "Build-Depends:")
            (haskell-cabal-subsection-arrange-lines)
            (string= (buffer-string)
                     "Executable bin-1
    Main-Is:          TestParsing.hs
    Build-Depends:    base
                    , bytestring
                    , directory
                    , filepath
                    , text
    Ghc-Options: -O -Wall
"))))

(ert-deftest haskell-cabal-subsection-arrange-lines-quoted-items ()
  (should (with-temp-buffer
            (insert "Executable bin-1
    Main-Is:          TestParsing.hs
    GHC-Options:      -fprof-auto \"-with-rtsopts=-N -p -s -h -i0.1\"
")
            (haskell-cabal-mode)
            (goto-char (point-min))
            (search-forward "GHC-Options:")
            (haskell-cabal-subsection-arrange-lines)
            (string= (buffer-string)
                     "Executable bin-1
    Main-Is:          TestParsing.hs
    GHC-Options:      -fprof-auto \"-with-rtsopts=-N -p -s -h -i0.1\"
"))))

(ert-deftest haskell-cabal-subsection-arrange-lines-no-commas-quoted-comma ()
  (should (with-temp-buffer
            (insert "Executable bin-1
    Main-Is:          TestParsing.hs
    GHC-Options:      -Wall -fprof-auto \"foo, bar\"
")
            (haskell-cabal-mode)
            (goto-char (point-min))
            (search-forward "GHC-Options:")
            (haskell-cabal-subsection-arrange-lines)
            (string= (buffer-string)
                     "Executable bin-1
    Main-Is:          TestParsing.hs
    GHC-Options:      -Wall -fprof-auto \"foo, bar\"
"))))

(ert-deftest haskell-cabal-subsection-arrange-lines-single-line-quoted-comma ()
  (should (with-temp-buffer
            (insert "Executable bin-1
    Main-Is:          TestParsing.hs
    GHC-Options:      -Wall,-fprof-auto \"foo, bar\"
")
            (haskell-cabal-mode)
            (goto-char (point-min))
            (search-forward "GHC-Options:")
            (haskell-cabal-subsection-arrange-lines)
            (string= (buffer-string)
                     "Executable bin-1
    Main-Is:          TestParsing.hs
    GHC-Options:      -Wall, -fprof-auto \"foo, bar\"
"))))

(ert-deftest haskell-cabal-subsection-arrange-lines-trailing-commas-quoted-comma ()
  (should (with-temp-buffer
            (insert "Executable bin-1
    Main-Is:          TestParsing.hs
    GHC-Options:      -Wall,
                      -fprof-auto \"foo, bar\"
")
            (haskell-cabal-mode)
            (goto-char (point-min))
            (search-forward "GHC-Options:")
            (haskell-cabal-subsection-arrange-lines)
            (string= (buffer-string)
                     "Executable bin-1
    Main-Is:          TestParsing.hs
    GHC-Options:      -Wall,
                      -fprof-auto \"foo, bar\"
"))))

(ert-deftest haskell-cabal-subsection-arrange-lines-commas-before-quoted-comma ()
  (should (with-temp-buffer
            (insert "Executable bin-1
    Main-Is:          TestParsing.hs
    GHC-Options:      -Wall
                    , -fprof-auto \"foo, bar\"
")
            (haskell-cabal-mode)
            (goto-char (point-min))
            (search-forward "GHC-Options:")
            (haskell-cabal-subsection-arrange-lines)
            (string= (buffer-string)
                     "Executable bin-1
    Main-Is:          TestParsing.hs
    GHC-Options:      -Wall
                    , -fprof-auto \"foo, bar\"
"))))

(ert-deftest haskell-cabal-subsection-arrange-lines-comma-in-commment ()
  (should (with-temp-buffer
            (insert "Executable bin-1
    Main-Is:          TestParsing.hs
    Other-Modules:    Some.Module
                      Some.Other.Other.Module
                      Some.Other.Module
                      -- Foo, bar
")
            (haskell-cabal-mode)
            (goto-char (point-min))
            (search-forward "Other-Modules:")
            (haskell-cabal-subsection-arrange-lines)
            (string= (buffer-string)
                     "Executable bin-1
    Main-Is:          TestParsing.hs
    Other-Modules:    Some.Module
                      Some.Other.Module
                      Some.Other.Other.Module
                      -- Foo, bar
"))))

(provide 'haskell-cabal-tests)

;;; haskell-cabal-tests.el ends here
