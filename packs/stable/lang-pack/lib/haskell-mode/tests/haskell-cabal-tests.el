;;; haskell-cabal-tests.el -*- lexical-binding: t -*-
;;; Code:

(require 'ert)
(require 'haskell-cabal)

(defconst haskell-cabal-test-data-dir
  (file-name-as-directory
   (expand-file-name "test-data" (if load-file-name
                                     (file-name-directory load-file-name)
                                   default-directory))))

(unless (file-directory-p haskell-cabal-test-data-dir)
  (error "Couldn't find tests/test-data subdir"))

(ert-deftest haskell-cabal-enum-targets-1 ()
  "Test enumerating .cabal targets for use by cabal-install."
  (with-temp-buffer
    (haskell-cabal-mode)
    (setq default-directory haskell-cabal-test-data-dir)
    (should (equal '("lib:Test" "test:test-1" "bench:bench-1" "exe:bin-1")
                   (haskell-cabal-enum-targets)))))

(ert-deftest haskell-cabal-enum-targets-2 ()
  "Test enumerating .cabal targets for use by stack."
  (with-temp-buffer
    (haskell-cabal-mode)
    (setq default-directory haskell-cabal-test-data-dir)
    (should (equal '("Test:lib" "Test:test:test-1" "Test:bench:bench-1" "Test:exe:bin-1")
                   (haskell-cabal-enum-targets 'stack-ghci)))))

(ert-deftest haskell-cabal-get-field-1 ()
  (with-temp-buffer
    (setq default-directory haskell-cabal-test-data-dir)
    (set-visited-file-name (expand-file-name "Source.hs") t t)
    (should (equal "Simple"
                   (haskell-cabal-get-field "build-type")))))

(ert-deftest haskell-cabal-compute-checksum-1 ()
  (should (equal "263e67082326a27585639420f4d42c8b"
                 (haskell-cabal-compute-checksum haskell-cabal-test-data-dir))))

(ert-deftest haskell-cabal-compute-next-prev-section-1 ()
  (with-temp-buffer
    (setq default-directory haskell-cabal-test-data-dir)
    (insert-file-contents (expand-file-name "Test.cabal"))
    (haskell-cabal-mode)
    (goto-char (point-min))
    (haskell-cabal-next-section)
    (haskell-cabal-next-subsection)
    (haskell-cabal-previous-subsection)
    (haskell-cabal-previous-section)))

(ert-deftest haskell-cabal-period-is-a-word-break ()
  (with-temp-buffer
    (insert "
Executable bin
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
  (with-temp-buffer
    (insert "
Executable bin-1
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
    (should (string= (buffer-string) "
Executable bin-1
    Main-Is:          TestParsing.hs
    Build-Depends:    base,
                      bytestring,
                      directory,
                      filepath,
                      text
    Ghc-Options: -O -Wall
"))))

(ert-deftest haskell-cabal-subsection-arrange-lines-keep-commas-before ()
  (with-temp-buffer
    (insert "
Executable bin-1
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
    (should (string= (buffer-string) "
Executable bin-1
    Main-Is:          TestParsing.hs
    Build-Depends:    base
                    , bytestring
                    , directory
                    , filepath
                    , text
    Ghc-Options: -O -Wall
"))))

(ert-deftest haskell-cabal-subsection-arrange-lines-no-commas ()
  (with-temp-buffer
    (insert "
Executable bin-1
    Main-Is:          TestParsing.hs
    Other-Modules:    Some.Module
                      Some.Other.Other.Module
                      Some.Other.Module
")
    (haskell-cabal-mode)
    (goto-char (point-min))
    (search-forward "Other-Modules:")
    (haskell-cabal-subsection-arrange-lines)
    (should (string= (buffer-string) "
Executable bin-1
    Main-Is:          TestParsing.hs
    Other-Modules:    Some.Module
                      Some.Other.Module
                      Some.Other.Other.Module
"))))

(ert-deftest haskell-cabal-subsection-arrange-lines-mixed-styles ()
  (with-temp-buffer
    (insert "
Executable bin-1
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
    (should (string= (buffer-string) "
Executable bin-1
    Main-Is:          TestParsing.hs
    Build-Depends:    base
                    , bytestring
                    , directory
                    , filepath
                    , text
    Ghc-Options: -O -Wall
"))))

(ert-deftest haskell-cabal-subsection-arrange-lines-quoted-items ()
  (with-temp-buffer
    (insert "
Executable bin-1
    Main-Is:          TestParsing.hs
    GHC-Options:      -fprof-auto \"-with-rtsopts=-N -p -s -h -i0.1\"
")
    (haskell-cabal-mode)
    (goto-char (point-min))
    (search-forward "GHC-Options:")
    (haskell-cabal-subsection-arrange-lines)
    (should (string= (buffer-string) "
Executable bin-1
    Main-Is:          TestParsing.hs
    GHC-Options:      -fprof-auto \"-with-rtsopts=-N -p -s -h -i0.1\"
"))))

(ert-deftest haskell-cabal-subsection-arrange-lines-no-commas-quoted-comma ()
  (with-temp-buffer
    (insert "
Executable bin-1
    Main-Is:          TestParsing.hs
    GHC-Options:      -Wall -fprof-auto \"foo, bar\"
")
    (haskell-cabal-mode)
    (goto-char (point-min))
    (search-forward "GHC-Options:")
    (haskell-cabal-subsection-arrange-lines)
    (should (string= (buffer-string) "
Executable bin-1
    Main-Is:          TestParsing.hs
    GHC-Options:      -Wall -fprof-auto \"foo, bar\"
"))))

(ert-deftest haskell-cabal-subsection-arrange-lines-single-line-quoted-comma ()
  (with-temp-buffer
    (insert "
Executable bin-1
    Main-Is:          TestParsing.hs
    GHC-Options:      -Wall,-fprof-auto \"foo, bar\"
")
    (haskell-cabal-mode)
    (goto-char (point-min))
    (search-forward "GHC-Options:")
    (haskell-cabal-subsection-arrange-lines)
    (string= (buffer-string) "
Executable bin-1
    Main-Is:          TestParsing.hs
    GHC-Options:      -Wall, -fprof-auto \"foo, bar\"
")))

(ert-deftest haskell-cabal-subsection-arrange-lines-trailing-commas-quoted-comma ()
  (with-temp-buffer
    (insert "
Executable bin-1
    Main-Is:          TestParsing.hs
    GHC-Options:      -Wall,
                      -fprof-auto \"foo, bar\"
")
    (haskell-cabal-mode)
    (goto-char (point-min))
    (search-forward "GHC-Options:")
    (haskell-cabal-subsection-arrange-lines)
    (string= (buffer-string) "
Executable bin-1
    Main-Is:          TestParsing.hs
    GHC-Options:      -Wall,
                      -fprof-auto \"foo, bar\"
")))

(ert-deftest haskell-cabal-subsection-arrange-lines-commas-before-quoted-comma ()
  (with-temp-buffer
    (insert "
Executable bin-1
    Main-Is:          TestParsing.hs
    GHC-Options:      -Wall
                    , -fprof-auto \"foo, bar\"
")
    (haskell-cabal-mode)
    (goto-char (point-min))
    (search-forward "GHC-Options:")
    (haskell-cabal-subsection-arrange-lines)
    (string= (buffer-string) "
Executable bin-1
    Main-Is:          TestParsing.hs
    GHC-Options:      -Wall
                    , -fprof-auto \"foo, bar\"
")))

(ert-deftest haskell-cabal-subsection-arrange-lines-comma-in-commment ()
  (with-temp-buffer
    (insert "
Executable bin-1
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
    (should (string= (buffer-string) "
Executable bin-1
    Main-Is:          TestParsing.hs
    Other-Modules:    Some.Module
                      Some.Other.Module
                      Some.Other.Other.Module
                      -- Foo, bar
"))))

(ert-deftest haskell-cabal-subsection-arrange-lines-comma-in-commment ()
  (with-temp-buffer
    (insert "
Executable bin-1
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
    (should (string= (buffer-string) "
Executable bin-1
    Main-Is:          TestParsing.hs
    Other-Modules:    Some.Module
                      Some.Other.Module
                      Some.Other.Other.Module
                      -- Foo, bar
"))))

(ert-deftest haskell-cabal-subsection-arrange-lines-dependencies ()
  (with-temp-buffer
    (insert "
Executable bin-1
    Main-Is:          TestParsing.hs
    Build-Depends: aeson
                 , text
                 , base >= 4.8 && < 5
                 , base64
                 , bytestring
                 , base-compat
")
    (haskell-cabal-mode)
    (goto-char (point-min))
    (search-forward "build-depends:")
    (haskell-cabal-subsection-arrange-lines)
    (should (string= (buffer-string) "
Executable bin-1
    Main-Is:          TestParsing.hs
    Build-Depends: base >= 4.8 && < 5
                 , aeson
                 , base-compat
                 , base64
                 , bytestring
                 , text
"))))

(ert-deftest haskell-cabal-add-dependency-01 ()
  ;; cannot add dependency when there is no 'Build-depends' section already
  :expected-result :failed
  (with-temp-buffer
    (insert "
Executable bin-1
    Main-Is:          TestParsing.hs
")
    (haskell-cabal-mode)
    (haskell-cabal-add-build-dependency "bytestring" nil t)
    (should (string= (buffer-string) "
Executable bin-1
    Main-Is:          TestParsing.hs
    Build-Depends:    bytestring
"))))

(ert-deftest haskell-cabal-add-dependency-02 ()
  (with-temp-buffer
    (insert "
Executable bin-1
    Main-Is:          TestParsing.hs
    Build-Depends:    base
")
    (haskell-cabal-mode)
    (haskell-cabal-add-build-dependency "bytestring" nil t)
    (should (string= (buffer-string) "
Executable bin-1
    Main-Is:          TestParsing.hs
    Build-Depends:    bytestring,
                      base
"))))

(ert-deftest haskell-cabal-add-dependency-02 ()
  (with-temp-buffer
    (insert "
Executable bin-1
    Main-Is:          TestParsing.hs
    Build-Depends:    base
")
    (haskell-cabal-mode)
    (haskell-cabal-add-build-dependency "bytestring" nil t)
    (should (string= (buffer-string) "
Executable bin-1
    Main-Is:          TestParsing.hs
    Build-Depends:    bytestring,
                      base
"))))

(ert-deftest haskell-cabal-add-dependency-03 ()
  (with-temp-buffer
    (insert "
Executable bin-1
    Main-Is:          TestParsing.hs
    Build-Depends:    base,
                      mtl
")
    (haskell-cabal-mode)
    (haskell-cabal-add-build-dependency "bytestring" nil t)
    (should (string= (buffer-string) "
Executable bin-1
    Main-Is:          TestParsing.hs
    Build-Depends:    bytestring,
                      base,
                      mtl
"))))

(ert-deftest haskell-cabal-add-dependency-04 ()
  (with-temp-buffer
    (insert "
Executable bin-1
    Main-Is:          TestParsing.hs
    Build-Depends:    base
                    , mtl
")
    (haskell-cabal-mode)
    (haskell-cabal-add-build-dependency "bytestring" nil t)
    (should (string= (buffer-string) "
Executable bin-1
    Main-Is:          TestParsing.hs
    Build-Depends:    bytestring
                    , base
                    , mtl
"))))


(provide 'haskell-cabal-tests)

;;; haskell-cabal-tests.el ends here
