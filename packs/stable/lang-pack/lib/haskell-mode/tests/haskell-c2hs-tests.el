;; haskell-c2hs-tests.el --- -*- lexical-binding: t; -*-

(require 'ert)
(require 'haskell-c2hs)
(require 'haskell-test-utils)

(ert-deftest haskell-c2hs-basic-import-hook ()
  "C2HS import hook"
  (check-properties
    '("{# import Foo #}")
    '(("{#" t haskell-c2hs-hook-pair-face)
      ("import" "w" haskell-c2hs-hook-name-face)
      ("#}" t haskell-c2hs-hook-pair-face))
    'haskell-c2hs-mode))

(ert-deftest haskell-c2hs-qualified-import-hook ()
  "C2HS qualified import hook"
  (check-properties
    '("{#import qualified Foo #}")
    '(("{#" t haskell-c2hs-hook-pair-face)
      ("import" "w" haskell-c2hs-hook-name-face)
      ("qualified" "w" haskell-c2hs-hook-name-face)
      ("#}" t haskell-c2hs-hook-pair-face))
    'haskell-c2hs-mode))

(ert-deftest haskell-c2hs-full-context-hook ()
  "C2HS full context hook"
  (check-properties
    '("{# context lib = \"libgtk.so\" prefix = \"gtk\" add prefix = \"CGtk\" #}")
    '(("{#" t haskell-c2hs-hook-pair-face)
      ("context" "w" haskell-c2hs-hook-name-face)
      ("lib" "w" haskell-c2hs-hook-name-face)
      ("prefix" "w" haskell-c2hs-hook-name-face)
      ("add" "w" haskell-c2hs-hook-name-face)
      ("prefix" "w" haskell-c2hs-hook-name-face)
      ("#}" t haskell-c2hs-hook-pair-face))
    'haskell-c2hs-mode))

(ert-deftest haskell-c2hs-type-hook ()
  "C2HS type hook"
  (check-properties
    '("{# type gint #}")
    '(("{#" t haskell-c2hs-hook-pair-face)
      ("type" "w" haskell-c2hs-hook-name-face)
      ("#}" t haskell-c2hs-hook-pair-face))
    'haskell-c2hs-mode))

(ert-deftest haskell-c2hs-sizeof-hook ()
  "C2HS sizeof hook"
  (check-properties
    '("{# sizeof double #}")
    '(("{#" t haskell-c2hs-hook-pair-face)
      ("sizeof" "w" haskell-c2hs-hook-name-face)
      ("#}" t haskell-c2hs-hook-pair-face))
    'haskell-c2hs-mode))

(ert-deftest haskell-c2hs-enum-hook ()
  "C2HS enum hook"
  ;; note that this has multiline constructs that do not work reliably at this point
  :expected-result :failed
  (check-properties
    '("{#enum v4l2_quantization as Quantization"
      "  { V4L2_QUANTIZATION_DEFAULT    as Default"
      "  , V4L2_QUANTIZATION_FULL_RANGE as FullRange"
      "  , V4L2_QUANTIZATION_LIM_RANGE  as LimitedRange"
      "  }"
      "  deriving (Show, Eq, Ord)"
      "  #}")
    '(("{#" t haskell-c2hs-hook-pair-face)
      ("enum" "w" haskell-c2hs-hook-name-face)
      ("as" "w" haskell-c2hs-hook-name-face)
      ("Quantization" "w" haskell-constructor-face)
      ("V4L2_QUANTIZATION_DEFAULT" "w" haskell-constructor-face)
      ("Default" "w" haskell-constructor-face)
      ("V4L2_QUANTIZATION_FULL_RANGE" "w" haskell-constructor-face)
      ("FullRange" "w" haskell-constructor-face)
      ("V4L2_QUANTIZATION_LIM_RANGE" "w" haskell-constructor-face)
      ("LimitedRange" "w" haskell-constructor-face)
      ("deriving" "w" haskell-keyword-face)
      ("Show" "w" haskell-constructor-face)
      ("Eq" "w" haskell-constructor-face)
      ("Ord" "w" haskell-constructor-face)
      ("#}" t haskell-c2hs-hook-pair-face))
    'haskell-c2hs-mode))

(ert-deftest haskell-c2hs-enum-define-hook ()
  "C2HS enum define hook"
  ;; note that this has multiline constructs that do not work reliably at this point
  :expected-result :failed
  (check-properties
    '("{#enum define MMapProtectionFlag"
      "  { PROT_READ  as ProtRead"
      "  , PROT_WRITE as ProtWrite"
      "  , PROT_EXEC  as ProtExec"
      "  , PROT_NONE  as ProtNone"
      "  }"
      "  deriving (Show, Eq, Ord)"
      "  #}"
      )
    '(("{#" t haskell-c2hs-hook-pair-face)
      ("enum" "w" haskell-c2hs-hook-name-face)
      ("define" "w" haskell-c2hs-hook-name-face)
      ("MMapProtectionFlag" "w" haskell-constructor-face)
      ("PROT_READ" "w" haskell-constructor-face)
      ("ProtRead" "w" haskell-constructor-face)
      ("PROT_WRITE" "w" haskell-constructor-face)
      ("ProtWrite" "w" haskell-constructor-face)
      ("PROT_EXEC" "w" haskell-constructor-face)
      ("ProtExec" "w" haskell-constructor-face)
      ("PROT_NONE" "w" haskell-constructor-face)
      ("ProtNone" "w" haskell-constructor-face)
      ("deriving" "w" haskell-keyword-face)
      ("Show" "w" haskell-constructor-face)
      ("Eq" "w" haskell-constructor-face)
      ("Ord" "w" haskell-constructor-face)
      ("#}" t haskell-c2hs-hook-pair-face))
    'haskell-c2hs-mode))

(ert-deftest haskell-c2hs-pure-call-hook ()
  "C2HS pure call hook"
  (check-properties
    '("sin :: Float -> Float"
      "sin  = {#call pure sin as \"_sin\"#}")
    '(("sin" "w" haskell-definition-face)
      ("::" t haskell-operator-face)
      ("Float" "w" haskell-type-face)
      ("->" t haskell-operator-face)
      ("Float" "w" haskell-type-face)
      ("sin" "w" haskell-definition-face)
      ("=" t haskell-operator-face)
      ("{#" t haskell-c2hs-hook-pair-face)
      ("call" "w" haskell-c2hs-hook-name-face)
      ("pure" "w" haskell-c2hs-hook-name-face)
      ("as" "w" haskell-c2hs-hook-name-face)
      ("#}" t haskell-c2hs-hook-pair-face))
    'haskell-c2hs-mode))

(ert-deftest haskell-c2hs-unsafe-call-hook ()
  "C2HS unsafe fun hook"
  (check-properties
    '("{#fun unsafe sin as ^#}")
    '(("{#" t haskell-c2hs-hook-pair-face)
      ("fun" "w" haskell-c2hs-hook-name-face)
      ("unsafe" "w" haskell-c2hs-hook-name-face)
      ("as" "w" haskell-c2hs-hook-name-face)
      ("^" t font-lock-negation-char-face)
      ("#}" t haskell-c2hs-hook-pair-face))
    'haskell-c2hs-mode))

(ert-deftest haskell-c2hs-pure-fun-hook ()
  "C2HS pure call hook"
  (check-properties
    '("{#fun pure sin as \"_sin\"#}")
    '(("{#" t haskell-c2hs-hook-pair-face)
      ("fun" "w" haskell-c2hs-hook-name-face)
      ("pure" "w" haskell-c2hs-hook-name-face)
      ("as" "w" haskell-c2hs-hook-name-face)
      ("#}" t haskell-c2hs-hook-pair-face))
    'haskell-c2hs-mode))

(ert-deftest haskell-c2hs-unsafe-fun-hook ()
  "C2HS unsafe fun hook"
  (check-properties
    '("{#fun unsafe sin as ^#}")
    '(("{#" t haskell-c2hs-hook-pair-face)
      ("fun" "w" haskell-c2hs-hook-name-face)
      ("unsafe" "w" haskell-c2hs-hook-name-face)
      ("as" "w" haskell-c2hs-hook-name-face)
      ("^" t font-lock-negation-char-face)
      ("#}" t haskell-c2hs-hook-pair-face))
    'haskell-c2hs-mode))

(ert-deftest haskell-c2hs-get-hook ()
  "C2HS get hook"
  (check-properties
    '("visualGetType              :: Visual -> IO VisualType"
      "visualGetType (Visual vis)  = liftM cToEnum $ {#get Visual->type#} vis")
    '(("visualGetType" "w" haskell-definition-face)
      ("::" t haskell-operator-face)
      ("Visual" "w" haskell-type-face)
      ("->" t haskell-operator-face)
      ("IO" "w" haskell-type-face)
      ("VisualType" "w" haskell-type-face)
      ("visualGetType" "w" haskell-definition-face)
      ("Visual" "w" haskell-constructor-face)
      ("=" t haskell-operator-face)
      ("$" t haskell-operator-face)
      ("{#" t haskell-c2hs-hook-pair-face)
      ("get" "w" haskell-c2hs-hook-name-face)
      ("Visual" "w" haskell-constructor-face)
      ("->" t haskell-operator-face)
      ("#}" t haskell-c2hs-hook-pair-face))
    'haskell-c2hs-mode))

(ert-deftest haskell-c2hs-set-hook ()
  "C2HS set hook"
  (check-properties
    '("{#set sockaddr_in.sin_family#} addr_in (cFromEnum AF_NET)")
    '(("{#" t haskell-c2hs-hook-pair-face)
      ("set" "w" haskell-c2hs-hook-name-face)
      ("#}" t haskell-c2hs-hook-pair-face)
      ("AF_NET" "w_" haskell-constructor-face))
    'haskell-c2hs-mode))

(ert-deftest haskell-c2hs-pointer-hook-1 ()
  "C2HS pointer hook"
  (check-properties
    '("{#pointer *GtkObject as Object foreign newtype#}")
    '(("{#" t haskell-c2hs-hook-pair-face)
      ("pointer" "w" haskell-c2hs-hook-name-face)
      ("*" t haskell-c2hs-hook-name-face)
      ("GtkObject" "w" haskell-constructor-face)
      ("as" "w" haskell-c2hs-hook-name-face)
      ("Object" "w" haskell-constructor-face)
      ("foreign" "w" haskell-c2hs-hook-name-face)
      ("newtype" "w" haskell-c2hs-hook-name-face)
      ("#}" t haskell-c2hs-hook-pair-face))
    'haskell-c2hs-mode))

(ert-deftest haskell-c2hs-pointer-hook-2 ()
  "C2HS pointer hook"
  (check-properties
    '("{# pointer point as PointPtr -> Point #}")
    '(("{#" t haskell-c2hs-hook-pair-face)
      ("pointer" "w" haskell-c2hs-hook-name-face)
      ("PointPtr" "w" haskell-constructor-face)
      ("->" t haskell-operator-face)
      ("Point" "w" haskell-constructor-face)
      ("#}" t haskell-c2hs-hook-pair-face))
   'haskell-c2hs-mode))

(ert-deftest haskell-c2hs-full-pointer-hook ()
  "C2HS full pointer hook"
  (check-properties
    '("{#pointer * foo_t as FooPtr stable -> MkFooPtr nocode#}")
    '(("{#" t haskell-c2hs-hook-pair-face)
      ("pointer" "w" haskell-c2hs-hook-name-face)
      ("*" t haskell-c2hs-hook-name-face)
      ("as" "w" haskell-c2hs-hook-name-face)
      ("FooPtr" "w" haskell-constructor-face)
      ("stable" "w" haskell-c2hs-hook-name-face)
      ("MkFooPtr" "w" haskell-constructor-face)
      ("nocode" "w" haskell-c2hs-hook-name-face)
      ("#}" t haskell-c2hs-hook-pair-face))
    'haskell-c2hs-mode))

(ert-deftest haskell-c2hs-class-hook ()
  "C2HS class hook"
  (check-properties
    '("{# class GtkObjectClass => GtkWidgetClass GtkWidget #}")
    '(("{#" t haskell-c2hs-hook-pair-face)
      ("class" "w" haskell-c2hs-hook-name-face)
      ("GtkObjectClass" "w" haskell-type-face)
      ("=>" t haskell-operator-face)
      ("GtkWidgetClass" "w" haskell-type-face)
      ("GtkWidget" "w" haskell-type-face)
      ("#}" t haskell-c2hs-hook-pair-face))
    'haskell-c2hs-mode))

(ert-deftest haskell-c2hs-alignof-hook ()
  "C2HS alignof hook"
  (check-properties
    '("gIntAlign :: Int"
      "gIntAlign  = {#alignof gint#}")
    '(("gIntAlign" "w" haskell-definition-face)
      ("::" t haskell-operator-face)
      ("Int" "w" haskell-type-face)
      ("gIntAlign" "w" haskell-definition-face)
      ("=" t haskell-operator-face)
      ("{#" t haskell-c2hs-hook-pair-face)
      ("alignof" "w" haskell-c2hs-hook-name-face)
      ("#}" t haskell-c2hs-hook-pair-face))
    'haskell-c2hs-mode))

(ert-deftest haskell-c2hs-offsetof-hook ()
  "C2HS offsetof hook"
  (check-properties
    '("{#offsetof struct_t->somefield#}")
    '(("{#" t haskell-c2hs-hook-pair-face)
      ("offsetof" "w" haskell-c2hs-hook-name-face)
      ("#}" t haskell-c2hs-hook-pair-face))
    'haskell-c2hs-mode))

(ert-deftest haskell-c2hs-const-hook ()
  "C2HS const hook"
  (check-properties
    '("{#const FOO_BAR #}")
    '(("{#" t haskell-c2hs-hook-pair-face)
      ("const" "w" haskell-c2hs-hook-name-face)
      ("#}" t haskell-c2hs-hook-pair-face))
    'haskell-c2hs-mode))

(ert-deftest haskell-c2hs-typedef-hook ()
  "C2HS typedef hook"
  (check-properties
    '("{# typedef size_t CSize #}")
    '(("{#" t haskell-c2hs-hook-pair-face)
      ("typedef" "w" haskell-c2hs-hook-name-face)
      ("CSize" "w" haskell-constructor-face)
      ("#}" t haskell-c2hs-hook-pair-face))
    'haskell-c2hs-mode))

(ert-deftest haskell-c2hs-nongnu-hook ()
  "C2HS nonGNU hook"
  (check-properties
    '("{#nonGNU#}")
    '(("{#" t haskell-c2hs-hook-pair-face)
      ("nonGNU" "w" haskell-c2hs-hook-name-face)
      ("#}" t haskell-c2hs-hook-pair-face))
    'haskell-c2hs-mode))

;; haskell-c2hs-tests.el ends here

