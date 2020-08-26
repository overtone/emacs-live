;;; elixir-mode-font-test.el --- Font highlighting testsuite

;;; Commentary:
;;
;; `elixir-test-with-temp-buffer' and `elixir-test-face-at' are both slightly
;; modified versions of the original at
;; https://github.com/lunaryorn/puppet-mode/blob/master/test/puppet-mode-test.el

;;; Code:

(defun elixir-test-face-at (pos &optional content)
  "Get the face at POS in CONTENT.

If CONTENT is not given, return the face at POS in the current
buffer."
  (if content
      (elixir-test-with-temp-buffer content
        (get-text-property pos 'face))
    (get-text-property pos 'face)))

(ert-deftest elixir-mode-syntax-table/fontify-regex ()
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
   "match = ~r/foo/
match=~r/foo/"
   (should (eq (elixir-test-face-at 1) 'font-lock-variable-name-face))
   (should (eq (elixir-test-face-at 9) 'font-lock-builtin-face))
   (should (eq (elixir-test-face-at 12) 'font-lock-string-face))
   (should (eq (elixir-test-face-at 18) 'font-lock-variable-name-face))
   ;; no face for regex delimiters
   (should (eq (elixir-test-face-at 15) nil))))

(ert-deftest elixir-mode-syntax-table/sigils ()
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
   "asdfg = ~s{Capitalized noncapitalized}"
   (should (eq (elixir-test-face-at 1) 'font-lock-variable-name-face))
   (should (eq (elixir-test-face-at 9) 'font-lock-builtin-face))
   (should (eq (elixir-test-face-at 12) 'font-lock-string-face))
   (should (eq (elixir-test-face-at 26) 'font-lock-string-face))
   ;; no face for regex delimiters
   (should (eq (elixir-test-face-at 38) nil))))

(ert-deftest elixir-mode-syntax-table/fontify-special-macros ()
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
   "__MODULE__
__DIR__
__aliases__
%__MODULE__
&__MODULE__
&abc__DIR__"
   (should (eq (elixir-test-face-at 4) 'font-lock-constant-face))
   (should (eq (elixir-test-face-at 14) 'font-lock-constant-face))
   (should (eq (elixir-test-face-at 24) 'font-lock-constant-face))
   (should (eq (elixir-test-face-at 34) 'font-lock-constant-face))
   (should (eq (elixir-test-face-at 44) 'font-lock-constant-face))
   (should-not (eq (elixir-test-face-at 60) 'font-lock-constant-face))))

(ert-deftest elixir-mode-syntax-table/fontify-modules-and-types ()
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
   "defmodule Application.Behavior do
  use Application.Behaviour
  Stand.Alone.call
  %RuntimeError{message: msg}
  &Enum"
   (should (eq (elixir-test-face-at 1) 'font-lock-keyword-face))
   (should (eq (elixir-test-face-at 11) 'font-lock-type-face))
   (should (eq (elixir-test-face-at 22) 'font-lock-type-face))
   (should (eq (elixir-test-face-at 23) 'font-lock-type-face))
   (should (eq (elixir-test-face-at 32) 'font-lock-keyword-face))
   (should (eq (elixir-test-face-at 37) 'font-lock-keyword-face))
   (should (eq (elixir-test-face-at 41) 'font-lock-type-face))
   (should (eq (elixir-test-face-at 52) 'font-lock-type-face))
   (should (eq (elixir-test-face-at 53) 'font-lock-type-face))
   (should (eq (elixir-test-face-at 68) 'font-lock-type-face))
   (should (eq (elixir-test-face-at 72) 'font-lock-type-face))
   (should (eq (elixir-test-face-at 114) 'font-lock-type-face))
   (should (eq (elixir-test-face-at 117) 'font-lock-type-face))
   ;; no face for function call
   (should (eq (elixir-test-face-at 79) nil))
   (should (eq (elixir-test-face-at 84) 'font-lock-type-face))
   ;; no face for curly braces
   (should (eq (elixir-test-face-at 97) nil))))

(ert-deftest elixir-mode-syntax-table/fontify-regex-with-quote ()
  "https://github.com/elixir-editors/emacs-elixir/issues/23"
  :tags '(fontification syntax-table)
  :expected-result :failed
  (elixir-test-with-temp-buffer
      "~r/\"/
x = 15"
    (should (eq (elixir-test-face-at 8) 'font-lock-variable-name-face))))

(ert-deftest elixir-mode-syntax-table/fontify-regex-with-question/1 ()
  "https://github.com/elixir-editors/emacs-elixir/issues/36"
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
      "~r/^matt: (?<ct>\d+)$/mg
x = 15"
    (should (eq (elixir-test-face-at 4) 'font-lock-string-face))
    (should (eq (elixir-test-face-at 25) 'font-lock-variable-name-face))))

(ert-deftest elixir-mode-syntax-table/fontify-regex-with-question/2 ()
  "https://github.com/elixir-editors/emacs-elixir/issues/29"
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
      "a = \"\" <> \"?\"
x = 15"
    (should (eq (elixir-test-face-at 15) 'font-lock-variable-name-face))))

(ert-deftest elixir-mode-syntax-table/fontify-defguard ()
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
      "defmodule Foo do
defguard is_foo(arg) when arg == true
end"
    (should (eq (elixir-test-face-at 18) 'font-lock-keyword-face))))

(ert-deftest elixir-mode-syntax-table/fontify-defguardp ()
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
      "defmodule Foo do
defguardp is_foo(arg) when arg == true
end"
    (should (eq (elixir-test-face-at 18) 'font-lock-keyword-face))))

(ert-deftest elixir-mode-syntax-table/fontify-function-name/1 ()
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
      "def fooBar do
  :foo
end"
    (should (eq (elixir-test-face-at 5) 'font-lock-function-name-face))
    (should (eq (elixir-test-face-at 8) 'font-lock-function-name-face))))

(ert-deftest elixir-mode-syntax-table/fontify-function-name/2 ()
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
      "def foo? do
  :foo
end"
    (should (eq (elixir-test-face-at 5) 'font-lock-function-name-face))
    (should (eq (elixir-test-face-at 8) 'font-lock-function-name-face))))

(ert-deftest elixir-mode-syntax-table/fontify-function-name/3 ()
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
      "def foo! do
  :foo
end"
    (should (eq (elixir-test-face-at 5) 'font-lock-function-name-face))
    (should (eq (elixir-test-face-at 8) 'font-lock-function-name-face))))

(ert-deftest elixir-mode-syntax-table/fontify-defoverridable/1 ()
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
      "defmodule Foo do
  defmacro __using__(_opts) do
    quote do
      def bar, do: :ok
      defoverridable [bar: 0]
    end
  end
end"
    (should (eq (elixir-test-face-at 91) 'font-lock-keyword-face))))

(ert-deftest elixir-mode-syntax-table/fontify-end-if-the-last-line-in-a-module-is-a-comment ()
  "https://github.com/elixir-editors/emacs-elixir/issues/283"
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
      "defmodule Foo do
  # foo
end"
    (should (eq (elixir-test-face-at 26) 'font-lock-keyword-face))))

(ert-deftest elixir-mode-syntax-table/fontify-heredoc/1 ()
  :tags '(fontification heredoc syntax-table)
  (elixir-test-with-temp-buffer
      "@doc \"\"\""
    (should (eq (elixir-test-face-at 1) 'elixir-attribute-face))
    (should (eq (elixir-test-face-at 2) 'elixir-attribute-face))
    (should (eq (elixir-test-face-at 6) 'font-lock-doc-face))))

(ert-deftest elixir-mode-syntax-table/fontify-heredoc/2 ()
  :tags '(fontification heredoc syntax-table)
  (elixir-test-with-temp-buffer
      "@moduledoc \"\"\""
    (should (eq (elixir-test-face-at 1) 'elixir-attribute-face))
    (should (eq (elixir-test-face-at 2) 'elixir-attribute-face))
    (should (eq (elixir-test-face-at 12) 'font-lock-doc-face))))

(ert-deftest elixir-mode-syntax-table/fontify-heredoc/3 ()
  :tags '(fontification heredoc syntax-table)
  (elixir-test-with-temp-buffer
      "~s\"\"\""
    (should (eq (elixir-test-face-at 1) 'font-lock-builtin-face))
    (should (eq (elixir-test-face-at 2) 'font-lock-builtin-face))
    (should (eq (elixir-test-face-at 3) 'font-lock-string-face))))

(ert-deftest elixir-mode-syntax-table/fontify-heredoc/4 ()
  :tags '(fontification heredoc syntax-table)
  (elixir-test-with-temp-buffer
      "@typedoc \"\"\""
    (should (eq (elixir-test-face-at 1) 'elixir-attribute-face))
    (should (eq (elixir-test-face-at 2) 'elixir-attribute-face))
    (should (eq (elixir-test-face-at 10) 'font-lock-doc-face))))

(ert-deftest elixir-mode-syntax-table/fontify-atoms ()
  :tags '(fontification atom syntax-table)
  (elixir-test-with-temp-buffer
      ":oriole
:andale
:ms2pid
:CapitalizedAtom
true
false
nil
true_false_nil
:insert!
:insert@
:insert?
"
    (should (eq (elixir-test-face-at 3) 'elixir-atom-face))
    (should (eq (elixir-test-face-at 5) 'elixir-atom-face))
    (should (eq (elixir-test-face-at 10) 'elixir-atom-face))
    (should (eq (elixir-test-face-at 13) 'elixir-atom-face))
    (should (eq (elixir-test-face-at 18) 'elixir-atom-face))
    (should (eq (elixir-test-face-at 23) 'elixir-atom-face))
    (should (eq (elixir-test-face-at 26) 'elixir-atom-face))
    (should (eq (elixir-test-face-at 43) 'elixir-atom-face))
    (should (eq (elixir-test-face-at 48) 'elixir-atom-face))
    (should (eq (elixir-test-face-at 54) 'elixir-atom-face))
    (should-not (eq (elixir-test-face-at 57) 'elixir-atom-face))
    (should (eq (elixir-test-face-at 74) 'elixir-atom-face))
    (should (eq (elixir-test-face-at 82) 'elixir-atom-face))
    (should (eq (elixir-test-face-at 97) 'elixir-atom-face))))

(ert-deftest elixir-mode-syntax-table/fontify-map-keys ()
  :tags '(fontification map syntax-table)
  (elixir-test-with-temp-buffer
      "%{a: 1, b: 2}"
    (should (eq (elixir-test-face-at 3) 'elixir-atom-face))
    (should (eq (elixir-test-face-at 4) 'elixir-atom-face))
    (should (eq (elixir-test-face-at 9) 'elixir-atom-face))
    (should (eq (elixir-test-face-at 10) 'elixir-atom-face)))

  ;; https://github.com/elixir-editors/emacs-elixir/issues/320
  (elixir-test-with-temp-buffer
   "<<foo::bar>>"
   (should-not (eq (elixir-test-face-at 3) 'elixir-atom-face))))

(ert-deftest elixir-mode-syntax-table/fontify-interpolation ()
  :tags '(fontification interpolation syntax-table)
  (elixir-test-with-temp-buffer
      "\"#{1 + 2} is 3.\""
    (should (eq (elixir-test-face-at 1) 'font-lock-string-face))
    (should (eq (elixir-test-face-at 11) 'font-lock-string-face))))

(ert-deftest elixir-mode-syntax-table/fontify-continuation-lines-assignment ()
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
   "some_var =
some_expr"
   (should (eq (elixir-test-face-at 1) 'font-lock-variable-name-face))))

(ert-deftest elixir-mode-syntax-table/dont-fontify-equal-match ()
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
   "this == that"
   (should-not (eq (elixir-test-face-at 2) 'font-lock-variable-name-face))))

(ert-deftest elixir-mode-syntax-table/fontify-triple-quoted-string ()
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
      "\"\"\"foo\"bar\"baz #{1 + 2} is 3.\"\"\""
    (should (eq (elixir-test-face-at 1) 'font-lock-string-face))
    (should (eq (elixir-test-face-at 5) 'font-lock-string-face))
    (should (eq (elixir-test-face-at 19) 'font-lock-variable-name-face))
    (should (eq (elixir-test-face-at 31) 'font-lock-string-face))))

(ert-deftest elixir-mode-syntax-table/fontify-atom-in-pattern-match ()
  :tags '(fontification atom syntax-table)
  (elixir-test-with-temp-buffer
   ":any = into_to_type(type)
:another=into_to_type(type)"
   (should (eq (elixir-test-face-at 3) 'elixir-atom-face))))

(ert-deftest elixir-mode-syntax-table/fontify-assignment-with-pattern/1 ()
  :expected-result :failed
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
   "{x, y} = some_expr"
   (should (eq (elixir-test-face-at 2) 'font-lock-variable-name-face))
   (should (eq (elixir-test-face-at 5) 'font-lock-variable-name-face))))

(ert-deftest elixir-mode-syntax-table/fontify-assignment-with-pattern/2 ()
  :expected-result :failed
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
   "[h|t] = some_expr"
   (should (eq (elixir-test-face-at 2) 'font-lock-variable-name-face))
   (should (eq (elixir-test-face-at 4) 'font-lock-variable-name-face))))

(ert-deftest elixir-mode-syntax-table/fontify-assignment-with-singleton ()
  "https://github.com/elixir-editors/emacs-elixir/issues/245"
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
   "true_false_nil = 1"
   (should (eq (elixir-test-face-at 1) 'font-lock-variable-name-face))
   (should (eq (elixir-test-face-at 6) 'font-lock-variable-name-face))
   (should (eq (elixir-test-face-at 12) 'font-lock-variable-name-face))))

(ert-deftest elixir-mode-syntax-table/fontify-keyword-after-dot ()
  "https://github.com/elixir-editors/emacs-elixir/issues/250"
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
   "Mix.raise
raise
Mix.def foo
Mix.import
import
Mix.after
after
Mix.when
when"
   (should-not (eq (elixir-test-face-at 5) 'font-lock-keyword-face))
   (should (eq (elixir-test-face-at 11) 'font-lock-keyword-face))
   (should-not (eq (elixir-test-face-at 21) 'font-lock-keyword-face))
   (should-not (eq (elixir-test-face-at 25) 'font-lock-function-name-face))
   (should-not (eq (elixir-test-face-at 33) 'font-lock-keyword-face))
   (should (eq (elixir-test-face-at 40) 'font-lock-keyword-face))

   (should-not (eq (elixir-test-face-at 51) 'font-lock-keyword-face))
   (should (eq (elixir-test-face-at 57) 'font-lock-keyword-face))
   (should-not (eq (elixir-test-face-at 67) 'font-lock-keyword-face))
   (should (eq (elixir-test-face-at 72) 'font-lock-keyword-face))))

(ert-deftest elixir-mode-syntax-table/highlight-send ()
  "Highlight send as a keyword"
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
   "defmodule Foo do
  def bar(pid) do
    send pid, :baz
  end
end"
   (should (eq (elixir-test-face-at 40) 'font-lock-keyword-face))))

(ert-deftest elixir-mode-syntax-table/highlight-with ()
  "Highlight with as a keyword"
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
   "defmodule Foo do
  def bar(opts) do
    with(
      {:ok, width} <- Map.fetch(opts, :width),
      {:ok, height} <- Map.fetch(opts, :height),
      do: {:ok, width * height}
    )
  end
end"
   (should (eq (elixir-test-face-at 41) 'font-lock-keyword-face))))

(ert-deftest elixir-mode-syntax-table/string-interpolation-in-words-list ()
  "https://github.com/elixir-editors/emacs-elixir/issues/263"
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
   "~w(SADD users #{user_id})"
   (should (eq (elixir-test-face-at 4) 'font-lock-string-face))

   (should-not (eq (elixir-test-face-at 15) 'font-lock-comment-face))
   (should-not (eq (elixir-test-face-at 17) 'font-lock-comment-face))
   (should-not (eq (elixir-test-face-at 25) 'font-lock-comment-face))))

(ert-deftest elixir-mode-syntax-table/quotes-in-sigils ()
  "https://github.com/elixir-editors/emacs-elixir/issues/265"
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
   "~s/\"/
~r|'|
~c\"'\"
~w'\"'
~s(\")
~r[\"]
~c{\"}
~w<\">
~s\"\"\"
foo
\"\"\"
~D(\")
~N(\")
~T(\")"
   (should-not (eq (elixir-test-face-at 5) 'font-lock-string-face))   ; ~s//

   (should-not (eq (elixir-test-face-at 7) 'font-lock-string-face))   ; ~r||
   (should     (eq (elixir-test-face-at 7) 'font-lock-builtin-face))
   (should-not (eq (elixir-test-face-at 11) 'font-lock-string-face))

   (should-not (eq (elixir-test-face-at 13) 'font-lock-string-face))  ; ~c""
   (should     (eq (elixir-test-face-at 13) 'font-lock-builtin-face))
   (should-not (eq (elixir-test-face-at 17) 'font-lock-string-face))

   (should-not (eq (elixir-test-face-at 19) 'font-lock-string-face))  ; ~w''
   (should     (eq (elixir-test-face-at 19) 'font-lock-builtin-face))
   (should-not (eq (elixir-test-face-at 23) 'font-lock-string-face))

   (should-not (eq (elixir-test-face-at 25) 'font-lock-string-face))  ; ~s()
   (should     (eq (elixir-test-face-at 25) 'font-lock-builtin-face)) ; ~s()
   (should-not (eq (elixir-test-face-at 29) 'font-lock-string-face))

   (should-not (eq (elixir-test-face-at 31) 'font-lock-string-face))  ; ~r[]
   (should     (eq (elixir-test-face-at 31) 'font-lock-builtin-face))
   (should-not (eq (elixir-test-face-at 35) 'font-lock-string-face))

   (should-not (eq (elixir-test-face-at 37) 'font-lock-string-face))  ; ~c{}
   (should     (eq (elixir-test-face-at 37) 'font-lock-builtin-face))
   (should-not (eq (elixir-test-face-at 41) 'font-lock-string-face))

   (should-not (eq (elixir-test-face-at 43) 'font-lock-string-face))  ; ~w<>
   (should     (eq (elixir-test-face-at 43) 'font-lock-builtin-face))
   (should-not (eq (elixir-test-face-at 47) 'font-lock-string-face))

   (should     (eq (elixir-test-face-at 51) 'font-lock-string-face))  ; ~s""" """
   (should     (eq (elixir-test-face-at 52) 'font-lock-string-face))
   (should     (eq (elixir-test-face-at 53) 'font-lock-string-face))
   (should     (eq (elixir-test-face-at 55) 'font-lock-string-face))

   (should     (eq (elixir-test-face-at 66) 'font-lock-string-face)) ; ~D()

   (should     (eq (elixir-test-face-at 72) 'font-lock-string-face)) ; ~N()

   (should     (eq (elixir-test-face-at 78) 'font-lock-string-face)))) ; ~T()

(ert-deftest elixir-mode-syntax-table/hashmark-in-sigils ()
  "Don't treat hashmark in sigils as comment"
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
   "~s(# foo)"
   (should-not (eq (elixir-test-face-at 4) 'font-lock-comment-face))
   (should     (eq (elixir-test-face-at 6) 'font-lock-string-face))))

(ert-deftest elixir-mode-syntax-table/highlight-modules-after-pipe ()
  "Module names must be hightligthed if preceded by a pipe character."
  (elixir-test-with-temp-buffer
   "|List"
   (should-not (eq (elixir-test-face-at 1) 'font-lock-type-face))
   (should     (eq (elixir-test-face-at 3) 'font-lock-type-face)))
  (elixir-test-with-temp-buffer
   "[req_code(op_name)|Enum.reverse(data)]"
   (should-not (eq (elixir-test-face-at 19) 'font-lock-type-face))
   (should     (eq (elixir-test-face-at 21) 'font-lock-type-face))))

(ert-deftest elixir-mode-syntax-table/sigils-in-string ()
  "https://github.com/elixir-editors/emacs-elixir/issues/275"
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
   "@one 1
@two \"~s\"
@three :tre
"
   (should-not (eq (elixir-test-face-at 18) 'font-lock-string-face))
   (should     (eq (elixir-test-face-at 19) 'elixir-attribute-face))
   (should-not (eq (elixir-test-face-at 25) 'font-lock-string-face))
   (should     (eq (elixir-test-face-at 26) 'elixir-atom-face))))

(ert-deftest elixir-mode-syntax-table/sigil-triple-quote ()
  "https://github.com/elixir-editors/emacs-elixir/issues/286"
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
   "defmodule IEx do
  @moduledoc ~S\"\"\"
  Elixir's interactive shell.
\"\"\"
end
"
   (should (eq (elixir-test-face-at 33) 'font-lock-string-face))))

(ert-deftest elixir-mode-syntax-table/single-triple-single-quote ()
  "https://github.com/elixir-editors/emacs-elixir/issues/309"
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
   "defmodule Module do
  @moduledoc ~S'''
  foo's \"bar\"
'''"
   (should (eq (elixir-test-face-at 34) 'font-lock-builtin-face)) ;; ~S
   (should (eq (elixir-test-face-at 35) 'font-lock-builtin-face))
   (should (eq (elixir-test-face-at 36) 'font-lock-string-face))  ;; '''
   (should (eq (elixir-test-face-at 37) 'font-lock-string-face))
   (should (eq (elixir-test-face-at 38) 'font-lock-string-face))
   (should (eq (elixir-test-face-at 54) 'font-lock-string-face))  ;; '''
   (should (eq (elixir-test-face-at 55) 'font-lock-string-face))
   (should (eq (elixir-test-face-at 56) 'font-lock-string-face))))

(ert-deftest elixir-mode-syntax-table/comment-out-ignored-var ()
  "https://github.com/elixir-editors/emacs-elixir/issues/292"
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
   "variable
    _var
    _x
    _x!
    _x?
    _
    __MODULE__
"
   (should (eq (elixir-test-face-at 4) nil))
   (should (eq (elixir-test-face-at 14) 'font-lock-comment-face))
   (should (eq (elixir-test-face-at 15) 'font-lock-comment-face))
   (should (eq (elixir-test-face-at 23) 'font-lock-comment-face))
   (should (eq (elixir-test-face-at 24) 'font-lock-comment-face))
   (should (eq (elixir-test-face-at 30) 'font-lock-comment-face))
   (should (eq (elixir-test-face-at 32) 'font-lock-comment-face))
   (should (eq (elixir-test-face-at 38) 'font-lock-comment-face))
   (should (eq (elixir-test-face-at 40) 'font-lock-comment-face))
   (should (eq (elixir-test-face-at 46) 'font-lock-constant-face))
   (should (eq (elixir-test-face-at 46) 'font-lock-constant-face))
   (should (eq (elixir-test-face-at 52) 'font-lock-constant-face))
   (should (eq (elixir-test-face-at 53) 'font-lock-constant-face))
   (should (eq (elixir-test-face-at 55) 'font-lock-constant-face))))

(ert-deftest elixir-mode-syntax-table/escaped-sigil-delimiter ()
  "https://github.com/elixir-editors/emacs-elixir/issues/302"
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
   "def player_id(video) do
  ~r/^.*(?:youtu.be\\/|v\\/|e\\/|u\\/\\w+\\/|embed\\/|v=)(?<id>[^#\\&\\?]*).*/
  |> Regex.named_captures(video.url)
  |> get_in([\"id\"])
end
"
   (should (eq (elixir-test-face-at 45) 'font-lock-string-face)) ;; escaped '/'
   (should (eq (elixir-test-face-at 84) 'font-lock-string-face)) ;; comment mark '#'

   (elixir-test-with-temp-buffer
    "~B/\\//
~C[\\]]
~R{\\}}
~C(\\))
~b|\\||
~c\"\\\"\"
~r'\\''
~s<\\>>"
    (should (eq (elixir-test-face-at 5) 'font-lock-string-face)) ;; '/'
    (should (eq (elixir-test-face-at 12) 'font-lock-string-face)) ;; '[]'
    (should (eq (elixir-test-face-at 19) 'font-lock-string-face)) ;; '{}'
    (should (eq (elixir-test-face-at 26) 'font-lock-string-face)) ;; '()'
    (should (eq (elixir-test-face-at 33) 'font-lock-string-face)) ;; '|'
    (should (eq (elixir-test-face-at 40) 'font-lock-string-face)) ;; '"'
    (should (eq (elixir-test-face-at 47) 'font-lock-string-face)) ;; '\''
    (should (eq (elixir-test-face-at 53) 'font-lock-string-face)) ;; '<>'
    )))

(ert-deftest elixir-mode-syntax-table/question-quote ()
  "https://github.com/elixir-editors/emacs-elixir/issues/185"
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
   "\"\\\"foo\\\"\" |> String.strip(?\")"
   (should-not (eq (elixir-test-face-at 28) 'font-lock-string-face)))

  (elixir-test-with-temp-buffer
   "\"\\\"foo\\\"\" |> String.strip(?')"
   (should-not (eq (elixir-test-face-at 28) 'font-lock-string-face))))

(ert-deftest elixir-mode-syntax-table/ignored-variables-in-pattern-match ()
  "https://github.com/elixir-editors/emacs-elixir/issues/361"
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
   "(_1_day = 86_400)
_1_day"
   (should (eq (elixir-test-face-at 2) 'font-lock-comment-face))
   (should (eq (elixir-test-face-at 19) 'font-lock-comment-face))))

(ert-deftest elixir-mode-in-docstring ()
  "https://github.com/elixir-editors/emacs-elixir/issues/355"
  :tags 'fontification
  (elixir-test-with-temp-buffer
      "# https://github.com/elixir-editors/emacs-elixir/issues/355

@moduledoc \"\"\"
Everything in here should be gray, including the @moduledoc and triple-quotes
\"\"\"

@doc \"\"\"
Everything in here should be gray, including the @doc and triple-quotes
\"\"\""
    ;; (switch-to-buffer (current-buffer))
    (search-forward "Everything")
    (should (elixir--docstring-p))
    (search-forward "Everything")
    (should (elixir--docstring-p))))

(ert-deftest elixir-mode-docstring-face ()
  "https://github.com/elixir-editors/emacs-elixir/issues/355"
  :tags 'fontification
  (elixir-test-with-temp-buffer
      "# https://github.com/elixir-editors/emacs-elixir/issues/355

@moduledoc \"\"\"
Everything in here should be gray, including the @moduledoc and triple-quotes
\"\"\"

@doc \"\"\"
Everything in here should be gray, including the @doc and triple-quotes
\"\"\""
    (switch-to-buffer (current-buffer))
    (search-forward "Everything")
    (should (eq 'font-lock-doc-face (get-char-property (point) 'face)))
    (search-forward "Everything")
    (should (eq 'font-lock-doc-face (get-char-property (point) 'face)))))

(provide 'elixir-mode-font-test)

;;; elixir-mode-font-test.el ends here
