;;; elixir-mode-indentation-test.el --- Indentation testsuite

;;; Commentary:
;;
;; Expected test failures indicates that the code tested by that test case is
;; indeed broken. My intention is that while working on a specific problem,
;; the failure expectation will be removed so that we know when the test case
;; passes.

;;; Code:

(elixir-def-indentation-test indent-use-dot-module-newline
                             (:tags '(indentation))
"
defmodule Foo do
use GenServer.Behaviour

def foobar do
if true, do: IO.puts \"yay\"
end
end
"

"
defmodule Foo do
  use GenServer.Behaviour

  def foobar do
    if true, do: IO.puts \"yay\"
  end
end
")

(elixir-def-indentation-test indent-use-dot-module
                             (:tags '(indentation))
"
defmodule Foo do
use GenServer.Behaviour
def foobar do
if true, do: IO.puts \"yay\"
end
end
"

"
defmodule Foo do
  use GenServer.Behaviour
  def foobar do
    if true, do: IO.puts \"yay\"
  end
end
")

(elixir-def-indentation-test indent-do-blocks
                             (:tags '(indentation))
"
defmodule Foo do
def foobar do
if true, do: IO.puts \"yay\"
20
end
end
"

"
defmodule Foo do
  def foobar do
    if true, do: IO.puts \"yay\"
    20
  end
end
")

(elixir-def-indentation-test indent-do-blocks-after-linebreak-two
                             (:tags '(indentation))
"
defmodule FooBar do
def foo do
if true, do: IO.puts \"yay\"
20
end

def bar do
if true, do: IO.puts \"yay\"
20
end
end
"

"
defmodule FooBar do
  def foo do
    if true, do: IO.puts \"yay\"
    20
  end

  def bar do
    if true, do: IO.puts \"yay\"
    20
  end
end
")

(elixir-def-indentation-test indent-do-blocks-after-linebreak-three
                             (:tags '(indentation))
"
defmodule FooBar do
def foo do
if true, do: IO.puts \"yay\"
20
end

def bar do
if true, do: IO.puts \"yay\"
20
end

def baz do
if true, do: IO.puts \"yay\"
20
end
end
"

"
defmodule FooBar do
  def foo do
    if true, do: IO.puts \"yay\"
    20
  end

  def bar do
    if true, do: IO.puts \"yay\"
    20
  end

  def baz do
    if true, do: IO.puts \"yay\"
    20
  end
end
")

(elixir-def-indentation-test indent-do-blocks-with-space-after-inline
                             (:tags '(indentation))
"
defmodule Foo do
def foobar do
if true, do: IO.puts \"yay\"

20
end
end
"

"
defmodule Foo do
  def foobar do
    if true, do: IO.puts \"yay\"

    20
  end
end
")

(elixir-def-indentation-test indent-after-empty-line
                             (:tags '(indentation))
"
def foo do
a = 2

b = a + 3

c = a * b
end
"

"
def foo do
  a = 2

  b = a + 3

  c = a * b
end
")

(elixir-def-indentation-test indent-function-calls-without-parens
                             (:tags '(indentation))
"
test \"foo\" do
assert true, \"should be true\"
assert !false, \"should still be true\"
end
"

"
test \"foo\" do
  assert true, \"should be true\"
  assert !false, \"should still be true\"
end
")

(elixir-def-indentation-test indent-records-correctly
                             (:tags '(indentation))
"
defmodule MyModule do
require Record
Record.defrecord :money, [:currency_unit, :amount]

Record.defrecord :animal, [:species, :name]
end
"

"
defmodule MyModule do
  require Record
  Record.defrecord :money, [:currency_unit, :amount]

  Record.defrecord :animal, [:species, :name]
end
")

(elixir-def-indentation-test indent-continuation-lines
                             (:tags '(indentation))
"
def foo do
has_something(x) &&
  has_something(y) ||
  has_something(z)
end
"

"
def foo do
  has_something(x) &&
    has_something(y) ||
    has_something(z)
end
")

(elixir-def-indentation-test indent-continuation-lines-with-comments/1
                             (:tags '(indentation))
"
has_something(x) &&  # foo
has_something(y) ||
has_something(z)
"

"
has_something(x) &&  # foo
  has_something(y) ||
  has_something(z)
")

(elixir-def-indentation-test indent-continuation-lines-with-comments/2
                             (:tags '(indentation))
"
has_something(x) &&
has_something(y) || # foo
has_something(z)
"

"
has_something(x) &&
  has_something(y) || # foo
  has_something(z)
")

(elixir-def-indentation-test indent-continuation-lines-with-comments/3
                             (:tags '(indentation))
"
def str(s, sub, start_pos, end_pos) when is_binary(s) and is_binary(sub) do # and start_pos <= end_pos do
                                                                             len = end_pos-start_pos
end
"

"
def str(s, sub, start_pos, end_pos) when is_binary(s) and is_binary(sub) do # and start_pos <= end_pos do
  len = end_pos-start_pos
end
")

(elixir-def-indentation-test indent-continuation-lines-assignment
                             (:tags '(indentation))
"
some_var =
some_expr
"

"
some_var =
  some_expr
")

(elixir-def-indentation-test indent-continuation-lines-assignment/2
                             (:tags '(indentation))
"
next_fun =
        case raw do
 true  -> &IO.each_binstream(&1, line_or_bytes)
      false -> &IO.each_stream(&1, line_or_bytes)
       end
"

"
next_fun =
  case raw do
    true  -> &IO.each_binstream(&1, line_or_bytes)
    false -> &IO.each_stream(&1, line_or_bytes)
  end
")

(elixir-def-indentation-test indent-continuation-lines-assignment/3
                             (:expected-result :failed :tags '(indentation))
"
start_fun =
    fn ->
 case :file.open(path, modes) do
  {:ok, device}    -> device
{:error, reason} ->
raise File.Error, reason: reason, action: \"stream\", path: path
end
end
"

"
start_fun =
  fn ->
    case :file.open(path, modes) do
      {:ok, device}    -> device
      {:error, reason} ->
        raise File.Error, reason: reason, action: \"stream\", path: path
    end
  end
")


(elixir-def-indentation-test indent-last-commented-line
                             (:tags '(indentation))
"
defmodule Foo do
def bar do
2
end

# last line
end
"

"
defmodule Foo do
  def bar do
    2
  end

  # last line
end
")

(elixir-def-indentation-test indent-if
                             (:tags '(indentation))
"
if condition do
yes
end
"

"
if condition do
  yes
end
")

(elixir-def-indentation-test indent-if-else
                             (:tags '(indentation))
"
if condition do
yes
else
no
end
"

"
if condition do
  yes
else
  no
end
")

(elixir-def-indentation-test indent-if-else/2
                             (:tags '(indentation))
"
if condition do
:foo
else
if condition, do: :bar
end
"

"
if condition do
  :foo
else
  if condition, do: :bar
end
")

(elixir-def-indentation-test indent-tuple-after-if-else
                             (:tags '(indentation))
"
if foo do
 :ok
else
{:tuple}
end
"

"
if foo do
  :ok
else
  {:tuple}
end
")

(elixir-def-indentation-test indent-non-finished-one-line-if-else
                             (:tags '(indentation))
"
if condition,
do: :foo,
else: :bar
"

"
if condition,
  do: :foo,
  else: :bar
")

(elixir-def-indentation-test indent-if-when-condition-is-a-named-function-on-a-module
                             (:tags '(indentation))
"
defmodule Whois do
  def lookup2(domain) do
    if Server.for(domain) do
          :ok
        else
          :error
        end
  end
end
"

"
defmodule Whois do
  def lookup2(domain) do
    if Server.for(domain) do
      :ok
    else
      :error
    end
  end
end
")

(elixir-def-indentation-test indent-try
                             (:tags '(indentation))
"
try do
foo
bar
end
"

"
try do
  foo
  bar
end
")

(elixir-def-indentation-test indent-try/after
                             (:tags '(indentation))
"
try do
foo
bar
after
after_everything()
post_that()
end
"

"
try do
  foo
  bar
after
  after_everything()
  post_that()
end
")

(elixir-def-indentation-test indent-try/catch/after
                             (:tags '(indentation))
"
try do
foo
bar
catch
baz ->
nope
[yeah] ->
maybe
after
after_everything()
post_that()
end
"

"
try do
  foo
  bar
catch
  baz ->
    nope
  [yeah] ->
    maybe
after
  after_everything()
  post_that()
end
")

(elixir-def-indentation-test indent-try/rescue/1
                             (:tags '(indentation))
"
try do
raise 'some error'
rescue
RuntimeError -> 'rescued a runtime error'
end
"

"
try do
  raise 'some error'
rescue
  RuntimeError -> 'rescued a runtime error'
end
")

(elixir-def-indentation-test indent-try/rescue/2
                             (:tags '(indentation))
"
try do
raise 'some error'
rescue
x in [RuntimeError] ->
x.message
end
"

"
try do
  raise 'some error'
rescue
  x in [RuntimeError] ->
    x.message
end
")

(elixir-def-indentation-test indent-block-inside-fn-match
                             (:tags '(indentation))
"
defp into(stream, device, raw) do
 fn
   :ok, {:cont, x} ->
  case raw do
true  -> IO.binwrite(device, x)
false -> IO.write(device, x)
end
:ok, _ -> stream
end
end
"

"
defp into(stream, device, raw) do
  fn
    :ok, {:cont, x} ->
      case raw do
        true  -> IO.binwrite(device, x)
        false -> IO.write(device, x)
      end
    :ok, _ -> stream
  end
end
")

(elixir-def-indentation-test indent-fn-in-assignment
                             (:tags '(indentation))
"
f = fn x, y ->
x + y
end
"

"
f = fn x, y ->
  x + y
end
")

(elixir-def-indentation-test indent-fn-as-arguments
                             (:tags '(indentation))
"
Enum.map 1..10, fn x ->
x + 1
end
"

"
Enum.map 1..10, fn x ->
  x + 1
end
")

(elixir-def-indentation-test indent-list-argument-continuation-lines-nicely
                             (:tags '(indentation))
"
to_process = [27, 33, 35, 11, 36, 29, 18, 37, 21, 31, 19, 10, 14, 30,
15, 17, 23, 28, 25, 34, 22, 20, 13, 16, 32, 12, 26, 24]
"

"
to_process = [27, 33, 35, 11, 36, 29, 18, 37, 21, 31, 19, 10, 14, 30,
              15, 17, 23, 28, 25, 34, 22, 20, 13, 16, 32, 12, 26, 24]
")

(elixir-def-indentation-test indent-nested-fn
                             (:tags '(indentation))
"
defmodule FooModule do
def foo do
x = fn(a, b) -> a + b end
end
end
"

"
defmodule FooModule do
  def foo do
    x = fn(a, b) -> a + b end
  end
end
")

(elixir-def-indentation-test indent-list-of-floats-aligns
                             (:tags '(indentation))
"
[1.2,
3.4]
"

"
[1.2,
 3.4]
")

(elixir-def-indentation-test indent-after-operator
                             (:tags '(indentation))
"
defmodule Banana do
def start do
a = \"\" <> \"?\"

case bar do
z -> 1
end

case foo do
?x -> x
end

end
end
"

"
defmodule Banana do
  def start do
    a = \"\" <> \"?\"

    case bar do
      z -> 1
    end

    case foo do
      ?x -> x
    end

  end
end
")

(elixir-def-indentation-test nested-modules
                             (:tags '(indentation))
"
defmodule Mod1 do
  defmodule Mod1a do
    def start do
      foo()
    end
  end
end
"

"
defmodule Mod1 do
  defmodule Mod1a do
    def start do
      foo()
    end
  end
end
")

(elixir-def-indentation-test cond-comment
                             (:tags '(indentation))
"
def foo() do
cond do
yadda ->
:ok
badda -> # comment throws this off
:what
end
end
"

"
def foo() do
  cond do
    yadda ->
      :ok
    badda -> # comment throws this off
      :what
  end
end
")

(elixir-def-indentation-test cond-within-with
                             (:expected-result :failed :tags '(indentation))
                             ;; https://github.com/elixir-editors/emacs-elixir/issues/319
"
with(
  foo <-
  cond do
    bar ->
      bar

    baz ->
      baz
  end,
  do: :ok
)
"
"
with(
  foo <-
    cond do
      bar ->
        bar

      baz ->
        baz
    end,
  do: :ok
)
")

(elixir-def-indentation-test with-statement
			     (:tags '(indentation))
"
defmodule Foo do

def bar do
with {:ok, width} <- Map.fetch(opts, :width),
{:ok, height} <- Map.fetch(opts, :height) do
{:ok, width * height}
else
:error ->
{:error, :wrong_data}
end
end

end
"

"
defmodule Foo do

  def bar do
    with {:ok, width} <- Map.fetch(opts, :width),
         {:ok, height} <- Map.fetch(opts, :height) do
      {:ok, width * height}
    else
      :error ->
        {:error, :wrong_data}
    end
  end

end
")

(elixir-def-indentation-test with-statement/2
			     (:tags '(indentation))
"
defmodule Foo do
  def bar do
    with {:ok, width} <- Map.fetch(opts, :width),
         {:ok, height} <- Map.fetch(opts, :height),
      do: {:ok, width * height}

    with({:ok, width} <- Map.fetch(opts, :width),
         {:ok, height} <- Map.fetch(opts, :height),
      do: {:ok, width * height})
  end
end
"

"
defmodule Foo do
  def bar do
    with {:ok, width} <- Map.fetch(opts, :width),
         {:ok, height} <- Map.fetch(opts, :height),
      do: {:ok, width * height}

    with({:ok, width} <- Map.fetch(opts, :width),
         {:ok, height} <- Map.fetch(opts, :height),
      do: {:ok, width * height})
  end
end
")

(elixir-def-indentation-test indent-heredoc
                             (:tags '(indentation))
"
defmodule Foo do
@doc \"\"\"
this is a heredoc string

\"\"\"
def convert do
x = 15
end
end
"

"
defmodule Foo do
  @doc \"\"\"
  this is a heredoc string

  \"\"\"
  def convert do
    x = 15
  end
end
")

(elixir-def-indentation-test indent-heredoc/2
                             (:tags '(indentation))
"
defmodule Foo do
@doc \"\"\"
      this is a heredoc string

     \"\"\"
  def convert do
       x = 15
  end

defmodule Bar do
    @moduledoc \"\"\"
 this is a heredoc string

last line
    \"\"\"
  end
end
"

"
defmodule Foo do
  @doc \"\"\"
  this is a heredoc string

  \"\"\"
  def convert do
    x = 15
  end

  defmodule Bar do
    @moduledoc \"\"\"
    this is a heredoc string

    last line
    \"\"\"
  end
end
")

(elixir-def-indentation-test indent-heredoc/3
                             (:tags '(indentation))
"
def foo() do
  \"\"\"
heredoc
\"\"\"
end

for x <- [1, 2, 3] do
\"\"\"
heredoc
  \"\"\"
end
"

"
def foo() do
  \"\"\"
  heredoc
  \"\"\"
end

for x <- [1, 2, 3] do
  \"\"\"
  heredoc
  \"\"\"
end
")

(elixir-def-indentation-test indent-multiline-pipes-after-call
                             (:tags '(indentation))
"
  some_string
      |> String.downcase
   |> String.strip
"

"
some_string
|> String.downcase
|> String.strip
")

(elixir-def-indentation-test indent-multiline-on-the-right-of-pattern-match
                             (:tags '(indentation))
"
sanitized_string =
        some_string
|> String.downcase
             |> String.strip
"

"
sanitized_string =
  some_string
  |> String.downcase
  |> String.strip
")

(elixir-def-indentation-test indent-pipes-after-assignment
                             (:tags '(indentation))
"
def foo(x) do
  a = x
       |> Enum.reverse
end
"

"
def foo(x) do
  a = x
  |> Enum.reverse
end
")

(elixir-def-indentation-test indent-pipes-inside-blocks
                             (:tags '(indentation))
"
defmodule Foo do
def bar do
 baz =
         [1,2,3,4,5,6,7,8,9]
  |> Enum.reverse
                  |> Enum.filter(&(&1 > 5))
  end
end
"

"
defmodule Foo do
  def bar do
    baz =
      [1,2,3,4,5,6,7,8,9]
      |> Enum.reverse
      |> Enum.filter(&(&1 > 5))
  end
end
")

(elixir-def-indentation-test indent-inside-parens
                             (:tags '(indentation))
"
x = do_something(
:foo,
:bar
)
"

"
x = do_something(
  :foo,
  :bar
)
")

(elixir-def-indentation-test indent-inside-parens/2
                             (:tags '(indentation))
"
x = do_something(:foo,
                    :bar)
"

"
x = do_something(:foo,
  :bar)
")

(elixir-def-indentation-test indent-inside-parens/3
                             (:tags '(indentation))
"
x = do_something(:foo, fn (arg) ->
                         do_another(arg)
                       end)
"

"
x = do_something(:foo, fn (arg) ->
  do_another(arg)
end)
")

(elixir-def-indentation-test indent-inside-parens/4
                             (:tags '(indentation))
"
defmodule Something do
def something do
x = do_something(:foo, fn (arg) ->
                         do_another(arg)
                       end)
end
end
"

"
defmodule Something do
  def something do
    x = do_something(:foo, fn (arg) ->
      do_another(arg)
    end)
  end
end
")

(elixir-def-indentation-test indent-inside-parens/5
                             (:tags '(indentation))
"
defmodule IndentPlayground do
def my_func(arr) do
 Enum.map(arr, fn(x) ->
  x * 2
end)
   #back here
end
end
"

"
defmodule IndentPlayground do
  def my_func(arr) do
    Enum.map(arr, fn(x) ->
      x * 2
    end)
    #back here
  end
end
")

(elixir-def-indentation-test indent-lone-keyword
                 (:tags '(indentation))
"
def foo do #comment
  :bar
end
"

"
def foo do #comment
  :bar
end
")

(elixir-def-indentation-test indent-single-line-match
                 (:tags '(indentation))
"
case x do
a -> b
c -> d
end
"

"
case x do
  a -> b
  c -> d
end
")

(elixir-def-indentation-test indent-multiline-match
                             (:tags '(indentation))
"
def foo do
case is_string(x) do
true ->
x2 = \" one\"
x <> x2
false ->
x2 = \" two\"
x <> x2
end
end
"

"
def foo do
  case is_string(x) do
    true ->
      x2 = \" one\"
      x <> x2
    false ->
      x2 = \" two\"
      x <> x2
  end
end
")

(elixir-def-indentation-test indent-multiline-match/2
                             (:tags '(indentation))
"
def foo do
case is_string(x) do
    true ->
x2 = \" one\"
      x <> x2
false ->
         x2 = \" two\"
  x <> x2
  end
end
"

"
def foo do
  case is_string(x) do
    true ->
      x2 = \" one\"
      x <> x2
    false ->
      x2 = \" two\"
      x <> x2
  end
end
")

(elixir-def-indentation-test indent-mixed-match
                             (:tags '(indentation))
"
case x do
a -> b
c ->
d
e -> f
end
"

"
case x do
  a -> b
  c ->
    d
  e -> f
end
")

(elixir-def-indentation-test indent-after-require-Record
                             (:tags '(indentation))
  ;; Mind the significant whitespace after `Record' in each case. There should
  ;; be two spaces after `Record', otherwise this test is meaningless.
"
defmodule RSS do
require Record

def zip(list1, list2) when length(list1) == length(list2) do
x = 1
end
end
"

"
defmodule RSS do
  require Record

  def zip(list1, list2) when length(list1) == length(list2) do
    x = 1
  end
end
")

(elixir-def-indentation-test indent-fn-in-multiline-assignment
                             (:expected-result :failed :tags '(indentation))
"
variable =
fn ->
case :file.open(path, modes) do
{:ok, device}    -> device
{:error, reason} ->
raise File.Error, reason: reason
end
end
"

"
variable =
  fn ->
    case :file.open(path, modes) do
      {:ok, device}    -> device
      {:error, reason} ->
        raise File.Error, reason: reason
    end
  end
")

(elixir-def-indentation-test indent-after-def-do-online
                             (:tags '(indentation))
"
defmodule Greeter do
 def hello, do: IO.puts \"hello\"
     def bye, do: IO.puts \"bye\"

  def hi(name) do
IO.puts \"Hi #{name}\"
end
end
"

"
defmodule Greeter do
  def hello, do: IO.puts \"hello\"
  def bye, do: IO.puts \"bye\"

  def hi(name) do
    IO.puts \"Hi #{name}\"
  end
end
")

(elixir-def-indentation-test indent-after-def-do-online/2
                             (:tags '(indentation))

"
defmodule ControlFlow do
  defmacro my_if(expr, do: if_block), do: if(expr, do: if_block, else: nil)
                                          defmacro my_if(expr, do: if_block, else: else_block) do
                                            quote do
                                              case unquote(expr) do
                                                result when result in [false, nil] -> unquote(else_block)
                                                _ -> unquote(if_block)
                                              end
                                            end
                                            end
end
"

"
defmodule ControlFlow do
  defmacro my_if(expr, do: if_block), do: if(expr, do: if_block, else: nil)
  defmacro my_if(expr, do: if_block, else: else_block) do
    quote do
      case unquote(expr) do
        result when result in [false, nil] -> unquote(else_block)
        _ -> unquote(if_block)
      end
    end
  end
end
")

(elixir-def-indentation-test indent-after-def-do-online/3
                             (:tags '(indentation))
"
defmodule Foo do
def bar(baz, quun \\\\ nil)
def bar(baz, quun) when baz == quun, do: baz
def bar(baz, quun), do: quun

  defp bar(baz, quun \\\\ nil)
        defp bar(baz, quun) when baz == quun, do: baz
     defp bar(baz, quun), do: quun

  defmacro bar(baz, quun \\\\ nil)
defmacro bar(baz, quun) when baz == quun, do: baz
          defmacro bar(baz, quun), do: quun

     defmacrop bar(baz, quun \\\\ nil)
defmacrop bar(baz, quun) when baz == quun, do: baz
  defmacrop bar(baz, quun), do: quun
end
"

"
defmodule Foo do
  def bar(baz, quun \\\\ nil)
  def bar(baz, quun) when baz == quun, do: baz
  def bar(baz, quun), do: quun

  defp bar(baz, quun \\\\ nil)
  defp bar(baz, quun) when baz == quun, do: baz
  defp bar(baz, quun), do: quun

  defmacro bar(baz, quun \\\\ nil)
  defmacro bar(baz, quun) when baz == quun, do: baz
  defmacro bar(baz, quun), do: quun

  defmacrop bar(baz, quun \\\\ nil)
  defmacrop bar(baz, quun) when baz == quun, do: baz
  defmacrop bar(baz, quun), do: quun
end
")

(elixir-def-indentation-test indent-after-not-finished-one-line-def
                             (:tags '(indentation))
"
defmodule Hello do
      defp skip,
do: true
def on?, do: true
defmacro switch,
do: on!
defp self, do: value
defmacrop whatever, do: do_it!
end
"

"
defmodule Hello do
  defp skip,
    do: true
  def on?, do: true
  defmacro switch,
    do: on!
  defp self, do: value
  defmacrop whatever, do: do_it!
end
")

(elixir-def-indentation-test indent-correct-with-multiple-one-line-macro-calls
                             (:tags '(indentation))
"
defmodule MyMacros do
  mymacro x1, do: [:x1]
mymacro x2, do: [:x2]
  mymacro x3, do: [:x3]
       mymacro x1, do: [:x1]
  mymacro x2, do: [:x2]
mymacro x3, do: [:x3]
end
"

"
defmodule MyMacros do
  mymacro x1, do: [:x1]
  mymacro x2, do: [:x2]
  mymacro x3, do: [:x3]
  mymacro x1, do: [:x1]
  mymacro x2, do: [:x2]
  mymacro x3, do: [:x3]
end
")

(elixir-def-indentation-test indent-binary-sequence
                             (:tags '(indentation))
"
defmodule ExampleTest do
     test \"the truth\" do
       assert <<1,2>> == <<1,2>>
assert 1 + 1 == 2
  end
end
"

"
defmodule ExampleTest do
  test \"the truth\" do
    assert <<1,2>> == <<1,2>>
    assert 1 + 1 == 2
  end
end
")

(elixir-def-indentation-test indent-binary-sequence-inside-match-block/2
                             (:tags '(indentation))
"
case asd do
<<c1::5, c2::5, c3::5, c4::5, c5::5, c6::5, c7::2>> ->
<<main::binary,
enc.(c1)::8, enc.(c2)::8, enc.(c3)::8, enc.(c4)::8,
enc.(c5)::8, enc.(c6)::8, enc.(bsl(c7, 3))::8, ?=>>
<<c1::5, c2::5, c3::5, c4::5, c5::4>> ->
<<main::binary,
enc.(c1)::8, enc.(c2)::8, enc.(c3)::8, enc.(c4)::8,
enc.(bsl(c5, 1))::8, ?=,  ?=, ?=>>
<<c1::5, c2::5, c3::5, c4::1>> ->
<<main::binary,
enc.(c1)::8, enc.(c2)::8,  enc.(c3)::8, enc.(bsl(c4, 4))::8,
?=, ?=,  ?=, ?=>>
<<c1::5, c2::3>> ->
<<main::binary,
enc.(c1)::8, enc.(bsl(c2, 2))::8, ?=, ?=,
?=, ?=, ?=, ?=>>
<<>> ->
main
end
"

"
case asd do
  <<c1::5, c2::5, c3::5, c4::5, c5::5, c6::5, c7::2>> ->
    <<main::binary,
      enc.(c1)::8, enc.(c2)::8, enc.(c3)::8, enc.(c4)::8,
      enc.(c5)::8, enc.(c6)::8, enc.(bsl(c7, 3))::8, ?=>>
  <<c1::5, c2::5, c3::5, c4::5, c5::4>> ->
    <<main::binary,
      enc.(c1)::8, enc.(c2)::8, enc.(c3)::8, enc.(c4)::8,
      enc.(bsl(c5, 1))::8, ?=,  ?=, ?=>>
  <<c1::5, c2::5, c3::5, c4::1>> ->
    <<main::binary,
      enc.(c1)::8, enc.(c2)::8,  enc.(c3)::8, enc.(bsl(c4, 4))::8,
      ?=, ?=,  ?=, ?=>>
  <<c1::5, c2::3>> ->
    <<main::binary,
      enc.(c1)::8, enc.(bsl(c2, 2))::8, ?=, ?=,
      ?=, ?=, ?=, ?=>>
  <<>> ->
    main
end
")

(elixir-def-indentation-test indent-inside-square-brackets
                             (:tags '(indentation))
"
children = [
        supervisor(Task.Supervisor, [[name: KVServer.TaskSupervisor]]),
worker(Task, [KVServer, :accept, [4040]])
]
"

"
children = [
  supervisor(Task.Supervisor, [[name: KVServer.TaskSupervisor]]),
  worker(Task, [KVServer, :accept, [4040]])
]
")

(elixir-def-indentation-test indent-after-reserved-word/case
                             (:tags '(indentation))
"
defmodule Application.Behavior do
def foo(test) do
  test_case = test.case
  run(test_case)
end
end
"

"
defmodule Application.Behavior do
  def foo(test) do
    test_case = test.case
    run(test_case)
  end
end
")

(elixir-def-indentation-test indent-after-reserved-word/try
                             (:tags '(indentation))
"
defmodule Application.Behavior do
def foo(test) do
  test_case = test.try
run(test_case)
    end
end
"

"
defmodule Application.Behavior do
  def foo(test) do
    test_case = test.try
    run(test_case)
  end
end
")

(elixir-def-indentation-test indent-after-reserved-word/rescue
                             (:tags '(indentation))
"
defmodule Application.Behavior do
def foo(test) do
test_case = test.rescue
           run(test_case)
    end
end
"

"
defmodule Application.Behavior do
  def foo(test) do
    test_case = test.rescue
    run(test_case)
  end
end
")


(elixir-def-indentation-test indent-after-reserved-word/if
                             (:tags '(indentation))
"
defmodule Application.Behavior do
def foo(test) do
test_case = test.if
           run(test_case)
    end
end
"

"
defmodule Application.Behavior do
  def foo(test) do
    test_case = test.if
    run(test_case)
  end
end
")

(elixir-def-indentation-test indent-after-bitstring/1
                             (:tags '(indentation))
"
defmodule X do
  def a, do: <<1 :: size(8)>>
      def b, do: <<2 :: size(8)>>
          def c, do: <<3 :: size(8)>>
end
"

"
defmodule X do
  def a, do: <<1 :: size(8)>>
  def b, do: <<2 :: size(8)>>
  def c, do: <<3 :: size(8)>>
end
")

(elixir-def-indentation-test indent-after-fn/1
                             (:tags '(indentation))
"
defmodule X do
  def func do
    Enum.filter([1,2,3],
      fn(1) -> true
             (2) -> false
  (_) -> true
      end)
  end
end
"

"
defmodule X do
  def func do
    Enum.filter([1,2,3],
      fn(1) -> true
        (2) -> false
        (_) -> true
      end)
  end
end
")

(elixir-def-indentation-test indent-outside-block
                             (:tags '(indentation))
"
  1 + 1  # => 2

sum = fn(a, b) ->
a + b
     end

sum.(1231, 3)

     a = 23
   a = a

23 / 3
"

"
1 + 1  # => 2

sum = fn(a, b) ->
  a + b
end

sum.(1231, 3)

a = 23
a = a

23 / 3
")

(elixir-def-indentation-test indent-list-elements
                             (:tags '(indentation))
"
defmodule Foo do
  def bar do
    [
 :foo,
         :bar
    ]
  end
end
"

"
defmodule Foo do
  def bar do
    [
      :foo,
      :bar
    ]
  end
end
")

(elixir-def-indentation-test indent-tuple-elements
                             (:tags '(indentation))
"
defmodule Foo do
  def bar do
    {
 :foo,
         :bar
    }
  end
end
"

"
defmodule Foo do
  def bar do
    {
      :foo,
      :bar
    }
  end
end
")

(elixir-def-indentation-test indent-maps-with-stings-as-keys
                             (:tags '(indentation))
"
%{
\"data\" => %{
\"foo\" => %{
\"bar\" => nil
}
}
}
"

"
%{
  \"data\" => %{
    \"foo\" => %{
      \"bar\" => nil
    }
  }
}
")

(elixir-def-indentation-test indent-maps-with-multiple-string-keys
                             (:expected-result :failed :tags '(indentation))
"
Enum.map [], fn x ->
%{
\"a\" => 5555,
\"b\" => 5555,
\"c\" => x,
}
end
"

"
Enum.map [], fn x ->
  %{
    \"a\" => 5555,
    \"b\" => 5555,
    \"c\" => x,
  }
end
")

(elixir-def-indentation-test indent-maps-and-structs-elements
                             (:tags '(indentation))
"
{
:foo,
         :bar
}

%GenEvent.Stream{
             manager: manager,
timeout: Keyword.get(options, :timeout, :infinity)}
"

"
{
  :foo,
  :bar
}

%GenEvent.Stream{
  manager: manager,
  timeout: Keyword.get(options, :timeout, :infinity)}
")

(elixir-def-indentation-test indent-parenthesis-inside-block
                             (:tags '(indentation))
"
defmodule Foo do
def bar do
()
  end
end
"

"
defmodule Foo do
  def bar do
    ()
  end
end
")

(elixir-def-indentation-test complex-case-with-matches/2
                             (:tags '(indentation))
"
case parse do
{ [ help: true ], _, _ } -> :help
{ _, [ user, project, count ], _ } ->
{ user, project, count }
  { _, [ user, project ], _ } -> { user, project, @default_count }
  _ -> :help
end
"

"
case parse do
  { [ help: true ], _, _ } -> :help
  { _, [ user, project, count ], _ } ->
    { user, project, count }
  { _, [ user, project ], _ } -> { user, project, @default_count }
  _ -> :help
end
")

(elixir-def-indentation-test complex-case-with-matches/3
                             (:tags '(indentation))
"
defmodule MyModule do
case File.read(\"/usr/share/dict/words\") do
{:ok, contents} ->
    {:something, contents}
      {:error, reason} ->
{:error, reason}
  end
end
"

"
defmodule MyModule do
  case File.read(\"/usr/share/dict/words\") do
    {:ok, contents} ->
      {:something, contents}
    {:error, reason} ->
      {:error, reason}
  end
end
")

(elixir-def-indentation-test complex-case-with-matches/4
                             (:tags '(indentation))
"
case :foo do
1 ->
 try true do
        :foo
end
2 ->
false
end

fn x ->
if true do
end
   end
"

"
case :foo do
  1 ->
    try true do
      :foo
    end
  2 ->
    false
end

fn x ->
  if true do
  end
end
")

(elixir-def-indentation-test complex-case-with-matches/5
                             (:tags '(indentation))
"
case parse do
  {} -> :help
   {} -> :help
_
end
"

"
case parse do
  {} -> :help
  {} -> :help
  _
end
")

(elixir-def-indentation-test case-with-multiline-maps
                             (:tags '(indentation))
"
case statement do
  %{\"foo\" => \"foo\",
     \"baz\" => \"baz\"} ->
:ok
_ ->
    :ok
end
"

"
case statement do
  %{\"foo\" => \"foo\",
    \"baz\" => \"baz\"} ->
    :ok
  _ ->
    :ok
end
")

(elixir-def-indentation-test case-with-for-comprehension
                             (:tags '(indentation))
"
case expression do
  true ->
  for _ <- [] do
    :ok
  end
end
"

"
case expression do
  true ->
    for _ <- [] do
      :ok
    end
end
")

(elixir-def-indentation-test indent-case-when-condition-is-a-named-function-on-a-module
                             (:tags '(indentation))
"
defmodule Whois do
  def lookup1(domain) do
    case Server.for(domain) do
          {:ok, server} -> server
          :error -> {:error, :unsupported}
        end
  end
end
"

"
defmodule Whois do
  def lookup1(domain) do
    case Server.for(domain) do
      {:ok, server} -> server
      :error -> {:error, :unsupported}
    end
  end
end
")

(elixir-def-indentation-test close-map-curly-brackt
                             (:tags '(indentation))
"
config = %{
  async_cases: [],
exclude: opts[:exclude],
include: opts[:include],
           timeout: opts[:timeout],
  trace: opts[:trace]
        }
"

"
config = %{
  async_cases: [],
  exclude: opts[:exclude],
  include: opts[:include],
  timeout: opts[:timeout],
  trace: opts[:trace]
}
")


(elixir-def-indentation-test receive-after-block
                             (:tags '(indentation))
"
receive do
{:hello} -> :ok
    other ->
other
    after
2000 ->
IO.puts 'hello'
IO.puts 'status 2000 ends'
{ :ok } ->
IO.puts 'ok'
_ -> whatever
end
"

"
receive do
  {:hello} -> :ok
  other ->
    other
after
  2000 ->
    IO.puts 'hello'
    IO.puts 'status 2000 ends'
  { :ok } ->
    IO.puts 'ok'
  _ -> whatever
end
")

(elixir-def-indentation-test indent-after-for-online-definition
                             (:tags '(indentation))
"
defmodule Hello do
  def hi do
    hi = for i <- list, do: i
             # weird spacing now

         for i <- list, do: i
IO.puts 'WORKED'
  end
end

hi = for i <- list, do: i
         # weird spacing now
"

"
defmodule Hello do
  def hi do
    hi = for i <- list, do: i
    # weird spacing now

    for i <- list, do: i
    IO.puts 'WORKED'
  end
end

hi = for i <- list, do: i
# weird spacing now
")

(elixir-def-indentation-test indent-oneline-for-after-assignment
                             (:expected-result :failed :tags '(indentation))
"
hi =
for i <- list, do: i
"

"
hi =
  for i <- list, do: i
")

(elixir-def-indentation-test indent-multiline-for
                             (:tags '(indentation))
"
defmodule For do
def test do
  for {k, v} <- keyword,
  v = process_value(v),
into: %{}
   do: {v, k}

for {k, v} <- keyword,
v = process_value(v),
into: %{} do
{v, k}
    end
  end
end
"

"
defmodule For do
  def test do
    for {k, v} <- keyword,
      v = process_value(v),
      into: %{}
      do: {v, k}

    for {k, v} <- keyword,
      v = process_value(v),
      into: %{} do
        {v, k}
    end
  end
end
")

(elixir-def-indentation-test indent-multiline-for-with-assignment
                             (:expected-result :failed :tags '(indentation))
"
result =
for {k, v} <- keyword,
v = process_value(v),
into: %{}
do: {v, k}
"

"
result =
  for {k, v} <- keyword,
    v = process_value(v),
    into: %{}
    do: {v, k}
")

(elixir-def-indentation-test indent-multiline-for-do-end-with-assignment
                             (:expected-result :failed :tags '(indentation))
"
result =
for {k, v} <- keyword,
v = process_value(v),
into: %{}
do
{v, k}
end
"

"
result =
  for {k, v} <- keyword,
    v = process_value(v),
    into: %{} do
    {v, k}
  end
")

(elixir-def-indentation-test indent-multiline-function-calls-without-parenthesis
                             (:tags '(indentation))
"
some_method :arg1,
:arg2
other_method
"

"
some_method :arg1,
  :arg2
other_method
")

(elixir-def-indentation-test indent-multiline-function-calls-without-parenthesis/2
                             (:tags '(indentation))
"
some_method :arg1,
arg1: 1,
arg2: 2
other_method
"

"
some_method :arg1,
  arg1: 1,
  arg2: 2
other_method
")

(elixir-def-indentation-test indent-correct-inside-fn-block
                             (:tags '(indentation))
"
Enum.map(addresses, fn({mac_address, dbms}) ->
sum = Enum.reduce(dbms, fn(x, sum) -> x + sum end)
        average_dbm = sum / length(addresses)
end)
"

"
Enum.map(addresses, fn({mac_address, dbms}) ->
  sum = Enum.reduce(dbms, fn(x, sum) -> x + sum end)
  average_dbm = sum / length(addresses)
end)
")

(elixir-def-indentation-test indent-statement-with-anonymous-fn
                             (:tags '(indentation))
"
cond do
 is_nil(val) ->
  IO.puts \"OK\"
  Enum.any?(1..6, fn -> end)
true ->
end
"

"
cond do
  is_nil(val) ->
    IO.puts \"OK\"
    Enum.any?(1..6, fn -> end)
  true ->
end
")

(elixir-def-indentation-test indent-maps-inside-list
                             (:tags '(indentation))
"
[{:earmark, \"~> 0.1\", only: :dev},
             {:earmark, \"~> 0.1\", only: :dev},
{:ex_doc, \"~> 0.11\", only: :dev},
        {:ex_doc, \"~> 0.11\", only: :dev}]

[
{1, 2, 3},
{4, 5, 6},
    {7, 8, 9}
]

[
%{
 name: \"John Doe\",
  email: \"john@doe.org\"
},
%{
        name: \"Jane Doe\",
email: \"jane@doe.org\",
},
%{
  name: \"Josie Doe\",
    email: \"josie@doe.org\",
  },
]
"

"
[{:earmark, \"~> 0.1\", only: :dev},
 {:earmark, \"~> 0.1\", only: :dev},
 {:ex_doc, \"~> 0.11\", only: :dev},
 {:ex_doc, \"~> 0.11\", only: :dev}]

[
  {1, 2, 3},
  {4, 5, 6},
  {7, 8, 9}
]

[
  %{
    name: \"John Doe\",
    email: \"john@doe.org\"
  },
  %{
    name: \"Jane Doe\",
    email: \"jane@doe.org\",
  },
  %{
    name: \"Josie Doe\",
    email: \"josie@doe.org\",
  },
]
")

(elixir-def-indentation-test indent-before-comma
                             (:tags '(indentation))
"
defmodule TestIndentation do
import Enum

@att %{ab: 21,
   de: 22}
                  IO.inspect @att
end
"

"
defmodule TestIndentation do
  import Enum

  @att %{ab: 21,
         de: 22}
  IO.inspect @att
end
")

(elixir-def-indentation-test indent-complex-comments
                             (:tags '(indentation))
"
defmodule Foo do
  @initial_state %{
    socket: nil,
opts: nil,
# TODO: x,
    tail: \"\",
  }

  def bar do

    first = {
asdasd,
asdasd,
      asdad,
   # comment
# another comment
   value,
      another
}

 another = [
    asdasd,
    asdasd,
     asdad,
          # comment
 # another comment
      value,
   another
             ]

  end

  # one
# two
              # three
end
"

"
defmodule Foo do
  @initial_state %{
    socket: nil,
    opts: nil,
    # TODO: x,
    tail: \"\",
  }

  def bar do

    first = {
      asdasd,
      asdasd,
      asdad,
      # comment
      # another comment
      value,
      another
    }

    another = [
      asdasd,
      asdasd,
      asdad,
      # comment
      # another comment
      value,
      another
    ]

  end

  # one
  # two
  # three
end
")

(elixir-def-indentation-test indent-tuples-in-case-statement
                             (:tags '(indentation))
"
defmodule Foo do
def bar do
case info do
:init ->
{:foo, :bar}
{:foo, :bar}
{:foo, :bar}
{:foo, :bar}
end
end
end
"

"
defmodule Foo do
  def bar do
    case info do
      :init ->
        {:foo, :bar}
        {:foo, :bar}
        {:foo, :bar}
        {:foo, :bar}
    end
  end
end
"
			     )

(elixir-def-indentation-test indent-multiline-function-specs
                             (:expected-result :failed :tags '(indentation))
"
@callback init(args :: term) ::
{:ok, state} |
{:ok, state, timeout | :hibernate} |
:ignore |
{:stop, reason :: any} when state: any
"

"
@callback init(args :: term) ::
  {:ok, state} |
  {:ok, state, timeout | :hibernate} |
  :ignore |
  {:stop, reason :: any} when state: any
")

(elixir-def-indentation-test indent-multiline-function-specs-followed-by-a-function-def
                             (:expected-result :failed :tags '(indentation))
"
@spec foo(args :: term) ::
{:ok, state} |
{:ok, state, timeout | :hibernate} |
:ignore |
{:stop, reason :: any} when state: any
def foo(_opts) do
:ignore
end
"

"
@spec foo(args :: term) ::
  {:ok, state} |
  {:ok, state, timeout | :hibernate} |
  :ignore |
  {:stop, reason :: any} when state: any
def foo(_opts) do
  :ignore
end
")

;; We don't want automatic whitespace cleanup here because of the significant
;; whitespace after `Record' above. By setting `whitespace-action' to nil,
;; `whitespace-mode' won't automatically clean up trailing whitespace (in my
;; config, anyway).

;;; Local Variables:
;;; whitespace-action: nil
;;; End:

(provide 'elixir-mode-indentation-test)

;;; elixir-mode-indentation-test.el ends here
