## v2.5.0 - Unreleased

## v2.4.0 - 2021-10-05
* [#485](https://github.com/elixir-editors/emacs-elixir/pull/485) - Add require for cl-lib
* [#482](https://github.com/elixir-editors/emacs-elixir/pull/482) - Remove pkg-info dependency
* [#481](https://github.com/elixir-editors/emacs-elixir/pull/481) - Enable elixir-mode by default when opening mix.lock file.
* [#475](https://github.com/elixir-editors/emacs-elixir/pull/475) - Backport ppss accessors, use setq-local & more
* [#472](https://github.com/elixir-editors/emacs-elixir/pull/472) - chore: update guides with new build system
* [#471](https://github.com/elixir-editors/emacs-elixir/pull/471) - feat: switch CI and build system
* [#470](https://github.com/elixir-editors/emacs-elixir/pull/470) - Highlight atom map keys that end with a newline
* [#468](https://github.com/elixir-editors/emacs-elixir/pull/468) - Sigil heredoc support
* [#459](https://github.com/elixir-editors/emacs-elixir/pull/459) - Customizable face for numbers

## v2.3.2 - 2020-11-16
* [#460](https://github.com/elixir-editors/emacs-elixir/pull/460) - @typedoc recognized as heredoc
* [#454](https://github.com/elixir-editors/emacs-elixir/pull/454) - Add ~L, ~E, and ~e sigils for EEx and LiveView
* [#449](https://github.com/elixir-editors/emacs-elixir/pull/449) - Disable smie-blink-matching-inners-locally
* [#448](https://github.com/elixir-editors/emacs-elixir/pull/448) - make usable again in Emacs 27
* [#438](https://github.com/elixir-editors/emacs-elixir/pull/438) - Run mix directly
* [#442](https://github.com/elixir-editors/emacs-elixir/pull/442) - Add support for ~U sigil
* [#441](https://github.com/elixir-editors/emacs-elixir/pull/441) - Inherit faces from appropriate built-in ones
* [#433](https://github.com/elixir-editors/emacs-elixir/pull/433) - Special case for indentation of fat arrow in map literals
* [#420](https://github.com/elixir-editors/emacs-elixir/pull/420) - change to mix.exs directory before format
* [#425](https://github.com/elixir-editors/emacs-elixir/pull/425) - fontify defguard and defguardp
* [#418](https://github.com/elixir-editors/emacs-elixir/pull/418) - locate closest formatter.exs on save
* [#406](https://github.com/elixir-editors/emacs-elixir/pull/406) - add elixir-format function
* [#381](https://github.com/elixir-editors/emacs-elixir/pull/381) - Fontify ~N and ~T sigils
* [#383](https://github.com/elixir-editors/emacs-elixir/pull/383) - Fix @dox highlight
* [#386](https://github.com/elixir-editors/emacs-elixir/pull/387) - Fix a broken command name
* [#374](https://github.com/elixir-editors/emacs-elixir/pull/374) - Fix highlighting ignored variable in pattern match
* [#373](https://github.com/elixir-editors/emacs-elixir/pull/373) - Fontify ~D sigil
* [#368](https://github.com/elixir-editors/emacs-elixir/pull/368) - Implement moving defun command
* [#364](https://github.com/elixir-editors/emacs-elixir/pull/364) - Fix issue that emacs hangs after def? statement
* [#307](https://github.com/elixir-editors/emacs-elixir/pull/307) - Include - in punctuation class
* [#357](https://github.com/elixir-editors/emacs-elixir/pull/357) - remove elixir-negation-face in favor of font-lock
* [#358](https://github.com/elixir-editors/emacs-elixir/pull/358) - remove ignore-var-face in favor of comment-face
* [#350](https://github.com/elixir-editors/emacs-elixir/pull/350) - Correct implementation for with keyword
* [#345](https://github.com/elixir-editors/emacs-elixir/pull/345) - Correct default indent after match
* [#343](https://github.com/elixir-editors/emacs-elixir/pull/343) - Fix for do oneline blocks
* [#342](https://github.com/elixir-editors/emacs-elixir/pull/342) - Complex comment indentation
* [#340](https://github.com/elixir-editors/emacs-elixir/pull/340) - Fix invalid highlighting question quote
* [#339](https://github.com/elixir-editors/emacs-elixir/pull/339) - Fix heredoc indentation on first line inside block
* [#338](https://github.com/elixir-editors/emacs-elixir/pull/338) - Fix "variable binding depth exceeds max-specpdl-size" error

## v2.3.1 - 2016/04/19
* [#337](https://github.com/elixir-editors/emacs-elixir/pull/337) - Fix indentation issue after COMMA token
* [#333](https://github.com/elixir-editors/emacs-elixir/pull/333) - Fix indentation of second element inside list of tuples
* [#332](https://github.com/elixir-editors/emacs-elixir/pull/332) - Correct indent after using 'for' as function name
* [#329](https://github.com/elixir-editors/emacs-elixir/pull/329) - Indent by one level if current line belongs to function call

## v2.3.0 - 2016/04/13
* [#327](https://github.com/elixir-editors/emacs-elixir/pull/327) - Correct indentation of maps inside lists
* [#326](https://github.com/elixir-editors/emacs-elixir/pull/326) - Correct anonymous fun indent inside block
* [#325](https://github.com/elixir-editors/emacs-elixir/pull/325) - Fix indentation of statement keywords on dot call
* [#324](https://github.com/elixir-editors/emacs-elixir/pull/324) - added failing tests for named functions in if and case
* [#322](https://github.com/elixir-editors/emacs-elixir/pull/322) - added a failing indentation test for cond within with
* [#321](https://github.com/elixir-editors/emacs-elixir/pull/321) - Fix invalid highlighting '::' in binaries
* [#318](https://github.com/elixir-editors/emacs-elixir/pull/318) - Fix indent of pipes inside blocks of 'def' for example

## v2.2.9 - 2016/04/03
* [#317](https://github.com/elixir-editors/emacs-elixir/pull/317) - Correct pipeline indentation
* [#316](https://github.com/elixir-editors/emacs-elixir/pull/316) - Fix indentation of if within an else block
* [#315](https://github.com/elixir-editors/emacs-elixir/pull/315) - Correct indentation with for-comprehensions within blocks
* [#314](https://github.com/elixir-editors/emacs-elixir/pull/314) - Fix highlighting triple single quote(heredoc)
* [#313](https://github.com/elixir-editors/emacs-elixir/pull/313) - Correct indentation after a one line 'fn' definition
* [#305](https://github.com/elixir-editors/emacs-elixir/pull/305) - Added test case for for-comprehensions within case
* [#303](https://github.com/elixir-editors/emacs-elixir/pull/303) - Fix escaped delimiter in sigil issue
* [#295](https://github.com/elixir-editors/emacs-elixir/pull/295) - Demonstrate defstruct indention in a test case
* [#261](https://github.com/elixir-editors/emacs-elixir/pull/261) - Test for multi-line function calls without parenthesis
* [#299](https://github.com/elixir-editors/emacs-elixir/pull/299) - Added `with/1` to the highlighted keywords
* [#298](https://github.com/elixir-editors/emacs-elixir/pull/298) - Added a test for alignment of the last key in multiline maps in cases
* [#296](https://github.com/elixir-editors/emacs-elixir/pull/296) - Gray out ignored variables
* [#291](https://github.com/elixir-editors/emacs-elixir/pull/291) - Added a test for indenting non-finished one-line if-else
* [#289](https://github.com/elixir-editors/emacs-elixir/pull/289) - Added a test case for if within an else
* [#287](https://github.com/elixir-editors/emacs-elixir/pull/287) - Fix sigil triple quotes
* [#284](https://github.com/elixir-editors/emacs-elixir/pull/284) - Added a test for highlighting end after comment
* [#285](https://github.com/elixir-editors/emacs-elixir/pull/285) - Don't capture '(or line-start (not (any ".")))'
* [#282](https://github.com/elixir-editors/emacs-elixir/pull/282) - Indent multiple macro calls with do colon correct
* [#280](https://github.com/elixir-editors/emacs-elixir/pull/280) - Fix one line definitions with equal char inside guard
* [#279](https://github.com/elixir-editors/emacs-elixir/pull/279) - Fix indentation of single line fun declarations after single line fun declarations with when clauses
* [#277](https://github.com/elixir-editors/emacs-elixir/pull/277) - Fix syntax highlighting sigils in string
* [#273](https://github.com/elixir-editors/emacs-elixir/pull/273) - Removed send_after from highlighted keywords
* [#272](https://github.com/elixir-editors/emacs-elixir/pull/272) - Added `send` and `send_after` to font lock
* [#271](https://github.com/elixir-editors/emacs-elixir/pull/271) - Highlight module if preceded by a pipe

## v2.2.8 - 2015/10/19
* [#270](https://github.com/elixir-editors/emacs-elixir/pull/270) - Fix highlighting hashmark in sigil
* [#269](https://github.com/elixir-editors/emacs-elixir/pull/269) - Fix string interpolation
* [#268](https://github.com/elixir-editors/emacs-elixir/pull/268) - added font-lock to defoverridable
* [#262](https://github.com/elixir-editors/emacs-elixir/pull/262) - Add indentation tests for comprehensions
* [#267](https://github.com/elixir-editors/emacs-elixir/pull/267) - ~s is sigil, not attribute
* [#266](https://github.com/elixir-editors/emacs-elixir/pull/266) - Fix quotes in sigils
* [#264](https://github.com/elixir-editors/emacs-elixir/pull/264) - Fix string interpolation

## v2.2.7 - 2015/09/17
* [#260](https://github.com/elixir-editors/emacs-elixir/pull/260) - Correct indentation after "for" comprehension
* [#259](https://github.com/elixir-editors/emacs-elixir/pull/259) - Indent receive/after matches correct
* [#258](https://github.com/elixir-editors/emacs-elixir/pull/258) - Emacs hangs if `elixir-smie-forward-token` returns an empty string
* [#253](https://github.com/elixir-editors/emacs-elixir/pull/253) - Fix Highlight atom issue(atom contains '!', '?', '@')
* [#252](https://github.com/elixir-editors/emacs-elixir/pull/252) - Fix after dot highlighting
* [#249](https://github.com/elixir-editors/emacs-elixir/pull/249) - Add correct indent for "if" inside a "->" block
* [#246](https://github.com/elixir-editors/emacs-elixir/pull/246) - Fix highlighting true, false, nil
* [#244](https://github.com/elixir-editors/emacs-elixir/pull/244) - True,false, nil are highlighted as atoms
* [#241](https://github.com/elixir-editors/emacs-elixir/pull/241) - correct indent for oneline `do:` when moved to next line
* [#240](https://github.com/elixir-editors/emacs-elixir/pull/240) - Correct indent in case expression when returning a tuple
* [#236](https://github.com/elixir-editors/emacs-elixir/pull/236) - fontify special macros with prefix like '%' and '&'
* [#235](https://github.com/elixir-editors/emacs-elixir/pull/235) - correct indentation for identifiers which contains built in words after a dot

## v2.2.6 - 2015/08/05
* [#234](https://github.com/elixir-editors/emacs-elixir/pull/234) - don't highlights LHS as a variable in `==` case fixes #225
* [#233](https://github.com/elixir-editors/emacs-elixir/pull/233) - module syntax highlighting also works correctly with &
* [#232](https://github.com/elixir-editors/emacs-elixir/pull/232) - correct indentation for closing Map curly bracket fixes #223
* [#231](https://github.com/elixir-editors/emacs-elixir/pull/231) - correct indentation for block with multiple matches
* [#230](https://github.com/elixir-editors/emacs-elixir/pull/230) - update travis setup
* [#228](https://github.com/elixir-editors/emacs-elixir/pull/228) - clear elixir-mode from deprecated functions

## v2.2.5 - 2015/06/18
* [#222](https://github.com/elixir-editors/emacs-elixir/pull/222) - correct indentation inside heredoc strings
* [#221](https://github.com/elixir-editors/emacs-elixir/pull/221) - highlight atoms correctly in a pattern match
* [#220](https://github.com/elixir-editors/emacs-elixir/pull/220) - Correct indentation of parenthesized expression inside blocks
* [#214](https://github.com/elixir-editors/emacs-elixir/pull/214) - correct indentation after one line definition with if keyword
* [#213](https://github.com/elixir-editors/emacs-elixir/pull/213) - remove do keyword to handle also oneline definitions

## v2.2.4 - 2015/05/26
* [#203](https://github.com/elixir-editors/emacs-elixir/pull/203) - Implement triple-quoted strings.

## v2.2.3 - 2015/05/26
* [#202](https://github.com/elixir-editors/emacs-elixir/pull/202) - correct indentation after oneline def/if definition
* [#200](https://github.com/elixir-editors/emacs-elixir/pull/200) - correct elements indentation for structs, maps and tuples
* [#198](https://github.com/elixir-editors/emacs-elixir/pull/198) - indenting elements of a list correctly
* [#196](https://github.com/elixir-editors/emacs-elixir/pull/196) - correct indentation outside of blocks

## v2.2.2 - 2015/05/22
* [#193](https://github.com/elixir-editors/emacs-elixir/pull/193) - fix wrong indentation on empty line between existing code
* [#195](https://github.com/elixir-editors/emacs-elixir/pull/195) - highlighting of capitalized modules when used as structs
* [#192](https://github.com/elixir-editors/emacs-elixir/pull/192) - Fix (error "Lisp nesting exceeds `max-lisp-eval-depth'") which crashes correct indentation
* [#190](https://github.com/elixir-editors/emacs-elixir/pull/190) - correct indentation for multiclause anonymous functions
* [#189](https://github.com/elixir-editors/emacs-elixir/pull/189) - Modify indentation rules for one-line functions ending with bitstrings.
* [#179](https://github.com/elixir-editors/emacs-elixir/pull/179) - Modify syntax highlighting so there is no differentiating between built-in and user-defined modules.
* [#188](https://github.com/elixir-editors/emacs-elixir/pull/188) - Fix changelog.
* [#187](https://github.com/elixir-editors/emacs-elixir/pull/187) - Add unresolved test case.
* [#178](https://github.com/elixir-editors/emacs-elixir/pull/178) - Factor out `cask install` as its own task in Rakefile

## v2.2.1 - 2015/05/19
* [#186](https://github.com/elixir-editors/emacs-elixir/pull/186) - Remove undocumented failing tests.
* [#182](https://github.com/elixir-editors/emacs-elixir/pull/182) - Remove redundant local-pair tip for smartparens users
* [#183](https://github.com/elixir-editors/emacs-elixir/pull/183) - Fix typos in README.
* [#176](https://github.com/elixir-editors/emacs-elixir/pull/176) - Highlight digits in atoms
* [#173](https://github.com/elixir-editors/emacs-elixir/pull/173) - Add function to apply `fill-region` in doc strings
* [#175](https://github.com/elixir-editors/emacs-elixir/pull/175) - Add tips for smartparens users
* [#177](https://github.com/elixir-editors/emacs-elixir/pull/177) - Prefer Emacs over emacs.
* [#141](https://github.com/elixir-editors/emacs-elixir/pull/141) - Add documentation line explaining how to edit Elixir templates.
* [Syntax Highlighting] Modules don't start with underscore. This fix corrects the fontification of private function.

## v2.2.0 2014/12/31

### Enhancements

  * [Indentation] Indent listing inside square brackets. (#160)
  * [Indentation] Indent of binary sequence inside match block
  * [Indentation] Indent correct after a binary sequence `<<1,2,3,4>>`.
  * [Indentation] Indent correct after oneline `def ... do:` function
  * [Indentation] Correct behavior after last line in buffer. (#145)

## v2.1.1 - 2014/12/24

### Enhancements

  * [Indentation] Indent block inside a fn match.

### Bugfixes

  * [Indentation] #152 Fix indentation inside parens wrapped around def

## v2.1.0 - 2014/12/23

### Enhancements

  * [Indentation] Continue of indentation in multiple line assignment.
  * [Indentation] Always indent with only 2 spaces except when function arguments span multiple lines.
  * [Indentation] Fix the indentation for mixed matchings.
  * [Indentation] Pipe |> indentation works correctly.
  * [Syntax Highlighting] Fontify continuation lines assignment.

### Changes

  * [Deprecated] Add deprecated message for eval and quoted functions.

## v2.0.2 - 2014/10/29
  * [#136](https://github.com/elixir-editors/emacs-elixir/pull/136) - Expand def of block operator regex to include non-newlines.
  * [#137](https://github.com/elixir-editors/emacs-elixir/pull/137) - update rake tasks
  * [#135](https://github.com/elixir-editors/emacs-elixir/pull/135) - Update README so the MELPA badge points to stable build.
  * [#133](https://github.com/elixir-editors/emacs-elixir/pull/133) - refine readme
  * [#134](https://github.com/elixir-editors/emacs-elixir/pull/134) - refine the ability to show the current elixir-mode version
  * [#132](https://github.com/elixir-editors/emacs-elixir/pull/132) - Added: test coverage with undercover.el
  * [#131](https://github.com/elixir-editors/emacs-elixir/pull/131) - Fix documentation url
  * [#127](https://github.com/elixir-editors/emacs-elixir/pull/127) - Add failing test for comment in cond expression (fixed)
  * [#128](https://github.com/elixir-editors/emacs-elixir/pull/128) - Added: collection of failing tests
  * [#124](https://github.com/elixir-editors/emacs-elixir/pull/124) - Fixed: Build Status badge
  * [#123](https://github.com/elixir-editors/emacs-elixir/pull/123) - TravisCI: add-apt-repository and apt-get install fix
  * [#121](https://github.com/elixir-editors/emacs-elixir/pull/121) - Add TravisCI support
  * [#122](https://github.com/elixir-editors/emacs-elixir/pull/122) - Remove unused code

## v2.0.1 - 2014/09/11
  * [#119](https://github.com/elixir-editors/emacs-elixir/pull/119) - Fixed: indent-inside-parens
  * [#117](https://github.com/elixir-editors/emacs-elixir/pull/117) - Fixed: emacs 24.3 tests
  * [#116](https://github.com/elixir-editors/emacs-elixir/pull/116) - Add "How to run tests" section to CONTRIBUTING.md
  * [#118](https://github.com/elixir-editors/emacs-elixir/pull/118) - Added: try/rescue rule
  * [#114](https://github.com/elixir-editors/emacs-elixir/pull/114) - Fixed: run tests interectively for terminal

## v2.0.0 - 2014/09/08
  * [#113](https://github.com/elixir-editors/emacs-elixir/pull/113) - Cask and ert-runner support
  * [#110](https://github.com/elixir-editors/emacs-elixir/pull/110) - Added: ability to run tests via EVM
  * [#111](https://github.com/elixir-editors/emacs-elixir/pull/111) - Fixed: elixir-quoted-minor-mode tests for ert-run-tests-interactively
  * [#108](https://github.com/elixir-editors/emacs-elixir/pull/108) - Fix various issues caused by code followed by inline comments

## v1.5.0 - 2014/08/27
  * [#103](https://github.com/elixir-editors/emacs-elixir/pull/103) - Add elixir-quoted-minor-mode.

## v1.4.10 - 2014/08/26
  * [#102](https://github.com/elixir-editors/emacs-elixir/pull/102) - Add support for syntax highlighting for variable interpolation. Fixes #93
  * [#101](https://github.com/elixir-editors/emacs-elixir/pull/101) - Fix indentation after inline comment. Fixes #95

## v1.4.9 - 2014/08/25
  * [#100](https://github.com/elixir-editors/emacs-elixir/pull/100) - Fix indentation in multi-line match expressions. Fixes #98
  * [#99](https://github.com/elixir-editors/emacs-elixir/pull/99) - Tokenize trailing whitespace properly. Fixes #97
  * [#96](https://github.com/elixir-editors/emacs-elixir/pull/96) - Remove syntax highlighting for operators.

## v1.4.8 - 2014/08/19
  * [#92](https://github.com/elixir-editors/emacs-elixir/pull/92) - Update Rakefile to also run the release.py script. Refs #88.
  * [#91](https://github.com/elixir-editors/emacs-elixir/pull/91) - Updates to regexes for various lexemes
  * [#90](https://github.com/elixir-editors/emacs-elixir/pull/90) - Fix bug in atom highlighting.

## v1.4.7 - 2014/08/18
  * [#87](https://github.com/elixir-editors/emacs-elixir/pull/87) - Add syntax highlighting for heredocs & failing tests for indentation.
  * [#85](https://github.com/elixir-editors/emacs-elixir/pull/85) - Improve regex for defmodule highlighting.
  * [#84](https://github.com/elixir-editors/emacs-elixir/pull/84) - Improve fontification for identifiers.

## v1.4.6 - 2014/08/18
  * [#82](https://github.com/elixir-editors/emacs-elixir/pull/82) - Remove broken SMIE rule.

## v1.4.5 - 2014/08/18
  * [#81](https://github.com/elixir-editors/emacs-elixir/pull/81) - Rewrite token emitting functions

## v1.4.4 - 2014/08/18
  * [#79](https://github.com/elixir-editors/emacs-elixir/pull/79) - Remove erroneous defrecord syntax.

## v1.4.3 - 2014/08/16
  * [#75](https://github.com/elixir-editors/emacs-elixir/pull/75) - Clean up several minor bugbears in elixir-smie.
  * [#74](https://github.com/elixir-editors/emacs-elixir/pull/74) - Remove special indentation rules for operators, except booleans.

## v1.4.2 - 2014/08/15
  * [#73](https://github.com/elixir-editors/emacs-elixir/pull/73) - Fix buggy syntax highlighting behavior involving "?"
  * [#71](https://github.com/elixir-editors/emacs-elixir/pull/71) - Need two backslashes in regex string.
  * [#70](https://github.com/elixir-editors/emacs-elixir/pull/70) - Use define-derived-mode to define elixir-mode
  * [#69](https://github.com/elixir-editors/emacs-elixir/pull/69) - Remove unused variable

## v1.4.1 - 2014/08/11
  * [#66](https://github.com/elixir-editors/emacs-elixir/pull/66) - Indent correctly after one-liner if/do: statements. Fixes #65
  * [#64](https://github.com/elixir-editors/emacs-elixir/pull/64) - wrong indentation if space between if and statement
  * [#63](https://github.com/elixir-editors/emacs-elixir/pull/63) - Correctly indent one-line anon fns AND block fns. Fixes #59

## v1.4.0 - 2014/07/09
  * [#62](https://github.com/elixir-editors/emacs-elixir/pull/62) - Remove grammar entry causing erroneous alignment to ".". Fixes #49
  * [#61](https://github.com/elixir-editors/emacs-elixir/pull/61) - Remove "=" & left-assoc opers from "OP" regex. Fixes #18.
  * [#60](https://github.com/elixir-editors/emacs-elixir/pull/60) - Refactor font face defaults.
  * [#58](https://github.com/elixir-editors/emacs-elixir/pull/58) - Use string syntax highlighting for regex patterns.
  * [#57](https://github.com/elixir-editors/emacs-elixir/pull/57) - Add beginnings of font-face testing.

## v1.3.1 - 2014/07/05
  * [#52](https://github.com/elixir-editors/emacs-elixir/pull/52) - Add CLI for running elixir-mode tests.
  * [#48](https://github.com/elixir-editors/emacs-elixir/pull/48) - Add a SMIE rule function for "def". Fixes #38 and #41
  * [#50](https://github.com/elixir-editors/emacs-elixir/pull/50) - Remove grammar clause for "fn" terminal. Fixes #7
  * [#44](https://github.com/elixir-editors/emacs-elixir/pull/44) - sigil % to ~
  * [#46](https://github.com/elixir-editors/emacs-elixir/pull/46) - Added highlight for unless and Task module
  * [#45](https://github.com/elixir-editors/emacs-elixir/pull/45) - Highlight defstruct and Actor, Base modules
  * [#40](https://github.com/elixir-editors/emacs-elixir/pull/40) - IMenu: Show ExUnit tests under heading "Tests".
  * [#37](https://github.com/elixir-editors/emacs-elixir/pull/37) - Remove needless statement
  * [#35](https://github.com/elixir-editors/emacs-elixir/pull/35) - Added simple imenu support.
  * [#32](https://github.com/elixir-editors/emacs-elixir/pull/32) - Properly make variables local
  * [#31](https://github.com/elixir-editors/emacs-elixir/pull/31) - needed for effective code-navigation via syntax-ppss
  * [#28](https://github.com/elixir-editors/emacs-elixir/pull/28) - Recognize ? char syntax
  * [#24](https://github.com/elixir-editors/emacs-elixir/pull/24) - elixir-mode-eval-on-current-buffer binding comment incorrect
  * [#22](https://github.com/elixir-editors/emacs-elixir/pull/22) - Enhance `elixir-mode-iex` to accept additional arguments

## 1.3.0 (June 24, 2013)
  * Add `elixir-mode-eval-on-region` to evalute Elixir code on the
  marked region.
  * Add `elixir-mode-eval-on-current-buffer` to evalute Elixir code in the current buffer.
  * Add `elixir-mode-eval-on-current-line` to evalute Elixir code on the current line.
  * Add `elixir-mode-string-to-quoted-on-region` to get the representation of the expression on the marked region.
  * Add `elixir-mode-string-to-quoted-on-current-line` to get the
  representation of the expression on the current line.
