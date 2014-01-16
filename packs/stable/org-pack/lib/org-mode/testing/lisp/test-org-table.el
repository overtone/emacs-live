;;; test-org-table.el --- tests for org-table.el

;; Copyright (c)  David Maus
;; Authors: David Maus, Michael Brand

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;; Comments:

;; Template test file for Org-mode tests.  First the tests that are
;; also a howto example collection as a user documentation, more or
;; less all those using `org-test-table-target-expect'.  Then the
;; internal and more abstract tests.  See also the doc string of
;; `org-test-table-target-expect'.

;;; Code:

(require 'org-table)  ; `org-table-make-reference'

(ert-deftest test-org-table/simple-formula/no-grouping/no-title-row ()
  "Simple sum without grouping rows, without title row."
  (org-test-table-target-expect
   "
|       2 |
|       4 |
|       8 |
| replace |
"
   "
|  2 |
|  4 |
|  8 |
| 14 |
"
   1
   ;; Calc formula
   "#+TBLFM: @>$1 = vsum(@<..@>>)"
   ;; Lisp formula
   "#+TBLFM: @>$1 = '(+ @<..@>>); N"))

(ert-deftest test-org-table/simple-formula/no-grouping/with-title-row ()
  "Simple sum without grouping rows, with title row."
  (org-test-table-target-expect
   "
|     foo |
|---------|
|       2 |
|       4 |
|       8 |
| replace |
"
   "
| foo |
|-----|
|   2 |
|   4 |
|   8 |
|  14 |
"
   1
   ;; Calc formula
   "#+TBLFM: @>$1 = vsum(@I..@>>)"
   ;; Lisp formula
   "#+TBLFM: @>$1 = '(+ @I..@>>); N"))

(ert-deftest test-org-table/simple-formula/with-grouping/no-title-row ()
  "Simple sum with grouping rows, how not to do."
  ;; The first example has a problem, see the second example in this
  ;; ert-deftest.
  (org-test-table-target-expect
   "
|       2 |
|       4 |
|       8 |
|---------|
| replace |
"
   "
|  2 |
|  4 |
|  8 |
|----|
| 14 |
"
   1
   ;; Calc formula
   "#+TBLFM: $1 = vsum(@<..@>>)"
   ;; Lisp formula
   "#+TBLFM: $1 = '(+ @<..@>>); N")

  ;; The problem is that the first three rows with the summands are
  ;; considered the header and therefore column formulas are not
  ;; applied on them as shown below.  Also export behaves unexpected.
  ;; See next ert-deftest how to group rows right.
  (org-test-table-target-expect
   "
|       2 | replace |
|       4 | replace |
|       8 | replace |
|---------+---------|
| replace | replace |
"
   "
|  2 | replace |
|  4 | replace |
|  8 | replace |
|----+---------|
| 14 | 28      |
"
   2
   ;; Calc formula
   "#+TBLFM: @>$1 = vsum(@<..@>>) :: $2 = 2 * $1"
   ;; Lisp formula
   "#+TBLFM: @>$1 = '(+ @<..@>>); N :: $2 = '(* 2 $1); N"))

(ert-deftest test-org-table/simple-formula/with-grouping/with-title-row ()
  "Simple sum with grouping rows, how to do it right."
  ;; Always add a top row with the column names separated by hline to
  ;; get the desired header when you want to group rows.
  (org-test-table-target-expect
   "
|     foo | bar     |
|---------+---------|
|       2 | replace |
|       4 | replace |
|       8 | replace |
|---------+---------|
| replace | replace |
"
   "
| foo | bar |
|-----+-----|
|   2 |   4 |
|   4 |   8 |
|   8 |  16 |
|-----+-----|
|  14 |  28 |
"
   2
   ;; Calc formula
   "#+TBLFM: @>$1 = vsum(@I..@>>) :: $2 = 2 * $1"
   ;; Lisp formula
   "#+TBLFM: @>$1 = '(+ @I..@>>); N :: $2 = '(* 2 $1); N"))

(ert-deftest test-org-table/align ()
  "Align columns within Org buffer, depends on `org-table-number-regexp'."
  (org-test-table-target-expect "
| 0  |  0 |    0 |       0 |       0 |           0 |       0 |    0 |
| ab | 12 | 12.2 | 2.4e-08 | 2x10^12 | 4.034+-0.02 | 2.7(10) | >3.5 |
| ab | ab |   ab |      ab |      ab |          ab |      ab |   ab |
")
  (org-test-table-target-expect "
|          0 |           0 |   0 |    0 |    0 |   0 |
| <-0x0ab.cf | >-36#0vw.yz | nan | uinf | -inf | inf |
|         ab |          ab |  ab |   ab |   ab |  ab |
"))

(defconst references/target-normal "
| 0 | 1 | replace | replace | replace | replace | replace | replace |
| z | 1 | replace | replace | replace | replace | replace | replace |
|   | 1 | replace | replace | replace | replace | replace | replace |
|   |   | replace | replace | replace | replace | replace | replace |
"
  "Normal numbers and non-numbers for Lisp and Calc formula.")

(defconst references/target-special "
|  nan | 1 | replace | replace | replace | replace | replace | replace |
| uinf | 1 | replace | replace | replace | replace | replace | replace |
| -inf | 1 | replace | replace | replace | replace | replace | replace |
|  inf | 1 | replace | replace | replace | replace | replace | replace |
"
  "Special numbers for Calc formula.")

(ert-deftest test-org-table/references/mode-string-EL ()
  "Basic: Assign field reference, sum of field references, sum
and len of simple range reference (no row) and complex range
reference (with row).  Mode string EL."
  ;; Empty fields are kept during parsing field but lost as list
  ;; elements within Lisp formula syntactically when used literally
  ;; and not enclosed with " within fields, see last columns with len.
  (org-test-table-target-expect
   references/target-normal
   ;; All the #ERROR show that for Lisp calculations N has to be used.
   "
| 0 | 1 | 0 |      1 |      1 |      1 | 2 | 2 |
| z | 1 | z | #ERROR | #ERROR | #ERROR | 2 | 2 |
|   | 1 |   |      1 |      1 |      1 | 1 | 1 |
|   |   |   |      0 |      0 |      0 | 0 | 0 |
"
   1 (concat
      "#+TBLFM: $3 = '(identity \"$1\"); EL :: $4 = '(+ $1 $2); EL :: "
      "$5 = '(+ $1..$2); EL :: $6 = '(+ @0$1..@0$2); EL :: "
      "$7 = '(length '($1..$2)); EL :: $8 = '(length '(@0$1..@0$2)); EL"))

  ;; Empty fields are kept during parsing field _and_ as list elements
  ;; within Lisp formula syntactically even when used literally when
  ;; enclosed with " within fields, see last columns with len.
  (org-test-table-target-expect
   "
| \"0\" | \"1\" | repl | repl | repl | repl | repl | repl |
| \"z\" | \"1\" | repl | repl | repl | repl | repl | repl |
| \"\"  | \"1\" | repl | repl | repl | repl | repl | repl |
| \"\"  | \"\"  | repl | repl | repl | repl | repl | repl |
"
   "
| \"0\" | \"1\" | \"0\" | 1 | #ERROR | #ERROR | 2 | 2 |
| \"z\" | \"1\" | \"z\" | 1 | #ERROR | #ERROR | 2 | 2 |
| \"\"  | \"1\" | \"\"  | 1 | #ERROR | #ERROR | 2 | 2 |
| \"\"  | \"\"  | \"\"  | 0 | #ERROR | #ERROR | 2 | 2 |
"
   1 (concat
      "#+TBLFM: $3 = '(concat \"\\\"\" $1 \"\\\"\"); EL :: "
      "$4 = '(+ (string-to-number $1) (string-to-number $2)); EL :: "
      "$5 = '(+ $1..$2); EL :: $6 = '(+ @0$1..@0$2); EL :: "
      "$7 = '(length '($1..$2)); EL :: $8 = '(length '(@0$1..@0$2)); EL")))

(ert-deftest test-org-table/references/mode-string-E ()
  "Basic: Assign field reference, sum of field references, sum
and len of simple range reference (no row) and complex range
reference (with row).  Mode string E."
  (let ((lisp
	 (concat
	  "#+TBLFM: $3 = '(identity $1); E :: $4 = '(+ $1 $2); E :: "
	  "$5 = '(+ $1..$2); E :: $6 = '(+ @0$1..@0$2); E :: "
	  "$7 = '(length '($1..$2)); E :: $8 = '(length '(@0$1..@0$2)); E"))
	(calc
	 (concat
	  "#+TBLFM: $3 = $1; E :: $4 = $1 + $2; E :: "
	  "$5 = vsum($1..$2); E :: $6 = vsum(@0$1..@0$2); E :: "
	  "$7 = vlen($1..$2); E :: $8 = vlen(@0$1..@0$2); E")))
    (org-test-table-target-expect
     references/target-normal
     ;; All the #ERROR show that for Lisp calculations N has to be used.
     "
| 0 | 1 | 0 | #ERROR | #ERROR | #ERROR | 2 | 2 |
| z | 1 | z | #ERROR | #ERROR | #ERROR | 2 | 2 |
|   | 1 |   | #ERROR | #ERROR | #ERROR | 2 | 2 |
|   |   |   | #ERROR | #ERROR | #ERROR | 2 | 2 |
"
     1 lisp)
    (org-test-table-target-expect
     references/target-normal
     "
| 0 | 1 |   0 |     1 |     1 |     1 | 2 | 2 |
| z | 1 |   z | z + 1 | z + 1 | z + 1 | 2 | 2 |
|   | 1 | nan |   nan |   nan |   nan | 2 | 2 |
|   |   | nan |   nan |   nan |   nan | 2 | 2 |
"
     1 calc)
    (org-test-table-target-expect
     references/target-special
     "
|  nan | 1 |  nan |  nan |  nan |  nan | 2 | 2 |
| uinf | 1 | uinf | uinf | uinf | uinf | 2 | 2 |
| -inf | 1 | -inf | -inf | -inf | -inf | 2 | 2 |
|  inf | 1 |  inf |  inf |  inf |  inf | 2 | 2 |
"
     1 calc)))

(ert-deftest test-org-table/references/mode-string-EN ()
  "Basic: Assign field reference, sum of field references, sum
and len of simple range reference (no row) and complex range
reference (with row).  Mode string EN."
  (let ((lisp (concat
	       "#+TBLFM: $3 = '(identity $1); EN :: $4 = '(+ $1 $2); EN :: "
	       "$5 = '(+ $1..$2); EN :: $6 = '(+ @0$1..@0$2); EN :: "
	       "$7 = '(length '($1..$2)); EN :: "
	       "$8 = '(length '(@0$1..@0$2)); EN"))
	(calc (concat
	       "#+TBLFM: $3 = $1; EN :: $4 = $1 + $2; EN :: "
	       "$5 = vsum($1..$2); EN :: $6 = vsum(@0$1..@0$2); EN :: "
	       "$7 = vlen($1..$2); EN :: $8 = vlen(@0$1..@0$2); EN")))
    (org-test-table-target-expect
     references/target-normal
     "
| 0 | 1 | 0 | 1 | 1 | 1 | 2 | 2 |
| z | 1 | 0 | 1 | 1 | 1 | 2 | 2 |
|   | 1 | 0 | 1 | 1 | 1 | 2 | 2 |
|   |   | 0 | 0 | 0 | 0 | 2 | 2 |
"
     1 lisp calc)
    (org-test-table-target-expect
     references/target-special
     "
|  nan | 1 | 0 | 1 | 1 | 1 | 2 | 2 |
| uinf | 1 | 0 | 1 | 1 | 1 | 2 | 2 |
| -inf | 1 | 0 | 1 | 1 | 1 | 2 | 2 |
|  inf | 1 | 0 | 1 | 1 | 1 | 2 | 2 |
"
     1 calc)))

(ert-deftest test-org-table/references/mode-string-L ()
  "Basic: Assign field reference, sum of field references, sum
and len of simple range reference (no row) and complex range
reference (with row).  Mode string L."
  (org-test-table-target-expect
   references/target-normal
   ;; All the #ERROR show that for Lisp calculations N has to be used.
   "
| 0 | 1 | 0 |      1 |      1 |      1 | 2 | 2 |
| z | 1 | z | #ERROR | #ERROR | #ERROR | 2 | 2 |
|   | 1 |   |      1 |      1 |      1 | 1 | 1 |
|   |   |   |      0 |      0 |      0 | 0 | 0 |
"
   1 (concat
      "#+TBLFM: $3 = '(identity \"$1\"); L :: $4 = '(+ $1 $2); L :: "
      "$5 = '(+ $1..$2); L :: $6 = '(+ @0$1..@0$2); L :: "
      "$7 = '(length '($1..$2)); L :: $8 = '(length '(@0$1..@0$2)); L")))

(ert-deftest test-org-table/references/mode-string-none ()
  "Basic: Assign field reference, sum of field references, sum
and len of simple range reference (no row) and complex range
reference (with row).  No mode string."
  (let ((lisp (concat
	       "#+TBLFM: $3 = '(identity $1) :: $4 = '(+ $1 $2) :: "
	       "$5 = '(+ $1..$2) :: $6 = '(+ @0$1..@0$2) :: "
	       "$7 = '(length '($1..$2)) :: $8 = '(length '(@0$1..@0$2))"))
	(calc (concat
	       "#+TBLFM: $3 = $1 :: $4 = $1 + $2 :: "
	       "$5 = vsum($1..$2) :: $6 = vsum(@0$1..@0$2) :: "
	       "$7 = vlen($1..$2) :: $8 = vlen(@0$1..@0$2)")))
    (org-test-table-target-expect
     references/target-normal
     ;; All the #ERROR show that for Lisp calculations N has to be used.
     "
| 0 | 1 | 0 | #ERROR | #ERROR | #ERROR | 2 | 2 |
| z | 1 | z | #ERROR | #ERROR | #ERROR | 2 | 2 |
|   | 1 |   | #ERROR | #ERROR | #ERROR | 1 | 1 |
|   |   |   | #ERROR | 0      | 0      | 0 | 0 |
"
     1 lisp)
    (org-test-table-target-expect
     references/target-normal
     "
| 0 | 1 | 0 |     1 |     1 |     1 | 2 | 2 |
| z | 1 | z | z + 1 | z + 1 | z + 1 | 2 | 2 |
|   | 1 | 0 |     1 |     1 |     1 | 1 | 1 |
|   |   | 0 |     0 |     0 |     0 | 0 | 0 |
"
     1 calc)
    (org-test-table-target-expect
     references/target-special
     "
|  nan | 1 |  nan |  nan |  nan |  nan | 2 | 2 |
| uinf | 1 | uinf | uinf | uinf | uinf | 2 | 2 |
| -inf | 1 | -inf | -inf | -inf | -inf | 2 | 2 |
|  inf | 1 |  inf |  inf |  inf |  inf | 2 | 2 |
"
     1 calc)))

(ert-deftest test-org-table/references/mode-string-N ()
  "Basic: Assign field reference, sum of field references, sum
and len of simple range reference (no row) and complex range
reference (with row).  Mode string N."
  (let ((lisp
	 (concat
	  "#+TBLFM: $3 = '(identity $1); N :: $4 = '(+ $1 $2); N :: "
	  "$5 = '(+ $1..$2); N :: $6 = '(+ @0$1..@0$2); N :: "
	  "$7 = '(length '($1..$2)); N :: $8 = '(length '(@0$1..@0$2)); N"))
        (calc
	 (concat
	  "#+TBLFM: $3 = $1; N :: $4 = $1 + $2; N :: "
	  "$5 = vsum($1..$2); N :: $6 = vsum(@0$1..@0$2); N :: "
	  "$7 = vlen($1..$2); N :: $8 = vlen(@0$1..@0$2); N")))
    (org-test-table-target-expect
     references/target-normal
     "
| 0 | 1 | 0 | 1 | 1 | 1 | 2 | 2 |
| z | 1 | 0 | 1 | 1 | 1 | 2 | 2 |
|   | 1 | 0 | 1 | 1 | 1 | 1 | 1 |
|   |   | 0 | 0 | 0 | 0 | 0 | 0 |
"
     1 lisp calc)
    (org-test-table-target-expect
     references/target-special
     "
|  nan | 1 | 0 | 1 | 1 | 1 | 2 | 2 |
| uinf | 1 | 0 | 1 | 1 | 1 | 2 | 2 |
| -inf | 1 | 0 | 1 | 1 | 1 | 2 | 2 |
|  inf | 1 | 0 | 1 | 1 | 1 | 2 | 2 |
"
     1 calc)))

(ert-deftest test-org-table/compare ()
  "Basic: Compare field references in Calc."
  (org-test-table-target-expect
   "
|      | 0    | z    |      | nan  | uinf | -inf | inf  |
|------+------+------+------+------+------+------+------|
|    0 | repl | repl | repl | repl | repl | repl | repl |
|    z | repl | repl | repl | repl | repl | repl | repl |
|      | repl | repl | repl | repl | repl | repl | repl |
|  nan | repl | repl | repl | repl | repl | repl | repl |
| uinf | repl | repl | repl | repl | repl | repl | repl |
| -inf | repl | repl | repl | repl | repl | repl | repl |
|  inf | repl | repl | repl | repl | repl | repl | repl |
"
   "
|      | 0 | z |   | nan | uinf | -inf | inf |
|------+---+---+---+-----+------+------+-----|
|    0 | x |   |   |     |      |      |     |
|    z |   | x |   |     |      |      |     |
|      |   |   | x |     |      |      |     |
|  nan |   |   |   |   x |      |      |     |
| uinf |   |   |   |     |    x |      |     |
| -inf |   |   |   |     |      |    x |     |
|  inf |   |   |   |     |      |      |   x |
"
   1
   ;; Compare field reference ($1) with field reference (@1)
   "#+TBLFM: @I$<<..@>$> = if(\"$1\" == \"@1\", x, string(\"\")); E"
   ;; Compare field reference ($1) with absolute term
   (concat "#+TBLFM: "
	   "$2 = if(\"$1\" == \"(0)\"   , x, string(\"\")); E :: "
	   "$3 = if(\"$1\" == \"(z)\"   , x, string(\"\")); E :: "
	   "$4 = if(\"$1\" == \"nan\"   , x, string(\"\")); E :: "
	   "$5 = if(\"$1\" == \"(nan)\" , x, string(\"\")); E :: "
	   "$6 = if(\"$1\" == \"(uinf)\", x, string(\"\")); E :: "
	   "$7 = if(\"$1\" == \"(-inf)\", x, string(\"\")); E :: "
	   "$8 = if(\"$1\" == \"(inf)\" , x, string(\"\")); E"))

  ;; Check field reference converted from an empty field: Despite this
  ;; field reference will not end up in a result, Calc evaluates it.
  ;; Make sure that also then there is no Calc error.
  (org-test-table-target-expect
   "
|   0 | replace |
|   z | replace |
|     | replace |
| nan | replace |
"
   "
|   0 |     1 |
|   z | z + 1 |
|     |       |
| nan |   nan |
"
   1 "#+TBLFM: $2 = if(\"$1\" == \"nan\", string(\"\"), $1 + 1); E"))

(ert-deftest test-org-table/empty-field ()
  "Examples how to deal with empty fields."
  ;; Test if one field is empty, else do a calculation
  (org-test-table-target-expect
   "
| -1 | replace |
|  0 | replace |
|    | replace |
"
   "
| -1 | 0 |
|  0 | 1 |
|    |   |
"
   1
   ;; Calc formula
   "#+TBLFM: $2 = if(\"$1\" == \"nan\", string(\"\"), $1 + 1); E"
   ;; Lisp formula
   "#+TBLFM: $2 = '(if (eq \"$1\" \"\") \"\" (1+ $1)); L")

  ;; Test if several fields are empty, else do a calculation
  (org-test-table-target-expect
   "
| 1 | 2 | replace |
| 4 |   | replace |
|   | 8 | replace |
|   |   | replace |
"
   "
| 1 | 2 | 3 |
| 4 |   |   |
|   | 8 |   |
|   |   |   |
"
   1
   ;; Calc formula
   (concat "#+TBLFM: $3 = if(\"$1\" == \"nan\" || \"$2\" == \"nan\", "
	   "string(\"\"), $1 + $2); E")
   ;; Lisp formula
   (concat "#+TBLFM: $3 = '(if (or (eq \"$1\" \"\") (eq \"$2\" \"\")) "
	   "\"\" (+ $1 $2)); L"))

  ;; $2: Use $1 + 0.5 if $1 available, else only reformat $2 if $2 available
  (org-test-table-target-expect
   "
| 1.5 | 0 |
| 3.5 |   |
|     | 5 |
|     |   |
"
   "
| 1.5 | 2.0 |
| 3.5 | 4.0 |
|     | 5.0 |
|     |     |
"
   1
   ;; Calc formula
   (concat "#+TBLFM: $2 = if(\"$1\" == \"nan\", "
	   "if(\"$2\" == \"nan\", string(\"\"), $2 +.0), $1 + 0.5); E f-1")
   ;; Lisp formula not implemented yet
   )

  ;; Empty fields in simple and complex range reference
  (org-test-table-target-expect
   "
|   |   |   |   | repl | repl | repl | repl | repl | repl |
|   |   | 5 | 7 | repl | repl | repl | repl | repl | repl |
| 1 | 3 | 5 | 7 | repl | repl | repl | repl | repl | repl |
"
   "
|   |   |   |   |   |   |   |   | 0 | 0 |
|   |   | 5 | 7 |   |   | 6 | 6 | 3 | 3 |
| 1 | 3 | 5 | 7 | 4 | 4 | 4 | 4 | 4 | 4 |
"
   1
   ;; Calc formula
   (concat
    "#+TBLFM: "
    "$5 = if(typeof(vmean($1..$4)) == 12, "
    "string(\"\"), vmean($1..$4)); E :: "
    "$6 = if(typeof(vmean(@0$1..@0$4)) == 12, "
    "string(\"\"), vmean(@0$1..@0$4)); E :: "
    "$7 = if(\"$1..$4\" == \"[]\", string(\"\"), vmean($1..$4)) :: "
    "$8 = if(\"@0$1..@0$4\" == \"[]\", string(\"\"), vmean(@0$1..@0$4)) :: "
    "$9 = vmean($1..$4); EN :: "
    "$10 = vmean(@0$1..@0$4); EN")
   ;; Lisp formula
   (concat
    "#+TBLFM: "
    "$5 = '(let ((l '($1..$4))) (if (member \"\" l) \"\" "
    "(/ (apply '+ (mapcar 'string-to-number l)) (length l)))); E :: "
    "$6 = '(let ((l '(@0$1..@0$4))) (if (member \"\" l) \"\" "
    "(/ (apply '+ (mapcar 'string-to-number l)) (length l)))); E :: "
    "$7 = '(let ((l '($1..$4))) "
    "(if l (/ (apply '+ l) (length l)) \"\")); N :: "
    "$8 = '(let ((l '(@0$1..@0$4))) "
    "(if l (/ (apply '+ l) (length l)) \"\")); N :: "
    "$9 = '(/ (+ $1..$4) (length '($1..$4))); EN :: "
    "$10 = '(/ (+ @0$1..@0$4) (length '(@0$1..@0$4))); EN")
))

(ert-deftest test-org-table/copy-field ()
  "Experiments on how to copy one field into another field."
  (let ((target
	 "
| 0                | replace |
| a b              | replace |
| c   d            | replace |
|                  | replace |
| 2012-12          | replace |
| [2012-12-31 Mon] | replace |
"))
    ;; Lisp formula to copy literally
    (org-test-table-target-expect
     target
     "
| 0                | 0                |
| a b              | a b              |
| c   d            | c   d            |
|                  |                  |
| 2012-12          | 2012-12          |
| [2012-12-31 Mon] | [2012-12-31 Mon] |
"
     1 "#+TBLFM: $2 = '(identity $1)")

    ;; Calc formula to copy quite literally
    (org-test-table-target-expect
     target
     "
| 0                | 0                |
| a b              | a b              |
| c   d            | c   d            |
|                  |                  |
| 2012-12          | 2012-12          |
| [2012-12-31 Mon] | <2012-12-31 Mon> |
"
     1 (concat "#+TBLFM: $2 = if(\"$1\" == \"nan\", "
	       "string(\"\"), string(subvec(\"$1\", 2, vlen(\"$1\")))); E"))

    ;; Calc formula simple
    (org-test-table-target-expect
     target
     "
| 0                | 0                |
| a b              | a b              |
| c   d            | c d              |
|                  |                  |
| 2012-12          | 2000             |
| [2012-12-31 Mon] | <2012-12-31 Mon> |
"
     1 "#+TBLFM: $2 = if(\"$1\" == \"nan\", string(\"\"), $1); E")))

;; End of table examples and beginning of internal tests.

(ert-deftest test-org-table/org-table-make-reference/mode-string-EL ()
  (fset 'f 'org-table-make-reference)
  ;; For Lisp formula only
  (should (equal "0"   (f   "0"      t nil 'literal)))
  (should (equal "z"   (f   "z"      t nil 'literal)))
  (should (equal  ""   (f   ""       t nil 'literal)))
  (should (equal "0 1" (f '("0" "1") t nil 'literal)))
  (should (equal "z 1" (f '("z" "1") t nil 'literal)))
  (should (equal  " 1" (f '(""  "1") t nil 'literal)))
  (should (equal  " "  (f '(""  "" ) t nil 'literal))))

(ert-deftest test-org-table/org-table-make-reference/mode-string-E ()
  (fset 'f 'org-table-make-reference)
  ;; For Lisp formula
  (should (equal "\"0\""       (f   "0"         t nil t)))
  (should (equal "\"z\""       (f   "z"         t nil t)))
  (should (equal  "\"\""       (f   ""          t nil t)))
  (should (equal "\"0\" \"1\"" (f '("0"    "1") t nil t)))
  (should (equal "\"z\" \"1\"" (f '("z"    "1") t nil t)))
  (should (equal  "\"\" \"1\"" (f '(""     "1") t nil t)))
  (should (equal  "\"\" \"\""  (f '(""     "" ) t nil t)))
  ;; For Calc formula
  (should (equal  "(0)"        (f   "0"         t nil nil)))
  (should (equal  "(z)"        (f   "z"         t nil nil)))
  (should (equal  "nan"        (f   ""          t nil nil)))
  (should (equal  "[0,1]"      (f '("0"    "1") t nil nil)))
  (should (equal  "[z,1]"      (f '("z"    "1") t nil nil)))
  (should (equal  "[nan,1]"    (f '(""     "1") t nil nil)))
  (should (equal  "[nan,nan]"  (f '(""     "" ) t nil nil)))
  ;; For Calc formula, special numbers
  (should (equal  "(nan)"      (f    "nan"      t nil nil)))
  (should (equal "(uinf)"      (f   "uinf"      t nil nil)))
  (should (equal "(-inf)"      (f   "-inf"      t nil nil)))
  (should (equal  "(inf)"      (f    "inf"      t nil nil)))
  (should (equal  "[nan,1]"    (f '( "nan" "1") t nil nil)))
  (should (equal "[uinf,1]"    (f '("uinf" "1") t nil nil)))
  (should (equal "[-inf,1]"    (f '("-inf" "1") t nil nil)))
  (should (equal  "[inf,1]"    (f '( "inf" "1") t nil nil))))

(ert-deftest test-org-table/org-table-make-reference/mode-string-EN ()
  (fset 'f 'org-table-make-reference)
  ;; For Lisp formula
  (should (equal  "0"    (f   "0"         t t t)))
  (should (equal  "0"    (f   "z"         t t t)))
  (should (equal  "0"    (f   ""          t t t)))
  (should (equal  "0 1"  (f '("0"    "1") t t t)))
  (should (equal  "0 1"  (f '("z"    "1") t t t)))
  (should (equal  "0 1"  (f '(""     "1") t t t)))
  (should (equal  "0 0"  (f '(""     "" ) t t t)))
  ;; For Calc formula
  (should (equal "(0)"   (f   "0"         t t nil)))
  (should (equal "(0)"   (f   "z"         t t nil)))
  (should (equal "(0)"   (f   ""          t t nil)))
  (should (equal "[0,1]" (f '("0"    "1") t t nil)))
  (should (equal "[0,1]" (f '("z"    "1") t t nil)))
  (should (equal "[0,1]" (f '(""     "1") t t nil)))
  (should (equal "[0,0]" (f '(""     "" ) t t nil)))
  ;; For Calc formula, special numbers
  (should (equal "(0)"   (f    "nan"      t t nil)))
  (should (equal "(0)"   (f   "uinf"      t t nil)))
  (should (equal "(0)"   (f   "-inf"      t t nil)))
  (should (equal "(0)"   (f    "inf"      t t nil)))
  (should (equal "[0,1]" (f '( "nan" "1") t t nil)))
  (should (equal "[0,1]" (f '("uinf" "1") t t nil)))
  (should (equal "[0,1]" (f '("-inf" "1") t t nil)))
  (should (equal "[0,1]" (f '( "inf" "1") t t nil))))

(ert-deftest test-org-table/org-table-make-reference/mode-string-L ()
  (fset 'f 'org-table-make-reference)
  ;; For Lisp formula only
  (should (equal "0"   (f   "0"      nil nil 'literal)))
  (should (equal "z"   (f   "z"      nil nil 'literal)))
  (should (equal  ""   (f   ""       nil nil 'literal)))
  (should (equal "0 1" (f '("0" "1") nil nil 'literal)))
  (should (equal "z 1" (f '("z" "1") nil nil 'literal)))
  (should (equal   "1" (f '(""  "1") nil nil 'literal)))
  (should (equal  ""   (f '(""  "" ) nil nil 'literal))))

(ert-deftest test-org-table/org-table-make-reference/mode-string-none ()
  (fset 'f 'org-table-make-reference)
  ;; For Lisp formula
  (should (equal "\"0\""       (f   "0"         nil nil t)))
  (should (equal "\"z\""       (f   "z"         nil nil t)))
  (should (equal   ""          (f   ""          nil nil t)))
  (should (equal "\"0\" \"1\"" (f '("0"    "1") nil nil t)))
  (should (equal "\"z\" \"1\"" (f '("z"    "1") nil nil t)))
  (should (equal       "\"1\"" (f '(""     "1") nil nil t)))
  (should (equal      ""       (f '(""     "" ) nil nil t)))
  ;; For Calc formula
  (should (equal  "(0)"        (f   "0"         nil nil nil)))
  (should (equal  "(z)"        (f   "z"         nil nil nil)))
  (should (equal  "(0)"        (f   ""          nil nil nil)))
  (should (equal  "[0,1]"      (f '("0"    "1") nil nil nil)))
  (should (equal  "[z,1]"      (f '("z"    "1") nil nil nil)))
  (should (equal    "[1]"      (f '(""     "1") nil nil nil)))
  (should (equal   "[]"        (f '(""     "" ) nil nil nil)))
  ;; For Calc formula, special numbers
  (should (equal  "(nan)"      (f    "nan"      nil nil nil)))
  (should (equal "(uinf)"      (f   "uinf"      nil nil nil)))
  (should (equal "(-inf)"      (f   "-inf"      nil nil nil)))
  (should (equal  "(inf)"      (f    "inf"      nil nil nil)))
  (should (equal  "[nan,1]"    (f '( "nan" "1") nil nil nil)))
  (should (equal "[uinf,1]"    (f '("uinf" "1") nil nil nil)))
  (should (equal "[-inf,1]"    (f '("-inf" "1") nil nil nil)))
  (should (equal  "[inf,1]"    (f '( "inf" "1") nil nil nil))))

(ert-deftest test-org-table/org-table-make-reference/mode-string-N ()
  (fset 'f 'org-table-make-reference)
  ;; For Lisp formula
  (should (equal  "0"    (f   "0"         nil t t)))
  (should (equal  "0"    (f   "z"         nil t t)))
  (should (equal  ""     (f   ""          nil t t)))
  (should (equal  "0 1"  (f '("0"    "1") nil t t)))
  (should (equal  "0 1"  (f '("z"    "1") nil t t)))
  (should (equal    "1"  (f '(""     "1") nil t t)))
  (should (equal   ""    (f '(""     "" ) nil t t)))
  ;; For Calc formula
  (should (equal "(0)"   (f   "0"         nil t nil)))
  (should (equal "(0)"   (f   "z"         nil t nil)))
  (should (equal "(0)"   (f   ""          nil t nil)))
  (should (equal "[0,1]" (f '("0"    "1") nil t nil)))
  (should (equal "[0,1]" (f '("z"    "1") nil t nil)))
  (should (equal   "[1]" (f '(""     "1") nil t nil)))
  (should (equal  "[]"   (f '(""     "" ) nil t nil)))
  ;; For Calc formula, special numbers
  (should (equal "(0)"   (f    "nan"      nil t nil)))
  (should (equal "(0)"   (f   "uinf"      nil t nil)))
  (should (equal "(0)"   (f   "-inf"      nil t nil)))
  (should (equal "(0)"   (f    "inf"      nil t nil)))
  (should (equal "[0,1]" (f '( "nan" "1") nil t nil)))
  (should (equal "[0,1]" (f '("uinf" "1") nil t nil)))
  (should (equal "[0,1]" (f '("-inf" "1") nil t nil)))
  (should (equal "[0,1]" (f '( "inf" "1") nil t nil))))

(ert-deftest test-org-table/org-table-convert-refs-to-an/1 ()
  "Simple reference @1$1."
  (should
   (string= "A1" (org-table-convert-refs-to-an "@1$1"))))

;; TODO: Test broken
;; (ert-deftest test-org-table/org-table-convert-refs-to-an/2 ()
;;   "Self reference @1$1."
;;   (should
;;    (string= "A1 = $0" (org-table-convert-refs-to-an "@1$1 = $0"))))

(ert-deftest test-org-table/org-table-convert-refs-to-an/3 ()
  "Remote reference."
  (should
   (string= "C& = remote(FOO, @@#B&)" (org-table-convert-refs-to-an "$3 = remote(FOO, @@#$2)"))))

(ert-deftest test-org-table/org-table-convert-refs-to-rc/1 ()
  "Simple reference @1$1."
  (should
   (string= "@1$1" (org-table-convert-refs-to-rc "A1"))))

(ert-deftest test-org-table/org-table-convert-refs-to-rc/2 ()
  "Self reference $0."
  (should
   (string= "@1$1 = $0" (org-table-convert-refs-to-rc "A1 = $0"))))

;; TODO: Test Broken
;; (ert-deftest test-org-table/org-table-convert-refs-to-rc/3 ()
;;   "Remote reference."
;;   (should
;;    (string= "$3 = remote(FOO, @@#$2)" (org-table-convert-refs-to-rc "C& = remote(FOO, @@#B&)"))))

(ert-deftest test-org-table/remote-reference-access ()
  "Access to remote reference."
  (org-test-table-target-expect
   "
#+NAME: table
|   | 42 |

| replace |   |
"
   "
#+NAME: table
|   | 42 |

| 42 |   |
"
   1 "#+TBLFM: $1 = remote(table, @1$2)"))

(ert-deftest test-org-table/org-at-TBLFM-p ()
  (org-test-with-temp-text-in-file
      "
| 1 |
| 2 |
#+TBLFM: $2=$1*2

"
    (goto-char (point-min))
    (forward-line 2)
    (should (equal (org-at-TBLFM-p) nil))

    (goto-char (point-min))
    (forward-line 3)
    (should (equal (org-at-TBLFM-p) t))

    (goto-char (point-min))
    (forward-line 4)
    (should (equal (org-at-TBLFM-p) nil))))

(ert-deftest test-org-table/org-table-TBLFM-begin ()
  (org-test-with-temp-text-in-file
      "
| 1 |
| 2 |
#+TBLFM: $2=$1*2

"
    (goto-char (point-min))
    (should (equal (org-table-TBLFM-begin)
		   nil))

    (goto-char (point-min))
    (forward-line 1)
    (should (equal (org-table-TBLFM-begin)
		   nil))

    (goto-char (point-min))
    (forward-line 3)
    (should (= (org-table-TBLFM-begin)
		   14))

    (goto-char (point-min))
    (forward-line 4)
    (should (= (org-table-TBLFM-begin)
		   14))

    ))

(ert-deftest test-org-table/org-table-TBLFM-begin-for-multiple-TBLFM-lines ()
  "For multiple #+TBLFM lines."
  (org-test-with-temp-text-in-file
      "
| 1 |
| 2 |
#+TBLFM: $2=$1*1
#+TBLFM: $2=$1*2

"
    (goto-char (point-min))
    (should (equal (org-table-TBLFM-begin)
		   nil))

    (goto-char (point-min))
    (forward-line 1)
    (should (equal (org-table-TBLFM-begin)
		   nil))

    (goto-char (point-min))
    (forward-line 3)
    (should (= (org-table-TBLFM-begin)
		   14))

    (goto-char (point-min))
    (forward-line 4)
    (should (= (org-table-TBLFM-begin)
		   14))

    (goto-char (point-min))
    (forward-line 5)
    (should (= (org-table-TBLFM-begin)
		   14))

    ))

(ert-deftest test-org-table/org-table-TBLFM-begin-for-pultiple-TBLFM-lines-blocks ()
  (org-test-with-temp-text-in-file
      "
| 1 |
| 2 |
#+TBLFM: $2=$1*1
#+TBLFM: $2=$1*2

| 6 |
| 7 |
#+TBLFM: $2=$1*1
#+TBLFM: $2=$1*2

"
    (goto-char (point-min))
    (should (equal (org-table-TBLFM-begin)
		   nil))

    (goto-char (point-min))
    (forward-line 1)
    (should (equal (org-table-TBLFM-begin)
		   nil))

    (goto-char (point-min))
    (forward-line 3)
    (should (= (org-table-TBLFM-begin)
		   14))

    (goto-char (point-min))
    (forward-line 4)
    (should (= (org-table-TBLFM-begin)
		   14))

    (goto-char (point-min))
    (forward-line 5)
    (should (= (org-table-TBLFM-begin)
		   14))

    (goto-char (point-min))
    (forward-line 6)
    (should (= (org-table-TBLFM-begin)
		   14))

    (goto-char (point-min))
    (forward-line 8)
    (should (= (org-table-TBLFM-begin)
		   61))

    (goto-char (point-min))
    (forward-line 9)
    (should (= (org-table-TBLFM-begin)
		   61))

    (goto-char (point-min))
    (forward-line 10)
    (should (= (org-table-TBLFM-begin)
		   61))))

(ert-deftest test-org-table/org-table-calc-current-TBLFM ()
    (org-test-with-temp-text-in-file
      "
| 1 |   |
| 2 |   |
#+TBLFM: $2=$1*1
#+TBLFM: $2=$1*2
#+TBLFM: $2=$1*3
"
    (let ((got (progn (goto-char (point-min))
		      (forward-line 3)
		      (org-table-calc-current-TBLFM)
		      (buffer-string)))
	  (expect "
| 1 | 1 |
| 2 | 2 |
#+TBLFM: $2=$1*1
#+TBLFM: $2=$1*2
#+TBLFM: $2=$1*3
"))
      (should (string= got
		       expect)))

    (let ((got (progn (goto-char (point-min))
		      (forward-line 4)
		      (org-table-calc-current-TBLFM)
		      (buffer-string)))
	  (expect "
| 1 | 2 |
| 2 | 4 |
#+TBLFM: $2=$1*1
#+TBLFM: $2=$1*2
#+TBLFM: $2=$1*3
"))
      (should (string= got
		       expect)))))

(ert-deftest test-org-table/org-table-calc-current-TBLFM-when-stop-because-of-error ()
  "org-table-calc-current-TBLFM should preserve the input as it was."
  (org-test-with-temp-text-in-file
      "
| 1 | 1 |
| 2 | 2 |
#+TBLFM: $2=$1*1
#+TBLFM: $2=$1*2::$2=$1*2
#+TBLFM: $2=$1*3
"
    (let ((expect "
| 1 | 1 |
| 2 | 2 |
#+TBLFM: $2=$1*1
#+TBLFM: $2=$1*2::$2=$1*2
#+TBLFM: $2=$1*3
"))
      (goto-char (point-min))
      (forward-line 4)
      (should-error (org-table-calc-current-TBLFM))
      (setq got (buffer-string))
      (message "%s" got)
      (should (string= got
		       expect)))))

(provide 'test-org-table)

;;; test-org-table.el ends here
