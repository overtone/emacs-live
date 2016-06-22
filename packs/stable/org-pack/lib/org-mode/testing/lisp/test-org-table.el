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

;; Template test file for Org-mode tests.  Many tests are also a howto
;; example collection as a user documentation, more or less all those
;; using `org-test-table-target-expect'.  See also the doc string of
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

(ert-deftest test-org-table/lisp-return-value ()
  "Basic: Return value of Lisp formulas."
  (org-test-table-target-expect
   "
|                         | nil         | (list) | '() |
|-------------------------+-------------+--------+-----|
| type-of, no L           | replace (r) | r      | r   |
| type-of identity, no L  | r           | r      | r   |
| identity, no L          | r           | r      | r   |
|-------------------------+-------------+--------+-----|
| type-of \"@1\"            | r           | r      | r   |
| type-of (identity \"@1\") | r           | r      | r   |
| identity \"@1\"           | r           | r      | r   |
|-------------------------+-------------+--------+-----|
| type-of @1              | r           | r      | r   |
| type-of (identity @1)   | r           | r      | r   |
| identity @1             | r           | r      | r   |
"
   "
|                         | nil    | (list) | '()    |
|-------------------------+--------+--------+--------|
| type-of, no L           | string | string | string |
| type-of identity, no L  | string | string | string |
| identity, no L          | nil    | (list) | '()    |
|-------------------------+--------+--------+--------|
| type-of \"@1\"            | string | string | string |
| type-of (identity \"@1\") | string | string | string |
| identity \"@1\"           | nil    | (list) | '()    |
|-------------------------+--------+--------+--------|
| type-of @1              | symbol | symbol | symbol |
| type-of (identity @1)   | symbol | symbol | symbol |
| identity @1             | nil    | nil    | nil    |
"
   1 (concat "#+TBLFM: @2$<<..@2$> = '(type-of @1) :: "
	     "@3$<<..@3$> = '(type-of (identity @1)) :: "
	     "@4$<<..@4$> = '(identity @1) :: @5$<<..@>$> = '(@0$1); L")))

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
   "#+TBLFM: @<<$<<..@>$> = if(\"$1\" == \"@1\", x, string(\"\")); E"
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
  "Experiments on how to copy one field into another field.
See also `test-org-table/remote-reference-access'."
  (let ((target "
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

(ert-deftest test-org-table/sub-total ()
  "Grouped rows with sub-total.
Begin range with \"@II\" to handle multiline header.  Convert
integer to float with \"+.0\" for sub-total of items c1 and c2.
Sum empty fields as value zero but without ignoring them for
\"vlen\" with format specifier \"EN\".  Format possibly empty
results with the Calc formatter \"f-1\" instead of the printf
formatter \"%.1f\"."
  (org-test-table-target-expect
   "
|-------+---------+---------|
| Item  |    Item | Sub-    |
| name  |   value | total   |
|-------+---------+---------|
| a1    |     4.1 | replace |
| a2    |     8.2 | replace |
| a3    |         | replace |
|-------+---------+---------|
| b1    |    16.0 | replace |
|-------+---------+---------|
| c1    |      32 | replace |
| c2    |      64 | replace |
|-------+---------+---------|
| Total | replace | replace |
|-------+---------+---------|
"
   "
|-------+-------+-------|
| Item  |  Item |  Sub- |
| name  | value | total |
|-------+-------+-------|
| a1    |   4.1 |       |
| a2    |   8.2 |       |
| a3    |       |  12.3 |
|-------+-------+-------|
| b1    |  16.0 |  16.0 |
|-------+-------+-------|
| c1    |    32 |       |
| c2    |    64 |  96.0 |
|-------+-------+-------|
| Total | 124.3 |       |
|-------+-------+-------|
"
   1 (concat "#+TBLFM: @>$2 = vsum(@II..@>>) ::"
	     "$3 = if(vlen(@0..@+I) == 1, "
	     "vsum(@-I$2..@+I$2) +.0, string(\"\")); EN f-1 :: "
	     "@>$3 = string(\"\")")))

(ert-deftest test-org-table/org-lookup-all ()
  "Use `org-lookup-all' for several GROUP BY as in SQL and for ranking.
See also http://orgmode.org/worg/org-tutorials/org-lookups.html ."
  (let ((data "
#+NAME: data
| Purchase | Product | Shop | Rating |
|----------+---------+------+--------|
| a        | p1      | s1   |      1 |
| b        | p1      | s2   |      4 |
| c        | p2      | s1   |      2 |
| d        | p3      | s2   |      8 |
"))

    ;; Product rating and ranking by average purchase from "#+NAME: data"
    (org-test-table-target-expect
     (concat data "
| Product | Rating  | Ranking |
|---------+---------+---------|
| p1      | replace | replace |
| p2      | replace | replace |
| p3      | replace | replace |
")
     (concat data "
| Product | Rating | Ranking |
|---------+--------+---------|
| p1      |    2.5 |       2 |
| p2      |    2.0 |       3 |
| p3      |    8.0 |       1 |
")
    2 (concat
       "#+TBLFM: $2 = '(let ((all (org-lookup-all '$1 "
       "'(remote(data, @I$2..@>$2)) '(remote(data, @I$4..@>$4))))) "
       "(/ (apply '+ all) (length all) 1.0)); L :: "
       "$3 = '(+ 1 (length (org-lookup-all $2 '(@I$2..@>$2) nil '<))); N"))

    ;; Shop rating and ranking by average purchase from "#+NAME: data"
    (org-test-table-target-expect
     (concat data "
| Shop | Rating  | Ranking |
|------+---------+---------|
| s1   | replace | replace |
| s2   | replace | replace |
")
     (concat data "
| Shop | Rating | Ranking |
|------+--------+---------|
| s1   |    1.5 |       2 |
| s2   |    6.0 |       1 |
")
     2 (concat
       "#+TBLFM: $2 = '(let ((all (org-lookup-all '$1 "
       "'(remote(data, @I$3..@>$3)) '(remote(data, @I$4..@>$4))))) "
       "(/ (apply '+ all) (length all) 1.0)); L :: "
       "$3 = '(+ 1 (length (org-lookup-all $2 '(@I$2..@>$2) nil '<))); N"))))

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
  "Access to remote reference.
See also `test-org-table/copy-field'."
  (org-test-table-target-expect
   "
#+NAME: table
|   | x   42 |   |

| replace | replace |
"
   "
#+NAME: table
|   | x   42 |   |

| x   42 | 84 x |
"
   1 (concat "#+TBLFM: "
	     ;; Copy text without calculation: Use Lisp formula
	     "$1 = '(identity remote(table, @1$2)) :: "
	     ;; Do a calculation: Use Calc (or Lisp ) formula
	     "$2 = 2 * remote(table, @1$2)")))

(ert-deftest test-org-table/remote-reference-indirect ()
  "Access to remote reference with indirection of name or ID."
  (let ((source-tables "
#+NAME: 2012
| amount |
|--------|
|      1 |
|      2 |
|--------|
|      3 |
#+TBLFM: @>$1 = vsum(@I..@II)

#+NAME: 2013
| amount |
|--------|
|      4 |
|      8 |
|--------|
|     12 |
#+TBLFM: @>$1 = vsum(@I..@II)
"))

    ;; Read several remote references from same column
    (org-test-table-target-expect
     (concat source-tables "
#+NAME: summary
|  year | amount  |
|-------+---------|
|  2012 | replace |
|  2013 | replace |
|-------+---------|
| total | replace |
")
     (concat source-tables "
#+NAME: summary
|  year | amount |
|-------+--------|
|  2012 |      3 |
|  2013 |     12 |
|-------+--------|
| total |     15 |
")
     1
     ;; Calc formula
     "#+TBLFM: @<<$2..@>>$2 = remote($<, @>$1) :: @>$2 = vsum(@I..@II)"
     ;; Lisp formula
     (concat "#+TBLFM: @<<$2..@>>$2 = '(identity remote($<, @>$1)); N :: "
	     "@>$2 = '(+ @I..@II); N"))

    ;; Read several remote references from same row
    (org-test-table-target-expect
     (concat source-tables "
#+NAME: summary
| year   |    2012 |    2013 | total   |
|--------+---------+---------+---------|
| amount | replace | replace | replace |
")
     (concat source-tables "
#+NAME: summary
| year   | 2012 | 2013 | total |
|--------+------+------+-------|
| amount |    3 |   12 |    15 |
")
     1
     ;; Calc formula
     "#+TBLFM: @2$<<..@2$>> = remote(@<, @>$1) :: @2$> = vsum($<<..$>>)"
     ;; Lisp formula
     (concat "#+TBLFM: @2$<<..@2$>> = '(identity remote(@<, @>$1)); N :: "
	     "@2$> = '(+ $<<..$>>); N"))))

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

;;; Radio Tables

(ert-deftest test-org-table/to-generic ()
  "Test `orgtbl-to-generic' specifications."
  ;; Test :hline parameter.
  (should
   (equal "a\nb"
	  (orgtbl-to-generic (org-table-to-lisp "| a |\n|---|\n| b |")
			     '(:hline nil))))
  (should
   (equal "a\n~\nb"
	  (orgtbl-to-generic (org-table-to-lisp "| a |\n|---|\n| b |")
			     '(:hline "~"))))
  ;; Test :sep parameter.
  (should
   (equal "a!b\nc!d"
	  (orgtbl-to-generic
	   (org-table-to-lisp "| a | b |\n|---+---|\n| c | d |")
	   '(:sep "!"))))
  ;; Test :hsep parameter.
  (should
   (equal "a!b\nc?d"
	  (orgtbl-to-generic
	   (org-table-to-lisp "| a | b |\n|---+---|\n| c | d |")
	   '(:sep "?" :hsep "!"))))
  ;; Test :tstart parameter.
  (should
   (equal "<begin>\na"
	  (orgtbl-to-generic (org-table-to-lisp "| a |") '(:tstart "<begin>"))))
  (should
   (equal "<begin>\na"
	  (orgtbl-to-generic (org-table-to-lisp "| a |")
			     '(:tstart (lambda () "<begin>")))))
  (should
   (equal "a"
	  (orgtbl-to-generic (org-table-to-lisp "| a |")
			     '(:tstart "<begin>" :splice t))))
  ;; Test :tend parameter.
  (should
   (equal "a\n<end>"
	  (orgtbl-to-generic (org-table-to-lisp "| a |") '(:tend "<end>"))))
  (should
   (equal "a\n<end>"
	  (orgtbl-to-generic (org-table-to-lisp "| a |")
			     '(:tend (lambda () "<end>")))))
  (should
   (equal "a"
	  (orgtbl-to-generic (org-table-to-lisp "| a |")
			     '(:tend "<end>" :splice t))))
  ;; Test :lstart parameter.
  (should
   (equal "> a"
	  (orgtbl-to-generic
	   (org-table-to-lisp "| a |") '(:lstart "> "))))
  (should
   (equal "> a"
	  (orgtbl-to-generic (org-table-to-lisp "| a |")
			     '(:lstart (lambda () "> ")))))
  ;; Test :llstart parameter.
  (should
   (equal "> a\n>> b"
	  (orgtbl-to-generic (org-table-to-lisp "| a |\n|---|\n| b |")
			     '(:lstart "> " :llstart ">> "))))
  ;; Test :hlstart parameter.
  (should
   (equal "!> a\n> b"
	  (orgtbl-to-generic (org-table-to-lisp "| a |\n|---|\n| b |")
			     '(:lstart "> " :hlstart "!> "))))
  ;; Test :hllstart parameter.
  (should
   (equal "!> a\n!!> b\n> c"
	  (orgtbl-to-generic (org-table-to-lisp "| a |\n| b |\n|---|\n| c |")
			     '(:lstart "> " :hlstart "!> " :hllstart "!!> "))))
  ;; Test :lend parameter.
  (should
   (equal "a <"
	  (orgtbl-to-generic (org-table-to-lisp "| a |") '(:lend " <"))))
  ;; Test :llend parameter.
  (should
   (equal "a <\nb <<"
	  (orgtbl-to-generic (org-table-to-lisp "| a |\n|---|\n| b |")
			     '(:lend " <" :llend " <<"))))
  ;; Test :hlend parameter.
  (should
   (equal "a <!\nb <"
	  (orgtbl-to-generic (org-table-to-lisp "| a |\n|---|\n| b |")
			     '(:lend " <" :hlend " <!"))))
  ;; Test :hllend parameter.
  (should
   (equal "a <!\nb <!!\nc <"
	  (orgtbl-to-generic (org-table-to-lisp "| a |\n| b |\n|---|\n| c |")
			     '(:lend " <" :hlend " <!" :hllend " <!!"))))
  ;; Test :lfmt parameter.
  (should
   (equal "a!b"
	  (orgtbl-to-generic (org-table-to-lisp "| a | b |")
			     '(:lfmt "%s!%s"))))
  (should
   (equal "a+b"
	  (orgtbl-to-generic
	   (org-table-to-lisp "| a | b |")
	   '(:lfmt (lambda (c) (concat (car c) "+" (cadr c)))))))
  (should
   (equal "a!b"
	  (orgtbl-to-generic (org-table-to-lisp "| a | b |")
			     '(:lfmt "%s!%s" :lstart ">" :lend "<" :sep " "))))
  ;; Test :llfmt parameter.
  (should
   (equal "a!b"
	  (orgtbl-to-generic (org-table-to-lisp "| a | b |")
			     '(:llfmt "%s!%s"))))
  (should
   (equal "a!b\nc+d"
	  (orgtbl-to-generic
	   (org-table-to-lisp "| a | b |\n| c | d |")
	   '(:lfmt "%s!%s" :llfmt (lambda (c) (concat (car c) "+" (cadr c)))))))
  (should
   (equal "a!b"
	  (orgtbl-to-generic (org-table-to-lisp "| a | b |")
			     '(:llfmt "%s!%s" :lstart ">" :lend "<" :sep " "))))
  ;; Test :hlfmt parameter.
  (should
   (equal "a!b\ncd"
	  (orgtbl-to-generic
	   (org-table-to-lisp "| a | b |\n|---+---|\n| c | d |")
	   '(:hlfmt "%s!%s"))))
  (should
   (equal "a+b\ncd"
	  (orgtbl-to-generic
	   (org-table-to-lisp "| a | b |\n|---+---|\n| c | d |")
	   '(:hlfmt (lambda (c) (concat (car c) "+" (cadr c)))))))
  (should
   (equal "a!b\n>c d<"
	  (orgtbl-to-generic
	   (org-table-to-lisp "| a | b |\n|---+---|\n| c | d |")
	   '(:hlfmt "%s!%s" :lstart ">" :lend "<" :sep " "))))
  ;; Test :hllfmt parameter.
  (should
   (equal "a!b\ncd"
	  (orgtbl-to-generic
	   (org-table-to-lisp "| a | b |\n|---+---|\n| c | d |")
	   '(:hllfmt "%s!%s"))))
  (should
   (equal "a+b\ncd"
	  (orgtbl-to-generic
	   (org-table-to-lisp "| a | b |\n|---+---|\n| c | d |")
	   '(:hllfmt (lambda (c) (concat (car c) "+" (cadr c)))))))
  (should
   (equal "a!b\n>c d<"
	  (orgtbl-to-generic
	   (org-table-to-lisp "| a | b |\n|---+---|\n| c | d |")
	   '(:hllfmt "%s!%s" :lstart ">" :lend "<" :sep " "))))
  ;; Test :fmt parameter.
  (should
   (equal ">a<\n>b<"
	  (orgtbl-to-generic (org-table-to-lisp "| a |\n|---|\n| b |")
			     '(:fmt ">%s<"))))
  (should
   (equal ">a<b"
	  (orgtbl-to-generic (org-table-to-lisp "| a | b |")
			     '(:fmt (1 ">%s<" 2 (lambda (c) c))))))
  (should
   (equal "a b"
	  (orgtbl-to-generic (org-table-to-lisp "| a | b |")
			     '(:fmt (2 " %s")))))
  (should
   (equal ">a<"
	  (orgtbl-to-generic (org-table-to-lisp "| a |")
			     '(:fmt (lambda (c) (format ">%s<" c))))))
  ;; Test :hfmt parameter.
  (should
   (equal ">a<\nb"
	  (orgtbl-to-generic (org-table-to-lisp "| a |\n|---|\n| b |")
			     '(:hfmt ">%s<"))))
  (should
   (equal ">a<b\ncd"
	  (orgtbl-to-generic
	   (org-table-to-lisp "| a | b |\n|---+---|\n| c | d |")
	   '(:hfmt (1 ">%s<" 2 identity)))))
  (should
   (equal "a b\ncd"
	  (orgtbl-to-generic
	   (org-table-to-lisp "| a | b |\n|---+---|\n| c | d |")
	   '(:hfmt (2 " %s")))))
  (should
   (equal ">a<\nb"
	  (orgtbl-to-generic (org-table-to-lisp "| a |\n|---|\n| b |")
			     '(:hfmt (lambda (c) (format ">%s<" c))))))
  ;; Test :efmt parameter.
  (should
   (equal "2x10^3"
	  (orgtbl-to-generic (org-table-to-lisp "| 2e3 |")
			     '(:efmt "%sx10^%s"))))
  (should
   (equal "2x10^3"
	  (orgtbl-to-generic (org-table-to-lisp "| 2e3 |")
			     '(:efmt (lambda (m e) (concat m "x10^" e))))))
  (should
   (equal "2x10^3"
	  (orgtbl-to-generic (org-table-to-lisp "| 2e3 |")
			     '(:efmt (1 "%sx10^%s")))))
  (should
   (equal "2x10^3"
	  (orgtbl-to-generic
	   (org-table-to-lisp "| 2e3 |")
	   '(:efmt (1 (lambda (m e) (format "%sx10^%s" m e)))))))
  (should
   (equal "2e3"
	  (orgtbl-to-generic (org-table-to-lisp "| 2e3 |") '(:efmt nil))))
  ;; Test :skip parameter.
  (should
   (equal "cd"
	  (orgtbl-to-generic
	   (org-table-to-lisp "| \ | <c> |\n| a | b |\n|---+---|\n| c | d |")
	   '(:skip 2))))
  ;; Test :skipcols parameter.
  (should
   (equal "a\nc"
	  (orgtbl-to-generic
	   (org-table-to-lisp "| a | b |\n| c | d |") '(:skipcols (2)))))
  (should
   (equal "a\nc"
	  (orgtbl-to-generic
	   (org-table-to-lisp
	    "| / | <c> | <c> |\n| # | a | b |\n|---+---+---|\n|   | c | d |")
	   '(:skipcols (2)))))
  ;; Test :raw parameter.
  (when (featurep 'ox-latex)
    (should
     (org-string-match-p
      "/a/"
      (orgtbl-to-generic (org-table-to-lisp "| /a/ | b |")
			 '(:backend latex :raw t)))))
  ;; Hooks are ignored.
  (should
   (equal
    "a\nb"
    (let* ((fun-list (list (lambda (backend) (search-forward "a") (insert "hook"))))
	   (org-export-before-parsing-hook fun-list)
	   (org-export-before-processing-hook fun-list))
      (orgtbl-to-generic (org-table-to-lisp "| a |\n|---|\n| b |")
			 '(:hline nil)))))
  ;; User-defined export filters are ignored.
  (should
   (equal
    "a\nb"
    (let ((org-export-filter-table-cell-functions (list (lambda (c b i) "filter"))))
      (orgtbl-to-generic (org-table-to-lisp "| a |\n|---|\n| b |")
			 '(:hline nil)))))
  ;; Macros, even if unknown, are returned as-is.
  (should
   (equal "{{{macro}}}"
	  (orgtbl-to-generic (org-table-to-lisp "| {{{macro}}} |") nil))))

(ert-deftest test-org-table/to-latex ()
  "Test `orgtbl-to-latex' specifications."
  (should
   (equal "\\begin{tabular}{l}\na\\\\\n\\end{tabular}"
	  (orgtbl-to-latex (org-table-to-lisp "| a |") nil)))
  ;; Test :environment parameter.
  (should
   (equal "\\begin{tabularx}{l}\na\\\\\n\\end{tabularx}"
	  (orgtbl-to-latex (org-table-to-lisp "| a |")
			   '(:environment "tabularx"))))
  ;; Test :booktabs parameter.
  (should
   (org-string-match-p
    "\\toprule" (orgtbl-to-latex (org-table-to-lisp "| a |") '(:booktabs t))))
  ;; Test pseudo objects and :raw parameter.
  (should
   (org-string-match-p
    "\\$x\\$" (orgtbl-to-latex (org-table-to-lisp "| $x$ |") '(:raw t)))))

(ert-deftest test-org-table/to-html ()
  "Test `orgtbl-to-html' specifications."
  (should
   (equal (orgtbl-to-html (org-table-to-lisp "| a |") nil)
	  "<table border=\"2\" cellspacing=\"0\" cellpadding=\"6\" rules=\"groups\" frame=\"hsides\">


<colgroup>
<col  class=\"org-left\" />
</colgroup>
<tbody>
<tr>
<td class=\"org-left\">a</td>
</tr>
</tbody>
</table>"))
  ;; Test :attributes parameter.
  (should
   (org-string-match-p
    "<table>"
    (orgtbl-to-html (org-table-to-lisp "| a |") '(:attributes nil))))
  (should
   (org-string-match-p
    "<table border=\"2\">"
    (orgtbl-to-html (org-table-to-lisp "| a |") '(:attributes (:border "2"))))))

(ert-deftest test-org-table/to-texinfo ()
  "Test `orgtbl-to-texinfo' specifications."
  (should
   (equal "@multitable {a}\n@item a\n@end multitable"
	  (orgtbl-to-texinfo (org-table-to-lisp "| a |") nil)))
  ;; Test :columns parameter.
  (should
   (equal "@multitable @columnfractions .4 .6\n@item a\n@tab b\n@end multitable"
	  (orgtbl-to-texinfo (org-table-to-lisp "| a | b |")
			     '(:columns ".4 .6"))))
  (should
   (equal "@multitable @columnfractions .4 .6\n@item a\n@tab b\n@end multitable"
	  (orgtbl-to-texinfo (org-table-to-lisp "| a | b |")
			     '(:columns "@columnfractions .4 .6"))))
  (should
   (equal "@multitable {xxx} {xx}\n@item a\n@tab b\n@end multitable"
	  (orgtbl-to-texinfo (org-table-to-lisp "| a | b |")
			     '(:columns "{xxx} {xx}")))))

(ert-deftest test-org-table/to-orgtbl ()
  "Test `orgtbl-to-orgtbl' specifications."
  (should
   (equal "| a | b |\n|---+---|\n| c | d |"
	  (orgtbl-to-orgtbl
	   (org-table-to-lisp "| a | b |\n|---+---|\n| c | d |") nil))))

(ert-deftest test-org-table/to-unicode ()
  "Test `orgtbl-to-unicode' specifications."
  (should
   (equal "━━━\n a \n━━━"
	  (orgtbl-to-unicode (org-table-to-lisp "| a |") nil)))
  ;; Test :narrow parameter.
  (should
   (equal "━━━━\n => \n━━━━"
	  (orgtbl-to-unicode (org-table-to-lisp "| <2> |\n| xxx |")
			     '(:narrow t)))))

(ert-deftest test-org-table/send-region ()
  "Test `orgtbl-send-table' specifications."
  ;; Error when not at a table.
  (should-error
   (org-test-with-temp-text "Paragraph"
     (orgtbl-send-table)))
  ;; Error when destination is missing.
  (should-error
   (org-test-with-temp-text "#+ORGTBL: SEND\n<point>| a |"
     (orgtbl-send-table)))
  ;; Error when transformation function is not specified.
  (should-error
   (org-test-with-temp-text "
# BEGIN RECEIVE ORGTBL table
# END RECEIVE ORGTBL table
#+ORGTBL: SEND table
<point>| a |"
     (orgtbl-send-table)))
  ;; Standard test.
  (should
   (equal "| a |\n|---|\n| b |\n"
	  (org-test-with-temp-text "
# BEGIN RECEIVE ORGTBL table
# END RECEIVE ORGTBL table
#+ORGTBL: SEND table orgtbl-to-orgtbl :hlines nil
<point>| a |\n|---|\n| b |"
	    (orgtbl-send-table)
	    (goto-char (point-min))
	    (buffer-substring-no-properties
	     (search-forward "# BEGIN RECEIVE ORGTBL table\n")
	     (progn (search-forward "# END RECEIVE ORGTBL table")
		    (match-beginning 0)))))))


;;; Sorting

(ert-deftest test-org-table/sort-lines ()
  "Test `org-table-sort-lines' specifications."
  ;; Sort numerically.
  (should
   (equal "| 1 | 2 |\n| 2 | 4 |\n| 5 | 3 |\n"
	  (org-test-with-temp-text "| <point>1 | 2 |\n| 5 | 3 |\n| 2 | 4 |\n"
	    (org-table-sort-lines nil ?n)
	    (buffer-string))))
  (should
   (equal "| 5 | 3 |\n| 2 | 4 |\n| 1 | 2 |\n"
	  (org-test-with-temp-text "| <point>1 | 2 |\n| 5 | 3 |\n| 2 | 4 |\n"
	    (org-table-sort-lines nil ?N)
	    (buffer-string))))
  ;; Sort alphabetically.
  (should
   (equal "| a | x |\n| b | 4 |\n| c | 3 |\n"
	  (org-test-with-temp-text "| <point>a | x |\n| c | 3 |\n| b | 4 |\n"
	    (org-table-sort-lines nil ?a)
	    (buffer-string))))
  (should
   (equal "| c | 3 |\n| b | 4 |\n| a | x |\n"
	  (org-test-with-temp-text "| <point>a | x |\n| c | 3 |\n| b | 4 |\n"
	    (org-table-sort-lines nil ?A)
	    (buffer-string))))
  ;; Sort alphabetically with case.
  (should
   (equal "| C |\n| a |\n| b |\n"
	  (org-test-with-temp-text "| <point>a |\n| C |\n| b |\n"
	    (org-table-sort-lines t ?a)
	    (buffer-string))))
  (should
   (equal "| b |\n| a |\n| C |\n"
	  (org-test-with-temp-text "| <point>a |\n| C |\n| b |\n"
	    (org-table-sort-lines nil ?A)
	    (buffer-string))))
  ;; Sort by time (timestamps)
  (should
   (equal
    "| <2008-08-08 sat.> |\n| <2012-03-29 thu.> |\n| <2014-03-04 tue.> |\n"
    (org-test-with-temp-text
	"| <2014-03-04 tue.> |\n| <2008-08-08 sat.> |\n| <2012-03-29 thu.> |\n"
      (org-table-sort-lines nil ?t)
      (buffer-string))))
  (should
   (equal
    "| <2014-03-04 tue.> |\n| <2012-03-29 thu.> |\n| <2008-08-08 sat.> |\n"
    (org-test-with-temp-text
	"| <2014-03-04 tue.> |\n| <2008-08-08 sat.> |\n| <2012-03-29 thu.> |\n"
      (org-table-sort-lines nil ?T)
      (buffer-string))))
  ;; Sort by time (HH:MM values)
  (should
   (equal "| 1:00 |\n| 14:00 |\n| 17:00 |\n"
	  (org-test-with-temp-text "| 14:00 |\n| 17:00 |\n| 1:00 |\n"
	    (org-table-sort-lines nil ?t)
	    (buffer-string))))
  (should
   (equal "| 17:00 |\n| 14:00 |\n| 1:00 |\n"
	  (org-test-with-temp-text "| 14:00 |\n| 17:00 |\n| 1:00 |\n"
	    (org-table-sort-lines nil ?T)
	    (buffer-string))))
  ;; Sort with custom functions.
  (should
   (equal "| 22 |\n| 15 |\n| 18 |\n"
	  (org-test-with-temp-text "| 15 |\n| 22 |\n| 18 |\n"
	    (org-table-sort-lines nil ?f
				  (lambda (s) (% (string-to-number s) 10))
				  #'<)
	    (buffer-string))))
  (should
   (equal "| 18 |\n| 15 |\n| 22 |\n"
	  (org-test-with-temp-text "| 15 |\n| 22 |\n| 18 |\n"
	    (org-table-sort-lines nil ?F
				  (lambda (s) (% (string-to-number s) 10))
				  #'<)
	    (buffer-string))))
  ;; Sort according to current column.
  (should
   (equal "| 1 | 2 |\n| 7 | 3 |\n| 5 | 4 |\n"
	  (org-test-with-temp-text "| 1 | <point>2 |\n| 5 | 4 |\n| 7 | 3 |\n"
	    (org-table-sort-lines nil ?n)
	    (buffer-string))))
  ;; Sort between horizontal separators if possible.
  (should
   (equal
    "| 9 | 8 |\n|---+---|\n| 5 | 3 |\n| 7 | 4 |\n|---+---|\n| 1 | 2 |\n"
    (org-test-with-temp-text
	"| 9 | 8 |\n|---+---|\n| <point>7 | 4 |\n| 5 | 3 |\n|---+---|\n| 1 | 2 |\n"
      (org-table-sort-lines nil ?n)
      (buffer-string)))))


;;; Field formulas

(ert-deftest test-org-table/field-formula-outside-table ()
  "If `org-table-formula-create-columns' is nil, then a formula
that references an out-of-bounds column should do nothing. If it
is t, then new columns should be added as needed"

  (let ((org-table-formula-create-columns nil))

    (should-error
     (org-test-table-target-expect
      "
| 2 |
| 4 |
| 8 |
"
      "
| 2 |
| 4 |
| 8 |
"
      1
      "#+TBLFM: @1$2=5")
     :type (list 'error 'user-error)))

  (let ((org-table-formula-create-columns t))

    ;; make sure field formulas work
    (org-test-table-target-expect
     "
| 2 |
| 4 |
| 8 |
"
     "
| 2 | 5 |
| 4 |   |
| 8 |   |
"
     1
     "#+TBLFM: @1$2=5")

    ;; and make sure column formulas work too
    (org-test-table-target-expect
     "
| 2 |
| 4 |
| 8 |
"
     "
| 2 |   | 15 |
| 4 |   | 15 |
| 8 |   | 15 |
"
     1
     "#+TBLFM: $3=15")))

(ert-deftest test-org-table/duration ()
  "Test durations in table formulas."
  ;; Durations in cells.
  (should
   (string-match "| 2:12 | 1:47 | 03:59:00 |"
		 (org-test-with-temp-text "
       | 2:12 | 1:47 | |
       <point>#+TBLFM: @1$3=$1+$2;T"
		   (org-table-calc-current-TBLFM)
		   (buffer-string))))
  (should
   (string-match "| 3:02:20 | -2:07:00 | 0.92 |"
		 (org-test-with-temp-text "
       | 3:02:20 | -2:07:00 | |
       <point>#+TBLFM: @1$3=$1+$2;t"
		   (org-table-calc-current-TBLFM)
		   (buffer-string))))
  ;; Durations set through properties.
  (should
   (string-match "| 16:00:00 |"
		 (org-test-with-temp-text "* H
  :PROPERTIES:
  :time_constant: 08:00:00
  :END:

  |  |
  <point>#+TBLFM: $1=2*$PROP_time_constant;T"
		   (org-table-calc-current-TBLFM)
		   (buffer-string))))
  (should
   (string-match "| 16.00 |"
		 (org-test-with-temp-text "* H
  :PROPERTIES:
  :time_constant: 08:00:00
  :END:

  |  |
  <point>#+TBLFM: $1=2*$PROP_time_constant;t"
		   (org-table-calc-current-TBLFM)
		   (buffer-string)))))

(ert-deftest test-org-table/end-on-hline ()
  "Test with a table ending on a hline."
  (should
   (equal
    (org-test-with-temp-text
	"
| 1 | 2 | 3 |
| 4 | 5 | 6 |
|   |   |   |
|---+---+---|
<point>#+TBLFM: @3$2..@3$>=vsum(@1..@2)"
      (org-table-calc-current-TBLFM)
      (buffer-string))
    "
| 1 | 2 | 3 |
| 4 | 5 | 6 |
|   | 7 | 9 |
|---+---+---|
#+TBLFM: @3$2..@3$>=vsum(@1..@2)")))

(ert-deftest test-org-table/named-field ()
  "Test formula with a named field."
  (should
   (org-string-match-p
    "| +| +1 +|"
    (org-test-with-temp-text "
|   |      |
| ^ | name |
<point>#+TBLFM: $name=1"
      (org-table-calc-current-TBLFM)
      (buffer-string))))
  (should
   (org-string-match-p
    "| +| +1 +|"
    (org-test-with-temp-text "
| _ | name |
|   |      |
<point>#+TBLFM: $name=1"
      (org-table-calc-current-TBLFM)
      (buffer-string)))))

(ert-deftest test-org-table/named-column ()
  "Test formula with a named field."
  (should
   (org-string-match-p
    "| +| +1 +| +1 +|"
    (org-test-with-temp-text "
| ! | name |   |
|   |    1 |   |
<point>#+TBLFM: @2$3=$name"
      (org-table-calc-current-TBLFM)
      (buffer-string)))))

(ert-deftest test-org-table/tab-indent ()
  "Test named fields with tab indentation."
  (should
   (org-string-match-p
    "| # | 111 |"
    (org-test-with-temp-text
	"
	| ! |  sum |      | a |  b |   c |
	|---+------+------+---+----+-----|
	| # | 1011 | 1000 | 1 | 10 | 100 |
	<point>#+TBLFM: $2=$a+$b+$c
"
      (org-table-calc-current-TBLFM)
      (buffer-string)))))

(ert-deftest test-org-table/first-rc ()
  "Test \"$<\" constructs in formulas."
  (should
   (org-string-match-p
    "| 1 | 2 |"
    (org-test-with-temp-text
	"|   | 2 |
<point>#+TBLFM: $<=1"
      (org-table-calc-current-TBLFM)
      (buffer-string)))))


(provide 'test-org-table)

;;; test-org-table.el ends here
