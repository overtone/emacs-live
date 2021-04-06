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

;; Template test file for Org tests.  Many tests are also a howto
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
|       2 | header  |
|       4 | header  |
|       8 | header  |
|---------+---------|
| replace | replace |
"
   "
|  2 | header |
|  4 | header |
|  8 | header |
|----+--------|
| 14 | 28     |
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
    "$10 = '(/ (+ @0$1..@0$4) (length '(@0$1..@0$4))); EN")))

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
| [2012-12-31 Mon] | [2012-12-31 Mon] |
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
| [2012-12-31 Mon] | [2012-12-31 Mon] |
"
     1 "#+TBLFM: $2 = if(\"$1\" == \"nan\", string(\"\"), $1); E")))

(ert-deftest test-org-table/copy-down ()
  "Test `org-table-copy-down' specifications."
  ;; Error when there is nothing to copy in the current field or the
  ;; field above.
  (should-error
   (org-test-with-temp-text "|  |\n| <point> |"
     (org-table-copy-down 1)))
  ;; Error when there is nothing to copy in the Nth field.
  (should-error
   (org-test-with-temp-text "|    |\n| foo |\n| <point> |"
     (org-table-copy-down 2)))
  ;; In an empty field, copy field above.
  (should
   (equal "| foo |\n| foo |"
	  (org-test-with-temp-text "| foo |\n| <point> |"
	    (org-table-copy-down 1)
	    (buffer-string))))
  ;; In a non-empty field, copy it below.
  (should
   (equal "| foo |\n| foo |\n"
	  (org-test-with-temp-text "| <point>foo |"
	    (org-table-copy-down 1)
	    (buffer-string))))
  ;; If field is a number or a timestamp, or is prefixed or suffixed
  ;; with a number, increment it by one unit.
  (should
   (equal "| 1 |\n| 2 |\n"
	  (org-test-with-temp-text "| <point>1 |"
	    (let ((org-table-copy-increment t)) (org-table-copy-down 1))
	    (buffer-string))))
  (should
   (string-match-p "<2012-03-30"
		   (org-test-with-temp-text "| <point><2012-03-29> |"
		     (let ((org-table-copy-increment t))
		       (org-table-copy-down 1))
		     (buffer-string))))
  (should
   (equal "| A1 |\n| A2 |\n"
	  (org-test-with-temp-text "| <point>A1 |"
	    (let ((org-table-copy-increment t)) (org-table-copy-down 1))
	    (buffer-string))))
  (should
   (equal "| 1A |\n| 2A |\n"
	  (org-test-with-temp-text "| <point>1A |"
	    (let ((org-table-copy-increment t)) (org-table-copy-down 1))
	    (buffer-string))))
  ;; When `org-table-copy-increment' is nil, or when argument is 0, do
  ;; not increment.
  (should
   (equal "| 1 |\n| 1 |\n"
	  (org-test-with-temp-text "| <point>1 |"
	    (let ((org-table-copy-increment nil)) (org-table-copy-down 1))
	    (buffer-string))))
  (should
   (equal "| 1 |\n| 1 |\n"
	  (org-test-with-temp-text "| <point>1 |"
	    (let ((org-table-copy-increment t)) (org-table-copy-down 0))
	    (buffer-string))))
  ;; When there is a field just above field being incremented, try to
  ;; use it to guess increment step.
  (should
   (equal "| 4 |\n| 3 |\n| 2 |\n"
	  (org-test-with-temp-text "| 4 |\n| <point>3 |"
	    (let ((org-table-copy-increment t)) (org-table-copy-down 1))
	    (buffer-string))))
  (should
   (equal "| A0 |\n| A2 |\n| A4 |\n"
	  (org-test-with-temp-text "| A0 |\n| <point>A2 |"
	    (let ((org-table-copy-increment t)) (org-table-copy-down 1))
	    (buffer-string))))
  ;; Both fields need to have the same type.  In the special case of
  ;; number-prefixed or suffixed fields, make sure both fields have
  ;; the same pattern.
  (should
   (equal "| A4 |\n|  3 |\n|  4 |\n"
	  (org-test-with-temp-text "| A4 |\n| <point>3 |"
	    (let ((org-table-copy-increment t)) (org-table-copy-down 1))
	    (buffer-string))))
  (should
   (equal "| 0A |\n| A2 |\n| A3 |\n"
	  (org-test-with-temp-text "| 0A |\n| <point>A2 |"
	    (let ((org-table-copy-increment t)) (org-table-copy-down 1))
	    (buffer-string))))
  (should
   (equal "| A0 |\n| 2A |\n| 3A |\n"
	  (org-test-with-temp-text "| A0 |\n| <point>2A |"
	    (let ((org-table-copy-increment t)) (org-table-copy-down 1))
	    (buffer-string))))
  ;; Do not search field above past blank fields and horizontal
  ;; separators.
  (should
   (equal "| 4 |\n|---|\n| 3 |\n| 4 |\n"
	  (org-test-with-temp-text "| 4 |\n|---|\n| <point>3 |"
	    (let ((org-table-copy-increment t)) (org-table-copy-down 1))
	    (buffer-string))))
  (should
   (equal "| 4 |\n|   |\n| 3 |\n| 4 |\n"
	  (org-test-with-temp-text "| 4 |\n|   |\n| <point>3 |"
	    (let ((org-table-copy-increment t)) (org-table-copy-down 1))
	    (buffer-string))))
  ;; When `org-table-copy-increment' is a number, use it as the
  ;; increment step, ignoring any previous field.
  (should
   (equal "| 1 |\n| 3 |\n| 6 |\n"
	  (org-test-with-temp-text "| 1 |\n| <point>3 |"
	    (let ((org-table-copy-increment 3)) (org-table-copy-down 1))
	    (buffer-string))))
  ;; However, if argument is 0, do not increment whatsoever.
  (should
   (equal "| 1 |\n| 3 |\n| 3 |\n"
	  (org-test-with-temp-text "| 1 |\n| <point>3 |"
	    (let ((org-table-copy-increment t)) (org-table-copy-down 0))
	    (buffer-string))))
  (should
   (equal "| 1 |\n| 3 |\n| 3 |\n"
	  (org-test-with-temp-text "| 1 |\n| <point>3 |"
	    (let ((org-table-copy-increment 3)) (org-table-copy-down 0))
	    (buffer-string)))))

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
See also URL `https://orgmode.org/worg/org-tutorials/org-lookups.html'."
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
  ;; For Lisp formula only
  (should (equal "0" (org-table-make-reference "0" t nil 'literal)))
  (should (equal "z" (org-table-make-reference "z" t nil 'literal)))
  (should (equal "" (org-table-make-reference "" t nil 'literal)))
  (should (equal "0 1" (org-table-make-reference '("0" "1") t nil 'literal)))
  (should (equal "z 1" (org-table-make-reference '("z" "1") t nil 'literal)))
  (should (equal " 1" (org-table-make-reference '("" "1") t nil 'literal)))
  (should (equal " " (org-table-make-reference '("" "") t nil 'literal))))

(ert-deftest test-org-table/org-table-make-reference/mode-string-E ()
  ;; For Lisp formula
  (should (equal "\"0\"" (org-table-make-reference "0" t nil t)))
  (should (equal "\"z\"" (org-table-make-reference "z" t nil t)))
  (should (equal"\"\"" (org-table-make-reference "" t nil t)))
  (should (equal "\"0\" \"1\"" (org-table-make-reference '("0""1") t nil t)))
  (should (equal "\"z\" \"1\"" (org-table-make-reference '("z""1") t nil t)))
  (should (equal"\"\" \"1\"" (org-table-make-reference '("""1") t nil t)))
  (should (equal"\"\" \"\""(org-table-make-reference '("""" ) t nil t)))
  ;; For Calc formula
  (should (equal "(0)" (org-table-make-reference "0" t nil nil)))
  (should (equal "(z)" (org-table-make-reference "z" t nil nil)))
  (should (equal "nan" (org-table-make-reference "" t nil nil)))
  (should (equal "[0,1]" (org-table-make-reference '("0" "1") t nil nil)))
  (should (equal "[z,1]" (org-table-make-reference '("z" "1") t nil nil)))
  (should (equal "[nan,1]" (org-table-make-reference '("" "1") t nil nil)))
  (should (equal "[nan,nan]" (org-table-make-reference '("" "") t nil nil)))
  ;; For Calc formula, special numbers
  (should (equal "(nan)" (org-table-make-reference "nan" t nil nil)))
  (should (equal "(uinf)" (org-table-make-reference "uinf" t nil nil)))
  (should (equal "(-inf)" (org-table-make-reference "-inf" t nil nil)))
  (should (equal "(inf)" (org-table-make-reference "inf" t nil nil)))
  (should (equal "[nan,1]" (org-table-make-reference '("nan" "1") t nil nil)))
  (should (equal "[uinf,1]" (org-table-make-reference '("uinf" "1") t nil nil)))
  (should (equal "[-inf,1]" (org-table-make-reference '("-inf" "1") t nil nil)))
  (should (equal "[inf,1]" (org-table-make-reference '("inf" "1") t nil nil))))

(ert-deftest test-org-table/org-table-make-reference/mode-string-EN ()
  ;; For Lisp formula
  (should (equal "0" (org-table-make-reference "0" t t t)))
  (should (equal "0" (org-table-make-reference "z" t t t)))
  (should (equal "0" (org-table-make-reference "" t t t)))
  (should (equal "0 1" (org-table-make-reference '("0" "1") t t t)))
  (should (equal "0 1" (org-table-make-reference '("z" "1") t t t)))
  (should (equal "0 1" (org-table-make-reference '("" "1") t t t)))
  (should (equal "0 0" (org-table-make-reference '("" "" ) t t t)))
  ;; For Calc formula
  (should (equal "(0)" (org-table-make-reference "0" t t nil)))
  (should (equal "(0)" (org-table-make-reference "z" t t nil)))
  (should (equal "(0)" (org-table-make-reference "" t t nil)))
  (should (equal "[0,1]" (org-table-make-reference '("0" "1") t t nil)))
  (should (equal "[0,1]" (org-table-make-reference '("z" "1") t t nil)))
  (should (equal "[0,1]" (org-table-make-reference '("" "1") t t nil)))
  (should (equal "[0,0]" (org-table-make-reference '("" "" ) t t nil)))
  ;; For Calc formula, special numbers
  (should (equal "(0)" (org-table-make-reference "nan" t t nil)))
  (should (equal "(0)" (org-table-make-reference "uinf" t t nil)))
  (should (equal "(0)" (org-table-make-reference "-inf" t t nil)))
  (should (equal "(0)" (org-table-make-reference "inf" t t nil)))
  (should (equal "[0,1]" (org-table-make-reference '( "nan" "1") t t nil)))
  (should (equal "[0,1]" (org-table-make-reference '("uinf" "1") t t nil)))
  (should (equal "[0,1]" (org-table-make-reference '("-inf" "1") t t nil)))
  (should (equal "[0,1]" (org-table-make-reference '( "inf" "1") t t nil))))

(ert-deftest test-org-table/org-table-make-reference/mode-string-L ()
  ;; For Lisp formula only
  (should (equal "0" (org-table-make-reference "0" nil nil 'literal)))
  (should (equal "z" (org-table-make-reference "z" nil nil 'literal)))
  (should (equal "" (org-table-make-reference "" nil nil 'literal)))
  (should (equal "0 1" (org-table-make-reference '("0" "1") nil nil 'literal)))
  (should (equal "z 1" (org-table-make-reference '("z" "1") nil nil 'literal)))
  (should (equal "1" (org-table-make-reference '("" "1") nil nil 'literal)))
  (should (equal "" (org-table-make-reference '("" "" ) nil nil 'literal))))

(ert-deftest test-org-table/org-table-make-reference/mode-string-none ()
  ;; For Lisp formula
  (should (equal "\"0\"" (org-table-make-reference "0" nil nil t)))
  (should (equal "\"z\"" (org-table-make-reference "z" nil nil t)))
  (should (equal "" (org-table-make-reference "" nil nil t)))
  (should (equal "\"0\" \"1\"" (org-table-make-reference '("0" "1") nil nil t)))
  (should (equal "\"z\" \"1\"" (org-table-make-reference '("z" "1") nil nil t)))
  (should (equal "\"1\"" (org-table-make-reference '("" "1") nil nil t)))
  (should (equal "" (org-table-make-reference '("" "" ) nil nil t)))
  ;; For Calc formula
  (should (equal "(0)" (org-table-make-reference "0" nil nil nil)))
  (should (equal "(z)" (org-table-make-reference "z" nil nil nil)))
  (should (equal "(0)" (org-table-make-reference "" nil nil nil)))
  (should (equal "[0,1]" (org-table-make-reference '("0" "1") nil nil nil)))
  (should (equal "[z,1]" (org-table-make-reference '("z" "1") nil nil nil)))
  (should (equal "[1]" (org-table-make-reference '("" "1") nil nil nil)))
  (should (equal "[]" (org-table-make-reference '("" "" ) nil nil nil)))
  ;; For Calc formula, special numbers
  (should (equal "(nan)" (org-table-make-reference "nan" nil nil nil)))
  (should (equal "(uinf)" (org-table-make-reference "uinf" nil nil nil)))
  (should (equal "(-inf)" (org-table-make-reference "-inf" nil nil nil)))
  (should (equal "(inf)" (org-table-make-reference "inf" nil nil nil)))
  (should (equal "[nan,1]" (org-table-make-reference '( "nan" "1") nil nil nil)))
  (should (equal "[uinf,1]" (org-table-make-reference '("uinf" "1") nil nil nil)))
  (should (equal "[-inf,1]" (org-table-make-reference '("-inf" "1") nil nil nil)))
  (should (equal "[inf,1]" (org-table-make-reference '( "inf" "1") nil nil nil))))

(ert-deftest test-org-table/org-table-make-reference/mode-string-N ()
  ;; For Lisp formula
  (should (equal "0" (org-table-make-reference "0" nil t t)))
  (should (equal "0" (org-table-make-reference "z" nil t t)))
  (should (equal "" (org-table-make-reference "" nil t t)))
  (should (equal "0 1" (org-table-make-reference '("0" "1") nil t t)))
  (should (equal "0 1" (org-table-make-reference '("z" "1") nil t t)))
  (should (equal "1" (org-table-make-reference '("" "1") nil t t)))
  (should (equal "" (org-table-make-reference '("" "" ) nil t t)))
  ;; For Calc formula
  (should (equal "(0)" (org-table-make-reference "0" nil t nil)))
  (should (equal "(0)" (org-table-make-reference "z" nil t nil)))
  (should (equal "(0)" (org-table-make-reference "" nil t nil)))
  (should (equal "[0,1]" (org-table-make-reference '("0" "1") nil t nil)))
  (should (equal "[0,1]" (org-table-make-reference '("z" "1") nil t nil)))
  (should (equal "[1]" (org-table-make-reference '("" "1") nil t nil)))
  (should (equal "[]" (org-table-make-reference '("" "" ) nil t nil)))
  ;; For Calc formula, special numbers
  (should (equal "(0)" (org-table-make-reference "nan" nil t nil)))
  (should (equal "(0)" (org-table-make-reference "uinf" nil t nil)))
  (should (equal "(0)" (org-table-make-reference "-inf" nil t nil)))
  (should (equal "(0)" (org-table-make-reference "inf" nil t nil)))
  (should (equal "[0,1]" (org-table-make-reference '( "nan" "1") nil t nil)))
  (should (equal "[0,1]" (org-table-make-reference '("uinf" "1") nil t nil)))
  (should (equal "[0,1]" (org-table-make-reference '("-inf" "1") nil t nil)))
  (should (equal "[0,1]" (org-table-make-reference '( "inf" "1") nil t nil))))

(ert-deftest test-org-table/org-table-convert-refs-to-an/1 ()
  "Simple reference @2$1."
  (should
   (string= "A2" (org-table-convert-refs-to-an "@2$1"))))

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
  "Simple reference @2$1."
  (should
   (string= "@2$1" (org-table-convert-refs-to-rc "A2"))))

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
     (string-match-p
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
   (string-match-p
    "\\toprule" (orgtbl-to-latex (org-table-to-lisp "| a |") '(:booktabs t))))
  ;; Handle LaTeX snippets.
  (should
   (equal "\\begin{tabular}{l}\n\\(x\\)\\\\\n\\end{tabular}"
	  (orgtbl-to-latex (org-table-to-lisp "| $x$ |") nil)))
  ;; Test pseudo objects and :raw parameter.
  (should
   (string-match-p
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
   (string-match-p
    "<table>"
    (orgtbl-to-html (org-table-to-lisp "| a |") '(:attributes nil))))
  (should
   (string-match-p
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
		    (match-beginning 0))))))
  ;; Allow multiple receiver locations.
  (should
   (org-test-with-temp-text "
# BEGIN RECEIVE ORGTBL table
# END RECEIVE ORGTBL table

#+ORGTBL: SEND table orgtbl-to-orgtbl :hlines nil
<point>| a |

# BEGIN RECEIVE ORGTBL table
# END RECEIVE ORGTBL table"
     (orgtbl-send-table)
     (goto-char (point-min))
     (search-forward "| a |" nil t 3))))


;;; Align

(ert-deftest test-org-table/align ()
  "Test `org-table-align' specifications."
  ;; Regular test.
  (should
   (equal "| a |\n"
	  (org-test-with-temp-text "|   a |"
	    (org-table-align)
	    (buffer-string))))
  ;; Preserve alignment.
  (should
   (equal "  | a |\n"
	  (org-test-with-temp-text "  |   a |"
	    (org-table-align)
	    (buffer-string))))
  ;; Handle horizontal lines.
  (should
   (equal "| 123 |\n|-----|\n"
	  (org-test-with-temp-text "| 123 |\n|-|"
	    (org-table-align)
	    (buffer-string))))
  (should
   (equal "| a | b |\n|---+---|\n"
	  (org-test-with-temp-text "| a | b |\n|-+-|"
	    (org-table-align)
	    (buffer-string))))
  ;; Handle empty fields.
  (should
   (equal "| a   | bc |\n| bcd |    |\n"
	  (org-test-with-temp-text "| a | bc |\n| bcd |  |"
	    (org-table-align)
	    (buffer-string))))
  (should
   (equal "| abc | bc  |\n|     | bcd |\n"
	  (org-test-with-temp-text "| abc | bc |\n| | bcd |"
	    (org-table-align)
	    (buffer-string))))
  ;; Handle missing fields.
  (should
   (equal "| a | b |\n| c |   |\n"
	  (org-test-with-temp-text "| a | b |\n| c |"
	    (org-table-align)
	    (buffer-string))))
  (should
   (equal "| a | b |\n|---+---|\n"
	  (org-test-with-temp-text "| a | b |\n|---|"
	    (org-table-align)
	    (buffer-string))))
  ;; Alignment is done to the right when the ratio of numbers in the
  ;; column is superior to `org-table-number-fraction'.
  (should
   (equal "|   1 |\n|  12 |\n| abc |"
	  (org-test-with-temp-text "| 1 |\n| 12 |\n| abc |"
	    (let ((org-table-number-fraction 0.5)) (org-table-align))
	    (buffer-string))))
  (should
   (equal "| 1   |\n| ab  |\n| abc |"
	  (org-test-with-temp-text "| 1 |\n| ab |\n| abc |"
	    (let ((org-table-number-fraction 0.5)) (org-table-align))
	    (buffer-string))))
  ;; Obey to alignment cookies.
  (should
   (equal "| <r> |\n|  ab |\n| abc |"
	  (org-test-with-temp-text "| <r> |\n| ab |\n| abc |"
	    (let ((org-table-number-fraction 0.5)) (org-table-align))
	    (buffer-string))))
  (should
   (equal "| <l> |\n| 12  |\n| 123 |"
	  (org-test-with-temp-text "| <l> |\n| 12 |\n| 123 |"
	    (let ((org-table-number-fraction 0.5)) (org-table-align))
	    (buffer-string))))
  (should
   (equal "| <c> |\n|  1  |\n| 123 |"
	  (org-test-with-temp-text "| <c> |\n| 1 |\n| 123 |"
	    (let ((org-table-number-fraction 0.5)) (org-table-align))
	    (buffer-string)))))

(ert-deftest test-org-table/align-buffer-tables ()
  "Align all tables when updating buffer."
  (let ((before "
|  a  b  |

|  c  d  |
")
	(after "
| a  b |

| c  d |
"))
    (should (equal (org-test-with-temp-text before
		     (org-table-recalculate-buffer-tables)
		     (buffer-string))
		   after))
    (should (equal (org-test-with-temp-text before
		     (org-table-iterate-buffer-tables)
		     (buffer-string))
		   after))))


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
  ;; Sort alphabetically.  Enforce the C locale for consistent results.
  (let ((original-string-collate-lessp (symbol-function 'string-collate-lessp)))
    (cl-letf (((symbol-function 'string-collate-lessp)
	       (lambda (s1 s2 &optional locale ignore-case)
		 (funcall original-string-collate-lessp
			  s1 s2 "C" ignore-case))))
      (should
       (equal "| a | x |\n| B | 4 |\n| c | 3 |\n"
	      (org-test-with-temp-text "| <point>a | x |\n| c | 3 |\n| B | 4 |\n"
				       (org-table-sort-lines nil ?a)
				       (buffer-string))))
      (should
       (equal "| c | 3 |\n| B | 4 |\n| a | x |\n"
	      (org-test-with-temp-text "| <point>a | x |\n| c | 3 |\n| B | 4 |\n"
				       (org-table-sort-lines nil ?A)
				       (buffer-string))))
      ;; Sort alphabetically with case.
      (should
       (equal "| C |\n| a |\n| b |\n"
	      (org-test-with-temp-text "| <point>a |\n| C |\n| b |\n"
				       (org-table-sort-lines t ?a)
				       (buffer-string))))
      (should
       (equal "| C |\n| b |\n| a |\n"
	      (org-test-with-temp-text "| <point>a |\n| C |\n| b |\n"
				       (org-table-sort-lines nil ?A)
				       (buffer-string))))))
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
   (equal "| 1:00 |\n| 17:00 |\n| 114:00 |\n"
	  (org-test-with-temp-text "| 114:00 |\n| 17:00 |\n| 1:00 |\n"
	    (org-table-sort-lines nil ?t)
	    (buffer-string))))
  (should
   (equal "| 114:00 |\n| 17:00 |\n| 1:00 |\n"
	  (org-test-with-temp-text "| 114:00 |\n| 17:00 |\n| 1:00 |\n"
	    (org-table-sort-lines nil ?T)
	    (buffer-string))))
  ;; Sort by time (durations)
  (should
   (equal "| 1d 3:00 |\n| 28:00 |\n"
	  (org-test-with-temp-text "| 28:00 |\n| 1d 3:00 |\n"
	    (org-table-sort-lines nil ?t)
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


;;; Formulas

(ert-deftest test-org-table/eval-formula ()
  "Test `org-table-eval-formula' specifications."
  ;; Error when not on a table field.
  (should-error
   (org-test-with-temp-text "Text"
     (org-table-eval-formula)))
  (should-error
   (org-test-with-temp-text "| a |\n|---|<point>"
     (org-table-eval-formula)))
  (should-error
   (org-test-with-temp-text "| a |\n#+TBLFM:<point>"
     (org-table-eval-formula)))
  ;; Handle @<, @>, $< and $>.
  (should
   (equal "| 1 |\n| 1 |"
	  (org-test-with-temp-text "| <point>  |\n| 1 |"
	    (org-table-eval-formula nil "@>" nil nil t)
	    (buffer-string))))
  (should
   (equal "| 1 |\n| 1 |"
	  (org-test-with-temp-text "| 1 |\n| <point>  |"
	    (org-table-eval-formula nil "@<" nil nil t)
	    (buffer-string))))
  (should
   (equal "| 1 | 1 |"
	  (org-test-with-temp-text "| <point>  | 1 |"
	    (org-table-eval-formula nil "$>" nil nil t)
	    (buffer-string))))
  (should
   (equal "| 1 | 1 |"
	  (org-test-with-temp-text "| 1 | <point>  |"
	    (org-table-eval-formula nil "$<" nil nil t)
	    (buffer-string)))))

(ert-deftest test-org-table/field-formula-outside-table ()
  "Test `org-table-formula-create-columns' variable."
  ;; Refuse to create column if variable is nil.
  (should-error
   (org-test-with-temp-text "
| 2 |
| 4 |
| 8 |
<point>#+TBLFM: @1$2=5"
     (let ((org-table-formula-create-columns nil))
       (org-table-calc-current-TBLFM))
     (buffer-string))
   :type (list 'error 'user-error))
  ;; If the variable is non-nil, field formulas and columns formulas
  ;; can create tables.
  (should
   (equal
    "
| 2 | 5 |
| 4 |   |
| 8 |   |
#+TBLFM: @1$2=5"
    (org-test-with-temp-text "
| 2 |
| 4 |
| 8 |
<point>#+TBLFM: @1$2=5"
      (let ((org-table-formula-create-columns t))
	(org-table-calc-current-TBLFM))
      (buffer-string))))
  (should
   (equal
    "
| 2 |   | 15 |
| 4 |   | 15 |
| 8 |   | 15 |
#+TBLFM: $3=15"
    (org-test-with-temp-text "
| 2 |
| 4 |
| 8 |
<point>#+TBLFM: $3=15"
      (let ((org-table-formula-create-columns t))
	(org-table-calc-current-TBLFM))
      (buffer-string)))))

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
   (string-match "| 2:12 | 1:47 | 03:59 |"
		 (org-test-with-temp-text "
       | 2:12 | 1:47 | |
       <point>#+TBLFM: @1$3=$1+$2;U"
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
   (string-match-p
    "| +| +1 +|"
    (org-test-with-temp-text "
|   |      |
| ^ | name |
<point>#+TBLFM: $name=1"
      (org-table-calc-current-TBLFM)
      (buffer-string))))
  (should
   (string-match-p
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
   (string-match-p
    "| +| +1 +| +1 +|"
    (org-test-with-temp-text "
| ! | name |   |
|   |    1 |   |
<point>#+TBLFM: @2$3=$name"
      (org-table-calc-current-TBLFM)
      (buffer-string)))))

(ert-deftest test-org-table/formula-priority ()
  "Test field formula priority over column formula."
  ;; Field formulas bind stronger than column formulas.
  (should
   (equal
    "| 1 |  3 |\n| 2 | 99 |\n"
    (org-test-with-temp-text
	"| 1 |   |\n| 2 |   |\n<point>#+tblfm: $2=3*$1::@2$2=99"
      (org-table-calc-current-TBLFM)
      (buffer-substring-no-properties (point-min) (point)))))
  ;; When field formula is removed, table formulas is applied again.
  (should
   (equal
    "| 1 | 3 |\n| 2 | 6 |\n"
    (org-test-with-temp-text
	"| 1 |   |\n| 2 |   |\n#+tblfm: $2=3*$1<point>::@2$2=99"
      (org-table-calc-current-TBLFM)
      (delete-region (point) (line-end-position))
      (org-table-calc-current-TBLFM)
      (buffer-substring-no-properties (point-min) (line-beginning-position))))))

(ert-deftest test-org-table/tab-indent ()
  "Test named fields with tab indentation."
  (should
   (string-match-p
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
  "Test \"$<\" and \"@<\" constructs in formulas."
  (should
   (string-match-p
    "| 1 | 2 |"
    (org-test-with-temp-text
	"|   | 2 |
<point>#+TBLFM: $<=1"
      (org-table-calc-current-TBLFM)
      (buffer-string))))
  (should
   (string-match-p
    "| 2 |\n| 2 |"
    (org-test-with-temp-text
	"| 2 |\n|   |
<point>#+TBLFM: @2$1=@<"
      (org-table-calc-current-TBLFM)
      (buffer-string)))))

(ert-deftest test-org-table/last-rc ()
  "Test \"$>\" and \"@>\" constructs in formulas."
  (should
   (string-match-p
    "| 2 | 1 |"
    (org-test-with-temp-text
	"| 2 |   |\n<point>#+TBLFM: $>=1"
      (org-table-calc-current-TBLFM)
      (buffer-string))))
  (should
   (string-match-p
    "| 2 |\n| 2 |"
    (org-test-with-temp-text
	"| 2 |\n|   |\n<point>#+TBLFM: @>$1=@<"
      (org-table-calc-current-TBLFM)
      (buffer-string)))))

(ert-deftest test-org-table/time-stamps ()
  "Test time-stamps handling."
  ;; Standard test.
  (should
   (string-match-p
    "| 1 |"
    (org-test-with-temp-text
	"| <2016-07-07 Sun> | <2016-07-08 Fri> |   |\n<point>#+TBLFM: $3=$2-$1"
      (org-table-calc-current-TBLFM)
      (buffer-string))))
  ;; Handle locale specific time-stamps.
  (should
   (string-match-p
    "| 1 |"
    (org-test-with-temp-text
	"| <2016-07-07 Do> | <2016-07-08 Fr> |   |\n<point>#+TBLFM: $3=$2-$1"
      (org-table-calc-current-TBLFM)
      (buffer-string)))))


(ert-deftest test-org-table/orgtbl-ascii-draw ()
  "Test `orgtbl-ascii-draw'."
  ;; First value: Make sure that an integer input value is converted to a
  ;; float before division. Further values: Show some float input value
  ;; ranges corresponding to the same bar width.
  (should
   (equal
    (org-test-with-temp-text
	"
|    Value | <l>     |
|----------+---------|
|       19 | replace |
|----------+---------|
| -0.50001 | replace |
| -0.49999 | replace |
|  0.49999 | replace |
|  0.50001 | replace |
|  1.49999 | replace |
| 22.50001 | replace |
| 23.49999 | replace |
| 23.50001 | replace |
| 24.49999 | replace |
| 24.50001 | replace |
<point>#+TBLFM: $2 = '(orgtbl-ascii-draw $1 0 24 3 \" 12345678\")"
      (org-table-calc-current-TBLFM)
      (buffer-string))
    "
|    Value | <l>       |
|----------+-----------|
|       19 | 883       |
|----------+-----------|
| -0.50001 | too small |
| -0.49999 |           |
|  0.49999 |           |
|  0.50001 | 1         |
|  1.49999 | 1         |
| 22.50001 | 887       |
| 23.49999 | 887       |
| 23.50001 | 888       |
| 24.49999 | 888       |
| 24.50001 | too large |
#+TBLFM: $2 = '(orgtbl-ascii-draw $1 0 24 3 \" 12345678\")"))
  ;; Draw bars with a bullet. The bullet does not count in the parameter
  ;; WIDTH of `orgtbl-ascii-draw'.
  (should
   (equal
    (org-test-with-temp-text
	"
| -1 | replace |
|  0 | replace |
|  1 | replace |
|  2 | replace |
|  3 | replace |
|  4 | replace |
<point>#+TBLFM: $2 = '(orgtbl-ascii-draw $1 0 3 3 \"$-\")"
      (org-table-calc-current-TBLFM)
      (buffer-string))
    "
| -1 | too small |
|  0 | $         |
|  1 | -$        |
|  2 | --$       |
|  3 | ---$      |
|  4 | too large |
#+TBLFM: $2 = '(orgtbl-ascii-draw $1 0 3 3 \"$-\")")))

(ert-deftest test-org-table/single-rowgroup ()
  "Test column formula in a table with a single rowgroup."
  (should
   (equal
    "
|---+---|
| 1 | 0 |
|---+---|
#+TBLFM: $2=$1-1"
    (org-test-with-temp-text "
|---+---|
| 1 |   |
|---+---|
<point>#+TBLFM: $2=$1-1"
      (org-table-calc-current-TBLFM)
      (buffer-string))))
  (should
   (equal
    "
| 1 | 0 |
#+TBLFM: $2=$1-1"
    (org-test-with-temp-text "
| 1 |   |
<point>#+TBLFM: $2=$1-1"
      (org-table-calc-current-TBLFM)
      (buffer-string)))))


;;; Navigation

(ert-deftest test-org-table/next-field ()
  "Test `org-table-next-field' specifications."
  ;; Regular test.
  (should
   (equal
    "b"
    (org-test-with-temp-text "| a<point> | b |"
      (org-table-next-field)
      (org-trim (org-table-get-field)))))
  ;; Create new rows as needed.
  (should
   (equal
    "| a |\n|   |\n"
    (org-test-with-temp-text "| a<point> |"
      (org-table-next-field)
      (buffer-string))))
  ;; Jump over hlines, if `org-table-tab-jumps-over-hlines' is
  ;; non-nil.
  (should
   (equal
    "b"
    (org-test-with-temp-text "| a<point> |\n|---|\n| b |"
      (let ((org-table-tab-jumps-over-hlines t)) (org-table-next-field))
      (org-trim (org-table-get-field)))))
  ;; If `org-table-tab-jumps-over-hlines' is nil, however, create
  ;; a new row before the rule.
  (should
   (equal
    "| a |\n|   |\n|---|\n| b |"
    (org-test-with-temp-text "| a<point> |\n|---|\n| b |"
      (let ((org-table-tab-jumps-over-hlines nil)) (org-table-next-field))
      (buffer-string)))))

(ert-deftest test-org-table/previous-field ()
  "Test `org-table-previous-field' specifications."
  ;; Regular tests.
  (should
   (eq ?a
       (org-test-with-temp-text "| a | <point>b |"
	 (org-table-previous-field)
	 (char-after))))
  (should
   (eq ?a
       (org-test-with-temp-text "| a |\n| <point>b |"
	 (org-table-previous-field)
	 (char-after))))
  ;; Find previous field across horizontal rules.
  (should
   (eq ?a
       (org-test-with-temp-text "| a |\n|---|\n| <point>b |"
	 (org-table-previous-field)
	 (char-after))))
  ;; When called on a horizontal rule, find previous data field.
  (should
   (eq ?b
       (org-test-with-temp-text "| a | b |\n|---+-<point>--|"
	 (org-table-previous-field)
	 (char-after))))
  ;; Error when at first field.  Make sure to preserve original
  ;; position.
  (should-error
   (org-test-with-temp-text "| <point> a|"
     (org-table-previous-field)))
  (should-error
   (org-test-with-temp-text "|---|\n| <point>a |"
     (org-table-previous-field)))
  (should
   (eq ?a
       (org-test-with-temp-text "|---|\n| <point>a |"
	 (ignore-errors (org-table-previous-field))
	 (char-after)))))


;;; Deleting columns
(ert-deftest test-org-table/delete-column ()
  "Test `org-table-delete-column'."
  ;; Error when outside a table.
  (should-error
   (org-test-with-temp-text "Paragraph"
     (org-table-delete-column)))
  ;; Delete first column.
  (should
   (equal "| a |\n"
	  (org-test-with-temp-text
	      "| <point>  | a |\n"
	    (org-table-delete-column)
	    (buffer-string))))
  ;; Delete column and check location of point.
  (should
   (= 2
      (org-test-with-temp-text
	  "| a | <point>b  | c |"
	(org-table-delete-column)
	(org-table-current-column))))
  ;; Delete column when at end of line and after a "|".
  (should
   (equal "| a |\n"
	  (org-test-with-temp-text
	      "| a | b |<point>\n"
	    (org-table-delete-column)
	    (buffer-string))))
  (should
   (equal "| a |\n"
	  (org-test-with-temp-text
	      "| a | b |   <point>\n"
	    (org-table-delete-column)
	    (buffer-string))))
  ;; Delete two columns starting with the last column.
  (should
   (equal "| a |\n"
	  (org-test-with-temp-text
	      "| a | b  | c<point> |"
	    (org-table-delete-column)
	    (org-table-delete-column)
	    (buffer-string)))))


;;; Inserting rows, inserting columns

(ert-deftest test-org-table/insert-column ()
  "Test `org-table-insert-column' specifications."
  ;; Error when outside a table.
  (should-error
   (org-test-with-temp-text "Paragraph"
     (org-table-insert-column)))
  ;; Insert new column after current one.
  (should
   (equal "|   | a |\n"
	  (org-test-with-temp-text "| a |"
	    (org-table-insert-column)
	    (buffer-string))))
  (should
   (equal "|   | a | b |\n"
	  (org-test-with-temp-text "| <point>a | b |"
	    (org-table-insert-column)
	    (buffer-string))))
  ;; Move point into the newly created column.
  (should
   (equal "  | a |"
	  (org-test-with-temp-text "| <point>a |"
	    (org-table-insert-column)
	    (buffer-substring-no-properties (point) (line-end-position)))))
  (should
   (equal "  | a | b |"
	  (org-test-with-temp-text "| <point>a | b |"
	    (org-table-insert-column)
	    (buffer-substring-no-properties (point) (line-end-position)))))
  ;; Handle missing vertical bar in the last column.
  (should
   (equal "|   | a |\n"
	  (org-test-with-temp-text "| a"
	    (org-table-insert-column)
	    (buffer-string))))
  (should
   (equal "  | a |"
	  (org-test-with-temp-text "| <point>a"
	    (org-table-insert-column)
	    (buffer-substring-no-properties (point) (line-end-position)))))
  ;; Handle column insertion when point is before first column.
  (should
   (equal " |   | a |\n"
	  (org-test-with-temp-text " | a |"
	    (org-table-insert-column)
	    (buffer-string))))
  (should
   (equal " |   | a | b |\n"
	  (org-test-with-temp-text " | a | b |"
	    (org-table-insert-column)
	    (buffer-string)))))

(ert-deftest test-org-table/insert-column-with-formula ()
  "Test `org-table-insert-column' with a formula in place."
  (should
   (equal "|   | 1 | 1 | 2 |
#+TBLFM: $4=$2+$3"
	  (org-test-with-temp-text
	   "| 1<point> | 1 | 2 |
#+TBLFM: $3=$1+$2"
	   (org-table-insert-column)
	   (buffer-substring-no-properties (point-min) (point-max))))))


;;; Moving single cells
(ert-deftest test-org-table/move-cell-down ()
  "Test `org-table-move-cell-down' specifications."
  ;; Error out when cell cannot be moved due to not in table, in the
  ;; last row of the table, or is on a hline.
  (should-error
   (org-test-with-temp-text "not in\na table\n"
     (org-table-move-cell-down)))
  (should-error
   (org-test-with-temp-text "| a |"
     (org-table-move-cell-down)))
  (should-error
   (org-test-with-temp-text "| a |\n"
     (org-table-move-cell-down)))
  (should-error
   (org-test-with-temp-text "| a | <point>b |\n"
     (org-table-move-cell-down)))
  (should-error
   (org-test-with-temp-text "| a | b |\n| <point>c | d |\n"
     (org-table-move-cell-down)))
  (should-error
   (org-test-with-temp-text "| a | b |\n| c | <point>d |\n"
     (org-table-move-cell-down)))
  (should-error
   (org-test-with-temp-text "| <point>a |\n|---|\n"
     (org-table-move-cell-down)))
  (should-error
   (org-test-with-temp-text "|<point>---|\n| a |\n"
     (org-table-move-cell-down)))
  ;; Check for correct cell movement
  (should (equal (concat "| c | b |\n"
			 "| a | d |\n"
			 "| e | f |\n")
		 (org-test-with-temp-text
		     (concat "| <point>a | b |\n"
			     "| c | d |\n"
			     "| e | f |\n")
		   (org-table-move-cell-down)
		   (buffer-string))))
  (should (equal (concat "| a | d |\n"
			 "| c | b |\n"
			 "| e | f |\n")
		 (org-test-with-temp-text
		     (concat "| a | <point>b |\n"
			     "| c | d |\n"
			     "| e | f |\n")
		   (org-table-move-cell-down)
		   (buffer-string))))
  (should (equal (concat "| a | b |\n"
			 "| e | d |\n"
			 "| c | f |\n")
		 (org-test-with-temp-text
		     (concat "| a | b |\n"
			     "| <point>c | d |\n"
			     "| e | f |\n")
		   (org-table-move-cell-down)
		   (buffer-string))))
  (should (equal (concat "| a | d |\n"
			 "| c | f |\n"
			 "| e | b |\n")
		 (org-test-with-temp-text
		     (concat "| a |<point> b |\n"
			     "| c | d |\n"
			     "| e | f |\n")
		   (org-table-move-cell-down)
		   (org-table-move-cell-down)
		   (buffer-string))))
  ;; Check for correct handling of hlines which should not change
  ;; position on single cell moves.
  (should (equal (concat "| c | b |\n"
			 "|---+---|\n"
			 "| a | d |\n"
			 "| e | f |\n")
		 (org-test-with-temp-text
		     (concat "| <point>a | b |\n"
			     "|---+---|\n"
			     "| c | d |\n"
			     "| e | f |\n")
		   (org-table-move-cell-down)
		   (buffer-string))))
  (should (equal (concat "| a | d |\n"
			 "|---+---|\n"
			 "| c | f |\n"
			 "| e | b |\n")
		 (org-test-with-temp-text
		     (concat "| a | <point>b |\n"
			     "|---+---|\n"
			     "| c | d |\n"
			     "| e | f |\n")
		   (org-table-move-cell-down)
		   (org-table-move-cell-down)
		   (buffer-string))))
  (should (equal (concat "| a | b |\n"
			 "|---+---|\n"
			 "| c | f |\n"
			 "| e | d |\n")
		 (org-test-with-temp-text
		     (concat "| a | b |\n"
			     "|---+---|\n"
			     "| c | <point>d |\n"
			     "| e | f |\n")
		   (org-table-move-cell-down)
		   (buffer-string))))
  ;; Move single cell even without a final newline.
  (should (equal (concat "| a | d |\n"
			 "|---+---|\n"
			 "| c | f |\n"
			 "| e | b |\n")
		 (org-test-with-temp-text
		     (concat "| a | <point>b |\n"
			     "|---+---|\n"
			     "| c | d |\n"
			     "| e | f |")
		   (org-table-move-cell-down)
		   (org-table-move-cell-down)
		   (buffer-string)))))

(ert-deftest test-org-table/move-cell-up ()
  "Test `org-table-move-cell-up' specifications."
  ;; Error out when cell cannot be moved due to not in table, in the
  ;; last row of the table, or is on a hline.
  (should-error
   (org-test-with-temp-text "not in\na table\n"
     (org-table-move-cell-up)))
  (should-error
   (org-test-with-temp-text "| a |"
     (org-table-move-cell-up)))
  (should-error
   (org-test-with-temp-text "| a |\n"
     (org-table-move-cell-up)))
  (should-error
   (org-test-with-temp-text "| <point>a | b |\n"
     (org-table-move-cell-up)))
  (should-error
   (org-test-with-temp-text "| a | <point>b |\n| c | d |\n"
     (org-table-move-cell-up)))
  (should-error
   (org-test-with-temp-text "| <point>a |\n|---|\n"
     (org-table-move-cell-up)))
  (should-error
   (org-test-with-temp-text "|<point>---|\n| a |\n"
     (org-table-move-cell-up)))
  ;; Check for correct cell movement.
  (should (equal (concat "| c | b |\n"
			 "| a | d |\n"
			 "| e | f |\n")
		 (org-test-with-temp-text
		     (concat "| a | b |\n"
			     "| <point>c | d |\n"
			     "| e | f |\n")
		   (org-table-move-cell-up)
		   (buffer-string))))
  (should (equal (concat "| a | d |\n"
			 "| c | b |\n"
			 "| e | f |\n")
		 (org-test-with-temp-text
		     (concat "| a | b |\n"
			     "| c | <point>d |\n"
			     "| e | f |\n")
		   (org-table-move-cell-up)
		   (buffer-string))))
  (should (equal (concat "| a | b |\n"
			 "| e | d |\n"
			 "| c | f |\n")
		 (org-test-with-temp-text
		     (concat "| a | b |\n"
			     "| c | d |\n"
			     "| <point>e | f |\n")
		   (org-table-move-cell-up)
		   (buffer-string))))
  (should (equal (concat "| a | f |\n"
			 "| c | b |\n"
			 "| e | d |\n")
		 (org-test-with-temp-text
		     (concat "| a | b |\n"
			     "| c | d |\n"
			     "| e |<point> f |\n")
		   (org-table-move-cell-up)
		   (org-table-move-cell-up)
		   (buffer-string))))
  ;; Check for correct handling of hlines which should not change
  ;; position on single cell moves.
  (should (equal (concat "| c | b |\n"
			 "|---+---|\n"
			 "| a | d |\n"
			 "| e | f |\n")
		 (org-test-with-temp-text
		     (concat "| a | b |\n"
			     "|---+---|\n"
			     "| <point>c | d |\n"
			     "| e | f |\n")
		   (org-table-move-cell-up)
		   (buffer-string))))
  (should (equal (concat "| a | f |\n"
			 "|---+---|\n"
			 "| c | b |\n"
			 "| e | d |\n")
		 (org-test-with-temp-text
		     (concat "| a | b |\n"
			     "|---+---|\n"
			     "| c | d |\n"
			     "| e | <point>f |\n")
		   (org-table-move-cell-up)
		   (org-table-move-cell-up)
		   (buffer-string))))
  (should (equal (concat "| a | b |\n"
			 "|---+---|\n"
			 "| c | f |\n"
			 "| e | d |\n")
		 (org-test-with-temp-text
		     (concat "| a | b |\n"
			     "|---+---|\n"
			     "| c | d |\n"
			     "| e | <point>f |\n")
		   (org-table-move-cell-up)
		   (buffer-string))))
  ;; Move single cell even without a final newline.
  (should (equal (concat "| a | f |\n"
			 "|---+---|\n"
			 "| c | b |\n"
			 "| e | d |\n")
		 (org-test-with-temp-text
		     (concat "| a | b |\n"
			     "|---+---|\n"
			     "| c | d |\n"
			     "| e | <point>f |")
		   (org-table-move-cell-up)
		   (org-table-move-cell-up)
		   (buffer-string)))))

(ert-deftest test-org-table/move-cell-right ()
  "Test `org-table-move-cell-right' specifications."
  ;; Error out when cell cannot be moved due to not in table, in the
  ;; last col of the table, or is on a hline.
  (should-error
   (org-test-with-temp-text "not in\na table\n"
     (org-table-move-cell-right)))
  (should-error
   (org-test-with-temp-text "| a |"
     (org-table-move-cell-right)))
  (should-error
   (org-test-with-temp-text "| a |\n"
     (org-table-move-cell-right)))
  (should-error
   (org-test-with-temp-text "| <point>a |\n| b |\n"
     (org-table-move-cell-right)))
  (should-error
   (org-test-with-temp-text "| a | <point>b |\n| c | d |\n"
     (org-table-move-cell-right)))
  (should-error
   (org-test-with-temp-text "| <point>a |\n|---|\n"
     (org-table-move-cell-right)))
  (should-error
   (org-test-with-temp-text "|<point>---|\n| a |\n"
     (org-table-move-cell-right)))
  ;; Check for correct cell movement.
  (should (equal (concat "| b | a | c |\n"
			 "| d | e | f |\n")
		 (org-test-with-temp-text
		     (concat "| <point>a | b | c |\n"
			     "| d | e | f |\n")
		   (org-table-move-cell-right)
		   (buffer-string))))
  (should (equal (concat "| b | c | a |\n"
			 "| d | e | f |\n")
		 (org-test-with-temp-text
		     (concat "| <point>a | b | c |\n"
			     "| d | e | f |\n")
		   (org-table-move-cell-right)
		   (org-table-move-cell-right)
		   (buffer-string))))
  (should (equal (concat "| a | b | c |\n"
			 "| e | f | d |\n")
		 (org-test-with-temp-text
		     (concat "| a | b | c |\n"
			     "| <point> d | e | f |\n")
		   (org-table-move-cell-right)
		   (org-table-move-cell-right)
		   (buffer-string))))
  (should (equal (concat "| a | b | c |\n"
			 "| d | f | e |\n")
		 (org-test-with-temp-text
		     (concat "| a | b | c |\n"
			     "| d | <point>e | f |\n")
		   (org-table-move-cell-right)
		   (buffer-string))))
  (should (equal (concat "| a | b | c |\n"
			 "|---+---+---|\n"
			 "| e | f | d |\n")
		 (org-test-with-temp-text
		     (concat "| a | b | c |\n"
			     "|---+---+---|\n"
			     "| <point>d | e | f |\n")
		   (org-table-move-cell-right)
		   (org-table-move-cell-right)
		   (buffer-string))))
  ;; Move single cell even without a final newline.
  (should (equal (concat "| a | b | c |\n"
			 "|---+---+---|\n"
			 "| e | d | f |\n")
		 (org-test-with-temp-text
		     (concat "| a | b | c |\n"
			     "|---+---+---|\n"
			     "| <point>d | e | f |")
		   (org-table-move-cell-right)
		   (buffer-string)))))

(ert-deftest test-org-table/move-cell-left ()
  "Test `org-table-move-cell-left' specifications."
  ;; Error out when cell cannot be moved due to not in table, in the
  ;; last col of the table, or is on a hline.
  (should-error
   (org-test-with-temp-text "not in\na table\n"
     (org-table-move-cell-left)))
  (should-error
   (org-test-with-temp-text "| a |"
     (org-table-move-cell-left)))
  (should-error
   (org-test-with-temp-text "| a |\n"
     (org-table-move-cell-left)))
  (should-error
   (org-test-with-temp-text "| <point>a |\n| b |\n"
     (org-table-move-cell-left)))
  (should-error
   (org-test-with-temp-text "| <point>a | b |\n| c | d |\n"
     (org-table-move-cell-left)))
  (should-error
   (org-test-with-temp-text "| <point>a |\n|---|\n"
     (org-table-move-cell-left)))
  (should-error
   (org-test-with-temp-text "|<point>---|\n| a |\n"
     (org-table-move-cell-left)))
  ;; Check for correct cell movement.
  (should (equal (concat "| b | a | c |\n"
			 "| d | e | f |\n")
		 (org-test-with-temp-text
		     (concat "| a | <point>b | c |\n"
			     "| d | e | f |\n")
		   (org-table-move-cell-left)
		   (buffer-string))))
  (should (equal (concat "| c | a | b |\n"
			 "| d | e | f |\n")
		 (org-test-with-temp-text
		     (concat "| a | b | <point>c |\n"
			     "| d | e | f |\n")
		   (org-table-move-cell-left)
		   (org-table-move-cell-left)
		   (buffer-string))))
  (should (equal (concat "| a | b | c |\n"
			 "| f | d | e |\n")
		 (org-test-with-temp-text
		     (concat "| a | b | c |\n"
			     "| d | e | <point>f |\n")
		   (org-table-move-cell-left)
		   (org-table-move-cell-left)
		   (buffer-string))))
  (should (equal (concat "| a | b | c |\n"
			 "| d | f | e |\n")
		 (org-test-with-temp-text
		     (concat "| a | b | c |\n"
			     "| d | e | <point>f |\n")
		   (org-table-move-cell-left)
		   (buffer-string))))
  (should (equal (concat "| a | b | c |\n"
			 "|---+---+---|\n"
			 "| f | d | e |\n")
		 (org-test-with-temp-text
		     (concat "| a | b | c |\n"
			     "|---+---+---|\n"
			     "| d | e | <point>f |\n")
		   (org-table-move-cell-left)
		   (org-table-move-cell-left)
		   (buffer-string))))
  ;; Move single cell even without a final newline.
  (should (equal (concat "| a | b | c |\n"
			 "|---+---+---|\n"
			 "| e | d | f |\n")
		 (org-test-with-temp-text
		     (concat "| a | b | c |\n"
			     "|---+---+---|\n"
			     "| d | <point>e | f |")
		   (org-table-move-cell-left)
		   (buffer-string)))))


;;; Moving rows, moving columns

(ert-deftest test-org-table/move-row-down ()
  "Test `org-table-move-row-down' specifications."
  ;; Error out when row cannot be moved, e.g., it is the last row in
  ;; the table.
  (should-error
   (org-test-with-temp-text "| a |"
     (org-table-move-row-down)))
  (should-error
   (org-test-with-temp-text "| a |\n"
     (org-table-move-row-down)))
  (should-error
   (org-test-with-temp-text "| a |\n| <point>b |"
     (org-table-move-row-down)))
  ;; Move data lines.
  (should
   (equal "| b |\n| a |\n"
	  (org-test-with-temp-text "| a |\n| b |\n"
	    (org-table-move-row-down)
	    (buffer-string))))
  (should
   (equal "|---|\n| a |\n"
	  (org-test-with-temp-text "| a |\n|---|\n"
	    (org-table-move-row-down)
	    (buffer-string))))
  ;; Move hlines.
  (should
   (equal "| b |\n|---|\n"
	  (org-test-with-temp-text "|---|\n| b |\n"
	    (org-table-move-row-down)
	    (buffer-string))))
  (should
   (equal "|---|\n|---|\n"
	  (org-test-with-temp-text "|---|\n|---|\n"
	    (org-table-move-row-down)
	    (buffer-string))))
  ;; Move rows even without a final newline.
  (should
   (equal "| b |\n| a |\n"
	  (org-test-with-temp-text "| a |\n| b |"
	    (org-table-move-row-down)
	    (buffer-string)))))

(ert-deftest test-org-table/move-row-up ()
  "Test `org-table-move-row-up' specifications."
  ;; Error out when row cannot be moved, e.g., it is the first row in
  ;; the table.
  (should-error
   (org-test-with-temp-text "| a |"
     (org-table-move-row-up)))
  (should-error
   (org-test-with-temp-text "| a |\n"
     (org-table-move-row-up)))
  ;; Move data lines.
  (should
   (equal "| b |\n| a |\n"
	  (org-test-with-temp-text "| a |\n| <point>b |\n"
	    (org-table-move-row-up)
	    (buffer-string))))
  (should
   (equal "| b |\n|---|\n"
	  (org-test-with-temp-text "|---|\n| <point>b |\n"
	    (org-table-move-row-up)
	    (buffer-string))))
  ;; Move hlines.
  (should
   (equal "|---|\n| a |\n"
	  (org-test-with-temp-text "| a |\n|<point>---|\n"
	    (org-table-move-row-up)
	    (buffer-string))))
  (should
   (equal "|---|\n|---|\n"
	  (org-test-with-temp-text "|---|\n|<point>---|\n"
	    (org-table-move-row-up)
	    (buffer-string))))
  ;; Move rows even without a final newline.
  (should
   (equal "| b |\n| a |\n"
	  (org-test-with-temp-text "| a |\n| <point>b |"
	    (org-table-move-row-up)
	    (buffer-string)))))



;;; Shrunk columns

(ert-deftest test-org-table/toggle-column-width ()
  "Test `org-table-toggle-columns-width' specifications."
  ;; Error when not at a column.
  (should-error
   (org-test-with-temp-text "<point>a"
     (org-table-toggle-column-width)))
  ;; A shrunk column is overlaid with
  ;; `org-table-shrunk-column-indicator'.
  (should
   (equal org-table-shrunk-column-indicator
	  (org-test-with-temp-text "| <point>a |"
	    (org-table-toggle-column-width)
	    (overlay-get (car (overlays-at (point))) 'display))))
  (should
   (equal org-table-shrunk-column-indicator
	  (org-test-with-temp-text "| a |\n|-<point>--|"
	    (org-table-toggle-column-width)
	    (overlay-get (car (overlays-at (point))) 'display))))
  ;; Shrink every field in the same column.
  (should
   (equal org-table-shrunk-column-indicator
	  (org-test-with-temp-text "| a |\n|-<point>--|"
	    (org-table-toggle-column-width)
	    (overlay-get (car (overlays-at (1+ (line-beginning-position 0))))
			 'display))))
  ;; When column is already shrunk, expand it, i.e., remove overlays.
  (should-not
   (org-test-with-temp-text "| <point>a |"
     (org-table-toggle-column-width)
     (org-table-toggle-column-width)
     (overlays-in (point-min) (point-max))))
  (should-not
   (org-test-with-temp-text "| a |\n| <point>b |"
     (org-table-toggle-column-width)
     (org-table-toggle-column-width)
     (overlays-in (point-min) (point-max))))
  ;; With a column width cookie, limit overlay to the specified number
  ;; of characters.
  (should
   (equal "| abc"
	  (org-test-with-temp-text "| <3>  |\n| <point>abcd |"
	    (org-table-toggle-column-width)
	    (buffer-substring (line-beginning-position)
			      (overlay-start
			       (car (overlays-in (line-beginning-position)
						 (line-end-position))))))))
  (should
   (equal "| a  "
	  (org-test-with-temp-text "| <3>  |\n| <point>a   |"
	    (org-table-toggle-column-width)
	    (buffer-substring (line-beginning-position)
			      (overlay-start
			       (car (overlays-in (line-beginning-position)
						 (line-end-position))))))))
  (should
   (equal (concat "----" org-table-shrunk-column-indicator)
	  (org-test-with-temp-text "| <3>  |\n|--<point>----|"
	    (org-table-toggle-column-width)
	    (overlay-get
	     (car (overlays-in (line-beginning-position)
			       (line-end-position)))
	     'display))))
  ;; Width only takes into account visible characters.
  (should
   (equal "| [[http"
	  (org-test-with-temp-text "| <4> |\n| <point>[[http://orgmode.org]] |"
	    (org-table-toggle-column-width)
	    (buffer-substring (line-beginning-position)
			      (overlay-start
			       (car (overlays-in (line-beginning-position)
						 (line-end-position))))))))
  ;; Before the first column or after the last one, ask for columns
  ;; ranges.
  (should
   (catch :exit
     (org-test-with-temp-text "| a |"
       (cl-letf (((symbol-function 'read-string)
		  (lambda (&rest_) (throw :exit t))))
	 (org-table-toggle-column-width)
	 nil))))
  (should
   (catch :exit
     (org-test-with-temp-text "| a |<point>"
       (cl-letf (((symbol-function 'read-string)
		  (lambda (&rest_) (throw :exit t))))
	 (org-table-toggle-column-width)
	 nil))))
  ;; When optional argument ARG is a string, toggle specified columns.
  (should
   (equal org-table-shrunk-column-indicator
	  (org-test-with-temp-text "| <point>a | b |"
	    (org-table-toggle-column-width "2")
	    (overlay-get (car (overlays-at (- (point-max) 2))) 'display))))
  (should
   (equal '("b" "c")
	  (org-test-with-temp-text "| a | b | c | d |"
	    (org-table-toggle-column-width "2-3")
	    (sort (mapcar (lambda (o) (overlay-get o 'help-echo))
			  (overlays-in (point-min) (point-max)))
		  #'string-lessp))))
  (should
   (equal '("b" "c" "d")
	  (org-test-with-temp-text "| a | b | c | d |"
	    (org-table-toggle-column-width "2-")
	    (sort (mapcar (lambda (o) (overlay-get o 'help-echo))
			  (overlays-in (point-min) (point-max)))
		  #'string-lessp))))
  (should
   (equal '("a" "b")
	  (org-test-with-temp-text "| a | b | c | d |"
	    (org-table-toggle-column-width "-2")
	    (sort (mapcar (lambda (o) (overlay-get o 'help-echo))
			  (overlays-in (point-min) (point-max)))
		  #'string-lessp))))
  (should
   (equal '("a" "b" "c" "d")
	  (org-test-with-temp-text "| a | b | c | d |"
	    (org-table-toggle-column-width "-")
	    (sort (mapcar (lambda (o) (overlay-get o 'help-echo))
			  (overlays-in (point-min) (point-max)))
		  #'string-lessp))))
  (should
   (equal '("a" "d")
	  (org-test-with-temp-text "| a | b | c | d |"
	    (org-table-toggle-column-width "1-3")
	    (org-table-toggle-column-width "2-4")
	    (sort (mapcar (lambda (o) (overlay-get o 'help-echo))
			  (overlays-in (point-min) (point-max)))
		  #'string-lessp))))
  ;; When ARG is (16), remove any column overlay.
  (should-not
   (org-test-with-temp-text "| <point>a |"
     (org-table-toggle-column-width)
     (org-table-toggle-column-width '(16))
     (overlays-in (point-min) (point-max))))
  (should-not
   (org-test-with-temp-text "| a | b | c | d |"
     (org-table-toggle-column-width "-")
     (org-table-toggle-column-width '(16))
     (overlays-in (point-min) (point-max)))))

(ert-deftest test-org-table/shrunk-columns ()
  "Test behaviour of shrunk column."
  ;; Edition automatically expands a shrunk column.
  (should-not
   (org-test-with-temp-text "| <point>a |"
     (org-table-toggle-column-width)
     (insert "a")
     (overlays-in (point-min) (point-max))))
  ;; Other columns are not changed.
  (should
   (org-test-with-temp-text "| <point>a | b |"
     (org-table-toggle-column-width "-")
     (insert "a")
     (overlays-in (point-min) (point-max))))
  ;; Moving a shrunk column doesn't alter its state.
  (should
   (equal "a"
	  (org-test-with-temp-text "| <point>a | b |"
	    (org-table-toggle-column-width)
	    (org-table-move-column-right)
	    (overlay-get (car (overlays-at (point))) 'help-echo))))
  (should
   (equal "a"
	  (org-test-with-temp-text "| <point>a |\n| b |"
	    (org-table-toggle-column-width)
	    (org-table-move-row-down)
	    (overlay-get (car (overlays-at (point))) 'help-echo))))
  ;; State is preserved upon inserting a column.
  (should
   (equal '("a")
	  (org-test-with-temp-text "| <point>a |"
	    (org-table-toggle-column-width)
	    (org-table-insert-column)
	    (sort (mapcar (lambda (o) (overlay-get o 'help-echo))
			  (overlays-in (point-min) (point-max)))
		  #'string-lessp))))
  ;; State is preserved upon deleting a column.
  (should
   (equal '("a" "c")
	  (org-test-with-temp-text "| a | <point>b | c |"
	    (org-table-toggle-column-width "-")
	    (org-table-delete-column)
	    (sort (mapcar (lambda (o) (overlay-get o 'help-echo))
			  (overlays-in (point-min) (point-max)))
		  #'string-lessp))))
  ;; State is preserved upon deleting a row.
  (should
   (equal '("b1" "b2")
	  (org-test-with-temp-text "| a1 | a2 |\n| b1 | b2 |"
	    (org-table-toggle-column-width "-")
	    (org-table-kill-row)
	    (sort (mapcar (lambda (o) (overlay-get o 'help-echo))
			  (overlays-in (point-min) (point-max)))
		  #'string-lessp))))
  (should
   (equal '("a1" "a2")
	  (org-test-with-temp-text "| a1 | a2 |\n| <point>b1 | b2 |"
	    (org-table-toggle-column-width "-")
	    (org-table-kill-row)
	    (sort (mapcar (lambda (o) (overlay-get o 'help-echo))
			  (overlays-in (point-min) (point-max)))
		  #'string-lessp))))
  ;; State is preserved upon inserting a row or hline.
  (should
   (equal '("" "a1" "b1")
	  (org-test-with-temp-text "| a1 | a2 |\n| <point>b1 | b2 |"
	    (org-table-toggle-column-width)
	    (org-table-insert-row)
	    (sort (mapcar (lambda (o) (overlay-get o 'help-echo))
			  (overlays-in (point-min) (point-max)))
		  #'string-lessp))))
  (should
   (equal '("a1" "b1")
	  (org-test-with-temp-text "| a1 | a2 |\n| <point>b1 | b2 |"
	    (org-table-toggle-column-width)
	    (org-table-insert-hline)
	    (sort (mapcar (lambda (o) (overlay-get o 'help-echo))
			  (overlays-in (point-min) (point-max)))
		  #'string-lessp))))
  ;; State is preserved upon sorting a column for all the columns but
  ;; the one being sorted.
  (should
   (equal '("a2" "b2")
	  (org-test-with-temp-text "| <point>a1 | a2 |\n| <point>b1 | b2 |"
	    (org-table-toggle-column-width "-")
	    (org-table-sort-lines nil ?A)
	    (sort (mapcar (lambda (o) (overlay-get o 'help-echo))
			  (overlays-in (point-min) (point-max)))
		  #'string-lessp))))
  ;; State is preserved upon replacing a field non-interactively.
  (should
   (equal '("a")
	  (org-test-with-temp-text "| <point>a |"
	    (org-table-toggle-column-width)
	    (org-table-get-field nil "b")
	    (mapcar (lambda (o) (overlay-get o 'help-echo))
		    (overlays-in (point-min) (point-max))))))
  ;; Moving to next field doesn't change shrunk state.
  (should
   (equal "a"
	  (org-test-with-temp-text "| <point>a | b |"
	    (org-table-toggle-column-width)
	    (org-table-next-field)
	    (overlay-get (car (overlays-at (1+ (line-beginning-position))))
			 'help-echo))))
  (should
   (equal "b"
	  (org-test-with-temp-text "| a | <point>b |"
	    (org-table-toggle-column-width)
	    (goto-char 2)
	    (org-table-next-field)
	    (overlay-get (car (overlays-at (point))) 'help-echo))))
  ;; Aligning table doesn't alter shrunk state.
  (should
   (equal "a"
	  (org-test-with-temp-text "| <point>a | b   |"
	    (org-table-toggle-column-width)
	    (org-table-align)
	    (overlay-get (car (overlays-at (1+ (line-beginning-position))))
			 'help-echo))))
  (should
   (equal "b"
	  (org-test-with-temp-text "|---+-----|\n| a | <point>b   |"
	    (org-table-toggle-column-width)
	    (org-table-align)
	    (overlay-get (car (overlays-at (point)))
			 'help-echo))))
  (should
   (equal
    '("b")
    (org-test-with-temp-text "|---+-----|\n| a | <point>b   |"
      (org-table-toggle-column-width)
      (org-table-align)
      (mapcar (lambda (o) (overlay-get o 'help-echo))
	      (overlays-in (line-beginning-position) (line-end-position))))))
  ;; Recalculating formulas doesn't change shrunk state.
  (should
   (equal "2"
	  (org-test-with-temp-text "| 1 | <point>0 |\n#+TBLFM: $2=$1+1\n"
	    (org-table-toggle-column-width)
	    (org-table-recalculate)
	    (overlay-get (car (overlays-at (point))) 'help-echo)))))



;;; Miscellaneous

(ert-deftest test-org-table/current-column ()
  "Test `org-table-current-column' specifications."
  (should
   (= 1 (org-test-with-temp-text "| <point>a |"
	  (org-table-current-column))))
  (should
   (= 1 (org-test-with-temp-text "|-<point>--|"
	  (org-table-current-column))))
  (should
   (= 2 (org-test-with-temp-text "| 1 | <point>2 |"
	  (org-table-current-column))))
  (should
   (= 2 (org-test-with-temp-text "|---+-<point>--|"
	  (org-table-current-column)))))

(ert-deftest test-org-table/get-field ()
  "Test `org-table-get-field' specifications."
  ;; Regular test.
  (should
   (equal " a "
	  (org-test-with-temp-text "| <point>a |" (org-table-get-field))))
  ;; Get field in open last column.
  (should
   (equal " a "
	  (org-test-with-temp-text "| <point>a " (org-table-get-field))))
  ;; Get empty field.
  (should
   (equal ""
	  (org-test-with-temp-text "|<point>|" (org-table-get-field))))
  (should
   (equal " "
	  (org-test-with-temp-text "| <point>|" (org-table-get-field))))
  ;; Outside the table, return the empty string.
  (should
   (equal ""
	  (org-test-with-temp-text "<point>| a |" (org-table-get-field))))
  (should
   (equal ""
	  (org-test-with-temp-text "| a |<point>" (org-table-get-field))))
  ;; With optional N argument, select a particular column in current
  ;; row.
  (should
   (equal " 3 "
	  (org-test-with-temp-text "| 1 | 2 | 3 |" (org-table-get-field 3))))
  (should
   (equal " 4 "
	  (org-test-with-temp-text "| 1 | 2 |\n<point>| 3 | 4 |"
	    (org-table-get-field 2))))
  ;; REPLACE optional argument is used to replace selected field.
  (should
   (equal "| foo |"
	  (org-test-with-temp-text "| <point>1 |"
	    (org-table-get-field nil " foo ")
	    (buffer-string))))
  (should
   (equal "| 1 | 2 | foo |"
	  (org-test-with-temp-text "| 1 | 2 | 3 |"
	    (org-table-get-field 3 " foo ")
	    (buffer-string))))
  (should
   (equal " 4 "
	  (org-test-with-temp-text "| 1 | 2 |\n<point>| 3 | 4 |"
	    (org-table-get-field 2))))
  ;; An empty REPLACE string clears the field.
  (should
   (equal "| |"
	  (org-test-with-temp-text "| <point>1 |"
	    (org-table-get-field nil "")
	    (buffer-string))))
  ;; When using REPLACE still return old value.
  (should
   (equal " 1 "
	  (org-test-with-temp-text "| <point>1 |"
	    (org-table-get-field nil " foo ")))))

(provide 'test-org-table)

;;; test-org-table.el ends here
