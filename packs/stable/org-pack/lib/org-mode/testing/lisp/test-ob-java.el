;;; test-ob-java.el --- tests for ob-java.el

;; Copyright (c) 2020-2021 Free Software Foundation, Inc.
;; Authors: Eric Schulte
;;          Dan Davison
;; Maintainer: Ian Martins <ianxm@jhu.edu>

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:
(require 'org-test "../testing/org-test")

(require 'ob-core)
(defvar org-babel-temporary-directory ; from ob-core
  (if (boundp 'org-babel-temporary-directory)
    org-babel-temporary-directory
  (temporary-file-directory)))

(org-test-for-executable "java")
(org-test-for-executable "javac")
(unless (featurep 'ob-java)
  (signal 'missing-test-dependency "Support for java code blocks"))

; simple tests

(ert-deftest ob-java/simple ()
  "Hello world program that writes output. Also tests that
ob-java defaults to scripting mode."
  (org-test-with-temp-text
      "#+begin_src java :dir 'nil :results silent
System.out.print(42);
#+end_src"
   (should (string= "42" (org-babel-execute-src-block)))))

(ert-deftest ob-java/simple-with-bracket ()
  "Hello world program that outputs an open square bracket."
  (org-test-with-temp-text
      "#+begin_src java :dir 'nil :results output silent
System.out.print(\"[42\");
#+end_src"
   (should (string= "[42" (org-babel-execute-src-block)))))

(ert-deftest ob-java/simple-with-quote ()
  "Hello world program that writes quotes."
  (org-test-with-temp-text
      "#+begin_src java :dir 'nil :results output silent
System.out.print(\"\\\"42\\\"\");
#+end_src"
   (should (string= "\"42\"" (org-babel-execute-src-block)))))

(ert-deftest ob-java/simple-return-int ()
  "Hello world program that returns an int value."
  (org-test-with-temp-text
      "#+begin_src java :dir 'nil :results value silent
return 42;
#+end_src"
   (should (eq 42 (org-babel-execute-src-block)))))

(ert-deftest ob-java/simple-return-float ()
  "Hello world program that returns a float value."
  (org-test-with-temp-text
      "#+begin_src java :dir 'nil :results value silent
return 42.0;
#+end_src"
   (should (equal 42.0 (org-babel-execute-src-block)))))

(ert-deftest ob-java/simple-return-string ()
  "Hello world program that returns a string value."
  (org-test-with-temp-text
      "#+begin_src java :dir 'nil :results value silent
return \"forty two\";
#+end_src"
    (should (string= "forty two" (org-babel-execute-src-block)))))

(ert-deftest ob-java/simple-with-main ()
  "Hello world program that defines a main function."
  (org-test-with-temp-text
      "#+begin_src java :dir 'nil :results output silent
public static void main(String[] args) {
    System.out.print(42);
}
#+end_src"
    (should (string= "42" (org-babel-execute-src-block)))))

(ert-deftest ob-java/simple-with-two-methods ()
  "Hello world program with two methods and no class."
  (org-test-with-temp-text
      "#+begin_src java :dir 'nil :results output silent
public static void main(String[] args) {
    System.out.print(foo());
}
public static int foo() {
    return 42;
}
#+end_src"
    (should (string= "42" (org-babel-execute-src-block)))))

(ert-deftest ob-java/simple-with-no-main ()
  "Hello world program with no main method.  Babel adds a dummy one so it can run without error."
  (org-test-with-temp-text
      "#+begin_src java :dir 'nil :results output silent
public static int foo() {
    return 42;
}
#+end_src"
    (should (string= "success" (org-babel-execute-src-block)))))

(ert-deftest ob-java/simple-with-main-args-array ()
  "Hello world program that defines a main function with the square brackets after `args'."
  (org-test-with-temp-text
      "#+begin_src java :dir 'nil :results output silent
public static void main(String args[]) {
    System.out.print(42);
}
#+end_src"
    (should (string= "42" (org-babel-execute-src-block)))))

(ert-deftest ob-java/simple-with-main-whitespace ()
  "Hello world program that defines a main function with the square brackets after `args'."
  (org-test-with-temp-text
      "#+begin_src java :dir 'nil :results output silent
public
static
void
main
 (
 String
 args []
 )
{
    System.out.print(42);
}
#+end_src"
    (should (string= "42" (org-babel-execute-src-block)))))

(ert-deftest ob-java/simple-with-class ()
  "Hello world program that defines a class."
  (org-test-with-temp-text
      "#+begin_src java :dir 'nil :results output silent
public class Simple {
    public static void main(String[] args) {
        System.out.print(42);
    }
}
#+end_src"
   (should (string= "42" (org-babel-execute-src-block)))))

(ert-deftest ob-java/simple-with-non-public-class ()
  "Hello world program that defines a non-public class."
  (org-test-with-temp-text
      "#+begin_src java :dir 'nil :results output silent
class Simple {
    public static void main(String[] args) {
        System.out.print(42);
    }
}
#+end_src"
   (should (string= "42" (org-babel-execute-src-block)))))

(ert-deftest ob-java/simple-with-class-and-package ()
  "Hello world program that defines a class and package."
  (org-test-with-temp-text
      "#+begin_src java :dir 'nil :results output silent
package pkg;
public class Simple {
    public static void main(String[] args) {
        System.out.print(42);
    }
}
#+end_src"
    (should (string= "42" (org-babel-execute-src-block)))))

(ert-deftest ob-java/simple-with-class-attr ()
  "Hello world program with class header attribute."
  (org-test-with-temp-text
      "#+begin_src java :dir 'nil :results output silent :classname Simple
public static void main(String[] args) {
    System.out.print(42);
}
#+end_src"
   (should (string= "42" (org-babel-execute-src-block)))))

(ert-deftest ob-java/simple-with-class-attr-with-package ()
  "Hello world program with class attr with package."
  (org-test-with-temp-text
      "#+begin_src java :dir 'nil :results output silent :classname pkg.Simple
public static void main(String[] args) {
    System.out.print(42);
}
#+end_src"
    (should (string= "42" (org-babel-execute-src-block)))))


(ert-deftest ob-java/one-arg ()
  "Command line arg."
  (org-test-with-temp-text
      "#+begin_src java :dir 'nil :results output silent :cmdargs \"fortytwo\"
System.out.print(args[0]);
#+end_src"
    (should (string= "fortytwo" (org-babel-execute-src-block)))))

(ert-deftest ob-java/args-quoted-string ()
  "Two command line args, first contains a space."
  (org-test-with-temp-text
      "#+begin_src java :dir 'nil :results output silent :cmdargs \"\\\"forty two\\\" 42\"
System.out.println(args[0]);
System.out.println(args[1]);
#+end_src"
    (should (string= "forty two\n42\n" (org-babel-execute-src-block)))))

;; var tests

(ert-deftest ob-java/integer-var ()
  "Read and write an integer variable."
  (org-test-with-temp-text
      "#+begin_src java :dir 'nil :var a=42 :results output silent
System.out.print(a);
#+end_src"
    (should (string= "42" (org-babel-execute-src-block)))))

(ert-deftest ob-java/var-with-main ()
  "Read and write an integer variable, with main function provided."
  (org-test-with-temp-text
      "#+begin_src java :dir 'nil :var a=42 :results output silent
public static void main(String[] args) {
    System.out.print(a);
}
#+end_src"
    (should (string= "42" (org-babel-execute-src-block)))))

(ert-deftest ob-java/var-with-class ()
  "Read and write an integer variable, with class provided."
  (org-test-with-temp-text
      "#+begin_src java :dir 'nil :var a=42 :results output silent
public class Main {
    public static void main(String[] args) {
        System.out.print(a);
    }
}
#+end_src"
    (should (string= "42" (org-babel-execute-src-block)))))

(ert-deftest ob-java/var-with-class-and-package ()
  "Read and write an integer variable, with class and package provided."
  (org-test-with-temp-text
      "#+begin_src java :dir 'nil :var a=42 :results output silent
package pkg;
public class Main {
    public static void main(String[] args) {
        System.out.print(a);
    }
}
#+end_src"
    (should (string= "42" (org-babel-execute-src-block)))))

(ert-deftest ob-java/var-with-class-and-hanging-curlies ()
  "Read and write an integer variable, with class with hanging curlies."
  (org-test-with-temp-text
      "#+begin_src java :dir 'nil :var a=42 :results output silent
public class Main
{
    public static void main(String[] args)
    {
        System.out.print(a);
    }
}
#+end_src"
    (should (string= "42" (org-babel-execute-src-block)))))

(ert-deftest ob-java/two-vars ()
  "Read two integer variables, combine and write them."
  (org-test-with-temp-text
      "#+begin_src java :dir 'nil :var a=21 b=2 :results output silent
System.out.print(a*b);
#+end_src"
    (should (string= "42" (org-babel-execute-src-block)))))

(ert-deftest ob-java/string-var ()
  "Read and write a string variable."
  (org-test-with-temp-text
      "#+begin_src java :dir 'nil :var a=\"forty two\" :results output silent
System.out.print(String.format(\"%s, len=%d\", a, a.length()));
#+end_src"
    (should (string= "forty two, len=9" (org-babel-execute-src-block)))))

(ert-deftest ob-java/multiline-string-var ()
  "Java doesn't support multiline string literals, so this errors."
  (org-test-with-temp-text
      "#+begin_src java :dir 'nil :var a=\"forty\ntwo\" :results output silent
System.out.print(String.format(\"%s, len=%d\", a, a.length()));
#+end_src"
    (should-error (org-babel-execute-src-block)))
  :type 'error)

;; return list

(ert-deftest ob-java/return-vector-using-list ()
  "Return a vector using a list."
  (org-test-with-temp-text
      "#+begin_src java :dir 'nil :results value vector silent
import java.util.List;
import java.util.Arrays;
List<List<Integer>> a = Arrays.asList(Arrays.asList(4),
                                      Arrays.asList(2));
return a;
#+end_src"
    (should (equal '((4) (2))
                   (org-babel-execute-src-block)))))

(ert-deftest ob-java/return-vector-using-array ()
  "Return a vector using an array."
  (org-test-with-temp-text
      "#+begin_src java :dir 'nil :results value vector silent
Integer[][] a = {{4}, {2}};
return a;
#+end_src"
    (should (equal '((4) (2))
                   (org-babel-execute-src-block)))))

(ert-deftest ob-java/read-return-list ()
  "Read and return a list."
  (org-test-with-temp-text
      "#+begin_src java :dir 'nil :var a=java_list :results value silent
import java.util.List;
import java.util.Arrays;
List<String> b = Arrays.asList(a.get(0).get(0),
                               a.get(1).get(0));
return b;
#+end_src

#+name: java_list
- forty
- two"
    (should (equal '("forty" "two")
                   (org-babel-execute-src-block)))))

(ert-deftest ob-java/read-list-return-array ()
  "Read a list and return an array."
  (org-test-with-temp-text
      "#+begin_src java :dir 'nil :var a=java_list :results value silent
String[] b = {a.get(0).get(0), a.get(1).get(0)};
return b;
#+end_src

#+name: java_list
- forty
- two"
    (should (equal '("forty" "two")
                   (org-babel-execute-src-block)))))

(ert-deftest ob-java/read-return-list-with-package ()
  "Return a vector."
  (org-test-with-temp-text
      "#+begin_src java :dir 'nil :var a=java_list :results value silent
package pkg;
import java.util.List;
import java.util.Arrays;
List<String> b = Arrays.asList(a.get(0).get(0),
                               a.get(1).get(0));
return b;
#+end_src

#+name: java_list
- forty
- two"
    (should (equal '("forty" "two")
                   (org-babel-execute-src-block)))))

(ert-deftest ob-java/output-list-with-spaces ()
  "Return a vector."
  (org-test-with-temp-text
      "#+begin_src java :dir 'nil :results output list raw silent
System.out.println(\"forty two\");
System.out.println(\"forty two\");
#+end_src"
    (should (equal "forty two\nforty two\n"
                   (org-babel-execute-src-block)))))

;; list vars

(ert-deftest ob-java/list-var ()
  "Read and write a list variable."
  (org-test-with-temp-text
      "#+begin_src java :dir 'nil :var a='(\"forty\" \"two\") :results value silent
import java.util.List;
List<String> b = a;
return b;
#+end_src"
    (should (equal '("forty" "two")
                   (org-babel-execute-src-block)))))

(ert-deftest ob-java/vector-var ()
  "Read and write a vector variable."
  (org-test-with-temp-text
      "#+begin_src java :dir 'nil :var a='[\"forty\" \"two\"] :results value silent
import java.util.List;
List<String> b = a;
return b;
#+end_src"
    (should (equal '("forty" "two")
                   (org-babel-execute-src-block)))))

(ert-deftest ob-java/matrix-var ()
  "Read and write matrix variable."
  (org-test-with-temp-text
      "#+begin_src java :dir 'nil :var a=java_matrix :results value silent
import java.util.List;
import java.util.Arrays;
List<List<Integer>> b = Arrays.asList(Arrays.asList(a.get(0).get(0), a.get(1).get(0)),
                                      Arrays.asList(a.get(0).get(1), a.get(1).get(1)));
return b; // transpose
#+end_src

#+name: java_matrix
| 2 | 1 |
| 4 | 2 |"
    (should (equal '((2 4) (1 2))
                   (org-babel-execute-src-block)))))

(ert-deftest ob-java/matrix-var-with-header ()
  "Read matrix variable and write it with header."
  (org-test-with-temp-text
      "#+begin_src java :dir 'nil :var a=java_matrix :results value table silent
import java.util.List;
import java.util.Arrays;
List<List> b = Arrays.asList(Arrays.asList(\"col1\", \"col2\"),
                                     null,
                                     Arrays.asList(a.get(0).get(0), a.get(1).get(0)),
                                     Arrays.asList(a.get(0).get(1), a.get(1).get(1)));
return b; // transpose
#+end_src

#+name: java_matrix
| 2 | 1 |
| 4 | 2 |"
    (should (equal '(("col1" "col2") hline (2 4) (1 2))
                   (org-babel-execute-src-block)))))

;; output table

(ert-deftest ob-java/output-table-with-header ()
  "Write a table that includes a header."
  (org-test-with-temp-text
      "#+begin_src java :dir 'nil :var a=java_matrix :results output raw table silent
System.out.println(\"|col1|col2|\");
System.out.println(\"|-\");
for (int ii=0; ii<a.size(); ii++) {
    for (int jj=0; jj<a.get(0).size(); jj++) {
        System.out.print(\"|\" + a.get(ii).get(jj));
    }
    System.out.println(\"\");
 }
#+end_src

#+name: java_matrix
| 2 | 1 |
| 4 | 2 |"
    (should (equal "|col1|col2|\n|-\n|2|1\n|4|2\n"
                   (org-babel-execute-src-block)))))

(ert-deftest ob-java/inhomogeneous_table ()
  "Read and write an inhomogeneous table."
  (org-test-with-temp-text
      "#+begin_src java :dir 'nil :var a=java_table :results value silent
import java.util.List;
import java.util.Arrays;
List<List> b = Arrays.asList(Arrays.asList(a.get(0).get(0),
                                           Integer.parseInt(a.get(0).get(1))*2),
                             Arrays.asList(a.get(1).get(0),
                                           Integer.parseInt(a.get(1).get(1))*2));
return b;
#+end_src

#+name: java_table
  | string | number |
  |--------+--------|
  | forty  |      2 |
  | two    |      1 |"
   (should (equal
            '(("forty" 4) ("two" 2))
            (org-babel-execute-src-block)))))

;; imports

(ert-deftest ob-java/import_library ()
  "Import a standard java library."
  (org-test-with-temp-text
      "#+begin_src java :dir 'nil :results output silent :imports java.util.Base64
byte[] encoded = Base64.getEncoder().encode(\"42\".getBytes());
String decoded = new String(Base64.getDecoder().decode(encoded));
System.out.print(String.format(\"encoded=%s, decoded=%s\", new String(encoded), decoded));
#+end_src"
   (should (string=
            "encoded=NDI=, decoded=42"
            (org-babel-execute-src-block)))))

(ert-deftest ob-java/import_library_inline ()
  "Import a standard java library."
  (org-test-with-temp-text
      "#+begin_src java :dir 'nil :results output silent
import java.util.Base64;
byte[] encoded = Base64.getEncoder().encode(\"42\".getBytes());
String decoded = new String(Base64.getDecoder().decode(encoded));
System.out.print(String.format(\"encoded=%s, decoded=%s\", new String(encoded), decoded));
#+end_src"
   (should (string=
            "encoded=NDI=, decoded=42"
            (org-babel-execute-src-block)))))

;; tangle

(ert-deftest ob-java/tangle ()
  "Tangle a source block."
  (org-test-with-temp-text-in-file
      "#+begin_src java :dir 'nil :tangle \"Tangle.java\" :results value :classname Tangle
return \"tangled\";
#+end_src"
    (should
     (string=
      "public class Tangle {
    public static void main(String[] args) {
        return \"tangled\";
    }
}
"
      (unwind-protect
          (progn (org-babel-tangle)
                 (with-temp-buffer
                   (insert-file-contents "Tangle.java")
                   (untabify (point-min) (point-max))
                   (buffer-string)))
        (delete-file "Tangle.java"))))))

(ert-deftest ob-java/tangle-with-package ()
  "Tangle a source block."
  (org-test-with-temp-text-in-file
      "#+begin_src java :dir 'nil :tangle \"tangle/Tangle.java\" :results value :classname tangle.Tangle
return \"tangled\";
#+end_src"
    (should
     (string=
      "package tangle;

public class Tangle {
    public static void main(String[] args) {
        return \"tangled\";
    }
}
"
      (unwind-protect
          (progn
            (make-directory "tangle")
            (org-babel-tangle)
            (with-temp-buffer
              (insert-file-contents "tangle/Tangle.java")
              (untabify (point-min) (point-max))
              (buffer-string)))
        (delete-file "tangle/Tangle.java")
        (delete-directory "tangle"))))))


;; specify output dir

(ert-deftest ob-java/simple-dir ()
  "Hello world program that writes output."
  (org-test-with-temp-text
      (format  "#+begin_src java :dir %s :results output silent
System.out.print(42);
#+end_src" org-babel-temporary-directory)
    (should (string=
             "42"
             (unwind-protect
                 (org-babel-execute-src-block)
               (delete-file (concat (file-name-as-directory org-babel-temporary-directory)
                                    "Main.java"))
               (delete-file (concat (file-name-as-directory org-babel-temporary-directory)
                                    "Main.class")))))))

(ert-deftest ob-java/simple-dir-with-package ()
  "Hello world program that writes output."
  (org-test-with-temp-text
      (format "#+begin_src java :dir %s :results output silent
package pkg;

public class Main {
    public static void main(String[] args) {
      System.out.print(42);
    }
}
#+end_src" org-babel-temporary-directory)
    (should (string=
             "42"
             (unwind-protect
                 (org-babel-execute-src-block)
               (delete-file (concat (file-name-as-directory org-babel-temporary-directory)
                                    "pkg/Main.java"))
               (delete-file (concat (file-name-as-directory org-babel-temporary-directory)
                                    "pkg/Main.class"))
               (delete-directory (concat (file-name-as-directory org-babel-temporary-directory)
                                         "pkg")))))))


;;; test-ob-java.el ends here
