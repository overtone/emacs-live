;;; test-ob-vala.el --- tests for ob-vala.el

;; Copyright (C) 2017, 2019 Christian Garbs
;; Authors: Christian Garbs

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

;;; Code:
(unless (featurep 'ob-vala)
  (signal 'missing-test-dependency "Support for Vala code blocks"))

(org-test-for-executable org-babel-vala-compiler)

(ert-deftest ob-vala/assert ()
  (should t))

(ert-deftest ob-vala/static-output ()
  "Parse static output to variable."
  (should (= 42
	     (org-test-with-temp-text
	      "
#+begin_src vala
  class Demo.HelloWorld : GLib.Object {
      public static int main(string[] args) {
          stdout.printf(\"42\n\");
          return 0;
      }
  }
#+end_src"
	      (org-babel-next-src-block)
	      (org-babel-execute-src-block)))))

(ert-deftest ob-vala/return-numerics ()
  "Return multiple numeric values."
  (should (equal '((0) (1) (2))
		 (org-test-with-temp-text
		  "
#+begin_src vala
  class Demo.HelloWorld : GLib.Object {
      public static int main(string[] args) {
          for (int i=0; i<3; i++) {
              stdout.printf(\"%d\n\", i);
          }
          return 0;
      }
  }
#+end_src"
		  (org-babel-next-src-block)
		  (org-babel-execute-src-block)))))

(ert-deftest ob-vala/compiler-args ()
  "Pass compiler arguments."
  (should (string= "Foo"
		   (org-test-with-temp-text
		    "
#+begin_src vala :flags -D FOO
  class Demo.HelloWorld : GLib.Object {
      public static int main(string[] args) {
#if FOO
          stdout.printf(\"Foo\n\");
#else
          stdout.printf(\"No foo\n\");
#endif
          return 0;
      }
  }
#+end_src"
		    (org-babel-next-src-block)
		    (org-babel-execute-src-block)))))

(ert-deftest ob-vala/comdline-args ()
  "Pass commandline arguments."
  (should (equal '(("foo") ("bar"))
		 (org-test-with-temp-text
		  "
#+begin_src vala :cmdline foo bar
  class Demo.HelloWorld : GLib.Object {
      public static int main(string[] args) {
          // skip args[0], it is the binary name
          for (int i=1; i < args.length; i++) {
              stdout.printf(\"%s\n\" , args[i]);
          }
          return 0;
      }
  }
#+end_src"
		  (org-babel-next-src-block)
		  (org-babel-execute-src-block)))))


;;; test-ob-vala.el ends here
