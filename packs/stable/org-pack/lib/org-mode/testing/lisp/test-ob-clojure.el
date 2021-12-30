;;; test-ob-clojure.el

;; Copyright (c) 2018-2021 Free Software Foundation, Inc.
;; Authors: stardiviner

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

;;; Comments:

;; Org tests for ob-clojure.el live here

;;; Code:
(org-test-for-executable "cider")
(unless (featurep 'cider)
  (signal 'missing-test-dependency "CIDER"))
(unless (featurep 'ob-clojure)
  (signal 'missing-test-dependency "Support for Clojure code blocks"))

(ert-deftest ob-clojure/simple-session ()
  (org-test-with-temp-text
      "#+begin_src clojure :session
(print \"hello, world\")
#+end_src
"
    (should (string= "hello, world" (org-babel-execute-src-block)))))

(ert-deftest ob-clojure/initiate-session ()
  (org-test-with-temp-text
      "#+begin_src clojure :session :var a=1 :results output
(print \"hello, world\")
#+end_src

#+begin_src clojure :session :results output
(print a)
#+end_src"
    (goto-char (point-min))
    (org-babel-switch-to-session)
    (sleep-for 2)
    (org-babel-execute-maybe)
    (org-babel-next-src-block)
    (goto-char (org-babel-result-end))
    (forward-line 2)
    (should (string=
	     ": 1"
	     (buffer-substring-no-properties (point-at-bol) (point-at-eol))))))

(ert-deftest ob-clojure/initiate-session-with-var ()
  (org-test-with-temp-text
      "#+begin_src clojure :session :var a=1 :results output
(print a)
#+end_src"
    (org-babel-next-src-block)
    (org-babel-initiate-session)
    (sleep-for 2)
    (org-babel-execute-maybe)
    (goto-char (org-babel-result-end))
    (forward-line 2)
    (should (string=
	     ": 1"
	     (buffer-substring-no-properties (point-at-bol) (point-at-eol))))))
(ert-deftest ob-clojure/tangle-without-ns ()
  (org-test-with-temp-text
   "#+begin_src clojure :tangle /tmp/test.clj
(print 1)
#+end_src"
   (org-babel-next-src-block)
   (org-babel-tangle)
   (should
    (string=
     "(print 1)
"
     (with-temp-buffer
       (insert-file-contents "/tmp/test.clj")
       (buffer-substring-no-properties (point-min) (point-max)))))))

(provide 'test-ob-clojure)

 ;;; test-ob-clojure.el ends here
