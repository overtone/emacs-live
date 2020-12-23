;;; test-ob-eshell.el

;; Copyright (c) 2018 stardiviner
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Comment:

;; Template test file for Org tests

;;; Code:
(unless (featurep 'ob-eshell)
  (signal 'missing-test-dependency "Support for Eshell code blocks"))

(ert-deftest ob-eshell/execute ()
  "Test ob-eshell execute."
  (should
   (string=
    (org-test-with-temp-text
	"#+begin_src eshell
echo 2
#+end_src"
      (org-babel-execute-src-block))
    ": 2")))

(ert-deftest ob-eshell/variables-assignment ()
  "Test ob-eshell variables assignment."
  (should
   (string=
    (org-test-with-temp-text
	"#+begin_src eshell :var hi=\"hello, world\"
echo $hi
#+end_src"
      (org-babel-execute-src-block))
    ": hello, world")))

(ert-deftest ob-eshell/session ()
  "Test ob-eshell session."
  (should
   (string=
    (org-test-with-temp-text
	"#+begin_src eshell :session
(setq hi \"hello, world\")
#+end_src

#+begin_src eshell :session
echo $hi
#+end_src"
      (org-babel-execute-src-block)
      (org-babel-next-src-block)
      (org-babel-execute-src-block)
      (goto-char (org-babel-where-is-src-block-result))
      (forward-line)
      (buffer-substring-no-properties (point) (line-end-position)))
    ": hello, world")))

(provide 'test-ob-eshell)

;;; test-ob-eshell.el ends here
