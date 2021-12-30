;;; test-ob-plantuml.el --- tests for ob-plantuml.el

;; Copyright (c) 2016, 2019 Thibault Marin
;; Authors: Thibault Marin

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
(unless (featurep 'ob-plantuml)
  (signal 'missing-test-dependency "Support for PlantUML code blocks"))

(ert-deftest test-ob-plantuml/single-var ()
  "Test file output with input variable."
  (should
   (string=
    "@startuml
!define CLASSNAME test_class
class CLASSNAME
@enduml"
    (let ((org-plantuml-jar-path nil))
      (org-test-with-temp-text
	  "#+name: variable_value
: test_class

#+header: :file tmp.puml
#+header: :var CLASSNAME=variable_value
#+begin_src plantuml
class CLASSNAME
#+end_src"
        (org-babel-next-src-block)
	(let ((src-block-info (cdr (org-babel-get-src-block-info))))
	  (org-babel-plantuml-make-body
	   (car src-block-info)
	   (car (cdr src-block-info)))))))))


(ert-deftest test-ob-plantuml/prologue ()
  "Test file output with prologue."
  (should
   (string=
    "@startuml
skinparam classBackgroundColor #FF0000
class test_class
@enduml"
    (let ((org-plantuml-jar-path nil))
      (org-test-with-temp-text
	  "#+header: :file tmp.puml
#+header: :prologue skinparam classBackgroundColor #FF0000
#+begin_src plantuml
class test_class
#+end_src"
        (org-babel-next-src-block)
	(let ((src-block-info (cdr (org-babel-get-src-block-info))))
	  (org-babel-plantuml-make-body
	   (car src-block-info)
	   (car (cdr src-block-info)))))))))

(provide 'test-ob-plantuml)

;;; test-ob-plantuml.el ends here
