;;; test-ob-lob.el --- test for ob-lob.el

;; Copyright (c) 2010-2014 Eric Schulte
;; Authors: Eric Schulte

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


;;; Tests
(org-babel-lob-ingest
 (expand-file-name
  "library-of-babel.org"
  (expand-file-name
   "doc"
    (expand-file-name
     ".."
     (expand-file-name
      ".."
      (file-name-directory
       (or load-file-name buffer-file-name)))))))

(ert-deftest test-ob-lob/ingest ()
  "Test the ingestion of an org-mode file."
  (should (< 0 (org-babel-lob-ingest
		(expand-file-name "babel.org" org-test-example-dir)))))

(ert-deftest test-ob-lob/call-with-header-arguments ()
  "Test the evaluation of a library of babel #+call: line."
  (org-test-at-id "fab7e291-fde6-45fc-bf6e-a485b8bca2f0"
    (move-beginning-of-line 1)
    (forward-line 6)
    (message (buffer-substring (point-at-bol) (point-at-eol)))
    (should (string= "testing" (org-babel-lob-execute
				(org-babel-lob-get-info))))
    (forward-line 1)
    (should (string= "testing" (caar (org-babel-lob-execute
				      (org-babel-lob-get-info)))))
    (forward-line 1)
    (should (string= "testing" (org-babel-lob-execute
    				(org-babel-lob-get-info))))
    (forward-line 1)
    (should (string= "testing" (caar (org-babel-lob-execute
    				      (org-babel-lob-get-info)))))
    (forward-line 1)
    (should (string= "testing" (org-babel-lob-execute
				(org-babel-lob-get-info))))
    (forward-line 1)
    (should (string= "testing" (caar (org-babel-lob-execute
    				      (org-babel-lob-get-info)))))
    (forward-line 1) (beginning-of-line) (forward-char 27)
    (should (string= "testing" (org-babel-lob-execute
				(org-babel-lob-get-info))))
    (forward-line 1) (beginning-of-line) (forward-char 27)
    (should (string= "testing" (caar (org-babel-lob-execute
				      (org-babel-lob-get-info)))))
    (forward-line 1) (beginning-of-line)
    (should (= 4 (org-babel-lob-execute (org-babel-lob-get-info))))
    (forward-line 1)
    (should (string= "testing" (org-babel-lob-execute
				(org-babel-lob-get-info))))
    (forward-line 1)
    (should (string= "123" (org-babel-lob-execute (org-babel-lob-get-info))))))

(ert-deftest test-ob-lob/export-lob-lines ()
  "Test the export of a variety of library babel call lines."
  (org-test-at-id "72ddeed3-2d17-4c7f-8192-a575d535d3fc"
    (org-narrow-to-subtree)
    (let ((buf (current-buffer))
	  (string (buffer-string)))
      (with-temp-buffer
	(org-mode)
	(insert string)
	(let ((org-current-export-file buf))
	  (org-babel-exp-process-buffer))
	(message (buffer-string))
	(goto-char (point-min))
	(should (re-search-forward "^: 0" nil t))
	(should (re-search-forward "call =2= stuck" nil t))
	(should (re-search-forward
		 "exported =call_double(it=2)= because" nil t))
	(should (re-search-forward "^=6= because" nil t))
	(should (re-search-forward "results 8 should" nil t))
	(should (re-search-forward "following 2\\*5==10= should" nil t))))))

(ert-deftest test-ob-lob/do-not-eval-lob-lines-in-example-blocks-on-export ()
  (require 'ox)
  (org-test-with-temp-text-in-file "
for export
#+begin_example
#+call: rubbish()
#+end_example"
    (should (progn (org-export-execute-babel-code) t))))


(provide 'test-ob-lob)

;;; test-ob-lob.el ends here
