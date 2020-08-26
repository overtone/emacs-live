;;; test-org-protocol.el --- tests for org-protocol.el                  -*- lexical-binding: t; -*-

;; Copyright (c)  Sacha Chua
;; Authors: Sacha Chua

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

(require 'cl-lib)

(unless (featurep 'org-protocol)
  (signal 'missing-test-dependency "Support for org-protocol"))

(ert-deftest test-org-protocol/org-protocol-parse-parameters ()
  "Test `org-protocol-parse-parameters' specifications."
  ;; Ignore lists
  (let ((data (org-protocol-parse-parameters '(:url "abc" :title "def") nil)))
    (should (string= (plist-get data :url) "abc"))
    (should (string= (plist-get data :title) "def")))
  ;; Parse new-style links
  (let ((data (org-protocol-parse-parameters "url=abc&title=def" t)))
    (should (string= (plist-get data :url) "abc"))
    (should (string= (plist-get data :title) "def")))
  ;; Parse new-style complex links
  (let* ((url (concat "template=p&"
		      "url=https%3A%2F%2Forgmode.org%2Forg.html%23capture-protocol&"
		      "title=The%20Org%20Manual&"
		      "body=9.4.2%20capture%20protocol"))
	 (data (org-protocol-parse-parameters url)))
    (should (string= (plist-get data :template) "p"))
    (should (string= (plist-get data :url) "https://orgmode.org/org.html#capture-protocol"))
    (should (string= (plist-get data :title) "The Org Manual"))
    (should (string= (plist-get data :body) "9.4.2 capture protocol")))
  ;; Parse old-style links
  (let ((data (org-protocol-parse-parameters "abc/def" nil '(:url :title))))
    (should (string= (plist-get data :url) "abc"))
    (should (string= (plist-get data :title) "def")))
  ;; Parse old-style links even without keys
  (let ((data (org-protocol-parse-parameters "b/abc/def" nil)))
    (should (equal data '("b" "abc" "def"))))
  ;; Parse old-style links with key/val pairs
  (let ((data (org-protocol-parse-parameters "b/abc/extrakey/extraval" nil '(:param1 :param2))))
    (should (string= (plist-get data :param1) "b"))
    (should (string= (plist-get data :param2) "abc"))
    (should (string= (plist-get data :extrakey) "extraval"))))

(ert-deftest test-org-protocol/org-protocol-store-link ()
  "Test `org-protocol-store-link' specifications."
  ;; Old link style
  (let ((uri "/some/directory/org-protocol:/store-link:/URL/TITLE"))
    (should (null (org-protocol-check-filename-for-protocol uri (list uri) nil)))
    (should (equal (car org-stored-links) '("URL" "TITLE"))))
  ;; URL encoded
  (let ((uri (format "/some/directory/org-protocol:/store-link:/%s/TITLE"
		     (url-hexify-string "http://example.com"))))
    (should (null (org-protocol-check-filename-for-protocol uri (list uri) nil)))
    (should (equal (car org-stored-links) '("http://example.com" "TITLE"))))
  ;; Handle multiple slashes, old link style
  (let ((uri "/some/directory/org-protocol://store-link://URL2//TITLE2"))
    (should (null (org-protocol-check-filename-for-protocol uri (list uri) nil)))
    (should (equal (car org-stored-links) '("URL2" "TITLE2"))))
  ;; New link style
  (let ((uri "/some/directory/org-protocol://store-link?url=URL3&title=TITLE3"))
    (should (null (org-protocol-check-filename-for-protocol uri (list uri) nil)))
    (should (equal (car org-stored-links) '("URL3" "TITLE3")))))

(ert-deftest test-org-protocol/org-protocol-capture ()
  "Test `org-protocol-capture' specifications."
  (let* ((org-protocol-default-template-key "t")
	 (temp-file-name (make-temp-file "org-protocol-test"))
	 (org-capture-templates
	  `(("t" "Test" entry (file ,temp-file-name) "** TODO\n\n%i\n\n%a\n" :kill-buffer t)
	    ("x" "With params" entry (file ,temp-file-name) "** SOMEDAY\n\n%i\n\n%a\n" :kill-buffer t)
	    ("X" "Just the template" entry (file ,temp-file-name) "** Hello World\n\n%i\n\nGoodbye World\n" :kill-buffer t)))
	 (test-urls
	  '(
	    ;; Old style:
	    ;; - multiple slashes
	    ("/some/directory/org-protocol:/capture:/URL/TITLE"
	     . "** TODO\n\n\n\n[[URL][TITLE]]\n")
	    ;; - body specification
	    ("/some/directory/org-protocol:/capture:/URL/TITLE/BODY"
	     . "** TODO\n\nBODY\n\n[[URL][TITLE]]\n")
	    ;; - template
	    ("/some/directory/org-protocol:/capture:/x/URL/TITLE/BODY"
	     . "** SOMEDAY\n\nBODY\n\n[[URL][TITLE]]\n")
	    ;; - query parameters, not sure how to include them in template
	    ("/some/directory/org-protocol:/capture:/x/URL/TITLE/BODY/from/example"
	     . "** SOMEDAY\n\nBODY\n\n[[URL][TITLE]]\n")
	    ;; New style:
	    ;; - multiple slashes
	    ("/some/directory/org-protocol:/capture?url=NEWURL&title=TITLE"
	     . "** TODO\n\n\n\n[[NEWURL][TITLE]]\n")
	    ;; - body specification
	    ("/some/directory/org-protocol:/capture?url=NEWURL&title=TITLE&body=BODY"
	     . "** TODO\n\nBODY\n\n[[NEWURL][TITLE]]\n")
	    ;; - template
	    ("/some/directory/org-protocol:/capture?template=x&url=NEWURL&title=TITLE&body=BODY"
	     . "** SOMEDAY\n\nBODY\n\n[[NEWURL][TITLE]]\n")
	    ;; - no url specified
	    ("/some/directory/org-protocol:/capture?template=x&title=TITLE&body=BODY"
	    . "** SOMEDAY\n\nBODY\n\nTITLE\n")
	    ;; - no title specified
	    ("/some/directory/org-protocol:/capture?template=x&url=NEWURL&body=BODY"
	     . "** SOMEDAY\n\nBODY\n\n[[NEWURL][NEWURL]]\n")
	    ;; - just the template
	    ("/some/directory/org-protocol:/capture?template=X"
	     . "** Hello World\n\n\n\nGoodbye World\n")
	    ;; - query parameters, not sure how to include them in template
	    ("/some/directory/org-protocol:/capture?template=x&url=URL&title=TITLE&body=BODY&from=example"
	     . "** SOMEDAY\n\nBODY\n\n[[URL][TITLE]]\n")
	    )))
    ;; Old link style
    (mapc
     (lambda (test-case)
       (let ((uri (car test-case)))
	 (org-protocol-check-filename-for-protocol uri (list uri) nil)
	 (should (string= (buffer-string) (cdr test-case)))
	 (org-capture-kill)))
     test-urls)
    (delete-file temp-file-name)))

(ert-deftest test-org-protocol/org-protocol-open-source ()
  "Test org-protocol://open-source links."
  (let* ((temp-file-name1 (make-temp-file "org-protocol-test1"))
	 (temp-file-name2 (make-temp-file "org-protocol-test2"))
	 (org-protocol-project-alist
	  `((test1
	     :base-url "http://example.com/"
	     :online-suffix ".html"
	     :working-directory ,(file-name-directory temp-file-name1))
	    (test2
	     :base-url "http://another.example.com/"
	     :online-suffix ".js"
	     :working-directory ,(file-name-directory temp-file-name2))
	    (test3
	     :base-url "https://blog-example.com/"
	     :working-directory ,(file-name-directory temp-file-name2)
	     :online-suffix ".html"
	     :working-suffix ".md"
	     :rewrites (("\\(https://blog-example.com/[0-9]+/[0-9]+/[0-9]+/\\)" . ".md")))))
	 (test-cases
	  (list
	   ;; Old-style URLs
	   (cons
	    (concat "/some/directory/org-protocol:/open-source:/"
		    (url-hexify-string
		     (concat "http://example.com/" (file-name-nondirectory temp-file-name1) ".html")))
	    temp-file-name1)
	   (cons
	    (concat "/some/directory/org-protocol:/open-source:/"
		    (url-hexify-string
		     (concat "http://another.example.com/" (file-name-nondirectory temp-file-name2) ".js")))
	    temp-file-name2)
	   ;; New-style URLs
	   (cons
	    (concat "/some/directory/org-protocol:/open-source?url="
		    (url-hexify-string
		     (concat "http://example.com/" (file-name-nondirectory temp-file-name1) ".html")))
	    temp-file-name1)
	   (cons
	    (concat "/some/directory/org-protocol:/open-source?url="
		    (url-hexify-string
		     (concat "http://another.example.com/" (file-name-nondirectory temp-file-name2) ".js")))
	    temp-file-name2))))
    (mapc (lambda (test-case)
	    (should (string=
		     (org-protocol-check-filename-for-protocol
		      (car test-case)
		      (list (car test-case)) nil)
		     (cdr test-case))))
	  test-cases)
    (delete-file temp-file-name1)
    (delete-file temp-file-name2)))

(defun test-org-protocol/org-protocol-greedy-handler (fname)
  ;; fname should be a list of parsed items
  (should (listp fname))
  nil)

(ert-deftest test-org-protocol/org-protocol-with-greedy-handler ()
  "Check that greedy handlers are called with all the filenames."
  (let ((org-protocol-protocol-alist
	 '(("protocol-a" :protocol "greedy" :function test-org-protocol/org-protocol-greedy-handler :kill-client t :greedy t))))
    ;; Neither of these should signal errors
    (let ((uri "/some/dir/org-protocol://greedy?a=b&c=d")
	  (uri2 "/some/dir/org-protocol://greedy?e=f&g=h"))
      (org-protocol-check-filename-for-protocol uri (list uri uri2) nil))))


;; TODO: Verify greedy protocol handling
;;; test-org-protocol.el ends here
