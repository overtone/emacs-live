;;; refheap.el --- A library for pasting to https://refheap.com
;; Copyright 2012-2014 Anthony Grimes
;; Author: Anthony Grimes
;; URL: https://github.com/Raynes/refheap.el
;; Version: 0.0.7
;; Package-Requires: ((json "1.2"))

(require 'json)
(require 'url)

(defgroup refheap nil
  "A library for pasting to refheap.com"
  :prefix "refheap-"
  :group 'applications)

(defvar refheap-supported-modes '((nrepl-clojure-mode . "Clojure")
                                  (markdown-mode . "Markdown")
                                  (clojure-mode . "Clojure")
                                  (clojurescript-mode . "Clojure")
                                  (diff-mode . "Diff")
                                  (fancy-mode . "Fancy")
                                  (groovy-mode . "Groovy")
                                  (grails-mode . "Groovy")
                                  (io-mode . "Io")
                                  (ioke-mode . "Ioke")
                                  (lua-mode . "Lua")
                                  (perl-mode . "Perl")
                                  (python-mode . "Python")
                                  (ruby-mode . "Ruby")
                                  (c-mode . "C")
                                  (c++-mode . "C++")
                                  (tcl-mode . "Tcl")
                                  (ada-mode . "Ada")
                                  (d-mode . "D")
                                  (delphi-mode . "Delphi")
                                  (dylan-mode . "Dylan")
                                  (fortran-mode . "Fortran")
                                  (go-mode . "Go")
                                  (java-mode . "Java")
                                  (nimrod-mode . "Nimrod")
                                  (objc-mode . "Objective C")
                                  (ooc-mode . "ooc")
                                  (prolog-mode . "Prolog")
                                  (scala-mode . "Scala")
                                  (vala-mode . "Vala")
                                  (boo-mode . "Boo")
                                  (csharp-mode . "C#")
                                  (fsharp-mode . "F#")
                                  (vbnet-mode . "VB.NET")
                                  (common-lisp-mode . "Common Lisp")
                                  (erlang . "Erlang")
                                  (haskell-mode . "Haskell")
                                  (tuareg-mode . "OCaml")
                                  (scheme-mode . "Scheme")
                                  (emacs-lisp-mode . "Emacs Lisp")
                                  (r-mode . "R")
                                  (applescript-mode "AppleScript")
                                  (sh-mode . "Bash")
                                  (batch-mode . "Batch")
                                  (newspeak-mode . "NewSpeak")
                                  (protobuf-mode . "Protobuf")
                                  (rebol-mode . "REBOL")
                                  (latex-mode . "LaTeX")
                                  (actionscript-mode . "ActionScript")
                                  (yaml-mode . "YAML")
                                  (coffeescript-mode . "CoffeeScript")
                                  (css-mode . "CSS")
                                  (html-mode . "HTML")
                                  (haml-mode . "HAML")
                                  (js-mode . "Javascript")
                                  (php-mode . "PHP")
                                  (text-mode . "Plain Text")
                                  (sass-mode . "SASS")
                                  (xml-mode . "XML")))

(defcustom refheap-base-url "https://www.refheap.com"
  "Your RefHeap URL."
  :type 'string
  :group 'refheap)

(defcustom refheap-user nil
  "Your RefHeap username."
  :type 'string
  :group 'refheap)

(defcustom refheap-token nil
  "Your RefHeap API token."
  :type 'string
  :group 'refheap)

(defun read-refheap-url (status)
  "Get the url from the refheap server response."
  (search-forward "\n\n")
  (let ((location (cdr (assoc 'url
                       (json-read-from-string
                        (buffer-substring (point)
                                          (point-max)))))))
    (message "Paste created: %s" location)
    (kill-new location)
    (kill-buffer (current-buffer))))

(defun refheap-paste (text mode private)
  "Send text to the refheap server, with highlighting mode, optionally private."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data
         (concat "private=" (if private "true" "false") "&"
                 "language=" (url-hexify-string mode) "&"
                 "contents=" (url-hexify-string text)
                 (when (and refheap-user refheap-token)
                   (concat "&username=" refheap-user "&"
                           "token=" (url-hexify-string refheap-token))))))
    (url-retrieve (format "%s/api/paste" refheap-base-url) 'read-refheap-url)))

;;;###autoload
(defun refheap-paste-region (begin end &optional private)
  "Paste the current region to refheap. With prefix arg, paste privately."
  (interactive "r\nP")
  (let ((hl (or (cdr (assoc major-mode refheap-supported-modes))
                "Plain Text")))
    (refheap-paste (buffer-substring begin end) hl private)))

;;;###autoload
(defun refheap-paste-region-private (begin end)
  "Paste the current region to a private refheap entry."
  (interactive "r")
  (refheap-paste-region begin end t))

;;;###autoload
(defun refheap-paste-buffer (&optional private)
  "Paste the current buffer to refheap. With prefix arg, paste privately."
  (interactive "P")
  (refheap-paste-region (point-min) (point-max) private))

;;;###autoload
(defun refheap-paste-buffer-private ()
  "Paste the current buffer to a private refheap entry."
  (interactive)
  (refheap-paste-buffer t))

;;;###autoload
(defun refheap-paste-region-or-buffer (&optional private)
  "Paste the current region (or buffer, if no region is active) to refheap.
With prefix arg, paste privately."
  (interactive "P")
  (if (use-region-p)
      (refheap-paste-region (region-beginning) (region-end) private)
    (refheap-paste-buffer private)))

(provide 'refheap)
;;; refheap.el ends here
