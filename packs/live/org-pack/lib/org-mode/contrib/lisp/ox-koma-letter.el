;;; ox-koma-letter.el --- KOMA Scrlttr2 Back-End for Org Export Engine

;; Copyright (C) 2007-2012  Free Software Foundation, Inc.

;; Author: Nicolas Goaziou <n.goaziou AT gmail DOT com>
;;         Alan Schmitt <alan.schmitt AT polytechnique DOT org>
;; Keywords: org, wp, tex

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This library implements a KOMA Scrlttr2 back-end, derived from the
;; LaTeX one.
;;
;; Depending on the desired output format, three commands are provided
;; for export: `org-koma-letter-export-as-latex' (temporary buffer),
;; `org-koma-letter-export-to-latex' ("tex" file) and
;; `org-koma-letter-export-to-pdf' ("pdf" file).
;;
;; On top of buffer keywords supported by `latex' back-end (see
;; `org-latex-options-alist'), this back-end introduces the following
;; keywords: "CLOSING" (see `org-koma-letter-closing'), "FROM_ADDRESS"
;; (see `org-koma-letter-from-address'), "LCO" (see
;; `org-koma-letter-class-option-file'), "OPENING" (see
;; `org-koma-letter-opening'), "PHONE_NUMBER" (see
;; `org-koma-letter-phone-number'), "SIGNATURE" (see
;; `org-koma-letter-signature') and "TO_ADDRESS".
;;
;; You will need to add an appropriate association in
;; `org-latex-classes' in order to use the KOMA Scrlttr2 class.  For
;; example, you can use the following code:
;;
;;   (add-to-list 'org-latex-classes
;;                '("my-letter"
;;                  "\\documentclass\[%
;;   DIV=14,
;;   fontsize=12pt,
;;   parskip=half,
;;   subject=titled,
;;   backaddress=false,
;;   fromalign=left,
;;   fromemail=true,
;;   fromphone=true\]\{scrlttr2\}
;;   \[DEFAULT-PACKAGES]
;;   \[PACKAGES]
;;   \[EXTRA]"))
;;
;; Then, in your Org document, be sure to require the proper class
;; with :
;;
;;    #+LATEX_CLASS: my-letter


;;; Code:

(require 'ox-latex)


;;; User-Configurable Variables

(defgroup org-export-koma-letter nil
  "Options for exporting to KOMA scrlttr2 class in LaTeX export."
  :tag "Org Koma-Letter"
  :group 'org-export)

(defcustom org-koma-letter-class-option-file "NF"
  "Letter Class Option File."
  :group 'org-export-koma-letter
  :type 'string)

(defcustom org-koma-letter-closing "See you soon,"
  "Koma-Letter's closing, as a string."
  :group 'org-export-koma-letter
  :type 'string)

(defcustom org-koma-letter-from-address "Somewhere \\ Over the rainbow."
  "Sender's address, as a string."
  :group 'org-export-koma-letter
  :type 'string)

(defcustom org-koma-letter-opening "Dear Sir,"
  "Letter's opening, as a string."
  :group 'org-export-koma-letter
  :type 'string)

(defcustom org-koma-letter-phone-number "00-00-00-00"
  "Sender's phone number, as a string."
  :group 'org-export-koma-letter
  :type 'string)

(defcustom org-koma-letter-signature "\\usekomavar{fromname}"
  "String used as the signature."
  :group 'org-export-koma-letter
  :type 'string)


;;; Define Back-End

(org-export-define-derived-backend 'koma-letter 'latex
  :options-alist
  '((:closing "CLOSING" nil org-koma-letter-closing)
    (:from-address "FROM_ADDRESS" nil org-koma-letter-from-address newline)
    (:lco "LCO" nil org-koma-letter-class-option-file)
    (:opening "OPENING" nil org-koma-letter-opening)
    (:phone-number "PHONE_NUMBER" nil org-koma-letter-phone-number)
    (:signature "SIGNATURE" nil nil newline)
    (:to-address "TO_ADDRESS" nil nil newline))
  :translate-alist '((export-block . org-koma-letter-export-block)
		     (export-snippet . org-koma-letter-export-snippet)
		     (keyword . org-koma-letter-keyword)
		     (template . org-koma-letter-template))
  :menu-entry
  '(?k "Export with KOMA Scrlttr2"
       ((?K "As LaTeX buffer" org-koma-letter-export-as-latex)
	(?k "As LaTeX file" org-koma-letter-export-to-latex)
	(?p "As PDF file" org-koma-letter-export-to-pdf)
	(?O "As PDF file and open"
	    (lambda (a s v b)
	      (if a (org-koma-letter-export-to-pdf t s v b)
		(org-open-file (org-koma-letter-export-to-pdf nil s v b))))))))


;;; Transcode Functions

;;;; Export Block

(defun org-koma-letter-export-block (export-block contents info)
  "Transcode an EXPORT-BLOCK element into KOMA Scrlttr2 code.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (when (member (org-element-property :type export-block) '("KOMA-LETTER" "LATEX"))
    (org-remove-indentation (org-element-property :value export-block))))

;;;; Export Snippet

(defun org-koma-letter-export-snippet (export-snippet contents info)
  "Transcode an EXPORT-SNIPPET object into KOMA Scrlttr2 code.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (when (memq (org-export-snippet-backend export-snippet) '(latex koma-letter))
    (org-element-property :value export-snippet)))

;;;; Keyword

(defun org-koma-letter-keyword (keyword contents info)
  "Transcode a KEYWORD element into KOMA Scrlttr2 code.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((key (org-element-property :key keyword))
        (value (org-element-property :value keyword)))
    ;; Handle specifically BEAMER and TOC (headlines only) keywords.
    ;; Otherwise, fallback to `latex' back-end.
    (if (equal key "KOMA-LETTER") value
      (org-export-with-backend 'latex keyword contents info))))

;;;; Template

(defun org-koma-letter-template (contents info)
  "Return complete document string after KOMA Scrlttr2 conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   ;; Time-stamp.
   (and (plist-get info :time-stamp-file)
        (format-time-string "%% Created %Y-%m-%d %a %H:%M\n"))
   ;; Document class and packages.
   (let ((class (plist-get info :latex-class))
         (class-options (plist-get info :latex-class-options)))
     (org-element-normalize-string
      (let* ((header (nth 1 (assoc class org-latex-classes)))
             (document-class-string
              (and (stringp header)
                   (if (not class-options) header
		     (replace-regexp-in-string
		      "^[ \t]*\\\\documentclass\\(\\(\\[[^]]*\\]\\)?\\)"
		      class-options header t nil 1)))))
        (if (not document-class-string)
	    (user-error "Unknown LaTeX class `%s'")
          (org-latex-guess-babel-language
           (org-latex-guess-inputenc
            (org-splice-latex-header
             document-class-string
             org-latex-default-packages-alist ; defined in org.el
             org-latex-packages-alist nil     ; defined in org.el
	     (concat (plist-get info :latex-header)
		     (plist-get info :latex-header-extra))))
           info)))))
   ;; Define "From" data.
   (format "\\setkomavar{fromname}{%s}\n"
           (org-export-data (plist-get info :author) info))
   (format "\\setkomavar{fromaddress}{%s}\n" (plist-get info :from-address))
   (format "\\setkomavar{signature}{%s}\n" (plist-get info :signature))
   (format "\\setkomavar{fromemail}{%s}\n"
           (org-export-data (plist-get info :email) info))
   (format "\\setkomavar{fromphone}{%s}\n" (plist-get info :phone-number))
   ;; Date.
   (format "\\date{%s}\n" (org-export-data (org-export-get-date info) info))
   ;; Letter Class Option File
   (format "\\LoadLetterOption{%s}\n" (plist-get info :lco))
   ;; Letter start.
   "\\begin{document}\n\n"
   (format "\\setkomavar{subject}{%s}\n\n"
           (org-export-data (plist-get info :title) info))
   (format "\\begin{letter}{%%\n%s}\n\n"
	   (or (plist-get info :to-address) "no address given"))
   ;; Opening.
   (format "\\opening{%s}\n\n" (plist-get info :opening))
   ;; Letter body.
   contents
   ;; Closing.
   (format "\n\\closing{%s}\n\n" (plist-get info :closing))
   ;; Letter end.
   "\\end{letter}\n\\end{document}"))



;;; Commands

;;;###autoload
(defun org-koma-letter-export-as-latex
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a KOMA Scrlttr2 letter.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{letter}\" and \"\\end{letter}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org KOMA-LETTER Export*\".  It
will be displayed if `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (if async
      (org-export-async-start
	  (lambda (output)
	    (with-current-buffer (get-buffer-create "*Org KOMA-LETTER Export*")
	      (erase-buffer)
	      (insert output)
	      (goto-char (point-min))
	      (LaTeX-mode)
	      (org-export-add-to-stack (current-buffer) 'koma-letter)))
	`(org-export-as 'koma-letter ,subtreep ,visible-only ,body-only
			',ext-plist))
    (let ((outbuf (org-export-to-buffer
		   'koma-letter "*Org KOMA-LETTER Export*"
		   subtreep visible-only body-only ext-plist)))
      (with-current-buffer outbuf (LaTeX-mode))
      (when org-export-show-temporary-export-buffer
	(switch-to-buffer-other-window outbuf)))))

;;;###autoload
(defun org-koma-letter-export-to-latex
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a KOMA Scrlttr2 letter (tex).

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{letter}\" and \"\\end{letter}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

When optional argument PUB-DIR is set, use it as the publishing
directory.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".tex" subtreep)))
    (if async
	(org-export-async-start
	    (lambda (f) (org-export-add-to-stack f 'koma-letter))
	  `(expand-file-name
	    (org-export-to-file
	     'koma-letter ,outfile ,subtreep ,visible-only ,body-only
	     ',ext-plist)))
      (org-export-to-file
       'koma-letter outfile subtreep visible-only body-only ext-plist))))

;;;###autoload
(defun org-koma-letter-export-to-pdf
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a KOMA Scrlttr2 letter (pdf).

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{letter}\" and \"\\end{letter}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return PDF file's name."
  (interactive)
  (if async
      (let ((outfile (org-export-output-file-name ".tex" subtreep)))
	(org-export-async-start
	    (lambda (f) (org-export-add-to-stack f 'koma-letter))
	  `(expand-file-name
	    (org-latex-compile
	     (org-export-to-file
	      'koma-letter ,outfile ,subtreep ,visible-only ,body-only
	      ',ext-plist)))))
    (org-latex-compile
     (org-koma-letter-export-to-latex
      nil subtreep visible-only body-only ext-plist))))


(provide 'ox-koma-letter)
;;; ox-koma-letter.el ends here
