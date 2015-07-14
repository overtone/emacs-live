;;; org-footnote.el --- Footnote support in Org and elsewhere
;;
;; Copyright (C) 2009-2015 Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains the code dealing with footnotes in Org-mode.
;; The code can also be used in arbitrary text modes to provide
;; footnotes.  Compared to Steven L Baur's footnote.el it provides
;; better support for resuming editing.  It is less configurable than
;; Steve's code, though.

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'org-macs)
(require 'org-compat)

(declare-function message-point-in-header-p "message" ())
(declare-function org-at-comment-p "org" ())
(declare-function org-at-heading-p "org" (&optional ignored))
(declare-function org-back-over-empty-lines "org" ())
(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-combine-plists "org" (&rest plists))
(declare-function org-edit-footnote-reference "org-src" ())
(declare-function org-element-context "org-element" (&optional element))
(declare-function org-element-property "org-element" (property element))
(declare-function org-element-type "org-element" (element))
(declare-function org-end-of-subtree "org"  (&optional invisible-ok to-heading))
(declare-function org-fill-paragraph "org" (&optional justify))
(declare-function org-icompleting-read "org" (&rest args))
(declare-function org-id-uuid "org-id" ())
(declare-function org-in-block-p "org" (names))
(declare-function org-in-regexp "org" (re &optional nlines visually))
(declare-function org-in-verbatim-emphasis "org" ())
(declare-function org-inside-LaTeX-fragment-p "org" ())
(declare-function org-inside-latex-macro-p "org" ())
(declare-function org-mark-ring-push "org" (&optional pos buffer))
(declare-function org-show-context "org" (&optional key))
(declare-function org-skip-whitespace "org" ())
(declare-function org-skip-whitespace "org" ())
(declare-function org-trim "org" (s))
(declare-function outline-next-heading "outline")

(defvar message-cite-prefix-regexp)	; defined in message.el
(defvar message-signature-separator)	; defined in message.el
(defvar org-bracket-link-regexp)	; defined in org.el
(defvar org-complex-heading-regexp)	; defined in org.el
(defvar org-element-all-elements)	; defined in org-element.el
(defvar org-element-all-objects)	; defined in org-element.el
(defvar org-odd-levels-only)		; defined in org.el
(defvar org-outline-regexp-bol)		; defined in org.el

(defconst org-footnote-re
  ;; Only [1]-like footnotes are closed in this regexp, as footnotes
  ;; from other types might contain square brackets (i.e. links) in
  ;; their definition.
  ;;
  ;; `org-re' is used for regexp compatibility with XEmacs.
  (concat "\\[\\(?:"
	  ;; Match inline footnotes.
	  (org-re "fn:\\([-_[:word:]]+\\)?:\\|")
	  ;; Match other footnotes.
	  "\\(?:\\([0-9]+\\)\\]\\)\\|"
	  (org-re "\\(fn:[-_[:word:]]+\\)")
	  "\\)")
  "Regular expression for matching footnotes.")

(defconst org-footnote-definition-re
  (org-re "^\\[\\([0-9]+\\|fn:[-_[:word:]]+\\)\\]")
  "Regular expression matching the definition of a footnote.")

(defconst org-footnote-forbidden-blocks
  '("ascii" "beamer" "comment" "example" "html" "latex" "odt" "src")
  "Names of blocks where footnotes are not allowed.")

(defgroup org-footnote nil
  "Footnotes in Org-mode."
  :tag "Org Footnote"
  :group 'org)

(defcustom org-footnote-section "Footnotes"
  "Outline heading containing footnote definitions.

This can be nil, to place footnotes locally at the end of the
current outline node.  If can also be the name of a special
outline heading under which footnotes should be put.

This variable defines the place where Org puts the definition
automatically, i.e. when creating the footnote, and when sorting
the notes.  However, by hand you may place definitions
*anywhere*.

If this is a string, during export, all subtrees starting with
this heading will be ignored.

If you don't use the customize interface to change this variable,
you will need to run the following command after the change:

  \\[universal-argument] \\[org-element-cache-reset]"
  :group 'org-footnote
  :initialize 'custom-initialize-default
  :set (lambda (var val)
	 (set var val)
	 (when (fboundp 'org-element-cache-reset)
	   (org-element-cache-reset 'all)))
  :type '(choice
	  (string :tag "Collect footnotes under heading")
	  (const :tag "Define footnotes locally" nil)))

(defcustom org-footnote-tag-for-non-org-mode-files "Footnotes:"
  "Tag marking the beginning of footnote section.
The Org footnote engine can be used in arbitrary text files as well
as in Org-mode.  Outside Org mode, new footnotes are always placed at
the end of the file.  When you normalize the notes, any line containing
only this tag will be removed, a new one will be inserted at the end
of the file, followed by the collected and normalized footnotes.

If you don't want any tag in such buffers, set this variable to nil."
  :group 'org-footnote
  :type '(choice
	  (string :tag "Collect footnotes under tag")
	  (const :tag "Don't use a tag" nil)))

(defcustom org-footnote-define-inline nil
  "Non-nil means define footnotes inline, at reference location.
When nil, footnotes will be defined in a special section near
the end of the document.  When t, the [fn:label:definition] notation
will be used to define the footnote at the reference position."
  :group 'org-footnote
  :type 'boolean)

(defcustom org-footnote-auto-label t
  "Non-nil means define automatically new labels for footnotes.
Possible values are:

nil        Prompt the user for each label.
t          Create unique labels of the form [fn:1], [fn:2], etc.
confirm    Like t, but let the user edit the created value.
           The label can be removed from the minibuffer to create
           an anonymous footnote.
random	   Automatically generate a unique, random label.
plain      Automatically create plain number labels like [1]."
  :group 'org-footnote
  :type '(choice
	  (const :tag "Prompt for label" nil)
	  (const :tag "Create automatic [fn:N]" t)
	  (const :tag "Offer automatic [fn:N] for editing" confirm)
	  (const :tag "Create a random label" random)
	  (const :tag "Create automatic [N]" plain)))

(defcustom org-footnote-auto-adjust nil
  "Non-nil means automatically adjust footnotes after insert/delete.
When this is t, after each insertion or deletion of a footnote,
simple fn:N footnotes will be renumbered, and all footnotes will be sorted.
If you want to have just sorting or just renumbering, set this variable
to `sort' or `renumber'.

The main values of this variable can be set with in-buffer options:

#+STARTUP: fnadjust
#+STARTUP: nofnadjust"
  :group 'org-footnote
  :type '(choice
	  (const :tag "No adjustment" nil)
	  (const :tag "Renumber" renumber)
	  (const :tag "Sort" sort)
	  (const :tag "Renumber and Sort" t)))

(defcustom org-footnote-fill-after-inline-note-extraction nil
  "Non-nil means fill paragraphs after extracting footnotes.
When extracting inline footnotes, the lengths of lines can change a lot.
When this option is set, paragraphs from which an inline footnote has been
extracted will be filled again."
  :group 'org-footnote
  :type 'boolean)

(defun org-footnote-in-valid-context-p ()
  "Is point in a context where footnotes are allowed?"
  (save-match-data
    (not (or (org-at-comment-p)
	     (org-inside-LaTeX-fragment-p)
	     ;; Avoid literal example.
	     (org-in-verbatim-emphasis)
	     (save-excursion
	       (beginning-of-line)
	       (looking-at "[ \t]*:[ \t]+"))
	     ;; Avoid cited text and headers in message-mode.
	     (and (derived-mode-p 'message-mode)
		  (or (save-excursion
			(beginning-of-line)
			(looking-at message-cite-prefix-regexp))
		      (message-point-in-header-p)))
	     ;; Avoid forbidden blocks.
	     (org-in-block-p org-footnote-forbidden-blocks)))))

(defun org-footnote-at-reference-p ()
  "Is the cursor at a footnote reference?

If so, return a list containing its label, beginning and ending
positions, and the definition, when inlined."
  (when (and (org-footnote-in-valid-context-p)
	     (or (looking-at org-footnote-re)
		 (org-in-regexp org-footnote-re)
		 (save-excursion (re-search-backward org-footnote-re nil t)))
	     (/= (match-beginning 0) (point-at-bol)))
    (let* ((beg (match-beginning 0))
	   (label (or (org-match-string-no-properties 2)
		      (org-match-string-no-properties 3)
		      ;; Anonymous footnotes don't have labels
		      (and (match-string 1)
			   (concat "fn:" (org-match-string-no-properties 1)))))
	   ;; Inline footnotes don't end at (match-end 0) as
	   ;; `org-footnote-re' stops just after the second colon.
	   ;; Find the real ending with `scan-sexps', so Org doesn't
	   ;; get fooled by unrelated closing square brackets.
	   (end (ignore-errors (scan-sexps beg 1))))
      ;; Point is really at a reference if it's located before true
      ;; ending of the footnote.
      (when (and end (< (point) end)
		 ;; Verify match isn't a part of a link.
		 (not (save-excursion
			(goto-char beg)
			(let ((linkp
			       (save-match-data
				 (org-in-regexp org-bracket-link-regexp))))
			  (and linkp (< (point) (cdr linkp))))))
		 ;; Verify point doesn't belong to a LaTeX macro.
		 (not (org-inside-latex-macro-p)))
	(list label beg end
	      ;; Definition: ensure this is an inline footnote first.
	      (and (or (not label) (match-string 1))
		   (org-trim (buffer-substring-no-properties
			      (match-end 0) (1- end)))))))))

(defun org-footnote-at-definition-p ()
  "Is point within a footnote definition?

This matches only pure definitions like [1] or [fn:name] at the
beginning of a line.  It does not match references like
\[fn:name:definition], where the footnote text is included and
defined locally.

The return value will be nil if not at a footnote definition, and
a list with label, start, end and definition of the footnote
otherwise."
  (when (save-excursion (beginning-of-line) (org-footnote-in-valid-context-p))
    (save-excursion
      (end-of-line)
      ;; Footnotes definitions are separated by new headlines, another
      ;; footnote definition or 2 blank lines.
      (let ((lim (save-excursion
		   (re-search-backward
		    (concat org-outline-regexp-bol
			    "\\|^\\([ \t]*\n\\)\\{2,\\}") nil t))))
	(when (re-search-backward org-footnote-definition-re lim t)
	  (let ((label (org-match-string-no-properties 1))
		(beg (match-beginning 0))
		(beg-def (match-end 0))
		;; In message-mode, do not search after signature.
		(end (let ((bound (and (derived-mode-p 'message-mode)
				       (save-excursion
					 (goto-char (point-max))
					 (re-search-backward
					  message-signature-separator nil t)))))
		       (if (progn
			     (end-of-line)
			     (re-search-forward
			      (concat org-outline-regexp-bol "\\|"
				      org-footnote-definition-re "\\|"
				      "^\\([ \t]*\n\\)\\{2,\\}") bound 'move))
			   (match-beginning 0)
			 (point)))))
	    (list label beg end
		  (org-trim (buffer-substring-no-properties beg-def end)))))))))

(defun org-footnote-get-next-reference (&optional label backward limit)
  "Return complete reference of the next footnote.

If LABEL is provided, get the next reference of that footnote.  If
BACKWARD is non-nil, find previous reference instead.  LIMIT is
the buffer position bounding the search.

Return value is a list like those provided by `org-footnote-at-reference-p'.
If no footnote is found, return nil."
  (save-excursion
    (let* ((label-fmt (if label (format "\\[%s[]:]" label) org-footnote-re)))
      (catch 'exit
	(while t
	  (unless (funcall (if backward #'re-search-backward #'re-search-forward)
			   label-fmt limit t)
	    (throw 'exit nil))
	  (unless backward (backward-char))
	  (let ((ref (org-footnote-at-reference-p)))
	    (when ref (throw 'exit ref))))))))

(defun org-footnote-next-reference-or-definition (limit)
  "Move point to next footnote reference or definition.

LIMIT is the buffer position bounding the search.

Return value is a list like those provided by
`org-footnote-at-reference-p' or `org-footnote-at-definition-p'.
If no footnote is found, return nil."
  (let* (ref (origin (point)))
    (catch 'exit
      (while t
	(unless (re-search-forward org-footnote-re limit t)
	  (goto-char origin)
	  (throw 'exit nil))
	;; Beware: with [1]-like footnotes point will be just after
	;; the closing square bracket.
	(backward-char)
	(cond
	 ((setq ref (org-footnote-at-reference-p))
	  (throw 'exit ref))
	 ;; Definition: also grab the last square bracket, only
	 ;; matched in `org-footnote-re' for [1]-like footnotes.
	 ((save-match-data (org-footnote-at-definition-p))
	  (let ((end (match-end 0)))
	    (throw 'exit
		   (list nil (match-beginning 0)
			 (if (eq (char-before end) 93) end (1+ end)))))))))))

(defun org-footnote-get-definition (label)
  "Return label, boundaries and definition of the footnote LABEL."
  (let* ((label (regexp-quote (org-footnote-normalize-label label)))
	 (re (format "^\\[%s\\]\\|.\\[%s:" label label)))
    (org-with-wide-buffer
     (goto-char (point-min))
     (catch 'found
       (while (re-search-forward re nil t)
	 (let* ((datum (progn (backward-char) (org-element-context)))
		(type (org-element-type datum)))
	   (when (memq type '(footnote-definition footnote-reference))
	     (throw 'found
		    (list
		     label
		     (org-element-property :begin datum)
		     (org-element-property :end datum)
		     (let ((cbeg (org-element-property :contents-begin datum)))
		       (if (not cbeg) ""
			 (replace-regexp-in-string
			  "[ \t\n]*\\'"
			  ""
			  (buffer-substring-no-properties
			   cbeg
			   (org-element-property :contents-end datum))))))))))
       nil))))

(defun org-footnote-goto-definition (label &optional location)
  "Move point to the definition of the footnote LABEL.

LOCATION, when non-nil specifies the buffer position of the
definition.

Throw an error if there is no definition or if it cannot be
reached from current narrowed part of buffer.  Return a non-nil
value if point was successfully moved."
  (interactive "sLabel: ")
  (let ((def-start (or location (nth 1 (org-footnote-get-definition label)))))
    (cond
     ((not def-start)
      (user-error "Cannot find definition of footnote %s" label))
     ((or (> def-start (point-max)) (< def-start (point-min)))
      (user-error "Definition is outside narrowed part of buffer")))
    (org-mark-ring-push)
    (goto-char def-start)
    (looking-at (format "\\[%s[]:]" label))
    (goto-char (match-end 0))
    (org-show-context 'link-search)
    (when (derived-mode-p 'org-mode)
      (message
       (substitute-command-keys
	"Edit definition and go back with `\\[org-mark-ring-goto]' or, if \
unique, with `\\[org-ctrl-c-ctrl-c]'.")))
    t))

(defun org-footnote-goto-previous-reference (label)
  "Find the first closest (to point) reference of footnote with label LABEL."
  (interactive "sLabel: ")
  (org-mark-ring-push)
  (let* ((label (org-footnote-normalize-label label)) ref)
    (save-excursion
      (setq ref (or (org-footnote-get-next-reference label t)
		    (org-footnote-get-next-reference label)
		    (save-restriction
		      (widen)
		      (or
		       (org-footnote-get-next-reference label t)
		       (org-footnote-get-next-reference label))))))
    (if (not ref)
	(error "Cannot find reference of footnote %s" label)
      (goto-char (nth 1 ref))
      (org-show-context 'link-search))))

(defun org-footnote-normalize-label (label)
  "Return LABEL as an appropriate string."
  (cond
   ((numberp label) (number-to-string label))
   ((equal "" label) nil)
   ((not (string-match "^[0-9]+$\\|^fn:" label))
    (concat "fn:" label))
   (t label)))

(defun org-footnote-all-labels (&optional with-defs)
  "Return list with all defined foot labels used in the buffer.

If WITH-DEFS is non-nil, also associate the definition to each
label.  The function will then return an alist whose key is label
and value definition."
  (let* (rtn
	 (push-to-rtn
	  (function
	   ;; Depending on WITH-DEFS, store label or (label . def) of
	   ;; footnote reference/definition given as argument in RTN.
	   (lambda (el)
	     (let ((lbl (car el)))
	       (push (if with-defs (cons lbl (nth 3 el)) lbl) rtn))))))
    (save-excursion
      (save-restriction
	(widen)
	;; Find all labels found in definitions.
	(goto-char (point-min))
	(let (def)
	  (while (re-search-forward org-footnote-definition-re nil t)
	    (when (setq def (org-footnote-at-definition-p))
	      (funcall push-to-rtn def))))
	;; Find all labels found in references.
	(goto-char (point-min))
	(let (ref)
	  (while (setq ref (org-footnote-get-next-reference))
	    (goto-char (nth 2 ref))
	    (and (car ref)		; ignore anonymous footnotes
		 (not (funcall (if with-defs #'assoc #'member) (car ref) rtn))
		 (funcall push-to-rtn ref))))))
    rtn))

(defun org-footnote-unique-label (&optional current)
  "Return a new unique footnote label.

The function returns the first \"fn:N\" or \"N\" label that is
currently not used.

Optional argument CURRENT is the list of labels active in the
buffer."
  (unless current (setq current (org-footnote-all-labels)))
  (let ((fmt (if (eq org-footnote-auto-label 'plain) "%d" "fn:%d"))
	(cnt 1))
    (while (member (format fmt cnt) current)
      (incf cnt))
    (format fmt cnt)))

(defun org-footnote--allow-reference-p ()
  "Non-nil when a footnote reference can be inserted at point."
  ;; XXX: This is similar to `org-footnote-in-valid-context-p' but
  ;; more accurate and usually faster, except in some corner cases.
  ;; It may replace it after doing proper benchmarks as it would be
  ;; used in fontification.
  (unless (bolp)
    (let* ((context (org-element-context))
	   (type (org-element-type context)))
      (cond
       ;; No footnote reference in attributes.
       ((let ((post (org-element-property :post-affiliated context)))
	  (and post (< (point) post)))
	nil)
       ;; Paragraphs and blank lines at top of document are fine.
       ((memq type '(nil paragraph)))
       ;; So are contents of verse blocks.
       ((eq type 'verse-block)
	(and (>= (point) (org-element-property :contents-begin context))
	     (< (point) (org-element-property :contents-end context))))
       ;; In an headline or inlinetask, point must be either on the
       ;; heading itself or on the blank lines below.
       ((memq type '(headline inlinetask))
	(or (not (org-at-heading-p))
	    (and (save-excursion (beginning-of-line)
				 (and (let ((case-fold-search t))
					(not (looking-at "\\*+ END[ \t]*$")))
				      (looking-at org-complex-heading-regexp)))
		 (match-beginning 4)
		 (>= (point) (match-beginning 4))
		 (or (not (match-beginning 5))
		     (< (point) (match-beginning 5))))))
       ;; White spaces after an object or blank lines after an element
       ;; are OK.
       ((>= (point)
	    (save-excursion (goto-char (org-element-property :end context))
			    (skip-chars-backward " \r\t\n")
			    (if (memq type org-element-all-objects) (point)
			      (1+ (line-beginning-position 2))))))
       ;; Other elements are invalid.
       ((memq type org-element-all-elements) nil)
       ;; Just before object is fine.
       ((= (point) (org-element-property :begin context)))
       ;; Within recursive object too, but not in a link.
       ((eq type 'link) nil)
       ((let ((cbeg (org-element-property :contents-begin context))
	      (cend (org-element-property :contents-end context)))
	  (and cbeg (>= (point) cbeg) (<= (point) cend))))))))

(defun org-footnote-new ()
  "Insert a new footnote.
This command prompts for a label.  If this is a label referencing an
existing label, only insert the label.  If the footnote label is empty
or new, let the user edit the definition of the footnote."
  (interactive)
  (unless (org-footnote--allow-reference-p)
    (user-error "Cannot insert a footnote here"))
  (let* ((all (org-footnote-all-labels))
	 (label
	  (org-footnote-normalize-label
	   (if (eq org-footnote-auto-label 'random)
	       (format "fn:%x" (random #x100000000))
	     (let ((propose (org-footnote-unique-label all)))
	       (if (memq org-footnote-auto-label '(t plain)) propose
		 (org-icompleting-read
		  "Label (leave empty for anonymous): "
		  (mapcar #'list all) nil nil
		  (and (eq org-footnote-auto-label 'confirm) propose))))))))
    (cond ((not label)
	   (insert "[fn::]")
	   (backward-char 1))
	  ((member label all)
	   (insert "[" label "]")
	   (message "New reference to existing note"))
	  (org-footnote-define-inline
	   (insert "[" label ":]")
	   (backward-char 1)
	   (org-footnote-auto-adjust-maybe))
	  (t
	   (insert "[" label "]")
	   (let ((l (copy-marker (org-footnote-create-definition label))))
	     (org-footnote-auto-adjust-maybe)
	     (or (ignore-errors (org-footnote-goto-definition label l))
		 ;; Since definition was created outside current
		 ;; scope, edit it remotely.
		 (progn (set-marker l nil)
			(org-edit-footnote-reference))))))))

(defvar org-blank-before-new-entry) ; Silence byte-compiler.
(defun org-footnote-create-definition (label)
  "Start the definition of a footnote with label LABEL.
Return buffer position at the beginning of the definition.  In an
Org buffer, this function doesn't move point."
  (let ((label (org-footnote-normalize-label label))
	electric-indent-mode)		; Prevent wrong indentation.
    (cond
     ;; In an Org document.
     ((derived-mode-p 'org-mode)
      ;; If `org-footnote-section' is defined, find it, or create it
      ;; at the end of the buffer.
      (org-with-wide-buffer
       (cond
	((not org-footnote-section)
	 (org-footnote--goto-local-insertion-point))
	((save-excursion
	   (goto-char (point-min))
	   (re-search-forward
	    (concat "^\\*+[ \t]+" (regexp-quote org-footnote-section) "[ \t]*$")
	    nil t))
	 (goto-char (match-end 0))
	 (forward-line)
	 (unless (bolp) (insert "\n")))
	(t
	 (goto-char (point-max))
	 (unless (bolp) (insert "\n"))
	 ;; Insert new section.  Separate it from the previous one
	 ;; with a blank line, unless `org-blank-before-new-entry'
	 ;; explicitly says no.
	 (when (and (cdr (assq 'heading org-blank-before-new-entry))
		    (zerop (save-excursion (org-back-over-empty-lines))))
	   (insert "\n"))
	 (insert "* " org-footnote-section "\n")))
       (when (zerop (org-back-over-empty-lines)) (insert "\n"))
       (insert "[" label "] \n")
       (line-beginning-position 0)))
     (t
      ;; In a non-Org file.  Search for footnote tag, or create it if
      ;; specified (at the end of buffer, or before signature if in
      ;; Message mode).  Set point after any definition already there.
      (let ((tag (and org-footnote-tag-for-non-org-mode-files
		      (concat "^" (regexp-quote
				   org-footnote-tag-for-non-org-mode-files)
			      "[ \t]*$")))
	    (max (if (and (derived-mode-p 'message-mode)
			  (goto-char (point-max))
			  (re-search-backward
			   message-signature-separator nil t))
		     (progn
		       ;; Ensure one blank line separates last
		       ;; footnote from signature.
		       (beginning-of-line)
		       (open-line 2)
		       (point-marker))
		   (point-max-marker))))
	(set-marker-insertion-type max t)
	(goto-char max)
	;; Check if the footnote tag is defined but missing.  In this
	;; case, insert it, before any footnote or one blank line
	;; after any previous text.
	(when (and tag (not (re-search-backward tag nil t)))
	  (skip-chars-backward " \t\r\n")
	  (while (re-search-backward org-footnote-definition-re nil t))
	  (unless (bolp) (newline 2))
	  (insert org-footnote-tag-for-non-org-mode-files "\n\n"))
	;; Remove superfluous white space and clear marker.
	(goto-char max)
	(skip-chars-backward " \t\r\n")
	(delete-region (point) max)
	(unless (bolp) (newline))
	(set-marker max nil))
      (when (zerop (org-back-over-empty-lines)) (insert "\n"))
      (insert "[" label "] \n")
      (backward-char)
      (line-beginning-position)))))

;;;###autoload
(defun org-footnote-action (&optional special)
  "Do the right thing for footnotes.

When at a footnote reference, jump to the definition.

When at a definition, jump to the references if they exist, offer
to create them otherwise.

When neither at definition or reference, create a new footnote,
interactively if possible.

With prefix arg SPECIAL, or when no footnote can be created,
offer additional commands in a menu."
  (interactive "P")
  (let* ((context (and (not special) (org-element-context)))
	 (type (org-element-type context)))
    (cond
     ((eq type 'footnote-reference)
      (let ((label (org-element-property :label context)))
	(cond
	 ;; Anonymous footnote: move point at the beginning of its
	 ;; definition.
	 ((not label)
	  (goto-char (org-element-property :contents-begin context)))
	 ;; Check if a definition exists: then move to it.
	 ((let ((p (nth 1 (org-footnote-get-definition label))))
	    (when p (org-footnote-goto-definition label p))))
	 ;; No definition exists: offer to create it.
	 ((yes-or-no-p (format "No definition for %s.  Create one? " label))
	  (let ((p (org-footnote-create-definition label)))
	    (or (ignore-errors (org-footnote-goto-definition label p))
		;; Since definition was created outside current scope,
		;; edit it remotely.
		(org-edit-footnote-reference)))))))
     ((eq type 'footnote-definition)
      (org-footnote-goto-previous-reference
       (org-element-property :label context)))
     ((or special (not (org-footnote--allow-reference-p)))
      (message "Footnotes: [s]ort  |  [r]enumber fn:N  |  [S]=r+s  |  \
->[n]umeric  |  [d]elete")
      (let ((c (read-char-exclusive)))
	(cond
	 ((eq c ?s) (org-footnote-normalize 'sort))
	 ((eq c ?r) (org-footnote-renumber-fn:N))
	 ((eq c ?S)
	  (org-footnote-renumber-fn:N)
	  (org-footnote-normalize 'sort))
	 ((eq c ?n) (org-footnote-normalize))
	 ((eq c ?d) (org-footnote-delete))
	 (t (error "No such footnote command %c" c)))))
     (t (org-footnote-new)))))

;;;###autoload
(defun org-footnote-normalize (&optional sort-only)
  "Collect the footnotes in various formats and normalize them.

This finds the different sorts of footnotes allowed in Org, and
normalizes them to the usual [N] format.

When SORT-ONLY is set, only sort the footnote definitions into the
referenced sequence."
  ;; This is based on Paul's function, but rewritten.
  ;;
  ;; Re-create `org-with-limited-levels', but not limited to Org
  ;; buffers.
  (let* ((limit-level
	  (and (boundp 'org-inlinetask-min-level)
	       org-inlinetask-min-level
	       (1- org-inlinetask-min-level)))
	 (nstars (and limit-level
		      (if org-odd-levels-only (1- (* limit-level 2))
			limit-level)))
	 (org-outline-regexp
	  (concat "\\*" (if nstars (format "\\{1,%d\\} " nstars) "+ ")))
	 (count 0)
	 ins-point ref ref-table)
    (org-with-wide-buffer
     ;; 1. Find every footnote reference, extract the definition, and
     ;;    collect that data in REF-TABLE.  If SORT-ONLY is nil, also
     ;;    normalize references.
     (goto-char (point-min))
     (while (setq ref (org-footnote-get-next-reference))
       (let* ((lbl (car ref))
	      (pos (nth 1 ref))
	      ;; When footnote isn't anonymous, check if it's label
	      ;; (REF) is already stored in REF-TABLE.  In that case,
	      ;; extract number used to identify it (MARKER).  If
	      ;; footnote is unknown, increment the global counter
	      ;; (COUNT) to create an unused identifier.
	      (a (and lbl (assoc lbl ref-table)))
	      (marker (or (nth 1 a) (incf count)))
	      ;; Is the reference inline or pointing to an inline
	      ;; footnote?
	      (inlinep (or (stringp (nth 3 ref)) (nth 3 a))))
	 ;; Replace footnote reference with [MARKER].  Maybe fill
	 ;; paragraph once done.  If SORT-ONLY is non-nil, only move
	 ;; to the end of reference found to avoid matching it twice.
	 (if sort-only (goto-char (nth 2 ref))
	   (delete-region (nth 1 ref) (nth 2 ref))
	   (goto-char (nth 1 ref))
	   (insert (format "[%d]" marker))
	   (and inlinep
		org-footnote-fill-after-inline-note-extraction
		(org-fill-paragraph)))
	 ;; Add label (REF), identifier (MARKER), definition (DEF)
	 ;; type (INLINEP) and position (POS) to REF-TABLE if data was
	 ;; unknown.
	 (unless a
	   (let ((def (or (nth 3 ref)	; Inline definition.
			  (nth 3 (org-footnote-get-definition lbl)))))
	     (push (list lbl marker def
			 ;; Reference beginning position is a marker
			 ;; to preserve it during further buffer
			 ;; modifications.
			 inlinep (copy-marker pos)) ref-table)))))
     ;; 2. Find and remove the footnote section, if any.  Also
     ;;    determine where footnotes shall be inserted (INS-POINT).
     (cond
      ((and org-footnote-section (derived-mode-p 'org-mode))
       (goto-char (point-min))
       (if (re-search-forward
	    (concat "^\\*[ \t]+" (regexp-quote org-footnote-section)
		    "[ \t]*$") nil t)
	   (delete-region (match-beginning 0) (org-end-of-subtree t t)))
       ;; A new footnote section is inserted by default at the end of
       ;; the buffer.
       (goto-char (point-max))
       (skip-chars-backward " \r\t\n")
       (forward-line)
       (unless (bolp) (newline)))
      ;; No footnote section set: Footnotes will be added at the end
      ;; of the section containing their first reference.
      ((derived-mode-p 'org-mode))
      (t
       ;; Remove any left-over tag in the buffer, if one is set up.
       (when org-footnote-tag-for-non-org-mode-files
	 (let ((tag (concat "^" (regexp-quote
				 org-footnote-tag-for-non-org-mode-files)
			    "[ \t]*$")))
	   (goto-char (point-min))
	   (while (re-search-forward tag nil t)
	     (replace-match "")
	     (delete-region (point) (progn (forward-line) (point))))))
       ;; In Message mode, ensure footnotes are inserted before the
       ;; signature.
       (if (and (derived-mode-p 'message-mode)
		(goto-char (point-max))
		(re-search-backward message-signature-separator nil t))
	   (beginning-of-line)
	 (goto-char (point-max)))))
     (setq ins-point (point-marker))
     ;; 3. Clean-up REF-TABLE.
     (setq ref-table
	   (delq nil
		 (mapcar
		  (lambda (x)
		    (cond
		     ;; When only sorting, ignore inline footnotes.
		     ;; Also clear position marker.
		     ((and sort-only (nth 3 x))
		      (set-marker (nth 4 x) nil) nil)
		     ;; No definition available: provide one.
		     ((not (nth 2 x))
		      (append
		       (list (car x) (nth 1 x)
			     (format "DEFINITION NOT FOUND: %s" (car x)))
		       (nthcdr 3 x)))
		     (t x)))
		  ref-table)))
     (setq ref-table (nreverse ref-table))
     ;; 4. Remove left-over definitions in the buffer.
     (dolist (x ref-table)
       (unless (nth 3 x) (org-footnote-delete-definitions (car x))))
     ;; 5. Insert the footnotes again in the buffer, at the
     ;;    appropriate spot.
     (goto-char ins-point)
     (cond
      ;; No footnote: exit.
      ((not ref-table))
      ;; Cases when footnotes should be inserted in one place.
      ((or (not (derived-mode-p 'org-mode)) org-footnote-section)
       ;; Insert again the section title, if any.  Ensure that title,
       ;; or the subsequent footnotes, will be separated by a blank
       ;; lines from the rest of the document.  In an Org buffer,
       ;; separate section with a blank line, unless explicitly stated
       ;; in `org-blank-before-new-entry'.
       (if (not (derived-mode-p 'org-mode))
	   (progn (skip-chars-backward " \t\n\r")
		  (delete-region (point) ins-point)
		  (unless (bolp) (newline))
		  (when org-footnote-tag-for-non-org-mode-files
		    (insert "\n" org-footnote-tag-for-non-org-mode-files "\n")))
	 (when (and (cdr (assq 'heading org-blank-before-new-entry))
		    (zerop (save-excursion (org-back-over-empty-lines))))
	   (insert "\n"))
	 (insert "* " org-footnote-section "\n"))
       (set-marker ins-point nil)
       ;; Insert the footnotes, separated by a blank line.
       (insert
	(mapconcat
	 (lambda (x)
	   ;; Clean markers.
	   (set-marker (nth 4 x) nil)
	   (format "\n[%s] %s" (nth (if sort-only 0 1) x) (nth 2 x)))
	 ref-table "\n"))
       (unless (eobp) (insert "\n\n")))
      ;; Each footnote definition has to be inserted at the end of the
      ;; section where its first reference belongs.
      (t
       (dolist (x ref-table)
	 (let ((pos (nth 4 x)))
	   (goto-char pos)
	   ;; Clean marker.
	   (set-marker pos nil))
	 (org-footnote--goto-local-insertion-point)
	 (insert (format "\n[%s] %s\n"
			 (nth (if sort-only 0 1) x)
			 (nth 2 x)))))))))

(defun org-footnote--goto-local-insertion-point ()
  "Find insertion point for footnote, just before next outline heading.
Assume insertion point is within currently accessible part of the buffer."
  (org-with-limited-levels (outline-next-heading))
  ;; Skip file local variables.  See `modify-file-local-variable'.
  (when (eobp)
    (let ((case-fold-search t))
      (re-search-backward "^[ \t]*# +Local Variables:"
			  (max (- (point-max) 3000) (point-min))
			  t)))
  (skip-chars-backward " \t\n")
  (forward-line)
  (unless (bolp) (insert "\n")))

(defun org-footnote-delete-references (label)
  "Delete every reference to footnote LABEL.
Return the number of footnotes removed."
  (save-excursion
    (goto-char (point-min))
    (let (ref (nref 0))
      (while (setq ref (org-footnote-get-next-reference label))
	(goto-char (nth 1 ref))
	(delete-region (nth 1 ref) (nth 2 ref))
	(incf nref))
      nref)))

(defun org-footnote-delete-definitions (label)
  "Delete every definition of the footnote LABEL.
Return the number of footnotes removed."
  (save-excursion
    (goto-char (point-min))
    (let ((def-re (concat "^\\[" (regexp-quote label) "\\]"))
	  (ndef 0))
      (while (re-search-forward def-re nil t)
	(let ((full-def (org-footnote-at-definition-p)))
	  (when full-def
	    ;; Remove the footnote, and all blank lines before it.
	    (goto-char (nth 1 full-def))
	    (skip-chars-backward " \r\t\n")
	    (unless (bolp) (forward-line))
	    (delete-region (point) (nth 2 full-def))
	    (incf ndef))))
      ndef)))

(defun org-footnote-delete (&optional label)
  "Delete the footnote at point.
This will remove the definition (even multiple definitions if they exist)
and all references of a footnote label.

If LABEL is non-nil, delete that footnote instead."
  (catch 'done
    (let* ((nref 0) (ndef 0) x
	   ;; 1. Determine LABEL of footnote at point.
	   (label (cond
		   ;; LABEL is provided as argument.
		   (label)
		   ;; Footnote reference at point.  If the footnote is
		   ;; anonymous, delete it and exit instead.
		   ((setq x (org-footnote-at-reference-p))
		    (or (car x)
			(progn
			  (delete-region (nth 1 x) (nth 2 x))
			  (message "Anonymous footnote removed")
			  (throw 'done t))))
		   ;; Footnote definition at point.
		   ((setq x (org-footnote-at-definition-p))
		    (car x))
		   (t (error "Don't know which footnote to remove")))))
      ;; 2. Now that LABEL is non-nil, find every reference and every
      ;; definition, and delete them.
      (setq nref (org-footnote-delete-references label)
	    ndef (org-footnote-delete-definitions label))
      ;; 3. Verify consistency of footnotes and notify user.
      (org-footnote-auto-adjust-maybe)
      (message "%d definition(s) of and %d reference(s) of footnote %s removed"
	       ndef nref label))))

(defun org-footnote-renumber-fn:N ()
  "Renumber the simple footnotes like fn:17 into a sequence in the document."
  (interactive)
  (let (map (n 0))
    (org-with-wide-buffer
     (goto-char (point-min))
     (while (re-search-forward "\\[fn:\\([0-9]+\\)[]:]" nil t)
       (save-excursion
	 (goto-char (match-beginning 0))
	 ;; Ensure match is a footnote reference or definition.
	 (when (save-match-data (if (bolp)
				    (org-footnote-at-definition-p)
				  (org-footnote-at-reference-p)))
	   (let ((new-val (or (cdr (assoc (match-string 1) map))
			      (number-to-string (incf n)))))
	     (unless (assoc (match-string 1) map)
	       (push (cons (match-string 1) new-val) map))
	     (replace-match new-val nil nil nil 1))))))))

(defun org-footnote-auto-adjust-maybe ()
  "Renumber and/or sort footnotes according to user settings."
  (when (memq org-footnote-auto-adjust '(t renumber))
    (org-footnote-renumber-fn:N))
  (when (memq org-footnote-auto-adjust '(t sort))
    (let ((label (car (org-footnote-at-definition-p))))
      (org-footnote-normalize 'sort)
      (when label
	(goto-char (point-min))
	(and (re-search-forward (concat "^\\[" (regexp-quote label) "\\]")
				nil t)
	     (progn (insert " ")
		    (just-one-space)))))))

(provide 'org-footnote)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-footnote.el ends here
