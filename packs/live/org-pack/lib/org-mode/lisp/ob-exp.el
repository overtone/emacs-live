;;; ob-exp.el --- Exportation of org-babel source blocks

;; Copyright (C) 2009-2013 Free Software Foundation, Inc.

;; Authors: Eric Schulte
;;	Dan Davison
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:
(require 'ob-core)
(eval-when-compile
  (require 'cl))

(defvar org-current-export-file)
(defvar org-babel-lob-one-liner-regexp)
(defvar org-babel-ref-split-regexp)
(defvar org-list-forbidden-blocks)

(declare-function org-babel-lob-get-info "ob-lob" ())
(declare-function org-babel-eval-wipe-error-buffer "ob-eval" ())
(declare-function org-between-regexps-p "org"
		  (start-re end-re &optional lim-up lim-down))
(declare-function org-get-indentation "org" (&optional line))
(declare-function org-heading-components "org" ())
(declare-function org-in-block-p "org" (names))
(declare-function org-in-verbatim-emphasis "org" ())
(declare-function org-link-search "org" (s &optional type avoid-pos stealth))
(declare-function org-fill-template "org" (template alist))
(declare-function org-split-string "org" (string &optional separators))
(declare-function org-element-at-point "org-element" (&optional keep-trail))
(declare-function org-element-context "org-element" ())
(declare-function org-element-property "org-element" (property element))
(declare-function org-element-type "org-element" (element))
(declare-function org-escape-code-in-string "org-src" (s))

(defcustom org-export-babel-evaluate t
  "Switch controlling code evaluation during export.
When set to nil no code will be evaluated as part of the export
process.  When set to 'inline-only, only inline code blocks will
be executed."
  :group 'org-babel
  :version "24.1"
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Only inline code" inline-only)
		 (const :tag "Always" t)))
(put 'org-export-babel-evaluate 'safe-local-variable (lambda (x) (eq x nil)))

(defun org-babel-exp-get-export-buffer ()
  "Return the current export buffer if possible."
  (cond
   ((bufferp org-current-export-file) org-current-export-file)
   (org-current-export-file (get-file-buffer org-current-export-file))
   ('otherwise
    (error "Requested export buffer when `org-current-export-file' is nil"))))

(defmacro org-babel-exp-in-export-file (lang &rest body)
  (declare (indent 1))
  `(let* ((lang-headers (intern (concat "org-babel-default-header-args:" ,lang)))
	  (heading (nth 4 (ignore-errors (org-heading-components))))
	  (export-buffer (current-buffer))
	  (original-buffer (org-babel-exp-get-export-buffer)) results)
     (when original-buffer
       ;; resolve parameters in the original file so that
       ;; headline and file-wide parameters are included, attempt
       ;; to go to the same heading in the original file
       (set-buffer original-buffer)
       (save-restriction
	 (when heading
	   (condition-case nil
	       (let ((org-link-search-inhibit-query t))
		 (org-link-search heading))
	     (error (when heading
		      (goto-char (point-min))
		      (re-search-forward (regexp-quote heading) nil t)))))
	 (setq results ,@body))
       (set-buffer export-buffer)
       results)))
(def-edebug-spec org-babel-exp-in-export-file (form body))

(defun org-babel-exp-src-block (&rest headers)
  "Process source block for export.
Depending on the 'export' headers argument, replace the source
code block like this:

both ---- display the code and the results

code ---- the default, display the code inside the block but do
          not process

results - just like none only the block is run on export ensuring
          that it's results are present in the org-mode buffer

none ---- do not display either code or results upon export

Assume point is at the beginning of block's starting line."
  (interactive)
  (unless noninteractive (message "org-babel-exp processing..."))
  (save-excursion
    (let* ((info (org-babel-get-src-block-info 'light))
	   (lang (nth 0 info))
	   (raw-params (nth 2 info)) hash)
      ;; bail if we couldn't get any info from the block
      (when info
	;; if we're actually going to need the parameters
	(when (member (cdr (assoc :exports (nth 2 info))) '("both" "results"))
	  (org-babel-exp-in-export-file lang
	    (setf (nth 2 info)
		  (org-babel-process-params
		   (org-babel-merge-params
		    org-babel-default-header-args
		    (org-babel-params-from-properties lang)
		    (if (boundp lang-headers) (eval lang-headers) nil)
		    raw-params))))
	  (setf hash (org-babel-sha1-hash info)))
	(org-babel-exp-do-export info 'block hash)))))

(defcustom org-babel-exp-call-line-template
  ""
  "Template used to export call lines.
This template may be customized to include the call line name
with any export markup.  The template is filled out using
`org-fill-template', and the following %keys may be used.

 line --- call line

An example value would be \"\\n: call: %line\" to export the call line
wrapped in a verbatim environment.

Note: the results are inserted separately after the contents of
this template."
  :group 'org-babel
  :type 'string)

(defvar org-babel-default-lob-header-args)
(defun org-babel-exp-non-block-elements (start end)
  "Process inline source and call lines between START and END for export."
  (interactive)
  (save-excursion
    (goto-char start)
    (unless (markerp end)
      (let ((m (make-marker)))
	(set-marker m end (current-buffer))
	(setq end m)))
    (let ((rx (concat "\\(?:"  org-babel-inline-src-block-regexp
		      "\\|" org-babel-lob-one-liner-regexp "\\)")))
      (while (re-search-forward rx end t)
	(save-excursion
	  (let* ((element (save-excursion
			    ;; If match is inline, point is at its
			    ;; end.  Move backward so
			    ;; `org-element-context' can get the
			    ;; object, not the following one.
			    (backward-char)
			    (save-match-data (org-element-context))))
		 (type (org-element-type element)))
	    (when (memq type '(babel-call inline-babel-call inline-src-block))
	      (let ((beg-el (org-element-property :begin element))
		    (end-el (org-element-property :end element)))
		(case type
		  (inline-src-block
		   (let* ((info (org-babel-parse-inline-src-block-match))
			  (params (nth 2 info)))
		     (setf (nth 1 info)
			   (if (and (cdr (assoc :noweb params))
				    (string= "yes" (cdr (assoc :noweb params))))
			       (org-babel-expand-noweb-references
				info (org-babel-exp-get-export-buffer))
			     (nth 1 info)))
		     (goto-char beg-el)
		     (let ((replacement (org-babel-exp-do-export info 'inline)))
		       (if (equal replacement "")
			   ;; Replacement code is empty: completely
			   ;; remove inline src block, including extra
			   ;; white space that might have been created
			   ;; when inserting results.
			   (delete-region beg-el
					  (progn (goto-char end-el)
						 (skip-chars-forward " \t")
						 (point)))
			 ;; Otherwise: remove inline src block but
			 ;; preserve following white spaces.  Then
			 ;; insert value.
			 (delete-region beg-el
					(progn (goto-char end-el)
					       (skip-chars-backward " \t")
					       (point)))
			 (insert replacement)))))
		  ((babel-call inline-babel-call)
		   (let* ((lob-info (org-babel-lob-get-info))
			  (results
			   (org-babel-exp-do-export
			    (list "emacs-lisp" "results"
				  (org-babel-merge-params
				   org-babel-default-header-args
				   org-babel-default-lob-header-args
				   (org-babel-params-from-properties)
				   (org-babel-parse-header-arguments
				    (org-no-properties
				     (concat ":var results="
					     (mapconcat 'identity
							(butlast lob-info)
							" ")))))
				  "" nil (car (last lob-info)))
			    'lob))
			  (rep (org-fill-template
				org-babel-exp-call-line-template
				`(("line"  . ,(nth 0 lob-info))))))
		     ;; If replacement is empty, completely remove the
		     ;; object/element, including any extra white space
		     ;; that might have been created when including
		     ;; results.
		     (if (equal rep "")
			 (delete-region
			  beg-el
			  (progn (goto-char end-el)
				 (if (not (eq type 'babel-call))
				     (progn (skip-chars-forward " \t") (point))
				   (skip-chars-forward " \r\t\n")
				   (line-beginning-position))))
		       ;; Otherwise, preserve following white
		       ;; spaces/newlines and then, insert replacement
		       ;; string.
		       (goto-char beg-el)
		       (delete-region beg-el
				      (progn (goto-char end-el)
					     (skip-chars-backward " \r\t\n")
					     (point)))
		       (insert rep)))))))))))))

(defvar org-src-preserve-indentation)	; From org-src.el
(defun org-babel-exp-process-buffer ()
  "Execute all blocks in visible part of buffer."
  (interactive)
  (save-window-excursion
    (let ((case-fold-search t)
	  (pos (point-min)))
      (goto-char pos)
      (while (re-search-forward "^[ \t]*#\\+BEGIN_SRC" nil t)
        (let ((element (save-match-data (org-element-at-point))))
          (when (eq (org-element-type element) 'src-block)
            (let* ((match-start (copy-marker (match-beginning 0)))
                   (begin (copy-marker (org-element-property :begin element)))
                   ;; Make sure we don't remove any blank lines after
                   ;; the block when replacing it.
                   (block-end (save-excursion
				(goto-char (org-element-property :end element))
				(skip-chars-backward " \r\t\n")
				(copy-marker (line-end-position))))
                   (ind (org-get-indentation))
                   (headers
		    (cons
		     (org-element-property :language element)
		     (let ((params (org-element-property :parameters element)))
		       (and params (org-split-string params "[ \t]+")))))
                   (preserve-indent
		    (or org-src-preserve-indentation
			(org-element-property :preserve-indent element))))
              ;; Execute all non-block elements between POS and
              ;; current block.
              (org-babel-exp-non-block-elements pos begin)
	      ;; Take care of matched block: compute replacement
	      ;; string. In particular, a nil REPLACEMENT means the
	      ;; block should be left as-is while an empty string
	      ;; should remove the block.
              (let ((replacement (progn (goto-char match-start)
					(org-babel-exp-src-block headers))))
                (cond ((not replacement) (goto-char block-end))
		      ((equal replacement "")
		       (delete-region begin
				      (progn (goto-char block-end)
					     (skip-chars-forward " \r\t\n")
					     (if (eobp) (point)
					       (line-beginning-position)))))
		      (t
		       (goto-char match-start)
		       (delete-region (point) block-end)
		       (insert replacement)
		       (if preserve-indent
			   ;; Indent only the code block markers.
			   (save-excursion (skip-chars-backward " \r\t\n")
					   (indent-line-to ind)
					   (goto-char match-start)
					   (indent-line-to ind))
			 ;; Indent everything.
			 (indent-rigidly match-start (point) ind)))))
	      (setq pos (line-beginning-position))
              ;; Cleanup markers.
	      (set-marker match-start nil)
	      (set-marker begin nil)
              (set-marker block-end nil)))))
      ;; Eventually execute all non-block Babel elements between last
      ;; src-block and end of buffer.
      (org-babel-exp-non-block-elements pos (point-max)))))

(defun org-babel-in-example-or-verbatim ()
  "Return true if point is in example or verbatim code.
Example and verbatim code include escaped portions of
an org-mode buffer code that should be treated as normal
org-mode text."
  (or (save-match-data
	(save-excursion
	  (goto-char (point-at-bol))
	  (looking-at "[ \t]*:[ \t]")))
      (org-in-verbatim-emphasis)
      (org-in-block-p org-list-forbidden-blocks)
      (org-between-regexps-p "^[ \t]*#\\+begin_src" "^[ \t]*#\\+end_src")))

(defun org-babel-exp-do-export (info type &optional hash)
  "Return a string with the exported content of a code block.
The function respects the value of the :exports header argument."
  (let ((silently (lambda () (let ((session (cdr (assoc :session (nth 2 info)))))
			       (when (not (and session (equal "none" session)))
				 (org-babel-exp-results info type 'silent)))))
	(clean (lambda () (unless (eq type 'inline) (org-babel-remove-result info)))))
    (case (intern (or (cdr (assoc :exports (nth 2 info))) "code"))
      ('none (funcall silently) (funcall clean) "")
      ('code (funcall silently) (funcall clean) (org-babel-exp-code info))
      ('results (org-babel-exp-results info type nil hash) "")
      ('both (org-babel-exp-results info type nil hash)
	     (org-babel-exp-code info)))))

(defcustom org-babel-exp-code-template
  "#+BEGIN_SRC %lang%flags\n%body\n#+END_SRC"
  "Template used to export the body of code blocks.
This template may be customized to include additional information
such as the code block name, or the values of particular header
arguments.  The template is filled out using `org-fill-template',
and the following %keys may be used.

 lang ------ the language of the code block
 name ------ the name of the code block
 body ------ the body of the code block
 flags ----- the flags passed to the code block

In addition to the keys mentioned above, every header argument
defined for the code block may be used as a key and will be
replaced with its value."
  :group 'org-babel
  :type 'string)

(defun org-babel-exp-code (info)
  "Return the original code block formatted for export."
  (setf (nth 1 info)
	(if (string= "strip-export" (cdr (assoc :noweb (nth 2 info))))
	    (replace-regexp-in-string
	     (org-babel-noweb-wrap) "" (nth 1 info))
	  (if (org-babel-noweb-p (nth 2 info) :export)
	      (org-babel-expand-noweb-references
	       info (org-babel-exp-get-export-buffer))
	    (nth 1 info))))
  (org-fill-template
   org-babel-exp-code-template
   `(("lang"  . ,(nth 0 info))
     ("body"  . ,(org-escape-code-in-string (nth 1 info)))
     ,@(mapcar (lambda (pair)
		 (cons (substring (symbol-name (car pair)) 1)
		       (format "%S" (cdr pair))))
	       (nth 2 info))
     ("flags" . ,((lambda (f) (when f (concat " " f))) (nth 3 info)))
     ("name"  . ,(or (nth 4 info) "")))))

(defun org-babel-exp-results (info type &optional silent hash)
  "Evaluate and return the results of the current code block for export.
Results are prepared in a manner suitable for export by org-mode.
This function is called by `org-babel-exp-do-export'.  The code
block will be evaluated.  Optional argument SILENT can be used to
inhibit insertion of results into the buffer."
  (when (and (or (eq org-export-babel-evaluate t)
		 (and (eq type 'inline)
		      (eq org-export-babel-evaluate 'inline-only)))
	     (not (and hash (equal hash (org-babel-current-result-hash)))))
    (let ((lang (nth 0 info))
	  (body (if (org-babel-noweb-p (nth 2 info) :eval)
		    (org-babel-expand-noweb-references
		     info (org-babel-exp-get-export-buffer))
		  (nth 1 info)))
	  (info (copy-sequence info)))
      ;; skip code blocks which we can't evaluate
      (when (fboundp (intern (concat "org-babel-execute:" lang)))
	(org-babel-eval-wipe-error-buffer)
	(prog1 nil
	  (setf (nth 1 info) body)
	  (setf (nth 2 info)
		(org-babel-exp-in-export-file lang
		  (org-babel-process-params
		   (org-babel-merge-params
		    (nth 2 info)
		    `((:results . ,(if silent "silent" "replace")))))))
	  (cond
	   ((equal type 'block)
	    (org-babel-execute-src-block nil info))
	   ((equal type 'inline)
	    ;; position the point on the inline source block allowing
	    ;; `org-babel-insert-result' to check that the block is
	    ;; inline
	    (re-search-backward "[ \f\t\n\r\v]" nil t)
	    (re-search-forward org-babel-inline-src-block-regexp nil t)
	    (re-search-backward "src_" nil t)
	    (org-babel-execute-src-block nil info))
	   ((equal type 'lob)
	    (save-excursion
	      (re-search-backward org-babel-lob-one-liner-regexp nil t)
	      (let (org-confirm-babel-evaluate)
		(org-babel-execute-src-block nil info))))))))))


(provide 'ob-exp)

;;; ob-exp.el ends here
