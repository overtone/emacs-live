;;; ob-core.el --- working with code blocks in org-mode

;; Copyright (C) 2009-2016 Free Software Foundation, Inc.

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
(eval-when-compile
  (require 'cl))
(require 'ob-eval)
(require 'org-macs)
(require 'org-compat)

(defconst org-babel-exeext
  (if (memq system-type '(windows-nt cygwin))
      ".exe"
    nil))

;; dynamically scoped for tramp
(defvar org-babel-call-process-region-original nil)
(defvar org-babel-library-of-babel)
(defvar org-edit-src-content-indentation)
(defvar org-src-lang-modes)

(declare-function outline-show-all "outline" ())
(declare-function org-every "org" (pred seq))
(declare-function org-get-indentation "org" (&optional line))
(declare-function org-remove-indentation "org" (code &optional n))
(declare-function org-reduce "org" (CL-FUNC CL-SEQ &rest CL-KEYS))
(declare-function org-mark-ring-push "org" (&optional pos buffer))
(declare-function tramp-compat-make-temp-file "tramp-compat"
                  (filename &optional dir-flag))
(declare-function tramp-dissect-file-name "tramp" (name &optional nodefault))
(declare-function tramp-file-name-user "tramp" (vec))
(declare-function tramp-file-name-host "tramp" (vec))
(declare-function with-parsed-tramp-file-name "tramp" (filename var &rest body))
(declare-function org-icompleting-read "org" (&rest args))
(declare-function org-edit-src-code "org-src" (&optional code edit-buffer-name))
(declare-function org-edit-src-exit "org-src"  ())
(declare-function org-open-at-point "org" (&optional in-emacs reference-buffer))
(declare-function org-save-outline-visibility "org-macs" (use-markers &rest body))
(declare-function org-outline-overlay-data "org" (&optional use-markers))
(declare-function org-set-outline-overlay-data "org" (data))
(declare-function org-narrow-to-subtree "org" ())
(declare-function org-split-string "org" (string &optional separators))
(declare-function org-entry-get "org"
		  (pom property &optional inherit literal-nil))
(declare-function org-make-options-regexp "org" (kwds &optional extra))
(declare-function org-do-remove-indentation "org" (&optional n))
(declare-function org-next-block "org" (arg &optional backward block-regexp))
(declare-function org-previous-block "org" (arg &optional block-regexp))
(declare-function org-show-context "org" (&optional key))
(declare-function org-at-table-p "org" (&optional table-type))
(declare-function org-cycle "org" (&optional arg))
(declare-function org-uniquify "org" (list))
(declare-function org-current-level "org" ())
(declare-function org-table-import "org-table" (file arg))
(declare-function org-add-hook "org-compat"
		  (hook function &optional append local))
(declare-function org-table-align "org-table" ())
(declare-function org-table-end "org-table" (&optional table-type))
(declare-function orgtbl-to-generic "org-table" (table params))
(declare-function orgtbl-to-orgtbl "org-table" (table params))
(declare-function org-babel-tangle-comment-links "ob-tangle" (&optional info))
(declare-function org-babel-lob-get-info "ob-lob" nil)
(declare-function org-babel-ref-split-args "ob-ref" (arg-string))
(declare-function org-babel-ref-parse "ob-ref" (assignment))
(declare-function org-babel-ref-resolve "ob-ref" (ref))
(declare-function org-babel-ref-goto-headline-id "ob-ref" (id))
(declare-function org-babel-ref-headline-body "ob-ref" ())
(declare-function org-babel-lob-execute-maybe "ob-lob" ())
(declare-function org-number-sequence "org-compat" (from &optional to inc))
(declare-function org-at-item-p "org-list" ())
(declare-function org-list-parse-list "org-list" (&optional delete))
(declare-function org-list-to-generic "org-list" (LIST PARAMS))
(declare-function org-list-struct "org-list" ())
(declare-function org-list-prevs-alist "org-list" (struct))
(declare-function org-list-get-list-end "org-list" (item struct prevs))
(declare-function org-remove-if "org" (predicate seq))
(declare-function org-completing-read "org" (&rest args))
(declare-function org-escape-code-in-region "org-src" (beg end))
(declare-function org-unescape-code-in-string "org-src" (s))
(declare-function org-table-to-lisp "org-table" (&optional txt))
(declare-function org-reverse-string "org" (string))
(declare-function org-element-context "org-element" (&optional element))
(declare-function org-element-type "org-element" (element))
(declare-function org-element-at-point "org-element" ())
(declare-function org-element-normalize-string "org-element" (s))
(declare-function org-element-property "org-element" (property element))
(declare-function org-every "org" (pred seq))
(declare-function org-macro-escape-arguments "org-macro" (&rest args))

(defgroup org-babel nil
  "Code block evaluation and management in `org-mode' documents."
  :tag "Babel"
  :group 'org)

(defcustom org-confirm-babel-evaluate t
  "Confirm before evaluation.
\\<org-mode-map>\
Require confirmation before interactively evaluating code
blocks in Org-mode buffers.  The default value of this variable
is t, meaning confirmation is required for any code block
evaluation.  This variable can be set to nil to inhibit any
future confirmation requests.  This variable can also be set to a
function which takes two arguments the language of the code block
and the body of the code block.  Such a function should then
return a non-nil value if the user should be prompted for
execution or nil if no prompt is required.

Warning: Disabling confirmation may result in accidental
evaluation of potentially harmful code.  It may be advisable
remove code block execution from \\[org-ctrl-c-ctrl-c] \
as further protection
against accidental code block evaluation.  The
`org-babel-no-eval-on-ctrl-c-ctrl-c' variable can be used to
remove code block execution from the \\[org-ctrl-c-ctrl-c] keybinding."
  :group 'org-babel
  :version "24.1"
  :type '(choice boolean function))
;; don't allow this variable to be changed through file settings
(put 'org-confirm-babel-evaluate 'safe-local-variable (lambda (x) (eq x t)))

(defcustom org-babel-no-eval-on-ctrl-c-ctrl-c nil
  "\\<org-mode-map>\
Remove code block evaluation from the \\[org-ctrl-c-ctrl-c] key binding."
  :group 'org-babel
  :version "24.1"
  :type 'boolean)

(defcustom org-babel-results-keyword "RESULTS"
  "Keyword used to name results generated by code blocks.
It should be \"RESULTS\".  However any capitalization may be
used."
  :group 'org-babel
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string
  :safe (lambda (v)
	  (and (stringp v)
	       (eq (compare-strings "RESULTS" nil nil v nil nil t)
		   t))))

(defcustom org-babel-noweb-wrap-start "<<"
  "String used to begin a noweb reference in a code block.
See also `org-babel-noweb-wrap-end'."
  :group 'org-babel
  :type 'string)

(defcustom org-babel-noweb-wrap-end ">>"
  "String used to end a noweb reference in a code block.
See also `org-babel-noweb-wrap-start'."
  :group 'org-babel
  :type 'string)

(defcustom org-babel-inline-result-wrap "=%s="
  "Format string used to wrap inline results.
This string must include a \"%s\" which will be replaced by the results."
  :group 'org-babel
  :type 'string)
(put 'org-babel-inline-result-wrap
     'safe-local-variable
     (lambda (value)
       (and (stringp value)
	    (string-match-p "%s" value))))

(defun org-babel-noweb-wrap (&optional regexp)
  (concat org-babel-noweb-wrap-start
	  (or regexp "\\([^ \t\n].+?[^ \t]\\|[^ \t\n]\\)")
	  org-babel-noweb-wrap-end))

(defvar org-babel-src-name-regexp
  "^[ \t]*#\\+name:[ \t]*"
  "Regular expression used to match a source name line.")

(defvar org-babel-multi-line-header-regexp
  "^[ \t]*#\\+headers?:[ \t]*\\([^\n]*\\)$"
  "Regular expression used to match multi-line header arguments.")

(defvar org-babel-src-block-regexp
  (concat
   ;; (1) indentation                 (2) lang
   "^\\([ \t]*\\)#\\+begin_src[ \t]+\\([^ \f\t\n\r\v]+\\)[ \t]*"
   ;; (3) switches
   "\\([^\":\n]*\"[^\"\n*]*\"[^\":\n]*\\|[^\":\n]*\\)"
   ;; (4) header arguments
   "\\([^\n]*\\)\n"
   ;; (5) body
   "\\([^\000]*?\n\\)??[ \t]*#\\+end_src")
  "Regexp used to identify code blocks.")

(defvar org-babel-inline-src-block-regexp
  (concat
   ;; (1) replacement target (2) lang
   "\\(?:^\\|[^-[:alnum:]]?\\)\\(src_\\([^ \f\t\n\r\v[]+\\)"
   ;; (3,4) (unused, headers)
   "\\(\\|\\[[ \t]*\\(.*?\\)\\]\\)"
   ;; (5) body
   "{\\([^\f\n\r\v]+?\\)}\\)")
  "Regexp used to identify inline src-blocks.")

(defun org-babel-get-header (params key &optional others)
  "Select only header argument of type KEY from a list.
Optional argument OTHERS indicates that only the header that do
not match KEY should be returned."
  (delq nil
	(mapcar
	 (lambda (p) (when (funcall (if others #'not #'identity) (eq (car p) key)) p))
	 params)))

(defun org-babel-get-inline-src-block-matches ()
  "Set match data if within body of an inline source block.
Returns non-nil if match-data set"
 (save-excursion
    (let ((datum (org-element-context)))
      (when (eq (org-element-type datum) 'inline-src-block)
	(goto-char (org-element-property :begin datum))
	(when (looking-at org-babel-inline-src-block-regexp)
	  t )))))

(defvar org-babel-inline-lob-one-liner-regexp)
(defun org-babel-get-lob-one-liner-matches ()
  "Set match data if on line of an lob one liner.
Returns non-nil if match-data set"
  (save-excursion
    (let ((datum (org-element-context)))
      (when (eq (org-element-type datum) 'inline-babel-call)
	(goto-char (org-element-property :begin datum))))
    (if (looking-at org-babel-inline-lob-one-liner-regexp)
	t
      nil)))

(defun org-babel-get-src-block-info (&optional light)
  "Get information on the current source block.

Optional argument LIGHT does not resolve remote variable
references; a process which could likely result in the execution
of other code blocks.

Returns a list
 (language body header-arguments-alist switches name indent block-head)."
  (let ((case-fold-search t) head info name indent)
    ;; full code block
    (if (setq head (org-babel-where-is-src-block-head))
	(save-excursion
	  (goto-char head)
	  (setq info (org-babel-parse-src-block-match))
	  (setq indent (car (last info)))
	  (setq info (butlast info))
	  (while (and (= 0 (forward-line -1))
		      (looking-at org-babel-multi-line-header-regexp))
	    (setf (nth 2 info)
		  (org-babel-merge-params
		   (nth 2 info)
		   (org-babel-parse-header-arguments (match-string 1)))))
	  (when (looking-at (org-babel-named-src-block-regexp-for-name))
	    (setq name (org-match-string-no-properties 9))))
      ;; inline source block
      (when (org-babel-get-inline-src-block-matches)
	(setq head (match-beginning 0))
	(setq info (org-babel-parse-inline-src-block-match))))
    ;; resolve variable references and add summary parameters
    (when (and info (not light))
      (setf (nth 2 info) (org-babel-process-params (nth 2 info))))
    (when info
      (setf (nth 2 info) (org-babel-generate-file-param name (nth 2 info))))
    (when info (append info (list name indent head)))))

(defvar org-babel-exp-reference-buffer nil
  "Buffer containing original contents of the exported buffer.
This is used by Babel to resolve references in source blocks.
Its value is dynamically bound during export.")

(defmacro org-babel-check-confirm-evaluate (info &rest body)
  "Evaluate BODY with special execution confirmation variables set.

Specifically; NOEVAL will indicate if evaluation is allowed,
QUERY will indicate if a user query is required, CODE-BLOCK will
hold the language of the code block, and BLOCK-NAME will hold the
name of the code block."
  (declare (indent defun))
  (org-with-gensyms
      (lang block-body headers name head eval eval-no export eval-no-export)
    `(let* ((,lang           (nth 0 ,info))
	    (,block-body     (nth 1 ,info))
	    (,headers        (nth 2 ,info))
	    (,name           (nth 4 ,info))
	    (,head           (nth 6 ,info))
	    (,eval           (or (cdr  (assoc :eval   ,headers))
				 (when (assoc :noeval ,headers) "no")))
	    (,eval-no        (or (equal ,eval "no")
				 (equal ,eval "never")))
	    (,export         org-babel-exp-reference-buffer)
	    (,eval-no-export (and ,export (or (equal ,eval "no-export")
					      (equal ,eval "never-export"))))
	    (noeval          (or ,eval-no ,eval-no-export))
	    (query           (or (equal ,eval "query")
				 (and ,export (equal ,eval "query-export"))
				 (if (functionp org-confirm-babel-evaluate)
				     (save-excursion
				       (goto-char ,head)
				       (funcall org-confirm-babel-evaluate
						,lang ,block-body))
				   org-confirm-babel-evaluate)))
	    (code-block      (if ,info (format  " %s "  ,lang) " "))
	    (block-name      (if ,name (format " (%s) " ,name) " ")))
       ;; Silence byte-compiler if `body' doesn't use those vars.
       (ignore noeval query)
       ,@body)))

(defsubst org-babel-check-evaluate (info)
  "Check if code block INFO should be evaluated.
Do not query the user."
  (org-babel-check-confirm-evaluate info
    (not (when noeval
	   (message "Evaluation of this%scode-block%sis disabled."
                    code-block block-name)))))

 ;; dynamically scoped for asynchronous export
(defvar org-babel-confirm-evaluate-answer-no)

(defsubst org-babel-confirm-evaluate (info)
  "Confirm evaluation of the code block INFO.

If the variable `org-babel-confirm-evaluate-answer-no' is bound
to a non-nil value, auto-answer with \"no\".

This query can also be suppressed by setting the value of
`org-confirm-babel-evaluate' to nil, in which case all future
interactive code block evaluations will proceed without any
confirmation from the user.

Note disabling confirmation may result in accidental evaluation
of potentially harmful code."
  (org-babel-check-confirm-evaluate info
    (not (when query
	   (unless
	       (and (not (org-bound-and-true-p
			  org-babel-confirm-evaluate-answer-no))
		    (yes-or-no-p
		     (format "Evaluate this%scode block%son your system? "
			     code-block block-name)))
	     (message "Evaluation of this%scode-block%sis aborted."
                      code-block block-name))))))

;;;###autoload
(defun org-babel-execute-safely-maybe ()
  (unless org-babel-no-eval-on-ctrl-c-ctrl-c
    (org-babel-execute-maybe)))

(add-hook 'org-ctrl-c-ctrl-c-hook 'org-babel-execute-safely-maybe)

;;;###autoload
(defun org-babel-execute-maybe ()
  (interactive)
  (or (org-babel-execute-src-block-maybe)
      (org-babel-lob-execute-maybe)))

(defmacro org-babel-when-in-src-block (&rest body)
  "Execute BODY if point is in a source block and return t.

Otherwise do nothing and return nil."
  `(if (or (org-babel-where-is-src-block-head)
           (org-babel-get-inline-src-block-matches))
       (progn
	 ,@body
	 t)
     nil))

(defun org-babel-execute-src-block-maybe ()
  "Conditionally execute a source block.
Detect if this is context for a Babel src-block and if so
then run `org-babel-execute-src-block'."
  (interactive)
  (org-babel-when-in-src-block
   (org-babel-eval-wipe-error-buffer)
   (org-babel-execute-src-block current-prefix-arg)))

;;;###autoload
(defun org-babel-view-src-block-info ()
  "Display information on the current source block.
This includes header arguments, language and name, and is largely
a window into the `org-babel-get-src-block-info' function."
  (interactive)
  (let ((info (org-babel-get-src-block-info 'light))
	(full (lambda (it) (> (length it) 0)))
	(printf (lambda (fmt &rest args) (princ (apply #'format fmt args)))))
    (when info
      (with-help-window (help-buffer)
	(let ((name        (nth 4 info))
	      (lang        (nth 0 info))
	      (switches    (nth 3 info))
	      (header-args (nth 2 info)))
	  (when name            (funcall printf "Name: %s\n"     name))
	  (when lang            (funcall printf "Lang: %s\n"     lang))
	  (funcall printf "Properties:\n")
	  (funcall printf "\t:header-args \t%s\n" (org-entry-get (point) "header-args" t))
	  (funcall printf "\t:header-args:%s \t%s\n" lang (org-entry-get (point) (concat "header-args:" lang) t))

	  (when (funcall full switches) (funcall printf "Switches: %s\n" switches))
	  (funcall printf "Header Arguments:\n")
	  (dolist (pair (sort header-args
			      (lambda (a b) (string< (symbol-name (car a))
						     (symbol-name (car b))))))
	    (when (funcall full (format "%s" (cdr pair)))
	      (funcall printf "\t%S%s\t%s\n"
		       (car pair)
		       (if (> (length (format "%S" (car pair))) 7) "" "\t")
		       (cdr pair)))))))))

;;;###autoload
(defun org-babel-expand-src-block-maybe ()
  "Conditionally expand a source block.
Detect if this is context for a org-babel src-block and if so
then run `org-babel-expand-src-block'."
  (interactive)
  (org-babel-when-in-src-block
   (org-babel-expand-src-block current-prefix-arg)))

;;;###autoload
(defun org-babel-load-in-session-maybe ()
  "Conditionally load a source block in a session.
Detect if this is context for a org-babel src-block and if so
then run `org-babel-load-in-session'."
  (interactive)
  (org-babel-when-in-src-block
   (org-babel-load-in-session current-prefix-arg)))

(add-hook 'org-metaup-hook 'org-babel-load-in-session-maybe)

;;;###autoload
(defun org-babel-pop-to-session-maybe ()
  "Conditionally pop to a session.
Detect if this is context for a org-babel src-block and if so
then run `org-babel-switch-to-session'."
  (interactive)
  (org-babel-when-in-src-block
   (org-babel-switch-to-session current-prefix-arg)))

(add-hook 'org-metadown-hook 'org-babel-pop-to-session-maybe)

(defconst org-babel-common-header-args-w-values
  '((cache	. ((no yes)))
    (cmdline	. :any)
    (colnames	. ((nil no yes)))
    (comments	. ((no link yes org both noweb)))
    (dir	. :any)
    (eval	. ((yes no no-export strip-export never-export eval never
			query)))
    (exports	. ((code results both none)))
    (epilogue   . :any)
    (file	. :any)
    (file-desc  . :any)
    (file-ext   . :any)
    (hlines	. ((no yes)))
    (mkdirp	. ((yes no)))
    (no-expand)
    (noeval)
    (noweb	. ((yes no tangle no-export strip-export)))
    (noweb-ref	. :any)
    (noweb-sep  . :any)
    (output-dir . :any)
    (padline	. ((yes no)))
    (post       . :any)
    (prologue   . :any)
    (results	. ((file list vector table scalar verbatim)
		   (raw html latex org code pp drawer)
		   (replace silent none append prepend)
		   (output value)))
    (rownames	. ((no yes)))
    (sep	. :any)
    (session	. :any)
    (shebang	. :any)
    (tangle	. ((tangle yes no :any)))
    (tangle-mode . ((#o755 #o555 #o444 :any)))
    (var	. :any)
    (wrap       . :any)))

(defconst org-babel-header-arg-names
  (mapcar #'car org-babel-common-header-args-w-values)
  "Common header arguments used by org-babel.
Note that individual languages may define their own language
specific header arguments as well.")

(defconst org-babel-safe-header-args
  '(:cache :colnames :comments :exports :epilogue :hlines :noeval
	   :noweb :noweb-ref :noweb-sep :padline :prologue :rownames
	   :sep :session :tangle :wrap
	   (:eval . ("never" "query"))
	   (:results . (lambda (str) (not (string-match "file" str)))))
  "A list of safe header arguments for babel source blocks.

The list can have entries of the following forms:
- :ARG                     -> :ARG is always a safe header arg
- (:ARG . (VAL1 VAL2 ...)) -> :ARG is safe as a header arg if it is
                              `equal' to one of the VALs.
- (:ARG . FN)              -> :ARG is safe as a header arg if the function FN
                              returns non-nil.  FN is passed one
                              argument, the value of the header arg
                              (as a string).")

(defmacro org-babel-header-args-safe-fn (safe-list)
  "Return a function that determines whether a list of header args are safe.

Intended usage is:
\(put \\='org-babel-default-header-args \\='safe-local-variable
 (org-babel-header-args-safe-p org-babel-safe-header-args)

This allows org-babel languages to extend the list of safe values for
their `org-babel-default-header-args:foo' variable.

For the format of SAFE-LIST, see `org-babel-safe-header-args'."
  `(lambda (value)
     (and (listp value)
	  (org-every
	   (lambda (pair)
	     (and (consp pair)
		  (org-babel-one-header-arg-safe-p pair ,safe-list)))
	   value))))

(defvar org-babel-default-header-args
  '((:session . "none") (:results . "replace") (:exports . "code")
    (:cache . "no") (:noweb . "no") (:hlines . "no") (:tangle . "no"))
  "Default arguments to use when evaluating a source block.")
(put 'org-babel-default-header-args 'safe-local-variable
     (org-babel-header-args-safe-fn org-babel-safe-header-args))

(defvar org-babel-default-inline-header-args
  '((:session . "none") (:results . "replace")
    (:exports . "results") (:hlines . "yes"))
  "Default arguments to use when evaluating an inline source block.")
(put 'org-babel-default-inline-header-args 'safe-local-variable
     (org-babel-header-args-safe-fn org-babel-safe-header-args))

(defvar org-babel-data-names '("tblname" "results" "name"))

(defvar org-babel-result-regexp
  (concat "^[ \t]*#\\+"
	  (regexp-opt org-babel-data-names t)
	  "\\(\\[\\("
	  ;; FIXME The string below is `org-ts-regexp'
	  "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} ?[^\r\n>]*?\\)>"
	  " \\)?\\([[:alnum:]]+\\)\\]\\)?\\:[ \t]*")
  "Regular expression used to match result lines.
If the results are associated with a hash key then the hash will
be saved in the second match data.")

(defvar org-babel-result-w-name-regexp
  (concat org-babel-result-regexp
	  "\\([^ ()\f\t\n\r\v]+\\)\\((\\(.*\\))\\|\\)"))

(defvar org-babel-min-lines-for-block-output 10
  "The minimum number of lines for block output.
If number of lines of output is equal to or exceeds this
value, the output is placed in a #+begin_example...#+end_example
block.  Otherwise the output is marked as literal by inserting
colons at the starts of the lines.  This variable only takes
effect if the :results output option is in effect.")

(defvar org-babel-noweb-error-all-langs nil
  "Raise errors when noweb references don't resolve.
Also see `org-babel-noweb-error-langs' to control noweb errors on
a language by language bases.")

(defvar org-babel-noweb-error-langs nil
  "Languages for which Babel will raise literate programming errors.
List of languages for which errors should be raised when the
source code block satisfying a noweb reference in this language
can not be resolved.  Also see `org-babel-noweb-error-all-langs'
to raise errors for all languages.")

(defvar org-babel-hash-show 4
  "Number of initial characters to show of a hidden results hash.")

(defvar org-babel-hash-show-time nil
  "Non-nil means show the time the code block was evaluated in the result hash.")

(defvar org-babel-after-execute-hook nil
  "Hook for functions to be called after `org-babel-execute-src-block'")

(defun org-babel-named-src-block-regexp-for-name (&optional name)
  "This generates a regexp used to match a src block named NAME.
If NAME is nil, match any name.  Matched name is then put in
match group 9.  Other match groups are defined in
`org-babel-src-block-regexp'."
  (concat org-babel-src-name-regexp
	  (concat (if name (regexp-quote name) "\\(?9:.*?\\)") "[ \t]*" )
	  "\\(?:\n[ \t]*#\\+\\S-+:.*\\)*?"
	  "\n"
	  (substring org-babel-src-block-regexp 1)))

(defun org-babel-named-data-regexp-for-name (name)
  "This generates a regexp used to match data named NAME."
  (concat org-babel-result-regexp (regexp-quote name) "\\([ \t]\\|$\\)"))

;;; functions
(defvar call-process-region)
(defvar org-babel-current-src-block-location nil
  "Marker pointing to the src block currently being executed.
This may also point to a call line or an inline code block.  If
multiple blocks are being executed (e.g., in chained execution
through use of the :var header argument) this marker points to
the outer-most code block.")

(defvar *this*)

;;;###autoload
(defun org-babel-execute-src-block (&optional arg info params)
  "Execute the current source code block.
Insert the results of execution into the buffer.  Source code
execution and the collection and formatting of results can be
controlled through a variety of header arguments.

With prefix argument ARG, force re-execution even if an existing
result cached in the buffer would otherwise have been returned.

Optionally supply a value for INFO in the form returned by
`org-babel-get-src-block-info'.

Optionally supply a value for PARAMS which will be merged with
the header arguments specified at the front of the source code
block."
  (interactive)
  (let* ((org-babel-current-src-block-location
	  (or org-babel-current-src-block-location
	      (nth 6 info)
	      (org-babel-where-is-src-block-head)
	      ;; inline src block
	      (and (org-babel-get-inline-src-block-matches)
		   (match-beginning 0))))
	 (info (if info
		   (copy-tree info)
		 (org-babel-get-src-block-info)))
	 (merged-params (org-babel-merge-params (nth 2 info) params)))
    (when (org-babel-check-evaluate
	   (let ((i info)) (setf (nth 2 i) merged-params) i))
      (let* ((params (if params
			 (org-babel-process-params merged-params)
		       (nth 2 info)))
	     (cachep (and (not arg) (cdr (assoc :cache params))
			   (string= "yes" (cdr (assoc :cache params)))))
	     (new-hash (when cachep (org-babel-sha1-hash info)))
	     (old-hash (when cachep (org-babel-current-result-hash)))
	     (cache-current-p (and (not arg) new-hash
				   (equal new-hash old-hash))))
	(cond
	 (cache-current-p
	  (save-excursion ;; return cached result
	    (goto-char (org-babel-where-is-src-block-result nil info))
	    (forward-line)
	    (skip-chars-forward " \t")
	    (let ((result (org-babel-read-result)))
	      (message (replace-regexp-in-string
			"%" "%%" (format "%S" result)))
	      result)))
	 ((org-babel-confirm-evaluate
	   (let ((i info)) (setf (nth 2 i) merged-params) i))
	  (let* ((lang (nth 0 info))
		 (result-params (cdr (assoc :result-params params)))
		 (body (setf (nth 1 info)
			     (if (org-babel-noweb-p params :eval)
				 (org-babel-expand-noweb-references info)
			       (nth 1 info))))
		 (dir (cdr (assoc :dir params)))
		 (default-directory
		   (or (and dir (file-name-as-directory (expand-file-name dir)))
		       default-directory))
		 (org-babel-call-process-region-original ;; for tramp handler
		  (or (org-bound-and-true-p
		       org-babel-call-process-region-original)
		      (symbol-function 'call-process-region)))
		 (indent (nth 5 info))
		 result cmd)
	    (unwind-protect
		(let ((call-process-region
		       (lambda (&rest args)
			 (apply 'org-babel-tramp-handle-call-process-region
				args))))
		  (let ((lang-check
			 (lambda (f)
			   (let ((f (intern (concat "org-babel-execute:" f))))
			     (when (fboundp f) f)))))
		    (setq cmd
			  (or (funcall lang-check lang)
			      (funcall lang-check
				       (symbol-name
					(cdr (assoc lang org-src-lang-modes))))
			      (error "No org-babel-execute function for %s!"
				     lang))))
		  (message "executing %s code block%s..."
			   (capitalize lang)
			   (if (nth 4 info) (format " (%s)" (nth 4 info)) ""))
		  (if (member "none" result-params)
		      (progn
			(funcall cmd body params)
			(message "result silenced")
			(setq result nil))
		    (setq result
			  (let ((result (funcall cmd body params)))
                            (if (and (eq (cdr (assoc :result-type params))
                                         'value)
                                     (or (member "vector" result-params)
                                         (member "table" result-params))
                                     (not (listp result)))
                                (list (list result)) result)))
		    ;; If non-empty result and :file then write to :file.
		    (when (cdr (assoc :file params))
		      (when result
			(with-temp-file (cdr (assoc :file params))
			  (insert
			   (org-babel-format-result
			    result (cdr (assoc :sep (nth 2 info)))))))
		      (setq result (cdr (assoc :file params))))
		    ;; Possibly perform post process provided its appropriate.
		    (when (cdr (assoc :post params))
		      (let ((*this* (if (cdr (assoc :file params))
					(org-babel-result-to-file
					 (cdr (assoc :file params))
					 (when (assoc :file-desc params)
					   (or (cdr (assoc :file-desc params))
					       result)))
				      result)))
			(setq result (org-babel-ref-resolve
				      (cdr (assoc :post params))))
			(when (cdr (assoc :file params))
			  (setq result-params
				(remove "file" result-params)))))
		    (org-babel-insert-result
		     result result-params info new-hash indent lang))
                  (run-hooks 'org-babel-after-execute-hook)
		  result)
	      (setq call-process-region
		    'org-babel-call-process-region-original)))))))))

(defun org-babel-expand-body:generic (body params &optional var-lines)
  "Expand BODY with PARAMS.
Expand a block of code with org-babel according to its header
arguments.  This generic implementation of body expansion is
called for languages which have not defined their own specific
org-babel-expand-body:lang function."
  (let ((pro (cdr (assoc :prologue params)))
	(epi (cdr (assoc :epilogue params))))
    (mapconcat #'identity
	       (append (when pro (list pro))
		       var-lines
		       (list body)
		       (when epi (list epi)))
	       "\n")))

;;;###autoload
(defun org-babel-expand-src-block (&optional _arg info params)
  "Expand the current source code block.
Expand according to the source code block's header
arguments and pop open the results in a preview buffer."
  (interactive)
  (let* ((info (or info (org-babel-get-src-block-info)))
         (lang (nth 0 info))
	 (params (setf (nth 2 info)
                       (sort (org-babel-merge-params (nth 2 info) params)
                             (lambda (el1 el2) (string< (symbol-name (car el1))
							(symbol-name (car el2)))))))
         (body (setf (nth 1 info)
		     (if (org-babel-noweb-p params :eval)
			 (org-babel-expand-noweb-references info) (nth 1 info))))
         (expand-cmd (intern (concat "org-babel-expand-body:" lang)))
	 (assignments-cmd (intern (concat "org-babel-variable-assignments:"
					  lang)))
         (expanded
	  (if (fboundp expand-cmd) (funcall expand-cmd body params)
	    (org-babel-expand-body:generic
	     body params (and (fboundp assignments-cmd)
			      (funcall assignments-cmd params))))))
    (if (org-called-interactively-p 'any)
	(org-edit-src-code
	 expanded (concat "*Org-Babel Preview " (buffer-name) "[ " lang " ]*"))
      expanded)))

(defun org-babel-edit-distance (s1 s2)
  "Return the edit (levenshtein) distance between strings S1 S2."
  (let* ((l1 (length s1))
	 (l2 (length s2))
	 (dist (vconcat (mapcar (lambda (_) (make-vector (1+ l2) nil))
				(number-sequence 1 (1+ l1)))))
	 (in (lambda (i j) (aref (aref dist i) j))))
    (setf (aref (aref dist 0) 0) 0)
    (dolist (j (number-sequence 1 l2))
      (setf (aref (aref dist 0) j) j))
    (dolist (i (number-sequence 1 l1))
      (setf (aref (aref dist i) 0) i)
      (dolist (j (number-sequence 1 l2))
	(setf (aref (aref dist i) j)
	      (min
	       (1+ (funcall in (1- i) j))
	       (1+ (funcall in i (1- j)))
	       (+ (if (equal (aref s1 (1- i)) (aref s2 (1- j))) 0 1)
		  (funcall in (1- i) (1- j)))))))
    (funcall in l1 l2)))

(defun org-babel-combine-header-arg-lists (original &rest others)
  "Combine a number of lists of header argument names and arguments."
  (let ((results (copy-sequence original)))
    (dolist (new-list others)
      (dolist (arg-pair new-list)
	(let ((header (car arg-pair)))
	  (setq results
		(cons arg-pair (org-remove-if
				(lambda (pair) (equal header (car pair)))
				results))))))
    results))

;;;###autoload
(defun org-babel-check-src-block ()
  "Check for misspelled header arguments in the current code block."
  (interactive)
  ;; TODO: report malformed code block
  ;; TODO: report incompatible combinations of header arguments
  ;; TODO: report uninitialized variables
  (let ((too-close 2) ;; <- control closeness to report potential match
	(names (mapcar #'symbol-name org-babel-header-arg-names)))
    (dolist (header (mapcar (lambda (arg) (substring (symbol-name (car arg)) 1))
			    (and (org-babel-where-is-src-block-head)
				 (org-babel-parse-header-arguments
				  (org-no-properties
				   (match-string 4))))))
      (dolist (name names)
	(when (and (not (string= header name))
		   (<= (org-babel-edit-distance header name) too-close)
		   (not (member header names)))
	  (error "Supplied header \"%S\" is suspiciously close to \"%S\""
		 header name))))
    (message "No suspicious header arguments found.")))

;;;###autoload
(defun org-babel-insert-header-arg (&optional header-arg value)
  "Insert a header argument selecting from lists of common args and values."
  (interactive)
  (let* ((info (org-babel-get-src-block-info 'light))
	 (lang (car info))
	 (begin (nth 6 info))
	 (lang-headers (intern (concat "org-babel-header-args:" lang)))
	 (headers (org-babel-combine-header-arg-lists
		   org-babel-common-header-args-w-values
		   (when (boundp lang-headers) (eval lang-headers))))
	 (header-arg (or header-arg
			 (org-icompleting-read
			  "Header Arg: "
			  (mapcar
			   (lambda (header-spec) (symbol-name (car header-spec)))
			   headers))))
	 (vals (cdr (assoc (intern header-arg) headers)))
	 (value (or value
		    (cond
		     ((eq vals :any)
		      (read-from-minibuffer "value: "))
		     ((listp vals)
		      (mapconcat
		       (lambda (group)
			 (let ((arg (org-icompleting-read
				     "Value: "
				     (cons "default"
					   (mapcar #'symbol-name group)))))
			   (if (and arg (not (string= "default" arg)))
			       (concat arg " ")
			     "")))
		       vals ""))))))
    (save-excursion
      (goto-char begin)
      (goto-char (point-at-eol))
      (unless (= (char-before (point)) ?\ ) (insert " "))
      (insert ":" header-arg) (when value (insert " " value)))))

;; Add support for completing-read insertion of header arguments after ":"
(defun org-babel-header-arg-expand ()
  "Call `org-babel-enter-header-arg-w-completion' in appropriate contexts."
  (when (and (equal (char-before) ?\:) (org-babel-where-is-src-block-head))
    (org-babel-enter-header-arg-w-completion (match-string 2))))

(defun org-babel-enter-header-arg-w-completion (&optional lang)
  "Insert header argument appropriate for LANG with completion."
  (let* ((lang-headers-var (intern (concat "org-babel-header-args:" lang)))
         (lang-headers (when (boundp lang-headers-var) (eval lang-headers-var)))
	 (headers-w-values (org-babel-combine-header-arg-lists
			    org-babel-common-header-args-w-values lang-headers))
         (headers (mapcar #'symbol-name (mapcar #'car headers-w-values)))
         (header (org-completing-read "Header Arg: " headers))
         (args (cdr (assoc (intern header) headers-w-values)))
         (arg (when (and args (listp args))
                (org-completing-read
                 (format "%s: " header)
                 (mapcar #'symbol-name (apply #'append args))))))
    (insert (concat header " " (or arg "")))
    (cons header arg)))

(add-hook 'org-tab-first-hook 'org-babel-header-arg-expand)

;;;###autoload
(defun org-babel-load-in-session (&optional _arg info)
  "Load the body of the current source-code block.
Evaluate the header arguments for the source block before
entering the session.  After loading the body this pops open the
session."
  (interactive)
  (let* ((info (or info (org-babel-get-src-block-info)))
         (lang (nth 0 info))
         (params (nth 2 info))
         (body (if (not info)
		   (user-error "No src code block at point")
		 (setf (nth 1 info)
		       (if (org-babel-noweb-p params :eval)
			   (org-babel-expand-noweb-references info)
			 (nth 1 info)))))
         (session (cdr (assoc :session params)))
	 (dir (cdr (assoc :dir params)))
	 (default-directory
	   (or (and dir (file-name-as-directory dir)) default-directory))
	 (cmd (intern (concat "org-babel-load-session:" lang))))
    (unless (fboundp cmd)
      (error "No org-babel-load-session function for %s!" lang))
    (pop-to-buffer (funcall cmd session body params))
    (end-of-line 1)))

;;;###autoload
(defun org-babel-initiate-session (&optional arg info)
  "Initiate session for current code block.
If called with a prefix argument then resolve any variable
references in the header arguments and assign these variables in
the session.  Copy the body of the code block to the kill ring."
  (interactive "P")
  (let* ((info (or info (org-babel-get-src-block-info (not arg))))
         (lang (nth 0 info))
         (body (nth 1 info))
         (params (nth 2 info))
         (session (cdr (assoc :session params)))
	 (dir (cdr (assoc :dir params)))
	 (default-directory
	   (or (and dir (file-name-as-directory dir)) default-directory))
	 (init-cmd (intern (format "org-babel-%s-initiate-session" lang)))
	 (prep-cmd (intern (concat "org-babel-prep-session:" lang))))
    (if (and (stringp session) (string= session "none"))
	(error "This block is not using a session!"))
    (unless (fboundp init-cmd)
      (error "No org-babel-initiate-session function for %s!" lang))
    (with-temp-buffer (insert (org-babel-trim body))
                      (copy-region-as-kill (point-min) (point-max)))
    (when arg
      (unless (fboundp prep-cmd)
	(error "No org-babel-prep-session function for %s!" lang))
      (funcall prep-cmd session params))
    (funcall init-cmd session params)))

;;;###autoload
(defun org-babel-switch-to-session (&optional arg info)
  "Switch to the session of the current code block.
Uses `org-babel-initiate-session' to start the session.  If called
with a prefix argument then this is passed on to
`org-babel-initiate-session'."
  (interactive "P")
  (pop-to-buffer (org-babel-initiate-session arg info))
  (end-of-line 1))

(defalias 'org-babel-pop-to-session 'org-babel-switch-to-session)

(defvar org-src-window-setup)

;;;###autoload
(defun org-babel-switch-to-session-with-code (&optional arg _info)
  "Switch to code buffer and display session."
  (interactive "P")
  (let ((swap-windows
	 (lambda ()
	   (let ((other-window-buffer (window-buffer (next-window))))
	     (set-window-buffer (next-window) (current-buffer))
	     (set-window-buffer (selected-window) other-window-buffer))
	   (other-window 1)))
	(info (org-babel-get-src-block-info))
	(org-src-window-setup 'reorganize-frame))
    (save-excursion
      (org-babel-switch-to-session arg info))
    (org-edit-src-code)
    (funcall swap-windows)))

;;;###autoload
(defmacro org-babel-do-in-edit-buffer (&rest body)
  "Evaluate BODY in edit buffer if there is a code block at point.
Return t if a code block was found at point, nil otherwise."
  `(let ((org-src-window-setup 'switch-invisibly))
     (when (and (org-babel-where-is-src-block-head)
		(org-edit-src-code))
       (unwind-protect (progn ,@body)
	 (org-edit-src-exit))
       t)))
(def-edebug-spec org-babel-do-in-edit-buffer (body))

(defun org-babel-do-key-sequence-in-edit-buffer (key)
  "Read key sequence and execute the command in edit buffer.
Enter a key sequence to be executed in the language major-mode
edit buffer.  For example, TAB will alter the contents of the
Org-mode code block according to the effect of TAB in the
language major-mode buffer.  For languages that support
interactive sessions, this can be used to send code from the Org
buffer to the session for evaluation using the native major-mode
evaluation mechanisms."
  (interactive "kEnter key-sequence to execute in edit buffer: ")
  (org-babel-do-in-edit-buffer
   (call-interactively
    (key-binding (or key (read-key-sequence nil))))))

(defvar org-bracket-link-regexp)

(defun org-babel-active-location-p ()
  (memq (org-element-type (save-match-data (org-element-context)))
	'(babel-call inline-babel-call inline-src-block src-block)))

;;;###autoload
(defun org-babel-open-src-block-result (&optional re-run)
  "If `point' is on a src block then open the results of the
source code block, otherwise return nil.  With optional prefix
argument RE-RUN the source-code block is evaluated even if
results already exist."
  (interactive "P")
  (let ((info (org-babel-get-src-block-info 'light)))
    (when info
      (save-excursion
	;; go to the results, if there aren't any then run the block
	(goto-char (or (and (not re-run) (org-babel-where-is-src-block-result))
		       (progn (org-babel-execute-src-block)
			      (org-babel-where-is-src-block-result))))
	(end-of-line 1)
	(while (looking-at "[\n\r\t\f ]") (forward-char 1))
	;; open the results
	(if (looking-at org-bracket-link-regexp)
	    ;; file results
	    (org-open-at-point)
	  (let ((r (org-babel-format-result
		    (org-babel-read-result) (cdr (assoc :sep (nth 2 info))))))
	    (pop-to-buffer (get-buffer-create "*Org-Babel Results*"))
	    (delete-region (point-min) (point-max))
	    (insert r)))
	t))))

;;;###autoload
(defmacro org-babel-map-src-blocks (file &rest body)
  "Evaluate BODY forms on each source-block in FILE.
If FILE is nil evaluate BODY forms on source blocks in current
buffer.  During evaluation of BODY the following local variables
are set relative to the currently matched code block.

full-block ------- string holding the entirety of the code block
beg-block -------- point at the beginning of the code block
end-block -------- point at the end of the matched code block
lang ------------- string holding the language of the code block
beg-lang --------- point at the beginning of the lang
end-lang --------- point at the end of the lang
switches --------- string holding the switches
beg-switches ----- point at the beginning of the switches
end-switches ----- point at the end of the switches
header-args ------ string holding the header-args
beg-header-args -- point at the beginning of the header-args
end-header-args -- point at the end of the header-args
body ------------- string holding the body of the code block
beg-body --------- point at the beginning of the body
end-body --------- point at the end of the body"
  (declare (indent 1))
  (let ((tempvar (make-symbol "file")))
    `(let* ((case-fold-search t)
	    (,tempvar ,file)
	    (visited-p (or (null ,tempvar)
			   (get-file-buffer (expand-file-name ,tempvar))))
	    (point (point)) to-be-removed)
       (save-window-excursion
	 (when ,tempvar (find-file ,tempvar))
	 (setq to-be-removed (current-buffer))
	 (goto-char (point-min))
	 (while (re-search-forward org-babel-src-block-regexp nil t)
	   (when (org-babel-active-location-p)
	     (goto-char (match-beginning 0))
	     (let ((full-block (match-string 0))
		   (beg-block (match-beginning 0))
		   (end-block (match-end 0))
		   (lang (match-string 2))
		   (beg-lang (match-beginning 2))
		   (end-lang (match-end 2))
		   (switches (match-string 3))
		   (beg-switches (match-beginning 3))
		   (end-switches (match-end 3))
		   (header-args (match-string 4))
		   (beg-header-args (match-beginning 4))
		   (end-header-args (match-end 4))
		   (body (match-string 5))
		   (beg-body (match-beginning 5))
		   (end-body (match-end 5)))
               ;; Silence byte-compiler in case `body' doesn't use all
               ;; those variables.
               (ignore full-block beg-block end-block lang
                       beg-lang end-lang switches beg-switches
                       end-switches header-args beg-header-args
                       end-header-args body beg-body end-body)
               ,@body
	       (goto-char end-block)))))
       (unless visited-p (kill-buffer to-be-removed))
       (goto-char point))))
(def-edebug-spec org-babel-map-src-blocks (form body))

;;;###autoload
(defmacro org-babel-map-inline-src-blocks (file &rest body)
  "Evaluate BODY forms on each inline source-block in FILE.
If FILE is nil evaluate BODY forms on source blocks in current
buffer."
  (declare (indent 1))
  (let ((tempvar (make-symbol "file")))
    `(let* ((case-fold-search t)
	    (,tempvar ,file)
	    (visited-p (or (null ,tempvar)
			   (get-file-buffer (expand-file-name ,tempvar))))
	    (point (point)) to-be-removed)
       (save-window-excursion
	 (when ,tempvar (find-file ,tempvar))
	 (setq to-be-removed (current-buffer))
	 (goto-char (point-min))
	 (while (re-search-forward org-babel-inline-src-block-regexp nil t)
	   (when (org-babel-active-location-p)
	     (goto-char (match-beginning 1))
	     (save-match-data ,@body))
	   (goto-char (match-end 0))))
       (unless visited-p (kill-buffer to-be-removed))
       (goto-char point))))
(def-edebug-spec org-babel-map-inline-src-blocks (form body))

(defvar org-babel-lob-one-liner-regexp)

;;;###autoload
(defmacro org-babel-map-call-lines (file &rest body)
  "Evaluate BODY forms on each call line in FILE.
If FILE is nil evaluate BODY forms on source blocks in current
buffer."
  (declare (indent 1))
  (let ((tempvar (make-symbol "file")))
    `(let* ((,tempvar ,file)
	    (visited-p (or (null ,tempvar)
			   (get-file-buffer (expand-file-name ,tempvar))))
	    (point (point)) to-be-removed)
       (save-window-excursion
	 (when ,tempvar (find-file ,tempvar))
	 (setq to-be-removed (current-buffer))
	 (goto-char (point-min))
	 (while (re-search-forward org-babel-lob-one-liner-regexp nil t)
	   (when (org-babel-active-location-p)
	     (goto-char (match-beginning 1))
	     (save-match-data ,@body))
	   (goto-char (match-end 0))))
       (unless visited-p (kill-buffer to-be-removed))
       (goto-char point))))
(def-edebug-spec org-babel-map-call-lines (form body))

;;;###autoload
(defmacro org-babel-map-executables (file &rest body)
  (declare (indent 1))
  (let ((tempvar (make-symbol "file"))
	(rx (make-symbol "rx")))
    `(let* ((,tempvar ,file)
	    (,rx (concat "\\(" org-babel-src-block-regexp
			 "\\|" org-babel-inline-src-block-regexp
			 "\\|" org-babel-lob-one-liner-regexp "\\)"))
	    (visited-p (or (null ,tempvar)
			   (get-file-buffer (expand-file-name ,tempvar))))
	    (point (point)) to-be-removed)
       (save-window-excursion
	 (when ,tempvar (find-file ,tempvar))
	 (setq to-be-removed (current-buffer))
	 (goto-char (point-min))
	 (while (re-search-forward ,rx nil t)
	   (when (org-babel-active-location-p)
	     (goto-char (match-beginning 1))
	     (when (looking-at org-babel-inline-src-block-regexp)
	       (forward-char 1))
	     (save-match-data ,@body))
	   (goto-char (match-end 0))))
       (unless visited-p (kill-buffer to-be-removed))
       (goto-char point))))
(def-edebug-spec org-babel-map-executables (form body))

;;;###autoload
(defun org-babel-execute-buffer (&optional arg)
  "Execute source code blocks in a buffer.
Call `org-babel-execute-src-block' on every source block in
the current buffer."
  (interactive "P")
  (org-babel-eval-wipe-error-buffer)
  (org-save-outline-visibility t
    (org-babel-map-executables nil
      (if (looking-at org-babel-lob-one-liner-regexp)
          (org-babel-lob-execute-maybe)
        (org-babel-execute-src-block arg)))))

;;;###autoload
(defun org-babel-execute-subtree (&optional arg)
  "Execute source code blocks in a subtree.
Call `org-babel-execute-src-block' on every source block in
the current subtree."
  (interactive "P")
  (save-restriction
    (save-excursion
      (org-narrow-to-subtree)
      (org-babel-execute-buffer arg)
      (widen))))

;;;###autoload
(defun org-babel-sha1-hash (&optional info)
  "Generate an sha1 hash based on the value of info."
  (interactive)
  (let ((print-level nil)
	(info (or info (org-babel-get-src-block-info))))
    (setf (nth 2 info)
	  (sort (copy-sequence (nth 2 info))
		(lambda (a b) (string< (car a) (car b)))))
    (let* ((rm (lambda (lst)
		 (dolist (p '("replace" "silent" "none"
			      "append" "prepend"))
		   (setq lst (remove p lst)))
		 lst))
	   (norm (lambda (arg)
		   (let ((v (if (and (listp (cdr arg)) (null (cddr arg)))
				(copy-sequence (cdr arg))
			      (cdr arg))))
		     (when (and v (not (and (sequencep v)
					    (not (consp v))
					    (= (length v) 0))))
		       (cond
			((and (listp v) ; lists are sorted
			      (member (car arg) '(:result-params)))
			 (sort (funcall rm v) #'string<))
			((and (stringp v) ; strings are sorted
			      (member (car arg) '(:results :exports)))
			 (mapconcat #'identity (sort (funcall rm (split-string v))
						     #'string<) " "))
			(t v))))))
	   ;; expanded body
	   (lang (nth 0 info))
	   (params (nth 2 info))
	   (body (if (org-babel-noweb-p params :eval)
			   (org-babel-expand-noweb-references info) (nth 1 info)))
	   (expand-cmd (intern (concat "org-babel-expand-body:" lang)))
	   (assignments-cmd (intern (concat "org-babel-variable-assignments:"
					    lang)))
	   (expanded
	    (if (fboundp expand-cmd) (funcall expand-cmd body params)
	      (org-babel-expand-body:generic
	       body params (and (fboundp assignments-cmd)
				(funcall assignments-cmd params))))))
      (let* ((it (format "%s-%s"
                         (mapconcat
                          #'identity
                          (delq nil (mapcar (lambda (arg)
                                              (let ((normalized (funcall norm arg)))
                                                (when normalized
                                                  (format "%S" normalized))))
                                            (nth 2 info))) ":")
                         expanded))
             (hash (sha1 it)))
        (when (org-called-interactively-p 'interactive) (message hash))
        hash))))

(defun org-babel-current-result-hash (&optional info)
  "Return the current in-buffer hash."
  (org-babel-where-is-src-block-result nil info)
  (org-no-properties (match-string 5)))

(defun org-babel-set-current-result-hash (hash info)
  "Set the current in-buffer hash to HASH."
  (org-babel-where-is-src-block-result nil info)
  (save-excursion (goto-char (match-beginning 5))
		  (mapc #'delete-overlay (overlays-at (point)))
		  (forward-char org-babel-hash-show)
		  (mapc #'delete-overlay (overlays-at (point)))
		  (replace-match hash nil nil nil 5)
		  (goto-char (point-at-bol))
		  (org-babel-hide-hash)))

(defun org-babel-hide-hash ()
  "Hide the hash in the current results line.
Only the initial `org-babel-hash-show' characters of the hash
will remain visible."
  (add-to-invisibility-spec '(org-babel-hide-hash . t))
  (save-excursion
    (when (and (re-search-forward org-babel-result-regexp nil t)
               (match-string 5))
      (let* ((start (match-beginning 5))
             (hide-start (+ org-babel-hash-show start))
             (end (match-end 5))
             (hash (match-string 5))
             ov1 ov2)
        (setq ov1 (make-overlay start hide-start))
        (setq ov2 (make-overlay hide-start end))
        (overlay-put ov2 'invisible 'org-babel-hide-hash)
        (overlay-put ov1 'babel-hash hash)))))

(defun org-babel-hide-all-hashes ()
  "Hide the hash in the current buffer.
Only the initial `org-babel-hash-show' characters of each hash
will remain visible.  This function should be called as part of
the `org-mode-hook'."
  (save-excursion
    (while (and (not org-babel-hash-show-time)
		(re-search-forward org-babel-result-regexp nil t))
      (goto-char (match-beginning 0))
      (org-babel-hide-hash)
      (goto-char (match-end 0)))))
(add-hook 'org-mode-hook 'org-babel-hide-all-hashes)

(defun org-babel-hash-at-point (&optional point)
  "Return the value of the hash at POINT.
\\<org-mode-map>\
The hash is also added as the last element of the kill ring.
This can be called with \\[org-ctrl-c-ctrl-c]."
  (interactive)
  (let ((hash (car (delq nil (mapcar
			      (lambda (ol) (overlay-get ol 'babel-hash))
                              (overlays-at (or point (point))))))))
    (when hash (kill-new hash) (message hash))))
(add-hook 'org-ctrl-c-ctrl-c-hook 'org-babel-hash-at-point)

(defun org-babel-result-hide-spec ()
  "Hide portions of results lines.
Add `org-babel-hide-result' as an invisibility spec for hiding
portions of results lines."
  (add-to-invisibility-spec '(org-babel-hide-result . t)))
(add-hook 'org-mode-hook 'org-babel-result-hide-spec)

(defvar org-babel-hide-result-overlays nil
  "Overlays hiding results.")

(defun org-babel-result-hide-all ()
  "Fold all results in the current buffer."
  (interactive)
  (org-babel-show-result-all)
  (save-excursion
    (while (re-search-forward org-babel-result-regexp nil t)
      (save-excursion (goto-char (match-beginning 0))
                      (org-babel-hide-result-toggle-maybe)))))

(defun org-babel-show-result-all ()
  "Unfold all results in the current buffer."
  (mapc 'delete-overlay org-babel-hide-result-overlays)
  (setq org-babel-hide-result-overlays nil))

;;;###autoload
(defun org-babel-hide-result-toggle-maybe ()
  "Toggle visibility of result at point."
  (interactive)
  (let ((case-fold-search t))
    (if (save-excursion
          (beginning-of-line 1)
          (looking-at org-babel-result-regexp))
        (progn (org-babel-hide-result-toggle)
               t) ;; to signal that we took action
      nil))) ;; to signal that we did not

(defun org-babel-hide-result-toggle (&optional force)
  "Toggle the visibility of the current result."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward org-babel-result-regexp nil t)
        (let ((start (progn (beginning-of-line 2) (- (point) 1)))
	      (end (progn
		     (while (looking-at org-babel-multi-line-header-regexp)
		       (forward-line 1))
		     (goto-char (- (org-babel-result-end) 1)) (point)))
	      ov)
          (if (memq t (mapcar (lambda (overlay)
                                (eq (overlay-get overlay 'invisible)
				    'org-babel-hide-result))
                              (overlays-at start)))
              (if (or (not force) (eq force 'off))
                  (mapc (lambda (ov)
                          (when (member ov org-babel-hide-result-overlays)
                            (setq org-babel-hide-result-overlays
                                  (delq ov org-babel-hide-result-overlays)))
                          (when (eq (overlay-get ov 'invisible)
                                    'org-babel-hide-result)
                            (delete-overlay ov)))
                        (overlays-at start)))
            (setq ov (make-overlay start end))
            (overlay-put ov 'invisible 'org-babel-hide-result)
            ;; make the block accessible to isearch
            (overlay-put
             ov 'isearch-open-invisible
             (lambda (ov)
               (when (member ov org-babel-hide-result-overlays)
                 (setq org-babel-hide-result-overlays
                       (delq ov org-babel-hide-result-overlays)))
               (when (eq (overlay-get ov 'invisible)
                         'org-babel-hide-result)
                 (delete-overlay ov))))
            (push ov org-babel-hide-result-overlays)))
      (error "Not looking at a result line"))))

;; org-tab-after-check-for-cycling-hook
(add-hook 'org-tab-first-hook 'org-babel-hide-result-toggle-maybe)
;; Remove overlays when changing major mode
(add-hook 'org-mode-hook
	  (lambda () (org-add-hook 'change-major-mode-hook
				   'org-babel-show-result-all 'append 'local)))

(defvar org-file-properties)
(defun org-babel-params-from-properties (&optional lang)
  "Retrieve parameters specified as properties.
Return a list of association lists of source block params
specified in the properties of the current outline entry."
  (save-match-data
    (list
     ;; DEPRECATED header arguments specified as separate property at
     ;; point of definition.
     (org-babel-parse-multiple-vars
      (delq nil
	    (mapcar
	     (lambda (header)
	       (let* ((arg (symbol-name (car header)))
		      (val (org-entry-get (point) arg t)))
		 (and val
		      (cons (intern (concat ":" arg))
			    (org-babel-read val)))))
	     (org-babel-combine-header-arg-lists
	      org-babel-common-header-args-w-values
	      (let ((sym (intern (concat "org-babel-header-args:" lang))))
		(and (boundp sym) (symbol-value sym)))))))
     ;; header arguments specified with the header-args property at
     ;; point of call.
     (org-babel-parse-header-arguments
      (org-entry-get org-babel-current-src-block-location
		     "header-args"
		     'inherit))
     (and lang	 ; language-specific header arguments at point of call
	  (org-babel-parse-header-arguments
	   (org-entry-get org-babel-current-src-block-location
			  (concat "header-args:" lang)
			  'inherit))))))

(defvar org-src-preserve-indentation) ;; declare defcustom from org-src
(defun org-babel-parse-src-block-match ()
  "Parse the results from a match of the `org-babel-src-block-regexp'."
  (let* ((block-indentation (string-width (match-string 1)))
	 (lang (org-match-string-no-properties 2))
         (lang-headers (intern (concat "org-babel-default-header-args:" lang)))
	 (switches (match-string 3))
         (body (let* ((body (org-match-string-no-properties 5))
		      (sub-length (- (length body) 1)))
		 (if (and (> sub-length 0)
			  (string= "\n" (substring body sub-length)))
		     (substring body 0 sub-length)
		   (or body ""))))
	 (preserve-indentation (or org-src-preserve-indentation
				   (save-match-data
				     (string-match "-i\\>" switches)))))
    (list lang
          ;; get block body less properties, protective commas, and indentation
          (with-temp-buffer
            (save-match-data
              (insert (org-unescape-code-in-string body))
	      (unless preserve-indentation (org-do-remove-indentation))
              (buffer-string)))
	  (apply #'org-babel-merge-params
		 org-babel-default-header-args
		 (when (boundp lang-headers) (eval lang-headers))
		 (append
		  (org-babel-params-from-properties lang)
		  (list (org-babel-parse-header-arguments
			 (org-no-properties (or (match-string 4) ""))))))
	  switches
	  block-indentation)))

(defun org-babel-parse-inline-src-block-match ()
  "Parse the results from a match of the `org-babel-inline-src-block-regexp'."
  (let* ((lang (org-no-properties (match-string 2)))
         (lang-headers (intern (concat "org-babel-default-header-args:" lang))))
    (list lang
          (org-unescape-code-in-string (org-no-properties (match-string 5)))
          (apply #'org-babel-merge-params
		 org-babel-default-inline-header-args
		 (if (boundp lang-headers) (eval lang-headers) nil)
		 (append
		  (org-babel-params-from-properties lang)
		  (list (org-babel-parse-header-arguments
			 (org-no-properties (or (match-string 4) ""))))))
	  nil)))

(defun org-babel-balanced-split (string alts)
  "Split STRING on instances of ALTS.
ALTS is a cons of two character options where each option may be
either the numeric code of a single character or a list of
character alternatives.  For example to split on balanced
instances of \"[ \t]:\" set ALTS to ((32 9) . 58)."
  (let* ((matches (lambda (ch spec) (if (listp spec) (member ch spec) (equal spec ch))))
	 (matched (lambda (ch last)
		    (if (consp alts)
			(and (funcall matches ch (cdr alts))
			     (funcall matches last (car alts)))
		      (funcall matches ch alts))))
	 (balance 0) (last 0)
	 quote partial lst)
    (mapc (lambda (ch)  ; split on [], (), "" balanced instances of [ \t]:
	    (setq balance (+ balance
			     (cond ((or (equal 91 ch) (equal 40 ch)) 1)
				   ((or (equal 93 ch) (equal 41 ch)) -1)
				   (t 0))))
	    (when (and (equal 34 ch) (not (equal 92 last)))
	      (setq quote (not quote)))
	    (setq partial (cons ch partial))
	    (when (and (= balance 0) (not quote) (funcall matched ch last))
	      (setq lst (cons (apply #'string (nreverse
					       (if (consp alts)
						   (cddr partial)
						 (cdr partial))))
			      lst))
	      (setq partial nil))
	    (setq last ch))
	  (string-to-list string))
    (nreverse (cons (apply #'string (nreverse partial)) lst))))

(defun org-babel-join-splits-near-ch (ch list)
  "Join splits where \"=\" is on either end of the split."
  (let ((last= (lambda (str) (= ch (aref str (1- (length str))))))
	(first= (lambda (str) (= ch (aref str 0)))))
    (reverse
     (org-reduce (lambda (acc el)
		   (let ((head (car acc)))
		     (if (and head (or (funcall last= head) (funcall first= el)))
			 (cons (concat head el) (cdr acc))
		       (cons el acc))))
		 list :initial-value nil))))

(defun org-babel-parse-header-arguments (arg-string)
  "Parse a string of header arguments returning an alist."
  (when (> (length arg-string) 0)
    (org-babel-parse-multiple-vars
     (delq nil
	   (mapcar
	    (lambda (arg)
	      (if (string-match
		   "\\([^ \f\t\n\r\v]+\\)[ \f\t\n\r\v]+\\([^ \f\t\n\r\v]+.*\\)"
		   arg)
		  (cons (intern (match-string 1 arg))
			(org-babel-read (org-babel-chomp (match-string 2 arg))))
		(cons (intern (org-babel-chomp arg)) nil)))
	    (let ((raw (org-babel-balanced-split arg-string '((32 9) . 58))))
              (cons (car raw) (mapcar (lambda (r) (concat ":" r)) (cdr raw)))))))))

(defun org-babel-parse-multiple-vars (header-arguments)
  "Expand multiple variable assignments behind a single :var keyword.

This allows expression of multiple variables with one :var as
shown below.

#+PROPERTY: var foo=1, bar=2"
  (let (results)
    (mapc (lambda (pair)
	    (if (eq (car pair) :var)
		(mapcar (lambda (v) (push (cons :var (org-babel-trim v)) results))
			(org-babel-join-splits-near-ch
			 61 (org-babel-balanced-split (cdr pair) 32)))
	      (push pair results)))
	  header-arguments)
    (nreverse results)))

(defun org-babel-process-params (params)
  "Expand variables in PARAMS and add summary parameters."
  (let* ((processed-vars (mapcar (lambda (el)
				   (if (consp (cdr el))
				       (cdr el)
				     (org-babel-ref-parse (cdr el))))
				 (org-babel-get-header params :var)))
	 (vars-and-names (if (and (assoc :colname-names params)
				  (assoc :rowname-names params))
			     (list processed-vars)
			   (org-babel-disassemble-tables
			    processed-vars
			    (cdr (assoc :hlines params))
			    (cdr (assoc :colnames params))
			    (cdr (assoc :rownames params)))))
	 (raw-result (or (cdr (assoc :results params)) ""))
	 (result-params (append
			 (split-string (if (stringp raw-result)
					   raw-result
					 (eval raw-result)))
			 (cdr (assoc :result-params params)))))
    (append
     (mapcar (lambda (var) (cons :var var)) (car vars-and-names))
     (list
      (cons :colname-names (or (cdr (assoc :colname-names params))
			       (cadr  vars-and-names)))
      (cons :rowname-names (or (cdr (assoc :rowname-names params))
			       (caddr vars-and-names)))
      (cons :result-params result-params)
      (cons :result-type  (cond ((member "output" result-params) 'output)
				((member "value" result-params) 'value)
				(t 'value))))
     (org-remove-if
      (lambda (x) (memq (car x) '(:colname-names :rowname-names :result-params
						 :result-type :var)))
      params))))

;; row and column names
(defun org-babel-del-hlines (table)
  "Remove all `hline's from TABLE."
  (remq 'hline table))

(defun org-babel-get-colnames (table)
  "Return the column names of TABLE.
Return a cons cell, the `car' of which contains the TABLE less
colnames, and the `cdr' of which contains a list of the column
names."
  (if (equal 'hline (nth 1 table))
      (cons (cddr table) (car table))
    (cons (cdr table) (car table))))

(defun org-babel-get-rownames (table)
  "Return the row names of TABLE.
Return a cons cell, the `car' of which contains the TABLE less
rownames, and the `cdr' of which contains a list of the rownames.
Note: this function removes any hlines in TABLE."
  (let* ((table (org-babel-del-hlines table))
	 (rownames (funcall (lambda ()
			      (let ((tp table))
				(mapcar
				 (lambda (_row)
				   (prog1
				       (pop (car tp))
				     (setq tp (cdr tp))))
				 table))))))
    (cons table rownames)))

(defun org-babel-put-colnames (table colnames)
  "Add COLNAMES to TABLE if they exist."
  (if colnames (apply 'list colnames 'hline table) table))

(defun org-babel-put-rownames (table rownames)
  "Add ROWNAMES to TABLE if they exist."
  (if rownames
      (mapcar (lambda (row)
                (if (listp row)
                    (cons (or (pop rownames) "") row)
                  row)) table)
    table))

(defun org-babel-pick-name (names selector)
  "Select one out of an alist of row or column names.
SELECTOR can be either a list of names in which case those names
will be returned directly, or an index into the list NAMES in
which case the indexed names will be return."
  (if (listp selector)
      selector
    (when names
      (if (and selector (symbolp selector) (not (equal t selector)))
	  (cdr (assoc selector names))
	(if (integerp selector)
	    (nth (- selector 1) names)
	  (cdr (car (last names))))))))

(defun org-babel-disassemble-tables (vars hlines colnames rownames)
  "Parse tables for further processing.
Process the variables in VARS according to the HLINES,
ROWNAMES and COLNAMES header arguments.  Return a list consisting
of the vars, cnames and rnames."
  (let (cnames rnames)
    (list
     (mapcar
      (lambda (var)
        (when (listp (cdr var))
          (when (and (not (equal colnames "no"))
                     (or colnames (and (equal (nth 1 (cdr var)) 'hline)
                                       (not (member 'hline (cddr (cdr var)))))))
            (let ((both (org-babel-get-colnames (cdr var))))
              (setq cnames (cons (cons (car var) (cdr both))
                                 cnames))
              (setq var (cons (car var) (car both)))))
          (when (and rownames (not (equal rownames "no")))
            (let ((both (org-babel-get-rownames (cdr var))))
              (setq rnames (cons (cons (car var) (cdr both))
                                 rnames))
              (setq var (cons (car var) (car both)))))
          (when (and hlines (not (equal hlines "yes")))
            (setq var (cons (car var) (org-babel-del-hlines (cdr var))))))
        var)
      vars)
     (reverse cnames) (reverse rnames))))

(defun org-babel-reassemble-table (table colnames rownames)
  "Add column and row names to a table.
Given a TABLE and set of COLNAMES and ROWNAMES add the names
to the table for reinsertion to org-mode."
  (if (listp table)
      (let ((table (if (and rownames (= (length table) (length rownames)))
                       (org-babel-put-rownames table rownames) table)))
        (if (and colnames (listp (car table)) (= (length (car table))
                                                 (length colnames)))
            (org-babel-put-colnames table colnames) table))
    table))

(defun org-babel-where-is-src-block-head (&optional src-block)
  "Find where the current source block begins.

If optional argument SRC-BLOCK is `src-block' type element, find
its current beginning instead.

Return the point at the beginning of the current source block.
Specifically at the beginning of the #+BEGIN_SRC line.  Also set
match-data relatively to `org-babel-src-block-regexp', which see.
If the point is not on a source block then return nil."
  (let ((element (or src-block (org-element-at-point))))
    (when (eq (org-element-type element) 'src-block)
      (let ((end (org-element-property :end element)))
	(org-with-wide-buffer
	 ;; Ensure point is not on a blank line after the block.
	 (beginning-of-line)
	 (skip-chars-forward " \r\t\n" end)
	 (when (< (point) end)
	   (prog1 (goto-char (org-element-property :post-affiliated element))
	     (looking-at org-babel-src-block-regexp))))))))

;;;###autoload
(defun org-babel-goto-src-block-head ()
  "Go to the beginning of the current code block."
  (interactive)
  (let ((head (org-babel-where-is-src-block-head)))
     (if head (goto-char head) (error "Not currently in a code block"))))

;;;###autoload
(defun org-babel-goto-named-src-block (name)
  "Go to a named source-code block."
  (interactive
   (let ((completion-ignore-case t)
	 (case-fold-search t)
	 (under-point (thing-at-point 'line)))
     (list (org-icompleting-read
	    "source-block name: " (org-babel-src-block-names) nil t
	    (cond
	     ;; noweb
	     ((string-match (org-babel-noweb-wrap) under-point)
	      (let ((block-name (match-string 1 under-point)))
		(string-match "[^(]*" block-name)
		(match-string 0 block-name)))
	     ;; #+call:
	     ((string-match org-babel-lob-one-liner-regexp under-point)
	      (let ((source-info (car (org-babel-lob-get-info))))
		(if (string-match "^\\([^\\[]+?\\)\\(\\[.*\\]\\)?(" source-info)
		    (let ((source-name (match-string 1 source-info)))
		      source-name))))
	     ;; #+results:
	     ((string-match (concat "#\\+" org-babel-results-keyword
				    "\\:\s+\\([^\\(]*\\)") under-point)
	      (match-string 1 under-point))
	     ;; symbol-at-point
	     ((and (thing-at-point 'symbol))
	      (org-babel-find-named-block (thing-at-point 'symbol))
	      (thing-at-point 'symbol))
	     (""))))))
  (let ((point (org-babel-find-named-block name)))
    (if point
        ;; taken from `org-open-at-point'
        (progn (org-mark-ring-push) (goto-char point) (org-show-context))
      (message "source-code block `%s' not found in this buffer" name))))

(defun org-babel-find-named-block (name)
  "Find a named source-code block.
Return the location of the source block identified by source
NAME, or nil if no such block exists.  Set match data according
to `org-babel-named-src-block-regexp'."
  (save-excursion
    (goto-char (point-min))
    (ignore-errors
      (org-next-block 1 nil (org-babel-named-src-block-regexp-for-name name)))))

(defun org-babel-src-block-names (&optional file)
  "Returns the names of source blocks in FILE or the current buffer."
  (when file (find-file file))
  (save-excursion
    (goto-char (point-min))
    (let ((re (org-babel-named-src-block-regexp-for-name))
	  names)
      (while (ignore-errors (org-next-block 1 nil re))
	(push (org-match-string-no-properties 9) names))
      names)))

;;;###autoload
(defun org-babel-goto-named-result (name)
  "Go to a named result."
  (interactive
   (let ((completion-ignore-case t))
     (list (org-icompleting-read "source-block name: "
				 (org-babel-result-names) nil t))))
  (let ((point (org-babel-find-named-result name)))
    (if point
        ;; taken from `org-open-at-point'
        (progn (goto-char point) (org-show-context))
      (message "result `%s' not found in this buffer" name))))

(defun org-babel-find-named-result (name &optional point)
  "Find a named result.
Return the location of the result named NAME in the current
buffer or nil if no such result exists."
  (save-excursion
    (let ((case-fold-search t))
      (goto-char (or point (point-min)))
      (catch 'is-a-code-block
	(when (re-search-forward
	       (concat org-babel-result-regexp
		       "[ \t]" (regexp-quote name) "[ \t]*[\n\f\v\r]")
               nil t)
	  (when (and (string= "name" (downcase (match-string 1)))
		     (or (beginning-of-line 1)
			 (looking-at org-babel-src-block-regexp)
			 (looking-at org-babel-multi-line-header-regexp)
			 (looking-at org-babel-lob-one-liner-regexp)))
	    (throw 'is-a-code-block (org-babel-find-named-result name (point))))
	  (beginning-of-line 0) (point))))))

(defun org-babel-result-names (&optional file)
  "Returns the names of results in FILE or the current buffer."
  (save-excursion
    (when file (find-file file)) (goto-char (point-min))
    (let ((case-fold-search t) names)
      (while (re-search-forward org-babel-result-w-name-regexp nil t)
	(setq names (cons (match-string 4) names)))
      names)))

;;;###autoload
(defun org-babel-next-src-block (&optional arg)
  "Jump to the next source block.
With optional prefix argument ARG, jump forward ARG many source blocks."
  (interactive "p")
  (org-next-block arg nil org-babel-src-block-regexp))

;;;###autoload
(defun org-babel-previous-src-block (&optional arg)
  "Jump to the previous source block.
With optional prefix argument ARG, jump backward ARG many source blocks."
  (interactive "p")
  (org-previous-block arg org-babel-src-block-regexp))

(defvar org-babel-load-languages)

;;;###autoload
(defun org-babel-mark-block ()
  "Mark current src block."
  (interactive)
  (let ((head (org-babel-where-is-src-block-head)))
    (when head
      (save-excursion
        (goto-char head)
        (looking-at org-babel-src-block-regexp))
      (push-mark (match-end 5) nil t)
      (goto-char (match-beginning 5)))))

(defun org-babel-demarcate-block (&optional arg)
  "Wrap or split the code in the region or on the point.
When called from inside of a code block the current block is
split.  When called from outside of a code block a new code block
is created.  In both cases if the region is demarcated and if the
region is not active then the point is demarcated."
  (interactive "P")
  (let* ((info (org-babel-get-src-block-info 'light))
	 (start (org-babel-where-is-src-block-head))
	 (block (and start (match-string 0)))
	 (headers (and start (match-string 4)))
	 (stars (concat (make-string (or (org-current-level) 1) ?*) " "))
	 (lower-case-p (and block
			    (let (case-fold-search)
			      (org-string-match-p "#\\+begin_src" block)))))
    (if info
        (mapc
         (lambda (place)
           (save-excursion
             (goto-char place)
             (let ((lang (nth 0 info))
                   (indent (make-string (nth 5 info) ? )))
	       (when (string-match "^[[:space:]]*$"
				   (buffer-substring (point-at-bol)
						     (point-at-eol)))
		 (delete-region (point-at-bol) (point-at-eol)))
               (insert (concat
			(if (looking-at "^") "" "\n")
			indent (funcall (if lower-case-p 'downcase 'upcase) "#+end_src\n")
			(if arg stars indent) "\n"
			indent (funcall (if lower-case-p 'downcase 'upcase) "#+begin_src ")
			lang
			(if (> (length headers) 1)
			    (concat " " headers) headers)
			(if (looking-at "[\n\r]")
			    ""
			  (concat "\n" (make-string (current-column) ? )))))))
	   (move-end-of-line 2))
         (sort (if (org-region-active-p) (list (mark) (point)) (list (point))) #'>))
      (let ((start (point))
	    (lang (org-icompleting-read
		   "Lang: "
		   (mapcar #'symbol-name
			   (delete-dups
			    (append (mapcar #'car org-babel-load-languages)
				    (mapcar (lambda (el) (intern (car el)))
					    org-src-lang-modes))))))
	    (body (delete-and-extract-region
		   (if (org-region-active-p) (mark) (point)) (point))))
	(insert (concat (if (looking-at "^") "" "\n")
			(if arg (concat stars "\n") "")
			(funcall (if lower-case-p 'downcase 'upcase) "#+begin_src ")
			lang "\n"
			body
			(if (or (= (length body) 0)
				(string-match "[\r\n]$" body)) "" "\n")
			(funcall (if lower-case-p 'downcase 'upcase) "#+end_src\n")))
	(goto-char start) (move-end-of-line 1)))))

(defvar org-babel-lob-one-liner-regexp)
(defun org-babel-where-is-src-block-result (&optional insert info hash indent)
  "Find where the current source block results begin.
Return the point at the beginning of the result of the current
source block.  Specifically at the beginning of the results line.
If no result exists for this block then create a results line
following the source block."
  (save-excursion
    (let* ((case-fold-search t)
	   (on-lob-line (save-excursion
			  (beginning-of-line 1)
			  (looking-at org-babel-lob-one-liner-regexp)))
	   (inlinep (when (org-babel-get-inline-src-block-matches)
		      (match-end 0)))
	   (name (nth 4 (or info (org-babel-get-src-block-info 'light))))
	   (head (unless on-lob-line (org-babel-where-is-src-block-head)))
	   found beg end)
      (when head (goto-char head))
      (org-with-wide-buffer
       (setq
	found ;; was there a result (before we potentially insert one)
	(or
	 inlinep
	 (and
	  ;; named results:
	  ;; - return t if it is found, else return nil
	  ;; - if it does not need to be rebuilt, then don't set end
	  ;; - if it does need to be rebuilt then do set end
	  name (setq beg (org-babel-find-named-result name))
	  (prog1 beg
	    (when (and hash (not (string= hash (match-string 5))))
	      (goto-char beg) (setq end beg) ;; beginning of result
	      (forward-line 1)
	      (delete-region end (org-babel-result-end)) nil)))
	 (and
	  ;; unnamed results:
	  ;; - return t if it is found, else return nil
	  ;; - if it is found, and the hash doesn't match, delete and set end
	  (or on-lob-line (re-search-forward "^[ \t]*#\\+end_src" nil t))
	  (progn (end-of-line 1)
		 (if (eobp) (insert "\n") (forward-char 1))
		 (setq end (point))
		 (and
		  (not name)
		  (progn ;; unnamed results line already exists
		    (catch 'non-comment
		      (while (re-search-forward "[^ \f\t\n\r\v]" nil t)
			(beginning-of-line 1)
			(cond
			 ((looking-at (concat org-babel-result-regexp "\n"))
			  (throw 'non-comment t))
			 ((and (looking-at "^[ \t]*#")
			       (not (looking-at
				     org-babel-lob-one-liner-regexp)))
			  (end-of-line 1))
			 (t (throw 'non-comment nil))))))
		  (let ((this-hash (match-string 5)))
		    (prog1 (point)
		      ;; must remove and rebuild if hash!=old-hash
		      (if (and hash (not (string= hash this-hash)))
			  (progn
			    (setq end (point-at-bol))
			    (forward-line 1)
			    (delete-region end (org-babel-result-end))
			    (setq beg end))
			(setq end nil))))))))))
      (if (not (and insert end)) found
	(goto-char end)
	(unless beg
	  (if (looking-at "[\n\r]") (forward-char 1) (insert "\n")))
	(when (wholenump indent) (indent-to indent))
	(insert (concat
		 "#+" org-babel-results-keyword
		 (when hash
		   (if org-babel-hash-show-time
		       (concat
			"["(format-time-string "<%Y-%m-%d %H:%M:%S>")" "hash"]")
		     (concat "["hash"]")))
		 ":"
		 (when name (concat " " name)) "\n"))
	(unless beg (insert "\n") (backward-char))
	(beginning-of-line 0)
	(when hash (org-babel-hide-hash))
	(point)))))

(defvar org-block-regexp)
(defun org-babel-read-result ()
  "Read the result at `point' into emacs-lisp."
  (let ((case-fold-search t) result-string)
    (cond
     ((org-at-table-p) (org-babel-read-table))
     ((org-at-item-p) (org-babel-read-list))
     ((looking-at org-bracket-link-regexp) (org-babel-read-link))
     ((looking-at org-block-regexp) (org-remove-indentation (match-string 4)))
     ((or (looking-at "^[ \t]*: ") (looking-at "^[ \t]*:$"))
      (setq result-string
	    (org-babel-trim
	     (mapconcat (lambda (line)
                          (or (and (> (length line) 1)
				   (string-match "^[ \t]*: ?\\(.+\\)" line)
				   (match-string 1 line))
			      ""))
			(split-string
			 (buffer-substring
                          (point) (org-babel-result-end)) "[\r\n]+")
			"\n")))
      (or (org-babel-number-p result-string) result-string))
     ((looking-at org-babel-result-regexp)
      (save-excursion (forward-line 1) (org-babel-read-result))))))

(defun org-babel-read-table ()
  "Read the table at `point' into emacs-lisp."
  (mapcar (lambda (row)
            (if (and (symbolp row) (equal row 'hline)) row
              (mapcar (lambda (el) (org-babel-read el 'inhibit-lisp-eval)) row)))
          (org-table-to-lisp)))

(defun org-babel-read-list ()
  "Read the list at `point' into emacs-lisp."
  (mapcar (lambda (el) (org-babel-read el 'inhibit-lisp-eval))
	  (mapcar #'cadr (cdr (org-list-parse-list)))))

(defvar org-link-types-re)
(defun org-babel-read-link ()
  "Read the link at `point' into emacs-lisp.
If the path of the link is a file path it is expanded using
`expand-file-name'."
  (let* ((case-fold-search t)
         (raw (and (looking-at org-bracket-link-regexp)
                   (org-no-properties (match-string 1))))
         (type (and (string-match org-link-types-re raw)
                    (match-string 1 raw))))
    (cond
     ((not type) (expand-file-name raw))
     ((string= type "file")
      (and (string-match "file\\(.*\\):\\(.+\\)" raw)
           (expand-file-name (match-string 2 raw))))
     (t raw))))

(defun org-babel-format-result (result &optional sep)
  "Format RESULT for writing to file."
  (let ((echo-res (lambda (r) (if (stringp r) r (format "%S" r)))))
    (if (listp result)
	;; table result
	(orgtbl-to-generic
	 result (list :sep (or sep "\t") :fmt echo-res))
      ;; scalar result
      (funcall echo-res result))))

(defun org-babel-insert-result
    (result &optional result-params info hash indent lang)
  "Insert RESULT into the current buffer.

By default RESULT is inserted after the end of the current source
block.  The RESULT of an inline source block usually will be
wrapped inside a `results' macro and placed on the same line as
the inline source block.  The macro is stripped upon export.
Multiline and non-scalar RESULTS from inline source blocks are
not allowed.  With optional argument RESULT-PARAMS controls
insertion of results in the Org mode file.  RESULT-PARAMS can
take the following values:

replace - (default option) insert results after the source block
          or inline source block replacing any previously
          inserted results.

silent -- no results are inserted into the Org-mode buffer but
          the results are echoed to the minibuffer and are
          ingested by Emacs (a potentially time consuming
          process).

file ---- the results are interpreted as a file path, and are
          inserted into the buffer using the Org-mode file syntax.

list ---- the results are interpreted as an Org-mode list.

raw ----- results are added directly to the Org-mode file.  This
          is a good option if you code block will output org-mode
          formatted text.

drawer -- results are added directly to the Org-mode file as with
          \"raw\", but are wrapped in a RESULTS drawer or results
          macro, allowing them to later be replaced or removed
          automatically.

org ----- results are added inside of a \"src_org{}\" or \"#+BEGIN_SRC
          org\" block depending on whether the current source block is
          inline or not.  They are not comma-escaped when inserted,
          but Org syntax here will be discarded when exporting the
          file.

html ---- results are added inside of a #+BEGIN_HTML block or
          html export snippet depending on whether the current
          source block is inline or not.  This is a good option
          if your code block will output html formatted text.

latex --- results are added inside of a #+BEGIN_LATEX block or
          latex export snippet depending on whether the current
          source block is inline or not.  This is a good option
          if your code block will output latex formatted text.

code ---- the results are extracted in the syntax of the source
          code of the language being evaluated and are added
          inside of a source block with the source-code language
          set appropriately.  Also, source block inlining is
          preserved in this case.  Note this relies on the
          optional LANG argument.

list ---- the results are rendered as a list.  This option not
          allowed for inline src blocks.

table --- the results are rendered as a table.  This option not
          allowed for inline src blocks.

INFO may provide the values of these header arguments (in the
`header-arguments-alist' see the docstring for
`org-babel-get-src-block-info'):

:file --- the name of the file to which output should be written.

:wrap --- the effect is similar to `latex' in RESULT-PARAMS but
          using the argument supplied to specify the export block
          or snippet type."

  (if (stringp result)
      (progn
        (setq result (org-no-properties result))
        (when (member "file" result-params)
	  (setq result (org-babel-result-to-file
			result (when (assoc :file-desc (nth 2 info))
				 (or (cdr (assoc :file-desc (nth 2 info)))
				     result))))))
    (unless (listp result) (setq result (format "%S" result))))
  (if (and result-params (member "silent" result-params))
      (progn
	(message (replace-regexp-in-string "%" "%%" (format "%S" result)))
	result)
    (save-excursion
      (let* ((inlinep
	      (save-excursion
		(when (or (org-babel-get-inline-src-block-matches)
			  (org-babel-get-lob-one-liner-matches))
		  (goto-char (match-end 0))
		  (org-babel-remove-inline-result)
		  (insert " ")
		  (point))))
	     (existing-result
	      (unless inlinep
		(org-babel-where-is-src-block-result t info hash indent)))
	     (bad-inline-p
	      (when inlinep
		(or
		 (and (member "table" result-params) "`:results table'")
		 (and (listp result) "list result")
		 (and (org-string-match-p "\n." result) "multiline result")
		 (and (member "list" result-params) "`:results list'"))))
	     (results-switches
	      (cdr (assoc :results_switches (nth 2 info))))
	     (visible-beg (point-min-marker))
	     (visible-end (point-max-marker))
	     ;; When results exist outside of the current visible
	     ;; region of the buffer, be sure to widen buffer to
	     ;; update them.
	     (outside-scope-p (and existing-result
				   (or (> visible-beg existing-result)
				       (<= visible-end existing-result))))
	     beg end)
	(when (and (stringp result)  ; ensure results end in a newline
		   (not inlinep)
		   (> (length result) 0)
		   (not (or (string-equal (substring result -1) "\n")
			    (string-equal (substring result -1) "\r"))))
	  (setq result (concat result "\n")))
	(unwind-protect
	    (progn
	      (when outside-scope-p (widen))
	      (if (not existing-result)
		  (setq beg (or inlinep (point)))
		(goto-char existing-result)
		(save-excursion
		  (re-search-forward "#" nil t)
		  (setq indent (- (current-column) 1)))
		(forward-line 1)
		(setq beg (point))
		(cond
		 ((member "replace" result-params)
		  (delete-region (point) (org-babel-result-end)))
		 ((member "append" result-params)
		  (goto-char (org-babel-result-end)) (setq beg (point-marker)))
		 ((member "prepend" result-params)))) ; already there
	      (setq results-switches
		    (if results-switches (concat " " results-switches) ""))
	      (let ((wrap (lambda (start finish &optional no-escape no-newlines
				    inline-start inline-finish)
			    (when inlinep
			      (setq start inline-start)
			      (setq finish inline-finish)
			      (setq no-newlines t))
			    (goto-char end)
			    (insert (concat finish (unless no-newlines "\n")))
			    (goto-char beg)
			    (insert (concat start (unless no-newlines "\n")))
			    (unless no-escape
			      (org-escape-code-in-region (min (point) end) end))
			    (goto-char end)
			    (unless no-newlines (goto-char (point-at-eol)))
			    (setq end (point-marker))))
		    (tabulablep
		     (lambda (r)
		       ;; Non-nil when result R can be turned into
		       ;; a table.
		       (and (listp r)
			    (null (cdr (last r)))
			    (org-every
			     (lambda (e) (or (atom e) (null (cdr (last e)))))
			     result)))))
		;; insert results based on type
		(cond
		 ;; Do nothing for an empty result.
		 ((null result))
		 ;; Illegal inline result or params.
		 (bad-inline-p
		  (error "Inline error: %s cannot be used" bad-inline-p))
		 ;; insert a list if preferred
		 ((member "list" result-params)
		  (insert
		   (org-babel-trim
		    (org-list-to-generic
		     (cons 'unordered
			   (mapcar
			    (lambda (el) (list nil (if (stringp el) el (format "%S" el))))
			    (if (listp result) result (split-string result "\n" t))))
		     '(:splicep nil :istart "- " :iend "\n")))
		   "\n"))
		 ;; Try hard to print RESULT as a table.  Give up if
		 ;; it contains an improper list.
		 ((funcall tabulablep result)
		  (goto-char beg)
		  (insert (concat (orgtbl-to-orgtbl
				   (if (org-every
					(lambda (e)
					  (or (eq e 'hline) (listp e)))
					result)
				       result
				     (list result))
				   nil)
				  "\n"))
		  (goto-char beg)
		  (when (org-at-table-p) (org-table-align))
		  (goto-char (org-table-end)))
		 ;; Print verbatim a list that cannot be turned into
		 ;; a table.
		 ((listp result) (insert (format "%s\n" result)))
		 ((member "file" result-params)
		  (when inlinep
		    (goto-char inlinep)
		    (setq result (org-macro-escape-arguments result)))
		  (insert result))
		 ((and inlinep
		       (not (member "raw" result-params)))
		  (goto-char inlinep)
		  (insert (org-macro-escape-arguments
			   (org-babel-chomp result "\n"))))
		 (t (goto-char beg) (insert result)))
		(setq end (point-marker))
		;; possibly wrap result
		(cond
		 (bad-inline-p)		; Do nothing.
		 ((assoc :wrap (nth 2 info))
		  (let ((name (or (cdr (assoc :wrap (nth 2 info))) "RESULTS")))
		    (funcall wrap (concat "#+BEGIN_" name)
			     (concat "#+END_" (car (org-split-string name)))
			     nil nil (concat "{{{results(@@" name ":") "@@)}}}")))
		 ((member "html" result-params)
		  (funcall wrap "#+BEGIN_HTML" "#+END_HTML" nil nil
			   "{{{results(@@html:" "@@)}}}"))
		 ((member "latex" result-params)
		  (funcall wrap "#+BEGIN_LaTeX" "#+END_LaTeX" nil nil
			   "{{{results(@@latex:" "@@)}}}"))
		 ((member "org" result-params)
		  (goto-char beg) (if (org-at-table-p) (org-cycle))
		  (funcall wrap "#+BEGIN_SRC org" "#+END_SRC" nil nil
			   "{{{results(src_org{" "})}}}"))
		 ((member "code" result-params)
		  (let ((lang (or lang "none")))
		    (funcall wrap (format "#+BEGIN_SRC %s%s" lang results-switches)
			     "#+END_SRC" nil nil
			     (format "{{{results(src_%s[%s]{" lang results-switches)
			     "})}}}")))
		 ((member "raw" result-params)
		  (goto-char beg) (if (org-at-table-p) (org-cycle)))
		 ((or (member "drawer" result-params)
		      ;; Stay backward compatible with <7.9.2
		      (member "wrap" result-params))
		  (goto-char beg) (if (org-at-table-p) (org-cycle))
		  (funcall wrap ":RESULTS:" ":END:" 'no-escape nil
			   "{{{results(" ")}}}"))
		 ((and inlinep (member "file" result-params))
		  (funcall wrap nil nil nil nil "{{{results(" ")}}}"))
		 ((and (not (funcall tabulablep result))
		       (not (member "file" result-params)))
		  (let ((org-babel-inline-result-wrap
			 ;; Hard code {{{results(...)}}} on top of customization.
			 (format "{{{results(%s)}}}" org-babel-inline-result-wrap)))
		    (org-babel-examplify-region beg end results-switches)
		    (setq end (point))))))
	      ;; possibly indent the results to match the #+results line
	      (when (and (not inlinep) (numberp indent) indent (> indent 0)
			 ;; in this case `table-align' does the work for us
			 (not (and (listp result)
				   (member "append" result-params))))
		(indent-rigidly beg end indent))
	      (if (null result)
		  (if (member "value" result-params)
		      (message "Code block returned no value.")
		    (message "Code block produced no output."))
		(message "Code block evaluation complete.")))
	  (when outside-scope-p (narrow-to-region visible-beg visible-end))
	  (set-marker visible-beg nil)
	  (set-marker visible-end nil))))))

(defun org-babel-remove-result (&optional info keep-keyword)
  "Remove the result of the current source block."
  (interactive)
  (let ((location (org-babel-where-is-src-block-result nil info)))
    (when location
      (save-excursion
        (goto-char location)
	(when (looking-at (concat org-babel-result-regexp ".*$"))
	  (delete-region
	   (if keep-keyword (1+ (match-end 0)) (1- (match-beginning 0)))
	   (progn (forward-line 1) (org-babel-result-end))))))))

(defun org-babel-remove-inline-result ()
  "Remove the result of the current inline-src-block or babel call.
The result must be wrapped in a `results' macro to be removed.
Leading whitespace is trimmed."
  (interactive)
  (let* ((el (org-element-context))
	 (post-blank (org-element-property :post-blank el)))
    (when (memq (org-element-type el) '(inline-src-block inline-babel-call))
      (org-with-wide-buffer
        (goto-char (org-element-property :end el))
        (let ((el (org-element-context)))
	  (when (and (eq (org-element-type el) 'macro)
		     (string= (org-element-property :key el) "results"))
	    (delete-region ; And leading whitespace.
	     (- (org-element-property :begin el) post-blank)
	     (- (org-element-property :end el)
		(org-element-property :post-blank el)))))))))

(defun org-babel-remove-result-one-or-many (x)
  "Remove the result of the current source block.
If called with a prefix argument, remove all result blocks
in the buffer."
  (interactive "P")
  (if x
      (org-babel-map-src-blocks nil (org-babel-remove-result))
    (org-babel-remove-result)))

(defun org-babel-result-end ()
  "Return the point at the end of the current set of results."
  (save-excursion
    (cond
     ((org-at-table-p) (progn (goto-char (org-table-end)) (point)))
     ((org-at-item-p) (let* ((struct (org-list-struct))
			     (prvs (org-list-prevs-alist struct)))
			(org-list-get-list-end (point-at-bol) struct prvs)))
     ((let ((case-fold-search t)) (looking-at "^\\([ \t]*\\):results:"))
      (progn (re-search-forward (concat "^" (match-string 1) ":END:"))
	     (forward-char 1) (point)))
     (t
      (let ((case-fold-search t))
	(if (looking-at (concat "[ \t]*#\\+begin_\\([^ \t\n\r]+\\)"))
	    (progn (re-search-forward (concat "[ \t]*#\\+end_" (match-string 1))
				      nil t)
		   (forward-char 1))
	  (while (looking-at "[ \t]*\\(: \\|:$\\|\\[\\[\\)")
	    (forward-line 1))))
      (point)))))

(defun org-babel-result-to-file (result &optional description)
  "Convert RESULT into an `org-mode' link with optional DESCRIPTION.
If the `default-directory' is different from the containing
file's directory then expand relative links."
  (when (stringp result)
    (format "[[file:%s]%s]"
	    (if (and default-directory
		     buffer-file-name
		     (not (string= (expand-file-name default-directory)
				   (expand-file-name
				    (file-name-directory buffer-file-name)))))
		(expand-file-name result default-directory)
	      result)
	    (if description (concat "[" description "]") ""))))

(defvar org-babel-capitalize-example-region-markers nil
  "Make true to capitalize begin/end example markers inserted by code blocks.")

(define-obsolete-function-alias
  'org-babel-examplize-region
  'org-babel-examplify-region "25.1")

(defun org-babel-examplify-region (beg end &optional results-switches)
  "Comment out region using the inline `==' or `: ' org example quote."
  (interactive "*r")
  (let ((chars-between (lambda (b e)
			 (not (string-match "^[\\s]*$"
					    (buffer-substring b e)))))
	(maybe-cap (lambda (str) (if org-babel-capitalize-example-region-markers
				(upcase str) str)))
	(beg-bol (save-excursion (goto-char beg) (point-at-bol)))
	(end-bol (save-excursion (goto-char end) (point-at-bol)))
	(end-eol (save-excursion (goto-char end) (point-at-eol))))
    (if (and (not (= end end-bol))
	     (or (funcall chars-between beg-bol beg)
		 (funcall chars-between end end-eol)))
	(save-excursion
	  (goto-char beg)
	  (insert (format org-babel-inline-result-wrap
			  (prog1 (buffer-substring beg end)
			    (delete-region beg end)))))
      (let ((size (count-lines beg end)))
	(save-excursion
	  (cond ((= size 0))	      ; do nothing for an empty result
		((< size org-babel-min-lines-for-block-output)
		 (goto-char beg)
		 (dotimes (n size)
		   (beginning-of-line 1) (insert ": ") (forward-line 1)))
		(t
		 (goto-char beg)
		 (insert (if results-switches
			     (format "%s%s\n"
				     (funcall maybe-cap "#+begin_example")
				     results-switches)
			   (funcall maybe-cap "#+begin_example\n")))
		 (if (markerp end) (goto-char end) (forward-char (- end beg)))
		 (insert (funcall maybe-cap "#+end_example\n")))))))))

(defun org-babel-update-block-body (new-body)
  "Update the body of the current code block to NEW-BODY."
  (let ((element (org-element-at-point)))
    (unless (eq (org-element-type element) 'src-block)
      (error "Not in a source block"))
    (goto-char (org-babel-where-is-src-block-head element))
    (let* ((ind (org-get-indentation))
	   (body-start (line-beginning-position 2))
	   (body (org-element-normalize-string
		  (if (or org-src-preserve-indentation
			  (org-element-property :preserve-indent element))
		      new-body
		    (with-temp-buffer
		      (insert (org-remove-indentation new-body))
		      (indent-rigidly
		       (point-min)
		       (point-max)
		       (+ ind org-edit-src-content-indentation))
		      (buffer-string))))))
      (delete-region body-start
		     (org-with-wide-buffer
		      (goto-char (org-element-property :end element))
		      (skip-chars-backward " \t\n")
		      (line-beginning-position)))
      (goto-char body-start)
      (insert body))))

(defun org-babel-merge-params (&rest plists)
  "Combine all parameter association lists in PLISTS.
Later elements of PLISTS override the values of previous elements.
This takes into account some special considerations for certain
parameters when merging lists."
  (let* ((results-exclusive-groups
	  (mapcar (lambda (group) (mapcar #'symbol-name group))
		  (cdr (assoc 'results org-babel-common-header-args-w-values))))
	 (exports-exclusive-groups
	  (mapcar (lambda (group) (mapcar #'symbol-name group))
		  (cdr (assoc 'exports org-babel-common-header-args-w-values))))
	 (variable-index 0)
	 (e-merge (lambda (exclusive-groups &rest result-params)
		    ;; maintain exclusivity of mutually exclusive parameters
		    (let (output)
		      (mapc (lambda (new-params)
			      (mapc (lambda (new-param)
				      (mapc (lambda (exclusive-group)
					      (when (member new-param exclusive-group)
						(mapcar (lambda (excluded-param)
							  (setq output
								(delete
								 excluded-param
								 output)))
							exclusive-group)))
					    exclusive-groups)
				      (setq output (org-uniquify
						    (cons new-param output))))
				    new-params))
			    result-params)
		      output)))
	 params results exports tangle noweb cache vars shebang comments padline
	 clearnames)

    (mapc
     (lambda (plist)
       (mapc
	(lambda (pair)
	  (case (car pair)
	    (:var
	     (let ((name (if (listp (cdr pair))
			     (cadr pair)
			   (and (string-match "^\\([^= \f\t\n\r\v]+\\)[ \t]*="
					      (cdr pair))
				(intern (match-string 1 (cdr pair)))))))
	       (if name
		   (setq vars
			 (append
			  (if (member name (mapcar #'car vars))
			      (progn
				(push name clearnames)
				(delq nil
				      (mapcar
				       (lambda (p)
					 (unless (equal (car p) name) p))
				       vars)))
			    vars)
			  (list (cons name pair))))
		 ;; if no name is given and we already have named variables
		 ;; then assign to named variables in order
		 (if (and vars (nth variable-index vars))
		     (let ((name (car (nth variable-index vars))))
		       (push name clearnames) ; clear out colnames
					      ; and rownames
					      ; for replace vars
		       (prog1 (setf (cddr (nth variable-index vars))
				    (concat (symbol-name name) "=" (cdr pair)))
			 (incf variable-index)))
		   (error "Variable \"%s\" must be assigned a default value"
			  (cdr pair))))))
	    (:results
	     (setq results (funcall e-merge results-exclusive-groups
				    results
				    (split-string
				     (let ((r (cdr pair)))
				       (if (stringp r) r (eval r)))))))
	    (:file
	     (when (cdr pair)
	       (setq results (funcall e-merge results-exclusive-groups
				      results '("file")))
	       (unless (or (member "both" exports)
			   (member "none" exports)
			   (member "code" exports))
		 (setq exports (funcall e-merge exports-exclusive-groups
					exports '("results"))))
	       (setq params (cons pair (assq-delete-all (car pair) params)))))
	    (:file-ext
	     (when (cdr pair)
	       (setq results (funcall e-merge results-exclusive-groups
				      results '("file")))
	       (unless (or (member "both" exports)
			   (member "none" exports)
			   (member "code" exports))
		 (setq exports (funcall e-merge exports-exclusive-groups
					exports '("results"))))
	       (setq params (cons pair (assq-delete-all (car pair) params)))))
	    (:exports
	     (setq exports (funcall e-merge exports-exclusive-groups
				    exports
				    (split-string (or (cdr pair) "")))))
	    (:tangle ;; take the latest -- always overwrite
	     (setq tangle (or (list (cdr pair)) tangle)))
	    (:noweb
	     (setq noweb (funcall e-merge
				  '(("yes" "no" "tangle" "no-export"
				     "strip-export" "eval"))
				  noweb
				  (split-string (or (cdr pair) "")))))
	    (:cache
	     (setq cache (funcall e-merge '(("yes" "no")) cache
				  (split-string (or (cdr pair) "")))))
	    (:padline
	     (setq padline (funcall e-merge '(("yes" "no")) padline
				    (split-string (or (cdr pair) "")))))
	    (:shebang ;; take the latest -- always overwrite
	     (setq shebang (or (list (cdr pair)) shebang)))
	    (:comments
	     (setq comments (funcall e-merge '(("yes" "no")) comments
				     (split-string (or (cdr pair) "")))))
	    (t ;; replace: this covers e.g. :session
	     (setq params (cons pair (assq-delete-all (car pair) params))))))
	plist))
     plists)
    (setq vars (reverse vars))
    (while vars (setq params (cons (cons :var (cddr (pop vars))) params)))
    ;; clear out col-names and row-names for replaced variables
    (mapc
     (lambda (name)
       (mapc
	(lambda (param)
	  (when (assoc param params)
	    (setf (cdr (assoc param params))
		  (org-remove-if (lambda (pair) (equal (car pair) name))
				 (cdr (assoc param params))))
	    (setf params (org-remove-if (lambda (pair) (and (equal (car pair) param)
						       (null (cdr pair))))
					params))))
	(list :colname-names :rowname-names)))
     clearnames)
    (mapc
     (lambda (hd)
       (let ((key (intern (concat ":" (symbol-name hd))))
	     (val (eval hd)))
	 (setf params (cons (cons key (mapconcat 'identity val " ")) params))))
     '(results exports tangle noweb padline cache shebang comments))
    params))

(defvar org-babel-use-quick-and-dirty-noweb-expansion nil
  "Set to true to use regular expressions to expand noweb references.
This results in much faster noweb reference expansion but does
not properly allow code blocks to inherit the \":noweb-ref\"
header argument from buffer or subtree wide properties.")

(defun org-babel-noweb-p (params context)
  "Check if PARAMS require expansion in CONTEXT.
CONTEXT may be one of :tangle, :export or :eval."
  (let* (intersect
	 (intersect (lambda (as bs)
		      (when as
			(if (member (car as) bs)
			    (car as)
			  (funcall intersect (cdr as) bs))))))
    (funcall intersect (case context
			 (:tangle '("yes" "tangle" "no-export" "strip-export"))
			 (:eval   '("yes" "no-export" "strip-export" "eval"))
			 (:export '("yes")))
	     (split-string (or (cdr (assoc :noweb params)) "")))))

(defun org-babel-expand-noweb-references (&optional info parent-buffer)
  "Expand Noweb references in the body of the current source code block.

For example the following reference would be replaced with the
body of the source-code block named `example-block'.

<<example-block>>

Note that any text preceding the <<foo>> construct on a line will
be interposed between the lines of the replacement text.  So for
example if <<foo>> is placed behind a comment, then the entire
replacement text will also be commented.

This function must be called from inside of the buffer containing
the source-code block which holds BODY.

In addition the following syntax can be used to insert the
results of evaluating the source-code block named `example-block'.

<<example-block()>>

Any optional arguments can be passed to example-block by placing
the arguments inside the parenthesis following the convention
defined by `org-babel-lob'.  For example

<<example-block(a=9)>>

would set the value of argument \"a\" equal to \"9\".  Note that
these arguments are not evaluated in the current source-code
block but are passed literally to the \"example-block\"."
  (let* ((parent-buffer (or parent-buffer (current-buffer)))
         (info (or info (org-babel-get-src-block-info 'light)))
         (lang (nth 0 info))
         (body (nth 1 info))
	 (ob-nww-start org-babel-noweb-wrap-start)
	 (ob-nww-end org-babel-noweb-wrap-end)
	 (comment (string= "noweb" (cdr (assoc :comments (nth 2 info)))))
	 (rx-prefix (concat "\\(" org-babel-src-name-regexp "\\|"
			    ":noweb-ref[ \t]+" "\\)"))
         (new-body "")
	 (nb-add (lambda (text) (setq new-body (concat new-body text))))
	 (c-wrap (lambda (text)
		   (with-temp-buffer
		     (funcall (intern (concat lang "-mode")))
		     (comment-region (point) (progn (insert text) (point)))
		     (org-babel-trim (buffer-string)))))
	 index source-name evaluate prefix)
    (with-temp-buffer
      (org-set-local 'org-babel-noweb-wrap-start ob-nww-start)
      (org-set-local 'org-babel-noweb-wrap-end ob-nww-end)
      (insert body) (goto-char (point-min))
      (setq index (point))
      (while (and (re-search-forward (org-babel-noweb-wrap) nil t))
	(save-match-data (setf source-name (match-string 1)))
	(save-match-data (setq evaluate (string-match "(.*)" source-name)))
	(save-match-data
	  (setq prefix
		(buffer-substring (match-beginning 0)
				  (save-excursion
				    (beginning-of-line 1) (point)))))
	;; add interval to new-body (removing noweb reference)
	(goto-char (match-beginning 0))
	(funcall nb-add (buffer-substring index (point)))
	(goto-char (match-end 0))
	(setq index (point))
	(funcall
         nb-add
         (with-current-buffer parent-buffer
           (save-restriction
             (widen)
             (mapconcat ;; Interpose PREFIX between every line.
              #'identity
              (split-string
               (if evaluate
                   (let ((raw (org-babel-ref-resolve source-name)))
                     (if (stringp raw) raw (format "%S" raw)))
                 (or
                  ;; Retrieve from the library of babel.
                  (nth 2 (assoc (intern source-name)
                                org-babel-library-of-babel))
                  ;; Return the contents of headlines literally.
                  (save-excursion
                    (when (org-babel-ref-goto-headline-id source-name)
			      (org-babel-ref-headline-body)))
                  ;; Find the expansion of reference in this buffer.
                  (let ((rx (concat rx-prefix source-name "[ \t\n]"))
                        expansion)
                    (save-excursion
                      (goto-char (point-min))
                      (if org-babel-use-quick-and-dirty-noweb-expansion
                          (while (re-search-forward rx nil t)
                            (let* ((i (org-babel-get-src-block-info 'light))
                                   (body (org-babel-expand-noweb-references i))
                                   (sep (or (cdr (assoc :noweb-sep (nth 2 i)))
                                            "\n"))
                                   (full (if comment
                                             (let ((cs (org-babel-tangle-comment-links i)))
                                                (concat (funcall c-wrap (car cs)) "\n"
                                                        body "\n"
                                                        (funcall c-wrap (cadr cs))))
                                           body)))
                              (setq expansion (cons sep (cons full expansion)))))
                        (org-babel-map-src-blocks nil
			  (let ((i (org-babel-get-src-block-info 'light)))
                            (when (equal (or (cdr (assoc :noweb-ref (nth 2 i)))
                                             (nth 4 i))
                                         source-name)
                              (let* ((body (org-babel-expand-noweb-references i))
                                     (sep (or (cdr (assoc :noweb-sep (nth 2 i)))
                                              "\n"))
                                     (full (if comment
                                               (let ((cs (org-babel-tangle-comment-links i)))
                                                  (concat (funcall c-wrap (car cs)) "\n"
                                                          body "\n"
                                                          (funcall c-wrap (cadr cs))))
                                             body)))
                                (setq expansion
                                      (cons sep (cons full expansion)))))))))
                    (and expansion
                         (mapconcat #'identity (nreverse (cdr expansion)) "")))
                  ;; Possibly raise an error if named block doesn't exist.
                  (if (or org-babel-noweb-error-all-langs
			  (member lang org-babel-noweb-error-langs))
                      (error "%s" (concat
                                   (org-babel-noweb-wrap source-name)
                                   "could not be resolved (see "
                                   "`org-babel-noweb-error-langs')"))
                    "")))
               "[\n\r]") (concat "\n" prefix))))))
      (funcall nb-add (buffer-substring index (point-max))))
    new-body))

(defun org-babel--script-escape-inner (str)
  (let (in-single in-double backslash out)
    (mapc
     (lambda (ch)
       (setq
	out
	(if backslash
	    (progn
	      (setq backslash nil)
	      (cond
	       ((and in-single (eq ch ?'))
		;; Escaped single quote inside single quoted string:
		;; emit just a single quote, since we've changed the
		;; outer quotes to double.
		(cons ch out))
	       ((eq ch ?\")
		;; Escaped double quote
		(if in-single
		    ;; This should be interpreted as backslash+quote,
		    ;; not an escape.  Emit a three backslashes
		    ;; followed by a quote (because one layer of
		    ;; quoting will be stripped by `org-babel-read').
		    (append (list ch ?\\ ?\\ ?\\) out)
		  ;; Otherwise we are in a double-quoted string.  Emit
		  ;; a single escaped quote
		  (append (list ch ?\\) out)))
	       ((eq ch ?\\)
		;; Escaped backslash: emit a single escaped backslash
		(append (list ?\\ ?\\) out))
	       ;; Other: emit a quoted backslash followed by whatever
	       ;; the character was (because one layer of quoting will
	       ;; be stripped by `org-babel-read').
	       (t (append (list ch ?\\ ?\\) out))))
	  (case ch
	    (?\[ (if (or in-double in-single)
		     (cons ?\[ out)
		   (cons ?\( out)))
	    (?\] (if (or in-double in-single)
		     (cons ?\] out)
		   (cons ?\) out)))
	    (?\{ (if (or in-double in-single)
		     (cons ?\{ out)
		   (cons ?\( out)))
	    (?\} (if (or in-double in-single)
		     (cons ?\} out)
		   (cons ?\) out)))
	    (?, (if (or in-double in-single)
		    (cons ?, out) (cons ?\s out)))
	    (?\' (if in-double
		     (cons ?\' out)
		   (setq in-single (not in-single)) (cons ?\" out)))
	    (?\" (if in-single
		     (append (list ?\" ?\\) out)
		   (setq in-double (not in-double)) (cons ?\" out)))
	    (?\\ (unless (or in-single in-double)
		   (error "Can't handle backslash outside string in `org-babel-script-escape'"))
		 (setq backslash t)
		 out)
	    (t  (cons ch out))))))
     (string-to-list str))
    (when (or in-single in-double)
      (error "Unterminated string in `org-babel-script-escape'"))
    (apply #'string (reverse out))))

(defun org-babel-script-escape (str &optional force)
  "Safely convert tables into elisp lists."
  (unless (stringp str)
    (error "`org-babel-script-escape' expects a string"))
  (let ((escaped
	 (cond
	  ((and (> (length str) 2)
		(or (and (string-equal "[" (substring str 0 1))
			 (string-equal "]" (substring str -1)))
		    (and (string-equal "{" (substring str 0 1))
			 (string-equal "}" (substring str -1)))
		    (and (string-equal "(" (substring str 0 1))
			 (string-equal ")" (substring str -1)))))

	   (concat "'" (org-babel--script-escape-inner str)))
	  ((or force
	       (and (> (length str) 2)
		    (or (and (string-equal "'" (substring str 0 1))
			     (string-equal "'" (substring str -1)))
			;; We need to pass double-quoted strings
			;; through the backslash-twiddling bits, even
			;; though we don't need to change their
			;; delimiters.
			(and (string-equal "\"" (substring str 0 1))
			     (string-equal "\"" (substring str -1))))))
	   (org-babel--script-escape-inner str))
	  (t str))))
    (condition-case nil (org-babel-read escaped) (error escaped))))

(defun org-babel-read (cell &optional inhibit-lisp-eval)
  "Convert the string value of CELL to a number if appropriate.
Otherwise if CELL looks like lisp (meaning it starts with a
\"(\", \"\\='\", \"\\=`\" or a \"[\") then read and evaluate it as
lisp, otherwise return it unmodified as a string.  Optional
argument INHIBIT-LISP-EVAL inhibits lisp evaluation for
situations in which is it not appropriate."
  (if (and (stringp cell) (not (equal cell "")))
      (or (org-babel-number-p cell)
          (if (and (not inhibit-lisp-eval)
		   (or (member (substring cell 0 1) '("(" "'" "`" "["))
		       (string= cell "*this*")))
              (eval (read cell))
            (if (string= (substring cell 0 1) "\"")
		(read cell)
	      (progn (set-text-properties 0 (length cell) nil cell) cell))))
    cell))

(defun org-babel-number-p (string)
  "If STRING represents a number return its value."
  (if (and (string-match "[0-9]+" string)
	   (string-match "^-?[0-9]*\\.?[0-9]*$" string)
           (= (length (substring string (match-beginning 0)
				 (match-end 0)))
	      (length string)))
      (string-to-number string)))

(defun org-babel-import-elisp-from-file (file-name &optional separator)
  "Read the results located at FILE-NAME into an elisp table.
If the table is trivial, then return it as a scalar."
  (let (result)
    (save-window-excursion
      (with-temp-buffer
	(condition-case err
	    (progn
	      (org-table-import file-name separator)
	      (delete-file file-name)
	      (setq result (mapcar (lambda (row)
				     (mapcar #'org-babel-string-read row))
				   (org-table-to-lisp))))
	  (error (message "Error reading results: %s" err) nil)))
      (if (null (cdr result)) ;; if result is trivial vector, then scalarize it
	  (if (consp (car result))
	      (if (null (cdr (car result)))
		  (caar result)
		result)
	    (car result))
	result))))

(defun org-babel-string-read (cell)
  "Strip nested \"s from around strings."
  (org-babel-read (or (and (stringp cell)
                           (string-match "\\\"\\(.+\\)\\\"" cell)
                           (match-string 1 cell))
                      cell) t))

(defun org-babel-chomp (string &optional regexp)
  "Strip a trailing space or carriage return from STRING.
The default regexp used is \"[ \\f\\t\\n\\r\\v]\" but another one
can be specified as the REGEXP argument."
  (let ((regexp (or regexp "[ \f\t\n\r\v]")))
    (while (and (> (length string) 0)
                (string-match regexp (substring string -1)))
      (setq string (substring string 0 -1)))
    string))

(defun org-babel-trim (string &optional regexp)
  "Strip a leading and trailing space or carriage return from STRING.
Like `org-babel-chomp', but run on both the first and last
character of the string."
  (org-babel-chomp
   (org-reverse-string
    (org-babel-chomp (org-reverse-string string) regexp)) regexp))

(defun org-babel-tramp-handle-call-process-region
  (start end program &optional delete buffer display &rest args)
  "Use Tramp to handle `call-process-region'.
Fixes a bug in `tramp-handle-call-process-region'."
  (if (and (featurep 'tramp) (file-remote-p default-directory))
      (let ((tmpfile (tramp-compat-make-temp-file "")))
	(write-region start end tmpfile)
	(when delete (delete-region start end))
	(unwind-protect
	    ;;	(apply 'call-process program tmpfile buffer display args)
            ;; bug in tramp
	    (apply 'process-file program tmpfile buffer display args)
	  (delete-file tmpfile)))
    ;; org-babel-call-process-region-original is the original emacs
    ;; definition.  It is in scope from the let binding in
    ;; org-babel-execute-src-block
    (apply org-babel-call-process-region-original
           start end program delete buffer display args)))

(defun org-babel-local-file-name (file)
  "Return the local name component of FILE."
  (or (file-remote-p file 'localname) file))

(defun org-babel-process-file-name (name &optional no-quote-p)
  "Prepare NAME to be used in an external process.
If NAME specifies a remote location, the remote portion of the
name is removed, since in that case the process will be executing
remotely.  The file name is then processed by `expand-file-name'.
Unless second argument NO-QUOTE-P is non-nil, the file name is
additionally processed by `shell-quote-argument'"
  (let ((f (org-babel-local-file-name (expand-file-name name))))
    (if no-quote-p f (shell-quote-argument f))))

(defvar org-babel-temporary-directory)
(unless (or noninteractive (boundp 'org-babel-temporary-directory))
  (defvar org-babel-temporary-directory
    (or (and (boundp 'org-babel-temporary-directory)
	     (file-exists-p org-babel-temporary-directory)
	     org-babel-temporary-directory)
	(make-temp-file "babel-" t))
    "Directory to hold temporary files created to execute code blocks.
Used by `org-babel-temp-file'.  This directory will be removed on
Emacs shutdown."))

(defcustom org-babel-remote-temporary-directory "/tmp/"
  "Directory to hold temporary files on remote hosts."
  :group 'org-babel
  :type 'string)

(defmacro org-babel-result-cond (result-params scalar-form &rest table-forms)
  "Call the code to parse raw string results according to RESULT-PARAMS."
  (declare (indent 1)
	   (debug (form form &rest form)))
  (org-with-gensyms (params)
    `(let ((,params ,result-params))
       (unless (member "none" ,params)
	 (if (or (member "scalar" ,params)
		 (member "verbatim" ,params)
		 (member "html" ,params)
		 (member "code" ,params)
		 (member "pp" ,params)
		 (member "file" ,params)
		 (and (or (member "output" ,params)
			  (member "raw"    ,params)
			  (member "org"    ,params)
			  (member "drawer" ,params))
		      (not (member "table" ,params))))
	     ,scalar-form
	   ,@table-forms)))))
(def-edebug-spec org-babel-result-cond (form form body))

(defun org-babel-temp-file (prefix &optional suffix)
  "Create a temporary file in the `org-babel-temporary-directory'.
Passes PREFIX and SUFFIX directly to `make-temp-file' with the
value of `temporary-file-directory' temporarily set to the value
of `org-babel-temporary-directory'."
  (if (file-remote-p default-directory)
      (let ((prefix
             (concat (file-remote-p default-directory)
                     (expand-file-name
		      prefix org-babel-remote-temporary-directory))))
        (make-temp-file prefix nil suffix))
    (let ((temporary-file-directory
	   (or (and (boundp 'org-babel-temporary-directory)
		    (file-exists-p org-babel-temporary-directory)
		    org-babel-temporary-directory)
	       temporary-file-directory)))
      (make-temp-file prefix nil suffix))))

(defun org-babel-remove-temporary-directory ()
  "Remove `org-babel-temporary-directory' on Emacs shutdown."
  (when (and (boundp 'org-babel-temporary-directory)
	     (file-exists-p org-babel-temporary-directory))
    ;; taken from `delete-directory' in files.el
    (condition-case nil
	(progn
	  (mapc (lambda (file)
		  ;; This test is equivalent to
		  ;; (and (file-directory-p fn) (not (file-symlink-p fn)))
		  ;; but more efficient
		  (if (eq t (car (file-attributes file)))
		      (delete-directory file)
		    (delete-file file)))
		;; We do not want to delete "." and "..".
		(directory-files org-babel-temporary-directory 'full
				 "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*"))
	  (delete-directory org-babel-temporary-directory))
      (error
       (message "Failed to remove temporary Org-babel directory %s"
		(if (boundp 'org-babel-temporary-directory)
		    org-babel-temporary-directory
		  "[directory not defined]"))))))

(add-hook 'kill-emacs-hook 'org-babel-remove-temporary-directory)

(defun org-babel-one-header-arg-safe-p (pair safe-list)
  "Determine if the PAIR is a safe babel header arg according to SAFE-LIST.

For the format of SAFE-LIST, see `org-babel-safe-header-args'."
  (and (consp pair)
       (keywordp (car pair))
       (stringp (cdr pair))
       (or
	(memq (car pair) safe-list)
	(let ((entry (assq (car pair) safe-list)))
	  (and entry
	       (consp entry)
	       (cond ((functionp (cdr entry))
		       (funcall (cdr entry) (cdr pair)))
		     ((listp (cdr entry))
		      (member (cdr pair) (cdr entry)))
		     (t nil)))))))

(defun org-babel-generate-file-param (src-name params)
  "Calculate the filename for source block results.

The directory is calculated from the :output-dir property of the
source block; if not specified, use the current directory.

If the source block has a #+NAME and the :file parameter does not
contain any period characters, then the :file parameter is
treated as an extension, and the output file name is the
concatenation of the directory (as calculated above), the block
name, a period, and the parameter value as a file extension.
Otherwise, the :file parameter is treated as a full file name,
and the output file name is the directory (as calculated above)
plus the parameter value."
  (let* ((file-cons (assq :file params))
	   (file-ext-cons (assq :file-ext params))
	   (file-ext (cdr-safe file-ext-cons))
	   (dir (cdr-safe (assq :output-dir params)))
	   fname)
    ;; create the output-dir if it does not exist
    (when dir
      (make-directory dir t))
    (if file-cons
	;; :file given; add :output-dir if given
	(when dir
	  (setcdr file-cons (concat (file-name-as-directory dir) (cdr file-cons))))
      ;; :file not given; compute from name and :file-ext if possible
      (when (and src-name file-ext)
	(if dir
	    (setq fname (concat (file-name-as-directory (or dir ""))
				src-name "." file-ext))
	  (setq fname (concat src-name "." file-ext)))
	(setq params (cons (cons :file fname) params))))
    params))

;;; Used by backends: R, Maxima, Octave.
(defun org-babel-graphical-output-file (params)
  "File where a babel block should send graphical output, per PARAMS."
  (unless (assq :file params)
    (if (assq :file-ext params)
	(user-error ":file-ext given but no :file generated; did you forget to give a block a #+NAME?")
      (user-error "No :file header argument given; cannot create graphical result.")))
  (and (member "graphics" (cdr (assq :result-params params)))
       (cdr (assq :file params))))

(provide 'ob-core)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ob-core.el ends here
