;;;; org-test.el --- Tests for Org

;; Copyright (c) 2010-2015 Sebastian Rose, Eric Schulte
;; Authors:
;;     Sebastian Rose, Hannover, Germany, sebastian_rose gmx de
;;     Eric Schulte, Santa Fe, New Mexico, USA, schulte.eric gmail com
;;     David Maus, Brunswick, Germany, dmaus ictsoc de

;; Released under the GNU General Public License version 3
;; see: http://www.gnu.org/licenses/gpl-3.0.html

;; Definition of `special-mode' copied from Emacs23's simple.el to be
;; provide a testing environment for Emacs22.

;;;; Comments:

;; Interactive testing for Org mode.

;; The heart of all this is the commands `org-test-current-defun'.  If
;; called while in a `defun' all ert tests with names matching the
;; name of the function are run.

;;; Test Development
;; For test development purposes a number of navigation and test
;; function construction routines are available as a git submodule
;; (jump.el)
;; Install with...
;; $ git submodule init
;; $ git submodule update


;;;; Code:

;;; Ob constants

(defconst org-test-file-ob-anchor
  "94839181-184f-4ff4-a72f-94214df6f5ba")

(defconst org-test-link-in-heading-file-ob-anchor
  "a8b1d111-eca8-49f0-8930-56d4f0875155")

(unless (and (boundp 'org-batch-test) org-batch-test)
  (let* ((org-test-dir (expand-file-name
			(file-name-directory
			 (or load-file-name buffer-file-name))))
	 (org-lisp-dir (expand-file-name
			(concat org-test-dir "../lisp"))))

    (unless (featurep 'org)
      (setq load-path (cons org-lisp-dir load-path))
      (require 'org)
      (require 'org-id)
      (require 'ox)
      (org-babel-do-load-languages
       'org-babel-load-languages '((shell . t) (org . t))))

    (let ((load-path (cons org-test-dir
			   (cons (expand-file-name "jump" org-test-dir)
				 load-path))))
      (require 'cl-lib)
      (require 'ert)
      (require 'ert-x)
      (when (file-exists-p (expand-file-name "jump/jump.el" org-test-dir))
	(require 'jump)
	(require 'which-func)))))

(defconst org-test-default-test-file-name "tests.el"
  "For each defun a separate file with tests may be defined.
tests.el is the fallback or default if you like.")

(defconst org-test-default-directory-name "testing"
  "Basename or the directory where the tests live.
org-test searches this directory up the directory tree.")

(defconst org-test-dir
  (expand-file-name (file-name-directory (or load-file-name buffer-file-name))))

(defconst org-base-dir
  (expand-file-name ".." org-test-dir))

(defconst org-test-example-dir
  (expand-file-name "examples" org-test-dir))

(defconst org-test-file
  (expand-file-name "normal.org" org-test-example-dir))

(defconst org-test-no-heading-file
  (expand-file-name "no-heading.org" org-test-example-dir))

(defconst org-test-attachments-file
  (expand-file-name "attachments.org" org-test-example-dir))

(defconst org-test-link-in-heading-file
  (expand-file-name "link-in-heading.org" org-test-dir))

(defconst org-id-locations-file
  (expand-file-name ".test-org-id-locations" org-test-dir))


;;; Functions for writing tests
(put 'missing-test-dependency
     'error-conditions
     '(error missing-test-dependency))

(defun org-test-for-executable (exe)
  "Throw an error if EXE is not available.
This can be used at the top of code-block-language specific test
files to avoid loading the file on systems without the
executable."
  (unless (cl-reduce
	   (lambda (acc dir)
	     (or acc (file-exists-p (expand-file-name exe dir))))
	   exec-path :initial-value nil)
    (signal 'missing-test-dependency (list exe))))

(defun org-test-buffer (&optional file)
  "TODO:  Setup and return a buffer to work with.
If file is non-nil insert its contents in there.")

(defun org-test-compare-with-file (&optional file)
  "TODO:  Compare the contents of the test buffer with FILE.
If file is not given, search for a file named after the test
currently executed.")

(defmacro org-test-at-id (id &rest body)
  "Run body after placing the point in the headline identified by ID."
  (declare (indent 1))
  `(let* ((id-location (org-id-find ,id))
	  (id-file (car id-location))
	  (visited-p (get-file-buffer id-file))
	  to-be-removed)
     (unwind-protect
	 (save-window-excursion
	   (save-match-data
	     (org-id-goto ,id)
	     (setq to-be-removed (current-buffer))
	     (condition-case nil
		 (progn
		   (org-show-subtree)
		   (org-show-all '(blocks)))
	       (error nil))
	     (save-restriction ,@body)))
       (unless (or visited-p (not to-be-removed))
	 (kill-buffer to-be-removed)))))
(def-edebug-spec org-test-at-id (form body))

(defmacro org-test-in-example-file (file &rest body)
  "Execute body in the Org example file."
  (declare (indent 1))
  `(let* ((my-file (or ,file org-test-file))
	  (visited-p (get-file-buffer my-file))
	  to-be-removed
	  results)
     (save-window-excursion
       (save-match-data
	 (find-file my-file)
	 (unless (eq major-mode 'org-mode)
	   (org-mode))
	 (setq to-be-removed (current-buffer))
	 (goto-char (point-min))
	 (condition-case nil
	     (progn
	       (outline-next-visible-heading 1)
	       (org-show-subtree)
	       (org-show-all '(blocks)))
	   (error nil))
	 (setq results (save-restriction ,@body))))
     (unless visited-p
       (kill-buffer to-be-removed))
     results))
(def-edebug-spec org-test-in-example-file (form body))

(defmacro org-test-at-marker (file marker &rest body)
  "Run body after placing the point at MARKER in FILE.
Note the uuidgen command-line command can be useful for
generating unique markers for insertion as anchors into org
files."
  (declare (indent 2))
  `(org-test-in-example-file ,file
     (goto-char (point-min))
     (re-search-forward (regexp-quote ,marker))
     ,@body))
(def-edebug-spec org-test-at-marker (form form body))

(defmacro org-test-with-temp-text (text &rest body)
  "Run body in a temporary buffer with Org mode as the active
mode holding TEXT.  If the string \"<point>\" appears in TEXT
then remove it and place the point there before running BODY,
otherwise place the point at the beginning of the inserted text."
  (declare (indent 1))
  `(let ((inside-text (if (stringp ,text) ,text (eval ,text)))
	 (org-mode-hook nil))
     (with-temp-buffer
       (org-mode)
       (let ((point (string-match "<point>" inside-text)))
	 (if point
	     (progn
	       (insert (replace-match "" nil nil inside-text))
	       (goto-char (1+ (match-beginning 0))))
	   (insert inside-text)
	   (goto-char (point-min))))
       (font-lock-ensure (point-min) (point-max))
       ,@body)))
(def-edebug-spec org-test-with-temp-text (form body))

(defmacro org-test-with-temp-text-in-file (text &rest body)
  "Run body in a temporary file buffer with Org mode as the active mode.
If the string \"<point>\" appears in TEXT then remove it and
place the point there before running BODY, otherwise place the
point at the beginning of the buffer."
  (declare (indent 1))
  `(let ((file (make-temp-file "org-test"))
	 (inside-text (if (stringp ,text) ,text (eval ,text)))
	 buffer)
     (with-temp-file file (insert inside-text))
     (unwind-protect
	 (progn
	   (setq buffer (find-file file))
	   (when (re-search-forward "<point>" nil t)
	     (replace-match ""))
	   (org-mode)
	   (progn ,@body))
       (let ((kill-buffer-query-functions nil))
	 (when buffer
	   (set-buffer buffer)
	   ;; Ignore changes, we're deleting the file in the next step
	   ;; anyways.
	   (set-buffer-modified-p nil)
	   (kill-buffer))
	 (delete-file file)))))
(def-edebug-spec org-test-with-temp-text-in-file (form body))

(defun org-test-table-target-expect (target &optional expect laps
&rest tblfm)
  "For all TBLFM: Apply the formula to TARGET, compare EXPECT with result.
Either LAPS and TBLFM are nil and the table will only be aligned
or LAPS is the count of recalculations that should be made on
each TBLFM.  To save ERT run time keep LAPS as low as possible to
get the table stable.  Anyhow, if LAPS is 'iterate then iterate,
but this will run one recalculation longer.  When EXPECT is nil
it will be set to TARGET.

When running a test interactively in ERT is not enough and you
need to examine the target table with e. g. the Org formula
debugger or an Emacs Lisp debugger (e. g. with point in a data
field and calling the instrumented `org-table-eval-formula') then
copy and paste the table with formula from the ERT results buffer
or temporarily substitute the `org-test-with-temp-text' of this
function with `org-test-with-temp-text-in-file'.  Also consider
setting `pp-escape-newlines' to nil manually."
  (require 'pp)
  (require 'ert)
  (let ((back pp-escape-newlines) (current-tblfm))
    (unless tblfm
      (should-not laps)
      (push "" tblfm))  ; Dummy formula.
    (unless expect (setq expect target))
    (while (setq current-tblfm (pop tblfm))
      (org-test-with-temp-text (concat target current-tblfm)
	;; Search the last of possibly several tables, let the ERT
	;; test fail if not found.
	(goto-char (point-max))
	(while (not (org-at-table-p))
	  (should (eq 0 (forward-line -1))))
	(when laps
	  (if (and (symbolp laps) (eq laps 'iterate))
	      (should (org-table-recalculate 'iterate t))
	    (should (integerp laps))
	    (should (< 0 laps))
	    (let ((cnt laps))
	      (while (< 0 cnt)
		(should (org-table-recalculate 'all t))
		(setq cnt (1- cnt))))))
	(org-table-align)
	(setq pp-escape-newlines nil)
	;; Declutter the ERT results buffer by giving only variables
	;; and not directly the forms to `should'.
	(let ((expect (concat expect current-tblfm))
	      (result (buffer-substring-no-properties
		       (point-min) (point-max))))
	  (should (equal expect result)))
	;; If `should' passed then set back `pp-escape-newlines' here,
	;; else leave it nil as a side effect to see the failed table
	;; on multiple lines in the ERT results buffer.
	(setq pp-escape-newlines back)))))


;;; Navigation Functions
(when (featurep 'jump)
  (defjump org-test-jump
    (("lisp/\\1.el" . "testing/lisp/test-\\1.el")
     ("lisp/\\1.el" . "testing/lisp/\\1.el/test.*.el")
     ("testing/lisp/test-\\1.el" . "lisp/\\1.el")
     ("testing/lisp/\\1.el" . "lisp/\\1.el/test.*.el"))
    (concat org-base-dir "/")
    "Jump between Org files and their tests."
    (lambda (path)
      (let* ((full-path (expand-file-name path org-base-dir))
	     (file-name (file-name-nondirectory path))
	     (name (file-name-sans-extension file-name)))
	(find-file full-path)
	(insert
	 ";;; " file-name "\n\n"
	 ";; Copyright (c) " (nth 5 (decode-time (current-time)))
	 " " user-full-name "\n"
	 ";; Authors: " user-full-name "\n\n"
	 ";; Released under the GNU General Public License version 3\n"
	 ";; see: http://www.gnu.org/licenses/gpl-3.0.html\n\n"
	 ";;;; Comments:\n\n"
	 ";; Template test file for Org tests\n\n"
	 "\n"
	 ";;; Code:\n"
	 "(let ((load-path (cons (expand-file-name\n"
	 "			\"..\" (file-name-directory\n"
	 "			      (or load-file-name buffer-file-name)))\n"
	 "		       load-path)))\n"
	 "  (require 'org-test)\n\n"
	 "\n"
	 ";;; Tests\n"
	 "(ert-deftest " name "/example-test ()\n"
	 "  \"Just an example to get you started.\"\n"
	 "  (should t)\n"
	 "  (should-not nil)\n"
	 "  (should-error (error \"errr...\")))\n\n\n"
	 "(provide '" name ")\n\n"
	 ";;; " file-name " ends here\n") full-path))
    (lambda () ((lambda (res) (if (listp res) (car res) res)) (which-function)))))

(define-key emacs-lisp-mode-map "\M-\C-j" 'org-test-jump)


;;; Miscellaneous helper functions
(defun org-test-strip-text-props (s)
  "Return S without any text properties."
  (let ((noprop (copy-sequence s)))
    (set-text-properties 0 (length noprop) nil noprop)
    noprop))


(defun org-test-string-exact-match (regex string &optional start)
  "Case sensitive string-match"
  (let ((case-fold-search nil)
        (case-replace nil))
    (if(and (equal regex "")
	    (not(equal string "")))
        nil
      (if (equal 0 (string-match regex string start))
          t
        nil))))

;;; Load and Run tests
(defun org-test-load ()
  "Load up the Org test suite."
  (interactive)
  (cl-flet ((rld (base)
	      ;; Recursively load all files, if files throw errors
	      ;; then silently ignore the error and continue to the
	      ;; next file.  This allows files to error out if
	      ;; required executables aren't available.
	      (mapc
	       (lambda (path)
		 (if (file-directory-p path)
		     (rld path)
		   (condition-case err
		       (when (string-match "^[A-Za-z].*\\.el$"
					   (file-name-nondirectory path))
			 (load-file path))
		     (missing-test-dependency
		      (let ((name (intern
				   (concat "org-missing-dependency/"
					   (file-name-nondirectory
					    (file-name-sans-extension path))))))
			(eval `(ert-deftest ,name ()
				 :expected-result :failed (should nil))))))))
	       (directory-files base 'full
				"^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*\\.el$"))))
    (rld (expand-file-name "lisp" org-test-dir))))

(defun org-test-current-defun ()
  "Test the current function."
  (interactive)
  (ert (which-function)))

(defun org-test-current-file ()
  "Run all tests for current file."
  (interactive)
  (ert (concat "test-"
	       (file-name-sans-extension
		(file-name-nondirectory (buffer-file-name)))
	       "/")))

(defvar org-test-buffers nil
  "Hold buffers open for running Org tests.")

(defun org-test-touch-all-examples ()
  (dolist (file (directory-files
		 org-test-example-dir 'full
		 "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*\\.org$"))
    (unless (get-file-buffer file)
      (add-to-list 'org-test-buffers (find-file file)))))

(defun org-test-kill-all-examples ()
  (while org-test-buffers
    (let ((b (pop org-test-buffers)))
      (when (buffer-live-p b) (kill-buffer b)))))

(defun org-test-update-id-locations ()
  (org-id-update-id-locations
   (directory-files
    org-test-example-dir 'full
    "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*\\.org$")))

(defun org-test-run-batch-tests (&optional org-test-selector)
  "Run all tests matching an optional regex which defaults to \"\\(org\\|ob\\)\".
Load all test files first."
  (interactive)
  (let ((org-id-track-globally t)
	(org-test-selector
	 (if org-test-selector org-test-selector "\\(org\\|ob\\)"))
	org-confirm-babel-evaluate org-startup-folded vc-handled-backends)
    (org-test-touch-all-examples)
    (org-test-update-id-locations)
    (org-test-load)
    (message "selected tests: %s" org-test-selector)
    (ert-run-tests-batch-and-exit org-test-selector)))

(defun org-test-run-all-tests ()
  "Run all defined tests matching \"\\(org\\|ob\\)\".
Load all test files first."
  (interactive)
  (org-test-touch-all-examples)
  (org-test-update-id-locations)
  (org-test-load)
  (ert "\\(org\\|ob\\)")
  (org-test-kill-all-examples))

(defmacro org-test-at-time (time &rest body)
  "Run body while pretending that the current time is TIME.
TIME can be a non-nil Lisp time value, or a string specifying a date and time."
  (declare (indent 1))
  (let ((tm (cl-gensym))
	(at (cl-gensym)))
    `(let* ((,tm ,time)
	    (,at (if (stringp ,tm)
		     (apply #'encode-time (org-parse-time-string ,tm))
		   ,tm)))
       (cl-letf
	   ;; Wrap builtins whose behavior can depend on the current time.
	   (((symbol-function 'current-time)
	     (lambda () ,at))
	    ((symbol-function 'current-time-string)
	     (lambda (&optional time &rest args)
	       (apply ,(symbol-function 'current-time-string)
		      (or time ,at) args)))
	    ((symbol-function 'current-time-zone)
	     (lambda (&optional time &rest args)
	       (apply ,(symbol-function 'current-time-zone)
		      (or time ,at) args)))
	    ((symbol-function 'decode-time)
	     (lambda (&optional time) (funcall ,(symbol-function 'decode-time)
					       (or time ,at))))
	    ((symbol-function 'encode-time)
	     (lambda (time &rest args)
	       (apply ,(symbol-function 'encode-time) (or time ,at) args)))
	    ((symbol-function 'float-time)
	     (lambda (&optional time)
	       (funcall ,(symbol-function 'float-time) (or time ,at))))
	    ((symbol-function 'format-time-string)
	     (lambda (format &optional time &rest args)
	       (apply ,(symbol-function 'format-time-string)
		      format (or time ,at) args)))
	    ((symbol-function 'set-file-times)
	     (lambda (file &optional time)
	       (funcall ,(symbol-function 'set-file-times) file (or time ,at))))
	    ((symbol-function 'time-add)
	     (lambda (a b) (funcall ,(symbol-function 'time-add)
				    (or a ,at) (or b ,at))))
	    ((symbol-function 'time-equal-p)
	     (lambda (a b) (funcall ,(symbol-function 'time-equal-p)
				    (or a ,at) (or b ,at))))
	    ((symbol-function 'time-less-p)
	     (lambda (a b) (funcall ,(symbol-function 'time-less-p)
				    (or a ,at) (or b ,at))))
	    ((symbol-function 'time-subtract)
	     (lambda (a b) (funcall ,(symbol-function 'time-subtract)
				    (or a ,at) (or b ,at)))))
	 ,@body))))

(provide 'org-test)

;;; org-test.el ends here
