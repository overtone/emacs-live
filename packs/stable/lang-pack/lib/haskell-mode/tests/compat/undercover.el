;;; undercover.el --- Test coverage library for Emacs -*- lexical-binding: t -*-

;; Copyright (c) 2014 Sviridov Alexander

;; Author: Sviridov Alexander <sviridov.vmi@gmail.com>
;; URL: https://github.com/sviridov/undercover.el
;; Created: Sat Sep 27 2014
;; Keywords: lisp, tests, coverage, tools
;; Version: 0.5.0
;; Package-Requires: ((emacs "24") (dash "2.0.0") (shut-up "0.3.2"))

;;; Commentary:

;; Provides a test coverage tools for Emacs packages.

;;; Code:

(eval-when-compile (require 'cl))

(require 'edebug)
(require 'json)
(require 'dash)
(require 'shut-up)

(defconst undercover-version "0.5.0")

(defvar undercover-force-coverage nil
  "If nil, test coverage check will be done only under continuous integration service.")

(defvar undercover--send-report t
  "If not nil, test coverage report will be sent to coveralls.io.")

(defvar undercover--report-file-path "/tmp/undercover_coveralls_report"
  "Path to save coveralls.io report.")

(defvar undercover--files nil
  "List of files for test coverage check.")

(defvar undercover--files-coverage-statistics (make-hash-table :test 'equal)
  "Table of coverage statistics for each file in `undercover--files'.")

(defvar undercover--old-edebug-make-form-wrapper
  (symbol-function 'edebug-make-form-wrapper))

;; Utilities

(defun undercover--fill-hash-table (hash-table &rest keys-and-values)
  "Fill HASH-TABLE from KEYS-AND-VALUES."
  (loop for (key value) on keys-and-values by #'cddr
        do (puthash key value hash-table))
  hash-table)

(defun undercover--make-hash-table (&rest keys-and-values)
  "Create new hash-table and fill it from KEYS-AND-VALUES."
  (apply #'undercover--fill-hash-table (make-hash-table :test 'equal) keys-and-values))

(defun undercover--wildcards-to-files (wildcards)
  "Return list of files matched by WILDCARDS.
Example of WILDCARDS: (\"*.el\" \"subdir/*.el\" (:exclude \"exclude-*.el\"))."
  (destructuring-bind (exclude-clauses include-wildcards)
      (--separate (and (consp it) (eq :exclude (car it))) wildcards)
    (let* ((exclude-wildcards (-mapcat #'cdr exclude-clauses))
           (exclude-files (-mapcat #'file-expand-wildcards exclude-wildcards))
           (include-files (-mapcat #'file-expand-wildcards include-wildcards)))
      (-difference include-files exclude-files))))

;; `edebug' related functions and hacks:

;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=6415
(def-edebug-spec cl-destructuring-bind (sexp form body))
(def-edebug-spec destructuring-bind (sexp form body))

(def-edebug-spec cl-symbol-macrolet ((&rest (symbolp sexp)) cl-declarations body))
(def-edebug-spec symbol-macrolet ((&rest (symbolp sexp)) cl-declarations body))

(def-edebug-spec cl-type-spec sexp)

(def-edebug-spec when-let ([&or (symbolp form) (&rest (symbolp form))] body))

(defun undercover--fallback-file-handler (operation args)
  "Handle any file OPERATION with ARGS."
  (let ((inhibit-file-name-handlers
         (cons 'undercover-file-handler
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (apply operation args)))

(defun undercover--load-file-handler (file)
  "Handle `load' FILE operation."
  (let ((edebug-all-defs (undercover--coverage-enabled-p))
        (load-file-name (file-truename file))
        (load-in-progress t))
    (unwind-protect
        (progn
          (save-excursion (eval-buffer (find-file load-file-name)))
          (push load-file-name undercover--files))
      (switch-to-buffer (current-buffer)))))

(defun undercover--show-load-file-error (filename)
  (message "UNDERCOVER: error while covering %s" filename)
  (message "UNDERCOVER: please open a new issue at https://github.com/sviridov/undercover.el/issues"))

(defun undercover-file-handler (operation &rest args)
  "Handle `load' OPERATION.  Ignore all ARGS except first."
  (if (eq 'load operation)
      (condition-case nil
          (undercover--load-file-handler (car args))
        (error
         (undercover--show-load-file-error (car args))
         (undercover--fallback-file-handler operation args)))
    (undercover--fallback-file-handler operation args)))

(defun undercover--edebug-files (files)
  "Use `edebug' package to instrument all macros and functions in FILES."
  (when files
    (let ((regexp (->> files (regexp-opt) (format "/%s$"))))
      (add-to-list 'file-name-handler-alist (cons regexp 'undercover-file-handler)))))

(setf (symbol-function 'undercover--stop-point-before)
      (lambda (before-index)
        "Increase number of times that stop point at BEFORE-INDEX was covered."
        (when (boundp 'edebug-freq-count)
          (incf (aref edebug-freq-count before-index)))
        before-index))

(setf (symbol-function 'undercover--stop-point-after)
      (cons 'macro
        (lambda (before-index after-index form)
          "Increase number of times that stop point at AFTER-INDEX was covered."
         `(let ((before-index ,before-index)
                (after-index ,after-index))
            (unwind-protect ,form
              (when (boundp 'edebug-freq-count)
                (aset edebug-freq-count after-index (+ 1 (aref edebug-freq-count after-index)))
                (undercover--align-counts-between-stop-points before-index after-index)))))))

(setf (symbol-function 'undercover--align-counts-between-stop-points)
      (lambda (before-index after-index)
        "Decrease number of times that stop points between BEFORE-INDEX and AFTER-INDEX are covered."
        (do ((index (1+ before-index) (1+ index)))
            ((>= index after-index))
          (setf (aref edebug-freq-count index)
                (min (aref edebug-freq-count index)
                     (aref edebug-freq-count before-index))))))

(defun undercover--stop-points (name)
  "Return stop points ordered by position for NAME."
  (append (nth 2 (get name 'edebug)) nil))

(defun undercover--stop-points-covers (name)
  "Return number of covers for each stop point ordered by position for NAME."
  (append (get name 'edebug-freq-count) nil))

(defun undercover--shut-up-edebug-message ()
  "Muffle `edebug' message \"EDEBUG: function\"."
  ;; HACK: I don't use `defadvice' because of cryptic error with `shut-up-sink'.
  ;; https://travis-ci.org/sviridov/multiple-cursors.el/builds/37529750#L1387
  ;; https://travis-ci.org/sviridov/expand-region.el/builds/37576813#L285
  (setf (symbol-function 'edebug-make-form-wrapper)
        (lambda (&rest args)
          (shut-up (apply undercover--old-edebug-make-form-wrapper args)))))

(defun undercover--set-edebug-handlers ()
  "Replace and advice some `edebug' functions with `undercover' handlers."
  (defalias 'edebug-before 'undercover--stop-point-before)
  (defalias 'edebug-after 'undercover--stop-point-after)
  (undercover--shut-up-edebug-message)
  ;; HACK: Ensures that debugger is turned off.
  ;; https://travis-ci.org/sviridov/multiple-cursors.el/builds/37672312#L350
  ;; https://travis-ci.org/sviridov/expand-region.el/builds/37577423#L336
  (setq debug-on-error  nil
        debug-on-signal nil
        edebug-on-error nil))

;; Coverage statistics related functions:

(defun undercover--symbol-coverage-statistics (edebug-symbol statistics)
  "Collect coverage statistics for EDEBUG-SYMBOL into STATISTICS hash."
  (let* ((start-marker (car (get edebug-symbol 'edebug)))
         (points (undercover--stop-points edebug-symbol))
         (points-covers (undercover--stop-points-covers edebug-symbol))
         (points-and-covers (map 'list #'cons points points-covers)))
    (dolist (point-and-cover points-and-covers)
      (let* ((point (car point-and-cover))
             (line  (line-number-at-pos (+ point start-marker)))
             (cover (cdr point-and-cover))
             (previous-score (gethash line statistics cover))
             (new-score (min previous-score cover)))
        (puthash line new-score statistics)))))

(defun undercover--file-coverage-statistics ()
  "Collect coverage statistics for current-file into hash.
Keys of that hash are line numbers.
Values of that hash are number of covers."
  (let ((statistics (make-hash-table)))
    (dolist (edebug-data edebug-form-data)
      (let ((edebug-symbol (car edebug-data)))
        (when (get edebug-symbol 'edebug)
          (undercover--symbol-coverage-statistics edebug-symbol statistics))))
    statistics))

(defun undercover--collect-file-coverage (file)
  "Collect coverage statistics for FILE."
  (save-excursion
    (find-file file)
    (if edebug-form-data
        (undercover--fill-hash-table undercover--files-coverage-statistics
          file (undercover--file-coverage-statistics))
      (setq undercover--files (delq file undercover--files)))))

(defun undercover--collect-files-coverage (files)
  "Collect coverage statistics for each file in FILES."
  (dolist (file files)
    (undercover--collect-file-coverage file)))

;; Continuous integration related functions:

(defun undercover--under-travic-ci-p ()
  "Check that `undercover' running under Travis CI service."
  (getenv "TRAVIS"))

(defun undercover--coveralls-repo-token ()
  "Return coveralls.io repo token provided by user."
  (getenv "COVERALLS_REPO_TOKEN"))

(defun undercover--under-ci-p ()
  "Check that `undercover' running under continuous integration service."
  (or (undercover--coveralls-repo-token) (undercover--under-travic-ci-p)))

;;; Reports related functions:

(defun undercover--determine-report-type ()
  "Automatic report-type determination."
  (and (undercover--under-ci-p) 'coveralls))

(defun undercover--get-git-info (&rest args)
  "Execute Git with ARGS, returning the first line of its output."
  (with-temp-buffer
    (apply #'process-file "git" nil t nil "--no-pager" args)
    (goto-char (point-min))
    (buffer-substring-no-properties
     (line-beginning-position)
     (line-end-position))))

(defun undercover--get-git-info-from-log (format)
  "Get first line of Git log in given FORMAT."
  (undercover--get-git-info "log" "-1" (format "--pretty=format:%%%s" format)))

(defun undercover--get-git-remotes ()
  "Return list of Git remotes."
  (with-temp-buffer
    (process-file "git" nil t nil "--no-pager" "remote")
    (let ((remotes (split-string (buffer-string) "\n" t))
          (config-path-format (format "remote.%%s.url"))
          (remotes-info nil))
      (dolist (remote remotes remotes-info)
        (let* ((remote-url (undercover--get-git-info "config" (format config-path-format remote)))
               (remote-table (undercover--make-hash-table
                              "name" remote
                              "url"  remote-url)))
          (push remote-table remotes-info))))))

;; coveralls.io report:

(defun undercover--update-coveralls-report-with-repo-token (report)
  "Update test coverage REPORT for coveralls.io with repository token."
  (puthash "repo_token" (undercover--coveralls-repo-token) report))

(defun undercover--try-update-coveralls-report-with-shippable (report)
  "Update test coverage REPORT for coveralls.io with Shippable service information."
  (when (getenv "SHIPPABLE")
    (undercover--fill-hash-table report
      "service_name"   "shippable"
      "service_job_id" (getenv "BUILD_NUMBER"))
    (unless (string-equal "false" (getenv "PULL_REQUEST"))
      (undercover--fill-hash-table report
        "service_pull_request" (getenv "PULL_REQUEST")))))

(defun undercover--update-coveralls-report-with-travis-ci (report)
  "Update test coverage REPORT for coveralls.io with Travis CI service information."
  (undercover--fill-hash-table report
    "service_name"   "travis-ci"
    "service_job_id" (getenv "TRAVIS_JOB_ID")))

(defun undercover--update-coveralls-report-with-git (report)
  "Update test coverage REPORT for coveralls.io with Git information."
  (undercover--fill-hash-table report
    "git" (undercover--make-hash-table
           "branch"  (undercover--get-git-info "rev-parse" "--abbrev-ref" "HEAD")
           "remotes" (undercover--get-git-remotes)
           "head"    (undercover--make-hash-table
                      "id"              (undercover--get-git-info-from-log "H")
                      "author_name"     (undercover--get-git-info-from-log "aN")
                      "author_email"    (undercover--get-git-info-from-log "ae")
                      "committer_name"  (undercover--get-git-info-from-log "cN")
                      "committer_email" (undercover--get-git-info-from-log "ce")
                      "message"         (undercover--get-git-info-from-log "s")))))

(defun undercover--coveralls-file-coverage-report (statistics)
  "Translate file coverage STATISTICS into coveralls.io format."
  (let (file-coverage)
    (dotimes (line (count-lines (point-min) (point-max)))
      (push (gethash (1+ line) statistics) file-coverage))
    (nreverse file-coverage)))

(defun undercover--coveralls-file-report (file)
  "Create part of coveralls.io report for FILE."
  (save-excursion
    (find-file file)
    (let ((file-name (file-relative-name file (locate-dominating-file default-directory ".git")))
          (file-content (buffer-substring-no-properties (point-min) (point-max)))
          (coverage-report (undercover--coveralls-file-coverage-report
                            (gethash file undercover--files-coverage-statistics))))
      (undercover--make-hash-table
       "name"     file-name
       "source"   file-content
       "coverage" coverage-report))))

(defun undercover--fill-coveralls-report (report)
  "Fill test coverage REPORT for coveralls.io."
  (undercover--fill-hash-table report
    "source_files" (mapcar #'undercover--coveralls-file-report undercover--files)))

(defun undercover--merge-coveralls-report-file-lines-coverage (old-coverage new-coverage)
  "Merge test coverage for lines from OLD-COVERAGE and NEW-COVERAGE."
  (loop for (old-line-coverage . new-line-coverage)
        in (-zip-fill 0 old-coverage new-coverage)
        collect (cond
                 ((null old-line-coverage) new-line-coverage)
                 ((null new-line-coverage) old-line-coverage)
                 (t (+ new-line-coverage old-line-coverage)))))

(defun undercover--merge-coveralls-report-file-coverage (old-file-hash source-files-report)
  "Merge test coverage from OLD-FILE-HASH into SOURCE-FILES-REPORT."
  (let* ((file-name (gethash "name" old-file-hash))
         (old-coverage (gethash "coverage" old-file-hash))
         (new-file-hash (--first (string-equal file-name (gethash "name" it))
                                 source-files-report)))
    (if new-file-hash
        (undercover--fill-hash-table new-file-hash
          "coverage" (undercover--merge-coveralls-report-file-lines-coverage
                      old-coverage (gethash "coverage" new-file-hash)))
      (rplacd (last source-files-report)
              (cons old-file-hash nil)))))

(defun undercover--merge-coveralls-reports (report)
  "Merge test coverage REPORT with existing from `undercover--report-file-path'."
  (ignore-errors
    (let* ((json-object-type 'hash-table)
           (json-array-type 'list)
           (old-report (json-read-file undercover--report-file-path))
           (new-source-files-report (gethash "source_files" report)))
      (dolist (old-file-hash (gethash "source_files" old-report))
        (undercover--merge-coveralls-report-file-coverage
         old-file-hash new-source-files-report)))))

(defun undercover--create-coveralls-report ()
  "Create test coverage report for coveralls.io."
  (undercover--collect-files-coverage undercover--files)
  (let ((report (make-hash-table :test 'equal)))
    (cond
     ((undercover--coveralls-repo-token)
      (undercover--update-coveralls-report-with-repo-token report)
      (undercover--try-update-coveralls-report-with-shippable report))
     ((undercover--under-travic-ci-p) (undercover--update-coveralls-report-with-travis-ci report))
     (t (error "Unsupported coveralls.io report")))
    (undercover--update-coveralls-report-with-git report)
    (undercover--fill-coveralls-report report)
    (undercover--merge-coveralls-reports report)
    (json-encode report)))

(defun undercover--save-coveralls-report (json-report)
  "Save JSON-REPORT to `undercover--report-file-path'."
  (save-excursion
    (shut-up
      (find-file undercover--report-file-path)
      (erase-buffer)
      (insert json-report)
      (save-buffer))))

(defun undercover--send-coveralls-report ()
  "Send report to coveralls.io."
  (let ((coveralls-url "https://coveralls.io/api/v1/jobs"))
    (message "Sending: report to coveralls.io")
    (shut-up
     (shell-command
      (format "curl -v -include --form json_file=@%s %s" undercover--report-file-path coveralls-url)))
    (message "Sending: OK")))

(defun undercover--coveralls-report ()
  "Create and submit test coverage report to coveralls.io."
  (undercover--save-coveralls-report (undercover--create-coveralls-report))
  (when undercover--send-report
    (undercover--send-coveralls-report)))

;; `ert-runner' related functions:

(defun undercover-safe-report ()
  "Version of `undercover-report' that ignore errors."
  (ignore-errors
    (undercover-report)))

(defun undercover-report-on-kill ()
  "Add `undercover-safe-report' to `kill-emacs-hook'."
  (add-hook 'kill-emacs-hook 'undercover-safe-report))

;;; Main functions:

(defun undercover--coverage-enabled-p ()
  "Check that `undercover' is enabled."
  (or undercover-force-coverage (undercover--under-ci-p)))

(defun undercover-report (&optional report-type)
  "Create and submit (if needed) test coverage report based on REPORT-TYPE.
Posible values of REPORT-TYPE: coveralls."
  (when undercover--files
    (case (or report-type (undercover--determine-report-type))
      (coveralls (undercover--coveralls-report))
      (t (error "Unsupported report-type")))))

(defun undercover--set-options (configuration)
  "Read CONFIGURATION.
Set `undercover--send-report' and `undercover--report-file-path'.
Return wildcards."
  (destructuring-bind (wildcards options)
      (--separate (or (stringp it) (eq :exclude (car-safe it))) configuration)
    (dolist (option options wildcards)
      (case (car-safe option)
        (:report-file (setq undercover--report-file-path (cadr option)))
        (:send-report (setq undercover--send-report (cadr option)))
        (otherwise (error "Unsupported option: %s" option))))))

(defun undercover--setup (configuration)
  "Enable test coverage for files matched by CONFIGURATION."
  (when (undercover--coverage-enabled-p)
    (undercover--set-edebug-handlers)
    (undercover-report-on-kill)
    (let ((wildcards (undercover--set-options configuration)))
      (undercover--edebug-files (undercover--wildcards-to-files wildcards)))))

;;;###autoload
(defmacro undercover (&rest configuration)
  "Enable test coverage for files matched by CONFIGURATION.
Example of CONFIGURATION: (\"*.el\" \"subdir/*.el\" (:exclude \"exclude-*.el\")).

If running under Travic CI automatically generate report
on `kill-emacs' and send it to coveralls.io."
  `(undercover--setup
    (list
     ,@(--map (if (atom it) it `(list ,@it))
              configuration))))

(provide 'undercover)
;;; undercover.el ends here
