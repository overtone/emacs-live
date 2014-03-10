;;; clojure-test-mode.el --- Minor mode for Clojure tests

;; Copyright © 2009-2011 Phil Hagelberg

;; Author: Phil Hagelberg <technomancy@gmail.com>
;; URL: http://emacswiki.org/cgi-bin/wiki/ClojureTestMode
;; Version: 3.0.0
;; Keywords: languages, lisp, test
;; Package-Requires: ((clojure-mode "1.7") (cider "0.4.0"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides support for running Clojure tests (using the
;; clojure.test framework) via nrepl.el and seeing feedback in the test
;; buffer about which tests failed or errored.

;;; Usage:

;; Once you have an nrepl session active, you can run the tests in the
;; current buffer with C-c C-,. Failing tests and errors will be
;; highlighted using overlays. To clear the overlays, use C-c k.

;; You can jump between implementation and test files with <kbd>C-c C-t</kbd> if
;; your project is laid out in a way that clojure-test-mode expects. Your
;; project root should have a `src/` directory containing files that correspond
;; to their namespace. It should also have a `test/` directory containing files
;; that correspond to their namespace, and the test namespaces should mirror the
;; implementation namespaces with the addition of "-test" as the suffix to the
;; last segment of the namespace.

;; So `my.project.frob` would be found in `src/my/project/frob.clj` and its
;; tests would be in `test/my/project/frob_test.clj` in the
;; `my.project.frob-test` namespace.

;; This behavior can also be overridden by setting `clojure-test-for-fn` and
;; `clojure-test-implementation-for-fn` with functions of your choosing.
;; `clojure-test-for-fn` takes an implementation namespace and returns the full
;; path of the test file.  `clojure-test-implementation-for-fn` takes a test
;; namespace and returns the full path for the implementation file.

;;; History:

;; 1.0: 2009-03-12
;;  * Initial Release

;; 1.1: 2009-04-28
;;  * Fix to work with latest version of test-is. (circa Clojure 1.0)

;; 1.2: 2009-05-19
;;  * Add clojure-test-jump-to-(test|implementation).

;; 1.3: 2009-11-10
;;  * Update to use clojure.test instead of clojure.contrib.test-is.
;;  * Fix bug suppressing test report output in repl.

;; 1.4: 2010-05-13
;;  * Fix jump-to-test
;;  * Update to work with Clojure 1.2.
;;  * Added next/prev problem.
;;  * Depend upon slime, not swank-clojure.
;;  * Don't move the mark when activating.

;; 1.5: 2010-09-16
;;  * Allow customization of clojure-test-ns-segment-position.
;;  * Fixes for Clojure 1.2.
;;  * Check for active slime connection.
;;  * Fix test toggling with negative segment-position.

;; 1.5.1: 2010-11-27
;;  * Add marker between each test run.

;; 1.5.2: 2011-03-11
;;  * Make clojure-test-run-tests force reload. Requires swank-clojure 1.3.0.

;; 1.5.3 2011-03-14
;;  * Fix clojure-test-run-test to use fixtures.

;; 1.5.4 2011-03-16
;;  * Fix clojure-test-run-tests to wait until tests are reloaded.

;; 1.5.5 2011-04-08
;;  * Fix coloring/reporting
;;  * Don't trigger slime-connected-hook.

;; 1.5.6 2011-06-15
;;  * Remove heinous clojure.test/report monkeypatch.

;; 1.6.0 2011-11-06
;;  * Compatibility with Clojure 1.3.
;;  * Support narrowing.
;;  * Fix a bug in clojure-test-mode-test-one-in-ns.

;; 2.0.0 2012-12-29
;;  * Replace slime with nrepl.el

;; 3.0.0 2013-12-27
;;  * Replace nrepl.el with cider
;;  * Improve clojure-test-maybe-enable heuristic
;;  * Obsolete clojure-test-jump-to-implementation in favour of other libs

;;; TODO:

;; * Prefix arg to jump-to-impl should open in other window
;; * Put Testing indicator in modeline while tests are running
;; * Integrate with M-x next-error
;; * Error messages need line number.
;; * Currently show-message needs point to be on the line with the
;;   "is" invocation; this could be cleaned up.

;;; Code:

(require 'cl)
(require 'clojure-mode)
(require 'which-func)
(require 'nrepl-client)
(require 'cider-interaction)

;; Faces

(defface clojure-test-failure-face
  '((((class color) (background light))
     :background "orange red") ;; TODO: Hard to read strings over this.
    (((class color) (background dark))
     :background "firebrick"))
  "Face for failures in Clojure tests."
  :group 'clojure-test-mode)

(defface clojure-test-error-face
  '((((class color) (background light))
     :background "orange1")
    (((class color) (background dark))
     :background "orange4"))
  "Face for errors in Clojure tests."
  :group 'clojure-test-mode)

(defface clojure-test-success-face
  '((((class color) (background light))
     :foreground "black"
     :background "green")
    (((class color) (background dark))
     :foreground "black"
     :background "green"))
  "Face for success in Clojure tests."
  :group 'clojure-test-mode)

;; Counts

(defvar clojure-test-count 0)
(defvar clojure-test-failure-count 0)
(defvar clojure-test-error-count 0)

;; Consts

(defconst clojure-test-ignore-results
  '(:end-test-ns :begin-test-var :end-test-var)
  "Results from test-is that we don't use")

;; Support Functions

(defun clojure-test-make-handler (callback)
  (lexical-let ((buffer (current-buffer))
                (callback callback))
    (nrepl-make-response-handler buffer
                                 (lambda (buffer value)
                                   (funcall callback buffer value))
                                 (lambda (buffer value)
                                   (cider-repl-emit-interactive-output value))
                                 (lambda (buffer err)
                                   (cider-repl-emit-interactive-output err))
                                 '())))

(defun clojure-test-eval (string &optional handler)
  (nrepl-send-string string
                     (clojure-test-make-handler (or handler #'identity))
                     (or (cider-current-ns) "user")
                     (nrepl-current-tooling-session)))

(defun clojure-test-load-reporting ()
  "Redefine the test-is report function to store results in metadata."
  (when (cider-connected-p)
    (nrepl-send-string-sync
     "(ns clojure.test.mode
        (:use [clojure.test :only [file-position *testing-vars* *test-out*
                                   join-fixtures *report-counters* do-report
                                   test-var *initial-report-counters*]]
              [clojure.pprint :only [pprint]]))

    (def #^{:dynamic true} *clojure-test-mode-out* nil)
    (def fail-events #{:fail :error})
    (defn report [event]
     (if-let [current-test (last clojure.test/*testing-vars*)]
        (alter-meta! current-test
                     assoc :status (conj (:status (meta current-test))
                                     [(:type event)
                                      (:message event)
                                      (when (fail-events (:type event))
                                        (str (:expected event)))
                                      (when (fail-events (:type event))
                                        (str (:actual event)))
                                      (case (:type event)
                                        :fail (with-out-str (pprint (:actual event)))
                                        :error (with-out-str
                                                (clojure.stacktrace/print-cause-trace
                                                (:actual event)))
                                        nil)
                                      (if (and (= (:major *clojure-version*) 1)
                                               (< (:minor *clojure-version*) 2))
                                          ((file-position 2) 1)
                                          (if (= (:type event) :error)
                                              ((file-position 3) 1)
                                              (:line event)))])))
     (binding [*test-out* (or *clojure-test-mode-out* *out*)]
       ((.getRawRoot #'clojure.test/report) event)))

   (defn clojure-test-mode-test-one-var [test-ns test-name]
     (let [v (ns-resolve test-ns test-name)
           once-fixture-fn (join-fixtures (::once-fixtures (meta (find-ns test-ns))))
           each-fixture-fn (join-fixtures (::each-fixtures (meta (find-ns test-ns))))]
       (once-fixture-fn
        (fn []
          (when (:test (meta v))
            (each-fixture-fn (fn [] (test-var v))))))))

    ;; adapted from test-ns
    (defn clojure-test-mode-test-one-in-ns [ns test-name]
      (binding [*report-counters* (ref *initial-report-counters*)]
        (let [ns-obj (the-ns ns)]
          (do-report {:type :begin-test-ns, :ns ns-obj})
          ;; If the namespace has a test-ns-hook function, call that:
          (if-let [v (find-var (symbol (str (ns-name ns-obj)) \"test-ns-hook\"))]
            ((var-get v))
            ;; Otherwise, just test every var in the namespace.
            (clojure-test-mode-test-one-var ns test-name))
          (do-report {:type :end-test-ns, :ns ns-obj}))
        (do-report (assoc @*report-counters* :type :summary))))"
     (or (cider-current-ns) "user")
     (nrepl-current-tooling-session))))

(defun clojure-test-get-results (buffer result)
  (with-current-buffer buffer
    (clojure-test-eval
     (concat "(map #(cons (str (:name (meta %)))
                (:status (meta %))) (vals (ns-interns '"
             (clojure-find-ns) ")))")
     #'clojure-test-extract-results)))

(defun clojure-test-extract-results (buffer results)
  (with-current-buffer buffer
    (let ((result-vars (read results)))
      (mapc #'clojure-test-extract-result result-vars)
      (clojure-test-echo-results))))

(defun clojure-test-extract-result (result)
  "Parse the result from a single test. May contain multiple is blocks."
  (dolist (is-result (rest result))
    (unless (member (aref is-result 0) clojure-test-ignore-results)
      (incf clojure-test-count)
      (destructuring-bind (event msg expected actual pp-actual line)
          (coerce is-result 'list)
        (if (equal :fail event)
            (progn (incf clojure-test-failure-count)
                   (clojure-test-highlight-problem
                    line event (format "Expected %s, got %s" expected actual)
                    pp-actual))
          (when (equal :error event)
            (incf clojure-test-error-count)
            (clojure-test-highlight-problem
             line event actual pp-actual))))))
  (clojure-test-echo-results))

(defun clojure-test-echo-results ()
  (message
   (propertize
    (format "Ran %s tests. %s failures, %s errors."
            clojure-test-count clojure-test-failure-count
            clojure-test-error-count)
    'face
    (cond ((not (= clojure-test-error-count 0)) 'clojure-test-error-face)
          ((not (= clojure-test-failure-count 0)) 'clojure-test-failure-face)
          (t 'clojure-test-success-face)))))

(defun clojure-test-highlight-problem (line event message pp-actual)
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (let ((beg (point)))
      (end-of-line)
      (let ((overlay (make-overlay beg (point))))
        (overlay-put overlay 'face (if (equal event :fail)
                                       'clojure-test-failure-face
                                     'clojure-test-error-face))
        (overlay-put overlay 'help-echo message)
        (overlay-put overlay 'message message)
        (overlay-put overlay 'actual pp-actual)))))

;; Problem navigation
(defun clojure-test-find-next-problem (here)
  "Go to the next position with an overlay message.
Retuns the problem overlay if such a position is found, otherwise nil."
  (let ((current-overlays (overlays-at here))
        (next-overlays (next-overlay-change here)))
    (while (and (not (equal next-overlays (point-max)))
                (or
                 (not (overlays-at next-overlays))
                 (equal (overlays-at next-overlays)
                        current-overlays)))
      (setq next-overlays (next-overlay-change next-overlays)))
    (if (not (equal next-overlays (point-max)))
        (overlay-start (car (overlays-at next-overlays))))))

(defun clojure-test-find-previous-problem (here)
  "Go to the next position with the `clojure-test-problem' text property.
Retuns the problem overlay if such a position is found, otherwise nil."
  (let ((current-overlays (overlays-at here))
        (previous-overlays (previous-overlay-change here)))
    (while (and (not (equal previous-overlays (point-min)))
                (or
                 (not (overlays-at previous-overlays))
                 (equal (overlays-at previous-overlays)
                        current-overlays)))
      (setq previous-overlays (previous-overlay-change previous-overlays)))
    (if (not (equal previous-overlays (point-min)))
        (overlay-start (car (overlays-at previous-overlays))))))

;; File navigation

(defun clojure-test-implementation-for (namespace)
  "Returns the path of the src file for the given test namespace."
  (let* ((namespace (clojure-underscores-for-hyphens namespace))
         (segments (split-string namespace "\\."))
         (namespace-end (split-string (car (last segments)) "_"))
         (namespace-end (mapconcat 'identity (butlast namespace-end 1) "_"))
         (impl-segments (append (butlast segments 1) (list namespace-end))))
    (format "%s/src/%s.clj"
            (locate-dominating-file buffer-file-name "src/")
            (mapconcat 'identity impl-segments "/"))))

(defvar clojure-test-implementation-for-fn 'clojure-test-implementation-for
  "Var pointing to the function that will return the full path of the
Clojure src file for the given test namespace.")

;; Commands

(defun clojure-test-run-tests ()
  "Run all the tests in the current namespace."
  (interactive)
  (save-some-buffers nil (lambda () (equal major-mode 'clojure-mode)))
  (message "Testing...")
  (if (not (clojure-in-tests-p))
      (cider-load-file (buffer-file-name)))
  (save-window-excursion
    (if (not (clojure-in-tests-p))
        (clojure-jump-to-test))
    (clojure-test-clear)
    (clojure-test-eval (format "(binding [clojure.test/report clojure.test.mode/report]
                                       (clojure.test/run-tests '%s))"
                               (clojure-find-ns))
                       #'clojure-test-get-results)))

(defun clojure-test-run-test ()
  "Run the test at point."
  (interactive)
  (save-some-buffers nil (lambda () (equal major-mode 'clojure-mode)))
  (imenu--make-index-alist)
  (clojure-test-clear)
  (let* ((f (which-function))
         (test-name (if (listp f) (first f) f)))
    (clojure-test-eval (format "(binding [clojure.test/report clojure.test.mode/report]
                                  (load-file \"%s\")
                                  (clojure.test.mode/clojure-test-mode-test-one-in-ns '%s '%s)
                                  (cons (:name (meta (var %s))) (:status (meta (var %s)))))"
                               (buffer-file-name) (clojure-find-ns)
                               test-name test-name test-name)
                       (lambda (buffer result-str)
                         (with-current-buffer buffer
                           (let ((result (read result-str)))
                             (if (cdr result)
                                 (clojure-test-extract-result result)
                               (message "Not in a test."))))))))

(defun clojure-test-show-result ()
  "Show the result of the test under point."
  (interactive)
  (let ((overlay (find-if (lambda (o) (overlay-get o 'message))
                          (overlays-at (point)))))
    (if overlay
        (message (replace-regexp-in-string "%" "%%"
                                           (overlay-get overlay 'message))))))

(defun clojure-test-pprint-result ()
  "Show the result of the test under point."
  (interactive)
  (let ((overlay (find-if (lambda (o) (overlay-get o 'message))
                          (overlays-at (point)))))
    (when overlay
      (with-current-buffer (generate-new-buffer " *test-output*")
        (buffer-disable-undo)
        (insert (overlay-get overlay 'actual))
        (switch-to-buffer-other-window (current-buffer))))))

;;; ediff results
(defvar clojure-test-ediff-buffers nil)

(defun clojure-test-ediff-cleanup ()
  "A function for ediff-cleanup-hook, to cleanup the temporary ediff buffers"
  (mapc (lambda (b) (when (get-buffer b) (kill-buffer b)))
        clojure-test-ediff-buffers))

(defun clojure-test-ediff-result ()
  "Show the result of the test under point as an ediff"
  (interactive)
  (let ((overlay (find-if (lambda (o) (overlay-get o 'message))
                          (overlays-at (point)))))
    (if overlay
        (let* ((m (overlay-get overlay 'actual)))
          (let ((tmp-buffer (generate-new-buffer " *clojure-test-mode-tmp*"))
                (exp-buffer (generate-new-buffer " *expected*"))
                (act-buffer (generate-new-buffer " *actual*")))
            (with-current-buffer tmp-buffer
              (insert m)
              (clojure-mode)
              (goto-char (point-min))
              (forward-char) ; skip a paren
              (paredit-splice-sexp) ; splice
              (lexical-let ((p (point))) ; delete "not"
                (forward-sexp)
                (delete-region p (point)))
              (lexical-let ((p (point))) ; splice next sexp
                (forward-sexp)
                (backward-sexp)
                (forward-char)
                (paredit-splice-sexp))
              (lexical-let ((p (point))) ; delete operator
                (forward-sexp)
                (delete-region p (point)))
              (lexical-let ((p (point))) ; copy first expr
                (forward-sexp)
                (lexical-let ((p2 (point)))
                  (with-current-buffer exp-buffer
                    (insert-buffer-substring-as-yank tmp-buffer (+ 1 p) p2))))
              (lexical-let ((p (point))) ; copy next expr
                (forward-sexp)
                (lexical-let ((p2 (point)))
                  (with-current-buffer act-buffer
                    (insert-buffer-substring-as-yank tmp-buffer (+ 1 p) p2)))))
            (kill-buffer tmp-buffer)
            (setq clojure-test-ediff-buffers
                  (list (buffer-name exp-buffer) (buffer-name act-buffer)))
            (ediff-buffers
             (buffer-name exp-buffer) (buffer-name act-buffer)))))))

(defun clojure-test-load-current-buffer ()
  (let ((command (format "(clojure.core/load-file \"%s\")\n(in-ns '%s)"
                         (buffer-file-name)
                         (clojure-find-ns))))
    (nrepl-send-string-sync command)))

(defun clojure-test-clear ()
  "Remove overlays and clear stored results."
  (interactive)
  (remove-overlays)
  (setq clojure-test-count 0
        clojure-test-failure-count 0
        clojure-test-error-count 0)
  (clojure-test-load-current-buffer))

(defun clojure-test-next-problem ()
  "Go to and describe the next test problem in the buffer."
  (interactive)
  (let* ((here (point))
         (problem (clojure-test-find-next-problem here)))
    (if problem
        (goto-char problem)
      (goto-char here)
      (message "No next problem."))))

(defun clojure-test-previous-problem ()
  "Go to and describe the previous compiler problem in the buffer."
  (interactive)
  (let* ((here (point))
         (problem (clojure-test-find-previous-problem here)))
    (if problem
        (goto-char problem)
      (goto-char here)
      (message "No previous problem."))))

(defun clojure-test-jump-to-implementation ()
  "Jump from test file to implementation."
  (interactive)
  (find-file (funcall clojure-test-implementation-for-fn
                      (clojure-find-ns))))

(make-obsolete 'clojure-test-jump-to-implementation
               "use projectile or toggle.el instead." "3.0.0")

(defvar clojure-test-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-,") 'clojure-test-run-tests)
    (define-key map (kbd "C-c ,")   'clojure-test-run-tests)
    (define-key map (kbd "C-c M-,") 'clojure-test-run-test)
    (define-key map (kbd "C-c C-'") 'clojure-test-ediff-result)
    (define-key map (kbd "C-c M-'") 'clojure-test-pprint-result)
    (define-key map (kbd "C-c '")   'clojure-test-show-result)
    (define-key map (kbd "C-c k")   'clojure-test-clear)
    (define-key map (kbd "C-c C-t") 'clojure-jump-between-tests-and-code)
    (define-key map (kbd "M-p")     'clojure-test-previous-problem)
    (define-key map (kbd "M-n")     'clojure-test-next-problem)
    map)
  "Keymap for Clojure test mode.")

;;;###autoload
(define-minor-mode clojure-test-mode
  "A minor mode for running Clojure tests.

\\{clojure-test-mode-map}"
  nil " Test" clojure-test-mode-map
  (when (cider-connected-p)
    (clojure-test-load-reporting)))

(add-hook 'nrepl-connected-hook 'clojure-test-load-reporting)

(defconst clojure-test-regex
  (rx "clojure.test"))

;;;###autoload
(defun clojure-find-clojure-test ()
  (let ((regexp clojure-test-regex))
    (save-restriction
      (save-excursion
        (save-match-data
          (goto-char (point-min))
          (when (re-search-forward regexp nil t)
            (match-string-no-properties 0)))))))

;;;###autoload
(progn
  (defun clojure-test-maybe-enable ()
    "Enable clojure-test-mode if the current buffer contains a \"clojure.test\" bit in it."
    (when (clojure-find-clojure-test)
      (save-window-excursion
        (clojure-test-mode t))))

  (add-hook 'clojure-mode-hook 'clojure-test-maybe-enable))

(provide 'clojure-test-mode)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; clojure-test-mode.el ends here
