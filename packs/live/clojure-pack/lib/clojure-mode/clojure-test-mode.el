;;; clojure-test-mode.el --- Minor mode for Clojure tests

;; Copyright © 2009-2011 Phil Hagelberg

;; Author: Phil Hagelberg <technomancy@gmail.com>
;; URL: http://emacswiki.org/cgi-bin/wiki/ClojureTestMode
;; Version: 1.6.0
;; Keywords: languages, lisp, test
;; Package-Requires: ((slime "20091016") (clojure-mode "1.7"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides support for running Clojure tests (using the
;; clojure.test framework) via SLIME and seeing feedback in the test
;; buffer about which tests failed or errored.

;;; Installation:

;; Use package.el. You'll need to add Marmalade to your archives:

;; (require 'package)
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/"))

;; If you use a version of Emacs prior to 24 that doesn't include
;; package.el, you can get it from http://bit.ly/pkg-el23. If you have
;; an older package.el installed from tromey.com, you should upgrade
;; in order to support installation from multiple sources.

;; This library does not currently support clojure.contrib.test-is
;; from Clojure Contrib's 1.0-compatibility branch. If you need it,
;; please use version 1.2 of clojure-test-mode:

;; http://github.com/technomancy/clojure-mode/tree/test-1.2

;;; Usage:

;; Once you have a SLIME session active, you can run the tests in the
;; current buffer with C-c C-,. Failing tests and errors will be
;; highlighted using overlays. To clear the overlays, use C-c k.

;; You can jump between implementation and test files with C-c t if
;; your project is laid out in a way that clojure-test-mode
;; expects. Your project root should have a src/ directory containing
;; files that correspond to their namespace. It should also have a
;; test/ directory containing files that correspond to their
;; namespace, and the test namespaces should mirror the implementation
;; namespaces with the addition of "test" as the second-to-last
;; segment of the namespace.

;; So my.project.frob would be found in src/my/project/frob.clj and
;; its tests would be in test/my/project/test/frob.clj in the
;; my.project.test.frob namespace.

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

;;; TODO:

;; * Prefix arg to jump-to-impl should open in other window
;; * Put Testing indicator in modeline while tests are running
;; * Implement next-problem command
;; * Error messages need line number.
;; * Currently show-message needs point to be on the line with the
;;   "is" invocation; this could be cleaned up.

;;; Code:

(require 'clojure-mode)
(require 'cl)
(require 'slime)
(require 'which-func)

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

(defun clojure-test-eval (string &optional handler)
  (slime-eval-async `(swank:eval-and-grab-output ,string)
                    (or handler #'identity)))

(defun clojure-test-eval-sync (string)
  (slime-eval `(swank:eval-and-grab-output ,string)))

(defun clojure-test-load-reporting ()
  "Redefine the test-is report function to store results in metadata."
  (when (eq (compare-strings "clojure" 0 7 (slime-connection-name) 0 7) t)
    (clojure-test-eval-sync
     "(ns clojure.test.mode
        (:use [clojure.test :only [file-position *testing-vars* *test-out*
                                   join-fixtures *report-counters* do-report
                                   test-var *initial-report-counters*]]))

    (def #^{:dynamic true} *clojure-test-mode-out* nil)
    (defn report [event]
     (if-let [current-test (last clojure.test/*testing-vars*)]
             (alter-meta! current-test
                          assoc :status (conj (:status (meta current-test))
                                          [(:type event) (:message event)
                                           (str (:expected event))
                                           (str (:actual event))
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
        (do-report (assoc @*report-counters* :type :summary)))) ")))

(defun clojure-test-get-results (result)
  (clojure-test-eval
   (concat "(map #(cons (str (:name (meta %)))
                (:status (meta %))) (vals (ns-interns '"
           (slime-current-package) ")))")
   #'clojure-test-extract-results))

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

(defun clojure-test-extract-results (results)
  (let ((result-vars (read (cadr results))))
    ;; slime-eval-async hands us a cons with a useless car
    (mapc #'clojure-test-extract-result result-vars)
    ;; (slime-repl-emit (concat "\n" (make-string (1- (window-width)) ?=) "\n"))
    (clojure-test-echo-results)))

(defun clojure-test-extract-result (result)
  "Parse the result from a single test. May contain multiple is blocks."
  (dolist (is-result (rest result))
    (unless (member (aref is-result 0) clojure-test-ignore-results)
      (incf clojure-test-count)
      (destructuring-bind (event msg expected actual line) (coerce is-result 'list)
        (if (equal :fail event)
            (progn (incf clojure-test-failure-count)
                   (clojure-test-highlight-problem
                    line event (format "Expected %s, got %s" expected actual)))
          (when (equal :error event)
            (incf clojure-test-error-count)
            (clojure-test-highlight-problem line event actual)))))))


(defun clojure-test-highlight-problem (line event message)
  (save-excursion
    (goto-line line)
    (let ((beg (point)))
      (end-of-line)
      (let ((overlay (make-overlay beg (point))))
        (overlay-put overlay 'face (if (equal event :fail)
                                       'clojure-test-failure-face
                                     'clojure-test-error-face))
        (overlay-put overlay 'message message)))))

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
  (let* ((namespace (clojure-underscores-for-hyphens namespace))
         (segments (split-string namespace "\\."))
         (test-position
          (if (> 0 clojure-test-ns-segment-position)
              (1- (+ (length segments) clojure-test-ns-segment-position))
            clojure-test-ns-segment-position))
         (before (subseq segments 0 test-position))
         (after (subseq segments (1+ test-position)))
         (impl-segments (append before after)))
    (mapconcat 'identity impl-segments "/")))

;; Commands

(defun clojure-test-run-tests ()
  "Run all the tests in the current namespace."
  (interactive)
  (save-some-buffers nil (lambda () (equal major-mode 'clojure-mode)))
  (message "Testing...")
  (save-window-excursion
    (if (not (clojure-in-tests-p))
        (clojure-jump-to-test))
    (clojure-test-clear
     (lambda (&rest args)
       ;; clojure-test-eval will wrap in with-out-str
       (slime-eval-async `(swank:load-file
                           ,(slime-to-lisp-filename
                             (expand-file-name (buffer-file-name))))
                         (lambda (&rest args)
                           (slime-eval-async '(swank:interactive-eval
                                               "(binding [clojure.test/report
                                               clojure.test.mode/report]
                                                (clojure.test/run-tests))")
                                             #'clojure-test-get-results)))))))
(defun clojure-test-run-test ()
  "Run the test at point."
  (interactive)
  (save-some-buffers nil (lambda () (equal major-mode 'clojure-mode)))
  (clojure-test-clear
   (lambda (&rest args)
     (let* ((f (which-function))
            (test-name (if (listp f) (first f) f)))
       (slime-eval-async
        `(swank:interactive-eval
          ,(format "(binding [clojure.test/report clojure.test.mode/report]
                        (load-file \"%s\")
                        (clojure.test.mode/clojure-test-mode-test-one-in-ns '%s '%s)
                        (cons (:name (meta (var %s))) (:status (meta (var %s)))))"
                   (buffer-file-name)
                   (slime-current-package) test-name
                   test-name test-name))
        (lambda (result-str)
          (let ((result (read result-str)))
            (if (cdr result)
                (clojure-test-extract-result result)
              (message "Not in a test.")))))))))

(defun clojure-test-show-result ()
  "Show the result of the test under point."
  (interactive)
  (let ((overlay (find-if (lambda (o) (overlay-get o 'message))
                          (overlays-at (point)))))
    (if overlay
        (message (replace-regexp-in-string "%" "%%"
                                           (overlay-get overlay 'message))))))

(defun clojure-test-clear (&optional callback)
  "Remove overlays and clear stored results."
  (interactive)
  (remove-overlays)
  (setq clojure-test-count 0
        clojure-test-failure-count 0
        clojure-test-error-count 0)
  (clojure-test-eval
   "(doseq [t (vals (ns-interns *ns*))]
      (alter-meta! t assoc :status [])
      (alter-meta! t assoc :test nil))"
   callback))

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
  (find-file (format "%s/src/%s.clj"
                     (locate-dominating-file buffer-file-name "src/")
                     (clojure-test-implementation-for (clojure-find-package)))))

(defvar clojure-test-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-,") 'clojure-test-run-tests)
    (define-key map (kbd "C-c ,")   'clojure-test-run-tests)
    (define-key map (kbd "C-c M-,") 'clojure-test-run-test)
    (define-key map (kbd "C-c C-'") 'clojure-test-show-result)
    (define-key map (kbd "C-c '")   'clojure-test-show-result)
    (define-key map (kbd "C-c k")   'clojure-test-clear)
    (define-key map (kbd "C-c t")   'clojure-test-jump-to-implementation)
    (define-key map (kbd "M-p")     'clojure-test-previous-problem)
    (define-key map (kbd "M-n")     'clojure-test-next-problem)
    map)
  "Keymap for Clojure test mode.")

;;;###autoload
(define-minor-mode clojure-test-mode
  "A minor mode for running Clojure tests."
  nil " Test" clojure-test-mode-map
  (when (slime-connected-p)
    (clojure-test-load-reporting)))

(add-hook 'slime-connected-hook 'clojure-test-load-reporting)

;;;###autoload
(progn
  (defun clojure-test-maybe-enable ()
    "Enable clojure-test-mode if the current buffer contains a namespace
with a \"test.\" bit on it."
    (let ((ns (clojure-find-package))) ; defined in clojure-mode.el
      (when (and ns (string-match "test\\(\\.\\|$\\)" ns))
        (save-window-excursion
          (clojure-test-mode t)))))
  (add-hook 'clojure-mode-hook 'clojure-test-maybe-enable))

(provide 'clojure-test-mode)
;;; clojure-test-mode.el ends here
