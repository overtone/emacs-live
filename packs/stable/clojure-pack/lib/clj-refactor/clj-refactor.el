;;; clj-refactor.el --- A collection of clojure refactoring functions

;; Copyright © 2012-2014 Magnar Sveen <magnars@gmail.com>

;; Author: Magnar Sveen <magnars@gmail.com>
;; Version: 0.13.0
;; Keywords: convenience
;; Package-Requires: ((s "1.8.0") (dash "2.4.0") (yasnippet "0.6.1") (paredit "22") (multiple-cursors "1.2.2") (cider "0.6.0"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; ## Installation
;;
;; I highly recommended installing clj-refactor through elpa.
;;
;; It's available on [marmalade](http://marmalade-repo.org/) and
;; [melpa](http://melpa.milkbox.net/):
;;
;;     M-x package-install clj-refactor
;;
;; You can also install the dependencies on your own, and just dump
;; clj-refactor in your path somewhere:
;;
;;  - <a href="https://github.com/magnars/s.el">s.el</a>
;;  - <a href="https://github.com/magnars/dash.el">dash.el</a>
;;

;; ## Setup
;;
;;     (require 'clj-refactor)
;;     (add-hook 'clojure-mode-hook (lambda ()
;;                                    (clj-refactor-mode 1)
;;                                    ;; insert keybinding setup here
;;                                    ))
;;
;; You'll also have to set up the keybindings in the lambda. Read on.

;; ## Setup keybindings
;;
;; All functions in clj-refactor have a two-letter mnemonic shortcut. You
;; get to choose how those are bound. Here's how:
;;
;;     (cljr-add-keybindings-with-prefix "C-c C-m")
;;     ;; eg. rename files with `C-c C-m rf`.
;;
;; If you would rather have a modifier key, instead of a prefix, do:
;;
;;     (cljr-add-keybindings-with-modifier "C-s-")
;;     ;; eg. rename files with `C-s-r C-s-f`.
;;
;; If neither of these appeal to your sense of keyboard layout aesthetics, feel free
;; to pick and choose your own keybindings with a smattering of:
;;
;;     (define-key clj-refactor-map (kbd "C-x C-r") 'cljr-rename-file)

;; ## Use
;;
;; This is it so far:
;;
;;  - `rf`: rename file, update ns-declaration, and then query-replace new ns in project.
;;  - `ar`: add :require to namespace declaration, then jump back
;;  - `au`: add :use to namespace declaration, then jump back
;;  - `ai`: add :import to namespace declaration, then jump back
;;  - `th`: thread another expression
;;  - `uw`: unwind a threaded expression
;;
;; Combine with your keybinding prefix/modifier.

;; ## Automatic insertion of namespace declaration
;;
;; When you open a blank `.clj`-file, clj-refactor inserts the namespace
;; declaration for you.
;;
;; It will also add the relevant `:use` clauses in test files, normally
;; using `clojure.test`, but if you're depending on midje in your
;; `project.clj` it uses that instead.
;;
;; Like clojure-mode, clj-refactor presumes that you are postfixing your
;; test files with `_test`.
;;
;; Prefer to insert your own ns-declarations? Then:
;;
;; (setq clj-add-ns-to-blank-clj-files nil)

;;; Code:

(require 'dash)
(require 's)
(require 'yasnippet)
(require 'paredit)
(require 'multiple-cursors-core)
(require 'clojure-mode)
(require 'cider)

(defcustom cljr-add-ns-to-blank-clj-files t
  "When true, automatically adds a ns form to new clj files."
  :group 'cljr
  :type 'boolean)

(defcustom cljr-sort-comparator 'cljr--string-natural-comparator
  "The comparator function to use to sort ns declaration. Set your
   own if you see fit. Comparator is called with two elements of
   the sub section of the ns declaration, and should return non-nil
   if the first element should sort before the second."
  :group 'cljr
  :type 'function)

(defcustom cljr-auto-sort-ns t
  "When true, sort ns form whenever adding to the form using clj-refactor
   functions."
  :group 'cljr
  :type 'boolean)

(defcustom cljr-magic-requires t
  "When true, suggests requiring common namespaces when you type
  its short form. Set to :prompt to ask before doing anything."
  :group 'cljr
  :type '(choice (const :tag "true" t)
                 (const :tag "prompt" :prompt)
                 (const :tag "false" nil)))

(defcustom cljr-use-metadata-for-privacy nil
  "When nil, `cljr-cycle-privacy' will use (defn- f []).
   When t, it will use (defn ^:private f [])"
  :group 'cljr
  :type 'boolean)

(defcustom cljr-project-clean-prompt t
  "When true prompts to ask before doing anything if false
   runs project clean functions without warning."
  :group 'cljr
  :type 'boolean)

(defcustom cljr-project-clean-functions
  (list 'cljr-remove-unused-requires 'cljr-sort-ns)
  "List of functions to run on all the clj files in the project
   when you perform project clean."
  :group 'cljr
  :type '(repeat function))

(defcustom cljr-debug-functions "println,pr,prn"
  "List of functions used for debug purposes.
Used in `cljr-remove-debug-fns' feature."
  :group 'cljr
  :type 'string)

(defvar cljr-magic-require-namespaces
  '(("io"   . "clojure.java.io")
    ("set"  . "clojure.set")
    ("str"  . "clojure.string")
    ("walk" . "clojure.walk")
    ("zip"  . "clojure.zip")))

(defvar clj-refactor-map (make-sparse-keymap) "")

(define-key clj-refactor-map [remap paredit-raise-sexp] 'cljr-raise-sexp)
(define-key clj-refactor-map [remap paredit-splice-sexp-killing-backward] 'cljr-splice-sexp-killing-backward)
(define-key clj-refactor-map [remap paredit-splice-sexp-killing-forward] 'cljr-splice-sexp-killing-forward)
(define-key clj-refactor-map (kbd "/") 'cljr-slash)

(defun cljr--fix-special-modifier-combinations (key)
  (case key
    ("C-s-i" "s-TAB")
    ("C-s-m" "s-RET")
    (otherwise key)))

(defun cljr--key-pairs-with-modifier (modifier keys)
  (->> (string-to-list keys)
    (--map (cljr--fix-special-modifier-combinations
            (concat modifier (char-to-string it))))
    (s-join " ")
    (read-kbd-macro)))

(defun cljr--key-pairs-with-prefix (prefix keys)
  (read-kbd-macro (concat prefix " " keys)))

(defun cljr--add-keybindings (key-fn)
  (define-key clj-refactor-map (funcall key-fn "ad") 'cljr-add-declaration)
  (define-key clj-refactor-map (funcall key-fn "ai") 'cljr-add-import-to-ns)
  (define-key clj-refactor-map (funcall key-fn "ap") 'cljr-add-project-dependency)
  (define-key clj-refactor-map (funcall key-fn "ar") 'cljr-add-require-to-ns)
  (define-key clj-refactor-map (funcall key-fn "au") 'cljr-add-use-to-ns)
  (define-key clj-refactor-map (funcall key-fn "cc") 'cljr-cycle-coll)
  (define-key clj-refactor-map (funcall key-fn "ci") 'cljr-cycle-if)
  (define-key clj-refactor-map (funcall key-fn "cp") 'cljr-cycle-privacy)
  (define-key clj-refactor-map (funcall key-fn "cs") 'cljr-cycle-stringlike)
  (define-key clj-refactor-map (funcall key-fn "ct") 'cljr-cycle-thread)
  (define-key clj-refactor-map (funcall key-fn "dk") 'cljr-destructure-keys)
  (define-key clj-refactor-map (funcall key-fn "el") 'cljr-expand-let)
  (define-key clj-refactor-map (funcall key-fn "il") 'cljr-introduce-let)
  (define-key clj-refactor-map (funcall key-fn "mf") 'cljr-move-form)
  (define-key clj-refactor-map (funcall key-fn "ml") 'cljr-move-to-let)
  (define-key clj-refactor-map (funcall key-fn "pc") 'cljr-project-clean)
  (define-key clj-refactor-map (funcall key-fn "rf") 'cljr-rename-file)
  (define-key clj-refactor-map (funcall key-fn "rr") 'cljr-remove-unused-requires)
  (define-key clj-refactor-map (funcall key-fn "ru") 'cljr-replace-use)
  (define-key clj-refactor-map (funcall key-fn "sn") 'cljr-sort-ns)
  (define-key clj-refactor-map (funcall key-fn "sp") 'cljr-sort-project-dependencies)
  (define-key clj-refactor-map (funcall key-fn "sr") 'cljr-stop-referring)
  (define-key clj-refactor-map (funcall key-fn "tf") 'cljr-thread-first-all)
  (define-key clj-refactor-map (funcall key-fn "th") 'cljr-thread)
  (define-key clj-refactor-map (funcall key-fn "tl") 'cljr-thread-last-all)
  (define-key clj-refactor-map (funcall key-fn "ua") 'cljr-unwind-all)
  (define-key clj-refactor-map (funcall key-fn "uw") 'cljr-unwind)
  (define-key clj-refactor-map (funcall key-fn "rd") 'cljr-remove-debug-fns))

;;;###autoload
(defun cljr-add-keybindings-with-prefix (prefix)
  (cljr--add-keybindings (-partial 'cljr--key-pairs-with-prefix prefix)))

;;;###autoload
(defun cljr-add-keybindings-with-modifier (modifier)
  (cljr--add-keybindings (-partial 'cljr--key-pairs-with-modifier modifier)))

;; ------ utilities -----------

(defun cljr--delete-and-extract-sexp ()
  (let* ((beg (point))
         (end (progn (paredit-forward)
                     (point)))
         (contents (buffer-substring beg end)))
    (delete-region beg end)
    contents))

(defun cljr--delete-and-extract-sexp-with-nested-sexps ()
  "Returns list of strings representing the nested sexps if there is any.
   In case there are no nested sexp the list will have only one element.
   Not recursive, does not drill down into nested sexps
   inside the first level nested sexps."
  (let* ((beg (point))
         (sexp-start beg)
         (end (progn (paredit-forward)
                     (point)))
         nested)
    (paredit-backward)
    (paredit-forward-down)
    (while (/= sexp-start end)
      (paredit-move-forward)
      (push (s-trim (buffer-substring sexp-start (point))) nested)
      (setq sexp-start (point)))
    (delete-region beg end)
    (nreverse (cons (concat (nth 1 nested) (car nested)) (or (nthcdr 2 nested) '())))))

(defun cljr--search-forward-within-sexp (s &optional save-excursion)
  "Searches forward for S in the current sexp.

if SAVE-EXCURSION is T POINT does not move."
  (let ((bound (save-excursion (forward-list 1) (point))))
    (if save-excursion
        (save-excursion
          (search-forward s bound t))
      (search-forward s bound t))))

(defun cljr--goto-toplevel ()
  (paredit-backward-up (cljr--depth-at-point)))

(defun cljr--toplevel-p ()
  "T unless we're in an s-expression or string."
  (= (cljr--depth-at-point) 0))

(defun cljr--depth-at-point ()
  "Returns the depth in s-expressions, or strings, at point."
  (let ((depth (first (paredit-current-parse-state))))
    (if (paredit-in-string-p)
        (1+ depth)
      depth)))

(defun cljr--cleanup-whitespace (stuff)
  "Removes blank lines preceding `stuff' as well as trailing whitespace."
  (with-temp-buffer
    (insert stuff)
    (goto-char (point-min))
    (delete-blank-lines)
    (when (looking-at "[ \t]*$")
      (delete-region (point-at-bol) (point-at-eol)))
    (let ((delete-trailing-lines t))
      (delete-trailing-whitespace)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun cljr--delete-line ()
  "Deletes the current line without introducing whitespace
errors."
  (delete-region (point-at-bol) (line-end-position))
  (join-line)
  (paredit-forward-delete 1))

(defun cljr--just-one-blank-line ()
  (newline 2)
  (forward-line -1)
  (delete-blank-lines))

;; ------ file -----------

(defun cljr--project-dir ()
  (or (ignore-errors
        (file-truename
         (locate-dominating-file default-directory "project.clj")))
      (ignore-errors (file-truename
                      (locate-dominating-file default-directory "pom.xml")))))

(defun cljr--project-file ()
  (or (ignore-errors
        (expand-file-name "project.clj" (cljr--project-dir)))
      (ignore-errors (expand-file-name "pom.xml" (cljr--project-dir)))))

(defun cljr--project-files ()
  (split-string (shell-command-to-string
                 (format "find %s -type f \\( %s \\) %s | head -n %s"
                         (cljr--project-dir)
                         (format "-name \"%s\"" "*.clj")
                         "-not -regex \".*svn.*\""
                         1000))))

(defun cljr--rename-file (filename new-name)
  (let ((old-ns (clojure-find-ns)))
    (rename-file filename new-name 1)
    (rename-buffer new-name)
    (set-visited-file-name new-name)
    (clojure-update-ns)
    (let ((old-syntax (char-to-string (char-syntax ?/))))
      (modify-syntax-entry ?/ " ")
      (save-window-excursion
        (save-excursion
          (ignore-errors
            (tags-query-replace (concat (regexp-quote old-ns) "\\_>")
                                (clojure-expected-ns) nil
                                '(cljr--project-files)))))
      (modify-syntax-entry ?/ old-syntax))
    (save-buffer)
    (save-some-buffers)))

;;;###autoload
(defun cljr-rename-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (cljr--rename-file filename new-name)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

;; ------ ns statements -----------

(defun cljr--goto-ns ()
  (goto-char (point-min))
  (if (re-search-forward clojure-namespace-name-regex nil t)
      (cljr--goto-toplevel)
    (error "No namespace declaration found")))

(defun cljr--insert-in-ns (type)
  (cljr--goto-ns)
  (if (cljr--search-forward-within-sexp (concat "(" type))
      (if (looking-at " *)")
          (progn
            (search-backward "(")
            (forward-list 1)
            (forward-char -1)
            (insert " "))
        (search-backward "(")
        (forward-list 1)
        (forward-char -1)
        (newline-and-indent))
    (forward-list 1)
    (forward-char -1)
    (newline-and-indent)
    (insert "(" type " )")
    (forward-char -1)))

(defun cljr--project-depends-on-p (package)
  (save-window-excursion
    (find-file (cljr--project-file))
    (goto-char (point-min))
    (search-forward package nil t)))

(defun cljr--add-test-use-declarations ()
  (save-excursion
    (let ((ns (clojure-find-ns)))
      (cljr--insert-in-ns ":require")
      (insert "[" (s-chop-suffix "-test" ns) " :refer :all]")
      (cljr--insert-in-ns ":require")
      (insert "[" (if (cljr--project-depends-on-p "midje")
                      "midje.sweet"
                    "clojure.test")
              " :refer :all]"))))

(defun cljr--in-tests-p ()
  "Check whether the current file is a test file.

Two checks are made - whether the namespace of the file has the
word test in it and whether the file lives under the test/ directory."
  (or (string-match-p "test\." (clojure-find-ns))
      (string-match-p "/test" (buffer-file-name))))

(defun cljr--add-ns-if-blank-clj-file ()
  (ignore-errors
    (when (and cljr-add-ns-to-blank-clj-files
               (or (s-ends-with? ".clj" (buffer-file-name))
                   (s-ends-with? ".cljs" (buffer-file-name))
                   (s-ends-with? ".cljx" (buffer-file-name)))
               (= (point-min) (point-max)))
      (clojure-insert-ns-form)
      (newline 2)
      (when (cljr--in-tests-p)
        (cljr--add-test-use-declarations)))))

(add-hook 'find-file-hook 'cljr--add-ns-if-blank-clj-file)

(defun cljr--verify-underscores-in-filename ()
  (let ((file-name (buffer-file-name)))
    (when (and
           file-name
           (not (file-exists-p file-name)) ;; only new files
           (s-matches? "-[^/]+\.clj$" file-name)
           (yes-or-no-p "The file name contains dashes. Replace with underscores?"))
      (let ((new-name (concat
                       (file-name-directory file-name)
                       (s-replace "-" "_" (file-name-nondirectory file-name)))))
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (message "Changed file name to '%s'"
                 (file-name-nondirectory new-name))))))

(add-hook 'find-file-hook 'cljr--verify-underscores-in-filename)

(defun cljr--extract-ns-statements (statement-type with-nested)
  (cljr--goto-ns)
  (if (not (cljr--search-forward-within-sexp (concat "(" statement-type)))
      '()
    (let (statements)
      (while (not (looking-at " *)"))
        (push (if with-nested
                  (cljr--delete-and-extract-sexp-with-nested-sexps)
                (cljr--delete-and-extract-sexp)) statements))
      statements)))

(defun cljr--only-alpha-chars (s)
  (replace-regexp-in-string "[^[:alnum:]]" "" s))

(defun cljr--string-natural-comparator (s1 s2)
  (string< (cljr--only-alpha-chars s1)
           (cljr--only-alpha-chars s2)))

(defun cljr--string-length-comparator (s1 s2)
  (> (length s1)
     (length s2)))

(defun cljr--semantic-comparator (ns s1 s2)
  "Sorts used, required namespaces closer to the ns of the current buffer
   before the rest.
   When above is not applicable falls back to natural comparator."
  (let ((shared-length-s1
         (length (s-shared-start ns (cljr--extract-sexp-content s1))))
        (shared-length-s2
         (length (s-shared-start ns (cljr--extract-sexp-content s2)))))
    (if (/= shared-length-s1 shared-length-s2)
        (> shared-length-s1 shared-length-s2)
      (cljr--string-natural-comparator s1 s2))))

(defun cljr-create-comparator (comparator-fn)
  (if (eq comparator-fn 'cljr--semantic-comparator)
      (-partial 'cljr--semantic-comparator (clojure-find-ns))
    comparator-fn))

;;;###autoload
(defun cljr-sort-ns ()
  (interactive)
  (save-excursion
    (let ((comparator (cljr-create-comparator cljr-sort-comparator)))
      (dolist (statement-type '(":require" ":use" ":import"))
        (ignore-errors
          (dolist (statement (->> (cljr--extract-ns-statements statement-type nil)
                               (-map 's-trim)
                               (-sort comparator)
                               (-distinct)))
            (cljr--insert-in-ns statement-type)
            (insert statement)))))))

(defun cljr--is-require-flag (req-statement)
  (let ((t-req (s-trim req-statement)))
    (or (string= t-req ":reload")
        (string= t-req ":reload-all")
        (string= t-req ":verbose"))))

(defun cljr--req-element-regexp (refered postfix)
  (concat "^[[:space:]]*[^;]*"
          "[^[:word:]^-]"
          (regexp-quote refered)
          postfix))

(defun cljr--extract-sexp-content (sexp)
  (replace-regexp-in-string "\\[?(?]?)?" "" sexp))

(defun cljr--is-name-in-use-ast-p (name)
  (cljr--goto-ns)
  (paredit-forward)
  (let* ((body (replace-regexp-in-string "\"" "\"" (buffer-substring-no-properties (point-min) (point-max))))
         (e (cljr--extract-sexp-content name))
         (result (plist-get (nrepl-send-request-sync
                             (list "op" "refactor"
                                   "ns-string" body
                                   "refactor-fn" "find-referred"
                                   "referred" e))
                            :value)))
    (when result e)))

(defun cljr--is-name-in-use-vanilla-p (name)
  (goto-char (point-min))
  (let ((e (cljr--extract-sexp-content name)))
    (when (re-search-forward (cljr--req-element-regexp e "[^[:word:]^-]") nil t) e)))

(defun cljr--is-name-in-use-p (name)
  (if (and (cider-connected-p) (nrepl-op-supported-p "refactor"))
      (progn
        (message "refactor-nrepl is used")
        (cljr--is-name-in-use-ast-p name))
    (progn
      (message "clj-refactor middleware is not found. Failing back to vanilla elisp impl")
      (cljr--is-name-in-use-vanilla-p name))))

(defun cljr-remove-debug-fns ()
  (interactive)
  (cljr--assert-middleware)
  (let* ((body (replace-regexp-in-string "\"" "\"" (buffer-substring-no-properties (point-min) (point-max))))
         (result (plist-get (nrepl-send-request-sync
                             (list "op" "refactor"
                                   "ns-string" body
                                   "refactor-fn" "find-debug-fns"
                                   "debug-fns" cljr-debug-functions))
                            :value))
         (debug-fn-tuples (pop result))
         (removed-lines 0))
    (while debug-fn-tuples
      (let ((line (- (1- (car debug-fn-tuples)) removed-lines))
            (end-line (nth 1 debug-fn-tuples))
            (column (nth 2 debug-fn-tuples)))
        (message "removing %s at line %s [%s] column %s (end-line %s end-column %s)" (-last-item debug-fn-tuples) line (car debug-fn-tuples) column end-line (nth 3 debug-fn-tuples))
        (save-excursion
          (goto-char (point-min))
          (forward-line line)
          (move-to-column column)
          (paredit-backward)
          (cljr--delete-and-extract-sexp)
          (join-line))
        (setq removed-lines (+ removed-lines (1+ (- end-line (car debug-fn-tuples))))))
      (setq debug-fn-tuples (pop result)))))


(defun cljr--rectify-refer-type-require (sexp-as-list refer-index as-used as-index)
  (let* ((as-after-refer (and as-used (> as-index refer-index)))
         (sexp-wo-as (if as-after-refer
                         (-take as-index sexp-as-list)
                       sexp-as-list))
         (referred-names (->> sexp-wo-as
                           (nthcdr (1+ refer-index))
                           (-map 'cljr--is-name-in-use-p)
                           (delq nil))))
    (cond (referred-names
           (format "%s [%s]%s"
                   (s-join " " (if (and as-used (< as-index refer-index))
                                   (-take (1+ refer-index) sexp-as-list)
                                 (list (replace-regexp-in-string "(" "[" (car sexp-as-list)) ":refer")))
                   (s-join " " referred-names)
                   (if as-after-refer
                       (concat " " (s-join " " (list ":as" (nth (1+ as-index) sexp-as-list))))
                     "]")))
          (as-used
           (format "%s]" (s-join " " (list (car sexp-as-list)
                                           (nth as-index sexp-as-list)
                                           (cljr--extract-sexp-content (nth (1+ as-index) sexp-as-list)))))))))

(defun cljr--is-simple-req-statement-in-use (sexp as-list alias-used refer-used)
  (or (s-match ":refer[[:space:]]+:all" sexp)
      (cljr--is-require-flag (cljr--extract-sexp-content sexp))
      (and (= 1 (safe-length as-list))
           (re-search-forward (cljr--req-element-regexp (cljr--extract-sexp-content (car as-list)) "/") nil t))
      (and alias-used (not refer-used))))

(defun cljr--rectify-simple-req-statement (req sexp-as-list)
  (save-excursion
    (goto-char (point-min))
    (let* ((refer-index (-elem-index ":refer" sexp-as-list))
           (as-index (-elem-index ":as" sexp-as-list))
           (as-used (and as-index
                         (re-search-forward (cljr--req-element-regexp (cljr--extract-sexp-content (nth (1+ as-index) sexp-as-list)) "/") nil t))))
      (cond ((cljr--is-simple-req-statement-in-use req sexp-as-list as-used refer-index) req)
            (refer-index
             (cljr--rectify-refer-type-require sexp-as-list refer-index as-used as-index))))))

(defun cljr--is-prefix-element-in-use (f-elem p-elem)
  (goto-char (point-min))
  (let ((elem (replace-regexp-in-string "]]]?" "]" p-elem)))
    (if (s-matches? "^\\[\\|(" elem)
        (let ((result (cljr--rectify-simple-req-statement elem (split-string elem))))
          (when result (concat "\n" result)))
      (when (re-search-forward (cljr--req-element-regexp (s-join "." (list f-elem (cljr--extract-sexp-content elem))) "/") nil t) (cljr--extract-sexp-content elem)))))

(defun cljr--rectify-prefix-list-req-statement (require-as-list)
  (let* ((first-element (cljr--extract-sexp-content (car require-as-list)))
         (used-elements (->> require-as-list
                          (nthcdr 1)
                          (-map (apply-partially 'cljr--is-prefix-element-in-use first-element))
                          (delq nil))))
    (when used-elements
      (format "[%s %s]" first-element (s-join " " used-elements)))))

(defun cljr--rectify-req-statement (require-as-list)
  (save-excursion
    (let ((sexp-as-list (-flatten (-map (lambda (sexp) (split-string sexp)) require-as-list))))
      (if (or (= 1 (safe-length sexp-as-list))
              (string= ":refer" (nth 1 sexp-as-list))
              (string= ":as" (nth 1 sexp-as-list)))
          (cljr--rectify-simple-req-statement (s-join " " require-as-list) sexp-as-list)
        (cljr--rectify-prefix-list-req-statement require-as-list)))))

(defun cljr--remove-require ()
  (search-backward "(")
  (cljr--delete-and-extract-sexp)
  (join-line))

;;;###autoload
(defun cljr-remove-unused-requires ()
  (interactive)
  (save-excursion
    (let (req-exists)
      (dolist (statement (->> (cljr--extract-ns-statements ":require" t)
                           (-map 'cljr--rectify-req-statement)
                           (delq nil)
                           (nreverse)))
        (cljr--insert-in-ns ":require")
        (insert statement)
        (setq req-exists t))
      (when (not req-exists) (cljr--remove-require)))
    (paredit-backward-up)
    (let ((beg (point))
          (end (progn (paredit-forward) (point))))
      (indent-region beg end)
      (when cljr-auto-sort-ns
        (cljr-sort-ns)))))

(defvar cljr--tmp-marker (make-marker))

(defun cljr--pop-tmp-marker-after-yasnippet-1 (&rest ignore)
  (goto-char cljr--tmp-marker)
  (set-marker cljr--tmp-marker nil)
  (remove-hook 'yas/after-exit-snippet-hook 'cljr--pop-tmp-marker-after-yasnippet-1 t))

(defun cljr--pop-tmp-marker-after-yasnippet ()
  (add-hook 'yas/after-exit-snippet-hook 'cljr--pop-tmp-marker-after-yasnippet-1 nil t))

(defun cljr--sort-and-remove-hook (&rest ignore)
  (cljr-sort-ns)
  (remove-hook 'yas/after-exit-snippet-hook 'cljr--pop-tmp-marker-after-yasnippet-1 t))

(defun cljr--add-yas-snippet-sort-ns-hook ()
  (add-hook 'yas/after-exit-snippet-hook 'cljr--sort-and-remove-hook nil t))

;;;###autoload
(defun cljr-add-require-to-ns ()
  (interactive)
  (set-marker cljr--tmp-marker (point))
  (cljr--insert-in-ns ":require")
  (cljr--pop-tmp-marker-after-yasnippet)
  (when cljr-auto-sort-ns
    (cljr--add-yas-snippet-sort-ns-hook))
  (yas-expand-snippet "${1:[${2:$3 :as $4}]}$0"))

;;;###autoload
(defun cljr-add-use-to-ns ()
  (interactive)
  (set-marker cljr--tmp-marker (point))
  (cljr--insert-in-ns ":require")
  (cljr--pop-tmp-marker-after-yasnippet)
  (when cljr-auto-sort-ns
    (cljr--add-yas-snippet-sort-ns-hook))
  (yas-expand-snippet "[$1 :refer ${2:[$3]}]$0"))

;;;###autoload
(defun cljr-add-import-to-ns ()
  (interactive)
  (set-marker cljr--tmp-marker (point))
  (cljr--insert-in-ns ":import")
  (cljr--pop-tmp-marker-after-yasnippet)
  (when cljr-auto-sort-ns
    (cljr--add-yas-snippet-sort-ns-hook))
  (yas-expand-snippet "$1"))

(defun cljr--extract-ns-from-use ()
  "Let point be denoted by |.  Then, when called on: |[used.ns ...]
returns used.ns, when called on (:use some.ns) returns some.ns"
  (let* ((form (format "%s" (sexp-at-point))))
    (if (looking-at "(:use [A-z.0-9-]+)")
        (s-chop-suffix ")" (second (s-split " " form)))
      (substring form 1 (min (or (s-index-of " " form) (1- (length form))
                                 (1- (length form))))))))

(defun cljr--extract-multiple-ns-from-use ()
  "Let point be denoted by |.  Then, when called on: |[used.ns lib1 lib2]
returns (used.ns.lib1 used.ns.lib2)"
  (let* ((form (format "%s" (sexp-at-point)))
         (form (substring form 1 (1- (length form))))
         (words (s-split " " form))
         (prefix (pop words))
         (libs (nreverse words)))
    (-map (lambda (lib) (concat prefix "." lib)) libs)))

(defun cljr--multiple-namespaces-p (use-form)
  "Returns t if the use form looks like [some.lib ns1 ns2 ...]"
  (unless (s-contains? ":only" (format "%s" use-form))
    (s-matches-p "\\[[A-z0-9.]+ \\(\\([A-z0-9]+ \\)\\|\\([A-z0-9]+\\)\\)+\\]"
                 (format "%s" use-form))))

(defun cljr--more-namespaces-in-use-p (nth)
  "Checks for, and moves POINT to, the NTH :use clause."
  (cljr--goto-ns)
  (cljr--search-forward-within-sexp "(:use ")
  (paredit-backward-up)
  (let ((use-end (save-excursion (forward-sexp) (point))))
    (prog1
        (re-search-forward "\\(\\(\\( \\)\\{2,\\}\\|:use \\)\\(\\[\\(.\\|\n\\)*?\\]\\)\\)\\|\\((:use [^]]+?)\\)" use-end t nth)
      (if (and (looking-back "\\]") (looking-at "\\]"))
          (paredit-backward-up)
        (paredit-backward)))))

(defun cljr--extract-used-namespaces ()
  "Return list of all the namespaces that are :used."
  (let (libs use-start next-use-clause)
    (cljr--goto-ns)
    (if (not (cljr--search-forward-within-sexp "(:use "))
        (message "There is no :use clause in the ns declaration.")
      (save-excursion
        (paredit-backward-up)
        (paredit-forward))
      (let ((next-use-clause 1))
        (while (cljr--more-namespaces-in-use-p next-use-clause)
          (push (if (cljr--multiple-namespaces-p (sexp-at-point))
                    (cljr--extract-multiple-ns-from-use)
                  (cljr--extract-ns-from-use))
                libs)
          (setq next-use-clause (1+ next-use-clause)))
        (nreverse (-flatten libs))))))

;;;###autoload
(defun cljr-replace-use ()
  "Replace any :use clause with the equivalent :require clause.

Presently, there's no support for :use clauses containing :exclude."
  (interactive)
  (save-excursion
    (dolist (used-ns (cljr--extract-used-namespaces))
      (cljr--goto-ns)
      (cljr--search-forward-within-sexp used-ns)
      (if (ignore-errors (cljr--search-forward-within-sexp ":only"))
          (progn
            (paredit-forward-down)
            (let ((names (buffer-substring-no-properties (point)
                                                         (progn
                                                           (paredit-forward-up)
                                                           (1- (point))))))
              (cljr--insert-in-ns ":require")
              (insert (format "[%s :refer [%s]]" used-ns names))))
        (cljr--insert-in-ns ":require")
        (insert (format "[%s :refer :all]" used-ns))))
    (cljr--goto-ns)
    (cljr--search-forward-within-sexp ":use")
    (paredit-backward-up)
    (cljr--delete-and-extract-sexp)
    (join-line)
    (when (looking-at " ")
      (delete-char 1))
    (cljr--goto-ns)
    (paredit-forward)
    (indent-region (point-min) (point)))
  (when cljr-auto-sort-ns
    (cljr-sort-ns)))

;;;###autoload
(defun cljr-stop-referring ()
  (interactive)
  (save-excursion
    (paredit-backward-up)
    (unless (looking-at "\\[")
      (error "Place cursor on the namespace whose vars you want to stop referring to."))
    (paredit-backward-up)
    (unless (looking-at "(:require ")
      (error "Place cursor on the namespace whose vars you want to stop referring to.")))
  (save-excursion
    (paredit-backward-up)
    (let* ((bound (save-excursion
                    (paredit-forward)
                    (point)))
           (ns (save-excursion
                 (paredit-forward-down)
                 (search-forward " :as " bound t)
                 (let ((beg (point)))
                   (paredit-forward)
                   (buffer-substring-no-properties beg (point))))))
      (unless (re-search-forward " :refer " bound t)
        (error "No :refer clause found."))
      (when (looking-at ":all")
        (error "Not smart enough to stop referring to :all unfortunately."))
      (paredit-forward-down)
      (let* ((beg (point))
             (str (progn (paredit-forward-up)
                         (paredit-backward-down)
                         (buffer-substring-no-properties beg (point))))
             (symbols (s-split " " (s-trim str) t)))
        (paredit-backward-up)
        (paredit-backward)
        (kill-sexp 2)
        (just-one-space 0)
        (cljr--add-ns-prefix ns symbols)))))

(defun cljr--add-ns-prefix (ns symbols)
  "Adds an NS prefix to every symbol in SYMBOLS."
  (save-excursion
    (cljr--goto-ns)
    (paredit-forward)
    (let ((case-fold-search nil))
      (while (re-search-forward (regexp-opt symbols 'symbols) nil t)
        (paredit-backward)
        (insert ns "/")
        (paredit-forward)))))

;;;###autoload
(defun cljr-move-form ()
  "Move the form containing POINT to a new namespace.

If REGION is active, move all forms contained by region. "
  (interactive)
  (let* ((forms (if (region-active-p)
                    (let ((beg (region-beginning))
                          (end (region-end)))
                      (prog2
                          (paredit-check-region-for-delete beg end)
                          (buffer-substring-no-properties beg end)
                        (delete-region beg end)))
                  (cljr--goto-toplevel)
                  (prog1 (cljr--delete-and-extract-sexp)
                    (join-line)
                    (join-line)
                    (delete-char 1))))
         (forms (cljr--cleanup-whitespace forms)))
    (let (ns names)
      (save-window-excursion
        (ido-find-file)
        (goto-char (point-max))
        (open-line 2)
        (forward-line 2)
        (insert forms)
        (save-buffer)
        (setq ns (cljr--current-namespace)
              names (cljr--name-of-defns forms)))
      (cljr--update-ns-after-moving-fns ns (nreverse names))))
  (cljr--just-one-blank-line))

(defun cljr--update-ns-after-moving-fns (ns &optional refer-names)
  "Updates the current ns declaration after moving defn forms out of the
  current file and to NS.  Optionally referring the names in REFER-NAMES."
  (save-excursion
    (cljr--goto-ns)
    (paredit-forward)
    (let* ((end-of-ns-form (prog1 (point) (paredit-backward)))
           (ns-present-p (cljr--search-forward-within-sexp ns :save-excursion))
           (refer-present-p (cljr--search-forward-within-sexp ":refer" :save-excursion))
           (refer-all-p (cljr--search-forward-within-sexp ":refer :all" :save-excursion))
           (require-present-p (cljr--search-forward-within-sexp
                               (s-concat ":require [" ns)
                               :save-excursion)))
      (if ns-present-p
          (unless (or refer-all-p (null refer-names))
            (if refer-present-p
                (cljr--append-names-to-refer ns refer-names)
              (when require-present-p
                (cljr--append-refer-clause ns refer-names))))
        (cljr--new-require-clause ns refer-names))
      (when cljr-auto-sort-ns
        (cljr-sort-ns)))))

(defun cljr--append-refer-clause (ns refer-names)
  "Appends :refer [REFER-NAMES] to the :require clause for NS."
  (save-excursion
    (cljr--goto-ns)
    (re-search-forward ":require")
    (re-search-forward ns)
    (paredit-forward-up)
    (backward-char)
    (insert " :refer [" (s-join " " refer-names) "]")))

(defun cljr--append-names-to-refer (ns names)
  "Append NAMES to the :refer vector for NS"
  (save-excursion
    (cljr--goto-ns)
    (re-search-forward ":require")
    (re-search-forward ns)
    (re-search-forward ":refer")
    (paredit-forward)
    (backward-char)
    (apply #'insert " " (-interpose " " names))))

(defun cljr--new-require-clause (ns &optional refer-names)
  "Creates a new :require clause for NS.

Optionally adds :refer [REFER-NAMES] clause."
  (cljr--insert-in-ns ":require")
  (insert "[" ns "]")
  (when refer-names
    (cljr--append-refer-clause ns refer-names)))

(defun cljr--name-of-defns (string-with-defns &optional include-private)
  "Returns a list of the function names in STRING-WITH-DEFNS,
optionally including those that are declared private."
  (with-temp-buffer
    (insert string-with-defns)
    (goto-char (point-min))
    (let ((count (paredit-count-sexps-forward))
          (names '()))
      (dotimes (_ count)
        (paredit-forward-down)
        (cljr--goto-toplevel)
        (forward-char)
        (if (and include-private (looking-at "defn-"))
            (push (cljr--name-of-current-def) names)
          (when (looking-at "defn ")
            (push (cljr--name-of-current-def) names)))
        (paredit-forward-up))
      names)))

(defun cljr--current-namespace ()
  (save-excursion
    (cljr--goto-ns)
    (forward-char)
    (paredit-forward)
    (forward-char)
    (let ((beg (point))
          (end (progn (paredit-forward) (point))))
      (buffer-substring-no-properties beg end))))

;; ------ declare statements -----------

(defun cljr--goto-declare ()
  (goto-char (point-min))
  (if (re-search-forward "(declare" nil t)
      (paredit-forward-up)
    (cljr--goto-ns)
    (paredit-forward)
    (open-line 2)
    (forward-line 2)
    (insert "(declare)")))

(defun cljr--name-of-current-def ()
  (cljr--goto-toplevel)
  (ignore-errors (forward-char))
  (when (looking-at "def")
    (paredit-forward)
    (while (looking-at " ^")
      (paredit-forward))
    (forward-char)
    (let ((beg (point))
          (end (progn (paredit-forward) (point))))
      (buffer-substring-no-properties beg end))))

;;;###autoload
(defun cljr-add-declaration ()
  (interactive)
  (save-excursion
    (-if-let (def (cljr--name-of-current-def))
        (progn (cljr--goto-declare)
               (backward-char)
               (insert " " def))
      (message "Not inside a def form."))))

;; ------ threading and unwinding -----------

(defun cljr--unwind-first ()
  (paredit-forward)
  (save-excursion
    (let ((contents (cljr--delete-and-extract-sexp)))
      (when (looking-at " *\n")
        (join-line -1))
      (cljr--ensure-parens-around-function-names)
      (paredit-forward-down)
      (paredit-forward)
      (insert contents)))
  (forward-char))

(defun cljr--ensure-parens-around-function-names ()
  (unless (looking-at "[\n\r\t ]?(")
    (skip-syntax-forward " ")
    (paredit-wrap-round)
    (paredit-backward-up)))

(defun cljr--unwind-last ()
  (paredit-forward)
  (save-excursion
    (let ((contents (cljr--delete-and-extract-sexp)))
      (when (looking-at " *\n")
        (join-line -1))
      (cljr--ensure-parens-around-function-names)
      (paredit-forward)
      (paredit-backward-down)
      (insert contents)))
  (forward-char))

(defun cljr--nothing-more-to-unwind ()
  (save-excursion
    (let ((beg (point)))
      (paredit-forward)
      (paredit-backward-down)
      (paredit-backward) ;; the last sexp
      (paredit-backward) ;; the threading macro
      (paredit-backward) ;; and the paren
      (= beg (point)))))

(defun cljr--pop-out-of-threading ()
  (paredit-forward-down)
  (paredit-forward)
  (paredit-raise-sexp))

(defun cljr--goto-thread ()
  (while (not (or (cljr--toplevel-p)
                  (looking-at "\(.*->>?[\n\r\t ]")))
    (paredit-backward-up)))

(defun cljr--reindent-thread ()
  (cljr--goto-thread)
  (let ((beg (point))
        (end (progn (paredit-forward) (point))))
    (indent-region beg end)))

;;;###autoload
(defun cljr-cycle-thread ()
  (interactive)
  (save-excursion
    (cljr--goto-thread)
    (cond
     ((looking-at ".*->>")
      (paredit-forward-down)
      (paredit-forward)
      (backward-char)
      (delete-region (point) (+ 1 (point)))
      (cljr--reindent-thread))

     ((looking-at ".*->[^>]")
      (paredit-forward-down)
      (paredit-forward)
      (insert ">")
      (cljr--reindent-thread)))))

;;;###autoload
(defun cljr-unwind ()
  (interactive)
  (ignore-errors
    (forward-char 3))
  (search-backward-regexp "([^-]*->")
  (if (cljr--nothing-more-to-unwind)
      (progn
        (cljr--pop-out-of-threading)
        nil)
    (paredit-forward-down)
    (cond
     ((looking-at "[^-]*->[\n\r\t ]")  (cljr--unwind-first))
     ((looking-at "[^-]*->>[\n\r\t ]") (cljr--unwind-last)))
    t))

;;;###autoload
(defun cljr-unwind-all ()
  (interactive)
  (while (cljr-unwind)
    t))

(defun cljr--remove-superfluous-parens ()
  (when (looking-at "([^ )]+)")
    (paredit-forward-down)
    (paredit-raise-sexp)))

(defun cljr--thread-first ()
  (paredit-forward-down)
  (paredit-forward)
  (let* ((beg (point))
         (end (progn (paredit-forward)
                     (point)))
         (contents (buffer-substring beg end)))
    (if (string= contents ")")
        (progn
          (message "Nothing more to thread.")
          nil)
      (delete-region beg end)
      (paredit-backward-up)
      (just-one-space 0)
      (insert contents)
      (newline-and-indent)
      (cljr--remove-superfluous-parens)
      t)))

(defun cljr--thread-last ()
  (paredit-forward)
  (paredit-forward)
  (paredit-backward-down)
  (let* ((end (point))
         (beg (progn (paredit-backward)
                     (point)))
         (contents (buffer-substring beg end)))
    (if (looking-back "(" 1)
        (progn
          (message "Nothing more to thread.")
          nil)
      (delete-region beg end)
      (just-one-space 0)
      (paredit-backward-up)
      (insert contents)
      (newline-and-indent)
      (cljr--remove-superfluous-parens)
      t)))

(defun cljr--thread-guard ()
  (save-excursion
    (paredit-forward)
    (if (looking-at "[\n\r\t ]*(")
        t
      (message "Can only thread into lists.")
      nil)))

;;;###autoload
(defun cljr-thread ()
  (interactive)
  (when (looking-at "(?[^-]*-?>")
    (goto-char (match-end 0)))
  (search-backward-regexp "([^-]*->")
  (paredit-forward-down)
  (if (not (cljr--thread-guard))
      nil
    (cond
     ((looking-at "[^-]*->[\n\r\t ]")  (cljr--thread-first))
     ((looking-at "[^-]*->>[\n\r\t ]") (cljr--thread-last)))))

;;;###autoload
(defun cljr-thread-first-all ()
  (interactive)
  (save-excursion
    (paredit-wrap-round)
    (insert "-> "))
  (while (save-excursion (cljr-thread))
    t))

;;;###autoload
(defun cljr-thread-last-all ()
  (interactive)
  (save-excursion
    (paredit-wrap-round)
    (insert "->> "))
  (while (save-excursion (cljr-thread))
    t))

;; ------ let binding ----------

;;;###autoload
(defun cljr-introduce-let ()
  (interactive)
  (paredit-wrap-round)
  (insert "let ")
  (paredit-wrap-square)
  (insert " ")
  (backward-char)
  (mc/create-fake-cursor-at-point)
  (paredit-forward-up)
  (newline-and-indent)
  (mc/maybe-multiple-cursors-mode))

(add-to-list 'mc--default-cmds-to-run-once 'cljr-introduce-let)

(defun cljr--goto-let ()
  (while (not (or (cljr--toplevel-p)
                  (looking-at "\(\\(when-let\\|if-let\\|let\\)\\( \\|\\[\\)")))
    (paredit-backward-up)))

(defun cljr--extract-let-bindings ()
  "Returns a list of lists. The inner lists contain two elements first is
   the binding, second is the init-expr"
  (cljr--goto-let)
  (paredit-forward-down 2)
  (paredit-backward)
  (let* ((start (point))
         (sexp-start start)
         (end (progn (paredit-forward)
                     (point)))
         bindings)
    (paredit-backward)
    (paredit-forward-down)
    (while (/= sexp-start end)
      (paredit-move-forward)
      (let ((sexp (buffer-substring sexp-start (point))))
        (push (s-trim
               (if (= start sexp-start)
                   (substring sexp 1)
                 sexp))
              bindings))
      (setq sexp-start (point)))
    (-partition 2 (nreverse bindings))))

(defun cljr--sexp-regexp (sexp)
  (concat "\\([^[:word:]^-]\\)"
          (s-join "[[:space:]\n\r]+" (-map 'regexp-quote (s-split " " sexp t)))
          "\\([^[:word:]^-]\\)"))

(defun cljr--replace-sexp-with-binding (binding)
  (save-excursion
    (let ((bind-var (car binding))
          (init-expr (-last-item binding))
          (end (save-excursion (progn (cljr--goto-let)
                                      (paredit-forward)
                                      (point)))))
      (while (re-search-forward (cljr--sexp-regexp init-expr) end t)
        (replace-match (concat "\\1" bind-var "\\2"))))))

;;;###autoload
(defun cljr-expand-let ()
  (interactive)
  (ignore-errors
    (forward-char 4))
  (cljr--goto-let)
  (paredit-forward-down 2)
  (paredit-forward-up)
  (skip-syntax-forward " >")
  (paredit-convolute-sexp)
  (-map 'cljr--replace-sexp-with-binding (cljr--extract-let-bindings)))

(defun cljr--replace-sexp-with-binding-in-let ()
  (-map 'cljr--replace-sexp-with-binding (cljr--extract-let-bindings))
  (remove-hook 'multiple-cursors-mode-disabled-hook 'replace-sexp-with-binding-in-let))

;;;###autoload
(defun cljr-move-to-let ()
  (interactive)
  (save-excursion
    (let ((contents (cljr--delete-and-extract-sexp)))
      (cljr--prepare-to-insert-new-let-binding)
      (insert contents))
    (backward-sexp)
    (insert " ")
    (backward-char)
    (mc/create-fake-cursor-at-point))
  (add-hook 'multiple-cursors-mode-disabled-hook 'cljr--replace-sexp-with-binding-in-let)
  (mc/maybe-multiple-cursors-mode))

(defun cljr--prepare-to-insert-new-let-binding ()
  (if (cljr--inside-let-binding-form-p)
      (progn
        (paredit-backward-up (- (cljr--depth-at-point)
                                (cljr--depth-of-let-bindings)))
        (paredit-backward)
        (newline-and-indent)
        (previous-line)
        (indent-for-tab-command))
    (cljr--goto-let)
    (search-forward "[")
    (paredit-backward)
    (paredit-forward)
    (paredit-backward-down)
    (backward-char)
    (if (looking-at "\\[ *\\]")
        (forward-char)
      (forward-char)
      (newline-and-indent))))

(defun cljr--inside-let-binding-form-p ()
  (save-excursion
    (let ((pos (point)))
      (cljr--goto-let)
      (re-search-forward "\\[")
      (if (< pos (point))
          nil
        (paredit-forward-up)
        (< pos (point))))))

(defun cljr--depth-of-let-bindings ()
  "Returns the depth where the variable bindings for the active
let are."
  (save-excursion
    (cljr--goto-let)
    (re-search-forward "\\[")
    (cljr--depth-at-point)))

(add-to-list 'mc--default-cmds-to-run-once 'cljr-move-to-let)

;; ------ Destructuring ----

(defun cljr--find-symbol-at-point ()
  (save-excursion
    (when (looking-back "\\s_\\|\\sw" 1)
      (paredit-backward))
    (let ((beg (point)))
      (paredit-forward)
      (buffer-substring-no-properties beg (point)))))

;;;###autoload
(defun cljr-destructure-keys ()
  (interactive)
  (save-excursion
    (paredit-backward-up)
    (unless (looking-at "\\[")
      (error "Place point on the symbol to destructure inside the [let form]")))
  (let* ((symbol (cljr--find-symbol-at-point))
         (re (concat "(:\\(\\sw\\|\\s_\\)+ " (regexp-quote symbol) ")"))
         (bound (save-excursion
                  (paredit-backward-up 2)
                  (paredit-forward)
                  (point)))
         symbols include-as)
    (save-excursion ;; collect all symbols
      (paredit-forward)
      (while (re-search-forward re bound t)
        (paredit-backward)
        (paredit-forward-down)
        (paredit-raise-sexp)
        (delete-char 1)
        (!cons (cljr--find-symbol-at-point) symbols)))
    (save-excursion ;; find new bound
      (paredit-backward-up 2)
      (paredit-forward)
      (setq bound (point)))
    (save-excursion ;; are there any more usages of symbol?
      (paredit-forward-up)
      (when (re-search-forward (regexp-opt (list symbol) 'symbols) bound t)
        (setq include-as t)))
    (when (looking-back "\\s_\\|\\sw" 1)
      (paredit-backward))
    (kill-sexp)
    (insert "{:keys [" (s-join " " (-distinct (reverse symbols))) "]"
            (if include-as (concat " :as " symbol) "") "}")))

;; ------ Cycling ----------

;;;###autoload
(defun cljr-cycle-privacy ()
  (interactive)
  (save-excursion
    (search-backward-regexp "\\((defn-? \\)\\|\\((def \\)")
    (cond
     ((and cljr-use-metadata-for-privacy
           (looking-at "(defn ^:private"))
      (forward-char 5)
      (delete-char 10))
     ((and (not cljr-use-metadata-for-privacy)
           (looking-at "(defn-"))
      (forward-char 5)
      (delete-char 1))
     ((and cljr-use-metadata-for-privacy
           (looking-at "(defn"))
      (forward-char 5)
      (insert " ^:private"))
     ((and (not cljr-use-metadata-for-privacy)
           (looking-at "(defn"))
      (forward-char 5)
      (insert "-"))
     ((looking-at "(def ^:private")
      (forward-char 5)
      (delete-char 10))
     ((looking-at "(def ")
      (forward-char 5)
      (insert "^:private ")))))

;;;###autoload
(defun cljr-cycle-stringlike ()
  "Removed, use `clojure-toggle-keyword-string'"
  (interactive)
  (message "Removed, use `clojure-toggle-keyword-string'"))

;;;###autoload
(defun cljr-cycle-coll ()
  "Convert the coll at (point) from (x) -> {x} -> [x] -> -> #{x} -> (x) recur"
  (interactive)
  (save-excursion
    (while (and
            (> (point) 1)
            (not (eq (string-to-char "(") (char-after)))
            (not (string= "#{" (buffer-substring (point) (+ 2 (point)))))
            (not (eq (string-to-char "{") (char-after)))
            (not (eq (string-to-char "[") (char-after))))
      (backward-char))

    (cond
     ((eq (string-to-char "(") (char-after))
      (insert "{" (substring (cljr--delete-and-extract-sexp) 1 -1) "}"))

     ((eq (string-to-char "#") (char-after))
      (delete-char 1)
      (insert "(" (substring (cljr--delete-and-extract-sexp) 1 -1) ")"))

     ((eq (string-to-char "{") (char-after))
      (if (not (equal (string-to-char "#") (char-before)))
          (insert "[" (substring (cljr--delete-and-extract-sexp) 1 -1) "]")
        (backward-char)
        (delete-char 1)
        (insert "(" (substring (cljr--delete-and-extract-sexp) 1 -1) ")")))

     ((eq (string-to-char "[") (char-after))
      (insert "#{" (substring (cljr--delete-and-extract-sexp) 1 -1) "}"))

     ((equal 1 (point))
      (message "beginning of file reached, this was probably a mistake.")))))

(defun cljr--goto-if ()
  (while (not (or (cljr--toplevel-p)
                  (looking-at "\\((if \\)\\|\\((if-not \\)")))
    (paredit-backward-up)))

;;;###autoload
(defun cljr-cycle-if ()
  "Cycle surrounding if or if-not, to if-not or if"
  (interactive)
  (save-excursion
    (cljr--goto-if)
    (cond
     ((looking-at "(if-not")
      (forward-char 3)
      (delete-char 4)
      (paredit-forward)
      (paredit-forward)
      (transpose-sexps 1))
     ((looking-at "(if")
      (forward-char 3)
      (insert "-not")
      (paredit-forward)
      (paredit-forward)
      (transpose-sexps 1)))))

;;;###autoload
(defun cljr-raise-sexp (&optional argument)
  "Like paredit-raise-sexp, but removes # in front of function literals and sets."
  (interactive "P")
  (paredit-raise-sexp argument)
  (when (looking-back " #" 2)
    (delete-char -1)))

;;;###autoload
(defun cljr-splice-sexp-killing-backward (&optional argument)
  "Like paredit-splice-sexp-killing-backward, but removes # in
front of function literals and sets."
  (interactive "P")
  (paredit-splice-sexp-killing-backward argument)
  (when (looking-back " #" 2)
    (delete-char -1)))

;;;###autoload
(defun cljr-splice-sexp-killing-forward (&optional argument)
  "Like paredit-splice-sexp-killing-backward, but removes # in
front of function literals and sets."
  (interactive "P")
  (save-excursion
    (paredit-backward-up)
    (when (looking-back " #" 2)
      (delete-char -1)))
  (paredit-splice-sexp-killing-forward argument))

;; ------ magic requires -------

(defvar cljr--magic-requires-re
  (concat "(\\(" (regexp-opt (-map 'car cljr-magic-require-namespaces)) "\\)/"))

;;;###autoload
(defun cljr-slash ()
  "Inserts / as normal, but also checks for common namespace shorthands to require."
  (interactive)
  (insert "/")
  (when (and cljr-magic-requires
             (looking-back cljr--magic-requires-re 6))
    (let* ((short (match-string-no-properties 1))
           (long (aget cljr-magic-require-namespaces short)))
      (if (and (not (cljr--in-namespace-declaration? (concat ":as " short)))
               (or (not (eq :prompt cljr-magic-requires))
                   (yes-or-no-p (format "Add %s :as %s to requires?" long short))))
          (save-excursion
            (cljr--insert-in-ns ":require")
            (insert (format "[%s :as %s]" long short))
            (when cljr-auto-sort-ns
              (cljr-sort-ns)))))))

(defun aget (map key)
  (cdr (assoc key map)))

(defun cljr--in-namespace-declaration? (s)
  (save-excursion
    (cljr--goto-ns)
    (cljr--search-forward-within-sexp s)))

;; ------ project clean --------

;;;###autoload
(defun cljr-project-clean ()
  "Runs `cljr-project-clean-functions' on every clojure file, then
sorts the project's dependency vectors."
  (interactive)
  (when (or (not cljr-project-clean-prompt)
            (yes-or-no-p "Cleaning your project might change many of your clj files. Do you want to proceed?"))
    (dolist (filename (cljr--project-files))
      (when (s-ends-with? "clj" filename)
        (let ((buffer (get-file-buffer filename))
              find-file-p)
          (if buffer
              (set-buffer buffer)
            (setq find-file-p t)
            (find-file filename))
          (ignore-errors (-map 'funcall cljr-project-clean-functions))
          (save-buffer)
          (when find-file-p
            (kill-buffer)))))
    (cljr-sort-project-dependencies)
    (message "Project clean done.")))

;;;###autoload
(defun cljr-sort-project-dependencies ()
  (interactive)
  "Sorts all dependency vectors in project.clj"
  (save-window-excursion
    (find-file (cljr--project-file))
    (goto-char (point-min))
    (while (re-search-forward ":dependencies" (point-max) t)
      (forward-char)
      (when (looking-at "\\[")
        (->> (cljr--delete-and-extract-sexp)
          (s-chop-prefix "[")
          (s-chop-suffix "]")
          s-lines
          (-map #'s-trim)
          (-sort #'string<)
          (s-join "\n")
          (insert "["))
        (insert "]")))
    (indent-region (point-min) (point-max))
    (save-buffer)))

(defun cljr--get-artifacts-from-middleware (force)
  (message "Retrieving list of available libraries...")
  (let ((nrepl-sync-request-timeout nil))
    (s-split " " (plist-get (nrepl-send-request-sync
                             (list "op" "artifact-list"
                                   "force" (if force "true" "false")))
                            :value))))

(defun cljr-update-artifact-cache ()
  (interactive)
  (nrepl-send-request (list "op" "artifact-list"
                            "force" "true")
                      (lambda (_) (message "Artifact cache updated"))))

(defun cljr--get-versions-from-middleware (artifact)
  (s-split " " (plist-get (nrepl-send-request-sync
                           (list "op" "artifact-versions"
                                 "artifact" artifact))
                          :value)))

(defun cljr--prompt-user-for (prompt choices)
  (completing-read prompt choices))

(defun cljr--add-project-dependency (artifact version)
  (save-window-excursion
    (find-file (cljr--project-file))
    (goto-char (point-min))
    (re-search-forward ":dependencies")
    (paredit-forward)
    (paredit-backward-down)
    (newline-and-indent)
    (insert "[" artifact " \"" version "\"]")
    (save-buffer))
  (message "Added %s version %s as a project dependency" artifact version))

(defun cljr--assert-middleware ()
  (unless (featurep 'cider)
    (error "CIDER isn't installed!"))
  (unless (cider-connected-p)
    (error "CIDER isn't connected!"))
  (unless (nrepl-op-supported-p "refactor")
    (error "nrepl-refactor middleware not available!")))

(defun cljr--assert-leiningen-project ()
  (unless (string= (file-name-nondirectory (or (cljr--project-file) ""))
                   "project.clj")
    (error "Can't find project.clj!")))

(defun cljr-add-project-dependency (force)
  (interactive "P")
  (cljr--assert-leiningen-project)
  (cljr--assert-middleware)
  (-when-let* ((lib-name (->> (cljr--get-artifacts-from-middleware force)
                           (cljr--prompt-user-for "Artifact: ")))
               (version (->> (cljr--get-versions-from-middleware lib-name)
                          (cljr--prompt-user-for "Version: "))))
    (cljr--add-project-dependency lib-name version)))

;; ------ minor mode -----------
;;;###autoload
(define-minor-mode clj-refactor-mode
  "A mode to keep the clj-refactor keybindings."
  nil " cljr" clj-refactor-map)

(provide 'clj-refactor)
;;; clj-refactor.el ends here
