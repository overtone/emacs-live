;;; clj-refactor.el --- A collection of commands for refactoring Clojure code -*- lexical-binding: t -*-

;; Copyright © 2012-2014 Magnar Sveen
;; Copyright © 2014-2021 Magnar Sveen, Lars Andersen, Benedek Fazekas, Bozhidar Batsov

;; Author: Magnar Sveen <magnars@gmail.com>
;;         Lars Andersen <expez@expez.com>
;;         Benedek Fazekas <benedek.fazekas@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.dev>
;; Version: 3.2.2
;; Keywords: convenience, clojure, cider

;; Package-Requires: ((emacs "26.1") (seq "2.19") (yasnippet "0.6.1") (paredit "24") (multiple-cursors "1.2.2") (clojure-mode "5.9") (cider "1.0") (parseedn "1.0.6") (inflections "2.3") (hydra "0.13.2"))

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

;; See README.md at https://github.com/clojure-emacs/clj-refactor.el

;;; Code:

(require 'seq)

(require 'clj-refactor-compat)
(require 'yasnippet)
(require 'paredit)
(require 'multiple-cursors-core)
(require 'clojure-mode)
(require 'cider)
(require 'parseedn)
(require 'sgml-mode)
(require 'inflections)
(require 'hydra)
(require 'subword)

(defgroup cljr nil
  "Clojure refactoring facilities."
  :prefix "cljr-"
  :group 'clojure
  :link '(url-link :tag "GitHub"
                   "https://github.com/clojure-emacs/clj-refactor.el"))

(defcustom cljr-add-ns-to-blank-clj-files t
  "If t, automatically add a ns form to new .clj files."
  :group 'cljr
  :type 'boolean
  :safe #'booleanp)

(defcustom cljr-auto-sort-ns t
  "If t, sort ns form after any command that changes it."
  :group 'cljr
  :type 'boolean
  :safe #'booleanp)

(defcustom cljr-magic-requires t
  "Whether to automatically require common namespaces when they are used.
These are the namespaces listed in `cljr-magic-require-namespaces'
and returned by the `namespace-aliases' middleware op.

If this variable is `:prompt', typing the short form followed by
`\\[cljr-slash]' will ask if you want to add the corresponding require
statement to the ns form.
Any other non-nil value means to add the form without asking."
  :group 'cljr
  :type '(choice (const :tag "true" t)
                 (const :tag "prompt" :prompt)
                 (const :tag "false" nil)))

(defcustom cljr-magic-require-namespaces
  '(("io"   . "clojure.java.io")
    ("set"  . "clojure.set")
    ("str"  . "clojure.string")
    ("walk" . "clojure.walk")
    ("zip"  . "clojure.zip"))
  "Alist of aliases and namespaces used by `cljr-slash'."
  :type '(repeat (cons (string :tag "Short alias")
                       (string :tag "Full namespace")))
  :safe #'listp
  :group 'cljr)

(defcustom cljr-project-clean-prompt t
  "If t, `cljr-project-clean' asks before doing anything.
If nil, the project clean functions are run without warning."
  :group 'cljr
  :type 'boolean
  :safe #'booleanp)

(defcustom cljr-project-clean-functions
  (list #'cljr-clean-ns)
  "List of functions called by `cljr-project-clean'.
These are called on all .clj files in the project."
  :group 'cljr
  :type '(repeat function)
  :safe #'listp)

(defcustom cljr-project-clean-exceptions '("dev/user.clj" "project.clj" "boot.clj")
  "A list of files that `cljr-project-clean' should avoid."
  :group 'cljr
  :type '(repeat string)
  :safe #'listp)

(defcustom cljr-hotload-dependencies nil
  "If t, newly added dependencies are also hotloaded into the repl.
This only applies to dependencies added by `cljr-add-project-dependency'."
  :group 'cljr
  :type 'boolean
  :safe #'booleanp)

(defcustom cljr-favor-private-functions t
  "If t, refactorings insert private function declarations."
  :group 'cljr
  :type 'boolean
  :safe #'booleanp)

(defcustom cljr-favor-prefix-notation nil
  "If t, `cljr-clean-ns' favors prefix notation in the ns form."
  :group 'cljr
  :type 'boolean
  :safe #'booleanp)

(defcustom cljr-insert-newline-after-require t
  "If t, `cljr-clean-ns' will place a newline after the `:require` and `:import` tokens."
  :group 'cljr
  :type 'boolean
  :safe #'booleanp)

(defcustom cljr-use-multiple-cursors t
  "If t, some refactorings use the `multiple-cursors' package.
This improves interactivity of the commands.  If nil, those
refactorings will use regular prompts instead."
  :group 'cljr
  :type 'boolean
  :safe #'booleanp)

(defcustom cljr-auto-clean-ns t
  "If t, call `cljr-clean-ns' after commands that change the ns."
  :group 'cljr
  :type 'boolean
  :safe #'booleanp)

(defcustom cljr-populate-artifact-cache-on-startup t
  "If t, the middleware will eagerly populate the artifact cache.
This makes `cljr-add-project-dependency' as snappy as can be."
  :group 'cljr
  :type 'boolean
  :safe #'booleanp)

(defcustom cljr-warn-on-eval t
  "If t, warn the user before running any op that requires ASTs to be built
   that the project will be evaled.  If this is not preferred the op will
   be aborted.  Also effectively overrides `cljr-eagerly-build-asts-on-startup'
   so if this is on the AST cache is not warmed at startup or after certain
   operations."
  :group 'cljr
  :type 'boolean
  :safe #'booleanp)

(defcustom cljr-eagerly-build-asts-on-startup t
  "If t, the middleware will eagerly populate the ast cache.
This makes `cljr-find-usages' and `cljr-rename-symbol' as snappy
as can be."
  :group 'cljr
  :type 'boolean
  :safe #'booleanp)

(defcustom cljr-suppress-middleware-warnings nil
  "If t, no middleware warnings are printed to the repl."
  :group 'cljr
  :type 'boolean
  :safe #'booleanp)

(defcustom cljr-suppress-no-project-warning nil
  "If t, no warning is printed when starting a REPL outside a project.
By default, a warning is printed in this case since clj-refactor
will not work as expected in such REPLs."
  :group 'cljr
  :type 'boolean
  :safe #'booleanp)

(defcustom cljr-ignore-analyzer-errors nil
  "If t, `cljr-find-usages' `cljr-inline-symbol' `cljr-rename-symbol'
ignores namespaces that cannot be analyzed.
If any namespaces presents an analyzer error, it is skipped and
the command carries on looking for the given symbol in those
namespaces which can be analyzed.

If nil, `cljr-find-usages'  `cljr-inline-symbol' `cljr-rename-symbol'
won't run if there is a broken namespace in the project."
  :group 'cljr
  :type 'boolean
  :safe #'booleanp)

(define-obsolete-variable-alias 'cljr-find-usages-ignore-analyzer-errors 'cljr-ignore-analyzer-errors "2.3.0")

(defcustom cljr-auto-eval-ns-form t
  "When true refactorings which change the ns form also trigger
  its re-evaluation."
  :group 'cljr
  :type 'boolean
  :safe #'booleanp)

(defcustom cljr-midje-test-declaration "[midje.sweet :as midje]"
  "The require form to use when midje is in use."
  :group 'cljr
  :type 'string
  :safe #'stringp)

(defcustom cljr-expectations-test-declaration "[expectations :as e]"
  "The require form to use when expectations is in use."
  :group 'cljr
  :type 'string
  :safe #'stringp)

(defcustom cljr-cljc-clojure-test-declaration "#?(:clj [clojure.test :as t]
:cljs [cljs.test :as t :include-macros true])"
  "The require form to use when clojure.test and cljs.test is in use in a cljc file."
  :group 'cljr
  :type 'string
  :safe #'stringp)

(defcustom cljr-cljs-clojure-test-declaration "[cljs.test :as t :include-macros true]"
  "The require form to use when cljs.test is in use in a cljs file."
  :group 'cljr
  :type 'string
  :safe #'stringp)

(defcustom cljr-clojure-test-declaration "[clojure.test :as t]"
  "The require form to use when clojure.test is in use in a clj file."
  :group 'cljr
  :type 'string
  :safe #'stringp)

(defcustom cljr-clojure-test-namespace-under-test-alias  "sut"
  "The package alias to use for the namespace under test."
  :group 'cljr
  :type 'string
  :safe #'stringp)

(defcustom cljr-inject-dependencies-at-jack-in t
  "When nil, do not inject repl dependencies (most likely nREPL middlewares) at `cider-jack-in' time."
  :group 'cljr
  :type 'boolean
  :safe #'booleanp)

(defcustom cljr-assume-language-context nil
  "If set to 'clj' or 'cljs', clj-refactor will use that value in situations
  where the language context is ambiguous. If set to nil, a popup will be
  created in each ambiguous case asking user to choose language context."
  :group 'cljr
  :type 'string
  :safe #'stringp)

(defcustom cljr-libspec-whitelist
  '("^cljsns" "^slingshot.test" "^monger.joda-time" "^monger.json")
  "List of regexes to match against libspec names which shouldn't be pruned.

This is useful when `clean-ns' should leave a libspec alone even
if it appears to be unused."
  :group 'cljr
  :type '(repeat string)
  :safe #'listp)

(defvar cljr-minimum-clojure-version "1.8.0"
  "The oldest Clojure version supported by our middleware.")

(defvar clj-refactor-map (make-sparse-keymap) "")

(defvar cljr--add-require-snippet
  "${1:[${2:${3:} :as ${4:${3:$(cljr--ns-name yas-text)}}}]}"
  "The snippet used in in `cljr-add-require-to-ns'")

(defun cljr--ns-name (ns)
  "Return the last name in a full NS."
  (replace-regexp-in-string ".*\\." "" ns))

(defvar cljr--add-use-snippet "[$1 :refer ${2:[$3]}]"
  "The snippet used in in `cljr-add-use-to-ns'.")

(defvar *cljr--noninteractive* nil
  "t, when our interactive functions are called programmatically.")

(defvar cljr--file-column-pattern
  "^\\(.+?\\):\\([1-9][0-9]*\\):\\([0-9][0-9]*\\): "
  "A regexp pattern that groups output into filename, line number and column number.")

(defvar cljr--debug-mode nil)

(defvar cljr--occurrences nil)
(defvar cljr--signature-changes nil)
(defvar cljr--change-signature-buffer "*cljr-change-signature*")
(defvar cljr--manual-intervention-buffer "*cljr-manual-intervention*")
(defvar cljr--find-symbol-buffer "*cljr-find-usages*")
(defvar cljr--post-command-messages nil "Message(s) to display after the current command is done.")

(defcustom cljr-before-warming-ast-cache-hook nil
  "Runs before each time the AST is loaded."
  :group 'cljr
  :type 'hook)

(defcustom cljr-after-warming-ast-cache-hook nil
  "Runs after each time the AST is loaded."
  :group 'cljr
  :type 'hook)

(defcustom cljr-middleware-ignored-paths nil
  "List of (Java style) regexes to paths that should be ignored
  by the middleware."
  :group 'cljr
  :type '(repeat string)
  :safe #'listp)

;;; Buffer Local Declarations

;; tracking state of find-symbol buffer

(defvar-local cjr--occurrence-count 0 "Counts occurrences of found symbols")

(defvar-local cljr--num-syms -1 "Keeps track of overall number of symbol occurrences")

(defvar-local cljr--occurrence-ids '() "Keeps track of already processed symbol occurrences")

(defmacro cljr--update-file (filename &rest body)
  "If there is an open buffer for FILENAME, then change that.
Otherwise open the file and do the changes non-interactively."
  (declare (debug (form body))
           (indent 1))
  (let ((fn (make-symbol "filename"))
        (bf (make-symbol "buffer"))
        (wo (make-symbol "was-open")))
    `(let* ((,fn ,filename)
            (,wo (get-file-buffer ,fn))
            (,bf (find-file-noselect ,fn)))
       (when ,bf
         (set-buffer ,bf)
         ,@body
         (save-buffer)
         (when (not ,wo)
           ;; Don't accumulate open buffers, since this can slow down Emacs for large projects:
           (kill-buffer))))))

(define-key clj-refactor-map [remap paredit-raise-sexp] 'cljr-raise-sexp)
(define-key clj-refactor-map [remap paredit-splice-sexp-killing-backward] 'cljr-splice-sexp-killing-backward)
(define-key clj-refactor-map [remap paredit-splice-sexp-killing-forward] 'cljr-splice-sexp-killing-forward)
(define-key clj-refactor-map (kbd "/") 'cljr-slash)

(defun cljr--use-multiple-cursors-p ()
  (and cljr-use-multiple-cursors
       (not (bound-and-true-p evil-mode))))

(defun cljr--vector-at-point-p ()
  (eq (char-after) ?\[))

(defun cljr--fix-special-modifier-combinations (key)
  (cl-case key
    ("C-s-i" "s-TAB")
    ("C-s-m" "s-RET")
    (otherwise key)))

(defun cljr--key-pairs-with-modifier (modifier keys)
  (read-kbd-macro
   (string-join
    (seq-map
     (lambda (it)
       (cljr--fix-special-modifier-combinations (concat modifier (char-to-string it))))
     (string-to-list keys)) " ")))

(defun cljr--key-pairs-with-prefix (prefix keys)
  (read-kbd-macro (concat prefix " " keys)))

(defvar cljr--all-helpers
  '(("ai" . (cljr-add-import-to-ns "Add import to ns" ?i ("ns")))
    ("am" . (cljr-add-missing-libspec "Add missing libspec" ?m ("ns")))
    ("ap" . (cljr-add-project-dependency "Add project dependency" ?p ("ns" "project")))
    ("ar" . (cljr-add-require-to-ns "Add require to ns" ?r ("ns")))
    ("as" . (cljr-add-stubs "Add stubs for the interface/protocol at point" ?s ("toplevel-form")))
    ("au" . (cljr-add-use-to-ns "Add use to ns" ?U ("ns")))
    ("ci" . (clojure-cycle-if "Cycle if" ?I ("code")))
    ("cn" . (cljr-clean-ns "Clean ns" ?c ("ns")))
    ("cp" . (clojure-cycle-privacy "Cycle privacy" ?P ("toplevel-form")))
    ("cs" . (cljr-change-function-signature "Change function signature" ?C ("toplevel-form" "project")))
    ("ct" . (cljr-cycle-thread "Cycle thread" ?t ("code")))
    ("dk" . (cljr-destructure-keys "Destructure keys" ?d ("code")))
    ("ec" . (cljr-extract-constant "Extract constant" ?c ("toplevel-form")))
    ("ed" . (cljr-extract-def "Extract form as def" ?D ("toplevel-form")))
    ("ef" . (cljr-extract-function "Extract function" ?e ("toplevel-form")))
    ("el" . (cljr-expand-let "Expand let" ?e ("code")))
    ("fe" . (cljr-create-fn-from-example "Create function from example" ?f ("toplevel-form")))
    ("fu" . (cljr-find-usages "Find usages" ?u ("project" "code")))
    ("hd" . (cljr-hotload-dependency "Hotload dependency" ?h ("project")))
    ("il" . (cljr-introduce-let "Introduce let" ?l ("code")))
    ("is" . (cljr-inline-symbol "Inline symbol" ?i ("project" "toplevel-form" "code")))
    ("mf" . (cljr-move-form "Move form" ?m ("toplevel-form" "project")))
    ("ml" . (cljr-move-to-let "Move to let" ?m ("code")))
    ("pc" . (cljr-project-clean "Project clean" ?c ("project")))
    ("pf" . (cljr-promote-function "Promote function" ?p ("code" "toplevel-form")))
    ("rf" . (cljr-rename-file-or-dir "Rename file-or-dir" ?r ("project" "toplevel-form")))
    ("rl" . (cljr-remove-let "Remove let" ?r ("code")))
    ("rm" . (cljr-require-macro "Add to or extend the require-macros form" ?M ("ns")))
    ("rs" . (cljr-rename-symbol "Rename symbol" ?s ("project" "code")))
    ("sc" . (cljr-show-changelog "Show the project's changelog" ?c ("cljr")))
    ("sp" . (cljr-sort-project-dependencies "Sort project dependencies" ?S ("project")))
    ("sr" . (cljr-stop-referring "Stop referring" ?t ("ns")))
    ("tf" . (clojure-thread-first-all "Thread first all" ?f ("code")))
    ("th" . (clojure-thread "Thread" ?T ("code")))
    ("tl" . (clojure-thread-last-all "Thread last all" ?L ("code")))
    ("ua" . (clojure-unwind-all "Unwind all" ?U ("code")))
    ("up" . (cljr-update-project-dependencies "Update project dependencies" ?U ("project")))
    ("uw" . (clojure-unwind "Unwind" ?w ("code")))
    ("ad" . (cljr-add-declaration "Add declaration" ?d ("toplevel-form")))
    ("?" . (cljr-describe-refactoring "Describe refactoring" ?d ("cljr")))
    ("hh" . (hydra-cljr-help-menu/body "Parent menu for hydra menus" ?h ("hydra")))
    ("hn" . (hydra-cljr-ns-menu/body "Hydra menu for ns refactorings" ?n ("hydra")))
    ("hc" . (hydra-cljr-code-menu/body "Hydra menu for code refactorings" ?c ("hydra")))
    ("hp" . (hydra-cljr-project-menu/body "Hydra menu for project refactorings" ?p ("hydra")))
    ("ht" . (hydra-cljr-toplevel-form-menu/body "Hydra menu for top level refactorings " ?t ("hydra")))
    ("hs" . (hydra-cljr-cljr-menu/body "Hydra menu for self features" ?s ("hydra")))))


(defhydra hydra-cljr-ns-menu (:color pink :hint nil)
  "
 Ns related refactorings
------------------------------------------------------------------------------------------------------------------------------------------------------
_ai_: Add import to ns                             _am_: Add missing libspec                          _ap_: Add project dependency
_ar_: Add require to ns                            _au_: Add use to ns                                _cn_: Clean ns
_rm_: Require a macro into the ns                  _sr_: Stop referring
_b_: Back to previous Hydra
"
  ("ai" cljr-add-import-to-ns) ("am" cljr-add-missing-libspec)
  ("ap" cljr-add-project-dependency) ("ar" cljr-add-require-to-ns)
  ("au" cljr-add-use-to-ns) ("cn" cljr-clean-ns)
  ("rm" cljr-require-macro) ("sr" cljr-stop-referring)
  ("b" hydra-cljr-help-menu/body :exit t)
  ("q" nil "quit"))

(defhydra hydra-cljr-code-menu (:color pink :hint nil)
  "
 Code related refactorings
------------------------------------------------------------------------------------------------------------------------------------------------------
_ci_: Cycle if                                     _ct_: Cycle thread
_dk_: Destructure keys                             _el_: Expand let                                   _fu_: Find usages
_il_: Introduce let                                _is_: Inline symbol                                _ml_: Move to let
_pf_: Promote function                             _rl_: Remove let                                   _rs_: Rename symbol
_tf_: Thread first all                             _th_: Thread                                       _tl_: Thread last all
_ua_: Unwind all                                   _uw_: Unwind
_b_: Back to previous Hydra
"
  ("ci" clojure-cycle-if) ("ct" cljr-cycle-thread)
  ("dk" cljr-destructure-keys) ("el" cljr-expand-let)
  ("fu" cljr-find-usages) ("il" cljr-introduce-let)
  ("is" cljr-inline-symbol) ("ml" cljr-move-to-let)
  ("pf" cljr-promote-function) ("rl" cljr-remove-let)
  ("rs" cljr-rename-symbol) ("tf" clojure-thread-first-all)
  ("th" clojure-thread) ("tl" clojure-thread-last-all)
  ("ua" clojure-unwind-all) ("uw" clojure-unwind)
  ("b" hydra-cljr-help-menu/body :exit t)
  ("q" nil "quit"))

(defhydra hydra-cljr-project-menu (:color pink :hint nil)
  "
 Project related refactorings
------------------------------------------------------------------------------------------------------------------------------------------------------
_ap_: Add project dependency                       _cs_: Change function signature                    _fu_: Find usages
_hd_: Hotload dependency                           _is_: Inline symbol                                _mf_: Move form
_pc_: Project clean                                _rf_: Rename file-or-dir _rs_: Rename symbol       _sp_: Sort project dependencies
_up_: Update project dependencies
_b_: Back to previous Hydra
"
  ("ap" cljr-add-project-dependency) ("cs" cljr-change-function-signature)
  ("fu" cljr-find-usages) ("hd" cljr-hotload-dependency)
  ("is" cljr-inline-symbol) ("mf" cljr-move-form)
  ("pc" cljr-project-clean) ("rf" cljr-rename-file-or-dir)
  ("rs" cljr-rename-symbol) ("sp" cljr-sort-project-dependencies)
  ("up" cljr-update-project-dependencies)
  ("b" hydra-cljr-help-menu/body :exit t)
  ("q" nil "quit"))

(defhydra hydra-cljr-toplevel-form-menu (:color pink :hint nil)
  "
 Toplevel form related refactorings
------------------------------------------------------------------------------------------------------------------------------------------------------
_as_: Add stubs for the interface/protocol at point_cp_: Cycle privacy                                _cs_: Change function signature
_ec_: Extract constant                             _ed_: Extract form as def                          _ef_: Extract function
_fe_: Create function from example                 _is_: Inline symbol                                _mf_: Move form
_pf_: Promote function                             _rf_: Rename file-or-dir                           _ad_: Add declaration
_b_: Back to previous Hydra
"
  ("as" cljr-add-stubs) ("cp" clojure-cycle-privacy)
  ("cs" cljr-change-function-signature) ("ec" cljr-extract-constant)
  ("ed" cljr-extract-def) ("ef" cljr-extract-function)
  ("fe" cljr-create-fn-from-example) ("is" cljr-inline-symbol)
  ("mf" cljr-move-form) ("pf" cljr-promote-function)
  ("rf" cljr-rename-file-or-dir) ("ad" cljr-add-declaration)
  ("b" hydra-cljr-help-menu/body :exit t)
  ("q" nil "quit"))

(defhydra hydra-cljr-cljr-menu (:color pink :hint nil)
  "
 Cljr related refactorings
------------------------------------------------------------------------------------------------------------------------------------------------------
_sc_: Show the project's changelog                 _?_: Describe refactoring
_b_: Back to previous Hydra
"
  ("sc" cljr-show-changelog) ("?" cljr-describe-refactoring)
  ("b" hydra-cljr-help-menu/body :exit t)
  ("q" nil "quit"))

(defhydra hydra-cljr-help-menu (:color pink :hint nil)
  "
Available refactoring types
-----------------------------------------------------------------------------
_n_: Ns related refactorings      _c_: Code related refactorings
_p_: Project related refactorings _t_: Top level forms related refactorings
_s_: Refactor related functions
"

  ("n" hydra-cljr-ns-menu/body :exit t)
  ("c" hydra-cljr-code-menu/body :exit t)
  ("p" hydra-cljr-project-menu/body :exit t)
  ("t" hydra-cljr-toplevel-form-menu/body :exit t)
  ("s" hydra-cljr-cljr-menu/body :exit t)
  ("q" nil "quit" :color blue))

(defun cljr--add-keybindings (key-fn)
  "Build the keymap from the list of keys/functions in `cljr--all-helpers'."
  (dolist (details cljr--all-helpers)
    (let ((key (car details))
          (fn (cadr details)))
      (define-key clj-refactor-map (funcall key-fn key) fn))))

;;;###autoload
(defun cljr-add-keybindings-with-prefix (prefix)
  "Bind keys in `cljr--all-helpers' under a PREFIX key."
  (cljr--add-keybindings (apply-partially 'cljr--key-pairs-with-prefix prefix)))

;;;###autoload
(defun cljr-add-keybindings-with-modifier (modifier)
  "Bind keys in `cljr--all-helpers' under a MODIFIER key."
  (cljr--add-keybindings (apply-partially 'cljr--key-pairs-with-modifier modifier)))


;; ------ utilities -----------

(defun cljr--extract-sexp ()
  (buffer-substring (point) (cljr--point-after 'paredit-forward)))

(defun cljr--delete-sexp ()
  (delete-region (point) (cljr--point-after 'paredit-forward)))

(defun cljr--extract-sexp-as-list (&optional with-whitespace)
  "Returns list of strings representing the elements of the SEXP at point.

If optional `with-whitespace' is T sexp elements are not trimmed."
  (save-excursion
    (let* ((beg (progn (paredit-backward-up)
                       (forward-char)
                       (point)))
           (end (1- (cljr--point-after 'paredit-forward-up)))
           sexp-elems)
      (while (/= (point) end)
        (paredit-forward)
        (let ((sexp-elem (buffer-substring-no-properties beg (point))))
          (push (if with-whitespace sexp-elem (string-trim sexp-elem)) sexp-elems))
        (setq beg (point)))
      (nreverse sexp-elems))))

(defun cljr--extract-region (beg end)
  (prog1
      (buffer-substring-no-properties beg end)
    (delete-region beg end)))

(defun cljr--comment-line-p ()
  (save-excursion
    (goto-char (point-at-bol))
    (looking-at "\\s-*;+")))

(defun cljr--search-forward-within-sexp (s &optional save-excursion)
  "Searches forward for S in the current sexp.

if SAVE-EXCURSION is T POINT does not move."
  (let ((bound (save-excursion (forward-list 1) (point))))
    (if save-excursion
        (save-excursion
          (search-forward s bound t))
      (search-forward s bound t))))

(defun cljr--goto-toplevel ()
  (paredit-backward-up (cljr--depth-at-point))
  (when (looking-back "#" 1)
    (backward-char)))

(defun cljr--top-level-p ()
  "T unless we're in an s-expression or string."
  (= (cljr--depth-at-point) 0))

(defun cljr--depth-at-point ()
  "Returns the depth in s-expressions, or strings, at point."
  (let ((depth (car (paredit-current-parse-state))))
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

(defun cljr--looking-at-dependency-p ()
  (or
   ;; boot & leiningen dependency vector
   (looking-at "\\[[^[[:space:]]+[[:space:]]+\"")
   ;; clj dependency style
   (looking-at "\\([a-z0-9\-\./]+\\)[[:space:]]*\{.*\\(:mvn\\|:local\\|:git\\)/\\(root\\|version\\|url\\)[[:space:]]+\\(\"[^\"]+\"\\)")))

(defun cljr--just-one-blank-line ()
  "Ensure there's only one blank line at POINT."
  (newline 2)
  (forward-line -1)
  (delete-blank-lines))

(defun cljr--point-after (&rest actions)
  "Returns POINT after performing ACTIONS.

An action is either the symbol of a function or a two element
list of (fn args) to pass to `apply''"
  (save-excursion
    (dolist (fn-and-args actions)
      (let ((f (if (listp fn-and-args) (car fn-and-args) fn-and-args))
            (args (if (listp fn-and-args) (cdr fn-and-args) nil)))
        (apply f args)))
    (point)))

(defun cljr--whitespacep (s)
  "True if S contains only whitespace."
  (or (null s) (string-blank-p (string-trim s))))

(defun cljr--make-room-for-toplevel-form ()
  (if (cljr--whitespacep (buffer-substring-no-properties (point) (point-max)))
      ;; make room at end of buffer
      (progn (open-line 2)
             (delete-blank-lines)
             (open-line 1)
             (forward-line))
    (cljr--goto-toplevel)
    (goto-char (point-at-bol))
    (open-line 2)))

(defun cljr--new-toplevel-form (form)
  "Insert a new toplevel FORM before the form containing POINT."
  (cljr--make-room-for-toplevel-form)
  (insert form))

(defun cljr--goto-toplevel-forward ()
  "Move forward and up until we reach toplevel."
  (paredit-forward-up (cljr--depth-at-point)))

(defun cljr--indent-defun ()
  "Indent the toplevel form containing point"
  (indent-region (cljr--point-after 'cljr--goto-toplevel)
                 (cljr--point-after 'cljr--goto-toplevel-forward)))

(defun cljr--point-at-text-matching
    (regexp direction &optional bound noerror count)
  "Return the point after searching in DIRECTION for TEXT.

DIRECTION is either :forward or :backward.

the optional arguments are passed on the to search function.  See
e.g. `re-search-forward'"
  (save-excursion
    (cond
     ;; NOTE: non-optional direction is intentional because I think it
     ;; improves readability greatly at the call site
     ((eq direction :forward)
      (re-search-forward regexp bound noerror count))
     ((eq direction :backward)
      (re-search-backward regexp bound noerror count))
     (t (error "Only know how to search :forward or :backward, you asked for '%s'"
               direction)))))

(defun cljr--inside-prefixed-libspec-vector-p ()
  "If we're inside a prefixed libspec vector then point is
assumed to be just inside the []

Note that this function also moves point from the suffix to the prefix."
  (and (looking-back "\\[" 1)
       (progn (paredit-backward-up 2)
              (paredit-forward-down)
              (looking-back "\\[" 1))))

(defun cljr--resolve-alias (alias)
  "Look up ALIAS in the ns form.

if alias is util and the ns-from contains

(:require [refactor-nrepl [util s-expresions]])
refactor-nrepl.util will be returned."
  (save-excursion
    (cljr--goto-ns)
    (when (re-search-forward
           (format ":as\\s-*\n*\\s-*%s\\_>" (regexp-quote alias))
           (cljr--point-after 'paredit-forward)
           :noerror)
      (paredit-backward-up)
      (paredit-forward-down)
      (let ((ns (buffer-substring-no-properties
                 (point)
                 (cljr--point-after 'paredit-forward))))
        (if (cljr--inside-prefixed-libspec-vector-p)
            (format "%s.%s" (buffer-substring-no-properties
                             (point) (cljr--point-after 'paredit-forward))
                    ns)
          ns)))))

(defun cljr--point-for-anon-function ()
  "Returns the location of point if the point is currently placed
at the opening parentheses of an anonymous function."
  (cond
   ((looking-at "(fn \\(\\_<[^ ]+\\_>[[:space:]\n]+\\)?\\[")
    (point))
   ((save-excursion (backward-char) (looking-at "#("))
    (1- (point)))))

(defun cljr--goto-fn-definition ()
  (if (zerop (car (paredit-current-parse-state)))
      (error "Not inside a s-expression.")
    (let* ((pt-orig (point))
           (search-bound (cljr--point-after 'cljr--goto-toplevel))
           found-fn-p)
      (while (not found-fn-p)
        (paredit-backward-up)
        (if-let (fn-beg (cljr--point-for-anon-function))
            (let ((fn-end (save-excursion (paredit-forward) (point))))
              (when (and (< fn-beg pt-orig) (< pt-orig fn-end))
                (setq found-fn-p t)
                (when (looking-back "#" 1)
                  (backward-char))))
          (when (<= (point) search-bound)
            (error "Can't find definition of anonymous function!")))))))

(defun cljr--evenp (n)
  ;; evenp lives in cl.el...
  (zerop (mod n 2)))

(defun cljr--create-msg (op &rest kvs)
  "Create a msg for the middleware for OP and optionally include
  the kv pairs KVS.

All config settings are included in the created msg."
  (cl-assert (cljr--evenp (length kvs)) nil "Can't create msg to send to the middleware.\
  Received an uneven number of kv pairs: %s " kvs)
  (apply #'list "op" op
         "prefix-rewriting"
         (if cljr-favor-prefix-notation
             "true"
           "false")
         "insert-newline-after-require"
         (if cljr-insert-newline-after-require
             "true"
           "false")
         "debug"
         (if cljr--debug-mode
             "true"
           "false")
         kvs))

(defun cljr--post-command-message (format-string &rest args)
  "Display msg in a post command hook, to ensure it doesn't drown
  in emacs' general chatter."
  (push (apply #'format format-string args)
        cljr--post-command-messages))

(defun cljr--post-command-hook ()
  (seq-map #'message cljr--post-command-messages)
  (setq cljr--post-command-messages nil))

(defun cljr-show-changelog ()
  "Show the changelog for `clj-refactor'.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-show-changelog"
  (interactive)
  (find-file (format "%s/CHANGELOG.md" (file-name-directory (locate-library "clj-refactor"))))
  (when (fboundp 'markdown-mode)
    (markdown-mode))
  (view-mode 1))


;; ------ reify protocol defrecord -----------

(defun cljr--goto-reify ()
  (let ((point (point)))
    (while (not (or (cljr--top-level-p)
                    (looking-at-p "(reify")))
      (paredit-backward-up))
    (unless (looking-at-p "(reify")
      (goto-char point)
      (error "Can't find call to reify!"))))

(defun cljr-reify-to-defrecord ()
  "Replace a call to reify with a call to a new constructor.
A new record is created to define this constructor."
  (interactive "")
  (cljr--goto-reify)
  (let ((record-name (cljr--prompt-user-for "Name of new record: "))
        (reify-sexp (clojure-delete-and-extract-sexp))
        (placeholder "#85dffa31d"))
    (insert placeholder)
    (cljr--new-toplevel-form reify-sexp)
    (paredit-backward)
    (paredit-forward-down)
    (clojure-delete-and-extract-sexp)
    (insert "defrecord " record-name " []")
    (if (looking-at-p "[ \t]*$")
        (forward-line)
      (newline-and-indent))
    (cljr--goto-toplevel)
    (indent-region (point) (cljr--point-after 'paredit-forward))
    (re-search-forward placeholder)
    (paredit-backward)
    (clojure-delete-and-extract-sexp)
    (insert "("record-name ".)")
    (paredit-backward-down)))

;; ------ file -----------

(defun cljr--locate-project-file (file)
  (ignore-errors
    (file-truename
     (locate-dominating-file default-directory file))))

(defun cljr--project-dir ()
  (or
   (thread-last  '("project.clj" "build.boot" "deps.edn" "shadow-cljs.edn" "pom.xml")
     (mapcar 'cljr--locate-project-file)
     (delete 'nil)
     car)
   ""))

(defun cljr--inside-project-p ()
  "Return non-nil if `default-directory' is inside a Clojure project."
  (not (string-empty-p (cljr--project-dir))))

(defun cljr--project-file ()
  (let ((project-dir (cljr--project-dir)))
    (or (let ((file (expand-file-name "project.clj" project-dir)))
          (and (file-exists-p file) file))
        (let ((file (expand-file-name "build.boot" project-dir)))
          (and (file-exists-p file) file))
        (let ((file (expand-file-name "deps.edn" project-dir)))
          (and (file-exists-p file) file))
        (let ((file (expand-file-name "shadow-cljs.edn" project-dir)))
          (and (file-exists-p file) file))
        (let ((file (expand-file-name "pom.xml" project-dir)))
          (and (file-exists-p file) file)))))

(defun cljr--project-files ()
  (split-string (shell-command-to-string
                 (format "find %s -type f \\( %s -or %s \\) %s | head -n %s"
                         (cljr--project-dir)
                         (format "-name \"%s\"" "*.clj")
                         (format "-name \"%s\"" "*.cljc")
                         "-not -regex \".*svn.*\""
                         1000))))

(defun cljr--project-with-deps-p (project-file)
  (string-match "/deps.edn$" project-file))

(defun cljr--buffers-visiting-dir (dir)
  (seq-filter (lambda (buf)
                (when-let (path (buffer-file-name buf))
                  (string-prefix-p dir path :ignore-case)))
              (buffer-list)))

(defun cljr--revisit-buffers (buffers new-dir active)
  "After moving a directory revisit all files visited by BUFFERS
  by looking them up in NEW-DIR.

ACTIVE is the buffer the user was looking at when the command was
issued, and should be left focused."
  (let ((files (directory-files new-dir))
        (new-dir (if (string-suffix-p "/" new-dir) new-dir (format "%s/" new-dir)))
        (same-file (lambda (buf f)
                     (when (string= (file-name-nondirectory f)
                                    (file-name-nondirectory (buffer-file-name buf)))
                       f))))
    (dolist (buf buffers)
      (find-file
       (format "%s%s" new-dir (seq-some (apply-partially same-file buf) files)))
      (kill-buffer buf))
    (find-file (format "%s/%s" new-dir (seq-some (apply-partially same-file active) files)))))


(defcustom cljr-print-right-margin 72
  "Will be forwarded to `clojure.pprint/*print-right-margin*'
when refactor-nrepl pretty-prints ns forms,
as performed after `clean-ns', `rename-file-or-dir', etc.
You can set it to the string \"nil\" for disabling line wrapping.

See also: `cljr-print-miser-width'."
  :group 'cljr
  :type '(choice integer string)
  :safe (lambda (s) (or (integerp s) (stringp s)))
  :package-version "3.2.0")

(defcustom cljr-print-miser-width 40
  "Will be forwarded to `clojure.pprint/*print-miser-width*'
when refactor-nrepl pretty-prints ns forms,
as performed after `clean-ns', `rename-file-or-dir', etc.
You can set it to the string \"nil\" for disabling line wrapping.

See also: `cljr-print-right-margin'."
  :group 'cljr
  :type '(choice integer string)
  :safe (lambda (s) (or (integerp s) (stringp s)))
  :package-version "3.2.0")

;;;###autoload
(defun cljr-rename-file-or-dir (old-path new-path)
  "Rename a file or directory of files.
Buffers visiting any affected file are killed and the
corresponding files are revisited.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-rename-file-or-dir"
  (interactive
   (let ((old (read-file-name "Old path: " nil nil 'mustmatch "")))
     (list old
           (if (file-directory-p old)
               (read-directory-name "New path: " old)
             (read-file-name "New path: "
                             (file-name-directory old)
                             nil nil
                             (file-name-nondirectory old))))))
  (cljr--ensure-op-supported "rename-file-or-dir")
  (when (cljr--asts-y-or-n-p)
    (let* ((active-buffer (current-buffer))
           (affected-buffers (when (file-directory-p old-path)
                               (cljr--buffers-visiting-dir old-path)))
           (old-path (expand-file-name old-path))
           (new-path (cljr--maybe-replace-dash-in-file-name (expand-file-name new-path)))
           (nrepl-new-path (funcall cider-to-nrepl-filename-function new-path))
           (nrepl-old-path (funcall cider-to-nrepl-filename-function old-path)))
      (when (y-or-n-p (format "Really rename %s to %s?" old-path new-path))
        (let* ((changed-files (cljr--call-middleware-sync
                               (cljr--create-msg "rename-file-or-dir"
                                                 "old-path" nrepl-old-path
                                                 "new-path" nrepl-new-path
                                                 "print-right-margin" cljr-print-right-margin
                                                 "print-miser-width" cljr-print-miser-width)
                               "touched"))
               (changed-files-count (length changed-files)))
          (cond
           ((null changed-files) (cljr--post-command-message "Rename complete! No files affected."))
           ((= changed-files-count 1) (cljr--post-command-message "Renamed %s to %s." old-path new-path))
           (t (cljr--post-command-message "Rename complete! %s files affected." changed-files-count)))
          (when (and (> changed-files-count 0) (not cljr-warn-on-eval))
            (cljr--warm-ast-cache)))
        (if affected-buffers
            (cljr--revisit-buffers affected-buffers new-path active-buffer)
          (kill-buffer active-buffer)
          (find-file new-path))))))

;;;###autoload
(defun cljr-rename-file (new-path)
  (interactive
   (let ((old (buffer-file-name)))
     (list (read-file-name "New path: "
                           (file-name-directory old)
                           nil nil
                           (file-name-nondirectory old)))))
  (cljr-rename-file-or-dir (buffer-file-name) new-path))

(defun cljr--op-supported-p (op)
  "Is the OP we require provided by the current middleware stack?"
  (cider-nrepl-op-supported-p op))

(defun cljr--assert-middleware ()
  (unless (featurep 'cider)
    (user-error "CIDER isn't installed!"))
  (unless (cider-connected-p)
    (user-error "CIDER isn't connected!"))
  (unless (cljr--op-supported-p "find-symbol")
    (user-error "The refactor-nrepl middleware isn't available! \
Did you remember to install it?")))

(defun cljr--ensure-op-supported (op)
  "Check for support of middleware op OP.

Signal an error if it is not supported, otherwise return OP."
  (cljr--assert-middleware)
  (if (cljr--op-supported-p op)
      op
    (user-error "Can't find nREPL middleware providing op \"%s\".  \
Please, install (or update) refactor-nrepl %s and restart the REPL."
                op (upcase cljr-injected-middleware-version))))

(defun cljr--assert-leiningen-project ()
  (unless (string= (file-name-nondirectory (or (cljr--project-file) ""))
                   "project.clj")
    (user-error "Can't find project.clj!")))

;; ------ ns statements -----------

(defun cljr--goto-ns ()
  "Go to the first namespace defining form in the buffer."
  (goto-char (point-min))
  (if (re-search-forward clojure-namespace-regexp nil t)
      (cljr--goto-toplevel)
    (error "No namespace declaration found")))

(defun cljr--goto-closest-ns ()
  "Go to the closest namespace defining form.
Go to the namespace form closest to point and above it.  If there is
no namespace form above point, return the first one in the buffer."
  (save-restriction
    (widen)
    ;; The closest ns form above point.
    (when (or (re-search-backward clojure-namespace-regexp nil t)
              ;; Or any form at all.
              (and (goto-char (point-min))
                   (re-search-forward clojure-namespace-regexp nil t)))
      (cljr--goto-toplevel))))

(defun cljr--goto-cljs-branch ()
  "Move POINT to the cljs branch of the reader conditional following point."
  (if (re-search-forward ":cljs" (cljr--point-after 'paredit-forward) :no-error)
      (save-excursion
        (paredit-backward-up)
        (unless (looking-back "#\?@?" 3)
          (error "No cljs branch found")))
    (error "No cljs branch found")))

(defun cljr--insert-in-ns (type &optional cljs?)
  "Insert another clause into the TYPE clause of the ns statement.

TYPE is :require, :use etc

If CLJS? is T we insert in the cljs part of the ns declaration."
  (cljr--goto-ns)
  (when cljs?
    (cljr--goto-cljs-branch))
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
  (with-current-buffer
      (find-file-noselect (cljr--project-file))
    (goto-char (point-min))
    (search-forward package nil t)))

(defun cljr--find-source-ns-of-test-ns (test-ns test-file)
  (let* ((ns-chunks (split-string test-ns "[.]" t))
         (test-name (car (last ns-chunks)))
         (src-dir-name (replace-regexp-in-string "test/" "src/" (file-name-directory test-file) t t))
         (replace-underscore (apply-partially 'replace-regexp-in-string "_" "-"))
         (src-ns (car (seq-filter (lambda (it) (or (string-prefix-p it test-name)
                                                   (string-suffix-p it test-name)))
                                  (seq-map (lambda (file-name)
                                             (funcall replace-underscore
                                                      (file-name-sans-extension file-name)))
                                           (directory-files src-dir-name))))))
    (when src-ns
      (mapconcat 'identity (append (butlast ns-chunks) (list src-ns)) "."))))

(defun cljr--cljs-file-p (&optional buf)
  "Is BUF, or the current buffer, visiting a cljs file?"
  (string-equal (file-name-extension (buffer-file-name (or buf (current-buffer))))
                "cljs"))

(defun cljr--cljc-file-p (&optional buf)
  "Is BUF, or the current buffer, visiting a cljc file?"
  (string-equal (file-name-extension (buffer-file-name (or buf (current-buffer))))
                "cljc"))

(defun cljr--clj-file-p (&optional buf)
  "Is BUF, or the current buffer, visiting a clj file?"
  (or (eq major-mode 'clojure-mode)
      (string-equal (file-name-extension (buffer-file-name (or buf (current-buffer))))
                    "clj")))

(defun cljr--add-test-declarations ()
  (save-excursion
    (let* ((ns (clojure-find-ns))
           (source-ns (cljr--find-source-ns-of-test-ns ns (buffer-file-name))))
      (cljr--insert-in-ns ":require")
      (when source-ns
        (insert "[" source-ns " :as "
                cljr-clojure-test-namespace-under-test-alias "]"))
      (cljr--insert-in-ns ":require")
      (insert (cond
               ((cljr--project-depends-on-p "midje")
                cljr-midje-test-declaration)
               ((cljr--project-depends-on-p "expectations")
                cljr-expectations-test-declaration)
               ((cljr--cljs-file-p)
                cljr-cljs-clojure-test-declaration)
               ((cljr--cljc-file-p)
                cljr-cljc-clojure-test-declaration)
               (t cljr-clojure-test-declaration))))
    (indent-region (point-min) (point-max))))

(defun cljr--in-tests-p ()
  "Check whether the current file is a test file.

Two checks are made - whether the namespace of the file has the
word test in it and whether the file lives under the test/ directory."
  (or (string-match-p "\\btest\\b" (clojure-find-ns))
      (string-match-p "/test" (buffer-file-name))))

(defun cljr--clojure-ish-filename-p (file-name)
  (or (string-suffix-p ".clj" file-name)
      (string-suffix-p ".cljs" file-name)
      (string-suffix-p ".cljx" file-name)
      (string-suffix-p ".cljc" file-name)))

(defun cljr--clojure-filename-p (&optional file-name)
  (or (string-suffix-p ".clj" (or file-name (buffer-file-name)))
      (string-suffix-p ".cljc" (or file-name (buffer-file-name)))))

(defun cljr--add-ns-if-blank-clj-file ()
  (ignore-errors
    (when (and cljr-add-ns-to-blank-clj-files
               (cljr--clojure-ish-filename-p (buffer-file-name))
               (= (point-min) (point-max)))
      (insert (format "(ns %s)\n\n" (cider-expected-ns)))
      (when (cljr--in-tests-p)
        (cljr--add-test-declarations)))))

(add-hook 'find-file-hook 'cljr--add-ns-if-blank-clj-file)

(defun cljr--dash-in-file-name-p (file-name)
  (and file-name (string-match-p "-[^/]+\.clj[sxc]?$" file-name)))

(defun cljr--maybe-replace-dash-in-file-name (file-name)
  (if (and (cljr--dash-in-file-name-p file-name)
           (yes-or-no-p "The file name contains dashes. Replace with underscores? "))
      (concat (file-name-directory file-name)
              (replace-regexp-in-string "-" "_" (file-name-nondirectory file-name)))
    file-name))

(defun cljr--ensure-no-dashes-in-filename ()
  (when (and (buffer-file-name)
             (not (file-exists-p (buffer-file-name))) ; only new files
             (cljr--dash-in-file-name-p (buffer-file-name)))
    (let ((new-name (cljr--maybe-replace-dash-in-file-name (buffer-file-name))))
      (rename-buffer new-name)
      (set-visited-file-name new-name)
      (message "Changed file name to '%s'"
               (file-name-nondirectory new-name)))))

(add-hook 'find-file-hook 'cljr--ensure-no-dashes-in-filename)

(defun cljr--get-ns-statements (statement-type)
  (save-excursion
    (cljr--goto-ns)
    (when (cljr--search-forward-within-sexp (concat "(" statement-type))
      (let ((beg (point)))
        (paredit-forward-up)
        (paredit-backward-down)
        (buffer-substring-no-properties beg (point))))))

(defun cljr--get-ns-statements-as-list (statement-type)
  (save-excursion
    (cljr--goto-ns)
    (when (cljr--search-forward-within-sexp (concat "(" statement-type))
      (cljr--extract-sexp-as-list t))))

(defun cljr--extract-sexp-content (sexp)
  (replace-regexp-in-string "\\[?(?]?)?" "" sexp))

(defun cljr--maybe-clean-ns ()
  (when (and cljr-auto-clean-ns (cider-connected-p)
             (cljr--op-supported-p "clean-ns"))
    (let ((*cljr--noninteractive* t))
      (cljr-clean-ns))))

(defvar cljr--tmp-marker (make-marker))

(defun cljr--pop-tmp-marker-after-yasnippet-1 (&rest _)
  (goto-char cljr--tmp-marker)
  (set-marker cljr--tmp-marker nil)
  (remove-hook 'yas-after-exit-snippet-hook
               'cljr--pop-tmp-marker-after-yasnippet-1 :local))

(defun cljr--pop-tmp-marker-after-yasnippet ()
  (add-hook 'yas-after-exit-snippet-hook
            'cljr--pop-tmp-marker-after-yasnippet-1 nil :local))

(defun cljr--maybe-eval-ns-form-and-remove-hook ()
  (cljr--maybe-eval-ns-form)
  (remove-hook 'yas-after-exit-snippet-hook
               'cljr--maybe-eval-ns-form-and-remove-hook :local))

(defun cljr--maybe-sort-ns ()
  (when (and cljr-auto-sort-ns (cider-connected-p)
             (cljr--op-supported-p "clean-ns"))
    (cljr--clean-ns nil :no-pruning)))

(defun cljr--sort-and-remove-hook (&rest _)
  (cljr--maybe-sort-ns)
  (remove-hook 'yas-after-exit-snippet-hook
               'cljr--sort-and-remove-hook :local))

(defun cljr--add-yas-ns-updated-hook ()
  (add-hook 'yas-after-exit-snippet-hook 'cljr--sort-and-remove-hook nil :local)
  (add-hook 'yas-after-exit-snippet-hook
            'cljr--maybe-eval-ns-form-and-remove-hook nil :local))

;;;###autoload
(defun cljr-add-require-to-ns (cljs?)
  "Add a require statement to the ns form in current buffer.

With a prefix act on the cljs part of the ns declaration.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-add-require-to-ns"
  (interactive "P")
  (set-marker cljr--tmp-marker (point))
  (cljr--insert-in-ns ":require" cljs?)
  (cljr--pop-tmp-marker-after-yasnippet)
  (cljr--add-yas-ns-updated-hook)
  (yas-expand-snippet cljr--add-require-snippet))

;;;###autoload
(defun cljr-add-use-to-ns (cljs?)
  "Add a use statement to the buffer's ns form.

With a prefix act on the cljs part of the ns declaration.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-add-use-to-ns"
  (interactive "P")
  (set-marker cljr--tmp-marker (point))
  (cljr--insert-in-ns ":require" cljs?)
  (cljr--pop-tmp-marker-after-yasnippet)
  (cljr--add-yas-ns-updated-hook)
  (yas-expand-snippet cljr--add-use-snippet))

;;;###autoload
(defun cljr-add-import-to-ns (&optional cljs?)
  "Add an import statement to the buffer's ns form.

With a prefix act on the cljs part of the ns declaration.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-add-import-to-ns"
  (interactive "P")
  (set-marker cljr--tmp-marker (point))
  (cljr--insert-in-ns ":import" cljs?)
  (cljr--pop-tmp-marker-after-yasnippet)
  (cljr--add-yas-ns-updated-hook)
  (yas-expand-snippet "$1"))

(defun cljr--extract-refer-all-namespaces ()
  "Returns a list of all the namespaces that are required with :refer :all"
  (cljr--goto-ns)
  (when (search-forward ":require" nil t)
    (let (nses
          (require-end (save-excursion
                         (progn
                           (paredit-backward-up)
                           (paredit-forward)
                           (point)))))
      (if (not (save-excursion (re-search-forward ":refer\s:all" require-end t)))
          (message "There is no :refer :all in the ns declaration.")
        (while (re-search-forward ":refer\s:all" require-end t)
          (paredit-backward-up)
          (let ((prefix (save-excursion
                          (when (progn
                                  (paredit-backward-up)
                                  (not (looking-at-p "(:require")))
                            (paredit-forward-down)
                            (buffer-substring (point) (cljr--point-after 'paredit-forward)))))
                (ns (progn
                      (paredit-forward-down)
                      (buffer-substring (point) (cljr--point-after 'paredit-forward)))))
            (push
             (if prefix
                 (concat prefix "." ns)
               ns)
             nses)
            (paredit-forward-up)))
        (nreverse nses)))))

;;;###autoload
(defun cljr-require-macro ()
  "Add a require statement for a macro to the ns form in current buffer.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-require-macro"
  (interactive)
  (set-marker cljr--tmp-marker (point))
  (cljr--insert-in-ns ":require-macros")
  (cljr--pop-tmp-marker-after-yasnippet)
  (cljr--add-yas-ns-updated-hook)
  (yas-expand-snippet cljr--add-require-snippet))

;;;###autoload
(defun cljr-stop-referring ()
  "Stop referring to vars in the namespace at point.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-stop-referring"
  (interactive)
  (save-excursion
    (paredit-backward-up)
    (unless (looking-at "\\[")
      (user-error "Place cursor on the namespace whose vars you want to stop referring to."))
    (paredit-backward-up)
    (unless (looking-at "(:require")
      (user-error "Place cursor on the namespace whose vars you want to stop referring to.")))
  (save-excursion
    (paredit-backward-up)
    (let* ((bound (save-excursion
                    (paredit-forward)
                    (point)))
           (ns (save-excursion
                 (paredit-forward-down)
                 (let ((beg (point)))
                   (paredit-forward)
                   (buffer-substring-no-properties beg (point)))))
           (alias (save-excursion
                    (paredit-forward-down)
                    (when (search-forward " :as " bound t)
                      (let ((beg (point)))
                        (paredit-forward)
                        (buffer-substring-no-properties beg (point)))))))
      (unless (re-search-forward ":refer" bound t)
        (user-error "No :refer clause found."))
      (if (looking-at "\s*:all")
          (progn
            (paredit-backward-up)
            (cljr--replace-refer-all ns alias))
        (paredit-forward-down)
        (let* ((beg (point))
               (str (progn (paredit-forward-up)
                           (paredit-backward-down)
                           (buffer-substring-no-properties beg (point))))
               (symbols (split-string (string-trim str) " " t)))
          (paredit-backward-up)
          (paredit-backward)
          (clojure-delete-and-extract-sexp)
          (clojure-delete-and-extract-sexp)
          (cond
           ((looking-at-p "\\s-*[\\w:]+")
            (just-one-space))

           ((looking-back "\\w+ " 3)
            (just-one-space 0))

           (t (join-line)))
          (cljr--add-ns-prefix (or alias ns) symbols))))))

(defun cljr--add-ns-prefix (ns symbols)
  "Adds an NS prefix to every symbol in SYMBOLS."
  (save-excursion
    (cljr--goto-ns)
    (paredit-forward)
    (let ((case-fold-search nil))
      (while (re-search-forward (regexp-opt symbols 'symbols) nil t)
        (when (save-excursion
                (paredit-backward-up)
                (not (looking-at "\"")))
          (paredit-backward)
          (insert ns "/")
          (paredit-forward))))))

(defun cljr--insert-with-proper-whitespace (forms)
  (open-line 2)
  (forward-line 2)
  (let ((p (point)))
    (insert forms)
    (open-line 2)
    (forward-line)
    (cljr--just-one-blank-line)
    (save-excursion
      (goto-char p)
      (cljr--just-one-blank-line))))

(defun cljr--slice-at (pattern s)
  "Slice S up at every index matching PATTERN"
  (let ((slices (split-string s pattern)))
    (cons (car slices) (mapcar (lambda (slice) (concat pattern slice)) (cdr slices)))))

;;;###autoload
(defun cljr-move-form ()
  "Move the form containing POINT to a new namespace.

If REGION is active, move all forms contained by region.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-move-form"
  (interactive)
  (let* ((forms (if (region-active-p)
                    (let ((beg (region-beginning))
                          (end (region-end)))
                      (prog2
                          (paredit-check-region-for-delete beg end)
                          (buffer-substring-no-properties beg end)
                        (delete-region beg end)))
                  (cljr--goto-toplevel)
                  (prog1 (clojure-delete-and-extract-sexp)
                    (join-line)
                    (join-line)
                    (delete-char 1))))
         (forms (cljr--cleanup-whitespace forms))
         (requires (cl-rest (cljr--get-ns-statements-as-list ":require"))))
    (let (ns names target-ns-alias
             (target-ns-regexp-template "[\(\[]\\s-*%s")
             (target-ns-alias-template ":as\\s-*\n*\\s-*\\(.*\\)\\_>\\s-*\n*\\s-*[\]\)]"))
      (save-window-excursion
        (ido-find-file)
        (setq ns (cljr--current-namespace)
              names (cljr--name-of-defns forms)
              target-ns-alias (when-let ((filtered-require (seq-find
                                                            (lambda (it)
                                                              (string-match-p (format target-ns-regexp-template ns) it))
                                                            requires)))
                                (thread-last filtered-require
                                  (cljr--slice-at ":as")
                                  last
                                  car
                                  (replace-regexp-in-string (format target-ns-alias-template ns) "\\1")
                                  string-trim)))
        (goto-char (point-max))
        (cljr--insert-with-proper-whitespace
         (cljr--remove-references-of-target-ns forms ns target-ns-alias))
        (when requires
          (cljr--insert-in-ns ":require")
          (thread-last (seq-remove (lambda (it) (string-match-p (format target-ns-regexp-template ns) it)) requires)
            (apply #'concat)
            string-trim
            insert)
          (cljr-clean-ns))
        (save-buffer))
      (cljr--update-ns-after-moving-fns ns (nreverse names))
      (cljr-clean-ns)))
  (cljr--just-one-blank-line))

(defun cljr--remove-references-of-target-ns (forms ns alias)
  (thread-last (replace-regexp-in-string (format "\\_<%s/" alias) "" forms)
    (replace-regexp-in-string (format "\\_<%s/" ns) "")))

(defun cljr--update-ns-after-moving-fns (ns &optional refer-names)
  "Updates the current ns declaration after moving defn forms out of the
  current file and to NS.  Optionally referring the names in REFER-NAMES."
  (save-excursion
    (cljr--goto-ns)
    (let* ((ns-present-p (cljr--search-forward-within-sexp ns :save-excursion))
           (refer-present-p (cljr--search-forward-within-sexp ":refer" :save-excursion))
           (refer-all-p (cljr--search-forward-within-sexp ":refer :all" :save-excursion))
           (require-present-p (cljr--search-forward-within-sexp
                               (concat ":require [" ns)
                               :save-excursion)))
      (if ns-present-p
          (unless (or refer-all-p (null refer-names))
            (if refer-present-p
                (cljr--append-names-to-refer ns refer-names)
              (when require-present-p
                (cljr--append-refer-clause ns refer-names))))
        (cljr--new-require-clause ns refer-names)))))

(defun cljr--append-refer-clause (ns refer-names)
  "Appends :refer [REFER-NAMES] to the :require clause for NS."
  (save-excursion
    (cljr--goto-ns)
    (re-search-forward ":require")
    (re-search-forward ns)
    (paredit-forward-up)
    (backward-char)
    (insert " :refer [" (string-join refer-names  " ") "]")))

(defun cljr--append-names-to-refer (ns names)
  "Append NAMES to the :refer vector for NS"
  (save-excursion
    (cljr--goto-ns)
    (re-search-forward ":require")
    (re-search-forward ns)
    (re-search-forward ":refer")
    (paredit-forward)
    (backward-char)
    (insert (format " %s" (string-join names  " ")))))

(defun cljr--new-require-clause (ns &optional refer-names)
  "Creates a new :require clause for NS.

Optionally adds :refer [REFER-NAMES] clause."
  (cljr--insert-in-ns ":require")
  (insert "[" ns "]")
  (when refer-names
    (cljr--append-refer-clause ns refer-names)))

(defmacro cljr--with-string-content (s &rest body)
  (declare (debug (form body))
           (indent 1))
  `(with-temp-buffer
     (delay-mode-hooks
       (clojure-mode)
       (insert ,s)
       (goto-char (point-min))
       ,@body)))

(defun cljr--name-of-defns (string-with-defns &optional include-private)
  "Returns a list of the function names in STRING-WITH-DEFNS,
optionally including those that are declared private."
  (cljr--with-string-content string-with-defns
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
          (end (cljr--point-after 'paredit-forward)))
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
  (when (looking-at "\\(.+/\\)?def")
    (paredit-forward)
    (while (looking-at " ^")
      (paredit-forward))
    (forward-char)
    (let ((beg (point))
          (end (cljr--point-after 'paredit-forward)))
      (buffer-substring-no-properties beg end))))

(defun cljr--already-declared-p (def)
  (save-excursion
    (cljr--goto-declare)
    (re-search-backward def (cljr--point-after 'paredit-backward) :no-error)))

(defun cljr--add-declaration (def)
  (when (cljr--already-declared-p def)
    (user-error "%s is already declared" def))
  (save-excursion
    (cljr--goto-declare)
    (backward-char)
    (insert " " def)
    (cljr--post-command-message "Added declaration for %s" def)))

;;;###autoload
(defun cljr-add-declaration (for-thing-at-point-p)
  "Add a declare for the current def near the top of the buffer.

With a prefix add a declaration for the symbol under the cursor instead.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-add-declaration"
  (interactive "P")
  (if-let (def (and (not for-thing-at-point-p)
                    (save-excursion (cljr--name-of-current-def))))
      (cljr--add-declaration def)
    (cljr--add-declaration (cider-symbol-at-point))))

;; ------ extract constant ----------------

(defun cljr--existing-group-of-defs-p ()
  "Is there a group of defs at the beginning of the namespace?"
  (save-excursion
    (cljr--goto-ns)
    (paredit-forward)
    (cljr--skip-past-whitespace-and-comments)
    (looking-at-p "(def ")))

(defun cljr--prepare-to-append-to-existing-group-of-defs ()
  "Place point at the end of the final def form in the group of
defs appearing at the start of the ns.

Point is assumed to be just after the ns form."
  (cljr--skip-past-whitespace-and-comments)
  (while (looking-at-p "(def ")
    (paredit-forward)
    (forward-line)
    (cljr--skip-past-whitespace-and-comments))
  (paredit-backward)
  (paredit-forward))

(defun cljr--prepare-to-insert-new-def ()
  "Skip past all previously defined vars at the beginning of the ns."
  (cljr--goto-ns)
  (paredit-forward)
  (if (cljr--existing-group-of-defs-p)
      (cljr--prepare-to-append-to-existing-group-of-defs)
    (insert "\n")))

(defun cljr--extract-def-at-point (&optional const?)
  (let ((name (let ((highlight (cljr--highlight-sexp)))
                (unwind-protect
                    (cljr--prompt-user-for "Name: ")
                  (delete-overlay highlight))))
        (body (clojure-delete-and-extract-sexp))
        const-pos)
    (save-excursion
      (cljr--prepare-to-insert-new-def)
      (insert "\n(def ")
      (when const?
        (insert "^:const "))
      (insert name)
      (insert " " body ")")
      (setq const-pos (point)))
    (insert name)
    (save-excursion
      (query-replace body name nil const-pos (point-max)))))

;;;###autoload
(defun cljr-extract-constant ()
  "Extract form at (or above) point as a constant.
Create a def for it at the top level, and replace its current
occurrence with the defined name.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-extract-constant"
  (interactive)
  (cljr--extract-def-at-point :const))

;;;###autoload
(defun cljr-extract-def ()
  "Extract form at (or above) point as a def.
Create a def for it at the top level, and replace its current
occurrence with the defined name.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-extract-def"
  (interactive)
  (cljr--extract-def-at-point))

(defun cljr--goto-thread ()
  (while (not (or (cljr--top-level-p)
                  (looking-at "\(.*->>?[\n\r\t ]")))
    (paredit-backward-up)))

(defun cljr--reindent-thread ()
  (cljr--goto-thread)
  (let ((beg (point))
        (end (cljr--point-after 'paredit-forward)))
    (indent-region beg end)))

;;;###autoload
(defun cljr-cycle-thread ()
  "Cycle a threading macro between -> and ->>.
Also applies to other versions of the macros, like cond->.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-cycle-thread"
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

;; ------ let binding ----------

;;;###autoload
(defun cljr-introduce-let (&optional n)
  "Create a let form, binding the form at point.
The resulting let form can then be expanded with `\\[cljr-expand-let]'.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-introduce-let"
  (interactive "P")
  (if (not (cljr--use-multiple-cursors-p))
      (clojure-introduce-let n)
    (paredit-wrap-round)
    (insert "let ")
    (paredit-wrap-square)
    (insert " ")
    (backward-char)
    (mc/create-fake-cursor-at-point)
    (paredit-forward-up)
    (newline-and-indent)
    (mc/maybe-multiple-cursors-mode)))

(add-to-list 'mc--default-cmds-to-run-once 'cljr-introduce-let)

(defun cljr--get-let-bindings ()
  "Returns a list of lists. The inner lists contain two elements first is
   the binding, second is the init-expr"
  (seq-partition (clojure--read-let-bindings) 2))

(defun cljr--replace-sexp-with-binding (binding)
  (save-excursion
    (let ((bind-var (car binding))
          (init-expr (car (last binding)))
          (end (cljr--point-after 'clojure--goto-let 'paredit-forward)))
      (while (re-search-forward (clojure--sexp-regexp init-expr) end t)
        (replace-match (concat "\\1" bind-var "\\2"))))))

(defun cljr--one-shot-keybinding (key command)
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd key) command)
     map) t))

(defun cljr--paredit-convolute-no-advice ()
  (if (not (advice-member-p #'clojure--replace-let-bindings-and-indent 'paredit-convolute-sexp))
      (paredit-convolute-sexp)
    (advice-remove 'paredit-convolute-sexp #'clojure--replace-let-bindings-and-indent)
    (paredit-convolute-sexp)
    (advice-add 'paredit-convolute-sexp :after #'clojure--replace-let-bindings-and-indent)))

;;;###autoload
(defun cljr-expand-let ()
  "Expand the let form above point by one level.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-expand-let"
  (interactive)
  (multiple-cursors-mode 0)
  (clojure--goto-let)
  (paredit-forward-down 2)
  (paredit-forward-up)
  (cljr--skip-past-whitespace-and-comments)
  (cljr--paredit-convolute-no-advice)
  (mapc 'cljr--replace-sexp-with-binding (cljr--get-let-bindings))
  (cljr--one-shot-keybinding "l" 'cljr-expand-let))

(defun cljr--replace-sexp-with-binding-in-let ()
  (remove-hook 'multiple-cursors-mode-disabled-hook 'cljr--replace-sexp-with-binding-in-let)
  (save-excursion
    (mapc 'cljr--replace-sexp-with-binding (cljr--get-let-bindings))))

;;;###autoload
(defun cljr-move-to-let ()
  "Move the form at point to a binding in the nearest let.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-move-to-let"
  (interactive)
  (if (not (cljr--use-multiple-cursors-p))
      (clojure-move-to-let)
    (if (not (save-excursion (clojure--goto-let)))
        (cljr-introduce-let)
      (save-excursion
        (let ((contents (clojure-delete-and-extract-sexp)))
          (clojure--prepare-to-insert-new-let-binding)
          (insert contents))
        (backward-sexp)
        (insert " ")
        (backward-char)
        (mc/create-fake-cursor-at-point))
      (add-hook 'multiple-cursors-mode-disabled-hook 'cljr--replace-sexp-with-binding-in-let)
      (mc/maybe-multiple-cursors-mode))))

(defun cljr--eliminate-let ()
  "Remove a the nearest let form.

This function only does the actual removal."
  (clojure--goto-let)
  (paredit-forward-down)
  (paredit-forward 2)
  (paredit-splice-sexp-killing-backward))

(defun cljr-remove-let ()
  "Inlines all variables in the let form and removes it.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-remove-let"
  (interactive)
  (save-excursion
    (let ((*cljr--noninteractive* t)) ; make `cljr-inline-symbol' be quiet
      (clojure--goto-let)
      (paredit-forward-down 2)
      (dotimes (_ (length (save-excursion (cljr--get-let-bindings))))
        (cljr-inline-symbol)
        (cljr--skip-past-whitespace-and-comments)))))

(add-to-list 'mc--default-cmds-to-run-once 'cljr-move-to-let)

;; ------ Destructuring ----

;;;###autoload
(defun cljr-destructure-keys ()
  "Change a symbol binding at point to a destructuring bind.
Keys to use in the destructuring are inferred from the code, and
their usage is replaced with the new local variables.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-destructure-keys"
  (interactive)
  (save-excursion
    (paredit-backward-up)
    (unless (looking-at "\\[")
      (user-error "Place point on the symbol to destructure inside the [let form]")))
  (let* ((symbol (cider-symbol-at-point))
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
        (setq symbols (cons (cider-symbol-at-point) symbols))))
    (save-excursion ;; find new bound
      (paredit-backward-up 2)
      (paredit-forward)
      (setq bound (point)))
    (save-excursion ;; are there any more usages of symbol?
      (paredit-forward-up)
      (when (re-search-forward (regexp-opt (list symbol) 'symbols) bound t)
        (setq include-as t)))
    (when (looking-back "\\s_\\|\\sw" 3)
      (paredit-backward))
    (kill-sexp)
    (insert "{:keys [" (string-join (seq-uniq (reverse symbols)) " ") "]"
            (if include-as (concat " :as " symbol) "") "}")))

;; ------ Cycling ----------

(defun cljr--goto-if ()
  (while (not (or (cljr--top-level-p)
                  (looking-at "\\((if \\)\\|\\((if-not \\)")))
    (paredit-backward-up)))

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

(defun cljr--magic-requires-re ()
  (regexp-opt (seq-map 'car cljr-magic-require-namespaces)))

(defun cljr--goto-reader-conditional ()
  "Move point just before #?.

Return the value of point if we moved."
  (let ((start (point))
        found)
    (while (not (or found (cljr--top-level-p)))
      (paredit-backward-up)
      (when (looking-back (regexp-opt (list "#\?@" "#\?")) (point-at-bol))
        (paredit-backward)
        (setq found t)))
    (if found
        (point)
      (goto-char start)
      nil)))

(defun cljr--point-in-reader-conditional-p ()
  "Return t if point is inside a reader conditional."
  (save-excursion
    (cljr--goto-reader-conditional)))

(defun cljr--point-in-reader-conditional-branch-p (feature)
  "Is point in a reader conditional branch for FEATURE?

FEATURE is either :clj or :cljs."
  (cl-assert (or (eq feature :clj) (eq feature :cljs)) nil
             "FEATURE has to be either :clj or :cljs.  Received: %s" feature)
  (save-excursion
    (let ((start-reader-conditional
           (cljr--point-after 'cljr--goto-reader-conditional))
          (other (if (eq feature :clj) ":cljs\\b" ":clj\\b"))
          found)
      (when start-reader-conditional
        (while (not (or (setq found (looking-at-p (format "%s\\b" feature)))
                        (looking-at-p other)
                        (< (point) start-reader-conditional)))
          (paredit-backward))
        found))))

(defun cljr--clj-context-p ()
  "Is point in a clj context?"
  (or (cljr--clj-file-p)
      (when (cljr--cljc-file-p)
        (cond
         ((cljr--point-in-reader-conditional-p)
          (cljr--point-in-reader-conditional-branch-p :clj))
         (cljr-assume-language-context
          (string-equal cljr-assume-language-context "clj"))
         (t
          (string-equal (cljr--prompt-user-for "Language context at point? "
                                               (list "clj" "cljs"))
                        "clj"))))))

(defun cljr--aget (map key)
  (cdr (assoc key map)))

(defun cljr--call-middleware-for-namespace-aliases ()
  (thread-first "namespace-aliases"
    cljr--ensure-op-supported
    cljr--create-msg
    (cljr--call-middleware-sync "namespace-aliases")
    parseedn-read-str))

(defun cljr--get-aliases-from-middleware ()
  (when-let (aliases (cljr--call-middleware-for-namespace-aliases))
    (if (cljr--clj-context-p)
        (gethash :clj aliases)
      (gethash :cljs aliases))))

(defun cljr--js-alias-p (alias)
  (and (cljr--cljs-file-p)
       (string-equal "js" alias)))

(defun cljr--magic-requires-lookup-alias ()
  "Return (alias (ns.candidate1 ns.candidate1)) if we recognize
the alias in the project."
  (let ((short (thread-last (buffer-substring-no-properties
                             (cljr--point-after 'paredit-backward)
                             (1- (point)))
                 (string-remove-prefix "::")
                 (string-remove-prefix "@"))))
    (unless (or (cljr--resolve-alias short)
                (cljr--js-alias-p short))
      (if-let ((aliases (ignore-errors (cljr--get-aliases-from-middleware)))
               (candidates (gethash (intern short) aliases)))
          (list short candidates)
        (when (and cljr-magic-require-namespaces ; a regex against "" always triggers
                   (string-match-p (cljr--magic-requires-re) short))
          ;; This when-let might seem unnecessary but the regexp match
          ;; isn't perfect.
          (when-let (long (cljr--aget cljr-magic-require-namespaces short))
            (list short (list long))))))))

(defun cljr--in-keyword-sans-alias-p ()
  "Checks if thing at point is keyword without an alias."
  (let ((sym (cider-symbol-at-point)))
    (and (cljr--keywordp sym)
         (not (string-match-p "::.+" (cljr--symbol-prefix sym))))))

(defun cljr--in-map-destructuring? ()
  "True when `point' is inside a destructuring form."
  (ignore-errors
    (save-excursion
      (paredit-backward-up 2)
      (paredit-forward-down)
      (looking-at-p ":keys"))))

(defun cljr--in-ns-above-point-p ()
  (save-excursion
    (cljr--goto-closest-ns)
    (looking-at-p "(\\s-*in-ns")))

(defun cljr--in-reader-literal-p ()
  (save-excursion
    (clojure-backward-logical-sexp 1)
    (looking-at-p "#")))

(defun cljr--in-number-p ()
  (save-excursion
    (backward-sexp 1)
    (looking-at-p "[-+0-9]")))

;;;###autoload
(defun cljr-slash ()
  "Inserts / as normal, but also checks for common namespace shorthands to require.
If `cljr-magic-requires' is non-nil, executing this command after one of the aliases
listed in `cljr-magic-require-namespaces', or any alias used elsewhere in the project,
will add the corresponding require statement to the ns form."
  (interactive)
  (insert "/")
  (when-let (aliases (and cljr-magic-requires
                          (not (cljr--in-map-destructuring?))
                          (not (cljr--in-ns-above-point-p))
                          (not (cljr--in-reader-literal-p))
                          (not (cider-in-comment-p))
                          (not (cider-in-string-p))
                          (not (cljr--in-keyword-sans-alias-p))
                          (not (cljr--in-number-p))
                          (clojure-find-ns)
                          (cljr--magic-requires-lookup-alias)))
    (let ((short (cl-first aliases)))
      (when-let (long (cljr--prompt-user-for "Require " (cl-second aliases)))
        (when (and (not (cljr--in-namespace-declaration-p (concat ":as " short "\b")))
                   (or (not (eq :prompt cljr-magic-requires))
                       (not (> (length (cl-second aliases)) 1)) ; already prompted
                       (yes-or-no-p (format "Add %s :as %s to requires?" long short))))
          (save-excursion
            (cljr--insert-in-ns ":require")
            (let ((libspec (format "[%s :as %s]" long short)))
              (insert libspec)
              (ignore-errors (cljr--maybe-eval-ns-form))
              (cljr--indent-defun)
              (cljr--post-command-message "Required %s" libspec))))))))

(defun cljr--in-namespace-declaration-p (s)
  (save-excursion
    (cljr--goto-ns)
    (cljr--search-forward-within-sexp s)))

;; ------ project clean --------

(defun cljr--excluded-from-project-clean-p (filename)
  (member (thread-last filename
            (string-remove-prefix (cljr--project-dir))
            (string-remove-prefix "/"))
          cljr-project-clean-exceptions))

;;;###autoload
(defun cljr-project-clean ()
  "Run `cljr-project-clean-functions' on every clojure file, then
sorts the project's dependency vectors.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-project-clean"
  (interactive)
  (when (or (not cljr-project-clean-prompt)
            (yes-or-no-p "Cleaning your project might change many of your clj files. Do you want to proceed?"))
    (let ((*cljr--noninteractive* t))
      (dolist (filename (cljr--project-files))
        (when (and (cljr--clojure-filename-p filename)
                   (not (cljr--excluded-from-project-clean-p filename)))
          (cljr--update-file filename
            (ignore-errors (seq-map 'funcall cljr-project-clean-functions))))))
    (if (and (cider-connected-p) (not cljr-warn-on-eval) (cljr--op-supported-p "warm-ast-cache"))
        (cljr--warm-ast-cache))
    (cljr--post-command-message "Project clean done.")))

(defun cljr--extract-dependency-name ()
  (cl-assert (cljr--looking-at-dependency-p))
  (forward-char)
  (prog1
      (buffer-substring-no-properties
       (point)
       (cljr--point-after '(re-search-forward "\\s-") 'backward-char))
    (backward-char)
    (clojure-delete-and-extract-sexp)
    (delete-region (point-at-bol) (point-at-eol))
    (forward-line)
    (join-line)))

(defun cljr--empty-buffer-p (&optional buffer)
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (string-blank-p (string-trim (buffer-substring-no-properties (point-min) (point-max)))))))

(defun cljr--extract-next-dependency-name ()
  (while (not (or (cljr--empty-buffer-p)
                  (cljr--looking-at-dependency-p)))
    (delete-char 1))
  (when (cljr--looking-at-dependency-p)
    (cljr--extract-dependency-name)))

(defun cljr--get-sorted-dependency-names (deps)
  "Strips metadata and comments"
  (with-temp-buffer
    (let ((names (list)))
      (insert (thread-last deps (string-remove-prefix "[") (string-remove-suffix "]")))
      (goto-char (point-min))
      (while (not (cljr--empty-buffer-p))
        (push (cljr--extract-next-dependency-name) names))
      (string-join (seq-sort #'string< names) "\n "))))

(defun cljr--prepare-sort-buffer (sorted-names vectors-and-meta dividing-line)
  (insert sorted-names)
  (goto-char (point-max))
  (open-line 1)
  (forward-line)
  (insert dividing-line)
  (open-line 1)
  (forward-line)
  (insert vectors-and-meta))

(defun cljr--sort-dependency-vectors-with-meta-and-comments (dividing-line)
  ;;; The buffer looks like this:
  ;;; foo/bar
  ;;; <dividing-line>
  ;;; ^:src-dep [foo/bar "0.1.1"]

  ;;; Until we cross the dividing line, take a sorted line, find
  ;;; its equal in the raw content below and move that vector with meta and
  ;;; comments to the end of the buffer
  (goto-char (point-min))
  (while (not (looking-at dividing-line))
    (let ((dep (string-trim (cljr--extract-region (point) (point-at-eol))))
          start end vector-and-meta)
      (forward-line)
      (join-line)
      (re-search-forward dividing-line)
      (re-search-forward (concat "\\[" dep "\\s-+\""))
      (paredit-backward-up 2)
      (while (not (looking-back "^\\s-*" (point-at-bol)))
        (forward-char -1))
      (while (save-excursion (forward-line -1) (cljr--comment-line-p))
        (forward-line -1))
      (setq start (point))
      (re-search-forward (concat "\\[" dep "\\s-+\""))
      (setq end (max (point-at-eol)
                     (cljr--point-after
                      '(paredit-forward-up 2) '(move-end-of-line 1))))
      (setq vector-and-meta (buffer-substring-no-properties start end))
      (delete-region start end)
      (forward-line)
      (join-line)
      (goto-char (point-max))
      (open-line 1)
      (forward-line)
      (insert vector-and-meta)
      (goto-char (point-min))))
  (cljr--delete-line))

(defun cljr--sort-dependency-vectors (sorted-names vectors-and-meta)
  (with-temp-buffer
    (let ((dividing-line "<===============================>"))
      (cljr--prepare-sort-buffer sorted-names vectors-and-meta dividing-line)
      (cljr--sort-dependency-vectors-with-meta-and-comments dividing-line)
      (concat "[" (string-trim (buffer-substring-no-properties (point) (point-max))) "]"))))

;;;###autoload
(defun cljr-sort-project-dependencies ()
  "Sorts all dependency vectors in project.clj

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-sort-project-dependencies"
  (interactive)
  (let ((project-file (cljr--project-file)))
    (if (cljr--project-with-deps-p project-file)
        (user-error "Dependencies sorting not supported in deps.edn yet.")
      (cljr--update-file project-file
        (goto-char (point-min))
        (while (re-search-forward ":dependencies" (point-max) t)
          ;; Boot has quoted vectors, leiningen does not
          (while (not (looking-at-p "\\["))
            (forward-char))
          (thread-first (buffer-substring-no-properties (point)
                                                        (cljr--point-after 'paredit-forward))
            cljr--get-sorted-dependency-names
            (cljr--sort-dependency-vectors (thread-last (clojure-delete-and-extract-sexp)
                                             (string-remove-prefix "[")
                                             (string-remove-suffix "]")))
            insert))
        (indent-region (point-min) (point-max))
        (save-buffer)))))

(defun cljr--call-middleware-sync (request &optional key)
  "Call the middleware with REQUEST.

If it's present KEY indicates the key to extract from the response."
  (let* ((nrepl-sync-request-timeout (if cljr-warn-on-eval nil 25))
         (response (thread-first request cider-nrepl-send-sync-request cljr--maybe-rethrow-error)))
    (if key
        (nrepl-dict-get response key)
      response)))

(defun cljr--call-middleware-async (request &optional callback)
  (cider-nrepl-send-request request callback))

(defun cljr--get-artifacts-from-middleware (force)
  (message "Retrieving list of available libraries...")
  (let* ((request (cljr--create-msg "artifact-list" "force" (if force "true" "false")))
         (artifacts (cljr--call-middleware-sync request "artifacts")))
    (if artifacts
        artifacts
      (user-error "Empty artifact list received from middleware!"))))

(defun cljr--update-artifact-cache ()
  (cljr--call-middleware-async (cljr--create-msg "artifact-list"
                                                 "force" "true")
                               (lambda (_)
                                 (when cljr--debug-mode
                                   (message "Artifact cache updated")))))

(defun cljr--init-artifact-cache ()
  (cljr--call-middleware-async (cljr--create-msg "artifact-list"
                                                 "force" "false")
                               (lambda (_)
                                 (when cljr--debug-mode
                                   (message "Artifact cache updated")))))

(defun cljr--dictionary-lessp (str1 str2)
  "return t if STR1 is < STR2 when doing a dictionary compare
(splitting the string at numbers and doing numeric compare with them).
It is optimized for version comparisons, in that empty strings are sorted
before non-empty. This lets 1.7.0 be sorted above 1.7.0-RC1."
  (let ((str1-components (cljr--dict-split (downcase str1)))
        (str2-components (cljr--dict-split (downcase str2))))
    (cljr--dict-lessp str1-components str2-components)))

(defun cljr--dict-lessp (slist1 slist2)
  "compare the two lists of strings & numbers"
  (cond ((null slist1)
         (not (null slist2)))
        ((null slist2)
         t)
        ((and (numberp (car slist1))
              (stringp (car slist2)))
         t)
        ((and (numberp (car slist2))
              (stringp (car slist1)))
         nil)
        ((and (numberp (car slist1))
              (numberp (car slist2)))
         (or (< (car slist1) (car slist2))
             (and (= (car slist1) (car slist2))
                  (cljr--dict-lessp (cdr slist1) (cdr slist2)))))
        (t
         (or (string-lessp (car slist1) (car slist2))
             (and (string-equal (car slist1) (car slist2))
                  (cljr--dict-lessp (cdr slist1) (cdr slist2)))))))

(defun cljr--dict-split (str)
  "split a string into a list of number and non-number components"
  (save-match-data
    (let ((res nil))
      (while (and str (not (string-equal "" str)))
        (let ((p (string-match "[0-9]+" str)))
          (cond ((null p)
                 (setq res (cons str res))
                 (setq str nil))
                ((= p 0)
                 (setq res (cons (string-to-number (match-string 0 str)) res))
                 (setq str (substring str (match-end 0))))
                (t
                 (setq res (cons (substring str 0 (match-beginning 0)) res))
                 (setq str (substring str (match-beginning 0)))))))
      (reverse res))))

(defun cljr--get-versions-from-middleware (artifact)
  (let* ((request (cljr--create-msg "artifact-versions" "artifact" artifact))
         (versions (nreverse (sort (cljr--call-middleware-sync request "versions") 'cljr--dictionary-lessp))))
    (if versions
        versions
      (error "Empty version list received from middleware!"))))

(defun cljr--prompt-user-for (prompt &optional choices)
  "Prompt the user with PROMPT.

If CHOICES is provided provide a completed read among the
possible choices. If the choice is trivial, return it."
  (if choices
      (if (= (length choices) 1)
          (cl-first choices)
        (completing-read prompt choices nil nil nil nil (car choices)))
    (read-from-minibuffer prompt)))

(defun cljr--insert-into-leiningen-dependencies (artifact version)
  (re-search-forward ":dependencies")
  (paredit-forward)
  (paredit-backward-down)
  (newline-and-indent)
  (insert "[" artifact " \"" version "\"]"))

(defun cljr--insert-into-clj-dependencies (artifact version)
  (re-search-forward ":deps")
  (forward-sexp)
  (backward-char)
  (newline-and-indent)
  (let ((artifact (if (string-match-p "^.+/.+$" artifact)
                      artifact
                    (format "%s/%s" artifact artifact))))
    (insert artifact " {:mvn/version \"" version "\"}")))


(defun cljr--add-project-dependency (artifact version)
  (let* ((project-file (cljr--project-file))
         (deps (cljr--project-with-deps-p project-file)))
    (cljr--update-file project-file
      (goto-char (point-min))
      (if deps
          (cljr--insert-into-clj-dependencies artifact version)
        (cljr--insert-into-leiningen-dependencies artifact version))
      (cljr--post-command-message "Added %s version %s as a project dependency" artifact version)
      (when cljr-hotload-dependencies
        (if deps
            (back-to-indentation)
          (paredit-backward-down))
        (cljr-hotload-dependency)))))

;;;###autoload
(defun cljr-add-project-dependency (force)
  "Add a dependency to the project.clj file.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-add-project-dependency"
  (interactive "P")
  (cljr--ensure-op-supported "artifact-list")
  (when-let ((lib-name (thread-last (cljr--get-artifacts-from-middleware force)
                         (cljr--prompt-user-for "Artifact: ")))
             (version (thread-last (cljr--get-versions-from-middleware lib-name)
                        (cljr--prompt-user-for "Version: "))))
    (cljr--add-project-dependency lib-name version)))

;;;###autoload
(defun cljr-update-project-dependency (&optional version)
  "Update the version of the dependency at point."
  (interactive)
  (cljr--ensure-op-supported "artifact-list")
  (unless (cljr--looking-at-dependency-p)
    (user-error "Place cursor in front of dependency to update."))
  (save-excursion
    (let (lib-name
          (lein-style (cljr--vector-at-point-p)))
      (if lein-style
          (paredit-forward-down))
      (setq lib-name (cljr--extract-sexp))
      (paredit-forward)
      (skip-syntax-forward " ")
      (let ((artifact-version (or version
                                  (thread-last (cljr--get-versions-from-middleware lib-name)
                                    (cljr--prompt-user-for (concat lib-name " version: "))))))
        (cljr--delete-sexp)
        (if lein-style
            (insert "\"" artifact-version "\"")
          (insert "\{:mvn/version \"" artifact-version "\"\}")))))
  (when cljr-hotload-dependencies
    (cljr-hotload-dependency)
    (cljr--ensure-op-supported "artifact-list")))

;;;###autoload
(defun cljr-update-project-dependencies ()
  "Update all project dependencies.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-update-project-dependencies"
  (interactive)
  (cljr--ensure-op-supported "artifact-list")
  (let ((project-file (cljr--project-file)))
    (find-file project-file)
    (goto-char (point-min))
    (let (cljr-hotload-dependencies)
      (if (cljr--project-with-deps-p project-file)
          (cljr--update-dependencies ":deps" "}" 2)
        (cljr--update-dependencies ":dependencies" "]" 1)))))

(defun cljr--update-dependencies (keyword dependency-closing-brace forward-count)
  (while (re-search-forward keyword (point-max) t)
    (paredit-forward-down)
    (cljr--skip-past-whitespace-and-comments)
    (while (not (looking-at dependency-closing-brace))
      (let ((highlight (cljr--highlight-sexp)))
        (unwind-protect
            (cljr-update-project-dependency)
          (delete-overlay highlight)))
      (paredit-forward forward-count)
      (cljr--skip-past-whitespace-and-comments))))

(defun cljr--skip-past-whitespace-and-comments ()
  (skip-syntax-forward " >")
  (while (looking-at ";")
    (move-end-of-line 1)
    (forward-char)
    (skip-syntax-forward " >")))

(defun cljr--extract-anon-fn-name (sexp-str)
  (when (string-match "(fn \\(\\_<[^ ]+\\_>\\)?" sexp-str)
    (match-string-no-properties 1 sexp-str)))

(defun cljr--highlight (start end)
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'secondary-selection)
    (overlay-put overlay 'priority 100)
    overlay))

(defun cljr--highlight-sexp ()
  (cljr--highlight (point) (cljr--point-after 'paredit-forward)))

(defun cljr--string-present-p (s)
  (not (or (null s) (string-empty-p s))))

(defun cljr--promote-fn ()
  (save-excursion
    (let* ((locals (save-excursion (paredit-forward-down)
                                   (cljr--call-middleware-to-find-used-locals
                                    (buffer-file-name) (line-number-at-pos)
                                    (1+ (current-column)))))
           (fn (cljr--extract-sexp))
           (namedp (cljr--extract-anon-fn-name fn))
           (name (or namedp
                     (unless (cljr--use-multiple-cursors-p)
                       (let ((highlight (cljr--highlight-sexp)))
                         (unwind-protect
                             (read-string "Name: ")
                           (delete-overlay highlight))))))
           fn-start)
      (cljr--delete-sexp)
      (save-excursion
        (cljr--new-toplevel-form fn)
        (paredit-backward-down)
        (cljr--goto-fn-definition)
        (setq fn-start (point))
        (forward-char)
        (insert "de")
        (paredit-forward)
        (when cljr-favor-private-functions
          (if clojure-use-metadata-for-privacy
              (insert " ^:private")
            (insert "-")))
        (when (not namedp)
          (insert " ")
          (newline)
          (backward-char)
          (if name
              (insert name)
            (mc/create-fake-cursor-at-point)))
        (re-search-forward "\\[")
        (when (cljr--string-present-p locals)
          (insert locals)
          (unless (looking-at-p "\\]")
            (insert " ")))
        (paredit-forward-up)
        (unless (looking-at "\s*?$")
          (newline))
        (indent-region fn-start (cljr--point-after 'paredit-forward-up)))
      (when (cljr--string-present-p locals)
        (insert (format "(partial  %s)" locals))
        (backward-char (length (concat " " locals ")"))))
      (if name
          (insert name)
        (mc/maybe-multiple-cursors-mode)))))

(defun cljr--append-fn-parameter (param)
  (cljr--goto-fn-definition)
  (paredit-forward-down)
  (paredit-forward 2)
  (paredit-backward-down)
  (if (looking-back "\\[" 1)
      (insert param)
    (insert " " param)))

(defun cljr--promote-function-literal ()
  (delete-char 1)
  (let ((body (clojure-delete-and-extract-sexp)))
    (insert "(fn [] " body ")"))
  (backward-char)
  (cljr--goto-fn-definition)
  (let ((fn-start (point))
        var replacement)
    (while (re-search-forward "%[1-9&]?" (cljr--point-after 'paredit-forward) t)
      (setq var (buffer-substring (point) (cljr--point-after 'paredit-backward)))
      (setq replacement (read-string (format "%s => " var)))
      (cljr--append-fn-parameter (if (string= "%&" var)
                                     (format "& %s" replacement)
                                   replacement))
      (goto-char (1+ fn-start))
      (let ((end (cljr--point-after '(paredit-forward-up 2))))
        (while (re-search-forward (format "\\s-%s\\(\\s-\\|\\|\n)\\)" var)
                                  end :no-error)
          (replace-match (format " %s\\1" replacement))))
      (goto-char fn-start))))

;;;###autoload
(defun cljr-promote-function (promote-to-defn)
  "Promote a function literal to an fn, or an fn to a defn.
With prefix PROMOTE-TO-DEFN, promote to a defn even if it is a
function literal.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-promote-function"
  (interactive "P")
  (cljr--ensure-op-supported "find-used-locals")
  (when (cljr--asts-y-or-n-p)
    (save-excursion
      (cond
       ;; Already in the right place.
       ((or (looking-at-p "#(")
            (looking-at-p "(fn")))
       ;; Right after the #.
       ((and (eq (char-after) ?\()
             (eq (char-before) ?#))
        (forward-char -1))
       ;; Possibly inside a function.
       (t (cljr--goto-fn-definition)))
      ;; Now promote it.
      (if (looking-at "#(")
          (cljr--promote-function-literal)
        (cljr--promote-fn)))
    (when promote-to-defn
      (cljr--promote-fn))))

(add-to-list 'mc--default-cmds-to-run-once 'cljr-promote-function)

(defun cljr--insert-in-find-symbol-buffer (occurrence)
  (save-excursion
    (pop-to-buffer cljr--find-symbol-buffer)
    (goto-char (point-max))
    (insert occurrence)))

(defun cljr--end-of-buffer-p ()
  "True if point is at end of buffer"
  (= (point) (cljr--point-after 'end-of-buffer)))

(defun cljr--find-symbol-sync (symbol ns)
  (let* ((filename (funcall cider-to-nrepl-filename-function (buffer-file-name)))
         (line (line-number-at-pos))
         (column (1+ (current-column)))
         (dir (funcall cider-to-nrepl-filename-function (cljr--project-dir)))
         (request (cljr--create-msg "find-symbol"
                                    "ns" ns
                                    "dir" dir
                                    "file" filename
                                    "line" line
                                    "column" column
                                    "name" symbol
                                    "ignore-errors"
                                    (when cljr-ignore-analyzer-errors "true")))
         occurrences)
    (with-temp-buffer
      (insert (cljr--call-middleware-sync request "occurrence"))
      (unless (cljr--empty-buffer-p)
        (goto-char (point-min))
        (while (not (cljr--end-of-buffer-p))
          (dolist (edn (parseedn-read))
            (push edn occurrences)))))
    occurrences))

(defun cljr--find-symbol (symbol ns callback)
  (let* ((filename (funcall cider-to-nrepl-filename-function (buffer-file-name)))
         (line (line-number-at-pos))
         (column (1+ (current-column)))
         (dir (funcall cider-to-nrepl-filename-function (cljr--project-dir)))
         (find-symbol-request
          (cljr--create-msg "find-symbol"
                            "ns" ns
                            "dir" dir
                            "file" filename
                            "line" line
                            "column" column
                            "name" symbol
                            "ignore-paths" cljr-middleware-ignored-paths
                            "ignore-errors"
                            (when cljr-ignore-analyzer-errors "true"))))
    (with-current-buffer (with-no-warnings (cider-current-repl))
      (setq cjr--occurrence-count 0)
      (setq cljr--num-syms -1)
      (setq cljr--occurrence-ids '()))
    (cljr--call-middleware-async find-symbol-request callback)))

(defun cljr--first-line (s)
  (thread-first s (split-string "\\(\r\n\\|[\n\r]\\)") car string-trim))

(defun cljr--project-relative-path (path)
  "Denormalize PATH to make it relative to the project root."
  (string-remove-prefix (cljr--project-dir) path))

(defun cljr--get-valid-filename (hash)
  "Get :file value from the hash table and convert path if necessary."
  (funcall cider-from-nrepl-filename-function (gethash :file hash)))

(defun cljr--format-symbol-occurrence (occurrence)
  (let ((file (cljr--get-valid-filename occurrence))
        (line (gethash :line-beg occurrence))
        (col (1- (gethash :col-beg occurrence)))
        (match (gethash :match occurrence)))
    (format "%s:%s:%s: %s\n" (cljr--project-relative-path file) line col
            (cljr--first-line match))))

(defun cljr--format-and-insert-symbol-occurrence (occurrence-resp)
  ;; The middleware sends either an occurrence or a final count, never
  ;; both in the same message.
  (cljr--maybe-rethrow-error occurrence-resp)
  (if-let (count (nrepl-dict-get occurrence-resp "count"))
      (progn
        (setq cljr--num-syms count)
        (when (= cjr--occurrence-count cljr--num-syms)
          (cljr--finalise-find-symbol-buffer cljr--num-syms)))
    (when-let (occurrence-data (nrepl-dict-get occurrence-resp "occurrence"))
      (let* ((occurrence (parseedn-read-str occurrence-data))
             (occurrence-id (format "%s%s"
                                    (cljr--get-valid-filename occurrence)
                                    (gethash :line-beg occurrence))))
        (cl-incf cjr--occurrence-count)
        (unless (member occurrence-id cljr--occurrence-ids)
          (setq cljr--occurrence-ids
                (cons occurrence-id cljr--occurrence-ids))
          (thread-last occurrence
            cljr--format-symbol-occurrence
            cljr--insert-in-find-symbol-buffer))))))

(defun cljr--finalise-find-symbol-buffer (total)
  (with-current-buffer "*cljr-find-usages*"
    (goto-char (point-max))
    (insert (format "\nFind symbol finished: %d occurrence%s found"
                    total (if (> total 1) "s" "")))
    ;; Place point on first occurrence
    (goto-char (point-min))
    (forward-line 2)))

(defun cljr--setup-find-symbol-buffer (symbol-name)
  (save-window-excursion
    (when (get-buffer cljr--find-symbol-buffer)
      (kill-buffer cljr--find-symbol-buffer))
    (pop-to-buffer cljr--find-symbol-buffer)
    (with-current-buffer "*cljr-find-usages*"
      (insert (format "'%s' occurs in the following places:\n\n" symbol-name))
      (grep-mode)
      (set (make-local-variable 'compilation-error-regexp-alist)
           (list 'compilation-cljr-nogroup))
      (set (make-local-variable 'compilation-error-regexp-alist-alist)
           (list (cons 'compilation-cljr-nogroup (list cljr--file-column-pattern 1 2 3))))
      (setq buffer-read-only nil)
      (setq-local compilation-search-path (list (cljr--project-dir))))))

(defun cljr--asts-y-or-n-p ()
  (or
   (not cljr-warn-on-eval)
   (y-or-n-p "To perform this op the project needs to be evaluated.\n  Analyzing a large project might take a while: hit C-g to abort.\n  (Set cljr-warn-on-eval to nil to analyze the project without warning)\n  Do you want to proceed?")))

;;;###autoload
(defun cljr-find-usages ()
  "Find all usages of the symbol at point in the project.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-find-usages"
  (interactive)
  (cljr--ensure-op-supported "find-symbol")
  (when (cljr--asts-y-or-n-p)
    (save-buffer)
    (let* ((cljr--find-symbol-buffer "*cljr-find-usages*")
           (symbol (cider-symbol-at-point))
           (var-info (cljr--var-info symbol))
           (ns (nrepl-dict-get var-info "ns"))
           (symbol-name (nrepl-dict-get var-info "name")))
      (cljr--setup-find-symbol-buffer (or symbol-name symbol))
      (cljr--find-symbol (or symbol-name symbol) ns
                         #'cljr--format-and-insert-symbol-occurrence))))

(defun cljr--rename-occurrence (file line-beg col-beg line-end col-end name new-name)
  (save-excursion
    (with-current-buffer
        (find-file-noselect file)
      (let* ((name (thread-last name cljr--symbol-suffix regexp-quote))
             (search-limit (save-excursion
                             (progn
                               (goto-char (point-min))
                               (forward-line (1- line-end))
                               (move-to-column (1- col-end))
                               (point)))))
        (goto-char (point-min))
        (forward-line (1- line-beg))
        (move-to-column (1- col-beg))
        ;; When the match is a definition, the position of the symbol
        ;; isn't returned but the beginning of the defining form
        (when (looking-at-p "(\\s-*def")
          (re-search-forward name)
          (paredit-backward))
        (when (re-search-forward (format "\\(.+/\\)?\\(%s\\)" name) search-limit t)
          (replace-match (format "\\1%s" new-name) t)))
      (save-buffer))))

(defun cljr--rename-occurrences (occurrences new-name)
  (dolist (symbol-meta occurrences)
    (let* ((file (cljr--get-valid-filename symbol-meta))
           (line-beg (gethash :line-beg symbol-meta))
           (col-beg (gethash :col-beg symbol-meta))
           (line-end (gethash :line-end symbol-meta))
           (col-end (gethash :col-end symbol-meta))
           (name (gethash :name symbol-meta))
           (new-name* (if (stringp new-name)
                          new-name
                        (funcall new-name name))))
      (cljr--rename-occurrence file line-beg col-beg line-end col-end name new-name*))))

;;;###autoload
(defun cljr-rename-symbol (&optional new-name)
  "Rename the symbol at point and all of its occurrences.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-rename-symbol"
  (interactive)
  (cljr--ensure-op-supported "find-symbol")
  (when (cljr--asts-y-or-n-p)
    (save-buffer)
    (let* ((symbol (cider-symbol-at-point))
           (var-info (cljr--var-info symbol))
           (symbol-name (nrepl-dict-get var-info "name"))
           (ns (nrepl-dict-get var-info "ns"))
           (name (or symbol-name symbol))
           (_ (unless *cljr--noninteractive* (message "Fetching symbol occurrences...")))
           (occurrences (cljr--find-symbol-sync name ns))
           (new-name (or new-name (read-from-minibuffer "New name: "
                                                        (cljr--symbol-suffix symbol)))))
      (cljr--rename-occurrences occurrences new-name)
      (cljr--post-command-message "Renamed %s occurrences of %s" (length occurrences) name)
      (when (and (> (length occurrences) 0) (not cljr-warn-on-eval))
        (cljr--warm-ast-cache)))))

(defun cljr--replace-refer-all-with-alias (ns publics-occurrences alias)
  (let ((ns-last-token (car (last (split-string ns "\\.")))))
    (when (re-search-forward (format "\\(%s\\).*?\\([\]\)]\\)" ns-last-token) nil t)
      (replace-match (format "\\1 :as %s\\2" alias)))
    (perform-replace (format "%s/" alias) "" nil nil t)
    (cljr--rename-occurrences publics-occurrences (lambda (old-name) (concat alias "/" old-name)))))

(defun cljr--replace-refer-all (ns alias)
  "Replaces :refer :all style require with alias :as style require.

Also adds the alias prefix to all occurrences of public symbols in the namespace.
"
  (cljr--ensure-op-supported "find-used-publics")
  (let ((filename (funcall cider-to-nrepl-filename-function (buffer-file-name))))
    (let* ((alias (or alias
                      (cljr--prompt-user-for (format "alias for [%s]: " ns))))
           (request
            (cljr--create-msg "find-used-publics"
                              "used-ns" ns
                              "file" filename))
           (occurrences (thread-last (cljr--call-middleware-sync request "used-publics")
                          (parseedn-read-str))))
      (cljr--replace-refer-all-with-alias ns occurrences alias))))

(defun cljr--maybe-nses-in-bad-state (response)
  (let ((asts-in-bad-state (seq-filter
                            (lambda (it)
                              (not (stringp (car (last it)))))
                            (thread-first (nrepl-dict-get response "ast-statuses")
                              parseedn-read-str
                              (seq-partition 2)))))
    (when (not (= 0 (length asts-in-bad-state)))
      (user-error (concat "Some namespaces are in a bad state: "
                          (string-join
                           (seq-map
                            (lambda (it)
                              (format "error \"%s\" in %s" (car (last (car (last it)))) (car it)))
                            asts-in-bad-state) "; "))))))

(defun cljr--warm-ast-cache ()
  (run-hooks 'cljr-before-warming-ast-cache-hook)
  (cljr--call-middleware-async
   (cljr--create-msg "warm-ast-cache"
                     "ignore-paths" cljr-middleware-ignored-paths)
   (lambda (res)
     (run-hook-with-args 'cljr-after-warming-ast-cache-hook res)
     (cljr--maybe-rethrow-error res)
     (cljr--maybe-nses-in-bad-state res)
     (when cljr--debug-mode
       (message "AST index updated")))))

(defun cljr--replace-ns (new-ns)
  (save-excursion
    (cljr--goto-ns)
    (clojure-delete-and-extract-sexp)
    (insert new-ns)
    (cljr--just-one-blank-line)))

(defun cljr--clean-ns (&optional path no-prune?)
  "If PATH is passed use that instead of the path to the current buffer

If NO-PRUNE is passed, the default is overridden and unused stuff isn't \
removed."
  ;; Don't save prematurely when called from `cljr-project-clean'
  (unless (and *cljr--noninteractive*
               (not (buffer-modified-p)))
    (save-buffer))
  (let* ((path (funcall cider-to-nrepl-filename-function (or path (buffer-file-name))))
        (relative-path (cljr--project-relative-path path)))
    (when-let (new-ns (cljr--call-middleware-sync
                       (cljr--create-msg "clean-ns"
                                         "path" path
                                         "relative-path" relative-path
                                         "libspec-whitelist" cljr-libspec-whitelist
                                         "print-right-margin" cljr-print-right-margin
                                         "print-miser-width" cljr-print-miser-width
                                         "prune-ns-form" (if no-prune? "false"
                                                           "true"))
                       "ns"))
      (cljr--replace-ns new-ns))
    (unless *cljr--noninteractive*
      (cljr--post-command-message "Namespace form cleaned!"))))

;;;###autoload
(defun cljr-clean-ns ()
  "Clean the ns form for the current buffer.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-clean-ns"
  (interactive)
  (cljr--ensure-op-supported "clean-ns")
  (cider-eval-ns-form)
  (cljr--clean-ns))

(defun cljr--narrow-candidates (candidates symbol)
  (if (= (length candidates) 0)
      (error "Couldn't find any symbols matching %s on classpath."
             (cljr--symbol-suffix symbol))
    (let* ((names (seq-map (lambda (c) (gethash :name c)) candidates))
           (name (intern-soft (cljr--prompt-user-for "Require: " names))))
      (seq-find (lambda (c) (equal name (gethash :name c))) candidates))))

(defun cljr--insert-libspec-verbosely (libspec)
  (insert (format "%s" libspec))
  (cljr--indent-defun)
  (cljr--post-command-message "%s added to ns" libspec))

(defun cljr--insert-missing-import (missing)
  (save-excursion
    (cljr--insert-in-ns ":import")
    (cljr--insert-libspec-verbosely missing)))

(defun cljr--qualified-symbol-p (symbol)
  (thread-last symbol
    (format "%s")
    regexp-quote
    (string-match-p "/")
    null
    not))

(defun cljr--symbol-prefix (symbol)
  "java.util.Date => java.util
str/split => str
split => ''"
  (cond ((cljr--qualified-symbol-p symbol) (car (split-string symbol "/")))
        ((string-match-p "\\w+\\.\\w+" symbol)
         (string-join (butlast (split-string symbol "\\.")) "."))
        (t "")))

(defun cljr--insert-missing-require (symbol missing-symbol type)
  "Require MISSING-SYMBOL.

Inspect SYMBOL, the thing at point, to find out whether we have
to create an alias or refer."
  (save-excursion
    (cljr--insert-in-ns ":require")
    (let ((missing (format "%s" missing-symbol))
          (alias? (cljr--qualified-symbol-p symbol)))
      (cond
       ;;  defrecord / deftype where the package must be required
       ((eq type :type)
        (cljr--insert-libspec-verbosely (cljr--symbol-prefix missing)))
       ;; Fully qualified symbol
       ((and (cljr--qualified-symbol-p symbol)
             (string= (cljr--symbol-prefix symbol) missing))
        (cljr--insert-libspec-verbosely missing))
       (alias?
        (cljr--insert-libspec-verbosely (format "[%s :as %s]" missing
                                                (cljr--symbol-prefix symbol))))
       (t (cljr--insert-libspec-verbosely (format "[%s :refer [%s]]"
                                                  missing symbol)))))))

(defun cljr--add-missing-libspec (symbol candidates)
  (let* ((candidate (cljr--narrow-candidates candidates symbol))
         (missing-symbol (gethash :name candidate))
         (type (gethash :type candidate)))
    (cond ((eq type :ns) (cljr--insert-missing-require symbol missing-symbol type))
          ((eq type :type)
           ;; We need to both require the ns, to trigger compilation,
           ;; and then import the java class

           ;; In the line below we're assuming that all clojure code
           ;; will prefer - over _ when naming namespaces :(
           (progn (cljr--insert-missing-require
                   symbol
                   (replace-regexp-in-string "_" "-" (format "%s" missing-symbol)) type)
                  (cljr--insert-missing-import missing-symbol)))
          ((eq type :class) (cljr--insert-missing-import missing-symbol))
          (t (error (format "Unknown type %s" type))))))

(defun cljr--symbol-suffix (symbol)
  "java.util.Date => Date
Date => Date
clojure.string/split => split
str/split => split"
  (let ((name (cljr--normalize-symbol-name symbol)))
    (cond
     ((string-match-p "\\w+\\.\\w+" name)
      (thread-first name (split-string "\\.") last car cljr--symbol-suffix))
     ((cljr--qualified-symbol-p name)
      (thread-first name (split-string "/") cadr cljr--symbol-suffix))
     (t name))))

(defun cljr--normalize-symbol-name (name)
  "Removes reader macros and quoting

Date. -> Date
@sym => sym
#'sym => sym
'sym => sym
~sym => sym
~@sym => sym"
  (cond
   ((string-suffix-p "." name)
    (thread-last name (string-remove-suffix ".") cljr--normalize-symbol-name))
   ((string-prefix-p "#'" name)
    (thread-last name (string-remove-prefix "#'") cljr--normalize-symbol-name))
   ((string-prefix-p "'" name)
    (thread-last name (string-remove-prefix "'") cljr--normalize-symbol-name))
   ((string-prefix-p "~" name)
    (thread-last name (string-remove-prefix "~") cljr--normalize-symbol-name))
   ((string-prefix-p "~@" name)
    (thread-last name (string-remove-prefix "~@") cljr--normalize-symbol-name))
   ((string-prefix-p "@" name)
    (thread-last name (string-remove-prefix "@") cljr--normalize-symbol-name))
   (t name)))

(defun cljr--call-middleware-to-resolve-missing (symbol)
  ;; Just so this part can be mocked out in a step definition
  (when-let (candidates (thread-first (cljr--create-msg "resolve-missing"
                                                        "symbol" symbol
                                                        "session"
                                                        (with-no-warnings (cider-nrepl-eval-session)))
                          (cljr--call-middleware-sync
                           "candidates")))
    (parseedn-read-str candidates)))

(defun cljr--get-error-value (response)
  "Gets the error value from the middleware response.

We can't simply call `nrepl-dict-get' because the error value
itself might be `nil'."
  (cl-assert (nrepl-dict-p response) nil
             "Response from middleware isn't an nrepl-dict!")
  (if-let (err (nrepl-dict-get response "err"))
      (error (format "Error in nrepl-refactor: %s" err))
    (let* ((maybe-error-and-rest
            (seq-drop-while (lambda (e)
                              (not (and (stringp e) (string-equal e "error"))))
                            response))
           (maybe-error (car maybe-error-and-rest)))
      (when (and (stringp maybe-error) (string-equal maybe-error "error"))
        (or (cadr maybe-error-and-rest)
            (format "Error 'nil' returned from middleware. %s"
                    "Please contact your local administrator."))))))

(defun cljr--format-escape (msg)
  "Make the message consumable by format."
  (replace-regexp-in-string "%" "%%" msg))

(defun cljr--maybe-rethrow-error (response)
  (if-let (err (cljr--get-error-value response))
      (error (cljr--format-escape err))
    response))

(defun cljr--maybe-eval-ns-form ()
  (when (and cljr-auto-eval-ns-form (cider-connected-p))
    (cider-eval-ns-form)))

;;;###autoload
(defun cljr-add-missing-libspec ()
  "Requires or imports the symbol at point.

If the symbol at point is of the form str/join then the ns
containing join will be aliased to str.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-add-missing-libspec"
  (interactive)
  (cljr--ensure-op-supported "resolve-missing")
  (let* ((symbol (cider-symbol-at-point))
         (candidates (cljr--call-middleware-to-resolve-missing symbol)))
    (if (and candidates (< 0 (length candidates)))
        (cljr--add-missing-libspec symbol candidates)
      (cljr--post-command-message "Can't find %s on classpath" (cljr--symbol-suffix symbol))))
  (cljr--maybe-clean-ns)
  (cljr--maybe-eval-ns-form))

(defun cljr--dependency-at-point ()
  "Returns project dependency at point.

Recognizes both leiningen- and deps.edn-style dependencies, but the latter is always
transformed back to leiningen dependency vector which is what nrepl backend
expects for hot-loading."
  (save-excursion
    (ignore-errors
      (while (not (cljr--looking-at-dependency-p))
        (paredit-backward-up))

      (if (cljr--vector-at-point-p)
          (buffer-substring-no-properties (point)
                                          (cljr--point-after 'paredit-forward))
        (concat "["
                (match-string-no-properties 1)
                " "
                (match-string-no-properties 4)
                "]")))))

;;;###autoload
(defun cljr-hotload-dependency ()
  "Download a dependency (if needed) and hotload it into the current repl session.

Defaults to the dependency vector at point, but prompts if none is found.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-hotload-dependency"
  (interactive)
  (user-error "Temporarily disabled due to changes introduced with Java 9. See https://github.com/clojure-emacs/refactor-nrepl/pull/301"))

(defun cljr--defn-str (&optional public)
  (if public
      "(defn "
    (concat "(defn"
            (if cljr-favor-private-functions
                (if clojure-use-metadata-for-privacy
                    " ^:private "
                  "- ")
              " "))))

(defun cljr--call-middleware-to-find-used-locals (file line column)
  (let ((file (funcall cider-to-nrepl-filename-function file)))
    (string-join
     (cljr--call-middleware-sync
      (cljr--create-msg "find-used-locals" "file" file "line" line
                        "column" column)
      "used-locals") " ")))

(defun cljr--goto-enclosing-sexp ()
  (let ((sexp-regexp (rx (or "(" "#{" "{" "["))))
    (unless (looking-at sexp-regexp)
      (paredit-backward-up))
    (when (looking-back "#" 1)
      (forward-char -1))))

;;;###autoload
(defun cljr-extract-function ()
  "Extract the form at (or above) point as a top-level defn.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-extract-function"
  (interactive)
  (cljr--ensure-op-supported "find-used-locals")
  (when (cljr--asts-y-or-n-p)
    (save-buffer)
    (cljr--goto-enclosing-sexp)
    (let* ((unbound (cljr--call-middleware-to-find-used-locals
                     (buffer-file-name) (line-number-at-pos)
                     ;; +1 because the middleware expects indexing from 1
                     ;; +1 more because point has to be inside the sexp,
                     ;; not on the opening paren
                     (+ (current-column) 2)))
           (name (unless (cljr--use-multiple-cursors-p)
                   (let ((highlight (cljr--highlight-sexp)))
                     (unwind-protect
                         (cljr--prompt-user-for "Name: ")
                       (delete-overlay highlight)))))
           (body (clojure-delete-and-extract-sexp)))
      (save-excursion
        (cljr--make-room-for-toplevel-form)
        (insert (cljr--defn-str))
        (if name
            (insert name)
          (mc/create-fake-cursor-at-point))
        (newline)
        (indent-according-to-mode)
        (insert "[" unbound "]")
        (newline-and-indent)
        (insert body ")"))
      (insert "(")
      (when name (insert name))
      (save-excursion
        (unless (string-blank-p unbound)
          (insert " " unbound))
        (insert ")"))
      (unless name
        (mc/maybe-multiple-cursors-mode)))))

(add-to-list 'mc--default-cmds-to-run-once 'cljr-extract-function)

(defun cljr--at-end-of-symbol-at-point ()
  (looking-back (regexp-quote (cider-symbol-at-point)) (point-at-bol)))

(defun cljr--insert-function-stubs (functions)
  (unless (cljr--at-end-of-symbol-at-point)
    (paredit-forward))
  (save-excursion
    (dolist (fn functions)
      (newline-and-indent)
      (insert "(" (gethash :name fn) " " (gethash :parameter-list fn) ")")))
  (when (> (length functions) 0)
    ;; Move cursor to point where the first functino body goes
    (paredit-forward-down)
    (paredit-forward 2)))

;;;###autoload
(defun cljr-add-stubs ()
  "Adds implementation stubs for the interface or protocol at point.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-add-stubs"
  (interactive)
  (cljr--ensure-op-supported "stubs-for-interface")
  (let* ((interface (cider-symbol-at-point))
         (prefix? (cljr--symbol-prefix interface))
         (alias? (cljr--resolve-alias prefix?))
         (interface (if (not (string-blank-p prefix?))
                        (if alias?
                            (format "%s/%s" alias? (cljr--symbol-suffix interface))
                          interface)
                      (format "%s/%s" (cider-current-ns) interface)))
         (functions (parseedn-read-str (cljr--call-middleware-sync
                               (cljr--create-msg "stubs-for-interface"
                                                 "interface" interface)
                               "functions"))))
    (cljr--insert-function-stubs functions)))

(defun cljr--delete-definition (definition)
  "Delete a definition as part of inlining a symbol."
  (let ((file (cljr--get-valid-filename definition))
        (line-beg (gethash :line-beg definition))
        (col-beg (gethash :col-beg definition)))
    (with-current-buffer (find-file-noselect file)
      (goto-char (point-min))
      (forward-line (1- line-beg))
      (forward-char (1- col-beg))
      (clojure-delete-and-extract-sexp)
      (when (clojure--inside-let-binding-p)
        (clojure-delete-and-extract-sexp)
        (if (save-excursion (cljr--get-let-bindings))
            (progn
              (while (looking-at-p "\s*\n")
                (forward-line)
                (join-line))
              (when (looking-at-p "]")
                ;; we just deleted the last binding in the vector
                (join-line)))
          (cljr--eliminate-let))
        (cljr--indent-defun))
      (when (looking-at-p "\s*\n")
        (cljr--just-one-blank-line))
      (save-buffer))))

(defun cljr--sort-occurrences (occurrences)
  "Sort the OCCURRENCES so the last ones in the file comes first."
  (seq-sort (lambda (o1 o2)
              (let ((o1-line (gethash :line-beg o1))
                    (o2-line (gethash :line-beg o2))
                    (o1-col (gethash :col-beg o1))
                    (o2-col (gethash :col-beg o2)))
                (cond
                 ((< o1-line o2-line) o2)
                 ((> o1-line o2-line) o1)
                 ((< o1-col o2-col ) o2)
                 ((> o1-col o2-col) o1)
                 (t (error "Sort occurrences failed to compare %s %s %s %s"
                           o1-line o2-line o1-col o2-col)))))
            occurrences))

(defun cljr--inline-fn-at-call-site (def call-site)
  "Point is at a call site, where the sexp call-site has just
  been extracted."
  (let ((args (cl-rest call-site))
        (params (with-temp-buffer
                  (insert def)
                  (goto-char (point-min))
                  (paredit-forward-down 2)
                  (cljr--extract-sexp-as-list)))
        (def (with-temp-buffer
               (insert def)
               (goto-char (point-min))
               (paredit-forward-down 2)
               (paredit-forward-up)
               (paredit-splice-sexp-killing-backward)
               (buffer-string))))
    (dotimes (i (length args))
      (setq def (replace-regexp-in-string (format "\\_<%s\\_>" (nth i params))
                                          (nth i args) def t t)))
    (insert def)))

(defun cljr--inline-symbol (definition occurrences)
  (let* ((def (gethash :definition definition))
         (inline-fn-p (string-prefix-p "(fn" def)))
    (dolist (symbol-meta (cljr--sort-occurrences occurrences))
      (let* ((file (cljr--get-valid-filename symbol-meta))
             (line-beg (gethash :line-beg symbol-meta))
             (col-beg (gethash :col-beg symbol-meta)))
        (with-current-buffer (find-file-noselect file)
          (goto-char (point-min))
          (forward-line (1- line-beg))
          (forward-char (1- col-beg))
          (let* ((call-site-p (and inline-fn-p
                                   (looking-back "(\s*" (point-at-bol))))
                 (sexp (if call-site-p
                           (prog1 (cljr--extract-sexp-as-list)
                             (paredit-backward-up)
                             (clojure-delete-and-extract-sexp))
                         (clojure-delete-and-extract-sexp))))
            (if call-site-p
                (cljr--inline-fn-at-call-site def sexp)
              (insert def)))))))
  (save-buffer)
  (cljr--delete-definition definition))

(defun cljr--var-info (&optional symbol all)
  "Like `cider-var-info' but also handles locally bound vars.

If SYMBOL is passed we assume it's a global var and look it up.

If SYMBOL is nil the symbol at point is used and we consider
locals in that context.

If the symbol is bound locally nil will be returned.

ALL has the same meaning as for `cider-var-info'"
  (if symbol
      (cider-var-info symbol all)
    (let ((used-locals (split-string (cljr--call-middleware-to-find-used-locals
                                      (expand-file-name (buffer-file-name))
                                      (line-number-at-pos) (1+ (current-column))) " "))
          (symbol (cider-symbol-at-point)))
      (unless (member symbol used-locals)
        (cider-var-info symbol all)))))

;;;###autoload
(defun cljr-inline-symbol ()
  "Inline the symbol at point.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-inline-symbol"
  (interactive)
  (cljr--ensure-op-supported "extract-definition")
  (when (cljr--asts-y-or-n-p)
    (save-buffer)
    (save-excursion
      (let* ((filename (funcall cider-to-nrepl-filename-function (buffer-file-name)))
             (line (line-number-at-pos))
             (column (1+ (current-column)))
             (dir (cljr--project-dir))
             (symbol (cider-symbol-at-point))
             (var-info (cljr--var-info))
             (ns (or (nrepl-dict-get var-info "ns") (cider-current-ns)))
             (symbol-name (or (nrepl-dict-get var-info "name") symbol))
             (extract-definition-request (list
                                          "op" "extract-definition"
                                          "ns" ns
                                          "dir" dir
                                          "file" filename
                                          "line" line
                                          "column" column
                                          "name" symbol-name
                                          "ignore-errors"
                                          (when cljr-ignore-analyzer-errors "true")))
             (response (parseedn-read-str (cljr--call-middleware-sync
                                  extract-definition-request "definition")))
             (definition (gethash :definition response))
             (occurrences (gethash :occurrences response)))
        (cljr--inline-symbol definition occurrences)
        (unless *cljr--noninteractive* ; don't spam when called from `cljr-remove-let'
          (if occurrences
              (cljr--post-command-message "Inlined %s occurrence(s) of '%s'" (length occurrences) symbol)
            (cljr--post-command-message "No occurrences of '%s' found.  Deleted the definition." symbol)))))
    (cljr--indent-defun)))

(defun cljr--version (&optional remove-package-version)
  "Get the version of `clj-refactor' from the package header.

if REMOVE-PACKAGE_VERSION is t get rid of the (package: 20150828.1048) suffix."
  (let ((version (replace-regexp-in-string "snapshot" "-SNAPSHOT"
                                           (string-trim (pkg-info-version-info 'clj-refactor)))))
    (if remove-package-version
        (replace-regexp-in-string " (.*)" "" version)
      version)))

;; We used to derive the version out of `(cljr--version t)`,
;; but now prefer a fixed version to fully decouple things and prevent unforeseen behavior.
;; This suits better our current pace of development.
(defcustom cljr-injected-middleware-version "3.1.0"
  "The refactor-nrepl version to be injected.

You can customize this in order to try out new releases.

If customizing it, you most likely should `(setq cljr-suppress-middleware-warnings nil)' too,
for avoiding a warning that would be irrelevant for this case."
  :group 'cljr
  :type 'string
  :safe #'stringp
  :package-version "3.0.0")

(defun cljr--middleware-version ()
  (cljr--call-middleware-sync (cljr--create-msg "version") "version"))

(defun cljr--check-middleware-version ()
  "Check whether clj-refactor and nrepl-refactor versions are the same"
  (if (cljr--inside-project-p)
      (let ((refactor-nrepl-version (or (cljr--middleware-version)
                                        "n/a")))
        (unless (string-equal (downcase refactor-nrepl-version)
                              (downcase cljr-injected-middleware-version))
          (cider-repl-emit-interactive-stderr
           (format "WARNING: clj-refactor and refactor-nrepl are out of sync.
Their versions are %s and %s, respectively.
You can mute this warning by changing cljr-suppress-middleware-warnings."
                   (cljr--version) refactor-nrepl-version))))
    (unless cljr-suppress-no-project-warning
      (cider-repl-emit-interactive-stderr
       "WARNING: No Clojure project was detected. The
refactor-nrepl middleware was not enabled. (You can mute this
warning by customizing `cljr-suppress-no-project-warning'.)"))))

;;;###autoload
(defun cljr-version ()
  "Returns the version of the middleware as well as this package."
  (interactive)
  (message "clj-refactor %s, refactor-nrepl %s"
           (cljr--version)
           (or (ignore-errors (cljr--middleware-version))
               "is unreachable")))

;;;###autoload
(defun cljr-toggle-debug-mode ()
  (interactive)
  (setq cljr--debug-mode (not cljr--debug-mode))
  (if cljr--debug-mode
      (message "Debug mode on")
    (message "Debug mode off")))

(defun cljr--check-clojure-version ()
  (if-let ((clojure-version (cider--clojure-version)))
      (when (version< clojure-version cljr-minimum-clojure-version)
        (cider-repl-emit-interactive-stderr
         (format "WARNING: Clojure version (%s) is not supported (minimum %s). The refactor-nrepl middleware won't work and has been disabled!"
                 clojure-version cljr-minimum-clojure-version)))
    (cider-repl-emit-interactive-stderr
     (format "WARNING: Can't determine Clojure version.  The refactor-nrepl middleware requires clojure %s (or newer)" cljr-minimum-clojure-version))))

(defun cljr--init-middleware ()
  (unless cljr-suppress-middleware-warnings
    (cljr--check-clojure-version)
    (cljr--check-middleware-version))
  ;; Best effort; don't freak people out with errors
  (ignore-errors
    (when (cljr--middleware-version) ; check if middleware is running
      (when cljr-populate-artifact-cache-on-startup
        (cljr--init-artifact-cache))
      (when (and (not cljr-warn-on-eval)
                 cljr-eagerly-build-asts-on-startup)
        (cljr--warm-ast-cache)))))

(defvar cljr--list-fold-function-names
  '("map" "mapv" "pmap" "keep" "mapcat" "filter" "remove" "take-while" "drop-while"
    "group-by" "partition-by" "some" "every?" "not-every?" "not-any?"))

(defvar cljr--list-fold-function-names-with-index
  '("map-indexed" "keep-indexed"))

(defun cljr--chop-prefix (prefix s)
  "Remove PREFIX if it is at the start of S."
  (declare (pure t) (side-effect-free t))
  (let ((pos (length prefix)))
    (if (and (>= (length s) (length prefix))
             (string= prefix (substring s 0 pos)))
        (substring s pos)
      s)))

(defun cljr--ns-path (ns-name)
  "Find the file path to the ns named NS-NAME."
  (cider-ensure-connected)
  (cider-ensure-op-supported "ns-path")
  (cljr--chop-prefix "file:"
   (cider-sync-request:ns-path ns-name)))

;;;###autoload
(defun cljr-create-fn-from-example ()
  "Create a top-level defn for the symbol at point.
The context in which symbol is being used should be that of a
function, and the arglist of the defn is guessed from this
context.

For instance, if the symbol is the first argument of a `map'
call, the defn is created with one argument. If it is the first
argument of a `reduce', the defn will take two arguments.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-create-fn-from-example"
  (interactive)
  (while (cljr--keywordp (car (cljr--extract-sexp-as-list)))
    (paredit-backward-up))
  (let* ((sexp-forms* (cljr--extract-sexp-as-list))
         (fn-name (car sexp-forms*))
         (symbol-at-point (or (cider-symbol-at-point) ""))
         (parent-fn (ignore-errors
                      (save-excursion
                        (paredit-backward-up 2)
                        (forward-char)
                        (cljr--extract-sexp))))
         (args (cl-rest (cond
                         ((or (string= parent-fn "->")
                              (string= parent-fn "->>"))
                          (save-excursion
                            (paredit-backward-up)
                            (cljr--unwind-and-extract-this-as-list fn-name)))

                         ((or (string= fn-name "->")
                              (string= fn-name "->>"))
                          (save-excursion
                            (setq fn-name symbol-at-point)
                            (cljr--unwind-and-extract-this-as-list fn-name)))

                         (:else sexp-forms*))))
         (prefix (cljr--symbol-prefix symbol-at-point))
         (path (when (cljr--string-present-p prefix)
                 (cljr--ns-path (cljr--resolve-alias prefix)))))
    (push-mark)
    (if (cljr--symbolp symbol-at-point)
        (cond ((string= fn-name "update")
               (cljr--create-fn-from-update args path))

              ((string= fn-name "update-in")
               (cljr--create-fn-from-update-in path))

              ((string= fn-name "sort-by")
               (cljr--create-fn-from-sort-by args path))

              ((string= fn-name "sort")
               (cljr--create-fn-from-sort args path))

              ((string= fn-name "reduce")
               (cljr--create-fn-from-reduce args path))

              ((string= fn-name "repeatedly")
               (cljr--insert-example-fn symbol-at-point nil path))

              ((member fn-name cljr--list-fold-function-names)
               (cljr--create-fn-from-list-fold args path))

              ((member fn-name cljr--list-fold-function-names-with-index)
               (cljr--create-fn-from-list-fold-with-index args path))

              ((and (featurep 'cider) (cider-connected-p)
                    (cljr--var-info fn-name :all))
               (cljr--insert-example-fn symbol-at-point (list "args") path))

              (:else
               (cljr--insert-example-fn fn-name args path)))
      (cljr--insert-example-fn fn-name args path))))

(defun cljr--inflect-last-word (f s)
  (when s
    (save-match-data
      (let* ((words (split-string s "-"))
             (last-word (car (last words)))
             (prefix (butlast words)))
        (mapconcat 'identity
                   (append prefix (list (funcall f last-word)))
                   "-")))))

(defun cljr--create-fn-from-list-fold (args path)
  (cljr--insert-example-fn (car args)
                           (seq-map
                            (lambda (it)
                              (when-let (name (cljr--guess-param-name it))
                                (cljr--inflect-last-word 'inflection-singularize-string name)))
                            (cdr args))
                           path))

(defun cljr--create-fn-from-list-fold-with-index (args path)
  (cljr--insert-example-fn (car args)
                           (cons "index"
                                 (seq-map
                                  (lambda (it)
                                    (when-let (name (cljr--guess-param-name it))
                                      (cljr--inflect-last-word 'inflection-singularize-string name)))
                                  (cdr args)))
                           path))

(defun cljr--create-fn-from-update (args path)
  (let ((keyfn (cadr args)))
    (cljr--insert-example-fn (cider-symbol-at-point)
                             (if (cljr--keywordp keyfn)
                                 (list (string-remove-prefix ":" keyfn))
                               (list 0))
                             path)))

(defun cljr--create-fn-from-update-in (path)
  (let ((last-path-entry (save-excursion
                           (paredit-backward-down)
                           (cider-symbol-at-point))))
    (cljr--insert-example-fn (cider-symbol-at-point)
                             (if (cljr--keywordp last-path-entry)
                                 (list (string-remove-prefix ":" last-path-entry))
                               (list 0))
                             path)))

(defun cljr--create-fn-from-sort (args path)
  (let* ((fn-name (cider-symbol-at-point))
         (param-name (when-let (coll-name (cljr--guess-param-name (car (last args))))
                       (cljr--inflect-last-word 'inflection-singularize-string coll-name))))
    (cljr--insert-example-fn fn-name
                             (if param-name
                                 (list (concat param-name "-a")
                                       (concat param-name "-b"))
                               (list "a" "b"))
                             path)))

(defun cljr--create-fn-from-sort-by (args path)
  (let* ((fn-name (cider-symbol-at-point))
         (making-comparator? (and (string= fn-name (cadr args))
                                  (= 3 (length args))))
         (param-name (if making-comparator?
                         (when (cljr--keywordp (car args))
                           (string-remove-prefix ":" (car args)))
                       (when-let (coll-name (cljr--guess-param-name (car (last args))))
                         (cljr--inflect-last-word 'inflection-singularize-string coll-name)))))
    (cljr--insert-example-fn fn-name
                             (if making-comparator?
                                 (if param-name
                                     (list (concat param-name "-a")
                                           (concat param-name "-b"))
                                   (list "a" "b"))
                               (list param-name))
                             path)))

(defun cljr--create-fn-from-reduce (args path)
  (cljr--insert-example-fn
   (car args)
   (list (or (and (= 3 (length args))
                  (cljr--guess-param-name (nth 1 args)))
             "acc")
         (when-let (name (cljr--guess-param-name (car (last args))))
           (cljr--inflect-last-word 'inflection-singularize-string name)))
   path))

(defun cljr--unwind-and-extract-this-as-list (name)
  (let* ((parent-sexp (progn
                        (paredit-backward-up)
                        (cljr--extract-sexp)))
         (unwound (cljr--unwind-s parent-sexp)))
    (cljr--with-string-content unwound
      (search-forward (concat "(" name))
      (cljr--extract-sexp-as-list))))

(defun cljr--unwind-s (s)
  (if (string-prefix-p "(->" s)
      (cljr--with-string-content s
        (clojure-unwind-all)
        (buffer-substring (point-min) (point-max)))
    s))

(defun cljr--keywordp (s)
  (string-match-p "^::?[^0-9:[{(\"][^[{(\"]*$"
                  (replace-regexp-in-string "\n" " " s)))

(defun cljr--symbolp (s)
  "True when S is a symbol."
  (string-match-p "^[^0-9:[{(\"][^[{(\"]*$"
                  (replace-regexp-in-string "\n" " " s)))

(defun cljr--keyword-lookup-p (s)
  (string-match "^(:\\([^ 0-9:[{(\"][^[{(\"]+\\) " s))

(defun cljr--first-fn-call-s (s)
  (cljr--with-string-content s
    (when (looking-at "(")
      (paredit-forward-down)
      (cljr--extract-sexp))))

(defun cljr--first-arg-s (s)
  (cljr--with-string-content s
    (paredit-forward-down)
    (paredit-forward)
    (cljr--skip-past-whitespace-and-comments)
    (cljr--extract-sexp)))

(defun cljr--last-arg-s (s)
  (cljr--with-string-content s
    (paredit-forward)
    (paredit-backward-down)
    (paredit-backward)
    (cljr--extract-sexp)))

(defvar cljr--fns-that-get-item-out-of-coll
  (list "first" "second" "last" "fnext" "nth" "rand-nth"))

(defun cljr--strip-keyword-ns (s)
  (when (string-match "[^/]+$" s)
    (substring s (car (match-data)) (car (cdr (match-data))))))

(defun cljr--dashed-words (s)
  "Take the string S and replace all the word separators with '-'
and make the whole string lower-cased."
  (with-temp-buffer
    (insert s)
    (goto-char (point-min))
    (while (not (eobp))
      (subword-forward)
      (insert " "))
    (mapconcat 'identity (split-string (downcase (buffer-string))) "-")))

(defun cljr--guess-param-name (form)
  (let* ((prepped-form (cljr--strip-off-semantic-noops
                        (cljr--unwind-s form)))
         (fn-call (cljr--first-fn-call-s prepped-form)))
    (cond
     ((cljr--symbolp prepped-form)
      prepped-form)
     ((cljr--keyword-lookup-p prepped-form)
      (cljr--strip-keyword-ns (match-string 1 prepped-form)))
     ((and fn-call (string-suffix-p "." fn-call))
      (cljr--dashed-words (car (last (split-string fn-call "\\." t)))))
     ((and fn-call (string-prefix-p "create-" fn-call))
      (string-remove-prefix "create-" fn-call))
     ((and fn-call (string-prefix-p ".get" fn-call))
      (cljr--dashed-words (string-remove-prefix ".get" fn-call)))
     ((string= "get-in" fn-call)
      (cljr--find-param-name-from-get-in prepped-form))
     ((string= "get" fn-call)
      (cljr--find-param-name-from-get prepped-form))
     ((string= "repeat" fn-call)
      (inflection-pluralize-string
       (cljr--guess-param-name (cljr--last-arg-s prepped-form))))
     ((member fn-call cljr--fns-that-get-item-out-of-coll)
      (cljr--inflect-last-word 'inflection-singularize-string
       (cljr--guess-param-name (cljr--first-arg-s prepped-form)))))))

(defvar cljr--semantic-noops--first-position
  (list "assoc" "assoc-in" "update" "update-in" "dissoc" "conj" "concat"
        "cycle" "rest" "nthrest" "nthnext" "next" "nnext" "butlast"
        "reverse" "vec" "set" "distinct"))

(defvar cljr--semantic-noops--last-position
  (list "filter" "filterv" "remove" "take-nth" "cons" "drop" "drop-while"
        "take-last" "take" "take-while" "drop-last" "sort" "sort-by"))

(defun cljr--strip-off-semantic-noops (form)
  "The idea here is that each of these functions, when called on
   something, doesn't truly change what that something is - so we
   can ignore them when trying to figure out a name for a parameter."
  (cljr--with-string-content form
    (let* ((fn-at-point (lambda ()
                          (ignore-errors
                            (save-excursion
                              (paredit-forward-down)
                              (cljr--extract-sexp)))))
           (fn (funcall fn-at-point)))
      (while (or (member fn cljr--semantic-noops--first-position)
                 (member fn cljr--semantic-noops--last-position))
        (if (member fn cljr--semantic-noops--first-position)
            (progn
              (paredit-forward-down)
              (paredit-forward)
              (cljr--skip-past-whitespace-and-comments))
          (paredit-forward)
          (paredit-backward-down)
          (paredit-backward))
        (setq fn (funcall fn-at-point))))
    (cljr--extract-sexp)))

(defun cljr--find-param-name-from-get-in (form)
  (let ((last-path-entry (cljr--with-string-content form
                           (paredit-forward-down)
                           (paredit-forward 3)
                           (paredit-backward-down)
                           (cider-symbol-at-point))))
    (when (cljr--keywordp last-path-entry)
      (string-remove-prefix ":" last-path-entry))))

(defun cljr--find-param-name-from-get (form)
  (let ((key (cljr--with-string-content form
               (paredit-forward-down)
               (paredit-forward 2)
               (cljr--skip-past-whitespace-and-comments)
               (cljr--extract-sexp))))
    (when (cljr--keywordp key)
      (string-remove-prefix ":" key))))

(defun cljr--insert-example-fn (name args path)
  "Create a new function from NAME and ARGS.

If PATH is non-nil append the new function to the end of the file
at PATH."
  (let* ((params (lambda (word i)
                   (format "${%s:%s}" (+ i 1)
                           (or (and word (cljr--guess-param-name word))
                               (format "arg%s" i)))))
         (stub (concat (cljr--defn-str path)
                       (if path (cljr--symbol-suffix name) name)
                       " ["
                       (string-join (seq-map-indexed params args)  " ")
                       "]\n$0)")))
    (when path
      (find-file-other-window path)
      (goto-char (point-max)))
    (cljr--make-room-for-toplevel-form)
    (yas-expand-snippet stub)))

(defun cljr--extract-wiki-description (description-buffer)
  (with-current-buffer description-buffer
    (goto-char (point-min))
    (while (not (looking-at-p "<div id=\"wiki-body\""))
      (delete-char 1))
    (sgml-skip-tag-forward 1)
    (buffer-substring (point-min) (point))))

;;;###autoload
(defun cljr-describe-refactoring (cljr-fn)
  "Show the wiki page, in emacs, for one of the available refactorings.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-describe-refactoring"
  (interactive (list (cljr--prompt-user-for "Refactoring to describe: "
                                            (mapcar (lambda (entry) (cadr entry))
                                                    cljr--all-helpers))))
  (let* ((wiki-base-url "https://github.com/clojure-emacs/clj-refactor.el/wiki/")
         (description-buffer "*cljr-describe-refactoring*")
         (description (cljr--extract-wiki-description
                       (url-retrieve-synchronously
                        (concat wiki-base-url cljr-fn)))))
    (pop-to-buffer description-buffer)
    (delete-region (point-min) (point-max))
    (insert description)
    (shr-render-region (point-min) (point-max))
    (view-mode 1)))

(defun cljr--get-function-params (fn)
  "Retrieve the parameters for FN."
  (let* ((info (cljr--var-info fn))
         ;; arglists-str looks like this: ([arg] [arg1 arg2] ...)
         (arglists-str (nrepl-dict-get info "arglists-str")))
    (unless arglists-str
      (error "Couldn't retrieve the parameter list for %s" fn))
    (let* ((arglists-str (substring arglists-str 1 -1)))
      (unless (string-match-p "^\\[[^]]+\\]$" arglists-str)
        (error "Can't do work on functions of multiple arities"))
      (split-string (substring arglists-str 1 -1) " "))))

(defvar cljr--change-signature-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "M-n") #'cljr--move-param-down)
    (define-key keymap (kbd "M-p") #'cljr--move-param-up)
    (define-key keymap (kbd "C-c C-k") #'cljr--abort-signature-edit)
    (define-key keymap (kbd "q") #'cljr--abort-signature-edit)
    (define-key keymap (kbd "e") #'cljr--edit-parameter-name)
    (define-key keymap (kbd "C-c C-c") #'cljr--commit-signature-edit)
    (define-key keymap (kbd "RET") #'cljr--commit-signature-edit)
    keymap))

(defun cljr--abort-signature-edit ()
  (interactive)
  (kill-buffer cljr--change-signature-buffer))

(defun cljr--move-line-up ()
  "Move the current line up."
  (let ((col (current-column)))
    (transpose-lines 1)
    (forward-line -2)
    (move-to-column col)))

(defun cljr--move-line-down ()
  "Move the current line down."
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun cljr--signature-change-at-index (signature-changes i)
  (seq-find (lambda (change) (= (gethash :new-index change) i))
            signature-changes))

(defun cljr--dec-parameter-index ()
  (let* ((index (1- (line-number-at-pos)))
         (parameter-info (cljr--signature-change-at-index
                          cljr--signature-changes index))
         (neighbor-info (cljr--signature-change-at-index
                         cljr--signature-changes (1- index))))
    (puthash :new-index (1- (gethash :new-index parameter-info))
             parameter-info)
    (puthash :new-index (1+ (gethash :new-index neighbor-info))
             neighbor-info)))

(defun cljr--inc-parameter-index ()
  (let* ((index (1- (line-number-at-pos)))
         (parameter-info (cljr--signature-change-at-index
                          cljr--signature-changes index))
         (neighbor-info (cljr--signature-change-at-index
                         cljr--signature-changes (1+ index))))
    (puthash :new-index (1+ (gethash :new-index parameter-info))
             parameter-info)
    (puthash :new-index (1- (gethash :new-index neighbor-info))
             neighbor-info)))

(defun cljr--move-param-down ()
  (interactive)
  (unless (save-excursion (beginning-of-line) (or (looking-at-p "\\s-*$")
                                                  (looking-at-p "#")))
    (when (save-excursion (beginning-of-line) (looking-at-p "& "))
      (error "Can't move the rest parameter!"))
    (view-mode -1)
    (cljr--move-line-down)
    (view-mode 1)
    (cljr--dec-parameter-index)))

(defun cljr--move-param-up ()
  (interactive)
  (unless (or (= (line-number-at-pos) 1)
              (or (looking-at-p "\\s-*$")
                  (looking-at-p "#")))
    (when (save-excursion (beginning-of-line) (looking-at-p "& "))
      (error "Can't move the rest parameter!"))
    (view-mode -1)
    (cljr--move-line-up)
    (cljr--inc-parameter-index)
    (view-mode 1)))

(defun cljr--edit-parameter-name ()
  (interactive)
  (let* ((index (1- (line-number-at-pos)))
         (new-name (read-from-minibuffer
                    "New parameter name: "
                    (save-excursion
                      (beginning-of-line)
                      (buffer-substring (point)
                                        (cljr--point-after 'paredit-forward)))))
         (parameter-info (nth index cljr--signature-changes)))
    (puthash :new-name new-name parameter-info)
    (view-mode -1)
    (delete-region (point-at-bol) (point-at-eol))
    (insert new-name)
    (view-mode 1)))

(defun cljr--defnp (match)
  (string-match-p (rx (seq line-start (* whitespace) "("
                           (? (+ (or (in "a-z") (in "A-z") (in "0-9")
                                     (in "-") (in "._/"))))
                           "defn"))
                  match))

(defun cljr--update-parameter-name (new-name)
  (cljr--skip-past-whitespace-and-comments)
  (forward-char)
  (cljr-rename-symbol new-name))

(defun cljr--forward-parameter ()
  "Move point forward across one parameter.

This includes skipping past any type information added by
prismatic/schema and moving paste any whitespace"
  (paredit-forward)
  (cljr--skip-past-whitespace-and-comments)
  (when (looking-at-p ":-")
    (paredit-forward 2))
  (cljr--skip-past-whitespace-and-comments))

(defun cljr--update-signature-names (signature-changes)
  "Point is assumed to be at the the first character in the
  lambda list.

Updates the names of the function parameters."
  (dolist (changes signature-changes)
    (unless (string= (gethash :new-name changes)
                     (gethash :old-name changes))
      (cljr--update-parameter-name (gethash :new-name changes)))
    (cljr--forward-parameter)))

(defun cljr--delete-and-extract-region (beg end)
  (prog1
      (buffer-substring-no-properties beg end)
    (delete-region beg end)))

(defun cljr--delete-and-extract-function-parameter ()
  (cljr--skip-past-whitespace-and-comments)
  (let (parameter)
    (push (cljr--delete-and-extract-region
           (point) (cljr--point-after 'paredit-forward))
          parameter)
    (delete-region (point) (cljr--point-after
                            'cljr--skip-past-whitespace-and-comments))
    (when (looking-at-p ":-")
      (push (cljr--delete-and-extract-region
             (point) (cljr--point-after '(paredit-forward 2)))
            parameter))
    (delete-region (point) (cljr--point-after
                            'cljr--skip-past-whitespace-and-comments))
    (string-join (nreverse parameter) " ")))

(defun cljr--maybe-wrap-form ()
  "Insert newlines in or prior to the current form to prevent long lines.

Point is assumed to be at the end of the form."
  (let ((breakpoint (or (and (boundp 'whitespace-line-column)
                             whitespace-line-column)
                        80)))
    (when (> (current-column) breakpoint)
      (paredit-backward-up)
      (if (and (not (looking-back "^\\s-*" (point-at-bol))) (looking-at-p "\\["))
          (newline-and-indent) ; Put lambdalist on its own line
        (paredit-forward-down)
        (cljr--forward-parameter) ; don't break right after ( or [
        (while (save-excursion (cljr--forward-parameter)
                               (< (current-column) breakpoint))
          (cljr--forward-parameter))
        (newline-and-indent)))))

(defun cljr--update-signature-order (signature-changes)
  "Point is assumed to be at the first character in the lambda list.

Updates the ordering of the function parameters."
  (unless (seq-every-p (lambda (c) (= (gethash :new-index c) (gethash :old-index c)))
                       signature-changes)
    (let (parameters)
      ;; extract parameters
      (dolist (_ signature-changes)
        (push (cljr--delete-and-extract-function-parameter)
              parameters))
      (setq parameters (nreverse parameters))
      ;; leave point in empty lambda list
      (paredit-backward-up)
      (paredit-forward-down)
      (delete-region (point) (cljr--point-after 'cljr--skip-past-whitespace-and-comments))
      ;; insert parameters in new order
      (dotimes (i (length parameters))
        (let ((old-name (gethash :old-name
                                 (cljr--signature-change-at-index
                                  signature-changes i))))
          (insert (seq-find (lambda (param)
                              (string-prefix-p old-name param))
                            parameters)))
        (unless (= (1+ i) (length parameters))
          (insert " ")))
      (cljr--maybe-wrap-form))))

(defun cljr--goto-lambda-list ()
  "Move into the lambda list of the function definition beginning
at point.

E.g. move point from here:  |(defn foo [bar baz] ...)
to here:  (defn foo [|bar baz] ...)"
  (paredit-forward-down)
  (cljr--skip-past-whitespace-and-comments)
  (while (not (looking-at-p "\\["))
    (paredit-forward)
    (cljr--skip-past-whitespace-and-comments))
  (paredit-forward-down))

(defun cljr--update-function-signature (signature-changes)
  "Point is assumed to be just prior to the function definition
  we're about to update."
  (cljr--goto-lambda-list)
  (cljr--update-signature-names signature-changes)
  (cljr--goto-toplevel)
  (cljr--goto-lambda-list)
  (cljr--update-signature-order signature-changes))

(defun cljr--call-site-p (fn)
  "Is point at a call-site for FN?"
  (save-excursion
    (ignore-errors
      (paredit-backward-up)
      (paredit-forward-down)
      (string-suffix-p (cljr--symbol-suffix fn) (cider-symbol-at-point)))))

(defun cljr--no-changes-to-parameter-order-p (signature-changes)
  (seq-every-p (lambda (e) (= (gethash :new-index e) (gethash :old-index e)))
               signature-changes))

(defun cljr--update-call-site (signature-changes)
  "Point is assumed to be at the name of the function being
called."
  (unless (cljr--no-changes-to-parameter-order-p signature-changes)
    (cljr--forward-parameter)
    (let (args)
      (dotimes (_ (length signature-changes))
        (push (cljr--delete-and-extract-function-parameter) args))
      (setq args (nreverse args))
      (dotimes (i (length args))
        (insert (nth (gethash :old-index
                              (seq-find (lambda (c) (= (gethash :new-index c) i))
                                        signature-changes))
                     args))
        (unless (= (1+ i) (length args))
          (insert " ")))
      (cljr--maybe-wrap-form))))

(defun cljr--append-to-manual-intervention-buffer ()
  "Append the current line to the buffer of stuff requiring
manual intervention."
  (let ((line (string-trim (buffer-substring-no-properties
                            (point-at-bol) (point-at-eol))))
        (linum (line-number-at-pos))
        (file (buffer-file-name)))
    (with-current-buffer (get-buffer-create cljr--manual-intervention-buffer)
      (goto-char (point-max))
      (insert (format "%s:%s: %s\n" file linum line)))))

(defun cljr--update-apply-call-site (signature-changes)
  "Update a call-site where apply is used to call the function
  whose signature we're currently editing.

point is assumed to be at the function name"
  (unless (cljr--no-changes-to-parameter-order-p signature-changes)
    (let ((num-args 0)
          (max-index (thread-last signature-changes
                       (seq-map (lambda (c) (let ((new  (gethash :new-index c))
                                                  (old (gethash :old-index c)))
                                              (if (/= old new)
                                                  (max old new)))))
                       (seq-remove #'null)
                       (apply #'max)))
          beg end)
      (cljr--skip-past-whitespace-and-comments)
      (setq beg (point))
      (paredit-forward)
      (setq end (cljr--point-after 'paredit-forward-up))
      (while (< (save-excursion (cljr--point-after 'cljr--forward-parameter)) end)
        (cljr--forward-parameter)
        (setq num-args (1+ num-args)))
      (if (>=  max-index (1- num-args))
          ;; Some of the arguments in the final list of args to apply have changed
          (cljr--append-to-manual-intervention-buffer)
        (goto-char beg)
        (cljr--update-call-site signature-changes)))))

(defun cljr--update-partial-call-site (signature-changes)
  "Update a call-site with partial application of the function
  whose signature we're currently editing.

This only handles the case where we have (partial my-fn a b c)
and only parameters a b or c are affected.

point is assumed to be at the function name"
  (unless (cljr--no-changes-to-parameter-order-p signature-changes)
    (let ((num-partials 0)
          (max-index (thread-last signature-changes
                       (seq-map (lambda (c) (let ((new  (gethash :new-index c))
                                                  (old (gethash :old-index c)))
                                              (when (/= old new)
                                                (max old new)))))
                       (seq-remove #'null)
                       (apply #'max)))
          beg end)
      (setq beg (point))
      (cljr--skip-past-whitespace-and-comments)
      (paredit-forward 1)
      (setq end (cljr--point-after 'paredit-forward-up))
      (while (< (save-excursion (cljr--point-after 'cljr--forward-parameter)) end)
        (cljr--forward-parameter)
        (setq num-partials (1+ num-partials)))
      (if (>=  max-index num-partials)
          (cljr--append-to-manual-intervention-buffer)
        (goto-char beg)
        (cljr--update-call-site (seq-remove (lambda (c)
                                              (>= (gethash :new-index c) num-partials))
                                            signature-changes))))))

(defun cljr--apply-call-site-p ()
  "Is the function invocation at this place being done using
  apply?

Point is assumed to be at the function being called."
  (ignore-errors
    (save-excursion
      (paredit-backward-up)
      (paredit-forward-down)
      (looking-at-p "apply"))))

(defun cljr--partial-call-site-p ()
  "Is the function invocation at this place being done using
  partial.

Point is assumed to be at the function being called."
  (ignore-errors
    (save-excursion
      (paredit-backward-up)
      (paredit-forward-down)
      (looking-at-p "partial"))))

(defun cljr--ignorable-occurrence-p ()
  (save-excursion
    (cljr--goto-toplevel)
    (looking-at-p "\\s-*(ns")))

(defun cljr--change-function-signature (occurrences signature-changes)
  ;; SIGNATURE-CHANGES is a list of hashmaps with keys:
  ;; :old-index, :new-index, :old-name :new-name
  ;; Indexing is from 0
  ;; The OCCURRENCES are the same as those returned by `cljr--find-symbol'
  (let ((*cljr--noninteractive* t))
    (dolist (symbol-meta occurrences)
      (let ((file (cljr--get-valid-filename symbol-meta))
            (line-beg (gethash :line-beg symbol-meta))
            (col-beg (gethash :col-beg symbol-meta))
            (name (gethash :name symbol-meta))
            (match (gethash :match symbol-meta)))
        (with-current-buffer
            (find-file-noselect file)
          (goto-char (point-min))
          (forward-line (1- line-beg))
          (move-to-column (1- col-beg))
          (cond
           ((cljr--ignorable-occurrence-p) :do-nothing)
           ((cljr--call-site-p name) (cljr--update-call-site signature-changes))
           ((cljr--partial-call-site-p)
            (cljr--update-partial-call-site signature-changes))
           ((cljr--apply-call-site-p)
            (cljr--update-apply-call-site signature-changes))
           ((cljr--defnp match)
            (cljr--update-function-signature signature-changes))
           (t (cljr--append-to-manual-intervention-buffer)))
          (save-buffer)))))
  (unless (cljr--empty-buffer-p (get-buffer-create cljr--manual-intervention-buffer))
    (pop-to-buffer cljr--manual-intervention-buffer)
    (goto-char (point-min))
    (insert "The following occurrence(s) couldn't be handled automatically:\n\n")
    (grep-mode)
    (setq-local compilation-search-path (list (cljr--project-dir)))))

(defun cljr--commit-signature-edit ()
  (interactive)
  (cljr--change-function-signature cljr--occurrences cljr--signature-changes)
  (kill-buffer cljr--change-signature-buffer))

(define-derived-mode cljr--change-signature-mode fundamental-mode
  "Change Signature"
  "Major mode for refactoring function signatures.")

(defun cljr--setup-change-signature-buffer (control-buffer params)
  (when (get-buffer control-buffer)
    (kill-buffer control-buffer))
  (pop-to-buffer control-buffer)
  (delete-region (point-min) (point-max))
  (insert "

# M-n and M-p to re-order parameters.
# e or C-c C-e to edit a name.
# RET or C-c C-c when you're happy with your changes.
# q or C-c C-k to abort. ")
  (goto-char (point-min))
  (insert (string-join params "\n"))
  (forward-line -1)
  (when (looking-at-p "&")
    (forward-line 1)
    (join-line))
  (setq cljr--signature-changes (let (signature-changes)
                                  (dotimes (i (length params))
                                    (let ((h (make-hash-table)))
                                      (puthash :old-index i h)
                                      (puthash :new-index i h)
                                      (puthash :old-name (nth i params) h)
                                      (puthash :new-name (nth i params) h)
                                      (push h signature-changes)))
                                  (nreverse signature-changes)))
  (cljr--change-signature-mode)
  (view-mode))

;;;###autoload
(defun cljr-change-function-signature ()
  "Change the function signature of the function at point.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-change-function-signature"
  (interactive)
  (cljr--ensure-op-supported "find-symbol")
  (when (cljr--asts-y-or-n-p)
    (let* ((fn (cider-symbol-at-point))
           (params (cljr--get-function-params fn))
           (var-info (cljr--var-info fn))
           (ns (nrepl-dict-get var-info "ns")))
      (setq cljr--occurrences (cljr--find-symbol-sync fn ns)
            cljr--signature-changes nil)
      (cljr--setup-change-signature-buffer cljr--change-signature-buffer params)
      (when (get-buffer cljr--manual-intervention-buffer)
        (kill-buffer cljr--manual-intervention-buffer))
      (pop-to-buffer cljr--change-signature-buffer))))

;;;###autoload
(defun cljr--inject-middleware-p (&rest _)
  "Return non-nil if nREPL middleware should be injected."
  (cljr--inside-project-p))

;;;###autoload
(defun cljr--inject-jack-in-dependencies ()
  "Inject the REPL dependencies of clj-refactor at `cider-jack-in'.
If injecting the dependencies is not preferred set `cljr-inject-dependencies-at-jack-in' to nil."
  (when (and cljr-inject-dependencies-at-jack-in
             (boundp 'cider-jack-in-lein-plugins)
             (boundp 'cider-jack-in-nrepl-middlewares))
    (add-to-list 'cider-jack-in-lein-plugins `("refactor-nrepl/refactor-nrepl" ,cljr-injected-middleware-version
                                               :predicate cljr--inject-middleware-p))
    (add-to-list 'cider-jack-in-nrepl-middlewares '("refactor-nrepl.middleware/wrap-refactor"
                                                    :predicate cljr--inject-middleware-p))))

;;;###autoload
(eval-after-load 'cider
  '(cljr--inject-jack-in-dependencies))

(add-hook 'cider-connected-hook #'cljr--init-middleware)

;; moved to Clojure mode, made obsolete here
(define-obsolete-variable-alias 'cljr-thread-all-but-last 'clojure-thread-all-but-last "2.3.0")
(define-obsolete-variable-alias 'cljr-use-metadata-for-privacy 'clojure-use-metadata-for-privacy "2.3.0")

(define-obsolete-function-alias 'cljr-thread 'clojure-thread "2.3.0")
(define-obsolete-function-alias 'cljr-thread-first-all 'clojure-thread-first-all "2.3.0")
(define-obsolete-function-alias 'cljr-thread-last-all 'clojure-thread-last-all "2.3.0")
(define-obsolete-function-alias 'cljr-unwind 'clojure-unwind "2.3.0")
(define-obsolete-function-alias 'cljr-unwind-all 'clojure-unwind-all "2.3.0")
(define-obsolete-function-alias 'cljr-cycle-privacy 'clojure-cycle-privacy "2.3.0")
(define-obsolete-function-alias 'cljr-cycle-if 'clojure-cycle-if "2.3.0")
(make-obsolete 'cljr-cycle-coll "reworked into convert collection to list, quoted list, map, vector, set in Clojure mode." "2.3.0")

;; ------ minor mode -----------
;;;###autoload
(define-minor-mode clj-refactor-mode
  "A mode to keep the clj-refactor keybindings.

\\{clj-refactor-map}"
  nil " cljr" clj-refactor-map
  (if clj-refactor-mode
      (add-hook 'post-command-hook #'cljr--post-command-hook :append :local)
    (remove-hook 'post-command-hook #'cljr--post-command-hook :local)))

(provide 'clj-refactor)
;;; clj-refactor.el ends here
