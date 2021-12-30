;;; haskell-compile.el --- Haskell/GHC compilation sub-mode -*- lexical-binding: t -*-

;; Copyright (C) 2013  Herbert Valerio Riedel
;;               2020  Marc Berkowitz <mberkowitz@github.com>
;;               2020  Jacob Ilsø

;; Author: Herbert Valerio Riedel <hvr@gnu.org>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Simple GHC-centric compilation sub-mode; see info node
;; `(haskell-mode)compilation' for more information

;;; Code:

(require 'compile)
(require 'haskell-cabal)
(require 'haskell-customize)
(require 'ansi-color)
(eval-when-compile (require 'subr-x))

;;;###autoload
(defgroup haskell-compile nil
  "Settings for Haskell compilation mode"
  :link '(custom-manual "(haskell-mode)compilation")
  :group 'haskell)

(defcustom haskell-compile-cabal-build-command
  "cabal build --ghc-option=-ferror-spans"
  "Default build command to use for `haskell-cabal-build' when a cabal file is detected.
For legacy compat, `%s' is replaced by the cabal package top folder."
  :group 'haskell-compile
  :type 'string)

(defcustom haskell-compile-cabal-build-alt-command
  "cabal clean -s && cabal build --ghc-option=-ferror-spans"
  "Alternative build command to use when `haskell-cabal-build' is called with a negative prefix argument.
For legacy compat, `%s' is replaced by the cabal package top folder."
  :group 'haskell-compile
  :type 'string)

(defcustom haskell-compile-stack-build-command
  "stack build --fast"
  "Default build command to use for `haskell-stack-build' when a stack file is detected.
For legacy compat, `%s' is replaced by the stack package top folder."
  :group 'haskell-compile
  :type 'string)

(defcustom haskell-compile-stack-build-alt-command
  "stack clean && stack build --fast"
  "Alternative build command to use when `haskell-stack-build' is called with a negative prefix argument.
For legacy compat, `%s' is replaced by the stack package top folder."
  :group 'haskell-compile
  :type 'string)

(defcustom haskell-compile-command
  "ghc -Wall -ferror-spans -fforce-recomp -c %s"
  "Default build command to use for `haskell-cabal-build' when no cabal or stack file is detected.
The `%s' placeholder is replaced by the current buffer's filename."
  :group 'haskell-compile
  :type 'string)

(defcustom haskell-compile-ghc-filter-linker-messages
  t
  "Filter out unremarkable \"Loading package...\" linker messages during compilation."
  :group 'haskell-compile
  :type 'boolean)

(defcustom haskell-compiler-type
  'auto
  "Controls whether to use cabal, stack, or ghc to compile.
   Auto (the default) means infer from the presence of a cabal or stack spec file,
   following same rules as haskell-process-type."
    :type '(choice (const auto) (const ghc) (const stack) (const cabal))
    :group 'haskell-compile)
(make-variable-buffer-local 'haskell-compiler-type)

(defconst haskell-compilation-error-regexp-alist
  `((,(concat
       "^ *\\([^\n\r\t>]*\s*> \\)?" ;; if using multi-package stack project, remove the package name that is prepended
       "\\(?1:[^\t\r\n]+?\\):"
       "\\(?:"
       "\\(?2:[0-9]+\\):\\(?4:[0-9]+\\)\\(?:-\\(?5:[0-9]+\\)\\)?" ;; "121:1" & "12:3-5"
       "\\|"
       "(\\(?2:[0-9]+\\),\\(?4:[0-9]+\\))-(\\(?3:[0-9]+\\),\\(?5:[0-9]+\\))" ;; "(289,5)-(291,36)"
       "\\)"
       ":\\(?6:\n?[ \t]+[Ww]arning:\\)?")
     1 (2 . 3) (4 . 5) (6 . nil)) ;; error/warning locus

    ;; multiple declarations
    ("^    \\(?:Declared at:\\|            \\) \\(?1:[^ \t\r\n]+\\):\\(?2:[0-9]+\\):\\(?4:[0-9]+\\)$"
     1 2 4 0) ;; info locus

    ;; failed tasty tests
    (".*error, called at \\(.*\\.hs\\):\\([0-9]+\\):\\([0-9]+\\) in .*" 1 2 3 2 1)
    (" +\\(.*\\.hs\\):\\([0-9]+\\):$" 1 2 nil 2 1)

    ;; this is the weakest pattern as it's subject to line wrapping et al.
    (" at \\(?1:[^ \t\r\n]+\\):\\(?2:[0-9]+\\):\\(?4:[0-9]+\\)\\(?:-\\(?5:[0-9]+\\)\\)?[)]?$"
     1 2 (4 . 5) 0)) ;; info locus
  "Regexps used for matching GHC compile messages.
See `compilation-error-regexp-alist' for semantics.")

(defvar haskell-compilation-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map compilation-mode-map))
  "Keymap for `haskell-compilation-mode' buffers.
This is a child of `compilation-mode-map'.")

(defun haskell-compilation-filter-hook ()
  "Local `compilation-filter-hook' for `haskell-compilation-mode'."

  (when haskell-compile-ghc-filter-linker-messages
    (delete-matching-lines "^ *Loading package [^ \t\r\n]+ [.]+ linking [.]+ done\\.$"
                           (save-excursion (goto-char compilation-filter-start)
                                           (line-beginning-position))
                           (point)))

  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point-max))))

(define-compilation-mode haskell-compilation-mode "HsCompilation"
  "Haskell/GHC specific `compilation-mode' derivative.
This mode provides support for GHC 7.[46]'s compile
messages. Specifically, also the `-ferror-spans` source location
format is supported, as well as info-locations within compile
messages pointing to additional source locations."
  (setq-local compilation-error-regexp-alist
              haskell-compilation-error-regexp-alist)

  (add-hook 'compilation-filter-hook
            'haskell-compilation-filter-hook nil t)
  )

;;;###autoload
(defun haskell-compile (&optional edit-command)
  "Run a compile command for the current Haskell buffer.
Obeys haskell-compiler-type to choose the appropriate build command.

If prefix argument EDIT-COMMAND is non-nil (and not a negative
prefix `-'), prompt for a custom compile command.

If EDIT-COMMAND contains the negative prefix argument `-', call
the alternative command defined in
`haskell-compile-stack-build-alt-command' /
`haskell-compile-cabal-build-alt-command'.

If there is no prefix argument, the most recent custom compile
command is used, falling back to
`haskell-compile-stack-build-command' for stack builds
`haskell-compile-cabal-build-command' for cabal builds, and
`haskell-compile-command' otherwise.

'% characters in the `-command' templates are replaced by the
base directory for build tools, or the current buffer for
`haskell-compile-command'."
  (interactive "P")
  (save-some-buffers (not compilation-ask-about-save)
                     compilation-save-buffers-predicate)
  (let (htype dir)
    ;;test haskell-compiler-type to set htype and dir
    (cond
     ((eq haskell-compiler-type 'cabal)
      (setq htype 'cabal)
      (setq dir (haskell-cabal-find-dir)))
     ((eq haskell-compiler-type 'stack)
      (setq htype 'stack)
      (setq dir (locate-dominating-file default-directory "stack.yaml")))
     ((eq haskell-compiler-type 'ghc)
      (setq htype 'ghc))
     ((eq haskell-compiler-type 'auto)
      (let ((r (haskell-build-type)))
        (setq htype (car r))
        (setq dir   (cdr r))))
     (t (error "Invalid haskell-compiler-type")))
    ;; now test htype and compile
    (cond
     ((or (eq htype 'cabal) (eq htype 'cabal-project)) ; run cabal
      (let ((command     haskell-compile-cabal-build-command)
            (alt-command haskell-compile-cabal-build-alt-command))
        (when (eq htype 'cabal-project) ;no default target
          (setq command (concat command " all")
                alt-command (concat alt-command " all")))
        (haskell--compile dir edit-command
           'haskell--compile-cabal-last
           command alt-command)))
     ((eq htype 'stack)
      (haskell--compile dir edit-command
        'haskell--compile-stack-last
        haskell-compile-stack-build-command
        haskell-compile-stack-build-alt-command))
     ((eq htype 'ghc)
      (haskell--compile (buffer-file-name) edit-command
        'haskell--compile-ghc-last
        haskell-compile-command
        haskell-compile-command)))))

;; Save commands for reuse, but only when in same context.
;; Hence save a pair (COMMAND . DIR); or nil.
(defvar haskell--compile-stack-last nil)
(defvar haskell--compile-cabal-last nil)
(defvar haskell--compile-ghc-last nil)

;; called only by (haskell-compile):
(defun haskell--compile (dir-or-file edit last-sym fallback alt)
  (let* ((dir-or-file (or dir-or-file default-directory))
         (local-dir-or-file (or (file-remote-p dir-or-file 'localname) dir-or-file))
         (last-pair (symbol-value last-sym))
         (last-command (car last-pair))
         (last-dir (cdr last-pair))
         (default (or (and last-dir (eq last-dir local-dir-or-file) last-command)
                      fallback))
         (template (cond
                    ((null edit) default)
                    ((eq edit '-) alt)
                    (t (compilation-read-command default))))
         (command (format template (shell-quote-argument local-dir-or-file)))
         (dir (if (directory-name-p local-dir-or-file)
                  local-dir-or-file
                default-directory))
         (name (if (directory-name-p local-dir-or-file)
                   (file-name-base (directory-file-name local-dir-or-file))
                 (file-name-nondirectory local-dir-or-file))))
    (unless (eq edit'-)
      (set last-sym (cons template local-dir-or-file)))
    (let ((default-directory dir))
      (compilation-start
       command
       'haskell-compilation-mode
       (lambda (mode) (format "*%s* <%s>" mode name))))))

(provide 'haskell-compile)
;;; haskell-compile.el ends here
