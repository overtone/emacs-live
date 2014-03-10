;;; haskell-cabal.el --- Support for Cabal packages

;; Copyright (C) 2007, 2008  Stefan Monnier

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Todo:

;; - distinguish continued lines from indented lines.
;; - indent-line-function.
;; - outline-minor-mode.

;;; Code:

;; (defun haskell-cabal-extract-fields-from-doc ()
;;   (require 'xml)
;;   (with-no-warnings (require 'cl))
;;   (let ((section (completing-read
;;                   "Section: "
;;                   '("general-fields" "library" "executable" "buildinfo"))))
;;     (goto-char (point-min))
;;     (search-forward (concat "<sect3 id=\"" section "\">")))
;;   (let* ((xml (xml-parse-region
;;                (progn (search-forward "<variablelist>") (match-beginning 0))
;;                (progn (search-forward "</variablelist>") (point))))
;;          (varlist (remove-if-not 'consp (cddar xml)))
;;          (syms (mapcar (lambda (entry) (caddr (assq 'literal (assq 'term entry))))
;;                        varlist))
;;          (fields (mapcar (lambda (sym) (substring-no-properties sym 0 -1)) syms)))
;;     fields))

(with-no-warnings (require 'cl))
(require 'haskell-utils)

(defconst haskell-cabal-general-fields
  ;; Extracted with (haskell-cabal-extract-fields-from-doc "general-fields")
  '("name" "version" "cabal-version" "license" "license-file" "copyright"
    "author" "maintainer" "stability" "homepage" "package-url" "synopsis"
    "description" "category" "tested-with" "build-depends" "data-files"
    "extra-source-files" "extra-tmp-files"))

(defconst haskell-cabal-library-fields
  ;; Extracted with (haskell-cabal-extract-fields-from-doc "library")
  '("exposed-modules"))

(defconst haskell-cabal-executable-fields
  ;; Extracted with (haskell-cabal-extract-fields-from-doc "executable")
  '("executable" "main-is"))

(defconst haskell-cabal-buildinfo-fields
  ;; Extracted with (haskell-cabal-extract-fields-from-doc "buildinfo")
  '("buildable" "other-modules" "hs-source-dirs" "extensions" "ghc-options"
    "ghc-prof-options" "hugs-options" "nhc-options" "includes"
    "install-includes" "include-dirs" "c-sources" "extra-libraries"
    "extra-lib-dirs" "cc-options" "ld-options" "frameworks"))

(defvar haskell-cabal-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; The comment syntax can't be described simply in syntax-table.
    ;; We could use font-lock-syntactic-keywords, but is it worth it?
    ;; (modify-syntax-entry ?-  ". 12" st)
    (modify-syntax-entry ?\n ">" st)
    st))

(defvar haskell-cabal-font-lock-keywords
  ;; The comment syntax can't be described simply in syntax-table.
  ;; We could use font-lock-syntactic-keywords, but is it worth it?
  '(("^[ \t]*--.*" . font-lock-comment-face)
    ("^ *\\([^ \t:]+\\):" (1 font-lock-keyword-face))
    ("^\\(Library\\)[ \t]*\\({\\|$\\)" (1 font-lock-keyword-face))
    ("^\\(Executable\\|Test-Suite\\|Benchmark\\)[ \t]+\\([^\n \t]*\\)"
     (1 font-lock-keyword-face) (2 font-lock-function-name-face))
    ("^\\(Flag\\)[ \t]+\\([^\n \t]*\\)"
     (1 font-lock-keyword-face) (2 font-lock-constant-face))
    ("^\\(Source-Repository\\)[ \t]+\\(head\\|this\\)"
     (1 font-lock-keyword-face) (2 font-lock-constant-face))
    ("^ *\\(if\\)[ \t]+.*\\({\\|$\\)" (1 font-lock-keyword-face))
    ("^ *\\(}[ \t]*\\)?\\(else\\)[ \t]*\\({\\|$\\)"
     (2 font-lock-keyword-face))))

(defvar haskell-cabal-buffers nil
  "List of Cabal buffers.")

(defun haskell-cabal-buffers-clean (&optional buffer)
  (let ((bufs ()))
    (dolist (buf haskell-cabal-buffers)
      (if (and (buffer-live-p buf) (not (eq buf buffer))
               (with-current-buffer buf (derived-mode-p 'haskell-cabal-mode)))
          (push buf bufs)))
    (setq haskell-cabal-buffers bufs)))

(defun haskell-cabal-unregister-buffer ()
  (haskell-cabal-buffers-clean (current-buffer)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))

;;;###autoload
(define-derived-mode haskell-cabal-mode fundamental-mode "Haskell-Cabal"
  "Major mode for Cabal package description files."
  (set (make-local-variable 'font-lock-defaults)
       '(haskell-cabal-font-lock-keywords t t nil nil))
  (add-to-list 'haskell-cabal-buffers (current-buffer))
  (add-hook 'change-major-mode-hook 'haskell-cabal-unregister-buffer nil 'local)
  (add-hook 'kill-buffer-hook 'haskell-cabal-unregister-buffer nil 'local)
  (set (make-local-variable 'comment-start) "-- ")
  (set (make-local-variable 'comment-start-skip) "\\(^[ \t]*\\)--[ \t]*")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-end-skip) "[ \t]*\\(\\s>\\|\n\\)")
  )

(defun haskell-cabal-get-setting (name)
  (save-excursion
    (let ((case-fold-search t))
      (goto-char (point-min))
      (when (re-search-forward
             (concat "^[ \t]*" (regexp-quote name)
                     ":[ \t]*\\(.*\\(\n[ \t]+[ \t\n].*\\)*\\)")
             nil t)
        (let ((val (match-string 1))
              (start 1))
          (when (match-end 2)             ;Multiple lines.
            ;; The documentation is not very precise about what to do about
            ;; the \n and the indentation: are they part of the value or
            ;; the encoding?  I take the point of view that \n is part of
            ;; the value (so that values can span multiple lines as well),
            ;; and that only the first char in the indentation is part of
            ;; the encoding, the rest is part of the value (otherwise, lines
            ;; in the value cannot start with spaces or tabs).
            (while (string-match "^[ \t]\\(?:\\.$\\)?" val start)
              (setq start (1+ (match-beginning 0)))
              (setq val (replace-match "" t t val))))
          val)))))

;;;###autoload
(defun haskell-cabal-get-dir ()
  "Get the Cabal dir for a new project. Various ways of figuring this out,
   and indeed just prompting the user. Do them all."
  (let* ((file (haskell-cabal-find-file))
         (dir (when file (file-name-directory file))))
    (haskell-utils-read-directory-name
     (format "Cabal dir%s: " (if file (format " (guessed from %s)" (file-relative-name file)) ""))
     dir)))

(defun haskell-cabal-compute-checksum (dir)
  "Compute MD5 checksum of package description file in DIR.
Return nil if no Cabal description file could be located via
`haskell-cabal-find-pkg-desc'."
  (let ((cabal-file (haskell-cabal-find-pkg-desc dir)))
    (when cabal-file
      (with-temp-buffer
        (insert-file-contents cabal-file)
        (md5 (buffer-string))))))

(defun haskell-cabal-find-file (&optional dir)
  "Search for package description file upwards starting from DIR.
If DIR is nil, `default-directory' is used as starting point for
directory traversal.  Upward traversal is aborted if file owner
changes.  Uses`haskell-cabal-find-pkg-desc' internally."
  (catch 'found
    (let ((user (nth 2 (file-attributes (or dir default-directory))))
          ;; Abbreviate, so as to stop when we cross ~/.
          (root (abbreviate-file-name (or dir default-directory))))
      ;; traverse current dir up to root as long as file owner doesn't change
      (while (and root (equal user (nth 2 (file-attributes root))))
        (let ((cabal-file (haskell-cabal-find-pkg-desc root)))
          (when cabal-file
            (throw 'found cabal-file)))

        (let ((proot (file-name-directory (directory-file-name root))))
          (if (equal proot root) ;; fix-point reached?
              (throw 'found nil)
            (setq root proot))))
      nil)))

(defun haskell-cabal-find-pkg-desc (dir &optional allow-multiple)
  "Find a package description file in the directory DIR.
Returns nil if none or multiple \".cabal\" files were found.  If
ALLOW-MULTIPLE is non nil, in case of multiple \".cabal\" files,
a list is returned instead of failing with a nil result."
  ;; This is basically a port of Cabal's
  ;; Distribution.Simple.Utils.findPackageDesc function
  ;;  http://hackage.haskell.org/packages/archive/Cabal/1.16.0.3/doc/html/Distribution-Simple-Utils.html
  ;; but without the exception throwing.
  (let* ((cabal-files
          (remove-if 'file-directory-p
                     (remove-if-not 'file-exists-p
                                    (directory-files dir t ".\\.cabal\\'")))))
    (cond
     ((= (length cabal-files) 1) (car cabal-files)) ;; exactly one candidate found
     (allow-multiple cabal-files) ;; pass-thru multiple candidates
     (t nil))))

(defun haskell-cabal-find-dir (&optional dir)
  "Like `haskell-cabal-find-file' but returns directory instead.
See `haskell-cabal-find-file' for meaning of DIR argument."
  (let ((cabal-file (haskell-cabal-find-file dir)))
    (when cabal-file
      (file-name-directory cabal-file))))

;;;###autoload
(defun haskell-cabal-visit-file (other-window)
  "Locate and visit package description file for file visited by current buffer.
This uses `haskell-cabal-find-file' to locate the closest
\".cabal\" file and open it.  This command assumes a common Cabal
project structure where the \".cabal\" file is in the top-folder
of the project, and all files related to the project are in or
below the top-folder.  If called with non-nil prefix argument
OTHER-WINDOW use `find-file-other-window'."
  (interactive "P")
  ;; Note: We aren't allowed to rely on haskell-session here (which,
  ;; in pathological cases, can have a different .cabal file
  ;; associated with the current buffer)
  (if buffer-file-name
      (let ((cabal-file (haskell-cabal-find-file (file-name-directory buffer-file-name))))
        (if cabal-file
            (if other-window
                (find-file-other-window cabal-file)
              (find-file cabal-file))
          (error "Could not locate \".cabal\" file for %S" buffer-file-name)))
    (error "Cannot locate \".cabal\" file for buffers not visiting any file")))

(defvar haskell-cabal-commands
  '("install"
    "update"
    "list"
    "info"
    "upgrade"
    "fetch"
    "unpack"
    "check"
    "sdist"
    "upload"
    "report"
    "init"
    "configure"
    "build"
    "copy"
    "haddock"
    "clean"
    "hscolour"
    "register"
    "test"
    "help"))

(provide 'haskell-cabal)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; haskell-cabal.el ends here
