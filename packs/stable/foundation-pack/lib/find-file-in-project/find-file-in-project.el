;;; find-file-in-project.el --- Find file/directory and review Diff/Patch/Commit efficiently -*- coding: utf-8 -*-

;; Copyright (C) 2006-2009, 2011-2012, 2015-2018
;;   Phil Hagelberg, Doug Alcorn, Will Farrington, Chen Bin
;;
;; Version: 6.1.2
;; Author: Phil Hagelberg, Doug Alcorn, and Will Farrington
;; Maintainer: Chen Bin <chenbin.sh@gmail.com>
;; URL: https://github.com/redguardtoo/find-file-in-project
;; Package-Requires: ((emacs "25.1"))
;; Created: 2008-03-18
;; Keywords: project, convenience
;; EmacsWiki: FindFileInProject

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This program provides methods to find file in project.
;;
;; Features,
;; - Only dependency is BSD/GNU find
;; - Works on Windows with minimum setup
;; - Works on Tramp Mode (https://www.emacswiki.org/emacs/TrampMode)
;; - fd (faster alternative of find, see https://github.com/sharkdp/fd) is supported
;; - Uses native API `completing-read' and supports helm/ivy/consult/selectrum out of box.
;;
;;   Helm setup,
;;     (helm-mode 1)
;;
;;   Ivy setup,
;;     (ivy-mode 1)
;;
;;   Ido setup,
;;     (setq ffip-prefer-ido-mode t)
;;
;; Usage,
;;   - You can insert "(setq ffip-use-rust-fd t)" into ".emacs" to use fd (alternative of find)
;;   - `find-file-in-project-at-point' guess the file path at point and
;;      find file
;;   - `find-file-in-project-by-selected' uses the selected region
;;      as the keyword to search file.  You can provide the keyword
;;      if no region is selected.
;;   - `find-directory-in-project-by-selected' uses the select region
;;      to find directory.  You can provide the keyword if no region
;;      is selected.
;;   - `find-file-in-project' starts search file immediately
;;   - `ffip-create-project-file' creates ".dir-locals.el"
;;   - `ffip-lisp-find-file-in-project' finds file in project.
;;     If its parameter is not nil, it find directory.
;;     This command is written in pure Lisp and does not use any third party
;;     command line program.  So it works in all environments.
;;
;; A project is found by searching up the directory tree until a file
;; is found that matches `ffip-project-file'.
;; You can set `ffip-project-root-function' to provide an alternate
;; function to search for the project root.  By default, it looks only
;; for files whose names match `ffip-patterns',

;; If you have so many files that it becomes unwieldy, you can set
;; `ffip-find-options' to a string which will be passed to the `find'
;; invocation in order to exclude irrelevant subdirectories/files.
;; For instance, in a Ruby on Rails project, you are interested in all
;; .rb files that don't exist in the "vendor" directory.  In that case
;; you could set `ffip-find-options' to "-not -regex \".*vendor.*\"".

;; `ffip-insert-file' insert file content into current buffer.

;; `find-file-with-similar-name' find file with similar name to current
;; opened file. The regular expression `ffip-strip-file-name-regex' is
;; also used by `find-file-with-similar-name'.
;;
;; all these variables may be overridden on a per-directory basis in
;; your ".dir-locals.el".  See (info "(Emacs) Directory Variables") for
;; details.
;;
;; Sample ".dir-locals.el",
;;
;; ((nil . ((ffip-project-root . "~/projs/PROJECT_DIR")
;;          ;; ignore files bigger than 64k and directory "dist/" when searching
;;          (ffip-find-options . "-not -size +64k -not -iwholename '*/dist/*'")
;;          ;; only search files with following extensions
;;          (ffip-patterns . ("*.html" "*.js" "*.css" "*.java" "*.xml" "*.js"))
;;          (eval . (progn
;;                    (require 'find-file-in-project)
;;                    ;; ignore directory ".tox/" when searching
;;                    (setq ffip-prune-patterns `("*/.tox" ,@ffip-prune-patterns))
                      ;; ignore BMP image file
;;                    (setq ffip-ignore-filenames `("*.bmp" ,@ffip-ignore-filenames))
;;                    ;; Do NOT ignore directory "bin/" when searching
;;                    (setq ffip-prune-patterns `(delete "*/bin" ,@ffip-prune-patterns))))
;;          )))
;;
;; To find in current directory, use `find-file-in-current-directory'
;; and `find-file-in-current-directory-by-selected'.
;;
;; `ffip-fix-file-path-at-point' replaces path at point with correct relative/absolute path.
;;
;; File/directory searching actions are automatically stored into `ffip-find-files-history'.
;; Use `ffip-find-files-resume' to replay any previous action.
;; The maximum number of items of the history is set in `ffip-find-files-history-max-items'.
;;
;; `ffip-show-diff' execute the backend from `ffip-diff-backends'.
;; The output is in Unified Diff Format and inserted into *ffip-diff* buffer.
;; Press "o" or "C-c C-c" or "ENTER" or `M-x ffip-diff-find-file' in the
;; buffer to open corresponding file.  Please note some backends assume that the git cli program
;; is added into environment variable PATH.
;;
;; `ffip-diff-find-file-before-hook' is called in `ffip-diff-find-file'.
;; Two file names are passed to it as parameters.  One name is returned by the hook
;; as the file searching keyword.
;;
;; `ffip-diff-apply-hunk' applies current hunk in `diff-mode' (please note
;; `ffip-diff-mode' inherits from `diff-mode') to the target.
;; file. The target file could be located by searching `recentf-list'.
;; Except this extra feature, `ffip-diff-apply-hunk' is same as `diff-apply-hunk'.
;; So `diff-apply-hunk' can be replaced by `ffip-diff-apply-hunk'.
;;
;; `ffip-diff-filter-hunks-by-file-name' can filter hunks by their file names.
;; User input pattern "regex !exclude1 exclude1" means the hunk's file name does match "regex".
;; But does not match "exclude1" or "exclude2".;
;; Please note in "regex", space represents any string.
;;
;; If you use `evil-mode', insert below code into ~/.emacs,
;;   (defun ffip-diff-mode-hook-setup ()
;;       (evil-local-set-key 'normal "K" 'diff-hunk-prev)
;;       (evil-local-set-key 'normal "J" 'diff-hunk-next)
;;       (evil-local-set-key 'normal "P" 'diff-file-prev)
;;       (evil-local-set-key 'normal "N" 'diff-file-next)
;;       (evil-local-set-key 'normal (kbd "RET") 'ffip-diff-find-file)
;;       (evil-local-set-key 'normal "o" 'ffip-diff-find-file))
;;   (add-hook 'ffip-diff-mode-hook 'ffip-diff-mode-hook-setup)

;; `find-relative-path' find file/directory and copy its relative path
;; into `kill-ring'. You can customize `ffip-find-relative-path-callback'
;; to format the relative path,
;;   (setq ffip-find-relative-path-callback 'ffip-copy-reactjs-import)
;;   (setq ffip-find-relative-path-callback 'ffip-copy-org-file-link)
;;
;; BSD/GNU Find can be installed through Cygwin or MYSYS2 on Windows.
;; Executable is automatically detected. But you can manually specify
;; the executable location by insert below code into ".emacs",
;;
;;   (if (eq system-type 'windows-nt)
;;      (setq ffip-find-executable "c:\\\\cygwin64\\\\bin\\\\find"))
;;
;; This program works on Windows/Cygwin/Linux/macOS
;;
;; See https://github.com/redguardtoo/find-file-in-project for advanced tips.

;;; Code:

(require 'find-lisp)
(require 'diff-mode)
(require 'windmove)
(require 'subr-x)
(require 'ido)

(defgroup ffip nil
  "Find File in Project."
  :group 'convenience)

(defcustom ffip-use-rust-fd nil
  "Use rust fd instead of find."
  :link '(url-link :tag "fd @ GitHub"
                   "https://github.com/sharkdp/fd")
  :group 'ffip
  :type 'boolean
  :safe #'booleanp)


(defcustom ffip-rust-fd-respect-ignore-files t
  "Don't show search results from '.*ignore' files."
  :group 'ffip
  :type 'boolean
  :safe #'booleanp)

(defcustom ffip-project-search-function 'ffip-project-search-default-function
  "Function to execute find program in shell."
  :group 'ffip
  :type 'function
  :safe #'functionp)

(defcustom ffip-prefer-ido-mode nil
  "Prefer `ido-completing-read' to filter file candidates."
  :group 'ffip
  :type 'boolean
  :safe #'booleanp)

(defcustom ffip-rust-fd-extra-opts ""
  "Rust fd extra options passed to cli."
  :group 'ffip
  :type 'string)

(defcustom ffip-diff-find-file-by-file-name-p nil
  "When find file in diff hunk, only search by file name."
  :group 'ffip
  :type 'boolean
  :safe #'booleanp)

(defcustom ffip-strip-file-name-regex
  "\\(\\.mock\\|\\.test\\|\\.mockup\\)"
  "Strip file name to get minimum keyword with this regex.
It's used by `find-file-with-similar-name'."
  :group 'ffip
  :type 'regexp)

(defcustom ffip-find-files-history-max-items 4
  "Maximum number of items stored in `ffip-find-files-history'.
If this number is zero, no item is saved into `ffip-find-files-history'."
  :group 'ffip
  :type 'integer)

(defvar ffip-filename-history nil
  "History of file names provided by users.")

(defvar ffip-find-files-history nil
  "History generated by `ffip-find-files'.")

(defvar ffip-diff-find-file-before-hook nil
  "Hook before `ffip-diff-find-file' move focus out of *ffip-diff* buffer.")

(defvar ffip-read-file-name-hijacked-p nil
  "Internal flag used by `ffip-diff-apply-hunk'.")

(defvar ffip-diff-apply-hunk-hook nil
  "Hook when `ffip-diff-apply-hunk' find the file to apply hunk.
The file path is passed to the hook as the first argument.")

(defvar ffip-relative-path-pattern "^\\(\\.\\.*/\\)+"
  "Pattern of relative path.")

(defun ffip-nonempty-lines (str)
  "Return non empty lines from STR."
  (split-string str "[\r\n]+" t))

(defun ffip-diff-git-versions ()
  "List all versions of code under Git."
  (let* ((cmd1 "git branch --no-color --all")
         (cmd2 (concat "git --no-pager log --date=short --pretty=format:'%h|%ad|%s|%an'" buffer-file-name)))
    (nconc (ffip-nonempty-lines (shell-command-to-string cmd1))
           (ffip-nonempty-lines (shell-command-to-string cmd2)))))

(defun ffip-diff-select-version ()
  "Select a version from git history."
  (let* ((line (completing-read "Select from git history: " (ffip-diff-git-versions)))
         (version (replace-regexp-in-string "^ *\\*? *"
                                            ""
                                            (car (split-string line "|" t)))))
    version))

;;;###autoload
(defun ffip-git-diff-current-file ()
  "Compare another version of current file."
  (let* ((default-directory (locate-dominating-file default-directory ".git")))
    (shell-command-to-string (format "git --no-pager diff %s:%s %s"
                                     (ffip-diff-select-version)
                                     (file-relative-name buffer-file-name default-directory)
                                     buffer-file-name))))

(defun ffip-git-diff-project()
  "Compare another version of project."
  (let* ((default-directory (locate-dominating-file default-directory ".git")))
    (shell-command-to-string (format "git --no-pager diff %s"
                                     (ffip-diff-select-version)))))

(defun ffip-git-diff-directory()
  "Compare another version of current directory."
  (when buffer-file-name
    (let* ((dir (read-directory-name "Directory: " (file-name-directory buffer-file-name)))
           (default-directory (locate-dominating-file default-directory ".git")))
      (shell-command-to-string (format "git --no-pager diff %s -- \"%s\""
                                       (ffip-diff-select-version)
                                       dir)))))

(defun ffip-git-diff-file-extension()
  "Compare another version of files matching file extensions from user input.
File extensions are separated by space character."
  (let* ((patterns (read-string "File extensions (e.g., \"cpp py\" matches C++ and Python code files): "
                                (and buffer-file-name (file-name-extension buffer-file-name)))))
    (when patterns
      (let* ((default-directory (locate-dominating-file default-directory ".git")))
        (setq patterns
              (mapconcat (lambda (s) (format "\"*.%s\"" s)) (split-string patterns " +") " "))
        (shell-command-to-string (format "git --no-pager diff %s -- %s"
                                         (ffip-diff-select-version)
                                         patterns))))))

(defvar ffip-diff-backends
  '(ffip-git-diff-current-file
    ffip-git-diff-project
    ffip-git-diff-directory
    ffip-git-diff-file-extension
    ("`git diff HEAD^` in project" . "cd $(git rev-parse --show-toplevel) && git diff HEAD^")
    ("`git diff --cached` in project" . "cd $(git rev-parse --show-toplevel) && git diff --cached")
    ("`git diff` in project" . "cd $(git rev-parse --show-toplevel) && git diff")
    ("`git diff` current file" . (shell-command-to-string (format "cd $(git rev-parse --show-toplevel) && git diff \"%s\""
                                                                    (buffer-file-name))))
    ;; git option "--cc" shows changes in merge commits
    ("`git log -p` current file" . (shell-command-to-string (format "cd $(git rev-parse --show-toplevel) && git --no-pager log --date=short -p --cc \"%s\""
                                                     (buffer-file-name))))
    ("`git log -S keyword -p` in project" . (shell-command-to-string (format "cd $(git rev-parse --show-toplevel) && git --no-pager log --date=short -S\"%s\" -p --cc"
                                                              (read-string "Git search string: "))))
    ("Diff from `kill-ring'" . (car kill-ring)))
  "The list of back-ends.
If back-end is string, it's run in `shell-command-to-string'.
If it's a function or expression, it'll be executed and return a string.

The output is inserted into *ffip-diff* buffer.")

(defcustom ffip-find-executable nil
  "Path of GNU find.  If nil we will guess."
  :group 'ffip
  :type 'string)

(defcustom ffip-project-file '(".svn" ".hg" ".git")
  "The file/directory used to locate project root.
May be set using .dir-locals.el.  Checks each entry if set to a list."
  :group 'ffip
  :type '(repeat string))

(defcustom ffip-patterns nil
  "List of glob patterns to look for with `find-file-in-project'."
  :group 'ffip
  :type '(repeat string))

(defvar ffip-match-path-instead-of-filename nil
  "Match full path instead of file name.")

;; For "GNU/BSD Find", "*/test/*" matches "./test/" and "./dir/test/"
;;
;; But for "rust fd", only "test/*" matches "./test/" and "./dir/test/";
;; "*/test/*" won't match "./test/" but matches "./dir/test/"
;; Maybe it's a fd bug.
(defcustom ffip-prune-patterns
  '(;; VCS
    "*/.git"
    "*/.svn"
    "*/.cvs"
    "*/.tox"
    "*/.bzr"
    "*/.hg"
    "*/.DS_Store"
    "*/.sass-cache"
    "*/elpy"
    "*/dcache"
    "*/.npm"
    "*/.tmp"
    "*/.idea"
    "*/node_modules"
    "*/bower_components"
    "*/.gradle"
    "*/.cask")
  "Ignored directories(prune patterns)."
  :group 'ffip
  :type '(repeat string))

(defcustom ffip-ignore-filenames
  '(;; VCS
    ;; project misc
    "*.log"
    ;; Ctags
    "tags"
    "TAGS"
    ;; compressed
    "*.tgz"
    "*.gz"
    "*.xz"
    "*.zip"
    "*.tar"
    "*.rar"
    ;; Global/Cscope
    "GTAGS"
    "GPATH"
    "GRTAGS"
    "cscope.files"
    ;; html/javascript/css
    "*bundle.js"
    "*min.js"
    "*min.css"
    ;; Images
    "*.png"
    "*.jpg"
    "*.jpeg"
    "*.gif"
    "*.bmp"
    "*.tiff"
    "*.ico"
    ;; documents
    "*.doc"
    "*.docx"
    "*.xls"
    "*.ppt"
    "*.pdf"
    "*.odt"
    ;; C/C++
    "*.obj"
    "*.so"
    "*.o"
    "*.a"
    "*.ifso"
    "*.tbd"
    "*.dylib"
    "*.lib"
    "*.d"
    "*.dll"
    "*.exe"
    ;; Java
    ".metadata*"
    "*.class"
    "*.war"
    "*.jar"
    ;; Emacs/Vim
    "*flymake"
    "#*#"
    ".#*"
    "*.swp"
    "*~"
    "*.elc"
    ;; Python
    "*.pyc")
  "Ignored file names.  Wildcast is supported."
  :group 'ffip
  :type '(repeat string))

(defcustom ffip-find-options ""
  "Extra options to pass to `find' when using `find-file-in-project'.

Use this to exclude portions of your project: \"-not -regex \\\".*svn.*\\\"\"."
  :group 'ffip
  :type 'string)

(defcustom ffip-find-pre-path-options ""
  "Options for find program.

GNU Find requires '-H', '-L', '-P', '-D' and `-O' appear before first path '.'.
For example, use '-L' to follow symbolic links."
  :group 'ffip
  :type 'string)

(defcustom ffip-project-root nil
  "If non-nil, overrides the project root directory location."
  :group 'ffip
  :type 'string)

(defcustom ffip-project-root-function nil
  "If non-nil, this function is called to determine the project root.
This overrides variable `ffip-project-root' when set."
  :group 'ffip
  :type 'function)

(defvar ffip-debug nil "Print debug information.")

;;;###autoload
(defun ffip-copy-without-change (p)
  "Copy P without change."
  (kill-new p)
  (message "%s => kill-ring" p))

;;;###autoload
(defun ffip-copy-reactjs-import(p)
  "Create ReactJS link from P and copy the result."
  (setq p (format "import str from '%s';" p))
  (kill-new p)
  (message "%s => kill-ring" p))

;;;###autoload
(defun ffip-copy-org-file-link(p)
  "Create org link from P and copy the result."
  (setq p (format "[[file:%s]]" p))
  (kill-new p)
  (message "%s => kill-ring" p))

;;;###autoload
(defcustom ffip-find-relative-path-callback 'ffip-copy-without-change
  "The callback after calling `find-relative-path'."
  :group 'ffip
  :type 'function)

(defun ffip--some (predicate seq)
  "Return if PREDICATE is t for any element of SEQ."
  (let* (elem rlt)
    (while (and (setq elem (car seq))
                (not rlt))
      (setq seq (cdr seq))
      (setq rlt (funcall predicate elem)))
    rlt))

;;;###autoload
(defun ffip-project-root ()
  "Return project root or `default-directory'."
  (let* ((project-root (or ffip-project-root
                           (cond
                            ((functionp ffip-project-root-function)
                             (funcall ffip-project-root-function))
                            ((listp ffip-project-file)
                             (ffip--some (apply-partially 'locate-dominating-file
                                                          default-directory)
                                         ffip-project-file))
                            (t
                             (locate-dominating-file default-directory
                                                     ffip-project-file))))))
    (or (and project-root (file-name-as-directory project-root))
        default-directory)))

(defun ffip--read-file-text (file)
  "Read text from FILE."
  (read (decode-coding-string
         (with-temp-buffer
           (set-buffer-multibyte nil)
           (setq buffer-file-coding-system 'binary)
           (insert-file-contents-literally file)
           (buffer-substring-no-properties (point-min) (point-max))) 'utf-8)))

;;;###autoload
(defun ffip-get-project-root-directory ()
  "Get the full path of project root directory."
  (if ffip-project-root (file-name-as-directory ffip-project-root)
    (ffip-project-root)))

;;;###autoload
(defun ffip-filename-identity (keyword)
  "Return identical KEYWORD."
  keyword)

;;;###autoload
(defun ffip-filename-camelcase-to-dashes (keyword &optional check-only)
  "Convert KEYWORD from camel cased to dash separated.
If CHECK-ONLY is true, only do the check."
  (let* (rlt)
    (cond
     (check-only
      (setq rlt (string-match "^[a-z0-9]+[A-Z][A-Za-z0-9]+$" keyword))
      (if ffip-debug (message "ffip-filename-camelcase-to-dashes called. check-only keyword=%s rlt=%s" keyword rlt)))
     (t
      (let* ((case-fold-search nil))
        ;; case sensitive replace
        (setq rlt (downcase (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1-\\2" keyword))))

      (if (string= rlt (downcase keyword)) (setq rlt nil))

      (if (and rlt ffip-debug) (message "ffip-filename-camelcase-to-dashes called. rlt=%s" rlt))))
    rlt))

;;;###autoload
(defun ffip-filename-dashes-to-camelcase (keyword &optional check-only)
  "Convert KEYWORD from dash separated to camel cased.
If CHECK-ONLY is true, only do the check."
  (let* (rlt)
    (cond
     (check-only
        (setq rlt (string-match "^[A-Za-z0-9]+\\(-[A-Za-z0-9]+\\)+$" keyword))
        (if ffip-debug (message "ffip-filename-dashes-to-camelcase called. check-only keyword=%s rlt=%s" keyword rlt)))
     (t
      (setq rlt (mapconcat (lambda (s) (capitalize s)) (split-string keyword "-") ""))

      (let ((first-char (substring rlt 0 1)))
       (setq rlt (concat "[" first-char (downcase first-char) "]" (substring rlt 1))))
      (if (and rlt ffip-debug) (message "ffip-filename-dashes-to-camelcase called. rlt=%s" rlt))))
    rlt))

(defun ffip--create-filename-pattern-for-gnufind (keyword)
  "Create search pattern from KEYWORD."
  (let* ((rlt ""))
    (cond
     ((not keyword)
      (setq rlt ""))
     (t
      (setq rlt (concat (if ffip-match-path-instead-of-filename "-iwholename" "-iname")
                        " \"*"
                        keyword
                        "*\"" ))))
    (if ffip-debug (message "ffip--create-filename-pattern-for-gnufind called. rlt=%s" rlt))
    rlt))

(defun ffip--win-executable-find (exe)
  "Find EXE on windows."
  (let* ((drivers '("c" "d" "e" "g" "h" "i" "j" "k"))
         (i 0)
         j
         (dirs '(":\\\\cygwin64\\\\bin\\\\"
                 ":\\\\msys64\\\\usr\\\\bin\\\\"))
         rlt)
    (while (and (not rlt)
                (< i (length dirs)))
      (setq j 0)
      (while (and (not rlt)
                  (< j (length drivers)))
        (setq rlt (executable-find (concat (nth j drivers) (nth i dirs) exe)))
        (setq j (1+ j)))
      (setq i (1+ i)))
    (unless rlt
      ;; nothing found, fall back to exe
      (setq rlt exe))
    rlt))

(defun ffip--executable-find ()
  "Find EXE on all environments."
  (let* ((exe (if ffip-use-rust-fd "fd" "find"))
         rlt)
    (cond
     ((file-remote-p default-directory)
      ;; In tramp mode and local windows, remote nix-like,
      ;; the `ffip-find-executable' with windows path can't be applied.
      ;; Assume remote server has already added EXE into $PATH!
      ;; Thanks for ShuguangSun for the fix
      (setq rlt exe))
     ((setq rlt ffip-find-executable))
     ((eq system-type 'windows-nt)
      ;; in case PATH is not setup properly
      (cond
       (ffip-use-rust-fd
        (setq rlt (concat (getenv "USERPROFILE")
                          "\\\\.cargo\\\\bin\\\\"
                          exe
                          ".exe"))
        (unless (file-exists-p rlt)
          (setq rlt exe)))
       (t
        (setq rlt (ffip--win-executable-find exe)))))
     ((setq rlt (executable-find exe)))
     (t
      ;; well, `executable-find' failed
      (setq rlt exe)))
    rlt))

(defun ffip--join-patterns (patterns)
  "Convert PATTERNS into cli arguments."
  (cond
   ((and ffip-patterns (not ffip-use-rust-fd))
    (format "\\( %s \\)" (mapconcat (lambda (pat) (format "-iwholename \"%s\"" pat))
                                    patterns " -or ")))
   (t
    ;; rust fd only supports ONE pattern (and it's regular expression)
    ;; which is precious resource to waste here
    "")))

(defun ffip--prune-patterns ()
  "Turn `ffip-prune-patterns' into a string that `find' can use."
  ;; Both fd and find use "glob pattern"
  ;; @see https://en.wikipedia.org/wiki/Glob_%28programming%29
  (cond
   (ffip-use-rust-fd
    ;; fd match relative path
    (mapconcat (lambda (p)
                 (format "-E \"%s\"" (replace-regexp-in-string "^\*/" "" p)))
               ffip-prune-patterns " "))
   (t
    ;; find match whole path
    (mapconcat (lambda (p)
                 (format "-iwholename \"%s\"" p))
               ffip-prune-patterns " -or "))))

(defun ffip--ignore-file-names ()
  "Turn `ffip-ignore-filenames' into a string that `find' can use."
  ;; @see `ffip-prune-patterns' for fd vs find.
  (cond
   (ffip-use-rust-fd
    (mapconcat (lambda (p)
                 (format "-E \"%s\"" p))
               ffip-ignore-filenames " "))
   (t
    (mapconcat (lambda (n) (format "-not -name \"%s\"" n))
               ffip-ignore-filenames " "))))

;;;###autoload
(defun ffip-completing-read (prompt collection &optional action)
  "Read a string in minibuffer, with completion.

PROMPT is a string with same format parameters in `completing-read'.
COLLECTION is a list of strings.

ACTION is a lambda function to call after selecting a result.

This function returns the selected candidate or nil."
  (let* (selected)
    (cond
     ((= 1 (length collection))
      ;; select the only candidate immediately
      (setq selected (car collection)))

     (ffip-prefer-ido-mode
      ;; ido can only handle list of strings
      (setq selected (ido-completing-read prompt (mapcar 'car collection))))

     (t
      (setq selected (completing-read prompt collection))
      (setq selected (or (assoc selected collection) selected))))

    (when selected
      ;; make sure only the string/file is passed to action
      (let* ((default-directory (ffip-get-project-root-directory))
             (result (if (consp selected) (cdr selected) selected)))
        (if action (funcall action result) result)))))

(defun ffip-create-shell-command (keyword find-directory-p)
  "Produce command to search KEYWORD.
If FIND-DIRECTORY-P is t, we look up directory instead of file.
Rust fd use regular expression.
BSD/GNU Find use glob pattern."
  (let* (cmd fmt tgt)
    (cond
     (ffip-use-rust-fd
      ;; `-H` => search hidden files
      ;; `-E` => exclude pattern
      ;; `-c` => color
      ;; `-i` => case insensitive
      ;; `-t` => directory (d) or file (f)
      ;; `-p` => match full path
      (setq fmt (concat "%s %s -c never -H -i -t %s %s %s %s"
                        (if ffip-rust-fd-respect-ignore-files "" " -I")
                        (if ffip-match-path-instead-of-filename " -p" "")
                        " "
                        ffip-rust-fd-extra-opts
                        " %s"))
      ;; fd use regular expression for target pattern (but glob pattern when excluding, sigh)
      (setq tgt (if keyword (format "\".*%s\"" keyword) "")))
     (t
      (setq tgt
            (if find-directory-p (format "-iwholename \"*%s\"" keyword)
              (ffip--create-filename-pattern-for-gnufind keyword)))
      (setq fmt (concat "%s "
                        ffip-find-pre-path-options
                        " . \\( %s \\) -prune -o -type %s %s %s %s %s -print"))))

    (setq cmd (format fmt
                      (ffip--executable-find)
                      (ffip--prune-patterns)
                      (if find-directory-p "d" "f")
                      (ffip--ignore-file-names)
                      ffip-find-options
                      (ffip--join-patterns ffip-patterns)
                      tgt))
    cmd))

(defun ffip-glob-to-regex (s)
  "Convert glob pattern S into regular expression."
  (setq s (replace-regexp-in-string "\\." "\\\\." s))
  (setq s (replace-regexp-in-string "\*" ".*" s))
  s)

(defmacro ffip-push-one-candidate (file result)
  "Push FILE into RESULT."
  ;; @see https://www.murilopereira.com/how-to-open-a-file-in-emacs/
  ;; also @see #15 improving handling of directories containing space
  `(push (cons (replace-regexp-in-string "^\./" "" ,file) ,file) ,result))

(defun ffip-project-search-default-function (find-command)
  "Execute FIND-COMMAND in shell and split its output into lines."
  (if ffip-debug "ffip-project-search-default-function => find-command=%s" find-command)
  (split-string (shell-command-to-string find-command) "[\r\n]+" t))

;;;###autoload
(defun ffip-project-search (keyword &optional find-directory-p)
  "Return an alist of all filenames in the project and their path.

Files with duplicate filenames are suffixed with the name of the
directory they are found in so that they are unique.

If KEYWORD is string, it's the file name or file path to find file.
If KEYWORD is list, it's the list of file names.
IF FIND-DIRECTORY-P is t, we are searching directories, else files."
  (let* ((default-directory (ffip-get-project-root-directory))
         (cmd (ffip-create-shell-command keyword find-directory-p))
         (fd-file-pattern (concat "^" (mapconcat 'ffip-glob-to-regex ffip-patterns "\\|") "$"))
         (collection (funcall ffip-project-search-function cmd))
         rlt)

    (if ffip-debug (message "run command at %s: %s" default-directory cmd))

    ;; use simple loop statement for clean code
    (cond
     ((and ffip-use-rust-fd ffip-patterns)
      (let* ((fd-file-pattern (concat "^"
                                      (mapconcat 'ffip-glob-to-regex ffip-patterns "\\|")
                                      "$")))
        (dolist (file collection)
          ;; filter result with Lisp because fd does NOT support multiple patterns
          (if (string-match fd-file-pattern file) (ffip-push-one-candidate file rlt)))))

     (t
      (dolist (file collection)
        (ffip-push-one-candidate file rlt))))

    (nreverse rlt)))

(defun ffip--forward-line (lnum)
  "Forward LNUM lines."
  (if ffip-debug (message "ffip--forward-line called => %s" lnum))
  (when (and lnum (> lnum 0))
    (goto-char (point-min))
    (forward-line (1- lnum))))

(defun ffip-hint ()
  "Hint."
  (let ((root (ffip-get-project-root-directory)))
    (format "Find in %s/: "
            (file-name-nondirectory (directory-file-name root)))))

(defun ffip-select-and-open-file (files new-window-p directory-p fn lnum)
  "Select and open file from FILES.
If NEW-WINDOW-P is t, create a new window for opened file.
If DIRECTORY-P is t, open directory instead of file.
IF the function FN is not nil, call it after opening the selected file.
After opening the file, forward LNUM lines."
  (ffip-completing-read
   (ffip-hint)
   files
   `(lambda (file)
      ;; only one item in project files
      (if ,directory-p
          (if (quote ,new-window-p)
              (dired-other-window file)
            (switch-to-buffer (dired file)))
        ;; open file
        (if (quote ,new-window-p)
            (find-file-other-window file)
          (find-file file))
        ;; goto line if needed
        (ffip--forward-line ,lnum)
        (if ,fn (funcall ,fn file))))))

;;;###autoload
(defun ffip-find-files (keyword open-another-window &optional find-directory-p fn)
  "Use KEYWORD to find files.
If OPEN-ANOTHER-WINDOW is t, the results are displayed in a new window.
If FIND-DIRECTORY-P is t, only search directories.  FN is callback.
This function is the API to find files."
  (let* (cands lnum file)
    ;; extract line num if exists
    (when (and keyword (stringp keyword)
               (string-match "^\\(.*\\):\\([0-9]+\\):?$" keyword))
      (setq lnum (string-to-number (match-string 2 keyword)))
      (setq keyword (match-string 1 keyword)))

    (setq cands (ffip-project-search keyword find-directory-p))
    (cond
     ((> (length cands) 0)

      (unless (eq ffip-find-files-history-max-items 0)
        ;; save the history
        (when (>= (length ffip-find-files-history)
                  ffip-find-files-history-max-items)
          ;; kick out the oldest item
          (setq ffip-find-files-history (butlast ffip-find-files-history)))
        ;; add the latest item
        (push (list :files cands
                    :keyword keyword
                    :directory-p find-directory-p
                    :function fn
                    :forward-lines lnum
                    :default-directory (ffip-get-project-root-directory))
              ffip-find-files-history))

      (ffip-select-and-open-file cands
                                 open-another-window
                                 find-directory-p
                                 fn
                                 lnum))

     (t
      (message "Nothing found!")))))

;;;###autoload
(defun ffip-find-files-resume (&optional n)
  "Resume the last Nth `ffip-find-file' operation.
Please note N is zero originated."
  (interactive "P")
  (unless n (setq n 0))
  (cond
   ((>= n ffip-find-files-history-max-items)
    (message "There are only %d items in `ffip-find-files-history'."
             ffip-find-files-history-max-items))
   (t
    (let* ((item (nth n ffip-find-files-history))
           (default-directory (plist-get item :default-directory)))
      (ffip-select-and-open-file (plist-get item :files)
                                 nil
                                 (plist-get item :directory-p)
                                 (plist-get item :function)
                                 (plist-get item :forward-lines))))))

(defun ffip--prepare-root-data-for-project-file (root)
  "Prepare data for ROOT."
  (cons 'ffip-project-root root))

(defun ffip--read-selected ()
  "Read select string."
  (buffer-substring-no-properties (region-beginning) (region-end)))

(defun ffip-read-keyword ()
  "Read keyword from selected text or user input."
  (let* ((hint (if ffip-use-rust-fd "Enter regex (or press ENTER): "
                 "Enter keyword (or press ENTER): "))
         rlt)
    (cond
     ((region-active-p)
      (setq ffip-filename-history (add-to-list 'ffip-filename-history
                                               (ffip--read-selected)))
      (setq rlt (ffip--read-selected)))
     (t
      (setq rlt (read-from-minibuffer hint nil nil nil 'ffip-filename-history))))
    (if rlt (string-trim rlt) rlt)))

;;;###autoload
(defun ffip-create-project-file ()
  "Create or Append .dir-locals.el to set up per directory.
You can move .dir-locals.el to root directory.
See (info \"(Emacs) Directory Variables\") for details."
  (interactive)
  (let* ((root (read-directory-name "Project root directory: " default-directory))
         (file (if (and root (file-exists-p root))
                   (concat (file-name-as-directory root) ".dir-locals.el"))))
    (when file
      (with-temp-buffer
        (let ((print-level nil)  (print-length nil) sexp (rlt '(a)))
          (cond
           ;; modify existing .dir-locals.el
           ((file-exists-p file)
            (let (sub-sexp new-sub-sexp)
              (setq sexp (ffip--read-file-text file))
              ;; valid .dir-locals.el
              (when sexp
                ;; the list for nil
                (setq sub-sexp (assoc nil sexp))
                (cond
                 ;; `(nil (prop1 . val1) (prop2 . val2))' exists
                 (sub-sexp
                  ;; remove (ffip-project-root . "/path/file")
                  (if (assoc 'ffip-project-root sub-sexp)
                      (setq new-sub-sexp (delete (assoc 'ffip-project-root sub-sexp) sub-sexp))
                    (setq new-sub-sexp sub-sexp))
                  (add-to-list 'new-sub-sexp (ffip--prepare-root-data-for-project-file root) t)
                  ;; update sexp
                  (setq sexp (delete sub-sexp sexp))
                  (add-to-list 'sexp new-sub-sexp))
                 (t
                  ;; add `(nil (ffip-project-root . "path/file"))'
                  (add-to-list 'sexp (list nil (ffip--prepare-root-data-for-project-file root))))))
              ))
           (t
            ;; a new .dir-locals.el
            (setq sexp (list (list nil (ffip--prepare-root-data-for-project-file root))))))
          (when sexp
            (insert (format "%S" sexp))
            (write-file file)
            (message "%s created." file)))))))

;;;###autoload
(defun ffip-current-full-filename-match-pattern-p (regex)
  "Is current full file name (including directory) match the REGEX?"
  (let* ((dir (if (buffer-file-name) (buffer-file-name) "")))
    (string-match-p regex dir)))

;;;###autoload
(defun find-file-in-project (&optional open-another-window)
"More powerful and efficient `find-file-in-project-by-selected' is recommended.

Prompt with a completing list of all files in the project to find one.
If OPEN-ANOTHER-WINDOW is not nil, the file will be opened in new window.
The project's scope is defined as the first directory containing
a `ffip-project-file' whose value is \".git\" by default.
You can override this by setting the variable `ffip-project-root'."
  (interactive "P")
  (ffip-find-files nil open-another-window))

(defun ffip-file-name-relative-p (filename)
  "Is FILENAME relative?"
  (if (string-match-p ffip-relative-path-pattern filename) t))

(defun ffip-guess-file-name-at-point ()
  "Guess file name at point."
  (or (and (region-active-p) (ffip--read-selected))
      (thing-at-point 'filename)
      (thing-at-point 'symbol)
      (read-string "No file name at point. Please provide one: ")))

(defun ffip--guess-physical-path (file)
  "Return physical full path of FILE which does exist."
  (let* (rlt tmp)
    ;; only deal with file path
    (when (or (file-name-absolute-p file)
              (ffip-file-name-relative-p file))
      (cond
       ;; file already exists
       ((and (file-exists-p file)
             ;; not directory
             (not (car (file-attributes file))))
        (setq rlt (file-truename file)))

       ;; extra effort for javascript like language
       ;; "./lib/A" could mean "./lib/A.js" or "./lib/A/index.js"
       ((and (or (derived-mode-p 'js-mode)
                 (memq major-mode '(typescript-mode)))
             (string-match-p "^[^.]*$"(file-name-nondirectory file)))
        (dolist (ext '(".ts" ".js"))
          ;; guess physical path
          (cond
           ;; "./lib/A.js" or "./lib/A.ts"
           ((file-exists-p (setq tmp (concat file ext)))
            (setq rlt (file-truename tmp)))

           ;; "./lib/A/index.js" or "./lib/A/index.ts"
           ((file-exists-p (setq tmp (concat (file-name-as-directory file) "index" ext)))
            (setq rlt (file-truename tmp))))))))

    rlt))

;;;###autoload
(defun find-file-in-project-at-point (&optional open-another-window)
  "Find file whose name is guessed around point.
If OPEN-ANOTHER-WINDOW is not nil, the file will be opened in new window."
  (interactive "P")
  (let* ((fn (ffip-guess-file-name-at-point))
         ;; could be a path
         (ffip-match-path-instead-of-filename t)
         full-path)
    (cond
     (fn
      (cond
       ;; is relative/full path and path is real
       ((setq full-path (ffip--guess-physical-path fn))
        (if open-another-window (find-file-other-window full-path)
          (find-file full-path)))

       ;; absolute path which does not exist
       ((file-name-absolute-p fn)
        ;; search file name only
        (let* ((ffip-match-path-instead-of-filename nil))
          (ffip-find-files (file-name-nondirectory fn) open-another-window)))

       (t
        ;; strip prefix "../../" or "././" from file name
        (ffip-find-files (replace-regexp-in-string ffip-relative-path-pattern "" fn)
                         open-another-window))))
     (t
      (message "No file name is provided.")))))

(defun ffip-parent-directory (level directory)
  "Return LEVEL up parent directory of DIRECTORY."
  (let* ((rlt directory))
    (while (and (> level 0) (not (string= "" rlt)))
      (setq rlt (file-name-directory (directory-file-name rlt)))
      (setq level (1- level)))
    (if (string= "" rlt) (setq rlt nil))
    rlt))

;;;###autoload
(defun find-file-in-current-directory (&optional level)
  "Search file in current directory or LEVEL up parent directory."
  (interactive "P")
  (unless level (setq level 0))
  (let* ((ffip-project-root (ffip-parent-directory level default-directory)))
    (find-file-in-project nil)))

;;;###autoload
(defun find-file-in-project-by-selected (&optional open-another-window)
  "Same as `find-file-in-project' but more powerful and faster.
It use string from selected region to search files in the project.
If no region is selected, you could provide a keyword.

Keyword could be ANY part of the file's full path and support wildcard.
For example, to find /home/john/proj1/test.js, below keywords are valid:
- test.js
- roj1/tes
- john*test

If keyword contains line number like \"hello.txt:32\" or \"hello.txt:32:\",
we will move to that line in opened file.

If keyword is empty, it behaves same as `find-file-in-project'.

If OPEN-ANOTHER-WINDOW is not nil, the file will be opened in new window."
  (interactive "P")
  (ffip-find-files (ffip-read-keyword) open-another-window))

;;;###autoload
(defun ffip-insert-file ()
  "Insert contents of file in current buffer.
The file name is selected interactively from candidates in project."
  (interactive)
  (let* ((cands (ffip-project-search (ffip-read-keyword)))
         root)
    (when (> (length cands) 0)
      (ffip-completing-read (ffip-hint)
                            cands
                            'insert-file))))

;;;###autoload
(defun find-file-with-similar-name (&optional open-another-window)
  "Use base name of current file as keyword which could be further stripped.
by `ffip-strip-file-name-regex'.
If OPEN-ANOTHER-WINDOW is not nil, the file will be opened in new window."
  (interactive "P")
  (when buffer-file-name
    (let* ((keyword (concat (file-name-base buffer-file-name) ".*") ))
      (if ffip-strip-file-name-regex
          (setq keyword (replace-regexp-in-string ffip-strip-file-name-regex
                                                  ""
                                                  keyword)))
      (ffip-find-files keyword open-another-window))))

;;;###autoload
(defun find-file-in-current-directory-by-selected (&optional open-another-window)
  "Like `find-file-in-project-by-selected' but search current directory.
If OPEN-ANOTHER-WINDOW is not nil, the file will be opened in new window."
  (interactive "P")
  (let* ((ffip-project-root default-directory))
    (find-file-in-project-by-selected open-another-window)))

;;;###autoload
(defun ffip-find-relative-path(&optional find-directory-p)
  "Find file/directory and copy its relative path into `kill-ring'.
If FIND-DIRECTORY-P is t, copy the directory path.

Set `ffip-find-relative-path-callback' to format the result,
  (setq ffip-find-relative-path-callback 'ffip-copy-reactjs-import)
  (setq ffip-find-relative-path-callback 'ffip-copy-org-file-link)"
  (interactive "P")
  (let* ((cands (ffip-project-search (ffip-read-keyword) find-directory-p)))
    (cond
     ((> (length cands) 0)
      (ffip-completing-read
       (ffip-hint)
       cands
       `(lambda (file)
          ;; only one item in project files
          (if ,find-directory-p
              (setq file (file-name-as-directory file)))
              (setq file (file-relative-name file
                                             (if buffer-file-name
                                                 (file-name-directory buffer-file-name)
                                               (expand-file-name default-directory))))
              (funcall ffip-find-relative-path-callback file))))
     (t
      (message "Nothing found!")))))

;;;###autoload
(defun find-directory-in-project-by-selected (&optional open-another-window)
  "Similar to `find-file-in-project-by-selected'.
Use string from selected region to find directory in the project.
If no region is selected, you need provide keyword.

Keyword could be directory's base-name only or parent-directory+base-name
For example, to find /home/john/proj1/test, below keywords are valid:
- test
- roj1/test
- john*test

If OPEN-ANOTHER-WINDOW is not nil, the file will be opened in new window."
  (interactive "P")
  (ffip-find-files (ffip-read-keyword) open-another-window t))

(defun ffip--prune-patterns-regex ()
  "Convert `ffip--prune-patterns-regex to regex."
  (let* ((rlt (mapconcat 'identity ffip-prune-patterns "\\|")))
    (setq rlt (replace-regexp-in-string "\\." "\\\\." rlt))
    (setq rlt (replace-regexp-in-string "\\*" ".*" rlt))
    ;; file name or directory name
    (concat rlt "\\($\\|/\\)" )))

;;;###autoload
(defun ffip-lisp-find-file-in-project (&optional directory-p)
  "If DIRECTORY-P is nil, find file in project, or else find directory.
This command works in any environment (Windows, etc) out of box."
  (interactive "P")
  (let* ((root (ffip-get-project-root-directory))
         (input-regex (read-string "Input regex (or press ENTER): "))
         (find-lisp-regexp (if (string= input-regex "") ".*" input-regex))
         cands
         (ignored-regex (ffip--prune-patterns-regex)))
    (cond
     (directory-p
      (setq cands (find-lisp-find-files-internal
                  root
                  'find-lisp-file-predicate-is-directory
                  'find-lisp-default-directory-predicate)))
     (t
      (setq cands (find-lisp-find-files-internal
                   root
                   'find-lisp-default-file-predicate
                   'find-lisp-default-directory-predicate))))
    (setq cands
          (delq nil
                (mapcar `(lambda (c)
                           (unless (string-match ,ignored-regex c) c))
                        cands)))
    (ffip-completing-read
     (format "%s %s: " (if directory-p "directories" "files") root)
     cands
     `(lambda (file)
        (if ,directory-p
            (switch-to-buffer (dired file))
          (find-file file))))))

;;;###autoload
(defalias 'ffip 'find-file-in-project)
;;;###autoload
(defalias 'find-relative-path 'ffip-find-relative-path)

(defun ffip-path (candidate)
  "Get path from CANDIDATE."
  (let* ((default-directory (ffip-project-root)))
    (file-truename (if (consp candidate) (cdr candidate) candidate))))

;;;###autoload
(defun ffip-diff-quit ()
  "Quit."
  (interactive)
  ;; kill buffer instead of bury it
  (quit-window t))

;;;###autoload
(defun ffip-diff-find-file (&optional open-another-window)
  "File file(s) in current hunk.
If OPEN-ANOTHER-WINDOW is not nil, the file will be opened in new window."
  (interactive "P")
  (let* ((files (mapcar (lambda (f) (replace-regexp-in-string "^[^/]*/" "" f)) (diff-hunk-file-names)))
         (alnum 0)
         (blnum 0)
         (regex "\\(?:\\*\\{15\\}.*\n\\)?[-@* ]*\\([0-9,]+\\)\\([ acd+]+\\([0-9,]+\\)\\)?")
         (ffip-match-path-instead-of-filename (not ffip-diff-find-file-by-file-name-p))
         keyword)

    (save-excursion
      (diff-beginning-of-hunk t)
      (when (looking-at regex)
        (setq alnum (string-to-number (match-string 1)))
        (setq blnum (string-to-number (match-string 3)))))

    (cond
     ((or (null files) (eq (length files) 0))
      (message "No file is found!"))

     ;; file A and file B have the same name
     ((or (and (> (length files) 1)
               (string= (nth 0 files) (nth 1 files)))
          (eq (length files) 1))
      (setq keyword (nth 0 files))
      (when ffip-diff-find-file-by-file-name-p
        (setq keyword (file-name-nondirectory keyword)))

      (ffip-find-files keyword
                       open-another-window
                       nil
                       `(lambda (opened-file)
                          ;; use line number in new file since there
                          ;; is only one file name candidate
                          (ffip--forward-line ,blnum))))

     (t
      (unless (setq keyword (run-hook-with-args 'ffip-diff-find-file-before-hook
                                                (nth 0 files)
                                                (nth 1 files)))
        ;; pick a file name from A and B
        (setq keyword (cond
                       ((string= (nth 0 files) "null")
                        (nth 1 files))
                       (t
                        (nth 0 files)))))
      (when ffip-diff-find-file-by-file-name-p
        (setq keyword (file-name-nondirectory keyword)))

      (ffip-find-files keyword
                       open-another-window
                       nil
                       (lambda (opened-file)
                         (cond
                          ((string= (file-name-nondirectory opened-file) (nth 0 files))
                           (ffip--forward-line alnum))
                          (t
                           (ffip--forward-line blnum)))))))))

(defvar ffip-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map diff-mode-map)
    (define-key map (kbd "C-c C-k") 'ffip-diff-filter-hunks-by-file-name)
    (define-key map [remap diff-goto-source] 'ffip-diff-find-file)
    map)
  "Mode map based on `diff-mode-map'.")

;;;###autoload
(define-derived-mode ffip-diff-mode diff-mode "ffip"
  "Show diff/patch."
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (use-local-map ffip-diff-mode-map))

(defun ffip-show-content-in-diff-mode (content)
  "Insert CONTENT into *ffip-diff* buffer."
  (cond
   ((and content (not (string= content "")))
    (let (rlt-buf)
      (if (get-buffer "*ffip-diff*")
          (kill-buffer "*ffip-diff*"))
      (setq rlt-buf (get-buffer-create "*ffip-diff*"))
      (save-current-buffer
        (switch-to-buffer-other-window rlt-buf)
        (set-buffer rlt-buf)
        (erase-buffer)
        (insert content)
        (ffip-diff-mode)
        (goto-char (point-min)))))
   (t
    (message "Output is empty!"))))

(defun ffip-diff-execute-backend (backend)
  "Execute BACKEND."
  (if backend
      (cond
       ;; shell command
       ((stringp backend)
        (ffip-show-content-in-diff-mode (shell-command-to-string backend)))
       ;; command
       ((functionp backend)
        (ffip-show-content-in-diff-mode (funcall backend)))
       ;; lisp expression
       ((consp backend)
        (ffip-show-content-in-diff-mode (funcall `(lambda () ,backend)))))))

(defun ffip-backend-description (backend)
  "Get BACKEND description."
  (let* (rlt)
    (cond
     ;; shell command
     ((stringp backend)
      (setq rlt backend))
     ;; command
     ((functionp backend)
      (setq rlt (symbol-name backend)))
     ;; lisp expression
     ((consp backend)
      ;; (cons "description" actual-backend)
      (if (stringp (car backend))
          (setq rlt (car backend))
        (setq rlt "unknown"))))
    rlt))

;;;###autoload
(defun ffip-show-diff-internal (&optional num)
  "Show the diff output by executing selected `ffip-diff-backends'.
NUM is the index selected backend from `ffip-diff-backends'.
NUM is zero based whose default value is zero."
  (interactive "P")
  (cond
   ((or (not num) (< num 0))
    (setq num 0))
   ((> num (length ffip-diff-backends))
    (setq num (1- (length ffip-diff-backends)))))

  (let* ((backend (nth num ffip-diff-backends)))
    (if (and (consp backend)
             (stringp (car backend)))
        (setq backend (cdr backend)))
    (ffip-diff-execute-backend backend)))

;;;###autoload
(defun ffip-show-diff-by-description (&optional num)
  "Show the diff output by executing selected `ffip-diff-backends'.
NUM is the backend index of `ffip-diff-backends'.
If NUM is not nil, the corresponding backend is executed directly."
  (interactive "P")
  (cond
   (num
    (ffip-show-diff-internal num))
   (t
    (let* (descriptions
           (i 0))
      ;; format backend descriptions
      (dolist (b ffip-diff-backends)
        (add-to-list 'descriptions
                     (format "%s: %s"
                             i
                             (ffip-backend-description b)) t)
        (setq i (+ 1 i)))
      (ffip-completing-read
       "Run diff backend: "
       descriptions
       (lambda (file)
         (if (string-match "^\\([0-9]+\\): " file)
             (ffip-show-diff-internal (string-to-number (match-string 1 file))))))))))

;;;###autoload
(defalias 'ffip-show-diff 'ffip-show-diff-by-description)

(defadvice read-file-name (around ffip-read-file-name-hack activate)
  "Advice `read-file-name'."
  (cond
   (ffip-read-file-name-hijacked-p
    ;; only hack read-file-name once
    (setq ffip-read-file-name-hijacked-p nil)
    (let* ((args (ad-get-args 0))
           (file-name (file-name-nondirectory (nth 2 args)))
           (default-directory (ffip-project-root))
           (cands (ffip-project-search file-name))
           (rlt (if cands (ffip-completing-read "Files: " cands))))
      (when rlt
        (setq rlt (file-truename rlt))
        (run-hook-with-args 'ffip-diff-apply-hunk-hook rlt)
        (setq ad-return-value rlt))))
   (t
    ad-do-it)))

;;;###autoload
(defun ffip-diff-apply-hunk (&optional reverse)
  "Apply current hunk in `diff-mode'.  Try to locate the file to patch.
Similar to `diff-apply-hunk' but smarter.
Please read documentation of `diff-apply-hunk' to get more details.
If REVERSE is t, applied patch is reverted."
  (interactive "P")
  (cond
   ((derived-mode-p 'diff-mode)
    (setq ffip-read-file-name-hijacked-p t)
    (diff-apply-hunk reverse)
    (setq ffip-read-file-name-hijacked-p nil))
   (t
    (message "This command only run in `diff-mode' and `ffip-diff-mode'."))))

(defun ffip-diff-hunk-file-name-match-p (keyword neg-keywords)
  "Current hunk's file name does match KEYWORD and doesn't NEG-KEYWORDS."
  (let* ((filenames (diff-hunk-file-names))
         (f0 (nth 0 filenames))
         (f1 (nth 1 filenames))
         rlt)
    (when (and filenames (> (length filenames) 1))
      (setq rlt (or (string-match keyword f0) (string-match keyword f1)))

      (let ((i 0) nk)
        (while (and rlt (< i (length neg-keywords)))
          (setq nk (nth i neg-keywords))
          (setq rlt (not (or (string-match nk f0) (string-match nk f1))))
          (setq i (1+ i)))))
    rlt))

;;;###autoload
(defun ffip-diff-filter-hunks-by-file-name ()
  "Filter hunks by file names which are generated from user input patterns.
E.g., \"regex !exclude1 exclude1\" means the hunk's file name should
match \"regex\", but should not match \"exclude1\" or \"exclude2\".
Please note in \"regex\", space represents any string."
  (interactive)
  (cond
   ((derived-mode-p 'diff-mode)
    (let* ((pattern (read-string "File pattern (e.g., \"regex !exclude1 exclude2\"): "))
           arr
           keyword
           neg-keywords
           (first-hunk-position (save-excursion
                                  (goto-char (point-min))
                                  (re-search-forward diff-hunk-header-re)
                                  (line-end-position))))
      (cond
       ((and pattern (not (string= pattern "")))
        (setq arr (split-string pattern "!"))
        (setq keyword (string-trim (nth 0 arr)))
        (when (> (length arr) 1)
          (setq neg-keywords (split-string (string-trim (nth 1 arr)) " +")))

        ;; kill from the bottom to the top
        (goto-char (point-max))
        (condition-case nil
            (while (not (and (<= (point) first-hunk-position)
                             (ffip-diff-hunk-file-name-match-p keyword neg-keywords)))
              (cond
               ((not (ffip-diff-hunk-file-name-match-p keyword neg-keywords))
                (diff-file-kill))
               (t
                (diff-file-prev))))
          (error nil)))
       (t
        (message "File kill pattern should not be empty")))))
   (t
    (message "This command only run in `diff-mode' and `ffip-diff-mode'."))))

;;;###autoload
(defun ffip-fix-file-path-at-point (&optional absolute-path-p)
  "Fix file path at point.
If ABSOLUTE-PATH-P is t, old path is replaced by correct absolute path.
Or else it's replaced by relative path."
  (interactive "P")
  (let* ((filename (ffip-guess-file-name-at-point))
         full-path
         cands)
    (cond
     ((not filename)
      (message "There is no file path at point."))

     ;; path at point is a path of physical file
     ((setq full-path (ffip--guess-physical-path filename))
      nil)

     ;; find a file
     ((setq cands
            (ffip-project-search (replace-regexp-in-string ffip-relative-path-pattern
                                                           ""
                                                           filename)))
      (cond
       ((eq (length cands) 1)
        (setq full-path (nth 0 cands)))
       (t
        (setq full-path (completing-read "Find file: " cands))))))

    (when full-path
      (if (consp full-path) (setq full-path (cdr full-path)))

      ;; convert to actual full path
      (let* ((default-directory (ffip-get-project-root-directory)))
        (setq full-path (file-truename full-path)))

      (let* ((bounds (bounds-of-thing-at-point 'filename))
             (path (if absolute-path-p full-path
                     (file-relative-name full-path
                                         ;; compare with current file's directory if possible
                                         (and buffer-file-name (file-name-directory buffer-file-name))))))
        (goto-char (car bounds))
        (delete-region (car bounds) (cdr bounds))
        (insert (replace-regexp-in-string "/index\\.[jt]s" "" path))))))

;; safe locals
(progn
  (put 'ffip-diff-backends 'safe-local-variable 'listp)
  (put 'ffip-patterns 'safe-local-variable 'listp)
  (put 'ffip-prune-patterns 'safe-local-variable 'listp)
  (put 'ffip-ignore-filenames 'safe-local-variable 'listp)
  (put 'ffip-match-path-instead-of-filename 'safe-local-variable 'booleanp)
  (put 'ffip-project-file 'safe-local-variable 'stringp)
  (put 'ffip-strip-file-name-regex 'safe-local-variable 'stringp)
  (put 'ffip-project-root 'safe-local-variable 'stringp))

(provide 'find-file-in-project)
;;; find-file-in-project.el ends here
