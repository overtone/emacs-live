;;; find-file-in-project.el --- Find files in a project quickly, on any OS

;; Copyright (C) 2006-2009, 2011-2012, 2015
;;   Phil Hagelberg, Doug Alcorn, and Will Farrington
;;
;; Version: 4.8
;; Author: Phil Hagelberg, Doug Alcorn, and Will Farrington
;; Maintainer: Chen Bin <chenbin.sh@gmail.com>
;; URL: https://github.com/technomancy/find-file-in-project
;; Package-Requires: ((swiper "0.7.0") (emacs "24.3"))
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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This program provides a couple methods for quickly finding any file
;; in a given project.  It depends on GNU find.
;;
;; Usage,
;;   - `M-x find-file-in-project-by-selected' use the selected region
;;      as the keyword to search file.  Or you need provide the keyword
;;      if no region selected.
;;   - `M-x find-directory-in-project-by-selected' use the select region
;;      to find directory.  Or you need provide the keyword if no region
;;      selected.
;;   - `M-x find-file-in-project' will start search file immediately
;;   - `M-x ffip-create-project-file' create .dir-locals.el
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

;; The variable `ffip-filename-rules' create some extra file names for
;; search when calling `find-file-in-project-by-selected'. For example,
;; When file basename `helloWorld' provided, `HelloWorld', `hello-world'
;; are added as the file name search patterns.
;; `C-h v ffip-filename-rules' to see its default value.
;;
;; All these variables may be overridden on a per-directory basis in
;; your .dir-locals.el.  See (info "(Emacs) Directory Variables") for
;; details.

;; To find in *current directory*, use `find-file-in-current-directory'
;; and `find-file-in-current-directory-by-selected'.

;; ivy-mode is used for filter/search UI
;; In ivy-mode, SPACE is translated to regex ".*".
;; For example, the search string "dec fun pro" is transformed into
;; a regex "\\(dec\\).*\\(fun\\).*\\(pro\\)"
;; `C-h i g (ivy)' for more key-binding tips.
;;
;; You switch to ido-mode by `(setq ffip-prefer-ido-mode t)'

;; GNU Find can be installed,
;;   - through `Brew' on OS X
;;   - through `Cygwin' or `MYSYS2' on Windows.
;; Find executable will be automatically detected. But you can manually
;; specify the executable location by insert below code into ~/.emacs,
;;
;;   (if (eq system-type 'windows-nt)
;;      (setq ffip-find-executable "c:\\\\cygwin64\\\\bin\\\\find")
;; This program works on Windows/Cygwin/Linux/Mac Emacs.
;;
;; Windows setup is as easy as installing Cygwin into default directory on
;; ANY driver. That's all.
;;
;; See https://github.com/technomancy/find-file-in-project for advanced tips

;; Recommended binding: (global-set-key (kbd "C-x f") 'find-file-in-project)

;;; Code:

(require 'ivy)

(defvar ffip-filename-rules
  '(ffip-filename-identity
    (ffip-filename-dashes-to-camelcase ffip-filename-camelcase-to-dashes)))

(defvar ffip-find-executable nil "Path of GNU find.  If nil, we will find `find' path automatically.")

(defvar ffip-project-file '(".svn" ".git" ".hg")
  "The file that should be used to define a project root.
May be set using .dir-locals.el.  Checks each entry if set to a list.")

(defvar ffip-prefer-ido-mode nil
  "Use `ido-mode' instead of `ivy-mode' for displaying candidates.")

(defvar ffip-patterns nil
  "List of patterns to look for with `find-file-in-project'.")

(defvar ffip-match-path-instead-of-filename nil
  "Match full path instead of file name when calling `find-file-in-project-by-selected'")

(defvar ffip-prune-patterns
  '(;; VCS
    "*/.git/*"
    "*/.svn/*"
    "*/.cvs/*"
    "*/.bzr/*"
    "*/.hg/*"
    ;; project misc
    "*.log"
    "*/bin/*"
    ;; Mac
    "*/.DS_Store/*"
    ;; Ctags
    "*/tags"
    "*/TAGS"
    ;; Global/Cscope
    "*/GTAGS"
    "*/GPATH"
    "*/GRTAGS"
    "*/cscope.files"
    ;; html/javascript/css
    "*/.npm/*"
    "*/.idea/*"
    "*min.js"
    "*min.css"
    "*/node_modules/*"
    "*/bower_components/*"
    ;; Images
    "*.png"
    "*.jpg"
    "*.jpeg"
    "*.gif"
    "*.bmp"
    "*.tiff"
    ;; documents
    "*.doc"
    "*.docx"
    "*.pdf"
    ;; C/C++
    "*.obj"
    "*.o"
    "*.a"
    "*.dylib"
    "*.lib"
    "*.d"
    "*.dll"
    "*.exe"
    ;; Java
    "*/.metadata*"
    "*/.gradle/*"
    "*.class"
    "*.war"
    "*.jar"
    ;; Emacs/Vim
    "*flymake"
    "*/#*#"
    ".#*"
    "*.swp"
    "*~"
    "*.elc"
    "*/.cask/*"
    ;; Python
    "*.pyc")
  "List of directory/file patterns to not descend into when listing files in `find-file-in-project'.")

(defvar ffip-find-options ""
  "Extra options to pass to `find' when using `find-file-in-project'.

Use this to exclude portions of your project: \"-not -regex \\\".*svn.*\\\"\".")

(defvar ffip-project-root nil
  "If non-nil, overrides the project root directory location.")

(defvar ffip-project-root-function nil
  "If non-nil, this function is called to determine the project root.

This overrides variable `ffip-project-root' when set.")

(defvar ffip-full-paths t
  "If non-nil, show fully project-relative paths.")

(defvar ffip-debug nil "Print debug information.")

;;;###autoload
(defun ffip-project-root ()
  "Return the root of the project."
  (let ((project-root (or ffip-project-root
                          (if (functionp ffip-project-root-function)
                              (funcall ffip-project-root-function)
                            (if (listp ffip-project-file)
                                (cl-some (apply-partially 'locate-dominating-file
                                                       default-directory)
                                      ffip-project-file)
                              (locate-dominating-file default-directory
                                                      ffip-project-file))))))
    (or project-root
        (progn (message "No project was defined for the current file.")
               nil))))

(defun ffip--find-rule-to-execute (keyword f)
  "If F is a function, return it.

If F is a list, assume each element is a function.
Run each element with keyword as 1st parameter as KEYWORD and 2nd parameter as t.
If the result is true, return the function."
  (let (rlt found fn)
    (cond
     ((functionp f) (setq rlt f))

     ((listp f)
      (while (and f (not found))
        (setq fn (car f))
        (if (funcall fn keyword t)
            (setq found t)
          (setq f (cdr f))))
      (setq rlt (if found fn 'identity)))

     (t (setq rlt 'identity)))

    rlt))


;;;###autoload
(defun ffip-filename-identity (keyword)
  "Return identical KEYWORD."
  keyword)

;;;###autoload
(defun ffip-filename-camelcase-to-dashes (keyword &optional check-only)
  "Convert KEYWORD from camel cased to dash seperated.
If CHECK-ONLY is true, only do the check."
  (let (rlt
        (old-flag case-fold-search))
    (cond
     (check-only
      (setq rlt (string-match "^[a-z0-9]+[A-Z][A-Za-z0-9]+$" keyword))
      (if ffip-debug (message "ffip-filename-camelcase-to-dashes called. check-only keyword=%s rlt=%s" keyword rlt)))
     (t
      (setq case-fold-search nil)
      ;; case sensitive replace
      (setq rlt (downcase (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1-\\2" keyword)))
      (setq case-fold-search old-flag)

      (if (string= rlt (downcase keyword)) (setq rlt nil))

      (if (and rlt ffip-debug)
          (message "ffip-filename-camelcase-to-dashes called. rlt=%s" rlt))))
    rlt))

;;;###autoload
(defun ffip-filename-dashes-to-camelcase (keyword &optional check-only)
  "Convert KEYWORD from dash seperated to camel cased.
If CHECK-ONLY is true, only do the check."
  (let (rlt)
    (cond
     (check-only
        (setq rlt (string-match "^[A-Za-z0-9]+\\(-[A-Za-z0-9]+\\)+$" keyword))
        (if ffip-debug (message "ffip-filename-dashes-to-camelcase called. check-only keyword=%s rlt=%s" keyword rlt)))
     (t
      (setq rlt (mapconcat (lambda (s) (capitalize s)) (split-string keyword "-") ""))

      (let ((first-char (substring rlt 0 1)))
       (setq rlt (concat "[" first-char (downcase first-char) "]" (substring rlt 1))))
      (if (and rlt ffip-debug)
          (message "ffip-filename-dashes-to-camelcase called. rlt=%s" rlt))))
    rlt))

(defun ffip--create-filename-pattern-for-gnufind (keyword)
  (let ((rlt ""))
    (cond
     ((not keyword)
      (setq rlt ""))
     ((not ffip-filename-rules)
      (setq rlt (concat (if ffip-match-path-instead-of-filename "-iwholename" "-name")
                        " \"*"
                        keyword
                        "*\"" )))
     (t
      (dolist (f ffip-filename-rules rlt)
        (let (tmp fn)
          (setq fn (ffip--find-rule-to-execute keyword f))
          (setq tmp (funcall fn keyword))
          (when tmp
            (setq rlt (concat rlt (unless (string= rlt "") " -o")
                              " "
                              (if ffip-match-path-instead-of-filename "-iwholename" "-name")
                              " \"*"
                              tmp
                              "*\"")))))
      (unless (string= "" rlt)
        (setq rlt (concat "\\(" rlt " \\)")))
      ))
    (if ffip-debug (message "ffip--create-filename-pattern-for-gnufind called. rlt=%s" rlt))
    rlt))

(defun ffip--guess-gnu-find-on-windows (driver path)
  (let (rlt)
    (if (executable-find (concat driver path))
        (setq rlt (concat driver path)))
    rlt))

(defun ffip--guess-gnu-find ()
  (let (rlt)
    (if (eq system-type 'windows-nt)
        (cond
         ;; cygwin
         ((setq rlt (ffip--guess-gnu-find-on-windows "c" ":\\\\cygwin64\\\\bin\\\\find")))
         ((setq rlt (ffip--guess-gnu-find-on-windows "d" ":\\\\cygwin64\\\\bin\\\\find")))
         ((setq rlt (ffip--guess-gnu-find-on-windows "e" ":\\\\cygwin64\\\\bin\\\\find")))
         ((setq rlt (ffip--guess-gnu-find-on-windows "c" ":\\\\cygwin\\\\bin\\\\find")))
         ((setq rlt (ffip--guess-gnu-find-on-windows "d" ":\\\\cygwin\\\\bin\\\\find")))
         ((setq rlt (ffip--guess-gnu-find-on-windows "e" ":\\\\cygwin\\\\bin\\\\find")))
         ;; msys2
         ((setq rlt (ffip--guess-gnu-find-on-windows "c" ":\\\\msys64\\\\usr\\\\bin\\\\find")))
         ((setq rlt (ffip--guess-gnu-find-on-windows "d" ":\\\\msys64\\\\usr\\\\bin\\\\find")))
         ((setq rlt (ffip--guess-gnu-find-on-windows "e" ":\\\\msys64\\\\usr\\\\bin\\\\find")))
         ((setq rlt (ffip--guess-gnu-find-on-windows "c" ":\\\\msys32\\\\usr\\\\bin\\\\find")))
         ((setq rlt (ffip--guess-gnu-find-on-windows "d" ":\\\\msys32\\\\usr\\\\bin\\\\find")))
         ((setq rlt (ffip--guess-gnu-find-on-windows "e" ":\\\\msys32\\\\usr\\\\bin\\\\find")))))
    (unless rlt (setq rlt "find"))
    rlt))

(defun ffip--join-patterns (patterns)
  "Turn `ffip-patterns' into a string that `find' can use."
  (if ffip-patterns
      (format "\\( %s \\)" (mapconcat (lambda (pat) (format "-iwholename \"%s\"" pat))
                         patterns " -or "))
    ""))

(defun ffip--prune-patterns ()
  "Turn `ffip-prune-patterns' into a string that `find' can use."
  (mapconcat (lambda (pat) (format "-iwholename \"%s\"" pat))
             ffip-prune-patterns " -or "))

(defun ffip-completing-read (prompt collection action)
  (cond
    ((= 1 (length collection))
     ;; open file directly
     (funcall action (car collection)))
    ;; support ido-mode
    ((and ffip-prefer-ido-mode (boundp 'ido-mode) ido-mode)
     (funcall action (ido-completing-read prompt collection)))
    (t
     (ivy-read prompt collection
               :action action))))

(defun ffip-project-search (keyword find-directory)
  "Return an alist of all filenames in the project and their path.

Files with duplicate filenames are suffixed with the name of the
directory they are found in so that they are unique."
  (let (rlt
        cmd
        (old-default-directory default-directory)
        (file-alist nil)
        (root (expand-file-name (or ffip-project-root (ffip-project-root)
                                    (error "No project root found")))))
    (cd (file-name-as-directory root))
    ;; make the prune pattern more general
    (setq cmd (format "%s . \\( %s \\) -prune -o -type %s %s %s %s -print"
                      (if ffip-find-executable ffip-find-executable (ffip--guess-gnu-find))
                      (ffip--prune-patterns)
                      (if find-directory "d" "f")
                      (ffip--join-patterns ffip-patterns)
                      ;; When finding directory, the keyword is like:
                      ;; "proj/hello/world"
                      (if find-directory (format "-iwholename \"*%s\"" keyword)
                          (ffip--create-filename-pattern-for-gnufind keyword))
                      ffip-find-options))

    (if ffip-debug (message "run cmd at %s: %s" default-directory cmd))
    (setq rlt
          (mapcar (lambda (file)
                    (if ffip-full-paths
                        (cons (replace-regexp-in-string "^\./" "" file)
                              (expand-file-name file))
                      (let ((file-cons (cons (file-name-nondirectory file)
                                             (expand-file-name file))))
                        (add-to-list 'file-alist file-cons)
                        file-cons)))
                  ;; #15 improving handling of directories containing space
                  (split-string (shell-command-to-string cmd) "[\r\n]+" t)))

    ;; restore the original default-directory
    (cd old-default-directory)
    rlt))

(defun ffip-find-files (keyword open-another-window &optional find-directory)
  (let* ((project-files (ffip-project-search keyword find-directory))
         (files (mapcar 'car project-files))
         file root)
    (if (> (length files) 0)
        (progn
          (setq root (file-name-nondirectory
                      (directory-file-name
                       (or ffip-project-root (ffip-project-root)))))
          (ffip-completing-read
           (format "Find in %s/: " root)
           files
           (lambda (file)
             (let ((rlt (cdr (assoc file project-files))))
               (if find-directory
                   ;; open dired because this rlt is a directory
                   (if open-another-window
                       (dired-other-window rlt)
                     (switch-to-buffer (dired rlt)))
                 (if open-another-window
                     (find-file-other-window rlt)
                   (find-file rlt)))))))
      (message "Nothing found!"))))

;;;###autoload
(defun ffip-create-project-file ()
  "Create .dir-locals.el to setup find-file-in-project per directory.
Modify and place .dir-locals.el to your project root.
See (info \"(Emacs) Directory Variables\") for details."
  (interactive)
  (let ((file (concat (file-name-as-directory default-directory) ".dir-locals.el")))
    (with-temp-file file
      (insert (concat ";; Generated by find-file-in-project.\n"
                      "((nil . ((ffip-project-root . \"" default-directory "\")\n"
                      ";;       (ffip-find-options . \"-not -size +64k\")\n"
                      ";;       (ffip-patterns . (\"*.html\" \"*.js\" \"*.css\" \"*.java\" \"*.el\" \"*.js\"))\n"
                      ";;       (ffip-prune-patterns . (\"*/.git/*\" \"*/node_modules/*\" \"*/index.js\"))\n"
                      "         )))")))
    (message "%s created." file)))

;;;###autoload
(defun ffip-current-full-filename-match-pattern-p (regex)
  "Is current full file name (including directory) match the REGEX?"
  (let ((dir (if (buffer-file-name) (buffer-file-name) "")))
    (string-match-p regex dir)))

;;;###autoload
(defun find-file-in-project (&optional open-another-window)
  "Prompt with a completing list of all files in the project to find one.

If OPEN-ANOTHER-WINDOW is not nil, the file will be opened in new window.

The project's scope is defined as the first directory containing
a `ffip-project-file' (It's value is \".git\" by default.

You can override this by setting the variable `ffip-project-root'."

  (interactive "P")
  (ffip-find-files nil open-another-window))

;;;###autoload
(defun ffip-get-project-root-directory ()
  "Get the full path of project root directory."
  (expand-file-name (or ffip-project-root
                        (ffip-project-root))))

;;;###autoload
(defun find-file-in-current-directory (&optional open-another-window)
  "Like `find-file-in-project'.  But search only in current directory."
  (interactive "P")
  (let ((old-dir ffip-project-root))
    (setq ffip-project-root default-directory)
    (find-file-in-project open-another-window)
    ;; restore to original value
    (setq ffip-project-root old-dir)))

;;;###autoload
(defun find-file-in-project-by-selected (&optional open-another-window)
  "Similar to `find-file-in-project'.
But use string from selected region to search files in the project.
If no region is selected, you need provide keyword.

Keyword could be ANY part of the file's full path and support wildcard.
For example, to find /home/john/proj1/test.js, below keywords are valid:
- test.js
- roj1/tes
- john*test

If OPEN-ANOTHER-WINDOW is not nil, the file will be opened in new window."
  (interactive "P")
  (let ((keyword (if (region-active-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   (read-string "Enter keyword:"))))
    (ffip-find-files keyword open-another-window)))

;;;###autoload
(defun find-file-in-current-directory-by-selected (&optional open-another-window)
  "Like `find-file-in-project-by-selected'.  But search only in current directory."
  (interactive "P")
  (let ((old-dir ffip-project-root))
    (setq ffip-project-root default-directory)
    (find-file-in-project-by-selected open-another-window)
    ;; restore to original value
    (setq ffip-project-root old-dir)))

;;;###autoload
(defun find-directory-in-project-by-selected (&optional open-another-window)
  "Similar to `find-file-in-project-by-selected'.
Use string from selected region to find directory in the project.
If no region is selected, you need provide keyword.

Keyword could be directory's base-name only or parent-directoy+base-name
For example, to find /home/john/proj1/test, below keywords are valid:
- test
- roj1/test
- john*test

If OPEN-ANOTHER-WINDOW is not nil, the file will be opened in new window."
  (interactive "P")
  (let ((keyword (if (region-active-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   (read-string "Enter keyword:"))))
    (ffip-find-files keyword open-another-window t)))

;;;###autoload
(defalias 'ffip 'find-file-in-project)

;; safe locals
(progn
  (put 'ffip-patterns 'safe-local-variable 'listp)
  (put 'ffip-prune-patterns 'safe-local-variable 'listp)
  (put 'ffip-filename-rules 'safe-local-variable 'listp)
  (put 'ffip-match-path-instead-of-filename 'safe-local-variable 'booleanp)
  (put 'ffip-project-file 'safe-local-variable 'stringp)
  (put 'ffip-project-root 'safe-local-variable 'stringp))

(provide 'find-file-in-project)
;;; find-file-in-project.el ends here
