;;; eldo.el --- Elisp Doc-to-Org converter

;; Copyright (C) 2012--2019  Bastien Guerry
;;
;; Author: Bastien Guerry <bzg@gnu.org>
;; Keywords: elisp, documentation, org
;; Homepage: https://orgmode.org
;;
;; This file is not part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; M-x eldo-make-doc RET will create a file with documentation for hooks,
;; commands and options, given a subset of *.el files.  Use an .org file
;; to write the documentation.
;;
;; This file is inspired by Nic Ferrier's wikidoc.el, with contributions
;; from Eric Schulte and Thorsten Jolitz.
;;
;;; Todo:
;;
;; - refactor and add customizable variables?

(defvar eldo-keymaps nil)

(defvar eldo-git-raw-file
  "https://orgmode.org/cgit.cgi/org-mode.git/plain/lisp/%s")

(defvar eldo-git-search-string
  "https://orgmode.org/cgit.cgi/org-mode.git/log/?qt=grep&q=%s")

(defvar eldo-file nil)

(defun eldo-load (dir prefix)
  "Load Elisp files in DIR with PREFIX."
  (if (string= (file-name-extension dir) "el")
      (load-file dir)
    (dolist (file (directory-files
		   (expand-file-name dir)
		   'full (concat (or prefix "") ".*\.el$")))
      (load-file file))))

(defun eldo-make-doc (dir prefix)
  "Insert documentation in the current buffer."
  (interactive "fDirectory or file: \nsPrefix: ")
  (eldo-load dir prefix)
  (let (eldo-keymaps hks cmds opts vars funcs)
    (mapatoms
     (lambda(a)
       (when (string-prefix-p prefix (symbol-name a))
	 (cond ((keymapp a) (setq eldo-keymaps (cons a eldo-keymaps)))
	       ((string-match "hook$\\|functions$" (symbol-name a))
		(setq hks (cons a hks)))
	       ((commandp a) (setq cmds (cons a cmds)))
	       ((get a 'custom-type) (setq opts (cons a opts)))
	       ((fboundp a) (setq funcs (cons a funcs)))
	       (t (setq vars (cons a vars)))))))
    (find-file (or eldo-file (read-file-name "Write to file: ")))
    (org-mode)
    (eldo-write-hooks hks)
    (eldo-write-commands cmds)
    (eldo-write-options opts)))

(defun eldo-write-hooks (hooks)
  "Write hooks documentation in the current buffer."
  (insert "* Hooks\n")
  (org-set-property "CUSTOM_ID" "hooks")
  (dolist (h hooks)
    (unless (null (find-lisp-object-file-name h 'defvar))
      (insert "\n\n** " (symbol-name h))
      (let ((f (file-name-nondirectory (find-lisp-object-file-name h 'defvar)))
	    (val (replace-regexp-in-string
		  "\n" "\\\\n"
		  (prin1-to-string (car (get h 'standard-value)))))
	    (version (get h 'custom-version))
	    (d (get h 'variable-documentation)))
	(if (> (length val) 30) (setq val (concat (substring val 0 30) "...")))
	(insert
	 " =" val "=\n"
	 (if version (format "- *Since:* Emacs version %s\n" version) "")
	 (format (concat "- *In file:* [[" eldo-git-raw-file "][%s]]\n") f f)
	 (format (concat "- [[" eldo-git-search-string
			 "][Find modifications in git logs]]\n\n") (symbol-name h)))
	(when (stringp d) (insert (eldo-make-verbatim d)))))
    (org-set-property "CUSTOM_ID" (symbol-name h))
    (goto-char (point-max))))

(defun eldo-write-commands (commands)
  "Write commands documentation in the current buffer."
  (insert "\n* Commands\n")
  (org-set-property "CUSTOM_ID" "commands")
  (dolist (c commands)
    (when (find-lisp-object-file-name c 'defun)
      (let ((f (file-name-nondirectory (find-lisp-object-file-name c 'defun)))
	    (key (mapconcat 'key-description (where-is-internal c eldo-keymaps) ", "))
	    (args (help-function-arglist c t))
	    (d (documentation c)))
	(insert "\n** " (symbol-name c) (if args (format " =%s=\n" args) "\n"))
	(org-set-property "CUSTOM_ID" (symbol-name c))
	(insert
	 (if (and key (not (string= key ""))) (format "\n- *Access:* ~%s~" key) "")
	 (format (concat "\n- *In file:* [[" eldo-git-raw-file "][%s]]\n") f f)
	 (format (concat "- [[" eldo-git-search-string
			 "][Find modifications in git logs]]\n\n") (symbol-name c)))
	(when (stringp d) (insert (eldo-make-verbatim d))))
      (goto-char (point-max)))))

(defun eldo-write-options (options)
  "Write options documentation in the current buffer."
  (insert "\n* Options\n")
  (org-set-property "CUSTOM_ID" "options")
  (dolist (o options)
    (when (find-lisp-object-file-name o 'defvar)
      (insert "\n\n** " (symbol-name o))
      (let ((f (file-name-nondirectory (find-lisp-object-file-name o 'defvar)))
	    (val (replace-regexp-in-string
		  "\n" "\\\\n"
		  (prin1-to-string (car (get o 'standard-value)))))
	    (version (get o 'custom-version))
	    (type (prin1-to-string (get o 'custom-type)))
	    (d (get o 'variable-documentation)))
	(if (> (length val) 30) (setq val (concat (substring val 0 30) "...")))
	(if (> (length type) 30) (setq type (concat (substring type 0 30) "...")))
	(insert
	 " =" val "=\n\n"
	 (format "- *Type:* %s\n" type)
	 (if version (format "- *Since:* Emacs version %s\n" version) "")
	 (format (concat "- *In file:* [[" eldo-git-raw-file "][%s]]\n") f f)
	 (format (concat "- [[" eldo-git-search-string
			 "][Find modifications in git logs]]\n\n") (symbol-name o)))
	(when (stringp d) (insert (eldo-make-verbatim d)))))
    (org-set-property "CUSTOM_ID" (symbol-name o))
    (goto-char (point-max))))

(defun eldo-make-verbatim (string)
  "Convert STRING to a verbatim region in Org."
  (let ((str (split-string string "\n")))
    (mapconcat (lambda(s) (concat ": " s)) str "\n")))
