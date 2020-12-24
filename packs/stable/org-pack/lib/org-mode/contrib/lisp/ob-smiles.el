;;; ob-smiles.el --- Org-mode Babel support for SMILES.
;;; -*- coding: utf-8 -*-

;; Keywords: org babel SMILES
;; Version: 0.0.1
;; Package-Requires: ((smiles-mode "0.0.1") (org "8"))

;;; Commentary:

;;; I copy code from:
;;; http://kitchingroup.cheme.cmu.edu/blog/2016/03/26/A-molecule-link-for-org-mode

;; Author: John Kitchin [jkitchin@andrew.cmu.edu]
;; Maintainer: stardiviner [numbchild@gmail.com]

;;; Code:

;; Org-mode Babel
(defun org-babel-execute:smiles (body params)
  "Execute SMILES babel `BODY' with `PARAMS'."
  (shell-command-to-string
   (format "obabel -:\"%s\" -osvg 2> /dev/null" body)))

;; Org-mode link
(defun molecule-jump (name)
  "Jump to molecule `NAME' definition."
  (org-mark-ring-push)
  (org-link-open-from-string (format "[[%s]]" path)))

(defun molecule-export (path desc backend)
  "Export molecule to HTML format on `PATH' with `DESC' and `BACKEND'."
  (let ((name (save-window-excursion
                (molecule-jump path)
                (org-element-property :name (org-element-context)))))
    (cond
     ((eq 'html backend)
      (format "<a href=\"#%s\">%s</a>" name name)))))

(org-add-link-type
 "molecule"
 'molecule-jump
 'molecule-export)

;; org-mode element
(org-element-map (org-element-parse-buffer)
    'src-block
  (lambda (src)
    (when (string= "smiles" (org-element-property :language src))
      (org-element-property :name src))))


(provide 'ob-smiles)

;;; ob-smiles.el ends here
