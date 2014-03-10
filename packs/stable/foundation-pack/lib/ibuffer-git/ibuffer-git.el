;;; ibuffer-git.el --- show git status in ibuffer column

;; Copyright (C) 2010  Jonathan Rockway

;; Author: Jonathan Rockway <jon@jrock.us>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package adds git integration to Ibuffer.  Two columns are
;; defined, git-status and git-status-mini.
;;
;; To actually make these columns show up, you need to customize
;; `ibuffer-formats'.  The symbol `git-status-mini' can be inserted
;; where you want it, and `git-status' should be inserted with
;; something as something like `(git-status 8 8 :left)', where 8 is
;; the number you picked for `ibuffer-git-column-length'.

;;; Code:

(require 'ibuffer)
(require 'cl)

(defgroup ibuffer-git nil
  "Git integration for Ibuffer"
  :group 'ibuffer)

(defun* ibuffer-git-check-status (filename)
  "Return a cons cell representing the number of lines added to and removed from FILENAME since the last git commit.  Return NIL if the file is not under git's control (or there is some other error), and `(0 . 0)' if there are no changes.

FILENAME must be a filename."
  (condition-case e
      (let* ((default-directory (file-name-directory filename))
             (res (car (process-lines "git" "diff" "--numstat"
                                      (file-name-nondirectory filename)))))
        (cond ((not res) (cons 0 0))
              ((string-match "^\\([0-9]+\\)\t\\([0-9]+\\)\t" res)
               (cons (read (match-string 1 res)) (read (match-string 2 res))))
              (t nil)))
    (error nil)))

(defface ibuffer-git-add-face '((t (:inherit (diff-added))))
  "Face to show +s in"
  :group 'ibuffer-git)

(defface ibuffer-git-del-face '((t (:inherit (diff-removed))))
  "Face to show -s in"
  :group 'ibuffer-git)

(defcustom ibuffer-git-column-length 8
  "How big your non-mini git status column in Ibuffer will be."
  :group 'ibuffer-git
  :type 'integer)

(defun ibuffer-git-format-result (res)
  "Format the results of `ibuffer-git-check-status' for display in the ibuffer.

Argument RES is a cons cell in the format `(ADD . DEL)'."
  (if (not res) ""
    (destructuring-bind (add . del) res
      (cond ((and (= 0 add) (= 0 del)) "")
            (t (let* ((add-ratio (/ (float add) (+ add del)))
                      (plus  (ceiling (* add-ratio ibuffer-git-column-length)))
                      (minus (- ibuffer-git-column-length plus)))
                 (concat
                  (propertize (concat (loop for i from 1 to (min add plus) collect ?+))
                              'face 'ibuffer-git-add-face)
                  (propertize (concat (loop for i from 1 to (min del minus) collect ?-))
                              'face 'ibuffer-git-del-face))))))))

(defvar ibuffer-git-status-keymap (make-sparse-keymap)
  "Keymap for clicking on the diffs")

(define-key ibuffer-git-status-keymap (kbd "<mouse-2>") #'ibuffer-git-visit-diff)

(defun ibuffer-git-visit-diff (event)
  "Show the detailed diff for the ibuffer entry at the point.
Argument EVENT is the mouse event that triggered us."
  (interactive "e")
    (with-current-buffer
        (progn (mouse-set-point event)
               (ibuffer-current-buffer t))
      (vc-diff nil t)))

(define-ibuffer-column git-status
  (:name "Git"
   :inline t
   :props ('mouse-face 'highlight
           'keymap ibuffer-git-status-keymap
           'help-echo "mouse-2: see detailed diff"))
   (ibuffer-git-format-result
    (ignore-errors (when (buffer-file-name)
                     (ibuffer-git-check-status (buffer-file-name))))))


(define-ibuffer-column git-status-mini (:name "G" :inline t)
  (destructuring-bind (a . d)
      (or (ignore-errors (when (buffer-file-name)
                           (ibuffer-git-check-status (buffer-file-name))))
          (cons 0 0))
    (cond ((= 0 (+ a d)) " ")
          ((< a d) (propertize "-" 'face 'ibuffer-git-del-face))
          ((>= a d) (propertize "+" 'face 'ibuffer-git-add-face)))))

(provide 'ibuffer-git)
;;; ibuffer-git.el ends here
