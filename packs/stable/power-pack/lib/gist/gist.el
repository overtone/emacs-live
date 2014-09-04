;;; gist.el --- Emacs integration for gist.github.com

;; Author: Yann Hodique <yann.hodique@gmail.com>
;; Original author: Christian Neukirchen <purl.org/net/chneukirchen>
;; Contributors:
;; Chris Wanstrath <chris@ozmm.org>
;; Will Farrington <wcfarrington@gmail.com>
;; Michael Ivey
;; Phil Hagelberg
;; Dan McKinley
;; Version: 1.1.1
;; Keywords: gist git github paste pastie pastebin
;; Package-Requires: ((eieio "1.3") (gh "0.7.2") (tabulated-list "0"))

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; An Emacs interface for managing gists (http://gist.github.com).

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'eieio)
(require 'eieio-base)
(require 'timezone)

(require 'gh-gist)
(require 'gh-profile)

(require 'tabulated-list)

(defgroup gist nil
  "Gist"
  :group 'applications)

(defcustom gist-list-format '((id "Id" 10 nil identity)
                              (created "Created" 20 nil "%D %R")
                              (visibility "Visibility" 10 nil
                                          (lambda (public)
                                            (or (and public "public")
                                                "private")))
                              (description "Description" 0 nil identity))
  "Format for gist list"
  :type '(alist :key-type
                (choice (const :tag "Id" id)
                        (const :tag "Creation date" created)
                        (const :tag "Visibility" visibility)
                        (const :tag "Description" description))
                :value-type
                (list
                 (string :tag "Label")
                 (integer :tag "Field length")
                 (boolean :tag "Sortable")
                 (choice (string :tag "Format")
                         (function :tag "Formatter"))))
  :group 'gist)

(defcustom gist-view-gist nil
  "If non-nil, automatically use `browse-url' to view gists after
they're posted.")

(defvar gist-supported-modes-alist '((action-script-mode . "as")
                                     (c-mode . "c")
                                     (c++-mode . "cpp")
                                     (clojure-mode . "clj")
                                     (common-lisp-mode . "lisp")
                                     (css-mode . "css")
                                     (diff-mode . "diff")
                                     (emacs-lisp-mode . "el")
                                     (lisp-interaction-mode . "el")
                                     (erlang-mode . "erl")
                                     (haskell-mode . "hs")
                                     (html-mode . "html")
                                     (io-mode . "io")
                                     (java-mode . "java")
                                     (javascript-mode . "js")
                                     (jde-mode . "java")
                                     (js2-mode . "js")
                                     (lua-mode . "lua")
                                     (ocaml-mode . "ml")
                                     (objective-c-mode . "m")
                                     (perl-mode . "pl")
                                     (php-mode . "php")
                                     (python-mode . "py")
                                     (ruby-mode . "rb")
                                     (text-mode . "txt")
                                     (scala-mode . "scala")
                                     (sql-mode . "sql")
                                     (scheme-mode . "scm")
                                     (smalltalk-mode . "st")
                                     (sh-mode . "sh")
                                     (tcl-mode . "tcl")
                                     (tex-mode . "tex")
                                     (xml-mode . "xml")))

(defvar gist-list-db nil)

(defvar gist-id nil)
(make-variable-buffer-local 'gist-id)

(defvar gist-filename nil)
(make-variable-buffer-local 'gist-filename)

(defun gist-get-api (&optional sync)
  (let ((gh-profile-current-profile
         (or gh-profile-current-profile (gh-profile-completing-read))))
    (gh-gist-api "api" :sync sync :cache t :num-retries 1)))

(defun gist-internal-new (files &optional private description callback)
  (let* ((api (gist-get-api))
         (gist (gh-gist-gist-stub "gist"
                                  :public (not private)
                                  :description (or description "")
                                  :files files))
         (resp (gh-gist-new api gist)))
    (gh-url-add-response-callback
     resp
     (lexical-let ((profile (oref api :profile))
                   (cb callback))
       (lambda (gist)
         (let ((gh-profile-current-profile profile))
           (funcall (or cb 'gist-created-callback) gist)))))))

;;;###autoload
(defun gist-region (begin end &optional private callback)
  "Post the current region as a new paste at gist.github.com
Copies the URL into the kill ring.

With a prefix argument, makes a private paste."
  (interactive "r\nP")
  (let* ((file (or (buffer-file-name) (buffer-name)))
         (name (file-name-nondirectory file))
         (ext (or (cdr (assoc major-mode gist-supported-modes-alist))
                  (file-name-extension file)
                  "txt"))
         (fname (concat (file-name-sans-extension name) "." ext))
         (files (list
                 (gh-gist-gist-file "file"
                                    :filename fname
                                    :content (buffer-substring begin end)))))
    (gist-internal-new files private nil callback)))

(defun gist-files (filenames &optional private callback)
  (let ((files nil))
    (dolist (f filenames)
      (with-temp-buffer
        (insert-file-contents f)
        (let ((name (file-name-nondirectory f)))
          (push (gh-gist-gist-file name :filename name :content (buffer-string))
                files))))
    (gist-internal-new files private nil callback)))

(defun gist-created-callback (gist)
  (let ((location (oref gist :html-url)))
    (gist-list-reload t)
    (message "Paste created: %s" location)
    (when gist-view-gist
      (browse-url location))
    (kill-new location)))

;;;###autoload
(defun gist-region-private (begin end)
  "Post the current region as a new private paste at gist.github.com
Copies the URL into the kill ring."
  (interactive "r")
  (gist-region begin end t))

;;;###autoload
(defun gist-buffer (&optional private)
  "Post the current buffer as a new paste at gist.github.com.
Copies the URL into the kill ring.

With a prefix argument, makes a private paste."
  (interactive "P")
  (gist-region (point-min) (point-max) private))

;;;###autoload
(defun gist-buffer-private ()
  "Post the current buffer as a new private paste at gist.github.com.
Copies the URL into the kill ring."
  (interactive)
  (gist-region-private (point-min) (point-max)))

;;;###autoload
(defun gist-region-or-buffer (&optional private)
  "Post either the current region, or if mark is not set, the
  current buffer as a new paste at gist.github.com

Copies the URL into the kill ring.

With a prefix argument, makes a private paste."
  (interactive "P")
  (condition-case nil
      (gist-region (point) (mark) private)
    (mark-inactive (gist-buffer private))))

;;;###autoload
(defun gist-region-or-buffer-private ()
  "Post either the current region, or if mark is not set, the
  current buffer as a new private paste at gist.github.com

Copies the URL into the kill ring."
  (interactive)
  (condition-case nil
      (gist-region-private (point) (mark))
    (mark-inactive (gist-buffer-private))))

;;;###autoload
(defun gist-list (&optional force-reload background)
  "Displays a list of all of the current user's gists in a new buffer."
  (interactive "P")
  ;; if buffer exists, it contains the current gh profile
  (let* ((gh-profile-current-profile (or gh-profile-current-profile
                                         (gh-profile-completing-read)))
         (bufname (format "*%s:gists*" gh-profile-current-profile))
         (api (gist-get-api nil)))
    (when force-reload
      (pcache-clear (oref api :cache))
      (or background (message "Retrieving list of your gists...")))
    (unless (and background (not (get-buffer bufname)))
      (let ((resp (gh-gist-list api)))
        (gh-url-add-response-callback
         resp
         (lexical-let ((buffer bufname))
           (lambda (gists)
             (with-current-buffer (get-buffer-create buffer)
               (gist-lists-retrieved-callback gists background)))))
        (gh-url-add-response-callback
         resp
         (lexical-let ((profile (oref api :profile))
                       (buffer bufname))
           (lambda (&rest args)
             (with-current-buffer buffer
               (setq gh-profile-current-profile profile)))))))))

(defun gist-list-reload (&optional background)
  (interactive)
  (gist-list t background))

(defun gist-tabulated-entry (gist)
  (let* ((data (gist-parse-gist gist))
         (repo (car data)))
    (list repo (apply 'vector data))))

(defun gist-lists-retrieved-callback (gists &optional background)
  "Called when the list of gists has been retrieved. Displays
the list."
  (setq gist-list-db gists)
  (gist-list-render background))

(defun gist--get-time (gist)
  (let* ((date (timezone-parse-date (oref gist :date)))
         (time (timezone-parse-time (aref date 3))))
    (encode-time (string-to-number (aref time 2))
                 (string-to-number (aref time 1))
                 (string-to-number (aref time 0))
                 (string-to-number (aref date 2))
                 (string-to-number (aref date 1))
                 (string-to-number (aref date 0))
                 (aref date 4))))

(defun gist-parse-gist (gist)
  "Returns a list of the gist's attributes for display, given the xml list
for the gist."
  (let ((repo (oref gist :id))
        (creation (gist--get-time gist))
        (desc (or (oref gist :description) ""))
        (public (oref gist :public)))
    (loop for (id label width sort format) in gist-list-format
          collect (let ((string-formatter (if (eq id 'created)
                                              'format-time-string
                                            'format))
                        (value (cond ((eq id 'id) repo)
                                     ((eq id 'created) creation)
                                     ((eq id 'visibility) public)
                                     ((eq id 'description) desc))))
                    (funcall (if (stringp format)
                                 (lambda (val)
                                   (funcall string-formatter format val))
                               format)
                             value)))))

;;;###autoload
(defun gist-fetch (id)
  (interactive "sGist ID: ")
  (let ((gist nil)
        (multi nil)
        (prefix (format "*gist %s*" id))
        (result nil)
        (profile (gh-profile-current-profile)))
    (dolist (g gist-list-db)
      (when (string= (oref g :id) id)
        (setq gist g)))
    (let ((api (gist-get-api t)))
      (cond ((null gist)
             ;; fetch it
             (setq gist (oref (gh-gist-get api id) :data))
             (add-to-list 'gist-list-db gist))
            ((not (gh-gist-gist-has-files gist))
             (gh-gist-get api gist))))
    (let ((files (oref gist :files)))
      (setq multi (< 1 (length files)))
      (dolist (f files)
        (let ((buffer (get-buffer-create (format "%s/%s" prefix
                                                 (oref f :filename))))
              (mode (car (rassoc (file-name-extension (oref f :filename))
                                 gist-supported-modes-alist))))
          (with-current-buffer buffer
            (delete-region (point-min) (point-max))
            (insert (oref f :content))
            (let ((fname (oref f :filename)))
              ;; set major mode
              (if (fboundp mode)
                  (funcall mode)
                (let ((buffer-file-name fname))
                  (normal-mode)))
              ;; set minor mode
              (gist-mode 1)
              (setq gist-id id
                    gist-filename fname
                    gh-profile-current-profile profile))
            (set-buffer-modified-p nil))
          (setq result buffer))))
    (if multi
        (let ((ibuffer-mode-hook nil)
              (ibuffer-use-header-line nil)
              (ibuffer-show-empty-filter-groups nil))
          (ibuffer t prefix
                   `((name . ,(regexp-quote (concat prefix "/"))))
                   nil nil
                   nil
                   '((name))))
      (switch-to-buffer-other-window result))))

(defun gist-fetch-current ()
  (interactive)
  (gist-fetch (tabulated-list-get-id)))

(defun gist-edit-current-description ()
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (gist (gist-list-db-get-gist id))
         (old-descr (oref gist :description))
         (new-descr (read-from-minibuffer "Description: " old-descr)))
    (let* ((g (clone gist
                     :files nil
                     :description new-descr))
           (api (gist-get-api t))
           (resp (gh-gist-edit api g)))
      (gh-url-add-response-callback resp
                                    (lambda (gist)
                                      (gist-list-reload))))))

(defun gist-add-buffer (buffer)
  (interactive "bBuffer: ")
  (let* ((buffer (get-buffer buffer))
         (id (tabulated-list-get-id))
         (gist (gist-list-db-get-gist id))
         (fname (file-name-nondirectory (or (buffer-file-name buffer) (buffer-name buffer)))))
    (let* ((g (clone gist :files
                     (list
                      (gh-gist-gist-file "file"
                                         :filename fname
                                         :content (with-current-buffer buffer
                                                    (buffer-string ))))))
           (api (gist-get-api t))
           (resp (gh-gist-edit api g)))
      (gh-url-add-response-callback resp
                                    (lambda (gist)
                                      (gist-list-reload))))))

(defun gist-remove-file (fname)
  (interactive (list
                (completing-read
                 "Filename to remove: "
                 (let* ((id (tabulated-list-get-id))
                        (gist (gist-list-db-get-gist id)))
                   (mapcar #'(lambda (f) (oref f :filename))
                           (oref gist :files))))))
  (let* ((id (tabulated-list-get-id))
         (gist (gist-list-db-get-gist id)))
    (let* ((g (clone gist :files
                     (list
                      (gh-gist-gist-file "file"
                                         :filename fname
                                         :content nil))))
           (api (gist-get-api t))
           (resp (gh-gist-edit api g)))
      (gh-url-add-response-callback resp
                                    (lambda (gist)
                                      (gist-list-reload))))))

(defun gist-kill-current ()
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when (yes-or-no-p (format "Really delete gist %s ? " id) )
      (let* ((api (gist-get-api t))
             (resp (gh-gist-delete api id)))
        (gist-list-reload)))))

(defun gist-current-url ()
  "Helper function to fetch current gist url"
  (let* ((id (tabulated-list-get-id))
         (gist (gist-list-db-get-gist id)))
    (oref gist :html-url)))

(defun gist-print-current-url ()
  "Display the currently selected gist's url in the echo area and
put it into `kill-ring'."
  (interactive)
  (kill-new (message (gist-current-url))))

(defun gist-browse-current-url ()
  "Browse current gist on github"
  (interactive)
  (browse-url (gist-current-url)))

(defvar gist-list-menu-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "\C-m" 'gist-fetch-current)
    (define-key map "g" 'gist-list-reload)
    (define-key map "e" 'gist-edit-current-description)
    (define-key map "k" 'gist-kill-current)
    (define-key map "+" 'gist-add-buffer)
    (define-key map "-" 'gist-remove-file)
    (define-key map "y" 'gist-print-current-url)
    (define-key map "b" 'gist-browse-current-url)
    map))

(define-derived-mode gist-list-mode tabulated-list-mode "Gist Menu"
  "Major mode for browsing gists.
\\<gist-list-menu-mode-map>
\\{gist-list-menu-mode-map}"
  (setq tabulated-list-format
        (apply 'vector
               (loop for (sym label width sort format) in gist-list-format
                     collect (list label width sort)))
        tabulated-list-padding 2
        tabulated-list-sort-key nil)
  (tabulated-list-init-header)
  (use-local-map gist-list-menu-mode-map))

(defun gist-list-render (&optional background)
  (gist-list-mode)
  (setq tabulated-list-entries
        (mapcar 'gist-tabulated-entry gist-list-db))
  (tabulated-list-print)
  (gist-list-tag-multi-files)
  (unless background
    (set-window-buffer nil (current-buffer))))

(defun gist-list-tag-multi-files ()
  (let ((ids nil))
    (dolist (gist gist-list-db)
      (when (< 1 (length (oref gist :files)))
        (push (oref gist :id) ids)))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (if (member (tabulated-list-get-id) ids)
            (tabulated-list-put-tag "+" t)
          (forward-line 1))))))

(defun gist-list-db-get-gist (id)
  (loop for gist in gist-list-db if (string= (oref gist :id) id)
        return gist))

;;; Gist minor mode

(defun gist-mode-edit-buffer (&optional new-name)
  (when (or (buffer-modified-p) new-name)
    (let* ((id gist-id)
           (gist (gist-list-db-get-gist id))
           (files (list
                   (make-instance 'gh-gist-gist-file
                                  :filename (or new-name gist-filename)
                                  :content (buffer-string)))))
      (when new-name
        ;; remove old file as well
        (add-to-list 'files
                     (make-instance 'gh-gist-gist-file
                                    :filename gist-filename
                                    :content nil)))
      (let* ((g (clone gist
                       :files files))
             (api (gist-get-api t))
             (resp (gh-gist-edit api g)))
        (gh-url-add-response-callback
         resp
         (lambda (gist)
           (set-buffer-modified-p nil)
           (when new-name
             (rename-buffer (replace-regexp-in-string "/.*$"
                                                      (concat "/" new-name)
                                                      (buffer-name)))
             (setq gist-filename new-name))
           (let ((g (gist-list-db-get-gist (oref gist :id))))
             (oset g :files (oref gist :files)))))))))

(defun gist-mode-save-buffer ()
  (interactive)
  (gist-mode-edit-buffer))

(defun gist-mode-write-file ()
  (interactive)
  (let ((new-name (read-from-minibuffer "File name: " gist-filename)))
    (gist-mode-edit-buffer new-name)))

(defvar gist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap save-buffer] 'gist-mode-save-buffer)
    (define-key map [remap write-file] 'gist-mode-write-file)
    map))

(define-minor-mode gist-mode
  "Minor mode for buffers containing gists files"
  :lighter " gist"
  :map 'gist-mode-map)

;;; Dired integration

(require 'dired)

(defun dired-do-gist (&optional private)
  (interactive "P")
  (gist-files (dired-get-marked-files) private))

(define-key dired-mode-map "@" 'dired-do-gist)

(provide 'gist)
;;; gist.el ends here.
