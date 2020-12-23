;;; sesman-browser.el --- Interactive Browser for Sesman -*- lexical-binding: t -*-
;;
;; Copyright (C) 2018, Vitalie Spinu
;; Author: Vitalie Spinu
;; URL: https://github.com/vspinu/sesman
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is *NOT* part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Interactive session browser.
;;
;;; Code:

(require 'seq)
(require 'sesman)

(defgroup sesman-browser nil
  "Browser for Sesman."
  :prefix "sesman-browser-"
  :group 'sesman
  :link '(url-link :tag "GitHub" "https://github.com/vspinu/sesman"))

(defface sesman-browser-highligh-face
  '((default (:inherit highlight :weight bold)))
  "Face used to highlight currently selected button."
  :group 'sesman-browser)

(defface sesman-browser-button-face
  '((default (:inherit button :slant italic)))
  "Face used to highlight currently selected object."
  :group 'sesman-browser)

(defvar-local sesman-browser--sort-types '(name relevance))
(defcustom sesman-browser-sort-type 'name
  "Default sorting type in sesman browser buffers.
Currently can be either 'name  or 'relevance."
  :type '(choice (const name) (const relevance))
  :group 'sesman-browser)

(defvar sesman-browser-map
  (let (sesman-browser-map)
    (define-prefix-command 'sesman-browser-map)
    (define-key sesman-browser-map (kbd "r") #'sesman-browser-restart-session)
    (define-key sesman-browser-map (kbd "q") #'sesman-browser-quit-session)
    (define-key sesman-browser-map (kbd "b") #'sesman-browser-link-with-buffer)
    (define-key sesman-browser-map (kbd "d") #'sesman-browser-link-with-directory)
    (define-key sesman-browser-map (kbd "p") #'sesman-browser-link-with-project)
    (define-key sesman-browser-map (kbd "u") #'sesman-browser-unlink)
    sesman-browser-map)
  "Prefix keymap for sesman commands from sesman browser.")

(defvar sesman-browser-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'sesman-browser-vertical-next)
    (define-key map (kbd "p") #'sesman-browser-vertical-prev)
    (define-key map (kbd "f") #'sesman-browser-forward)
    (define-key map (kbd "b") #'sesman-browser-backward)
    (define-key map [remap forward-paragraph] #'sesman-browser-session-next)
    (define-key map [remap backward-paragraph] #'sesman-browser-session-prev)
    (define-key map (kbd "C-M-n") #'sesman-browser-session-next)
    (define-key map (kbd "C-M-p") #'sesman-browser-session-prev)
    (define-key map (kbd "<tab>") #'sesman-browser-forward)
    (define-key map (kbd "<backtab>") #'sesman-browser-backward)
    (define-key map (kbd "<RET>") #'sesman-goto)
    (define-key map (kbd "o") #'sesman-show)
    (define-key map (kbd "t") #'sesman-browser-toggle-sort)
    (define-key map (kbd "S") #'sesman-browser-toggle-sort)
    (define-key map (kbd "l b") #'sesman-browser-link-with-buffer)
    (define-key map (kbd "l d") #'sesman-browser-link-with-directory)
    (define-key map (kbd "l p") #'sesman-browser-link-with-project)
    (define-key map (kbd "u") #'sesman-browser-unlink)
    (define-key map (kbd "s") 'sesman-browser-map)
    (define-key map (kbd "C-c C-s") 'sesman-browser-map)
    (easy-menu-define sesman-browser-mode-map map
      "Sesman Browser"
      '("SesmanBrowser"
        ["Next row" sesman-browser-vertical-next]
        ["Previous row" sesman-browser-vertical-prev]
        ["Next button" sesman-browser-forward]
        ["Previous button" sesman-browser-backward]
        ["Next session" sesman-browser-session-next]
        ["Previous session" sesman-browser-session-prev]
        "--"
        ["Goto buffer" sesman-goto]
        ["Show buffer" sesman-show]
        "--"
        ["Link with Buffer" sesman-browser-link-with-buffer]
        ["Link with Directory" sesman-browser-link-with-directory]
        ["Link with Project" sesman-browser-link-with-project]
        ["Unlink" sesman-browser-unlink]
        "--"
        ["Toggle sort" sesman-browser-toggle-sort]
        ["Refresh View" revert-buffer]))
    map)
  "Local keymap in `sesman-browser-mode'.")


;;; Utilities

(defun sesman-browser--closeby-pos (prop lax)
  (or (when (get-text-property (point) prop)
        (point))
      (when (and (not (bobp))
                 (get-text-property (1- (point)) prop))
        (1- (point)))
      (when lax
        (let ((next (save-excursion
                      (and
                       (goto-char (next-single-char-property-change (point) prop))
                       (get-text-property (point) prop)
                       (point))))
              (prev (save-excursion
                      (and
                       (goto-char (previous-single-char-property-change (point) prop))
                       (not (bobp))
                       (get-text-property (1- (point)) prop)
                       (1- (point))))))
          (if next
              (if prev
                  (if (< (- (point) prev) (- next (point)))
                      prev
                    next)
                next)
            prev)))))

(defun sesman-browser--closeby-value (prop lax)
  (when-let ((pos (sesman-browser--closeby-pos prop lax)))
    (get-text-property pos prop)))

(defun sesman-browser-get (what &optional no-error lax)
  "Get value of the property WHAT at point.
If NO-ERROR is non-nil, don't throw an error if no value has been found and
return nil. If LAX is non-nil, search nearby and return the closest value."
  (when (derived-mode-p 'sesman-browser-mode)
    (or (let ((prop (pcase what
                      ('session :sesman-session)
                      ('link    :sesman-link)
                      ('object  :sesman-object)
                      (_        what))))
          (sesman-browser--closeby-value prop 'lax))
        (unless no-error
          (user-error "No %s %s" what (if lax "nearby" "at point"))))))


;;; Navigation

(defvar-local sesman-browser--section-overlay nil)
(defvar-local sesman-browser--stop-overlay nil)

(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'sesman-left-bar
    [#b00001100] nil nil '(top t)))

(defun sesman-browser--next (prop)
  (let ((pos (point)))
    (goto-char (previous-single-char-property-change (point) prop))
    (unless (get-text-property (point) prop)
      (goto-char (previous-single-char-property-change (point) prop)))
    (when (bobp)
      (goto-char pos))))

(defun sesman-browser--prev (prop)
  (let ((pos (point)))
    (goto-char (next-single-char-property-change (point) prop))
    (unless (get-text-property (point) prop)
      (goto-char (next-single-char-property-change (point) prop)))
    (when (eobp)
      (goto-char pos))))

(defun sesman-browser-forward ()
  "Go to next button."
  (interactive)
  (sesman-browser--prev :sesman-stop))

(defun sesman-browser-backward ()
  "Go to previous button."
  (interactive)
  (sesman-browser--next :sesman-stop))

(defun sesman-browser-vertical-next ()
  "Go to next button section or row."
  (interactive)
  (sesman-browser--prev :sesman-vertical-stop))

(defun sesman-browser-vertical-prev ()
  "Go to previous button section or row."
  (interactive)
  (sesman-browser--next :sesman-vertical-stop))

(defun sesman-browser-session-next ()
  "Go to next session."
  (interactive)
  (sesman-browser--prev :sesman-session-stop))

(defun sesman-browser-session-prev ()
  "Go to previous session."
  (interactive)
  (sesman-browser--next :sesman-session-stop))


;;; Display

(defun sesman-goto (&optional no-switch)
  "Go to most relevant buffer for session at point.
If NO-SWITCH is non-nil, only display the buffer."
  (interactive "P")
  (let ((object (get-text-property (point) :sesman-object)))
    (if (and object (bufferp object))
        (if no-switch
            (display-buffer object)
          (pop-to-buffer object))
      (let* ((session (sesman-browser-get 'session))
             (info (sesman-session-info (sesman--system) session))
             (buffers (or (plist-get info :buffers)
                          (let ((objects (plist-get info :objects)))
                            (seq-filter #'bufferp objects)))))
        (if buffers
            (let ((most-recent-buf (seq-find (lambda (b)
                                               (member b buffers))
                                             (buffer-list))))
              (if no-switch
                  (display-buffer most-recent-buf)
                (pop-to-buffer most-recent-buf)))
          (user-error "Cannot jump to session %s; it doesn't contain any buffers" (car session)))))))

(defun sesman-show ()
  "Show the most relevant buffer for the session at point."
  (interactive)
  (sesman-goto 'no-switch))

(defun sesman-browser--sensor-function (&rest _ignore)
  (let ((beg (or (when (get-text-property (point) :sesman-stop)
                   (if (get-text-property (1- (point)) :sesman-stop)
                       (previous-single-char-property-change (point) :sesman-stop)
                     (point)))
                 (next-single-char-property-change (point) :sesman-stop)))
        (end (next-single-char-property-change (point) :sesman-stop)))
    (move-overlay sesman-browser--stop-overlay beg end)
    (when window-system
      (let ((beg (get-text-property (point) :sesman-fragment-beg))
            (end (get-text-property (point) :sesman-fragment-end)))
        (when (and beg end)
          (move-overlay sesman-browser--section-overlay beg end))))))


;;; Sesman UI

(defun sesman-browser-quit-session ()
  "Quite session at point."
  (interactive)
  (sesman-quit (sesman-browser-get 'session)))

(defun sesman-browser-restart-session ()
  "Restart session at point."
  (interactive)
  (sesman-restart (sesman-browser-get 'session)))

(defun sesman-browser-link-with-buffer ()
  "Ask for buffer to link session at point to."
  (interactive)
  (let ((session (sesman-browser-get 'session)))
    (sesman-link-with-buffer 'ask session)))

(defun sesman-browser-link-with-directory ()
  "Ask for directory to link session at point to."
  (interactive)
  (let ((session (sesman-browser-get 'session)))
    (sesman-link-with-directory 'ask session)))

(defun sesman-browser-link-with-project ()
  "Ask for project to link session at point to."
  (interactive)
  (let ((session (sesman-browser-get 'session)))
    (sesman-link-with-project 'ask session)))

(defun sesman-browser-unlink ()
  "Unlink the link at point or ask for link to unlink."
  (interactive)
  (if-let ((link (sesman-browser-get 'link 'no-error)))
      (sesman--unlink link)
    (if-let ((links (sesman-links (sesman--system)
                                  (sesman-browser-get 'session))))
        (mapc #'sesman--unlink
              (sesman--ask-for-link "Unlink: " links 'ask-all))
      (user-error "No links for session %s" (car (sesman-browser-get 'session)))))
  (run-hooks 'sesman-post-command-hook))


;;; Major Mode

(defun sesman-browser-revert (&rest _ignore)
  "Refresh current browser buffer."
  (let ((pos (point)))
    (sesman-browser)
    ;; simple but not particularly reliable or useful
    (goto-char (min pos (point-max)))))

(defun sesman-browser-revert-all (system)
  "Refresh all Sesman SYSTEM browsers."
  (mapc (lambda (b)
          (with-current-buffer b
            (when (and (derived-mode-p 'sesman-browser-mode)
                       (eq system (sesman--system)))
              (sesman-browser-revert))))
        (buffer-list)))

(defun sesman-browser--goto-stop (stop-value)
  (let ((search t))
    (goto-char (point-min))
    (while search
      (goto-char (next-single-char-property-change (point) :sesman-stop))
      (if (eobp)
          (progn (setq search nil)
                 (goto-char (next-single-char-property-change (point-min) :sesman-stop)))
        (when (equal (get-text-property (point) :sesman-stop) stop-value)
          (setq search nil))))))

(defun sesman-browser-toggle-sort ()
  "Toggle sorting of sessions.
See `sesman-browser-sort-type' for the default sorting type."
  (interactive)
  (when (eq sesman-browser-sort-type
            (car sesman-browser--sort-types))
    (pop sesman-browser--sort-types))
  (unless sesman-browser--sort-types
    (setq-local sesman-browser--sort-types (default-value 'sesman-browser--sort-types)))
  (setq sesman-browser-sort-type (pop sesman-browser--sort-types))
  (let ((stop (sesman-browser-get :sesman-stop nil 'lax)))
    (sesman-browser)
    (sesman-browser--goto-stop stop)
    (sesman-browser--sensor-function))
  (message "Sorted by %s"
           (propertize (symbol-name sesman-browser-sort-type) 'face 'bold)))

(define-derived-mode sesman-browser-mode special-mode "SesmanBrowser"
  "Interactive view of Sesman sessions.
When applicable, system specific commands are locally bound to j when point is
on a session object."
  ;; ensure there is a sesman-system here
  (sesman--system)
  (delete-all-overlays)
  (setq-local sesman-browser--stop-overlay (make-overlay (point) (point)))
  (overlay-put sesman-browser--stop-overlay 'face 'sesman-browser-highligh-face)
  (setq-local sesman-browser--section-overlay (make-overlay (point) (point)))
  (when window-system
    (let* ((fringe-spec '(left-fringe sesman-left-bar sesman-browser-highligh-face))
           (dummy-string (propertize "|" 'display fringe-spec)))
      (overlay-put sesman-browser--section-overlay 'line-prefix dummy-string)))
  (add-hook 'sesman-post-command-hook 'sesman-browser-revert nil t)
  (setq-local display-buffer-base-action '(nil . ((inhibit-same-window . t))))
  (setq-local sesman-browser--sort-types (default-value 'sesman-browser--sort-types))
  (setq-local revert-buffer-function #'sesman-browser-revert))

(defun sesman-browser--insert-session (system ses i)
  (let ((ses-name (car ses))
        (head-template "%17s")
        beg end)
    (setq beg (point))

    ;; session header
    (insert (format "%3d: " i))
    (insert (propertize (car ses)
                        :sesman-stop ses-name
                        :sesman-vertical-stop t
                        :sesman-session-stop t
                        'face 'bold
                        'cursor-sensor-functions (list #'sesman-browser--sensor-function)
                        'mouse-face 'highlight)
            "\n")

    ;; links
    (insert (format head-template "linked-to: "))
    (let ((link-groups (sesman-grouped-links system ses))
          (vert-stop))
      (dolist (grp link-groups)
        (let* ((type (car grp)))
          (dolist (link (cdr grp))
            (when (> (current-column) fill-column)
              (insert "\n" (format head-template " "))
              (setq vert-stop nil))
            (let ((val (sesman--abbrev-path-maybe (sesman--lnk-value link))))
              (insert (propertize (sesman--format-context type val 'sesman-browser-button-face)
                                  :sesman-stop (car link)
                                  :sesman-vertical-stop (unless vert-stop (setq vert-stop t))
                                  :sesman-link link
                                  'cursor-sensor-functions (list #'sesman-browser--sensor-function)
                                  'mouse-face 'highlight)))
            (insert "  ")))))
    (insert "\n")

    ;; objects
    (insert (format head-template "objects: "))
    (let* ((info (sesman-session-info system ses))
           (map (plist-get info :map))
           (objects (plist-get info :objects))
           (strings (or (plist-get info :strings)
                        (mapcar (lambda (x) (format "%s" x)) objects)))
           (kvals (seq-mapn #'cons objects strings))
           (kvals (seq-sort (lambda (a b) (string-lessp (cdr a) (cdr b)))
                            kvals))
           (vert-stop))
      (dolist (kv kvals)
        (when (> (current-column) fill-column)
          (insert "\n" (format head-template " "))
          (setq vert-stop nil))
        (let ((str (replace-regexp-in-string ses-name "%s" (cdr kv) nil t)))
          (insert (propertize str
                              :sesman-stop str
                              :sesman-vertical-stop (unless vert-stop (setq vert-stop t))
                              :sesman-object (car kv)
                              'cursor-sensor-functions (list #'sesman-browser--sensor-function)
                              'face 'sesman-browser-button-face
                              'mouse-face 'highlight
                              'help-echo "mouse-2: visit in other window"
                              'keymap map)
                  "  "))))

    ;; session properties
    (setq end (point))
    (put-text-property beg end :sesman-session ses)
    (put-text-property beg end :sesman-session-name ses-name)
    (put-text-property beg end :sesman-fragment-beg beg)
    (put-text-property beg end :sesman-fragment-end end)
    (insert "\n\n")))

;;;###autoload
(defun sesman-browser ()
  "Display an interactive session browser.
See `sesman-browser-mode' for more details."
  (interactive)
  (let* ((system (sesman--system))
         (pop-to (called-interactively-p 'any))
         (sessions (sesman-sessions system))
         (cur-session (when pop-to
                        (sesman-current-session 'CIDER)))
         (buff (get-buffer-create (format "*sesman %s browser*" system))))
    (with-current-buffer buff
      (setq-local sesman-system system)
      (sesman-browser-mode)
      (cursor-sensor-mode 1)
      (let ((inhibit-read-only t)
            (sessions (pcase sesman-browser-sort-type
                        ('name (seq-sort (lambda (a b) (string-greaterp (car b) (car a)))
                                         sessions))
                        ('relevance (sesman--sort-sessions system sessions))
                        (_ (error "Invalid `sesman-browser-sort-type'"))))
            (i 0))
        (erase-buffer)
        (insert "\n ")
        (insert (propertize (format "%s Sessions:" system)
                            'face '(bold font-lock-keyword-face)))
        (insert "\n\n")
        (dolist (ses sessions)
          (setq i (1+ i))
          (sesman-browser--insert-session system ses i))
        (when pop-to
          (pop-to-buffer buff)
          (sesman-browser--goto-stop (car cur-session)))
        (sesman-browser--sensor-function)))))

(provide 'sesman-browser)
;;; sesman-browser.el ends here
