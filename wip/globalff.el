;;; globalff.el --- Global find file

;; Copyright (C) 2006 Tamas Patrovics

;; $Date: 2007/04/03 19:07:40 $
;; Latest version: http://www.emacswiki.org/cgi-bin/wiki/GlobalFF

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Start with M-x globallff and type in any substring of any path on
;; your system to display the matching files. The displayed list is
;; updated dynamically as you type more characters or delete some.
;;
;; Needs an up-to-date locate database for file name searching.
;;
;; Since the search is based on locate you can use any globbing
;; characters allowed by the locate command.
;;
;; You can move up/down the list with the cursor keys (I know these
;; bindings are not very Emacsian, but I happen to like them) and
;; select a file to open with Enter.
;;
;; You can quit with C-g.
;;
;; See the variable `globalff-map' for further bindings.
;;
;;
;; XEmacs port was done by Stefan Kamphausen.
;; Customize support contributed by Lennart Borgman and Stefan Kamphausen.
;; Camel case support added by Eyal Erez.
;;

;;; Code:

(eval-when-compile (require 'cl))

(if (featurep 'xemacs)
    (require 'overlay))

;;
;; User configurable variables
;;

(defgroup globalff nil
 "Globally find a file using locate."
  :tag "GlobalFF"
  :link '(url-link :tag "Home Page"
                   "http://www.emacswiki.org/cgi-bin/wiki/GlobalFF")
  :link '(emacs-commentary-link
          :tag "Commentary in globalff.el" "globalff.el")
  :prefix "globalff-"
 :group 'convenience)

(defcustom globalff-case-sensitive-search nil
 "*Whether to use case sensitive pattern matching."
 :type 'boolean
 :group 'globalff)

(defcustom globalff-regexp-search nil
 "*Whether to use regular expression pattern matching."
 :type 'boolean
 :group 'globalff)

(defcustom globalff-camelcase-search nil
 "*Whether to use camelcase pattern matching.  Camel case matching is
useful for languages like Java which start words with uppercase
letters.  So, if you want to match the filename
\"MyLongFileWithLongName\", you can type: \"MyLoFiW\", or even MLFW,
which will usually give you a close to unique match."
 :type 'boolean
 :group 'globalff)

(defcustom globalff-basename-match nil
 "*Whether to match on the base name of paths instead of on the whole
path."
 :type 'boolean
 :group 'globalff)

(defcustom globalff-databases nil
  "*List of database files separated with colon to be used by the
locate command. If nil then the system default database is used."
  :type 'string
  :group 'globalff)

(setq globalff-filter-regexps '("/target/"))

(defcustom globalff-filter-regexps nil
 "*List of regular expressions to filter out unwanted files from the
output."
 :type '(repeat regexp)
 :group 'globalff)

(defcustom globalff-transform-regexps nil
 "*List of (REGEXP . REPLACEMENT) pairs to transform matching file
path names. It's useful when the matching path names are very long and
they have a component which can safely be replaced with a shorter
indicator string.

For example this rule:

    (push '(\"^/very/long/path/to/projectx/\" . \"<projx>/\")
      globalff-transform-regexps)

will display file names under \"projectx\" like this:

    <projx>/sources/main.c
    <projx>/sources/test.c

"
 :type '(repeat (cons regexp regexp))
 :group 'globalff)

(defcustom globalff-minimum-input-length 5
  "*The minimum number of characters needed to start file searching."
  :type 'integer
  :group 'globalff)

(defcustom globalff-search-delay 0.5
  "*Idle time after last input event, before starting the search."
  :type 'number
  :group 'globalff)

;(setq globalff-matching-filename-limit 100)


(defcustom globalff-matching-filename-limit 500
  "*If there are more matching file names than the given limit the
search is terminated automatically. This is useful if a too broad
search input is given and there are hundreds or thousands of matches.

If you don't want to limit the number of matches then set it to nil
instead of a very high number."
  :type '(choice integer (const nil))
  :group 'globalff)

(defcustom globalff-adaptive-selection nil
 "*If enabled the last file chosen for the same input is preselected
automatically instead of the first one in the list. If no exact input
match is found then the most recent input pattern which matches the
beginning of the current input is used.

Doesn't do anything if the user moves the selection manually, before a
file is selected automatically.

This option makes it possible to use a short input string to locate a
previously visited file again quickly."
 :type 'boolean
 :group 'globalff)

(defcustom globalff-history-length 100
  "*Number of previous file selections saved if
`globalff-adaptive-selection' is enabled."
  :type 'integer
  :group 'globalff)

(defcustom globalff-history-file "~/.globalff_history"
  "*Name of the history file where previous file selections saved if
`globalff-adaptive-selection' is enabled."
  :type 'file
  :group 'globalff)


(defface globalff-selection-face
  ;; check if inherit attribute is supported
  (if (assq :inherit custom-face-attributes)
      '((t (:inherit highlight :underline nil)))

    '((((class color) (background light))
       (:background "darkseagreen2"))
      (((class color) (background dark))
       (:background "darkolivegreen"))
      (t (:inverse-video t))))
  "Face for highlighting the currently selected file name.")


(defvar globalff-map
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "C-n") 'globalff-next-line)
    (define-key map (kbd "C-p") 'globalff-previous-line)
    (define-key map (kbd "<prior>") 'globalff-previous-page)
    (define-key map (kbd "<next>") 'globalff-next-page)
    (define-key map (kbd "C-c") 'globalff-toggle-case-sensitive-search)
    ;; I wanted to choose C-t as a homage to iswitchb, but
    ;; transpose-chars can be useful during pattern editing
    (define-key map (kbd "C-r") 'globalff-toggle-regexp-search)
    (define-key map (kbd "C-t") 'globalff-toggle-camelcase-search)
    (define-key map (kbd "C-w") 'globalff-toggle-basename-match)
    (define-key map (kbd "C-s") 'globalff-toggle-around-globs)
    (define-key map (kbd "<RET>") 'globalff-exit-minibuffer)
    (define-key map (kbd "C-<return>") 'globalff-copy-file-name-and-exit)
    map)
  "Keymap for globalff.")

;;
;; End of user configurable variables
;;

(defconst globalff-buffer "*globalff*"
  "Buffer used for finding files.")

(defconst globalff-process nil
  "The current search process.")

(defvar globalff-previous-input ""
  "The previous input substring used for searching.")

(defvar globalff-overlay nil
  "Overlay used to highlight the current selection.")

(defvar globalff-history nil
  "List of the previous file selections if
`globalff-adaptive-selection' is enabled.")

(defvar globalff-adaptive-selection-target nil
  "The search output filter looks for this file name in the output if
`globalff-adaptive-selection' is enabled.")

(defun identity-filter (process string)
  (with-current-buffer globalff-buffer
      (save-excursion
        (goto-char (point-max))
        (insert string)
        (insert "\n===== end ====="))))

(defun dumb-filter (process string)
  (with-current-buffer globalff-buffer
      (save-excursion
        (goto-char (point-max))

        (insert "\n===== end ====="))))



(setq globalff-output "")

(setq globalff-score-cache (make-hash-table :test 'equal))
(setq globalff-result-cache (make-hash-table :test 'equal))

(defun globalff-output-filter (process string)
  "Avoid moving of point if the buffer is empty."
(print "*")
  (setq globalff-output (concat globalff-output string))

  (with-current-buffer globalff-buffer
    (save-excursion
      (erase-buffer)

      (defun mem-score (n)
        (let ((cached-score (gethash '(n globalff-previous-input) globalff-score-cache)))
          (if (eql cached-score nil)
              (puthash '(n globalff-previous-input)
                       (first (b-flx-score n globalff-previous-input))
                       globalff-score-cache)
            cached-score)))

      (defun pred1 (x) (string-match "/target/" x))

      (if (< 300 (list-length (split-string globalff-output)))
          (progn (globalff-kill-process)
              (globalff-set-state "killed")
              (insert "too many results!" ))

        (let* ((split-list (split-string globalff-output))
               (filter-list (remove-if #'pred1 split-list))
               (sort-list (sort filter-list
                                (lambda (a b) (>= (mem-score a)
                                                  (mem-score b)))))
               (final-str (join-string sort-list "\n")))

          (insert final-str)
;          (insert globalff-output)
          (insert "\n==end=="))))

    (if (= (overlay-start globalff-overlay) ; no selection yet
           (overlay-end globalff-overlay))
        (unless (= (point-at-eol) (point-max)) ; incomplete line
          (globalff-mark-current-line)))))


(defun globalff-mark-current-line ()
  "Mark current line with a distinctive color."
  (move-overlay globalff-overlay (point-at-bol) (point-at-eol)))


(defun globalff-previous-line ()
  "Move selection to the previous line."
  (interactive)
  (globalff-move-selection 'next-line -1))


(defun globalff-next-line ()
  "Move selection to the next line."
  (interactive)
  (globalff-move-selection 'next-line 1))


(defun globalff-previous-page ()
  "Move selection back with a pageful."
  (interactive)
  (globalff-move-selection 'scroll-down nil))


(defun globalff-next-page ()
  "Move selection forward with a pageful."
  (interactive)
  (globalff-move-selection 'scroll-up nil))


(defun globalff-move-selection (movefunc movearg)
  "Move the selection marker to a new position determined by
MOVEFUNC and MOVEARG."
  (unless (= (buffer-size (get-buffer globalff-buffer)) 0)
    (save-selected-window
      (select-window (get-buffer-window globalff-buffer))

      (condition-case nil
          (funcall movefunc movearg)
        (beginning-of-buffer (goto-char (point-min)))
        (end-of-buffer (goto-char (point-max))))

      ;; if line end is point-max then it's either an incomplete line or
      ;; the end of the output, so move up a line
      (if (= (point-at-eol) (point-max))
          (next-line -1))

      ;; if the user moved the selection then adaptive selection
      ;; shouldn't touch it
      (setq globalff-adaptive-selection-target nil)

      (globalff-mark-current-line))))


(defun globalff-process-sentinel (process event)
  "Prevent printing of process status messages into the output buffer."
  (unless (eq 'run (process-status process))
    (globalff-set-state "finished")))


(defun globalff-check-input ()
  "Check input string and start/stop search if necessary."
  (if (sit-for globalff-search-delay)
      (unless (equal (minibuffer-contents) globalff-previous-input)
        (globalff-restart-search))))


(defun globalff-restart-search ()
  "Stop the current search if any and start a new one if needed."

  (let ((input (minibuffer-contents)))
    (setq globalff-previous-input input)
    (setq globalff-output "")

    (globalff-kill-process)
    (with-current-buffer globalff-buffer
      (erase-buffer))
    (globalff-set-state "idle")

    (unless (or (equal input "")
                (< (length input) globalff-minimum-input-length))
      (let ((cmd (append

                  (unless globalff-case-sensitive-search
                    (list "-i"))

                  (if globalff-basename-match
                      (list "-b"))

                  (when globalff-databases
                    (list (concat "--database="
                                  globalff-databases)))

                  (if globalff-regexp-search
                      (list "-r"))

                  (list (globalff-wild-generate input)))))
        (print "command:")
        (print cmd)
        (setq globalff-process
              (apply 'start-process "globalff-process" nil
                     "mlocate"
                     cmd)))

      (globalff-set-state "searching")
      (move-overlay globalff-overlay (point-min) (point-min))

      (if globalff-adaptive-selection
          (let ((item (assoc input globalff-history)))
            ;; if no exact match found then try prefix match
            (unless item
              (let ((input-length (length input)))
                (setq item
                      (some (lambda (test-item)
                              (let ((str (car test-item)))
                                (when (and (> (length str) input-length)
                                           (string= (substring str 0
                                                               input-length)
                                                    input))
                                  test-item)))

                            globalff-history))))

            (setq globalff-adaptive-selection-target (cdr ))))

      (set-process-filter globalff-process 'globalff-output-filter)
                                        ;      (set-process-filter globalff-process 'identity-filter)
                                        ;      (set-process-filter globalff-process 'dumb-filter)
      (set-process-sentinel globalff-process 'globalff-process-sentinel))))


(defun globalff-camelcase-generate (string)
  "Generates the camel case matching by add .* before each capital
letter."
  (let ((result "") (index 0) (case-fold-search nil))
    (progn
      (while (< index (length string))
        (let ((c (substring string index (+ index 1))))
          (setq result
                (if (or (string= c "A") (string= c "B") (string= c "C") (string= c "D")
                        (string= c "E") (string= c "F") (string= c "G") (string= c "H")
                        (string= c "I") (string= c "J") (string= c "K") (string= c "L")
                        (string= c "M") (string= c "N") (string= c "O") (string= c "P")
                        (string= c "Q") (string= c "R") (string= c "S") (string= c "T")
                        (string= c "U") (string= c "V") (string= c "W") (string= c "X")
                        (string= c "Y") (string= c "Z"))
                    (concat result ".*" c)
                  (concat result c))
                index (+ index 1))))
      (concat "" result))))



(defun globalff-wild-generate (string)
  (concat "*"
          (replace-regexp-in-string "\s+" "*" string)
          "*"))

(defun globalff-kill-process ()
  "Kill find process."
  (when globalff-process
    ;; detach associated functions
    (set-process-filter globalff-process nil)
    (set-process-sentinel globalff-process nil)
    (delete-process globalff-process)
    (setq globalff-process nil)))


(defun globalff-set-state (state)
  "Set STATE in mode line."
  (with-current-buffer globalff-buffer
    (setq mode-line-process (concat ":" (if globalff-case-sensitive-search
                                            "case"
                                          "nocase")
                                    "/"  (if globalff-regexp-search
                                            "regexp"
                                           "glob")
                                    "/"  (if globalff-camelcase-search
                                            "camel"
                                           "nocamel")
                                    "/"  (if globalff-basename-match
                                            "base"
                                           "whole")
                                    ":" state))
    (force-mode-line-update)))


(defun globalff-toggle-case-sensitive-search ()
  "Toggle state of case sensitive pattern matching."
  (interactive)
  (setq globalff-case-sensitive-search (not globalff-case-sensitive-search))
  (globalff-restart-search))


(defun globalff-toggle-regexp-search ()
  "Toggle state of regular expression pattern matching."
  (interactive)
  (setq globalff-regexp-search (not globalff-regexp-search))
  (globalff-restart-search))


(defun globalff-toggle-camelcase-search ()
  "Toggle state of camelcase pattern matching."
  (interactive)
  (setq globalff-camelcase-search (not globalff-camelcase-search))
  (if globalff-camelcase-search
      (setq globalff-case-sensitive-search t
	    globalff-regexp-search t))
  (globalff-restart-search))

(defun globalff-toggle-basename-match ()
  "Toggle matching on basename vs. on whole path."
  (interactive)
  (setq globalff-basename-match (not globalff-basename-match))
  (globalff-restart-search))


(defun globalff-toggle-around-globs ()
  "Put/remove asterisks around pattern if glob matching is used. This
make it easier to use globs, since by default glob patterns have to
match the file name exactly."
  (interactive)
  (unless globalff-regexp-search
    (let* ((pattern (minibuffer-contents))
           (len (length pattern)))
      (if (> len 2)
          (save-excursion
            (if  (and (= (aref pattern 0) ?*)
                      (= (aref pattern (1- len)) ?*))
                ;; remove asterisks from around pattern
                (progn
                  (beginning-of-line)
                  (delete-char 1)
                  (end-of-line)
                  (delete-char -1))

                ;; put asterisks around pattern
              (beginning-of-line)
              (insert "*")
              (end-of-line)
              (insert "*")))))))


(defun globalff-exit-minibuffer ()
  "Store the current pattern and file name in `globalff-history' if
`globalff-adaptive-selection' is enabled and exit the minibuffer."
  (interactive)
  (if globalff-adaptive-selection
      (let ((input (minibuffer-contents))
            (selected (globalff-get-selected-file t)))
        (unless (or (equal input "")
                    (equal selected ""))
          (let ((item (assoc input globalff-history)))
            (if item
                (setq globalff-history (delete item globalff-history)))
            (push (cons input selected) globalff-history)

            (if (> (length globalff-history) globalff-history-length)
                (nbutlast globalff-history))))))

  (exit-minibuffer))


(defun globalff-copy-file-name-and-exit ()
  "Copy selected file name and abort the search."
  (interactive)
  (kill-new (globalff-get-selected-file))
  (exit-minibuffer))


(when globalff-adaptive-selection
  (load-file globalff-history-file)
  (add-hook 'kill-emacs-hook 'globalff-save-history))

(defun globalff-save-history ()
  "Save history of used pattern-file name pairs used by
`globalff-adaptive-selection'."
  (interactive)
  (with-temp-buffer
    (insert
     ";; -*- mode: emacs-lisp -*-\n"
     ";; History entries used for globalff adaptive selection.\n")
    (prin1 `(setq globalff-history ',globalff-history) (current-buffer))
    (insert ?\n)
    (write-region (point-min) (point-max) globalff-history-file nil
                  (unless (interactive-p) 'quiet))))


(defun globalff-get-selected-file (&optional rendered)
  "Return the currently selected file path.

If RENDERED is non-nil then the visible path name is returned instead
of the real path name of the file."
  (with-current-buffer globalff-buffer
    (or (and (not rendered)
             (get-text-property (overlay-start globalff-overlay)
                                'globalff-orig-filename))
        (buffer-substring-no-properties (overlay-start globalff-overlay)
                                        (overlay-end globalff-overlay)))))


(defun globalff-do ()
  "The guts of globalff.

It expects that `globalff-buffer' is selected already."
  (erase-buffer)
  (setq mode-name "GlobalFF")

  (if globalff-overlay
      ;; make sure the overlay belongs to the globalff buffer if
      ;; it's newly created
      (move-overlay globalff-overlay (point-min) (point-min)
                    (get-buffer globalff-buffer))

    (setq globalff-overlay (make-overlay (point-min) (point-min)))
    (overlay-put globalff-overlay 'face 'globalff-selection-face))

  (globalff-set-state "idle")
  (setq globalff-previous-input "")
  (add-hook 'post-command-hook 'globalff-check-input)

  (with-current-buffer globalff-buffer
    (setq cursor-type nil))

  (unwind-protect
      (let ((minibuffer-local-map globalff-map))
        (read-string "substring: "))

    (remove-hook 'post-command-hook 'globalff-check-input)
    (globalff-kill-process)

    (with-current-buffer globalff-buffer
      (setq cursor-type t))))


(defun globalff ()
  "Start global find file."
  (interactive)
  (let ((winconfig (current-window-configuration)))
    (pop-to-buffer globalff-buffer)
    (unwind-protect
        (globalff-do)
      (set-window-configuration winconfig)))

  (unless (or (= (buffer-size (get-buffer globalff-buffer)) 0)
              (eq this-command 'globalff-copy-file-name-and-exit))
    (find-file (globalff-get-selected-file))))

;; this feature is experimental, that's why the variables are here

;; customize the popup frame according to your own taste and set (setq
;; gnuserv-frame (selected-frame)) to prevent gnuserv's own frame from
;; appearing

(setq globalff-popup-frame (make-frame '((name . "Select file")
                                         (height . 30)
                                         (top . 200)
                                         (visibility . nil))))


;;
;; Invoke this function outside of emacs with
;;
;; 	gnuclient -eval '(globalff-get-file-and-insert)'
;;
;; You can bind it to a global hotkey using xbindkeys. Note that the
;; function below uses xte to send the path to the active window.
;;

(defun globalff-get-file-and-insert ()
  "Select a file with globalff and send the path to the currently
selected application."
  (interactive)
  (let ((frame (selected-frame)))
    (unwind-protect
        (progn
          (make-frame-visible globalff-popup-frame)
          (select-frame globalff-popup-frame)
          (switch-to-buffer globalff-buffer)
          (globalff-do))

      (make-frame-invisible globalff-popup-frame)))

  (let ((file (globalff-get-selected-file)))
    (unless (equal file "")
      ;; it's needed for some reason
      (sit-for 1)
      (call-process "xte" nil nil nil (concat "str " file)))))


;;; XEmacs compatibility

(unless (fboundp 'minibuffer-contents)
  (defun minibuffer-contents ()
    "Return the user input in a minbuffer as a string.
The current buffer must be a minibuffer."
    (field-string (point-max))))

(provide 'globalff)
;;; globalff.el ends here
