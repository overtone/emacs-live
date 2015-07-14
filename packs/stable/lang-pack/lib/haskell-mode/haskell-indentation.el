;;; haskell-indentation.el -- indentation module for Haskell Mode

;; Copyright (C) 2013  Kristof Bastiaensen, Gergely Risko

;; Author: Kristof Bastiaensen <kristof.bastiaensen@vleeuwen.org>
;; Author: Gergely Risko <errge@nilcons.com>
;; Keywords: indentation haskell
;; URL: https://github.com/haskell/haskell-mode
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Installation:
;;
;; To turn indentation on for all Haskell buffers under Haskell mode
;; <http://www.haskell.org/haskell-mode/> add this to .emacs:
;;
;;    (add-hook haskell-mode-hook 'haskell-indentation-mode)
;;
;; Otherwise, call `haskell-indentation-mode'.

;;; Code:

(require 'hl-line)
(require 'syntax)
(require 'cl-lib)

(defvar haskell-indentation-dyn-first-position)
(defvar haskell-indentation-dyn-last-direction)
(defvar haskell-indentation-dyn-last-indentations)


(defgroup haskell-indentation nil
  "Haskell indentation."
  :link '(custom-manual "(haskell-mode)Indentation")
  :group 'haskell
  :prefix "haskell-indentation-")

(defcustom haskell-indentation-show-indentations nil
  "If t the current line's indentation points will be showed as
underscore overlays in new haskell-mode buffers.  Use
`haskell-indentation-enable-show-indentations' and
`haskell-indentation-disable-show-indentations' to switch the
behavior for already existing buffers."
  :type 'boolean
  :group 'haskell-indentation)

(defcustom haskell-indentation-show-indentations-after-eol nil
  "If t, try to show indentation points after the end of line.
This requires strange overlay hacks and can collide with other
modes (e.g. fill-column-indicator)."
  :type 'boolean
  :group 'haskell-indentation)

(defface haskell-indentation-show-normal-face
  '((t :underline t))
  "Default face for indentations overlay."
  :group 'haskell-indentation)

(defface haskell-indentation-show-hl-line-face
  '((t :underline t :inherit hl-line))
  "Face used for indentations overlay after EOL if hl-line mode is enabled."
  :group 'haskell-indentation)


(defcustom haskell-indentation-indent-leftmost t
  "Indent to the left margin after certain keywords (for example after let .. in, case .. of).  If set to t it will only indent to the left.  If nil only relative to the containing expression.  If set to the keyword 'both then both positions are allowed."
  :type 'symbol
  :group 'haskell-indentation)

(defcustom haskell-indentation-layout-offset 2
  "Extra indentation to add before expressions in a haskell layout list."
  :type 'integer
  :group 'haskell-indentation)

(defcustom haskell-indentation-starter-offset 2
  "Extra indentation after an opening keyword (e.g. let)."
  :type 'integer
  :group 'haskell-indentation)

(defcustom haskell-indentation-left-offset 2
  "Extra indentation after an indentation to the left (e.g. after do)."
  :type 'integer
  :group 'haskell-indentation)

(defcustom  haskell-indentation-ifte-offset 2
  "Extra indentation after the keywords `if' `then' or `else'."
  :type 'integer
  :group 'haskell-indentation)

(defcustom haskell-indentation-where-pre-offset 2
  "Extra indentation before the keyword `where'."
  :type 'integer
  :group 'haskell-indentation)

(defcustom haskell-indentation-where-post-offset 2
  "Extra indentation after the keyword `where'."
  :type 'integer
  :group 'haskell-indentation)

(defconst haskell-indentation-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "RET") 'haskell-indentation-newline-and-indent)
    (define-key keymap (kbd "<backtab>") 'haskell-indentation-indent-backwards)
    keymap))

;;;###autoload
(define-minor-mode haskell-indentation-mode
  "Haskell indentation mode that deals with the layout rule.
It rebinds RET, DEL and BACKSPACE, so that indentations can be
set and deleted as if they were real tabs.  It supports
autofill-mode.

It is possible to render indent stops for current line as
overlays.  Please read documentation for option
`haskell-indentation-enable-show-indentations' and option
`haskell-indentation-show-indentations-after-eol'.  These options
were disabled by default because in most cases occurs overlay
clashing with other modes."
  :lighter " Ind"
  :keymap haskell-indentation-mode-map
  (kill-local-variable 'indent-line-function)
  (kill-local-variable 'indent-region-function)
  (kill-local-variable 'normal-auto-fill-function)
  (when haskell-indentation-mode
    (setq max-lisp-eval-depth (max max-lisp-eval-depth 600)) ;; set a higher limit for recursion
    (set (make-local-variable 'indent-line-function) 'haskell-indentation-indent-line)
    (set (make-local-variable 'indent-region-function) 'haskell-indentation-indent-region)
    (set (make-local-variable 'normal-auto-fill-function) 'haskell-indentation-auto-fill-function)
    (when haskell-indentation-show-indentations (haskell-indentation-enable-show-indentations))))

;;;###autoload
(defun turn-on-haskell-indentation ()
  "Turn on the haskell-indentation minor mode."
  (interactive)
  (haskell-indentation-mode t))
(make-obsolete 'turn-on-haskell-indentation
               'haskell-indentation-mode
               "2015-05-25")

(defun haskell-indentation-parse-error (&rest args)
  (let ((msg (apply 'format args)))
    (message "%s" msg)
    (throw 'parse-error msg)))

(defvar haskell-literate)
(defun haskell-indentation-birdp ()
  "Return t if this is a literate haskell buffer in bird style, nil otherwise."
  (eq haskell-literate 'bird))

;;---------------------------------------- UI starts here

(defun haskell-indentation-auto-fill-function ()
  (when (> (current-column) fill-column)
    (while (> (current-column) fill-column)
      (skip-syntax-backward "-")
      (skip-syntax-backward "^-"))
    (let ((indent (car (last (haskell-indentation-find-indentations-safe)))))
      (delete-horizontal-space)
      (newline)
      (when (haskell-indentation-birdp) (insert ">"))
      (indent-to indent)
      (end-of-line))))

(defun haskell-indentation-reindent-to (col &optional move)
  "Reindent current line to COL, also move the point there if MOVE"
  (let* ((cc (current-column))
         (ci (haskell-indentation-current-indentation)))
    (save-excursion
      (move-to-column ci)
      (if (<= ci col)
          (insert-before-markers (make-string (- col ci) ? ))
        (delete-char (- col ci))))
    (when move
      (move-to-column col))))

(defun haskell-indentation-indent-rigidly (start end arg)
  "Indent all lines starting in the region sideways by ARG columns.
Called from a program, takes three arguments, START, END and ARG.
You can remove all indentation from a region by giving a large negative ARG.
Handles bird style literate haskell too."
  (interactive "r\np")
  (save-excursion
    (goto-char end)
    (let ((end-marker (point-marker)))
      (goto-char start)
      (or (bolp) (forward-line 0))
      (while (< (point) end-marker)
        (let ((ci (haskell-indentation-current-indentation)))
          (when (and t
                     (eq (char-after) ?>))
            (forward-char 1))
          (skip-syntax-forward "-")
          (unless (eolp)
            (haskell-indentation-reindent-to (max 0 (+ ci arg))))
          (forward-line 1)))
      (move-marker end-marker nil))))

(defun haskell-indentation-current-indentation ()
  "Column position of first non whitespace character in current line"
  (save-excursion
    (beginning-of-line)
    (when (haskell-indentation-birdp) (forward-char))
    (skip-syntax-forward "-")
    (current-column)))

(defun haskell-indentation-bird-outside-codep ()
  "True iff we are in bird literate mode, but outside of code"
  (and (haskell-indentation-birdp)
       (or (< (current-column) 2)
           (save-excursion
             (beginning-of-line)
             (not (eq (char-after) ?>))))))

(defun haskell-indentation-delete-horizontal-space-and-newline ()
  (delete-horizontal-space)
  (newline))

(defun haskell-indentation-newline-and-indent ()
  "Ran on C-j or RET"
  (interactive)
  ;; On RET (or C-j), we:
  ;;   - just jump to the next line if literate haskell, but outside code
  (if (haskell-indentation-bird-outside-codep)
      (haskell-indentation-delete-horizontal-space-and-newline)
    ;; - just jump to the next line if parse-error
    (catch 'parse-error
     (haskell-indentation-delete-horizontal-space-and-newline)
     (let* ((cc (current-column))
            (ci (haskell-indentation-current-indentation))
            (indentations (haskell-indentation-find-indentations-safe)))
       (when (haskell-indentation-birdp) (insert "> "))
       (haskell-indentation-reindent-to
        (haskell-indentation-next-indentation (- ci 1) indentations 'nofail)
        'move)))))

(defun haskell-indentation-next-indentation (col indentations &optional nofail)
  "Find the leftmost indentation which is greater than COL.
Or returns the last indentation if there are no bigger ones and
NOFAIL is non-nil."
  (when (null indentations) (error "haskell-indentation-next-indentation called with empty list"))
  (or (cl-find-if #'(lambda (i) (> i col)) indentations)
      (when nofail (car (last indentations)))))

(defun haskell-indentation-previous-indentation (col indentations &optional nofail)
  "Find the rightmost indentation which is less than COL."
  (when (null indentations) (error "haskell-indentation-previous-indentation called with empty list"))
  (let ((rev (reverse indentations)))
    (or (cl-find-if #'(lambda (i) (< i col)) rev)
        (when nofail (car rev)))))

(defun haskell-indentation-indent-line ()
  "Auto indentation on TAB.
Do nothing inside multiline comments and multiline strings.
Start enumerating the indentation points to the right.  The user
can continue by repeatedly pressing TAB.  When there is no more
indentation points to the right, we switch going to the left."
  (interactive)
  ;; try to repeat
  (when (not (haskell-indentation-indent-line-repeat))
    (setq haskell-indentation-dyn-last-direction nil)
    ;; do nothing if we're inside a string or comment
    (unless (save-excursion
              (beginning-of-line)
              (nth 8 (syntax-ppss)))
      ;; parse error is intentionally not catched here, it may come from
      ;; haskell-indentation-find-indentations-safe, but escapes the scope and aborts the
      ;; opertaion before any moving happens
      (let* ((cc (current-column))
             (ci (haskell-indentation-current-indentation))
             (inds (save-excursion
                     (move-to-column ci)
                     (haskell-indentation-find-indentations-safe)))
             (valid (memq ci inds))
             (cursor-in-whitespace (< cc ci)))
        ;; can't happen right now, because of -safe, but we may want to have this in the future
        ;; (when (null inds)
        ;;   (error "returned indentations empty, but no parse error"))
        (if (and valid cursor-in-whitespace)
            (move-to-column ci)
          (haskell-indentation-reindent-to (haskell-indentation-next-indentation ci inds 'nofail) cursor-in-whitespace))
        (setq haskell-indentation-dyn-last-direction 'right)
        (setq haskell-indentation-dyn-first-position (haskell-indentation-current-indentation))
        (setq haskell-indentation-dyn-last-indentations inds)))))

(defun haskell-indentation-indent-line-repeat ()
  "Ran if the user repeatedly presses the TAB key"
  (cond
   ((and (memq last-command '(indent-for-tab-command haskell-indentation-indent-backwards))
         (eq haskell-indentation-dyn-last-direction 'region))
    (let ((mark-even-if-inactive t))
      (haskell-indentation-indent-rigidly (region-beginning) (region-end) 1))
    t)
   ((and (eq last-command 'indent-for-tab-command)
         (memq haskell-indentation-dyn-last-direction '(left right))
         haskell-indentation-dyn-last-indentations)
    (let* ((cc (current-column))
           (ci (haskell-indentation-current-indentation)))
      (if (eq haskell-indentation-dyn-last-direction 'left)
          (haskell-indentation-reindent-to (haskell-indentation-previous-indentation ci haskell-indentation-dyn-last-indentations 'nofail))
        ;; right
        (if (haskell-indentation-next-indentation ci haskell-indentation-dyn-last-indentations)
            (haskell-indentation-reindent-to (haskell-indentation-next-indentation ci haskell-indentation-dyn-last-indentations 'nofail))
          ;; but failed, switch to left
          (setq haskell-indentation-dyn-last-direction 'left)
          ;; and skip to the point where the user started pressing TABs.
          ;; except if there are <= 2 indentation points, because this
          ;; behavior is very confusing in that case
          (when (< 2 (length haskell-indentation-dyn-last-indentations))
            (haskell-indentation-reindent-to haskell-indentation-dyn-first-position))
          (haskell-indentation-indent-line-repeat))))
    t)
   (t nil)))

(defun haskell-indentation-indent-region (start end)
  (setq haskell-indentation-dyn-last-direction 'region)
  (haskell-indentation-indent-rigidly start end 1)
  (message "Press TAB or S-TAB again to indent the region more"))

(defun haskell-indentation-indent-backwards ()
  "Indent the current line to the previous indentation point"
  (interactive)
  (cond
   ((and (memq last-command '(indent-for-tab-command haskell-indentation-indent-backwards))
         (eq haskell-indentation-dyn-last-direction 'region))
    (let ((mark-even-if-inactive t))
      (haskell-indentation-indent-rigidly (region-beginning) (region-end) -1)))
   ((use-region-p)
    (setq haskell-indentation-dyn-last-direction 'region)
    (haskell-indentation-indent-rigidly (region-beginning) (region-end) -1)
    (message "Press TAB or S-TAB again to indent the region more"))
   (t
    (setq haskell-indentation-dyn-last-direction nil)
    (let* ((cc (current-column))
           (ci (haskell-indentation-current-indentation))
           (inds (save-excursion
                   (move-to-column ci)
                   (haskell-indentation-find-indentations-safe)))
           (cursor-in-whitespace (< cc ci))
           (pi (haskell-indentation-previous-indentation ci inds)))
      (if (null pi)
          ;; if there are no more indentations to the left, just go to column 0
          (haskell-indentation-reindent-to (car (haskell-indentation-first-indentation)) cursor-in-whitespace)
        (haskell-indentation-reindent-to pi cursor-in-whitespace))))))

;;---------------------------------------- haskell-indentation show indentations UI starts here
(defvar haskell-indentation-dyn-show-indentations nil
  "Whether showing of indentation points is enabled in this buffer.")
(make-variable-buffer-local 'haskell-indentation-dyn-show-indentations)
(defvar haskell-indentation-dyn-overlays nil
  "Overlays used by haskell-indentation-enable-show-indentations.")
(make-variable-buffer-local 'haskell-indentation-dyn-overlays)

(defun haskell-indentation-init-overlays (n)
  "Makes sure that haskell-indentation-dyn-overlays contains at least N overlays."
  (let* ((clen (length haskell-indentation-dyn-overlays))
         (needed (- n clen)))
    (dotimes (n needed haskell-indentation-dyn-overlays)
      (setq haskell-indentation-dyn-overlays
            (cons (make-overlay 1 1) haskell-indentation-dyn-overlays)))))

(defun haskell-indentation-unshow-overlays ()
  "Unshows all the overlays."
  (mapc #'delete-overlay haskell-indentation-dyn-overlays))

(defvar haskell-indentation-pending-delay-show-overlays nil
  "Indicates that there are pending overlays to be shown.

Holds time object value as received from `run-at-time'.

Used to debounce `haskell-indentation-delay-show-overlays'.")
(make-local-variable 'haskell-indentation-pending-delay-show-overlays)

(defun haskell-indentation-delay-show-overlays ()
  "Show overlays after a little while so that it does not get in
the way of normal cursor movement.

If there is a running show overlays timer cancel it first."
  (when haskell-indentation-pending-delay-show-overlays
    (cancel-timer haskell-indentation-pending-delay-show-overlays))
  (setq haskell-indentation-pending-delay-show-overlays
        (run-at-time "0.1 sec" nil
                     (lambda ()
                       (setq haskell-indentation-pending-delay-show-overlays nil)
                       (haskell-indentation-show-overlays)))))

(defun haskell-indentation-show-overlays ()
  "Put an underscore overlay at all the indentations points in
the current buffer."
  (if (and (memq major-mode '(haskell-mode literate-haskell-mode))
           (memq 'haskell-indentation-mode minor-mode-list)
           haskell-indentation-dyn-show-indentations)
      (catch 'parse-error
        (save-excursion
          (let* ((columns (progn
                            (end-of-line)
                            (current-column)))
                 (ci (haskell-indentation-current-indentation))
                 (allinds (save-excursion
                            (move-to-column ci); XXX: remove when haskell-indentation-find-indentations is fixed
                            ;; don't freak out on parse-error
                            (haskell-indentation-find-indentations-safe)))
                 ;; indentations that are easy to show
                 (inds (cl-remove-if (lambda (i) (>= i columns)) allinds))
                 ;; tricky indentations, that are after the current EOL
                 (overinds (cl-member-if (lambda (i) (>= i columns)) allinds))
                 ;; +1: leave space for an extra overlay to show overinds
                 (overlays (haskell-indentation-init-overlays (+ 1 (length inds)))))
            (while inds
              (move-to-column (car inds))
              (overlay-put (car overlays) 'face 'haskell-indentation-show-normal-face)
              (overlay-put (car overlays) 'after-string nil)
              (move-overlay (car overlays) (point) (+ 1 (point)))
              (setq inds (cdr inds))
              (setq overlays (cdr overlays)))
            (when (and overinds
                       haskell-indentation-show-indentations-after-eol)
              (let ((o (car overlays))
                    (s (make-string (+ 1 (- (car (last overinds)) columns)) ? )))
                ;; needed for the cursor to be in the good position, see:
                ;;   http://lists.gnu.org/archive/html/bug-gnu-emacs/2013-03/msg00079.html
                (put-text-property 0 1 'cursor t s)
                ;; color the whole line ending overlay with hl-line face if needed
                (when (or hl-line-mode global-hl-line-mode)
                  (put-text-property 0 (length s) 'face 'hl-line s))
                ;; put in the underlines at the correct positions
                (dolist (i overinds)
                  (put-text-property
                   (- i columns) (+ 1 (- i columns))
                   'face (if (or hl-line-mode global-hl-line-mode)
                             'haskell-indentation-show-hl-line-face
                           'haskell-indentation-show-normal-face)
                   s))
                (overlay-put o 'face nil)
                (overlay-put o 'after-string s)
                (end-of-line)
                (move-overlay o (point) (point)))))))))

(defun haskell-indentation-enable-show-indentations ()
  "Enable showing of indentation points in the current buffer."
  (interactive)
  (setq haskell-indentation-dyn-show-indentations t)
  (setq haskell-indentation-pending-delay-show-overlays nil)
  (add-hook 'change-major-mode-hook #'haskell-indentation-unshow-overlays nil t)
  (add-hook 'pre-command-hook #'haskell-indentation-unshow-overlays nil t)
  (add-hook 'post-command-hook #'haskell-indentation-delay-show-overlays nil t))

(defun haskell-indentation-disable-show-indentations ()
  "Disable showing of indentation points in the current buffer."
  (interactive)
  (setq haskell-indentation-dyn-show-indentations nil)
  (remove-hook 'post-command-hook #'haskell-indentation-delay-show-overlays t)
  (haskell-indentation-unshow-overlays)
  (remove-hook 'change-major-mode-hook #'haskell-indentation-unshow-overlays t)
  (remove-hook 'pre-command-hook #'haskell-indentation-unshow-overlays t))

;;---------------------------------------- parser starts here

;; The parser is implemented als a recursive descent parser.  Each
;; parser advances the point to after the expression it parses, and
;; sets the dynamic scoped variables containing the information about
;; the indentations.  The dynamic scoping allows transparent
;; backtracking to previous states of these variables.  A new state
;; can be set using LET.  When the scope of this function ends,
;; the variable is automatically reverted to it's old value.

;; This is basicly a performance hack.  It would have been possible
;; to thread this state using a association-list through the parsers, but it
;; would be probably more complicated and slower due to the lack
;; of real closures in ELISP.
;;
;; When finished parsing, the tokenizer returns 'end-token, and
;; following-token is set to the token after point.  The parser adds
;; its indentations to possible-indentations and returns to it's
;; parent, or exits non-locally by throwing parse-end, so that the
;; parent will not add new indentations to it.

;; the parse 'state':
(defvar following-token)   ;; the next token after parsing finished
(defvar current-token)     ;;; the token at the current parser point or a pseudo-token (see haskell-indentation-read-next-token)
(defvar left-indent)       ;; most left possible indentation
(defvar starter-indent)    ;; column at a keyword
(defvar current-indent)    ;; the most right indentation
(defvar layout-indent)     ;; the column of the layout list
(defvar parse-line-number) ;; the number of lines parsed
(defvar possible-indentations) ;; the return value of the indentations
(defvar indentation-point) ;; where to stop parsing

(defun haskell-indentation-goto-least-indentation ()
  (beginning-of-line)
  (if (haskell-indentation-birdp)
      (catch 'return
        (while t
          (when (not (eq (char-after) ?>))
            (forward-line)
            (forward-char 2)
            (throw 'return nil))
          (let ((ps (nth 8 (syntax-ppss))))
            (when ps ;; inside comment or string
              (goto-char ps)
              (beginning-of-line)))
          (when (and (>= 2 (haskell-indentation-current-indentation))
                     (not (looking-at ">\\s-*$")))
            (forward-char 2)
            (throw 'return nil))
          (when (bobp)
            (forward-char 2)
            (throw 'return nil))
          (forward-line -1)))
    ;; not bird style
    (catch 'return
      (while (not (bobp))
        (forward-comment (- (buffer-size)))
        (beginning-of-line)
        (let ((ps (nth 8 (syntax-ppss))))
          (when ps ;; inside comment or string
            (goto-char ps)))
        (when (= 0 (haskell-indentation-current-indentation))
          (throw 'return nil))))
    (beginning-of-line)
    (when (bobp)
      (forward-comment (buffer-size)))))

(defun haskell-indentation-parse-to-indentations ()
  (save-excursion
    (skip-syntax-forward "-")
    (let ((indentation-point (point))
          (layout-indent 0)
          (parse-line-number 0)
          (current-indent haskell-indentation-layout-offset)
          (starter-indent haskell-indentation-layout-offset)
          (left-indent haskell-indentation-layout-offset)
          (case-fold-search nil)
          current-token
          following-token
          possible-indentations)
      (haskell-indentation-goto-least-indentation)
      (if (<= indentation-point (point))
          (haskell-indentation-first-indentation)
        (setq current-token (haskell-indentation-peek-token))
        (catch 'parse-end
          (haskell-indentation-toplevel)
          (unless (eq current-token 'end-tokens)
            (haskell-indentation-parse-error "Illegal token: %s" current-token)))
        possible-indentations))))

(defun haskell-indentation-first-indentation ()
  (if (haskell-indentation-birdp) '(2) '(0)))

(defun haskell-indentation-find-indentations ()
  (let ((ppss (syntax-ppss)))
    (cond
     ((nth 3 ppss)
      (haskell-indentation-first-indentation))
     ((nth 4 ppss)
      (if (save-excursion
            (and (skip-syntax-forward "-")
                 (eolp)
                 (not (> (forward-line 1) 0))
                 (not (nth 4 (syntax-ppss)))))
          (haskell-indentation-parse-to-indentations)
        (haskell-indentation-first-indentation)))
     (t
      (haskell-indentation-parse-to-indentations)))))

;; XXX: this is a hack, the parser shouldn't return nil without parse-error
(defun haskell-indentation-find-indentations-safe ()
  (or (haskell-indentation-find-indentations)
      (haskell-indentation-first-indentation)))

(defconst haskell-indentation-unicode-tokens
  '(("→" . "->")     ;; #x2192 RIGHTWARDS ARROW
    ("∷" . "::")     ;; #x2237 PROPORTION
    ("←" . "<-")     ;; #x2190 LEFTWARDS ARROW
    ("⇒" . "=>")     ;; #x21D2 RIGHTWARDS DOUBLE ARROW
    ("∀" . "forall") ;; #x2200 FOR ALL
    ("⤙" . "-<")     ;; #x2919 LEFTWARDS ARROW-TAIL
    ("⤚" . ">-")     ;; #x291A RIGHTWARDS ARROW-TAIL
    ("⤛" . "-<<")    ;; #x291B LEFTWARDS DOUBLE ARROW-TAIL
    ("⤜" . ">>-")    ;; #x291C RIGHTWARDS DOUBLE ARROW-TAIL
    ("★" . "*"))     ;; #x2605 BLACK STAR
  "Translation dictionary from UnicodeSyntax tokens to their ASCII representation.")

;; toplevel keywords
(defconst haskell-indentation-toplevel-list
  '(("module" . haskell-indentation-module)
    ("data" . (lambda () (haskell-indentation-statement-right #'haskell-indentation-data)))
    ("type" . (lambda () (haskell-indentation-statement-right #'haskell-indentation-data)))
    ("newtype" . (lambda () (haskell-indentation-statement-right #'haskell-indentation-data)))
    ("class" . haskell-indentation-class-declaration)
    ("instance" . haskell-indentation-class-declaration )))

;; tokens in type declarations
(defconst haskell-indentation-type-list
  '(("::"    . (lambda () (haskell-indentation-with-starter
                           (lambda () (haskell-indentation-separated #'haskell-indentation-type "->")))))
    ("("     . (lambda () (haskell-indentation-list #'haskell-indentation-type ")" ",")))
    ("["     . (lambda () (haskell-indentation-list #'haskell-indentation-type "]" ",")))
    ("{"     . (lambda () (haskell-indentation-list #'haskell-indentation-type "}" ",")))))

;; keywords in expressions
(defconst haskell-indentation-expression-list
  '(("data" . haskell-indentation-data)
    ("type" . haskell-indentation-data)
    ("newtype" . haskell-indentation-data)
    ("if"    . haskell-indentation-if)
    ("let"   . (lambda () (haskell-indentation-phrase
                           '(haskell-indentation-declaration-layout
                             "in" haskell-indentation-expression))))
    ("do"    . (lambda () (haskell-indentation-with-starter
                           #'haskell-indentation-expression-layout)))
    ("mdo"   . (lambda () (haskell-indentation-with-starter
                           #'haskell-indentation-expression-layout)))
    ("rec"   . (lambda () (haskell-indentation-with-starter
                           #'haskell-indentation-expression-layout)))
    ("case"  . (lambda () (haskell-indentation-phrase
                           '(haskell-indentation-expression
                             "of" haskell-indentation-case-layout))))
    ("\\"    . (lambda () (haskell-indentation-with-starter
                           #'haskell-indentation-lambda-maybe-lambdacase)))
    ("proc"  . (lambda () (haskell-indentation-phrase
                           '(haskell-indentation-expression
                             "->" haskell-indentation-expression))))
    ("where" . (lambda () (haskell-indentation-with-starter
                           #'haskell-indentation-declaration-layout nil t)))
    ("::"    . (lambda () (haskell-indentation-with-starter
                           (lambda () (haskell-indentation-separated #'haskell-indentation-type "->")))))
    ("="     . (lambda () (haskell-indentation-statement-right #'haskell-indentation-expression)))
    ("<-"    . (lambda () (haskell-indentation-statement-right #'haskell-indentation-expression)))
    ("("     . (lambda () (haskell-indentation-list #'haskell-indentation-expression ")" '(list "," "->"))))
    ("["     . (lambda () (haskell-indentation-list #'haskell-indentation-expression "]" "," "|")))
    ("{"     . (lambda () (haskell-indentation-list #'haskell-indentation-expression "}" ",")))))

;; a layout list with expressions, such as after do
(defun haskell-indentation-expression-layout ()
  (haskell-indentation-layout #'haskell-indentation-expression))

;; a layout list with declarations, such as after where
(defun haskell-indentation-declaration-layout ()
  (haskell-indentation-layout #'haskell-indentation-declaration))

;; a layout list with case expressions
(defun haskell-indentation-case-layout ()
  (haskell-indentation-layout #'haskell-indentation-case))

;; After a lambda (backslash) there are two possible cases:
;;   - the new lambdacase expression, that can be recognized by the
;;     next token being "case",
;;   - or simply an anonymous function definition in the form of
;;     "expression -> expression".
(defun haskell-indentation-lambda-maybe-lambdacase ()
  (if (string= current-token "case")
      (haskell-indentation-with-starter
       #'haskell-indentation-case-layout)
    (haskell-indentation-phrase-rest
     '(haskell-indentation-expression "->" haskell-indentation-expression))))

;; a functional dependency
(defun haskell-indentation-fundep ()
  (haskell-indentation-with-starter
   (lambda () (haskell-indentation-separated #'haskell-indentation-fundep1 ","))))

(defun haskell-indentation-fundep1 ()
  (let ((current-indent (current-column)))
    (while (member current-token '(value "->"))
      (haskell-indentation-read-next-token))
    (when (and (eq current-token 'end-tokens)
               (member following-token '(value "->")))
      (haskell-indentation-add-indentation current-indent))))

;; the toplevel parser
(defun haskell-indentation-toplevel ()
  (haskell-indentation-layout
   (lambda ()
     (let ((parser (assoc current-token haskell-indentation-toplevel-list)))
       (if parser
           (funcall (cdr parser))
         (haskell-indentation-declaration))))))

;; a type declaration
(defun haskell-indentation-type ()
  (let ((current-indent (current-column)))
    (catch 'return
      (while t
        (cond
         ((member current-token '(value operator "->"))
          (haskell-indentation-read-next-token))

         ((eq current-token 'end-tokens)
          (when (member following-token
                        '(value operator no-following-token
                                "->" "(" "[" "{" "::"))
            (haskell-indentation-add-indentation current-indent))
          (throw 'return nil))
         (t (let ((parser (assoc current-token haskell-indentation-type-list)))
              (if (not parser)
                  (throw 'return nil)
                (funcall (cdr parser))))))))))

;; a data or type declaration
(defun haskell-indentation-data ()
  (haskell-indentation-with-starter
   (lambda ()
     (when (string= current-token "instance")
       (haskell-indentation-read-next-token))
     (haskell-indentation-type)
     (cond ((string= current-token "=")
            (haskell-indentation-with-starter
             (lambda () (haskell-indentation-separated #'haskell-indentation-type "|" "deriving"))))
	   ((string= current-token "where")
            (haskell-indentation-with-starter
             #'haskell-indentation-expression-layout nil))))))

;; a class declaration
(defun haskell-indentation-class-declaration ()
  (haskell-indentation-with-starter
   (lambda ()
     (haskell-indentation-type)
     (when (string= current-token "|")
       (haskell-indentation-fundep))
     (when (string= current-token "where")
       (haskell-indentation-with-starter
        #'haskell-indentation-declaration-layout nil)))))

;; a module declaration
(defun haskell-indentation-module ()
  (haskell-indentation-with-starter
   (lambda ()
     (let ((current-indent (current-column)))
       (haskell-indentation-read-next-token)
       (when (string= current-token "(")
         (haskell-indentation-list
          #'haskell-indentation-module-export
          ")" ","))
       (when (eq current-token 'end-tokens)
         (haskell-indentation-add-indentation current-indent)
         (throw 'parse-end nil))
       (when (string= current-token "where")
         (haskell-indentation-read-next-token)
         (when (eq current-token 'end-tokens)
           (haskell-indentation-add-layout-indent)
           (throw 'parse-end nil))
         (haskell-indentation-layout #'haskell-indentation-toplevel))))))

;; an export list
(defun haskell-indentation-module-export ()
  (cond ((string= current-token "module")
         (let ((current-indent (current-column)))
           (haskell-indentation-read-next-token)
           (cond ((eq current-token 'end-tokens)
                  (haskell-indentation-add-indentation current-indent))
                 ((eq current-token 'value)
                  (haskell-indentation-read-next-token)))))
        (t (haskell-indentation-type))))

;; an list, pair or other expression containing multiple
;; items parsed by parser, separated by sep or stmt-sep, and ending in
;; end.
(defun haskell-indentation-list (parser end sep &optional stmt-sep)
  (haskell-indentation-with-starter
   `(lambda () (haskell-indentation-separated #',parser
                                              ,sep
                                              ,stmt-sep))
   end))

;; An expression starting with a keyword or paren.  Skip the keyword
;; or paren.
(defun haskell-indentation-with-starter (parser &optional end where-expr?)
  (let ((starter-column (current-column))
        (current-indent current-indent)
        (left-indent (if (= (current-column) (haskell-indentation-current-indentation))
                         (current-column) left-indent)))
    (haskell-indentation-read-next-token)
    (when (eq current-token 'end-tokens)
      (cond ((equal following-token end)
	     (haskell-indentation-add-indentation starter-column)) ; indent before keyword or paren
	    (where-expr?
	     (haskell-indentation-add-where-post-indent left-indent)) ;; left indent + where post indent
	    (t
	     (haskell-indentation-add-left-indent)))
      (throw 'parse-end nil))
    (let* ((current-indent (current-column))
           (starter-indent (min starter-column current-indent))
           (left-indent (if end (+ current-indent haskell-indentation-starter-offset)
                          left-indent)))
      (funcall parser)
      (cond ((eq current-token 'end-tokens)
             (when (equal following-token end)
               (haskell-indentation-add-indentation starter-indent))  ; indent before keyword or paren
             (when end (throw 'parse-end nil))) ;; add no more indentations if we expect a closing keyword
            ((equal current-token end)
             (haskell-indentation-read-next-token)) ;; continue
            (end (haskell-indentation-parse-error "Illegal token: %s" current-token))))))

(defun haskell-indentation-case-alternative ()
  (setq left-indent (current-column))
  (haskell-indentation-separated #'haskell-indentation-expression "," nil)
  (cond ((eq current-token 'end-tokens)
         (haskell-indentation-add-indentation current-indent))
        ((string= current-token "->")
         (haskell-indentation-statement-right #'haskell-indentation-expression))
        ;; otherwise fallthrough
        ))

(defun haskell-indentation-case ()
  (haskell-indentation-expression)
  (cond ((eq current-token 'end-tokens)
         (haskell-indentation-add-indentation current-indent))
        ((string= current-token "|")
	 (haskell-indentation-with-starter
	  (lambda ()
	    (haskell-indentation-separated #'haskell-indentation-case-alternative "|" nil))
	  nil))
        ((string= current-token "->")
         (haskell-indentation-statement-right #'haskell-indentation-expression))
        ;; otherwise fallthrough
        ))

;; the right side of a statement.  Sets current-indent
;; to the current column and cals the given parser.
;; if parsing ends here, set indentation to left-indent.
(defun haskell-indentation-statement-right (parser)
  (haskell-indentation-read-next-token)
  (when (eq current-token 'end-tokens)
    (haskell-indentation-add-left-indent)
    (haskell-indentation-add-indentation current-indent)
    (throw 'parse-end nil))
  (let ((current-indent (current-column)))
    (funcall parser)))


(defun haskell-indentation-guard ()
  (setq left-indent (current-column))
  (haskell-indentation-separated
   #'haskell-indentation-expression "," nil))

;; function or type declaration
(defun haskell-indentation-declaration ()
  (haskell-indentation-separated #'haskell-indentation-expression "," nil)
  (cond ((string= current-token "|")
         (haskell-indentation-with-starter
          (lambda () (haskell-indentation-separated
		      #'haskell-indentation-guard "|" nil))
          nil))
        ((eq current-token 'end-tokens)
         (when (member following-token '("|" "=" "::" ","))
           (haskell-indentation-add-indentation current-indent)
           (throw 'parse-end nil)))))

;; enter a layout list, where each layout item is parsed by parser.
(defun haskell-indentation-layout (parser)
  (if (string= current-token "{")
      (haskell-indentation-list parser "}" ";") ;; explicit layout
    (haskell-indentation-implicit-layout-list parser)))

(defun haskell-indentation-expression-token (token)
  (member token '("if" "let" "do" "case" "\\" "(" "{" "[" "::"
                  value operator no-following-token)))

;; parse an expression until an unknown token is encountered.
(defun haskell-indentation-expression ()
  (let ((current-indent (current-column)))
    (catch 'return
      (while t
        (cond
         ((memq current-token '(value operator))
          (haskell-indentation-read-next-token))

         ((eq current-token 'end-tokens)
          (cond ((string= following-token "where")
                 (haskell-indentation-add-where-pre-indent)) ; before a where
                ((haskell-indentation-expression-token following-token)
                 (haskell-indentation-add-indentation
		  current-indent))) ;; a normal expression
          (throw 'return nil))

         (t (let ((parser (assoc current-token haskell-indentation-expression-list)))
              (when (null parser)
                (throw 'return nil)) ; not expression token, so exit
	      (funcall (cdr parser)) ; run parser
              (when (and (eq current-token 'end-tokens)
                         (string= (car parser) "let")
                         (= haskell-indentation-layout-offset current-indent)
                         (haskell-indentation-expression-token following-token))
                ;; inside a layout, after a let construct
		;; for example: do let a = 20
                (haskell-indentation-add-layout-indent)
                (throw 'parse-end nil))

	      ;; after an 'open' expression such as 'if', exit
              (unless (member (car parser) '("(" "[" "{" "do" "case"))
                (throw 'return nil)))))))))

(defun haskell-indentation-test-indentations ()
  (interactive)
  (let ((indentations (save-excursion (haskell-indentation-find-indentations-safe)))
        (str "")
        (pos 0))
    (while indentations
      (when (>= (car indentations) pos)
        (setq str (concat str (make-string (- (car indentations) pos) ?\ )
                          "|"))
        (setq pos (+ 1 (car indentations))))
      (setq indentations (cdr indentations)))
    (end-of-line)
    (newline)
    (insert str)))


;; evaluate parser separated by separator and stmt-separator.
;; if stmt-separator is not nil, it will be used to set a
;; new starter-indent.
;; for example
;; [ i | i <- [1..10]
;;     ,
(defun haskell-indentation-separated (parser separator &optional stmt-separator)
  (catch 'return
    (unless (listp separator)
      (setq separator (list separator)))
    (unless (listp stmt-separator)
      (setq stmt-separator (list stmt-separator)))
    (while t
      (funcall parser)
      (cond ((member current-token separator)
             (haskell-indentation-at-separator))

            ((member current-token stmt-separator)
             (setq starter-indent (current-column))
             (haskell-indentation-at-separator))

            ((eq current-token 'end-tokens)
             (cond ((or (member following-token separator)
                        (member following-token stmt-separator))
		    ;; set an indentation before a separator,
		    ;; for example:
		    ;;  [ 1   or   [ 1 | a
		    ;;  , 2            , 20
                    (haskell-indentation-add-indentation starter-indent)
                    (throw 'parse-end nil)))
             (throw 'return nil))

            (t (throw 'return nil))))))

;; At a separator.
;; If at a new line, set starter-indent at the separator
;; and current-indent after the separator
;; For example:
;; l = [  1
;;      , 2
;;      ,    -- start now here

(defun haskell-indentation-at-separator ()
  (let ((separator-column
         (and (= (current-column) (haskell-indentation-current-indentation))
              (current-column))))
    (haskell-indentation-read-next-token)
    (cond ((eq current-token 'end-tokens)
           (haskell-indentation-add-indentation current-indent)
           (throw 'return nil))
          (separator-column ;; on the beginning of the line
           (setq current-indent (current-column))
           (setq starter-indent separator-column)))))

;; An implicit layout list.  This sets the layout-indent
;; variable to the column where the layout starts.
(defun haskell-indentation-implicit-layout-list (parser)
  (let* ((layout-indent (current-column))
         (current-indent (current-column))
         (left-indent (current-column)))
    (catch 'return
      (while t
        (let ((left-indent left-indent))
          (funcall parser))
        (cond ((member current-token '(layout-item ";"))
               (haskell-indentation-read-next-token))
              ((eq current-token 'end-tokens)
               (when (or (haskell-indentation-expression-token following-token)
                         (string= following-token ";"))
                 (haskell-indentation-add-layout-indent))
               (throw 'return nil))
              (t (throw 'return nil))))))
  ;; put haskell-indentation-read-next-token outside the current-indent definition
  ;; so it will not return 'layout-end again
  (when (eq current-token 'layout-end)
    (haskell-indentation-read-next-token))) ;; leave layout at 'layout-end or illegal token

(defun haskell-indentation-if ()
  (haskell-indentation-with-starter
   (lambda ()
     (if (string= current-token "|")
	 (haskell-indentation-with-starter
	  (lambda ()
	    (haskell-indentation-separated
	     #'haskell-indentation-case-alternative "|" nil))
	  nil)
       (haskell-indentation-phrase-rest
	'(haskell-indentation-expression
	  "then" haskell-indentation-expression
	  "else" haskell-indentation-expression))))
   nil))

(defun haskell-indentation-phrase (phrase)
  (haskell-indentation-with-starter
   `(lambda () (haskell-indentation-phrase-rest ',phrase))
   nil))

(defun haskell-indentation-phrase-rest (phrase)
  (let ((starter-line parse-line-number))
    (let ((current-indent (current-column)))
      (funcall (car phrase)))
    (cond
     ((eq current-token 'end-tokens)
      (cond ((null (cdr phrase))) ;; fallthrough
            ((equal following-token (cadr phrase))
             (haskell-indentation-add-indentation starter-indent)
             (throw 'parse-end nil))
            ((string= (cadr phrase) "in")
             (when (= left-indent layout-indent)
               (haskell-indentation-add-layout-indent)
               (throw 'parse-end nil)))
            (t (throw 'parse-end nil))))

     ((null (cdr phrase)))

     ((equal (cadr phrase) current-token)
      (let* ((on-new-line (= (current-column) (haskell-indentation-current-indentation)))
             (lines-between (- parse-line-number starter-line))
             (left-indent (if (<= lines-between 0)
                              left-indent
                            starter-indent)))
        (haskell-indentation-read-next-token)
	(when (eq current-token 'end-tokens)
          (cond ((member (cadr phrase) '("then" "else"))
		 (haskell-indentation-add-indentation
		  (+ starter-indent haskell-indentation-ifte-offset)))

		((member (cadr phrase) '("in" "->"))
		 ;; expression ending in another expression
		 (when (or (not haskell-indentation-indent-leftmost)
			   (eq haskell-indentation-indent-leftmost 'both))
		   (haskell-indentation-add-indentation
		    (+ starter-indent haskell-indentation-starter-offset)))
		 (when haskell-indentation-indent-leftmost
		   (haskell-indentation-add-indentation
		    (if on-new-line
			(+ left-indent haskell-indentation-starter-offset)
		      left-indent))))

		 (t
		  (when (or (not haskell-indentation-indent-leftmost)
			    (eq haskell-indentation-indent-leftmost 'both))
		    (haskell-indentation-add-indentation
		     (+ starter-indent haskell-indentation-starter-offset)))
		  (when haskell-indentation-indent-leftmost
		   (haskell-indentation-add-indentation
		    (if on-new-line
			(+ left-indent haskell-indentation-starter-offset)
		      left-indent)))))
          (throw 'parse-end nil))
        (haskell-indentation-phrase-rest (cddr phrase))))

     ((string= (cadr phrase) "in")) ;; fallthrough
     (t (haskell-indentation-parse-error "Expecting %s" (cadr phrase))))))

(defun haskell-indentation-add-indentation (indent)
  (haskell-indentation-push-indentation
   (if (<= indent layout-indent)
       (+ layout-indent haskell-indentation-layout-offset)
     indent)))

(defun haskell-indentation-add-layout-indent ()
  (haskell-indentation-push-indentation layout-indent))

(defun haskell-indentation-add-where-pre-indent ()
  (haskell-indentation-push-indentation
   (+ layout-indent haskell-indentation-where-pre-offset))
  (if (= layout-indent haskell-indentation-layout-offset)
      (haskell-indentation-push-indentation
       haskell-indentation-where-pre-offset)))

(defun haskell-indentation-add-where-post-indent (indent)
  (haskell-indentation-push-indentation
   (+ indent haskell-indentation-where-post-offset)))

(defun haskell-indentation-add-left-indent ()
  (haskell-indentation-add-indentation
   (+ left-indent haskell-indentation-left-offset)))

(defun haskell-indentation-push-indentation (indent)
  (when (or (null possible-indentations)
            (< indent (car possible-indentations)))
    (setq possible-indentations
          (cons indent possible-indentations))))

(defun haskell-indentation-token-test ()
  (let ((current-token nil)
        (following-token nil)
        (layout-indent 0)
        (parse-line-number 0)
        (indentation-point (mark)))
    (haskell-indentation-read-next-token)))

;; Go to the next token and set current-token to the next token.
;; The following symbols are used as pseudo tokens:
;;
;; 'layout-item: A new item in a layout list.  The next token
;;               will be the first token from the item.
;; 'layout-end:  the end of a layout list.  Next token will be
;;               the first token after the layout list.
;; 'end-tokens:  back at point where we started, following-token
;;               will be set to the next token.
;;
;; if we are at a new line, parse-line is increased, and
;; current-indent and left-indent are set to the indentation
;; of the line.

(defun haskell-indentation-read-next-token ()
  (cond ((eq current-token 'end-tokens)
         'end-tokens)
        ((eq current-token 'layout-end)
         (cond ((> layout-indent (current-column))
                'layout-end)
               ((= layout-indent (current-column))
                (setq current-token 'layout-item))
               ((< layout-indent (current-column))
                (setq current-token (haskell-indentation-peek-token)))))
        ((eq current-token 'layout-item)
         (setq current-token (haskell-indentation-peek-token)))
        ((> layout-indent (current-column))
         (setq current-token 'layout-end))
        (t
         (haskell-indentation-skip-token)
         (if (>= (point) indentation-point)
             (progn
               (setq following-token
                     (if (= (point) indentation-point)
                         (haskell-indentation-peek-token)
                       'no-following-token))
               (setq current-token 'end-tokens))
           (when (= (current-column) (haskell-indentation-current-indentation))
             ;; on a new line
             (setq current-indent (current-column))
             (setq left-indent (current-column))
             (setq parse-line-number (+ parse-line-number 1)))
           (cond ((> layout-indent (current-column))
                  (setq current-token 'layout-end))
                 ((= layout-indent (current-column))
                  (setq current-token 'layout-item))
                 (t (setq current-token (haskell-indentation-peek-token))))))))

(defun haskell-indentation-peek-token ()
  "Return token starting at point."
  (cond ((looking-at "\\(if\\|then\\|else\\|let\\|in\\|mdo\\|rec\\|do\\|proc\\|case\\|of\\|where\\|module\\|deriving\\|data\\|type\\|newtype\\|class\\|instance\\)\\([^[:alnum:]'_]\\|$\\)")
         (match-string-no-properties 1))
        ((looking-at "[][(){}[,;]")
         (match-string-no-properties 0))
        ((looking-at "\\(\\\\\\|->\\|→\\|<-\\|←\\|::\\|∷\\|=\\||\\)\\([^-:!#$%&*+./<=>?@\\\\^|~]\\|$\\)")
         (match-string-no-properties 1))
        ((looking-at "\\(→\\|←\\|∷\\)\\([^-:!#$%&*+./<=>?@\\\\^|~]\\|$\\)")
         (let ((tok (match-string-no-properties 1)))
           (or (cdr (assoc tok haskell-indentation-unicode-tokens)) tok)))
        ((looking-at"[-:!#$%&*+./<=>?@\\\\^|~`]" )
         'operator)
        (t 'value)))

(defun haskell-indentation-skip-token ()
  "Skip to the next token."
  (let ((case-fold-search nil))

    (if (or (looking-at "'\\([^\\']\\|\\\\.\\)*'")
            (looking-at "\"\\([^\\\"]\\|\\\\.\\)*\"")
            (looking-at         ; Hierarchical names always start with uppercase
             "[[:upper:]]\\(\\s_\\|\\sw\\|'\\)*\\(\\.\\(\\s_\\|\\sw\\|'\\)+\\)*")
            (looking-at "\\(\\s_\\|\\sw\\)\\(\\s_\\|\\sw\\|'\\)*") ; Only unqualified vars can start with lowercase
            (looking-at "[0-9][0-9oOxXeE+-]*")
            (looking-at "[-:!#$%&*+./<=>?@\\\\^|~]+")
            (looking-at "[](){}[,;]")
            (looking-at "`[[:alnum:]']*`"))
        (goto-char (match-end 0))
      ;; otherwise skip until space found
      (skip-syntax-forward "^-"))
    (forward-comment (buffer-size))
    (while (and (haskell-indentation-birdp)
                (bolp)
                (eq (char-after) ?>))
      (forward-char)
      (forward-comment (buffer-size)))))

(provide 'haskell-indentation)

;; Local Variables:
;; tab-width: 8
;; End:

;;; haskell-indentation.el ends here
