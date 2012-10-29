;;; expand-region-core.el --- Increase selected region by semantic units.

;; Copyright (C) 2011 Magnar Sveen

;; Author: Magnar Sveen <magnars@gmail.com>
;; Keywords: marking region

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

;; The core functionality of expand-region.

;; All changes to this file must be accompanied by feature tests.
;; They are written in [Ecukes](http://ecukes.info), a Cucumber for Emacs.
;;
;; To fetch the test dependencies:
;;
;;     $ cd /path/to/expand-region
;;     $ git submodule init
;;     $ git submodule update
;;
;; Run the tests with:
;;
;;     $ ./util/ecukes/ecukes features

;;; Code:

(eval-when-compile (require 'cl))

(defvar er/history '()
  "A history of start and end points so we can contract after expanding.")

;; history is always local to a single buffer
(make-variable-buffer-local 'er/history)

(defvar er--space-str " \t\n")
(defvar er--blank-list (append er--space-str nil))

(set-default 'er--show-expansion-message nil)

;; Default expansions

(defun er/mark-word ()
  "Mark the entire word around or in front of point."
  (interactive)
  (let ((word-regexp "\\sw"))
    (when (or (looking-at word-regexp)
              (looking-back word-regexp))
      (skip-syntax-forward "w")
      (set-mark (point))
      (while (looking-back word-regexp)
        (backward-char)))))

(defun er/mark-symbol ()
  "Mark the entire symbol around or in front of point."
  (interactive)
  (let ((symbol-regexp "\\s_\\|\\sw"))
    (when (or (looking-at symbol-regexp)
              (looking-back symbol-regexp))
      (skip-syntax-forward "_w")
      (set-mark (point))
      (while (looking-back symbol-regexp)
        (backward-char)))))

(defun er/mark-symbol-with-prefix ()
  "Mark the entire symbol around or in front of point, including prefix."
  (interactive)
  (let ((symbol-regexp "\\s_\\|\\sw")
        (prefix-regexp "\\s'"))
    (when (or (looking-at prefix-regexp)
              (looking-at symbol-regexp)
              (looking-back symbol-regexp))
      (skip-syntax-forward "'")
      (skip-syntax-forward "_w")
      (set-mark (point))
      (while (or (looking-back symbol-regexp)
                 (looking-back prefix-regexp))
        (backward-char)))))

;; Mark method call

(defun er/mark-next-accessor ()
  "Presumes that current symbol is already marked, skips over one
period and marks next symbol."
  (interactive)
  (when (use-region-p)
    (when (< (point) (mark))
      (exchange-point-and-mark))
    (let ((symbol-regexp "\\s_\\|\\sw"))
      (when (looking-at "\\.")
        (forward-char 1)
        (skip-syntax-forward "_w")
        (exchange-point-and-mark)))))

(defun er/mark-method-call ()
  "Mark the current symbol (including dots) and then paren to closing paren."
  (interactive)
  (let ((symbol-regexp "\\s_\\|\\sw\\|\\."))
    (when (or (looking-at symbol-regexp)
              (looking-back symbol-regexp))
      (skip-syntax-backward "_w.")
      (set-mark (point))
      (while (looking-at symbol-regexp)
        (forward-char))
      (if (looking-at "(")
          (forward-list))
      (exchange-point-and-mark))))

;; Comments

(defun er--point-is-in-comment-p ()
  "t if point is in comment, otherwise nil"
  (or (nth 4 (syntax-ppss))
      (memq (get-text-property (point) 'face) '(font-lock-comment-face font-lock-comment-delimiter-face))))

(defun er/mark-comment ()
  "Mark the entire comment around point."
  (interactive)
  (when (er--point-is-in-comment-p)
    (let ((p (point)))
      (while (er--point-is-in-comment-p)
        (forward-char 1))
      (skip-chars-backward " \n\t\r")
      (set-mark (point))
      (goto-char p)
      (while (er--point-is-in-comment-p)
        (forward-char -1))
      (skip-chars-forward " \n\t\r"))))

;; Quotes

(defun er--current-quotes-char ()
  "The char that is the current quote delimiter"
  (nth 3 (syntax-ppss)))

(defalias 'er--point-inside-string-p 'er--current-quotes-char)

(defun er--move-point-forward-out-of-string ()
  "Move point forward until it exits the current quoted string."
  (while (er--point-inside-string-p) (forward-char)))

(defun er--move-point-backward-out-of-string ()
  "Move point backward until it exits the current quoted string."
  (while (er--point-inside-string-p) (backward-char)))

(defun er/mark-inside-quotes ()
  "Mark the inside of the current string, not including the quotation marks."
  (interactive)
  (when (er--point-inside-string-p)
    (er--move-point-backward-out-of-string)
    (forward-char)
    (set-mark (point))
    (er--move-point-forward-out-of-string)
    (backward-char)
    (exchange-point-and-mark)))

(defun er/mark-outside-quotes ()
  "Mark the current string, including the quotation marks."
  (interactive)
  (if (er--point-inside-string-p)
      (er--move-point-backward-out-of-string)
    (when (and (not (use-region-p))
               (looking-back "\\s\""))
      (backward-char)
      (er--move-point-backward-out-of-string)))
  (when (looking-at "\\s\"")
    (set-mark (point))
    (forward-char)
    (er--move-point-forward-out-of-string)
    (exchange-point-and-mark)))

;; Pairs - ie [] () {} etc

(defun er--point-inside-pairs-p ()
  "Is point inside any pairs?"
  (> (car (syntax-ppss)) 0))

(defun er/mark-inside-pairs ()
  "Mark inside pairs (as defined by the mode), not including the pairs."
  (interactive)
  (when (er--point-inside-pairs-p)
    (goto-char (nth 1 (syntax-ppss)))
    (set-mark (save-excursion
                (forward-char 1)
                (skip-chars-forward er--space-str)
                (point)))
    (forward-list)
    (backward-char)
    (skip-chars-backward er--space-str)
    (exchange-point-and-mark)))

(defun er--looking-at-pair ()
  "Is point looking at an opening pair char?"
  (looking-at "\\s("))

(defun er--looking-at-marked-pair ()
  "Is point looking at a pair that is entirely marked?"
  (and (er--looking-at-pair)
       (use-region-p)
       (>= (mark)
           (save-excursion
             (forward-list)
             (point)))))

(defun er/mark-outside-pairs ()
  "Mark pairs (as defined by the mode), including the pair chars."
  (interactive)
  (if (looking-back "\\s)+\\=")
      (ignore-errors (backward-list 1))
    (skip-chars-forward er--space-str))
  (when (and (er--point-inside-pairs-p)
             (or (not (er--looking-at-pair))
                 (er--looking-at-marked-pair)))
    (goto-char (nth 1 (syntax-ppss))))
  (when (er--looking-at-pair)
    (set-mark (point))
    (forward-list)
    (exchange-point-and-mark)))

;; Methods to try expanding to

(setq er/try-expand-list '(er/mark-word
                           er/mark-symbol
                           er/mark-symbol-with-prefix
                           er/mark-next-accessor
                           er/mark-method-call
                           er/mark-inside-quotes
                           er/mark-outside-quotes
                           er/mark-inside-pairs
                           er/mark-outside-pairs
                           er/mark-comment))

(defun er--prepare-expanding ()
  (when (and (er--first-invocation)
             (not (use-region-p)))
    (push-mark nil t)  ;; one for keeping starting position
    (push-mark nil t)) ;; one for replace by set-mark in expansions

  (when (not (eq t transient-mark-mode))
    (setq transient-mark-mode (cons 'only transient-mark-mode))))

(defun er--copy-region-to-register ()
  (when (and (stringp expand-region-autocopy-register)
             (> (length expand-region-autocopy-register) 0))
    (set-register (aref expand-region-autocopy-register 0)
                  (filter-buffer-substring (region-beginning) (region-end)))))

(defun er--expand-region-1 ()
  "Increase selected region by semantic units.
Basically it runs all the mark-functions in `er/try-expand-list'
and chooses the one that increases the size of the region while
moving point or mark as little as possible."
  (let* ((p1 (point))
         (p2 (if (use-region-p) (mark) (point)))
         (start (min p1 p2))
         (end (max p1 p2))
         (try-list er/try-expand-list)
         (best-start (point-min))
         (best-end (point-max))
         (set-mark-default-inactive nil))

    ;; add hook to clear history on buffer changes
    (unless er/history
      (add-hook 'after-change-functions 'er/clear-history t t))

    ;; remember the start and end points so we can contract later
    ;; unless we're already at maximum size
    (unless (and (= start best-start)
                 (= end best-end))
      (push (cons start end) er/history))

    (when (and expand-region-skip-whitespace
               (er--point-is-surrounded-by-white-space)
               (= start end))
      (skip-chars-forward er--space-str)
      (setq start (point)))

    (while try-list
      (save-excursion
        (ignore-errors
          (funcall (car try-list))
          (when (and (region-active-p)
                     (er--this-expansion-is-better))
            (setq best-start (point))
            (setq best-end (mark))
            (when (and er--show-expansion-message (not (minibufferp)))
              (message "%S" (car try-list))))))
      (setq try-list (cdr try-list)))

    (goto-char best-start)
    (set-mark best-end)

    (er--copy-region-to-register)

    (when (and (= best-start (point-min))
               (= best-end (point-max))) ;; We didn't find anything new, so exit early
      (setq arg 0))))

(defun er--this-expansion-is-better ()
  "t if the current region is an improvement on previous expansions.

This is provided as a separate function for those that would like
to override the heuristic."
  (and
   (<= (point) start)
   (>= (mark) end)
   (> (- (mark) (point)) (- end start))
   (or (> (point) best-start)
       (and (= (point) best-start)
            (< (mark) best-end)))))

(defun er/contract-region (arg)
  "Contract the selected region to its previous size.
With prefix argument contracts that many times.
If prefix argument is negative calls `er/expand-region'.
If prefix argument is 0 it resets point and mark to their state
before calling `er/expand-region' for the first time."
  (interactive "p")
  (if (< arg 0)
      (er/expand-region (- arg))
    (when er/history
      ;; Be sure to reset them all if called with 0
      (when (= arg 0)
        (setq arg (length er/history)))

      (when (not transient-mark-mode)
        (setq transient-mark-mode (cons 'only transient-mark-mode)))

      ;; Advance through the list the desired distance
      (while (and (cdr er/history)
                  (> arg 1))
        (setq arg (- arg 1))
        (setq er/history (cdr er/history)))
      ;; Reset point and mark
      (let* ((last (pop er/history))
             (start (car last))
             (end (cdr last)))
        (goto-char start)
        (set-mark end)

        (er--copy-region-to-register)

        (when (eq start end)
          (deactivate-mark)
          (er/clear-history))))))

(defun er/prepare-for-more-expansions-internal (repeat-key-str)
  "Return bindings and a message to inform user about them"
  (let ((msg (format "Type %s to expand again" repeat-key-str))
        (bindings (list (cons repeat-key-str '(er/expand-region 1)))))
    ;; If contract and expand are on the same binding, ignore contract
    (unless (string-equal repeat-key-str expand-region-contract-fast-key)
      (setq msg (concat msg (format ", %s to contract" expand-region-contract-fast-key)))
      (push (cons expand-region-contract-fast-key '(er/contract-region 1)) bindings))
    ;; If reset and either expand or contract are on the same binding, ignore reset
    (unless (or (string-equal repeat-key-str expand-region-reset-fast-key)
                (string-equal expand-region-contract-fast-key expand-region-reset-fast-key))
      (setq msg (concat msg (format ", %s to reset" expand-region-reset-fast-key)))
      (push (cons expand-region-reset-fast-key '(er/expand-region 0)) bindings))
    (cons msg bindings)))

(defun er/prepare-for-more-expansions ()
  "Let one expand more by just pressing the last key."
  (let* ((repeat-key (event-basic-type last-input-event))
         (repeat-key-str (single-key-description repeat-key))
         (msg-and-bindings (er/prepare-for-more-expansions-internal repeat-key-str))
         (msg (car msg-and-bindings))
         (bindings (cdr msg-and-bindings)))
    (when repeat-key
      (set-temporary-overlay-map
       (let ((map (make-sparse-keymap)))
         (dolist (binding bindings map)
           (define-key map (read-kbd-macro (car binding))
             `(lambda ()
                (interactive)
                (setq this-command `,(cadr ',binding))
                (or (minibufferp) (message "%s" ,msg))
                (eval `,(cdr ',binding))))))
       t)
      (or (minibufferp) (message "%s" msg)))))

(when (not (fboundp 'set-temporary-overlay-map))
  ;; Backport this function from newer emacs versions
  (defun set-temporary-overlay-map (map &optional keep-pred)
    "Set a new keymap that will only exist for a short period of time.
The new keymap to use must be given in the MAP variable. When to
remove the keymap depends on user input and KEEP-PRED:

- if KEEP-PRED is nil (the default), the keymap disappears as
  soon as any key is pressed, whether or not the key is in MAP;

- if KEEP-PRED is t, the keymap disappears as soon as a key *not*
  in MAP is pressed;

- otherwise, KEEP-PRED must be a 0-arguments predicate that will
  decide if the keymap should be removed (if predicate returns
  nil) or kept (otherwise). The predicate will be called after
  each key sequence."

    (let* ((clearfunsym (make-symbol "clear-temporary-overlay-map"))
           (overlaysym (make-symbol "t"))
           (alist (list (cons overlaysym map)))
           (clearfun
            `(lambda ()
               (unless ,(cond ((null keep-pred) nil)
                              ((eq t keep-pred)
                               `(eq this-command
                                    (lookup-key ',map
                                                (this-command-keys-vector))))
                              (t `(funcall ',keep-pred)))
                 (remove-hook 'pre-command-hook ',clearfunsym)
                 (setq emulation-mode-map-alists
                       (delq ',alist emulation-mode-map-alists))))))
      (set overlaysym overlaysym)
      (fset clearfunsym clearfun)
      (add-hook 'pre-command-hook clearfunsym)

      (push alist emulation-mode-map-alists))))

(defadvice keyboard-quit (before collapse-region activate)
  (when (memq last-command '(er/expand-region er/contract-region))
    (er/contract-region 0)))

(defadvice minibuffer-keyboard-quit (around collapse-region activate)
  (if (memq last-command '(er/expand-region er/contract-region))
      (er/contract-region 0)
    ad-do-it))

(defadvice cua-cancel (before collapse-region activate)
  (when (memq last-command '(er/expand-region er/contract-region))
    (er/contract-region 0)))

(defun er/clear-history (&rest args)
  "Clear the history."
  (setq er/history '())
  (remove-hook 'after-change-functions 'er/clear-history t))

(defsubst er--first-invocation ()
  "t if this is the first invocation of er/expand-region or er/contract-region"
  (not (memq last-command '(er/expand-region er/contract-region))))

(defun er--point-is-surrounded-by-white-space ()
  (and (or (memq (char-before) er--blank-list)
           (eq (point) (point-min)))
       (memq (char-after) er--blank-list)))

(defun er/enable-mode-expansions (mode add-fn)
  (add-hook (intern (format "%s-hook" mode)) add-fn)
  (save-window-excursion
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p mode)
          (funcall add-fn))))))

(provide 'expand-region-core)

;;; expand-region-core.el ends here
