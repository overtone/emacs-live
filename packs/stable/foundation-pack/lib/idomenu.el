(require 'ido)
(require 'imenu)

(defun idomenu--guess-default (index-alist symbol)
  "Guess a default choice from the given symbol."
  (catch 'found
    (let ((regex (concat "\\_<" (regexp-quote symbol) "\\_>")))
      (dolist (item index-alist)
        (if (string-match regex (car item)) (throw 'found (car item)))))))

(defun idomenu--read (index-alist &optional prompt guess)
  "Read a choice from an Imenu alist via Ido."
  (let* ((symatpt (thing-at-point 'symbol))
         (default (and guess symatpt (idomenu--guess-default index-alist symatpt)))
         (names (mapcar 'car index-alist))
         (name (ido-completing-read (or prompt "imenu ") names
                                    nil t nil nil default))
         (choice (assoc name index-alist)))
    (if (imenu--subalist-p choice)
        (idomenu--read (cdr choice) prompt nil)
      choice)))

(defun idomenu--trim (str)
  "Trim leading and tailing whitespace from STR."
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" s)))

(defun idomenu--trim-alist (index-alist)
  "There must be a better way to apply a function to all cars of an alist"
  (mapcar (lambda (pair) (cons (idomenu--trim (car pair)) (cdr pair)))
          index-alist))

;;;###autoload
(defun idomenu ()
  "Switch to a buffer-local tag from Imenu via Ido."
  (interactive)
  ;; ido initialization
  (ido-init-completion-maps)
  (add-hook 'minibuffer-setup-hook 'ido-minibuffer-setup)
  (add-hook 'choose-completion-string-functions 'ido-choose-completion-string)
  (add-hook 'kill-emacs-hook 'ido-kill-emacs-hook)
  ;; set up ido completion list
  (let ((index-alist (cdr (imenu--make-index-alist))))
    (if (equal index-alist '(nil))
        (message "No imenu tags in buffer")
      (imenu (idomenu--read (idomenu--trim-alist index-alist) nil t)))))

(provide 'idomenu)
;;; idomenu.el ends here
