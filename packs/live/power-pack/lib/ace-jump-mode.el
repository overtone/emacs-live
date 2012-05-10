;;; ace-jump-mode.el --- a quick cursor location minor mode for emacs

;; Copyright (C) 2011 Free Software Foundation, Inc.

;; Author   : winterTTr <winterTTr@gmail.com>
;; URL      : https://github.com/winterTTr/ace-jump-mode/
;; Version  : 1.0
;; Keywords : motion, location, cursor

;; This file is part of GNU Emacs.

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

;;; INTRODUCTION
;;

;; What's this?
;;
;; It is a minor mode for Emacs, enabling fast/direct cursor-moving in
;; current view.

;; Where does minor mode come from ?
;;
;; I firstly see such kind of moving style is in a vim plugin called
;; EasyMotion. It really attract me a lot. EasyMotion provides a much
;; simpler way to use some motions in vim. It takes the out of w or
;; f{char} by highlighting all possible choices and allowing you to
;; press one key to jump directly to the target. So I decide to write
;; one for Emacs.
;;
;; So I must thank to :
;;         Bartlomiej P. for his PreciseJump
;;         Kim Silkebækken for his EasyMotion


;; What's ace-jump-mode ?
;;
;; ace-jump-mode is an fast/direct cursor location minor mode. It will
;; create the N-Branch search tree internal and marks all the possible
;; position with predefined keys in current view. Allowing you to move
;; to the character/word/line almost directly.
;;

;; What is implemented now ?
;;
;; I do not implement everything from EasyMotion. Because I what to
;; make AceJump as simple as possible, and you don’t even need to
;; spend more than 2 minutes to learn how to use it. So now, there is
;; only three sub-mode, which can help you to quick move to a specific
;; character , word and (non-empty) line. Enjoy it~
;;
;; Of course, if you have any cool suggestion, feel free to tell me at
;; anytime. I will put that to top of my TODO list :D
;;

;;; Usage
;;
;; Add the following code to your init file, of course you can select
;; the key which you prefer to.
;; ----------------------------------------------------------
;; (add-to-list 'load-path "which-folder-ace-jump-mode-file-in/")
;; (require 'ace-jump-mode)
;; (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
;;
;; ;;If you also use viper mode :
;; (define-key viper-vi-global-user-map (kbd "SPC") 'ace-jump-mode)
;; ----------------------------------------------------------
;;

;; Code goes here

(eval-when-compile
  (require 'cl))


;;; register as a minor mode
(or (assq 'ace-jump-mode minor-mode-alist)
    (nconc minor-mode-alist
          (list '(ace-jump-mode ace-jump-mode))))

;; custoize variable
(defvar ace-jump-word-mode-use-query-char t
  "If we need to ask for the query char before enter `ace-jump-word-mode'")

(defvar ace-jump-mode-case-sensitive-search t
  "If non-nil, the ace-jump mode will use case-sensitive search
Otherwise, ace-jump mode will use case-insensitive search.")

(defvar ace-jump-mode-submode-list
  '(ace-jump-word-mode
    ace-jump-char-mode
    ace-jump-line-mode)
  "*The mode list when start ace jump mode.
The sequence is the calling sequence when give prefix argument.

Such as:
  If you use the default sequence, which is
      '(ace-jump-word-mode
        ace-jump-char-mode
        ace-jump-line-mode)
and using key to start up ace jump mode, such as 'C-c SPC',
then the usage to start each mode is as below:

   C-c SPC           ==> ace-jump-word-mode
   C-u C-c SPC       ==> ace-jump-char-mode
   C-u C-u C-c SPC   ==> ace-jump-line-mode

Currently, the valid submode is:
   `ace-jump-word-mode'
   `ace-jump-char-mode'
   `ace-jump-line-mode'

")

(defvar ace-jump-mode-move-keys
  (nconc (loop for i from ?a to ?z collect i)
         (loop for i from ?A to ?Z collect i))
  "*The keys that used to move when enter AceJump mode.
Each key should only an printable character, whose name will
fill each possible location.

If you want your own moving keys, you can custom that as follow,
for example, you only want to use lower case character:
(setq ace-jump-mode-move-keys (loop for i from ?a to ?z collect i)) ")


;;; some buffer specific variable
(defvar ace-jump-mode nil
  "AceJump minor mode.")
(defvar ace-jump-background-overlay nil
  "Background overlay which will grey all the display")
(defvar ace-jump-search-tree nil
  "N-branch Search tree. Every leaf node holds the overlay that
is used to highlight the target positions.")
(defvar ace-jump-query-char nil
  "This is local to buffer, save the query char used between internal
mode change via \"M-n\" or \"M-p\"")
(defvar ace-jump-current-mode nil
  "Save the current mode")

(make-variable-buffer-local 'ace-jump-mode)
(make-variable-buffer-local 'ace-jump-background-overlay)
(make-variable-buffer-local 'ace-jump-search-tree)
(make-variable-buffer-local 'ace-jump-query-char)
(make-variable-buffer-local 'ace-jump-current-mode)


(defgroup ace-jump nil
  "ace jump group"
  :group 'convenience)

;;; define the face
(defface ace-jump-face-background
  '((t (:foreground "gray40")))
  "Face for background of AceJump motion"
  :group 'ace-jump)


(defface ace-jump-face-foreground
  '((((class color)) (:foreground "red"))
    (((background dark)) (:foreground "gray100"))
    (((background light)) (:foreground "gray0"))
     (t (:foreground "gray100")))
  "Face for foreground of AceJump motion"
  :group 'ace-jump)


(defvar ace-jump-mode-hook nil
  "Funciton(s) to call after start AceJump mode")

(defvar ace-jump-mode-end-hook nil
  "Funciton(s) to call after stop AceJump mode")

(defvar ace-jump-mode-before-jump-hook nil
  "Function(s) to call just before moving the cursor to a selected match")

(defun ace-jump-query-char-p ( query-char )
  "Check if the query char is valid,
we can only allow to query printable ascii char"
  (and (> query-char #x1F) (< query-char #x7F)) )

(defun ace-jump-search-candidate( re-query-string &optional start-point end-point )
  "Search the RE-QUERY-STRING in current view, and return the candidate position list.
RE-QUERY-STRING should be an valid regex used for `search-forward-regexp'.

You can also specify the START-POINT , END-POINT.
If you omit them, it will use the full screen in current window.

You can control whether use the case sensitive or not by `ace-jump-mode-case-sensitive-search'.

Every possible `match-beginning' will be collected and return as a list."
  (let* ((current-window (selected-window))
         (start-point (or start-point (window-start current-window)))
         (end-point   (or end-point   (window-end   current-window))))
    (save-excursion
      (goto-char start-point)
      (let ((case-fold-search (not ace-jump-mode-case-sensitive-search)))
        (loop while (search-forward-regexp re-query-string end-point t)
                    collect (match-beginning 0))))))

(defun ace-jump-tree-breadth-first-construct (total-leaf-node max-child-node)
  "Constrct the search tree, each item in the tree is a cons cell.
The (car tree-node) is the type, which should be only 'branch or 'leaf.
The (cdr tree-node) is data stored in a leaf when type is 'leaf,
while a child node list when type is 'branch"
  (let ((left-leaf-node (- total-leaf-node 1))
        (q (make-aj-queue))
        (node nil)
        (root (cons 'leaf nil)) )
    ;; we push the node into queue and make candidate-sum -1, so
    ;; create the start condition for the while loop
    (aj-queue-push root q)
    (while (> left-leaf-node 0)
      (setq node (aj-queue-pop q))
      ;; when a node is picked up from stack, it will be changed to a
      ;; branch node, we lose a leaft node
      (setf (car node) 'branch)
      ;; so we need to add the sum of leaf nodes that we wish to create
      (setq left-leaf-node (1+ left-leaf-node))
      (if (<= left-leaf-node max-child-node)
          ;; current child can fill the left leaf
          (progn
            (setf (cdr node)
                  (loop for i from 1 to left-leaf-node
                        collect (cons 'leaf nil)))
            ;; so this should be the last action for while
            (setq left-leaf-node 0))
        ;; the child can not cover the left leaf
        (progn
          ;; fill as much as possible. Push them to queue, so it have
          ;; the oppotunity to become 'branch node if necessary
          (setf (cdr node)
                (loop for i from 1 to max-child-node
                      collect (let ((n (cons 'leaf nil)))
                                (aj-queue-push n q)
                                n)))
          (setq left-leaf-node (- left-leaf-node max-child-node)))))
    ;; return the root node
    root))

(defun ace-jump-tree-preorder-traverse (tree &optional leaf-func branch-func)
  "we move over tree via preorder, and call BRANCH-FUNC on each branch
node and call LEAF-FUNC on each leaf node"
  ;; use stack to do preorder traverse
  (let ((s (list tree)))
    (while (not (null s))
      ;; pick up one from stack
      (let ((node (car s)))
        ;; update stack
        (setq s (cdr s))
        (cond
         ((eq (car node) 'branch)
            ;; a branch node
          (if branch-func
              (funcall branch-func node))
          ;; push all child node into stack
          (setq s (append (cdr node) s)))
         ((eq (car node) 'leaf)
          (if leaf-func
              (funcall leaf-func node)))
         (t
          (error "[AceJump] Internal Error: invalid tree node type")))))))


(defun ace-jump-populate-overlay-to-search-tree (tree candidate-list)
  "Populate the overlay to search tree, every leaf will give one overlay"
  (let* ((position-list candidate-list)
         (func-create-overlay (lambda (node)
                                (let* ((pos (car position-list))
                                       (ol (make-overlay pos (1+ pos) (current-buffer))))
                                  (setf (cdr node) ol)
                                  (overlay-put ol 'face 'ace-jump-face-foreground)
                                  (setq position-list (cdr position-list))))))
    (ace-jump-tree-preorder-traverse tree func-create-overlay)
    tree))


(defun ace-jump-delete-overlay-in-search-tree (tree)
  "Delete all the overlay in search tree leaf node"
  (let ((func-delete-overlay (lambda (node)
                               (delete-overlay (cdr node))
                               (setf (cdr node) nil))))
    (ace-jump-tree-preorder-traverse tree func-delete-overlay)))


(defun ace-jump-update-overlay-in-search-tree (tree keys)
  "Update overlay 'display property using each name in keys"
  (let* ((key ?\0)
         (func-update-overlay (lambda (node)
                                (overlay-put (cdr node)
                                             'display
                                             (make-string 1 key)))))
    (loop for k in keys
          for n in (cdr tree)
          do (progn
               (setq key k)
               (if (eq (car n) 'branch)
                   (ace-jump-tree-preorder-traverse n
                                                       func-update-overlay)
                 (funcall func-update-overlay n))))))


(defun ace-jump-do( re-query-string &optional start-point end-point )
  "The main function to start the AceJump mode.
QUERY-STRING should be a valid regexp string, which finally pass to `search-forward-regexp'.

You can set the search area by START-POINT and END-POINT.
If you omit them, use the full screen as default.

You can constrol whether use the case sensitive via `ace-jump-mode-case-sensitive-search'.
"
  ;; we check the move key to make it valid, cause it can be customized by user
  (if (or (null ace-jump-mode-move-keys)
          (< (length ace-jump-mode-move-keys) 2)
          (not (every #'characterp ace-jump-mode-move-keys)))
    (error "[AceJump] Invalid move keys: check ace-jump-mode-move-keys"))
  ;; search candidate position
  (let ((candidate-list (ace-jump-search-candidate re-query-string start-point end-point)))
    (cond
     ;; cannot find any one
     ((null candidate-list)
      (error "[AceJump] No one found"))
     ;; we only find one, so move to it directly
     ((= (length candidate-list) 1)
      (goto-char (car candidate-list))
      (message "[AceJump] One candicate, move to it directly"))
     ;; more than one, we need to enter AceJump mode
     (t
      ;; create background
      (setq ace-jump-background-overlay
            (make-overlay (or start-point (window-start (selected-window)))
                          (or end-point (window-end   (selected-window)))
                          (current-buffer)))
      (overlay-put ace-jump-background-overlay 'face 'ace-jump-face-background)

      ;; construct search tree and populate overlay into tree
      (setq ace-jump-search-tree (ace-jump-tree-breadth-first-construct
                                     (length candidate-list)
                                     (length ace-jump-mode-move-keys)))
      (ace-jump-populate-overlay-to-search-tree ace-jump-search-tree
                                                   candidate-list)
      (ace-jump-update-overlay-in-search-tree ace-jump-search-tree
                                                 ace-jump-mode-move-keys)

      ;; do minor mode configuration
      (cond
       ((eq ace-jump-current-mode 'ace-jump-char-mode)
        (setq ace-jump-mode " AceJump - Char"))
       ((eq ace-jump-current-mode 'ace-jump-word-mode)
        (setq ace-jump-mode " AceJump - Word"))
       ((eq ace-jump-current-mode 'ace-jump-line-mode)
        (setq ace-jump-mode " AceJump - Line"))
       (t
        (setq ace-jump-mode " AceJump")))
      (force-mode-line-update)


      ;; override the local key map
      (setq overriding-local-map
            (let ( (map (make-keymap)) )
              (dolist (key-code ace-jump-mode-move-keys)
                (define-key map (make-string 1 key-code) 'ace-jump-move))
              (define-key map (kbd "C-c C-c") 'ace-jump-quick-exchange)
              (define-key map [t] 'ace-jump-done)
              map))

      (run-hooks 'ace-jump-mode-hook)

      (add-hook 'mouse-leave-buffer-hook 'ace-jump-done)
      (add-hook 'kbd-macro-termination-hook 'ace-jump-done)))))

(defun ace-jump-quick-exchange ()
  "The function that we can use to quick exhange the current mode between
word-mode and char-mode"
  (interactive)
  (cond
   ((eq ace-jump-current-mode 'ace-jump-char-mode)
    (if ace-jump-query-char
        ;; ace-jump-done will clean the query char, so we need to save it
        (let ((query-char ace-jump-query-char))
          (ace-jump-done)
          ;; restore the flag
          (setq ace-jump-query-char query-char)
          (setq ace-jump-current-mode 'ace-jump-word-mode)
          (ace-jump-do (concat "\\b"
                               (regexp-quote (make-string 1 query-char)))))))
   ((eq ace-jump-current-mode 'ace-jump-word-mode)
    (if ace-jump-query-char
        ;; ace-jump-done will clean the query char, so we need to save it
        (let ((query-char ace-jump-query-char))
          (ace-jump-done)
          ;; restore the flag
          (setq ace-jump-query-char query-char)
          (setq ace-jump-current-mode 'ace-jump-char-mode)
          (ace-jump-do (regexp-quote (make-string 1 query-char))))))
   ((eq ace-jump-current-mode 'ace-jump-line-mode)
    nil)
   (t
    nil)))




(defun ace-jump-char-mode ()
  "AceJump char mode"
  (interactive)
  (let ((query-char (read-char "Query Char:")))
    (if (ace-jump-query-char-p query-char)
        (progn
          (setq ace-jump-query-char query-char)
          (setq ace-jump-current-mode 'ace-jump-char-mode)
          (ace-jump-do (regexp-quote (make-string 1 query-char))))
      (error "[AceJump] Non-printable char"))))

(defun ace-jump-word-mode ()
  "AceJump word mode.
You can set `ace-jump-word-mode-use-query-char' to nil to prevent
asking for a head char, that will mark all the word in current
buffer."
  (interactive)
  (let ((head-char (if ace-jump-word-mode-use-query-char
                       (read-char "Head Char:")
                     nil)))
    (cond
     ((null head-char)
      (ace-jump-do "\\b\\sw"))
     ((ace-jump-query-char-p head-char)
      (setq ace-jump-query-char head-char)
      (setq ace-jump-current-mode 'ace-jump-word-mode)
      (ace-jump-do (concat "\\b"
                           (regexp-quote (make-string 1 head-char)))))
     (t
      (error "[AceJump] Non-printable char")))))


(defun ace-jump-line-mode ()
  "AceJump line mode.
Marked each no empty line and move there"
  (interactive)
  (setq ace-jump-current-mode 'ace-jump-line-mode)
  (ace-jump-do "^."))

;;;###autoload
(defun ace-jump-mode(&optional prefix)
  "AceJump mode is a minor mode for you to quick jump to a
position in the curret view.
   There is three submode now:
     `ace-jump-char-mode'
     `ace-jump-word-mode'
     `ace-jump-line-mode'

You can specify the sequence about which mode should enter
by customize `ace-jump-mode-submode-list'.

If you do not want to query char for word mode, you can change
`ace-jump-word-mode-use-query-char' to nil.

If you don't like the default move keys, you can change it by
setting `ace-jump-mode-move-keys'.

You can constrol whether use the case sensitive via
`ace-jump-mode-case-sensitive-search'.
"
  (interactive "p")
  (let ((index (/ prefix 4))
        (submode-list-length (length ace-jump-mode-submode-list)))
    (if (< index 0)
        (error "[AceJump] Invalid prefix command"))
    (if (>= index submode-list-length)
        (setq index (1- submode-list-length)))
    (funcall (nth index ace-jump-mode-submode-list))))

(defun ace-jump-move ()
  "move cursor based on user input"
  (interactive)
  (let* ((index (let ((ret (position (aref (this-command-keys) 0)
                                     ace-jump-mode-move-keys)))
                  (if ret ret (length ace-jump-mode-move-keys))))
         (node (nth index (cdr ace-jump-search-tree))))
    (cond
     ;; we do not find key in search tree. This can happen, for
     ;; example, when there is only three selections in screen
     ;; (totally five move-keys), but user press the forth move key
     ((null node)
      (message "No such selection")
      (ace-jump-done))
     ;; this is a branch node, which means there need further
     ;; selection
     ((eq (car node) 'branch)
      (let ((old-tree ace-jump-search-tree))
        ;; we use sub tree in next move, create a new root node
        ;; whose child is the sub tree nodes
        (setq ace-jump-search-tree (cons 'branch (cdr node)))
        (ace-jump-update-overlay-in-search-tree ace-jump-search-tree
                                                   ace-jump-mode-move-keys)

        ;; this is important, we need remove the subtree first before
        ;; do delete, we set the child nodes to nil
        (setf (cdr node) nil)
        (ace-jump-delete-overlay-in-search-tree old-tree)))
     ;; if the node is leaf node, this is the final one
     ((eq (car node) 'leaf)
      (run-hooks 'ace-jump-mode-before-jump-hook)
      (goto-char (overlay-start (cdr node)))
      (ace-jump-done))
     (t
      (ace-jump-done)
      (error "[AceJump] Internal error: tree node type is invalid")))))



(defun ace-jump-done()
  "stop AceJump motion"
  (interactive)
  ;; clear the status flag
  (setq ace-jump-query-char nil)
  (setq ace-jump-current-mode nil)

  ;; clean the status line
  (setq ace-jump-mode nil)
  (force-mode-line-update)

  ;; delete background overlay
  (when (not (null ace-jump-background-overlay))
      (delete-overlay ace-jump-background-overlay)
      (setq ace-jump-background-overlay nil))

  ;; delete overlays in search tree
  (ace-jump-delete-overlay-in-search-tree ace-jump-search-tree)
  (setq ace-jump-search-tree nil)

  (setq overriding-local-map nil)
  (run-hooks 'ace-jump-mode-end-hook)

  (remove-hook 'mouse-leave-buffer-hook 'ace-jump-done)
  (remove-hook 'kbd-macro-termination-hook 'ace-jump-done))


;;;; ============================================
;;;; Utilities for ace-jump-mode
;;;; ============================================

(defstruct aj-queue head tail)

(defun aj-queue-push (item q)
  "enqueue"
  (let ( (head (aj-queue-head q) )
         (tail (aj-queue-tail q) )
         (c (list item) ) )
    (cond
     ((null (aj-queue-head q))
      (setf (aj-queue-head q) c)
      (setf (aj-queue-tail q) c))
     (t
      (setf (cdr (aj-queue-tail q)) c)
      (setf (aj-queue-tail q) c)))))

(defun aj-queue-pop (q)
  "dequeue"
  (if (null (aj-queue-head q))
      (error "[AceJump] Interal Error: Empty queue"))

  (let ((ret (aj-queue-head q)))
    (if (eq ret (aj-queue-tail q))
        ;; only one item left
        (progn
          (setf (aj-queue-head q) nil)
          (setf (aj-queue-tail q) nil))
      ;; multi item left, move forward the head
      (setf (aj-queue-head q) (cdr ret)))
    (car ret)))



(provide 'ace-jump-mode)

;;; ace-jump-mode.el ends here
