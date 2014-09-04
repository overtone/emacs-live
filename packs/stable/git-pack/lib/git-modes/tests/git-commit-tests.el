(require 'cl)
(require 'ert)

(require 'git-commit-mode)

;;; tests

(defmacro git-commit-with-temp-buffer (&rest body)
  "Like `with-temp-buffer', but put the buffer in `git-commit' mode."
  `(with-temp-buffer
     (git-commit-mode)
     (erase-buffer)                   ; `git-commit-mode' adds newline
     (remove-hook 'kill-buffer-query-functions
                  'git-commit-kill-buffer-noop t)
     ,@body))


(defmacro git-commit-message-end-position-test (test-name msg comment)
  "Make a `git-commit-message-end-position' test."
  (declare (indent defun))
  `(ert-deftest ,(intern (concat "git-commit-message-end-position-test-"
                                 (symbol-name test-name))) ()
     (concat "Test `git-commit-message-end-position': " ,(symbol-name test-name))
     (git-commit-with-temp-buffer
       (insert ,msg ,comment)
       (should (equal (buffer-substring
                       1 (git-commit-find-pseudo-header-position))
                      ,msg)))))

(git-commit-message-end-position-test msg+nl+comment
  "msg\n\n" "#comment\n")
(git-commit-message-end-position-test msg+comment
  "msg\n" "#comment\n")
(git-commit-message-end-position-test msg+2comment
  "msg\n\n" "#comment\n\n#more comment\n")
(git-commit-message-end-position-test nl+comment
  "\n\n" "#comment\n\n#more comment\n")

(defconst git-commit-test-message-history
  (let ((ring (make-ring log-edit-maximum-comment-ring-size)))
    (ring-insert ring "msg one\n\n")
    (ring-insert ring "msg two\n\n")
    (ring-insert ring "msg three\n\n")
    ring))

(defmacro git-commit-with-temp-message-history (&rest body)
  `(let ((log-edit-comment-ring (ring-copy git-commit-test-message-history))
         (log-edit-comment-ring-index nil))
     (git-commit-with-temp-buffer
       ,@body)))

(ert-deftest git-commit-message-history-leave-comments ()
  "History cycling commands should not affect comments"
  (git-commit-with-temp-message-history
   (insert "current msg\n\n#comment")
   (git-commit-prev-message 1)
   (should (equal (buffer-string) "msg three\n\n#comment"))))

(ert-deftest git-commit-message-history-leave-comments-empty ()
  "History cycling commands should not affect comments, start from empty message."
  (git-commit-with-temp-message-history
   (insert "\n\n#comment")
   (git-commit-prev-message 1)
   (should (equal (buffer-string) "msg three\n\n#comment"))))
