;; haskell-exec-tests.el --- -*- lexical-binding: t; -*-

(require 'ert)
(require 'haskell-test-utils)

(defvar haskell-example-script "echo")

(defun haskell-exec-test-output-argv-and-copy-stdin ()
  (let (line)
    (while argv
      (message-stdout "%s" (pop argv)))
    (while (setq line (read-stdin))
      (message-stdout "%s" line))))

(ert-deftest haskell-exec-subst-script ()
  (with-script-path haskell-example-script haskell-exec-test-output-argv-and-copy-stdin
    (with-temp-buffer
      (insert "line1\n")
      (insert "line2\n")
      (insert "line3-no-newline")
      (call-process-region (point-min) (point-max) haskell-example-script t t nil "-a" "--arg1" "/zonk" "filename.el")
      (should (equal "-a\n--arg1\n/zonk\nfilename.el\nline1\nline2\nline3-no-newline\n"
                     (buffer-substring-no-properties (point-min) (point-max)))))))
