;;; elixir-format-test.el --- Basic tests for elixir-format

;;; Code:

(ert-deftest elixir-format-indents-a-buffer ()
  (when elixir-formatter-supported
    (ert-with-test-buffer (:name "(Expected)indents-a-buffer")
      (insert elixir-format-test-example)
      (elixir-format)
      (should (equal (buffer-string) elixir-format-formatted-test-example)))))

(ert-deftest elixir-format-indents-a-buffer-and-undoes-changes ()
  (when elixir-formatter-supported
    (ert-with-test-buffer ()
      (buffer-enable-undo)
      (setq buffer-undo-list nil)

      (insert elixir-format-test-example)

      (undo-boundary)
      (elixir-format)

      (should (equal (buffer-string) elixir-format-formatted-test-example))
      (undo 0)
      (should (equal (buffer-string) elixir-format-test-example)))))

(ert-deftest elixir-format-should-run-hook-before-formatting ()
  (when elixir-formatter-supported
    (ert-with-test-buffer ()
      (let ((has-been-run nil))
        (insert elixir-format-test-example)
        (add-hook 'elixir-format-hook (lambda () (setq has-been-run t)))
        (elixir-format)
        (should (equal has-been-run t))))))

(ert-deftest elixir-format-should-message-on-error ()
  (when elixir-formatter-supported
    (ert-with-test-buffer ()
      (insert elixir-format-wrong-test-example)
      (should-error
       (elixir-format)))))

(provide 'elixir-format-test)

;;; elixir-format-test.el ends here.
