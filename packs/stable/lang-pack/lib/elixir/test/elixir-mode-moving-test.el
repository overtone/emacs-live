;;; elixir-mode-moving-test.el --- Tests for moving cursor functions

;;; Code:

(ert-deftest beginning-of-defun ()
  :tags '(moving)
  (elixir-test-with-temp-buffer
   "def foo do
  :bar
end
"
   (search-forward ":bar")
   (call-interactively 'beginning-of-defun)
   (should (= (point) (point-min)))))

(ert-deftest beginning-of-defun-nested ()
  :tags '(moving)
  (elixir-test-with-temp-buffer
   "
defmodule Foo do
  def bar do
    :baz
  end
end
"
   (search-forward ":baz")
   (call-interactively 'beginning-of-defun)
   (should (and (= (line-number-at-pos) 3) (bolp)))))

(ert-deftest end-of-defun ()
  :tags '(moving)
  (elixir-test-with-temp-buffer
   "def foo do
  :bar
end
"
   (search-forward ":bar")
   (call-interactively 'end-of-defun)
   (should (= (point) (point-max)))))

(ert-deftest end-of-defun-oneline ()
  :tags '(moving)
  (elixir-test-with-temp-buffer
   "def foo do: :bar"
   (search-forward ":bar")
   (call-interactively 'end-of-defun)
   (should (= (point) (line-end-position)))))

(ert-deftest end-of-defun-nested ()
  :tags '(moving)
  (elixir-test-with-temp-buffer
   "
defmodule Foo do
  def bar do
    :baz
  end
end
"
   (forward-line 1)
   (call-interactively 'end-of-defun)
   (should (= (point) (point-max)))

   (goto-char (point-min))
   (search-forward ":baz")
   (call-interactively 'end-of-defun)
   (should (and (= (line-number-at-pos) 6) (bolp)))))

(ert-deftest end-of-mark-defun ()
  :tags '(moving)
  (elixir-test-with-temp-buffer
   "
defmodule Foo do
  def bar do
    :baz
  end
end
"
   (goto-char (point-min))
   (search-forward ":baz")
   (call-interactively 'mark-defun)
   (should (= (count-lines (region-beginning) (region-end)) 3))))

(provide 'elixir-mode-moving-test)

;;; elixir-mode-helper-test.el ends here
