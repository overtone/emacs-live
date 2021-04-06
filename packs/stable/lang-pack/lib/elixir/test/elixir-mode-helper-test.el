;;; elixir-mode-helper-test.el --- Tests for helper functions

;;; Code:

(ert-deftest check-if-currently-inside-heredoc ()
  (should (with-temp-buffer
            (elixir-mode)
            (insert "
defmodule Module.Name do

  @moduledoc \"\"\"
  ## Examples

  ....
  \"\"\"

end")
            (goto-line 7)
            (elixir-smie--heredoc-at-current-point-p)))
  (should (not (with-temp-buffer
                 (elixir-mode)
                 (insert "
defmodule Module.Name do

  @moduledoc \"\"\"
  ## Examples

  ....
  \"\"\"

end")
                 (goto-line 3)
                 (elixir-smie--heredoc-at-current-point-p)))))

(ert-deftest get-previous-line-indentation ()
  (should (equal 2
                 (with-temp-buffer
                   (elixir-mode)
                   (insert "
defmodule Module.Name do
  def what do
    1 + 1
  end
end")
                   (goto-line 4)
                   (elixir-smie--previous-line-indentation))))
  )


(ert-deftest check-if-previous-line-blank ()
  (should (not (with-temp-buffer
                 (elixir-mode)
                 (insert "
defmodule Module.Name do

  def what do
    1 + 1
  end
end")
                 (goto-line 3)
                 (elixir-smie--previous-line-empty-p))))
  (should (with-temp-buffer
            (elixir-mode)
            (insert "
defmodule Module.Name do


  def what do
    1 + 1
  end
end")
            (goto-line 4)
            (elixir-smie--previous-line-empty-p))))


;;; elixir-mode-helper-test.el --- Tests for helper functions

;;; Code:

(ert-deftest check-if-currently-inside-heredoc ()
  (should (with-temp-buffer
            (elixir-mode)
            (insert "
defmodule Module.Name do

  @moduledoc \"\"\"
  ## Examples

  ....
  \"\"\"

end")
            (goto-line 7)
            (elixir-smie--heredoc-at-current-point-p)))
  (should (not (with-temp-buffer
                 (elixir-mode)
                 (insert "
defmodule Module.Name do

  @moduledoc \"\"\"
  ## Examples

  ....
  \"\"\"

end")
                 (goto-line 3)
                 (elixir-smie--heredoc-at-current-point-p)))))

(ert-deftest get-previous-line-indentation ()
  (should (equal 2
                 (with-temp-buffer
                   (elixir-mode)
                   (insert "
defmodule Module.Name do
  def what do
    1 + 1
  end
end")
                   (goto-line 4)
                   (elixir-smie--previous-line-indentation))))
  )


(ert-deftest check-if-previous-line-blank ()
  (should (not (with-temp-buffer
                 (elixir-mode)
                 (insert "
defmodule Module.Name do

  def what do
    1 + 1
  end
end")
                 (goto-line 3)
                 (elixir-smie--previous-line-empty-p))))
  (should (with-temp-buffer
            (elixir-mode)
            (insert "
defmodule Module.Name do


  def what do
    1 + 1
  end
end")
            (goto-line 4)
            (elixir-smie--previous-line-empty-p))))


(ert-deftest test-current-line-contains-built-in-keyword ()
  (should (not (with-temp-buffer
                 (elixir-mode)
                 (insert "
defmodule Module.Name do

  def hey(test) do
    test.case
  end
end")
                 (goto-line 4)
                 (elixir-smie-current-line-contains-built-in-keyword-p))))
  (should (not (with-temp-buffer
                 (elixir-mode)
                 (insert "
defmodule Module.Name do

  def hey(test) do
    case do
    end
  end
end")
                 (goto-line 4)
                 (elixir-smie-current-line-contains-built-in-keyword-p)))))

(ert-deftest test-if-string-is-empty ()
  (should (equal (elixir-smie-empty-string-p nil)
                 t))
  (should (equal (elixir-smie-empty-string-p "")
                 t))
  (should (equal (elixir-smie-empty-string-p " ")
                 t))
  (should (equal (elixir-smie-empty-string-p "story")
                 nil))
  (should (equal (elixir-smie-empty-string-p "    ")
                 t)))

(provide 'elixir-mode-helper-test)

;;; elixir-mode-helper-test.el ends here
