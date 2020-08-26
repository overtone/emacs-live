;;; test-helper.el --- Test helper

;;; Commentary:
;;

;;; Code:

(require 'ert-x)          ; `ert-with-test-buffer'
(require 'cl-lib)         ; `cl-defmacro'

(message "Running tests on Emacs %s" emacs-version)

;; The test fixtures assume an indentation width of 2, so we need to set that
;; up for the tests.
(setq-default default-tab-width 2
              indent-tabs-mode nil)

;; Load the elixir-mode under test
(require 'elixir-mode)

;; Load elixir-format under test
(require 'elixir-format)

;; Helpers

(cl-defmacro elixir-deftest (name args &body body)
  (declare (indent 2))
  `(ert-deftest ,(intern (format "elixir-ert-%s" name)) ()
     ""
     ,@args
     (let ((elixir-smie-verbose-p t))
       ,@body)))

(cl-defmacro elixir-ert-with-test-buffer ((&rest args) initial-contents &body body)
  (declare (indent 2))
  `(ert-with-test-buffer (,@args)
     (elixir-mode)
     (insert ,initial-contents)
     ,@body))

(defmacro elixir-test-with-temp-buffer (content &rest body)
  "Evaluate BODY in a temporary buffer with CONTENT."
  (declare (debug t)
           (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (elixir-mode)
     (font-lock-fontify-buffer)
     (goto-char (point-min))
     ,@body))

(cl-defmacro elixir-def-indentation-test (name args initial-contents expected-output)
  (declare (indent 2))
  `(elixir-deftest ,name ,args
     (elixir-ert-with-test-buffer (:name ,(format "(Expected)" name))
         ,initial-contents
       (let ((indented (ert-buffer-string-reindented)))
         (delete-region (point-min) (point-max))
         (insert ,expected-output)
         (ert-with-test-buffer (:name ,(format "(Actual)" name))
           (elixir-mode)
           (insert indented)
           (should (equal indented ,expected-output)))))))

(when (s-contains? "--win" (getenv "ERT_RUNNER_ARGS"))
  (defun ert-runner/run-tests-batch-and-exit (selector)
    (ert-run-tests-interactively selector)))

(setq elixir-format-elixir-path (executable-find "elixir"))
(setq elixir-format-mix-path (executable-find "mix"))

(defconst elixir-format-test-example "defmodule Foo do
use GenServer.Behaviour
def foobar do
if true, do: IO.puts \"yay\"
end
end")

(defconst elixir-format-wrong-test-example "defmodule Foo do
use GenServer.Behaviour
def foobar do
if true, do: IO.puts \"yay\"
end")

(setq elixir-version (let ((str (shell-command-to-string (concat elixir-format-elixir-path " --version"))))
  (car (when (string-match "\s\\(.+[.].+[.].+\\)[\s\n]" str)
    (list (match-string 1 str))))))

(defconst elixir-formatter-supported
  (>= (string-to-number elixir-version) (string-to-number "1.6"))
  )

(defconst elixir-format-formatted-test-example
  "defmodule Foo do
  use GenServer.Behaviour

  def foobar do
    if true, do: IO.puts(\"yay\")
  end
end
")

;;; test-helper.el ends here
