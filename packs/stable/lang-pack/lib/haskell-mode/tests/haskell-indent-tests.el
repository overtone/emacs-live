(require 'ert)
(require 'haskell-indent)
(require 'haskell-mode)


(ert-deftest haskell-indent-in-comment-1 ()
  "Document bad behavior. Should not assert."
  :expected-result :failed
  ;; Emacs 25 (snapshot) starts debugger on cl-assert
  ;; even in batch mode. So we do not run this test.
  (skip-unless (< emacs-major-version 25))
  (should (with-temp-buffer
	    (haskell-mode)
	    (haskell-indent-mode)
	    (insert (concat "module Test where\n"
			    "-- {{{ A header\n"
			    "--\n"))
	    (indent-for-tab-command)
	    t)))


;; haskell-indent-put-region-in-literate happens to be in haskell-indent
;; when the function is moved, move the tests also
(ert-deftest haskell-indent-put-region-in-literate-1 ()
  (should (equal "> literate"
		 (with-temp-buffer
		   (insert "literate")
		   (literate-haskell-mode)
		   (haskell-indent-put-region-in-literate (point-min) (point-max))
		   (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest haskell-indent-put-region-in-literate-2 ()
  :expected-result (if (< emacs-major-version 24)
		       :failed
		     :passed)
  (should (equal "literate"
		 (with-temp-buffer
		   (insert "> literate")
		   (literate-haskell-mode)
		   (haskell-indent-put-region-in-literate (point-min) (point-max) -1)
		   (buffer-substring-no-properties (point-min) (point-max))))))
