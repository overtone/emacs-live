(defvar functions '())

(defun example-to-string (example)
  (let ((actual (car example))
        (expected (cadr (cdr example))))
    (replace-regexp-in-string
     "\r" "\\r"
     (replace-regexp-in-string
      "\t" "\\t"
      (replace-regexp-in-string
       "\n" "\\n"
       (replace-regexp-in-string
        "\\\\\\?" "?"
        (format "%S ;; => %S" actual expected)) t t) t t) t t)))

(defun examples-to-strings (examples)
  (let (result)
    (while examples
      (setq result (cons (example-to-string examples) result))
      (setq examples (cddr (cdr examples))))
    (nreverse result)))

(defun docs--signature (cmd)
  (if (eq 'macro (car cmd))
      (car (cddr cmd))
    (cadr cmd)))

(defun docs--docstring (cmd)
  (if (eq 'macro (car cmd))
      (cadr (cddr cmd))
    (car (cddr cmd))))

(defmacro defexamples (cmd &rest examples)
  (declare (indent 1))
  `(add-to-list 'functions (list
                            ',cmd
                            (docs--signature (symbol-function ',cmd))
                            (docs--docstring (symbol-function ',cmd))
                            (examples-to-strings ',examples))))

(defmacro def-example-group (group &rest examples)
  `(progn
     (add-to-list 'functions ,group)
     ,@examples))

(defun quote-and-downcase (string)
  (format "`%s`" (downcase string)))

(defun quote-docstring (docstring)
  (let (case-fold-search)
    (setq docstring (replace-regexp-in-string "\\b\\([A-Z][A-Z-]*[0-9]*\\)\\b" 'quote-and-downcase docstring t))
    (setq docstring (replace-regexp-in-string "`\\([^ ]+\\)'" "`\\1`" docstring t)))
  docstring)

(defun function-to-md (function)
  (if (stringp function)
      ""
    (let ((command-name (car function))
          (signature (cadr function))
          (docstring (quote-docstring (cadr (cdr function))))
          (examples (cadr (cddr function))))
      (format "### %s `%s`\n\n%s\n\n```cl\n%s\n```\n"
              command-name
              signature
              docstring
              (mapconcat 'identity (three-first examples) "\n")))))

(defun docs--chop-suffix (suffix s)
  "Remove SUFFIX if it is at end of S."
  (let ((pos (- (length suffix))))
    (if (and (>= (length s) (length suffix))
             (string= suffix (substring s pos)))
        (substring s 0 pos)
      s)))

(defun github-id (command-name signature)
  (docs--chop-suffix
   "-"
   (replace-regexp-in-string "[^a-zA-Z0-9-]+" "-" (format "%S %S" command-name signature))))

(defun function-summary (function)
  (if (stringp function)
      (concat "\n### " function "\n")
    (let ((command-name (car function))
          (signature (cadr function)))
      (format "* [%s](#%s) `%s`" command-name (github-id command-name signature) signature))))

(defun simplify-quotes ()
  (goto-char (point-min))
  (while (search-forward "(quote nil)" nil t)
    (replace-match "'()"))
  (goto-char (point-min))
  (while (search-forward "(quote " nil t)
    (forward-char -7)
    (let ((p (point)))
      (forward-sexp 1)
      (delete-char -1)
      (goto-char p)
      (delete-char 7)
      (insert "'"))))

(defun goto-and-remove (s)
  (goto-char (point-min))
  (search-forward s)
  (delete-char (- (length s))))

(defun create-docs-file ()
  (let ((functions (nreverse functions)))
    (with-temp-file "./README.md"
      (insert-file-contents-literally "./readme-template.md")

      (goto-and-remove "[[ function-list ]]")
      (insert (mapconcat 'function-summary functions "\n"))

      (goto-and-remove "[[ function-docs ]]")
      (insert (mapconcat 'function-to-md functions "\n"))

      (simplify-quotes))))

(defun three-first (list)
  (let (first)
    (when (car list)
      (setq first (cons (car list) first))
      (when (cadr list)
        (setq first (cons (cadr list) first))
        (when (car (cddr list))
          (setq first (cons (car (cddr list)) first)))))
    (nreverse first)))
