(Given "^I open file \"\\(.+\\)\"$"
       (lambda (filename)
         (setq default-directory clj-refactor-root-path)
         (find-file filename)))

(Given "^I have a project \"\\([^\"]+\\)\" in \"\\([^\"]+\\)\"$"
       (lambda (project-name dir-name)
         (setq default-directory clj-refactor-root-path)

         ;; delete old directory
         (when (file-exists-p dir-name)
           (delete-directory dir-name t))

         ;; create directory structure
         (mkdir (expand-file-name project-name (expand-file-name "src" dir-name)) t)
         (mkdir (expand-file-name project-name (expand-file-name "test" dir-name)) t)

         ;; add project.clj
         (with-temp-file (expand-file-name "project.clj" dir-name)
           (insert "(defproject " project-name " \"0.1.0-SNAPSHOT\")"))))

(Given "^I have a clojure-file \"\\([^\"]+\\)\"$"
     (lambda (file-name)
       (setq default-directory clj-refactor-root-path)
       (find-file file-name)
       (save-buffer)
       (kill-buffer)))

(Then "^the file should be named \"\\([^\"]+\\)\"$"
      (lambda (file-name-postfix)
        (assert (s-ends-with? file-name-postfix (buffer-file-name)) nil "Expected %S to end with %S" (buffer-file-name) file-name-postfix)))

(And "^the cursor is inside the first defn form$"
  (lambda ()
    (goto-char (point-min))
    (re-search-forward "defn")))
