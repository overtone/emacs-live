;; This is an example of how you could set up this file. This setup
;; requires a directory called util in the project root and that the
;; util directory contains the testing tools ert and espuds.

(let* ((features-directory
        (file-name-directory
         (directory-file-name (file-name-directory load-file-name))))
       (project-directory
        (file-name-directory
         (directory-file-name features-directory))))
  (setq clj-refactor-root-path project-directory))

(add-to-list 'load-path clj-refactor-root-path)

(require 'clojure-mode)
(require 'clj-refactor)
(require 'espuds)
(require 'ert)
(require 's)

(Setup
 ;; Used in cljr--maybe-eval-ns-form
 (defun cider-eval-ns-form (&rest _))
 (defun cljr--ensure-op-supported (op) t)
 (let ((tmp-dir(expand-file-name "tmp" clj-refactor-root-path)))
   (when (file-directory-p tmp-dir)
     (delete-directory tmp-dir t)))
 (yas-global-mode 1)
 (setq cljr-use-multiple-cursors t)
 (cljr-add-keybindings-with-prefix "C-!")
 (add-hook 'clojure-mode-hook (lambda () (clj-refactor-mode))))

(defun kill-matching-buffers-dont-ask (regexp &optional internal-too)
  (dolist (buffer (buffer-list))
    (let ((name (buffer-name buffer)))
      (when (and name (not (string-equal name ""))
                 (or internal-too (/= (aref name 0) ?\s))
                 (string-match regexp name))
        (with-current-buffer buffer
          (set-buffer-modified-p nil)
          (kill-buffer))))))

(defun kill-all-buffers-dont-ask (&optional internal-too)
  (dolist (buffer (buffer-list))
    (let ((name (buffer-name buffer)))
      (when (and name (not (string-equal name ""))
                 (or internal-too (/= (aref name 0) ?\s)))
        (with-current-buffer buffer
          (set-buffer-modified-p nil)
          (kill-buffer))))))

(Before
 )

(After
 (kill-all-buffers-dont-ask))

(Teardown
 ;; After when everything has been run
 )
