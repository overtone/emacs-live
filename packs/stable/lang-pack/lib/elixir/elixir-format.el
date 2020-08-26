;;; elixir-format.el --- Emacs plugin to mix format Elixir files

;; Copyright 2017-2018 Anil Wadghule, Christian Kruse

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; The elixir-format function formats the elixir files with Elixir's `mix format`
;; command

;; e.g.
;; M-x elixir-format
;;

(require 'ansi-color)

(defcustom elixir-format-arguments nil
  "Additional arguments to 'mix format'"
  :type '(repeat string)
  :group 'elixir-format)

(defcustom elixir-format-hook nil
  "Hook called by `elixir-format'."
  :type 'hook
  :group 'elixir-format)


;;; Code:

(defun elixir-format--errbuff ()
  (get-buffer-create "*elixir-format-errors*"))

(defun elixir-format--outbuff ()
  (get-buffer-create "*elixir-format-output*"))

(defun elixir-format--elixir-executable ()
  (executable-find "elixir"))

(defun elixir-format--mix-executable ()
  (executable-find "mix"))

;;;###autoload
(defun elixir-format (&optional called-interactively-p)
  (interactive "p")
  (if (not (elixir-format--elixir-and-mix-path-set-p))
      (elixir-format--display-missing-executables-error called-interactively-p)
    (unwind-protect
        (save-restriction
          (elixir-format--clean-output-buffers)
          (elixir-format--run-format called-interactively-p)))))

(defun elixir-format--elixir-and-mix-path-set-p ()
  (and (elixir-format--elixir-executable)
       (elixir-format--mix-executable)))

(defun elixir-format--display-missing-executables-error (called-interactively-p)
  (with-current-buffer (elixir-format--errbuff)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert "Emacs is unable to find the executables for elixir and/or mix. Either they are not installed on your system or emacs' PATH is not as wide as it needs to be. The latter is most likely to happen on OSX, in which case the simplest answer may be to add the exec-path-from-shell package to your configuration.")
    (setq buffer-read-only t)
    (ansi-color-apply-on-region (point-min) (point-max))
    (special-mode)
    (if called-interactively-p
        (display-buffer (elixir-format--errbuff))
      (error "Elixir Format error see %s" (elixir-format--errbuff)))))

(defun elixir-format--clean-output-buffers ()
  (with-current-buffer (elixir-format--outbuff)
    (erase-buffer))

  (with-current-buffer (elixir-format--errbuff)
    (setq buffer-read-only nil)
    (erase-buffer)))

(defun elixir-format--target-file-name ()
  "Returns the file name of current visited file.

If the buffer is not visiting any file (like during tests) then
it returns a file name based on the name of the buffer."
  (or buffer-file-name (concat (secure-hash 'md5 (buffer-name)) ".ex")))

(defun elixir-format--temp-file-path ()
  "Make a temp file in the current directory, because mix format
applies rules based on path patterns and looks for .formatter.exs
files in subdirectories."
  (let ((target-file-name (elixir-format--target-file-name)))
    (concat (file-name-sans-extension target-file-name)
            "-emacs-elixir-format."
            (file-name-extension target-file-name))))

(defun elixir-format--run-format (called-interactively-p)
  (let ((tmpfile (elixir-format--temp-file-path))
        (our-elixir-format-arguments (list "format")))

    (write-region nil nil tmpfile)
    (run-hooks 'elixir-format-hook)

    (when elixir-format-arguments
      (setq our-elixir-format-arguments (append our-elixir-format-arguments elixir-format-arguments)))
    (setq our-elixir-format-arguments (append our-elixir-format-arguments (list tmpfile)))

    (if (zerop (elixir-format--from-mix-root (elixir-format--mix-executable) (elixir-format--errbuff) our-elixir-format-arguments))
        (elixir-format--call-format-command tmpfile)
      (elixir-format--failed-to-format called-interactively-p))
    (delete-file tmpfile)
    (kill-buffer (elixir-format--outbuff))))

(defun elixir-format--call-format-command (tmpfile)
  (if (zerop (call-process-region (point-min) (point-max) "diff" nil (elixir-format--outbuff) nil "-n" "-" tmpfile))
      (message "File is already formatted")
    (elixir-format--apply-rcs-patch (elixir-format--outbuff))
    (message "elixir-format format applied"))
  (kill-buffer (elixir-format--errbuff)))

(defun elixir-format--failed-to-format (called-interactively-p)
  (with-current-buffer (elixir-format--errbuff)
    (setq buffer-read-only t)
    (ansi-color-apply-on-region (point-min) (point-max))
    (special-mode))

  (if called-interactively-p
      (display-buffer (elixir-format--errbuff))
    (error "elixir-format failed: see %s" (buffer-name (elixir-format--errbuff)))))

(defun elixir-format--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer.
Shamelessly stolen from go-mode (https://github.com/dominikh/go-mode.el)"

  (let ((target-buffer (current-buffer))
        ;; Relative offset between buffer line numbers and line numbers
        ;; in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so
        ;; we have to keep an offset when making changes to the
        ;; buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it
        ;; negative), deleting lines increments it. This order
        ;; simplifies the forward-line invocations.
        (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "Invalid rcs patch or internal error in elixir-format--apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (cl-decf line-offset len)
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (elixir-format--goto-line (- from line-offset))
                (cl-incf line-offset len)
                (elixir-format--delete-whole-line len)))
             (t
              (error "Invalid rcs patch or internal error in elixir-format--apply-rcs-patch")))))))))

(defun elixir-format--goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

(defun elixir-format--delete-whole-line (&optional arg)
  "Delete the current line without putting it in the `kill-ring'.
Derived from function `kill-whole-line'.  ARG is defined as for that
function.

Shamelessly stolen from go-mode (https://github.com/dominikh/go-mode.el)"
  (setq arg (or arg 1))
  (if (and (> arg 0)
           (eobp)
           (save-excursion (forward-visible-line 0) (eobp)))
      (signal 'end-of-buffer nil))
  (if (and (< arg 0)
           (bobp)
           (save-excursion (end-of-visible-line) (bobp)))
      (signal 'beginning-of-buffer nil))
  (cond ((zerop arg)
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (end-of-visible-line) (point))))
        ((< arg 0)
         (delete-region (progn (end-of-visible-line) (point))
                        (progn (forward-visible-line (1+ arg))
                               (unless (bobp)
                                 (backward-char))
                               (point))))
        (t
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (forward-visible-line arg) (point))))))


(defun elixir-format--from-mix-root (mix-path errbuff format-arguments)
  "Run mix format where `mix.exs' is located, because mix is
meant to be run from the project root. Otherwise, run in the
current directory."
  (let ((original-default-directory default-directory)
        (mix-dir (locate-dominating-file (elixir-format--target-file-name) "mix.exs")))

    (when mix-dir
      (setq default-directory (expand-file-name mix-dir)))

    (message (concat "Run "
                     (abbreviate-file-name default-directory) ": "
                     (mapconcat 'identity format-arguments " ")))

    (let ((result (apply #'call-process
                         mix-path nil errbuff nil format-arguments)))
      (setq default-directory original-default-directory)
      result)))

(provide 'elixir-format)

;;; elixir-format.el ends here
