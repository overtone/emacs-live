
;;; custom-ruby.el --- custom Ruby helper commands

;; Copyright (C) 2007  Ola Bini

;; Author: Ola Bini <ola.bini@gmail.com>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Code:

(defun ruby-eval-buffer () (interactive)
      "Evaluate the buffer with ruby."
      (shell-command-on-region (point-min) (point-max) "ruby"))

(defun ruby-run-ruby  ()
   "Runs ruby on current buffer"
   (interactive)
   (compile (concat "/usr/local/bin/ruby " (buffer-file-name)) t))


(defun ruby-visit-source ()
 "If the current line contains text like '../src/program.rb:34', visit
that file in the other window and position point on that line."
 (interactive)
 (let* ((start-boundary (save-excursion (beginning-of-line) (point)))
        (regexp (concat "\\([ \t\n\r\"'([<{]\\|^\\)" ; non file chars or
                                       ; effective
                                       ; beginning of file
                        "\\(.+\\.rb\\):\\([0-9]+\\)")) ; file.rb:NNN
        (matchp (save-excursion
                  (end-of-line)
                  ;; if two matches on line, the second is most likely
                  ;; to be useful, so search backward.
                  (re-search-backward regexp start-boundary t))))
   (cond (matchp
          (let ((file (buffer-substring (match-beginning 2)
                                        (match-end 2)))
                (line (buffer-substring (match-beginning 3)
                                        (match-end 3))))
                                       ; Windows: Find-file doesn't seem to work with Cygwin
                                       ; //<drive>/ format or the odd /cygdrive/<drive>/ format
            (if (or (string-match "//\\(.\\)\\(.*\\)" file)
                    (string-match "/cygdrive/\\(.\\)\\(.*\\)" file))
                (setq file
                      (concat (substring file
                                         (match-beginning 1)
                                         (match-end 1))
                              ":"
                              (substring file
                                         (match-beginning 2)
                                         (match-end 2)))))

            (find-file-other-window file)
            (goto-line (string-to-int line))))
         (t
          (error "No ruby location on line.")))))


(defun ruby-eval-line () (interactive)
      "Evaluate the line with ruby."
      (shell-command-on-region (line-beginning-position) (line-end-position) "ruby"))

(defun ruby-send-block-or-line ()
 (save-excursion
   (if (re-search-backward "[\n\t ]\\(.*\\)[\n\t ]" nil t)
       (let ((foo (match-string 0)))
         (set-text-properties 0 (length foo) nil foo)
         (if (string= foo "end")
             (begin
              (previous-line -1)
              (ruby-send-definition))
             (ruby-send-region (line-beginning-position) (line-end-position)))))))

;; run the current test function

(setq imenu-auto-rescan t)       ; ensure function names auto-refresh
(setq imenu-max-item-length 200) ; ensure function names are not truncated
(require 'which-func)

(defun ruby-test-function ()
 "Test the current ruby function (must be runable via ruby <buffer> --name <test>)."
 (interactive)
 (let* ((funname (which-function))
        (fn (and (string-match "#\\(.*\\)" funname) (match-string 1 funname))))
   (compile (concat "ruby " (file-name-nondirectory (buffer-file-name)) " --name " fn))))

(defvar jruby-program-name "jirb --noreadline"
 "*Program invoked by the run-jruby command")
(defvar jruby-buffer nil "current jruby (actually jirb) process buffer.")

(defun run-jruby (cmd)
 "Run an inferior JRuby process, input and output via buffer *jruby*.
If there is a process already running in `*jruby*', switch to that buffer.
With argument, allows you to edit the command line (default is value
of `jruby-program-name').  Runs the hooks `inferior-ruby-mode-hook'
\(after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"

 (interactive (list (if current-prefix-arg
			 (read-string "Run JRuby: " jruby-program-name)
			 jruby-program-name)))
 (if (not (comint-check-proc "*jruby*"))
     (let ((cmdlist (ruby-args-to-list cmd)))
	(set-buffer (apply 'make-comint "jruby" (car cmdlist)
			   nil (cdr cmdlist)))
	(inferior-ruby-mode)))
 (setq jruby-program-name cmd)
 (setq jruby-buffer "*jruby*")
 (pop-to-buffer "*jruby*"))

(eval-after-load 'ruby-mode
  '(progn
     (define-key ruby-mode-map "\C-c\C-e" 'ruby-eval-buffer)
     (define-key ruby-mode-map "\C-c\C-s" 'ruby-eval-line)
     (define-key ruby-mode-map "\C-c\C-b" 'ruby-send-block-or-line)
     ;; run the current test function using F8 key
     (define-key ruby-mode-map [f8] 'ruby-test-function)

     ;; I bind the above to ^h^h, an odd choice, because that's easy to
     ;; type after reaching the line with ^p or ^n.
     (global-set-key "\C-h\C-h" 'ruby-visit-source)
     (global-set-key "\C-c\C-d" 'run-ruby)
     (global-set-key "\C-c\C-f" 'run-jruby)
     )
  )


;; Overwrite this one to make sure it does the same as I want it too
(defun ruby-indent-or-complete ()
 "Complete if point is at end of a word, otherwise indent line."
 (interactive)
 (indent-or-complete))

(provide 'custom-ruby)
;;; custom-ruby.el ends here