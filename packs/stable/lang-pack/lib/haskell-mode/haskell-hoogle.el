;;; haskell-hoogle.el --- Look up Haskell documentation via hoogle -*- lexical-binding: t; -*-

;; Copyright © 2015 Steve Purcell
;;             2016 Arthur Fayzrakhmanov

;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords: docs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Functions for looking up documentation with hoogle, via
;; either local or remote servers.

;;; Code:

(require 'ansi-color)
(require 'haskell-mode)
(require 'haskell-utils)

(defun hoogle-prompt ()
  "Prompt for Hoogle query."
  (let ((def (haskell-ident-at-point)))
    (if (and def (symbolp def)) (setq def (symbol-name def)))
    (list (read-string (if def
                           (format "Hoogle query (default %s): " def)
                         "Hoogle query: ")
                       nil nil def)
          )))

(defcustom haskell-hoogle-command
  (if (executable-find "hoogle") "hoogle")
  "Name of the command to use to query Hoogle.
Can also be a function that returns the command as a string.
If nil, use the Hoogle web-site."
  :group 'haskell
  :type '(choice (const :tag "Use Web-site" nil)
                 string
                 function))

(defcustom haskell-hoogle-url "https://hoogle.haskell.org/?hoogle=%s"
  "Default value for hoogle web site."
  :group 'haskell
  :type '(choice
          (const :tag "haskell-org" "https://hoogle.haskell.org/?hoogle=%s")
          (const :tag "fp-complete" "https://www.stackage.org/lts/hoogle?q=%s")
          string))

;;;###autoload
(defun haskell-hoogle (query &optional info)
  "Do a Hoogle search for QUERY.

If prefix argument INFO is given, then `haskell-hoogle-command'
is asked to show extra info for the items matching QUERY.."
  (interactive (append (hoogle-prompt) current-prefix-arg))
  (if (null haskell-hoogle-command)
      (browse-url (format haskell-hoogle-url (url-hexify-string query)))
    (let* ((command (concat (if (functionp haskell-hoogle-command)
                                (funcall haskell-hoogle-command)
                              haskell-hoogle-command)
                          (if info " -i " "")
                          " --color " (shell-quote-argument query)))
         (output (shell-command-to-string command)))
    (with-help-window "*hoogle*"
      (with-current-buffer standard-output
        (insert output)
        (ansi-color-apply-on-region (point-min) (point-max)))))))

;;;###autoload
(defalias 'hoogle 'haskell-hoogle)

;;;###autoload
(defun haskell-hoogle-lookup-from-website (query)
  "Lookup QUERY at `haskell-hoogle-url'."
  (interactive (hoogle-prompt))
  (browse-url (format haskell-hoogle-url (url-hexify-string query))))

(defcustom haskell-hoogle-server-command (lambda (port)
                                           (list "hoogle" "server"
                                            "--local"
                                            "-p"
                                            (number-to-string port)))
  "Command used to start the local hoogle server."
  :group 'haskell
  :type 'function
  )

(defvar haskell-hoogle-server-process-name "emacs-local-hoogle")
(defvar haskell-hoogle-server-buffer-name (format "*%s*" haskell-hoogle-server-process-name))
(defvar haskell-hoogle-port-number 49513 "Port number.")
(defvar haskell-hoogle-server-process nil "The process handle of the local hoogle server.")

(defun haskell-hoogle-start-server ()
  "Start hoogle local server."
  (interactive)
  (unless (haskell-hoogle-server-live-p)
    (set 'haskell-hoogle-server-process
         (apply 'start-process
                (append (list haskell-hoogle-server-process-name
                              (get-buffer-create haskell-hoogle-server-buffer-name))
                        (funcall haskell-hoogle-server-command haskell-hoogle-port-number))))
    )
  )

(defun haskell-hoogle-server-live-p ()
  "Whether the hoogle server process is live."
  (condition-case _err
      (process-live-p haskell-hoogle-server-process)
    (error nil)))

(defun haskell-hoogle-kill-server ()
  "Kill the hoogle server if it is live."
  (interactive)
  (when (haskell-hoogle-server-live-p)
    (kill-process (get-buffer-create haskell-hoogle-server-buffer-name))
    (set 'haskell-hoogle-server-process nil)))

;;;###autoload
(defun haskell-hoogle-lookup-from-local ()
  "Lookup QUERY on local hoogle server."
  (interactive)
  (if (haskell-hoogle-server-live-p)
      (browse-url (format "http://localhost:%i/?hoogle=%s"
                          haskell-hoogle-port-number
                          (car (hoogle-prompt))))
    (haskell-mode-toggle-interactive-prompt-state)
    (unwind-protect
        (when (y-or-n-p "Hoogle server not running, start hoogle server? ")
          (haskell-hoogle-start-server))
      (haskell-mode-toggle-interactive-prompt-state t))))

(provide 'haskell-hoogle)
;;; haskell-hoogle.el ends here
