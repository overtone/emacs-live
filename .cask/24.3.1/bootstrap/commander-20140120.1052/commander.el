;;; commander.el --- Emacs command line parser

;; Copyright (C) 2013 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.7.0
;; Package-Version: 20140120.1052
;; Keywords: cli, argv
;; URL: http://github.com/rejeep/commander.el
;; Package-Requires: ((s "1.6.0") (dash "2.0.0") (cl-lib "0.3") (f "0.6.1"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:



(require 'cl-lib)
(require 'f)
(require 's)
(require 'dash)



(cl-defstruct commander-option
  "Structure describing an option.

Slots:

`flag' The option name (-f, -foo, --foo).

`description' Description of what the option does.

`function' Function to run when option used.

`default-values' Default values to call `function' with if none given.

`required' Required argument(s).

`optional' Optional argument(s).

`zero-or-more' Zero or more arguments allowed or required.

`one-or-more' One or more arguments allowed or required.

`to-string' String representation of option."
  flag flags description function default-values required optional
  zero-or-more one-or-more to-string)

(cl-defstruct commander-command
  "Structure describing a command.

Slots:

`command' The command name (foo, foo-bar).

`description' Description of what the command does.

`function' Function to run when command used.

`default-values' Default values to call `function' with if none given.

`required' Required argument(s).

`optional' Optional argument(s).

`zero-or-more' Zero or more arguments allowed or required.

`one-or-more' One or more arguments allowed or required.

`to-string' String representation of command."
  command description function default-values required optional
  zero-or-more one-or-more to-string)

(cl-defstruct commander-default-command
  "Structure describing the default command.

Slots:

`command' The name of the default command.

`arguments' The arguments to use for `command'."
  command arguments)

(cl-defstruct commander-no-command
  "Structure describing the no command.

Slots:

`function' The function to call when no command.

`arguments' The arguments to use for `function'."
  function arguments)




(defvar commander-options nil
  "List of all options.")

(defvar commander-commands nil
  "List of all commands.")

(defvar commander-parsing-done nil
  "Is parsing done or not.")

(defvar commander-name nil
  "Name of program.")

(defvar commander-description nil
  "Description of program.")

(defvar commander-default-config nil
  "List of default CLI configuration options from config file.")

(defvar commander-default-command nil
  "Command to use when no command parsed.")

(defvar commander-no-command nil
  "Command to use when no command, only options and input.")

(defvar commander-args nil
  "If parse directive is not called explicitly, use this first, then `command-line-args-left'.")

(defconst commander-option-re
  "\\(-[A-Za-z0-9-]\\|--?[A-Za-z0-9][A-Za-z0-9-]+\\)"
  "Regex matching an option flag.")

(defconst commander-command-re
  "\\([A-Za-z0-9][A-Za-z0-9-]*\\)"
  "Regex matching an command.")



(defun commander--find-option (option)
  (-first
   (lambda (commander-option)
     (equal (commander-option-flag commander-option) option))
   commander-options))

(defun commander--find-command (command)
  (-first
   (lambda (commander-command)
     (equal (commander-command-command commander-command) command))
   commander-commands))

(defun commander--handle-options (arguments)
  (let (rest (i 0))
    (while (< i (length arguments))
      (let ((argument (nth i arguments)))
        (if (s-matches? (concat "\\`" commander-option-re "\\'") argument)
            (let ((commander-option (commander--find-option argument)))
              (if commander-option
                  (let* ((function (commander-option-function commander-option))
                         (default-values (commander-option-default-values commander-option))
                         (required (commander-option-required commander-option))
                         (optional (commander-option-optional commander-option))
                         (zero-or-more (commander-option-zero-or-more commander-option))
                         (one-or-more (commander-option-one-or-more commander-option))
                         (option-arguments
                          (when (or required optional)
                            (if (or (and required one-or-more) (and optional zero-or-more))
                                (let (next-arguments)
                                  (while (and (nth (1+ i) arguments) (not (s-matches? (s-concat "\\`" commander-option-re "\\'") (nth (1+ i) arguments))))
                                    (setq i (1+ i))
                                    (push (nth i arguments) next-arguments))
                                  (nreverse next-arguments))
                              (when (and (nth (1+ i) arguments) (not (s-matches? (s-concat "\\`" commander-option-re "\\'") (nth (1+ i) arguments))))
                                (setq i (1+ i))
                                (nth i arguments))))))
                    (cond (required
                           (if option-arguments
                               (if one-or-more
                                   (apply function option-arguments)
                                 (funcall function option-arguments))
                             (if one-or-more
                                 (error "Option `%s` requires at least one argument" argument)
                               (error "Option `%s` requires argument" argument))))
                          (optional
                           (if zero-or-more
                               (apply function (or option-arguments default-values))
                             (if option-arguments
                                 (funcall function option-arguments)
                               (apply function default-values))))
                          (t (funcall function))))
                (error "Option `%s` not available" argument)))
          (push argument rest)))
      (setq i (1+ i)))
    (nreverse rest)))

(defun commander--handle-command (arguments)
  (let* ((command (car arguments))
         (rest (cdr arguments))
         (commander-command (commander--find-command command)))
    (if commander-command
        (let ((function (commander-command-function commander-command))
              (default-values (commander-command-default-values commander-command))
              (required (commander-command-required commander-command))
              (optional (commander-command-optional commander-command))
              (zero-or-more (commander-command-zero-or-more commander-command))
              (one-or-more (commander-command-one-or-more commander-command)))
          (unless rest
            (setq rest default-values))
          (cond (required
                 (if rest
                     (apply function rest)
                   (if one-or-more
                       (error "Command `%s` requires at least one argument" command)
                     (error "Command `%s` requires argument" command))))
                (optional
                 (apply function rest))
                (t
                 (funcall function))))
      (if commander-no-command
          (let ((function (commander-no-command-function commander-no-command)))
            (unless arguments
              (setq arguments (commander-no-command-arguments commander-no-command)))
            (apply function arguments))
        (when command (error "Command `%s` not available" command))))))

(defun commander--usage-commands ()
  (nreverse commander-commands))

(defun commander--usage-options ()
  (let ((-compare-fn
         (lambda (option-a option-b)
           (string=
            (commander-option-to-string option-a)
            (commander-option-to-string option-b)))))
    (nreverse (-uniq commander-options))))


;;;; Usage

(defun commander--usage-padding ()
  (let (max-option (max-option-value 0) max-command (max-command-value 0))
    (--each commander-options
      (setq max-option-value (max max-option-value (length (commander-option-to-string it)))))
    (--each commander-commands
      (setq max-command-value (max max-command-value (length (commander-command-to-string it)))))
    (+ (max max-option-value max-command-value) 10)))

(defun commander--usage-command-or-option (to-string description)
  (unless (listp description)
    (setq description (list description)))
  (let ((padding (commander--usage-padding)))
    (s-concat
     " "
     to-string
     (s-repeat (- padding (length to-string)) " ")
     (car description)
     (s-join
      ""
      (--map
       (s-concat "\n" (s-repeat (1+ padding) " ") it)
       (cdr description))))))

(defun commander--usage-command (commander-command)
  (let ((to-string (commander-command-to-string commander-command))
        (description (commander-command-description commander-command)))
    (commander--usage-command-or-option to-string description)))

(defun commander--usage-option (commander-option)
  (let ((to-string (commander-option-to-string commander-option))
        (description (commander-option-description commander-option)))
    (commander--usage-command-or-option to-string description)))

(defun commander-usage ()
  "Return usage information as a string."
  (let ((name (or commander-name (f-filename load-file-name)))
        (commands-string
         (s-join "\n" (--map (commander--usage-command it) (commander--usage-commands))))
        (options-string
         (s-join "\n" (--map (commander--usage-option it) (commander--usage-options)))))
    (s-concat
     (format "USAGE: %s [COMMAND] [OPTIONS]" name)
     (when commander-description
       (s-concat "\n\n" commander-description))
     (when commander-commands
       (s-concat "\n\nCOMMANDS:\n\n" commands-string))
     (when commander-options
       (s-concat "\n\nOPTIONS:\n\n" options-string)))))

(defun commander-usage-for (command-name)
  "Return description for COMMAND-NAME.

Return value is always a list with one item for each row."
  (-if-let (command (commander--find-command command-name))
      (let ((description (commander-command-description command)))
        (unless (listp description)
          (setq description (list description)))
        description)
    (error "No such command: %s" command-name)))

(defun commander-print-usage ()
  "Print usage information."
  (princ (concat (commander-usage) "\n")))

(defun commander-print-usage-for (command-name)
  "Print usage information for COMMAND-NAME."
  (-each (commander-usage-for command-name)
         (lambda (row)
           (princ (concat row "\n")))))

(defun commander-print-usage-and-exit (&optional exit-code)
  "Print usage information and exit.

If EXIT-CODE is specified, with with this code.  Default exit
code is 0."
  (commander-print-usage)
  (kill-emacs (or exit-code 0)))

(defun commander-print-usage-for-and-exit (command-name &optional exit-code)
  "Print usage information for COMMAND-NAME and exit.

If EXIT-CODE is specified, with with this code.  Default exit
code is 0."
  (commander-print-usage-for command-name)
  (kill-emacs (or exit-code 0)))



(defun commander-option (flags description function &rest default-values)
  (let (required optional zero-or-more one-or-more)
    (-map
     (lambda (flag)
       (let ((to-string flags))
         (let ((matches (s-match (concat "\\`" commander-option-re " " "<\\(.+\\)>" "\\'") flag)))
           (when matches
             (setq flag (nth 1 matches))
             (when (nth 2 matches)
               (setq required t)
               (if (equal (nth 2 matches) "*")
                   (setq one-or-more t)))))
         (let ((matches (s-match (concat "\\`" commander-option-re " " "\\[\\(.+\\)\\]" "\\'") flag)))
           (when matches
             (setq flag (nth 1 matches))
             (when (nth 2 matches)
               (setq optional t)
               (if (equal (nth 2 matches) "*")
                   (setq zero-or-more t)))))
         (add-to-list
          'commander-options
          (make-commander-option
           :flag flag
           :flags flags
           :description description
           :function function
           :default-values default-values
           :required required
           :optional optional
           :zero-or-more zero-or-more
           :one-or-more one-or-more
           :to-string to-string))))
     (-map 's-trim (s-split "," flags)))))

(defun commander-command (command description function &rest args)
  (let* (required
         optional
         zero-or-more
         one-or-more
         (to-string command)
         (default-values (-take-while 'stringp args)))
    (let ((matches (s-match (concat "\\`" commander-command-re " " "<\\(.+\\)>" "\\'") command)))
      (when matches
        (setq command (nth 1 matches))
        (when (nth 2 matches)
          (setq required t)
          (if (equal (nth 2 matches) "*")
              (setq one-or-more t)))))
    (let ((matches (s-match (concat "\\`" commander-command-re " " "\\[\\(.+\\)\\]" "\\'") command)))
      (when matches
        (setq command (nth 1 matches))
        (when (nth 2 matches)
          (setq optional t)
          (if (equal (nth 2 matches) "*")
              (setq zero-or-more t)))))
    (add-to-list
     'commander-commands
     (make-commander-command
      :command command
      :description description
      :function function
      :default-values default-values
      :required required
      :optional optional
      :zero-or-more zero-or-more
      :one-or-more one-or-more
      :to-string to-string))))

(defun commander-ignore-p ()
  "Returns true if parsing should be ignored, false otherwise.

By setting the variable `commander-ignore' to true, the parsing
will be ignored.  This is useful in for example unit tests."
  (and (boundp 'commander-ignore) commander-ignore))

(defun commander-parse (arguments)
  (unless (commander-ignore-p)
    (let* ((rest-config (commander--handle-options commander-default-config))
           (rest (or (commander--handle-options arguments) rest-config)))
      (unless rest
        (if commander-default-command
            (let ((command (commander-default-command-command commander-default-command))
                  (arguments (commander-default-command-arguments commander-default-command)))
              (setq rest (cons command arguments)))))
      (commander--handle-command rest))))

(defun commander-name (name)
  (setq commander-name name))

(defun commander-description (description)
  (setq commander-description description))

(defun commander-config (file)
  (when (f-file? file)
    (let ((lines (-reject 's-blank? (s-lines (f-read-text file 'utf-8)))))
      (setq commander-default-config
            (-flatten (--map (s-split " " it) lines))))))

(defun commander-default (command-or-function arguments)
  (if (stringp command-or-function)
      (setq
       commander-default-command
       (make-commander-default-command
        :command command-or-function
        :arguments arguments))
    (setq
     commander-no-command
     (make-commander-no-command
      :function command-or-function
      :arguments arguments))))



(defun commander--make-args (args)
  "Make proper command/option arguments from ARGS.

ARGS is the args that are passed to the `command' and `option'
directives. The return value is a list complete list that can be
sent to `commander-command' and `commander-options'.

If ARGS does not contain documentation, it is fetched from the
function doc string."
  (when (functionp (nth 1 args))
    (let ((description
           (-if-let (description (documentation (nth 1 args)))
               (s-lines description)
             "")))
      (setq args (-insert-at 1 description args))))
  args)

(defmacro commander (&rest forms)
  `(progn
     (setq commander-default-config nil)
     (setq commander-options nil)
     (setq commander-commands nil)
     (setq commander-name nil)
     (setq commander-description nil)
     (setq commander-default-command nil)
     (setq commander-no-command nil)
     (setq commander-parsing-done nil)
     (-each
      ',forms
      (lambda (form)
        (cl-case (car form)
          (option
           (cl-destructuring-bind (_ &rest args) form
             (apply 'commander-option (commander--make-args args))))
          (command
           (cl-destructuring-bind (_ &rest args) form
             (apply 'commander-command (commander--make-args args))))
          (parse
           (cl-destructuring-bind (_ arguments) form
             (commander-parse arguments)
             (setq commander-parsing-done t)))
          (name
           (cl-destructuring-bind (_ name) form
             (commander-name name)))
          (description
           (cl-destructuring-bind (_ description) form
             (commander-description description)))
          (config
           (cl-destructuring-bind (_ file) form
             (commander-config file)))
          (default
            (cl-destructuring-bind (_ command-or-function &rest arguments) form
              (commander-default command-or-function arguments)))
          (t (error "Unknown directive: %S" form)))))
     (unless commander-parsing-done
       (commander-parse (or commander-args (cdr command-line-args-left))))))

(provide 'commander)

;;; commander.el ends here
