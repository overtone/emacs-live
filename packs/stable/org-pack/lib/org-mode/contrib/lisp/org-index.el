;;; org-index.el --- A personal index for org and beyond

;; Copyright (C) 2011-2016 Free Software Foundation, Inc.

;; Author: Marc Ihm <org-index@2484.de>
;; Version: 4.2.1
;; Keywords: outlines index

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Purpose:
;;
;;  Fast search for selected org headings and things outside of org.
;;
;;  This package creates and updates an index table of headings or
;;  keywords, references and ids, where each line points to a heading
;;  within org or references something outside.  This table is sorted by
;;  usage count, so that frequently used lines appear among the first
;;  search results.
;;
;;  References are essentially small numbers (e.g. 'R237' or '--455--'), as
;;  created by this package; they are well suited to be used outside of
;;  org, e.g. in folder names, issue trackers or on printed documents.
;;
;;  On first invocation org-index will guide you to create a dedicated node
;;  for its index table and its configuration flags.
;;
;;  For basic usage, subcommands 'add' and 'occur' are most important.
;;
;;
;; Setup:
;;
;;  - Add these lines to your .emacs:
;;
;;    (require 'org-index)
;;    (org-index-default-keybindings)   ; optional
;;
;;  - Restart your Emacs to make these lines effective.
;;
;;  - Invoke `org-index', which will assist in creating your index
;;    table.  The variable org-index-id will be persisted within your
;;    customization file (typically .emacs).
;;
;;
;; Further reading:
;;
;;  See the documentation of `org-index', which can also be read
;;  by invoking `org-index' and choosing the help-command.
;;
;;
;; Updates:
;;
;;  The latest tested version of this file can always be found at:
;;
;;    http://orgmode.org/cgit.cgi/org-mode.git/plain/contrib/lisp/org-index.el

;;; Change Log:

;;   [2015-03-18 We] Version 4.2.1
;;   - No garbage in kill-ring
;;   - No recentering after add
;;
;;   [2015-03-08 Su] Version 4.2.0
;;   - Reference numbers for subcommands can be passed as a prefix argument
;;   - Renamed subcommand 'point' to 'ping'
;;   - New variable org-index-default-keybindings-list with a list of
;;     default keybindings for org-index-default-keybindings
;;   - Added new column level
;;   - removed flags get-category-on-add and get-heading-on-add
;;
;;   [2015-03-05 Th] Version 4.1.1 and 4.1.2
;;   - org-mark-ring is now used more consistently
;;   - Bugfix when going to a heading by ref
;;
;;   [2015-02-26 Th] Version 4.0.0 and 4.1.0:
;;   - Removed command "leave"; rather go back with org-mark-ring-goto
;;   - Property "org-index-ref" is no longer used or needed
;;   - Renamed column "link" to "id"
;;   - Added maintainance options to find duplicate rows, to check ids,
;;     update index or remove property org-index-ref from nodes
;;   - New command point
;;   - Shortened versin history
;;
;;   [2014-12-07 Sa] to [2015-01-31 Sa] Version 3.0.0 to 3.2.0:
;;   - Complete sorting of index only occurs in idle-timer
;;   - New command "maintain"  with some subcommands
;;   - Rewrote command "occur" with overlays in an indirect buffer
;;   - introduced variable org-index-version
;;   - Command "add" updates index, if node is already present
;;   - New commands "add" and "delete" to easily add and remove
;;     the current node to or from your index.
;;   - New command "example" to create an example index.
;;   - Moved flags to a list within the same node as the index table;
;;     this breaks compatibility to prior versions of the package.
;;   - Several new flags that are explained within index node.
;;   - Removed commands "reuse", "missing", "put", "goto",
;;     "update", "link", "fill", "unhighlight"
;;   - New function `org-index-default-keybindings'
;;
;;   [2012-12-07 Fr] to [2014-04-26 Sa] Version 2.0.0 to 2.4.3:
;;   - New functions org-index-new-line and org-index-get-line
;;     offer access to org-index from other lisp programs
;;   - Regression tests with ert
;;   - Renamed from "org-favtable" to "org-index"
;;   - Added an assistant to set up the index table
;;   - occur is now incremental, searching as you type
;;   - Integrated with org-mark-ring-goto
;;   - Added full support for ids
;;   - Renamed the package from "org-reftable" to "org-favtable"
;;   - Additional columns are required (e.g. "link"). Error messages will
;;     guide you
;;   - Ask user explicitly, which command to invoke
;;   - Renamed the package from "org-refer-by-number" to "org-reftable"
;;
;;   [2011-12-10 Sa] to [2012-09-22 Sa] Version Version 1.2.0 to 1.5.0:
;;    - New command "sort" to sort a buffer or region by reference number
;;    - New commands "highlight" and "unhighlight" to mark references
;;    - New command "head" to find a headline with a reference number
;;    - New commands occur and multi-occur
;;    - Started this Change Log

;;; Code:

(require 'org-table)
(require 'cl)

(defcustom org-index-id nil
  "Id of the Org-mode node, which contains the index table."
  :group 'org
  :group 'org-index)

;; Version of this package
(defvar org-index-version "4.2.1" "Version of `org-index', format is major.minor.bugfix, where \"major\" is a change in index-table and \"minor\" are new features.")

;; Variables to hold the configuration of the index table
(defvar org-index--maxref nil "Maximum number from reference table (e.g. '153').")
(defvar org-index--head nil "Any header before number (e.g. 'R').")
(defvar org-index--tail nil "Tail after number (e.g. '}' or ')'.")
(defvar org-index--numcols nil "Number of columns in index table.")
(defvar org-index--ref-regex nil "Regular expression to match a reference.")
(defvar org-index--ref-format nil "Format, that can print a reference.")
(defvar org-index--columns nil "Columns of index-table.")
(defvar org-index--special-columns nil "Columns with flags, that may appear only once.")
(defvar org-index--flagged-columns nil "Columns with flags, that may appear multiple times.")
(defvar org-index--buffer nil "Buffer of index table.")
(defvar org-index--point nil "Position at start of headline of index table.")
(defvar org-index--below-hline nil "Position of first cell in first line below hline.")
(defvar org-index--headings nil "Headlines of index-table as a string.")
(defvar org-index--headings-visible nil "Visible part of headlines of index-table as a string.")
(defvar org-index--keymap nil "Keymap for shortcuts for some commands of `org-index'. Filled and activated by `org-index-default-keybings'.")

;; Variables to hold context and state
(defvar org-index--last-ref nil "Last reference created or visited.")
(defvar org-index--category-before nil "Category of node before.")
(defvar org-index--active-region nil "Active region, initially.  I.e. what has been marked.")
(defvar org-index--below-cursor nil "Word below cursor.")
(defvar org-index--within-node nil "True, if we are within node of the index table.")
(defvar org-index--message-text nil "Text that was issued as an explanation; helpful for regression tests.")
(defvar org-index--occur-help-text nil "Text for help in occur buffer.")
(defvar org-index--occur-help-overlay nil "Overlay for help in occur buffer.")
(defvar org-index--occur-stack nil "Stack with overlays for hiding lines.")
(defvar org-index--occur-tail-overlay nil "Overlay to cover invisible lines.")
(defvar org-index--last-sort nil "Last column, the index has been sorted after.")
(defvar org-index--sort-timer nil "Timer to sort index in correct order.")
(defvar org-index--aligned nil "Remember for this Emacs session, if table has been aligned at least once.")

;; static information for this program package
(defconst org-index--commands '(occur add delete head ping enter ref help example sort multi-occur highlight maintain) "List of commands available.")
(defconst org-index--required-flags '(sort) "Flags that are required.")
(defconst org-index--single-flags '(sort point-on-add yank-after-add shift-ref-and-date-on-add) "Flags, that may only appear once; these can appear as special-columns.")
(defconst org-index--multiple-flags '(edit-on-add) "Flags, that might appear multiple times.")
(defconst org-index--all-flags (append org-index--single-flags org-index--multiple-flags) "All flags.")
(defconst org-index--required-headings '(ref id created last-accessed count) "All required headings.")
(defconst org-index--valid-headings (append org-index--required-headings '(keywords category level)) "All valid headings.")
(defconst org-index--occur-buffer-name "*org-index-occur*" "Name of occur buffer.")
(defconst org-index--sort-idle-delay 300 "Delay in seconds after which buffer will sorted.")
(defvar org-index-default-keybindings-list '(("a" . 'add) ("i " . nil) ("o" . 'occur) ("a" . 'add) ("d" . 'delete) ("h" . 'head) ("e" . 'enter) ("p." . 'ping) ("r" . 'ref) ("?" . 'help)) "One-letter short cuts for selected subcommands of `org-index', put in effect by `org-index-default-keybindings'")
(defconst org-index--sample-flags
"
  - columns-and-flags :: associate columns of index table with flags. Do not remove.
    - ref
      - yank-after-add
    - category
      - edit-on-add
    - keywords
      - edit-on-add
      - point-on-add
    - count
      - sort
    - last-accessed
    - created
    - id
    - all-columns-explained :: All columns of the index table and their meaning.
      - ref :: The reference number; will be generated automatically.
      - id :: id of the node, that this line represents
      - created :: When has this entry been created ?
      - last-accessed :: When has this entry been accessed last ?
      - count :: How many times has this entry been picked ?
      - keywords :: Optional column, suggested to keep a list of keywords,
        which may match your input during occur. While adding a line to your index,
        this column will be filled with the nodes heading.
      - category :: (optional) column to store the category of newly added nodes.
      - level :: Nesting level of node
      - Any name starting with a dot (`.') :: No predefined meaning,
        depends on its flags.
    - all-flags-explained :: All flags, that can be associated with columns.
      - sort :: Sort whole table according to this column.
      - yank-after-add :: This column will be yanked after picking this line during
        occur.
      - edit-on-add :: This field will be presented for editing, when adding
        a new line to your index.
      - point-on-add :: Point will land here, when adding a new line, e.g. with
        command ref.
      - shift-ref-and-date-on-add :: Remove leading reference and timestamp on add."
"A sample string of flags.")


(defmacro org-index--on (column value &rest body)
  "Execute the forms in BODY with point on index line whose COLUMN is VALUE.
The value returned is the value of the last form in BODY or nil,
if VALUE cannot be found."
  (declare (indent 2) (debug t))
  (let ((pointvar (make-symbol "point")) ; avoid clash with same-named variables in body
        (foundvar (make-symbol "found"))
        (retvar (make-symbol "ret")))
    `(save-current-buffer
       (set-buffer org-index--buffer)
       (setq ,pointvar (point))
       (setq ,foundvar nil)
       (setq ,retvar nil)

       (setq ,foundvar (org-index--go ,column ,value))
       (when ,foundvar
         (setq ,retvar (progn ,@body)))
       
       (goto-char ,pointvar)
       
       ,retvar)))


(defun org-index (&optional command search-ref arg)
  "Fast search for selected org headings and things outside of org.

This package creates and updates an index table of headings or
keywords, references and ids, where each line points to a heading
within org or references something outside.  This table is sorted by
usage count, so that frequently used lines appear among the first
search results.

References are essentially small numbers (e.g. 'R237' or '--455--'), as
created by this package; they are well suited to be used outside of
org, e.g. in folder names, issue trackers or on printed documents.

On first invocation `org-index' will guide you to create a dedicated node
for its index table and its configuration flags.

For basic usage, subcommands 'add' and 'occur' are most important.

This is version 4.2.1 of org-index.el.
\\<org-mode-map>
The function `org-index' operates on a dedicated table, the index
table, which lives within its own Org-mode node.  The table and
its containing node will be created, when you first invoke
`org-index'.  The node also contains a commented list, describing
the columns of the index table and their associated flags.  The
node is found through its id, which is stored within the variable
`org-index-id'.


The function `org-index' is the only interactive function of this
package and its main entry point; it will present you with a list
of subcommands to choose from:

  occur: Incremental search, that shows matching lines from the
    index table.  It is updated after every keystroke.  You may
    enter a list of words seperated by space or comma (`,'), to
    select lines that contain all of the given words.

  add: Add the current node to your index, so that it can be
    found through the subcommand \"occur\". Update index,
    if node has already been present.

  delete: Delete the current node from your index.

  head: Ask for a reference number and search for this heading.

  enter: Enter index table and maybe go to a specific reference;
    use `org-mark-ring-goto' (\\[org-mark-ring-goto]) to go back.

  ping: Echo line from index table for current node or first of
    its ancestor from index.

  ref: Create a new reference.

  help: Show this text.

  example: Create a temporary index, that will not be saved, but
    may serve as an example.

  sort: Sort lines in index, in region or buffer by contained
    reference, or sort index by count, reference or last access.

  multi-occur: Apply Emacs standard `multi-occur' operation on all
    `org-mode' buffers to search for the given reference.

  highlight: Highlight or unhiglight references in active region or buffer.
     Call with prefix argument (`C-u') to remove highlights.

  maintain: Offers some choices to check, update or fix your index.

If you invoke `org-index' for the first time, an assistant will be
invoked, that helps you to create your own, commented index.

Use `org-index-default-keybindings' to establish convenient
keyboard shortcuts.

See the commented list of flags within your index node for ways to
modify the behaviour of org-index.

A numeric prefix argument is used as a reference number for
commands, that need one (e.g. 'head').

Optional arguments for use from elisp: COMMAND is a symbol naming
the command to execute. SEARCH-REF specifies a reference to
search for, if needed. ARG allows passing in a prefix argument
as in interactive calls."

  (interactive "i\ni\nP")

  (let (search-id             ; id to search for
        sort-what             ; sort what ?
        kill-new-text         ; text that will be appended to kill ring
        message-text)         ; text that will be issued as an explanation


    ;;
    ;; Initialize and parse
    ;;

    ;; creates index table, if necessary
    (org-index--verify-id)

    ;; Get configuration of index table
    (org-index--parse-table)

    ;; store context information
    (org-index--retrieve-context)


    ;;
    ;; Arrange for proper sorting of index
    ;;

    ;; lets assume, that it has been sorted this way (we try hard to make sure)
    (unless org-index--last-sort (setq org-index--last-sort (org-index--special-column 'sort)))
    ;; rearrange for index beeing sorted into default sort order after 300 secs of idle time
    (unless org-index--sort-timer
      (setq org-index--sort-timer
            (run-with-idle-timer org-index--sort-idle-delay t 'org-index--sort-silent)))


    ;;
    ;; Find out, what we are supposed to do
    ;;

    ;; check or read command
    (if command
        (unless (memq command org-index--commands)
          (error "Unknown command '%s' passed as argument, valid choices are any of these symbols: %s"
                 command (mapconcat 'symbol-name org-index--commands ",")))
      (setq command (intern (org-completing-read
                             "Please choose: "
                             (mapcar 'symbol-name org-index--commands)))))


    ;;
    ;; Get search string, if required; process possible sources one after
    ;; another (lisp argument, prefix argumen, user input).
    ;;

    ;; Try prefix, if no lisp argument given
    (if (and (not search-ref)
             (numberp arg))
        (setq search-ref (format "%s%d%s" org-index--head arg org-index--tail)))
    
    ;; These actions really need a search string and may even prompt for it
    (when (memq command '(enter head multi-occur))

      ;; search from surrounding text ?
      (unless search-ref
        (if org-index--within-node

            (if (org-at-table-p)
                (setq search-ref (org-index--get-or-set-field 'ref)))
          
          (if (and org-index--below-cursor
                   (string-match (concat "\\(" org-index--ref-regex "\\)")
                                 org-index--below-cursor))
              (setq search-ref (match-string 1 org-index--below-cursor)))))
      
      ;; If we still do not have a search string, ask user explicitly
      (unless search-ref
        (if (eq command 'enter)
            (let ((r (org-index--read-search-for-enter)))
              (setq search-ref (car r))
              (setq search-id (cdr r)))
          (setq search-ref (read-from-minibuffer "Search reference number: "))))

      ;; Clean up search string
      (when search-ref
        (setq search-ref (org-trim search-ref))
        (if (string-match "^[0-9]+$" search-ref)
            (setq search-ref (concat org-index--head search-ref org-index--tail)))
        (if (string= search-ref "") (setq search-ref nil)))

      (if (and (not search-ref)
               (not (eq command 'enter)))
        (error "Command %s needs a reference number" command)))

    
    ;;
    ;; Command sort needs to know in advance, what to sort for
    ;;
    
    (when (eq command 'sort)
      (setq sort-what (intern (org-completing-read "You may sort:\n  - index  : your index table by various columns\n  - region : the active region by contained reference\n  - buffer : the whole current buffer\nPlease choose what to sort: " (list "index" "region" "buffer") nil t))))
    
    
    ;;
    ;; Enter table
    ;;

    ;; Arrange for beeing able to return
    (when (and (memq command '(occur head enter ref example sort maintain))
               (not (string= (buffer-name) org-index--occur-buffer-name)))
      (org-mark-ring-push))

    ;; These commands will leave user in index table after they are finished
    (when (or (memq command '(enter ref maintain))
              (and (eq command 'sort)
                   (eq sort-what 'index)))

      (pop-to-buffer-same-window org-index--buffer)
      (goto-char org-index--point)
      (org-index--unfold-buffer))


    ;;
    ;; Actually do, what is requested
    ;;

    (cond


     ((eq command 'help)

      ;; bring up help-buffer for this function
      (describe-function 'org-index))


     ((eq command 'multi-occur)

      ;; Construct list of all org-buffers
      (let (buff org-buffers)
        (dolist (buff (buffer-list))
          (set-buffer buff)
          (if (string= major-mode "org-mode")
              (setq org-buffers (cons buff org-buffers))))

        ;; Do multi-occur
        (multi-occur org-buffers (org-index--make-guarded-search search-ref))

        ;; Present results
        (if (get-buffer "*Occur*")
            (progn
              (setq message-text (format "multi-occur for '%s'" search-ref))
              (other-window 1)
              (toggle-truncate-lines 1))
          (setq message-text (format "Did not find '%s'" search-ref)))))


     ((eq command 'add)

      (let ((r (org-index--do-add-or-update)))
        (setq message-text (car r))
        (setq kill-new-text (cdr r))))


     ((eq command 'delete)

      (setq message-text (org-index--do-delete)))


     ((eq command 'head)

      (if (and org-index--within-node
               (org-at-table-p))
          (setq search-id (org-index--get-or-set-field 'id)))

      (setq search-id (or search-id (org-index--id-from-ref search-ref))) 
      (setq message-text
            (if search-id
                (org-index--do-head search-ref search-id)
              (message "Current line has no id."))))


     ((eq command 'enter)

      (goto-char org-index--below-hline)

      (setq message-text

            (if search-ref
                (if (org-index--go 'ref search-ref)
                    (progn
                      (org-index--update-current-line)
                      (org-table-goto-column (org-index--column-num 'ref))
                      (format "Found index line '%s'" search-ref))
                  (format "Did not find index line with reference '%s'" search-ref))

              (if search-id
                  (if (org-index--go 'id search-id)
                      (progn
                        (org-index--update-current-line)
                        (org-table-goto-column (org-index--column-num 'ref))
                        (format "Found index line '%s'" (org-index--get-or-set-field 'ref)))
                    (format "Did not find index line with id '%s'" search-id))

                ;; simply go into table
                (setq message-text "At index table"))))

      (recenter))


     ((eq command 'ping)

      (let ((moved-up 0) id info reached-top)

        (unless (string= major-mode "org-mode") (error "No node at point"))
        ;; take id from current node or reference
        (setq id (if search-ref
                     (org-index--id-from-ref search-ref)
                   (org-id-get)))

        ;; move up until we find a node in index
        (save-excursion
          (outline-back-to-heading)
          (while (not (or info
                          reached-top))
            (if id
                (setq info (org-index--on 'id id
                             (mapcar (lambda (x) (org-index--get-or-set-field x))
                                     (list 'ref 'count 'created 'last-accessed 'category 'keywords 'ref)))))

            (setq reached-top (= (org-outline-level) 1))

            (unless (or info
                        reached-top)
              (outline-up-heading 1 t)
              (incf moved-up))

            (setq id (org-id-get))))
        
        (if info
            (progn
              (setq message-text
                    (apply 'format
                           (append (list "'%s'%shas been accessed %s times between %s and %s; category is '%s', keywords are '%s'"
                                         (pop info)
                                         (if (> moved-up 0) (format " (parent node, %d level up) " moved-up) " "))
                                   info)))
              (setq kill-new-text (car (last info))))
          (setq message-text "Neither this node nor any of its parents is part of index"))))


     ((eq command 'occur)

      (set-buffer org-index--buffer)
      (org-index--do-occur))


     ((eq command 'ref)

      (let (new)

        ;; add a new row
        (setq new (org-index--create-new-line))

        ;; fill special columns with standard values
        (org-table-goto-column (org-index--column-num 'ref))
        (insert new)
        (setq org-index--last-ref new)

        ;; goto point-field or first empty one or first field
        (if (org-index--special-column 'point-on-add)
            (org-table-goto-column (org-index--column-num (org-index--special-column 'point-on-add)))
          (unless (catch 'empty
                    (dotimes (col org-index--numcols)
                      (org-table-goto-column (+ col 1))
                      (if (string= (org-trim (org-table-get-field)) "")
                          (throw 'empty t))))
            ;; none found, goto first
            (org-table-goto-column 1)))

        (if org-index--active-region (setq kill-new-text org-index--active-region))
        (setq message-text (format "Adding a new row with ref '%s'" new))))


     ((eq command 'sort)

      (let ((columns (list "ref" "count" "created" "last-accessed" "id"))
            sort groups-and-counts)

        (cond
         ((eq sort-what 'index)
          (setq sort
                (intern
                 (org-icompleting-read
                  "Please choose column to sort index table: "
                  (append (copy-list columns) (list "group-by"))
                  nil t nil nil (symbol-name (org-index--special-column 'sort)))))

          (when (eq sort 'group-by)
              (setq sort
                    (intern
                     (org-icompleting-read
                      "Please choose column to group index table by: "
                      columns
                      nil t nil nil (symbol-name (org-index--special-column 'sort)))))
              (setq groups-and-counts (org-index--collect-sort-groups sort)))

          (org-index--do-sort-index sort (first groups-and-counts))
          (org-table-goto-column (org-index--column-num sort))
          ;; When saving index, it should again be sorted correctly
          (with-current-buffer org-index--buffer
            (add-hook 'before-save-hook 'org-index--sort-silent t))
          
          (setq message-text
                (format
                 (concat "Your index has been sorted temporarily by %s and will be sorted again by %s after %d seconds of idle time"
                         (if groups-and-counts
                             "; %d groups with equal %s and a total of %d lines have been found"
                           ""))
                 (symbol-name sort)
                 (org-index--special-column 'sort)
                 org-index--sort-idle-delay
                 (second groups-and-counts)
                 (symbol-name sort)
                 (third groups-and-counts))))

         ((memq sort-what '(region buffer))
          (org-index--do-sort-lines sort-what)
          (setq message-text (format "Sorted %s by contained references" sort-what))))))


     ((eq command 'highlight)

      (let ((where "buffer"))
        (save-excursion
          (save-restriction
            (when (and transient-mark-mode
                       mark-active)
              (narrow-to-region (region-beginning) (region-end))
              (setq where "region"))

            (if arg
                (progn
                  (unhighlight-regexp org-index--ref-regex)
                  (setq message-text (format "Removed highlights for references in %s" where)))
              (highlight-regexp org-index--ref-regex 'isearch)
              (setq message-text (format "Highlighted references in %s" where)))))))


     ((eq command 'maintain)
      (setq message-text (org-index--do-maintain)))

     
     ((eq command 'example)

      (if (y-or-n-p "This assistant will help you to create a temporary index with detailed comments.\nDo you want to proceed ? ")
          (org-index--create-index t)))


     (t (error "Unknown subcommand '%s'" command)))


    ;; tell, what we have done and what can be yanked
    (if kill-new-text (setq kill-new-text
                            (substring-no-properties kill-new-text)))
    (if (string= kill-new-text "") (setq kill-new-text nil))
    (let ((m (concat
              message-text
              (if (and message-text kill-new-text)
                  " and r"
                (if kill-new-text "R" ""))
              (if kill-new-text (format "eady to yank '%s'." kill-new-text) (if message-text "." "")))))
      (unless (string= m "")
        (message m)
        (setq org-index--message-text m)))
    (if kill-new-text (kill-new kill-new-text))))


(defun org-index-default-keybindings (&optional prefix)
  "Set default keybindings for `org-index'.

Invoke subcommands of org index with a single key
sequence. Establish the common prefix key 'C-c i' which should be
followed by the first letter of a subcommand.

The ist of letters and subcommands is specified in within
`org-index-default-keybindings-list'.
  
See `org-index' for a description of all subcommands.

Optional argument PREFIX specifies common prefix, defaults to 'C-c i'"
  (interactive)

  (define-prefix-command 'org-index--keymap)
  ;; prefix command
  (global-set-key (kbd (or prefix "C-c i")) 'org-index--keymap)
  ;; loop over subcommands
  (mapcar
   (lambda (x)
     ;; loop over letters, that invoke the same subcommand
     (mapcar (lambda (c)
               (define-key org-index--keymap (kbd (char-to-string c))
                 `(lambda (arg) (interactive "P")
                    (message nil)
                    (org-index ,(cdr x) nil arg))))
             (car x)))
   org-index-default-keybindings-list))


(defun org-index-new-line (&rest keys-values)
  "Create a new line within the index table, returning its reference.

The function takes a varying number of argument pairs; each pair
is a symbol for an existing column heading followed by its value.
The return value is the new reference.

Example:

  (message \"Created reference %s\"
           (org-index-new-line 'keywords \"foo bar\" 'category \"baz\"))

Optional argument KEYS-VALUES specifies content of new line."

  (org-index--verify-id)
  (org-index--parse-table)

  (car (apply 'org-index--do-new-line keys-values)))


(defun org-index--do-new-line (&rest keys-values)
  "Do the work for `org-index-new-line'.
Optional argument KEYS-VALUES specifies content of new line."

  (save-excursion
    (org-index--retrieve-context)
    (with-current-buffer org-index--buffer
      (goto-char org-index--point)

      ;; check arguments early; they might come from lisp-user
      (let ((kvs keys-values)
            k v)
        (while kvs
          (setq k (car kvs))
          (setq v (cadr kvs))
          (if (eq k 'ref)
              (unless (memq v '(t nil))
                (error "Column 'ref' accepts only \"t\" or \"nil\""))
            (if (or (not (symbolp k))
                    (and (symbolp v) (not (eq v t)) (not (eq v nil))))
                (error "Arguments must be alternation of key and value")))
          (unless (org-index--column-num k)
            (error "Unknown column or column not defined in table: '%s'" (symbol-name k)))
          (setq kvs (cddr kvs))))

      (let (ref yank)
        ;; create new line
        (setq ref (org-index--create-new-line))
        (plist-put keys-values 'ref ref)

        ;; fill columns
        (let ((kvs keys-values)
              k v n)
          (while kvs
            (setq k (car kvs))
            (setq v (cadr kvs))
            (org-table-goto-column (org-index--column-num k))
            (insert (org-trim v))
            (setq kvs (cddr kvs))))

        ;; align and fontify line
        (org-index--promote-current-line)
        (org-index--align-and-fontify-current-line)
        
        ;; get column to yank
        (setq yank (org-index--get-or-set-field (org-index--special-column 'yank-after-add)))

        (cons ref yank)))))


(defun org-index-get-line (column value)
  "Retrieve an existing line within the index table by ref or id.
Return its contents as a property list.

The function `plist-get' may be used to retrieve specific elements
from the result.

Example:

  (plist-get (org-index-get-line 'ref \"R12\") 'count)

retrieves the value of the count-column for reference number 12.

Argument COLUMN is a symbol, either ref or id,
argument VALUE specifies the value to search for."
  ;; check arguments
  (unless (memq column '(ref id))
    (error "Argument column can only be 'ref' or 'id'"))

  (unless value
    (error "Need a value to search for"))
  
  (org-index--verify-id)
  (org-index--parse-table)

  (org-index--get-line column value))


(defun org-index--get-line (column value)
  "Find a line by ID, return its contents.
Argument COLUMN and VALUE specify line to get."
  (let (content)
    (org-index--on
     column value
     (mapc (lambda (x)
             (if (and (numberp (cdr x))
                      (> (cdr x) 0))
                 (setq content (cons (car x) (cons (or (org-index--get-or-set-field (car x)) "") content)))))
           (reverse org-index--columns)))
    content))


(defun org-index--delete-line (id)
  "Delete a line specified by ID."
  (let (content)
    (org-index--on
     'id id
     (let ((start (line-beginning-position)))
       (beginning-of-line)
       (forward-line)
       (delete-region start (point))
       t))))


(defun org-index--ref-from-id (id)
  "Get reference from line ID."
  (org-index--on 'id id (org-index--get-or-set-field 'ref)))


(defun org-index--id-from-ref (ref)
  "Get id from line REF."
  (org-index--on 'ref ref (org-index--get-or-set-field 'id)))


(defun org-index--read-search-for-enter ()
    "Special input routine for command enter."
  ;; Accept single char commands or switch to reading a sequence of digits
  (let (char prompt search-ref search-id)
    
    ;; start with short prompt but give more help on next iteration
    (setq prompt "Please specify, where to go in index (0-9.,space,backspace,return or ? for help): ")
    
    ;; read one character
    (while (not (memq char (append (number-sequence ?0 ?9) (list ?\d ?\b ?\r ?\j ?\s ?.))))
      (setq char (read-char prompt))
      (setq prompt "Go to index table and specific position. Digits specify a reference number to got to, <space> goes to top of index, <backspace> or <delete> to last line created and <return> or `.' to index line of current node. Please choose: "))
    
    (if (memq char (number-sequence ?0 ?9))
        ;; read rest of digits
        (setq search-ref (read-from-minibuffer "Search reference number: " (char-to-string char))))
    ;; decode single chars
    (if (memq char '(?\r ?\n ?.)) (setq search-id (org-id-get)))
    (if (memq char '(?\d ?\b)) (setq search-ref (number-to-string org-index--maxref)))

    (cons search-ref search-id)))


(defun org-index--verify-id ()
  "Check, that we have a valid id."

  ;; Check id
  (unless org-index-id
    (let ((answer (org-completing-read "Cannot find an index (org-index-id is not set). You may:\n  - read-help    : to learn more about org-index\n  - create-index : invoke an assistant to create an initial index\nPlease choose: " (list "read-help" "create-index") nil t nil nil "read-help")))
      (if (string= "create-index" answer)
          (org-index--create-missing-index "Variable org-index-id is not set, so probably no index table has been created yet.")
        (describe-function 'org-index))))

  ;; Find node
  (let (marker)
    (setq marker (org-id-find org-index-id 'marker))
    (unless marker (org-index--create-missing-index "Cannot find the node with id \"%s\" (as specified by variable org-index-id)." org-index-id))
    ; Try again with new node
    (setq marker (org-id-find org-index-id 'marker))
    (unless marker (error "Could not create node"))
    (setq org-index--buffer (marker-buffer marker)
          org-index--point (marker-position marker))
    (move-marker marker nil)))


(defun org-index--retrieve-context ()
  "Collect context information before starting with command."

  ;; Get the content of the active region or the word under cursor
  (setq org-index--active-region
        (if (and transient-mark-mode mark-active)
            (buffer-substring (region-beginning) (region-end))
          nil))
  (setq org-index--below-cursor (thing-at-point 'symbol))

  ;; get category of current node
  (setq org-index--category-before
        (save-excursion ; workaround: org-get-category does not give category when at end of buffer
          (beginning-of-line)
          (org-get-category (point) t)))

  ;; Find out, if we are within index table or not
  (setq org-index--within-node (string= (org-id-get) org-index-id)))


(defun org-index--parse-table ()
  "Parse content of index table."

  (let (ref-field
        id-field
        initial-point
        end-of-headings
        start-of-headings)

    (with-current-buffer org-index--buffer

      (setq org-index--maxref 0)
      (setq initial-point (point))

      (org-index--go-below-hline)

      ;; align and fontify table once for this emacs session
      (unless org-index--aligned
        (org-table-align) ; needs to happen before fontification to be effective ?
        (let ((is-modified (buffer-modified-p))
              (below (point)))
          (while (org-at-table-p)
            (forward-line))
          (font-lock-fontify-region below (point))
          (org-index--go-below-hline)
          (setq org-index--aligned t)
          (set-buffer-modified-p is-modified)))
      
      (org-index--go-below-hline)
      (setq org-index--below-hline (point-marker))
      (beginning-of-line)

      ;; get headings to display during occur
      (setq end-of-headings (point))
      (while (org-at-table-p) (forward-line -1))
      (forward-line)
      (setq start-of-headings (point))
      (setq org-index--headings-visible (substring-no-properties (org-index--copy-visible start-of-headings end-of-headings)))
      (setq org-index--headings (buffer-substring start-of-headings end-of-headings))

      ;; count columns
      (org-table-goto-column 100)
      (setq org-index--numcols (- (org-table-current-column) 1))

      ;; go to top of table
      (while (org-at-table-p)
        (forward-line -1))
      (forward-line)

      ;; parse line of headings
      (org-index--parse-headings)

      ;; parse list of flags
      (goto-char org-index--point)
      (org-index--parse-flags)

      ;; Retrieve any decorations around the number within the first nonempty ref-field
      (goto-char org-index--below-hline)
      (while (and (org-at-table-p)
                  (not (setq ref-field (org-index--get-or-set-field 'ref))))
        (forward-line))

      ;; Some Checking
      (unless ref-field
        (org-index--report-index-error "Reference column is empty"))

      (unless (string-match "^\\([^0-9]*\\)\\([0-9]+\\)\\([^0-9]*\\)$" ref-field)
        (org-index--report-index-error
         "First reference in index table ('%s') does not contain a number" ref-field))

      ;; These are the decorations used within the first ref of index
      (setq org-index--head (match-string 1 ref-field))
      (setq org-index--tail (match-string 3 ref-field))
      (setq org-index--ref-regex (concat (regexp-quote org-index--head)
                                         "\\([0-9]+\\)"
                                         (regexp-quote org-index--tail)))
      (setq org-index--ref-format (concat org-index--head "%d" org-index--tail))

      ;; Go through table to find maximum number and do some checking
      (let ((ref 0))

        (while (org-at-table-p)

          (setq ref-field (org-index--get-or-set-field 'ref))
          (setq id-field (org-index--get-or-set-field 'id))

          (when (and (not ref-field)
                     (not id-field))
            (kill-whole-line)
            (message "Removing line from index-table with both ref and id empty"))

          (if ref-field
              (if (string-match org-index--ref-regex ref-field)
                  ;; grab number
                  (setq ref (string-to-number (match-string 1 ref-field)))
                (kill-whole-line)
                (message "Removing line from index-table whose ref does not contain a number")))

          ;; check, if higher ref
          (if (> ref org-index--maxref) (setq org-index--maxref ref))

          (forward-line 1)))

      ;; go back to initial position
      (goto-char initial-point))))


(defun org-index--do-maintain ()
  "Choose among and perform some tasks to maintain index."
  (let ((check-what) (max-mini-window-height 1.0) message-text)
    (setq check-what (intern (org-completing-read "These checks and fixes are available:\n  - statistics : compute statistics about index table\n  - check      : check ids by visiting their nodes\n  - duplicates : check index for duplicate rows (any column)\n  - clean      : remove obsolete property org-index-id\n  - update     : update content of index lines, with an id \nPlease choose: " (list "statistics" "check" "duplicates" "clean" "update") nil t nil nil "statistics")))
    (message nil)
    
    (cond
     ((eq check-what 'check)
      (setq message-text (or (org-index--check-ids)
                             "No problems found")))

     ((eq check-what 'statistics)
      (setq message-text (org-index--do-statistics)))

     ((eq check-what 'duplicates)
      (setq message-text "Finding duplcates can be done by sorting your index appropriately: Choose 'group-by' and select a column; rows will then be sorted together, if they have the same value within the coosen column."))

     ((eq check-what 'clean)
      (let ((lines 0))
        (org-map-entries
         (lambda ()
           (when (org-entry-get (point) "org-index-ref")
             (incf lines)
             (org-entry-delete (point) "org-index-ref")))
         nil 'agenda)
        (setq message-text (format "Removed property 'org-index-ref' from %d lines" lines))))
     
     ((eq check-what 'update)
      (if (y-or-n-p "Updating your index will overwrite certain columns with content from the associated heading and category.  If unsure, you may try this for a single, already existing line of your index by doing `add' from within your index.  Are you SURE to proceed for ALL INDEX LINES ? ")
          (setq message-text (org-index--update-all-lines))
        (setq message-text "Canceled."))))
    message-text))


(defun org-index--do-sort-index (sort &optional groups)
  "Sort index table according to SORT, optinally with GROUPS."

  (let ((is-modified (buffer-modified-p))
        top
        bottom
        ref-field
        count-field)

    (unless buffer-read-only

      (message "Sorting table for %s..." (symbol-name sort))
      (undo-boundary)

      (let ((message-log-max nil)) ; we have just issued a message, dont need those of sort-subr

        ;; get boundaries of table
        (goto-char org-index--below-hline)
        (forward-line 0)
        (setq top (point))
        (while (org-at-table-p) (forward-line))

        ;; kill all empty rows at bottom
        (while (progn
                 (forward-line -1)
                 (org-table-goto-column 1)
                 (and
                  (not (org-index--get-or-set-field 'ref))
                  (not (org-index--get-or-set-field 'id))))
          (org-table-kill-row))
        (forward-line 1)
        (setq bottom (point))
        
        ;; sort lines
        (save-restriction
          (narrow-to-region top bottom)
          (goto-char top)
          (sort-subr t
                     'forward-line
                     'end-of-line
                     (lambda ()
                       (concat
                        (if groups
                            (format "%06d-" (cdr (assoc (org-index--get-or-set-field sort) groups)))
                          "")
                        (org-index--get-sort-key sort t)))
                     nil
                     'string<)
          (goto-char (point-min))

          ;; restore modification state
          (set-buffer-modified-p is-modified)))

        (setq org-index--last-sort sort))))


(defun org-index--collect-sort-groups (sort)
  "Collect groups to SORT for."
  (let ((count-groups 0) (count-lines 0)
        groups key key-value)
    
    (org-index--on
        nil nil
        (while (org-at-table-p)
          (setq key (org-index--get-or-set-field sort))
          (setq key-value (assoc key groups))
          (if key-value
              (progn
                (incf (cdr key-value)))
            (setq groups (cons (cons key 1) groups)))
          (forward-line)))

    (mapc (lambda (x) (when (> (cdr x) 1)
                   (incf count-groups)
                   (incf count-lines (cdr x))))
          groups)

    (list groups count-groups count-lines)))


(defun org-index--do-sort-lines (what)
  "Sort lines in WHAT according to contained reference."
  (save-restriction
    (cond
     ((eq what 'region)
      (if (region-active-p)
          (narrow-to-region (region-beginning) (region-end))
        (error "No active region, cannot sort")))
     ((eq what 'buffer)
      (unless (y-or-n-p "Sort whole current buffer ? ")
        (error "Canceled"))
      (narrow-to-region (point-min) (point-max))))

    (goto-char (point-min))
    (sort-subr nil 'forward-line 'end-of-line
               (lambda ()
                 (if (looking-at (concat ".*"
                                         (org-index--make-guarded-search org-index--ref-regex 'dont-quote)))
                     (string-to-number (match-string 1))
                   0)))))


(defun org-index--go-below-hline ()
  "Move below hline in index-table."

  (let ((count 0)
        (errstring (format "index table within node %s" org-index-id)))

    (goto-char org-index--point)

    ;; go to heading of node
    (while (not (org-at-heading-p)) (forward-line -1))
    (forward-line 1)

    ;; go to first table, but make sure we do not get into another node
    (while (and (not (org-at-table-p))
                (not (org-at-heading-p))
                (not (eobp)))
      (forward-line))

    ;; check, if there really is a table
    (unless (org-at-table-p)
      (org-index--create-missing-index "Cannot find %s." errstring))

    ;; go just after hline
    (while (and (not (org-at-table-hline-p))
                (org-at-table-p))
      (forward-line))
    (forward-line)

    ;; and check
    (unless (org-at-table-p)
      (org-index--report-index-error "Cannot find a hline within %s" errstring))

    (org-table-goto-column 1)))


(defun org-index--parse-headings ()
  "Parse headings of index table."

  (let (field        ;; field content
        field-symbol ;; and as a symbol
        found)

    (setq org-index--columns nil)

    ;; For each column
    (dotimes (col org-index--numcols)

      (setq field (substring-no-properties (downcase (org-trim (org-table-get-field (+ col 1))))))

      (if (string= field "")
          (error "Heading of column cannot be empty"))
      (if (and (not (string= (substring field 0 1) "."))
               (not (member (intern field) org-index--valid-headings)))

          (if (string= field "link")
              ;; Ask user to migrate his index to new version (since [2015-02-11 Mi])
              (progn
                ;; pop to index buffer
                (pop-to-buffer-same-window org-index--buffer)
                (goto-char org-index--below-hline)
                (org-reveal t)
                ;; go to column
                (while (org-at-table-p)
                  (forward-line -1))
                (forward-line)
                (org-table-goto-column (+ 1 col))
                (error "Column 'link' should be named 'id' with recent versions of org-index,\nplease adjust your table (cursor is already positioned right)"))
            (error "Column name '%s' is not a valid heading (custom headings may start with a dot, e.g. '.foo')" field)))

      (setq field-symbol (intern field))

      ;; check if heading has already appeared
      (if (assoc field-symbol org-index--columns)
          (org-index--report-index-error
           "'%s' appears two times as column heading" (downcase field))
        ;; add it to list at front, reverse later
        (setq org-index--columns (cons (cons field-symbol (+ col 1)) org-index--columns)))))

  (setq org-index--columns (reverse org-index--columns))

  ;; check if all necessary headings have appeared
  (mapc (lambda (head)
          (unless (cdr (assoc head org-index--columns))
            (org-index--report-index-error "No column has heading '%s'" head)))
        org-index--required-headings))


(defun org-index--parse-flags ()
  "Parse list of flags in index table."

  (let (parent parent-is-comment child)

    ;; reset configuration variables
    (setq org-index--special-columns nil)
    (setq org-index--flagged-columns nil)

    (org-index--goto-list "columns-and-flags" t)
    (forward-line 1)

    ;; outer loop over columns
    (while (and (setq parent (org-index--parse-list-item))
                parent
                (> (cdr (assoc :indent parent)) 0))

      (setq parent-is-comment (member (cdr (assoc :text parent)) '("all-columns-explained" "all-flags-explained")))

      ;; check, that we have a valid heading
      (unless (or parent-is-comment
                  (assoc (cdr (assoc :sym parent)) org-index--columns))
        (when (string= "link" (cdr (assoc :text parent)))
          (pop-to-buffer-same-window org-index--buffer)
          (org-reveal t)
          (error "Flag 'link' should be named 'id' with recent versions of org-index,\nplease adjust this flag (cursor is already positioned right)"))
        (org-index--report-index-error "'%s' appears within flags, but not as a index column.  " (cdr (assoc :text parent))))

      ;; inner loop over children
      (while (and (forward-line 1)
                  (setq child (org-index--parse-list-item))
                  child
                  (> (cdr (assoc :indent child))
                     (cdr (assoc :indent parent))))

        (unless parent-is-comment
          ;; check, that we have a valid flag
          (unless (memq (cdr (assoc :sym child)) org-index--all-flags)
            (org-index--report-index-error "'%s' is not a valid flag" (cdr (assoc :text child))))

          ;; process flag with respect to current index-column
          (if (memq (cdr (assoc :sym child)) org-index--single-flags)
              ;; Check, that none of org-index--single-flags appears twice
              (if (assoc (cdr (assoc :sym child)) org-index--special-columns)
                  (org-index--report-index-error
                   "More than one column is marked with flag '%s'" (cdr (assoc :text child)))
                ;; add it to list
                (setq org-index--special-columns (cons (cons (cdr (assoc :sym child)) (cdr (assoc :sym parent)))
                                                       org-index--special-columns))))

          ;; all flags are stored in org-index--flagged-columns
          (let ((l (assoc (cdr (assoc :sym child)) org-index--flagged-columns))) ;; list of flag and columns, that carry this flag
            (unless l
              ;; no list of columns with this flag is present, create one
              (setq org-index--flagged-columns
                    (cons (cons (cdr (assoc :sym child)) nil)
                          org-index--flagged-columns))
              (setq l (car org-index--flagged-columns)))
            ;; prepend this column to list of columns with this flag
            (setcdr l (cons (cdr (assoc :sym parent)) (cdr l)))))))

    ;; check, that all needed flags have been specified
    (mapc (lambda (x)
            (unless (assoc x org-index--special-columns)
              (org-index--report-index-error "Required flag '%s' does not appear" (substring (symbol-name x) 1))))
          org-index--required-flags)))


(defun org-index--goto-list (name &optional required non-top)
  "Goto list NAME (maybe NON-TOP Level) in index node, err if REQUIRED list is not present."
  (goto-char org-index--point)

  ;; go to heading of node
  (while (not (org-at-heading-p)) (forward-line -1))
  (forward-line 1)

  ;; go to named list
  (while (and (not (let ((item (org-index--parse-list-item)))
                     (if item
                         (and (or non-top (= (cdr (assoc :indent item)) 0)) ;; accept only toplevel ?
                              (string= (cdr (assoc :text item)) name)) ;; with requested name
                       nil)))
              (not (org-at-table-p))
              (not (org-at-heading-p))
              (not (eobp)))
    (forward-line 1))

  (if (org-at-item-p)
      t
    (if required
        (org-index--report-index-error "Could not find required list '%s'" name)
      nil)))


(defun org-index--parse-list-item ()
  "Parse a list item into an assoc array (indent, checkbox, text, value)."

  ;; matche full list-item, maybe with checkbox and double-colon
  (if (looking-at org-list-full-item-re)

      ;; retrieve interesting parts of list item from match data
      (let (indent checkbox text value next-line)

        (setq indent
              (- (save-excursion (goto-char (match-beginning 1)) (current-column)) ; first column
                 (save-match-data (org-current-level)) ; indent-level
                 1))
        (setq checkbox (match-string 3))
        (setq text (match-string 4))
        (set (if text 'value 'text) (buffer-substring (match-end 0) (line-end-position))) ; regexp did not capture this

        ;; peek ahead, if item continues on next line
        (forward-line 1)
        (if (looking-at org-list-full-item-re)
            (forward-line -1) ; already at next item; go back
          (setq next-line (buffer-substring (line-beginning-position) (line-end-position))))
        
        ;; clean up strings
        (mapc (lambda (x)
                (if (stringp (symbol-value x))
                    (set x (org-trim (substring-no-properties (symbol-value x))))))
              '(text value next-line))

        (if next-line (setq text (concat text " " next-line))) ; append next line if
        
        (list (cons :indent indent) (cons :text text) (cons :value value) (cons :sym (intern text))))
    nil))


(defun org-index--create-missing-index (&rest reasons)
  "Create a new empty index table with detailed explanation.  Argument REASONS explains why."

  (org-index--ask-before-create-index "Cannot find your index table: "
                                      "new permanent" "."
                                      reasons)
  (org-index--create-index))


(defun org-index--report-index-error (&rest reasons)
  "Report an error (explained by REASONS) with the existing index and offer to create a valid one to compare with."

  (when org-index--buffer
    (pop-to-buffer-same-window org-index--buffer)
    (goto-char org-index--below-hline)
    (org-reveal t))
  (org-index--ask-before-create-index "The existing index contains this error: "
                                      "temporary" ", to compare with."
                                      reasons)
  (org-index--create-index t t))


(defun org-index--ask-before-create-index (explanation type for-what reasons)
                                                  ; checkdoc-params: (explanation type for-what reasons)
  "Ask the user before creating an index or throw error.  Arguments specify bits of issued message."
  (let (reason prompt)

    (setq reason (apply 'format reasons))

    (setq prompt (concat explanation reason "\n\n"
                         "However, this assistant can help you to create a "
                         type " index with detailed comments" for-what "\n\n"
                         "Do you want to proceed ?"))

    (unless (let ((max-mini-window-height 1.0))
              (y-or-n-p prompt))
      (error (concat explanation reason)))))


(defun org-index--create-index (&optional temporary compare)
  "Create a new empty index table with detailed explanation.
specify flag TEMPORARY for th new table temporary, maybe COMPARE it with existing index."
  (let (buffer
        title
        firstref
        id)

    (if temporary
        (let ((file-name (concat temporary-file-directory "org-index--example-index.org"))
              (buffer-name "*org-index-example-index*"))
          (setq buffer (get-buffer-create buffer-name))
          (with-current-buffer buffer
            ;; but it needs a file for its index to be found
            (unless (string= (buffer-file-name) file-name)
              (set-visited-file-name file-name))
            (rename-buffer buffer-name) ; name is change by line above

            (erase-buffer)
            (org-mode)))

      (setq buffer (get-buffer (org-completing-read "Please choose the buffer, where the new node for the index table should be created; the new node will be inserted at its end.\n\nBuffer: " (mapcar 'buffer-name (org-buffer-list))))))

    (setq title (read-from-minibuffer "Please enter the title of the index node: "))

    (while (progn
             (setq firstref (read-from-minibuffer "Please enter your first reference-number. This is a number preceeded by some non-digit chars and optionally followed by some more non-digit chars, e.g. 'R1', '-1-' or '#1#' (and your initial number does not need to be '1'). The format of your reference-numbers only needs to make sense for yourself, so that you can spot it easily in your texts or write it on a piece of paper; it should however not already appear to frequently within your existing notes, to avoid too many false hits when searching.\n\nPlease choose: "))
             (let (desc)
               (when (string-match "[[:blank:]]" firstref)
                 (setq desc "Contains whitespace"))
               (when (string-match "[[:cntrl:]]" firstref)
                 (setq desc "Contains control characters"))
               (unless (string-match "^[^0-9]+[0-9]+[^0-9]*$" firstref)
                 ;; firstref not okay, report details
                 (setq desc
                       (cond ((string= firstref "") "is empty")
                             ((not (string-match "^[^0-9]+" firstref)) "starts with a digit")
                             ((not (string-match "^[^0-9]+[0-9]+" firstref)) "does not contain a number")
                             ((not (string-match "^[^0-9]+[0-9]+[^0-9]*$" firstref)) "contains more than one sequence of digits")

                             )))
               (if desc
                   (progn
                     (read-from-minibuffer (format "Your input '%s' does not meet the requirements because it %s.\nPlease hit RET and try again: " firstref desc))
                     t)
                 nil))))

    (with-current-buffer buffer
      (goto-char (point-max))
      (insert (format "* %s %s\n" firstref title))
      (if temporary
          (insert "
  Below you find your temporary index table, which WILL NOT LAST LONGER
  THAN YOUR CURRENT EMACS SESSION.
")
        (insert "
  Below you find your initial index table, which will grow over time.
"))
      (insert "
  You may start using it by adding some lines. Just move to
  another heading, invoke `org-index' and choose the command
  'add'.  After adding a few nodes, try the command 'occur'
  to search among them.

  To gain further insight you may invoke the subcommand 'help', or
  read the description of `org-index'.

  Within the index table below, dhe sequence of columns does not
  matter. You may reorder them in any way you please. Columns are
  found by their heading. You may also add your own columns,
  which should start with a dot (e.g. '.custom').

  Following this explanations you will find the item-list
  `columns-and-flags', which influences the behaviour of
  `org-index'. See the explanations which are part of this list.

  This node needs not be a top level node; its name is completely
  at your choice; it is found through its ID only.
")
      (unless temporary
        (insert "
  Remark: These lines of explanation can be removed at any time.
"))

      (setq id (org-id-get-create))
      (insert (format "
%s


  | ref |  category | keywords | count | last-accessed | created | id |
  |     |           |          |       |               |         | <4>  |
  |-----+-----------+----------+-------+---------------+---------+------|
  | %s  |           | %s       |       |               | %s      | %s   |

"
                      org-index--sample-flags
                      firstref
                      "This node"
                      (with-temp-buffer (org-insert-time-stamp nil nil t))
                      id))

      ;; make sure, that node can be found
      (org-id-add-location id (buffer-file-name))
      (setq buffer-save-without-query t)
      (basic-save-buffer)

      (while (not (org-at-table-p)) (forward-line -1))
      (unless buffer-read-only (org-table-align))
      (while (not (org-at-heading-p)) (forward-line -1))

      ;; read back some info about new index
      (let ((org-index-id id))
	(org-index--verify-id))

      ;; remember at least for this session
      (setq org-index-id id)

      ;; present results to user
      (if temporary
          (progn
            ;; Present existing and temporary index together
            (when compare
              (pop-to-buffer-same-window org-index--buffer)
              (goto-char org-index--point)
              (org-index--unfold-buffer)
              (delete-other-windows)
              (select-window (split-window-vertically)))
            ;; show new index
            (pop-to-buffer-same-window buffer)
            (org-id-goto id)
            (org-index--unfold-buffer)
            (if compare
                (error "Please compare your existing index (upper window) and a temporary new one (lower window) to fix your index")
              (message "This is your new temporary index.")))
        (progn
          ;; Only show the new index
          (pop-to-buffer-same-window buffer)
          (delete-other-windows)
          (org-id-goto id)
          (org-index--unfold-buffer)
          (if (y-or-n-p "This is your new index table.  It is already set for this Emacs session, so you may try it out.  Do you want to save its id to make it available for future Emacs sessions too ? ")
              (progn
                (customize-save-variable 'org-index-id id)
                (error "Saved org-index-id '%s' to %s" id (or custom-file
							      user-init-file)))
            (let (sq)
              (setq sq (format "(setq org-index-id \"%s\")" id))
              (kill-new sq)
              (error "Did not make the id of this new index permanent; you may want to put\n\n   %s\n\ninto your own initialization; it is copied already, just yank it" sq))))))))


(defun org-index--unfold-buffer ()
  "Helper function to unfold buffer."
  (org-show-context)
  (org-show-subtree)
  (recenter 1)
  (save-excursion
    (org-back-to-heading)
    (forward-line) ;; on property drawer
    (org-cycle)
    (org-index--goto-list "columns-and-flags")
    (org-cycle)))


(defun org-index--update-line (&optional ref-or-id)
  "Update columns count and last-accessed in line REF-OR-ID."

  (let ((newcount 0)
        initial)

    (with-current-buffer org-index--buffer
      (unless buffer-read-only

        ;; search reference or id, if given (or assume, that we are already positioned right)
        (when ref-or-id
          (setq initial (point))
          (goto-char org-index--below-hline)
          (while (and (org-at-table-p)
                      (not (or (string= ref-or-id (org-index--get-or-set-field 'ref))
                               (string= ref-or-id (org-index--get-or-set-field 'id)))))
            (forward-line)))

        (if (not (org-at-table-p))
            (error "Did not find reference or id '%s'" ref-or-id)
          (org-index--update-current-line))

        (if initial (goto-char initial))))))


(defun org-index--update-current-line ()
  "Update current lines columns count and last-accessed."
  (let (newcount (count-field (org-index--get-or-set-field 'count)))

    ;; update count field only if number or empty
    (when (or (not count-field)
              (string-match "^[0-9]+$" count-field))
      (setq newcount (+ 1 (string-to-number (or count-field "0"))))
      (org-index--get-or-set-field 'count
                            (number-to-string newcount)))

    ;; update timestamp
    (org-table-goto-column (org-index--column-num 'last-accessed))
    (org-table-blank-field)
    (org-insert-time-stamp nil t t)

    ;; move line according to new content
    (org-index--promote-current-line)
    (org-index--align-and-fontify-current-line)))


(defun org-index--align-and-fontify-current-line ()
  "Make current line blend well among others."
  (let ((line (substring-no-properties (delete-and-extract-region (line-beginning-position) (line-end-position)))))
    ;; create minimum table with fixed-width columns to align and fontiry new line
    (insert (with-temp-buffer
              (org-set-font-lock-defaults)
              (insert org-index--headings-visible)
              (goto-char (point-min))
              ;; fill columns, so that aligning cannot shrink them
              (search-forward "|")
              (replace-string " " "." nil (point) (line-end-position))
              (replace-string ".|." " | " nil (line-beginning-position) (line-end-position))
              (replace-string "|." "| " nil (line-beginning-position) (line-end-position))
              (goto-char (point-max))
              (insert line)
              (forward-line 0)
              (org-table-align)
              (font-lock-fontify-region (point-min) (point-max))
              (goto-char (point-max))
              (forward-line -1)
              (buffer-substring (line-beginning-position) (line-end-position))))))


(defun org-index--promote-current-line ()
  "Move current line up in table according to changed sort fields."
  (let (begin end key
        (to-skip 0))

    (forward-line 0) ; stay at beginning of line

    (setq key (org-index--get-sort-key))
    (setq begin (point))
    (setq end (line-beginning-position 2))

    (forward-line -1)
    (while (and (org-at-table-p)
                (not (org-at-table-hline-p))
                (string< (org-index--get-sort-key) key))

      (incf to-skip)
      (forward-line -1))
    (forward-line 1)

    ;; insert line at new position
    (when (> to-skip 0)
      (insert (delete-and-extract-region begin end))
      (forward-line -1))))


(defun org-index--get-sort-key (&optional sort with-ref)
  "Get value for sorting from column SORT, optional WITH-REF."
  (let (ref
        ref-field
        key)

    (unless sort (setq sort org-index--last-sort)) ; use default value

    (when (or with-ref
              (eq sort 'ref))
      ;; get reference with leading zeroes, so it can be
      ;; sorted as text
      (setq ref-field (org-index--get-or-set-field 'ref))
      (string-match org-index--ref-regex ref-field)
      (setq ref (format
                 "%06d"
                 (string-to-number
                  (or (match-string 1 ref-field)
                      "0")))))

    (setq key
          (cond
           ((eq sort 'count)
            (format "%08d" (string-to-number (or (org-index--get-or-set-field 'count) ""))))
           ((eq sort 'ref)
            ref)
           ((eq sort 'id)
            (org-index--get-or-set-field sort))
           ((eq sort 'last-accessed)
            (org-index--get-or-set-field sort))
           ((eq sort 'created)
            (org-index--get-or-set-field sort))
           (t (error "This is a bug: unmatched case '%s'" sort))))

    (if with-ref (setq key (concat key ref)))

    key))


(defun org-index--get-or-set-field (key &optional value)
  "Retrieve field KEY from index table or set it to VALUE."
  (let (field)
    (save-excursion
      (setq field (org-trim (org-table-get-field (cdr (assoc key org-index--columns)) value)))
      (if (string= field "") (setq field nil))

      (org-no-properties field))))


(defun org-index--column-num (key)
  "Return number of column KEY."
  (if (numberp key)
      key
    (cdr (assoc key org-index--columns))))


(defun org-index--special-column (key)
  "Return column (not a number) for special column KEY."
  (cdr (assoc key org-index--special-columns)))


(defun org-index--flag-p (flag column)
  "Check if COLUMN has FLAG set."
  (unless (memq flag org-index--all-flags)
    (error (format "Internal error: unknown flag %s" (symbol-name flag))))
  (memq column (assoc flag org-index--flagged-columns)))


(defun org-index--make-guarded-search (ref &optional dont-quote)
  "Make robust search string from REF; DONT-QUOTE it, if requested."
  (concat "\\_<" (if dont-quote ref (regexp-quote ref)) "\\_>"))


(defun org-index--do-statistics ()
  "Compute statistics about index table."
  (let ((total 0)
        ref-field ref min max message)


    ;; go through table and remove all refs, that we see
    (goto-char org-index--below-hline)
    (while (org-at-table-p)

      ;; get ref-field and number
      (setq ref-field (org-index--get-or-set-field 'ref))
      (if (and ref-field
               (string-match org-index--ref-regex ref-field))
          (setq ref (string-to-number (match-string 1 ref-field))))

      ;; record min and max
      (if (or (not min) (< ref min)) (setq min ref))
      (if (or (not max) (> ref max)) (setq max ref))

      ;; count
      (setq total (1+ total))

      (forward-line))

    (setq message (format "First reference is %s, last %s; %d values in between, %d of them are used (%d percent)"
                               (format org-index--ref-format min)
                               (format org-index--ref-format max)
                               (1+ (- max min))
                               total
                               (truncate (* 100 (/ (float total) (1+ (- max min)))))
                                                              
))

    (goto-char org-index--below-hline)
    message))


(defun org-index--do-add-or-update ()
  "For current node or current line in index, add a new line to index table or update existing."

  (let* (id ref args yank ref-and-yank)

    ;; do the same things from within index and from outside
    (if org-index--within-node

        (progn
          (unless (org-at-table-p)
            (error "Within index node but not on table"))

          (setq id (org-index--get-or-set-field 'id))
          (setq ref (org-index--get-or-set-field 'ref))
          (setq args (org-index--collect-values-for-add-update-remote id))
          (org-index--write-fields-for-add-update args)
          (setq yank (org-index--get-or-set-field (org-index--special-column 'yank-after-add)))
           
          (cons (format "Updated index line %s" ref) yank))

      (unless (org-at-heading-p)
        (error "Not at headline"))

      (setq id (org-id-get-create))
      (setq ref (org-index--on 'id id (org-index--get-or-set-field 'ref)))
      (setq args (org-index--collect-values-for-add-update id ref))

      (if ref
          ;; already have a ref, find it in index and update fields
          (let ((kvs args)
                found-and-message)

            (org-index--on
                'ref ref
                (org-index--write-fields-for-add-update args)
                (setq yank (org-index--get-or-set-field (org-index--special-column 'yank-after-add))))
           
            (cons (format "Updated index line %s" ref) yank))

        ;; no ref here, create new line in index
        (setq ref-and-yank (apply 'org-index--do-new-line args))

        (cons (format "Added index line %s" (car ref-and-yank)) (concat (cdr ref-and-yank) " "))))))


(defun org-index--check-ids ()
  "Check, that ids really point to a node."

  (let ((lines 0)
        id ids marker)
    
    (goto-char org-index--below-hline)

    (catch 'problem
      (while (org-at-table-p)

        (when (setq id (org-index--get-or-set-field 'id))

          ;; check for double ids
          (when (member id ids)
            (org-table-goto-column (org-index--column-num 'id))
            (throw 'problem "This id appears twice in index; please use command 'maintain' to check for duplicate ids"))
          (incf lines)
          (setq ids (cons id ids))

          ;; check, if id is valid
          (setq marker (org-id-find id t))
          (unless marker
            (org-table-goto-column (org-index--column-num 'id))
            (throw 'problem "This id cannot be found")))

        (forward-line))

      (goto-char org-index--below-hline)
      nil)))

  
(defun org-index--update-all-lines ()
  "Update all lines of index at once."

  (let ((lines 0)
        id ref kvs)
    
    ;; check for double ids
    (or
     (org-index--check-ids)

     (progn
       (goto-char org-index--below-hline)
       (while (org-at-table-p)
         
         ;; update single line
         (when (setq id (org-index--get-or-set-field 'id))
           (setq ref (org-index--get-or-set-field 'ref))
           (setq kvs (org-index--collect-values-for-add-update-remote id))
           (org-index--write-fields-for-add-update kvs)
           (incf lines))
         (forward-line))

       (goto-char org-index--below-hline)
       (org-table-align)
       (format "Updated %d lines" lines)))))


(defun org-index--collect-values-for-add-update (id &optional silent category)
  "Collect values for adding or updating line specified by ID, do not ask if SILENT, use CATEGORY, if given."
  
  (let ((args (list 'ref t 'id id))
        content)
    
    (dolist (col-num org-index--columns)
    
      (setq content "")
    
      (if (eq (car col-num) 'keywords)
          (setq content (nth 4 (org-heading-components))))
    
      (if (eq (car col-num) 'category)
          (setq content (or category org-index--category-before)))

      (if (eq (car col-num) 'level)
          (setq content (number-to-string (org-outline-level))))
    
      ;; Shift ref and timestamp ?
      (if (org-index--flag-p 'shift-ref-and-date-on-add (car col-num))
          (dotimes (i 2)
            (if (or (string-match (concat "^\\s-*" org-index--ref-regex) content)
                    (string-match (concat org-ts-regexp-both) content))
                (setq content (substring content (match-end 0))))))
    
      (if (and (not silent)    ; do not edit, if heading has already been added
               (org-index--flag-p 'edit-on-add (car col-num)))
          (setq content (read-from-minibuffer
                         (format "Edit text for column '%s': " (symbol-name (car col-num)))
                         content)))
    
      (if (not (string= content ""))
          (setq args (append (list (car col-num) content) args))))
    args))


(defun org-index--collect-values-for-add-update-remote (id)
  "Wrap `org-index--collect-values-for-add-update' by prior moving to remote node identified by ID."
  
  (let (marker point args)

    (setq marker (org-id-find id t))
    ;; enter buffer and collect information
    (with-current-buffer (marker-buffer marker)
      (setq point (point))
      (goto-char marker)
      (setq args (org-index--collect-values-for-add-update id t (org-get-category (point) t)))
      (goto-char point))

    args))


(defun org-index--write-fields-for-add-update (kvs)
  "Update current line with values from KVS (keys-values)."
  (while kvs
    (unless (eq (car kvs) 'ref)
      (org-index--get-or-set-field (car kvs) (org-trim (cadr kvs))))
    (setq kvs (cddr kvs))))


(defun org-index--do-delete ()
  "Perform command delete."

  (unless (org-at-heading-p)
    (error "Not at headline"))

  (let* ((id (org-entry-get (point) "ID"))
         (ref (org-index--ref-from-id id)))

    ;; maybe delete from heading
    (if ref
        (save-excursion
          (end-of-line)
          (let ((end (point)))
            (beginning-of-line)
            (when (search-forward ref end t)
              (delete-char (- (length ref)))
              (just-one-space)))))

    ;; delete from index table
    (if  (org-index--delete-line id)
        (format "Deleted index line %s" ref)
      (format "Did not find id %s in index" id))))


(defun org-index--go (&optional column value)
  "Position cursor on index line where COLUMN equals VALUE.
Return t or nil, leave point on line or at top of table, needs to be in buffer initially."
  (let (found text)

    (unless (eq (current-buffer) org-index--buffer)
      (error "This is a bug: Not in index buffer"))

    ;; loop over lines
    (goto-char org-index--below-hline)
    (if column
        (progn
          (forward-line -1)
          (while (and (not found)
                      (forward-line)
                      (org-at-table-p))
            (setq found (string= value (org-index--get-or-set-field column)))))
      (setq found t))

    ;; return value
    (if found
        t
      (goto-char org-index--below-hline)
      nil)))


(defun org-index--do-head (ref id &optional other)
  "Perform command head: Find node with REF or ID and present it.
If OTHER in separate window."
  
  (setq org-index--last-ref ref)

  (let (message marker)

    (setq marker (org-id-find id t))

    (if marker
        (progn
          (org-index--update-line id)
          (let (cb)
            (if other
                (progn
                  (setq cb (current-buffer))
                  (pop-to-buffer (marker-buffer marker)))
              (pop-to-buffer-same-window (marker-buffer marker)))
              
            (goto-char marker)
            (org-reveal t)
            (org-show-entry)
            (recenter))
          (setq message (format "Found headline %s" ref)))
      (setq message (format "Did not find headline %s" ref)))))


(defun org-index--do-occur ()
  "Perform command occur."
  (let ((word "") ; last word to search for growing and shrinking on keystrokes
        (prompt "Search for: ")
        (lines-wanted (window-body-height))
        (lines-found 0)                      ; number of lines found
        words                                ; list words that should match
        occur-buffer
        begin ; position of first line
        narrow                         ; start of narrowed buffer
        help-text                      ; cons with help text short and long
        key-help                       ; for keys with special function
        search-text                    ; description of text to search for
        done                           ; true, if loop is done
        in-c-backspace                 ; true, while processing C-backspace
        show-headings                  ; true, if headings should be shown
        help-overlay                   ; Overlay with help text
        last-point                     ; Last position before end of search
        key                            ; input from user
        key-sequence)                  ; as a sequence

    
    ;; make and show buffer
    (if (get-buffer org-index--occur-buffer-name)
        (kill-buffer org-index--occur-buffer-name))
    (setq occur-buffer (make-indirect-buffer org-index--buffer org-index--occur-buffer-name))
    (pop-to-buffer-same-window occur-buffer)
    ;; avoid modifying direct buffer
    (setq buffer-read-only t)
    (toggle-truncate-lines 1)
    (setq font-lock-keywords-case-fold-search t)
    (setq case-fold-search t)

    ;; reset stack and overlays
    (setq org-index--occur-stack nil)
    (setq org-index--occur-tail-overlay nil)
    
    ;; narrow to table rows and one line before
    (goto-char (marker-position org-index--below-hline))
    (forward-line 0)
    (setq begin (point))
    (forward-line -1)
    (setq narrow (point))
    (while (org-at-table-p)
      (forward-line))
    (narrow-to-region narrow (point))
    (goto-char (point-min))
    (forward-line)

    ;; initialize help text
    (setq help-text (cons
                     "Incremental occur; `?' toggles help and headlines.\n"
                     (concat
                      (org-index--wrap
                       (concat
                        "Normal keys add to search word; <space> or <comma> start additional word; <backspace> erases last char, <C-backspace> last word; <return> jumps to heading, <tab> jumps to heading in other window; all other keys end search.\n"))
                      org-index--headings)))
    
    ;; insert overlays for help text and to cover unsearched lines
    (setq help-overlay (make-overlay (point-min) begin))
    (overlay-put help-overlay 'display (car help-text))
    (overlay-put help-overlay 'face 'org-agenda-dimmed-todo-face)
    (setq org-index--occur-tail-overlay (make-overlay (point-max) (point-max)))
    (overlay-put org-index--occur-tail-overlay 'invisible t)

    (while (not done)

      (if in-c-backspace
          (setq key "<backspace>")
        (setq search-text (mapconcat 'identity (reverse (cons word words)) ","))
        ;; read key
        (setq key-sequence
              (vector (read-key
                       (format "%s%s%s"
                               prompt
                               search-text
                               (if (string= search-text "") "" " ")))))
        (setq key (key-description key-sequence)))

      (cond


       ((string= key "<C-backspace>")
        (setq in-c-backspace t))


       ((member key (list "<backspace>" "DEL"))   ; erase last char

        (if (= (length word) 0)

            ;; nothing more to delete from current word; try next
            (progn
              (setq word (car words))
              (setq words (cdr words))
              (setq in-c-backspace nil))

          ;; unhighlight longer match
          (unhighlight-regexp (regexp-quote word))

          ;; some chars are left; shorten word
          (setq word (substring word 0 -1))
          (when (= (length word) 0) ; when nothing left, use next word from list
            (setq word (car words))
            (setq words (cdr words))
            (setq in-c-backspace nil))

          ;; free top list of overlays and remove list
          (setq lines-found (or (org-index--unhide) lines-wanted))
          (move-overlay org-index--occur-tail-overlay
                        (if org-index--occur-stack (cdr (assoc :end-of-visible (car org-index--occur-stack)))
                          (point-max))
                        (point-max))
        
                
          ;; highlight shorter word
          (unless (= (length word) 0)
            (highlight-regexp (regexp-quote word) 'isearch))

          ;; make sure, point is still visible
          (goto-char begin)))


       ((member key (list "SPC" ",")) ; space or comma: enter an additional search word

        ;; push current word and clear, no need to change display
        (setq words (cons word words))
        (setq word ""))


       ((string= key "?") ; question mark: toggle display of headlines and help
        (setq help-text (cons (cdr help-text) (car help-text)))
        (overlay-put help-overlay 'display (car help-text)))

       ((and (= (length key) 1)
             (aref printable-chars (elt key 0))) ; any printable char: add to current search word

        ;; unhighlight short word
        (unless (= (length word) 0)
          (unhighlight-regexp (regexp-quote word)))

        ;; add to word
        (setq word (concat word key))
                
        ;; make overlays to hide lines, that do not match longer word any more
        (goto-char begin)
        (setq lines-found (org-index--hide-with-overlays (cons word words) lines-wanted))
        (move-overlay org-index--occur-tail-overlay
                      (if org-index--occur-stack (cdr (assoc :end-of-visible (car org-index--occur-stack)))
                        (point-max))
                      (point-max))
        
        (goto-char begin)
                
        ;; highlight longer word
        (highlight-regexp (regexp-quote word) 'isearch)

        ;; make sure, point is on a visible line
        (line-move -1 t)
        (line-move 1 t))

       ;; anything else terminates loop
       (t (setq done t))))

    ;; put back input event, that caused the loop to end
    (unless (string= key "C-g")
      (setq unread-command-events (listify-key-sequence key-sequence))
      (message key))
    
    ;; postprocessing
    (setq last-point (point))
    
    ;; For performance reasons do not show matching lines for rest of table. So no code here.
    
    ;; make permanent copy
    ;; copy visible lines
    (let ((lines-collected 0)
          keymap line all-lines end-of-head)

      (setq cursor-type t)
      (goto-char begin)

      ;; collect all visible lines
      (while (and (not (eobp))
                  (< lines-collected lines-wanted))
        ;; skip over invisible lines
        (while (and (invisible-p (point))
                    (not (eobp)))
          (goto-char (1+ (overlay-end (car (overlays-at (point)))))))
        (setq line (buffer-substring (line-beginning-position) (line-end-position)))
        (unless (string= line "")
          (incf lines-collected)
          (setq all-lines (cons (concat line
                                        "\n")
                                all-lines)))
        (forward-line 1))
        
      (kill-buffer org-index--occur-buffer-name) ; cannot keep this buffer; might become stale soon

      ;; create new buffer
      (setq occur-buffer (get-buffer-create org-index--occur-buffer-name))
      (pop-to-buffer-same-window occur-buffer)
      (insert org-index--headings)
      (setq end-of-head (point))

      ;; insert into new buffer
      (save-excursion
        (apply 'insert (reverse all-lines))
        (if (= lines-collected lines-wanted)
            (insert "\n(more lines omitted)\n")))
      
      (org-mode)
      (setq truncate-lines t)
      (if (org-at-table-p) (org-table-align))
      (font-lock-fontify-buffer)

      ;; prepare help text
      (setq org-index--occur-help-overlay (make-overlay (point-min) end-of-head))
      (setq org-index--occur-help-text
            (cons
             (org-index--wrap
              (concat "Search is done; `?' toggles help and headlines.\n"))
             (concat
              (org-index--wrap (format (concat "Search is done. "
                                               (if (< lines-collected lines-wanted)
                                                   " Showing all %d matches for "
                                                 " Showing one window of matches for ")
                                               "\"" search-text
                                               "\". <return> jumps to heading, <tab> jumps to heading in other window, <S-return> to matching line in index, <space> increments count.\n" )
                                       (length all-lines)))
              org-index--headings)))
      
      (overlay-put org-index--occur-help-overlay 'display (car org-index--occur-help-text))
      (overlay-put org-index--occur-help-overlay 'face 'org-agenda-dimmed-todo-face)

      ;; highlight words
      (setq case-fold-search t)
      (setq font-lock-keywords-case-fold-search t)
      (mapc (lambda (w) (unless (or (not w) (string= w "")) (highlight-regexp (regexp-quote w) 'isearch)))
            (cons word words))

      (setq buffer-read-only t)

      ;; install keyboard-shortcuts
      (setq keymap (make-sparse-keymap))
      (set-keymap-parent keymap org-mode-map)

      (mapc (lambda (x) (define-key keymap (kbd x)
                     (lambda () (interactive)
                       (message "%s" (org-index--occur-to-head)))))
            (list "<return>" "RET"))

      (define-key keymap (kbd "<tab>")
        (lambda () (interactive)
          (message (org-index--occur-to-head t))))
      
      (define-key keymap (kbd "SPC")
        (lambda () (interactive)
          ;; increment in index
          (let ((ref (org-index--get-or-set-field 'ref))
                count)
            (org-index--on
                'ref ref
                (setq count (+ 1 (string-to-number (org-index--get-or-set-field 'count))))
                (org-index--get-or-set-field 'count (number-to-string count))
                (org-index--promote-current-line)
                (org-index--align-and-fontify-current-line))
            ;; increment in this buffer
            (let ((inhibit-read-only t))
              (org-index--get-or-set-field 'count (number-to-string count)))
            (message "Incremented count to %d" count))))
      
      (define-key keymap (kbd "<S-return>")
        (lambda () (interactive)
          (org-index 'enter (org-index--get-or-set-field 'ref))))
      
      (define-key keymap (kbd "?")
        (lambda () (interactive)
          (setq-local org-index--occur-help-text (cons (cdr org-index--occur-help-text) (car org-index--occur-help-text)))
          (overlay-put org-index--occur-help-overlay 'display (car org-index--occur-help-text))))
    
      (use-local-map keymap))))


(defun org-index--wrap (text)
  "Wrap TEXT at fill column."
  (with-temp-buffer
    (insert text)
    (fill-region (point-min) (point-max) nil t)
    (buffer-string)))


(defun org-index--occur-to-head (&optional other)
  "Helper for `org-index--occur', find heading with ref or id; if OTHER, in other window."
  (let ((ref (org-index--get-or-set-field 'ref))
        (id (org-index--get-or-set-field 'id)))
    (if id
        (org-index--do-head ref id other)
      (message "Current line has no id."))))


(defun org-index--hide-with-overlays (words lines-wanted)
  "Hide text that is currently visible and does not match WORDS by creating overlays; leave LINES-WANTED lines visible."
  (let ((lines-found 0)
        (end-of-visible (point))
        overlay overlays start matched)

    ;; main loop
    (while (and (not (eobp))
                (< lines-found lines-wanted))

      ;; skip invisible lines
      (while (and (not (eobp))
                  (and
                   (invisible-p (point))
                   (< (point) (overlay-start org-index--occur-tail-overlay))))
        (goto-char (overlay-end (car (overlays-at (point))))))

      ;; find stretch of lines, that are currently visible but should be invisible now
      (setq matched nil)
      (setq start (point))
      (while (and (not (eobp))
                  (not
                   (and
                    (invisible-p (point))
                    (< (point) (overlay-start org-index--occur-tail-overlay))))
                  (not (and (org-index--test-words words)
                            (setq matched t)))) ; for its side effect
        (forward-line 1))

      ;; create overlay to hide this stretch
      (when (< start (point))           ; avoid creating an empty overlay
        (setq overlay (make-overlay start (point)))
        (overlay-put overlay 'invisible t)
        (setq overlays (cons overlay overlays)))

      ;; skip and count line, that matched
      (when matched
        (forward-line 1)
        (setq end-of-visible (point))
        (incf lines-found)))
    
    ;; put new list on top of stack
    (setq org-index--occur-stack
          (cons (list (cons :overlays overlays)
                      (cons :end-of-visible end-of-visible)
                      (cons :lines lines-found))
                org-index--occur-stack))

    lines-found))


(defun org-index--unhide ()
  "Unhide text that does has been hidden by `org-index--hide-with-overlays'."
  (when org-index--occur-stack
    ;; delete overlays and make visible again
    (mapc (lambda (y)
            (delete-overlay y))
          (cdr (assoc :overlays (car org-index--occur-stack))))
          ;; remove from stack
    (setq org-index--occur-stack (cdr org-index--occur-stack))
    ;; return number of lines, that are now visible
    (if org-index--occur-stack (cdr (assoc :lines (car org-index--occur-stack))))))


(defun org-index--test-words (words)
  "Test current line for match against WORDS."
  (let (line)
    (setq line (downcase (buffer-substring (line-beginning-position) (line-beginning-position 2))))
    (catch 'not-found
      (dolist (w words)
        (or (search w line)
            (throw 'not-found nil)))
      t)))


(defun org-index--create-new-line ()
  "Do the common work for `org-index-new-line' and `org-index'."

  (let (new)

    ;; construct new reference
    (unless new
      (setq new (format "%s%d%s" org-index--head (1+ org-index--maxref) org-index--tail)))

    ;; insert ref or id as last or first line, depending on sort-column
    (goto-char org-index--below-hline)
    (if (eq (org-index--special-column 'sort) 'count)
        (progn
          (while (org-at-table-p)
            (forward-line))
          (forward-line -1)
          (org-table-insert-row t))
      (org-table-insert-row))

    ;; insert some of the standard values
    (org-table-goto-column (org-index--column-num 'created))
    (org-insert-time-stamp nil nil t)
    (org-table-goto-column (org-index--column-num 'count))
    (insert "1")

    new))


(defun org-index--sort-silent ()
  "Sort index for default column to remove any effects of temporary sorting."
  (save-excursion
    (org-index--verify-id)
    (org-index--parse-table)
    (org-index--on nil nil
      (org-index--do-sort-index (org-index--special-column 'sort))
      (org-table-align)
      (remove-hook 'before-save-hook 'org-index--sort-silent))))


(defun org-index--copy-visible (beg end)
  "Copy the visible parts of the region without adding it to kill-ring; copy of `org-copy-visible'"
  (let (snippets s)
    (save-excursion
      (save-restriction
	(narrow-to-region beg end)
	(setq s (goto-char (point-min)))
	(while (not (= (point) (point-max)))
	  (goto-char (org-find-invisible))
	  (push (buffer-substring s (point)) snippets)
	  (setq s (goto-char (org-find-visible))))))
    (apply 'concat (nreverse snippets))))


(provide 'org-index)

;; Local Variables:
;; fill-column: 75
;; comment-column: 50
;; End:

;;; org-index.el ends here
