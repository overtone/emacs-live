;;; org-contacts.el --- Contacts management

;; Copyright (C) 2010-2013 Julien Danjou <julien@danjou.info>

;; Author: Julien Danjou <julien@danjou.info>
;; Keywords: outlines, hypermedia, calendar
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains the code for managing your contacts into Org-mode.

;; To enter new contacts, you can use `org-capture' and a template just like
;; this:

;;         ("c" "Contacts" entry (file "~/Org/contacts.org")
;;          "* %(org-contacts-template-name)
;; :PROPERTIES:
;; :EMAIL: %(org-contacts-template-email)
;; :END:")))
;;
;;; Code:

(eval-when-compile
  (require 'cl))

(require 'org)
(require 'gnus-util)
(require 'gnus-art)
(require 'mail-utils)
(require 'org-agenda)
(require 'org-capture)

(defgroup org-contacts nil
  "Options about contacts management."
  :group 'org)

(defcustom org-contacts-files nil
  "List of Org files to use as contacts source.
When set to nil, all your Org files will be used."
  :type '(repeat file)
  :group 'org-contacts)

(defcustom org-contacts-email-property "EMAIL"
  "Name of the property for contact email address."
  :type 'string
  :group 'org-contacts)

(defcustom org-contacts-tel-property "PHONE"
  "Name of the property for contact phone number."
  :type 'string
  :group 'org-contacts)

(defcustom org-contacts-address-property "ADDRESS"
  "Name of the property for contact address."
  :type 'string
  :group 'org-contacts)

(defcustom org-contacts-birthday-property "BIRTHDAY"
  "Name of the property for contact birthday date."
  :type 'string
  :group 'org-contacts)

(defcustom org-contacts-note-property "NOTE"
  "Name of the property for contact note."
  :type 'string
  :group 'org-contacts)

(defcustom org-contacts-alias-property "ALIAS"
  "Name of the property for contact name alias."
  :type 'string
  :group 'org-contacts)


(defcustom org-contacts-birthday-format "Birthday: %l (%Y)"
  "Format of the anniversary agenda entry.
The following replacements are available:

  %h - Heading name
  %l - Link to the heading
  %y - Number of year
  %Y - Number of year (ordinal)"
  :type 'string
  :group 'org-contacts)

(defcustom org-contacts-last-read-mail-property "LAST_READ_MAIL"
  "Name of the property for contact last read email link storage."
  :type 'string
  :group 'org-contacts)

(defcustom org-contacts-icon-property "ICON"
  "Name of the property for contact icon."
  :type 'string
  :group 'org-contacts)

(defcustom org-contacts-nickname-property "NICKNAME"
  "Name of the property for IRC nickname match."
  :type 'string
  :group 'org-contacts)

(defcustom org-contacts-icon-size 32
  "Size of the contacts icons."
  :type 'string
  :group 'org-contacts)

(defcustom org-contacts-icon-use-gravatar (fboundp 'gravatar-retrieve)
  "Whether use Gravatar to fetch contact icons."
  :type 'boolean
  :group 'org-contacts)

(defcustom org-contacts-completion-ignore-case t
  "Ignore case when completing contacts."
  :type 'boolean
  :group 'org-contacts)

(defcustom org-contacts-group-prefix "+"
  "Group prefix."
  :type 'string
  :group 'org-contacts)

(defcustom org-contacts-matcher
  (mapconcat 'identity (list org-contacts-email-property
			     org-contacts-alias-property
			     org-contacts-tel-property
			     org-contacts-address-property
			     org-contacts-birthday-property)
			     "<>\"\"|")
  "Matching rule for finding heading that are contacts.
This can be a tag name, or a property check."
  :type 'string
  :group 'org-contacts)

(defcustom org-contacts-email-link-description-format "%s (%d)"
  "Format used to store links to email.
This overrides `org-email-link-description-format' if set."
  :group 'org-contacts
  :type 'string)

(defcustom org-contacts-vcard-file "contacts.vcf"
  "Default file for vcard export."
  :group 'org-contacts
  :type 'file)

(defcustom org-contacts-enable-completion t
  "Enable or not the completion in `message-mode' with `org-contacts'."
  :group 'org-contacts
  :type 'boolean)

;; Decalre external functions and variables
(declare-function org-reverse-string "org")
(declare-function diary-ordinal-suffix "ext:diary-lib")
(declare-function wl-summary-message-number "ext:wl-summary")
(declare-function wl-address-header-extract-address "ext:wl-address")
(declare-function wl-address-header-extract-realname "ext:wl-address")
(declare-function erc-buffer-list "ext:erc")
(declare-function erc-get-channel-user-list "ext:erc")
(declare-function google-maps-static-show "ext:google-maps-static")
(declare-function elmo-message-field "ext:elmo-pipe")
(declare-function std11-narrow-to-header "ext:std11")
(declare-function std11-fetch-field "ext:std11")

(defvar org-contacts-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "M" 'org-contacts-view-send-email)
    (define-key map "i" 'org-contacts-view-switch-to-irc-buffer)
    map)
  "The keymap used in `org-contacts' result list.")

(defvar org-contacts-db nil
  "Org Contacts database.")

(defvar org-contacts-last-update nil
  "Last time the Org Contacts database has been updated.")

(defun org-contacts-files ()
  "Return list of Org files to use for contact management."
  (or org-contacts-files (org-agenda-files t 'ifmode)))

(defun org-contacts-db-need-update-p ()
  "Determine whether `org-contacts-db' needs to be refreshed."
  (or (null org-contacts-last-update)
      (org-find-if (lambda (file)
		     (or (time-less-p org-contacts-last-update
				      (elt (file-attributes file) 5))))
		   (org-contacts-files))))

(defun org-contacts-db ()
  "Return the latest Org Contacts Database."
  (let* (todo-only
	 (contacts-matcher
	  (cdr (org-make-tags-matcher org-contacts-matcher)))
	 markers result)
    (when (org-contacts-db-need-update-p)
      (message "Update Org Contacts Database")
      (dolist (file (org-contacts-files))
	(org-check-agenda-file file)
	(with-current-buffer (org-get-agenda-file-buffer file)
	  (unless (eq major-mode 'org-mode)
	    (error "File %s is no in `org-mode'" file))
	  (org-scan-tags
	   '(add-to-list 'markers (set-marker (make-marker) (point)))
	   contacts-matcher
	   todo-only)))
      (dolist (marker markers result)
	(org-with-point-at marker
	  (add-to-list 'result
		       (list (org-get-heading t) marker (org-entry-properties marker 'all)))))
      (setf org-contacts-db result
	    org-contacts-last-update (current-time)))
    org-contacts-db))

(defun org-contacts-filter (&optional name-match tags-match)
  "Search for a contact maching NAME-MATCH and TAGS-MATCH.
If both match values are nil, return all contacts."
  (if (and (null name-match)
	   (null tags-match))
      (org-contacts-db)
    (loop for contact in (org-contacts-db)
	  if (or
	      (and name-match
		   (org-string-match-p name-match
				       (first contact)))
	      (and tags-match
		   (org-find-if (lambda (tag)
				  (org-string-match-p tags-match tag))
				(org-split-string
				 (or (cdr (assoc-string "ALLTAGS" (caddr contact))) "") ":"))))
	  collect contact)))

(when (not (fboundp 'completion-table-case-fold))
  ;; That function is new in Emacs 24...
  (defun completion-table-case-fold (table &optional dont-fold)
    (lambda (string pred action)
      (let ((completion-ignore-case (not dont-fold)))
	(complete-with-action action table string pred)))))

(defun org-contacts-try-completion-prefix (to-match collection &optional predicate)
  "Custom implementation of `try-completion'.
This version works only with list and alist and it looks at all
prefixes rather than just the beginning of the string."
  (loop with regexp = (concat "\\b" (regexp-quote to-match))
	with ret = nil
	with ret-start = nil
	with ret-end = nil

	for el in collection
	for string = (if (listp el) (car el) el)

	for start = (when (or (null predicate) (funcall predicate string))
		      (string-match regexp string))

	if start
	do (let ((end (match-end 0))
		 (len (length string)))
	     (if (= end len)
		 (return t)
	       (destructuring-bind (string start end)
		   (if (null ret)
		       (values string start end)
		     (org-contacts-common-substring
		      ret ret-start ret-end
		      string start end))
		 (setf ret string
		       ret-start start
		       ret-end end))))

	finally (return
		 (replace-regexp-in-string "\\`[ \t\n]*" "" ret))))

(defun org-contacts-compare-strings (s1 start1 end1 s2 start2 end2 &optional ignore-case)
  "Compare the contents of two strings, using `compare-strings'.

This function works like `compare-strings' excepted that it
returns a cons.
- The CAR is the number of characters that match at the beginning.
- The CDR is T is the two strings are the same and NIL otherwise."
  (let ((ret (compare-strings s1 start1 end1 s2 start2 end2 ignore-case)))
    (if (eq ret t)
	(cons (or end1 (length s1)) t)
      (cons (1- (abs ret)) nil))))

(defun org-contacts-common-substring (s1 start1 end1 s2 start2 end2)
  "Extract the common substring between S1 and S2.

This function extracts the common substring between S1 and S2 and
adjust the part that remains common.

START1 and END1 delimit the part in S1 that we know is common
between the two strings. This applies to START2 and END2 for S2.

This function returns a list whose contains:
- The common substring found.
- The new value of the start of the known inner substring.
- The new value of the end of the known inner substring."
  ;; Given two strings:
  ;; s1: "foo bar baz"
  ;; s2: "fooo bar baz"
  ;; and the inner substring is "bar"
  ;; then: start1 = 4, end1 = 6, start2 = 5, end2 = 7
  ;;
  ;; To find the common substring we will compare two substrings:
  ;; " oof" and " ooof" to find the beginning of the common substring.
  ;; " baz" and " baz" to find the end of the common substring.
  (let* ((len1 (length s1))
	 (start1 (or start1 0))
	 (end1 (or end1 len1))

	 (len2 (length s2))
	 (start2 (or start2 0))
	 (end2 (or end2 len2))

	 (new-start (car (org-contacts-compare-strings
			  (substring (org-reverse-string s1) (- len1 start1)) nil nil
			  (substring (org-reverse-string s2) (- len2 start2)) nil nil)))

	 (new-end (+ end1 (car (org-contacts-compare-strings
				(substring s1 end1) nil nil
				(substring s2 end2) nil nil)))))
    (list (substring s1 (- start1 new-start) new-end)
	  new-start
	  (+ new-start (- end1 start1)))))

(defun org-contacts-all-completions-prefix (to-match collection &optional predicate)
  "Custom version of `all-completions'.
This version works only with list and alist and it looks at all
prefixes rather than just the beginning of the string."
  (loop with regexp = (concat "\\b" (regexp-quote to-match))
	for el in collection
	for string = (if (listp el) (car el) el)
	for match? = (when (and (or (null predicate) (funcall predicate string)))
		       (string-match regexp string))
	if match?
	collect (progn
		  (let ((end (match-end 0)))
		    (org-no-properties string)
		    (when (< end (length string))
		      ;; Here we add a text property that will be used
		      ;; later to highlight the character right after
		      ;; the common part between each addresses.
		      ;; See `org-contacts-display-sort-function'.
		      (put-text-property end (1+ end) 'org-contacts-prefix 't string)))
		  string)))

(defun org-contacts-make-collection-prefix (collection)
  "Make a collection function from COLLECTION which will match on prefixes."
  (lexical-let ((collection collection))
    (lambda (string predicate flag)
      (cond ((eq flag nil)
	     (org-contacts-try-completion-prefix string collection predicate))
	    ((eq flag t)
	     ;; `org-contacts-all-completions-prefix' has already been
	     ;; used to compute `all-completions'.
	     collection)
	    ((eq flag 'lambda)
	     (org-contacts-test-completion-prefix string collection predicate))
	    ((and (listp flag) (eq (car flag) 'boundaries))
	     (destructuring-bind (to-ignore &rest suffix)
		 flag
	       (org-contacts-boundaries-prefix string collection predicate suffix)))
	    ((eq flag 'metadata)
	     (org-contacts-metadata-prefix string collection predicate))
	    (t nil			; operation unsupported
	       )))))

(defun org-contacts-display-sort-function (completions)
  "Sort function for contacts display."
  (mapcar (lambda (string)
	    (loop with len = (1- (length string))
		  for i upfrom 0 to len
		  if (memq 'org-contacts-prefix
			   (text-properties-at i string))
		  do (set-text-properties
		      i (1+ i)
		      (list 'font-lock-face
			    (if (char-equal (aref string i)
					    (string-to-char " "))
				;; Spaces can't be bold.
				'underline
			      'bold)) string)
		  else
		  do (set-text-properties i (1+ i) nil string)
		  finally (return string)))
	  completions))

(defun org-contacts-test-completion-prefix (string collection predicate)
  ;; Prevents `org-find-if' from redefining `predicate' and going into
  ;; an infinite loop.
  (lexical-let ((predicate predicate))
    (org-find-if (lambda (el)
		   (and (or (null predicate) (funcall predicate el))
			(string= string el)))
		 collection)))

(defun org-contacts-boundaries-prefix (string collection predicate suffix)
  (list* 'boundaries (completion-boundaries string collection predicate suffix)))

(defun org-contacts-metadata-prefix (string collection predicate)
  '(metadata .
	     ((display-sort-function . org-contacts-display-sort-function))))

(defun org-contacts-complete-group (start end string)
  "Complete text at START from a group.

A group FOO is composed of contacts with the tag FOO."
  (let* ((completion-ignore-case org-contacts-completion-ignore-case)
	 (group-completion-p (org-string-match-p
			      (concat "^" org-contacts-group-prefix) string)))
    (when group-completion-p
      (let ((completion-list
	     (all-completions
	      string
	      (mapcar (lambda (group)
			(propertize (concat org-contacts-group-prefix group)
				    'org-contacts-group group))
		      (org-uniquify
		       (loop for contact in (org-contacts-filter)
			     nconc (org-split-string
				    (or (cdr (assoc-string "ALLTAGS" (caddr contact))) "") ":")))))))
	(list start end
	      (if (= (length completion-list) 1)
		  ;; We've foudn the correct group, returns the address
		  (lexical-let ((tag (get-text-property 0 'org-contacts-group
							(car completion-list))))
		    (lambda (string pred &optional to-ignore)
		      (mapconcat 'identity
				 (loop for contact in (org-contacts-filter
						       nil
						       tag)
				       ;; The contact name is always the car of the assoc-list
				       ;; returned by `org-contacts-filter'.
				       for contact-name = (car contact)
				       ;; Grab the first email of the contact
				       for email = (car (split-string
							 (or
							  (cdr (assoc-string org-contacts-email-property
									     (caddr contact)))
							  "")))
				       ;; If the user has an email address, append USER <EMAIL>.
				       if email collect (org-contacts-format-email contact-name email))
				 ", ")))
		;; We haven't found the correct group
		(completion-table-case-fold completion-list
					    (not org-contacts-completion-ignore-case))))))))

(defun org-contacts-complete-name (start end string)
  "Complete text at START with a user name and email."
  (let* ((completion-ignore-case org-contacts-completion-ignore-case)
         (completion-list
	  (loop for contact in (org-contacts-filter)
		;; The contact name is always the car of the assoc-list
		;; returned by `org-contacts-filter'.
		for contact-name = (car contact)
		;; Build the list of the user email addresses.
		for email-list = (split-string (or
						(cdr (assoc-string org-contacts-email-property
								   (caddr contact))) ""))
		;; If the user has email addresses…
		if email-list
		;; … append a list of USER <EMAIL>.
		nconc (loop for email in email-list
			    collect (org-contacts-format-email contact-name email))))
	 (completion-list (org-contacts-all-completions-prefix
			   string
			   (org-uniquify completion-list))))
    (when completion-list
      (list start end
	    (org-contacts-make-collection-prefix completion-list)))))

(defun org-contacts-message-complete-function (&optional start)
  "Function used in `completion-at-point-functions' in `message-mode'."
  ;; Avoid to complete in `post-command-hook'.
  (when completion-in-region-mode
    (remove-hook 'post-command-hook #'completion-in-region--postch))
  (let ((mail-abbrev-mode-regexp
         "^\\(Resent-To\\|To\\|B?Cc\\|Reply-To\\|From\\|Mail-Followup-To\\|Mail-Copies-To\\|Disposition-Notification-To\\|Return-Receipt-To\\):"))
    (when (mail-abbrev-in-expansion-header-p)
      (lexical-let*
	  ((end (point))
	   (start (or start
		      (save-excursion
			(re-search-backward "\\(\\`\\|[\n:,]\\)[ \t]*")
			(goto-char (match-end 0))
			(point))))
	   (string (buffer-substring start end)))
	(or (org-contacts-complete-group start end string)
	    (org-contacts-complete-name start end string))))))

(defun org-contacts-gnus-get-name-email ()
  "Get name and email address from Gnus message."
  (if (gnus-alive-p)
      (gnus-with-article-headers
        (mail-extract-address-components
         (or (mail-fetch-field "From") "")))))

(defun org-contacts-gnus-article-from-get-marker ()
  "Return a marker for a contact based on From."
  (let* ((address (org-contacts-gnus-get-name-email))
         (name (car address))
         (email (cadr address)))
    (cadar (or (org-contacts-filter
                nil
                (concat org-contacts-email-property "={\\b" (regexp-quote email) "\\b}"))
               (when name
                 (org-contacts-filter
                  (concat "^" name "$")))))))

(defun org-contacts-gnus-article-from-goto ()
  "Go to contact in the From address of current Gnus message."
  (interactive)
  (let ((marker (org-contacts-gnus-article-from-get-marker)))
    (when marker
      (switch-to-buffer-other-window (marker-buffer marker))
      (goto-char marker)
      (when (eq major-mode 'org-mode)
        (org-show-context 'agenda)
        (save-excursion
          (and (outline-next-heading)
               ;; show the next heading
               (org-flag-heading nil)))))))

(org-no-warnings (defvar date)) ;; unprefixed, from calendar.el
(defun org-contacts-anniversaries (&optional field format)
  "Compute FIELD anniversary for each contact, returning FORMAT.
Default FIELD value is \"BIRTHDAY\".

Format is a string matching the following format specification:

  %h - Heading name
  %l - Link to the heading
  %y - Number of year
  %Y - Number of year (ordinal)"
  (let ((calendar-date-style 'american)
        (entry ""))
    (unless format (setq format org-contacts-birthday-format))
    (loop for contact in (org-contacts-filter)
          for anniv = (let ((anniv (cdr (assoc-string
                                         (or field org-contacts-birthday-property)
                                         (caddr contact)))))
                        (when anniv
                          (calendar-gregorian-from-absolute
                           (org-time-string-to-absolute anniv))))
          ;; Use `diary-anniversary' to compute anniversary.
          if (and anniv (apply 'diary-anniversary anniv))
          collect (format-spec format
                               `((?l . ,(org-with-point-at (cadr contact) (org-store-link nil)))
                                 (?h . ,(car contact))
                                 (?y . ,(- (calendar-extract-year date)
                                           (calendar-extract-year anniv)))
                                 (?Y . ,(let ((years (- (calendar-extract-year date)
                                                        (calendar-extract-year anniv))))
                                          (format "%d%s" years (diary-ordinal-suffix years)))))))))

(defun org-completing-read-date (prompt collection
                                        &optional predicate require-match initial-input
                                        hist def inherit-input-method)
  "Like `completing-read' but reads a date.
Only PROMPT and DEF are really used."
  (org-read-date nil nil nil prompt nil def))

(add-to-list 'org-property-set-functions-alist
             `(,org-contacts-birthday-property . org-completing-read-date))

(defun org-contacts-template-name (&optional return-value)
  "Try to return the contact name for a template.
If not found return RETURN-VALUE or something that would ask the user."
  (or (car (org-contacts-gnus-get-name-email))
      return-value
      "%^{Name}"))

(defun org-contacts-template-email (&optional return-value)
  "Try to return the contact email for a template.
If not found return RETURN-VALUE or something that would ask the user."
  (or (cadr (org-contacts-gnus-get-name-email))
      return-value
      (concat "%^{" org-contacts-email-property "}p")))

(defun org-contacts-gnus-store-last-mail ()
  "Store a link between mails and contacts.

This function should be called from `gnus-article-prepare-hook'."
  (let ((marker (org-contacts-gnus-article-from-get-marker)))
    (when marker
      (with-current-buffer (marker-buffer marker)
        (save-excursion
          (goto-char marker)
          (let* ((org-email-link-description-format (or org-contacts-email-link-description-format
                                                        org-email-link-description-format))
                 (link (gnus-with-article-buffer (org-store-link nil))))
            (org-set-property org-contacts-last-read-mail-property link)))))))

(defun org-contacts-icon-as-string ()
  "Return the contact icon as a string."
  (let ((image (org-contacts-get-icon)))
    (concat
     (propertize "-" 'display
                 (append
                  (if image
                      image
                    `'(space :width (,org-contacts-icon-size)))
                  '(:ascent center)))
     " ")))

;;;###autoload
(defun org-contacts (name)
  "Create agenda view for contacts matching NAME."
  (interactive (list (read-string "Name: ")))
  (let ((org-agenda-files (org-contacts-files))
        (org-agenda-skip-function
         (lambda () (org-agenda-skip-if nil `(notregexp ,name))))
        (org-agenda-prefix-format (propertize
				   "%(org-contacts-icon-as-string)% s%(org-contacts-irc-number-of-unread-messages) "
				   'keymap org-contacts-keymap))
        (org-agenda-overriding-header
         (or org-agenda-overriding-header
             (concat "List of contacts matching `" name "':"))))
    (setq org-agenda-skip-regexp name)
    (org-tags-view nil org-contacts-matcher)
    (with-current-buffer org-agenda-buffer-name
      (setq org-agenda-redo-command
            (list 'org-contacts name)))))

(defun org-contacts-completing-read (prompt
                                     &optional predicate
                                     initial-input hist def inherit-input-method)
  "Call `completing-read' with contacts name as collection."
  (org-completing-read
   prompt (org-contacts-filter) predicate t initial-input hist def inherit-input-method))

(defun org-contacts-format-name (name)
  "Trim any local formatting to get a bare NAME."
  ;; Remove radio targets characters
  (replace-regexp-in-string org-radio-target-regexp "\\1" name))

(defun org-contacts-format-email (name email)
  "Format an EMAIL address corresponding to NAME."
  (unless email
    (error "`email' cannot be nul"))
  (if name
      (concat (org-contacts-format-name name) " <" email ">")
    email))

(defun org-contacts-check-mail-address (mail)
  "Add MAIL address to contact at point if it does not have it."
  (let ((mails (org-entry-get (point) org-contacts-email-property)))
    (unless (member mail (split-string mails))
      (when (yes-or-no-p
             (format "Do you want to add this address to %s?" (org-get-heading t)))
        (org-set-property org-contacts-email-property (concat mails " " mail))))))

(defun org-contacts-gnus-check-mail-address ()
  "Check that contact has the current address recorded.
This function should be called from `gnus-article-prepare-hook'."
  (let ((marker (org-contacts-gnus-article-from-get-marker)))
    (when marker
      (org-with-point-at marker
        (org-contacts-check-mail-address (cadr (org-contacts-gnus-get-name-email)))))))

(defun org-contacts-gnus-insinuate ()
  "Add some hooks for Gnus user.
This adds `org-contacts-gnus-check-mail-address' and
`org-contacts-gnus-store-last-mail' to
`gnus-article-prepare-hook'.  It also adds a binding on `;' in
`gnus-summary-mode-map' to `org-contacts-gnus-article-from-goto'"
  (require 'gnus)
  (require 'gnus-art)
  (define-key gnus-summary-mode-map ";" 'org-contacts-gnus-article-from-goto)
  (add-hook 'gnus-article-prepare-hook 'org-contacts-gnus-check-mail-address)
  (add-hook 'gnus-article-prepare-hook 'org-contacts-gnus-store-last-mail))

(when (and org-contacts-enable-completion
	   (boundp 'completion-at-point-functions))
  (add-hook 'message-mode-hook
	    (lambda ()
	      (add-to-list 'completion-at-point-functions
			   'org-contacts-message-complete-function))))

(defun org-contacts-wl-get-from-header-content ()
  "Retrieve the content of the `From' header of an email.
Works from wl-summary-mode and mime-view-mode - that is while viewing email.
Depends on Wanderlust been loaded."
  (with-current-buffer (org-capture-get :original-buffer)
    (cond
     ((eq major-mode 'wl-summary-mode) (when (and (boundp 'wl-summary-buffer-elmo-folder)
						  wl-summary-buffer-elmo-folder)
                                         (elmo-message-field
                                          wl-summary-buffer-elmo-folder
                                          (wl-summary-message-number)
                                          'from)))
     ((eq major-mode 'mime-view-mode) (std11-narrow-to-header)
      (prog1
	  (std11-fetch-field "From")
	(widen))))))

(defun org-contacts-wl-get-name-email ()
  "Get name and email address from Wanderlust email.
See `org-contacts-wl-get-from-header-content' for limitations."
  (let ((from (org-contacts-wl-get-from-header-content)))
    (when from
      (list (wl-address-header-extract-realname from)
	    (wl-address-header-extract-address from)))))

(defun org-contacts-template-wl-name (&optional return-value)
  "Try to return the contact name for a template from wl.
If not found, return RETURN-VALUE or something that would ask the
user."
  (or (car (org-contacts-wl-get-name-email))
      return-value
      "%^{Name}"))

(defun org-contacts-template-wl-email (&optional return-value)
  "Try to return the contact email for a template from Wanderlust.
If not found return RETURN-VALUE or something that would ask the user."
  (or (cadr (org-contacts-wl-get-name-email))
      return-value
      (concat "%^{" org-contacts-email-property "}p")))

(defun org-contacts-view-send-email (&optional ask)
  "Send email to the contact at point.
If ASK is set, ask for the email address even if there's only one
address."
  (interactive "P")
  (let ((marker (org-get-at-bol 'org-hd-marker)))
    (org-with-point-at marker
      (let ((emails (org-entry-get (point) org-contacts-email-property)))
        (if emails
            (let ((email-list (split-string emails)))
              (if (and (= (length email-list) 1) (not ask))
                  (compose-mail (org-contacts-format-email
                                 (org-get-heading t) emails))
                (let ((email (completing-read "Send mail to which address: " email-list)))
                  (org-contacts-check-mail-address email)
                  (compose-mail (org-contacts-format-email (org-get-heading t) email)))))
          (error (format "This contact has no mail address set (no %s property)."
                         org-contacts-email-property)))))))

(defun org-contacts-get-icon (&optional pom)
  "Get icon for contact at POM."
  (setq pom (or pom (point)))
  (catch 'icon
    ;; Use `org-contacts-icon-property'
    (let ((image-data (org-entry-get pom org-contacts-icon-property)))
      (when image-data
        (throw 'icon
               (if (fboundp 'gnus-rescale-image)
                   (gnus-rescale-image (create-image image-data)
                                       (cons org-contacts-icon-size org-contacts-icon-size))
                 (create-image image-data)))))
    ;; Next, try Gravatar
    (when org-contacts-icon-use-gravatar
      (let* ((gravatar-size org-contacts-icon-size)
             (email-list (org-entry-get pom org-contacts-email-property))
             (gravatar
              (when email-list
                (loop for email in (split-string email-list)
                      for gravatar = (gravatar-retrieve-synchronously email)
                      if (and gravatar
                              (not (eq gravatar 'error)))
                      return gravatar))))
        (when gravatar (throw 'icon gravatar))))))

(defun org-contacts-irc-buffer (&optional pom)
  "Get the IRC buffer associated with the entry at POM."
  (setq pom (or pom (point)))
  (let ((nick (org-entry-get pom org-contacts-nickname-property)))
    (when nick
      (let ((buffer (get-buffer nick)))
        (when buffer
          (with-current-buffer buffer
            (when (eq major-mode 'erc-mode)
              buffer)))))))

(defun org-contacts-irc-number-of-unread-messages (&optional pom)
  "Return the number of unread messages for contact at POM."
  (when (boundp 'erc-modified-channels-alist)
    (let ((number (cadr (assoc (org-contacts-irc-buffer pom) erc-modified-channels-alist))))
      (if number
          (format (concat "%3d unread message" (if (> number 1) "s" " ") " ") number)
        (make-string 21 ? )))))

(defun org-contacts-view-switch-to-irc-buffer ()
  "Switch to the IRC buffer of the current contact if it has one."
  (interactive)
  (let ((marker (org-get-at-bol 'org-hd-marker)))
    (org-with-point-at marker
      (switch-to-buffer-other-window (org-contacts-irc-buffer)))))

(defun org-contacts-completing-read-nickname (prompt collection
                                                     &optional predicate require-match initial-input
                                                     hist def inherit-input-method)
  "Like `completing-read' but reads a nickname."
  (org-completing-read prompt (append collection (erc-nicknames-list)) predicate require-match
                       initial-input hist def inherit-input-method))

(defun erc-nicknames-list ()
  "Return all nicknames of all ERC buffers."
  (loop for buffer in (erc-buffer-list)
	nconc (with-current-buffer buffer
		(loop for user-entry in (mapcar 'car (erc-get-channel-user-list))
		      collect (elt user-entry 1)))))

(add-to-list 'org-property-set-functions-alist
             `(,org-contacts-nickname-property . org-contacts-completing-read-nickname))

(defun org-contacts-vcard-escape (str)
  "Escape ; , and \n in STR for the VCard format."
  ;; Thanks to this library for the regexp:
  ;; http://www.emacswiki.org/cgi-bin/wiki/bbdb-vcard-export.el
  (when str
    (replace-regexp-in-string
     "\n" "\\\\n"
     (replace-regexp-in-string "\\(;\\|,\\|\\\\\\)" "\\\\\\1" str))))

(defun org-contacts-vcard-encode-name (name)
  "Try to encode NAME as VCard's N property.
The N property expects

  FamilyName;GivenName;AdditionalNames;Prefix;Postfix.

Org-contacts does not specify how to encode the name.  So we try
to do our best."
  (concat (replace-regexp-in-string "\\(\\w+\\) \\(.*\\)" "\\2;\\1" name) ";;;"))

(defun org-contacts-vcard-format (contact)
  "Formats CONTACT in VCard 3.0 format."
  (let* ((properties (caddr contact))
	 (name (org-contacts-vcard-escape (car contact)))
	 (n (org-contacts-vcard-encode-name name))
	 (email (cdr (assoc-string org-contacts-email-property properties)))
	 (tel  (cdr (assoc-string org-contacts-tel-property properties)))
	 (note (cdr (assoc-string org-contacts-note-property properties)))
	 (bday (org-contacts-vcard-escape (cdr (assoc-string org-contacts-birthday-property properties))))
	 (addr (cdr (assoc-string org-contacts-address-property properties)))
	 (nick (org-contacts-vcard-escape (cdr (assoc-string org-contacts-nickname-property properties))))
	 (head (format "BEGIN:VCARD\nVERSION:3.0\nN:%s\nFN:%s\n" n name)))
    (concat head
	    (when email (progn
			  (setq emails-list (split-string email "[,;: ]+"))
			  (setq result "")
			  (while emails-list
			    (setq result (concat result  "EMAIL:" (car emails-list) "\n"))
			    (setq emails-list (cdr emails-list)))
			  result))
	    (when addr
	      (format "ADR:;;%s\n" (replace-regexp-in-string "\\, ?" ";" addr)))
	    (when tel (progn
			(setq phones-list (split-string tel "[,;: ]+"))
			(setq result "")
			(while phones-list
			  (setq result (concat result  "TEL:" (car phones-list) "\n"))
			  (setq phones-list (cdr phones-list)))
			result))
	    (when bday
	      (let ((cal-bday (calendar-gregorian-from-absolute (org-time-string-to-absolute bday))))
		(format "BDAY:%04d-%02d-%02d\n"
			(calendar-extract-year cal-bday)
			(calendar-extract-month cal-bday)
			(calendar-extract-day cal-bday))))
	    (when nick (format "NICKNAME:%s\n" nick))
	    (when note (format "NOTE:%s\n" note))
	    "END:VCARD\n\n")))

(defun org-contacts-export-as-vcard (&optional name file to-buffer)
  "Export all contacts matching NAME as VCard 3.0.
If TO-BUFFER is nil, the content is written to FILE or
`org-contacts-vcard-file'.  If TO-BUFFER is non-nil, the buffer
is created and the VCard is written into that buffer."
  (interactive) ; TODO ask for name?
  (let* ((filename (or file org-contacts-vcard-file))
	 (buffer (if to-buffer
		     (get-buffer-create to-buffer)
		   (find-file-noselect filename))))
    (message "Exporting...")
    (set-buffer buffer)
    (let ((inhibit-read-only t)) (erase-buffer))
    (fundamental-mode)
    (when (fboundp 'set-buffer-file-coding-system)
      (set-buffer-file-coding-system coding-system-for-write))
    (loop for contact in (org-contacts-filter name)
	  do (insert (org-contacts-vcard-format contact)))
    (if to-buffer
	(current-buffer)
      (progn (save-buffer) (kill-buffer)))))

(defun org-contacts-show-map (&optional name)
  "Show contacts on a map.
Requires google-maps-el."
  (interactive)
  (unless (fboundp 'google-maps-static-show)
    (error "`org-contacts-show-map' requires `google-maps-el'"))
  (google-maps-static-show
   :markers
   (loop
    for contact in (org-contacts-filter name)
    for addr = (cdr (assoc-string org-contacts-address-property (caddr contact)))
    if addr
    collect (cons (list addr) (list :label (string-to-char (car contact)))))))

(provide 'org-contacts)

(provide 'org-contacts)

;;; org-contacts.el ends here
