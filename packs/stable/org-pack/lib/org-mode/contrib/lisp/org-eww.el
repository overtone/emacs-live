;;; org-eww.el --- Store url and kill from Eww mode for Org

;; Copyright (C) 2014-2016 Free Software Foundation, Inc.

;; Author: Marco Wahl <marcowahlsoft>a<gmailcom>
;; Keywords: link, eww
;; Homepage: http://orgmode.org
;;
;; This file is not part of GNU Emacs.
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


;;; Commentary:

;; When this module is active `org-store-link' (often on key C-c l) in
;; a eww buffer stores a link to the current url of the eww buffer.

;; In an eww buffer function `org-eww-copy-for-org-mode' kills either
;; a region or the whole buffer if no region is set and transforms the
;; text on the fly so that it can be pasted into an org-mode buffer
;; with hot links.

;; C-c C-x C-w (and also C-c C-x M-w) trigger
;; `org-eww-copy-for-org-mode'.

;; Hint: A lot of code of this module comes from module org-w3m which
;; has been written by Andy Steward based on the idea of Richard
;; Riley.  Thanks!

;; Potential: Since the code for w3m and eww is so similar one could
;; try to refactor.


;;; Code:
(require 'org)


;; Store Org-link in eww-mode buffer
(add-hook 'org-store-link-functions 'org-eww-store-link)
(defun org-eww-store-link ()
  "Store a link to the url of a eww buffer."
  (when (eq major-mode 'eww-mode)
    (org-store-link-props
     :type "eww"
     :link (if (< emacs-major-version 25)
	       eww-current-url
	     (eww-current-url))
     :url (url-view-url t)
     :description (if (< emacs-major-version 25)
		      (or eww-current-title eww-current-url)
		    (or (plist-get eww-data :title)
			  (eww-current-url))))))


;; Some auxiliary functions concerning links in eww buffers
(defun org-eww-goto-next-url-property-change ()
  "Move cursor to the start of next link if exists.  Else no
move.  Return point."
  (goto-char
   (or (next-single-property-change (point) 'shr-url)
       (point))))

(defun org-eww-no-next-link-p ()
  "Whether there is no next link after the cursor.
Return t if there is no next link; otherwise, return nil."
  (save-excursion
    (and (eq (point) (org-eww-goto-next-url-property-change)) t)))

(defun org-eww-url-below-point ()
  "Return the url below point if there is an url; otherwise, return nil."
  (get-text-property (point) 'shr-url))


(defun org-eww-copy-for-org-mode ()
  "Copy current buffer content or active region with `org-mode' style links.
This will encode `link-title' and `link-location' with
`org-make-link-string', and insert the transformed test into the kill ring,
so that it can be yanked into an Org-mode buffer with links working correctly.

Further lines starting with a star get quoted with a comma to keep
the structure of the org file."
  (interactive)
  (let* ((regionp (org-region-active-p))
         (transform-start (point-min))
         (transform-end (point-max))
         return-content
         link-location link-title
         temp-position out-bound)
    (when regionp
      (setq transform-start (region-beginning))
      (setq transform-end (region-end))
      ;; Deactivate mark if current mark is activate.
      (if (fboundp 'deactivate-mark) (deactivate-mark)))
    (message "Transforming links...")
    (save-excursion
      (goto-char transform-start)
      (while (and (not out-bound)                 ; still inside region to copy
                  (not (org-eww-no-next-link-p))) ; there is a next link
        ;; store current point before jump next anchor
        (setq temp-position (point))
        ;; move to next anchor when current point is not at anchor
        (or (org-eww-url-below-point)
	    (org-eww-goto-next-url-property-change))
	(assert (org-eww-url-below-point) t
                "program logic error: point must have an url below but it hasn't")
	(if (<= (point) transform-end)  ; if point is inside transform bound
	    (progn
	      ;; get content between two links.
	      (if (> (point) temp-position)
		  (setq return-content (concat return-content
					       (buffer-substring
						temp-position (point)))))
	      ;; get link location at current point.
	      (setq link-location (org-eww-url-below-point))
	      ;; get link title at current point.
	      (setq link-title
		    (buffer-substring
		     (point)
		     (org-eww-goto-next-url-property-change)))
              ;; concat `org-mode' style url to `return-content'.
              (setq return-content (concat return-content
                                           (org-make-link-string
                                            link-location link-title))))
	  (goto-char temp-position)     ; reset point before jump next anchor
	  (setq out-bound t)            ; for break out `while' loop
	  ))
      ;; add the rest until end of the region to be copied
      (if (< (point) transform-end)
          (setq return-content
                (concat return-content
                        (buffer-substring (point) transform-end))))
      ;; quote lines starting with *
      (org-kill-new
       (with-temp-buffer
	 (insert return-content)
	 (goto-char 0)
	 (replace-regexp "^\*" ",*")
	 (buffer-string)))
      (message "Transforming links...done, use C-y to insert text into Org-mode file"))))


;; Additional keys for eww-mode

(defun org-eww-extend-eww-keymap ()
  (define-key eww-mode-map "\C-c\C-x\M-w" 'org-eww-copy-for-org-mode)
  (define-key eww-mode-map "\C-c\C-x\C-w" 'org-eww-copy-for-org-mode))

(when (and (boundp 'eww-mode-map)
           (keymapp eww-mode-map)) ; eww is already up.
  (org-eww-extend-eww-keymap))

(add-hook
 'eww-mode-hook
 (lambda () (org-eww-extend-eww-keymap)))


(provide 'org-eww)

;;; org-eww.el ends here
