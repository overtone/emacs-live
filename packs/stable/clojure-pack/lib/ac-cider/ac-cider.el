;;; ac-cider.el --- Clojure auto-complete sources using CIDER

;; Copyright (C) 2012-2015 Alex Yakushev <alex@bytopia.org>

;; Author: Alex Yakushev <alex@bytopia.org>
;;         Steve Purcell <steve@sanityinc.com>
;;         Sam Aaron <samaaron@gmail.com>
;;
;; URL: https://github.com/clojure-emacs/ac-cider
;; Keywords: languages, clojure, nrepl, cider, compliment
;; Version: 0.2.3
;; Package-Requires: ((cider "0.8.0") (auto-complete "1.4") (cl-lib "0.3"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides a number of auto-complete sources for Clojure projects using CIDER
;; and compliment. This is a replacement for now deprecated ac-nrepl project by
;; Steve Purcell.

;;; Installation:

;; Available as a package in melpa.org.
;; M-x package-install ac-cider

;;; Usage:

;;     (require 'ac-cider)
;;     (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
;;     (add-hook 'cider-mode-hook 'ac-cider-setup)
;;     (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
;;     (eval-after-load "auto-complete"
;;       '(progn
;;          (add-to-list 'ac-modes 'cider-mode)
;;          (add-to-list 'ac-modes 'cider-repl-mode)))

;; By default, entries in the popup menu will also display the namespace that
;; the symbol belongs to. To disable this behavior, add to your init file:

;;     (setq ac-cider-show-ns nil)

;; If you want to trigger auto-complete using TAB in CIDER buffers, add this to
;; your configuration file, but note that it is incompatible with (setq
;; tab-always-indent 'complete):

;;     (defun set-auto-complete-as-completion-at-point-function ()
;;       (setq completion-at-point-functions '(auto-complete)))
;;     (add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
;;
;;     (add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)

;;; Code:

(require 'cider)
(require 'auto-complete)
(require 'cl-lib)

(defun ac-cider-available-p ()
  "Return t if CIDER supports completion, otherwise nil."
  (functionp 'cider-complete))

(defvar ac-cider-documentation-cache '())

;; customization group
(defgroup ac-cider nil
  "auto-complete sources for cider."
  :prefix "ac-cider-"
  :group 'completion)

(defcustom ac-cider-show-ns t
  "Non-nil means rows in the AC popup menu will show the namespace of the item."
  :type 'boolean
  :group 'ac-cider)

(defun ac-cider-prepare-candidates (candidates)
  "Apply changes for auto-complete to the candidates returned from cider-complete."
  (if ac-cider-show-ns
      (mapcar
       (lambda (candidate)
         (let ((ns (get-text-property 0 'ns candidate)))
           ;; Copy the 'ns' value to 'summary' for auto-complete to display
           (when ns
             (add-text-properties 0 1 `(summary ,ns) candidate))
           candidate))
       candidates)
    candidates))

(defun ac-cider-candidates-everything ()
  "Return all candidates for a symbol at point."
  (setq ac-cider-documentation-cache nil)
  (when (cider-connected-p)
    (ac-cider-prepare-candidates (cider-complete ac-prefix))))

(defun ac-cider-documentation (symbol)
  "Return documentation for the given SYMBOL, if available.
Caches fetched documentation for the current completion call."
  (when symbol
    (let ((cached-doc (assoc (substring-no-properties symbol)
                             ac-cider-documentation-cache)))
      (if cached-doc
          (cdr cached-doc)
        (let* ((doc
                (substring-no-properties
                 (replace-regexp-in-string
                  "\r" ""
                  (nrepl-dict-get (cider-nrepl-send-sync-request
                                   (list "op" "complete-doc"
                                         "session" (cider-current-session)
                                         "ns" (cider-current-ns)
                                         "symbol" symbol))
                                  "completion-doc"))))
               (doc (if (string= "\"\"" doc)
                        "No documentation available."
                      doc)))
          (add-to-list 'ac-cider-documentation-cache
                       (cons (substring-no-properties symbol) doc))
          doc)))))

(defun ac-cider--is-separator (c)
  (or (= c 45) (= c 46)))

(defun ac-cider--is-capital (c)
  (and (<= 65 c) (<= c 90)))

(defun ac-cider-fuzzy-matches-p (prefix candidate)
  "Return t if PREFIX and CANDIDATE are matched."
  (let ((pre (string-to-list prefix))
        (cand (string-to-list candidate))
        (camelcase nil))
    (when (= (car pre) 46)
      (setq camelcase t)
      (setq pre (cdr pre))
      (setq cand (cdr cand)))
    (if (string-match-p (regexp-quote prefix) candidate)
        t
      (let ((result :not-yet) (skipping nil))
        (when (= (car pre) (car cand))
          (while (eq result :not-yet)
            (cond ((not pre) (setq result t))
                  ((not cand) (setq result nil))
                  (skipping (if camelcase
                                (if (ac-cider--is-capital (car cand))
                                    (setq skipping nil)
                                  (setq cand (cdr cand)))
                              (if (ac-cider--is-separator (car cand))
                                  (progn
                                    (when (ac-cider--is-separator (car pre))
                                      (setq pre (cdr pre)))
                                    (setq cand (cdr cand))
                                    (setq skipping nil))
                                (setq cand (cdr cand)))))
                  ((= (car pre) (car cand)) (progn (setq pre (cdr pre))
                                                   (setq cand (cdr cand))))
                  (t (progn (setq skipping
                                  (or camelcase
                                      (not (ac-cider--is-separator (car cand)))))
                            (setq cand (cdr cand))))))
          result)))))

(defun ac-cider-match-fuzzy (prefix candidates)
  "Returns candidates that match fuzzily with the prefix."
  (cl-remove-if-not (lambda (cand)
                      (ac-cider-fuzzy-matches-p prefix cand))
                    candidates))

;;;###autoload
(defface ac-cider-candidate-face
  '((t (:inherit ac-candidate-face)))
  "Face for nrepl candidates."
  :group 'auto-complete)

;;;###autoload
(defface ac-cider-selection-face
  '((t (:inherit ac-selection-face)))
  "Face for the nrepl selected candidate."
  :group 'auto-complete)

;;;###autoload
(defconst ac-cider-source-defaults
  '((available . ac-cider-available-p)
    (candidate-face . ac-cider-candidate-face)
    (selection-face . ac-cider-selection-face)
    (prefix . cider-completion-symbol-start-pos)
    (match . ac-cider-match-fuzzy)
    (document . ac-cider-documentation)
    (cache))
  "Defaults common to the various completion sources.")

;;;###autoload
(defvar ac-source-cider-everything
  (append
   '((candidates . ac-cider-candidates-everything)
     (symbol . "v"))
   ac-cider-source-defaults)
  "Auto-complete source for CIDER buffers.")

;;;###autoload
(defun ac-cider-setup ()
  "Add the CIDER completion source to the front of `ac-sources'.
This affects only the current buffer."
  (interactive)
  (setq-default ac-use-fuzzy nil)
  (add-to-list 'ac-sources 'ac-source-cider-everything))

;;;###autoload
(defun ac-cider-popup-doc ()
  "A popup alternative to `cider-doc'."
  (interactive)
  (popup-tip (ac-cider-documentation (symbol-name (symbol-at-point)))
             :point (cider-completion-symbol-start-pos)
             :around t
             :scroll-bar t
             :margin t))

(provide 'ac-cider)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; eval: (checkdoc-minor-mode 1)
;; End:

;;; ac-cider.el ends here
