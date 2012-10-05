;;; ac-nrepl.el --- An auto-complete source for Clojure using nrepl completions

;; Copyright (C) 2012  Steve Purcell <steve@sanityinc.com>

;; Author: Steve Purcell <steve@sanityinc.com>
;; URL: https://github.com/purcell/ac-nrepl
;; Keywords: languages, clojure, nrepl
;; Version: 0.1
;; Package-Requires: ((nrepl "0.1"))

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

;; Based ac-slime

;;; Installation:

;; Available as a package in both Melpa (recommended) at
;; http://melpa.milkbox.net/ and Marmalade at http://marmalade-repo.org/
;; M-x package-install ac-nrepl

;;; Usage:

;;     (require 'ac-nrepl)
;;     (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
;;     (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
;;     (eval-after-load "auto-complete"
;;       '(add-to-list 'ac-modes 'nrepl-mode))

;; If you want to trigger auto-complete using TAB in nrepl buffers, you may
;; want to use auto-complete in your `completion-at-point-functions':

;;     (defun set-auto-complete-as-completion-at-point-function ()
;;       (setq completion-at-point-functions '(auto-complete)))
;;     (add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
;;
;;     (add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
;;     (add-hook 'nrepl-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)

;;; Code:

(require 'nrepl)
(require 'auto-complete)

(defun ac-nrepl-available-p ()
  "Return t if nrepl is available for completion, otherwise nil."
  (condition-case nil
      (not (null (nrepl-current-session)))
    (error nil)))

(defun ac-nrepl-candidates* (clj)
  "Return filtered completion candidates returned by evaluating clj"
  (let* ((response (plist-get (nrepl-send-string-sync clj nrepl-buffer-ns) :value)))
    (when response
      (car (read-from-string response)))))

(defun ac-nrepl-filtered-clj (clj)
  (concat "(filter #(.startsWith % \"" ac-prefix "\")" (format clj ac-prefix) ")"))

(defun ac-nrepl-unfiltered-clj (clj)
  (format clj ac-prefix))

(defun ac-nrepl-candidates-ns ()
  (ac-nrepl-candidates*
   (ac-nrepl-filtered-clj "(complete.core/namespaces *ns*)")))

(defun ac-nrepl-candidates-vars ()
  (ac-nrepl-candidates*
   (ac-nrepl-filtered-clj "(complete.core/ns-vars *ns*)")))

(defun ac-nrepl-candidates-ns-classes ()
  (ac-nrepl-candidates*
   (ac-nrepl-filtered-clj "(complete.core/ns-classes *ns*)")))

(defun ac-nrepl-fetch-all-classes ()
  (ac-nrepl-candidates*
   (ac-nrepl-unfiltered-clj  "(concat @complete.core/nested-classes
                                      @complete.core/top-level-classes)")))

(defvar ac-nrepl-all-classes-cache
  '())

(defun ac-nrepl-cache-all-classes ()
  (message "Listing all matching JVM classes...")
  (if (eq '() ac-nrepl-all-classes-cache)
      (setq ac-nrepl-all-classes-cache (ac-nrepl-fetch-all-classes))
    ac-nrepl-all-classes-cache))

(defun ac-nrepl-candidates-all-classes ()
  (if (string-match-p (regexp-quote ".") ac-prefix)
      (ac-nrepl-cache-all-classes)
    '()))

(defun ac-nrepl-candidates-java-methods ()
  (ac-nrepl-candidates*
   (ac-nrepl-filtered-clj "(for [class (vals (ns-imports *ns*))
                                 method (.getMethods class)
                                 :when (not (java.lang.reflect.Modifier/isStatic (.getModifiers method)))]
                             (str \".\" (.getName method) \" [\"(.getName class)\"]\"))")))

(defun ac-nrepl-candidates-static-methods ()
  (ac-nrepl-candidates*
   (ac-nrepl-filtered-clj "(let [prefix \"%s\"]
                           (if-not (.contains prefix \"a\")
                             '()
                              (let [scope (symbol (first (.split prefix \"/\")))]
                                (map (fn [memb] (str scope \"/\" memb))
                                     (when-let [class (complete.core/resolve-class scope)]
                                       (complete.core/static-members class))))))  ")))

(defun ac-nrepl-documentation (symbol)
  "Return documentation for the given SYMBOL, if available."
  (substring-no-properties
   (replace-regexp-in-string
    "\r" ""
    (replace-regexp-in-string
     "^\\(  \\|-------------------------\r?\n\\)" ""
     (plist-get (nrepl-send-string-sync
                 (format "(try (eval '(clojure.repl/doc %s)) (catch Exception e (println \"\")))" symbol)
                 nrepl-buffer-ns)
                :stdout)))))

(defun ac-nrepl-symbol-start-pos ()
  "Find the starting position of the symbol at point, unless inside a string."
  (let ((sap (symbol-at-point)))
    (when (and sap (not (in-string-p)))
      (car (bounds-of-thing-at-point 'symbol)))))

;;;###autoload
(defface ac-nrepl-candidate-face
  '((t (:inherit ac-candidate-face)))
  "Face for nrepl candidates."
  :group 'auto-complete)

;;;###autoload
(defface ac-nrepl-selection-face
  '((t (:inherit ac-selection-face)))
  "Face for the nrepl selected candidate."
  :group 'auto-complete)

;;;###autoload
(defvar ac-source-nrepl-ns
  '((candidates . ac-nrepl-candidates-ns)
    (available . ac-nrepl-available-p)
    (candidate-face . ac-nrepl-candidate-face)
    (selection-face . ac-nrepl-selection-face)
    (prefix . ac-nrepl-symbol-start-pos)
    (symbol . "n")
    (document . ac-nrepl-documentation))
  "Auto-complete source for nrepl ns completion.")

(defvar ac-source-nrepl-vars
  '((candidates . ac-nrepl-candidates-vars)
    (available . ac-nrepl-available-p)
    (candidate-face . ac-nrepl-candidate-face)
    (selection-face . ac-nrepl-selection-face)
    (prefix . ac-nrepl-symbol-start-pos)
    (symbol . "v")
    (document . ac-nrepl-documentation))
  "Auto-complete source for nrepl var completion.")

(defvar ac-source-nrepl-ns-classes
  '((candidates . ac-nrepl-candidates-ns-classes)
    (available . ac-nrepl-available-p)
    (candidate-face . ac-nrepl-candidate-face)
    (selection-face . ac-nrepl-selection-face)
    (prefix . ac-nrepl-symbol-start-pos)
    (symbol . "c")
    (document . ac-nrepl-documentation))
  "Auto-complete source for nrepl ns-specific class completion.")

(defvar ac-source-nrepl-all-classes
  '((candidates . ac-nrepl-candidates-all-classes)
    (available . ac-nrepl-available-p)
    (candidate-face . ac-nrepl-candidate-face)
    (selection-face . ac-nrepl-selection-face)
    (prefix . ac-nrepl-symbol-start-pos)
    (symbol . "c")
    (document . ac-nrepl-documentation))
  "Auto-complete source for nrepl all class completion.")

(defun ac-nrepl-delete-java-class-hint ()
  (let ((beg (point)))
    (search-backward " [")
    (delete-region beg (point))))

(defvar ac-source-nrepl-java-methods
  '((candidates . ac-nrepl-candidates-java-methods)
    (available . ac-nrepl-available-p)
    (candidate-face . ac-nrepl-candidate-face)
    (selection-face . ac-nrepl-selection-face)
    (prefix . ac-nrepl-symbol-start-pos)
    (symbol . "m")
    (document . ac-nrepl-documentation)
    (action . ac-nrepl-delete-java-class-hint))
  "Auto-complete source for nrepl java method completion.")

(defvar ac-source-nrepl-static-methods
  '((candidates . ac-nrepl-candidates-static-methods)
    (available . ac-nrepl-available-p)
    (candidate-face . ac-nrepl-candidate-face)
    (selection-face . ac-nrepl-selection-face)
    (prefix . ac-nrepl-symbol-start-pos)
    (symbol . "s")
    (document . ac-nrepl-documentation))
  "Auto-complete source for nrepl java static method completion.")



;;;###autoload
(defun ac-nrepl-setup ()
  "Add the nrepl completion source to the front of `ac-sources'.
This affects only the current buffer."
  (interactive)
  (add-to-list 'ac-sources 'ac-source-nrepl-ns)
  (add-to-list 'ac-sources 'ac-source-nrepl-vars)
  (add-to-list 'ac-sources 'ac-source-nrepl-ns-classes)
  (add-to-list 'ac-sources 'ac-source-nrepl-all-classes)
  (add-to-list 'ac-sources 'ac-source-nrepl-java-methods)
  (add-to-list 'ac-sources 'ac-source-nrepl-static-methods))

(provide 'ac-nrepl)

;; Local Variables:
;; coding: utf-8
;; eval: (checkdoc-minor-mode 1)
;; End:

;;; ac-nrepl.el ends here
