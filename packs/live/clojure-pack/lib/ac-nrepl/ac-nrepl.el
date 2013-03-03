;;; ac-nrepl.el --- auto-complete sources for Clojure using nrepl completions

;; Copyright (C) 2012  Steve Purcell <steve@sanityinc.com>

;; Author: Steve Purcell <steve@sanityinc.com>
;;         Sam Aaron <samaaron@gmail.com>
;; URL: https://github.com/purcell/ac-nrepl
;; Keywords: languages, clojure, nrepl
;; Version: DEV
;; Package-Requires: ((nrepl "0.1") (auto-complete "1.4"))

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

;; Provides a number of auto-complete sources for Clojure projects
;; using nrepl.

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
;;
;; You might consider using ac-nrepl's popup documentation in place of `nrepl-doc':
;;
;;     (define-key nrepl-interaction-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)

;;; Code:

(require 'nrepl)
(require 'auto-complete)

(defun ac-nrepl-available-p ()
  "Return t if nrepl is available for completion, otherwise nil."
  (condition-case nil
      (not (null (nrepl-current-tooling-session)))
    (error nil)))

(defun ac-nrepl-sync-eval (clj)
  "Synchronously evaluate CLJ.
Result is a plist, as returned from `nrepl-send-string-sync'."
  (nrepl-send-string-sync clj (nrepl-current-ns) (nrepl-current-tooling-session)))

(defun ac-nrepl-candidates* (clj)
  "Return completion candidates produced by evaluating CLJ."
  (let ((response (plist-get (ac-nrepl-sync-eval (concat "(require 'complete.core) " clj))
                             :value)))
    (when response
      (car (read-from-string response)))))

(defun ac-nrepl-unfiltered-clj (clj)
  "Return a version of CLJ with the completion prefix inserted."
  (format clj ac-prefix))

(defun ac-nrepl-filtered-clj (clj)
  "Build an expression which extracts the prefixed values from CLJ."
  (concat "(filter #(.startsWith % \"" ac-prefix "\")"
          (ac-nrepl-unfiltered-clj clj) ")"))

(defun ac-nrepl-candidates-ns ()
  "Return namespace candidates."
  (ac-nrepl-candidates*
   (ac-nrepl-filtered-clj "(complete.core/namespaces *ns*)")))

(defun ac-nrepl-candidates-vars ()
  "Return var candidates."
  (ac-nrepl-candidates*
   (ac-nrepl-filtered-clj "(let [prefix \"%s\"]
    (if-not (.contains prefix \"/\")
      (complete.core/ns-vars *ns*)
      (let [ns-alias (symbol (first (.split prefix \"/\")))
            core     (find-ns 'clojure.core)]
        (if-let [ns (or (get (ns-aliases *ns*) ns-alias)
                        (find-ns ns-alias))]
          (let [vars (complete.core/ns-vars ns)
                vars (if (= core ns)
                       vars
                       (remove (into #{} (complete.core/ns-vars core)) vars))]
            (map (fn [x] (str ns-alias \"/\" x)) vars))
           '()))))")))

(defun ac-nrepl-candidates-ns-classes ()
  "Return namespaced class candidates."
  (ac-nrepl-candidates*
   (ac-nrepl-filtered-clj "(complete.core/ns-classes *ns*)")))

(defun ac-nrepl-fetch-all-classes ()
  "Return all class candidates."
  (ac-nrepl-candidates*
   (ac-nrepl-unfiltered-clj "(concat @complete.core/nested-classes
                                     @complete.core/top-level-classes)")))

(defvar ac-nrepl-all-classes-cache nil
  "Cached list of all classes loaded in the JVM backend.")

;;;###autoload
(defun ac-nrepl-clear-class-cache ()
  "Clear the class cache to prevent stale results."
  (setq ac-nrepl-all-classes-cache nil))

(defun ac-nrepl-cache-all-classes ()
  "Return a cached list of all class names loaded in the JVM backend."
  (setq ac-nrepl-all-classes-cache (ac-nrepl-fetch-all-classes)))

(defun ac-nrepl-refresh-class-cache ()
  "Refresh class cache"
  (ac-nrepl-clear-class-cache)
  (message "Caching JVM class names...")
  (ac-nrepl-cache-all-classes)
  (message ""))

;;;###autoload
(add-hook 'nrepl-connected-hook 'ac-nrepl-refresh-class-cache t)

(defun ac-nrepl-candidates-all-classes ()
  "Return java method candidates."
  (when (string-match-p "^[a-zA-Z]+[a-zA-Z0-9$_]*\\.[a-zA-Z0-9$_.]*$" ac-prefix)
    ac-nrepl-all-classes-cache))

(defun ac-nrepl-candidates-java-methods ()
  "Return java method candidates."
  (ac-nrepl-candidates*
   (ac-nrepl-filtered-clj
    "(for [class (vals (ns-imports *ns*))
           method (.getMethods class)
           :when (not (java.lang.reflect.Modifier/isStatic (.getModifiers method)))]
       (str \".\" (.getName method) \" [\"(.getName class)\"]\"))")))

(defun ac-nrepl-candidates-static-methods ()
  "Return static method candidates."
  (ac-nrepl-candidates*
   (ac-nrepl-filtered-clj
    "(let [prefix \"%s\"]
       (if (or (not (.contains prefix \"/\"))
               (.startsWith prefix \"/\"))
         '()
          (let [scope (symbol (first (.split prefix \"/\")))]
            (map (fn [memb] (str scope \"/\" memb))
                 (when-let [class (try (complete.core/resolve-class scope)
                                   (catch java.lang.ClassNotFoundException e nil))]
                   (complete.core/static-members class))))))  ")))

(defun ac-nrepl-documentation (symbol)
  "Return documentation for the given SYMBOL, if available."
  (let ((doc
         (substring-no-properties
          (replace-regexp-in-string
           "\r" ""
           (replace-regexp-in-string
            "^\\(  \\|-------------------------\r?\n\\)" ""
            (plist-get (ac-nrepl-sync-eval
                        (format "(try (eval '(clojure.repl/doc %s))
                               (catch Exception e (println \"\")))" symbol))
                       :stdout))))))
    (unless (string-match "\\`[ \t\n]*\\'" doc)
      doc)))

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
(defconst ac-nrepl-source-defaults
  '((available . ac-nrepl-available-p)
    (candidate-face . ac-nrepl-candidate-face)
    (selection-face . ac-nrepl-selection-face)
    (prefix . ac-nrepl-symbol-start-pos)
    (document . ac-nrepl-documentation))
  "Defaults common to the various completion sources.")

;;;###autoload
(defvar ac-source-nrepl-ns
  (append
   '((candidates . ac-nrepl-candidates-ns)
     (symbol . "n"))
   ac-nrepl-source-defaults)
  "Auto-complete source for nrepl ns completion.")

;;;###autoload
(defvar ac-source-nrepl-vars
  (append
   '((candidates . ac-nrepl-candidates-vars)
     (symbol . "v"))
   ac-nrepl-source-defaults)
  "Auto-complete source for nrepl var completion.")

;;;###autoload
(defvar ac-source-nrepl-ns-classes
  (append
   '((candidates . ac-nrepl-candidates-ns-classes)
     (symbol . "c"))
   ac-nrepl-source-defaults)
  "Auto-complete source for nrepl ns-specific class completion.")

;;;###autoload
(defvar ac-source-nrepl-all-classes
  (append
   '((candidates . ac-nrepl-candidates-all-classes)
     (symbol . "c"))
   ac-nrepl-source-defaults)
  "Auto-complete source for nrepl all class completion.")

(defun ac-nrepl-delete-java-class-hint ()
  "Remove the java class hint at point."
  (let ((beg (point)))
    (search-backward " [")
    (delete-region beg (point))))

;;;###autoload
(defvar ac-source-nrepl-java-methods
  (append
   '((candidates . ac-nrepl-candidates-java-methods)
     (symbol . "m")
     (action . ac-nrepl-delete-java-class-hint))
   ac-nrepl-source-defaults)
  "Auto-complete source for nrepl java method completion.")

;;;###autoload
(defvar ac-source-nrepl-static-methods
  (append
   '((candidates . ac-nrepl-candidates-static-methods)
     (symbol . "s"))
   ac-nrepl-source-defaults)
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

;;;###autoload
(defun ac-nrepl-popup-doc ()
  "A popup alternative to `nrepl-doc'."
  (interactive)
  (popup-tip (ac-nrepl-documentation (symbol-at-point))
             :point (ac-nrepl-symbol-start-pos)
             :around t
             :scroll-bar t
             :margin t))

(provide 'ac-nrepl)

;; Local Variables:
;; coding: utf-8
;; eval: (checkdoc-minor-mode 1)
;; End:

;;; ac-nrepl.el ends here
