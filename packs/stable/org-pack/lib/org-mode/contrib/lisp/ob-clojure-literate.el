;;; ob-clojure-literate.el --- Clojure's Org-mode Literate Programming.

;; Authors: stardiviner <numbchild@gmail.com>
;; Package-Requires: ((emacs "24.4") (org "9") (cider "0.16.0") (dash "2.12.0"))
;; Package-Version: 1.1
;; Keywords: tools
;; homepage: https://github.com/stardiviner/ob-clojure-literate

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Auto setup ob-clojure-literate scaffold and jack-in Clojure project.
;;
;; Usage:
;;
;; [M-x ob-clojure-literate-mode] to toggle this minor mode.

;;; Code:

(require 'ob-clojure)
(require 'cider)

(defgroup ob-clojure-literate nil
  "Clojure's Org-mode Literate Programming."
  :prefix "ob-clojure-literate-"
  :group 'ob-babel)

;;;###autoload
(defcustom ob-clojure-literate-auto-jackin-p nil
  "Auto jack in ob-clojure project.
Don't auto jack in by default for not rude."
  :type 'boolean
  :group 'ob-clojure-literate)

(defcustom ob-clojure-literate-project-location nil
  "The location for `ob-clojure-literate' scaffold project.
If it is nil, then `cider-jack-in' will jack-in outside of Clojure project.
If it is a directory, `ob-clojure-literate' will try to create Clojure project automatically."
  :type 'string
  :group 'ob-clojure-literate)

(defvar ob-clojure-literate-session nil)
(defvar ob-clojure-literate-original-ns nil)
(defvar ob-clojure-literate-session-ns nil)
(defvar ob-clojure-literate-cider-connections nil)

(defcustom ob-clojure-literate-default-session "*cider-repl localhost*"
  "The default session name for `ob-clojure-literate'."
  :type 'string
  :group 'ob-clojure-literate)

(defun ob-clojure-literate-any-connection-p ()
  "Return t if have any CIDER connection."
  (and
   ;; handle the case `cider-jack-in' is not finished creating connection, but `ob-clojure-literate-mode' is enabled.
   (not (null (cider-connections)))
   (not (null ob-clojure-literate-session)) ; before mode enabled, it is nil.
   (not (string-empty-p ob-clojure-literate-session)) ; after disable, it is "".
   ))

(defun ob-clojure-literate-get-session-list ()
  "Return a list of available started CIDER REPL sessions list."
  (mapcar #'buffer-name
	  ;; for multiple connections case.
	  ;; get global value instead of buffer local.
	  (default-value 'cider-connections)))

;;; Do not allow "ob-clojure" project session name.
(defun ob-clojure-literate-set-session ()
  "Set session name for buffer local."
  ;; if default session is the only one in connections list.
  (if (and (= (length (ob-clojure-literate-get-session-list)) 1)
           (member ob-clojure-literate-default-session (ob-clojure-literate-get-session-list)))
      (setq-local ob-clojure-literate-session ob-clojure-literate-default-session)
    ;; if have any connections, choose one from them.
    (if (ob-clojure-literate-any-connection-p)
        (setq-local ob-clojure-literate-session
                    (completing-read "Choose ob-clojure-literate :session : "
                                     (ob-clojure-literate-get-session-list)))
      ;; if none, set to default session name to fix `ob-clojure-literate-mode'
      ;; is enabled before `cider-jack-in' generated connections.
      (setq-local ob-clojure-literate-session
		  ob-clojure-literate-default-session))))

;;;###autoload
(defun ob-clojure-literate-specify-session ()
  "Specify ob-clojure header argument :session with value selected from a list of available sessions."
  (interactive)
  (let ((lang (nth 0 (org-babel-get-src-block-info))))
    (if (and (string= lang "clojure") ; only in clojure src block.
             (car (seq-filter ; only when :session is not specified yet.
                   (lambda (header-argument)
                     (if (eq (car header-argument) :session)
                         (not (null (cdr header-argument)))))
                   (nth 2 (org-babel-get-src-block-info)))))
        (org-babel-insert-header-arg
         "session"
         (format "\"%s\""
                 (completing-read
                  "Choose :session for ob-clojure-literate: "
                  (ob-clojure-literate-get-session-list))))
      (message "This function only used in `clojure' src block.")))
  )

;;; Auto start CIDER REPL session in a complete Leiningen project environment for Org-mode Babel to jack-in.
;;;###autoload
(defun ob-clojure-literate-auto-jackin ()
  "Auto setup ob-clojure-literate scaffold and jack-in Clojure project."
  (interactive)
  (cond
   ;; jack-in outside of Clojure project.
   ((null ob-clojure-literate-project-location)
    (if (member (get-buffer "*cider-repl localhost*") cider-connections)
	(message "CIDER default session already launched.")
      (cider-jack-in nil)))
   ((not (null ob-clojure-literate-project-location))
    (unless (file-directory-p (expand-file-name ob-clojure-literate-project-location))
      (make-directory ob-clojure-literate-project-location t)
      (let ((default-directory ob-clojure-literate-project-location))
	(shell-command "lein new ob-clojure")))
    (unless (or
             (and (cider-connected-p)
                  (if (not (null ob-clojure-literate-session))
		      (seq-contains cider-connections (get-buffer ob-clojure-literate-session))))
             cider-connections
	     (ob-clojure-literate-any-connection-p))
      ;; return back to original file.
      (if (not (and (= (length (ob-clojure-literate-get-session-list)) 1)
                    (member ob-clojure-literate-default-session (ob-clojure-literate-get-session-list))))
          (save-window-excursion
            (find-file (expand-file-name (concat ob-clojure-literate-project-location "ob-clojure/src/ob_clojure/core.clj")))
            (with-current-buffer "core.clj"
	      (cider-jack-in))))))))

(defun ob-clojure-literate-set-local-cider-connections (toggle?)
  "Set buffer local `cider-connections' for `ob-clojure-literate-mode' `TOGGLE?'."
  (if toggle?
      (progn
        (setq ob-clojure-literate-cider-connections cider-connections)
        (unless (local-variable-if-set-p 'cider-connections)
          (make-local-variable 'cider-connections))
        (setq-local cider-connections ob-clojure-literate-cider-connections))
    ;; store/restore emptied CIDER connections by `ob-clojure-literate-enable'.
    (kill-local-variable 'cider-connections) ; kill local variable so that I can get the original global variable value.
    ;; Empty all CIDER connections to avoid `cider-current-connection' return any connection.
    ;; FIXME: when try to enable, `cider-connections' is local and nil.
    ;; (if (and (= (length (ob-clojure-literate-get-session-list)) 1)
    ;;          (member ob-clojure-literate-default-session (ob-clojure-literate-get-session-list))))
    ;; (unless (local-variable-if-set-p 'cider-connections)
    ;;   (make-local-variable 'cider-connections))
    ;; (setq-local cider-connections '())
    ))

(defun ob-clojure-literate-set-ns (body params)
  "Fix the issue that `cider-current-ns' try to invoke `clojure-find-ns' to extract ns from buffer."
  ;; TODO: Is it possible to find ns in `body'?
  (when (ob-clojure-literate-any-connection-p)
    (setq ob-clojure-literate-original-ns (cider-current-ns))
    (with-current-buffer ob-clojure-literate-session
      (setq ob-clojure-literate-session-ns cider-buffer-ns))
    (setq-local cider-buffer-ns (or (cdr (assq :ns params))
				    ob-clojure-literate-session-ns)))
  (message (format "ob-clojure-literate: current CIDER ns is [%s]." cider-buffer-ns)))

(defun ob-clojure-literate-set-local-session (toggle?)
  "Set buffer local `org-babel-default-header-args:clojure' for `ob-clojure-literate-mode' `TOGGLE?'."
  (if toggle?
      (progn
        ;; set local default session for ob-clojure.
        (setq ob-clojure-literate-session (ob-clojure-literate-set-session))
        (unless (local-variable-if-set-p 'org-babel-default-header-args:clojure)
          (make-local-variable 'org-babel-default-header-args:clojure))
        (add-to-list 'org-babel-default-header-args:clojure
                     `(:session . ,ob-clojure-literate-session))
        )
    ;; remove :session from buffer local default header arguments list.
    (unless (local-variable-if-set-p 'org-babel-default-header-args:clojure)
      (make-local-variable 'org-babel-default-header-args:clojure))
    (setq org-babel-default-header-args:clojure
          (delq t
                (mapcar
                 (lambda (cons) (if (eq (car cons) :session) t cons))
                 org-babel-default-header-args:clojure)))))


;;; Support header arguments  :results graphics :file "image.png" by inject Clojure code.
(defun ob-clojure-literate-inject-code (args)
  "Inject Clojure code into `BODY' in `ARGS'.
It is used to change Clojure currently working directory in a FAKE way.
And generate inline graphics image file link result.
Use header argument like this:

:results graphics :file \"incanter-plot.png\"

Then you need to assign image variable to this :file value like:
(def incanter-plot (histogram (sample-normal 1000)))

*NOTE*: Currently only support Incanter's `save' function.
"
  (let* ((body (nth 0 args))
         (params (nth 1 args))
	 (dir (cdr (assq :dir params)))
	 (default-directory (and (buffer-file-name) (file-name-directory (buffer-file-name))))
         (directory (and dir (file-name-as-directory (expand-file-name dir))))
	 (result-type (cdr (assq :results params)))
	 (file (cdr (assq :file params)))
	 (file-name (and file (file-name-base file)))
	 ;; TODO: future support `:graphics-file' to avoid collision.
	 (graphics-result (member "graphics" (cdr (assq :result-params params))))
	 ;; (graphics-file (cdr (assq :graphics-file params)))
	 ;; (graphics-name (file-name-base graphics-file))
         (prepend-to-body (lambda (code)
                            (setq body (concat code "\n" body))))
	 (append-to-body (lambda (code)
			   (setq body (concat body "\n" code "\n"))))
         )
    (when directory
      (unless (file-directory-p (expand-file-name directory))
	(warn (format "Target directory %s does not exist, please create it." dir))))
    (when file
      (funcall append-to-body
      	       (format "(save %s \"%s\")" file-name (concat directory file)))
      )
    (list body params) ; return modified argument list
    ))

;;; support :results graphics :dir "data/image" :file "incanter-plot.png"
(defun ob-clojure-literate-support-graphics-result (result)
  "Support :results graphics :dir \"data/images\" :file \"incanter-plot.png\"
reset `RESULT' to `nil'."
  (let* ((params (nth 2 info))
	 (graphics-result (member "graphics" (cdr (assq :result-params params)))))
    (if graphics-result
	(setq result nil))
    result))


(defvar ob-clojure-literate-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `ob-clojure-literate-mode'.")

(define-key org-babel-map (kbd "M-s") 'ob-clojure-literate-specify-session)
(define-key org-babel-map (kbd "M-j") 'ob-clojure-literate-auto-jackin)
;; (define-key org-babel-map (kbd "M-e") 'cider-eval-last-sexp)
;; (define-key org-babel-map (kbd "M-d") 'cider-doc)

;;;###autoload
(defun ob-clojure-literate-enable ()
  "Enable Org-mode buffer locally for `ob-clojure-literate'."
  (when (and (not (null cider-connections)) ; only enable `ob-clojure-literate-mode' when has CIDER connections.
             (equal major-mode 'org-mode)) ; `ob-clojure-literate-mode' only works in `org-mode'.
    (ob-clojure-literate-set-local-cider-connections ob-clojure-literate-mode)
    (ob-clojure-literate-set-local-session ob-clojure-literate-mode)
    (advice-add 'org-babel-execute:clojure :before #'ob-clojure-literate-set-ns)
    (advice-add 'org-babel-expand-body:clojure :filter-args #'ob-clojure-literate-inject-code)
    (advice-add 'org-babel-execute:clojure :filter-return #'ob-clojure-literate-support-graphics-result)
    (message "ob-clojure-literate minor mode enabled.")))

;;;###autoload
(defun ob-clojure-literate-disable ()
  "Disable Org-mode buffer locally for `ob-clojure-literate'."
  (advice-remove 'org-babel-execute:clojure #'ob-clojure-literate-set-ns)
  (advice-remove 'org-babel-expand-body:clojure #'ob-clojure-literate-inject-code)
  (advice-remove 'org-babel-execute:clojure #'ob-clojure-literate-support-graphics-result)
  (setq-local cider-buffer-ns ob-clojure-literate-original-ns)
  (ob-clojure-literate-set-local-cider-connections ob-clojure-literate-mode)
  (ob-clojure-literate-set-local-session ob-clojure-literate-mode)
  (message "ob-clojure-literate minor mode disabled."))

;;;###autoload
(if ob-clojure-literate-auto-jackin-p (ob-clojure-literate-auto-jackin))

;;;###autoload
(define-minor-mode ob-clojure-literate-mode
  "A minor mode to toggle `ob-clojure-literate'."
  :require 'ob-clojure-literate
  :init-value nil
  :lighter " clj-lp"
  :group 'ob-clojure-literate
  :keymap ob-clojure-literate-mode-map
  :global nil
  (if ob-clojure-literate-mode
      (ob-clojure-literate-enable)
    (ob-clojure-literate-disable))
  )



(provide 'ob-clojure-literate)

;;; ob-clojure-literate.el ends here
