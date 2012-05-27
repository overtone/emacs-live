;; Emacs LIVE
;;
;; This is where everything starts. Do you remember this place?
;; It remembers you...

;; Store live base dirs
(setq live-root-dir (file-name-directory
                     (or (buffer-file-name) load-file-name)))

(setq
 live-tmp-dir      (file-name-as-directory (concat live-root-dir "tmp"))
 live-etc-dir      (file-name-as-directory (concat live-root-dir "etc"))
 live-packs-dir    (file-name-as-directory (concat live-root-dir "packs"))
 live-autosaves-dir(file-name-as-directory (concat live-tmp-dir  "autosaves"))
 live-backups-dir  (file-name-as-directory (concat live-tmp-dir  "backups"))
 live-load-pack-dir nil)

;; create tmp dirs if necessary
(make-directory live-etc-dir t)
(make-directory live-tmp-dir t)
(make-directory live-autosaves-dir t)
(make-directory live-backups-dir t)

;;default live packs
(let* ((live-dir (file-name-as-directory "live"))
       (user-dir (file-name-as-directory "user")))
  (setq live-packs (list (concat live-dir "foundation-pack")
                         (concat live-dir "colour-pack")
                         (concat live-dir "clojure-pack")
                         (concat live-dir "lang-pack")
                         (concat live-dir "power-pack")
                         (concat user-dir "user-pack"))))

;; Helper fn for loading live packs

(defun live-pack-config-dir ()
  "Returns the path of the config dir for the current pack"
  (file-name-as-directory (concat live-load-pack-dir "config")))

(defun live-pack-lib-dir ()
  "Returns the path of the lib dir for the current pack"
  (file-name-as-directory (concat live-load-pack-dir "lib")))

(defun live-load-pack (pack-name)
  "Load a live pack. This is a dir that contains at least a file
  called init.el. Adds the packs's lib dir to the load-path"
  (let* ((pack-name (if (symbolp pack-name)
                        (symbol-name pack-name)
                      pack-name))
         (pack-name-dir (if (file-name-absolute-p pack-name)
                            (file-name-as-directory pack-name)
                          (file-name-as-directory (concat live-packs-dir pack-name))))
         (pack-init (concat pack-name-dir "init.el")))
    (setq live-load-pack-dir pack-name-dir)
    (add-to-list 'load-path (live-pack-lib-dir))
    (if (file-exists-p pack-init)
        (load-file pack-init))
    (setq live-load-pack-dir nil)))

(defun live-add-pack-lib (p)
  "Adds the path (specified relative to the the pack's lib dir)
  to the load-path"
  (add-to-list 'load-path (concat (live-pack-lib-dir) p)))

(defun live-load-config-file (f-name)
  "Load the config file with name f-name in the current pack"
  (let* ((config-dir (live-pack-config-dir)))
    (load-file (concat config-dir f-name))))

;; Load `~/.emacs-live.el`. This allows you to override variables such
;; as live-packs (allowing you to specify pack loading order)
(let* ((pack-file (concat (file-name-as-directory "~") ".emacs-live.el")))
  (if (file-exists-p pack-file)
      (load-file pack-file)))

;; Load all packs - Power Extreme!
(mapcar (lambda (pack-name) (live-load-pack pack-name)) live-packs)

(defun live-user-first-name ()
  (car  (split-string user-full-name)))

(setq live-welcome-messages
      (list (concat "Hello " (live-user-first-name) ", somewhere in the world the sun is shining for you right now.")
            (concat "Hello " (live-user-first-name) ", it's lovely to see you again. I do hope that you're well.")
            (concat (live-user-first-name) ", turn your head towards the sun and the shadows will fall behind you.")
            ))

(defun live-welcome-message ()
  (nth (random* (length live-welcome-messages)) live-welcome-messages))

(setq initial-scratch-message (concat ";;     MM\"\"\"\"\"\"\"\"`M
;;     MM  mmmmmmmM
;;     M`      MMMM 88d8b.d8b. .d8888b. .d8888b. .d8888b.
;;     MM  MMMMMMMM 88''88'`88 88'  `88 88'  `\"\" Y8ooooo.
;;     MM  MMMMMMMM 88  88  88 88.  .88 88.  ...       88
;;     MM        .M dP  dP  dP `88888P8 '88888P' '88888P'
;;     MMMMMMMMMMMM
;;
;;         M\"\"MMMMMMMM M\"\"M M\"\"MMMMM\"\"M MM\"\"\"\"\"\"\"\"`M
;;         M  MMMMMMMM M  M M  MMMMM  M MM  mmmmmmmM
;;         M  MMMMMMMM M  M M  MMMMP  M M`      MMMM
;;         M  MMMMMMMM M  M M  MMMM' .M MM  MMMMMMMM
;;         M  MMMMMMMM M  M M  MMP' .MM MM  MMMMMMMM
;;         M         M M  M M     .dMMM MM        .M
;;         MMMMMMMMMMM MMMM MMMMMMMMMMM MMMMMMMMMMMM
;;
;;           http://github.com/overtone/emacs-live
;;
;; " (live-welcome-message) "

") )
