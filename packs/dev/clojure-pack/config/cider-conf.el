(live-add-pack-lib "cider")
(live-add-pack-lib "spinner-el")
(live-add-pack-lib "seq-el")

(require 'cider)
(require 'cider-apropos)
(require 'cider-macroexpansion)
(require 'cider-browse-ns)
(require 'cider-classpath)
(require 'cider-grimoire)
(require 'cider-test)

(defun live-windows-hide-eol ()
 "Do not show ^M in files containing mixed UNIX and DOS line endings."
 (interactive)
 (setq buffer-display-table (make-display-table))
 (aset buffer-display-table ?\^M []))

(when (eq system-type 'windows-nt)
  (add-hook 'nrepl-mode-hook 'live-windows-hide-eol ))

(add-hook 'cider-mode-hook 'eldoc-mode)
(add-hook 'cider-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'eldoc-mode)

(setq cider-popup-stacktraces t)
(setq cider-popup-stacktraces-in-repl t)
(add-to-list 'same-window-buffer-names "*cider*")
(setq cider-overlays-use-font-lock t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AR - see https://github.com/overtone/emacs-live/issues/219 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specify the print length to be 100 to stop infinite sequences killing
;; things. This might be dangerous for some people relying on
;; *print-length* being larger. Consider a work around
;; (defun live-nrepl-set-print-length ()
  ;; (nrepl-send-string-sync "(set! *print-length* 100)" "clojure.core"))

;; (add-hook 'nrepl-connected-hook 'live-nrepl-set-print-length)

;; Pull in the awesome clj-refactor lib by magnars
(live-add-pack-lib "jump-el")
(live-add-pack-lib "hydra")
(live-add-pack-lib "clj-refactor")
(require 'clj-refactor)
(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               (cljr-add-keybindings-with-prefix "C-c C-m")))
