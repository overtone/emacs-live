


(when (not (file-exists-p (concat (live-pack-lib-dir) "js2-mode.elc")))
  (byte-compile-file (concat (live-pack-lib-dir) "js2-mode.el"))
  (delete-other-windows))


(require 'js2-mode)

;(live-add-pack-lib "js2-mode")
(autoload 'js2-mode "js2-mode.el" nil t)

(add-to-list 'auto-mode-alist '("\\.js\\(on\\)?$" . js2-mode))
