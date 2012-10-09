(live-add-pack-lib "color-theme")
(require 'color-theme)

(global-hl-line-mode 1)

;; use blackbored colour theme
(load-file (concat (live-pack-lib-dir) "cyberpunk.el"))
(load-file (concat (live-pack-lib-dir) "gandalf.el"))

(color-theme-cyberpunk)

(set-cursor-color "yellow")
