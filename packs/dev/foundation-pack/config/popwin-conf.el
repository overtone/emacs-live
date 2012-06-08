(live-add-pack-lib "popwin")
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)

(setq popwin:special-display-config
      '(("*Help*") ("*Completions*" :noselect t)
        ("*compilation*" :noselect t)
        ("*Occur*" :noselect t)
        ("\\*Slime Description.*" :noselect t :regexp t :position right :width 74)
        ("*Ido Completions*" :noselect t)))
