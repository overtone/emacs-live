;;make sure ansi colour character escapes are honoured
(require 'ansi-color)
(ansi-color-for-comint-mode-on)

(setq visible-bell t
      font-lock-maximum-decoration t
      color-theme-is-global t)

;; Line-wrapping
(set-default 'fill-column 70)

;get rid of clutter
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
