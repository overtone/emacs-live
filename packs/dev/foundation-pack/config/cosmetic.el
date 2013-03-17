;;make sure ansi colour character escapes are honoured
(require 'ansi-color)
(ansi-color-for-comint-mode-on)

(setq font-lock-maximum-decoration t
      color-theme-is-global t)

;; Line-wrapping
(set-default 'fill-column 72)

;get rid of clutter
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;remove bells
(setq ring-bell-function 'ignore)

;; default darwin font
(require 'cl)

(defun live-set-default-darwin-font (font-string)
  (interactive "MNew darwin default font: ")
  (setq default-frame-alist
        (remove-if (lambda (x)
                     (eq 'font (car x)))
                   default-frame-alist))
  (cond
   ((and (window-system) (eq system-type 'darwin))
    (add-to-list 'default-frame-alist (cons 'font font-string)))))

(live-set-default-darwin-font "Menlo-12")

;; make fringe smaller
(if (fboundp 'fringe-mode)
    (fringe-mode 4))
