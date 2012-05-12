;; Gandalf Colour Theme
(require 'color-theme)

;;;###autoload
(defun color-theme-gandalf()
  "Gandalf colour theme by Sam Aaron"
  (interactive)
  (color-theme-install
   '(color-theme-gandalf
     ((background-color . "#AEAEAE")
      (background-mode . light)
      (border-color . "white")
      (cursor-color . "#96CBFE")
      (foreground-color . "#EDEDED")
      (mouse-color . "sienna1"))
     (default ((t (:background "white" :foreground "black"))))
     (blue ((t (:foreground "blue"))))
     (bold ((t (:bold t))))
     (bold-italic ((t (:bold t))))
     (border-glyph ((t (nil))))
     (buffers-tab ((t (:background "white" :foreground "black"))))
     (font-lock-builtin-face ((t (:foreground "black")))) ;; light blue
     (font-lock-comment-face ((t (:italic t :foreground "black"))))
     (font-lock-constant-face ((t (:foreground "black"))))
     (font-lock-doc-string-face ((t (:foreground "black"))))
     (font-lock-function-name-face ((t (:foreground "black"))))
     (font-lock-keyword-face ((t (:foreground "black"))))
     (font-lock-preprocessor-face ((t (:foreground "black"))))
     (font-lock-reference-face ((t (:foreground "black"))))

     (font-lock-regexp-grouping-backslash ((t (:foreground "black"))))
     (font-lock-regexp-grouping-construct ((t (:foreground "black"))))

     (font-lock-string-face ((t (:foreground "black"))))

     (font-lock-type-face ((t (:foreground "black"))))
     (font-lock-variable-name-face ((t (:foreground "#FF6400"))))
     (font-lock-warning-face ((t (:bold t :foreground "black"))))
     (gui-element ((t (:background "#333333" :foreground "#96CBFE"))))
     (region ((t (:background "#253B76"))))
     (mode-line ((t (:background "#333333" :foreground "#96CBFE"))))
     (highlight ((t (:background "#222222"))))
     (Highline-face ((t (:background "SeaGreen"))))
     (italic ((t (nil))))
     (left-margin ((t (nil))))
     (text-cursor ((t (:background "yellow" :foreground "black"))))
     (toolbar ((t (nil))))
     (underline ((nil (:underline nil))))
     (vertical-border ((t (:background "black" :foreground "#333333"))))
     (zmacs-region ((t (:background "snow" :foreground "ble")))))))

(custom-set-faces
 ;;magit colours
 '(magit-item-highlight ((((class color) (background dark)) (:background "gray15"))))
 '(magit-diff-add  ((((class color) (background dark)) (:foreground "chartreuse3"))))
 '(magit-diff-del ((((class color) (background dark)) (:foreground "violet red"))))
 '(magit-section-title ((((class color) (background dark)) (:foreground "deep pink"))))
 '(magit-diff-hunk-header ((((class color) (background dark)) (:foreground "orange"))))
 '(magit-branch ((((class color) (background dark)) (:foreground "gold"))))

 ;;nXhtml colours
 '(mumamo-background-chunk-major ((((class color) (background dark)) (:background "black"))))
 '(mumamo-background-chunk-submode1 ((((class color) (background dark)) (:background "black"))))

 '(eval-sexp-fu-flash ((((class color) (background dark)) (:background "grey15" :foreground "DeepPink3"))))

 ;;diff colours
 '(diff-removed ((t (:foreground "Red"))) 'now)
 '(diff-added ((t (:foreground "Green"))) 'now)

 ;;ediff
 '(ediff-even-diff-A ((((class color) (background dark)) (:background "dark green"))))
 '(ediff-odd-diff-A ((((class color) (background dark)) (:background "dark green"))))
 '(ediff-odd-diff-B ((((class color) (background dark)) (:background "dark red"))))
 '(ediff-even-diff-B ((((class color) (background dark)) (:background "dark red"))))
; '(ediff-current-diff-B ((((class color)) (:background "white"))))
; '(ediff-even-diff-A ((((class color)) nil)))
; '(ediff-even-diff-B ((((class color)) nil)))
; '(ediff-fine-diff-A ((((class color)) (:background "cyan"))))
; '(ediff-fine-diff-B ((((class color)) (:background "cyan"))))
; '(ediff-odd-diff-A ((((class color)) nil)))
; '(ediff-odd-diff-B ((((class color)) nil)))
 )
