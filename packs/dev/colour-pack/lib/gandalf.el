;; Gandalf Colour Theme
;;
;; "I will not say do not weep, for not all tears are an evil."
;;                              Gandalf.

(require 'color-theme)

;;;###autoload
(defun color-theme-gandalf ()
  "Gandalf colour theme by Sam Aaron"
  (interactive)
  (color-theme-install
   '(color-theme-gandalf
     ((background-color . "grey90")
      (background-mode . light)
      (border-color . "grey95")
      (cursor-color . "darkred")
      (foreground-color . "black")
      (mouse-color . "sienna1"))
     (default ((t (:background "white" :foreground "black"))))
     (blue ((t (:foreground "blue"))))
     (bold ((t (:bold t))))
     (bold-italic ((t (:bold t))))
     (border-glyph ((t (nil))))
     (buffers-tab ((t (:background "white" :foreground "black"))))
     (font-lock-builtin-face ((t (:foreground "blue" :bold t))))
     (font-lock-comment-face ((t (:italic t :foreground "grey50" ))))
     (font-lock-constant-face ((t (:foreground "dark blue"))))
     (font-lock-doc-string-face ((t (:foreground "black"))))
     (font-lock-function-name-face ((t (:foreground "deep pink" :bold t))))
     (font-lock-keyword-face ((t (:foreground "black" :bold t))))
     (font-lock-preprocessor-face ((t (:foreground "black"))))
     (font-lock-reference-face ((t (:foreground "dark cyan"))))

     (font-lock-regexp-grouping-backslash ((t (:foreground "black"))))
     (font-lock-regexp-grouping-construct ((t (:foreground "black"))))

     (font-lock-string-face ((t (:foreground "dark green"  :bold t))))

     (window-number-face ((t (:background "deep pink" :foreground "black"))))

     (font-lock-type-face ((t (:foreground "blue"))))
     (font-lock-variable-name-face ((t (:foreground "deep pink" :bold t))))
     (font-lock-warning-face ((t (:bold t :foreground "black"))))
     (gui-element ((t (:background "grey40" :foreground "#96CBFE"))))
     (region ((t (:background "#758BC6"))))
     (mode-line ((t (:background "deep pink" :foreground "white"))))
     (mode-line-inactive ((t (:background "gray50" :foreground "black"))))
     (highlight ((t (:background "grey70"))))
     (isearch ((t (:background "deep pink" :foreground "black"))))
     (isearch-fail ((t (:background "red1"))))
     (query-replace ((t (:background "grey40"))))
     (hl-line ((t (:background "grey85"))))
     (Highline-face ((t (:background "SeaGreen"))))
     (italic ((t (nil))))
     (left-margin ((t (nil))))
     (text-cursor ((t (:background "yellow" :foreground "black"))))
     (toolbar ((t (nil))))
     (underline ((nil (:underline nil))))
     (vertical-border ((t (:background "white" :foreground "#333333"))))
     (zmacs-region ((t (:background "snow" :foreground "ble"))))
     (erc-default-face ((t (:foreground "dark green"))))

     (minibuffer-prompt ((t (:foreground "black" :background "grey70"))))
     (ido-first-match ((t (:foreground "black" :background "grey70"))))
     (ido-only-match ((t (:foreground "black" :background "grey95"))))
     (ido-subdir ((t (:foreground "white" :background "#758BC6"))))
     (ido-indicator ((t (:foreground "black" :background "deep pink"))))
     (ido-incomplete-regexp ((t (:foreground "black" :background "deep pink"))))
     (flx-highlight-face ((t (:foreground "black" :background "deep pink"))))

     ;; magit
     (magit-item-highlight ((t (:background "gray95"))))
     (diff-file-header ((t (:background "gray90"))))
     (magit-diff-add ((t (:foreground "chartreuse3"))))
     (magit-diff-del ((t (:foreground "violet red"))))
     (magit-section-type ((t (:foreground "deep pink"))))
     (magit-diff-hunk-header ((t (:foreground "orange"))))
     (magit-branch ((t (:foreground "DarkGoldenRod"))))

     (git-commit-summary-face ((t (:foreground "black" :background nil))))
     (git-commit-comment-heading-face ((t (:background nil :foreground "deep pink"))))
     (git-commit-summary-face ((t (:background nil :foreground "white"))))
     (git-commit-branch-face ((t (:background nil :foreground "#FF6400"))))
     (git-commit-nonempty-second-line-face ((t (:background nil :foreground "#FBDE2D"))))

     (eval-sexp-fu-flash ((t (:background "DeepPink3" :foreground "black"))))
     (cider-error-highlight-face ((t (:background "color-52"))))

     ;;rainbow-delimiters (
     (rainbow-delimiters-depth-1-face ((t (:foreground "gray50"))))
     (rainbow-delimiters-depth-2-face ((t (:foreground "black"))))
     (rainbow-delimiters-depth-3-face ((t (:foreground "deep pink"))))
     (rainbow-delimiters-depth-4-face ((t (:foreground "#4c83ff"))))
     (rainbow-delimiters-depth-5-face ((t (:foreground "light green"))))
     (rainbow-delimiters-depth-6-face ((t (:foreground "dark blue"))))
     (rainbow-delimiters-depth-7-face ((t (:foreground "dark orange"))))
     (rainbow-delimiters-depth-8-face ((t (:foreground "slate blue"))))
     (rainbow-delimiters-depth-9-face ((t (:foreground "grey10"))))
     (rainbow-delimiters-unmatched-face ((t (:foreground "white"))))

     (vhl/default-face ((t (:background "grey60"))))
     (undo-tree-visualizer-active-branch-face ((t (:foreground "deep pink" :background "grey40"))))

     (markdown-link-face ((t (:background "#FBDE2D"))))

     (git-gutter:modified ((t (:foreground "#4c83ff" :background "gray60"))) )
     (git-gutter:deleted ((t (:foreground "gray10" :background "gray60"))) )
     (git-gutter:added ((t (:foreground "#61CE3C" :background "gray60" ))) )
     (git-gutter:unchanged ((t (:background "gray60" ))) )

     (term-color-black ((t (:background "white" :foreground "black"))))
     (term-color-blue ((t (:background "blue2" :foreground "blue2"))))
     (term-color-cyan ((t (:background "cyan3" :foreground "cyan3"))))
     (term-color-green ((t (:background "green3" :foreground "green3"))))
     (term-color-magenta ((t (:background "magenta3" :foreground "magenta3"))))
     (term-color-red ((t (:background "red3" :foreground "red3"))))
     (term-color-white ((t (:background "white" :foreground "white"))))
     (term-color-yellow ((t (:background "yellow3" :foreground "yellow3"))))
     )
   ))

(custom-set-faces
 ;;nXhtml colours
 '(mumamo-background-chunk-major ((((class color) (background dark)) (:background "black"))))
 '(mumamo-background-chunk-submode1 ((((class color) (background dark)) (:background "black"))))

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
