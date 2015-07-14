;; Cyberpunk Colour Theme
;;
;; "and he'd still see the matrix in his sleep, bright lattices of logic
;; unfolding across that colorless void..."
;;                   William Gibson, Neuromancer.
;;

(require 'color-theme)

;;;###autoload
(defun color-theme-cyberpunk ()
  "Cyberpunk colour theme by Sam Aaron."
  (interactive)
  (color-theme-install
   '(color-theme-cyberpunk
     ((background-color . "black")
      (background-mode . dark)
      (border-color . "black")
      (cursor-color . "#96CBFE")
      (foreground-color . "#EDEDED")
      (mouse-color . "sienna1"))

     (default ((t (:background "black" :foreground "light gray"))))
     (blue ((t (:foreground "blue"))))
     (bold ((t (:bold t))))
     (bold-italic ((t (:bold t))))
     (border-glyph ((t (nil))))
     (buffers-tab ((t (:background "#0C1021" :foreground "#F8F8F8"))))


     (font-lock-builtin-face ((t (:foreground "#FF6400"))))
     (font-lock-comment-face ((t (:italic t :foreground "#8B8989"))))
     (font-lock-constant-face ((t (:foreground "#4c83ff"))))
;;     (font-lock-doc-string-face ((t (:foreground "DarkOrange"))))
     (font-lock-doc-face ((t (:italic t :background "grey10" :foreground "white"))))
     (font-lock-function-name-face ((t (:foreground "deep pink"))))
     (font-lock-keyword-face ((t (:foreground "#FBDE2D"))))
     (font-lock-preprocessor-face ((t (:foreground "gray57"))))
     (font-lock-reference-face ((t (:foreground "medium slate blue"))))
     (font-lock-reference-face ((t (:foreground "gray"))))
     (font-lock-regexp-grouping-backslash ((t (:foreground "#E9C062"))))
     (font-lock-regexp-grouping-construct ((t (:foreground "red"))))
     (font-lock-string-face ((t (:foreground "#61CE3C"))))
     (font-lock-type-face ((t (:foreground "#D8FA3C"))))
     (font-lock-variable-name-face ((t (:foreground "#D8FA3C"))))
     (font-lock-warning-face ((t (:bold t :foreground "Pink"))))

     (gui-element ((t (:background "gray10" :foreground "#96CBFE"))))
;;     (region ((t (:foreground "black" :background "#7F073F"))))
     (region ((t ( :background "#7F073F"))))
     (mode-line ((t (:background "gray10" :foreground "#4c83ff"))))
     (mode-line-inactive ((t (:background "gray10" :foreground "gray30"))))

     (highlight ((t (:background "DarkOrange"))))
     (isearch ((t (:background "deep pink" :foreground "black"))))
     (isearch-fail ((t (:background "red4"))))
     (lazy-highlight ((t (:background "yellow" :foreground "black"))))
     (next-error ((t (:background "deep pink" :foreground "black"))))
     (query-replace ((t (:background "gray10"))))
     (Highline-face ((t (:background "SeaGreen"))))
     (hl-line ((t (:background "gray10"))))
     (italic ((t (nil))))
     (left-margin ((t (nil))))
     (text-cursor ((t (:background "yellow" :foreground "black"))))
     (toolbar ((t (nil))))
     (underline ((nil (:underline nil))))
     (vertical-border ((t (:background "black" :foreground "gray10"))))
     (zmacs-region ((t (:background "snow" :foreground "ble"))))

     (diff-added ((t (:foreground "green"))))
     (diff-removed ((t (:foreground "red"))))
     (diff-header ((t (:background "gray10" :foreground "#4c83ff"))))
     (diff-file-header ((t (:background "gray10" :foreground "deep pink"))))


     (magit-diff-add ((t (:foreground "green"))))
     (magit-diff-del ((t (:foreground "red"))))
     (magit-item-highlight ((t (:background "grey15"))))
     (magit-log-sha1 ((t (:foreground "#8B8989"))))
     (magit-log-author ((t (:foreground "#4c83ff"))))
     (magit-section-title ((t (:foreground "deep pink"))))
     (magit-diff-hunk-header ((t (:foreground "orange"))))
     (magit-log-head-label-tags ((t (:background "deep pink" :foreground "black"))))
     (magit-log-head-label-local ((t (:background "#FF6400" :foreground "black"))))
     (magit-log-head-label-remote ((t (:background "#61CE3C" :foreground "black"))))
     (magit-log-head-label-head ((t (:background "yellow" :foreground "black"))))
     (magit-branch ((t (:foreground "gold"))))

     (eval-sexp-fu-flash ((t (:background "grey15" :foreground "DeepPink3"))))

     (ac-completion-face ((t (:foreground "darkgray" :underline t))))
     (ac-candidate-face ((t (:background "gray60" :foreground "black"))))
     (ac-selection-face ((t (:background "deep pink" :foreground "black"))))
     (ac-yasnippet-candidate-face ((t (:background "gray60" :foreground "black"))))
     (ac-yasnippet-selection-face ((t (:background "deep pink" :foreground "black"))))
     (popup-isearch-match ((t (:background "black" :foreground "deep pink"))))
     (popup-tip-face ((t (:background "gray10" :foreground "white"))))
     (popup-scroll-bar-foreground-face ((t (:background "#0A0A0A"))))
     (popup-scroll-bar-background-face ((t (:background "gray10"))))

     (window-number-face ((t (:background "grey10" :foreground "#4c83ff"))))

     (yas/field-highlight-face ((t (:background "deep pink" :foreground "black"))))

     (show-paren-match-face ((t (:background "deep pink" :foreground "black"))))

     (naeu-green-face ((t (:foreground "green" :background "black"))))
     (naeu-pink-face ((t (:foreground "deep pink" :background "black"))))
     (naeu-blue-face ((t (:foreground "medium slate blue" :background "black"))))
     (naeu-orange-face ((t (:foreground "#FBDE2D" :background "black"))))
     (naeu-red-face ((t (:foreground "orange" :background "black"))))
     (naeu-grey-face ((t (:foreground "gray30" :background "black"))))

     (ido-first-match ((t (:foreground "deep pink" :background "black"))))
     (ido-only-match ((t (:foreground "deep pink" :background "black"))))
     (ido-subdir ((t (:foreground "gray60" :background "black"))))
     (ido-indicator ((t (:foreground "black" :background "deep pink"))))
     (flx-highlight-face ((t (:foreground "deep pink" :background "black"))))

     (match ((t (:foreground "deep pink" :background "blackn"))))
     (minibuffer-prompt ((t (:foreground "#61CE3C" :background "black"))))
     (grep-match-face ((t (:foreground "black" :background "deep pink"))))
     (grep-hit-face ((t (:foreground "black" :background "red"))))
     (grep-context-face ((t (:foreground "black" :background "deep pink"))))

     ;; ;;rainbow-delimiters [ {
     ;; (rainbow-delimiters-depth-1-face ((t (:foreground "dark gray"))))
     ;; (rainbow-delimiters-depth-2-face ((t (:foreground "green"))))
     ;; (rainbow-delimiters-depth-3-face ((t (:foreground "gold"))))
     ;; (rainbow-delimiters-depth-4-face ((t (:foreground "turquoise"))))
     ;; (rainbow-delimiters-depth-5-face ((t (:foreground "orange"))))
     ;; (rainbow-delimiters-depth-6-face ((t (:foreground "slate blue"))))
     ;; (rainbow-delimiters-depth-7-face ((t (:foreground "yellow"))))
     ;; (rainbow-delimiters-depth-8-face ((t (:foreground "light blue"))))
     ;; (rainbow-delimiters-depth-9-face ((t (:foreground "#7f7f7f"))))
     ;; (rainbow-delimiters-unmatched-face ((t (:foreground "white"))))

     ;;rainbow-delimiters (
     (rainbow-delimiters-depth-1-face ((t (:foreground "dark red"))))
     (rainbow-delimiters-depth-2-face ((t (:foreground "dark green"))))
     (rainbow-delimiters-depth-3-face ((t (:foreground "deep pink"))))
     (rainbow-delimiters-depth-4-face ((t (:foreground "yellow"))))
     (rainbow-delimiters-depth-5-face ((t (:foreground "green"))))
     (rainbow-delimiters-depth-6-face ((t (:foreground "light blue"))))
     (rainbow-delimiters-depth-7-face ((t (:foreground "orange"))))
     (rainbow-delimiters-depth-8-face ((t (:foreground "slate blue"))))
     (rainbow-delimiters-depth-9-face ((t (:foreground "light gray"))))
     (rainbow-delimiters-unmatched-face ((t (:foreground "white"))))

     (erc-notice-face ((t (:bold t :foreground "grey26"))))

     (erc-action-face ((t (:foreground "#FF6400"))))
;;     (erc-bold-face ((t (:bold t :weight bold))))
;;     (erc-button ((t (:bold t :weight bold))))
;;     (erc-command-indicator-face ((t (:bold t :weight bold))))
     (erc-current-nick-face ((t (:foreground "#FBDE2D"))))
     (erc-dangerous-host-face ((t (:foreground "red"))))
     (erc-default-face ((t (:foreground "#61CE3C"))))
     (erc-direct-msg-face ((t (:foreground "orange"))))
     (erc-error-face ((t (:foreground "red"))))
     (erc-fool-face ((t (:foreground "dim gray"))))
     (erc-header-line ((t (:background "grey90" :foreground "grey20"))))
     (erc-input-face ((t (:foreground "#4c83ff"))))
     (erc-inverse-face ((t (:background "Black" :foreground "White"))))
     (erc-keyword-face ((t (:foreground "deep pink"))))
     (erc-my-nick-face ((t (:bold t :foreground "deep pink" ))))
     (erc-nick-default-face ((t (:foreground "grey57"))))
     (erc-nick-msg-face ((t (:foreground "deep pink"))))

     (erc-pal-face ((t (:bold t :foreground "Magenta" :weight bold))))
     (erc-prompt-face ((t (:bold t :background "lightBlue2" :foreground "Black" :weight bold))))
     (erc-timestamp-face ((t (:foreground "dim gray"))))
     (erc-underline-face ((t (:underline t))))

     (vhl/default-face ((t (:background "gray10"))))
     (undo-tree-visualizer-active-branch-face ((t (:foreground "deep pink" :background "black"))))
     (hl-sexp-face ((t (:background "grey9"))))

     (browse-kill-ring-current-entry-face ((t (:background "gray10"))))

     (markdown-link-face ((t (:foreground "#FBDE2D"))))
     (markdown-url-face ((t (:foreground  "#61CE3C"))))
     (markdown-bold-face ((t (:foreground "#FF6400"))))
     (markdown-italic-face ((t (:italic t :foreground "#FF6400"))))
     (markdown-pre-face ((t (:foreground "#4c83ff"))))
     (markdown-inline-code-face ((t (:foreground "#4c83ff"))))
     (markdown-list-face ((t (:foreground "#8B8989"))))

     (git-gutter:modified ((t (:foreground "#4c83ff" :background "gray5"))) )
     (git-gutter:deleted ((t (:foreground "gray10" :background "gray5"))) )
     (git-gutter:added ((t (:foreground "#61CE3C" :background "gray5" ))) )
     (git-gutter:unchanged ((t (:background "grey5" ))) )

     (glsl-type-face ((t (:foreground "#4c83ff"))))
     (glsl-keyword-face ((t (:foreground "#FBDE2D"))))
     (glsl-variable-name-face ((t (:foreground "grey57"))))
     (glsl-builtin-face ((t (:foreground "#FF6400"))))

     (term-bold ((t (:bold t :weight bold))))
     (term-color-black ((t (:background "black" :foreground "black"))))
     (term-color-blue ((t (:background "#4c83ff" :foreground "#4c83ff"))))
     (term-color-cyan ((t (:background "cyan3" :foreground "cyan3"))))
     (term-color-green ((t (:background "green3" :foreground "green3"))))
     (term-color-magenta ((t (:background "magenta3" :foreground "magenta3"))))
     (term-color-red ((t (:background "red3" :foreground "red3"))))
     (term-color-white ((t (:background "white" :foreground "white"))))
     (term-color-yellow ((t (:background "yellow3" :foreground "yellow3"))))
     (term-underline ((t (:underline t))))

     (git-commit-comment-heading-face ((t (:background "black" :foreground "deep pink"))))
     (git-commit-summary-face ((t (:background "black" :foreground "white"))))
     (git-commit-branch-face ((t (:background "black" :foreground "#FF6400"))))
     (git-commit-nonempty-second-line-face ((t (:background "black" :foreground "#FBDE2D"))))

     (sp-wrap-overlay-face ((t (:background "green"))))
     (sp-pair-overlay-face ((t (:background "grey20"))))
     (sp-wrap-tag-overlay-face ((t (:background "grey20"))))

     (apropos-symbol ((t (:foreground "deep pink"))))
     (apropos-function-button ((t (:foreground "#4c83ff"))))
     (apropos-misc-button ((t (:foreground "yellow3"))))
     (apropos-variable-button ((t (:foreground "dark green"))))


     (cider-error-highlight-face ((t (:background "color-52"))))
     (cider-repl-err-output-face ((t (:foreground "deeppink"))))
     (cider-stacktrace-filter-shown-face ((t (:foreground "#4c83ff"))))
     ))
  )

(custom-set-faces

 ;;nXhtml colours
 '(mumamo-background-chunk-major ((((class color) (background dark)) (:background "black"))))
 '(mumamo-background-chunk-submode1 ((((class color) (background dark)) (:background "black"))))

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
