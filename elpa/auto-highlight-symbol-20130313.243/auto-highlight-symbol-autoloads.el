;;; auto-highlight-symbol-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (auto-highlight-symbol-mode global-auto-highlight-symbol-mode)
;;;;;;  "auto-highlight-symbol" "auto-highlight-symbol.el" (21641
;;;;;;  30884 699670 364000))
;;; Generated autoloads from auto-highlight-symbol.el

(defvar global-auto-highlight-symbol-mode nil "\
Non-nil if Global-Auto-Highlight-Symbol mode is enabled.
See the command `global-auto-highlight-symbol-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-auto-highlight-symbol-mode'.")

(custom-autoload 'global-auto-highlight-symbol-mode "auto-highlight-symbol" nil)

(autoload 'global-auto-highlight-symbol-mode "auto-highlight-symbol" "\
Toggle Auto-Highlight-Symbol mode in all buffers.
With prefix ARG, enable Global-Auto-Highlight-Symbol mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Auto-Highlight-Symbol mode is enabled in all buffers where
`ahs-mode-maybe' would do it.
See `auto-highlight-symbol-mode' for more information on Auto-Highlight-Symbol mode.

\(fn &optional ARG)" t nil)

(autoload 'auto-highlight-symbol-mode "auto-highlight-symbol" "\
Toggle Auto Highlight Symbol Mode

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("auto-highlight-symbol-pkg.el") (21641
;;;;;;  30884 727073 845000))

;;;***

(provide 'auto-highlight-symbol-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; auto-highlight-symbol-autoloads.el ends here
