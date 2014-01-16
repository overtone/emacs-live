;; - keep in sync dir-locals.el (for Emacs >= 24) with .dir-settings.el
;;   (for Emacs < 24)
;; - don't use a symbolic link to prevent problems on cygwin
;;   distributions (commit 971b9eeacd38959439ddaa7c650430cc2dcb673e)

((nil . ((indent-tabs-mode . t)
	 (tab-width . 8)
	 (fill-column . 70)
	 (sentence-end-double-space . t))))
