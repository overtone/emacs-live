(live-add-pack-lib "yasnippet")
(require 'yasnippet)
(setq live-yasnippet-dir (concat live-etc-dir "snippets"))
(setq yas-snippet-dirs `(,live-yasnippet-dir))
(yas-global-mode 1)

(defun live-reload-snippets ()
  (interactive)
  (yas-load-directory live-yasnippet-dir))
