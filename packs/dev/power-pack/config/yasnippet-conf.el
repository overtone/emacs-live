(live-add-pack-lib "yasnippet")
(require 'yasnippet)
(setq yas/snippet-dirs `(,(concat live-etc-dir "snippets")))
(yas/global-mode 1)
