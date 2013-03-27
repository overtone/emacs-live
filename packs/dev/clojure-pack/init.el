;; Clojure Pack

(require 'rainbow-delimiters)

(live-add-pack-lib "uuid")
(require 'uuid)

(live-load-config-file "paredit-conf.el")
(live-load-config-file "mic-paren-conf.el")
(live-load-config-file "highlight-flash-conf.el")
(live-load-config-file "clojure-conf.el")
(live-load-config-file "auto-complete-conf.el")
(live-load-config-file "nrepl-conf.el")
;;(live-load-config-file "overtone-conf.el") TODO - fix for nrepl
