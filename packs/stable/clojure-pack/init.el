;; Clojure Pack

(require 'rainbow-delimiters)
(require 'paredit)

(live-add-pack-lib "uuid")
(require 'uuid)

(live-load-config-file "paredit-conf.el")
(live-load-config-file "mic-paren-conf.el")
(live-load-config-file "auto-complete-conf.el")
(live-load-config-file "clojure-conf.el")
(live-load-config-file "cider-conf.el")
(live-load-config-file "highlight-flash-conf.el")
(live-load-config-file "clojure-test-conf.el")

;;(live-load-config-file "overtone-conf.el") TODO - fix for cider
