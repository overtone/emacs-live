;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(live-add-pack-lib "company-mode")
(require 'company)

(setq company-idle-delay 0)

(live-add-pack-lib "pos-tip")
(live-add-pack-lib "company-quickhelp")
(require 'company-quickhelp)
(company-quickhelp-mode 1)
