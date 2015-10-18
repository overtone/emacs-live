(live-add-pack-lib "squiggly-clojure/elisp/flycheck-clojure")
(live-add-pack-lib "squiggly-clojure/elisp/typed-clojure")
(live-add-pack-lib "let-alist")
(live-add-pack-lib "flycheck")
(live-add-pack-lib "flycheck-pos-tip")

(require 'let-alist)
(require 'flycheck-clojure)
(require 'clojure-typed-doc)
(require 'flycheck-pos-tip)
(require 'flycheck)

(eval-after-load 'flycheck '(flycheck-clojure-setup))
(add-hook 'after-init-hook #'global-flycheck-mode)

(eval-after-load 'flycheck
  '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
