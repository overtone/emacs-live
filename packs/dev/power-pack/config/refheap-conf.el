;;Create a file called ~/.refheap-pass with the following (use your own credentials):
;;(setq refheap-token "your token"
;;      refheap-user "your username")

(live-add-pack-lib "refheap")
(if (file-exists-p "~/.refheap-pass")
    (load "~/.refheap-pass"))

(require 'refheap)
