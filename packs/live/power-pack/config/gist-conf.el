;;override pcache's default cache dir
(defvar pcache-directory
  (let ((dir (file-name-as-directory (concat live-tmp-dir "pcache"))))
    (make-directory dir t)
    dir))

(live-add-pack-lib "gist")
(live-add-pack-lib "gh")
(live-add-pack-lib "pcache")
(live-add-pack-lib "logito")
(require 'gist)
