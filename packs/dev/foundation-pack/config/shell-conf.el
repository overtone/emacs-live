(defcustom eshell-directory-name
  (let* ((dir (concat live-tmp-dir "eshell")))
    (make-directory dir t)
    dir)
  "The directory where Eshell control files should be kept."
  :type 'directory
  :group 'eshell)

;;make sure ansi colour character escapes are honoured
(require 'ansi-color)
(ansi-color-for-comint-mode-on)
