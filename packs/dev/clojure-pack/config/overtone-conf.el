(defun live-overtone-display-log ()
  (interactive)
  (let* ((log-path (file-truename "~/.overtone/log/overtone.log")))
    (if (live-file-open-as-buffer-p log-path)
        (popwin:display-buffer (live-find-buffer-by-path log-path))
      (progn
        (let* ((buf (find-file-noselect log-path)))
          (popwin:display-buffer buf))))))

(defun live-overtone-stop ()
  (interactive)
  (slime-eval-async `(swank:eval-and-grab-output "(in-ns 'overtone.sc.server) (stop)")))
