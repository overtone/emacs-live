;; takes a file containing edn file names, one per line
;;
;;    locate *.edn > edn.list
;;
;; results end up as edn in *edn-parse-time-results*
(with-current-buffer (find-file-noselect "edn.list")
  (goto-char 1)
  (while (and (< (point) (point-max)))
    (end-of-line)
    (let* ((fn (buffer-substring-no-properties (line-beginning-position) (point)))
           (buff (find-file-noselect fn))
           (edn-time 0)
           (clj-time 0))
      ;;(message fn)
      (with-current-buffer buff
        (let ((start (time-to-seconds (current-time))))
          (parseedn-read)
          (setq clj-time (+ clj-time (- (time-to-seconds (current-time)) start))))
        (goto-char 1)
        (let ((start (time-to-seconds (current-time))))
          (edn-read)
          (setq edn-time (+ edn-time (- (time-to-seconds (current-time)) start)))))
      (kill-buffer buff)
      (when (< (point) (point-max)) (right-char))
      (with-current-buffer "*edn-parse-time-results*"
        (insert "{:file \"" fn "\", :edn-time " (number-to-string edn-time) ", :clj-time " (number-to-string clj-time) "}\n")))))
