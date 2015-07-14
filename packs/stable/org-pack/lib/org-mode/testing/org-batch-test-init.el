;;
;; Remove Org remnants built into Emacs
;;

;; clean load-path
(setq load-path
      (delq nil (mapcar
		 (function (lambda (p)
			     (unless (string-match "lisp\\(/packages\\)?/org$" p)
			       p)))
		 load-path)))
;; remove property list to defeat cus-load and remove autoloads
(mapatoms (function  (lambda (s)
		       (let ((sn (symbol-name s)))
			 (when (string-match "^\\(org\\|ob\\|ox\\)\\(-.*\\)?$" sn)
			   (setplist s nil)
			   (when (eq 'autoload (car-safe s))
			     (unintern s)))))))

;; we should now start from a clean slate
