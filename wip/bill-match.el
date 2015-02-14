(defun str->heatmap (s)
  (let ((r '())
        (bonus 0))
    (cl-loop for c in (str->chars s)
             do (setq r (append r (list c))
                 ))
    r))


(str->heatmap "abc")

(print (concat "a" 66))

(defun string->chars (s)
  (if (= 0 (length ""))
      '()
    (append (list (substring s 0 1)) (string->chars (substring s 1)))))

(append '( "a") '("b"))

(string->chars "abc")
(append (list (substring "abc" 0 1)) '())

(defun str->chars (s)
  (butlast (rest  (split-string s ""))))

(print  (str->chars "\\this\\is a-test-of+this function.php"))
