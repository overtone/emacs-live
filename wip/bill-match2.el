(defun str->chrlist (s)
  (butlast (rest  (split-string s ""))))

(str->chrlist "\\abc\\test.php")

(setq f "\\rjm\\SomeDir\\omeIrJm.php")

(defun is-sep? (c)
  (eq 0 (string-match-p "^[\\/.-]$" c)))

(defun is-cap? (c)
  (eq 0 (let ((case-fold-search nil))
          (string-match-p "^[A-Z]$" c))))

(defun str->heatmap (f)
  (cl-loop for c in (str->chrlist f )
               with r = '()
               with b = 3
               do (progn
                    (setq r (append r
                                    (list (+  (cond ((is-cap? c) 10)
                                                    ((is-sep? c) 1)
                                                    (t -1))
                                              b)
                                          )))
                    (setq b (cond ((is-sep? c) 3)
                                  ((is-cap? c) 0)
                                  (t (min -1 (- b 1))))))
               finally return r))

(str->heatmap f)


(is-cap? "A")

(is-cap? "d")

(is-sep? "-")

(string-match "[A-Z]" "---d--e")

(str->heatmap f)

(print  (str->heatmap "thisIs-Atest-of-/the...americanBr.o.a.d.cast"))

(defun b-flx-process-cache (str cache)
  "Get calculated heatmap from cache, add it if necessary."
  (let ((res (when cache
               (gethash str cache))))
    (or res
        (progn
          (setq res (flx-get-hash-for-string
                     str
                     (or (and cache (gethash 'heatmap-func cache))
                         'str->heatmap)))
          (when cache
            (puthash str res cache))
          res))))

(defun b-flx-score (str query &optional cache)
  "return best score matching QUERY against STR"
  (let ((query (replace-regexp-in-string "\s+" "" query)))
    (let ((result (or (unless (or (zerop (length query))
                                  (zerop (length str)))
                        (let* ((info-hash (b-flx-process-cache str cache))
                               (heatmap (gethash 'heatmap info-hash))
                               (matches (flx-get-matches info-hash query))
                               (best-score nil))
                          (mapc (lambda (match-positions)
                                  (let ((score 0)
                                        (contiguous-count 0)
                                        last-match)
                                    (cl-loop for index in match-positions
                                             do (progn
                                                  (if (and last-match
                                                           (= (1+ last-match) index))
                                                      (cl-incf contiguous-count)
                                                    (setq contiguous-count 0))

                                                  (if (> contiguous-count 0)
                                                      (cl-incf score (+ 1 (* 2 (min contiguous-count 4))))
                                                    (cl-incf score (aref heatmap index)))

                                                  (if (< score -1)
                                                      (cl-return score))

                                                  (setq last-match index)))
                                    (if (or (null best-score)
                                            (> score (car best-score)))
                                        (setq best-score (cons score match-positions)))))
                                matches)
                          best-score)) (list 0))))
                                        ;    (if (> (car result) 20) (print (concat str " " query " " (int-to-string (car result)))))
      result)))



(b-flx-score "Bobby" "Bo b")

(b-flx-score "Bobby" "")
(or nil 0)

(flx-get-heatmap-str "Hello!")
(str->heatmap "Hello!")

(makunbound 'dummycache)

(defvar dummycache nil)

(print  (flx-process-cache "aaab" dummycache))

(setq sss "/home/bill/Service-Broker/core.clj")

(setq hm1 (str->heatmap sss))

(defun find-next-match
  (query string heatmap offset sofar)
  (let ((sofar1 (or sofar '(-100 . -1))))
    (if (> offset (length string))
        sofar1
      (let ((idx (string-match query string offset)))
        (if (not idx)
            sofar1
          (let* ((score (nth idx heatmap))
                 (lscore (+ (length query) score))
                 (newval (if (> lscore (car sofar1))
                             (cons lscore idx)
                           sofar1)))
            (find-next-match query string heatmap (+ 1 idx) newval)))))))

(defun find-best-match
  (qlist string heatmap offset sofar)
  (if (cdr qlist)
      (let* ((sofar1 (or sofar '(0 . ())))
             (next (find-next-match (car qlist) string heatmap offset nil))
             (match (cons (+ (car sofar1) (car next))
                          (append (cdr sofar1) (list  (cdr next))))))
        (find-best-match (cdr qlist) string heatmap (+ 1 (cdr next)) match))
    sofar))

(defun find-bestest
  (query string)
  (find-best-match (split-string query " ")
                   string
                   (str->heatmap string)
                   0
                   nil))

(find-bestest "ho S rok clj" sss)

(find-best-match '("ho" "S" "rok" "clj") sss hm1 nil)

(find-next-match "ho" sss hm1 0 nil)

(append (list 1) (list  2))



(append '(1) '(2))

(nth 0 hm1)

(or nil '(0 . -1))
