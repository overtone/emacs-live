;;; fuzzy.el --- Fuzzy Matching

;; Copyright (C) 2010, 2011, 2012  Tomohiro Matsuyama

;; Author: Tomohiro Matsuyama <tomo@cx4a.org>
;; Keywords: convenience
;; Version: 0.2

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'cl)
(require 'regexp-opt)

(defgroup fuzzy nil
  "Fuzzy Matching"
  :group 'convenience
  :prefix "fuzzy-")



;;; Utilities

(defun fuzzy-current-time-float ()
  (let ((time (current-time)))
    (+ (* (float (first time))
          (lsh 2 16))
       (float (second time))
       (/ (float (third time))
          1000000))))

(defmacro* fuzzy-with-stopwatch ((&optional (elapsed-name 'elapsed)) &body body)
  (declare (indent 1))
  (let ((start (gensym "START")))
    `(let ((,start (fuzzy-current-time-float)))
       (flet ((,elapsed-name () (- (fuzzy-current-time-float) ,start)))
         ,@body))))

(defun* fuzzy-add-to-list-as-sorted (list-var value &key (test '<) (key 'identity))
  (let ((list (symbol-value list-var)))
    (if (or (null list)
            (funcall test
                     (funcall key value)
                     (funcall key (car list))))
        (set list-var (cons value list))
      (while (and list
                  (cdr list)
                  (funcall test
                           (funcall key (cadr list))
                           (funcall key value)))
        (setq list (cdr list)))
      (setcdr list (cons value (cdr list))))))

(defmacro* fuzzy-with-timeout ((timeout &optional timeout-result (tick-name 'tick)) &body body)
  (declare (indent 1))
  (let ((elapsed (gensym "ELAPSED")))
    `(catch 'timeout
       (fuzzy-with-stopwatch (,elapsed)
         (flet ((,tick-name ()
                  (when (and ,timeout (< ,timeout (,elapsed)))
                    (throw 'timeout ,timeout-result))))
           ,@body)))))

(defun fuzzy-count-matches-in-string (regexp string &optional start end)
  (setq start (or start 0)
        end   (or end (length string)))
  (loop for start = start then (1+ matched)
        for matched = (let ((case-fold-search nil))
                        (string-match regexp string start))
        while (and matched (< (1+ matched) end))
        count matched))



;;; Jaro-Winkler Distance

(defun fuzzy-jaro-winkler-distance (s1 s2)
  "Compute Jaro-Winkler distance. See
http://en.wikipedia.org/wiki/Jaro-Winkler_distance."
  (let* ((l1 (length s1))
         (l2 (length s2))
         (r (max 1 (1- (/ (max l1 l2) 2))))
         (m 0)
         (tr 0)
         (p 0)
         cs1 cs2)
    (loop with seen = (make-vector l2 nil)
          for i below l1
          for c1 = (aref s1 i) do
          (loop for j from (max 0 (- i r)) below (min l2 (+ i r))
                for c2 = (aref s2 j)
                if (and (char-equal c1 c2)
                        (null (aref seen j))) do
                  (push c1 cs1)
                  (aset seen j c2)
                  (incf m)
                and return nil)
          finally
          (setq cs1 (nreverse cs1)
                cs2 (loop for i below l2
                          for c = (aref seen i)
                          if c collect c)))
    (loop for c1 in cs1
          for c2 in cs2
          if (not (char-equal c1 c2))
          do (incf tr))
    (loop for i below (min m 5)
          for c1 across s1
          for c2 across s2
          while (char-equal c1 c2)
          do (incf p))
    (if (eq m 0)
        0.0
      (setq m (float m))
      (let* ((dj (/ (+ (/ m l1) (/ m l2) (/ (- m (/ tr 2)) m)) 3))
             (dw (+ dj (* p 0.1 (- 1 dj)))))
        dw))))

;; Make sure byte-compiled.
(eval-when (eval)
  (byte-compile 'fuzzy-jaro-winkler-distance))

(defalias 'fuzzy-jaro-winkler-score 'fuzzy-jaro-winkler-distance)



;;; Fuzzy Matching

(defcustom fuzzy-match-score-function 'fuzzy-jaro-winkler-score
  "Score function for fuzzy matching."
  :type 'function
  :group 'fuzzy)

(defcustom fuzzy-match-accept-error-rate 0.10
  "Fuzzy matching error threshold."
  :type 'number
  :group 'fuzzy)

(defcustom fuzzy-match-accept-length-difference 2
  "Fuzzy matching length difference threshold."
  :type 'number
  :group 'fuzzy)

(defvar fuzzy-match-score-cache
  (make-hash-table :test 'equal :weakness t))

(defun fuzzy-match-score (s1 s2 function)
  (let ((cache-key (list function s1 s2)))
    (or (gethash cache-key fuzzy-match-score-cache)
        (puthash cache-key
                 (funcall function s1 s2)
                 fuzzy-match-score-cache))))

(defun* fuzzy-match (s1 s2 &optional (function fuzzy-match-score-function))
  "Return t if S1 and S2 are matched. FUNCTION is a function
scoring between S1 and S2. The score must be between 0.0 and
1.0."
  (and (<= (abs (- (length s1) (length s2)))
           fuzzy-match-accept-length-difference)
       (>= (fuzzy-match-score s1 s2 function)
           (- 1 fuzzy-match-accept-error-rate))))



;;; Fuzzy Completion

(defun fuzzy-all-completions (string collection)
  "`all-completions' with fuzzy matching."
  (loop with length = (length string)
        for str in collection
	for len = (min (length str) (+ length fuzzy-match-accept-length-difference))
        if (fuzzy-match string (substring str 0 len))
        collect str))



;;; Fuzzy Search

(defvar fuzzy-search-some-char-regexp
  (format ".\\{0,%s\\}" fuzzy-match-accept-length-difference))

(defun fuzzy-search-regexp-compile (string)
  (flet ((opt (n)
           (regexp-opt-charset
            (append (substring string
                               (max 0 (- n 1))
                               (min (length string) (+ n 2)))
                    nil))))
    (concat
     "\\("
     (loop for i below (length string)
           for c = (if (evenp i) (opt i) fuzzy-search-some-char-regexp)
           concat c)
     "\\|"
     (loop for i below (length string)
           for c = (if (oddp i) (opt i) fuzzy-search-some-char-regexp)
           concat c)
     "\\)")))

(defun fuzzy-search-forward (string &optional bound noerror count)
  (let ((regexp (fuzzy-search-regexp-compile string))
        match-data)
    (save-excursion
      (while (and (null match-data)
                  (re-search-forward regexp bound t))
        (if (fuzzy-match string (match-string 1))
            (setq match-data (match-data))
          (goto-char (1+ (match-beginning 1))))))
    (when match-data
      (store-match-data match-data)
      (goto-char (match-end 1)))))

(defun fuzzy-search-backward (string &optional bound noerror count)
  (let* ((regexp (fuzzy-search-regexp-compile string))
         match-data begin end)
    (save-excursion
      (while (and (null match-data)
                  (re-search-backward regexp bound t))
        (setq begin (match-beginning 1)
              end   (match-end 1))
        (store-match-data nil)
        (goto-char (max (point-min) (- begin (* (length string) 2))))
        (while (re-search-forward regexp end t)
          (if (fuzzy-match string (match-string 1))
              (setq match-data (match-data))
            (goto-char (1+ (match-beginning 1)))))
        (unless match-data
          (goto-char begin)))
    (if match-data
        (progn
          (store-match-data match-data)
          (goto-char (match-beginning 1)))
      (store-match-data nil)))))



;;; Fuzzy Incremental Search

(defvar fuzzy-isearch nil)
(defvar fuzzy-isearch-failed-count 0)
(defvar fuzzy-isearch-enabled 'on-failed)
(defvar fuzzy-isearch-original-search-fun nil)
(defvar fuzzy-isearch-message-prefix
  (concat (propertize "[FUZZY]" 'face 'bold) " "))

(defun fuzzy-isearch-activate ()
  (setq fuzzy-isearch t)
  (setq fuzzy-isearch-failed-count 0))

(defun fuzzy-isearch-deactivate ()
  (setq fuzzy-isearch nil)
  (setq fuzzy-isearch-failed-count 0))

(defun fuzzy-isearch ()
  (cond (isearch-word
         (if isearch-forward 'word-search-forward 'word-search-backward))
        (isearch-regexp
         (if isearch-forward 're-search-forward 're-search-backward))
        ((or fuzzy-isearch
             (eq fuzzy-isearch-enabled 'always)
             (and (eq fuzzy-isearch-enabled 'on-failed)
                  (null isearch-success)
                  isearch-wrapped
                  (> (incf fuzzy-isearch-failed-count) 1)))
         (unless fuzzy-isearch
           (fuzzy-isearch-activate))
         (if isearch-forward 'fuzzy-search-forward 'fuzzy-search-backward))
        (t
         (if isearch-forward 'search-forward 'search-backward))))

(defun fuzzy-isearch-end-hook ()
  (fuzzy-isearch-deactivate))

(defun turn-on-fuzzy-isearch ()
  (interactive)
  (setq fuzzy-isearch-original-search-fun isearch-search-fun-function)
  (setq isearch-search-fun-function 'fuzzy-isearch)
  (add-hook 'isearch-mode-end-hook 'fuzzy-isearch-end-hook))

(defun turn-off-fuzzy-isearch ()
  (interactive)
  (setq isearch-search-fun-function fuzzy-isearch-original-search-fun)
  (remove-hook 'isearch-mode-end-hook 'fuzzy-isearch-end-hook))

(defadvice isearch-message-prefix (after fuzzy-isearch-message-prefix activate)
  (if fuzzy-isearch
      (setq ad-return-value (concat fuzzy-isearch-message-prefix ad-return-value))
    ad-return-value))



;;; QuickSilver's Abbreviation Scoring

(defun fuzzy-quicksilver-make-abbrev-regexp (abbrev)
  (concat "^"
          (loop for char across (downcase abbrev) concat
                (format ".*?\\(%s\\)"
                        (regexp-quote (string char))))))

(defun fuzzy-quicksilver-abbrev-penalty (string skip-start skip-end)
  (let ((skipped (- skip-end skip-start)))
    (cond
     ((zerop skipped) 0)
     ((string-match "[ \\t\\r\\n_-]+$" (substring string skip-start skip-end))
      (let ((seps (- (match-end 0) (match-beginning 0))))
        (+ seps (* (- skipped seps) 0.15))))
     ((let ((case-fold-search nil))
        (eq (string-match "[[:upper:]]" string skip-end) skip-end))
      (let ((ups (let ((case-fold-search nil))
                   (fuzzy-count-matches-in-string
                    "[[:upper:]]" string skip-start skip-end))))
        (+ ups (* (- skipped ups) 0.15))))
     (t skipped))))

(defun fuzzy-quicksilver-abbrev-score-nocache (string abbrev)
  (cond
   ((zerop (length abbrev))             0.9)
   ((< (length string) (length abbrev)) 0.0)
   ((let ((regexp (fuzzy-quicksilver-make-abbrev-regexp abbrev))
          (case-fold-search t))
      (string-match regexp string))
    (loop with groups = (cddr (match-data))
          while groups
          for prev    = 0 then end
          for start   = (pop groups)
          for end     = (pop groups)
          for matched = (- end start)
          for skipped = (- start prev)
          for penalty = (fuzzy-quicksilver-abbrev-penalty string prev start)
          sum (+ matched (- skipped penalty)) into point
          finally return
          (let* ((length (length string))
                 (rest (- length end)))
            (/ (+ point (* rest 0.9)) (float length)))))
   (t 0.0)))

;; Make sure byte-compiled.
(eval-when (eval)
  (byte-compile 'fuzzy-quicksilver-abbrev-score-nocache))

(defvar fuzzy-quicksilver-abbrev-score-cache
  (make-hash-table :test 'equal :weakness t))

(defun fuzzy-quicksilver-abbrev-score (string abbrev)
  (let ((cache-key (cons string abbrev)))
    (or (gethash cache-key fuzzy-quicksilver-abbrev-score-cache)
        (puthash cache-key
                 (fuzzy-quicksilver-abbrev-score-nocache string abbrev)
                 fuzzy-quicksilver-abbrev-score-cache))))

(defun* fuzzy-quicksilver-realtime-abbrev-score (list
                                                 abbrev
                                                 &key
                                                 limit
                                                 timeout
                                                 (quality 0.7)
                                                 &aux new-list)
  (fuzzy-with-timeout (timeout (nreverse new-list))
    (loop with length = 0
          for string in list
          for score = (fuzzy-quicksilver-abbrev-score string abbrev)
          if (>= score quality) do
          (fuzzy-add-to-list-as-sorted
           'new-list (cons string score)
           :test '<
           :key 'cdr)
          (incf length)
          if (and limit (> length limit)) do
          (pop new-list)
          (setq length limit)
          do (tick)
          finally return (nreverse new-list))))

(provide 'fuzzy)
;;; fuzzy.el ends here
