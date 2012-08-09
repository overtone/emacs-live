(setq slime-enable-evaluate-in-emacs t)

(defun live-clj-eval-sync (formstring)
  (slime-eval (list 'swank:eval-and-grab-output formstring)))

(defun live-clj-eval (formstring)
  (slime-eval-async (list 'swank:eval-and-grab-output formstring)))

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
  (live-clj-eval "(in-ns 'overtone.sc.server) (stop)"))

(defun live-overtone-volume (vol)
  (interactive "sVolume: ")
  (live-clj-eval (concat "(in-ns 'overtone.studio.mixer) (volume " vol ")")))

(setplist 'live-control-sym->overlay ())
(setplist 'live-control-overlay->sym ())

(defun live-add-live-control-overlay (overlay)
  (let ((s (gentemp ":")))
    (put 'live-control-overlay->sym overlay s)
    (put 'live-control-sym->overlay s overlay)
    (symbol-name s)))

(defun live-slime-eval-defun-surrounding-overlay (overlay)
  (save-excursion
    (save-window-excursion
      (switch-to-buffer (overlay-buffer overlay))
      (goto-char (overlay-start overlay))
      (let ((fn-at-point-escaped (replace-regexp-in-string "\"" "\\\""
                                                           (slime-defun-at-point) t t)))
        (slime-interactive-eval
         (concat "(try (eval (read-string "
                 "\"" fn-at-point-escaped "\" "
                 ")) (catch Exception e (println \"Error when evaluating top-level form after control change\")))"))))))

(defun live-control-overlay-modified (overlay after start end &rest
                                              original-len)
  (unless after
    (delete-overlay overlay)
    (live-clj-eval (concat "(in-ns 'overtone.studio.midi)
                            (remove-handler (midi-mk-control-key-keyword \"emacs-live-insert-midi-control-\"" (symbol-name (get 'live-control-overlay->sym overlay)) "))"))))

(defun live-insert-midi-control ()
  (interactive)
  (let ((m (point-marker)))
    (unless (eq (char-before) 32)
      (backward-word)
      (mark-word)
      (delete-region (region-beginning) (region-end)))
    (insert "<WAITING FOR MIDI CONTROLLER MOVEMENT>")
    (let ((new-overlay (make-overlay m (point))))
      (let ((new-overlay-sym (live-add-live-control-overlay new-overlay)))
        (overlay-put new-overlay 'face '(:background "deep pink"))
        (overlay-put new-overlay 'evaporate t)
        (overlay-put new-overlay 'modification-hooks
                     '(live-control-overlay-modified))
        (live-clj-eval (concat "(require 'overtone.core)
                                (in-ns 'overtone.studio.midi)
                               (def overlay-name " new-overlay-sym  ")"
                               "
                          (let [control-key (midi-capture-next-controller-control-key)
                                handler-key (midi-mk-control-key-keyword \"emacs-live-insert-midi-control-\"" new-overlay-sym ")]
                            (on-latest-event
                              control-key
                              (bound-fn [msg]
                                 (let [new (:data2 msg)]
                                   (swank.core/eval-in-emacs
                                     (str \"(live-update-overlay-and-eval-defun "

                               new-overlay-sym " \" new\")\"))))
                              handler-key))"))))))

(defun live-update-overlay-and-eval-defun (overlay-sym new-val)
  (let ((overlay (get 'live-control-sym->overlay overlay-sym))
        (inhibit-modification-hooks t))
    (when overlay
      (when (overlay-buffer overlay)
        (save-excursion
          (save-window-excursion
            (switch-to-buffer (overlay-buffer overlay))
            (goto-char (overlay-start overlay))
            (insert " " (int-to-string new-val) " ")
            (delete-region (point) (overlay-end overlay))
            (condition-case ex
                (live-slime-eval-defun-surrounding-overlay overlay)
              ('error (message (format "Could not eval top-level form."))))))))))
