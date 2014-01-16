;;; test-ob-lilypond.el --- tests for ob-lilypond.el

;; Copyright (c) 2010-2013 Martyn Jago
;; Authors: Martyn Jago

;; This file is not part of GNU Emacs.

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

;;; Code:
(unless (featurep 'ob-lilypond)
  (signal 'missing-test-dependency "Support for Lilypond code blocks"))

(save-excursion
  (set-buffer (get-buffer-create "test-ob-lilypond.el"))
  (setq ly-here
        (file-name-directory
         (or load-file-name (buffer-file-name)))))

(ert-deftest ob-lilypond/assert ()
  (should t))

(ert-deftest ob-lilypond/feature-provision ()
  (should (featurep 'ob-lilypond)))

(ert-deftest ob-lilypond/check-lilypond-alias ()
  (should (fboundp 'lilypond-mode)))

(ert-deftest ob-lilypond/org-babel-tangle-lang-exts ()
  (let ((found nil)
        (list org-babel-tangle-lang-exts))
    (while list
      (when (equal (car list) '("LilyPond" . "ly"))
        (setq found t))
      (setq list (cdr list)))
    (should found)))

(ert-deftest ob-lilypond/org-babel-prep-session:lilypond ()
  (should-error (org-babel-prep-session:lilypond nil nil))
  :type 'error)

(ert-deftest ob-lilypond/ly-compile-lilyfile ()
  (should (equal
           `(,(ly-determine-ly-path)    ;program
             nil                        ;infile
             "*lilypond*"               ;buffer
             t                          ;display
             ,(if ly-gen-png  "--png"  "") ;&rest...
             ,(if ly-gen-html "--html" "")
             ,(if ly-gen-pdf "--pdf" "")
             ,(if ly-use-eps  "-dbackend=eps" "")
             ,(if ly-gen-svg  "-dbackend=svg" "")
             "--output=test-file"
             "test-file.ly")
           (ly-compile-lilyfile "test-file.ly" t))))

(ert-deftest ob-lilypond/ly-compile-post-tangle ()
  (should (boundp 'ly-compile-post-tangle)))

(ert-deftest ob-lilypond/ly-display-pdf-post-tangle ()
  (should (boundp 'ly-display-pdf-post-tangle)))

(ert-deftest ob-lilypond/ly-play-midi-post-tangle ()
  (should (boundp 'ly-play-midi-post-tangle)))

(ert-deftest ob-lilypond/ly-OSX-ly-path ()
  (should (boundp 'ly-OSX-ly-path))
  (should (stringp ly-OSX-ly-path)))

(ert-deftest ob-lilypond/ly-OSX-pdf-path ()
  (should (boundp 'ly-OSX-pdf-path))
  (should (stringp ly-OSX-pdf-path)))

(ert-deftest ob-lilypond/ly-OSX-midi-path ()
  (should (boundp 'ly-OSX-midi-path))
  (should (stringp ly-OSX-midi-path)))

(ert-deftest ob-lilypond/ly-nix-ly-path ()
  (should (boundp 'ly-nix-ly-path))
  (should (stringp ly-nix-ly-path)))

(ert-deftest ob-lilypond/ly-nix-pdf-path ()
  (should (boundp 'ly-nix-pdf-path))
  (should (stringp ly-nix-pdf-path)))

(ert-deftest ob-lilypond/ly-nix-midi-path ()
  (should (boundp 'ly-nix-midi-path))
  (should (stringp ly-nix-midi-path)))

(ert-deftest ob-lilypond/ly-w32-ly-path ()
  (should (boundp 'ly-w32-ly-path))
  (should (stringp ly-w32-ly-path)))

(ert-deftest ob-lilypond/ly-w32-pdf-path ()
  (should (boundp 'ly-w32-pdf-path))
  (should (stringp ly-w32-pdf-path)))

(ert-deftest ob-lilypond/ly-w32-midi-path ()
  (should (boundp 'ly-w32-midi-path))
  (should (stringp ly-w32-midi-path)))

(ert-deftest ob-lilypond/ly-gen-png ()
  (should (boundp 'ly-gen-png)))

(ert-deftest ob-lilypond/ly-gen-svg ()
  (should (boundp 'ly-gen-svg)))

(ert-deftest ob-lilypond/ly-gen-html ()
  (should (boundp 'ly-gen-html)))

(ert-deftest ob-lilypond/ly-gen-html ()
  (should (boundp 'ly-gen-pdf)))

(ert-deftest ob-lilypond/use-eps ()
  (should (boundp 'ly-use-eps)))

(ert-deftest ob-lilypond/ly-arrange-mode ()
  (should (boundp 'ly-arrange-mode)))

;; (ert-deftest ob-lilypond/org-babel-default-header-args:lilypond ()
;;   (should (equal  '((:tangle . "yes")
;;                     (:noweb . "yes")
;;                     (:results . "silent")
;;                     (:comments . "yes"))
;;                   org-babel-default-header-args:lilypond)))

;;TODO finish...
(ert-deftest ob-lilypond/org-babel-expand-body:lilypond ()
  (should (equal "This is a test"
                 (org-babel-expand-body:lilypond "This is a test" ()))))

;;TODO (ert-deftest ly-test-org-babel-execute:lilypond ())
(ert-deftest ob-lilypond/ly-check-for-compile-error ()
  (set-buffer (get-buffer-create "*lilypond*"))
  (erase-buffer)
  (should (not (ly-check-for-compile-error nil t)))
  (insert-file-contents (concat ly-here
                                "../examples/ob-lilypond-test.error")
                        nil nil nil t)
  (goto-char (point-min))
  (should (ly-check-for-compile-error nil t))
  (kill-buffer "*lilypond*"))

(ert-deftest ob-lilypond/ly-process-compile-error ()
  (find-file-other-window (concat
                           ly-here
                           "../examples/ob-lilypond-broken.org"))
  (set-buffer (get-buffer-create "*lilypond*"))
  (insert-file-contents (concat
                         ly-here
                         "../examples/ob-lilypond-test.error")
                        nil nil nil t)
  (goto-char (point-min))
  (search-forward "error:" nil t)
  (should-error
   (ly-process-compile-error (concat
                              ly-here
                              "../examples/ob-lilypond-broken.ly"))
   :type 'error)
  (set-buffer "ob-lilypond-broken.org")
  (should (equal 238 (point)))
  (exchange-point-and-mark)
  (should (equal (+ 238 (length "line 25")) (point)))
  (kill-buffer "*lilypond*")
  (kill-buffer "ob-lilypond-broken.org"))

(ert-deftest ob-lilypond/ly-mark-error-line ()
  (let ((file-name (concat
                    ly-here
                    "../examples/ob-lilypond-broken.org"))
        (expected-point-min 198)
        (expected-point-max 205)
        (line "line 20"))
    (find-file-other-window file-name)
    (ly-mark-error-line file-name line)
    (should (equal expected-point-min (point)))

    (exchange-point-and-mark)
    (should (= expected-point-max (point)))
    (kill-buffer (file-name-nondirectory file-name))))

(ert-deftest ob-lilypond/ly-parse-line-num ()
  (with-temp-buffer
    (insert-file-contents (concat
                           ly-here
                           "../examples/ob-lilypond-test.error")
                          nil nil nil t)
    (goto-char (point-min))
    (search-forward "error:")
    (should (equal 25 (ly-parse-line-num (current-buffer))))))

(ert-deftest ob-lilypond/ly-parse-error-line ()
  (let ((ly-file (concat
                  ly-here
                  "../examples/ob-lilypond-broken.ly")))
    (should (equal "line 20"
                   (ly-parse-error-line ly-file 20)))
    (should (not (ly-parse-error-line ly-file 0)))))

(ert-deftest ob-lilypond/ly-attempt-to-open-pdf ()
  (let ((post-tangle ly-display-pdf-post-tangle)
        (ly-file (concat
                  ly-here
                  "../examples/ob-lilypond-test.ly"))
        (pdf-file (concat
                   ly-here
                   "../examples/ob-lilypond-test.pdf")))
    (setq ly-display-pdf-post-tangle t)
    (when (not (file-exists-p pdf-file))
      (set-buffer (get-buffer-create (file-name-nondirectory pdf-file)))
      (write-file pdf-file))
    (should (equal
             (concat
              (ly-determine-pdf-path) " " pdf-file)
             (ly-attempt-to-open-pdf ly-file t)))
    (delete-file pdf-file)
    (kill-buffer (file-name-nondirectory pdf-file))
    (should (equal
             "No pdf file generated so can't display!"
             (ly-attempt-to-open-pdf pdf-file)))
    (setq ly-display-pdf-post-tangle post-tangle)))

(ert-deftest ob-lilypond/ly-attempt-to-play-midi ()
  (let ((post-tangle ly-play-midi-post-tangle)
        (ly-file (concat
                  ly-here
                  "../examples/ob-lilypond-test.ly"))
        (midi-file (concat
                    ly-here
                    "../examples/ob-lilypond-test.midi")))
    (setq ly-play-midi-post-tangle t)
    (when (not (file-exists-p midi-file))
      (set-buffer (get-buffer-create (file-name-nondirectory midi-file)))
      (write-file midi-file))
    (should (equal
             (concat
              (ly-determine-midi-path) " " midi-file)
             (ly-attempt-to-play-midi ly-file t)))
    (delete-file midi-file)
    (kill-buffer (file-name-nondirectory midi-file))
    (should (equal
             "No midi file generated so can't play!"
             (ly-attempt-to-play-midi midi-file)))
    (setq ly-play-midi-post-tangle post-tangle)))

(ert-deftest ob-lilypond/ly-determine-ly-path ()
  (should (equal ly-OSX-ly-path
                 (ly-determine-ly-path "darwin")))
  (should (equal ly-w32-ly-path
                 (ly-determine-ly-path "windows-nt")))
  (should (equal ly-nix-ly-path
                 (ly-determine-ly-path "nix"))))

(ert-deftest ob-lilypond/ly-determine-pdf-path ()
  (should (equal ly-OSX-pdf-path
                 (ly-determine-pdf-path "darwin")))
  (should (equal ly-w32-pdf-path
                 (ly-determine-pdf-path "windows-nt")))
  (should (equal ly-nix-pdf-path
                 (ly-determine-pdf-path "nix"))))

(ert-deftest ob-lilypond/ly-determine-midi-path ()
  (should (equal ly-OSX-midi-path
                 (ly-determine-midi-path "darwin")))
  (should (equal ly-w32-midi-path
                 (ly-determine-midi-path "windows-nt")))
  (should (equal ly-nix-midi-path
                 (ly-determine-midi-path "nix"))))

(ert-deftest ob-lilypond/ly-toggle-midi-play-toggles-flag ()
  (if ly-play-midi-post-tangle
      (progn
        (ly-toggle-midi-play)
         (should (not ly-play-midi-post-tangle))
        (ly-toggle-midi-play)
        (should ly-play-midi-post-tangle))
    (ly-toggle-midi-play)
    (should ly-play-midi-post-tangle)
    (ly-toggle-midi-play)
    (should (not ly-play-midi-post-tangle))))

(ert-deftest ob-lilypond/ly-toggle-pdf-display-toggles-flag ()
  (if ly-display-pdf-post-tangle
      (progn
        (ly-toggle-pdf-display)
         (should (not ly-display-pdf-post-tangle))
        (ly-toggle-pdf-display)
        (should ly-display-pdf-post-tangle))
    (ly-toggle-pdf-display)
    (should ly-display-pdf-post-tangle)
    (ly-toggle-pdf-display)
    (should (not ly-display-pdf-post-tangle))))

(ert-deftest ob-lilypond/ly-toggle-pdf-generation-toggles-flag ()
  (if ly-gen-pdf
      (progn
        (ly-toggle-pdf-generation)
         (should (not ly-gen-pdf))
        (ly-toggle-pdf-generation)
        (should ly-gen-pdf))
    (ly-toggle-pdf-generation)
    (should ly-gen-pdf)
    (ly-toggle-pdf-generation)
    (should (not ly-gen-pdf))))

(ert-deftest ob-lilypond/ly-toggle-arrange-mode ()
  (if ly-arrange-mode
      (progn
        (ly-toggle-arrange-mode)
        (should (not ly-arrange-mode))
        (ly-toggle-arrange-mode)
        (should ly-arrange-mode))
    (ly-toggle-arrange-mode)
    (should ly-arrange-mode)
    (ly-toggle-arrange-mode)
    (should (not ly-arrange-mode))))

(ert-deftest ob-lilypond/ly-toggle-png-generation-toggles-flag ()
  (if ly-gen-png
      (progn
        (ly-toggle-png-generation)
         (should (not ly-gen-png))
        (ly-toggle-png-generation)
        (should ly-gen-png))
    (ly-toggle-png-generation)
    (should ly-gen-png)
    (ly-toggle-png-generation)
    (should (not ly-gen-png))))

(ert-deftest ob-lilypond/ly-toggle-html-generation-toggles-flag ()
  (if ly-gen-html
      (progn
        (ly-toggle-html-generation)
         (should (not ly-gen-html))
        (ly-toggle-html-generation)
        (should ly-gen-html))
    (ly-toggle-html-generation)
    (should ly-gen-html)
    (ly-toggle-html-generation)
    (should (not ly-gen-html))))

(ert-deftest ob-lilypond/ly-switch-extension-with-extensions ()
  (should (equal "test-name.xyz"
                 (ly-switch-extension "test-name" ".xyz")))
  (should (equal "test-name.xyz"
                 (ly-switch-extension "test-name.abc" ".xyz")))
  (should (equal "test-name"
                 (ly-switch-extension "test-name.abc" ""))))

(ert-deftest ob-lilypond/ly-switch-extension-with-paths ()
  (should (equal "/some/path/to/test-name.xyz"
                  (ly-switch-extension "/some/path/to/test-name" ".xyz"))))

(ert-deftest ob-lilypond/ly-get-header-args ()
  (should (equal '((:tangle . "yes")
                   (:noweb . "yes")
                   (:results . "silent")
                   (:cache . "yes")
                   (:comments . "yes"))
                 (ly-set-header-args t)))
  (should (equal '((:results . "file")
                   (:exports . "results"))
                 (ly-set-header-args nil))))

(ert-deftest ob-lilypond/ly-set-header-args ()
  (ly-set-header-args t)
  (should (equal '((:tangle . "yes")
                   (:noweb . "yes")
                   (:results . "silent")
                   (:cache . "yes")
                   (:comments . "yes"))
                 org-babel-default-header-args:lilypond))
  (ly-set-header-args nil)
  (should (equal '((:results . "file")
                   (:exports . "results"))
                 org-babel-default-header-args:lilypond)))

(provide 'test-ob-lilypond)

;;; test-ob-lilypond.el ends here
