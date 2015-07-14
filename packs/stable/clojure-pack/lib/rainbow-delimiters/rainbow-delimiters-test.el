;;; rainbow-delimiters-test.el --- rainbow-delimiters test suite

;; Author: Fanael Linithien <fanael4@gmail.com>
;; URL: https://github.com/Fanael/rainbow-delimiters

;; This file is NOT part of GNU Emacs.

;; Copyright (c) 2014-2015, Fanael Linithien
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;   * Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;   * Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
;; IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
;; OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; `rainbow-delimiters' test suite.

;;; Code:

(unless noninteractive
  (error "This file should only be used noninteractively"))

(push (file-name-directory load-file-name) load-path)
(setq font-lock-verbose nil)

(require 'rainbow-delimiters)
(require 'ert)

(defmacro with-temp-buffer-in-mode (mode &rest body)
  (declare (indent defun) (debug t))
  `(with-temp-buffer
     (funcall ,mode)
     (font-lock-mode)
     (rainbow-delimiters-mode)
     ,@body))

(defmacro with-string (strdecl &rest body)
  (declare (indent defun) (debug t))
  `(let ((,(car strdecl) ,(cadr strdecl)))
     (insert ,(car strdecl))
     (fontify-buffer)
     ,@body))

(defun fontify-buffer ()
  (if (fboundp 'font-lock-ensure)
      (font-lock-ensure)
    (with-no-warnings (font-lock-fontify-buffer))))

(defun fontify-without-rainbow-delimiters (mode text)
  (with-temp-buffer
    (funcall mode)
    (insert text)
    (fontify-buffer)
    (buffer-string)))

(defun should-do-nothing (mode str)
  (with-temp-buffer-in-mode mode
    (with-string (str str)
      (should (ert-equal-including-properties
               (buffer-string)
               (fontify-without-rainbow-delimiters mode str))))))

(ert-deftest can-enable-mode ()
  (with-temp-buffer
    (rainbow-delimiters-mode 1)
    (should rainbow-delimiters-mode)))

(ert-deftest can-disable-mode ()
  (with-temp-buffer
    (rainbow-delimiters-mode 1)
    (rainbow-delimiters-mode 0)
    (should-not rainbow-delimiters-mode)))

(defmacro highlights-matching-delim-test (name opening closing)
  `(ert-deftest ,(intern (format "highlights-matching-%s" name)) ()
     (with-temp-buffer-in-mode 'text-mode
       (with-string (str ,(format "%cfoo%c" opening closing))
         (should (ert-equal-including-properties
                  (buffer-string)
                  (progn
                    (add-text-properties 0 1 '(face (rainbow-delimiters-depth-1-face)) str)
                    (add-text-properties 4 5 '(face (rainbow-delimiters-depth-1-face)) str)
                    str)))))))

(highlights-matching-delim-test "parens" ?\( ?\))
(highlights-matching-delim-test "brackets" ?\[ ?\])
(highlights-matching-delim-test "braces" ?\{ ?\})

(defmacro highlights-matching-nested-delim-test (name opening closing)
  `(ert-deftest ,(intern (format "highlights-nested-matching-%s" name)) ()
     (with-temp-buffer-in-mode 'text-mode
       (with-string (str ,(format "%sfoo%s" (make-string 4 opening) (make-string 4 closing)))
         (should (ert-equal-including-properties
                  (buffer-string)
                  (progn
                    (add-text-properties 0 1 '(face (rainbow-delimiters-depth-1-face)) str)
                    (add-text-properties 1 2 '(face (rainbow-delimiters-depth-2-face)) str)
                    (add-text-properties 2 3 '(face (rainbow-delimiters-depth-3-face)) str)
                    (add-text-properties 3 4 '(face (rainbow-delimiters-depth-4-face)) str)
                    (add-text-properties 7 8 '(face (rainbow-delimiters-depth-4-face)) str)
                    (add-text-properties 8 9 '(face (rainbow-delimiters-depth-3-face)) str)
                    (add-text-properties 9 10 '(face (rainbow-delimiters-depth-2-face)) str)
                    (add-text-properties 10 11 '(face (rainbow-delimiters-depth-1-face)) str)
                    str)))))))

(highlights-matching-nested-delim-test "parens" ?\( ?\))
(highlights-matching-nested-delim-test "brackets" ?\[ ?\])
(highlights-matching-nested-delim-test "braces" ?\{ ?\})

(ert-deftest highlights-mixed-matching-delimiters ()
  (with-temp-buffer-in-mode 'text-mode
    (with-string (str "([{(foo)}])")
      (should (ert-equal-including-properties
               (buffer-string)
               #("([{(foo)}])"
                 0 1 (face (rainbow-delimiters-depth-1-face))
                 1 2 (face (rainbow-delimiters-depth-2-face))
                 2 3 (face (rainbow-delimiters-depth-3-face))
                 3 4 (face (rainbow-delimiters-depth-4-face))
                 7 8 (face (rainbow-delimiters-depth-4-face))
                 8 9 (face (rainbow-delimiters-depth-3-face))
                 9 10 (face (rainbow-delimiters-depth-2-face))
                 10 11 (face (rainbow-delimiters-depth-1-face))))))))

(ert-deftest highlights-all-delimiters ()
  (with-temp-buffer-in-mode 'c++-mode
    (with-string (str "foo<int> x;")
      (should (ert-equal-including-properties
               (progn
                 (remove-list-of-text-properties
                  (point-min) (point-max) '(category c-type syntax-table))
                 (buffer-string))
               #("foo<int> x;"
                 0 3 (face font-lock-type-face)
                 3 4 (face (rainbow-delimiters-depth-1-face))
                 4 7 (face font-lock-type-face)
                 7 8 (face (rainbow-delimiters-depth-1-face))
                 9 10 (face font-lock-variable-name-face)))))))

(ert-deftest doesnt-higlight-nondelimiters-1 ()
  (should-do-nothing 'text-mode "foo"))

(ert-deftest doesnt-higlight-nondelimiters-2 ()
  (should-do-nothing 'emacs-lisp-mode "{foo}"))

(ert-deftest doesnt-highlight-in-comments-1 ()
  (should-do-nothing 'emacs-lisp-mode "; ()[]"))

(ert-deftest doesnt-highlight-in-comments-2 ()
  (should-do-nothing 'pascal-mode "(* foo *)"))

(ert-deftest doesnt-highlight-in-strings ()
  (should-do-nothing 'emacs-lisp-mode "\"()\""))

(ert-deftest highlights-unmatched ()
  (with-temp-buffer-in-mode 'emacs-lisp-mode
    (with-string (str ")")
      (should (ert-equal-including-properties
               (buffer-string)
               #(")"
                 0 1 (face (rainbow-delimiters-unmatched-face))))))))

(ert-deftest highlights-mismatched ()
  (with-temp-buffer-in-mode 'emacs-lisp-mode
    (with-string (str "(]")
      (should (ert-equal-including-properties
               (buffer-string)
               #("(]"
                 0 1 (face (rainbow-delimiters-depth-1-face))
                 1 2 (face (rainbow-delimiters-mismatched-face))))))))

(ert-deftest doesnt-highlight-escaped-delimiters ()
  (with-temp-buffer-in-mode 'emacs-lisp-mode
    (with-string (str "(bar ?\\( (foo?))")
      (should (ert-equal-including-properties
               (buffer-string)
               #("(bar ?\\( (foo?))"
                 0 1
                 (face (rainbow-delimiters-depth-1-face))
                 9 10
                 (face (rainbow-delimiters-depth-2-face))
                 14 15
                 (face (rainbow-delimiters-depth-2-face))
                 15 16
                 (face (rainbow-delimiters-depth-1-face))))))))

(ert-deftest cycles-faces ()
  (let ((rainbow-delimiters-max-face-count 2))
    (with-temp-buffer-in-mode 'text-mode
      (with-string (str "(((())))")
        (should (ert-equal-including-properties
                 (buffer-string)
                 #("(((())))"
                   0 1 (face (rainbow-delimiters-depth-1-face))
                   1 2 (face (rainbow-delimiters-depth-2-face))
                   2 3 (face (rainbow-delimiters-depth-1-face))
                   3 4 (face (rainbow-delimiters-depth-2-face))
                   4 5 (face (rainbow-delimiters-depth-2-face))
                   5 6 (face (rainbow-delimiters-depth-1-face))
                   6 7 (face (rainbow-delimiters-depth-2-face))
                   7 8 (face (rainbow-delimiters-depth-1-face)))))))))

(ert-deftest doesnt-cycle-outermost-only-faces ()
  (let ((rainbow-delimiters-outermost-only-face-count 2)
        (rainbow-delimiters-max-face-count 3))
    (with-temp-buffer-in-mode 'text-mode
      (with-string (str "(((())))")
        (should (ert-equal-including-properties
                 (buffer-string)
                 #("(((())))"
                   0 1 (face (rainbow-delimiters-depth-1-face))
                   1 2 (face (rainbow-delimiters-depth-2-face))
                   2 3 (face (rainbow-delimiters-depth-3-face))
                   3 4 (face (rainbow-delimiters-depth-3-face))
                   4 5 (face (rainbow-delimiters-depth-3-face))
                   5 6 (face (rainbow-delimiters-depth-3-face))
                   6 7 (face (rainbow-delimiters-depth-2-face))
                   7 8 (face (rainbow-delimiters-depth-1-face)))))))))

(ert-deftest highlights-already-highlighted ()
  (with-temp-buffer-in-mode 'diff-mode
    (with-string (str "+ foo ()\n")
      (should (ert-equal-including-properties
               (buffer-string)
               #("+ foo ()\n"
                 0 1 (face diff-indicator-added)
                 1 6 (face diff-added)
                 6 7 (face (rainbow-delimiters-depth-1-face diff-added))
                 7 8 (face (rainbow-delimiters-depth-1-face diff-added))
                 8 9 (face diff-added)))))))

(ert-deftest can-customize-face-picker ()
  (let ((rainbow-delimiters-pick-face-function
         (lambda (_depth _match _loc)
           'font-lock-keyword-face)))
    (with-temp-buffer-in-mode 'emacs-lisp-mode
      (with-string (str "(())")
        (should (ert-equal-including-properties
                 (buffer-string)
                 #("(())"
                   0 1 (face (font-lock-keyword-face))
                   1 2 (face (font-lock-keyword-face))
                   2 3 (face (font-lock-keyword-face))
                   3 4 (face (font-lock-keyword-face)))))))))

(ert-deftest face-picker-can-disable-highlighting ()
  (let ((rainbow-delimiters-pick-face-function
         (lambda (depth match loc)
           (unless (memq (char-after loc) '(?\( ?\)))
             (rainbow-delimiters-default-pick-face depth match loc)))))
    (should-do-nothing 'text-mode "(((())))")))

(ert-deftest delimiters-disabled-by-face-picker-contribute-to-depth ()
  (let ((rainbow-delimiters-pick-face-function
         (lambda (depth match loc)
           (unless (memq (char-after loc) '(?\( ?\)))
             (rainbow-delimiters-default-pick-face depth match loc)))))
    (with-temp-buffer-in-mode 'text-mode
      (with-string (str "([])")
        (should (ert-equal-including-properties
                 (buffer-string)
                 #("([])"
                   1 2 (face (rainbow-delimiters-depth-2-face))
                   2 3 (face (rainbow-delimiters-depth-2-face)))))))))

(provide 'rainbow-delimiters-test)
;;; rainbow-delimiters-test.el ends here
