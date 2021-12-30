;;; logito-test.el -- test for logito.el

;; Copyright (C) 2020  Yann Hodique

;; Author: Yann Hodique <yann.hodique@gmail.com>
;; Keywords: lisp, tool

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;

;;; Code:

(require 'ert)
(require 'logito)
(require 'mocker)

;; generate test data
(defun logito-test-logs (logger)
  (let ((msg "plop"))
    (logito:error logger msg)
    (logito:info logger msg)
    (logito:verbose logger msg)
    (logito:debug logger msg)))

;; just for the sake of testing level definition, this is what's used as other
;; tests' threshold.
(logito-def-level test logito:verbose-level)

(ert-deftest logito-buffer-test ()
  (let ((nlines (with-temp-buffer
                  (let ((logger (logito-buffer-object
                                 :buffer (current-buffer) :level logito:test-level)))
                    (logito-test-logs logger)
                    (line-number-at-pos)))))
    ;; 2 lines per logged message, plus an additional newline, the debug level should be ignored
    (should (equal nlines 7))))

(ert-deftest logito-message-test ()
  (let ((logger (logito-message-object :level logito:test-level)))
    ;; message should be called 3 times since the debug level is ignored
    (mocker-let ((message (x) ((:input-matcher 'identity :occur 3))))
                (logito-test-logs logger))))
