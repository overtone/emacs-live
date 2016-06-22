;;; haskell-mode.el --- A Haskell editing mode    -*- coding: utf-8 -*-

;; Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008  Free Software Foundation, Inc
;; Copyright (C) 1992, 1997-1998  Simon Marlow, Graeme E Moss, and Tommy Thorn

;; Author:  2015      Gracjan Polak <gracjanpolak@gmail.com>
;; Keywords: screencast
;; Version: 0.1

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Library for Emacs screencasts. Code you execution in lisp, sprinkle
;; with capture functions, generate upload ready gif animations.

;;; Code:

(defvar gifcast--ns-current-frame-window-id nil
  "Mac OSX native window id of the current frame.")

(defvar gifcast--animation-frame-index nil
  "Current animation frame index.")

(defvar gifcast--action-list nil
  "Actions to run on timer.

List of lambdas or functions.

On next timer tick (car gifcast--action-list) will be run and
removed from this list.

Note that it is useful both to prepend actions (to be run next)
and append actions (to be run later, after everything already in the list has run.")

(defun gifcast--run-next-action ()
  "Run next action in gifcast--action-list.

Will start timer if needed to run follow up actions."
  (when (consp gifcast--action-list)
    (let ((c (car gifcast--action-list)))
      (setq gifcast--action-list (cdr gifcast--action-list))
      (funcall c)
      (run-at-time 0.1 nil #'gifcast--run-next-action))))

(defun gifcast-append-action (action)
  "Schedule ACTION to be run after all already scheduled actions."
  (when (null gifcast--action-list)
    (run-at-time 0.1 nil #'gifcast--run-next-action))
  (setq gifcast--action-list (append gifcast--action-list (list action))))

(defun gifcast-prepend-action (action)
  "Schedule ACTION to be run before other already scheduled actions."
  (when (null gifcast--action-list)
    (run-at-time 0.1 nil #'gifcast--run-next-action))
  (setq gifcast--action-list (cons action gifcast--action-list)))

(defun gifcast--ns-get-current-frame-window-id ()
  "Get native Mac OS X window id of the current frame."

  (with-temp-file "get-window-id.m"
    (insert "
#include <Cocoa/Cocoa.h>
#include <CoreGraphics/CGWindow.h>

int main(int argc, char **argv)
{
    NSArray *windows = (NSArray *)CGWindowListCopyWindowInfo(kCGWindowListExcludeDesktopElements,kCGNullWindowID);
    for(NSDictionary *window in windows) {
        if ([[window objectForKey:(NSString *)kCGWindowOwnerPID] isEqual:[NSNumber numberWithLongLong:atoi(argv[1])]]) {
            if ([[window objectForKey:(NSString *)kCGWindowName] isEqual:[NSString stringWithUTF8String:argv[2]]]) {
                printf(\"%d\\n\", [[window objectForKey:(NSString *)kCGWindowNumber] intValue]);
            }
        }
    }
}
"))
  (with-current-buffer "*Messages*"
    (call-process "clang" nil t nil
                  "get-window-id.m" "-o" "get-window-id" "-framework" "CoreGraphics" "-framework" "Cocoa"))
  (chmod "get-window-id" #o755)
  (catch 'return
    (with-temp-buffer
      (dolist (x '(1))

        (call-process (concat default-directory "get-window-id") nil t nil (number-to-string (emacs-pid))
                      (cdr (assoc 'name (frame-parameters (window-frame (get-buffer-window))))))

        (goto-char (point-min))
        (if (looking-at "[0-9]+")
            (throw 'return (match-string 0))
          (sit-for 1))))))

(defun gifcast--ns-capture (filename)
  "Capture sceenshot of current frame and save it to FILENAME.

Image format will be png."
  (with-current-buffer "*Messages*"
    (let ((args (list (concat "-l" gifcast--ns-current-frame-window-id) "-o" filename))
          (buffer-read-only nil))

      (apply #'call-process "screencapture" nil t nil args))))

(defmacro gifcast-animate (&rest body)
  "Entry point for animation definitions.

Use at top level like this:

(gifcast-animate
 (set-frame-size (window-frame (get-buffer-window)) 40 10)
 (when (get-buffer \"main.c\") (kill-buffer \"main.c\"))
 (switch-to-buffer (get-buffer-create \"main.c\"))
 (delete-other-windows)
 (tabbar-mode -1)
 (tool-bar-mode -1)
 (blink-cursor-mode -1)
 (my-mode)

 ...generate animation frames..
 )
"
  `(gifcast-append-action
    (lambda ()
      (unless gifcast--ns-current-frame-window-id
        (setq gifcast--ns-current-frame-window-id (gifcast--ns-get-current-frame-window-id)))

      (setq gifcast--animation-frame-index 1)

      ,@body)))

(defmacro gifcast-keys-async (keys &rest body)
  "Simulate keystrokes KEYS and continue with BODY.

Append KEYS to `unread-command-events' and let the main loop take
over. This requires function exit and continuing after a
timer. BODY is executed after 0.1s has elapsed.

Note that this is async programming so requires proper bookeeping
with repsect to current buffer, state and local variables.
"
  `(progn
     (setq unread-command-events (append unread-command-events (listify-key-sequence ,keys)))
     (gifcast-prepend-action (lambda () ,@body))))

(defun gifcast-capture ()
  "Capture current frame.

Capture file will be named based on internal index variable."
  (sit-for 0.1)
  (gifcast--ns-capture (concat "frame-" (number-to-string gifcast--animation-frame-index) ".png"))
  (setq gifcast--animation-frame-index (1+ gifcast--animation-frame-index)))

(defun gifcast-generate (filename)
  "Generate animation to FILENAME.

Takes frames collected since last `gifcast-animate' and generate
a GIF animation. FILENAME will be used as a file name for the
animation."
  (with-current-buffer "*Messages*"
    (let ((idx 1)
          args
          (buffer-read-only nil))
      (while (< idx gifcast--animation-frame-index)
        (setq args (append args (list (concat "frame-" (number-to-string idx) ".png"))))
        (setq idx (1+ idx)))

      (apply #'call-process "convert" nil t nil
             (append (list "-delay" "100") args (list "-layers" "OptimizePlus" "-alpha" "remove" filename))))))

(provide 'gifcast)
;;; gifcast ends here
