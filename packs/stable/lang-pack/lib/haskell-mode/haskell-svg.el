;;; haskell-svg.el --- SVG Rendering -*- lexical-binding: t -*-

;; Copyright (c) 2018 Federico Beffa. All rights reserved.

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

;;; Code:

(defcustom haskell-svg-render-images nil
  "Replace SVG image text with actual images."
  :group 'haskell-interactive
  :type 'boolean)

(defconst haskell-svg-supported (image-type-available-p 'svg)
  "Defines if SVG images are supported by this instance of Emacs.")



(defun haskell-svg-render-images-p ()
  "Shall we render SVG images?"
  (and haskell-svg-supported (display-images-p) haskell-svg-render-images))

(defun haskell-svg-maybe-render-images (text)
  "Render SVG images if desired and supported, or terurn the
input unmodified."
  (if (haskell-svg-render-images-p)
      (haskell-svg-render-images text)
    text))

(defun haskell-svg-render-images (text)
  "Replace an SVG image text with an actual image."
  (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (when (re-search-forward
             "\"?<\\?xml\\(.\\|\n\\|\r\\)* PUBLIC \"-//W3C//DTD SVG [0-9]\.[0-9]//EN\\(.\\|\n\\|\r\\)*</svg>\"?"
             nil t)
        (let ((svg-string (match-string 0))
              (begin (match-beginning 0))
              (end (match-end 0)))
          (delete-region begin end)
          (goto-char begin)
          (insert-image (create-image svg-string nil t) "SVG image")))
      (buffer-substring (point-min) (point-max))))

(defun haskell-svg-toggle-render-images ()
  "Toggle rendering of SVG images at the REPL output."
  (interactive)
  (setq haskell-svg-render-images (not haskell-svg-render-images)))



(provide 'haskell-svg)

;;; haskell-svg.el ends here
