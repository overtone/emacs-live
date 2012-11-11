;;; rainbow-mode.el --- Colorize color names in buffers

;; Copyright (C) 2010-2012 Free Software Foundation, Inc

;; Author: Julien Danjou <julien@danjou.info>
;; Keywords: faces
;; Version: 0.4

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This minor mode sets background color to strings that match color
;; names, e.g. #0000ff is displayed in white with a blue background.
;;

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'regexp-opt)
(require 'faces)
(require 'color)

(defgroup rainbow nil
  "Show color strings with a background color."
  :tag "Rainbow"
  :group 'help)

;; Hexadecimal colors
(defvar rainbow-hexadecimal-colors-font-lock-keywords
  '(("[^&]\\(#\\(?:[0-9a-fA-F]\\{3\\}\\)+\\{1,4\\}\\)"
     (1 (rainbow-colorize-itself 1)))
    ("^\\(#\\(?:[0-9a-fA-F]\\{3\\}\\)+\\{1,4\\}\\)"
     (0 (rainbow-colorize-itself)))
    ("[Rr][Gg][Bb]:[0-9a-fA-F]\\{1,4\\}/[0-9a-fA-F]\\{1,4\\}/[0-9a-fA-F]\\{1,4\\}"
     (0 (rainbow-colorize-itself)))
    ("[Rr][Gg][Bb][Ii]:[0-9.]+/[0-9.]+/[0-9.]+"
     (0 (rainbow-colorize-itself)))
    ("\\(?:[Cc][Ii][Ee]\\(?:[Xx][Yy][Zz]\\|[Uu][Vv][Yy]\\|[Xx][Yy][Yy]\\|[Ll][Aa][Bb]\\|[Ll][Uu][Vv]\\)\\|[Tt][Ee][Kk][Hh][Vv][Cc]\\):[+-]?[0-9.]+\\(?:[Ee][+-]?[0-9]+\\)?/[+-]?[0-9.]+\\(?:[Ee][+-]?[0-9]+\\)?/[+-]?[0-9.]+\\(?:[Ee][+-]?[0-9]+\\)?")
    (0 (rainbow-colorize-itself)))
  "Font-lock keywords to add for hexadecimal colors.")

;; rgb() colors
(defvar rainbow-html-rgb-colors-font-lock-keywords
  '(("rgb(\s*\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*)"
     (0 (rainbow-colorize-rgb)))
    ("rgba(\s*\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*,\s*[0-9]*\.?[0-9]+\s*%?\s*)"
     (0 (rainbow-colorize-rgb)))
    ("hsl(\s*\\([0-9]\\{1,3\\}\\)\s*,\s*\\([0-9]\\{1,3\\}\\)\s*%\s*,\s*\\([0-9]\\{1,3\\}\\)\s*%\s*)"
     (0 (rainbow-colorize-hsl)))
    ("hsla(\s*\\([0-9]\\{1,3\\}\\)\s*,\s*\\([0-9]\\{1,3\\}\\)\s*%\s*,\s*\\([0-9]\\{1,3\\}\\)\s*%\s*,\s*[0-9]*\.?[0-9]+\s*%?\s*)"
     (0 (rainbow-colorize-hsl))))
  "Font-lock keywords to add for RGB colors.")

;; HTML colors name
(defvar rainbow-html-colors-font-lock-keywords nil
  "Font-lock keywords to add for HTML colors.")
(make-variable-buffer-local 'rainbow-html-colors-font-lock-keywords)

(defcustom rainbow-html-colors-alist
  '(("AliceBlue" . "#F0F8FF")
    ("AntiqueWhite" . "#FAEBD7")
    ("Aqua" . "#00FFFF")
    ("Aquamarine" . "#7FFFD4")
    ("Azure" . "#F0FFFF")
    ("Beige" . "#F5F5DC")
    ("Bisque" . "#FFE4C4")
    ("Black" . "#000000")
    ("BlanchedAlmond" . "#FFEBCD")
    ("Blue" . "#0000FF")
    ("BlueViolet" . "#8A2BE2")
    ("Brown" . "#A52A2A")
    ("BurlyWood" . "#DEB887")
    ("CadetBlue" . "#5F9EA0")
    ("Chartreuse" . "#7FFF00")
    ("Chocolate" . "#D2691E")
    ("Coral" . "#FF7F50")
    ("CornflowerBlue" . "#6495ED")
    ("Cornsilk" . "#FFF8DC")
    ("Crimson" . "#DC143C")
    ("Cyan" . "#00FFFF")
    ("DarkBlue" . "#00008B")
    ("DarkCyan" . "#008B8B")
    ("DarkGoldenRod" . "#B8860B")
    ("DarkGray" . "#A9A9A9")
    ("DarkGrey" . "#A9A9A9")
    ("DarkGreen" . "#006400")
    ("DarkKhaki" . "#BDB76B")
    ("DarkMagenta" . "#8B008B")
    ("DarkOliveGreen" . "#556B2F")
    ("Darkorange" . "#FF8C00")
    ("DarkOrchid" . "#9932CC")
    ("DarkRed" . "#8B0000")
    ("DarkSalmon" . "#E9967A")
    ("DarkSeaGreen" . "#8FBC8F")
    ("DarkSlateBlue" . "#483D8B")
    ("DarkSlateGray" . "#2F4F4F")
    ("DarkSlateGrey" . "#2F4F4F")
    ("DarkTurquoise" . "#00CED1")
    ("DarkViolet" . "#9400D3")
    ("DeepPink" . "#FF1493")
    ("DeepSkyBlue" . "#00BFFF")
    ("DimGray" . "#696969")
    ("DimGrey" . "#696969")
    ("DodgerBlue" . "#1E90FF")
    ("FireBrick" . "#B22222")
    ("FloralWhite" . "#FFFAF0")
    ("ForestGreen" . "#228B22")
    ("Fuchsia" . "#FF00FF")
    ("Gainsboro" . "#DCDCDC")
    ("GhostWhite" . "#F8F8FF")
    ("Gold" . "#FFD700")
    ("GoldenRod" . "#DAA520")
    ("Gray" . "#808080")
    ("Grey" . "#808080")
    ("Green" . "#008000")
    ("GreenYellow" . "#ADFF2F")
    ("HoneyDew" . "#F0FFF0")
    ("HotPink" . "#FF69B4")
    ("IndianRed" . "#CD5C5C")
    ("Indigo" . "#4B0082")
    ("Ivory" . "#FFFFF0")
    ("Khaki" . "#F0E68C")
    ("Lavender" . "#E6E6FA")
    ("LavenderBlush" . "#FFF0F5")
    ("LawnGreen" . "#7CFC00")
    ("LemonChiffon" . "#FFFACD")
    ("LightBlue" . "#ADD8E6")
    ("LightCoral" . "#F08080")
    ("LightCyan" . "#E0FFFF")
    ("LightGoldenRodYellow" . "#FAFAD2")
    ("LightGray" . "#D3D3D3")
    ("LightGrey" . "#D3D3D3")
    ("LightGreen" . "#90EE90")
    ("LightPink" . "#FFB6C1")
    ("LightSalmon" . "#FFA07A")
    ("LightSeaGreen" . "#20B2AA")
    ("LightSkyBlue" . "#87CEFA")
    ("LightSlateGray" . "#778899")
    ("LightSlateGrey" . "#778899")
    ("LightSteelBlue" . "#B0C4DE")
    ("LightYellow" . "#FFFFE0")
    ("Lime" . "#00FF00")
    ("LimeGreen" . "#32CD32")
    ("Linen" . "#FAF0E6")
    ("Magenta" . "#FF00FF")
    ("Maroon" . "#800000")
    ("MediumAquaMarine" . "#66CDAA")
    ("MediumBlue" . "#0000CD")
    ("MediumOrchid" . "#BA55D3")
    ("MediumPurple" . "#9370D8")
    ("MediumSeaGreen" . "#3CB371")
    ("MediumSlateBlue" . "#7B68EE")
    ("MediumSpringGreen" . "#00FA9A")
    ("MediumTurquoise" . "#48D1CC")
    ("MediumVioletRed" . "#C71585")
    ("MidnightBlue" . "#191970")
    ("MintCream" . "#F5FFFA")
    ("MistyRose" . "#FFE4E1")
    ("Moccasin" . "#FFE4B5")
    ("NavajoWhite" . "#FFDEAD")
    ("Navy" . "#000080")
    ("OldLace" . "#FDF5E6")
    ("Olive" . "#808000")
    ("OliveDrab" . "#6B8E23")
    ("Orange" . "#FFA500")
    ("OrangeRed" . "#FF4500")
    ("Orchid" . "#DA70D6")
    ("PaleGoldenRod" . "#EEE8AA")
    ("PaleGreen" . "#98FB98")
    ("PaleTurquoise" . "#AFEEEE")
    ("PaleVioletRed" . "#D87093")
    ("PapayaWhip" . "#FFEFD5")
    ("PeachPuff" . "#FFDAB9")
    ("Peru" . "#CD853F")
    ("Pink" . "#FFC0CB")
    ("Plum" . "#DDA0DD")
    ("PowderBlue" . "#B0E0E6")
    ("Purple" . "#800080")
    ("Red" . "#FF0000")
    ("RosyBrown" . "#BC8F8F")
    ("RoyalBlue" . "#4169E1")
    ("SaddleBrown" . "#8B4513")
    ("Salmon" . "#FA8072")
    ("SandyBrown" . "#F4A460")
    ("SeaGreen" . "#2E8B57")
    ("SeaShell" . "#FFF5EE")
    ("Sienna" . "#A0522D")
    ("Silver" . "#C0C0C0")
    ("SkyBlue" . "#87CEEB")
    ("SlateBlue" . "#6A5ACD")
    ("SlateGray" . "#708090")
    ("SlateGrey" . "#708090")
    ("Snow" . "#FFFAFA")
    ("SpringGreen" . "#00FF7F")
    ("SteelBlue" . "#4682B4")
    ("Tan" . "#D2B48C")
    ("Teal" . "#008080")
    ("Thistle" . "#D8BFD8")
    ("Tomato" . "#FF6347")
    ("Turquoise" . "#40E0D0")
    ("Violet" . "#EE82EE")
    ("Wheat" . "#F5DEB3")
    ("White" . "#FFFFFF")
    ("WhiteSmoke" . "#F5F5F5")
    ("Yellow" . "#FFFF00")
    ("YellowGreen" . "#9ACD32"))
  "Alist of HTML colors.
Each entry should have the form (COLOR-NAME . HEXADECIMAL-COLOR)."
  :group 'rainbow)

(defcustom rainbow-html-colors-major-mode-list
  '(html-mode css-mode php-mode nxml-mode xml-mode)
  "List of major mode where HTML colors are enabled when
`rainbow-html-colors' is set to auto."
  :group 'rainbow)

(defcustom rainbow-html-colors 'auto
  "When to enable HTML colors.
If set to t, the HTML colors will be enabled.  If set to nil, the
HTML colors will not be enabled.  If set to auto, the HTML colors
will be enabled if a major mode has been detected from the
`rainbow-html-colors-major-mode-list'."
  :group 'rainbow)

;; X colors
(defvar rainbow-x-colors-font-lock-keywords
  `((,(regexp-opt (x-defined-colors) 'words)
     (0 (rainbow-colorize-itself))))
  "Font-lock keywords to add for X colors.")

(defcustom rainbow-x-colors-major-mode-list
  '(emacs-lisp-mode lisp-interaction-mode c-mode c++-mode java-mode)
  "List of major mode where X colors are enabled when
`rainbow-x-colors' is set to auto."
  :group 'rainbow)

(defcustom rainbow-x-colors 'auto
  "When to enable X colors.
If set to t, the X colors will be enabled.  If set to nil, the
X colors will not be enabled.  If set to auto, the X colors
will be enabled if a major mode has been detected from the
`rainbow-x-colors-major-mode-list'."
  :group 'rainbow)

;; LaTeX colors
(defvar rainbow-latex-rgb-colors-font-lock-keywords
  '(("{rgb}{\\([0-9.]+\\),\\([0-9.]+\\),\\([0-9.]+\\)}"
     (0 (rainbow-colorize-rgb-float)))
    ("{RGB}{\\([0-9]\\{1,3\\}\\),\\([0-9]\\{1,3\\}\\),\\([0-9]\\{1,3\\}\\)}"
     (0 (rainbow-colorize-rgb)))
    ("{HTML}{\\([0-9A-Fa-f]\\{6\\}\\)}"
     (0 (rainbow-colorize-hexadecimal-without-sharp))))
  "Font-lock keywords to add for X colors.")

(defcustom rainbow-latex-colors-major-mode-list
  '(latex-mode)
  "List of major mode where X colors are enabled when
`rainbow-x-colors' is set to auto."
  :group 'rainbow)

(defcustom rainbow-latex-colors 'auto
  "When to enable LaTeX colors.
If set to t, the LaTeX colors will be enabled. If set to nil, the
X colors will not be enabled.  If set to auto, the LaTeX colors
will be enabled if a major mode has been detected from the
`rainbow-latex-colors-major-mode-list'."
  :group 'rainbow)

;; Functions
(defun rainbow-colorize-match (color &optional match)
  "Return a matched string propertized with a face whose
background is COLOR. The foreground is computed using
`rainbow-color-luminance', and is either white or black."
  (let ((match (or match 0)))
    (put-text-property
     (match-beginning match) (match-end match)
     'face `((:foreground ,(if (> 0.5 (rainbow-x-color-luminance color))
                               "white" "black"))
             (:background ,color)))))

(defun rainbow-colorize-itself (&optional match)
  "Colorize a match with itself."
  (rainbow-colorize-match (match-string-no-properties (or match 0)) match))

(defun rainbow-colorize-hexadecimal-without-sharp ()
  "Colorize an hexadecimal colors and prepend # to it."
  (rainbow-colorize-match (concat "#" (match-string-no-properties 1))))

(defun rainbow-colorize-by-assoc (assoc-list)
  "Colorize a match with its association from ASSOC-LIST."
  (rainbow-colorize-match (cdr (assoc-string (match-string-no-properties 0)
                                             assoc-list t))))

(defun rainbow-rgb-relative-to-absolute (number)
  "Convert a relative NUMBER to absolute. If NUMBER is absolute, return NUMBER.
This will convert \"80 %\" to 204, \"100 %\" to 255 but \"123\" to \"123\"."
  (let ((string-length (- (length number) 1)))
    ;; Is this a number with %?
    (if (eq (elt number string-length) ?%)
        (/ (* (string-to-number (substring number 0 string-length)) 255) 100)
      (string-to-number number))))

(defun rainbow-colorize-hsl ()
  "Colorize a match with itself."
  (let ((h (/ (string-to-number (match-string-no-properties 1)) 360.0))
        (s (/ (string-to-number (match-string-no-properties 2)) 100.0))
        (l (/ (string-to-number (match-string-no-properties 3)) 100.0)))
    (rainbow-colorize-match
     (multiple-value-bind (r g b)
         (color-hsl-to-rgb h s l)
       (format "#%02X%02X%02X" (* r 255) (* g 255) (* b 255))))))

(defun rainbow-colorize-rgb ()
  "Colorize a match with itself."
  (let ((r (rainbow-rgb-relative-to-absolute (match-string-no-properties 1)))
        (g (rainbow-rgb-relative-to-absolute (match-string-no-properties 2)))
        (b (rainbow-rgb-relative-to-absolute (match-string-no-properties 3))))
    (rainbow-colorize-match (format "#%02X%02X%02X" r g b))))

(defun rainbow-colorize-rgb-float ()
  "Colorize a match with itself, with relative value."
  (let ((r (* (string-to-number (match-string-no-properties 1)) 255.0))
        (g (* (string-to-number (match-string-no-properties 2)) 255.0))
        (b (* (string-to-number (match-string-no-properties 3)) 255.0)))
    (rainbow-colorize-match (format "#%02X%02X%02X" r g b))))

(defun rainbow-color-luminance (red green blue)
  "Calculate the luminance of color composed of RED, BLUE and GREEN.
Return a value between 0 and 1."
  (/ (+ (* .2126 red) (* .7152 green) (* .0722 blue)) 256))

(defun rainbow-x-color-luminance (color)
  "Calculate the luminance of a color string (e.g. \"#ffaa00\", \"blue\").
Return a value between 0 and 1."
  (let* ((values (x-color-values color))
         (r (/ (car values) 256.0))
         (g (/ (cadr values) 256.0))
         (b (/ (caddr values) 256.0)))
    (rainbow-color-luminance r g b)))

(defun rainbow-turn-on ()
  "Turn on raibow-mode."
  (font-lock-add-keywords nil
                          rainbow-hexadecimal-colors-font-lock-keywords)
  ;; Activate X colors?
  (when (or (eq rainbow-x-colors t)
            (and (eq rainbow-x-colors 'auto)
                 (memq major-mode rainbow-x-colors-major-mode-list)))
    (font-lock-add-keywords nil
                            rainbow-x-colors-font-lock-keywords))
  ;; Activate LaTeX colors?
  (when (or (eq rainbow-latex-colors t)
            (and (eq rainbow-latex-colors 'auto)
                 (memq major-mode rainbow-latex-colors-major-mode-list)))
    (font-lock-add-keywords nil
                            rainbow-latex-rgb-colors-font-lock-keywords))
  ;; Activate HTML colors?
  (when (or (eq rainbow-html-colors t)
            (and (eq rainbow-html-colors 'auto)
                 (memq major-mode rainbow-html-colors-major-mode-list)))
    (setq rainbow-html-colors-font-lock-keywords
          `((,(regexp-opt (mapcar 'car rainbow-html-colors-alist) 'words)
             (0 (rainbow-colorize-by-assoc rainbow-html-colors-alist)))))
    (font-lock-add-keywords nil
                            `(,@rainbow-html-colors-font-lock-keywords
                              ,@rainbow-html-rgb-colors-font-lock-keywords))))

(defun rainbow-turn-off ()
  "Turn off rainbow-mode."
  (font-lock-remove-keywords
   nil
   `(,@rainbow-hexadecimal-colors-font-lock-keywords
     ,@rainbow-x-colors-font-lock-keywords
     ,@rainbow-latex-rgb-colors-font-lock-keywords
     ,@rainbow-html-colors-font-lock-keywords
     ,@rainbow-html-rgb-colors-font-lock-keywords)))

;;;###autoload
(define-minor-mode rainbow-mode
  "Colorize strings that represent colors.
This will fontify with colors the string like \"#aabbcc\" or \"blue\"."
  :lighter " Rbow"
  (progn
    (if rainbow-mode
        (rainbow-turn-on)
      (rainbow-turn-off))
    ;; Turn on font lock
    (font-lock-mode 1)))

(provide 'rainbow-mode)

;;; rainbow-mode.el ends here
